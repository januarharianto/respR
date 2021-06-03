
#' Calculate rate of change in oxygen over time
#'
#' `calc_rate` calculates the rate of change in oxygen concentration over time
#' in a data frame. You can perform a single regression on a subset of the data
#' frame by calling the `from` and `to` arguments to specify the data region in
#' terms of `oxygen` or `time` units, `row` region of the input data, or over a
#' `proportion` of the total oxygen used or produced (note, this last option works
#' poorly with noisy or fluctuating data). Multiple rates can
#' be calculated by using these arguments to enter vectors of paired values in
#' the appropriate metric. See Examples.
#'
#' The results are plotted by default with various summary statistics for the
#' regression. If multiple rates are determined, only the first is plotted.
#' Others can be plotted by assigning the output and using the `pos` argument,
#' for example `plot(x, pos = 2)`. The output object of class `calc_rate` also
#' supports the other generic S3 functions `print`, `summary`, and `mean`. This
#' last option outputs the mean value of all rates determined.
#'
#' There are no units involved in `calc_rate`. This is a deliberate decision.
#' Units are called in a later function when absolute and/or mass-specific rates
#' of oxygen use are computed in [convert_rate] and [convert_DO].
#'
#' @param x data frame or object of class `inspect`. This is the data to
#'   process.
#' @param from numeric value or vector. Defaults to `NULL`. Defines the lower
#'   bound(s) of the data frame to subset. Subsetting is based on the argument:
#'   `by`.
#' @param to numeric value or vector. Defaults to `NULL`. Defines the upper
#'   bound(s) of the data frame to subset. Subsetting is based on the argument:
#'   `by`.
#' @param by string. `time`, `row`, `o2` or `proportion` Defaults to `time`.This
#'   is the method used to subset the data.
#' @param plot logical. Defaults to `TRUE`. Plot the results.
#'
#' @importFrom data.table data.table rbindlist
#' @import utils
#' @import stats
#' @import graphics
#' @import grDevices
#'
#' @return A list object of class `calc_rate`.
#' @export
#'
#' @examples
#' # default - subset by 'time'
#' calc_rate(sardine.rd, from = 200, to = 1800)
#'
#' # subset by O2
#' calc_rate(sardine.rd, 94, 91, by = 'o2')
#'
#' # subset by row
#' calc_rate(sardine.rd, 1, 1000, by = 'row')
#'
#' # subset by proportion of total O2 used
#' x <- calc_rate(sardine.rd, .8, .2, by = 'proportion')
#'
#' ## summary and print
#' summary(x)
#' plot(x)
#'
#' # Using a vector in 'from' and 'to' perform multiple rate calculations:
#' x <- calc_rate(intermittent.rd,
#'                from = c(200,2300,4100),
#'                to = c(1800,3200,4600),
#'                by = 'time',
#'                plot = FALSE)
#' # View all rates
#' summary(x)
#' # Plot the third of these results
#' plot(x, pos = 3)

calc_rate <- function(x, from = NULL, to = NULL, by = "time", plot = TRUE) {

  # Validate inputs
  # Will migrate to assertive package when I get used to it..
  ## verify by input
  by <- verify_by(by, msg = "calc_rate:")

  # Extract data.frame from inspect functions
  if(any(class(x) %in% "inspect_data")) x <- x$dataframe # this will be removed later
  if(any(class(x) %in% "inspect")) x <- x$dataframe

  # By now, x input must be a data frame object
  if(!is.data.frame(x)) stop("calc_rate: Input must be a 'data.frame' or 'inspect' object.")

  # Format as data.table
  x <- data.table::data.table(x)
  if (length(x) > 2) {
    warning("calc_rate: Multi-column dataset detected in input. Selecting first two columns by default.\n  If these are not the intended data, inspect() or subset the data frame columns appropriately before running calc_rate()")
    x <- x[, 1:2]
  }

  # If 'from' and 'to' are NULL, we assume that the user is analysing all data
  if (all(sapply(list(from, to), is.null))) {
    from <- 1; to <- nrow(x); by <- "row"
  }


  # from/to checks  ---------------------------------------------------------

  # Ensure "from" and "to" are same length:
  if (length(from) != length(to)) stop("calc_rate: 'from' and 'to' have unequal lengths.")
  # values of "from" and "to" can't be equal (for any metric):
  if(any(mapply(function(p,q) p == q,
                p = from,
                q = to))) stop("calc_rate: some 'from' values are equal to the paired values in 'to'.")

  if(by == "time"){
    ## all 'from' should be less than its paired 'to'
    if(any(mapply(function(p,q) p > q,
                  p = from,
                  q = to))) stop("calc_rate: some 'from' time values are later than the paired values in 'to'.")

    t_range <- range(x[[1]], na.rm = TRUE)
    if(any(sapply(from, function(z) z > t_range[2])))
      stop("calc_rate: some 'from' time values are higher than the values present in 'x'.")
    if(any(sapply(to, function(z) z < t_range[1])))
      stop("calc_rate: some 'to' time values are lower than the values present in 'x'.")
    if(any(sapply(from, function(z) z < t_range[1])))
      message("calc_rate: some 'from' time values are lower than the values present in 'x'. The lowest time value will be used instead.")
    if(any(sapply(to, function(z) z > t_range[2])))
      message("calc_rate: some 'to' time values are higher than the values present in 'x'. The highest time value will be used instead.")
  }

  if(by == "row"){
    if(any(mapply(function(p,q) p > q,
                  p = from,
                  q = to))) stop("calc_rate: some 'from' row numbers are higher than the paired values in 'to'.")

    r_range <- range(1:nrow(x))
    if(any(sapply(from, function(z) z > r_range[2])))
      stop("calc_rate: some 'from' row numbers are beyond the number of rows present in 'x'.")
    if(any(sapply(to, function(z) z > r_range[2])))
      message("calc_rate: some 'to' row numbers are higher than the number of rows present in 'x'. The final row number will be used instead.")
  }

  if(by == "o2"){
    o_range <- range(x[[2]], na.rm = TRUE)

    ## can't have 'from' and 'to' both below or both above o2 range
    if(any(mapply(function(p,q) p < o_range[1] && q < o_range[1],
                  p = from,
                  q = to))) stop("calc_rate: some paired 'from' and 'to' values are both below the range of oxygen data in 'x'.")
    if(any(mapply(function(p,q) p > o_range[2] && q > o_range[2],
                  p = from,
                  q = to))) stop("calc_rate: some paired 'from' and 'to' values are both above the range of oxygen data in 'x'.")

    ## if any 'from' or 'to' are above or below o2 range
    if(any(sapply(from, function(z) z > o_range[2]))) {
      message("calc_rate: some 'from' oxygen values are higher than the values in 'x'. The highest available value will be used instead.")
    } else if(any(sapply(from, function(z) z < o_range[1]))) {
      message("calc_rate: some 'from' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
    }

    if(any(sapply(to, function(z) z > o_range[2]))) {
      message("calc_rate: some 'to' oxygen values are higher than the values in 'x'. The highest available value will be used instead.")
    } else if(any(sapply(to, function(z) z < o_range[1]))) {
      message("calc_rate: some 'to' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
    }
  }

  if(by == "proportion"){
    if(any(!(sapply(from, function(z) data.table::between(z, 0, 1)))))
      stop("calc_rate: for by = 'proportion' method, all 'from' values should be between 0 and 1.")
    if(any(!(sapply(to, function(z) data.table::between(z, 0, 1)))))
      stop("calc_rate: for by = 'proportion' method, all 'to' values should be between 0 and 1.")
  }

  # Subset the data:
  dt <- lapply(1:length(from), function(z) truncate_data(x, from[z], to[z], by))

  # Perform lm on data and extract coefficients
  coefs <- lapply(1:length(to), function(z) linear_fit(dt[[z]]))

  # Extract row, time and DO indices from subsets
  indices <- lapply(1:length(dt), function(z) extract_indices(x, dt, z))

  # Extract row, time and DO indices from subsets and add to results
  rdt <- data.table::rbindlist(lapply(1:length(to), function(x)
    cbind(coefs[[x]], indices[[x]])))

  # Include row and time lengths, and twopoint method in table

  rdt[, rowlength := endrow - row]
  rdt[, timelength := endtime - time]
  rdt[, rate_twopoint := ((endoxy - oxy) / timelength)]

  # Extract rate_b1
  rate <- rdt[,rate_b1]

  # Generate output
  out <- list(
    dataframe = x,
    from = from,
    to = to,
    by = by,
    subsets = dt,
    summary = rdt,
    rate = rate,
    rate_2pt = rdt$rate_twopoint
  )

  class(out) <- "calc_rate"

  # Plot if TRUE
  if (plot) plot(out)

  return(out)
}


#' @export
print.calc_rate <- function(x, ...) {
  cat("\n# print.calc_rate # ---------------------\n")
  cat("Rate(s):\n")
  print(x$rate)
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' @export
summary.calc_rate <- function(object, export = FALSE, ...) {

  cat("\n# summary.calc_rate # -------------------\n")
  print(object$summary)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(object$summary)) else
      return(invisible(object))
}

#' @export
plot.calc_rate <- function(x, pos = 1, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings

  if(is.null(pos)) pos <- 1
  if(pos > length(x$rate))
    stop("Invalid 'pos' rank: only ", length(x$rate), " rates found.")

  cat("\n# plot.calc_rate # ----------------------\n")
  cat('Plotting calc_rate result from position', pos, 'of', length(x$rate), '... \n')
  df  <- x$dataframe
  sdf <- x$subsets[[pos]]
  fit <- lm(sdf[[2]] ~ sdf[[1]], sdf)
  rsq <- signif(summary(fit)$r.squared, 3)

  par(mfrow = c(2, 2), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
  multi.p(df, sdf)  # full timeseries with lmfit
  sub.p(sdf, rsq = signif(rsq, 3)) # subset timeseries
  residual.p(fit)  # residual plot
  qq.p(fit)  # qqplot
  cat("-----------------------------------------\n")

  return(invisible(x))
  on.exit(par(parorig)) # revert par settings to original
}

#' @export
mean.calc_rate <- function(x, export = FALSE, ...){

  cat("\n# mean.calc_rate # ----------------------\n")
  if(length(x$rate) == 1) message("Only 1 rate found in calc_rate x. Returning mean rate anyway...")
  n <- length(x$rate)
  out <- mean(x$rate)
  cat("Mean of", n, "output rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}

# linear_fit --------------------------------------------------------------

#' Perform a linear regression on a data frame
#'
#' This is an internal function. Performs `lm` on a data frame object and returns
#' its coefficients.
#'
#' @param dt data frame.
#'
#' @keywords internal
#'
#' @return A data frame object of `lm()` coefficients.
#' @export
linear_fit <- function(dt) {
  fit <- lm(dt[[2]] ~ dt[[1]], dt)
  b0   <- coef(fit)[[1]]
  b1   <- coef(fit)[[2]]  # slope
  rsq  <- signif(summary(fit)$r.squared, 3) # r-square
  out  <- data.frame(intercept_b0 = b0, rate_b1 = b1, rsq)
  return(out)
}


# extract_indices ---------------------------------------------------------

#' Extract row, time and DO indices from a subset dataframe
#'
#' This is an internal function. Extracts row, time and DO values from a data
#' subset in a list.
#'
#' @param x data frame.
#' @param subsets list of data frames.
#' @param n numeric. Choose which subset in the list to extract data from.
#'
#' @importFrom data.table data.table
#'
#' @keywords internal
#'
#' @return A `data.table`` object.
#' @export
extract_indices <- function(x, subsets, n) {
  # This grabs the first and last-row data
  fl <- subsets[[n]][, .SD[c(1, .N)]]
  # Add row indices while flattening data into a row:
  out <- data.table::data.table(
    row = match(fl[[1]][1], x[[1]]), endrow = match(fl[[1]][2], x[[1]]),
    time = fl[[1]][1], endtime =  fl[[1]][2],
    oxy =fl[[2]][1], endoxy = fl[[2]][2])
  return(out)
}
