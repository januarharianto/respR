
#' Calculate rate of change in oxygen over time
#'
#' Calculates rate of oxygen uptake or production from respirometry data. A rate
#' can be determined over the whole dataset, or on subsets of the data using the
#' `from` and `to` arguments to specify data regions in terms of `oxygen` or
#' `time` units, `row` numbers of the input data, or over a `proportion` of the
#' total oxygen used or produced (note, this last option works poorly with noisy
#' or fluctuating data). Multiple rates can be extracted from the same dataset
#' by using these arguments to enter vectors of paired values in the appropriate
#' metric. See Examples.
#'
#' The function calculates rates by fitting a linear model of oxygen against
#' time, with the slope of this regression being the rate. There are no units
#' involved in `calc_rate`. This is a deliberate decision. The units of oxygen
#' concentration and time will be specified later in [`convert_rate()`] when
#' rates are converted to specific output units.
#'
#' For continuous data recordings, it is recommended a `data.frame` containing
#' the data be prepared via [`inspect()`], and entered as the `x` input. For
#' data not prepared like this, `x` can be a 2-column `data.frame` containing
#' numeric values of time (col 1) and oxygen (col 2) concentrations. If multiple
#' columns are found in either `inspect` or data frame inputs, only the first
#' two columns are used.
#'
#' ## Specifying regions
#'
#' For calculating rates over specific regions of the data, the `from` and `to`
#' inputs in the `by` units of `"time"` (the default), "`oxygen`", `"row"`, or
#' `"proportion"` can be used. The `from` and `to` inputs do not need to be
#' precise; the function will use the closest values found.
#'
#' Multiple regions can be examined within the same dataset by entering `from`
#' and `to` as vectors of paired values to specify different regions. In this
#' case, `$rate` in the output will be a vector of multiple rates with each
#' result corresponding to the position of the paired `from` and `to` inputs. If
#' `from` and `to` are `NULL` (the default), the rate is determined over the
#' entire dataset.
#'
#' ## Plot
#'
#' A plot is produced (provided `plot = TRUE`) showing the original data
#' timeseries of oxygen against time, with the region specified via the `from`
#' and `to` inputs highlighted, a close-up of this region with linear model
#' coefficients, and summary plots of fit and residuals. If multiple rates have
#' been calculated, by default the first is plotted. Others can be plotted by
#' changing the `pos` argument either in the main function call, or by plotting
#' the output, e.g. `plot(object, pos = 2)`. Console output messages can be
#' suppressed using `message = FALSE`.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints a single result, by default the first rate. Others can be
#' printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all results and metadata, or those
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The output can be
#' saved as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all rates, or those specified by the `pos`
#' input. e.g. `mean(x, pos = 1:5)` The output can be saved as a separate value
#' by passing `export = TRUE`.
#'
#' @param x object of class `inspect` or `data.frame`. This is the timeseries of
#'   paired values of oxygen against time from which to calculate rates.
#' @param from numeric value or vector. Defaults to `NULL`. The start of the
#'   region(s) over which you want to calculate the rate in the units specified
#'   in `by`. If a vector, each value must have a paired value in `to`.
#' @param to numeric value or vector. Defaults to `NULL`. The end of the
#'   region(s) over which you want to calculate the rate in the units specified
#'   in `by`. If a vector, each value must have a paired value in `from`.
#' @param by string. `"time"`, `"row"`, `"o2"` or `"proportion"` Defaults to
#'   `"time"`.This is the method used to subset the data region between `from`
#'   and `to`.
#' @param plot logical. Defaults to `TRUE`. Plot the results.
#' @param ... Allows additional plotting controls to be passed, such as `pos`
#'   and `message = FALSE`.
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

calc_rate <- function(x, from = NULL, to = NULL, by = "time", plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()

  # Validate inputs
  # Will migrate to assertive package when I get used to it..
  ## verify by input
  by <- respR:::verify_by(by, msg = "calc_rate:")

  # Extract data.frame from inspect functions
  if(any(class(x) %in% "inspect")) x <- x$dataframe

  # By now, x input must be a data frame object
  if(!is.data.frame(x)) stop("calc_rate: Input must be a 'data.frame' or 'inspect' object.")

  # Format as data.table
  x <- data.table::data.table(x)
  if (length(x) > 2) {
    warning("calc_rate: Multiple columns of O2 data found in input.\n  Rate(s) will be calculated from first column only!\n  To extract rates from other columns, use 'inspect()' to save them as separate objects.")
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


  # Subset and run lm -------------------------------------------------------
  # Subset the data:
  dt <- lapply(1:length(from), function(z) respR:::truncate_data(x, from[z], to[z], by))

  # Perform lm on data and extract coefficients
  coefs <- lapply(1:length(to), function(z) respR:::linear_fit(dt[[z]]))

  # Extract row, time and DO indices from subsets
  indices <- lapply(1:length(dt), function(z) extract_indices(x, dt, z))

  # Extract row, time and DO indices from subsets and add to results
  rdt <- data.table::rbindlist(lapply(1:length(to), function(x)
    cbind(coefs[[x]], indices[[x]])))

  # Include twopoint method in table
  # And `rate` at end on its own in case people use 2pt by mistake
  rdt[, rate.2pt := ((endoxy - oxy) / (endtime - time))]
  rdt[, rate := rate_b1]

  # Extract rate_b1
  rate <- rdt[,rate_b1]


  # Generate output ---------------------------------------------------------
  out <- list(
    call = call,
    dataframe = x,
    subsets = dt,
    summary = cbind(rank = 1:nrow(rdt), rdt),
    from = from,
    to = to,
    by = by,
    rate.2pt = rdt$rate.2pt,
    rate = rate
  )

  class(out) <- "calc_rate"

  # Plot if TRUE
  if (plot) plot(out, ...)

  return(out)
}


#' @export
print.calc_rate <- function(x, pos = 1, ...) {
  cat("\n# print.calc_rate # ---------------------")
  if(length(pos) > 1)
    stop("print.calc_rate: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate))
    stop("print.calc_rate: Invalid 'pos' rank: only ", length(x$rate), " rates found.")
  cat("\nRank", pos, "of", length(x$rate), "rates:")
  cat("\nRate:", x$rate[pos], "\n")
  cat("\n")
  if(length(x$rate) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")

  return(invisible(x))
}

#' @export
#' @importFrom data.table data.table
summary.calc_rate <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate)))
    stop("summary.calc_rate: Invalid 'pos' rank: only ", length(object$rate), " rates found.")

  cat("\n# summary.calc_rate # -------------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all rate results:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of rate results from entered 'pos' rank(s):")
    cat("\n")
    cat("\n")
  }

  out <- object$summary[pos,]
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
plot.calc_rate <- function(x, pos = 1, message = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  nres <- length(x$rate) # number of rates

  if(is.null(pos)) pos <- 1
  if(length(pos) > 1)
    stop("calc_rate: 'pos' should be a single value.")
  if(pos > nres || pos < 1)
    stop("calc_rate: Invalid 'pos' rank: only ", nres, " rates found.")

  if(message) {
    cat("\n# plot.calc_rate # ----------------------\n")
    if(pos == 1 && nres == 1)
      cat(glue::glue("calc_rate: Plotting rate from position {pos} of {nres} ..."), sep="\n")
    if(pos == 1 && nres > 1)
      cat(glue::glue("calc_rate: Plotting rate from position {pos} of {nres} ... \nTo plot others use 'pos'"), sep="\n")
    if(pos > 1)
      cat(glue::glue('calc_rate: Plotting rate from position {pos} of {nres} ...'), sep="\n")
  }

  df  <- x$dataframe
  sdf <- x$subsets[[pos]]
  fit <- lm(sdf[[2]] ~ sdf[[1]], sdf)
  rsq <- signif(summary(fit)$r.squared, 3)

  par(mfrow = c(2, 2),
      oma = c(0.4, 0.4, 1.5, 0.2),
      mai = c(0.3, 0.3, 0.2, 0.2),
      ps = 10,
      cex = 1,
      cex.main = 1)

  respR:::multi.p(df, sdf)  # full timeseries with lmfit
  respR:::sub.p(sdf, rsq = signif(rsq, 3)) # subset timeseries
  respR:::residual.p(fit)  # residual plot
  respR:::qq.p(fit)  # qqplot
  mtext(glue::glue("calc.rate: Rank {pos} of {nres} Total Rates"),
        outer = TRUE, cex = 1.2, line = 0.3, font = 2)

  if(message) cat("-----------------------------------------\n")

  return(invisible(x))
}

#' @export
mean.calc_rate <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.calc_rate # ----------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate)))
    stop("mean.calc_rate: Invalid 'pos' rank: only ", length(x$rate), " rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate)
    cat("Mean of all rate results:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate[pos])
  out <- mean(x$rate[pos])
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
