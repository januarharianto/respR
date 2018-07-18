
#' Calculate rate of change in oxygen over time
#'
#' `calc_rate` calculates the rate of change in oxygen concentration over time
#' in a data frame. You can perform single or multiple regressions on subsets of
#' the data frame by calling the `from` and `to` arguments.
#'
#' There are no units involved in `calc_rate`. This is a deliberate decision.
#' Units are called in a later function when volumetric and/or mass-specific
#' rates of oxygen use are computed in [convert_rate()] and [convert_DO()].
#'
#' @param x data frame or object of class `adjust_rate`. This is the data to
#'   process.
#' @param from numeric vector. Defaults to NULL. Defines the lower bound(s) of
#'   the data frame to subset. Subsetting is based on the argument: `by`.
#' @param to numeric vector. Defaults to NULL. Defines the upper bound(s) of the
#'   data frame to subset. Subsetting is based on the argument: `by`.
#' @param by string. `"time"`, `"row"`, `"o2"` or `"proportion"` Defaults to
#'   `"time"`.This is the method used to subset the data.
#' @param plot logical. Defaults to TRUE. Plot the results.
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
#' calc_rate(sardine.rd, from = 200, to = 1800)     # default subset by 'time'
#' calc_rate(sardine.rd, 93, 92, by = 'o2')         # subset by O2
#' calc_rate(sardine.rd, 200, 1800, by = 'row')     # subset by row
#' x <- calc_rate(sardine.rd, .8, .2, by = 'proportion') # subset by proportion
#' x
#' summary(x)
#' plot(x)
#'
#' # Using a vector in 'from' and 'to' perform multiple measurements:
#' data(intermittent.rd)
#' calc_rate(intermittent.rd, c(200,2300,4100), c(1800,3200,4600), by = 'time')
calc_rate <- function(x, from = NULL, to = NULL, by = "time", plot = TRUE) {

  # Validate inputs
  # Will migrate to assertive package when I get used to it..
  # Ensure "from" and "to" are same length:
  if (length(from) != length(to)) stop("'from' and 'to' have unequal lengths.")

  # Extract data.frame from inspect functions
  if(any(class(x) %in% "inspect_data")) x <- x$df # this will be removed later
  if(any(class(x) %in% "inspect")) x <- x$dataframe


  # By now, x input must be a data frame object
  if(!is.data.frame(x)) stop("Input must be a data.frame object.")

  # Format as data.table
  x <- data.table::data.table(x)
  x <- x[,1:2] # if data is > 2 columns, pick the first 2

  # If 'from' and 'to' are NULL, we assume that the user is analysing all data
  if (all(sapply(list(from, to), is.null))) {
    from <- 1; to <- nrow(x); by <- "row"
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

  # Plot if TRUE
  if (plot) {
    multi.p(x, dt, title = FALSE)
  }

  # Generate output
  out <- list(
    data = x,
    from = from, to = to, by = by,
    subsets = dt,
    summary = rdt,
    rate = rate,
    rate_2pt = rdt$rate_twopoint
    )

  class(out) <- "calc_rate"
  return(out)
}


#' @export
print.calc_rate <- function(x, ...) {
  cat("\n# calc_rate # -------------------\n")
  cat("Rate(s):\n")
  print(x$rate)
  # cat("-------------------------------\n")
  return(invisible(x))
}


#' @export
summary.calc_rate <- function(object, export = FALSE, ...) {
  cat("Summary:\n")
  print(object$summary)
  if (export) {
    return(invisible(object$summary))
  } else return(invisible(object))
}

#' @export
plot.calc_rate <- function(x, rep = 1, ...) {
  cat("\n# plot # ------------------------\n")
  cat('Plotting...this may take a while for large datasets.\n')
  df  <- x$data
  sdf <- x$subsets[[rep]]
  fit <- lm(sdf[[2]] ~ sdf[[1]], sdf)
  rsq <- signif(summary(fit)$r.squared, 3)

  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow = c(2, 2), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
  multi.p(df, sdf)  # full timeseries with lmfit
  sub.p(sdf, rsq = signif(rsq, 3)) # subset timeseries
  residual.p(fit)  # residual plot
  qq.p(fit)  # qqplot
  par(pardefault)  # revert par settings to original
  cat("Done.\n")
  # cat("-------------------------------\n")
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
