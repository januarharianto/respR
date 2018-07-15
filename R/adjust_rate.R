#' Simple rate adjustment function (background or oxygen flux)
#'
#' A simple rate correction function. Please note that the **sign** of the
#' number must be considered in the correction. Background respiration is
#' normally a negative number, while oxygen flux may or may not be negative.
#'
#' To account for background respiration and/or oxygen flux in open respirometry
#' experiments, we have provided a simple function to perform either, or both,
#' corrections. As a separate function we may develop this to support more
#' complex correction methods (e.g. non-linear) in the future.
#'
#' @md
#' @param x numeric. A single value, or any object of class `calc_rate` or `auto_rate`.
#'   This object contains the rate value that needs to be adjusted.
#' @param by either a numeric, or an object of class `calc_rate.bg`. This is the
#'   value that is used to perform the correction/adjustment.
#'
#' @return A list object of class `adjust_rate`.
#' @export
#'
#' @examples
#' # Note that respiration is negative by default (since it represents a
#' # decrease in dissolved oxygen), so both values are negative!
#' # Background respiration correction
#' adjust_rate(-7.44, -0.04) # this is (-7.44) - (-0.04) = -7.40
#' # Oxygen flux correction
#' adjust_rate(-7.44, 0.1) # this is (-7.44) - (0.1) = -7.54

adjust_rate <- function(x, by) {

  if (class(by) %in% "calc_rate.bg") by <-  by$bgrate
  if (!is.numeric(by))
    stop("'by' must be numeric or object of class 'calc_rate.bg'.")

  # TODO check that x is ok, too
  # for piping
  # fix at some point
  # if (class(x) %in% "calc_rate")

  # Use mean value of bgrate for correction
  by <- mean(by)

  # Determine rate input
  if (class(x) %in% c("calc_rate", "auto_rate")) {
    rate <- x$rate
  } else rate <- x

  # Perform correction
  corrected <- unname(unlist(rate - by))

  # Append the results to the object
  out <- c(input = x, list(adjustment = by, corrected = corrected))
  class(out) <- "adjust_rate"
  return(out)
}




#' @export
print.adjust_rate <- function(x, ...) {
  cat("Note: please consider the sign of the value while correcting the rate.")
  if (length(x) == 3) {
    cat("\nInput rate:", x$input)
  } else cat("\nInput rate:", x$input.rate)
  cat("\nAdjustment:", x$adjustment)
  cat("\n Adj. rate:", x$corrected, "\n")
  return(invisible(x))
}


summary.adjust_rate <- function(x) {
  cat("\n# summary.adjust_rate # -----------------\n")
  if (length(x) == 3) {
    rate <- x$input
  } else rate <- x$input.rate
  out <- data.table(rate, adjustment = x$adjustment, "adjusted rate" = x$corrected)
  print(out)
  return(invisible(x))
}