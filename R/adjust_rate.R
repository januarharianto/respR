#' Simple rate adjustment function for background correction or open respirometry oxygen flux
#'
#' A simple rate correction function. **Important:** Please note that the sign of the number must be considered in the correction. Respiration is normally a negative number, while oxygen flux may or may not be negative!
#'
#' Users may want to perform rate corrections due to background respiration or to account for oxygen flux in open respirometry experiments. We have provided a simple function to perform either, or both, corrections. This function is open to improvements and enhancements (thus, it is a separate function even though it performs a relatively rudimentary calculation).
#'
#' @md
#' @param df data frame, or any object of class `calc_rate` or `auto_rate`. This object should contain the rate that has not been adjusted, yet.
#' @param by either a numeric, or an object of class `calc_rate.bg`. This is the value to adjust the value with.
#'
#' @return A list object of class `adjust_rate`.
#' @export
#'
#' @examples
#' adjust_rate(7.44, -0.04) # this is simply 7.44 - 0.04 = 7.40
adjust_rate <- function(df, by) {

  if (class(by) %in% "calc_rate.bg") by = by$bgrate
  if (!is.numeric(by))
    stop("'by' must be numeric or object of class 'calc_rate.bg'.")

  # Use mean value of bgrate for correction
  by <- mean(by)

  # Perform correction
  if (class(df) %in% c("calc_rate", "auto_rate")) {
    rate <- df$rate
  } else rate <- df
    corrected <- unname(unlist(rate - by))

  # Append the results to the object
  out <- c(df, list(adjustment = by, corrected = corrected))
  class(out) <- "adjust_rate"
  return(out)
}




#' @export
print.adjust_rate <- function(x, ...) {
  cat("Note: please consider the sign of the value while correcting the rate.")
  cat("\nInput rate:", x$rate)
  cat("\nAdjustment:", x$adjustment)
  cat("\n Adj. rate:", x$corrected)
}
