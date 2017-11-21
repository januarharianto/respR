#' Simple rate adjustment function (background or oxygen flux)
#'
#' A simple rate correction function. Please note that the **sign** of the
#' number must be considered in the correction. Respiration is normally a
#' negative number, while oxygen flux may or may not be negative.
#'
#' To account for background respiration and/or oxygen flux in open respirometry
#' experiments, we have provided a simple function to perform either, or both,
#' corrections. As a separate function we may develop this to support more
#' complex correction methods in the future.
#'
#' @md
#' @param df data frame, or any object of class `calc_rate` or `auto_rate`. This
#'   object contains the value that needs to be adjusted.
#' @param by either a numeric, or an object of class `calc_rate.bg`. This is the
#'   value that is used to perform the correction/adjustment.
#'
#' @return A list object of class `adjust_rate`.
#' @export
#'
#' @examples
#' # Note that respiration is negative by default (since it represents a
#' # decrease in dissolved oxygen), so both values are negative!
#' adjust_rate(7.44, -0.04) # this is simply (-7.44) - (-0.04) = 7.40

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
  out <- c(input = df, list(adjustment = by, corrected = corrected))
  class(out) <- "adjust_rate"
  return(out)
}




#' @export
print.adjust_rate <- function(x, ...) {
  cat("Note: please consider the sign of the value while correcting the rate.")
  cat("\nInput rate:", x$input.rate)
  cat("\nAdjustment:", x$adjustment)
  cat("\n Adj. rate:", x$corrected)
}
