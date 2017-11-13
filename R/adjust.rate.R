#' Simple rate adjustment function for background correction or open respirometry oxygen flux
#'
#' A simple rate correction function. **Important:** Please note that the sign of the number must be considered in the correction. Respiration is normally a negative number, while oxygen flux may or may not be negative!
#'
#' @md
#' @param df data frame, or any object of class `c("calc.rate", "auto.rate")`
#' @param by either a numeric, or an object of class `calc.bg.rate``
#'
#' @return A list object of class `adjust.rate`.
#' @export
#'
#' @examples
#' adjust.rate(7.44, -0.04) # this is simply 7.44 - 0.04 = 7.40
#'
#' bg <- calc.rate.bg(urchins.rd, xcol = 1, ycol = 18:19)
#' rt <- calc.rate(inspect.data(urchins.rd, 1, 5))
#' adjust.rate(rt, bg)
adjust.rate <- function(df, by) {

  if (class(by) %in% "calc.rate.bg") by = by$bgrate
  if (!is.numeric(by))
    stop("'by' must be numeric or object of class 'calc.rate.bg'.")

  # Use mean value of bgrate for correction
  by <- mean(by)

  # Perform correction
  if (class(df) %in% c("calc.rate", "auto.rate")) {
    rate <- df$rate
  } else rate <- df
    corrected <- unname(unlist(rate - by))

  # Append the results to the object
  out <- c(df, list(adjustment = by, corrected = corrected))
  class(out) <- "adjust.rate"
  return(out)
}


print.adjust.rate <- function(x) {
  cat("Note: please consider the sign of the value while correcting the rate.")
  cat("\nInput rate:", x$rate)
  cat("\nAdjustment:", x$adjustment)
  cat("\n Adj. rate:", x$corrected)
}
