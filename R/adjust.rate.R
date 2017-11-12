#' Title
#'
#' @param x
#' @param by
#'
#' @return
#' @export
#'
#' @examples
adjust.rate <- function(x, by) {

  if (class(by) %in% "calc.rate.bg") by = by$bgrate
  if (!is.numeric(by))
    stop("'by' must be numeric or object of class 'calc.rate.bg'.")

  # Use mean value of bgrate for correction
  by <- mean(by)

  # Perform correction
  if (class(x) %in% c("calc.rate", "auto.rate")) {
    rate <- x$rate
  } else rate <- x
    corrected <- unname(unlist(rate + by))

  # Append the results to the object
  out <- c(x, adjusted = list(corrected))
  class(out) <- "adjust.rate"
  return(out)
}
