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

  if(!(is.numeric(x) | (class(x) %in% c("calc_rate", "auto_rate"))))
    stop("adjust_rate: 'x' must be numeric or an object of class 'calc_rate' or 'auto_rate'")

  if (class(by) %in% "calc_rate.bg") by <-  by$bgrate
  if (!is.numeric(by))
    stop("adjust_rate: 'by' must be numeric or object of class 'calc_rate.bg'.")

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
  adjusted.rate <- unname(unlist(rate - by))

  # Append the results to the object
  out <- c(input = x, list(adjustment = by, adjusted.rate = adjusted.rate))
  class(out) <- "adjust_rate"
  message("\nadjust_rate: Rate adjustments applied. Use print() command for more info.")
  return(out)
}




#' @export
print.adjust_rate <- function(object, pos = 1, ...) {
  cat("\n# print.adjust_rate # -------------------\n")
  cat("Note: please consider the sign of the correction value when adjusting the rate.\n")
  if(pos > length(object$adjusted.rate)) stop("Invalid 'pos' rank: only ", length(object$adjusted.rate), " adjusted rates found.")
  cat("\nRank", pos, "of", length(object$adjusted.rate), "adjusted rate(s) shown. To see all results use summary().")
  if (length(object) == 3) {
    cat("\nInput rate:", object$input[pos])
  } else cat("\nInput rate:", object$input.rate[pos])
  cat("\nAdjustment:", object$adjustment)
  cat("\nAdj. rate:", object$adjusted.rate[pos], "\n")
  cat("-----------------------------------------\n")

  return(invisible(object))
}

#' @export
summary.adjust_rate <- function(object, export = FALSE, ...) {
  cat("\n# summary.adjust_rate # -----------------\n")
  if (length(object) == 3) {
    rate <- object$input
  } else rate <- object$input.rate
  out <- data.table(rate, adjustment = object$adjustment, "adjusted rate" = object$adjusted.rate)
  print(out)

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.adjust_rate <- function(object, export = FALSE, ...){

  cat("\n# mean.adjust_rate # --------------------\n")
  if(length(object$adjusted.rate) == 1) warning("Only 1 rate found in adjust_rate object. Returning mean rate anyway...")
  n <- length(object$adjusted.rate)
  out <- mean(object$adjusted.rate)
  cat("Mean of", n, "adjusted rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}
