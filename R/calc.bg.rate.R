#' Calculate background rate of change of oxygen over time
#'
#' `calc.bg.rate` uses linear regression to calculate the rate of change of
#' oxygen over time for background corrections of the main data. Can be used on
#' multiple datasets of background measures, as long as the time data are
#' identical between measurements.
#'
#' The output of `calc.bg.rate` can be fed as an argument directly into the
#' functions [calc.rate()] and [auto.rate()].
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#' @param df data frame. The data frame that contains the data needed for the
#'   function `calc.bg.rate`.
#' @param xcol numeric. The time column is specified here. Defaults to "`1`".
#' @param ycol numeric vector. The O2 column(s) are specified here. Defaults to
#'   "`2`". You can put a number, or a vector e.g. `c(2:4)`.
#' @param plot logical. If `TRUE`, will plot the rate(s) in xy plot(s).
#'
#' @return An object of class `calc.bg.rate` containing a list of outputs:
#' \tabular{rll}{
#' \tab `lmfit`   \tab list of objects of class `lm`. There are all the
#'   regressions. \cr
#' \tab `results` \tab data frame. A summary of coefficients from each
#'   regression. \cr
#' \tab `rate`    \tab numeric. The output slope or rate of change in oxygen
#'   concentration. If more than one regression was performed, the average rate
#'   is displayed.
#' }
#'
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' # load data
#' data(urchin2013)
#' bg.u <- calc.bg.rate(urchin2013, xcol = 1, ycol = 18, plot = F)
#' bg.u
#'
#' # run multiple data
#' bg.multi <- calc.bg.rate(urchin2013 ,1, c(18:19))
#' bg.multi
#'
calc.bg.rate <- function(df, xcol = 1, ycol = 2, plot = T) {
  # Extract x:
  x <- df[xcol]
  # Extracy y:
  y <- df[ycol]
  # Perform lm on xy pairs
  fit <- lapply(c(1:length(y)), function(z) lm(y[[z]] ~ x[[1]]))
  # Extract coefficients from all lms
  results <- bind_rows(lapply(c(1:length(y)), function(x)
    data.frame(b0  = coef(fit[[x]])[[1]], # intercept b0
      b1  = coef(fit[[x]])[[2]], # slope b1
      rsq = signif(summary(fit[[x]])$r.squared, 3)))) # r-square
  rate <- mean(results$b1)
  out <- list(lmfit = fit,
    results = results,
    rate = rate)
  # check if plot is needed
  if (plot == TRUE) {
    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = n2mfrow(length(y)),
      oma = c(5,4,0,0) + 0,
      mar = c(0,0,3,1) + 0)  # replace par settings
    lapply(1:length(y), function(z) sub.p(data.frame(x[[1]], y[[z]]), title = F))
    par(pardefault)  # revert par settings to original
  }
  class(out) <- "calc.bg.rate"
  return(out)
}








#' @export
print.calc.bg.rate <- function(x) {
  cat(sprintf("%d regression(s) performed.\n", nrow(x$results)))
  cat("\nSummary coefficients:\n")
  print(x$results)
  if (nrow(x$results > 1)) {
    cat(sprintf("\nAverage Rate: %f\n", x$rate))
  } else cat(sprintf("\nRate: %f\n", x$rate))
}

