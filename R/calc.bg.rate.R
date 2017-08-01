#' Calculate background respiration
#'
#' @param df Data frame.
#' @param timecol Numeric. Defaults to 1. The column index of the data frame to use as time data.
#' @param bgcol Numeric or a list. Defaults to 2. The column index/indices of the data frame to use for DO data. A list e.g. `c(2:5)` will calculate the rate for each column individually, and then average the results.
#'
#' @return A summary list of rates, and the mean rate.
#' @export
#'
#' @examples
#' data(urchin2013)
#' calc.background(urchin2013, 1, c(18:21))
#'
calc.bg.rate <- function(df, timecol = 1, bgcol = 2) {
  df <- df[, c(timecol, bgcol)]   # subset dataframe
  n <- c(2:length(df)) # create index for subsetting dataframe
  # create individual dataframe subsets:
  bgsubs <- lapply(n, function(x) df[, c(1, x)])
  reps <- c(1:length(bgsubs)) # create another index for calcRate
  calc <- lapply(reps, function (x) calc.rate(bgsubs[[x]], plot = F)$average)
  plots <- lapply(reps, function (x) calc.rate(bgsubs[[x]], plot = F)$plot)
  # extract results:
  results <- as.vector(do.call(rbind, calc))
  average <- mean(results)
  sd <- sd(results)
  bgout <- list(plots = plots, results = results, average = average, sd = sd)
  class(bgout) <- 'calc.bg.rate'
  return(invisible(bgout))
}


#' @export
print.calc.bg.rate <- function(x) {
  results <- x$results
  average <- x$average
  sd <- sd(results)

  cat("Results\n")
  print(results)
  cat("Average, SD\n")
  print(c(average, sd))
}
