#' Subsample a data frame object
#'
#' A simple function that selects every "`n`" rows in a data frame to create a new data frame object. Effectifely "thins" large datasets.
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#' @param df data frame.
#' @param n numeric.
#' @param random_start logical.
#'
#' @return A data frame object.
#' @export
#'
#' @examples
#' # Subsample every 10 rows:
#' subsample(squid, n = 10)
#'
#' # Subsample with random first value:
#' subsample(sardine, 3, T)
#'
subsample <- function(df, n = 5, random_start = F) {
  # First check if [random_start] is true. If true, the
  # function will randomise the value taken from row 1:[n] and
  # insert it into the [from] argument in [seq].
  if (random_start == T) {
    start <- sample(1:n, 1)

    # If set to false, the function will sample the first row for
    # the [from] argument in [seq].
  } else if (random_start == F) {
    start <- 1
  }
  # Sample every [n] rows from dataframe, starting from [rs],
  # until end.  The function [seq.int] is used for increased
  # performance.
  subset <- df[seq.int(start, nrow(df), n), ]
  # Create the plots
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow=c(1,2))  # replace par settings
  plot(df, xlab = "Time", ylab = "Oxygen", col = r1, pch = 16,
    panel.first = c(rect(par("usr")[1],
      par("usr")[3],
      par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  title(main = "Full Timeseries", line = 0.5)
  plot(subset, xlab = "Time", ylab = "Oxygen", col = r1, pch = 16,
    panel.first = c(rect(par("usr")[1],
      par("usr")[3],
      par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  title(main = "Subsample Timeseries", line = 0.5)
  par(pardefault)  # revert par settings to original
  return(invisible(subset))
}
