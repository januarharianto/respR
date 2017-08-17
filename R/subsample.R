#' @import ggplot2
#'
#' @export
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
  # Create the plot and print table if plot = T
  p1 <- ggplot() +
    geom_point(data = df, aes(df[[1]], df[[2]]), shape = 21, size = .5
      , alpha = .4) +
    labs(x='Time', y='DO2') +
    theme_respr()

  p2 <- ggplot() +
    geom_point(data = subset, aes(subset[[1]], subset[[2]]), shape = 21,
      size = .5, alpha = .4) +
    labs(x='Time', y='DO2') +
    theme_respr()
  print(cowplot::plot_grid(p1, p2, ncol = 1))
  # message(sprintf('Data has been subsampled at every %d rows.', n))
  # message('Showing the first 6 rows:')
  # print(head(subset))
  return(invisible(subset))
}
