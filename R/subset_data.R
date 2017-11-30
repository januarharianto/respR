#' Truncate a data frame to create a subset of the data
#'
#' This function extracts a subset data frame based on a given 
#' set of rules. Can subset data based on ranges of: time, oxygen, 
#' row number, or proportion. Function assumes time is in the first 
#' column, and oxygen in the second column. 
#'
#' @param x data frame. The data to subset.
#' @param from numeric. Defines the lower bound(s) of the data frame to subset.
#'   Subsetting is based on the argument: `by`.
#' @param to numeric. Defines the upper bound(s) of the data frame to subset.
#'   Subsetting is based on the argument: `by`.
#' @param by string. "time", "row", "o2" or "proportion".
#'
#' @return A `data.table` object.
#' @export
subset_data <- function(x, from, to, by) {
  dt <- data.table::as.data.table(x)
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  }
  if (by == "row") {
    out <- dt[from:to]
  }
  if (by == "o2" & length(x) == 2) {
    top <- Position(function(z) z <= from, dt[[2]])
    bot <- Position(function(z) z <= to, dt[[2]])
    out <- dt[top:bot]
  }
  if (by == "proportion") {
    mx <- max(dt[[2]])
    mn <- min(dt[[2]])
    top <- Position(function(z) z <= (from * (mx - mn) + mn), dt[[2]])
    bot <- Position(function(z) z <= (to * (mx - mn) + mn), dt[[2]])
    out <- dt[top:bot]
  }
  return(out)
}
