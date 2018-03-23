#' Truncate a data frame to create a subset of the data
#'
#' This function extracts a subset data frame based on a given set of rules. Can
#' subset data based on ranges of: time, oxygen, row number, or proportion. To
#' subset by 'time' and by 'o2', the function assumes time is in the first
#' column, and oxygen in the second column. To subset by 'proportion', the
#' function calculates proportion data from the oxygen column. **This function
#' can subset any data frame by 'row'.**
#'
#'
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
#'
#' @examples
#' # Subset by time:
#' x <- subset_data(squid.rd, from = 2000, to = 4000, by = "time")
#' plot(x)
#' subset_data(flowthrough.rd, from = 50, to = 600, by = "time")
#'
#' # Subset by O_2:
#' subset_data(sardine.rd, from = 94, to = 91, by = "o2")
#'
#' # Subset by proportion:
#' subset_data(sardine.rd, from = 0.8, to = 0.4, by = "proportion")
#'
#' # Subset by row:
#' subset_data(flowthrough.rd, from = 10, to = 750, by = "row")
subset_data <- function(x, from, to, by) {
  
  # import from other respR functions
  if (any(class(df) %in% "inspect_data")) x <- x$df
  if (any(class(df) %in% "inspect")) x <- x$dataframe
  
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
