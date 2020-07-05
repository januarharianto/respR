#' Truncate a data frame or `inspect()`` object to create a subset of the data
#'
#' This function subsets a data frame or `inspect()` object based on a given set
#' of rules. It can subset data based on ranges of: time, oxygen, row number, or
#' proportion of total oxygen used. For data frames, to subset by 'time', 'o2',
#' or 'proportion', the time data is assumed to be in the first column, and
#' oxygen in the second column. For `inspect()` objects, the data will have been
#' coerced to this structure already. The function can subset **any** data frame
#' by 'row'.
#'
#' For multiple column data frames, such as with time in column 1, and multiple
#' columns of oxygen data, the subset object will include *all* columns. In the
#' case of subsetting `by = "o2"`, subsetting is based on the first column of
#' oxygen data (i.e. column 2), and all subsequent columns are subset between
#' the same rows regardless of oxygen values.
#'
#' This function is ideal for passing only some regions of your data to other
#' functions such as `auto_rate`, either by resaving them as a new object or
#' through the use of pipes (`%>%`). It is also ideal for use with
#' intermittent-flow data in loops, where each replicate can be extracted and
#' passed to an analytical function such as `calc_rate` or `auto_rate`. See
#' examples and vignettes.
#'
#' @param x data frame or `inspect` object. The data to subset.
#' @param from numeric. Defines the lower bound(s) of the data to subset.
#'   Subsetting is based on the argument: `by`.
#' @param to numeric. Defines the upper bound(s) of the data to subset.
#'   Subsetting is based on the argument: `by`.
#' @param by string. "time", "row", "o2" or "proportion".
#'
#' @return A `data.table` or `inspect` object.
#' @export
#'
#' @examples
#' # Subset by time:
#' data("squid.rd")
#' x <- subset_data(squid.rd, from = 2000, to = 4000, by = "time")
#' plot(x)
#'
#' data("flowthrough.rd")
#' subset_data(flowthrough.rd, from = 50, to = 600, by = "time")
#'
#' # Subset by O_2:
#' data("sardine.rd")
#' subset_data(sardine.rd, from = 94, to = 91, by = "o2")
#'
#' # Subset by proportion of total oxygen used:
#' data("sardine.rd")
#' subset_data(sardine.rd, from = 0.8, to = 0.4, by = "proportion")
#'
#' # Subset by row:
#' data("flowthrough.rd")
#' subset_data(flowthrough.rd, from = 10, to = 750, by = "row")
#'
#' # Pass (via piping) only part of a dataset to auto_rate
#' data("sardine.rd")
#' subset_data(sardine.rd, from = 94, to = 91, by = "o2") %>%
#' auto_rate()

subset_data <- function(x, from, to, by = "time") {

  ## verify by input
  by <- verify_by(by)

  # Check if object is from respR function(s)
  if (any(class(x) %in% "inspect_data")) {
    dt <- data.table(x$dataframe)
  } else if (any(class(x) %in% "inspect")) {
    dt <- data.table(x$dataframe)
  } else dt <- data.table(x)


  # Subset based on rule "by"
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  } else if (by == "row") {
    out <- dt[from:to]
  } else if (by == "o2") {
    top <- Position(function(z)
      z <= from, dt[[2]])
    bot <- Position(function(z)
      z <= to, dt[[2]])
    out <- dt[top:bot]
  } else if (by == "proportion") {
    mx <- max(dt[[2]])
    mn <- min(dt[[2]])
    top <-
      Position(function(z)
        z <= (from * (mx - mn) + mn), dt[[2]])
    bot <- Position(function(z)
      z <= (to * (mx - mn) + mn), dt[[2]])
    out <- dt[top:bot]
  }
  cat("\n# subset_data # -------------------------\n")
  cat("Original data:\n")
  print(dt, topn = 2)
  cat("\nNew data:\n")
  print(data.table(out), topn = 2)
  cat("-----------------------------------------\n")

  if (any(class(x) %in% "inspect")) {
    x$dataframe <- out
    return(invisible(x))
  } else if (any(class(x) %in% "inspect_data")) {
    x$dataframe <- out
  } else return(invisible(out))
}
