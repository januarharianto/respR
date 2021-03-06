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
#' # Subset by proportion:
#' data("sardine.rd")
#' subset_data(sardine.rd, from = 0.8, to = 0.4, by = "proportion")
#'
#' # Subset by row:
#' data("flowthrough.rd")
#' subset_data(flowthrough.rd, from = 10, to = 750, by = "row")
subset_data <- function(x, from, to, by = "time") {
  
  ## verify by input
  by <- verify_by(by)
  
  # Check if object is from respR function(s)
  if (any(class(x) %in% "inspect_data")) {
    dt <- data.table(x$df)
  } else if (any(class(x) %in% "inspect")) {
    dt <- data.table(x$dataframe)
  } else dt <- data.table(x)


  # Subset based on rule "by"
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  } else if (by == "row") {
    out <- dt[from:to]
  } else if (by == "o2" & length(x) == 2) {
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

  if (any(class(x) %in% "inspect")) {
    x$dataframe <- out
    return(invisible(x))
  } else if (any(class(x) %in% "inspect_data")) {
    x$df <- out
  } else return(invisible(out))
}
