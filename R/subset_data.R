#' Truncate a data frame, `inspect()`, or `inspect.ft()` object to create a
#' subset of the data
#'
#' `subset_data` subsets a data frame, `inspect()`, or `inspect.ft()` object
#' based on a given set of criteria. The function is ideal for passing only some
#' regions of your data to other functions such as `[auto_rate()]`, either by
#' saving them as a new object or through the use of pipes (`%>%`). It is also
#' ideal for use with intermittent-flow data in loops, where each replicate can
#' be extracted and passed to an analytical function such as `calc_rate` or
#' `auto_rate`. See examples and vignettes.
#'
#' The function can subset data based on ranges of: time, oxygen, row number, or
#' proportion of total oxygen used or produced (note, this last option works
#' poorly with noisy or fluctuating data). For data frames, to subset by `time`,
#' `o2`, or `proportion`, the time data is assumed to be in the first column,
#' and oxygen data in the second column. For `inspect()` and `inspect.ft()`
#' objects, the data will have been coerced to this structure already. In these
#' cases the `$dataframe` element in the output is replaced by the subset, and
#' in `inspect.ft()` the `$inputs` element is similarly subset and replaced.
#' Note for `inspect.ft()` objects, the oxygen data in column 2 will be either
#' `out.o2` data or `delta.o2` data depending on what was inspected. The
#' function can subset **any** data frame by `row`.
#'
#' @details When multiple columns are present, for example time in column 1, and
#'   multiple columns of oxygen data, the subset object will include *all*
#'   columns. In the case of subsetting `by = "o2"` or `by = "proportion"`,
#'   subsetting is based on the first column of oxygen data (i.e. column 2), and
#'   all subsequent columns are subset between the same rows regardless of
#'   oxygen values.
#'
#'   For `"time"` and `"row"` subsetting, `from` and `to` should be in correct
#'   order. No warning or messages are given if the values are beyond those in
#'   the dataframe. For instance, if `to = 100` and there are only 50 rows in
#'   the data, the last row (50) will be used instead. The same for `from` and
#'   `to` time values outside those in the data frame. For `"time"` subsetting,
#'   the values do not need to be precise; the function will use the closest
#'   values found.
#'
#'   For `"o2"` or `"proportion"` subsetting, `from` and `to` are generally
#'   interchangeable, and the function will subset data *between* the first and
#'   last occurrences (or closest occurrences) of these values. It works best
#'   with generally increasing or decreasing oxygen data, but results may vary
#'   with other data such as intermittent flow data or those in `inspect.ft`
#'   objects.
#'
#'   Note: for `inspect` and `inspect.ft` objects after subsetting, the
#'   locations of any data issues highlighted when the object is run through
#'   `print()`, will no longer be accurate. If these are inportant, best
#'   practice is to subset the original dataframe, and process the subset
#'   through `inspect` or `inspect.ft`.
#'
#' @param x data frame, `inspect`, or `inspect.ft()` object. The data to subset.
#' @param from numeric. Defines the lower bound(s) of the data to subset.
#'   Subsetting is based on the argument: `by`.
#' @param to numeric. Defines the upper bound(s) of the data to subset.
#'   Subsetting is based on the argument: `by`.
#' @param by string. "time", "row", "o2" or "proportion".
#'
#' @return If the input is a `data.frame`, the output is a `data.table` of the
#'   subset, otherwise it is an `inspect`, or `inspect.ft()` object depending on
#'   input.
#'
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

  # Check if object is from respR function(s)
  if (any(class(x) %in% "inspect")) {
    dt <- data.table(x$dataframe)
  } else if (any(class(x) %in% "inspect.ft")) {
    dt <- data.table(x$dataframe)
  } else dt <- data.table(x)

  ## verify by input
  by <- respR:::verify_by(by, msg = "subset_data:")

  ## if time, 'from' required, single val, not bigger than max value in time
  if(by == "time") {
    input.val(from,  num = TRUE, int = FALSE, req = TRUE,
              max = 1, min = 1, range = c(0, max(dt[[1]])),
              msg = "subset_data: 'from' -")
    ## if time, 'to' required, single val, greater than from
    input.val(to,  num = TRUE, int = FALSE, req = TRUE,
              max = 1, min = 1, range = c(from, Inf),
              msg = "subset_data: 'to' -")}

  ## if row, 'from' required, single val, integer
  if(by == "row") {
    input.val(from,  num = TRUE, int = TRUE, req = TRUE,
              max = 1, min = 1, range = c(1, nrow(dt)),
              msg = "subset_data: 'from' -")
    ## if row, 'to' required, single val, integer, greater than from
    input.val(to,  num = TRUE, int = TRUE, req = TRUE,
              max = 1, min = 1, range = c(from+1, Inf),
              msg = "subset_data: 'to' -")}

  ## if o2 or prop, 'from' & 'to' required, single val, numeric
  if(by == "o2" || by == "proportion") {
    input.val(from,  num = TRUE, int = FALSE, req = TRUE,
              max = 1, min = 1, range = c(-Inf, Inf),
              msg = "subset_data: 'from' -")
    input.val(to,  num = TRUE, int = FALSE, req = TRUE,
              max = 1, min = 1, range = c(-Inf, Inf),
              msg = "subset_data: 'to' -")}

  # Subset based on rule "by"
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  } else if (by == "row") {
    out <- dt[from:to]
  } else if (by == "o2") {
    out <- truncate_data(dt, from, to, by)
  } else if (by == "proportion") {
    out <- truncate_data(dt, from, to, by)
  }

  # inspect.ft has an additional element that needs subset - $input_data
  if (any(class(x) %in% "inspect.ft")) {

    # get indices of start and end of subset
    # Must be a better way of doing this...

    # collapse list of lists to list of dfs
    dt2 <- sapply(x$input_data, function(z) rbind.data.frame(z))
    # collapse list of dfs to dt
    dt2 <- as.data.table(dt2)
    # for some stupid reason the names get changed
    names(dt2) <- names(x$dataframe)

    # start and end of rows which are identical
    start <- min(which(duplicated(rbind(dt2, out), fromLast = TRUE)))
    end <- max(which(duplicated(rbind(dt2, out), fromLast = TRUE)))

    # subset x$input_data elements
    input_data_sub <- lapply(x$input_data, function(y) {
      lapply(y, function(z){
        z[start:end]
      })
    })

    x$input_data <- input_data_sub
  }

  cat("\n# subset_data # -------------------------\n")
  cat("Original data:\n")
  print(dt, topn = 2)
  cat("\nSubset data:\n")
  print(data.table(out), topn = 2)
  cat("-----------------------------------------\n")

  if (any(class(x) %in% "inspect")) {
    x$dataframe <- out
    return(invisible(x))
  } else if (any(class(x) %in% "inspect.ft")) {
    x$dataframe <- out
    return(invisible(x))
  } else return(invisible(out))
}

