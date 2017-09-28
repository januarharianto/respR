#' Check data frame for structural errors
#'
#' `check.input` scans a data frame for specific errors that may affect
#'   the use of functions in `respr`. Data checks include:
#'   * A test for NA inputs.
#'   * A test for NaN inputs.
#'   * A test for numeric data.
#'   * A test for duplicates (relevant for time data only).
#'   * A test for evenly-spaced data (relevant for time data only).
#'   * A check for column lengths.
#'
#' You may run `check.input` on a multi-column data frame. Alternatively, you
#' may run `check.input` on a 2-column data frame and it will automatically
#' assume that the first column is time data and the second column, oxygen
#' concentration data. A 2-column data frame can also be extracted directly from
#' a multi-column data frame if the `x` and `y` arguments are used within the
#' function. For 2-column data frames, `check.input` will automatically remove
#' NAs and produce a new data frame object, as long as no critical errors are
#' found in the data. If critical errors are found (e.g. non numeric data), the
#' function will stop. Two-column checks are more informative - since it is
#' assumed that the x column is time data and y column is dissolved oxygen data,
#' time-specific errors can be called out.
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#' @param df data frame. containing at least 2 columns of data.
#' @param x numeric. Corresponds to column number. `x = 1` will subset the first
#'   column of the data frame. Must be time data.
#' @param y numeric. Corresponds to column number. `y = 2` will subset the
#'   second column of the data frame. Must be dissolved oxygen data.
#' @param plot logical. When TRUE, a plot of the data is automatically produced
#'   if the data frame subset contains 2 columns. Otherwise, this does nothing.
#'
#' @return If a 2-column data frame is checked, `check.input` returns a data
#'   frame object that can be saved for use in [calc.rate()] and [auto.rate()].
#'
#' @export
#'
#' @examples
#' check.input(urchin2013)  # this does not produce a data frame object
#' check.input(urchin2013, x = 1, y = 15)  # this produces a data frame object
#' check.input(sardine)  # this produces a data frame object
#'
check.input <- function(df, x = NULL, y = NULL, plot = T) {
  df <- tibble::as_tibble(df)
  # if no columns are selected
  if (is.null(x) && is.null(y)) {
    df <- df
  } else if (is.numeric(x) && is.numeric(y)) {
    # otherwise, select columns
    x <- df[,x]
    y <- df[,y]
    df <- data.frame(x, y)
  } else stop("The arguments 'x' and/or 'y' must be numeric.")

  print(ls.str(df))

  # perform checks
  # check that columns are of equal lengths
  summary.lens <- sapply(1:length(df), function(x) length(df[[x]]))
  test.lens <- equal.lengths(summary.lens)

  # check that all columns are numeric or integer
  test.class <- sapply(df, class)
  summary.class <- sapply(test.class, function (x) x == 'numeric' | x == 'integer')
  if (any(summary.class == F, na.rm = T)) {
    # if not, terminate the function since more errors will pop up
    message("Non-numeric data detected. Are there typos in your dataset? Check:")
    print(test.class)
    # show.notanumber <- lapply(1:length(df), function(x) which(df[x] != 'numeric' && df[x] != 'integer'))
    stop("Function terminated since non-numeric data cannot be processed. ",call. = F)
  }

  # check for NA
  test.na.absent <- !any(is.na(df))
  summary.na <- !sapply(1:length(df), function(x) anyNA(df[[x]]))
  show.na <- lapply(1:length(df), function(x) which(is.na(df[x])))

  # check for duplicates
  summary.dupes <- sapply(1:length(df), function(x) !any(duplicated(df[[x]])))
  test.dupes.absent <- !any(summary.dupes == FALSE)
  show.duplicated <- lapply(1:length(df), function(x) which(duplicated(df[x]) == T))

  # check for evenly-spaced data
  summary.espace <- sapply(1:length(df), function(x) equal.lengths(diff(df[[x]])))
  test.space <- any(summary.espace == T)

  # summarise the results
  results <- noquote(rbind(as.character(summary.lens), summary.class, summary.na, summary.dupes, summary.espace))
  rownames(results) <- c('Length of data',
    'Data are numeric',
    'NA/NaN',
    'Duplicates',
    'Evenly-spaced')
  results[results == 'TRUE'] <- ' '
  results[results == 'FALSE'] <- 'X'
  print(results)

  # this activates when a 2-column data is selected, or if x and y are NOT NULL
  # the function will process the data frame i.e. convert numeric, remove NAs
  if (length(df) == 2 | !is.null(x) | !is.null(y)) {
    if (test.lens == F) stop("Data columns are not of equal length.\nDataframe NOT generated.", call. = F)

    if(test.class[1] == 'integer' | test.class[2] == 'integer') {
      df[1] <- as.numeric(df[[1]])
      df[2] <- as.numeric(df[[2]])
      test.class <- sapply(df, class)
      message("Note: Integer columns have been converted to numeric.")
    } #

    if (test.class[1] != 'numeric' && test.class[2] != 'numeric') {
      stop("Data must be numeric.\nDataframe NOT generated.", call. = F)
    } #

    if (test.na.absent == F) {
      df <- na.omit(df)
      message("Warning: NA values detected. Rows containing NA values have been automatically removed (see below).")
      print(show.na)
    } #

    if (summary.dupes[1] == F) {
      message("Duplicate time data found at rows:")
      print(show.duplicated[[1]])
      stop("Time data must not contain duplicates.\nDataframe NOT generated.", call. = F)
    }
    if (summary.espace[1] == F) message("Warning: Time data is unevenly spaced. If using `calc.rate`, please avoid subsetting\nby = 'row' unless you understand the range of data you are selecting using this method.")
    message("New dataframe generated.")

    # plot
    if (plot == T && length(df) == 2) {
      # p1 <- ggplot(df, aes(df[[1]], df[[2]])) +
      #   geom_point(size = 2, alpha = .6) +
      #   labs(x = 'Time', y = 'DO') +
      #   theme_respr()
      # print(p1)
      pardefault <- par(no.readonly = T)  # save original par settings
      plot(df, xlab = "Time", ylab = "DO", col = r1, pch = 16,
        panel.first = c(rect(par("usr")[1], par("usr")[3],
          par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
            lty = 1, lwd = 1.5)))
      par(pardefault)  # revert par settings to original
    } else if (plot == T && length(df) > 2)
      message("Data frame contains more than 2 columns - plot not generated")
    return(invisible(df))
  }
}




# ==============================================================================
# internal functions

# check equal lenght columns
equal.lengths <- function(x) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2]))
}
