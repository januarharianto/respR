#' Check for common errors in respirometry data
#'
#' `inspect_data()` scans a data frame for specific errors that may affect the
#' use of functions in `respR`. Data checks include: * A test for NA/NaN inputs.
#' * A test for numeric data. * A test for sequential time data. * A test for
#' duplicate time data. * A test for evenly-spaced time data.
#'
#'
#' Once data checks are complete, the function produces a list object which may
#' be directly loaded into [calc_rate()], [calc_rate.bg()], [auto_rate()] and
#' [pcrit()] for further analyses.
#'
#' @md
#' @param df data frame. Accepts data frame object of any size.
#' @param xcol numeric. Defaults to `1`. This is the time data.
#' @param ycol numaric. Defaults to `2`. This is the dissolved oxygen data.
#' @param highlight logical. Defaults to TRUE. Prints location (row #) of errors
#'   detected by the function.
#' @param plot logical. Defaults to TRUE. Produces plots for quick visual
#'   diagnostics.
#'
#' @return A list object of class `adjust_rate`.
#'
#' @import utils
#' @import stats
#' @import graphics
#' @export
#'
#' @examples
#' inspect_data(sardine.rd)
#' inspect_data(urchins.rd, 1, 5, highlight = FALSE)
#'
#' # It is also possible to load the function directly into respR's
#' # other functions:
#' calc_rate(inspect_data(sardine.rd, highlight = FALSE, plot = FALSE),
#'           from = 3000, to = 4000, by = "time")
inspect_data <- function(df, xcol = 1, ycol = 2, highlight = TRUE,
  plot = TRUE) {
  # Validate inputs
  if (!is.data.frame(df))
    stop("Test FAILED. `df` input must be a data frame object.")
  if (!is.numeric(xcol) | !is.numeric(ycol))
    stop("Test FAILED.'xcol' and 'ycol' inputs must be numeric.")

  # Format data (just in case)
  df <- data.frame(df)
  df <- df[c(xcol, ycol)]
  x <- df[, 1]
  y <- df[, 2]
  df.len <- nrow(df)

  # PERFORM CHECKS Rule: if logical result is TRUE, the test is
  # a FAIL.

  # Data columns. Are data numeric or a POSIX class?
  column.check <- test.cols(df)
  # Convert time to numeric if integer:
  if (column.check$is.x.integer)
    df <- column.check$x
  if (column.check$test.x)
    stop("Test FAILED. Time column (xcol) must be numeric or POSIX.",
      call. = F)
  if (column.check$test.y)
    stop("Test FAILED. Data column (ycol) must be numeric.",
      call. = F)
  # All data. Are there NA/NaN values?
  na.x <- test_na(x)
  na.y <- test_na(y)
  # Time data. Is time not sequential?
  seq.x <- test_seq(x)
  # Time data. Are there duplicate time values?
  dup.x <- test_dupe(x)
  # Time data. Is time not evenly spaced?
  equal.x <- test_space(x)

  if (highlight) {
    # Print summary
    print(ls.str(df))
    cat("---\n")
    summary <- noquote(rbind(na.x$check, na.y$check, seq.x$check,
      dup.x$check, equal.x$check))
    rownames(summary) <- c("No NA/NaN in Time (xcol)", "No NA/NaN in O2 (ycol)",
      "Sequential Time (xcol)", "Non-duplicated Time",
      "Evenly-spaced Time")
    colnames(summary) <- "Score"
    summary[summary == "TRUE"] <- "FAIL***"
    summary[summary == "FALSE"] <- "PASS"
    print(summary)
    cat("---\n")
  }

  if (na.x$check) {
    warning("Time column (xcol) contains NA or NaN data.",
      call. = F)
    if (highlight)
      cat("NA/NaN location(s) in time (xcol), by row:\n",
        na.x$highlight, "\n")
  }
  if (na.y$check) {
    warning("O2 column (ycol) contains NA or NaN data.",
      call. = F)
    if (highlight)
      cat("NA/NaN location(s) in O2 (ycol), by row:\n",
        na.y$highlight, "\n")
  }
  if (seq.x$check) {
    if (highlight)
      cat("Non-sequential time (xcol) location(s), by row:\n",
        seq.x$highlight, "\n")
    stop("Test FAILED. Time data (xcol) are not sequential.",
      " Please check the order of the data.", call. = F)
  }
  if (dup.x$check) {
    if (highlight)
      cat("Duplicate time data (xcol) location(s), by row:\n",
        dup.x$highlight, "\n")
    stop("Test FAILED. Duplicate time data (xcol) detected.",
      call. = F)
  }
  if (equal.x$check) {
    warning("Time data (xcol) is irregular. Subsetting by `row` may result in irregular durations.",
      call. = F)
    if (highlight)
      if(length(equal.x$highlight) > 50) {
        cat("Unevenly-spaced time (xcol) location(s), by row (first 50):\n",
          head(equal.x$highlight, 50), "\n")
      } else {
      cat("Unevenly-spaced time (xcol) location(s), by row:\n",
        equal.x$highlight, "\n")
      }
  }
  # Finally, if df is to be generated, remove NA
  if (na.x$check | na.y$check) {
    df <- na.omit(df)  # remove NA rows
    warning("Rows containing NA values have been automatically removed.",
      call. = F)
  }
  message("New dataframe generated.")

  ## PLOT
  if (plot) {
    # Calculate rolling regression
    roll <- static_roll(df, floor(0.1 * nrow(df)))$rate_b1

    # # Perform rolling mean
    # rmean <- roll::roll_mean(matrix(df[,2]), floor(0.2 * nrow(df)))
    # rmean <- na.omit(rmean)
    # mr.df <- data.frame(rmean, roll = abs(roll))
    # mr.df <- dplyr::arrange(mr.df, rmean)


    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = c(2, 1), mai = c(0.4, 0.4, 0.3, 0.3), ps = 10,
      cex = 1, cex.main = 1)
    plot(df, xlab = "", ylab = "", col = r1, pch = 16, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3),
      grid(col = "white", lty = 1, lwd = 1.5)))
    title(main = "Full Timeseries", line = 0.3)
    plot(abs(roll), xlab = "", ylab = "", col = r1, pch = 16,
      panel.first = c(rect(par("usr")[1], par("usr")[3],
        par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
    title(main = "Rolling Regression of Rate vs Index (Row No.)",
      line = 0.3)
    # plot(mr.df, col = r2, pch = 16, xlab = "", ylab = "", lwd = 2, panel.first = c(rect(par("usr")[1],
    #   par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white", lty = 1,
    #     lwd = 1.5)))
    par(pardefault)  # revert par settings to original
  }
  out <- list(df = df, check.columns = column.check, check.na.x = na.x,
    check.na.y = na.y, check.sequential = seq.x, check.dupe = dup.x,
    check.uneven.spacing = equal.x)


  class(out) <- "inspect_data"
  return(invisible(out))
}


# These functions are only used for `inspect_data()`, and will not be visible to
# the default user.

# test column classes
test.cols <- function(x) {
  class.xcol <- class(x[, 1])
  class.ycol <- class(x[, 2])
  # first check if time is integer, because we can fix it
  is.x.integer <- any(class(x[, 1]) == "integer")
  if (is.x.integer) {
    warning("Time column (xcol) is of class `integer' and has been converted to `numeric`.",
      call. = F)
    cnames <- names(x)
    convx <- as.numeric(x[, 1])
    x <- data.frame(convx, x[, 2])
    names(x) <- cnames
  }
  # check that xcol is numeric or date/time
  test.x <- !any(class(x[, 1]) %in% c("numeric", "POSIXct", "POSIXt"))
  test.y <- !is.numeric(x[, 2])
  out <- list(x = x, class.xcol = class.xcol, class.ycol = class.ycol,
    is.x.integer = is.x.integer, test.x = test.x, test.y = test.y)
  return(invisible(out))
}

# Test for NA/NaN
test_na <- function(x) {
  test <- is.na(x)
  check <- any(test)
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}

# Test for sequential time
test_seq <- function(x) {
  test <- diff(x) < 0
  test <- ifelse(is.na(test), FALSE, test)  # convert NA values to FALSE
  check <- any(test)
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}


#' Test for duplicate time
#'
#' This is an internal function.
#'
#' @keywords internal
#' @export
test_dupe <- function(x) {
  test <- x %in% unique(x[duplicated(x, incomparables = NA)])
  check <- any(test)
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}

# Calculate mode value in a list
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#' Test for evenly-spaced time
#'
#' This is an internal function.
#'
#' @keywords internal
#' @export
test_space <- function(x) {
  spacing <- diff(as.numeric(x))
  mod <- calc_mode(spacing)

  test <- spacing != mod
  # If spacing is even, there should only be 1 interval
  # detected:
  check <- length(unique(spacing)) > 1

  test <- ifelse(is.na(test), TRUE, test)  # convert NA values to FALSE
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}
