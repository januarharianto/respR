#' Title
#'
#' @param df
#' @param xcol
#' @param ycol
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
inspect.data <- function(df, xcol = 1, ycol = 2, highlight = TRUE, plot = TRUE) {
  # CHECK INPUTS
  if (!is.data.frame(df)) {
    stop("Test FAILED. `df` input must be a data frame object.")
  }
  if (!is.numeric(xcol) | !is.numeric(ycol)) {
    stop("Test FAILED.'xcol' and 'ycol' inputs must be numeric.")
  }
  # FORMAT DATA (just in case)
  df <- data.frame(df)
  df <- df[c(xcol, ycol)]
  x <- df[, 1]
  y <- df[, 2]
  df.len <- nrow(df)

  # PERFORM CHECKS INPUT CHECKS Rule: if logical result is TRUE, the test is a
  # FAIL. i.e. TRUE = FAIL

  # Data columns. Are data numeric or a POSIX class?
  column.check <- test.cols(df)
  # Convert time to numeric if integer:
  if (column.check$is.x.integer) {
    df <- column.check$x
  }
  if (column.check$test.x) {
    stop("Test FAILED. Time column (xcol) must be numeric or POSIX.", call. = F)
  } else if (column.check$test.y) {
    stop("Test FAILED. Data column (ycol) must be numeric.", call. = F)
  }
  # All data. Are there NA/NaN values?
  na.x <- test.na(x)
  na.y <- test.na(y)
  # Time data. Is time not sequential?
  seq.x <- test.seq(x)
  # Time data. Are there duplicate time values?
  dup.x <- test.dupe(x)
  # Time data. Is time not evenly spaced?
  equal.x <- test.space(x)

  if (highlight) {
    # Print summary
    print(ls.str(df))
    cat("---\n")
    summary <- noquote(rbind(na.x$check, na.y$check, seq.x$check, dup.x$check,
      equal.x$check))
    rownames(summary) <- c("No NA/NaN in Time (xcol)", "No NA/NaN in O2 (ycol)",
      "Sequential Time (xcol)", "Non-duplicated Time", "Evenly-spaced Time")
    colnames(summary) <- "Score"
    summary[summary == "TRUE"] <- "FAIL***"
    summary[summary == "FALSE"] <- "PASS"
    print(summary)
    cat("---\n")
  }

  if (na.x$check) {
    warning("Time column (xcol) contains NA or NaN data.", call. = F)
    if (highlight)
      cat("NA/NaN location(s) in time (xcol), by row:\n", na.x$highlight, "\n")
  }
  if (na.y$check) {
    warning("O2 column (ycol) contains NA or NaN data.", call. = F)
    if (highlight)
      cat("NA/NaN location(s) in O2 (ycol), by row:\n", na.y$highlight, "\n")
  }
  if (seq.x$check) {
    if (highlight) {
      cat("Non-sequential time (xcol) location(s), by row:\n", seq.x$highlight,
        "\n")
    }
    stop("Test FAILED. Time data (xcol) are not sequential.", " Please check the order of the data.",
      call. = F)
  }
  if (dup.x$check) {
    if (highlight) {
      cat("Duplicate time data (xcol) location(s), by row:\n", dup.x$highlight,
        "\n")
    }
    stop("Test FAILED. Duplicate time data (xcol) detected.", call. = F)
  }
  if (equal.x$check) {
    warning("Time data (xcol) is irregular. Avoid subsetting by `row` unless you know what you are doing.",
      call. = F)
    if (highlight)
      cat("Unevenly-spaced time (xcol) location(s), by row:\n", equal.x$highlight,
        "\n")
  }
  # Finally, if df is to be generated, remove NA
  if (na.x$check | na.y$check) {
    df <- na.omit(df)  # remove NA rows
    warning("Rows containing NA values have been automatically removed.", call. = F)
  }
  message("New dataframe generated.")

  ## PLOT
  if (plot) {
    plot(df, xlab = "Time", ylab = "PO_2", col = r1, pch = 16, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
      lty = 1, lwd = 1.5)))
  }
  out <- list(df = df, check.columns = column.check, check.na.x = na.x, check.na.y = na.y,
    check.sequential = seq.x, check.dupe = dup.x, check.uneven.spacing = equal.x)


  class(out) <- "inspect.data"
  return(invisible(out))
}



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
  out <- list(x = x, class.xcol = class.xcol, class.ycol = class.ycol, is.x.integer = is.x.integer,
    test.x = test.x, test.y = test.y)
  return(invisible(out))
}

# Test for NA/NaN
test.na <- function(x) {
  test <- is.na(x)
  check <- any(test)
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}

# Test for sequential time
test.seq <- function(x) {
  test <- diff(x) < 0
  test <- ifelse(is.na(test), FALSE, test)  # convert NA values to FALSE
  check <- any(test)
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}

# Test for duplicate time
test.dupe <- function(x) {
  test <- x %in% unique(x[duplicated(x, incomparables = NA)])
  check <- any(test)
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}

# Calculate mode value in a list
calc.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Test for evenly-spaced time
test.space <- function(x) {
  spacing <- diff(as.numeric(x))
  mod <- calc.mode(spacing)

  test <- spacing != mod
  # if spacing is even, there should only be 1 interval detected:
  check <- length(unique(spacing)) > 1

  test <- ifelse(is.na(test), TRUE, test)  # convert NA values to FALSE
  highlight <- which(test)
  out <- list(test = test, check = check, highlight = highlight)
  return(invisible(out))
}

