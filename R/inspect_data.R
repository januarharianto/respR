#' Check for common errors in respirometry data
#'
#' `inspect_data()` scans a data frame for specific errors that may affect the
#' use of functions in `respR`. Data checks include:
#'
#' * A test for NA/NaN inputs.
#' * A test for numeric data.
#' * A test for sequential time data.
#' * A test for duplicate time data.
#' * A test for evenly-spaced time data.
#'
#' Once data checks are complete, the function produces a list object which may
#' be directly loaded into [calc_rate()], [calc_rate.bg()], [auto_rate()] and
#' [pcrit()] (NC - i don't think this last one works) for further analyses.
#'
#' @md
#'
#' @param df data frame. Accepts data frame object of any size.
#' @param time numeric. Defaults to NULL. This is the time data.
#' @param oxygen numaric. Defaults to NULL. This is the dissolved oxygen data.
#' @param inflow numeric. Defaults to NULL. This is the incurrent oxygen data.
#' @param outflow numeric. Defaults to NULL. This is the excurrent oxygen data.
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
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' inspect_data(sardine.rd)
#' inspect_data(urchins.rd, 1, 5, highlight = FALSE)
inspect_data <- function(df, time = NULL, oxygen = NULL, inflow = NULL,
  outflow = NULL, highlight = TRUE, plot = TRUE) {

  # Validate inputs
  has.time <- is.numeric(time)
  has.oxygen <- is.numeric(oxygen)
  has.inflow <- is.numeric(inflow)
  has.outflow <- is.numeric(outflow)

  if (!is.data.frame(df)) stop("'df must be data.frame object!")

  if (is.null(c(time, oxygen, inflow, outflow))) {
    # Nothing is specified, subset columns 1 & 2 by default:
    dt <- data.table::data.table(time = df[[1]], oxygen = df[[2]])
    type <- "default"
  } else if (has.time && has.oxygen) {
    # Time and oxygen columns are specified:
    dt <- data.table::data.table(time = df[[time]], oxygen = df[[oxygen]])
    type <- "default"
  } else if (has.time && has.inflow && has.outflow) {
    # Time, inflow and outflow columns are specified:
    dt <- data.table::data.table(time = df[[time]], inflow = df[[inflow]],
      outflow = df[[outflow]])
    type <- "flowthrough"
  } else stop("Check inputs...")

  # First, convert time to numeric if it is integer
  if (any(class(dt[,1]) %in% "integer")) dt$Time <- as.numeric(dt$Time)

  # Check if time is POSIX* if it is, create unique timeseries for checking
  if (any(class(dt[[1]]) %in% c(c("POSIXct", "POSIXt")))) {
    time <- as.numeric(dt[[1]])
  } else time <- dt[[1]]

  # PERFORM CHECKS -------------------------------------
  # Rule: if logical result is TRUE, the test is a FAIL.

  # Test NA
  test_na <- lapply(dt, function(x) check_na(x))
  time_na <- test_na$time

  if (type == "default") {
    oxy_na <- test_na$oxygen
  } else {
    inflow_na <- test_na$inflow
    outflow_na <- test_na$outflow
  }

  # Test sequential, duplicate and unevenly spaced time
  time_seq <- check_seq(time)
  time_dup <- check_dup(time)
  time_evn <- check_evn(time)

  # Print summary now before we process the checks
  print(ls.str(df))
  cat("---\n")

  if (type == "default") {
    summary <- noquote(rbind(time_na$check, oxy_na$check, time_seq$check,
      time_dup$check, time_evn$check))
    rownames(summary) <- c("No NA/NaN in time", "No NA/NaN in Oxygen",
      "Sequential Time", "Non-duplicated Time",
      "Evenly-spaced Time")
  } else {
    summary <- noquote(rbind(time_na$check, inflow_na$check, outflow_na$check,
      time_seq$check, time_dup$check, time_evn$check))
    rownames(summary) <- c("No NA/NaN in time", "No NA/NaN in inflow",
      "no NA/NaN in outflow", "Sequential Time", "Non-duplicated Time",
      "Evenly-spaced Time")
  }

  colnames(summary) <- "Score"
  summary[summary == "TRUE"] <- "FAIL***"
  summary[summary == "FALSE"] <- "PASS"
  print(summary)

  # Process checks -------------------------------------

  if (time_na$check) {
    warning("time column contains NA or NaN data.", call. = F)
    if (highlight)
      cat("NA/NaN location(s) in time, by row:\n",
        time_na$which, "\n")
  }
  if (type == "default") {
    if (oxy_na$check) {
      warning("dissolved oxygen column contains NA or NaN data.", call. = F)
      if (highlight)
        cat("NA/NaN location(s) in time, by row:\n",
          oxy_na$which, "\n")
    }
  }

  if (type == "flowthrough") {
    if (inflow_na$check) {
      warning("inflow data contains NA/NaN.", call. = F)
      if (highlight)
        cat("NA/NaN location(s) in inflow, by row:\n",
          inflow_na$which, "\n")
    }
  }

  if (type == "flowthrough") {
    if (outflow_na$check) {
      warning("outflow data contains NA/NaN.", call. = F)
      if (highlight)
        cat("NA/NaN location(s) in outflo, by row:\n",
          outflow_na$which, "\n")
    }
  }

  if (time_seq$check) {
    if (highlight)
      cat("Non-sequential time location(s), by row:\n",
        time_seq$which, "\n")
    stop("Test FAILED. Time data are not sequential.",
      " Please check the order of the data.", call. = F)
  }

  if (time_dup$check) {
    if (highlight)
      cat("Duplicate time data location(s), by row:\n",
        time_dup$which, "\n")
    stop("Test FAILED. Duplicate time data (xcol) detected.",
      call. = F)
  }

  if (time_evn$check) {
    warning("Time data is irregular. ",
      "Subsetting by `row` may result in irregular durations.", call. = F)
    if (highlight)
      if(length(time_evn$which) > 50) {
        cat("More than 50 irregular time intervals detected,",
          " showing first 20 (by row):\n",
          head(time_evn$which, 20), "\n")
      } else {
        cat("Unevenly-spaced time location(s), by row:\n",
          time_evn$which, "\n")
      }
  }

  # Collate highlights
    if (type == "default") {
      highlights <- list(time_na$which, oxy_na$which, time_seq$which,
        time_dup$which, time_evn$which)
    } else highlights <- list(time_na$which, inflow_na$which, outflow_na$which,
      time_seq$which, time_dup$which, time_evn$which)

  # Finally, if df is to be generated, remove NA
  if (any(is.na(dt))) {
    df <- na.omit(dt)  # remove NA rows
    warning("Rows containing NA values have been automatically removed.",
      call. = F)
  }
  message("New dataframe generated.")

  ## PLOT
  if (plot) {
    # Calculate rolling regression
    roll <- static_roll(df, floor(0.1 * nrow(df)))$rate_b1
    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = c(2, 1), mai = c(0.4, 0.4, 0.3, 0.3), ps = 10,
      cex = 1, cex.main = 1)
    plot(df[[1]], df[[2]], xlab = "", ylab = "", col = r1, pch = 16,
      panel.first = c(rect(par("usr")[1],
        par("usr")[3], par("usr")[2], par("usr")[4], col = r3),
        grid(col = "white", lty = 1, lwd = 1.5)))
    title(main = "Full Timeseries", line = 0.3)
    plot(abs(roll), xlab = "", ylab = "", col = r1, pch = 16,
      panel.first = c(rect(par("usr")[1], par("usr")[3],
        par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
          lty = 1, lwd = 1.5)))
    title(main = "Rolling Regression of Rate vs Index (Row No.)",
      line = 0.3)
    par(pardefault)  # revert par settings to original
  }
  out <- list(df = dt, highlights = highlights)

  class(out) <- "inspect_data"
  return(invisible(out))
}



# check for NA values
check_na <- function(x) {
  test <- is.na(x)
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

# Check for sequential (monotonic) data
check_seq <- function(x) {
  test <- diff(x) < 0
  test <- ifelse(is.na(test), FALSE, test)  # convert NA values to FALSE
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

# Check for duplicate data
check_dup <- function(x) {
  test <- x %in% unique(x[duplicated(x, incomparables = NA)])
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

# Calculate mode
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

check_evn <- function(x) {
  spacing <- diff(as.numeric(x))
  mod <- calc_mode(spacing)

  test <- spacing != mod
  # If spacing is even, there should only be 1 interval detected:
  check <- length(unique(spacing)) > 1

  test <- ifelse(is.na(test), TRUE, test)  # convert NA values to FALSE
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}
