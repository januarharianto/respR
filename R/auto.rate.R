#' Automatically determine rate of change in oxygen concentration over time
#'
#' `auto.rate` automatically performs a rolling regression on a data frame to perform determinations of maximum, minimum, interval or "best fit" linear rate of change in oxygen concentration over time. First, a rolling regression of specified `width` is performed on the entire dataset to obtain all possible values. The computations are then ranked (or, arranged), based on the "`logic`" argument, and the output is summarised.
#'
#' **Units**
#'
#' There are no units of measurements involved in `auto.rate`. This is a deliberate decision. Units are called in a later function when volume- and/or weight-specific rates of oxygen concentration are computed in [calc.mo2()].
#'
#' **Assumptions**
#'
#' This function currently assumes that your data is evenly-spaced in time. A future version of this function will be able to accurately perform regressions of varying row widths due to unevenly-spaced time values.
#'
#' ***Ranking algorithms***
#'
#' For now, `auto.rate()` contains four ranking algorithms that can be called with the argument "`logic`":
#'
#' - `max` - regressions are arranged from highest absolute values, to the lowest.
#' - `min` - regressions are arranged from lowest absolute values, to the highest.
#' - `interval` - non-overlapping regressions are extracted from the rolled regrssions. They are not ranked.
#' - `automatic` - uses kernel density estimation to detect the most "linear" sections of the timeseries in descending order. This is an experimental technique and should be used with caution. We plan to refine this method in a future version of the package.
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#' @param df data frame. Should contain time (column 1) vs oxygen concentration (column 2) data.
#' @param width numeric. The number of rows (or the time interval, if `by = "time"`) to use when performing the rolling regression.
#' @param by string. Describes the method used to subset the data frame. Defaults to `"time"`. Available: "`time`", "`row`".
#' @param logic logical. Determines the ranking algorithm to use. Defaults to "`automatic`". Available: "`max`", "`min`", "`interval`", "`automatic`".
#' @param bg numeric, or an output of class [calc.bg.rate]. Value for background changes in O~2~ concentration. Should be negative, to represent rate of removal of o2. May also be positive in some cases, depending on your experimental setup.
#'
#' @return An object of class `auto.rate` containing a list of outputs:
#' \describe{
#' \item{`id`}{Method used to compute the results. Possible outputs: "`maxmin`", "`interval`", or "`automatic`".}
#' \item{`main.data`}{The original data frame.}
#' \item{`width`}{Numeric. The width, in rows, of the rolling window or interval used.}
#' \item{`by`}{String. The method used to subset the data frame.}
#' \item{`logic`}{The selected ranking algorithm used to produce the results.}
#' \item{`interval`}{Appears only if the argument `logic = "interval"`. This is the interval used to calculate the regressions and is specified by the number of rows in the original data frame. If the argument `by = "time"` is used, the time interval is converted to the number of rows.}
#' \item{`kernel.dens`}{Appears only if the argument `logic = "automatic"`. This is the output of the kernel density estimation analysis used to analyse the data for peak detection.}
#' \item{`peaks`}{Appears only if the argument `logic = "automatic"`. A data frame that contains density peak values that were detected from the kernel density estimate, identified by their index location, the slope (b1) at each peak, and the density calculated for each slope (dens).}
#' \item{`bin.width`}{Appears only if the argument `logic = "automatic"`. This is the bin.width used to determine the density kernel estimate.}
#' \item{`regressions`}{An output data frame that summarises the coefficients of all calculated regressions used to determine the results.}
#' \item{`results`}{A data frame summary of the result(s), which include regression coefficients and subset locations.}
#' }
#'
#' @importFrom dplyr arrange bind_rows mutate select everything
#' @importFrom tibble rowid_to_column
#' @importFrom roll roll_lm
#' @export
#'
#' @examples
#' # calculate maximum rate at width 500 (rows):
#' x <- auto.rate(sardine, width = 500, logic = "max")
#' print(x)
#' summary(x)
#' plot(x)
#'
#' # calculate rate at 500-second intervals along the timeseries:
#' x <- auto.rate(sardine, width = 500, by = "time", logic = "interval")
#' print(x)
#' summary(x)
#' plot(x, rank = 3) # view the diagnostic plot for the 3rd interval
#'
#' # Automatically calculate the most "linear" section of a timeseries
#' # based on kernel density estimates. When width is not specified, it
#' # is automatically set to 10% of the total length of data:
#' x <- auto.rate(sardine)
#'
#' # Automatically calculate the most "linear" section, where the rolling
#' # regression is calculated based on time instead of row. Background
#' # resporation can also be added for automatic correction:
#' x <- auto.rate(df = sardine, width = 600, by = "time", logic = "automatic",
#'   bg = 0.0002525)
#' print(x)
#' summary(x)
#' plot(x, rank = 1)
#'
auto.rate <- function(df, width = NULL, by = "row",
  logic = 'automatic', bg = NULL) {
  # if subsetting by row, and width is NULL, automatically set the width to
  #   20% of the total length of the dataset.
  if (is.null(width) && by == "row") width <- floor(0.2 * nrow(df))
  # If subsetting by time, width must NOT be null:
  if (is.null(width) && by == "time")
    stop("Please supply a `width` argument. It is currently NULL by default.")
  # If subsetting by time, time must be evenly spaced.
  if (by == "time") if (equal.lengths(diff(df[[1]])) == F)
      warning("Time intervals are not evenly spaced.")
  # Check that df is a data.frame object:
  if (any(class(df) != "data.frame"))
    stop("Input must be a data.frame object.")
  # Make sure that `logic` is correct:
  if (!(logic == "automatic" | logic == "max" | logic == "min" |
      logic == "interval"))
    stop("The `logic` argument can only be 'automatic', 'max', 'min' or 'interval'.")
  # How are we subsetting the data?
  # Note: only "time" and "row" supported here.
  if (by == "row") {
    fits <- rollfit(df, width)  # Perform rolling regressions
  } else if (by == "time") {
    # we approximate row time interval by looking at total time and dividing by
    # the total number of rows:
    row.interval <- floor(max(df[1])/(nrow(df)-1))
    width <- round(width/row.interval + 1)
    fits <- rollfit(df, width)
    # Estimate the no. of rows
  } else stop("Only 'row'and 'time' arguments are supported in 'by'.")
  fits <- rowid_to_column(fits)  # Append locations to data
  fits <- na.omit(fits)  # Remove NA results as they're not used
  # Select method of analysis: "max", "min", "interval" or "automatic":
  # Minimum rate ---------------------------------------------------------------
  if (logic == "min") {
    out <- arrange(fits, abs(b1))
    # Maximum rate ---------------------------------------------------------------
  } else if (logic == "max") {
    out <- arrange(fits, desc(abs(b1)))
    # Intervals ------------------------------------------------------------------
  } else if (logic == "interval") {
    # Generate the index to subset data for intervals:
    sequence <- seq(width, nrow(df), width)
    # Grab the intervals and bind them into a data frame:
    intv <- lapply(sequence, function(x) dplyr::filter(fits, rowid == x))
    out  <- bind_rows(intv)
    # Best linear fit ------------------------------------------------------------
  } else if (logic == "automatic") {
    # WARNING
    # This method is still experimental and should not be used to analyse data
    # unless you know what you are doing.
    pks <- k.peaks(fits)  # identify the peaks in kernel density estimate
    out <- match.data(df, fits, pks, width, bg)  # match to roll. reg.
    # --------------------------------------------------------------------------
  } else stop("Cannot ID the 'logic' argument. Hint: '?auto.rate'")
  # ----------------------------------------------------------------------------
  # Format some data here to prepare for output summary:
  if (logic == "min" | logic == "max" | logic == "interval") {
    # Calculate data locations using mutate::
    out <- mutate(out,
      from.row  = rowid - width + 1,
      to.row    = rowid,
      from.time = df[,1][from.row],
      to.time   = df[,1][to.row],
      from.o2   = df[,2][from.row],
      to.o2     = df[,2][to.row],
      row.len   = to.row - from.row + 1,
      time.len  = to.time - from.time)
  } else if (logic == "automatic") out <- out
  # ----------------------------------------------------------------------------
  # Include background rate, if provided, and also the corrected rate:
  if (is.null(bg)) {
    out <- out
  } else if (class(bg) == "calc.bg.rate") {
    bg <- bg$rate
    out <- mutate(out, bg = bg, "b1-bg" = b1 - bg)
    out <- select(out, b0, b1, bg, `b1-bg`, everything())  # rearrange
  } else if (class(bg) == "numeric") {
    out <- mutate(out, bg = bg, "b1-bg" = b1 - bg)
    out <- select(out, b0, b1, bg, `b1-bg`, everything())  # rearrange
  }
  # ----------------------------------------------------------------------------
  # Format output based on "logic" argument:
  if (logic == "max" | logic == "min") {
    out <- list(
      id          = "maxmin",
      main.data   = df,
      width       = width,
      by          = by,
      logic       = logic,
      regressions = fits,
      results     = out)
    message(sprintf("%d regressions fitted.", nrow(out$results)))
    if(!is.null(bg)) message("Background correction recognised and applied.")
  } else if (logic == "interval") {
    out <- list(
      id          = "interval",
      main.data   = df,
      interval    = width,
      by          = by,
      logic       = logic,
      regressions = fits,
      results     = out)
    message(sprintf("%d regressions fitted.", nrow(out$results)))
    if(!is.null(bg)) message("Background correction recognised and applied.")
  } else if (logic == "automatic") {
    out <- list(
      id          = "automatic",
      main.data   = df,
      width       = width,
      by          = by,
      logic       = logic,
      kernel.dens = pks$density,
      peaks       = pks$peaks,
      bin.width   = pks$density$bw,
      regressions = fits,
      results = out)
    message(sprintf("%d regressions fitted.", nrow(out$regressions)))
    message(sprintf("%d kernel density peaks detected and ranked.",
      nrow(out$results)))
    if(!is.null(bg)) message("Background correction recognised and applied.")

  }
  class(out) <- "auto.rate"
  return(out)
}








#' Print an object of class `auto.rate`
#'
#' @md
#' @param x object of class `auto.rate`.
#' @param rank numeric. Defaults to 1. Changing this number will allow user to view other ranked results.
#'
#' @return a print output.
#'
#' @importFrom dplyr select
#'
#' @keywords internal
#' @export
print.auto.rate <- function(x, rank = 1) {
  if (x$id == "maxmin") {
    if (x$results$b1[1] > x$results$b1[nrow(x$results)]) {
      cat("Ranked computation of rate of change of O2 concetration (minimum)\n")
    } else       cat("Ranked computation of rate of change of O2 concetration (maximum)\n")
    cat(sprintf("--- Result for Rank %d ---\n", rank))
    cat(sprintf("     Rate (b1): %f", x$results$b1[rank]))
    # add background rate stuff if it exists
    if (length(x$results) == 14) {
      cat(sprintf(" | Background (bg): %f", x$results$bg[rank]))
      cat(sprintf(" | Adj. Rate (b1-bg): %f", x$results$`b1-bg`[rank]))
    }
    cat(sprintf("\nIntercept (b0): %f", x$results$b0[rank]))
    cat(sprintf("\n          R-sq: %g\n", x$results$rsq[rank]))
    cat("\n--- Subset Information --- \n")
    # add background rate stuff if it exists
    if (length(x$results) == 14) {
      print(select(x$results[rank,], -(1:6)))
    } else print(select(x$results[rank,], -(1:4)))

  } else if (x$id == "interval") {
    cat("Computation of rate of change of O2 concetration (interval)\n")
    cat("--- First 6 Results ---\n")
    print(head(select(x$results, 2:4)))
    cat("\n--- Subset Information ---\n")
    print(head(select(x$results, -(1:4))))

  } else if (x$id == "automatic") {
    cat("Ranked computation of rate of change of O2 concetration (auto)\n")
    cat(sprintf("--- Result for Rank %d ---\n", rank))
    cat(sprintf("     Rate (b1): %f", x$results$b1[rank]))
    # add background rate stuff if it exists
    if (length(x$results) == 13) {
      cat(sprintf(" | Background (bg): %f", x$results$bg[rank]))
      cat(sprintf(" | Adj. Rate (b1-bg): %f", x$results$`b1-bg`[rank]))
    }
    cat(sprintf("\nIntercept (b0): %f", x$results$b0[rank]))
    cat(sprintf("\n          R-sq: %g\n", x$results$rsq[rank]))
    cat("\n--- Subset Information ---\n")
    # add background rate stuff if it exists
    if (length(x$results) == 13) {
      print(select(x$results[rank,], -(1:5)))
    } else print(select(x$results[rank,], -(1:3)))
  }
}








#' Summarise an object of class `auto.rate`
#'
#' @md
#' @param x object of class `auto.rate`.
#' @param n numeric. Defaults to 5. The number of results to show.
#'
#' @return a summary output.
#'
#' @keywords internal
#' @export
summary.auto.rate <- function(x, n = 5) {
  if (x$id == "maxmin") {
    if (x$results$b1[1] > x$results$b1[nrow(x$results)]) {
      cat("Method     : Minimum\n")
    } else cat("Method     : Maximum\n")
    cat(sprintf("Regressions: %d\n", nrow(x$results)))
  } else if (x$id == "interval") {
    cat("Method     : Interval\n")
    cat(sprintf("Regressions: %d\n", nrow(x$results)))
  } else if (x$id == "automatic") {
    cat("Method        : Auto\n")
    cat(sprintf("Regressions   : %d\n", nrow(x$regressions)))
    cat(sprintf("Peaks detected: %d\n", nrow(x$peaks)))
  }
  if (x$id == "interval") {
    if (nrow(x$results) < 11) {
    cat(sprintf("\n--- Summary of all %d regressions ---\n", nrow(x$results)))
    print(x$results, n)
    } else {
      cat(sprintf("\n--- Summary of the first %d regressions ---\n", n))
      print(head(x$results, n))
      }
  } else {
    cat(sprintf("\n--- Summary of the first %d regressions ---\n", n))
    print(head(x$results, n))
  }
}








#' Plot an object of class `auto.rate`
#'
#' @md
#' @param x object of class `auto.rate`.
#' @param rank numeric. Defaults to 1. Changing this number will allow user to view other ranked results.
#'
#' @return a plot output.
#'
#' @keywords internal
#' @export
plot.auto.rate <- function(x, rank = 1) {
  # Calculate common variables (should we do this in main function? hmm)
  df    <- x$main.data  # main timseries
  from  <- x$results$row[rank] # start row
  to    <- x$results$endrow[rank] # end row
  sdf   <- df[from:to,] # the subset timeseries after auto detection
  lmfit <- lm(sdf[[2]] ~ sdf[[1]], sdf)  # lm of subset

  # if max/min
  if (x$id == "maxmin") {
    rollwidth <- length(x$main.data[[1]])-length(x$regressions$b1) # roll width
    rolldf <- data.frame(x = df[(rollwidth + 1):length(x$main.data[[1]]),][[1]],
      y = x$regressions$b1)
    ranked.b1 <- x$results$b1[rank]

    mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5), nrow = 2, byrow = TRUE)
    layout(mat)
    multi.p(df, sdf)  # full timeseries with lmfit
    sub.p(sdf) # closed-up (subset timeseries)
    rollreg.p(rolldf, ranked.b1) # rolling regression series
    residual.p(lmfit)  # residual plot
    qq.p(lmfit)  # qq plot
    layout(1)
  } else if (x$id == "interval") {
    intervals <- x$results$to.time

    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = c(2, 2))  # replace par settings
    multi.p(df, sdf)
    abline(v = 0, lty = 3)
    abline(v = intervals, lty = 3)
    sub.p(sdf)
    residual.p(lmfit)
    qq.p(lmfit)
    par(pardefault)  # revert par settings to original
  } else if (x$id == "automatic") {
    rollwidth <- length(x$main.data[[1]])-length(x$regressions$b1) # roll width
    rolldf <- data.frame(x = df[(rollwidth + 1):length(x$main.data[[1]]),][[1]],
      y = x$regressions$b1)
    ranked.b1 <- x$results$b1[rank]
    dens <- x$kernel.dens
    peaks <- x$peaks[2:3]

    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = c(2, 3))  # replace par settings
    multi.p(df, sdf)  # full timeseries with lmfit
    sub.p(sdf)  # closed-up (subset timeseries)
    rollreg.p(rolldf, ranked.b1)  # rolling regression series with markline
    density.p(dens, peaks, rank) # density plot
    residual.p(lmfit) # residual plot
    qq.p(lmfit  )# qq plot
    par(pardefault)  # revert par settings to original
  }
}








# ==============================================================================
# Internal functions. I've taken these out to simplify the code and make code
# optimisation easier, if necessary, later on.

#' Perform rolling regressions of specific width
#'
#' This is an internal function. The typical user should not see this.
#'
#' @md
#' @param df data frame. The data to perform rolling regressions on.
#' @param width numeric. The rolling window.
#'
#' @return A data frame of coefficients.
#' @keywords internal
#'
rollfit <- function(df, width) {
  x <- matrix(df[[1]]) # convert data to matrices to work with roll_lm
  y <- matrix(df[[2]])
  rfit <- roll_lm(x, y, width)  # perform the rolling regression here
  # Bind results into a data frame:
  out <- cbind.data.frame(rfit$coefficients, signif(rfit$r.squared, 3))
  names(out) <- c("b0", "b1", "rsq")
  return(out)
}








#' Perform kernel density estimation of the data and identify the peaks
#'
#' This is an internal function. The typical user should not see this.
#'
#' @md
#' @param x data frame. Makes sense only if it this was an output of `rollfit()`.
#'
#' @return A list of 2 objects, "density" and "peaks".
#' @keywords internal
#'
k.peaks <- function(x) {
  # Perform kernel density estimation:
  dns <- density(x$b1, na.rm = T, bw = "SJ-ste", n = length(x$b1))
  pks <- which(diff(sign(diff(dns$y))) == -2) + 1  # ID peaks in density
  # Match peaks to density data:
  pks <- bind_rows(lapply(pks, function(x)
    data.frame(index = x, ref.b1 = dns$x, dens = dns$y)[x,]))
  pks <- arrange(pks, desc(dens))  # arrange in descending order
  output <- list(density = dns, peaks = pks)
  return(output)
}








#' Match peaks to original data frame
#'
#' This is an internal function. The typical user should not see this.
#'
#' This function uses the the peak values and bin width of the kernel density estimate to match the results to the data in the original data frame.
#'
#' @param df data frame.
#' @param fits data frame.
#' @param pks numeric vector.
#' @param width numeric.
#' @param bg numeric.
#'
#' @return a data frame.
#' @keywords internal
#'
match.data <- function(df, fits, pks, width, bg) {
  bw <- pks$density$bw  # extract bin width
  # Match all regressions used to determine each peak using the bin width:
  mat.regs <- lapply(pks$peaks[,2], function(x)
    dplyr::filter(fits, b1 <= (x+bw/2) & b1 >= (x-bw/2)))
  mat.regs <- mat.regs[sapply(mat.regs, nrow) > 0] # remove zero-length matches
  # Now match to the raw data, using matched regressions:
  mat.raw <- lapply(1:length(mat.regs), function(x)
    split(mat.regs[[x]],
      c(0, cumsum(abs(diff(mat.regs[[x]]$rowid)) > width))))
  # Identify the best fragments - first grab the index of longest fragments
  idx <- lapply(1:length(mat.raw), function(x)
    which.max(bind_rows(lapply(mat.raw[[x]], nrow))))
  idx <- do.call(rbind, idx)
  # The fragments are identified again:
  frags <- unname(mapply(function(x, y)
    mat.raw[[x]][y], 1:length(mat.raw), idx))
  # Perform calc.rate on all fragments:
  output <- lapply(1:length(frags), function(x)
    calc.rate(df = df,
      from = min(frags[[x]]$rowid - width + 1),
      to   = max(frags[[x]]$rowid),
      by   = 'row',
      bg   = bg,
      plot = F,
      verbose = F)$results)
  output <- bind_rows(output)  # bind output to a table
  return(output)
}
