#' Automatically determine rate of change in oxygen concentration over time
#'
#' `auto.rate` automatically performs a rolling regression on a data frame to perform determinations of maximum, minimum, interval or "most linear" rate of change in oxygen concentration over time. First, a rolling regression of specified `width` is performed on the entire dataset to obtain all possible values. The computations are then ranked (or, arranged), based on the "`logic`" argument, and the output is summarised.
#'
#' There are no units of measurements involved in `auto.rate`. This is a deliberate decision. Units are called in a later function when volume- and/or weight-specific rates of oxygen concentration are computed in [calc.mo2()].
#'
#' **Important**: Calling `auto.rate` using the argument `logic = "automatic"` uses kernel density estimation to detect the most "linear" sections of the timeseries in descending order. This is an experimental technique and may have unintended results, although it did work as intended for all the sample data that we threw at it. We plan to refine this technique in a future version of the package.
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#' @param df data frame. Should contain time (column 1) vs oxygen concentration (column 2) data.
#' @param width numeric. The number of rows (or the time interval, if `by = "time"`) to use when performing the rolling regression.
#' @param by string. Describes the method used to subset the data frame. Defaults to `"time"`. Available: "`time`", "`row`".
#' @param logic logical. Determines the type of data to output. Defaults to "`automatic`". Available: "`max`", "`min`", "`interval`", "`automatic`".
#' @param bg numeric, or an output of class [calc.bg.rate]. Value for backgroung respiration.
#'
#' @return An object of class `auto.rate` containing a list of outputs:
#' \describe{
#' \item{`id`}{Method used to compute the results. Possible outputs: "`maxmin`", "`interval`", or "`automatic`".}
#' \item{`interval`}{Appears only if the argument `logic = "interval"`. This is the interval used to calculate the regressions and is specified by the number of rows in the original data frame. If the argument `by = "time"` is used, the time interval is converted to the number of rows.}
#' \item{`main.data`}{The original data frame.}
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
auto.rate <- function(df, width = floor(0.1 * nrow(df)), by = "time",
  logic = 'automatic', bg = NULL) {
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
  } else stop("Only 'row'and 'time' arguments are supported in 'by'.", call = F)
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
  } else stop("Cannot ID the 'logic' argument. Hint: '?auto.rate'", call. = F)
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
      id = "interval",
      interval    = width,
      by          = by,
      logic       = logic,
      main.data   = df,
      regressions = fits,
      results     = out)
    message(sprintf("%d regressions fitted.", nrow(out$results)))
    if(!is.null(bg)) message("Background correction recognised and applied.")
  } else if (logic == "automatic") {
    out <- list(
      id = "automatic",
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


# ==============================================================================
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
      print(dplyr::select(x$results[rank,], -(1:6)))
    } else print(dplyr::select(x$results[rank,], -(1:4)))

  } else if (x$id == "interval") {
    cat("Computation of rate of change of O2 concetration (interval)\n")
    cat("--- First 6 Results ---\n")
    print(head(dplyr::select(x$results, 2:4)))
    cat("\n--- Subset Information ---\n")
    print(head(dplyr::select(x$results, -(1:4))))

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
      print(dplyr::select(x$results[rank,], -(1:5)))
    } else print(dplyr::select(x$results[rank,], -(1:3)))
  }
}

# ==============================================================================
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
    cat(sprintf("\n--- Summary of all %d regressions ---\n", nrow(x$results)))
    print(x$results, n)
  } else {
    cat(sprintf("\n--- Summary of the first %d results ---\n", n))
    print(head(x$results, n))
  }
}


# ==============================================================================
#' @export
plot.auto.rate <- function(x, rank = 1) {
  # Calculate common variables (should we do this in main function? hmm)
  df    <- x$main.data  # main timseries
  from  <- x$results$from.row[rank] # start row
  to    <- x$results$to.row[rank] # end row
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
    intervals <- x$results$to.row

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
# Internal functions

# perform rolling regression
rollfit <- function(df, width) {
  x <- matrix(df[[1]]) # convert data to matrices to work with roll_lm
  y <- matrix(df[[2]])
  rfit <- roll_lm(x, y, width)  # perform the rolling regression here
  # Bind results into a data frame:
  out <- cbind.data.frame(rfit$coefficients, signif(rfit$r.squared, 3))
  names(out) <- c("b0", "b1", "rsq")
  return(out)
}

# perform kernel density estimation of the data and identify the peaks
k.peaks <- function(x) {
  # Perform kernel density estimation:
  dns <- density(x$b1, na.rm = T, bw = "SJ", n = length(x$b1))
  pks <- which(diff(sign(diff(dns$y))) == -2) + 1  # ID peaks in density
  # Match peaks to density data:
  pks <- bind_rows(lapply(pks, function(x)
    data.frame(index = x, ref.b1 = dns$x, dens = dns$y)[x,]))
  pks <- arrange(pks, desc(dens))  # arrange in descending order
  output <- list(density = dns, peaks = pks)
  return(output)
}

# match peaks to data
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
