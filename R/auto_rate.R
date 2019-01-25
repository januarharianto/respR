#' Automatically determine rate of change in oxygen concentration over time
#'
#' `auto_rate` automatically performs a rolling regression on a data frame to
#' determine the *most linear, maximum, minimum*, or *interval* rate of change
#' in oxygen concentration over time. First, a rolling regression of specified
#' `width` is performed on the entire dataset to obtain all possible values. The
#' computations are then ranked (or, arranged), based on the "`logic`" argument,
#' and the output is summarised.
#'
#' **Units**
#'
#' There are no units of measurement involved in `auto_rate`. This is a
#' deliberate decision. Units are called in a later function when volumetric
#' and/or mass-specific rates of oxygen use are computed in [convert_rate()] and
#' [convert_DO()].
#'
#' ***Ranking algorithms***
#'
#' At present, `auto_rate()` contains four ranking algorithms that can be called
#' with the argument `method`:
#'
#' - `linear`: Uses kernel density estimation (KDE) to detect the "most
#' linear" sections of the timeseries. This is achieved by using the smoothing
#' bandwidth of the KDE to re-sample the "peaks" in the KDE to determine linear
#' regions in the data.
#'
#' - `max`: regressions are arranged from highest values, to the lowest.
#'
#' - `min`: regressions are arranged from lowest values, to the highest.
#'
#' - `interval`: non-overlapping regressions are extracted from the rolled
#' regrssions. They are not ranked.
#'
#' @param df data frame, or object of class `adjust_rate`. This is the data to
#'   process.
#' @param width numeric. Width of the rolling regression. Defaults to
#'   `floor(0.2*length of data)` if NULL. The length of data can either be time
#'   or row, as defined by the `by` argument. If a number is supplied and it is
#'   less than 1, the function automatically applies the equation `floor(width *
#'   length of data`. Otherwise it will simply use the number as the width.
#' @param by string. `"row"` or `"time"`. Defaults to `"row"`. However, if the
#'   function detects an irregular time series, a warning will be issued to
#'   recommend changing this argument to `"time"`. In most cases `"row"` is used
#'   by default as it is efficient. Switching to `"time"` causes the function to
#'   perform checks for irregular time at every iteration of the rolling
#'   regression, even if time is evenly spaced, which adds to computation time.
#' @param method string. `"linear"`, `"max"`, `"min"` or `"interval"`. Defaults
#'   to `linear`. Note that if `"linear"` is selected, the argument `width` will
#'   be set to default.
#' @param plot logical. Defaults to TRUE. Plot the results.
#' @param full logical. Defaults to FALSE. Change to TRUE to plot every single 
#'   data point. May increase computation time substantially.
#' @param parallel logical. Defaults to TRUE. Should parallel processing be
#'   used?
#'
#' @return A list object of class `auto_rate`.
#'
#' @import data.table
#' @import parallel
#'
#' @export
#'
#' @examples
#' # most linear section of the entire data
#' auto_rate(flowthrough.rd, parallel = FALSE)
#'
#' # LONG EXAMPLES
#' \dontrun{
#' # what is the lowest rate over a 10 minute (600s) period?
#' auto_rate(sardine.rd, method = "min", width = 600, by = "time", parallel = FALSE)
#' # what is the highest rate over a 10 minute (600s) period?
#' auto_rate(sardine.rd, method = "max", width = 600, by = "time", parallel = FALSE)
#' }
auto_rate <- function(df, width = NULL, by = "row", method = "linear",
                      plot = TRUE, full = FALSE, parallel = FALSE) {
  # Import and format data
  if (any(class(df) %in% "inspect_data")) df <- df$df
  if (any(class(df) %in% "inspect")) df <- df$dataframe

  # Input checks
  if(!is.data.frame(df))
    stop("`df` must be a data frame object.")

  # check that data frame length is 2, if not, select only the first 2 columns
  if (length(df) > 2) {
    message("auto_rate: Large dataframe detected. Selecting first 2 columns by default.")
    df <- df[,1:2]
  }

  if (!by %in% c("time", "row"))
    stop("Invalid `by`` input value, must be 'time' or 'row'.")
  if (!method %in% c("default", "linear", "max", "min", "interval"))
    stop("Invalid `method` input value, please run ?auto_rate for options.")

  dt <- data.table(df) # convert to data.table object
  setnames(dt, 1:2, c("x", "y"))

  # linear detection is always by "row" since linear detection is not dependent
  # on time but more on the stability of the data
  if (method == "linear") by <- "row"

  # determine absolute width value if fraction
  width <- calc_window(dt, width, by)

  if (method != "linear") {
    reg <- rolling_reg(dt, width, by, method)
    win <- reg$win
    roll <- reg$roll
  }
  if (method == "max") {
    rankroll <- reg$roll[order(rank(rate_b1))]  # order by size (from biggest)
  } else if (method == "min") {
    rankroll <- reg$roll[order(-rank(rate_b1))] # order by size (from smallest)
  } else if (method == "interval") {
    if (by == "row") {
      sequence <- seq(win, nrow(dt), win)  # generate the sequence interval
      # then subset the data based on the interval
      rankroll <- reg$roll[(seq(win, nrow(dt), win) - win + 1), ]
    } else if (by == "time") {
      sequence <- seq.int(min(dt[, x]), max(dt[, x]), by = win) # generate
      rankroll <- reg$roll[time %in% sequence] # subset
    }
  } else if (method == "linear") {
    kde <- kde_fit(dt, width, by, method, use = "all")  # use KDE technique
    # verify KDE technique:
    win <- floor(kde$win * .9)
    roll <- kde$roll # extract roll
    # double-check KDE:
    rankroll <- sapply(1:length(kde$subsets), function(x)
      kde_fit(kde$subsets[[x]], win, by, method, use = "top")$subsets)

    # perform calc_rate on each subset generated
    rankroll <- rbindlist(lapply(1:length(rankroll), function(xi)
      calc_rate(
        dt,
        from = head(rankroll[[xi]][[1]], 1),
        to = tail(rankroll[[xi]][[1]], 1),
        by = "time",
        plot = FALSE
      )$summary))
  }
  # add extra information for other methods that are not "linear"
  if (method != "linear") {
    # add row length and time length
    rankroll[, rowlength := endrow - row + 1][, timelength := endtime - time]
  }
  rankroll <- data.table(rankroll)
  # output
  out <- list(
    df = dt,
    width = win,
    by = by,
    method = method,
    roll = roll,
    summary = rankroll,
    rate = rankroll$rate_b1
  )
  if (method == "linear") {
    # append extra items to output if using linear method
    appendthis <- list(
      density = kde$density,
      peaks = kde$peaks,
      bandwidth = kde$bandwidth
    )
    out <- c(out, appendthis)
  }
  if (method == "linear") {
    message("\n", nrow(rankroll), " kernel density peaks detected and ranked.")
  }
  class(out) <- "auto_rate"
  if (plot) plot(out, label = FALSE, full = full)
  return(out)
}

#' @export
print.auto_rate <- function(x, pos = 1, ...) {
  cat("\n# auto_rate # ---------------------------\n")
  method <- x$method
  cat("Data is subset by", x$by, "using width of", x$width, "\n")
  cat(sprintf("Rates were computed using '%s' method\n", x$ method))
  if (method == "linear") {
    cat(
      nrow(x$summary),
      "linear regions detected in the kernel density estimate\n"
    )
  }

  if (method == "default") {
    cat("\nResult", pos, "of", nrow(x$summary), ":\n")
    cat("Rate:", x$summary$rate_b1[pos], "\n")
    cat("R.sq:", x$summary$rsq[pos])
  } else if (method %in% c("max", "min", "linear")) {
    cat("\nRank", pos, "of", nrow(x$summary), ":\n")
    cat("Rate:", x$summary$rate_b1[pos], "\n")
    cat("R.sq:", signif(x$summary$rsq[pos], 5), "\n")
    cat("Rows:", x$summary$row[pos], "to", x$summary$endrow[pos], "\n")
    cat("Time:", x$summary$time[pos], "to", x$summary$endtime[pos], "\n")
  } else if (method == "interval") {
    if (nrow(x$summary) > 5) {
      cat("\n=== Showing first 5 results of", nrow(x$summary), "===\n")
      print(x$summary[1:5])
    } else {
      cat("\n=== All", nrow(x$summary), "results of", nrow(x$summary), "===\n")
      print(x$summary)
    }
  }
  return(invisible(x)) # this lets us continue with dplyr pipes
}

#' @export
plot.auto_rate <- function(x, pos = 1, full = FALSE, choose = FALSE, label = TRUE, ...) {
  if (label) cat("\n# plot.auto_rate # ----------------------\n")
  if (!full) cat("You can select higher-resolution plots using argument `full = TRUE`. \n")
  # what method did auto_rate use?
  method <- x$method
  # extract data
  dta <- x$df
  dtbrange <- x$summary[pos]$row:x$summary[pos]$endrow
  dtb <- x$df[dtbrange]
  model <- lm(dtb[[2]]~dtb[[1]])
  if (method == "interval") {
    interval <- append(0, x$summary$endrow)
  } else interval <- NULL
  
  # generate plots
  p1 <- fullplot(dta, dtb, full, interval)
  p2 <- focusplot(dtb, full)
  if (!(method == "interval")) {
    p3 <- rollplot(x, pos, full)
    p4 <- drollplot(x, pos)
  }
  p5 <- residualplot(model, full)
  p6 <- qqplot(model, full)
  
  if (!choose & method == "interval") {
    pp <- gridExtra::arrangeGrob(p1, p2, p5, p6, nrow = 2)
    for(attempt in 1:10) {
      output <- tryCatch(
        expr={ plot(pp) },
        error=function(e) { return(e) }
      )
      if(!is(output, "error")) {
        break
      }
    }
  } else if (!choose) {
    pp <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6, nrow = 2)
    for(attempt in 1:10) {
      output <- tryCatch(
        expr={ plot(pp) },
        error=function(e) { return(e) }
      )
      if(!is(output, "error")) {
        break
      }
    }
  }
  
  # if user wants specific plots 
  if (choose == 1) {
    print(p1)
  } else if (choose == 2) {
    print(p2)
  } else if (choose == 3) {
    if (method == "interval") {
      stop("There is no rolling regression plot for interval analyis.")
    } else print(p3)
  } else if (choose == 4) {
    if (method == "interval") {
      stop("There is no kernel density plot for interval analyis.")
    } else print(p4)
  } else if (choose == 5) {
    print(p5)
  } else if (choose == 6) {
    print(p6)
  }
  if (label) cat("Done. \n")
  return(invisible(x))
}

# OLD PLOTTING FUNCTION USING BASE PLOT.
# Don't delete -- take as a reminder that this has been attempted before.
#
# plot.auto_rate <- function(x, pos = 1, choose = FALSE, label = TRUE, ...) {
#   if (label) cat("\n# plot.auto_rate # ----------------------\n")
#   # DEFINE OBJECTS
#   dt <- x$df
#   start <- x$summary$row[pos]
#   end <- x$summary$endrow[pos]
#   sdt <- dt[start:end]
#   rolldt <- data.table::data.table(x = x$roll$endtime, y = x$roll$rate)
#   rate <- x$summary$rate_b1[pos]
#   rsq <- signif(x$summary$rsq[pos],3)
#   fit <- lm(sdt[[2]] ~ sdt[[1]], sdt) # lm of subset
#   interval <- x$summary$endtime
#   startint <- min(interval) - x$width
#   dens <- x$density
#   peaks <- x$peaks[, 2:3]
# 
#   # PLOT BASED ON METHOD
#   if (x$method %in% c("default", "max", "min")) {
#     if (choose == FALSE) {
#       mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5), nrow = 2, byrow = TRUE)
#       pardefault <- par(no.readonly = T) # save original par settings
#       layout(mat)
#       par(mai = c(0.4, 0.4, 0.3, 0.3), ps = 10, cex = 1, cex.main = 1)
#       multi.p(dt, sdt)
#       sub.p(sdt, rsq = rsq)
#       rollreg.p(rolldt, rate)
#       residual.p(fit)
#       qq.p(fit)
#       layout(1)
#       par(pardefault) # revert par settings to original
#     }
#   } else if (x$method == "interval") {
#     if (choose == FALSE) {
#       pardefault <- par(no.readonly = T) # save original par settings
#       par(mfrow = c(2, 2), mai = c(.4, .4, .3, .3), ps = 10, cex = 1, cex.main = 1)
#       multi.p(dt, sdt)
#       abline(v = startint, lty = 3)
#       abline(v = interval, lty = 3)
#       sub.p(sdt, rsq = rsq)
#       residual.p(fit)
#       qq.p(fit)
#       par(pardefault) # revert par settings to original
#     }
#   } else if (x$method == "linear") {
#     if (choose == FALSE) {
#       pardefault <- par(no.readonly = T) # save original par settings
#       # par(mfrow = c(2, 3))  # replace par settings
#       par(mfrow = c(2, 3), mai = c(.4, .4, .3, .3), ps = 10, cex = 1, cex.main = 1)
#       multi.p(dt, sdt) # full timeseries with lmfit
#       sub.p(sdt, rsq = rsq) # closed-up (subset timeseries)
#       rollreg.p(rolldt, rate) # rolling regression series with markline
#       density.p(dens, peaks, pos) # density plot
#       residual.p(fit) # residual plot
#       qq.p(fit) # qq plot
#       par(pardefault) # revert par settings to original
#     }
#   }
# 
#   if (choose == 1) {
#     multi.p(dt, sdt) # full timeseries with lmfit
#     if (x$method == "interval") {
#       abline(v = startint, lty = 3)
#       abline(v = interval, lty = 3)
#     }
#   }
#   if (choose == 2) sub.p(sdt, rsq = rsq)  # subset plot
#   if (choose == 3) rollreg.p(rolldt, rate)  # rolling regression
#   if (choose == 4) density.p(dens, peaks, pos)  # density
#   if (choose == 5) residual.p(fit)  # residual plot
#   if (choose == 6) qq.p(fit)  #qq plot
# 
#   if (label) cat("Done.\n")
#   return(invisible(x))
# 
# }

#' @export
summary.auto_rate <- function(object, pos = NULL, export = FALSE, ...) {
  cat("Regressions :", nrow(object$roll))
  cat(" | Results :", nrow(object$summary))
  cat(" | Method :", object$method)
  cat(" | Roll width :", object$width)
  cat(" | Roll type :", object$by, "\n")

  if (object$method == "linear") {
    cat("\n=== Kernel Density ===")
    print(object$density)
  }
  if (is.null(pos)) {
    # if no row is specified, return all results
    cat("\n=== Summary of Results ===\n\n")
    print(data.table(object$summary))
    if (export) {
      return(invisible(data.table(object$summary)))
    } else return(invisible(object))
  } else {
    # otherwise, return row specified by `pos`
    cat("\n=== Summary of Ranked ",pos, "Result ===\n\n")
    print(data.table::data.table(object$summary)[pos])
    if (export) {
      return(invisible(data.table(object$summary)[pos]))
    } else return(invisible(object))
  }
}

#' Normal rolling regression
#'
#' This is an internal function.
#'
#' @importFrom roll roll_lm
#' @keywords internal
#' @export
static_roll <- function(df, win) {
  roll <- roll_lm(matrix(df[[1]]), matrix(df[[2]]), win)
  roll <- na.omit(data.table(cbind(roll$coefficients, roll$r.squared)))
  setnames(roll, 1:3, c("intercept_b0", "rate_b1", "rsq"))
  return(roll)
}


#' Perform time-width rolling regression
#'
#' This is an internal function. Used by [auto_rate()].
#'
#' @keywords internal
#' @export
time_roll <- function(dt, width, parallel = FALSE) {
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("V1", "V2"))

  # The cutoff specifies where to stop the rolling regression, based on width
  time_cutoff <- max(dt[,1]) - width
  row_cutoff <- max(dt[, which(V1 <= time_cutoff)])

  # Parallel is unstable on some machines tested. Not sure why (was good before)
  # Let's disable it while I debug the issues...
  # TODO: implement new parallelisation code that works

  # if (parallel) {
  #   no_cores <- parallel::detectCores()  # calc the no. of cores available
  #   if (os() == "win") {
  #     cl <- parallel::makeCluster(no_cores)
  #   } else cl <- parallel::makeCluster(no_cores, type = "FORK")
  #   parallel::clusterExport(cl, "time_lm")
  #   out <- parallel::parLapply(cl, 1:row_cutoff, function(x) time_lm(dt,
  #     dt[[1]][x], dt[[1]][x] + width))
  #   parallel::stopCluster(cl)  # stop cluster (release cores)
  # } else out <- lapply(1:row_cutoff, function(x) time_lm(dt,
  #   dt[[1]][x], dt[[1]][x] + width))
  if (parallel) NULL
  if (!parallel) NULL

  out <- lapply(1:row_cutoff, function(x) time_lm(dt,
    dt[[1]][x], dt[[1]][x] + width))
  out <- data.table::rbindlist(out)
  return(out)
}


#' Subset data by time and perform a linear regression.
#'
#' This is an internal function. Used with [time_roll()] and [auto_rate()].
#'
#' @keywords internal
#' @export
time_lm <- function(df, start, end) {
  dt <- data.table::data.table(df)
  data.table::setnames(dt, 1:2, c("x", "y"))
  sdt <- dt[x >= start & x <= end]
  fit <- .lm.fit(cbind(1, sdt[[1]]), sdt[[2]])  # perform lm
  coef <- coef(fit)
  intercept_b0 <- coef[1]
  rate_b1 <- coef[2]
  # Calculate coef of determination (rsq) manually
  ybar <- sdt[, mean(y)]
  sst <- sum(sdt[, (y-ybar)*(y-ybar)])
  ssr <- sum((fit$residuals)*(fit$residuals))
  rsq <- 1-(ssr/sst)
  out <- data.table::data.table(intercept_b0, rate_b1, rsq)
  return(out)
}


#' Rolling regression
#'
#' @keywords internal
#' @export
rolling_reg <- function(dt, width, by, method) {
  win <- calc_window(dt, width, by)
  # Rolling regression
  if (by == "time") {
    roll <- time_roll(dt, win, parallel = FALSE) # TODO: fix parallel here
  } else if (by == "row") {
    roll <- static_roll(dt, win)
  }

  # Attach row and time indices to the roll data
  if (by == "time") {
    roll[, row := seq_len(.N)]
    endrow <- sapply(dt[roll[, row], x], function(z)
      max(dt[, which(x <= z + win)]))
    roll[, endrow := endrow]
    roll[, time := dt[roll[, row], x]]
    roll[, endtime := dt[roll[, endrow], x]]
  } else if (by == "row") {
    roll[, row := seq_len(.N)]
    roll[, endrow := roll[, row] + win - 1]
    roll[, time := dt[roll[, row], x]]
    roll[, endtime := dt[roll[, endrow], x]]
  }
  out <- list(roll = data.table(roll), win = win)
  return(out)
}

#' Kernel density estimation and fitting
#'
#' @keywords internal
#' @export
kde_fit <- function(dt, width, by, method, use = "all") {
  # perform rolling regression
  reg <- rolling_reg(dt, width, by, method)
  roll <- reg$roll
  win <- reg$win

  # If entire width of data frame is selected, there's nothing to detect!
  if (nrow(roll) == 1) {
    subsets <- list(truncate_data(dt, roll$row, roll$endrow, "row"))
    d <- NULL
    peaks <- roll$rate_b1
    bw <- NULL

  } else {
    d <- density(roll$rate_b1, na.rm = T, bw = "SJ-ste", adjust = .95) # KDE
    bw <- d$bw
    peaks <- which(diff(sign(diff(d$y))) == -2) + 1 # index modes
    peaks <- lapply(peaks, function(x)
      data.table(index = x, peak_b1 = d$x, density = d$y)[x, ]) # match to roll
    if (use == "all") {
      peaks <- rbindlist(peaks)[order(-rank(density))] # rank by size
    } else {
      peaks <- rbindlist(peaks)[order(-rank(density))][1]
    }
    # match peaks to roll data
    frags <- lapply(
      peaks$peak_b1,
      function(x) roll[rate_b1 <= (x + d$bw*.95)][rate_b1 >= (x - d$bw*.95)]
    )
    # split non-overlapping rolls by window size
    if (by == "row") {
      frags <- lapply(1:length(frags), function(x) split(
        frags[[x]], c(0, cumsum(abs(diff(frags[[x]]$row)) > win))
      ))
    } else {
      frags <- lapply(1:length(frags), function(x) split(
        frags[[x]], c(0, cumsum(abs(diff(frags[[x]]$time)) > win))
      ))
    }
    # select longest fragments
    i <- sapply(1:length(frags), function(x) which.max(sapply(frags[[x]], nrow)))
    frags <- unname(mapply(function(x, y) frags[[x]][y], 1:length(frags), i))
    frags <- frags[sapply(frags, nrow) > 0] # remove zero-length data
    # Convert fragments to subsets
    subsets <- lapply(1:length(frags), function(x)
      truncate_data(
        dt, min(frags[[x]]$row),
        max(frags[[x]]$endrow), "row"
      ))
  }
  out <- list(
    win = win, roll = roll, subsets = subsets, density = d,
    peaks = peaks, bandwidth = bw
  )
  return(out)
}

#' Calculate rolling window size for rolling regression
#'
#' @keywords internal
#' @export
calc_window <- function(dt, width, by) {
  # Determine rolling window from `width`
  if (is.null(width)) {
    width <- .2
    if (by == "time") win <- floor(width * max(dt[[1]]))
    if (by == "row") win <- floor(width * nrow(dt))
  } else if (is.numeric(width)) {
    if (width <= 1) {
      if (by == "time") win <- floor(width * max(dt[[1]]))
      if (by == "row") win <- floor(width * nrow(dt))
    } else if (width > 1) win <- width
  } else {
    stop("'width' must be numeric.")
  }
  return(win)
}

