#' Automatically determine rate of change in oxygen concentration over time
#'
#' `auto_rate` automatically performs a rolling regression on a data frame to
#' perform determinations of maximum, minimum, interval or "best fit" linear
#' rate of change in oxygen concentration over time. First, a rolling regression
#' of specified `width` is performed on the entire dataset to obtain all
#' possible values. The computations are then ranked (or, arranged), based on
#' the "`logic`" argument, and the output is summarised.
#'
#' **Units** There are no units of measurements involved in `auto_rate`. This is
#' a deliberate decision. Units are called in a later function when volume-
#' and/or weight-specific rates of oxygen concentration are computed in
#' [convert_rate()] and [convert_DO()].
#'
#'
#' ***Ranking algorithms*** For now, `auto_rate()` contains four ranking
#' algorithms that can be called with the argument "`logic`":
#'
#' - `max`: regressions are arranged from highest absolute values, to the
#' lowest.
#'
#' - `min`: regressions are arranged from lowest absolute values, to the
#' highest.
#'
#' - `interval`: non-overlapping regressions are extracted from the rolled
#' regrssions. They are not ranked.
#'
#' - `linear`: Buses kernel density estimation to detect the most "linear"
#' sections of the timeseries in descending order.
#'
#' @param df data frame.
#' @param width numeric. Defaults to 25 percent of width if NULL.
#' @param by string. "row" or "time". Defaults to "row".
#' @param method string. "linear", "max", "min" or "interval".
#' @param plot logical. Defaults to TRUE. Automatically plot the results.
#' @param parallel logical. Defaults to TRUE. Should parallel processes be
#'   implemented to speed things up?
#'
#' @return A list object of class `auto_rate`.
#'
#' @import data.table
#' @import parallel
#' @export
#'
#' @examples
#' auto_rate(sardine.rd, parallel = FALSE)
auto_rate <- function(df, width = NULL, by = "row", method = "linear",
  plot = TRUE, parallel = TRUE) {
  tic()  # start time

  # Import from previous function(s)
  if(any(class(df) %in% "inspect_data")) df <- df$df

  # Input checks
  if(!is.data.frame(df))
    stop("`df` must be data frame, or an object of class `inspect_data`.")

  if (!by %in% c("time", "row")) stop("Invalid 'by' input value")
  if (!method %in% c("default", "linear", "max", "min", "interval"))
    stop("Invalid `method` input value.")

  # Convert time to numeric
  if (any(class(df[[1]]) %in% c("POSIXct", "POSIXt"))) {
    df <- data.frame(V1 = as.numeric(df[[1]]) - min(as.numeric(df[[1]])),
                     V2 = df[[2]])
  }

  # Check if time is uneven - if it is, give warning
  uneven <- test_space(df[[1]])
  if (uneven$check && by == "row")
    warning("Time data is irregular. Please use `by = 'time'.", call. = F)

  # Generate default width for rolling regression. This also applies if
  # "linear" method sis selected.
  if (is.null(width) | method == "linear") {
    if (by == "time") width <- floor(0.2 * max(df[[1]]))
    if (by == "row") width <- floor(0.2 * nrow(df))
  }

  # Format the data
  dt <- data.table::data.table(df)
  data.table::setnames(dt, 1:2, c("x", "y"))

  # Check if we are doing rolling regressions by row (fast) or time (slower).
  if (by == "time") {
    roll <- time_roll(dt, width, parallel)
  } else if (by == "row") {
    roll <- static_roll(dt, width)
  }

  # Attach row and time indices to the roll data.
  if (by == "time") {
    roll[, row := seq_len(.N)]
    # extract end rows by time
    endrow <- sapply(dt[roll[, row], x], function(z) max(dt[,
      which(x <= z + width)]))
    roll[, endrow := endrow]
    roll[, time := dt[roll[, row], x]]
    roll[, endtime := dt[roll[, endrow], x]]

  } else if (by == "row") {
    roll[, row := seq_len(.N)]
    roll[, endrow := roll[, row] + width - 1]
    roll[, time := dt[roll[, row], x]]
    roll[, endtime := dt[roll[, endrow], x]]
  }

  # Process the data based on "method".
  if (method == "default") {
    result <- roll
  } else if (method == "linear") {
    kdefit <- kde_fit(dt, roll, width, by)
    result <- kdefit$result
  } else if (method == "min") {
    result <- roll[order(-rank(rate_b1))]
  } else if (method == "max") {
    result <- roll[order(rate_b1)]
  } else if (method == "interval") {
    if (by == "row") {
      sequence <- seq(width, nrow(dt), width)
      result <- roll[(seq(width, nrow(dt), width) - width + 1), ]
    } else if (by == "time") {
      # generate time-based interval:
      sequence <- seq.int(min(dt[,x]), max(dt[,x]), by = width)
      # extract the intervals:
      result <- roll[time %in% sequence]
    }
  }

  # Generate output
  if (method != "linear") {
    # add row length and time length if NOT using linear method
    result[, `:=`(row.len, endrow - row + 1)][, `:=`(time.len,
      endtime - time)]
  }

  out <- list(df = dt,
    width   = width,
    by      = by,
    method  = method,
    roll    = roll,
    summary = result,
    rate    = result$rate_b1)

  if (method == "linear") {
    # append extra items to output if using linear method
    appendthis <- list(
      density = kdefit$density,
      peaks = kdefit$peaks,
      bandwidth = kdefit$bandwidth)
    out <- c(out, appendthis)
  }

  elapsed <- round(toc(), 2)
  if (method == "linear")
    message(nrow(result), " kernel density peaks detected and ranked.")
  class(out) <- "auto_rate"
  if (plot) plot(out)
  return(out)
}



#' @export
print.auto_rate <- function(x, pos = 1, ...) {
  method <- x$method
  cat("Data is subset by", x$by, "using width of", x$width, "\n")
  cat(sprintf("Rates were computed using '%s' method.\n", x$ method))
  if (method == "linear")

    cat(nrow(x$summary),
      "linear regions detected in the kernel density estimate.\n")

  if (method == "default") {

    cat("\n=== Result", pos, "of", nrow(x$summary), "===\n")
    cat("Rate:", x$summary$rate_b1[pos], "\n")
    cat("R.sq:", x$summary$rsq[pos])

  } else if (method %in% c("max", "min", "linear")) {

    cat("\n=== Rank", pos, "of", nrow(x$summary), "===\n")
    cat("Rate:", x$summary$rate_b1[pos], "\n")
    cat("R.sq:", x$summary$rsq[pos], "\n")
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
  # cat("\n\nSee also `summary()` and `plot()`\n")
}






#' @export
summary.auto_rate <- function(object, ...) {
  cat("Regressions :", nrow(object$roll))
  cat(" | Results :", nrow(object$summary))
  cat(" | Method :", object$method)
  cat(" | Roll width :", object$width)
  cat(" | Roll type :", object$by, "\n")

  if(object$method == "linear") {
    cat("\n=== Kernel Density ===")
    print(object$density)
  }

  cat("\n=== Summary of Results ===\n\n")
  print(data.table::data.table(object$summary))

  return(invisible(object$summary))
}







#' @export
plot.auto_rate <- function(x, pos = 1, ...) {
  # DEFINE OBJECTS
  dt <- x$df
  start <- x$summary$row[pos]
  end <- x$summary$endrow[pos]
  sdt <- dt[start:end]
  rolldt <- data.table::data.table(x = x$roll$endtime, y = x$roll$rate)
  rate <- x$summary$rate_b1[pos]
  fit <- lm(sdt[[2]] ~ sdt[[1]], sdt)  # lm of subset

  # PLOT BASED ON METHOD
  if (x$method %in% c("default", "max", "min")) {

    mat <- matrix(c(1,1,1, 2,2,2, 3,3, 4,4, 5,5), nrow = 2, byrow = TRUE)
    pardefault <- par(no.readonly = T)  # save original par settings
    layout(mat)
    par(mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
    multi.p(dt, sdt)
    sub.p(sdt)
    rollreg.p(rolldt, rate)
    residual.p(fit)
    qq.p(fit)
    layout(1)
    par(pardefault)  # revert par settings to original


  } else if (x$method == "interval") {

    interval <- x$summary$endtime
    startint <- min(interval) - x$width

    pardefault <- par(no.readonly = T)  # save original par settings
    # par(mfrow = c(2, 2))  # replace par settings
    par(mfrow = c(2, 2), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
    multi.p(dt, sdt)
    abline(v = startint, lty = 3)
    abline(v = interval, lty = 3)
    sub.p(sdt)
    residual.p(fit)
    qq.p(fit)
    par(pardefault)  # revert par settings to original

  } else if (x$method == "linear") {

    dens <- x$density
    peaks <- x$peaks[, 2:3]

    pardefault <- par(no.readonly = T)  # save original par settings
    # par(mfrow = c(2, 3))  # replace par settings
    par(mfrow = c(2, 3), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
    multi.p(dt, sdt)  # full timeseries with lmfit
    sub.p(sdt)  # closed-up (subset timeseries)
    rollreg.p(rolldt, rate)  # rolling regression series with markline
    density.p(dens, peaks, pos) # density plot
    residual.p(fit) # residual plot
    qq.p(fit) # qq plot
    par(pardefault)  # revert par settings to original

  }
}


#' Normal rolling regression
#'
#' This is an internal function.
#'
#' @keywords internal
#' @export
static_roll <- function(df, width) {
  x <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]), width)
  out <- na.omit(cbind(x$coefficients,x$r.squared))
  out <- data.table::data.table(out)
  data.table::setnames(out, 1:3, c("intercept_b0", "rate_b1", "rsq"))
  return(out)
}




#' Perform time-width rolling regression
#'
#' This is an internal function. Used by [auto_rate()].
#' @keywords internal
#' @export
time_roll <- function(dt, width) {
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("V1", "V2"))
  # Use parallel if df > 500 rows
  # The cutoff specifies where to stop the rolling regression, based on width
  time_cutoff <- max(dt[,1]) - width
  row_cutoff <- max(dt[, which(V1 <= time_cutoff)])
  # if (cutoff > 500) {
    no_cores <- parallel::detectCores()  # calc the no. of cores available
    if (os() == "win") {
      cl <- parallel::makeCluster(no_cores)
    } else cl <- parallel::makeCluster(no_cores, type = "FORK")
    parallel::clusterExport(cl, "time_lm")
    out <- parallel::parLapply(cl, 1:row_cutoff, function(x) time_lm(dt,
      dt[[1]][x], dt[[1]][x] + width))
    parallel::stopCluster(cl)  # stop cluster (release cores)
  # } else {
  #   out <- lapply(1:row_cutoff, function(x) time_lm(dt, dt[[1]][x],
  #     dt[[1]][x] + width))
  # }
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




#' Perform kernel density estimate and fitting
#'
#' This is an internal function and is used by [auto_rate()].
#'
#' @keywords internal
#' @export
kde_fit <- function(dt, roll, width, by) {
  dt <- data.table::data.table(dt)
  bw <- "nrd0" # "nrd"  "ucv"  "bcv"  "SJ-ste"  "SJ-dpi"
  dens <- density(roll$rate_b1, na.rm = T, bw = bw, n = length(roll$rate_b1))
  # Identify peaks
  peaks <- which(diff(sign(diff(dens$y))) == -2) + 1
  # Match peaks to roll data, and order by density size
  match.peaks <- lapply(peaks, function(x) data.table::data.table(index = x,
    peak.b1 = dens$x, density = dens$y)[x, ])
  match.peaks <- data.table::rbindlist(match.peaks)[order(-rank(density))] # ok so far
  # Use kde bandwidth to identify matching rate values
  bin <- dens$bw * .09
  match.regs <- lapply(match.peaks$peak.b1, function(x) roll[rate_b1 <= (x + bin)][rate_b1 >= (x - bin * 0.3)])
  # Make sure that the data is continuous. If not, split into
  # fragments
  if (by == "time") {
    match.raw <- lapply(1:length(match.regs), function(x) split(match.regs[[x]],
      c(0, cumsum(abs(diff(match.regs[[x]]$time)) > width))))
  } else if (by == "row") {
    match.raw <- lapply(1:length(match.regs), function(x) split(match.regs[[x]],
      c(0, cumsum(abs(diff(match.regs[[x]]$row)) > width))))
  }

  # Obtain rolling fragments
  raw.frags <- unname(unlist(match.raw, recursive = F))
  raw.frags <- raw.frags[sapply(raw.frags, nrow) > 0] # remove zero-length matches

  # Convert fragments to subsets
  subsets <- lapply(1:length(raw.frags), function(x)
    subset_data(dt, min(raw.frags[[x]]$row), max(raw.frags[[x]]$endrow), "row"))

  # Perform lm on each subset
  lapply(1:length(subsets), function(z)
    calc_rate(subsets[[z]], by = "row", plot = F))

  result <- data.table::rbindlist(lapply(1:length(raw.frags),
    function(z) calc_rate(dt, from = min(raw.frags[[z]]$time),
      to = max(raw.frags[[z]]$endtime), by = "time", plot = F)$summary))

  # Generate output
  out <- list(density = dens, peaks = match.peaks, bandwidth = bin,
    result = result)
  return(out)
}

