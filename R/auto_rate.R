#' auto.rate version 2
#'
#' @param df NULL
#' @param width NULL
#' @param by NULL
#' @param method NULL
#'
#' @return NULL
#' @import data.table
#' @import parallel
#' @export
#'
#' @examples NULL
auto_rate <- function(df, width = NULL, by = "time", method = "default",
  plot = TRUE) {
  tic()  # start time
  # Input checks
  if (!by %in% c("time", "row"))
    stop("Invalid 'by' input value")
  if (!method %in% c("default", "linear", "max", "min", "interval"))
    stop("Invalid `method` input value")
  if (is.null(width)) {
    if (by == "time") {
      width <- floor(0.2 * max(df[[1]]))
    } else if (by == "row") {
      width <- floor(0.2 * nrow(df))
    }
  }

  # FORMAT INPUTS
  dt <- data.table::data.table(df)
  data.table::setnames(dt, 1:2, c("x", "y"))

  # DETERMINE ROLL STYLE
  if (by == "time") {
    roll <- time.roll(dt, width)
  } else if (by == "row") {
    roll <- static.roll(dt, width)
  }

  # ATTACH ROW AND TIME INDEX
  if (by == "time") {
    roll[, `:=`(row, seq_len(.N))]  # first index by row
    # extract end rows by time
    endrow <- sapply(dt[roll[, row], x], function(z) max(dt[,
      which(x <= z + width)]))
    roll[, `:=`(endrow, endrow)]
    roll[, `:=`(time, dt[roll[, row], x])][, `:=`(endtime,
      dt[roll[, endrow], x])]

  } else if (by == "row") {
    roll[, `:=`(row, seq_len(.N))][, `:=`(endrow, roll[,
      row] + width - 1)]
    roll[, `:=`(time, dt[roll[, row], x])][, `:=`(endtime,
      dt[roll[, endrow], x])]
  }

  # PROCESS BY METHOD
  if (method == "default") {
    result <- roll
  } else if (method == "linear") {
    kdefit <- kde.fit(dt, roll, width, by)
    result <- kdefit$result
  } else if (method == "min") {
    result <- roll[order(-rank(rate))]
  } else if (method == "max") {
    result <- roll[order(rate)]
  } else if (method == "interval") {
    if (by == "row") {
      sequence <- seq(width, nrow(dt), width)
      result <- roll[(seq(width, nrow(dt), width) - width +
        1), ]
    } else if (by == "time") {
      # generate time-based interval:
      sequence <- seq.int(min(dt[, x]), max(dt[, x]), by = width)
      # extract the intervals:
      result <- roll[time %in% sequence]
    }
  }

  # GENERATE OUTPUT
  if (method != "linear") {
    # add row length and time length if NOT using linear method
    result[, `:=`(row.len, endrow - row + 1)][, `:=`(time.len,
      endtime - time)]
  }
  out <- list(df = dt, width = width, by = by, method = method,
              roll = roll, summary = result, rate = result$rate)
  if (method == "linear") {
    # append extra items to output if using linear method
    append <- list(density = kdefit$density, peaks = kdefit$peaks,
                   bandwidth = kdefit$bandwidth)
    out <- c(out, append)
  }
  iters <- nrow(dt) - max(dt[, which(x <= width)]) + 1
  elapsed <- round(toc(), 2)
  message(iters, " regressions performed in ", elapsed, " seconds")
  class(out) <- "auto_rate"
  return(out)
}




#' @export
print.auto_rate <- function(x, pos = 1) {
  method <- x$method
  cat("Data is subset by", x$by, "using width of", x$width, "\n")
  cat(sprintf("Rates were computed using '%s' method\n", x$ method))
  if (method == "linear")
    cat(nrow(x$summary), "linear regions detected in the kernel density estimate\n")

  if (method == "default") {

    cat("\n=== Result", pos, "of", nrow(x$summary), "===\n")
    cat("Rate:", x$summary$rate[pos], "\n")
    cat("R.sq:", x$summary$rsq[pos])

  } else if (method %in% c("max", "min", "linear")) {

    cat("\n=== Rank", pos, "of", nrow(x$summary), "===\n")
    cat("Rate:", x$summary$rate[pos], "\n")
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
  cat("\nSee also `summary()` and `plot()`\n")
}






#' @export
summary.auto_rate <- function(x) {
  cat("Regressions :", nrow(x$roll), "\n")
  cat("    Results :", nrow(x$summary), "\n")
  cat("     Method :", x$method, "\n")
  cat(" Roll width :", x$width, "\n")
  cat("  Roll type :", x$by, "\n\n")

  cat("=== Summary of Results ===\n\n")
  print(x$summary)

  if(x$method == "linear") {
    cat("\n=== Kernel Density Stats ===\n")
    print(x$density)
  }
}







#' @export
plot.auto_rate <- function(x, pos = 1) {
  # DEFINE OBJECTS
  dt <- x$df
  start <- x$summary$row[pos]
  end <- x$summary$endrow[pos]
  sdt <- dt[start:end]
  rolldt <- data.table::data.table(x = x$roll$endtime, y = x$roll$rate)
  rate <- x$summary$rate[pos]
  fit <- lm(sdt[[2]] ~ sdt[[1]], sdt)  # lm of subset

  # PLOT BASED ON METHOD
  if (x$method %in% c("default", "max", "min")) {

    mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5), nrow = 2, byrow = TRUE)
    layout(mat)
    multi.p(dt, sdt)
    sub.p(sdt)
    rollreg.p(rolldt, rate)
    residual.p(fit)
    qq.p(fit)
    layout(1)

  } else if (x$method == "interval") {

    interval <- x$summary$endtime
    startint <- min(interval) - x$width

    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = c(2, 2))  # replace par settings
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
    par(mfrow = c(2, 3))  # replace par settings
    multi.p(dt, sdt)  # full timeseries with lmfit
    sub.p(sdt)  # closed-up (subset timeseries)
    rollreg.p(rolldt, rate)  # rolling regression series with markline
    density.p(dens, peaks, pos) # density plot
    residual.p(fit) # residual plot
    qq.p(fit) # qq plot
    par(pardefault)  # revert par settings to original

  }
}








# Subset data by time and perform a linear regression. Used
# in conjunction with time.roll
time.lm <- function(df, start, end) {
  names(df) <- c("x", "y")
  dt <- data.table::data.table(df)
  sdt <- dt[x >= start & x <= end]
  fit <- .lm.fit(cbind(1, sdt[[1]]), sdt[[2]])  # perform lm
  coef <- coef(fit)
  intercept <- coef[1]
  rate <- coef[2]
  # Calculate coef of determination (rsq) manually
  ybar <- sdt[, mean(y)]
  sst <- sum(sdt[, (y-ybar)*(y-ybar)])
  ssr <- sum((fit$residuals)*(fit$residuals))
  rsq <- 1-(ssr/sst)
  out <- data.table::data.table(intercept, rate, rsq)
  return(out)
}






# Perform time-width rolling regression
time.roll <- function(dt, width) {
  # Use parallel if df > 500 rows
  cutoff <- nrow(dt) - max(dt[, which(x <= width)]) + 1
  if (cutoff > 500) {
    no_cores <- parallel::detectCores()  # calc the no. of cores available
    if (os() == "win") {
      cl <- parallel::makeCluster(no_cores)
    } else cl <- parallel::makeCluster(no_cores, type = "FORK")
    parallel::clusterExport(cl, "time.lm")
    out <- parallel::parLapply(cl, 1:cutoff, function(x) time.lm(dt,
      dt[[1]][x], dt[[1]][x] + width))
    parallel::stopCluster(cl)  # stop cluster (release cores)
  } else {
    out <- lapply(1:cutoff, function(x) time.lm(dt, dt[[1]][x],
      dt[[1]][x] + width))
  }
  out <- data.table::rbindlist(out)
  return(out)
}







# Perform kernel density estimate and fitting
kde.fit <- function(dt, roll, width, by) {
  bw <- "SJ-ste"
  dens <- density(roll$rate, na.rm = T, bw = bw, n = length(roll$rate))
  # Identify peaks
  peaks <- which(diff(sign(diff(dens$y))) == -2) + 1
  # Match peaks to roll data, and order by density size
  match.peaks <- lapply(peaks, function(x) data.table::data.table(index = x,
    peak.b1 = dens$x, density = dens$y)[x, ])
  match.peaks <- data.table::rbindlist(match.peaks)[order(-rank(density))] # ok so far
  # Use kde bandwidth to identify matching rate values
  bin <- dens$bw * 0.3
  match.regs <- lapply(match.peaks$peak.b1, function(x) roll[rate <= (x + bin)][rate >= (x - bin * 0.3)])
  # Make sure that the data is continuous. If not, split into
  # fragments
  if (by == "time") {
    match.raw <- lapply(1:length(match.regs), function(x) split(match.regs[[x]],
      c(0, cumsum(abs(diff(match.regs[[x]]$time)) > width))))
  } else if (by == "row") {
    match.raw <- lapply(1:length(match.regs), function(x) split(match.regs[[x]],
      sc(0, cumsum(abs(diff(match.regs[[x]]$row)) > width))))
  }
  # Perform lm on each fragment
  raw.frags <- unname(unlist(match.raw, recursive = F))
  raw.frags <- raw.frags[sapply(raw.frags, nrow) > 0] # remove zero-length matches
  result <- data.table::rbindlist(lapply(1:length(raw.frags),
    function(x) calc.rate(df = dt, from = min(raw.frags[[x]]$time),
      to = max(raw.frags[[x]]$endtime), by = "time", plot = FALSE,
      verbose = FALSE)$results))
  out <- list(density = dens, peaks = match.peaks, bandwidth = bin,
    result = result)
  return(out)
}

