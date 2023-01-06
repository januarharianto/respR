#' Validation function for auto_rate
#'
#' This is an internal function for `auto_rate()`. Used to validate inputs
#'
#' @param df data.frame object.
#' @param by string.
#' @param method string.
#'
#' @return a data.frame object of the original data
#'
#' @keywords internal
validate_auto_rate <- function(x, by, method) {

  # x validation
  if (!(any(class(x) %in% c("inspect", "data.frame"))))
    stop("auto_rate: Input data must be of class 'data.frame' or 'inspect'")

  # validate by
  by <- verify_by(by,  which = c("t", "r"), msg = "auto_rate:") # validate `by` input

  # validate method
  if (!(method %in% c("linear", "max", "min", "interval",
                      "rolling", "highest", "lowest", "maximum", "minimum")))
    stop("auto_rate: The 'method' input is not recognised: it should be 'linear',
    'highest', 'lowest', 'maximum', 'minimum', 'rolling', or 'interval'")

  # extract df
  if (any(class(x) %in% "inspect")) df <- x$dataframe else
    df <- x

  # select only first two columns by default if dataset is multi-column
  if (length(df) > 2) {
    warning("auto_rate: Multi-column dataset detected in input. Selecting first two columns by default.\n  If these are not the intended data, inspect() or subset the data frame columns appropriately before running auto_rate()")
    df <- df[, 1:2]
  }

  out <- list(by = by,
              df = data.table(df))

  return(out)
}


#' Perform rolling regression and rank from NUMERICAL minimum to maximum
#' i.e. includes sign - most negative rates are highest
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#'
#' @return a list object with appended class `auto_rate_min`
#' @keywords internal
auto_rate_min <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # message if mix of -ve and +ve
  if(any(rollreg$slope_b1 > 0) && any(rollreg$slope_b1 < 0))
    message("auto_rate: Note dataset contains both negative and positive rates. Ensure ordering 'method' is appropriate.")

  # order data by size, from biggest
  results <- rollreg[order(rank(slope_b1))]
  out <- list(roll = rollreg, results = results)
  class(out) <- append(class(out), "auto_rate_min")
  return(out)
}

#' Perform rolling regression and rank from NUMERICAL maximum to minimum
#' i.e. includes sign - most positive rates are highest
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#'
#' @return a list object with appended class `auto_rate_max`
#' @keywords internal
auto_rate_max <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # message if mix of -ve and +ve
  if(any(rollreg$slope_b1 > 0) && any(rollreg$slope_b1 < 0))
    message("auto_rate: Note dataset contains both negative and positive rates. Ensure ordering 'method' is appropriate.")

  # order data by size, from biggest
  results <- rollreg[order(-rank(slope_b1))]
  out <- list(roll = rollreg, results = results)
  class(out) <- append(class(out), "auto_rate_max")
  return(out)
}

#' Perform rolling regression and rank from ABSOLUTE highest to lowest
#'
#' i.e. ignores sign. should only be used when rates are all negative or all
#' positive
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#'
#' @return a list object with appended class `auto_rate_highest`
#' @keywords internal
auto_rate_highest <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # stop if mix of -ve and +ve
  if(any(rollreg$slope_b1 > 0) && any(rollreg$slope_b1 < 0))
    stop("auto_rate: Analysis produces both negative and positive rates. \n The 'highest' method is intended to order by the lowest *absolute* rate amongst rates all having the same sign.\n Use 'maximum' or 'minimum' method to order rates by *numerical* value.")
  # order data by absolute value, from highest
  results <- rollreg[order(-rank(abs(rollreg$slope_b1)))] ## note abs() operation
  out <- list(roll = rollreg, results = results)
  class(out) <- append(class(out), "auto_rate_highest")
  return(out)
}

#' Perform rolling regression and rank from ABSOLUTE lowest to highest
#'
#' i.e. ignores sign. should only be used when rates are all negative or all
#' positive
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#'
#' @return a list object with appended class `auto_rate_lowest`
#' @keywords internal
auto_rate_lowest <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # stop if mix of -ve and +ve
  if(any(rollreg$slope_b1 > 0) && any(rollreg$slope_b1 < 0))
    stop("auto_rate: Analysis produces both negative and positive rates. \n The 'lowest' method is intended to order by the lowest *absolute* rate amongst rates all having the same sign.\n Use 'maximum' or 'minimum' method to order rates by *numerical* value.")

  # order data by absolute value, from lowest
  results <- rollreg[order(rank(abs(rollreg$slope_b1)))] ## note abs() operation
  out <- list(roll = rollreg, results = results)
  class(out) <- append(class(out), "auto_rate_lowest")
  return(out)
}


#' Perform rolling regression of fixed width and do not reorder results
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#'
#' @return a list object with appended class `auto_rate_rolling`
#' @keywords internal
auto_rate_rolling <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # message if mix of -ve and +ve
  if(any(rollreg$slope_b1 > 0) && any(rollreg$slope_b1 < 0))
    message("auto_rate: Note dataset contains both negative and positive rates. Ensure ordering 'method' is appropriate.")

  # DO NOT reorder data
  results <- rollreg
  out <- list(roll = rollreg, results = results)
  class(out) <- append(class(out), "auto_rate_rolling")
  return(out)
}


#' Obtain rate values at non-overlapping intervals of a dataset
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#'
#' @return a list object with appended class `auto_rate_interval`
#' @keywords internal
auto_rate_interval <- function(dt, width, by) {

  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
    sequence <- seq(width, nrow(dt), width)
    results <- rollreg[sequence - width + 1]
  }

  if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
    sequence <- seq.int(min(dt[, 1]), max(dt[, 1]), width)
    results <- rollreg[time %in% sequence]
  }
  out <- list(roll = rollreg, results = results)
  class(out) <- append(class(out), "auto_rate_interval")
  return(out)
}


#' Linear detection method
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param verify logical. Should KDE be performed again to verify the detection?
#'
#' @return a list object
#' @keywords internal
auto_rate_linear <- function(dt, width, by, verify = TRUE) {

  # Convert 'width' to rows if it is 'by = "time"' since kernel_method only
  # takes row width
  if(by == "time") width <- floor(width/diff(range(nainf.omit(dt[[1]]))) * nrow(dt))

  # run kde
  kde <- kernel_method(dt, width)
  subsets <- kde$subsets

  # run verification step if necessary
  if (verify) {
    testwin <- floor(width * .85)
    subsets <- sapply(1:length(subsets), function(z)
      kernel_method(subsets[[z]], testwin, top_only = TRUE)$subsets)
  }

  # perform calc_rate on each subset generated
  output <- rbindlist(lapply(1:length(subsets), function(xi)
    calc_rate(
      dt,
      from = head(subsets[[xi]][[1]], 1),
      to = tail(subsets[[xi]][[1]], 1),
      by = "time",
      plot = FALSE
    )$summary))
  # reorder
  # remove rep, rank, rate2pt columns
  results <- output[,3:11]

  out <- list(results = results, roll = kde$rollreg,
              density = kde$density, peaks = kde$peaks)

  class(out) <- append(class(out), "auto_rate_linear")
  return(out)
}

#' Kernel density function
#'
#' This is an internal function for `auto_rate()`
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param top_only logical. Should only top ranked result be returned?
#'
#' @return A list object
#' @keywords internal
kernel_method <- function(dt, width, top_only = FALSE) {
  # linear detection is always by row since linear detection is not dependent
  # on time but more on the stability of the data
  rollreg <- rolling_reg_row(dt, width)

  # perform kernel density estimate
  d <- density(rollreg$slope_b1, na.rm = T, bw = "SJ-ste", adjust = .95)
  # extract bandwidth
  bw <- d$bw
  # identify peaks in kernel density:
  # THIS CAN FAIL TO FIND PEAKS
  # In which case auto_rate fails with obscure error:
  # Error: object of type 'closure' is not subsettable
  # BUT - apparently fixed/does not occur in R 4.2
  # Seen with small datasets only so far, so not sure any point trying to debug
  peaks <- which(diff(sign(diff(d$y))) == -2) + 1
  # match peaks to rate values:
  index <- rbindlist(lapply(peaks, function(x)
    data.table(index = x, peak_b1 = d$x, density = d$y)[x, ]))
  # rank density by size (biggest on top):
  if (top_only) {
    ranked_index <- index[order(-rank(density))][1]
  } else ranked_index <- index[order(-rank(density))]

  # identify data that match each peak rate:
  frags <- lapply(ranked_index$peak_b1, function(x)
    rollreg[slope_b1 <= (x + d$bw*.95)][slope_b1 >= (x - d$bw*.95)])
  # ensure that data segments that do not overlap are identified:
  frags <- lapply(1:length(frags), function(x)
    split(frags[[x]], c(0, cumsum(abs(diff(frags[[x]]$row)) > width))))
  # select longest fragments only:
  i <- sapply(1:length(frags),
              function(x) which.max(sapply(frags[[x]], nrow)))
  frags <- unname(mapply(function(x, y)
    frags[[x]][y], 1:length(frags), i))
  # remove zero-length data:
  # First remove same position from peaks (ranked_index), or later there are
  # issues with them not being same length
  ranked_index <- ranked_index[which(sapply(frags, nrow) > 0),]
  # then from fragments
  frags <- frags[sapply(frags, nrow) > 0]
  # convert fragments to single-data subsets:
  subsets <- lapply(1:length(frags), function(x)
    truncate_data(
      dt, min(frags[[x]]$row),
      max(frags[[x]]$endrow), "row"
    ))
  out <- list(rollreg = rollreg, subsets = subsets,
              peaks = ranked_index, density = d)
  return(out)
}



#' Perform regular rolling regression
#'
#' @param df data.frame object.
#' @param width numeric.
#'
#' @return a data.table object
#' @importFrom roll roll_lm
#' @keywords internal
rolling_reg_row <- function(df, width) {
  df <- data.table(df)
  setnames(df, 1:2, c("x", "y"))

  # perform rolling regression based on row numbers
  roll <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]), width, min_obs = 1)
  roll <- data.table(cbind(roll$coefficients, roll$r.squared))
  roll <- roll[-(1:(width-1)),]
  roll <- na.omit(roll)
  setnames(roll, 1:3, c("intercept_b0", "slope_b1", "rsq"))

  # add row indices
  roll[, row := seq_len(.N)]
  roll[, endrow := row + width - 1]
  roll[, time := df[roll[, row], x]]
  roll[, endtime := df[roll[, endrow], x]]
  roll[, oxy := df[roll[, row], y]]
  roll[, endoxy := df[roll[, endrow], y]]
  out <- roll[, c(4:9, 1:3)]
  return(out)
}


#' Perform rolling regression based on time units.
#'
#' @param df data.frame object.
#' @param width numeric.
#'
#' @return a data.table object
#' @keywords internal
rolling_reg_time <- function(df, width) {
  names(df) <- c('x','y')

  calc_coefs <- function(x, y) {
    b1 <- cov(x, y)/var(x)
    b0 <- mean(y) - (b1 * mean(x))
    r2 <- suppressWarnings(cor(x, y)^2)
    return(list(b0,b1,r2))
  }
  . <- x.x <- NULL
  results <- setDT(df)[.(start = x - width, end = x),
                       on = .(x >= start, x <= end),
                       as.list(calc_coefs(x.x, y)), by = .EACHI]
  setnames(results, 1:5, c('time', 'endtime', 'intercept_b0', 'slope_b1', 'rsq'))
  results <- results[time >= df[[1]][1]] # remove extra rows
  results[, row := seq_len(.N)]
  endrow <- sapply(results$endtime, function(i) df[, which(x == i)])
  endrow <- sort(unique(unlist(endrow))) #for obscure cases where there are duplicate times
  results[, endrow := endrow]
  results[, oxy := df[results[, row], y]]
  results[, endoxy := df[results[, endrow], y]]

  out <- results[,c(6:7, 1:2, 8:9, 3:5)]
  return(out)
}

#' Normal rolling regression
#'
#' This is an internal function.
#' @param df data.frame object.
#' @param win numeric. width in number of rows
#'
#' @return a data.table object
#' @importFrom roll roll_lm
#' @keywords internal
static_roll <- function(df, win) {
  roll <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]), width = win, min_obs = 1)
  # normally we would na.omit here, but next step will remove these initial NA rows anyway
  roll <- data.table(cbind(roll$coefficients, roll$r.squared))
  roll <- roll[-(1:(win-1)),]
  roll <- na.omit(roll)
  setnames(roll, 1:3, c("intercept_b0", "slope_b1", "rsq"))
  return(roll)
}

#' Perform time-width rolling regression
#'
#' This is an internal function. Used by [auto_rate()].
#' @param df data.frame object.
#' @param width numeric. width in time
#'
#' @return a data.table object
#' @keywords internal
#' @import parallel
time_roll <- function(dt, width, parallel = FALSE) {
  future_lapply <- plan <- NULL # global variables hack (unfortunate)
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("V1", "V2"))

  # The cutoff specifies where to stop the rolling regression, based on width
  time_cutoff <- max(dt[,1]) - width
  row_cutoff <- max(dt[, which(V1 <= time_cutoff)])

  # if(parallel) {
  #   oplan <- plan()
  #   on.exit(plan(oplan), add = TRUE)
  #   if (os() == 'win') {
  #     plan(multicore)
  #   } else plan(multisession)
  #   out <- future_lapply(1:row_cutoff, function(x) time_lm(dt,
  #     dt[[1]][x], dt[[1]][x] + width))
  # } else {
  #   out <- lapply(1:row_cutoff, function(x) time_lm(dt,
  #     dt[[1]][x], dt[[1]][x] + width))
  # }

  # parallelisation
  if (parallel) {
    no_cores <- parallel::detectCores()  # calc the no. of cores available
    if (os() == "win") {
      cl <- parallel::makeCluster(no_cores)
    } else cl <- parallel::makeCluster(no_cores, type = "FORK")
    parallel::clusterExport(cl, "time_lm")
    out <- parallel::parLapply(cl, 1:row_cutoff, function(x) time_lm(dt,
                                                                     dt[[1]][x], dt[[1]][x] + width))
    parallel::stopCluster(cl)  # stop cluster (release cores)
  } else out <- lapply(1:row_cutoff, function(x) time_lm(dt,
                                                         dt[[1]][x], dt[[1]][x] + width))

  out <- data.table::rbindlist(out)
  return(out)
}


#' Subset data by time and perform a linear regression.
#'
#' This is an internal function. Used with [time_roll()] and [auto_rate()].
#' @param df data.frame object.
#' @param start numeric. start time
#' @param end numeric. end time
#'
#' @return a data.table object
#' @keywords internal
time_lm <- function(df, start, end) {
  dt <- data.table::data.table(df)
  data.table::setnames(dt, 1:2, c("x", "y"))
  sdt <- dt[x >= start & x <= end]
  fit <- .lm.fit(cbind(1, sdt[[1]]), sdt[[2]])  # perform lm
  coef <- coef(fit)
  intercept_b0 <- coef[1]
  slope_b1 <- coef[2]
  # Calculate coef of determination (rsq) manually
  ybar <- sdt[, mean(y)]
  sst <- sum(sdt[, (y-ybar)*(y-ybar)])
  ssr <- sum((fit$residuals)*(fit$residuals))
  rsq <- 1-(ssr/sst)
  out <- data.table::data.table(intercept_b0, slope_b1, rsq)
  return(out)
}


#' Rolling regression
#'
#' @param df data.frame object.
#' @param width numeric. width in number of rows or time
#' @param by string. time or row
#'
#' @return a data.table object
#' @keywords internal
rolling_reg <- function(dt, width, by) {
  win <- width
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
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string. time or row
#'
#' @return A list object
#' @keywords internal
kde_fit <- function(dt, width, by, use = "all") {
  # perform rolling regression
  reg <- rolling_reg(dt, width, by)
  roll <- reg$roll
  win <- reg$win

  # If entire width of data frame is selected, there's nothing to detect!
  if (nrow(roll) == 1) {
    subsets <- list(truncate_data(dt, roll$row, roll$endrow, "row"))
    d <- NULL
    peaks <- roll$slope_b1
    bw <- NULL

  } else {
    d <- density(roll$slope_b1, na.rm = T, bw = "SJ-ste", adjust = .95) # KDE
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
      function(x) roll[slope_b1 <= (x + d$bw*.95)][slope_b1 >= (x - d$bw*.95)]
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
    i <- sapply(1:length(frags),
                function(x) which.max(sapply(frags[[x]], nrow)))
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

#' Automatically calculate rolling window
#'
#' The calculated value is used to determine the rolling window for rolling
#' regressions.
#'
#' @param dt data.frame object.
#' @param width numeric.
#' @param by string.
#' @param msg string. Attach function name for message (usually auto_rate)
#'
#' @return a numeric
#' @keywords internal
calc_win <- function(dt, width, by, msg) {

  if(by == "row"){
    if (is.null(width)) {
      width <- 0.2  # if no width is specified, set to 20 %
      message(glue::glue("{msg} Applying default 'width' of 0.2"))}
    if(width > 1 && width > nrow(dt))
      stop(glue::glue("{msg}'width' exceeds length of dataset"))
    if(width > 1 && !(width %% 1 == 0))
      stop(glue::glue("{msg}'width' should be an integer of 2 or higher"))
    if(width == 1)
      stop(glue::glue("{msg}'width' cannot be 1 row"))
    if(width == nrow(dt))
      stop(glue::glue("{msg}'width' cannot be the total number of rows in the input data"))

    if (width < 1) win <- floor(width * nrow(dt))  # set to proportion of length of data
    else win <- width
  }

  if(by == "time"){
    if (is.null(width)) {
      # if no width is specified, set to 20 % of total time data range
      width <- 0.2 * diff(range(nainf.omit(dt[[1]])))
      message(glue::glue("{msg} Applying default 'width' of 20% of time data range"))}
    if(width >= diff(range(nainf.omit(dt[[1]]))))
      stop(glue::glue("{msg} 'width' cannot exceed or equal total time data range"))

    win <- width
  }

  return(win)
}

#' Prints the density object for summary.auto_rate S3
#' Basically copied from stats:::print.density and edited to make
#' it more compact
#'
#' @param x auto_rate object.
#' @param digits numeric.
#'
#' @return Console output
#' @keywords internal
print_dens <- function (x, digits = NULL, ...){
  cat("\nCall: ", gsub("  ", "", deparse(x$call)), "\nData: ", x$data.name,
      " (", x$n, " obs.)", ", Bandwidth 'bw' = ", formatC(x$bw,
                                                          digits = digits), "\n", sep = "")
  print(summary(as.data.frame(x[c("x", "y")])), digits = digits,
        ...)
  invisible(x)
}

