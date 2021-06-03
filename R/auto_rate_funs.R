#' Validation function for auto_rate
#'
#' This is an internal function for `auto_rate()`. Used to validate input
#' arguments.
#'
#' @param df data.frame object.
#' @param by string.
#' @param method string.
#'
#' @return a data.frame object of the original data
#'
#' @importFrom assertthat assert_that
#' @export
#' @keywords internal
#'
#' @examples
#' # this will pass:
#' set.seed(88)
#' df <- sim_data(len = 50)$df
#' validate_auto_rate(df, by = "row", method = "max")
#'
#' # this will fail as df is not a data.frame object:
#' # set.seed(88)
#' # df <- c(1:40)
#' # validate_auto_rate(df, by = "row", method = "max")
#'
#' # this will fail as `by` argument isn't recognised:
#' # set.seed(88)
#' # df <- c(1:40)
#' # validate_auto_rate(df, by = "rat", method = "max")
#'
#' # this will fail as df is not a data.frame object:
#' # set.seed(88)
#' # df <- c(1:40)
#' # validate_auto_rate(df, by = "row", method = "max")
#'
#' # this will fail as `method` argument isn't recognised:
#' # set.seed(88)
#' # df <- c(1:40)
#' # validate_auto_rate(df, by = "row", method = "moo")
validate_auto_rate <- function(df, by, method) {

  # # does df exist?
  # is_defined <- function(sym) {
  #   sym <- deparse(substitute(sym))
  #   env <- parent.frame()
  #   exists(sym, env)
  # }
  # if (!is_defined(df)) stop('df object does not exist, please check')

  # convert data if necessary:
  if (any(class(df) %in% "inspect_data")) df <- df$dataframe
  if (any(class(df) %in% "inspect")) df <- df$dataframe

  # select only first two columns by default if dataset is multi-column
  if (length(df) > 2) {
    warning("auto_rate: Multi-column dataset detected in input. Selecting first two columns by default.\n  If these are not the intended data, inspect() or subset the data frame columns appropriately before running auto_rate()")
    df <- df[, 1:2]
  }
  by <- verify_by(by, msg = "auto_rate:") # validate `by` argument

  assertthat::assert_that(
    is.data.frame(df),
    msg = "auto_rate: Input data must be of class 'data.frame' or 'inspect'"
  )
  assertthat::assert_that(
    by %in% c("time", "row"),
    msg = "auto_rate: The 'by' argument must be 'time' or 'row'"
  )
  assertthat::assert_that(
    method %in% c("default", "linear", "max", "min", "interval",
                  "rolling",
                  "highest", "lowest", "maximum", "minimum"),
    msg = "auto_rate: The 'method' argument is not recognised: it should be 'linear',
    'highest', 'lowest', 'maximum', 'minimum', 'rolling', or 'interval'"
    # leave out max/min from message - old operators, we don't want them to be used
  )
  return(list(by = by, df = data.table(df)))
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
auto_rate_min <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # message if mix of -ve and +ve
  if(any(rollreg$rate_b1 > 0) && any(rollreg$rate_b1 < 0))
    message("auto_rate: Note dataset contains both negative and positive rates. Ensure ordering 'method' is appropriate.")

  # order data by size, from biggest
  results <- rollreg[order(rank(rate_b1))]
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
auto_rate_max <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # message if mix of -ve and +ve
  if(any(rollreg$rate_b1 > 0) && any(rollreg$rate_b1 < 0))
    message("auto_rate: Note dataset contains both negative and positive rates. Ensure ordering 'method' is appropriate.")

  # order data by size, from biggest
  results <- rollreg[order(-rank(rate_b1))]
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
auto_rate_highest <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # stop if mix of -ve and +ve
  if(any(rollreg$rate_b1 > 0) && any(rollreg$rate_b1 < 0))
    stop("auto_rate: Analysis produces both negative and positive rates. \n The 'highest' method is intended to order by the lowest *absolute* rate amongst rates all having the same sign.\n Use 'maximum' or 'minimum' method to order rates by *numerical* value.")
  # order data by absolute value, from highest
  results <- rollreg[order(-rank(abs(rollreg$rate_b1)))] ## note abs() operation
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
auto_rate_lowest <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # stop if mix of -ve and +ve
  if(any(rollreg$rate_b1 > 0) && any(rollreg$rate_b1 < 0))
    stop("auto_rate: Analysis produces both negative and positive rates. \n The 'lowest' method is intended to order by the lowest *absolute* rate amongst rates all having the same sign.\n Use 'maximum' or 'minimum' method to order rates by *numerical* value.")

  # order data by absolute value, from lowest
  results <- rollreg[order(rank(abs(rollreg$rate_b1)))] ## note abs() operation
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
auto_rate_rolling <- function(dt, width, by = 'row') {
  # perform rolling regression
  if (by == 'row') {
    rollreg <- rolling_reg_row(dt, width)
  } else if (by == 'time') {
    rollreg <- rolling_reg_time(dt, width)
  }

  # message if mix of -ve and +ve
  if(any(rollreg$rate_b1 > 0) && any(rollreg$rate_b1 < 0))
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
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
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
auto_rate_linear <- function(dt, width, verify = TRUE) {

  # define kde function
  kernel_method <- function(dt, width, top_only = FALSE) {
    # linear detection is always by row since linear detection is not dependent
    # on time but more on the stability of the data
    rollreg <- rolling_reg_row(dt, width)

    # perform kernel density estimate
    d <- density(rollreg$rate_b1, na.rm = T, bw = "SJ-ste", adjust = .95)
    # extract bandwidth
    bw <- d$bw
    # identify peaks in kernel density:
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
      rollreg[rate_b1 <= (x + d$bw*.95)][rate_b1 >= (x - d$bw*.95)])
    # ensure that data segments that do not overlap are identified:
    frags <- lapply(1:length(frags), function(x)
      split(frags[[x]], c(0, cumsum(abs(diff(frags[[x]]$row)) > width))))
    # select longest fragments only:
    i <- sapply(1:length(frags),
      function(x) which.max(sapply(frags[[x]], nrow)))
    frags <- unname(mapply(function(x, y)
      frags[[x]][y], 1:length(frags), i))
    # remove zero-length data:
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
  results <- output[,-(8:12)][, c(4:7, 1:3)]

  out <- list(results = results, roll = kde$rollreg,
    density = kde$density, peaks = kde$peaks)

  class(out) <- append(class(out), "auto_rate_linear")
  return(out)
}


#' Perform regular rolling regression
#'
#' @param df data.frame object.
#' @param width numeric.
#'
#' @return a a data.table object
#' @importFrom roll roll_lm
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
rolling_reg_row <- function(df, width) {
  df <- data.table(df)
  setnames(df, 1:2, c("x", "y"))

  # perform rolling regression based on row numbers
  roll <- roll_lm(matrix(df[[1]]), matrix(df[[2]]), width)
  roll <- na.omit(data.table(cbind(roll$coefficients, roll$r.squared)))
  setnames(roll, 1:3, c("intercept_b0", "rate_b1", "rsq"))

  # add row indices
  roll[, row := seq_len(.N)]
  roll[, endrow := row + width - 1]
  roll[, time := df[roll[, row], x]]
  roll[, endtime := df[roll[, endrow], x]]
  out <- roll[, c(4:7, 1:3)]
  return(out)
}


#' Perform rolling regression based on time units.
#'
#' @param df data.frame object.
#' @param width numeric.
#'
#' @return a data.table object
#' @export
#' @keywords internal
#'
#' @examples
#' NULL
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
  setnames(results, 1:5, c('time', 'endtime', 'intercept_b0', 'rate_b1', 'rsq'))
  results <- results[time >= df[[1]][1]] # remove extra rows
  results[, row := seq_len(.N)]
  endrow <- sapply(results$endtime, function(i) df[, which(x == i)])
  results[, endrow := endrow]
  out <- results[,c(6:7, 1:5)]
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
#'
#' @return a numeric
#' @export
#' @keywords internal
#' @examples
#' NULL
calc_rolling_win <- function(dt, width, by) {
  # this is an internal function so we don't have to validate data too much
  # however we make sure that input arguments are validated since they are
  # obtained from user input

  # validation
  if (is.null(width)) width <- .2  # if no width is specified, set to 20 %
  # if width > 1, check that value does not exceed length of data:
  if (width > 1 && width > nrow(dt)) stop("auto_rate: 'width' exceeds length of dataset")

  # perform calculations
  if (width <= 1 & by == 'time') {
    win <- floor(width * max(dt[[1]]))
  } else if (width <= 1 & by == 'row') {
    win <- floor(width * nrow(dt))  # set to 20 % of length of data
  } else if (width > 1) {
    win <- width
  }
  return(win)
}
