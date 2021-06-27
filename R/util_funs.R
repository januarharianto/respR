#' Pipe graphics direct from tidyverse-related package
#' @importFrom magrittr %>%
#' @name %>%
#' @keywords internal
#' @export
NULL

#' Select columns
#' @importFrom dplyr select
#' @name select
#' @keywords internal
#' @export
NULL

# check os - useful for parallel functions
os <- function() {
  if (.Platform$OS.type == "windows")
    "win" else if (Sys.info()["sysname"] == "Darwin")
      "mac" else if (.Platform$OS.type == "unix")
        "unix" else stop("Unknown OS")
}

# tic - for time elapsed
tic <- function(gcFirst = TRUE, type = c("elapsed", "user.self", "sys.self")) {
  type <- match.arg(type)
  assign(".type", type, envir = baseenv())
  if (gcFirst)
    gc(FALSE)
  tic <- proc.time()[type]
  assign(".tic", tic, envir = baseenv())
  invisible(tic)
}

# toc - for time elapsed
toc <- function() {
  type <- get(".type", envir = baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir = baseenv())
  elapsed <- (toc - tic)[[1]]
  return(elapsed)
}


# checks for `inspect()` functions --------------------------------

## combined check:
check_timeseries <- function(x, type = "time") {
  if (type == "time") {
    num <- sapply(x, function(y) check_num(y))
    ## if not numeric, no point doing these checks
    ## So instead we 'skip'
    if(!num[[1]][1]) nan <- sapply(x, function(y) check_na(y)) else
      nan <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) seq <- sapply(x, function(y) check_seq(y)) else
      seq <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) dup <- sapply(x, function(y) check_dup(y)) else
      dup <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) evn <- sapply(x, function(y) check_evn(y)) else
      evn <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    checks <- rbind(
      num[1, , drop = F],
      nan[1, , drop = F],
      seq[1, , drop = F],
      dup[1, , drop = F],
      evn[1, , drop = F]
    )
    locs <- rbind(
      num[2, , drop = F],
      nan[2, , drop = F],
      seq[2, , drop = F],
      dup[2, , drop = F],
      evn[2, , drop = F]
    )
  } else if (type == "oxygen") {
    num <- sapply(x, function(y) check_num(y))

    # if oxy column has passed numeric check, do check NA
    # otherwise return "skip"
    nan <- sapply(1:length(x), function(y) {
      if(!num[,y][[1]]) check_na(x[[y]]) else
      return(list(check = "skip", which = integer(0)))
    })

    seq <- NA
    dup <- NA
    evn <- NA
    checks <- rbind(
      num[1, , drop = F],
      nan[1, , drop = F],
      seq[1],
      dup[1],
      evn[1])
    locs <- rbind(
      num[2, , drop = F],
      nan[2, , drop = F],
      seq[1],
      dup[1],
      evn[1])
  }

  # rename rows - I'm sure I can make this more efficient... later..
  rnames <- c("numeric", "NA/NaN", "sequential", "duplicated", "evenly-spaced")
  rownames(checks) <- rnames
  rownames(locs) <- rnames

  return(list(checks, locs))
}

## check for non-numeric values - used in the function `inspect()`
## Note - checks for NOT numeric
check_num <- function(x) {
  test <- !is.numeric(x)
  check <- any(test)
  highlight <- rep(check, length(x))
  out <- list(check = check, which = highlight)
  return(out)
}

## check for NA values - used in the function `inspect()`
check_na <- function(x) {
  test <- is.na(x)
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

## check for sequential (monotonic) data - used in the function `inspect()`
check_seq <- function(x) {
  test <- diff(x) < 0
  test <- ifelse(is.na(test), FALSE, test)  # convert NA values to FALSE
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

# check for duplicate data (time) - used in the function `inspect()`
check_dup <- function(x) {
  test <- x %in% unique(x[duplicated(x, incomparables = NA)])
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

## calculate mode - used in the function `inspect()`
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## check for evenly-spaced data (time) - used in the function `inspect()`
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

# Internal truncate (similar to subset_data)
truncate_data <- function(x, from, to, by) {

  # import from other respR functions
  if (any(class(x) %in% "inspect")) x <- x$dataframe
  if (any(class(x) %in% "inspect.ft")) x <- x$dataframe

  dt <- data.table::as.data.table(x)

  ## time is ok, since it is always increasing
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  }
  ## row is ok, since it is always increasing
  if (by == "row") {
    ## Check rows within range
    if(from > nrow(dt)) stop("'from' row is greater than total number of rows.")
    ## Use last row if to row too large
    if(to > nrow(dt)) to <- nrow(dt)

    out <- dt[from:to]
  }
  ## o2 could be increasing or decreasing
  if (by == "o2") {

    # data range
    o_range <- range(dt[[2]], na.rm = TRUE)

    # use highest/lowest values if out of range
    if(from > o_range[2]) from <- o_range[2] else
      if(from < o_range[1]) from <- o_range[1]
      if(to > o_range[2]) to <- o_range[2] else
        if(to < o_range[1]) to <- o_range[1]

        ## dplyr::between needs them in low-high order
        lower <- sort(c(from, to))[1]
        upper <- sort(c(from, to))[2]
        # indices of data between these
        start <- min(which(dplyr::between(dt[[2]], lower, upper)))
        end <- max(which(dplyr::between(dt[[2]], lower, upper)))

        out <- dt[start:end]
  }
  if (by == "proportion") {
    ## has to handle by proportion produced as well as consumed!
    mx <- max(dt[[2]])
    mn <- min(dt[[2]])
    ## dplyr::between needs them in low-high order
    lower <- sort(c(from, to))[1]
    upper <- sort(c(from, to))[2]
    # indices of data between these
    start <- min(which(dplyr::between(dt[[2]],
                                      (lower * (mx - mn) + mn),
                                      (upper * (mx - mn) + mn))))
    end <- max(which(dplyr::between(dt[[2]],
                                    (lower * (mx - mn) + mn),
                                    (upper * (mx - mn) + mn))))

    # Old method - only works with oxy decrease
    #start <- Position(function(z) z <= (from * (mx - mn) + mn), dt[[2]])
    #end <- Position(function(z) z <= (to * (mx - mn) + mn), dt[[2]])

    out <- dt[start:end]
  }
  return(out)
}


# FUNCTIONS for P_crit----------------------------


#' Perform broken-stick regressions
#'
#' @keywords internal
#'
#' @export
broken_stick <- function(dt, n) {
  # Cut data into 2
  dta <- dt[1:n]
  dtb <- dt[(n + 1):nrow(dt)]

  # Perform lm
  lma <- .lm.fit(cbind(1, dta[[1]]), dta[[2]])
  lmb <- .lm.fit(cbind(1, dtb[[1]]), dtb[[2]])

  # Extract coefficients
  coefa <- coef(lma)
  coefb <- coef(lmb)

  # Calculate residual sum of squares
  trss <- sum(lma$residuals*lma$residuals) + sum(lmb$residuals*lmb$residuals)

  # Also, calculate intersect
  cm <- rbind(coefa, coefb)
  # https://stackoverflow.com/a/7114961
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])[1]

  # Calculate midpoint
  midpoint <-  (dta[,x][nrow(dta)] + dtb[,x][1]) / 2

  # List coefficients
  line1 <- data.table(rbind(coefa))
  names(line1) <- c("b0", "b1")
  line2 <- data.table(rbind(coefb))
  names(line2) <- c("b0", "b1")

  # Generate output
  out <- data.table::data.table(
    splitpoint = dta[,x][nrow(dta)],
    sumRSS = trss,
    pcrit.intercept = intersect,
    pcrit.mpoint = midpoint,
    l1_coef = line1,
    l2_coef = line2

  )
  return(out)
}




#' Generate a DO ~ PO2 data table from a DO timeseries
#'
#' @keywords internal
#'
#' @export
generate_mrdf <- function(dt, width) {
  # Ensure that dt is a data.table
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("x", "y"))

  # Extract columns
  x <- as.matrix(dt[,1])
  y <- as.matrix(dt[,2])

  # Then, perform rolling mean and lm
  rollx <- na.omit(roll::roll_mean(y, width))
  rolly <- static_roll(dt, width)

  # Then, combine into new data.table
  rdt <- data.table::data.table(rollx, abs(rolly$rate_b1))
  data.table::setnames(rdt, 1:2, c("x", "y"))
  return(rdt)
}

# Deal with pesky "no visible binding for global variable.." checks
x = NULL; endtime = NULL; rate_b1 = NULL; row.len = NULL; time.len = NULL
rowlength = NULL; endrow = NULL; timelength = NULL; rate.2pt = NULL
endoxy = NULL; oxy = NULL; sumRSS = NULL; do = NULL; y = NULL; V1 = NULL
..xcol = NULL; ..ycol = NULL; multicore = NULL; multisession = NULL


