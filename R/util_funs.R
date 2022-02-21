#' Pipe graphics direct from tidyverse-related package
#' @importFrom magrittr %>%
#' @name %>%
#' @return No value returned
#' @keywords internal
#' @export
NULL

#' Select columns
#' @importFrom dplyr select
#' @name select
#' @return No value returned
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
    if(!num[[1]][1]) inf <- sapply(x, function(y) check_inf(y)) else
      inf <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
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
      inf[1, , drop = F],
      nan[1, , drop = F],
      seq[1, , drop = F],
      dup[1, , drop = F],
      evn[1, , drop = F]
    )
    locs <- rbind(
      NA,
      inf[2, , drop = F],
      nan[2, , drop = F],
      seq[2, , drop = F],
      dup[2, , drop = F],
      evn[2, , drop = F]
    )
  } else if (type == "oxygen") {
    num <- sapply(x, function(y) check_num(y))

    # if oxy column has passed numeric check, do check
    # otherwise return "skip"
    # check inf
    inf <- sapply(1:length(x), function(y) {
      if(!num[,y][[1]]) check_inf(x[[y]]) else
      return(list(check = "skip", which = integer(0)))
    })
    # check nan
    nan <- sapply(1:length(x), function(y) {
      if(!num[,y][[1]]) check_na(x[[y]]) else
      return(list(check = "skip", which = integer(0)))
    })

    seq <- NA
    dup <- NA
    evn <- NA
    checks <- rbind(
      num[1, , drop = F],
      inf[1, , drop = F],
      nan[1, , drop = F],
      seq[1],
      dup[1],
      evn[1])
    locs <- rbind(
      NA,
      inf[2, , drop = F],
      nan[2, , drop = F],
      seq[1],
      dup[1],
      evn[1])
  }

  # rename rows - I'm sure I can make this more efficient... later..
  rnames <- c("numeric", "Inf/-Inf", "NA/NaN", "sequential", "duplicated", "evenly-spaced  ")
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

## check for Inf/-Inf values - used in the function `inspect()`
check_inf <- function(x) {
  test <- is.infinite(x)
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

  ## replace NULL inputs with defaults
  if (is.null(by)) by <- "time"

  ## verify by just in case
  by <- verify_by(by)

  ## replace NULL inputs with defaults
  if(is.null(from)){
    if(by == "time") from <- min(dt[[1]], na.rm = TRUE)
    if(by == "row") from <- 1
    if(by == "oxygen") from <- dt[[2]][1] # first oxygen value
    if(by == "proportion")
      stop("Please enter a proportion 'from' input.")
  }
  if(is.null(to)){
    if(by == "time") to <- max(dt[[1]], na.rm = TRUE)
    if(by == "row") to <- nrow(dt)
    if(by == "oxygen") to <- dt[[2]][nrow(dt)] # last oxygen value
    if(by == "proportion")
      stop("Please enter a proportion 'to' input.")
  }

  ## time is ok, since it is always increasing
  if (by == "time") {
    # if values out of range use lowest/highest available
    rng <- range(dt[[1]], na.rm = TRUE)
    if(from < rng[1]) from <- rng[1]
    if(to > rng[2]) to <- rng[2]
    #out <- dt[dt[[1]] >= from & dt[[1]] <= to] # old method
    # new method - finds closest value to each time
    out <- dt[dt[[1]] >= dt[[1]][which.min(abs(dt[[1]] - from))]
              & dt[[1]] <= dt[[1]][which.min(abs(dt[[1]] - to))]]
  }
  ## row is ok, since it is always increasing
  if (by == "row") {
    # if to value out of range use highest available
    if(to > nrow(dt)) to <- nrow(dt)
    out <- dt[from:to]
  }
  ## oxygen could be increasing or decreasing
  if (by == "oxygen") {

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
        start <- min(which(dplyr::between(dt[[2]], lower, upper)), na.rm = TRUE)
        end <- max(which(dplyr::between(dt[[2]], lower, upper)), na.rm = TRUE)

        out <- dt[start:end]
  }
  if (by == "proportion") {
    ## has to handle by proportion produced as well as consumed!
    mx <- max(dt[[2]], na.rm = TRUE)
    mn <- min(dt[[2]], na.rm = TRUE)
    ## dplyr::between needs them in low-high order
    lower <- sort(c(from, to))[1]
    upper <- sort(c(from, to))[2]
    # indices of data between these
    start <- min(which(dplyr::between(dt[[2]],
                                      (lower * (mx - mn) + mn),
                                      (upper * (mx - mn) + mn))),
                 na.rm = TRUE)
    end <- max(which(dplyr::between(dt[[2]],
                                    (lower * (mx - mn) + mn),
                                    (upper * (mx - mn) + mn))),
               na.rm = TRUE)

    out <- dt[start:end]
  }
  return(out)
}


# FUNCTIONS for P_crit----------------------------

#' Perform broken-stick regressions
#' @return a data.table object
#' @keywords internal

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
    pcrit.midpoint = midpoint,
    l1_coef = line1,
    l2_coef = line2

  )
  return(out)
}


#' Generate a DO ~ PO2 data table from a DO timeseries
#' @return a data.table object
#' @keywords internal
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
  rdt <- data.table::data.table(rollx, rolly$rate_b1)
  data.table::setnames(rdt, 1:2, c("x", "y"))
  return(rdt)
}

#' Omit NA, NaN, Inf and -Inf from a vector or dataframe columns
#'
#' For using with, for example, range to get axis range values in 'inspect'.
#' Previously, na.omit was used, then discovered data files with Inf values.
#' This causes axis limit range to be Inf, and xlim/ylim don't accept infinite
#' axes!
#'
#' If x is dataframe, it returns a vector of all columns appended together.
#' Only useful for getting range in this case, don't use for anything else.
#'
#' @return original vector without NA or Inf values or df all cols appended without these
#' @keywords internal
nainf.omit <- function(x) {
  if (is.vector(x)) z <- na.omit(x[is.finite(x)])

  if (is.data.frame(x)) {
    z <- lapply(x, function(y) na.omit(y[is.finite(y)]))
    z <- as.vector(unlist(z))
  }
  return(z)
}

# Deal with pesky "no visible binding for global variable.." checks
x = NULL; endtime = NULL; rate_b1 = NULL; row.len = NULL; time.len = NULL
rowlength = NULL; endrow = NULL; timelength = NULL; rate.2pt = NULL
endoxy = NULL; oxy = NULL; sumRSS = NULL; do = NULL; y = NULL; V1 = NULL
..xcol = NULL; ..ycol = NULL; multicore = NULL; multisession = NULL


