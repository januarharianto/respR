#' Pipe graphics direct from tidyverse-related package
#' @importFrom magrittr %>%
#' @name %>%
#' @export
NULL

#' Select columns
#' @importFrom dplyr select
#' @name select
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
    nan <- sapply(x, function(y) check_na(y))
    seq <- sapply(x, function(y) check_seq(y))
    dup <- sapply(x, function(y) check_dup(y))
    evn <- sapply(x, function(y) check_evn(y))
    checks <- rbind(
      nan[1, , drop = F], seq[1, , drop = F], dup[1, , drop = F],
      evn[1, , drop = F]
    )
    locs <- rbind(
      nan[2, , drop = F], seq[2, , drop = F], dup[2, , drop = F],
      evn[2, , drop = F]
    )
  } else if (type == "oxygen") {
    nan <- sapply(x, function(y) check_na(y))
    seq <- NA
    dup <- NA
    evn <- NA
    checks <- rbind(nan[1, , drop = F], seq[1], dup[1], evn[1])
    locs <- rbind(nan[2, , drop = F], seq[1], dup[1], evn[1])
  }

  # rename rows - I'm sure I can make this more efficient... later..
  rnames <- c("NA/NAN", "sequential", "duplicated", "evenly-spaced")
  rownames(checks) <- rnames
  rownames(locs) <- rnames

  return(list(checks, locs))
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
  if (any(class(x) %in% "inspect_data")) x <- x$dataframe
  if (any(class(x) %in% "inspect")) x <- x$dataframe

  dt <- data.table::as.data.table(x)
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  }
  if (by == "row") {
    ## Also new - check to row not too large
    if(to > nrow(dt)) stop("`to` row is greater than total number of rows.")
    out <- dt[from:to]
  }
  if (by == "o2" & length(dt) == 2) {
    top <- Position(function(z) z <= from, dt[[2]])

    ## this is new
    ## if less than min o2 value, match pos of min o2 value
    ## else pos of the actual to value
    if(to < min(dt[[2]])){
      bot <- Position(function(z) z <= min(dt[[2]]), dt[[2]])
    } else {
      bot <- Position(function(z) z <= to, dt[[2]])
    }

    out <- dt[top:bot]
  }
  if (by == "proportion") {
    mx <- max(dt[[2]])
    mn <- min(dt[[2]])
    top <- Position(function(z) z <= (from * (mx - mn) + mn), dt[[2]])
    bot <- Position(function(z) z <= (to * (mx - mn) + mn), dt[[2]])
    out <- dt[top:bot]
  }
  return(out)
}


# Verify by input for calc_rate etc.
verify_by <- function(by){
  ## no doubt this is easier with regex
  time_variations <- c("time", "Time", "TIME",
                       "tim", "Tim", "TIM",
                       "tm", "Tm", "TM",
                       "t", "T")
  ox_variations <- c("o2", "O2",
                     "oxygen", "Oxygen", "OXYGEN",
                     "oxy", "Oxy", "OXY",
                     "ox", "Ox", "OX",
                     "o", "O")
  row_variations <- c("row", "Row", "ROW",
                      "rw", "Rw", "RW",
                      "r", "R")
  prop_variations <- c("proportion", "Proportion", "PROPORTION",
                       "proport", "Proport", "PROPORT",
                       "prop", "Prop", "PROP",
                       "prp", "Prp", "PRP",
                       "pr", "Pr", "PR",
                       "p", "P")

  if(by %in% time_variations) by <- "time"
  else if(by %in% ox_variations) by <- "o2"
  else if(by %in% row_variations) by <- "row"
  else if(by %in% prop_variations) by <- "proportion"
  else stop("`by` input not recognised")

  return(by)
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
rowlength = NULL; endrow = NULL; timelength = NULL; rate_twopoint = NULL
endoxy = NULL; oxy = NULL; sumRSS = NULL; do = NULL; y = NULL; V1 = NULL
..xcol = NULL; ..ycol = NULL; multicore = NULL; multisession = NULL
