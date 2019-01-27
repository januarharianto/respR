# Deal with pesky "no visible binding for global variable.." checks
x = NULL; endtime = NULL; rate_b1 = NULL; row.len = NULL; time.len = NULL
rowlength = NULL; endrow = NULL; timelength = NULL; rate_twopoint = NULL
endoxy = NULL; oxy = NULL; sumRSS = NULL; do = NULL; y = NULL; V1 = NULL
..xcol = NULL; ..ycol = NULL; peak_b1 = NULL; .std.resid = NULL
.fitted = NULL; .resid = NULL

#' Pipe graphics
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
  if (any(class(x) %in% "inspect_data")) x <- x$df
  if (any(class(x) %in% "inspect")) x <- x$dataframe

  dt <- data.table::as.data.table(x)
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  }
  if (by == "row") {
    out <- dt[from:to]
  }
  if (by == "o2" & length(x) == 2) {
    top <- Position(function(z) z <= from, dt[[2]])
    bot <- Position(function(z) z <= to, dt[[2]])
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
