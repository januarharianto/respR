
# compact stopifnot method, if needed
# credit to https://stackoverflow.com/a/8345314
if.stop <- function (condition, msg) {
  if (condition) stop(msg, call. = FALSE)
}

# check that x and y are of equal length
# unequal.data <- data.frame(c(1:10), c(1:20))
# check.length(unequal.data[[1]], unequal.data[[2]])
check.length <- function(x, y) {
  check <- all.equal(length(x), length(y))
  id <- c(length(x), length(y))
  out <- list(check = check, id = id)
  return(out)
}

# check that data is numeric or time data, where appropriate
numeric.datetime <- function(x, y) {
  # check if x is numeric or time data
  check.x <- is.numeric(x) | inherits(x, "POSIXct") | inherits(x, "POSIXt")
  # check if y is numeric
  check.y <- is.numeric(y)
  out <- c(check.x, check.y)
  return(out)
}

# check for NA values (also less specific NaN)
# TRUE = has NA
# test:
# na.data <- data.frame(x = c(1:15), y = c(1:5, rep(NA, 3), 9:15))
# has.NA(na.data)
has.NA <- function(df) {
  check <- any(is.na(df))
  id <- which(is.na(df))
  out <- list(check = check, id = id)
  return(out)
}

# check for duplicated time
# TRUE = has dupes in time
# note: only checks time column, assumed to be first column
# test:
# dup.time.data <- data.frame(x = c(1:5, 1:5), y = c(1:10))
# dupes(dup.time.data)
dupes <- function(df, summary = F) {
  x      <- df[[1]]
  check  <- isTRUE(any(duplicated(x)))
  id    <- which(duplicated(x) == T)
  out <- list(check = check, id = id)
  return(out)
}


# Check for monotonically increasing data. Not used (switch to Spearman's)
# https://stackoverflow.com/a/13094801
# test:
# non.monotonic.data <- data.frame(x = c(1:4, 5:2, 1:7), y = c(1:15))
# monotonic(non.monotonic.data)
monotonic <- function(y, summary = F) {
  check <- all(y == cummax(y)) | all(y == cummin(y))
  out <- check
  return(out)
}

# Check for evenly spaced data
# https://stackoverflow.com/a/4752580
even.spaced <- function(x, tol = .Machine$double.eps * 100){
  x     <- diff(x)
  cond  <- range(x) / mean(x)
  check <- isTRUE(all.equal(cond[1], cond[2], tolerance = tol))
  out <- check
  return(out)
  }

