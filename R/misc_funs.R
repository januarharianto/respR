#' @title Wrapper for if-stop condition in functions.
#' @description A very simple function that may not be used...
#' @param condition The expression/condition to evaluate.
#' @param msg An error message which will be printed if condition is fulfiled.
#' @return NULL
#' @author Januar Harianto
#' @export
#' @examples
#' errorCheck(x > 5, "Value is bigger than 5.")
# credit to https://stackoverflow.com/a/8345314
errorCheck <- function (condition, msg) {
  if (condition)
    stop(msg, call. = FALSE)
}



#' @title Check for NA and NaN
#' @description Returns TRUE or FALSE. Also prints an index when summary = T.
#' Dataframe MUST be parsed through prepData() before this.
#' @param df Dataframe.
#' @param col Numeric. Selects column index to parse.
#' @param summary Logical. Defaults to FALSE.
#' @return NULL
#' @author Januar Harianto
#' @export
#' @examples
#' hasNA
# TRUE - there are NA values
# FALSE - no NA values
hasNA <- function(df, col=1, summary = F) {
  x <- df[[col]]
  check <- any(is.na(x))
  out <- which(is.na(x))
  if(summary)
    return(out)
  else
    return(check)
}



#' @title Check for duplicates
#' @author Januar Harianto
#' @export
dupes <- function(df, summary = F) {
  x      <- df[[1]]
  check  <- isTRUE(any(duplicated(x)))
  out    <- which(duplicated(x) == T)
  if(summary)
    return(out)
  else
    return(check)
}



#' @title Check for monotonically increasing data.
#' @author Januar Harianto
#' @export
# https://stackoverflow.com/a/13094801
# Maybe expand this to detect monotically decreasing data as well. Then, it
# might be implemented in a function to DETECT INTERMITTENT DATASET :D :D :D
monotonic <- function(df, summary=F){
  x <- df[[1]]
  check <- all(x == cummax(x))
  out   <- which(diff(x) < 0) # ID rows that return FALSE
  if (summary)
    return(out)
  else
    return(check)
}



#' @title Check for evenly spaced time data
#' @author Januar Harianto
#' @export
# https://stackoverflow.com/a/4752580
evenSpaced <- function(df, col=1, tol=.Machine$double.eps * 100){
  x     <- diff(df[[col]])
  cond  <- range(x) / mean(x)
  check <- isTRUE(all.equal(cond[1], cond[2], tolerance = tol))
  return(check)
}



#' @title Calculate mode. I may or may not use this...for auto detection of
#' best regression.
#' @author Januar Harianto
#' @export
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Just a benchmarking function (VERY SIMPLE), will delete this later -----------
# bench <- function(x){
#   system.time({x})
# }
