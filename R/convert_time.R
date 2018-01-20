#' Parse datetime data to numeric
#'
#' `convert_time()` is a function to parse class POSIX.ct or text strings of
#' date-time data to numeric time for use in `respR` functions.
#'
#' With the help of the excellent \pkg{lubridate} package, you can simplify time
#' conversion of dates and time into numeric format by speficying the formats in
#' the `format` argument, without the need to include separators and \% prefix.
#' Simply identify the order which the year, month, day, and time appears in
#' your string.
#'
#' \describe{
#' \item{`d`}{Day of the month as decimal number (01--31 or 0--31).}
#' \item{`m`}{Month of the year as decimal number (01--12 or 1--12).}
#' \item{`y`}{Year (2010, 2001, 1989).}
#' \item{`H`}{Hour, must be capitalised (`H`, not `h`). Decimal number (00--24 or 0--24).}
#' \item{`M`}{Minute, must be capitalised (`M`, not `m`). Decimal number (00--59 or 0--59).}
#' \item{`S`}{Second, must be capitalised (`S`, not `s`). Decimal number (00--59 or 0--59).}
#' \item{`p`}{AM/PM indicator. Adding this will format the data as 12-h date-time format.}
#' }
#'
#'
#' @param x vector or dataframe object. Your input data. Date-time data should
#'   be in the first column, and can be in any POSIXct or character string
#'   format.
#' @param format a character string of date-time formats recognised by
#'   `lubridate`. Extended formats are also supported, based on
#'   `parse_date_time()`.
#' @param start numeric. Defaults to `0`. At what time (in seconds) should the
#'   formatted time data start?
#'
#' @return A data frame object.
#' @export
#'
#' @examples
#'
#' # convert year-month-day hour-min-sec
#' x <- c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11")
#' convert_time(x)
#' ## [1]    0 3600 4440
#'
#' # convert day-month-year hour-min
#' x <- c("03-02-09 01:11", "03-02-09 02:11","03-02-09 02:25")
#' convert_time(x, format = "dmyHM")
#' convert_time(x, format = "dmy HM")
#' convert_time(x, format = "dmy_HM")
#' convert_time(x, format = "d m y H M")
#' ## [1]    0 3600 4440
#'
#' # convert when AM/PM is present
#' x <- c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM")
#' convert_time(x, format = "dmyHMS") # this is wrong
#' convert_time(x, format = "dmyHMSp")
#' convert_time(x, format = "dmy HMS p")
#' convert_time(x, format = "dmy_HMS_p")
#' convert_time(x, format = "d m y H M S p")
#' ## [1]    0 3600 8040
#'
#' # convert dataframe with year-month-day hour-min-sec
#' x <- data.frame(
#'   x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
#'   y = c(23, 34, 45))
#' convert_time(x)
#'
#' # convert dataframe with multiple columns and non-default format
#' x <- data.frame(
#'   x = c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM"),
#'   y = c(23, 34, 45),
#'   z = c(56, 67, 78))
#' convert_time(x, format = "dmyHMSp")
#'
convert_time <- function(x, format = "ymd_HMS", start = 0) {
  # is x a data frame? If it is, extract from first column. Else, move along..
  if (is.data.frame(x)) {
    xtime <- x[[1]]
  } else xtime <- x
  xdate <- parse_date_time(xtime, format) # format to datetime
  time_num <- as.numeric(xdate) - min(as.numeric(xdate)) # convert to numeric
  time_adj <- time_num + start # adjust start time

  # output - vector or df?
  if(is.vector(x)) {
    out <- time_adj
  } else {
    out <- data.frame(time_adj, x[,-1])
    names(out) <- names(x)
  }
  return(out)
}

