#' Parse date-time data to numeric
#'
#' A function to parse class POSIX.ct or text strings of date-time data to
#' numeric time for use in `respR` functions.
#'
#' **Input**
#'
#' Input can be a vector, or data frame. If a data frame, assumes date-time is
#' in column 1. Ideal structure for further processing in `respR` is a 2 column
#' data frame of Time and O2, however any multiple column data frame can be used
#' as long as the date-time to be parsed is in column 1. The output data frame
#' will be identical, except the original date-time column will be replaced by
#' the new numeric time data.
#'
#' Date-time data can be unspaced or separated by any combination of spaces,
#' forward slashes, hyphens, dots, commas, colons, semicolons, or underscores.
#'
#' E.g. all these are parsed as the same date-time: `"2010-02-28
#' 13:10:23", "20100228131023", "2010,02/28 13.10;23", "2010 02 28 13_10-23"`.
#'
#' - Times can be in 24H or 12H with AM/PM \cr E.g. "2010-02-28 13:10:23" or
#' "2010-02-28 1:10:23 PM"
#'
#' - Times without initial zero are parsed as 24H time \cr E.g. "1:10:23" is
#' same as "1:10:23 AM" or "01:10:23"
#'
#' - AM/PM take precedence over 24H formatting for 01-12h \cr E.g. "1:10:23 PM"
#' and "01:10:23 PM" are both same as "13:10:23"
#'
#' - However, 24H formatting for 13-24h takes precedence over AM/PM \cr E.g.
#' "13:10:23 AM" is identified as "1:10:23 PM" or "13:10:23"
#'
#' Regardless of input, all data are parsed to numeric time data in seconds
#' duration from the first entry, unless a `start` number is specified, in which
#' case the series starts at that number (in seconds) and all subsequent times
#' are shifted forward by the same amount.
#'
#' **Syntax**
#'
#' Simply identify the order which the year, month, day, and time appears in
#' your date-time input.
#'
#' \describe{ \item{`d`}{Day of the month as decimal number (01--31 or 0--31).}
#' \item{`m`}{Month of the year as decimal number (01--12 or 1--12).}
#' \item{`y`}{Year (2010, 2001, 1989).} \item{`H`}{Hour, must be capitalised
#' (`H`, not `h`). Decimal number (00--24 or 0--24).} \item{`M`}{Minute, must be
#' capitalised (`M`, not `m`). Decimal number (00--59 or 0--59).}
#' \item{`S`}{Second, must be capitalised (`S`, not `s`). Decimal number (00--59
#' or 0--59).} \item{`p`}{AM/PM indicator. Adding this will format the data as
#' 12-h date-time format.} }
#'
#' Print the order in the `format` string argument, using separators if you
#' choose to (optional): `"dmyHMS"`; `"dmy_HMS"` and `"d m y H M S"` are all the
#' same. Obviously, single experiments will not be conducted across different time
#' zones, so if a time zone is present, it is ignored for the purposes of
#' calculating numeric times.
#'
#' @param x vector or data frame containing strings or class POSIX.ct date-time
#'   data to be converted to numeric. If a data frame, assumes these data are in
#'   column 1.
#' @param format string. Code describing structure of date-time data. See
#'   details. Directly relates to functions in the package `lubridate`
#' @param start numeric. Default = 0. At what time (in seconds) should the
#'   formatted time data start?
#' @return A vector or data frame, depending on input. If a data frame, the
#'   output data frame is identical, except the original date-time data in
#'   column 1 will be replaced by a new column, `Time`, of numeric time data in
#'   seconds.
#'
#' @importFrom lubridate parse_date_time
#' @export
#' @seealso \code{\link{lubridate}}
#' @examples
#' # convert year-month-day hour-min-sec
#' x <- c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11")
#' format_time(x)
#' ## [1]    0 3600 4440
#'
#' # convert day-month-year hour-min
#' x <- c("03-02-09 01:11", "03-02-09 02:11","03-02-09 02:25")
#' format_time(x, format = "dmyHM")
#' format_time(x, "dmy HM")
#' format_time(x, "dmy_HM")
#' format_time(x, "d m y H M")
#' ## [1]    0 3600 4440
#'
#' # convert when AM/PM is present
#' x <- c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM")
#' format_time(x, format = "dmyHMS") # this is wrong
#' format_time(x, "dmyHMSp")
#' format_time(x, "dmy HMS p")
#' format_time(x, "dmy_HMS_p")
#' format_time(x, "d m y H M S p")
#' ## [1]    0 3600 8040
#'
#' # convert dataframe with year-month-day hour-min-sec
#' x <- data.frame(
#'   x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
#'   y = c(23, 34, 45))
#' format_time(x)
#'
#' # convert dataframe with multiple columns and non-default format
#' x <- data.frame(
#'   x = c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM"),
#'   y = c(23, 34, 45),
#'   z = c(56, 67, 78))
#' format_time(x, format = "dmyHMSp")
format_time <- function(x, format = "ymdHMS", start = 0) {
  ## take out date/times
  if(is.data.frame(x)){
    times <- x[,1]
  } else {
    times <- x
  }
  dates <- lubridate::parse_date_time(times, format) # format to datetime
  # convert to numeric:
  times_numeric <- as.numeric(difftime(dates, dates[1], units="secs"))
  out <- times_numeric + start # adjust start time, if needed

  # output - return vector or df
  if(is.vector(x)) {
    return(out)
  } else {
    out <- data.frame(times_numeric, x[,-1]) # replace original date-time column
    names(out) <- names(x)  # rename
    return(out)
  }

}
