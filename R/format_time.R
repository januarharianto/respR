#' Parse date-time data to numeric
#'
#' A function to parse class POSIX.ct or text strings of date-time data to
#' numeric time for use in `respR` functions.
#'
#' **Input**
#'
#' Input can be a vector, or data frame. If a vector, output is a vector of
#' equal length containing numeric time data. If a data frame, the column index
#' of the date-time data is specified using the `time =` input. By default the
#' first column is assumed to contain the date-time data (i.e. `time = 1`).
#'
#' Multiple columns can be specified (e.g. `time = c(1, 2)`) if the date-time
#' data is split over multiple columns (e.g. date in one column, time in
#' another). The function will combine these date and time strings together for
#' conversion. If multiple columns are specified, the `format` setting should
#' reflect the same order entered in `time`.
#'
#' For data frame inputs, a data frame is returned which is identical to the
#' input except a new column called `time_num` is added as the last column.
#'
#' Time-only data, that is times which lack an associated date, can also be
#' parsed. Normally, parsing time-only data will cause problems when the times
#' cross midnight (i.e. 00:00:00). However, the function attempts to identify
#' these occurences and parse the data correctly. It also prints a message with
#' the locations of these data regions for the user to check they look ok.
#'
#' Date-time data can be unspaced or separated by any combination of spaces,
#' forward slashes, hyphens, dots, commas, colons, semicolons, or underscores.
#'
#' E.g. all these are parsed as the same date-time: `"2010-02-28 13:10:23",
#' "20100228131023", "2010,02/28 13.10;23", "2010 02 28 13_10-23"`.
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
#' duration from the first entry starting at 1. However, if you want the times
#' to satrt at a different time, a `start` value can be specified, in which
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
#' same. Single datasets should not span different time zones, so if a time zone
#' is present it is ignored for the purposes of calculating numeric times. If
#' multiple columns have been specified in the `time` argument, the `format`
#' should reflect the same order.
#'
#' @param x vector or data frame containing strings or class POSIX.ct date-time
#'   data to be converted to numeric.
#' @param time numeric value or vector. Specifies column(s) containing date-time
#'   data
#' @param format string. Code describing structure of date-time data. See
#'   details. Directly relates to functionality in the package `lubridate`
#' @param start numeric. Default = 1. At what time (in seconds) should the
#'   formatted time data start?
#' @return A vector or data frame, depending on input. If input is a vector, a
#'   vector of same length containing numeric time is returned. If input is a
#'   data frame, the output data frame is identical, except a new column,
#'   `time_num`,of numeric time data in seconds is added as the last column.
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
#' ## [1]    0 3600 4440
#'
#' # convert when AM/PM is present
#' x <- c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM")
#' format_time(x, format = "dmyHMS") # this is wrong
#' format_time(x, format = "dmyHMSp")
#' ## [1]    0 3600 8040
#'
#' # convert dataframe with year-month-day hour-min-sec (ymdHMS default)
#' x <- data.frame(
#'   x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
#'   y = c(23, 34, 45))
#' format_time(x, time = 1)
#'
#' # convert dataframe with time in different column and non-default format
#' x <- data.frame(
#'   x = c(23, 34, 45),
#'   y = c("09-02-2018 11:11:11 AM", "09-02-2018 12:11:11 PM","09-02-2018 01:25:11 PM"),
#'   z = c(56, 67, 78))
#' format_time(x, time = 2, format = "dmyHMSp")
#'
#' # convert dataframe with separate date and time columns crossing midnight
#' x <- data.frame(
#'   w = c("09-02-18", "09-02-18","10-02-18"),
#'   x = c("22:11:11", "23:11:11","00:25:11"),
#'   y = c(23, 34, 45),
#'   z = c(56, 67, 78))
#' format_time(x, time = 2, format = "HMS") # WRONG! Crosses midnight
#' format_time(x, time = c(1,2), format = "dmyHMS") # Correct
#' format_time(x, time = c(2,1), format = "HMSdmy") # Different column order & format
#'
#' # convert dataframe with multiple date and time columns
#' x <- data.frame(
#'   v = c("09-02", "09-02","10-02"),
#'   w = c("2018", "2018","2018"),
#'   x = c("22:11:11", "23:11:11","00:25:11"),
#'   y = c(23, 34, 45),
#'   z = c(56, 67, 78))
#' format_time(x, time = c(1:3), format = "dmyHMS")

format_time <- function(x, time = 1, format = "ymdHMS", start = 1) {

    dt <- data.table(x)  # convert to data.table object, regardless of type

  # check if object is data frame and multiple time columns are specified
  # the time columns will be concatenated and then saved for further analysis
  if (length(time) > 1) {
    dt <- dt[, time, with = FALSE]  # extract columns
    # join date time columns and subset the result:
    ts <- dt[, ts := do.call(paste, c(.SD, sep = " "))]
    ts <- ts[, -time, with = FALSE]
  } else if (length(time) == 1) {
    ts <- dt[, time, with = FALSE]  # extract columns
  } else ts <- x # otherwise assume that object is a vector list of date(s)
  # end if

  # ERROR CHECK(S)
  # TODO: gotta add tryCatch() here if parse_date_time() fails.


  # formate to datetime, then convert to interval in seconds:
  dtm <- lubridate::parse_date_time(unlist(ts), format) # format to datetime
  # calculate the difference in all times, from starting time (dtm[1])
  intervals <- as.numeric(difftime(dtm, dtm[1], units = "secs"))

  # quality check - does time cross midnight?
  # usually happens when no date is provided
  if (any(diff(intervals) < 0)) {
    message("Time(s) cross midnight, attempting to parse correctly... ")
    # if format is "HMS", assume that neg interval time indicates midnight cross
    # i.e. switch from 23:59:59 to 00:00:00
    # create index to determine locations that signal different days
    # indx <- which(diff(intervals) < 0)
    indx <- c(0, which(diff(intervals) < 0), length(intervals))
    # generate new data based on index values and adjust dummy dates
    new_dtm <- lapply(1:(length(indx) - 1), function(i) {
      dtm[(indx[i] + 1):indx[i + 1]] + lubridate::days(i - 1)
    })
    new_dtm <- do.call("c", new_dtm)  # combine the output into a list
    # recalculate intervals with the corrected data:
    intervals <- as.numeric(difftime(new_dtm, new_dtm[1], units = "secs"))
  }  # end if

  # ERROR CHECK(S)
  # sometimes even if there is an output it may be wrong because data structure
  #   is wrong - so we check for issues before we continue.

  # check for non-sequential time
  if (check_seq(intervals)$check) stop("Parsing of time-only data unsuccessful:
    Non-sequential numeric time values found.")

  # adjust start time, if specified
  intervals <- intervals + start

  # output results
  # if original object was a data frame object, we append data on the right
  if (is.data.table(x)) {
    out <- data.table(x, time_num = intervals)
  } else if (is.data.frame(x)) {
    out <- cbind(x, time_num = intervals)
  } else {
    # otherwise just output the data as vector:
    out <- intervals
  }  # end if

  return(out)

}  # end format_time()
