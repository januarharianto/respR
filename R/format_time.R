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
#' input except a new column called `time.num` is added as the last column.
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
#'   `time.num`,of numeric time data in seconds is added as the last column.
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

  ## if multiple columns specified
  if(length(time) > 1){
    ## extract columns
    ## ifs here cos of WTF STUPID STUPID data table subsetting syntax
    if(is.data.table(x)) times <- x[,time, with = F]
    if(!is.data.table(x)) times <- x[,time]
    ## concatenate
    times <- apply(times, 1, function(x) paste(x, collapse = " "))
  }

  ## take out date/times
  if(is.data.frame(x) && length(time) == 1){
    times <- x[[time]]
  } else if(!is.data.frame(x)){
    times <- x
  }

  ## change if factor
  if(is.factor(times)) times <- as.character(times)

  # format to datetime
  dates <- lubridate::parse_date_time(times, format)

  # convert to numeric:
  times_numeric <- as.numeric(difftime(dates, dates[1], units="secs"))

  # Check if time crosses midnight
  # Should result in sudden time difference of -86400 ish
  # Though depends on the recording frequency
  # This *should* catch them
  if(any(diff(times_numeric) < -30000)){
    message("Times cross midnight, attempting to parse correctly... ")

    ## This is index of last date/time before midnight
    ## May be more than once for very long experiments
    locs <- which(abs(diff(times_numeric)) > 30000)

    ## We change the dummy dates lubridates adds before and after these
    ## rows

    ## loop to create correct number of new dates
    add_dates <- c()
    for(i in 1:(length(locs)+1)){
      add_dates[i] <- paste0("0000-01-0", i)
    }
    ## start/end row locations for new dates
    locs <- c(1, locs, locs+1, length(times_numeric))
    locs <- locs[order(locs)]

    # df to use in loop of start row, end row, new date
    row_dates <- data.frame(a = c(locs[seq(1, length(locs), 2)]),
                            b = c(locs[seq(2, length(locs), 2)]),
                            c = add_dates)

    # length(dates)
    new_dates <- c()
    for(i in 1:nrow(row_dates)){
      yyy <- gsub("0000-01-01", row_dates[i,3], dates[row_dates[i,1]:row_dates[i,2]])
      new_dates <- c(new_dates, yyy)
    }

    ## reparse new date/times
    new_format <- paste0("ymd", format)
    new_dates_posix <- lubridate::parse_date_time(new_dates, new_format)
    times_numeric <- as.numeric(difftime(new_dates_posix, new_dates_posix[1], units="secs"))

    print_midnight <- function() {
      cat("\n")

      rr <- locs[(locs %% 2 == 0)]
      ifelse(length(rr) > 1, rr1 <- rr[-length(rr)], rr1 <- rr)
      rr2 <- rr1 + 1
      rx1 <- x[rr1]
      rx2 <- x[rr2]

      for(i in 1:length(rr1)){

        if(i==1){cat("row", "       time", "         time.num", "\n")}
        cat("---\n")
        cat(rr1[i], "    ", times[rr1[i]], "    ", times_numeric[rr1[i]] + start, "\n")
        cat(rr2[i], "    ", times[rr2[i]], "    ", times_numeric[rr2[i]] + start, "\n")
        cat("---\n")
        cat("\n")

      }}

    if(any(diff(times_numeric) < 0)){
      message("Parsing of time-only data unsuccessful: \nNon-sequential numeric time values found.")
    } else {
      message("Parsing of time-only data successful.")
      message("Check these data locations carefully:")
      print_midnight()
    }

  }

  # adjust start time, if needed
  out <- times_numeric + start

  # output - return vector or df
  if(is.vector(x)) {
    return(out)
  } else {
    col_nms <- names(x)  # rename
    out <- cbind(x, out) # add new column as LAST col
    names(out) <- c(col_nms, "time.num")  # rename
    return(out)
  }

}
