#' Parse date-time data to numeric
#'
#' A function to parse class POSIX.ct or text strings of date-time data to numeric time
#' for use in respR functions. 
#'
#' ** Date-Time data inputs **
#' Input can be a vector, or data frame. If data frame, assumes date-times are in column 1. 
#' Date-time data can be unspaced or separated by any combination of spaces, forward slashes, 
#' hyphens, dots, colons, semicolons, or underscores. E.g. all these are parsed the same:
#' "2010-02-28 13:10:23", "20100228 131023", "2010/02/28 13.10;23", "2010 02 28 13_10-23". 
#' 
#' Times can be in 24H or 12H with AM/PM. 
#' E.g. "2010-02-28 13:10:23" or "2010-02-28 1:10:23 PM"
#' 
#' AM/PM take precedence over 24H formatting for 01-12h.
#' E.g. "1:10:23 PM" and "01:10:23 PM" are both same as "13:10:23"
#' 
#' However 24H formatting for 13-24h takes precedence over AM/PM
#' E.g. "13:10:23 AM" is identified as "1:10:23 PM" or "13:10:23"
#' 
#' Regardless of input, all data are parsed to numeric time data in seconds duration from 
#' the first entry, unless a `start` number is specified in which case the series starts at
#' that number and all subsequent times are shifted forward by the same amount. 
#'
#' ** Date-Time formatting syntax **
#' 
#' `time_format` directly corresponds to functions in `lubridate`
#' 
#' Time format considerations
#' - Should be formatted as date (in lowercase y, m, d) and time (h, m, s) separated by 
#' an underscore. 
#' - E.g. "2010-02-28 13:10:23" would be "ymd_hms"
#' - Year-Month-Day can be in any order (ymd, ydm, dmy, etc.).
#' - Hours-minutes-seconds not required, but if present must be in correct order:
#' hms, hm, or h
#' - Time data without date can be supplied (format = "hms", "hm", "h"), but must not 
#' cross midnight, or dates are required. It should be formatted as 24h time, or if 
#' longer than 12h have AM/PM appended. 
#' 
#' Not implemented yet
#' `event_names` and `event_times` are so events (e.g. flushes, changes in conditions) 
#' associated with original times in the experiment can be easily associated with new
#' time data.
#'
#' @md
#' @param x vector or data frame containing strings or class POSIX.ct date-time data 
#'  to be converted to numeric. If a data frame, assumes these data are in column 1.
#' @param time_format string. Code describing structure of date-time data. See details. 
#'  Directly relates to functions in the package `lubridate`
#' @param start numeric. Default = 0. At what time (in seconds) should the formatted time 
#'  data start?
#' @param event_names. String. Names events during the experiment at the times in 
#'  `event_times`. E.g. "First flush", "Water speed to 5 cm/s". 
#' @param event_times. String or class POSIX.ct date/time data matching the events in 
#'  `event_names`
#'
#' @return A vector or data frame, depending on input.
#' 
#' @import glue
#' @import lubridate
#' @export

format_data <- function(x, time_format = "ymd_hms", start = 0, event_names = NULL, event_times = NULL){
  
  require(lubridate)
  require(glue)
  
  ## take out date/times
  if(is.data.frame(x)){
    times <- x[,1]
  } else {
    times <- x
  }
  
  ## if only h:m:s supplied (time_format = "hms", "hm", or "h"), add in a dummy year/date
  if(time_format == "hms" || time_format == "hm" || time_format == "h"){
    times <- sapply(times, function(x) glue::glue("1900-01-01 ", x))
  }
  
  ## if only time supplied, use appropriate lubridate function
  if(time_format == "hms") {
    times_posixct <- ymd_hms(times)
  } else if(time_format == "hm") {
    times_posixct <- ymd_hm(times)
  } else if(time_format == "h") {
    times_posixct <- ymd_h(times)
  # else glue (paste) time_format as lubridate function name
  # and run it with get
  } else {
    times_posixct <- get(glue::glue(time_format))(times)}
  
  # make new posix time vector into numeric time difference from start
  times_numeric <- as.numeric(difftime(times_posixct,
                                       times_posixct[1], units="secs"))
  
  ## fix for start time
  times_numeric <- times_numeric + start
  
  # if input is a vector, return a vector, otherwise return a df
  if(is.data.frame(x) == FALSE){
    
    return(times_numeric)
    
  } else {
    
    results_df <- data.frame(times_numeric, x[,-1])
    names(results_df) <- names(x) 
    # ^ possibly should rename 1st col 'numeric_time' or something similar. 
    # Original name might be inappropriate
    return(results_df)
    
    # For when we want to support passing this to inspect_data
    # output <- list()
    # output[[1]] <- x
    # output[[2]] <- time_format
    # output[[3]] <- start
    # output[[4]] <- event_names
    # output[[5]] <- event_times
    # output[[6]] <- results_df
    #
    # names(output) <- c("raw data", "time format", "start time",
    #                    "event names", "event times", "formatted results")
    #
    # class(output) <- "format_data"
    # return(output)
  }
  
}

# 
# # test data
# test_data <- c("2010-02-01 13:10:23", "2010-02-01 14:10:23", "2010-02-01 15:10:23",
#                "2010-02-01 16:10:23","2010-02-01 17:10:23")
# 
# test_data_tz <- c("2010-02-01 13:10:23 UTC", "2010-02-01 14:10:23 UTC",
#                   "2010-02-01 15:10:23 UTC", "2010-02-01 16:10:23 UTC",
#                   "2010-02-01 17:10:23 UTC")
# 
# format_data(c("2010-02-01 13:10:23", "2010-02-02 13:10:23"),
#             time_format = "ymd_hms")
# 
# format_data(c("2010-02-01_1 AM", "2010-02-02_2 PM"),
#             time_format = "ymd_h")
# 
# format_data(c("13:10:23", "13:10:33"),
#             time_format = "hms")
# 
# 
# 
# ## vector
# respR::format_data(test_data)
# 
# ## vector, change start time
# format_data(test_data, start = 100)
# 
# ## df
# temp_df <- format_data(test_data_df)
# 
# ## already formatted as posix.ct time
# temp_posix <- format_data(test_data_posix)
# 
# 
# temp_df$`raw data`
# temp_posix$`raw data`
# 
# str(temp_posix$`raw data`)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# test_data_df <- data.frame(time = test_data, o2 = c(10,9,8,7,6))
# 
# test_data_posix <- data.frame(time = test_data, o2 = c(10,9,8,7,6))
# test_data_posix[,1] <- ymd_hms(test_data_posix[,1])
# 
# test_data_posix_tz <- data.frame(time = test_data_tz, o2 = c(10,9,8,7,6))
# test_data_posix_tz[,1] <- ymd_hms(test_data_posix[,1])
# 
# str(test_data_posix_tz)
# 
# x <- test_data
# x <- c("13:10:23", "13:10:33")
# time_format <- "hms"