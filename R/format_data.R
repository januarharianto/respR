#' @title Parse date-time data to numeric
#'
#' @description
#' \code{format_time} is a function to parse class POSIX.ct or text strings of date-time data to numeric time
#' for use in `respR` functions. 
#'
#' @details
#' 
#' ***Date-Time data inputs***
#' 
#' Input can be a vector, or data frame. If data frame, assumes date-times are in column 1. 
#' Date-time data can be unspaced or separated by any combination of spaces, forward slashes, 
#' hyphens, dots, commas, colons, semicolons, or underscores.  \cr
#' E.g. all these are parsed as the same date-time:  \cr
#' "2010-02-28 13:10:23", "20100228131023", "2010,02/28 13.10;23", "2010 02 28 13_10-23". 
#' 
#' - Times can be in 24H or 12H with AM/PM \cr
#' E.g. "2010-02-28 13:10:23" or "2010-02-28 1:10:23 PM"
#' 
#' - Times without initial zero are parsed as 24H time \cr
#' E.g. "1:10:23" is same as "1:10:23 AM" or "01:10:23"
#' 
#' - AM/PM take precedence over 24H formatting for 01-12h \cr
#' E.g. "1:10:23 PM" and "01:10:23 PM" are both same as "13:10:23"
#' 
#' - However 24H formatting for 13-24h takes precedence over AM/PM \cr
#' E.g. "13:10:23 AM" is identified as "1:10:23 PM" or "13:10:23"
#' 
#' Regardless of input, all data are parsed to numeric time data in seconds duration from 
#' the first entry, unless a `start` number is specified, in which case the series starts at
#' that number (in seconds) and all subsequent times are shifted forward by the same amount. 
#'
#' ***Date-Time formatting syntax***
#' 
#' `time_format` directly corresponds to functions in `lubridate` 
#' 
#' - Should be formatted as date (in lowercase y, m, d) and time (h, m, s) separated by 
#' an underscore. 
#' - E.g. "2010-02-28 13:10:23" would be `time_format = "ymd_hms"`
#' - Year-Month-Day can be in any order (ymd, ydm, dmy, etc.).
#' - Hours-minutes-seconds optional, but if present must be in correct order:
#' hms, hm, or h
#' - Time data without date can be supplied (`time_format = "hms", "hm", "h"`), but must 
#' not cross midnight. It should be formatted as 24h time, or if longer than 12h have AM/PM 
#' appended. 
#' - Obviously, single experiments will not be conducted across different time zones, so 
#' if a time zone is present, it is ignored for the purposes of calculating numeric times. 
#' 
#' ***Not implemented yet***
#' 
#' `event_names` and `event_times` are so events (e.g. flushes, changes in conditions) 
#' associated with original times in the experiment can be easily associated with new
#' time data.
#'
#' @usage
#' format_time(..., time_format = "ymd_hms", start = 0, event_names = NULL, event_times = NULL)
#' 
#' @md
#' @param ... vector or data frame containing strings or class POSIX.ct date-time data 
#'  to be converted to numeric. If a data frame, assumes these data are in column 1.
#' @param time_format string. Code describing structure of date-time data. See details. 
#'  Directly relates to functions in the package `lubridate`
#' @param start numeric. Default = 0. At what time (in seconds) should the formatted time 
#'  data start?
#' @param event_names String. Names for events during the experiment at the times in 
#'  `event_times`. E.g. "First flush", "Water speed to 5 cm/s". 
#' @param event_times String or class POSIX.ct date/time data matching the events in 
#'  `event_names`. Must be same format as in `...`
#'
#' @return A vector or data frame, depending on input.
#' 
#' @import glue
#' @import lubridate
#' @export

## To do
## add check for negative results in output? - one indicator of bad formatting
## support mins and hrs as output? - need extra argument

format_time <- function(x, time_format = "ymd_hms", start = 0, event_names = NULL, event_times = NULL){
  
  require(lubridate)
  require(glue)
  
  ## take out date/times
  if(is.data.frame(x)){
    times <- x[,1]
  } else {
    times <- x
  }
  
  ## if only Times supplied (time_format = "hms", "hm", or "h"), add in a dummy year-date
  if(time_format == "hms" || time_format == "hm" || time_format == "h"){
    ## need to makes times as.character first because R makes it a factor, 
    ## so glueing doesn't work properly
    times <- sapply(as.character(times), function(y) glue::glue("1900-01-01 ", y))
  }
  
  ## if only Times supplied, use appropriate lubridate function
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
  
  # make new posix time vector into numeric time difference from first entry
  times_numeric <- as.numeric(difftime(times_posixct,
                                       times_posixct[1], units="secs"))
  
  ## fix for start time
  times_numeric <- times_numeric + start
  
  # if input is a vector, return a vector, otherwise return a df
  if(is.vector(x) == TRUE){
    
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
    # class(output) <- "format_time"
    # return(output)
  }
  
}


## Tests
# 
# library(magrittr)
# 
# # 24h - default time_format
# ymd_hms_24h <- c("2010-02-01 11:10:23", "2010-02-01 12:10:23",
#                  "2010-02-01 13:10:23", "2010-02-01 14:10:23",
#                  "2010-02-01 15:10:23")
# ymd_hms_24h %>% respR::format_time()
# 
# # 12h - default time_format
# ymd_hms_12h_PM <- c("2010-02-01 11:10:23 AM", "2010-02-01 12:10:23 PM", 
#                     "2010-02-01 1:10:23 PM", "2010-02-01 2:10:23 PM",
#                     "2010-02-01 3:10:23 PM")
# ymd_hms_12h_PM %>% respR::format_time()
# 
# # different date order
# dmy_hms_24h <- c("01/02/2010 11:10:23", "01/02/2010 12:10:23",
#                  "01/02/2010 13:10:23", "01/02/2010 14:10:23",
#                  "01/02/2010 15:10:23")
# dmy_hms_24h %>% respR::format_time(time_format = "dmy_hms")
# 
# ## only hrs-mins
# dmy_hm_24h <- c("01/02/2010 11:10", "01/02/2010 12:10",
#                 "01/02/2010 13:10", "01/02/2010 14:10",
#                 "01/02/2010 15:10")
# dmy_hm_24h %>% respR::format_time(time_format = "dmy_hm")
# 
# dmy_hm_12h <- c("01/02/2010 11:10 AM", "01/02/2010 12:10 PM",
#                 "01/02/2010 1:10 PM", "01/02/2010 2:10 PM",
#                 "01/02/2010 3:10 PM")
# dmy_hm_12h %>% respR::format_time(time_format = "dmy_hm")
# 
# ## only hrs
# dmy_h_24h <- c("01/02/2010 11", "01/02/2010 12",
#                "01/02/2010 13", "01/02/2010 14",
#                "01/02/2010 15")
# dmy_h_24h %>% respR::format_time(time_format = "dmy_h")
# 
# dmy_h_12h <- c("01/02/2010 11 AM", "01/02/2010 12 PM",
#                "01/02/2010 1 PM", "01/02/2010 2 PM",
#                "01/02/2010 3 PM")
# dmy_h_12h %>% respR::format_time(time_format = "dmy_h")
# 
# 
# ## Times only
# ## Full times
# hms_24h <- c("11:10:23", "12:10:23",
#              "13:10:23", "14:10:23",
#              "15:10:23")
# hms_24h %>% respR::format_time(time_format = "hms")
# 
# hms_12h <- c("11:10:23 AM", "12:10:23 PM",
#              "1:10:23 PM", "2:10:23 PM",
#              "3:10:23 PM")
# hms_12h %>% respR::format_time(time_format = "hms")
# 
# ## hr-mins only
# hm_24h <- c("11:10", "12:10",
#             "13:10", "14:10",
#             "15:10")
# hm_24h %>% respR::format_time(time_format = "hm")
# 
# hm_12h <- c("11:10 AM", "12:10 PM",
#             "1:10 PM", "2:10 PM",
#             "3:10 PM")
# hm_12h %>% respR::format_time(time_format = "hm")
# 
# ## hrs only
# h_24h <- c("11", "12",
#            "13", "14",
#            "15")
# h_24h %>% respR::format_time(time_format = "h")
# 
# h_12h <- c("11 AM", "12 PM",
#            "1 PM", "2 PM",
#            "3 PM")
# h_12h %>% respR::format_time(time_format = "h")
# 
# 
# ## Changed start time
# ymd_hms_24h_alt_start <- c("2010-02-01 11:10:23", "2010-02-01 12:10:23",
#                            "2010-02-01 13:10:23", "2010-02-01 14:10:23",
#                            "2010-02-01 15:10:23")
# ymd_hms_24h_alt_start %>% respR::format_time(start = 100)
# 
# ## as df input
# ymd_hms_24h_df <- data.frame(time = c("2010-02-01 11:10:23", "2010-02-01 12:10:23",
#                                       "2010-02-01 13:10:23", "2010-02-01 14:10:23",
#                                       "2010-02-01 15:10:23"), o2 = c(100,90,80,70,60))
# ymd_hms_24h_df %>% respR::format_time()
# 
# ymd_hms_24h_df_times <- data.frame(time = c("11:10:23", "12:10:23",
#                                             "13:10:23", "14:10:23",
#                                             "15:10:23"), o2 = c(100,90,80,70,60))
# ymd_hms_24h_df_times %>% respR::format_time(time_format = "hms")
# 
# ymd_hm_12h_df_times <- data.frame(time = c("11:10 AM", "12:10 PM",
#                                            "1:10 PM", "2:10 PM",
#                                            "3:10 PM"), o2 = c(100,90,80,70,60))
# ymd_hm_12h_df_times %>% respR::format_time(time_format = "hm")
# 
# ## with time zone
# ymd_hms_24h_df_tz <- data.frame(time = c("2010-02-01 11:10:23 tz = UTC", "2010-02-01 12:10:23 tz = UTC",
#                                          "2010-02-01 13:10:23 tz = UTC", "2010-02-01 14:10:23 tz = UTC",
#                                          "2010-02-01 15:10:23 tz = UTC"), o2 = c(100,90,80,70,60))
# ymd_hms_24h_df_tz %>% respR::format_time()
# 
# ## with date-times already formatted as POSIX.ct
# ymd_hms_24h_df_posix <- data.frame(time = c("2010-02-01 11:10:23", "2010-02-01 12:10:23",
#                                             "2010-02-01 13:10:23", "2010-02-01 14:10:23",
#                                             "2010-02-01 15:10:23"), o2 = c(100,90,80,70,60))
# ymd_hms_24h_df_posix[,1] <- ymd_hms(ymd_hms_24h_df_posix[,1])
# str(ymd_hms_24h_df_posix)
# ymd_hms_24h_df_posix %>% respR::format_time()
# 
# ymd_hms_12h_df_posix <- data.frame(time = c("2010-02-01 11:10:23 AM", "2010-02-01 12:10:23 PM",
#                                             "2010-02-01 1:10:23 PM", "2010-02-01 2:10:23 PM",
#                                             "2010-02-01 3:10:23 PM"), o2 = c(100,90,80,70,60))
# ymd_hms_12h_df_posix[,1] <- ymd_hms(ymd_hms_12h_df_posix[,1])
# str(ymd_hms_12h_df_posix)
# ymd_hms_12h_df_posix %>% respR::format_time()
# 
