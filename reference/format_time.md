# Parse date-time data to numeric time for use in respR functions

A function to parse class POSIX.ct or text strings of date-time data to
numeric time for use in `respR` functions.

## Usage

``` r
format_time(x, time = 1, format = "ymdHMS", start = 1)
```

## Arguments

- x:

  vector or data frame containing strings or class POSIX.ct date-time
  data to be converted to numeric.

- time:

  numeric value or vector. Specifies column(s) containing date-time
  data. Default is `1`.

- format:

  string. Code describing structure of date-time data. See Details.

- start:

  numeric. At what time (in seconds) should the formatted time data
  start? Default is `1`.

## Value

**Output**: If the input is a vector, output is a vector of equal length
containing the numeric time data. For data frame inputs, an identical
data frame is returned, with a new column named `time_num` added as the
**final** column.

## Details

Regardless of input, all data are parsed to numeric time data in seconds
duration from the first entry starting at 1. If you want the times to
start at a different time, a `start` value can be specified, in which
case the series starts at that number (in seconds) and all subsequent
times are shifted forward by the same amount.

### Input

Input can be a vector, or data frame. If a data frame, the column(s) of
the date-time data are specified using the `time` input. By default the
first column is assumed to contain the date-time data (i.e. `time = 1`).

If the date-time data is split over several columns (e.g. date in one
column, time in another), multiple columns can be specified (e.g.
`time = c(1,2)`). In this case, the `format` setting should reflect the
correct order as entered in `time`.

### Time only data

Time-only data, that is times which lack an associated date, can also be
parsed. Normally, parsing time-only data will cause problems when the
times cross midnight (i.e. `00:00:00`). However, the function attempts
to identify when this occurs and parse the data correctly.

### Formatting

See the `lubridate` package for more detail on acceptable formatting.

Date-time data can be unspaced or separated by any combination of
spaces, forward slashes, hyphens, dots, commas, colons, semicolons, or
underscores. E.g. all these are parsed as the same date-time:
`"2010-02-28 13:10:23", "20100228131023", "2010,02/28 13.10;23", "2010 02 28 13_10-23"`.

- Times can be in 24H or 12H with AM/PM  
  E.g. "2010-02-28 13:10:23" or "2010-02-28 1:10:23 PM"

- Times without initial zero are parsed as 24H time  
  E.g. "1:10:23" is same as "1:10:23 AM" or "01:10:23"

- AM/PM take precedence over 24H formatting for 01-12h  
  E.g. "1:10:23 PM" and "01:10:23 PM" are both same as "13:10:23"

- However, 24H formatting for 13-24h takes precedence over AM/PM  
  E.g. "13:10:23 AM" is identified as "1:10:23 PM" or "13:10:23"

### Syntax of 'format' input

Specify the order of year, month, day, and time in your date-time input.

`d` - Day of the month as decimal number (01–31 or 1–31).

`m` - Month of the year as decimal number (01–12 or 1–12).

`y` - Year (2010, 2001, 1989).

`H` - Hour as decimal number (00–24 or 0–24 or 00-12 (see `p`)).

`M` - Minute as decimal number (00–59 or 0–59).

`S` - Second as decimal number (00–59 or 0–59).

`p` - AM/PM indicator for 12-h date-time format (e.g. "01/12/2020
1:30:44 PM " would be `"dmyHMSp"`).

Specify the order using the `format` input, using separators or not
(optional): `"dmyHMS"`; `"dmy_HMS"` and `"d m y H M S"` are all the
same. See Examples.

Single experimental datasets should never span different time zones, so
if a time zone is present it is ignored for the purposes of calculating
numeric times.

### More

For additional help, documentation, vignettes, and more visit the
`respR` website at <https://januarharianto.github.io/respR/>

## Examples

``` r
# Convert year-month-day hour-min-sec
x <- c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11")
format_time(x)
#> [1]    1 3601 4441

# Convert day-month-year hour-min, and use a separator in the format
x <- c("03-02-09 01:11", "03-02-09 02:11","03-02-09 02:25")
format_time(x, format = "dmy_HM")
#> [1]    1 3601 4441

# Convert when AM/PM is present
x <- c("09-02-03 11:11:11 AM", "09-02-03 12:11:11 PM","09-02-03 01:25:11 PM")
# This is WRONG - the AM/PM indicator is missing
format_time(x, format = "dmyHMS")
#> Times cross midnight, attempting to parse correctly... 
#> [1]     1  3601 51241
# This is correct
format_time(x, format = "dmyHMSp")
#> [1]    1 3601 8041

# Convert dataframe with year-month-day hour-min-sec (ymdHMS default)
x <- data.frame(
  x = c("09-02-03 01:11:11", "09-02-03 02:11:11","09-02-03 02:25:11"),
  y = c(23, 34, 45))
format_time(x, time = 1)
#>                   x  y time_num
#> 1 09-02-03 01:11:11 23        1
#> 2 09-02-03 02:11:11 34     3601
#> 3 09-02-03 02:25:11 45     4441

# Convert dataframe with time in a different column and non-default format
x <- data.frame(
  x = c(23, 34, 45),
  y = c("09-02-2018 11:11:11 AM", "09-02-2018 12:11:11 PM","09-02-2018 01:25:11 PM"),
  z = c(56, 67, 78))
format_time(x, time = 2, format = "dmyHMSp")
#>    x                      y  z time_num
#> 1 23 09-02-2018 11:11:11 AM 56        1
#> 2 34 09-02-2018 12:11:11 PM 67     3601
#> 3 45 09-02-2018 01:25:11 PM 78     8041

# Convert dataframe with separate date and time columns, and times crossing midnight
x <- data.frame(
  w = c("09-02-18", "09-02-18","10-02-18"),
  x = c("22:11:11", "23:11:11","00:25:11"),
  y = c(23, 34, 45),
  z = c(56, 67, 78))
# Crosses midnight, but parses correctly even without dates
format_time(x, time = 2, format = "HMS")
#> Times cross midnight, attempting to parse correctly... 
#>          w        x  y  z time_num
#> 1 09-02-18 22:11:11 23 56        1
#> 2 09-02-18 23:11:11 34 67     3601
#> 3 10-02-18 00:25:11 45 78     8041
# Include dates to double check
format_time(x, time = 1:2, format = "dmyHMS")
#>          w        x  y  z time_num
#> 1 09-02-18 22:11:11 23 56        1
#> 2 09-02-18 23:11:11 34 67     3601
#> 3 10-02-18 00:25:11 45 78     8041
# Input same as different column order & appropriate format order
format_time(x, time = 2:1, format = "HMSdmy")
#>          w        x  y  z time_num
#> 1 09-02-18 22:11:11 23 56        1
#> 2 09-02-18 23:11:11 34 67     3601
#> 3 10-02-18 00:25:11 45 78     8041

# Convert a data frame with date and time split over multiple columns
x <- data.frame(
  u = c("09", "09","10"),
  v = c("02", "02","02"),
  w = c("2018", "2018","2018"),
  x = c("22:11:11", "23:11:11","00:25:11"),
  y = c(23, 34, 45),
  z = c(56, 67, 78))
format_time(x, time = 1:4, format = "dmyHMS")
#>    u  v    w        x  y  z time_num
#> 1 09 02 2018 22:11:11 23 56        1
#> 2 09 02 2018 23:11:11 34 67     3601
#> 3 10 02 2018 00:25:11 45 78     8041
```
