# format_time: Convert date-time data to numeric time

To prepare data and extract rates the functions in `respR` require data
be put into a very simple structure - numeric time against oxygen in any
common units in a `data.frame`. In other words, the time data must be as
numeric timestamps or time-elapsed values in seconds, minutes, hours or
days, each with a paired oxygen value.

If you have imported data (see
[`vignette("importing")`](https://januarharianto.github.io/respR/articles/importing.md))
but it does not contain a numeric time or timestamp column, the
[`format_time()`](https://januarharianto.github.io/respR/reference/format_time.md)
function can parse date-time columns to numeric time-elapsed values.
Internally, it uses functionality in the package
[`lubridate`](https://lubridate.tidyverse.org).

## `format_time()`

The date-times can either be passed as a `vector` (for example, so it
can be appended to the original data frame), or as a `data.frame`. If
the input is a vector, the output is a vector of the same length. If it
is a data frame, the `time` input indicates the column number (or
numbers, if they is split across several) containing the date-times, the
default being `time = 1`. The resulting data frame will be identical,
with a new column named `time.num` containing the converted numeric time
added as the *last* column.

We also need to specify the `format` of the date-times in the correct
order. See
[`help(format_time)`](https://januarharianto.github.io/respR/reference/format_time.md)
for further info.

**Note**: numeric time data will always output in *seconds* regardless
of the input format. These can easily be converted to other units using
simple arithmetic (e.g. to hours: `time.num/60/60`).

### Example

Here’s an example of a 2-column data frame with date-time data and
oxygen.

``` r

head(data, n = 5)
#>               Date_Time O2_mg/L
#>                  <char>   <num>
#> 1: 5/11/2017 9:24:04 AM  10.056
#> 2: 5/11/2017 9:24:05 AM  10.015
#> 3: 5/11/2017 9:24:06 AM  10.012
#> 4: 5/11/2017 9:24:07 AM  10.027
#> 5: 5/11/2017 9:24:08 AM  10.032
```

We will convert these as both `vector` and `data.frame` inputs.

``` r

## Pass as vector
data_2 <- format_time(data[[1]], format = "dmyHMSp")
head(data_2)
#> [1] 1 2 3 4 5 6

## Pass as data frame
data_3 <- format_time(data, time = 1, format = "dmyHMSp")
head(data_3)
#>               Date_Time O2_mg/L time_num
#>                  <char>   <num>    <num>
#> 1: 5/11/2017 9:24:04 AM  10.056        1
#> 2: 5/11/2017 9:24:05 AM  10.015        2
#> 3: 5/11/2017 9:24:06 AM  10.012        3
#> 4: 5/11/2017 9:24:07 AM  10.027        4
#> 5: 5/11/2017 9:24:08 AM  10.032        5
#> 6: 5/11/2017 9:24:09 AM  10.073        6
```

### Modifying start time

By default, the numeric time data will start at 1, but we can override
this. This could be useful if data are split into separate files, and
you want to append the start of one onto the end of another, or you
simply want to link a specific numeric time value to the start of the
experiment.

``` r

## as data frame
data_4 <- format_time(data, time = 1, format = "dmyHMSp", start = 1000)
head(data_4)
#>               Date_Time O2_mg/L time_num
#>                  <char>   <num>    <num>
#> 1: 5/11/2017 9:24:04 AM  10.056     1000
#> 2: 5/11/2017 9:24:05 AM  10.015     1001
#> 3: 5/11/2017 9:24:06 AM  10.012     1002
#> 4: 5/11/2017 9:24:07 AM  10.027     1003
#> 5: 5/11/2017 9:24:08 AM  10.032     1004
#> 6: 5/11/2017 9:24:09 AM  10.073     1005
```

### Split date-times

In some oxygen probe system output files, the date and times are split
across multiple columns. In these cases, formatting the time only data
usually works, however the more careful and robust approach is to use
both. This can be done by specifying multiple columns as the `time`
input. Note, the `format` should reflect the correct order.

These data have dates and times in different columns.

``` r

data
#>        date  time    oxy
#> 1 5/11/2017 23:00 10.056
#> 2 5/11/2017 23:30 10.015
#> 3 6/11/2017 00:00 10.012
#> 4 6/11/2017 00:30 10.027
#> 5 6/11/2017 01:00 10.032
#> 6 6/11/2017 01:30 10.073
```

We use `time` to specify both columns, and have `format` in the same
order.

``` r

format_time(data, time = 1:2, format = "dmyHM")
#>        date  time    oxy time_num
#> 1 5/11/2017 23:00 10.056        1
#> 2 5/11/2017 23:30 10.015     1801
#> 3 6/11/2017 00:00 10.012     3601
#> 4 6/11/2017 00:30 10.027     5401
#> 5 6/11/2017 01:00 10.032     7201
#> 6 6/11/2017 01:30 10.073     9001
```

### Time only data

Some oxygen probe system files have time values, but no dates. These can
also be parsed. This even works if the time values cross midnight. Note
time data with AM/PM should have a `p` appended to the format.

``` r

format_time(data, time = 1, format = "HMSp")
#> Times cross midnight, attempting to parse correctly...
#>          time    oxy time_num
#> 1 11:00:00 PM 10.056        1
#> 2 11:30:00 PM 10.015     1801
#> 3 00:00:00 AM 10.012     3601
#> 4 00:30:00 AM 10.027     5401
#> 5 01:00:00 AM 10.032     7201
#> 6 01:30:00 AM 10.073     9001
```

### Dealing with timed events or notes

What if there are important notes or events associated with specific
times in your experiment? For example, flushing of chambers, imposing a
treatment, changing the temperature, noting a response, etc. Resetting
the times via formatting the time data may make these notes difficult to
associate to certain stages of the analysis. This is easily dealt with
by formatting the times of the events in the same way you formatted the
data. You only need to make sure at least one event is associated with
the *same start time* used for the experimental data.

Here’s an example of some experimental notes (in some systems such notes
can be entered in the software, and so may be included in output files,
or they could be copied from a lab book into a csv file and imported).

``` r

exp_notes
#>                times                    events
#> 1  8/17/2016 9:42:02          Experiment start
#> 2  8/17/2016 9:52:02        Flush period start
#> 3  8/17/2016 9:54:34          Flush period end
#> 4 8/17/2016 10:19:02  Specimen acting normally
#> 5 8/17/2016 12:04:54             Went to lunch
#> 6 8/17/2016 14:31:22 Swim speed set to 20 cm/s

format_time(exp_notes, format = "mdyHMS")
#>                times                    events time_num
#> 1  8/17/2016 9:42:02          Experiment start        1
#> 2  8/17/2016 9:52:02        Flush period start      601
#> 3  8/17/2016 9:54:34          Flush period end      753
#> 4 8/17/2016 10:19:02  Specimen acting normally     2221
#> 5 8/17/2016 12:04:54             Went to lunch     8573
#> 6 8/17/2016 14:31:22 Swim speed set to 20 cm/s    17361
```

These do not have to be in the same date-time format, or even at the
same precision, depending on how accurately you need to know when events
occurred. The important factors are associating at least one event with
the *same start time* used to format the experimental time data, and
using the correct `format` setting.

``` r

format_time(exp_notes, format = "HM")
#>   times                    events time_num
#> 1  9:42          Experiment start        1
#> 2  9:52        Flush period start      601
#> 3  9:54          Flush period end      721
#> 4 10:19  Specimen acting normally     2221
#> 5 12:04             Went to lunch     8521
#> 6 14:31 Swim speed set to 20 cm/s    17341
```

## Checking output and next steps

After your data is in a paired, numeric oxygen~time structure, it can be
passed to
[`inspect()`](https://januarharianto.github.io/respR/reference/inspect.md)
to check the importing or time formatting has worked, look for common
errors, and visualise the dataset. See
[`vignette("inspecting")`](https://januarharianto.github.io/respR/articles/inspecting.md).
