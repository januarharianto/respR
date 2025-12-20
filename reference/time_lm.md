# Subset data by time and perform a linear regression.

This is an internal function. Used with
[`time_roll()`](https://januarharianto.github.io/respR/reference/time_roll.md)
and
[`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md).

## Usage

``` r
time_lm(df, start, end)
```

## Arguments

- df:

  data.frame object.

- start:

  numeric. start time

- end:

  numeric. end time

## Value

a data.table object
