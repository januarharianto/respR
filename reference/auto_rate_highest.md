# Perform rolling regression and rank from ABSOLUTE highest to lowest

i.e. ignores sign. should only be used when rates are all negative or
all positive

## Usage

``` r
auto_rate_highest(dt, width, by = "row")
```

## Arguments

- dt:

  data.frame object.

- width:

  numeric.

- by:

  string.

## Value

a list object with appended class `auto_rate_highest`

## Details

This is an internal function for
[`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
