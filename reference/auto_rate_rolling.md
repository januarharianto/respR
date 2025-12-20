# Perform rolling regression of fixed width and do not reorder results

This is an internal function for
[`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)

## Usage

``` r
auto_rate_rolling(dt, width, by = "row")
```

## Arguments

- dt:

  data.frame object.

- width:

  numeric.

- by:

  string.

## Value

a list object with appended class `auto_rate_rolling`
