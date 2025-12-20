# Perform a linear regression on a data frame

This is an internal function. Performs `lm` on a data frame object and
returns its coefficients.

## Usage

``` r
linear_fit(dt)
```

## Arguments

- dt:

  data frame.

## Value

A data frame object of [`lm()`](https://rdrr.io/r/stats/lm.html)
coefficients.
