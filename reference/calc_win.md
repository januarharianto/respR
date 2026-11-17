# Automatically calculate rolling window

The calculated value is used to determine the rolling window for rolling
regressions.

## Usage

``` r
calc_win(dt, width, by, msg, rep = NULL)
```

## Arguments

- dt:

  data.frame object.

- width:

  numeric.

- by:

  string.

- msg:

  string. Attach function name for message (usually auto_rate)

- rep:

  optional replicate number to include in window-size errors.

## Value

a numeric
