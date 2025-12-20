# Function for auto_rate with replicate number in summary plus limit to n results

This is an internal function for
[`auto_rate.int()`](https://januarharianto.github.io/respR/reference/auto_rate.int.md).
Runs `auto_rate` and replaces the `$rep` column in `$summary` with the
`rep` input. Also limits number of results returned using `n`.

## Usage

``` r
auto_rate.rep(
  x,
  method = "linear",
  width = NULL,
  by = "row",
  plot = TRUE,
  rep = 1,
  n = 1,
  rep_row = NULL,
  meas_row = NULL,
  meas_endrow = NULL,
  rep_data = NULL,
  ...
)
```

## Arguments

- x:

  df or inspect obj

- method:

  auto_rate method

- width:

  auto_rate width

- by:

  auto_rate by

- plot:

  auto_rate plot

- rep:

  The replicate

- n:

  number of results to return

- rep_row:

  start row of this rep in the larger dataset

- meas_row:

  start row of the measure phase in *this* rep

- meas_endrow:

  end row of the measure phase in *this* rep

- rep_data:

  df of the entire replicate. Used in plotting.

- ...:

  pass plot stuff

## Value

a `auto_rate` object with `summary$rep` filled in with appropriate
replicate number
