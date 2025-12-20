# Function for calc_rate with replicate number in summary

This is an internal function for
[`calc_rate.int()`](https://januarharianto.github.io/respR/reference/calc_rate.int.md).
Runs `calc_rate` and replaces the `$rep` column in `$summary` with the
`rep` input.

## Usage

``` r
calc_rate.rep(
  x,
  from = NULL,
  to = NULL,
  by = "time",
  plot = TRUE,
  rep = 1,
  supp.mess = TRUE,
  ...
)
```

## Arguments

- x:

  df or inspect obj

- from:

  calc_rate from

- to:

  calc_rate to

- by:

  calc_rate by

- plot:

  calc_rate plot

- rep:

  The replicate

- supp.mess:

  Suppress messages?

## Value

a `calc_rate` object with `summary$rep` filled in with appropriate
replicate number
