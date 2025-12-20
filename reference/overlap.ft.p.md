# Plot convert_rate.ft summary tables

Plots `convert_rate.ft` summary table regressions in a way that
visualises how they are positioned within the data timeseries.

## Usage

``` r
overlap.ft.p(
  x,
  highlight = NULL,
  pos = NULL,
  legend = TRUE,
  quiet = FALSE,
  rate.rev = TRUE,
  msg = "overlap.ft.p",
  ...
)
```

## Arguments

- x:

  `convert_rate.ft`, `convert_rate.ft_select` object

- highlight:

  integer. Which result in the summary table to highlight on the plots.
  Defaults to 1. If it is outside the range of the summary rows it will
  default to 1.

- pos:

  integer(s). Choose which summary table rows to plot.

- legend:

  logical. Suppress plot legends.

- quiet:

  logical. Suppress console output.

- msg:

  string. For adding custom text to start of messages.

- ...:

  Allows additional plotting controls to be passed.

## Value

A plot of the auto_rate object results
