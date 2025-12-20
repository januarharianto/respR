# Plot convert_rate objects

Plot convert_rate objects

## Usage

``` r
# S3 method for class 'convert_rate'
plot(
  x,
  type = "full",
  pos = NULL,
  quiet = FALSE,
  highlight = NULL,
  legend = TRUE,
  rate.rev = TRUE,
  ...
)
```

## Arguments

- x:

  convert_rate object

- type:

  "full", "rate", "overlap"

- pos:

  Which summary rows to plot?

- quiet:

  logical. Suppress console output.

- highlight:

  Which to highlight in overlap plots.

- legend:

  logical. Suppress labels and legends.

- rate.rev:

  logical. Control direction of y-axis in rate plot.

- ...:

  Pass additional plotting parameters

## Value

A plot. No returned value.
