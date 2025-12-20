# Plot inspect.ft objects

Plot inspect.ft objects

## Usage

``` r
# S3 method for class 'inspect.ft'
plot(
  x,
  width = NULL,
  pos = NULL,
  quiet = FALSE,
  legend = TRUE,
  rate.rev = TRUE,
  ...
)
```

## Arguments

- x:

  inspect.ft object

- width:

  numeric. Smoothing factor (rolling mean) for delta oxygen values as
  proportion of total data length (0 to 1)

- pos:

  integer. Which result to plot.

- quiet:

  logical. Suppress console output.

- legend:

  logical. Suppress labels and legends.

- rate.rev:

  logical. Control direction of y-axis in delta oxygen plot.

- ...:

  Pass additional plotting parameters

## Value

A plot. No returned value.
