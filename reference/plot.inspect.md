# Plot inspect objects

Plot inspect objects

## Usage

``` r
# S3 method for class 'inspect'
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

  inspect object

- width:

  numeric. Width of rolling regression to determine rates in rolling
  rate plot as proportion of total data length (0 to 1)

- pos:

  integer. Which result to plot.

- quiet:

  logical. Suppress console output.

- legend:

  logical. Suppress labels and legends.

- rate.rev:

  logical. Control direction of y-axis in rolling rate plot.

- ...:

  Pass additional plotting parameters

## Value

A plot. No returned value.
