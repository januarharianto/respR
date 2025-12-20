# Perform `auto_rate()` iteratively and extract performance metrics

Randomly generate a dataset and runs
[`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
on the data to detect linear regions (with `method = "linear"`). The
function plots 4 exploratory graphs and outputs the results of a linear
regression between detected rate and true (known) rate, which can
demonstrate how much the function is able to predict true rate.

## Usage

``` r
test_lin(
  reps = 1,
  len = 300,
  sd = 0.05,
  type = "default",
  preview = FALSE,
  plot = FALSE
)
```

## Arguments

- reps:

  numeric. Number of times to iterate
  [`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
  on a randomly generated dataset. Defaults to 1.

- len:

  numeric. Length (number of observations) of the dataset to test
  [`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
  on. Defaults to 300.

- sd:

  numeric. Noise to add to the data. Defaults to .05 standard
  difference.

- type:

  character. Use "default", "corrupted" or "segmented" to pick one of
  the three different kinds of data to generate.

- preview:

  logical. This will show the randomly-generated data in your plot
  window at every iteration. **Note: will slow the function down.**
  Useful to see the shape of the data. Defaults to FALSE.

- plot:

  logical. This will show the diagnostic plots of
  [`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
  at every iteration. **Note: will severely slow the function down.**
  Useful to visualise what's being detected at every step. Defaults to
  FALSE.

## Value

An object of class `test_lin`. Contains linear regression results, and
data required to plot diagnostics.

## Examples

``` r
# run 3 iterations (please run at least 1000 times for more reliable visuals)
x <- test_lin(reps = 3)
#> auto_rate: Applying default 'width' of 0.2
#> auto_rate: Applying default 'width' of 0.2
#> auto_rate: Applying default 'width' of 0.2
# plot(x)
# plot(x, "a")  # view only plot "A"
# plot(x, "d")  # view only plot "D". You know what to do (for other plots).
```
