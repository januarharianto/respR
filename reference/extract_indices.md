# Extract row, time and DO indices from a subset dataframe

This is an internal function. Extracts row, time and DO values from a
data subset in a list.

## Usage

``` r
extract_indices(x, subsets, n)
```

## Arguments

- x:

  data frame.

- subsets:

  list of data frames.

- n:

  numeric. Choose which subset in the list to extract data from.

## Value

A \`data.table“ object.
