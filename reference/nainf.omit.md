# Omit NA, NaN, Inf and -Inf from a vector or dataframe columns

For using with, for example, range to get axis range values in
'inspect'. Previously, na.omit was used, then discovered data files with
Inf values. This causes axis limit range to be Inf, and xlim/ylim don't
accept infinite axes!

## Usage

``` r
nainf.omit(x)
```

## Value

original vector without NA or Inf values or df all cols appended without
these

## Details

If x is dataframe, it returns a vector of all columns appended together.
Only useful for getting range in this case, don't use for anything else.
