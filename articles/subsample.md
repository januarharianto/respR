# Thin larger datasets

Another useful utility function in `respR` in
[`subsample()`](https://januarharianto.github.io/respR/reference/subsample.md).
This is intended to thin or reduce the resolution of large datasets so
analysis processing times are reduced, which may be useful for
computationally intensive functions such as
[`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
and
[`oxy_crit()`](https://januarharianto.github.io/respR/reference/oxy_crit.md).

`respR` has been designed to be extremely fast in running analyses of
large datasets, and as computing power increases over time, there is
rarely any need to do this, and caution would suggest datasets are kept
complete where possible. We have found it useful primarily to reduce
data sizes to use with other
[packages](https://januarharianto.github.io/respR/articles/archive/auto_rate_comp.html)
which take a prohibitively long time to process data, so their results
can be compared with those of `respR`.

`subsample` has two ways of selecting the degree of subsampling. The `n`
input will cause every n’th element or row to be subsampled. The
probably more useful `length.out` input uniformly subsamples the data to
this total length or number of rows.

The function works with both vectors and data frames, and will output an
object of the same class. A plot is produced so you can check the
subsample is still representative of the original, or it can be
suppressed with `plot = FALSE`.

## Subsample by every n’th element

``` r

#' # Subsample by every 200th row:
subsample(squid.rd, n = 200, plot = FALSE)
```

## Subsample to a specific length

``` r

#' # Subsample to 100 rows:
subsample(sardine.rd, length.out = 100)
#> subsample: plotting first column of data only.
```

![Plot of sardine.rd respirometry data subsampled to 100 evenly-spaced
rows](subsample_files/figure-html/unnamed-chunk-2-1.png)

## Subsample a vector

``` r

subsample(sardine.rd[[2]], length.out = 500)
```

![Plot of sardine.rd oxygen vector subsampled to 500 evenly-spaced data
points](subsample_files/figure-html/unnamed-chunk-3-1.png)
