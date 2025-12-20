# Select rate results based on a range of criteria

The functions in `respR` are powerful, but outputs can be large and
difficult to explore, especially when there are hundreds to thousands of
results, for example the output of `auto_rate` on large datasets, or the
outputs of `calc_rate.int` from long intermittent-flow experiments.

The `select_rate` and `select_rate.ft` functions help explore, reorder,
and filter `convert_rate` and `convert_rate.ft` results according to
various criteria. For example, extracting only positive or negative
rates, only the highest or lowest rates, only those from certain data
regions, and numerous other methods that allow advanced filtering of
results so the final selection of rates is well-defined towards the
research question of interest. This also allows for highly consistent
reporting of results and rate selection criteria.

Multiple selection criteria can be applied by saving the output and
processing it through the function multiple times using different
methods, or alternatively via piping (`%>%` or `%>%`). See Examples.

*Note:* when choosing a `method`, keep in mind that to remain
mathematically consistent, `respR` outputs oxygen consumption (i.e.
respiration) rates as negative values. This is particularly important in
the difference between `highest/lowest` and `minimum/maximum` methods.
See Details.

When a rate result is omitted by the selection criteria, it is removed
from the `$rate.output` element of the `convert_rate` object, and the
associated data in `$summary` (i.e. that row) is removed. Some methods
can also be used with an `n = NULL` input to reorder the `$rate` and
`$summary` elements in various ways.

### Replicate and Rank columns

The summary table `$rank` column is context-specific, and what it
represents depends on the type of experiment analysed or the function
used to determine the rates. If numeric values were converted, it is the
order in which they were entered. Similarly, if `calc_rate` was used, it
is the order of rates as entered using `from` and `to` (if multiple
rates were determined). For `auto_rate` it relates to the `method`
input. For example it indicates the kernel density ranking if the
`linear` method was used, the ascending or descending ordering by
absolute rate value if `lowest` or `highest` were used, or the numerical
order if `minimum` or `maximum` were used. For intermittent-flow
experiments analysed via `calc_rate.int` and `auto_rate.int` these will
be ranked *within* each replicate as indicated in the `$rep` column. The
`$rep` and `$rank` columns can be used to keep track of selection or
reordering because the original values will be retained unchanged
through selection or reordering operations. The original order can
always be restored by using `method = "rep"` or `method = "rank"` with
`n = NULL`. In both these cases the `$summary` table and `$rate.output`
will be reordered by `$rep` (if used) then `$rank` to restore the
original ordering.

Note that if you are analysing intermittent-flow data and used
`auto_rate.int` but changed the `n` input to output more than one rate
result per replicate, the selection or reordering operations will not
take any account of this. You should carefully consider if or why you
need to output multiple rates per replicate in the first place. If you
have, you can perform selection on individual replicates by using
`method = "rep"` to select individual replicates then apply additional
selection criteria.

## Usage

``` r
select_rate(x, method = NULL, n = NULL)

select_rate.ft(x, method = NULL, n = NULL)
```

## Arguments

- x:

  list. An object of class `convert_rate` or `convert_rate_select`.

- method:

  string. Method by which to select or reorder rate results. For most
  methods matching results are *retained* in the output. See Details.

- n:

  numeric. Number, percentile, or range of results to retain or omit
  depending on `method`. Default is `NULL`, in which case some methods
  will instead reorder the results. See Details.

## Value

The output of `select_rate` is a `list` object which retains the
`convert_rate` class, with an additional `convert_rate_select` class
applied.

It contains two additional elements: `$original` contains the original,
unaltered `convert_rate` object, which will be retained unaltered
through multiple selection operations, that is even after processing
through the function multiple times. `$select_calls` contains the calls
for every selection operation that has been applied to the `$original`
object, from the first to the most recent. These additional elements
ensure the output contains the complete, reproducible history of the
`convert_rate` object having been processed.

## Details

These are the current methods by which rates in `convert_rate` objects
can be selected. Matching results are *retained* in the output. Some
methods can also be used to reorder the results. Note that the methods
selecting by rate value operate on the `$rate.output` element, that is
the final converted rate value.

### `positive`, `negative`

Selects all `positive` (\>0) or `negative` (\<0) rates. `n` is ignored.
Useful, for example, in respirometry on algae where both oxygen
consumption and production rates are recorded. Note, `respR` outputs
oxygen consumption (i.e. respiration) rates as *negative* values,
production rates as *positive*.

### `nonzero`, `zero`

Retains all `nonzero` rates (i.e. removes any zero rates), or retains
*only* `zero` rates (i.e. removes all rates with any value). `n` is
ignored.

### `lowest`, `highest`

These methods can only be used when rates all have the same sign, that
is are all negative or all positive. These select the lowest and highest
***absolute*** rate values. For example, if rates are all negative,
`method = 'highest'` will retain the highest magnitude rates regardless
of the sign. `n` should be an integer indicating the number of
lowest/highest rates to retain. If `n = NULL` the results will instead
be reordered by lowest or highest rate without any removed. See
`minimum` and `maximum` options for extracting *numerically* lowest and
highest rates.

### `lowest_percentile`, `highest_percentile`

These methods can also only be used when rates all have the same sign.
These retain the `n`'th lowest or highest percentile of ***absolute***
rate values. For example, if rates are all negative
`method = 'highest_percentile'` will retain the highest magnitude `n`'th
percentile regardless of the sign. `n` should be a percentile value
between 0 and 1. For example, to extract the lowest 10th percentile of
absolute rate values, you would enter
`method = 'lowest_percentile', n = 0.1`.

### `minimum`, `maximum`

In contrast to `lowest` and `highest`, these are *strictly numerical*
options which take full account of the sign of the rate, and can be used
where rates are a mix of positive and negative. For example,
`method = 'minimum'` will retain the minimum numerical value rates,
which would actually be the highest oxygen uptake rates. `n` is an
integer indicating how many of the min/max rates to retain. If
`n = NULL` the results will instead be reordered by minimum or maximum
rate without any removed.

### `minimum_percentile`, `maximum_percentile`

Like `min` and `max` these are *strictly numerical* inputs which retain
the `n`'th minimum or maximum percentile of the rates and take full
account of the sign. Here `n` should be a percentile value between 0
and 1. For example, if rates are all negative (i.e. typical uptake
rates), to extract the lowest 10th percentile of rates, you would enter
`method = 'maximum_percentile', n = 0.1`. This is because the *lowest*
negative rates are numerically the *maximum* rates (`highest/lowest`
percentile methods would be a better option in this case however).

### `rate`

Allows you to enter a value range of output rates to be retained.
Matching regressions in which the rate value falls within the `n` range
(inclusive) are retained. `n` should be a vector of two values. For
example, to retain only rates where the `rate` value is between 0.05 and
0.08: `method = 'rate', n = c(0.05, 0.08)`. Note this operates on the
`$rate.output` element, that is converted rate values.

### `rep`, `rank`

These refer to the respective columns of the `$summary` table. For
these, `n` should be a numeric vector of integers of `rep` or `rank`
values to retain. To retain a range use regular R syntax, e.g.
`n = 1:10`. If `n = NULL` no results will be removed, instead the
results will be reordered ascending by `rep` (if it contains values)
then `rank`. Essentially this restores the original ordering if other
reordering operations have been performed.

The values in these columns depend on the functions used to calculate
rates. If `calc_rate` was used, `rep` is `NA` and `rank` is the order of
rates as entered using `from` and `to` (if multiple rates were
determined). For `auto_rate`, `rep` is `NA` and `rank` relates to the
`method` input. For example it indicates the kernel density ranking if
the `linear` method was used, the ascending or descending ordering by
absolute rate value if `lowest` or `highest` were used, or by numerical
order if `minimum` or `maximum` were used. If `calc_rate.int` or
`auto_rate.int` were used, `rep` indicates the replicate number and the
`rank` column represents rank *within* the relevant replicate, and will
generally be filled with the value `1`. Therefore you need to adapt your
selection criteria appropriately towards which of these columns is
relevant.

### `rep_omit`, `rank_omit`

These refer to the `rep` and `rank` columns of the `$summary` table and
allow you to exclude rates from particular replicate or rank values. For
these, `n` should be a numeric vector of integers of `rep` or `rank`
values to OMIT. To omit a range use regular R syntax, e.g. `n = 1:10`.

### `rsq`, `row`, `time`, `density`

These methods refer to the respective columns of the `$summary` data
frame. For these, `n` should be a vector of two values. Matching
regressions in which the respective parameter falls within the `n` range
(inclusive) are retained. To retain all rates with a R-Squared 0.90 or
above: `method = 'rsq', n = c(0.9, 1)`. The `row` and `time` ranges
refer to the `$row`-`$endrow` or `$time`-`$endtime` columns and the
original raw data (`$dataframe` element of the `convert_rate` object),
and can be used to constrain results to rates from particular regions of
the data (although usually a better option is to
[`subset_data()`](https://januarharianto.github.io/respR/reference/subset_data.md)
prior to analysis). Note `time` is not the same as `duration` - see
later section - and `row` refers to rows of the raw data, **not** rows
of the summary table - see `manual` method for this. For all of these
methods, if `n = NULL` no results will be removed, instead the results
will be reordered by that respective column (descending for `rsq` and
`density`, ascending for `row`, and `time`).

### `intercept`, `slope`

These methods are similar to the above and refer to the `intercept_b0`
and `slope_b1` summary table columns. Note these linear model
coefficients represent different things in flowthrough vs. other
analyses. In non-flowthrough analyses slopes represent rates and
coefficients such as a high r-squared are important. In flowthrough,
slopes represent the stability of the data region, in that the closer
the slope is to zero, the less the delta oxygen values in that region
vary, which is an indication of a region of stable rates. In addition,
intercept values close to the calculated mean delta of the region also
indicate a region of stable rates. Therefore these methods are chiefly
useful in selection of flowthrough results, for example slopes close to
zero. If `n = NULL` no results will be removed, instead the results will
be reordered by ascending value by that column.

### `time_omit`, `row_omit`

These methods refer to the original data, and are intended to *exclude*
rates determined over particular data regions. This is useful in the
case of, for example, a data anomaly such as a spike or sensor dropout.
For these inputs, `n` are values (a single value, multiple values, or a
range) indicating data timepoints or rows of the original data to
exclude. Only rates (i.e. regressions) which *do not* utilise those
particular values are retained in the output. For example, if an anomaly
occurs precisely at timepoint 3000, `time_omit = 3000` means only rates
determined solely over regions before or after this will be retained. If
it occurs over a range this can be entered as,
`time_omit = c(3000,3200)`. If you want to exclude a regular occurrence,
for example the flushes in intermittent-flow respirometry, or any other
non-continuous values they can be entered as a vector, e.g.
`row_omit = c(1000, 2000, 3000)`. Note this last option can be extremely
computationally intensive when the vector or dataset is large, so should
only be used when a range cannot be entered as two values, which is much
faster. For both methods, input values must match exactly to values
present in the dataset.

### `oxygen`

This can be used to constrain rate results to regions of the data based
on oxygen values. `n` should be a vector of two values in the units of
oxygen in the raw data. Only rate regressions in which all datapoints
occur within this range (inclusive) are retained. Any which use even a
single value outside of this range are excluded. Note the summary table
columns `oxy` and `endoxy` refer to the first and last oxygen values in
the rate regression, which should broadly indicate which results will be
removed or retained, but this method examines *every* oxygen value in
the regression, not just first and last.

### `oxygen_omit`

Similar to `time_omit` and `row_omit` above, this can be used to *omit*
rate regressions which use particular oxygen values. For this `n` are
values (single or multiple) indicating oxygen values in the original raw
data to exclude. Every oxygen value used by each regression is checked,
and to be excluded an `n` value must match *exactly* to one in the data.
Therefore, note that if a regression is fit across the data region where
that value would occur, it is not necessarily excluded unless that
*exact value* occurs. You need to consider the precision of the data
values recorded. For example, if you wanted to exclude any rate using an
oxygen value of `7`, but your data are recorded to two decimals, a rate
fit across these data would *not* be excluded:
`c(7.03, 7.02, 7.01, 6.99, 6.98, ...)`. To get around this you can use
regular R syntax to input vectors at the correct precision, such as seq,
e.g. `seq(from = 7.05, to = 6.96, by = -0.01)`. This can be used to
input ranges of oxygen values to exclude.

### `duration`

This method allows selection of rates which have a specific duration
range. Here, `n` should be a numeric vector of two values. Use this to
set minimum and maximum durations in the time units of the original
data. For example, `n = c(0,500)` will retain only rates determined over
a maximum of 500 time units. To retain rates over a minimum duration,
set this using the minimum value plus the maximum duration or simply
infinity. For example, for rates determined over a minimum of 500 time
units `n = c(500,Inf)`)

### `manual`

This method simply allows particular rows of the `$summary` data frame
to be manually selected to be retained. For example, to keep only the
top row `method = 'manual', n = 1`. To keep multiple rows use regular
`R` selection syntax: `n = 1:3`, `n = c(1,2,3)`, `n = c(5,8,10)`, etc.
No value of `n` should exceed the number of rows in the `$summary` data
frame. Note this is not necessarily the same as selecting by the `rep`
or `rank` methods, as the table could already have undergone selection
or reordering.

### `manual_omit`

As above, but this allows particular rows of the `$summary` data frame
to be manually selected to be *omitted*.

### `overlap`

This method removes rates which overlap, that is regressions which are
partly or completely fit over the same rows of the original data. This
is useful in particular with `auto_rate` results. The `auto_rate`
`linear` method may identify multiple linear regions, some of which may
substantially overlap, or even be completely contained within others. In
such cases summary operations such as taking an average of the rate
values may be questionable, as certain values will be weighted higher
due to these multiple, overlapping results. This method removes
overlapping rates, using `n` as a threshold to determine degree of
permitted overlap. It is recommended this method be used after all other
selection criteria have been applied, as it is quite aggressive about
removing rates, and can be *very* computationally intensive when there
are many results.

While it can be used with `auto_rate` results determined via the
`rolling`, `lowest`, or `highest` methods, by their nature these methods
produce *all possible* overlapping regressions, ordered in various ways,
so other selection methods are more appropriate. The `overlap` method is
generally intended to be used in combination with the `auto_rate`
`linear` results, but may prove useful in other analyses.

Permitted overlap is determined by `n`, which indicates the proportion
of each particular regression which must overlap with another for it to
be regarded as overlapping. For example, `n = 0.2` means a regression
would have to overlap with at least one other by at least 20% of its
total length to be regarded as overlapping.

The `"overlap"` method performs two operations:

First, regardless of the `n` value, any rate regressions which are
completely contained within another are removed. This is also the only
operation if `n = 1`.

Secondly, for each regression in `$summary` starting from the bottom of
the summary table (usually the lowest ranked result, but this depends on
the analysis used and if any reordering has been already occurred), the
function checks if it overlaps with any others (accounting for `n`). If
not, the next lowest is checked, and the function progresses up the
summary table until it finds one that does. The first to be found
overlapping is then removed, and the process repeats starting again from
the bottom of the summary table. If no reordering to the results has
occurred, this means lower ranked results are removed first. This is
repeated iteratively until only non-overlapping rates (accounting for
`n`) remain.

If `n = 0`, only rates which do not overlap at all, that is share *no*
data, are retained. If `n = 1`, only rates which are 100% contained
within at least one other are removed.

### Reordering results

Several methods can be used to reorder results rather than select them,
by not entering an `n` input (that is, letting the `n = NULL` default be
applied). Several of these methods are named the same as those in
`auto_rate` for consistency and have equivalent outcomes, so this allows
results to be reordered to the equivalent of that method's results
without re-running the `auto_rate` analysis.

The `"row"` and `"rolling"` methods reorder sequentially by the starting
row of each regression (`$row` column).

The `"time"` method reorders sequentially by the starting time of each
regression (`$time` column).

`"linear"` and `"density"` are essentially identical, reordering by the
`$density` column. This metric is only produced by the `auto_rate`
`linear` method, so will not work with any other results.

`"rep"` or `"rank"` both reorder by the `$rep` then `$rank` columns.
What these represents is context dependent - see **Replicate and Rank
columns** section above. Each summary row `rep` and `rank` value is
retained unchanged regardless of how the results are subsequently
selected or reordered, so this will restore the original ordering after
other methods have been applied.

`"rsq"` reorders by `$rsq` from highest value to lowest.

`"intercept"` and `"slope"` reorder by the `$intercept_b0` and
`$slope_b1` columns from lowest value to highest.

`"highest"` and `"lowest"` reorder by absolute values of the
`$rate.output` column, that is highest or lowest in magnitude regardless
of the sign. They can only be used when rates all have the same sign.

`"maximum"` and `"minimum"` reorder by numerical values of the
`$rate.output` column, that is maximum or minimum in numerical value
taking account of the sign, and can be used when rates are a mix of
negative and positive.

### Numeric input conversions

For `convert_rate` objects which contain rates which have been converted
from numeric values, the summary table will contain a limited amount of
information, so many of the selection or reordering methods will not
work. In this case a warning is given and the original input is
returned.

### Plot

There is no plotting functionality in `select_rate`. However since the
output is a `convert_rate` object it can be plotted. See the **Plot**
section in
[`help("convert_rate")`](https://januarharianto.github.io/respR/reference/convert_rate.md).
To plot straight after a selection operation, pipe or enter the output
in [`plot()`](https://rdrr.io/r/graphics/plot.default.html). See
Examples.

### More

This help file can be found online
[here](https://januarharianto.github.io/respR/reference/select_rate.html),
where it is much easier to read.

For additional help, documentation, vignettes, and more visit the
`respR` website at <https://januarharianto.github.io/respR/>

## Examples

``` r
# \donttest{
## Object to filter
 ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
   auto_rate(plot = FALSE) %>%
   convert_rate(oxy.unit = "mg/L",
                time.unit = "s",
                output.unit = "mg/h",
                volume = 2.379) %>%
   summary()
#> inspect: Applying column default of 'time = 1'
#> inspect: Applying column default of 'oxygen = 2'
#> inspect: No issues detected while inspecting data frame.
#> 
#> # print.inspect # -----------------------
#>                 Time   O2
#> numeric         pass pass
#> Inf/-Inf        pass pass
#> NA/NaN          pass pass
#> sequential      pass    -
#> duplicated      pass    -
#> evenly-spaced   pass    -
#> 
#> -----------------------------------------
#> auto_rate: Applying default 'width' of 0.2
#> convert_rate: Object of class 'auto_rate' detected. Converting all rates in '$rate'.
#> 
#> # summary.convert_rate # ----------------
#> Summary of all converted rates:
#> 
#>     rep rank intercept_b0      slope_b1   rsq   density  row endrow time endtime  oxy endoxy          rate adjustment rate.adjusted    rate.input oxy.unit time.unit volume mass area  S  t  P  rate.abs rate.m.spec rate.a.spec output.unit rate.output
#>  1:  NA    1     8.523235 -0.0006079352 0.993 7411.6745 2201   3285 2200    3284 7.14   6.53 -0.0006079352         NA            NA -0.0006079352     mg/L       sec  2.379   NA   NA NA NA NA -5.206600          NA          NA     mgO2/hr   -5.206600
#>  2:  NA    2     7.112371 -0.0005658679 0.993 4432.7633  519   1564  518    1563 6.88   6.23 -0.0005658679         NA            NA -0.0005658679     mg/L       sec  2.379   NA   NA NA NA NA -4.846319          NA          NA     mgO2/hr   -4.846319
#>  3:  NA    3     2.917014  0.0010782058 0.704 1076.8737 3193   4069 3192    4068 6.61   7.11  0.0010782058         NA            NA  0.0010782058     mg/L       sec  2.379   NA   NA NA NA NA  9.234186          NA          NA     mgO2/hr    9.234186
#>  4:  NA    4     3.486415  0.0016056934 0.755  813.9211 1537   2414 1536    2413 6.26   7.06  0.0016056934         NA            NA  0.0016056934     mg/L       sec  2.379   NA   NA NA NA NA 13.751801          NA          NA     mgO2/hr   13.751801
#>  5:  NA    5     3.137644  0.0010175037 0.691  370.9729 3175   4087 3174    4086 6.60   7.07  0.0010175037         NA            NA  0.0010175037     mg/L       sec  2.379   NA   NA NA NA NA  8.714309          NA          NA     mgO2/hr    8.714309
#>  6:  NA    6     3.204332  0.0009992067 0.687  318.1555 3169   4092 3168    4091 6.60   7.06  0.0009992067         NA            NA  0.0009992067     mg/L       sec  2.379   NA   NA NA NA NA  8.557605          NA          NA     mgO2/hr    8.557605
#>  7:  NA    7     3.251506  0.0009862129 0.684  288.6348 3165   4096 3164    4095 6.61   7.03  0.0009862129         NA            NA  0.0009862129     mg/L       sec  2.379   NA   NA NA NA NA  8.446321          NA          NA     mgO2/hr    8.446321
#>  8:  NA    8     3.298448  0.0009732830 0.680  257.0379 3161   4100 3160    4099 6.61   7.08  0.0009732830         NA            NA  0.0009732830     mg/L       sec  2.379   NA   NA NA NA NA  8.335585          NA          NA     mgO2/hr    8.335585
#>  9:  NA    9     3.341216  0.0009613981 0.678  238.3070 3158   4105 3157    4104 6.61   7.08  0.0009613981         NA            NA  0.0009613981     mg/L       sec  2.379   NA   NA NA NA NA  8.233798          NA          NA     mgO2/hr    8.233798
#> 10:  NA   10     3.410602  0.0009422536 0.673  216.0462 3152   4112 3151    4111 6.61   7.06  0.0009422536         NA            NA  0.0009422536     mg/L       sec  2.379   NA   NA NA NA NA  8.069837          NA          NA     mgO2/hr    8.069837
#> 11:  NA   11     3.399817  0.0009452223 0.674  214.4826 3153   4111 3152    4110 6.61   7.06  0.0009452223         NA            NA  0.0009452223     mg/L       sec  2.379   NA   NA NA NA NA  8.095262          NA          NA     mgO2/hr    8.095262
#> 12:  NA   12     8.493468 -0.0005968452 0.994  204.3694 2125   3454 2124    3453 7.18   6.45 -0.0005968452         NA            NA -0.0005968452     mg/L       sec  2.379   NA   NA NA NA NA -5.111621          NA          NA     mgO2/hr   -5.111621
#> 13:  NA   13     8.488154 -0.0005949222 0.994  190.3709 2114   3464 2113    3463 7.21   6.45 -0.0005949222         NA            NA -0.0005949222     mg/L       sec  2.379   NA   NA NA NA NA -5.095151          NA          NA     mgO2/hr   -5.095151
#> 14:  NA   14     8.496112 -0.0005978305 0.994  189.7498 2128   3444 2127    3443 7.21   6.45 -0.0005978305         NA            NA -0.0005978305     mg/L       sec  2.379   NA   NA NA NA NA -5.120059          NA          NA     mgO2/hr   -5.120059
#> 15:  NA   15     3.706028  0.0014941168 0.743  155.6374 1512   2441 1511    2440 6.24   7.05  0.0014941168         NA            NA  0.0014941168     mg/L       sec  2.379   NA   NA NA NA NA 12.796214          NA          NA     mgO2/hr   12.796214
#> 16:  NA   16     3.713921  0.0014901077 0.743  154.6416 1511   2442 1510    2441 6.24   7.05  0.0014901077         NA            NA  0.0014901077     mg/L       sec  2.379   NA   NA NA NA NA 12.761878          NA          NA     mgO2/hr   12.761878
#> -----------------------------------------

 ## Select only negative rates
 ar_subs_neg <- select_rate(ar_obj, method = "negative") %>%
   summary()
#> select_rate: Object contains both negative and positive rates. Ensure the chosen `method` is appropriate.
#> select_rate: Selecting all negative rate values. 'n' input ignored...
#> ----- Selection complete. 11 rate(s) removed, 5 rate(s) remaining -----
#> 
#> # summary.convert_rate # ----------------
#> Summary of all converted rates:
#> 
#>    rep rank intercept_b0      slope_b1   rsq   density  row endrow time endtime  oxy endoxy          rate adjustment rate.adjusted    rate.input oxy.unit time.unit volume mass area  S  t  P  rate.abs rate.m.spec rate.a.spec output.unit rate.output
#> 1:  NA    1     8.523235 -0.0006079352 0.993 7411.6745 2201   3285 2200    3284 7.14   6.53 -0.0006079352         NA            NA -0.0006079352     mg/L       sec  2.379   NA   NA NA NA NA -5.206600          NA          NA     mgO2/hr   -5.206600
#> 2:  NA    2     7.112371 -0.0005658679 0.993 4432.7633  519   1564  518    1563 6.88   6.23 -0.0005658679         NA            NA -0.0005658679     mg/L       sec  2.379   NA   NA NA NA NA -4.846319          NA          NA     mgO2/hr   -4.846319
#> 3:  NA   12     8.493468 -0.0005968452 0.994  204.3694 2125   3454 2124    3453 7.18   6.45 -0.0005968452         NA            NA -0.0005968452     mg/L       sec  2.379   NA   NA NA NA NA -5.111621          NA          NA     mgO2/hr   -5.111621
#> 4:  NA   13     8.488154 -0.0005949222 0.994  190.3709 2114   3464 2113    3463 7.21   6.45 -0.0005949222         NA            NA -0.0005949222     mg/L       sec  2.379   NA   NA NA NA NA -5.095151          NA          NA     mgO2/hr   -5.095151
#> 5:  NA   14     8.496112 -0.0005978305 0.994  189.7498 2128   3444 2127    3443 7.21   6.45 -0.0005978305         NA            NA -0.0005978305     mg/L       sec  2.379   NA   NA NA NA NA -5.120059          NA          NA     mgO2/hr   -5.120059
#> -----------------------------------------

 ## Select only rates over 1000 seconds duration
 ar_subs_dur <- select_rate(ar_obj, method = "duration", n = c(1000, Inf)) %>%
   summary()
#> select_rate: Object contains both negative and positive rates. Ensure the chosen `method` is appropriate.
#> select_rate: Selecting rates with duration between 1000 and Inf...
#> ----- Selection complete. 11 rate(s) removed, 5 rate(s) remaining -----
#> 
#> # summary.convert_rate # ----------------
#> Summary of all converted rates:
#> 
#>    rep rank intercept_b0      slope_b1   rsq   density  row endrow time endtime  oxy endoxy          rate adjustment rate.adjusted    rate.input oxy.unit time.unit volume mass area  S  t  P  rate.abs rate.m.spec rate.a.spec output.unit rate.output
#> 1:  NA    1     8.523235 -0.0006079352 0.993 7411.6745 2201   3285 2200    3284 7.14   6.53 -0.0006079352         NA            NA -0.0006079352     mg/L       sec  2.379   NA   NA NA NA NA -5.206600          NA          NA     mgO2/hr   -5.206600
#> 2:  NA    2     7.112371 -0.0005658679 0.993 4432.7633  519   1564  518    1563 6.88   6.23 -0.0005658679         NA            NA -0.0005658679     mg/L       sec  2.379   NA   NA NA NA NA -4.846319          NA          NA     mgO2/hr   -4.846319
#> 3:  NA   12     8.493468 -0.0005968452 0.994  204.3694 2125   3454 2124    3453 7.18   6.45 -0.0005968452         NA            NA -0.0005968452     mg/L       sec  2.379   NA   NA NA NA NA -5.111621          NA          NA     mgO2/hr   -5.111621
#> 4:  NA   13     8.488154 -0.0005949222 0.994  190.3709 2114   3464 2113    3463 7.21   6.45 -0.0005949222         NA            NA -0.0005949222     mg/L       sec  2.379   NA   NA NA NA NA -5.095151          NA          NA     mgO2/hr   -5.095151
#> 5:  NA   14     8.496112 -0.0005978305 0.994  189.7498 2128   3444 2127    3443 7.21   6.45 -0.0005978305         NA            NA -0.0005978305     mg/L       sec  2.379   NA   NA NA NA NA -5.120059          NA          NA     mgO2/hr   -5.120059
#> -----------------------------------------

 ## Reorder rates sequentially (i.e. by starting row)
 ar_subs_dur <- select_rate(ar_obj, method = "row") %>%
   summary()
#> select_rate: Object contains both negative and positive rates. Ensure the chosen `method` is appropriate.
#> select_rate: Reordering results by 'row' method.
#> ----- Reordering complete. 16 rate(s) reordered by 'row' method -----
#> 
#> # summary.convert_rate # ----------------
#> Summary of all converted rates:
#> 
#>     rep rank intercept_b0      slope_b1   rsq   density  row endrow time endtime  oxy endoxy          rate adjustment rate.adjusted    rate.input oxy.unit time.unit volume mass area  S  t  P  rate.abs rate.m.spec rate.a.spec output.unit rate.output
#>  1:  NA    2     7.112371 -0.0005658679 0.993 4432.7633  519   1564  518    1563 6.88   6.23 -0.0005658679         NA            NA -0.0005658679     mg/L       sec  2.379   NA   NA NA NA NA -4.846319          NA          NA     mgO2/hr   -4.846319
#>  2:  NA   16     3.713921  0.0014901077 0.743  154.6416 1511   2442 1510    2441 6.24   7.05  0.0014901077         NA            NA  0.0014901077     mg/L       sec  2.379   NA   NA NA NA NA 12.761878          NA          NA     mgO2/hr   12.761878
#>  3:  NA   15     3.706028  0.0014941168 0.743  155.6374 1512   2441 1511    2440 6.24   7.05  0.0014941168         NA            NA  0.0014941168     mg/L       sec  2.379   NA   NA NA NA NA 12.796214          NA          NA     mgO2/hr   12.796214
#>  4:  NA    4     3.486415  0.0016056934 0.755  813.9211 1537   2414 1536    2413 6.26   7.06  0.0016056934         NA            NA  0.0016056934     mg/L       sec  2.379   NA   NA NA NA NA 13.751801          NA          NA     mgO2/hr   13.751801
#>  5:  NA   13     8.488154 -0.0005949222 0.994  190.3709 2114   3464 2113    3463 7.21   6.45 -0.0005949222         NA            NA -0.0005949222     mg/L       sec  2.379   NA   NA NA NA NA -5.095151          NA          NA     mgO2/hr   -5.095151
#>  6:  NA   12     8.493468 -0.0005968452 0.994  204.3694 2125   3454 2124    3453 7.18   6.45 -0.0005968452         NA            NA -0.0005968452     mg/L       sec  2.379   NA   NA NA NA NA -5.111621          NA          NA     mgO2/hr   -5.111621
#>  7:  NA   14     8.496112 -0.0005978305 0.994  189.7498 2128   3444 2127    3443 7.21   6.45 -0.0005978305         NA            NA -0.0005978305     mg/L       sec  2.379   NA   NA NA NA NA -5.120059          NA          NA     mgO2/hr   -5.120059
#>  8:  NA    1     8.523235 -0.0006079352 0.993 7411.6745 2201   3285 2200    3284 7.14   6.53 -0.0006079352         NA            NA -0.0006079352     mg/L       sec  2.379   NA   NA NA NA NA -5.206600          NA          NA     mgO2/hr   -5.206600
#>  9:  NA   10     3.410602  0.0009422536 0.673  216.0462 3152   4112 3151    4111 6.61   7.06  0.0009422536         NA            NA  0.0009422536     mg/L       sec  2.379   NA   NA NA NA NA  8.069837          NA          NA     mgO2/hr    8.069837
#> 10:  NA   11     3.399817  0.0009452223 0.674  214.4826 3153   4111 3152    4110 6.61   7.06  0.0009452223         NA            NA  0.0009452223     mg/L       sec  2.379   NA   NA NA NA NA  8.095262          NA          NA     mgO2/hr    8.095262
#> 11:  NA    9     3.341216  0.0009613981 0.678  238.3070 3158   4105 3157    4104 6.61   7.08  0.0009613981         NA            NA  0.0009613981     mg/L       sec  2.379   NA   NA NA NA NA  8.233798          NA          NA     mgO2/hr    8.233798
#> 12:  NA    8     3.298448  0.0009732830 0.680  257.0379 3161   4100 3160    4099 6.61   7.08  0.0009732830         NA            NA  0.0009732830     mg/L       sec  2.379   NA   NA NA NA NA  8.335585          NA          NA     mgO2/hr    8.335585
#> 13:  NA    7     3.251506  0.0009862129 0.684  288.6348 3165   4096 3164    4095 6.61   7.03  0.0009862129         NA            NA  0.0009862129     mg/L       sec  2.379   NA   NA NA NA NA  8.446321          NA          NA     mgO2/hr    8.446321
#> 14:  NA    6     3.204332  0.0009992067 0.687  318.1555 3169   4092 3168    4091 6.60   7.06  0.0009992067         NA            NA  0.0009992067     mg/L       sec  2.379   NA   NA NA NA NA  8.557605          NA          NA     mgO2/hr    8.557605
#> 15:  NA    5     3.137644  0.0010175037 0.691  370.9729 3175   4087 3174    4086 6.60   7.07  0.0010175037         NA            NA  0.0010175037     mg/L       sec  2.379   NA   NA NA NA NA  8.714309          NA          NA     mgO2/hr    8.714309
#> 16:  NA    3     2.917014  0.0010782058 0.704 1076.8737 3193   4069 3192    4068 6.61   7.11  0.0010782058         NA            NA  0.0010782058     mg/L       sec  2.379   NA   NA NA NA NA  9.234186          NA          NA     mgO2/hr    9.234186
#> -----------------------------------------

 ## Select rates with r-squared higher than 0.99,
 ## then select the lowest 10th percentile of the remaining rates,
 ## then take the mean of those
 inspect(squid.rd, plot = FALSE) %>%
   auto_rate(method = "linear",
             plot = FALSE) %>%
   convert_rate(oxy.unit = "mg/L",
                time.unit = "s",
                output.unit = "mg/h",
                volume = 2.379) %>%
   summary() %>%
   select_rate(method = "rsq", n = c(0.99, 1)) %>%
   select_rate(method = "lowest_percentile", n = 0.1) %>%
   mean()
#> inspect: Applying column default of 'time = 1'
#> inspect: Applying column default of 'oxygen = 2'
#> inspect: No issues detected while inspecting data frame.
#> 
#> # print.inspect # -----------------------
#>                 Time Oxygen
#> numeric         pass   pass
#> Inf/-Inf        pass   pass
#> NA/NaN          pass   pass
#> sequential      pass      -
#> duplicated      pass      -
#> evenly-spaced   pass      -
#> 
#> -----------------------------------------
#> auto_rate: Applying default 'width' of 0.2
#> convert_rate: Object of class 'auto_rate' detected. Converting all rates in '$rate'.
#> 
#> # summary.convert_rate # ----------------
#> Summary of all converted rates:
#> 
#>     rep rank intercept_b0       slope_b1   rsq   density   row endrow  time endtime      oxy   endoxy           rate adjustment rate.adjusted     rate.input oxy.unit time.unit volume mass area  S  t  P   rate.abs rate.m.spec rate.a.spec output.unit rate.output
#>  1:  NA    1     7.725675 -0.00030671573 1.000 41732.895  7587  13849  7586   13848 5.402033 3.490669 -0.00030671573         NA            NA -0.00030671573     mg/L       sec  2.379   NA   NA NA NA NA -2.6268362          NA          NA     mgO2/hr  -2.6268362
#>  2:  NA    2     7.696412 -0.00030427959 1.000 30776.050  2354  13848  2353   13847 7.005635 3.490669 -0.00030427959         NA            NA -0.00030427959     mg/L       sec  2.379   NA   NA NA NA NA -2.6059721          NA          NA     mgO2/hr  -2.6059721
#>  3:  NA    3     1.696591 -0.00001356731 0.986 17165.553 27029  33129 27028   33128 1.336335 1.247246 -0.00001356731         NA            NA -0.00001356731     mg/L       sec  2.379   NA   NA NA NA NA -0.1161958          NA          NA     mgO2/hr  -0.1161958
#>  4:  NA    4     7.746758 -0.00031634557 1.000  9137.385   491   6484   490    6483 7.580664 5.701696 -0.00031634557         NA            NA -0.00031634557     mg/L       sec  2.379   NA   NA NA NA NA -2.7093100          NA          NA     mgO2/hr  -2.7093100
#>  5:  NA    5     7.524215 -0.00028980112 0.999  4170.603 10296  16202 10295   16201 4.559737 2.867046 -0.00028980112         NA            NA -0.00028980112     mg/L       sec  2.379   NA   NA NA NA NA -2.4819727          NA          NA     mgO2/hr  -2.4819727
#>  6:  NA    6     7.524647 -0.00028983504 0.999  4081.274 10293  16198 10292   16197 4.551638 2.867046 -0.00028983504         NA            NA -0.00028983504     mg/L       sec  2.379   NA   NA NA NA NA -2.4822632          NA          NA     mgO2/hr  -2.4822632
#>  7:  NA    7     7.616206 -0.00029707913 0.999  4064.522  9530  15430  9529   15429 4.802707 3.061422 -0.00029707913         NA            NA -0.00029707913     mg/L       sec  2.379   NA   NA NA NA NA -2.5443045          NA          NA     mgO2/hr  -2.5443045
#>  8:  NA    8     7.399963 -0.00028073360 0.999  2957.126 11172  17064 11171   17063 4.300569 2.640274 -0.00028073360         NA            NA -0.00028073360     mg/L       sec  2.379   NA   NA NA NA NA -2.4043148          NA          NA     mgO2/hr  -2.4043148
#>  9:  NA    9     7.221442 -0.00026856267 0.999  2902.230 12191  18085 12190   18084 3.976609 2.405403 -0.00026856267         NA            NA -0.00026856267     mg/L       sec  2.379   NA   NA NA NA NA -2.3000781          NA          NA     mgO2/hr  -2.3000781
#> 10:  NA   10     7.228643 -0.00026904096 0.999  2855.974 12148  18046 12147   18045 3.992807 2.413502 -0.00026904096         NA            NA -0.00026904096     mg/L       sec  2.379   NA   NA NA NA NA -2.3041744          NA          NA     mgO2/hr  -2.3041744
#> 11:  NA   11     3.869899 -0.00009633980 0.984  2510.612 20621  26514 20620   26513 1.919463 1.360632 -0.00009633980         NA            NA -0.00009633980     mg/L       sec  2.379   NA   NA NA NA NA -0.8250926          NA          NA     mgO2/hr  -0.8250926
#> 12:  NA   12     3.872769 -0.00009646109 0.985  2428.173 20610  26507 20609   26506 1.919463 1.360632 -0.00009646109         NA            NA -0.00009646109     mg/L       sec  2.379   NA   NA NA NA NA -0.8261313          NA          NA     mgO2/hr  -0.8261313
#> 13:  NA   13     2.353593 -0.00003656900 0.887  2425.710 24183  30078 24182   30077 1.522612 1.287741 -0.00003656900         NA            NA -0.00003656900     mg/L       sec  2.379   NA   NA NA NA NA -0.3131916          NA          NA     mgO2/hr  -0.3131916
#> 14:  NA   14     2.555133 -0.00004402287 0.904  2285.575 23732  29621 23731   29620 1.563107 1.295840 -0.00004402287         NA            NA -0.00004402287     mg/L       sec  2.379   NA   NA NA NA NA -0.3770294          NA          NA     mgO2/hr  -0.3770294
#> 15:  NA   15     2.621934 -0.00004652541 0.910  2276.370 23586  29475 23585   29474 1.579305 1.295840 -0.00004652541         NA            NA -0.00004652541     mg/L       sec  2.379   NA   NA NA NA NA -0.3984623          NA          NA     mgO2/hr  -0.3984623
#> 16:  NA   16     6.921750 -0.00024956371 0.998  2196.575 13606  19510 13605   19509 3.547362 2.105740 -0.00024956371         NA            NA -0.00024956371     mg/L       sec  2.379   NA   NA NA NA NA -2.1373634          NA          NA     mgO2/hr  -2.1373634
#> 17:  NA   17     3.193194 -0.00006863164 0.966  2137.336 22329  28218 22328   28217 1.708889 1.312038 -0.00006863164         NA            NA -0.00006863164     mg/L       sec  2.379   NA   NA NA NA NA -0.5877888          NA          NA     mgO2/hr  -0.5877888
#> 18:  NA   18     3.238228 -0.00007042412 0.968  2128.287 22224  28114 22223   28113 1.716988 1.312038 -0.00007042412         NA            NA -0.00007042412     mg/L       sec  2.379   NA   NA NA NA NA -0.6031404          NA          NA     mgO2/hr  -0.6031404
#> 19:  NA   19     3.529099 -0.00008216956 0.980  2064.339 21528  27423 21527   27422 1.806077 1.320137 -0.00008216956         NA            NA -0.00008216956     mg/L       sec  2.379   NA   NA NA NA NA -0.7037330          NA          NA     mgO2/hr  -0.7037330
#> 20:  NA   20     3.527953 -0.00008212276 0.980  2061.832 21531  27426 21530   27425 1.806077 1.320137 -0.00008212276         NA            NA -0.00008212276     mg/L       sec  2.379   NA   NA NA NA NA -0.7033322          NA          NA     mgO2/hr  -0.7033322
#> 21:  NA   21     4.618462 -0.00012945932 0.986  1627.855 18663  24558 18662   24557 2.275819 1.482117 -0.00012945932         NA            NA -0.00012945932     mg/L       sec  2.379   NA   NA NA NA NA -1.1087414          NA          NA     mgO2/hr  -1.1087414
#> 22:  NA   22     6.256476 -0.00021131531 0.990  1483.947 15476  21362 15475   21361 3.053323 1.830374 -0.00021131531         NA            NA -0.00021131531     mg/L       sec  2.379   NA   NA NA NA NA -1.8097888          NA          NA     mgO2/hr  -1.8097888
#> 23:  NA   23     5.003125 -0.00014746532 0.983  1483.345 17884  23770 17883   23769 2.445898 1.563107 -0.00014746532         NA            NA -0.00014746532     mg/L       sec  2.379   NA   NA NA NA NA -1.2629520          NA          NA     mgO2/hr  -1.2629520
#> 24:  NA   24     6.536083 -0.00022690222 0.994  1479.930 14858  20751 14857   20750 3.223402 1.903265 -0.00022690222         NA            NA -0.00022690222     mg/L       sec  2.379   NA   NA NA NA NA -1.9432814          NA          NA     mgO2/hr  -1.9432814
#> 25:  NA   25     6.410919 -0.00021986472 0.993  1479.678 15148  21037 15147   21036 3.142412 1.870869 -0.00021986472         NA            NA -0.00021986472     mg/L       sec  2.379   NA   NA NA NA NA -1.8830094          NA          NA     mgO2/hr  -1.8830094
#> 26:  NA   26     5.448574 -0.00016920383 0.984  1444.875 17037  22925 17036   22924 2.648373 1.644097 -0.00016920383         NA            NA -0.00016920383     mg/L       sec  2.379   NA   NA NA NA NA -1.4491293          NA          NA     mgO2/hr  -1.4491293
#> 27:  NA   27     5.447812 -0.00016916519 0.984  1427.124 17038  22927 17037   22926 2.648373 1.635998 -0.00016916519         NA            NA -0.00016916519     mg/L       sec  2.379   NA   NA NA NA NA -1.4487984          NA          NA     mgO2/hr  -1.4487984
#> 28:  NA   28     5.612589 -0.00017745603 0.985  1414.277 16720  22608 16719   22607 2.721264 1.676493 -0.00017745603         NA            NA -0.00017745603     mg/L       sec  2.379   NA   NA NA NA NA -1.5198044          NA          NA     mgO2/hr  -1.5198044
#> 29:  NA   29     5.447593 -0.00016915367 0.984  1412.792 17038  22928 17037   22927 2.648373 1.635998 -0.00016915367         NA            NA -0.00016915367     mg/L       sec  2.379   NA   NA NA NA NA -1.4486997          NA          NA     mgO2/hr  -1.4486997
#> 30:  NA   30     5.529651 -0.00017326545 0.984  1404.338 16879  22768 16878   22767 2.688868 1.660295 -0.00017326545         NA            NA -0.00017326545     mg/L       sec  2.379   NA   NA NA NA NA -1.4839146          NA          NA     mgO2/hr  -1.4839146
#> 31:  NA   31     5.477106 -0.00017062925 0.984  1399.533 16981  22871 16980   22870 2.656472 1.644097 -0.00017062925         NA            NA -0.00017062925     mg/L       sec  2.379   NA   NA NA NA NA -1.4613371          NA          NA     mgO2/hr  -1.4613371
#>     rep rank intercept_b0       slope_b1   rsq   density   row endrow  time endtime      oxy   endoxy           rate adjustment rate.adjusted     rate.input oxy.unit time.unit volume mass area  S  t  P   rate.abs rate.m.spec rate.a.spec output.unit rate.output
#> -----------------------------------------
#> select_rate: Selecting rates with rsq values between 0.99 and 1...
#> ----- Selection complete. 18 rate(s) removed, 13 rate(s) remaining -----
#> select_rate: Selecting lowest 10th percentile of *absolute* rate values...
#> ----- Selection complete. 11 rate(s) removed, 2 rate(s) remaining -----
#> 
#> # mean.convert_rate # -------------------
#> Mean of all rate results:
#> 
#> Mean of 2 output rates:
#> [1] -1.846399
#> [1] "mgO2/hr"
#> -----------------------------------------
   # }
```
