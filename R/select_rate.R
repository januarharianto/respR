#' @title Select rate results based on a range of criteria
#'
#' @description The functions in `respR` are powerful, but outputs can be large
#'   and difficult to explore, especially when there are hundreds to thousands
#'   of results, for example the output of `auto_rate` on large datasets, or the
#'   outputs of `calc_rate.int` from long intermittent-flow experiments.
#'
#'   The `select_rate` and `select_rate.ft` functions help explore, reorder, and
#'   filter `convert_rate` and `convert_rate.ft` results according to various
#'   criteria. For example, extracting only positive or negative rates, only the
#'   highest or lowest rates, only those from certain data regions, and numerous
#'   other methods that allow advanced filtering of results so the final
#'   selection of rates is well-defined towards the research question of
#'   interest. This also allows for highly consistent reporting of results and
#'   rate selection criteria.
#'
#'   Multiple selection criteria can be applied by saving the output and
#'   processing it through the function multiple times using different methods,
#'   or alternatively via piping (`|>` or `%>%`). See Examples.
#'
#'   *Note:* when choosing a `method`, keep in mind that to remain
#'   mathematically consistent, `respR` outputs oxygen consumption (i.e.
#'   respiration) rates as negative values. This is particularly important in
#'   the difference between `highest/lowest` and `minimum/maximum` methods. See
#'   Details.
#'
#'   When a rate result is omitted by the selection criteria, it is removed from
#'   the `$rate.output` element of the `convert_rate` object, and the associated
#'   data in `$summary` (i.e. that row) is removed. Some methods can also be
#'   used with an `n = NULL` input to reorder the `$rate` and `$summary`
#'   elements in various ways.
#'
#'   ## Replicate and Rank columns
#'
#'   The summary table `$rank` column is context-specific, and what it
#'   represents depends on the type of experiment analysed or the function used
#'   to determine the rates. If numeric values were converted, it is the order
#'   in which they were entered. Similarly, if `calc_rate` was used, it is the
#'   order of rates as entered using `from` and `to` (if multiple rates were
#'   determined). For `auto_rate` it relates to the `method` input. For example
#'   it indicates the kernel density ranking if the `linear` method was used,
#'   the ascending or descending ordering by absolute rate value if `lowest` or
#'   `highest` were used, or the numerical order if `minimum` or `maximum` were
#'   used. For intermittent-flow experiments analysed via `calc_rate.int` and
#'   `auto_rate.int` these will be ranked *within* each replicate as indicated
#'   in the `$rep` column. The `$rep` and `$rank` columns can be used to keep
#'   track of selection or reordering because the original values will be
#'   retained unchanged through selection or reordering operations. The original
#'   order can always be restored by using `method = "rep"` or `method = "rank"`
#'   with `n = NULL`. In both these cases the `$summary` table and
#'   `$rate.output` will be reordered by `$rep` (if used) then `$rank` to
#'   restore the original ordering.
#'
#'   Note that if you are analysing intermittent-flow data and used
#'   `auto_rate.int` but changed the `n` input to output more than one rate
#'   result per replicate, the selection or reordering operations will not take
#'   any account of this. You should carefully consider if or why you need to
#'   output multiple rates per replicate in the first place. If you have, you
#'   can perform selection on individual replicates by using `method = "rep"` to
#'   select individual replicates then apply additional selection criteria.
#'
#' @details These are the current methods by which rates in `convert_rate`
#'   objects can be selected. Matching results are *retained* in the output.
#'   Some methods can also be used to reorder the results. Note that the methods
#'   selecting by rate value operate on the `$rate.output` element, that is the
#'   final converted rate value.
#'
#'   ## `positive`, `negative`
#'
#'   Selects all `positive` (>0) or `negative` (<0) rates. `n` is ignored.
#'   Useful, for example, in respirometry on algae where both oxygen consumption
#'   and production rates are recorded. Note, `respR` outputs oxygen consumption
#'   (i.e. respiration) rates as *negative* values, production rates as
#'   *positive*.
#'
#'   ## `nonzero`, `zero`
#'
#'   Retains all `nonzero` rates (i.e. removes any zero rates), or retains
#'   *only* `zero` rates (i.e. removes all rates with any value). `n` is
#'   ignored.
#'
#'   ## `lowest`, `highest`
#'
#'   These methods can only be used when rates all have the same sign, that is
#'   are all negative or all positive. These select the lowest and highest
#'   ***absolute*** rate values. For example, if rates are all negative, `method
#'   = 'highest'` will retain the highest magnitude rates regardless of the
#'   sign. `n` should be an integer indicating the number of lowest/highest
#'   rates to retain. If `n = NULL` the results will instead be reordered by
#'   lowest or highest rate without any removed. See `minimum` and `maximum`
#'   options for extracting *numerically* lowest and highest rates.
#'
#'   ## `lowest_percentile`, `highest_percentile`
#'
#'   These methods can also only be used when rates all have the same sign.
#'   These retain the `n`'th lowest or highest percentile of ***absolute*** rate
#'   values. For example, if rates are all negative `method =
#'   'highest_percentile'` will retain the highest magnitude `n`'th percentile
#'   regardless of the sign. `n` should be a percentile value between 0 and 1.
#'   For example, to extract the lowest 10th percentile of absolute rate values,
#'   you would enter `method = 'lowest_percentile', n = 0.1`.
#'
#'   ## `minimum`, `maximum`
#'
#'   In contrast to `lowest` and `highest`, these are *strictly numerical*
#'   options which take full account of the sign of the rate, and can be used
#'   where rates are a mix of positive and negative. For example, `method =
#'   'minimum'` will retain the minimum numerical value rates, which would
#'   actually be the highest oxygen uptake rates. `n` is an integer indicating
#'   how many of the min/max rates to retain. If `n = NULL` the results will
#'   instead be reordered by minimum or maximum rate without any removed.
#'
#'   ## `minimum_percentile`, `maximum_percentile`
#'
#'   Like `min` and `max` these are *strictly numerical* inputs which retain the
#'   `n`'th minimum or maximum percentile of the rates and take full account of
#'   the sign. Here `n` should be a percentile value between 0 and 1. For
#'   example, if rates are all negative (i.e. typical uptake rates), to extract
#'   the lowest 10th percentile of rates, you would enter `method =
#'   'maximum_percentile', n = 0.1`. This is because the *lowest* negative rates
#'   are numerically the *maximum* rates (`highest/lowest` percentile methods
#'   would be a better option in this case however).
#'
#'   ## `rate`
#'
#'   Allows you to enter a value range of output rates to be retained. Matching
#'   regressions in which the rate value falls within the `n` range (inclusive)
#'   are retained. `n` should be a vector of two values. For example, to retain
#'   only rates where the `rate` value is between 0.05 and 0.08: `method =
#'   'rate', n = c(0.05, 0.08)`. Note this operates on the `$rate.output`
#'   element, that is converted rate values.
#'
#'   ## `rep`, `rank`
#'
#'   These refer to the respective columns of the `$summary` table. For these,
#'   `n` should be a numeric vector of integers of `rep` or `rank` values to
#'   retain. To retain a range use regular R syntax, e.g. `n = 1:10`. If `n =
#'   NULL` no results will be removed, instead the results will be reordered
#'   ascending by `rep` (if it contains values) then `rank`. Essentially this
#'   restores the original ordering if other reordering operations have been
#'   performed.
#'
#'   The values in these columns depend on the functions used to calculate
#'   rates. If `calc_rate` was used, `rep` is `NA` and `rank` is the order of
#'   rates as entered using `from` and `to` (if multiple rates were determined).
#'   For `auto_rate`, `rep` is `NA` and `rank` relates to the `method` input.
#'   For example it indicates the kernel density ranking if the `linear` method
#'   was used, the ascending or descending ordering by absolute rate value if
#'   `lowest` or `highest` were used, or by numerical order if `minimum` or
#'   `maximum` were used. If `calc_rate.int` or `auto_rate.int` were used, `rep`
#'   indicates the replicate number and the `rank` column represents rank
#'   *within* the relevant replicate, and will generally be filled with the
#'   value `1`. Therefore you need to adapt your selection criteria
#'   appropriately towards which of these columns is relevant.
#'
#'   ## `rep_omit`, `rank_omit`
#'
#'   These refer to the `rep` and `rank` columns of the `$summary` table and
#'   allow you to exclude rates from particular replicate or rank values. For
#'   these, `n` should be a numeric vector of integers of `rep` or `rank` values
#'   to OMIT. To omit a range use regular R syntax, e.g. `n = 1:10`.
#'
#'   ## `rsq`, `row`, `time`, `density`
#'
#'   These methods refer to the respective columns of the `$summary` data frame.
#'   For these, `n` should be a vector of two values. Matching regressions in
#'   which the respective parameter falls within the `n` range (inclusive) are
#'   retained. To retain all rates with a R-Squared 0.90 or above: `method =
#'   'rsq', n = c(0.9, 1)`. The `row` and `time` ranges refer to the
#'   `$row`-`$endrow` or `$time`-`$endtime` columns and the original raw data
#'   (`$dataframe` element of the `convert_rate` object), and can be used to
#'   constrain results to rates from particular regions of the data (although
#'   usually a better option is to `subset_data()` prior to analysis). Note
#'   `time` is not the same as `duration` - see later section - and `row` refers
#'   to rows of the raw data, **not** rows of the summary table - see `manual`
#'   method for this. For all of these methods, if `n = NULL` no results will be
#'   removed, instead the results will be reordered by that respective column
#'   (descending for `rsq` and `density`, ascending for `row`, and `time`).
#'
#'   ## `intercept`, `slope`
#'
#'   These methods are similar to the above and refer to the `intercept_b0` and
#'   `slope_b1` summary table columns. Note these linear model coefficients
#'   represent different things in flowthrough vs. other analyses. In
#'   non-flowthrough analyses slopes represent rates and coefficients such as a
#'   high r-squared are important. In flowthrough, slopes represent the
#'   stability of the data region, in that the closer the slope is to zero, the
#'   less the delta oxygen values in that region vary, which is an indication of
#'   a region of stable rates. In addition, intercept values close to the
#'   calculated mean delta of the region also indicate a region of stable rates.
#'   Therefore these methods are chiefly useful in selection of flowthrough
#'   results, for example slopes close to zero. If `n = NULL` no results will be
#'   removed, instead the results will be reordered by ascending value by that
#'   column.
#'
#'   ## `time_omit`, `row_omit`
#'
#'   These methods refer to the original data, and are intended to *exclude*
#'   rates determined over particular data regions. This is useful in the case
#'   of, for example, a data anomaly such as a spike or sensor dropout. For
#'   these inputs, `n` are values (a single value, multiple values, or a range)
#'   indicating data timepoints or rows of the original data to exclude. Only
#'   rates (i.e. regressions) which *do not* utilise those particular values are
#'   retained in the output. For example, if an anomaly occurs precisely at
#'   timepoint 3000, `time_omit = 3000` means only rates determined solely over
#'   regions before or after this will be retained. If it occurs over a range
#'   this can be entered as, `time_omit = c(3000,3200)`. If you want to exclude
#'   a regular occurrence, for example the flushes in intermittent-flow
#'   respirometry, or any other non-continuous values they can be entered as a
#'   vector, e.g. `row_omit = c(1000, 2000, 3000)`. Note this last option can be
#'   extremely computationally intensive when the vector or dataset is large, so
#'   should only be used when a range cannot be entered as two values, which is
#'   much faster. For both methods, input values must match exactly to values
#'   present in the dataset.
#'
#'   ## `oxygen`
#'
#'   This can be used to constrain rate results to regions of the data based on
#'   oxygen values. `n` should be a vector of two values in the units of oxygen
#'   in the raw data. Only rate regressions in which all datapoints occur within
#'   this range (inclusive) are retained. Any which use even a single value
#'   outside of this range are excluded. Note the summary table columns `oxy`
#'   and `endoxy` refer to the first and last oxygen values in the rate
#'   regression, which should broadly indicate which results will be removed or
#'   retained, but this method examines *every* oxygen value in the regression,
#'   not just first and last.
#'
#'   ## `oxygen_omit`
#'
#'   Similar to `time_omit` and `row_omit` above, this can be used to *omit*
#'   rate regressions which use particular oxygen values. For this `n` are
#'   values (single or multiple) indicating oxygen values in the original raw
#'   data to exclude. Every oxygen value used by each regression is checked, and
#'   to be excluded an `n` value must match *exactly* to one in the data.
#'   Therefore, note that if a regression is fit across the data region where
#'   that value would occur, it is not necessarily excluded unless that *exact
#'   value* occurs. You need to consider the precision of the data values
#'   recorded. For example, if you wanted to exclude any rate using an oxygen
#'   value of `7`, but your data are recorded to two decimals, a rate fit across
#'   these data would *not* be excluded: `c(7.03, 7.02, 7.01, 6.99, 6.98, ...)`.
#'   To get around this you can use regular R syntax to input vectors at the
#'   correct precision, such as seq, e.g. `seq(from = 7.05, to = 6.96, by =
#'   -0.01)`. This can be used to input ranges of oxygen values to exclude.
#'
#'   ## `duration`
#'
#'   This method allows selection of rates which have a specific duration range.
#'   Here, `n` should be a numeric vector of two values. Use this to set minimum
#'   and maximum durations in the time units of the original data. For example,
#'   `n = c(0,500)` will retain only rates determined over a maximum of 500 time
#'   units. To retain rates over a minimum duration, set this using the minimum
#'   value plus the maximum duration or simply infinity. For example, for rates
#'   determined over a minimum of 500 time units `n = c(500,Inf)`)
#'
#'   ## `manual`
#'
#'   This method simply allows particular rows of the `$summary` data frame to
#'   be manually selected to be retained. For example, to keep only the top row
#'   `method = 'manual', n = 1`. To keep multiple rows use regular `R` selection
#'   syntax: `n = 1:3`, `n = c(1,2,3)`, `n = c(5,8,10)`, etc. No value of `n`
#'   should exceed the number of rows in the `$summary` data frame. Note this is
#'   not necessarily the same as selecting by the `rep` or `rank` methods, as
#'   the table could already have undergone selection or reordering.
#'
#'   ## `manual_omit`
#'
#'   As above, but this allows particular rows of the `$summary` data frame to
#'   be manually selected to be *omitted*.
#'
#'   ## `overlap`
#'
#'   This method removes rates which overlap, that is regressions which are
#'   partly or completely fit over the same rows of the original data. This is
#'   useful in particular with `auto_rate` results. The `auto_rate` `linear`
#'   method may identify multiple linear regions, some of which may
#'   substantially overlap, or even be completely contained within others. In
#'   such cases summary operations such as taking an average of the rate values
#'   may be questionable, as certain values will be weighted higher due to these
#'   multiple, overlapping results. This method removes overlapping rates, using
#'   `n` as a threshold to determine degree of permitted overlap. It is
#'   recommended this method be used after all other selection criteria have
#'   been applied, as it is quite aggressive about removing rates, and can be
#'   *very* computationally intensive when there are many results.
#'
#'   While it can be used with `auto_rate` results determined via the `rolling`,
#'   `lowest`, or `highest` methods, by their nature these methods produce *all
#'   possible* overlapping regressions, ordered in various ways, so other
#'   selection methods are more appropriate. The `overlap` method is generally
#'   intended to be used in combination with the `auto_rate` `linear` results,
#'   but may prove useful in other analyses.
#'
#'   Permitted overlap is determined by `n`, which indicates the proportion of
#'   each particular regression which must overlap with another for it to be
#'   regarded as overlapping. For example, `n = 0.2` means a regression would
#'   have to overlap with at least one other by at least 20% of its total length
#'   to be regarded as overlapping.
#'
#'   The `overlap` method performs two operations:
#'
#'   First, regardless of the `n` value, any rate regressions which are
#'   completely contained within another are removed (this is also the only
#'   operation if `n = 1`).
#'
#'   Secondly, for each regression in `$summary` starting from the bottom of the
#'   summary table (usually the lowest ranked result, but this depends on the
#'   analysis used and if any reordering has been already occurred), the
#'   function checks if it overlaps with any others (accounting for `n`). If
#'   not, the next lowest is checked, and the function progresses up the summary
#'   table until it finds one that does. The first to be found overlapping is
#'   then removed, and the process repeats starting again from the bottom of the
#'   summary table. If no reordering to the results has occurred, this means
#'   lower ranked results are removed first. This is repeated iteratively until
#'   only non-overlapping rates (accounting for `n`) remain.
#'
#'   If `n = 0`, only rates which do not overlap at all, that is share *no*
#'   data, are retained. If `n = 1`, only rates which are 100% contained within
#'   at least one other are removed.
#'
#'   ## Reordering results
#'
#'   Several methods can be used to reorder results rather than select them, by
#'   not entering an `n` input (that is, letting the `n = NULL` default be
#'   applied). Several of these methods are named the same as those in
#'   `auto_rate` for consistency and have equivalent outcomes, so this allows
#'   results to be reordered to the equivalent of that method's results without
#'   re-running the `auto_rate` analysis.
#'
#'   The `"row"` and `"rolling"` methods reorder sequentially by the starting
#'   row of each regression (`$row` column).
#'
#'   The `"time"` method reorders sequentially by the starting time of each
#'   regression (`$time` column).
#'
#'   `"linear"` and `"density"` are essentially identical, reordering by the
#'   `$density` column. This metric is only produced by the `auto_rate` `linear`
#'   method, so will not work with any other results.
#'
#'   `"rep"` or `"rank"` both reorder by the `$rep` then `$rank` columns. What
#'   these represents is context dependent - see **Replicate and Rank columns**
#'   section above. Each summary row `rep` and `rank` value is retained
#'   unchanged regardless of how the results are subsequently selected or
#'   reordered, so this will restore the original ordering after other methods
#'   have been applied.
#'
#'   `rsq` reorders by `$rsq` from highest value to lowest.
#'
#'   `intercept` and `slope` reorders by these columns from lowest value to
#'   highest.
#'
#'   `highest` and `lowest` reorder by absolute values of the `$rate.output`
#'   column, that is highest or lowest in magnitude regardless of the sign. They
#'   can only be used when rates all have the same sign.
#'
#'   `maximum` and `minimum` reorder by numerical values of the `$rate.output`
#'   column, that is maximum or minimum in numerical value taking account of the
#'   sign, and can be used when rates are a mix of negative and positive.
#'
#'   ## Numeric input conversions
#'
#'   For `convert_rate` objects which contain rates which have been converted
#'   from numeric values, the summary table will contain a limited amount of
#'   information, so many of the selection or reordering methods will not work.
#'   In this case a warning is given and the original input is returned.
#'
#'   ## Plot
#'
#'   There is no plotting functionality in `select_rate`. However since the
#'   output is a `convert_rate` object it can be plotted. See the **Plot**
#'   section in `help("convert_rate")`. To plot straight after a selection
#'   operation, pipe or enter the output in `plot()`. See Examples.
#'
#'   ## More
#'
#'   This help file can be found online
#'   [here](https://januarharianto.github.io/respR/reference/select_rate.html),
#'   where it is much easier to read.
#'
#'   For additional help, documentation, vignettes, and more visit the `respR`
#'   website at <https://januarharianto.github.io/respR/>
#'
#' @return The output of `select_rate` is a `list` object which retains the
#'   `convert_rate` class, with an additional `convert_rate_select` class
#'   applied.
#'
#'   It contains two additional elements: `$original` contains the original,
#'   unaltered `convert_rate` object, which will be retained unaltered through
#'   multiple selection operations, that is even after processing through the
#'   function multiple times. `$select_calls` contains the calls for every
#'   selection operation that has been applied to the `$original` object, from
#'   the first to the most recent. These additional elements ensure the output
#'   contains the complete, reproducible history of the `convert_rate` object
#'   having been processed.
#'
#' @param x list. An object of class `convert_rate` or `convert_rate_select`.
#' @param method string. Method by which to select or reorder rate results. For
#'   most methods matching results are *retained* in the output. See Details.
#' @param n numeric. Number, percentile, or range of results to retain or omit
#'   depending on `method`. Default is `NULL`, in which case some methods will
#'   instead reorder the results. See Details.
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom data.table between
#' @importFrom stats quantile
#' @importFrom dplyr arrange desc
#'
#' @examples
#' \donttest{
#' ## Object to filter
#'  ar_obj <- inspect(intermittent.rd, plot = FALSE) |>
#'    auto_rate(plot = FALSE) |>
#'    convert_rate(oxy.unit = "mg/L",
#'                 time.unit = "s",
#'                 output.unit = "mg/h",
#'                 volume = 2.379) |>
#'    summary()
#'
#'  ## Select only negative rates
#'  ar_subs_neg <- select_rate(ar_obj, method = "negative") |>
#'    summary()
#'
#'  ## Select only rates over 1000 seconds duration
#'  ar_subs_dur <- select_rate(ar_obj, method = "duration", n = c(1000, Inf)) |>
#'    summary()
#'
#'  ## Reorder rates sequentially (i.e. by starting row)
#'  ar_subs_dur <- select_rate(ar_obj, method = "row") |>
#'    summary()
#'
#'  ## Select rates with r-squared higher than 0.99,
#'  ## then select the lowest 10th percentile of the remaining rates,
#'  ## then take the mean of those
#'  inspect(squid.rd, plot = FALSE) |>
#'    auto_rate(method = "linear",
#'              plot = FALSE) |>
#'    convert_rate(oxy.unit = "mg/L",
#'                 time.unit = "s",
#'                 output.unit = "mg/h",
#'                 volume = 2.379) |>
#'    summary() |>
#'    select_rate(method = "rsq", n = c(0.99, 1)) |>
#'    select_rate(method = "lowest_percentile", n = 0.1) |>
#'    mean()
#'    }

select_rate <- function(x, method = NULL, n = NULL){

  ## Save function call for output
  call <- match.call()

  # Checks ------------------------------------------------------------------

  ## Check for valid convert_rate or convert_rate.ft object
  if(!(class.val(x, cnvr = TRUE, cnvr.ft = TRUE)))
    stop("select_rate: Input is not a 'convert_rate' or 'convert_rate.ft' object")

  ## Specify a method
  if(is.null(method)) stop("select_rate: Please specify a 'method'")

  ## Validate method
  if(!is.null(method) && !(method %in% c("overlap",
                                         "duration",
                                         "density",
                                         "manual",
                                         "manual_omit",
                                         "time",
                                         "time_omit",
                                         "row",
                                         "row_omit",
                                         "oxygen",
                                         "oxygen_omit",
                                         "rsq",
                                         "intercept",
                                         "slope",
                                         "rep",
                                         "rep_omit",
                                         "rank",
                                         "rank_omit",
                                         "rate",
                                         "rate.output",
                                         "maximum_percentile",
                                         "minimum_percentile",
                                         "maximum",
                                         "minimum",
                                         "lowest_percentile",
                                         "highest_percentile",
                                         "lowest",
                                         "highest",
                                         "zero",
                                         "nonzero",
                                         "negative",
                                         "positive",
                                         "rolling",
                                         "linear"))) stop("select_rate: 'method' input not recognised")

  ## If numeric conversions, then disallow methods where summary table values are NA
  ## Can identify as numeric input because dataframe will be NULL
  if(is.null(x$dataframe) && method %in% c("overlap",
                                           "duration",
                                           "density",
                                           "time",
                                           "time_omit",
                                           "row",
                                           "row_omit",
                                           "oxygen",
                                           "oxygen_omit",
                                           "rsq",
                                           "intercept",
                                           "slope",
                                           "rolling",
                                           "linear")) stop(glue::glue("select_rate: The '{method}' method is not accepted for 'convert_rate' objects which have been created using numeric inputs."))

  ## Disallow `density` or `linear` methods for anything other than `auto_rate` linear objects
  # If x$dataframe is NOT null (i.e. it is an object input rather than numerics)
  if(!(is.null(x$dataframe))) {
    # If summary$density is not all NA then it has to be "linear" ar object
    # edge case for empty objects too
    is.linear <- !(all(is.na(x$summary$density))) || nrow(x$summary) == 0
    # If it is NOT linear and method = "density" or "linear", stop
    if(method %in% c("density", "linear") && !is.linear)
      stop(glue::glue("select_rate: The '{method}' method is only accepted for rates determined in 'auto_rate' via the 'linear' method."))
  }

  ## Disallow 'oxygen' methods from calc_rate.bg objects
  ## Can't imagine anyone ever actually doing this, but may as well be thorough
  if(!(is.null(x$dataframe)) && inherits(x$inputs$x, "calc_rate.bg")) {
    if(method %in% c("oxygen", "oxygen_omit"))
      stop(glue::glue("select_rate: The '{method}' method is not accepted for 'calc_rate.bg' objects because rates may come from different columns of the dataframe."))
  }

  ## Message for pos and neg rates found
  if(any(x$rate.output > 0) && any(x$rate.output < 0)) message("select_rate: Object contains both negative and positive rates. Ensure the chosen `method` is appropriate.")


  # Extract data ------------------------------------------------------------

  ## Save unaltered input for output
  input_x <- x
  ## Extract original raw data
  raw_df <- input_x$dataframe
  # message here for numerics? - as dataframe will be NULL

  # Reordering --------------------------------------------------------------

  # Sequential by row
  # equiv to "rolling" in auto_rate
  # identical to method = "row" with n = NULL
  if(method == "rolling"){
    message("select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    summ <- x$summary
    summ <- arrange(summ, row)
    keep <- 0:nrow(summ) # needs to be zero to handle empty summary tables
    reordered <- TRUE
  }
  if(is.null(n) && method == "row"){
    message("select_rate: Reordering results by 'row' method.")
    summ <- x$summary
    summ <- arrange(summ, row)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }
  if(is.null(n) && method == "time"){
    message("select_rate: Reordering results by 'time' method.")
    summ <- x$summary
    summ <- arrange(summ, time)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }
  if(is.null(n) && method == "intercept"){
    message("select_rate: Reordering results by 'intercept' method.")
    summ <- x$summary
    summ <- arrange(summ, intercept_b0)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }
  if(is.null(n) && method == "slope"){
    message("select_rate: Reordering results by 'slope' method.")
    summ <- x$summary
    summ <- arrange(summ, slope_b1)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }
  # by linear method - this is identical to density with n = NULL below
  if(method == "linear"){
    message("select_rate: Reordering results by 'linear' method. 'n' input ignored...")
    summ <- x$summary
    summ <- arrange(summ, desc(density))
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }
  if(is.null(n) && method == "density"){
    message("select_rate: Reordering results by 'density' method.")
    summ <- x$summary
    summ <- arrange(summ, desc(density))
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }

  # Reorder by rep and/or rank column - essentially restores to original order
  # should be identical to above if "linear" method
  if(is.null(n) && method %in% c("rep", "rank")){
    message(glue::glue("select_rate: Reordering results by '{method}' method."))
    summ <- x$summary
    summ <- arrange(summ, rep, rank)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }

  # Reorder by rsq column
  if(is.null(n) && method == "rsq"){
    message("select_rate: Reordering results by 'rsq' method.")
    summ <- x$summary
    summ <- arrange(summ, desc(rsq))
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }

  # By lowest absolute value
  if(is.null(n) && method == "lowest"){
    if(any(x$rate.output > 0) && any(x$rate.output < 0)) stop(glue::glue("select_rate: Object contains both negative and positive rates. \n'{method}' method is intended to find {method} rate in absolute value amongst rates all having the same sign. \nSee 'positive' and 'negative' or 'maximum' and 'minimum' options."))
    message("select_rate: Reordering results by 'lowest' method.")
    summ <- x$summary
    # reorder ascending or descending based on sign
    if(any(summ$rate.output < 0)) summ <- arrange(summ, desc(rate.output)) else
      summ <- arrange(summ, rate.output)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }

  # By highest absolute value
  if(is.null(n) && method == "highest"){
    if(any(x$rate.output > 0) && any(x$rate.output < 0)) stop(glue::glue("select_rate: Object contains both negative and positive rates. \n'{method}' method is intended to find {method} rate in absolute value amongst rates all having the same sign. \nSee 'positive' and 'negative' or 'maximum' and 'minimum' options."))
    message("select_rate: Reordering results by 'highest' method.")
    summ <- x$summary
    # reorder ascending or descending based on sign
    if(any(summ$rate.output < 0)) summ <- arrange(summ, rate.output) else
      summ <- arrange(summ, desc(rate.output))
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }

  # By minimum numerical value
  if(is.null(n) && method == "minimum"){
    message("select_rate: Reordering results by 'minimum' method.")
    summ <- x$summary
    summ <- arrange(summ, rate.output)
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }

  # By maximum numerical value
  if(is.null(n) && method == "maximum"){
    message("select_rate: Reordering results by 'maximum' method.")
    summ <- x$summary
    summ <- arrange(summ, desc(rate.output))
    keep <- 0:nrow(summ)
    reordered <- TRUE
  }


  # Selection --------------------------------------------------------------

  # Positive rates only -----------------------------------------------------
  if(method == "positive"){
    message("select_rate: Selecting all positive rate values. 'n' input ignored...")
    keep <- which(x$rate.output > 0)
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # Negative rates only -----------------------------------------------------
  if(method == "negative"){
    message("select_rate: Selecting all negative rate values. 'n' input ignored...")
    keep <- which(x$rate.output < 0)
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # Nonzero rates only -----------------------------------------------------
  if(method == "nonzero"){
    message("select_rate: Selecting all non-zero rate values. 'n' input ignored...")
    keep <- which(x$rate.output != 0)
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # Zero rates only ---------------------------------------------------------
  if(method == "zero"){
    message("select_rate: Selecting all zero rate values. 'n' input ignored...")
    keep <- which(x$rate.output == 0)
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # lowest rates ------------------------------------------------------------
  ## Note these are NOT lowest *numerical*, but lowest *absolute* values
  ## E.g. if all negative will return the highest/least negative n values

  if(!is.null(n) && method == "lowest"){
    if(any(x$rate.output > 0) && any(x$rate.output < 0)) stop(glue::glue("select_rate: Object contains both negative and positive rates. \n'{method}' method is intended to find {method} rate in absolute value amongst rates all having the same sign. \nSee 'positive' and 'negative' or 'maximum' and 'minimum' options."))
    if(n %% 1 != 0 || n < 0) stop("select_rate: For 'lowest' method 'n' must contain only positive integers.")
    if(n > length(x$rate.output)) message("select_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("select_rate: Selecting lowest {n} *absolute* rate values..."))

    ## if all negative return HIGHEST numerical n
    if(all(x$rate.output <= 0)) keep <- sort(tail(order(x$rate.output), n))
    ## if all positive return LOWEST numerical n
    if(all(x$rate.output >= 0)) keep <- sort(head(order(x$rate.output), n))

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # highest rates -----------------------------------------------------------
  ## Note these are NOT highest *numerical*, but highest *absolute* values

  if(!is.null(n) && method == "highest"){
    if(any(x$rate.output > 0) && any(x$rate.output < 0)) stop(glue::glue("select_rate: Object contains both negative and positive rates. \n'{method}' method is intended to find {method} rate in absolute value amongst rates all having the same sign. \nSee 'positive' and 'negative' or 'maximum' and 'minimum' options."))
    if(n %% 1 != 0 || n < 0) stop("select_rate: For 'highest' method 'n' must contain only positive integers.")
    if(n > length(x$rate.output)) message("select_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("select_rate: Selecting highest {n} *absolute* rate values..."))

    ## if all negative return LOWEST numerical n
    if(all(x$rate.output <= 0)) keep <- sort(head(order(x$rate.output), n))
    ## if all positive return HIGHEST numerical n
    if(all(x$rate.output >= 0)) keep <- sort(tail(order(x$rate.output), n))

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # lowest percentile rates -------------------------------------------------
  if(method == "lowest_percentile") {
    if(any(x$rate.output > 0) && any(x$rate.output < 0)) stop(glue::glue("select_rate: Object contains both negative and positive rates. \n'{method}' method is intended to find {method} rate in absolute value amongst rates all having the same sign. \nSee 'positive' and 'negative' or 'maximum_percentile' and 'minimum_percentile' options."))
    if(n <= 0 || n >= 1) stop("select_rate: For 'percentile' methods 'n' must be between 0 and 1.")

    message(glue::glue("select_rate: Selecting lowest {n*100}th percentile of *absolute* rate values..."))

    ## if all negative return HIGHEST numerical nth percentile
    if(all(x$rate.output <= 0)) {
      cutoff <- stats::quantile(x$rate.output, 1-n)
      keep <- sort(which(x$rate.output %in% x$rate.output[x$rate.output >= cutoff]))
    }
    ## if all positive return LOWEST numerical nth percentile
    if(all(x$rate.output >= 0)) {
      cutoff <- stats::quantile(x$rate.output, n)
      keep <- sort(which(x$rate.output %in% x$rate.output[x$rate.output <= cutoff]))
    }

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # highest percentile rates ------------------------------------------------
  if(method == "highest_percentile") {
    if(any(x$rate.output > 0) && any(x$rate.output < 0)) stop(glue::glue("select_rate: Object contains both negative and positive rates. \n'{method}' method is intended to find {method} rate in absolute value amongst rates all having the same sign. \nSee 'positive' and 'negative' or 'maximum_percentile' and 'minimum_percentile' options."))
    if(n <= 0 || n >= 1) stop("select_rate: For 'percentile' methods 'n' must be between 0 and 1.")

    message(glue::glue("select_rate: Selecting highest {n*100}th percentile of *absolute* rate values..."))

    ## if all negative return LOWEST numerical nth percentile
    if(all(x$rate.output <= 0)) {
      cutoff <- stats::quantile(x$rate.output, n)
      keep <- sort(which(x$rate.output %in% x$rate.output[x$rate.output <= cutoff]))
    }
    ## if all positive return LOWEST numerical nth percentile
    if(all(x$rate.output >= 0)) {
      cutoff <- stats::quantile(x$rate.output, 1-n)
      keep <- sort(which(x$rate.output %in% x$rate.output[x$rate.output >= cutoff]))
    }

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # min n rates -------------------------------------------------------------
  ## These are lowest *numerical* values.
  ## i.e can mix -ve and +ve
  ## min = lowest/most negative
  if(!is.null(n) && method == "minimum") {
    if(n %% 1 != 0 || n < 0) stop("select_rate: For 'minimum' method 'n' must contain only positive integers.")
    if(n > length(x$rate.output)) message("select_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("select_rate: Selecting minimum {n} *numerical* rate values..."))
    keep <- sort(head(order(x$rate.output), n))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # max n rates -------------------------------------------------------------
  if(!is.null(n) && method == "maximum") {
    if(n %% 1 != 0 || n < 0) stop("select_rate: For 'maximum' method 'n' must contain only positive integers.")
    if(n > length(x$rate.output)) message("select_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("select_rate: Selecting maximum {n} *numerical* rate values..."))
    keep <- sort(tail(order(x$rate.output), n))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # min percentile rates ----------------------------------------------------
  if(method == "minimum_percentile") {
    if(n <= 0 || n >= 1) stop("select_rate: For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("select_rate: Selecting minimum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate.output, n)
    keep <- sort(which(x$rate.output %in% x$rate.output[x$rate.output <= cutoff]))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # max percentile rates ----------------------------------------------------
  if(method == "maximum_percentile") {
    if(n <= 0 || n >= 1) stop("select_rate: For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("select_rate: Selecting maximum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate.output, 1-n) ## NOTE DIFFERENCE TO ABOVE
    keep <- sort(which(x$rate.output %in% x$rate.output[x$rate.output >= cutoff]))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # rate --------------------------------------------------------------------
  if(method == "rate" || method == "rate.output"){
    if(length(n) != 2) stop("select_rate: For 'rate' method 'n' must be a vector of two values.")
    message(glue::glue("select_rate: Selecting rates with values between {n[1]} and {n[2]}..."))
    n_order <- sort(n)
    keep <- sort(which(data.table::between(x$rate.output, n_order[1], n_order[2])))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # rsq ---------------------------------------------------------------------
  if(!is.null(n) && method == "rsq"){
    if(length(n) != 2) stop("select_rate: For 'rsq' method 'n' must be a vector of two values.")
    message(glue::glue("select_rate: Selecting rates with rsq values between {n[1]} and {n[2]}..."))
    n_order <- sort(n)
    keep <- sort(which(data.table::between(x$summary$rsq, n_order[1], n_order[2])))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # intercept ---------------------------------------------------------------------
  if(!is.null(n) && method == "intercept"){
    if(length(n) != 2) stop("select_rate: For 'intercept' method 'n' must be a vector of two values.")
    message(glue::glue("select_rate: Selecting rates with intercept values between {n[1]} and {n[2]}..."))
    n_order <- sort(n)
    keep <- sort(which(data.table::between(x$summary$intercept_b0, n_order[1], n_order[2])))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # slope ---------------------------------------------------------------------
  if(!is.null(n) && method == "slope"){
    if(length(n) != 2) stop("select_rate: For 'slope' method 'n' must be a vector of two values.")
    message(glue::glue("select_rate: Selecting rates with slope values between {n[1]} and {n[2]}..."))
    n_order <- sort(n)
    keep <- sort(which(data.table::between(x$summary$slope_b1, n_order[1], n_order[2])))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # rep --------------------------------------------------------------------
  if(!is.null(n) && method == "rep"){

    if(all(is.na(x$summary$rep)))
      stop("select_rate: All 'rep' are NA so nothing to select!")
    if(!(is.numeric(n)) || any(n %% 1 != 0))
      stop("select_rate: For 'rep' method 'n' must contain only positive integers.")
    if(any(!(dplyr::between(n, min(x$summary$rep), max(x$summary$rep)))))
      stop("select_rate: Input for 'n': One or more 'rep' inputs out of range of 'summary$rep' values.")
    message(glue::glue("select_rate: Selecting rates from entered 'rep' replicates..."))

    n_order <- sort(n)
    keep <- sort(unique(unlist(lapply(n_order, function(z) which(z == x$summary$rep)))))
    summ <- x$summary
    reordered <- FALSE
  }

  # rep_omit --------------------------------------------------------------------
  if(!is.null(n) && method == "rep_omit"){

    if(all(is.na(x$summary$rep)))
      stop("select_rate: All 'rep' are NA so nothing to select!")
    if(!(is.numeric(n)) || any(n %% 1 != 0))
      stop("select_rate: For 'rep_omit' method 'n' must contain only positive integers.")
    if(any(!(dplyr::between(n, min(x$summary$rep), max(x$summary$rep)))))
      stop("select_rate: Input for 'n': One or more 'rep_omit' inputs out of range of 'summary$rep' values.")
    message(glue::glue("select_rate: Selecting rates which *ARE NOT* from 'rep_omit' replicates..."))

    n_order <- sort(n)
    remove <- sort(unique(unlist(lapply(n_order, function(z) which(z == x$summary$rep)))))
    keep <- (1:nrow(x$summary))[-remove]
    summ <- x$summary
    reordered <- FALSE
  }

  # rank --------------------------------------------------------------------
  if(!is.null(n) && method == "rank"){

    if(!(all(is.na(x$summary$rep))))
      message("select_rate: Note there are multiple replicates present in these results, which may have multiple ranks *within* them. \nEnsure the 'rank' method is the correct one for what you want to do.")
    if(!(is.numeric(n)) || any(n %% 1 != 0))
      stop("select_rate: For 'rank' method 'n' must contain only positive integers.")
    if(any(!(dplyr::between(n, min(x$summary$rank), max(x$summary$rank)))))
      stop("select_rate: Input for 'n': One or more 'rank' inputs out of range of 'summary$rank' values.")
    message(glue::glue("select_rate: Selecting rates with entered 'rank' values..."))

    n_order <- sort(n)
    keep <- sort(unique(unlist(lapply(n_order, function(z) which(z == x$summary$rank)))))
    summ <- x$summary
    reordered <- FALSE
  }

  # rank_omit --------------------------------------------------------------------
  if(!is.null(n) && method == "rank_omit"){

    if(!(all(is.na(x$summary$rep))))
      message("select_rate: Note there are multiple replicates present in these results, which may have multiple ranks *within* them. \nEnsure the 'rank_omit' method is the correct one for what you want to do.")
    if(!(is.numeric(n)) || any(n %% 1 != 0))
      stop("select_rate: For 'rank_omit' method 'n' must contain only positive integers.")
    if(any(!(dplyr::between(n, min(x$summary$rank), max(x$summary$rank)))))
      stop("select_rate: Input for 'n': One or more 'rank_omit' inputs out of range of 'summary$rank' values.")
    message(glue::glue("select_rate: Selecting rates with ranks which *ARE NOT* in 'rank_omit' input..."))

    n_order <- sort(n)
    remove <- sort(unique(unlist(lapply(n_order, function(z) which(z == x$summary$rank)))))
    keep <- (1:nrow(x$summary))[-remove]
    summ <- x$summary
    reordered <- FALSE
  }

  # row ---------------------------------------------------------------------
  if(!is.null(n) && method == "row"){
    if(length(n) != 2) stop("select_rate: For 'row' method 'n' must be a vector of two values.")
    if(any(n > dim(raw_df)[1])) stop("select_rate: Input for 'n': row inputs out of data frame range.")

    message(glue::glue("select_rate: Selecting rates which occur only between original data rows {n[1]} and {n[2]}..."))

    n_order <- sort(n)
    keep1 <- which(x$summary$row >= n_order[1])
    keep2 <- which(x$summary$endrow <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # row_omit ----------------------------------------------------------------
  if(method == "row_omit"){
    if(!(is.numeric(n)) || any(n %% 1 != 0))
      stop("select_rate: For 'row_omit' method 'n' must contain only positive integers.")
    message(glue::glue("select_rate: Selecting rates which *DO NOT* use original data row(s) in 'n' input..."))
    if(any(n > dim(raw_df)[1])) stop("select_rate: Input for 'n': row inputs out of data frame range.")

    n_order <- sort(n)

    # if 1 or 2 values in n, simple selection
    if(length(n_order) == 1) n_order <- c(n_order, n_order) ## if single value make 2-vec for code simplicity
    if(length(n_order) == 2) {
      keep1 <- which(x$summary$row > n_order[2]) ## start is AFTER n
      keep2 <- which(x$summary$endrow < n_order[1]) ## end is BEFORE n
      keep <- c(keep1, keep2)
      # otherwise do this possibly very slow loop
    } else {
      message("select_rate: For 'row_omit' method, selecting multiple 'n' inputs can be VERY SLOW. \nIf possible, specifying a range using lower and upper values is much faster.")
      remove <- c() # empty obj for loop results
      # for each n, go through summary table by row
      # if it occurs between row and endrow then that row of summary gets removed
      for(i in n_order){
        remove_i <- which(apply(x$summary, 1, function(z) any(i %in% z[7]:z[8])))
        remove <- sort(unique(c(remove, remove_i)))
        progress.bar(which(i == n_order), length(n_order), "select_rate: 'row_omit' progress")
      }
      cat("\n")
      # if nothing to remove, above outputs integer(0), so this is for that...
      if(length(remove) > 0) keep <- (1:nrow(x$summary))[-remove] else
        keep <- 1:nrow(x$summary)
    }

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # time --------------------------------------------------------------------
  if(!is.null(n) && method == "time"){
    if(length(n) != 2) stop("select_rate: For 'time' method 'n' must be a vector of two values.")
    if(any(n < range(raw_df[[1]], na.rm = TRUE)[1]) || any(n > range(raw_df[[1]], na.rm = TRUE)[2]))
      stop("select_rate: Input for 'n': time inputs out of time data range.")

    message(glue::glue("select_rate: Selecting rates which occur only between times {n[1]} and {n[2]}..."))

    n_order <- sort(n)
    keep1 <- which(x$summary$time >= n_order[1])
    keep2 <- which(x$summary$endtime <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # time_omit ---------------------------------------------------------------
  if(method == "time_omit"){
    if(!(is.numeric(n)))
      stop("select_rate: For 'time_omit' method 'n' must contain only numeric values of time.")
    message(glue::glue("select_rate: Selecting rates which *DO NOT* use time value(s) in 'n' input..."))
    if(any(n < range(raw_df[[1]], na.rm = TRUE)[1]) || any(n > range(raw_df[[1]], na.rm = TRUE)[2]))
      stop("select_rate: Input for 'n': time inputs out of time data range.")

    n_order <- sort(n)

    if(length(n_order) == 1) n <- c(n_order, n_order) ## if single value make 2-vec for code simplicity
    # if(any(n_order < range(x$dataframe[[1]], na.rm = TRUE)[1]) || any(n_order > range(x$dataframe[[1]], na.rm = TRUE)[2]))
    #   stop("select_rate: Input for 'n': time inputs out of time data range.")

    # if a range, single selection
    if(length(n_order) == 2){
      # which regs span n?
      keep1 <- which(x$summary$time > n_order[2]) ## start is AFTER n
      keep2 <- which(x$summary$endtime < n_order[1]) ## end is BEFORE n
      keep <- c(keep1, keep2)
      # otherwise do this possibly very slow loop
    } else {
      message("select_rate: For 'time_omit' method, selecting multiple 'n' inputs can be VERY SLOW. \nIf possible, specifying a range using lower and upper values is much faster.")

      remove <- c() # empty obj for loop results

      for(i in n_order){
        remove_i <- which(apply(x$summary, 1, function(z)
          data.table::between(i, z[9], z[10], incbounds = TRUE)))
        remove <- sort(unique(c(remove, remove_i)))
        progress.bar(which(i == n_order), length(n_order), "select_rate: 'time_omit' progress")
      }
      cat("\n")
      # if nothing to remove, above outputs integer(0), so this is for that...
      if(length(remove) > 0) keep <- (1:nrow(x$summary))[-remove] else
        keep <- 1:nrow(x$summary)
    }

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # oxygen ------------------------------------------------------------------
  if(method == "oxygen"){
    if(length(n) != 2) stop("select_rate: For 'oxygen' method 'n' must be a vector of two values.")

    message(glue::glue("select_rate: Selecting rates which occur only between oxygen values {n[1]} and {n[2]}..."))

    n_order <- sort(n)

    # which rates have at least one oxygen value below first value
    remove1 <- which(mapply(function(p,q) {any(raw_df[p:q,2] <= n_order[1])},
                            p = x$summary$row,
                            q = x$summary$endrow)
    )
    # which rates have at least one oxygen value above second value
    remove2 <- which(mapply(function(p,q) {any(raw_df[p:q,2] >= n_order[2])},
                            p = x$summary$row,
                            q = x$summary$endrow)
    )
    # combine
    remove <- sort(unique(c(remove1, remove2)))

    # Convert to keep
    # if nothing to remove, above outputs integer(0), so this is for that...
    if(length(remove) > 0) keep <- (1:nrow(x$summary))[-remove] else
      keep <- 1:nrow(x$summary)

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # oxygen_omit -------------------------------------------------------------
  if(method == "oxygen_omit"){

    if(!(is.numeric(n)))
      stop("select_rate: For 'oxygen_omit' method 'n' must contain only numeric values of oxygen.")
    message(glue::glue("select_rate: Selecting rates which *DO NOT* use oxygen value(s) in 'n' input..."))

    n_order <- sort(n)

    # which rates have at least one oxygen_omit value?
    remove <- c()
    for(z in n_order) {
      remove1 <- which(mapply(function(p,q) {any(raw_df[p:q,2] == z)},
                              p = x$summary$row,
                              q = x$summary$endrow))
      remove <- sort(unique(c(remove1, remove)))
    }

    # Convert to keep
    # if nothing to remove, above outputs integer(0), so this is for that...
    if(length(remove) > 0) keep <- (1:nrow(x$summary))[-remove] else
      keep <- 1:nrow(x$summary)

    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # manual ------------------------------------------------------------------
  if(method == "manual"){
    # check within range of summary length
    if(!all(n %in% 1:nrow(x$summary))) stop("select_rate: For 'manual' method: 'n' values are out of range of $summary data.frame rows...")
    message(glue::glue("select_rate: Selecting specified rows of the '$summary' table..."))
    n_order <- sort(n)
    keep <- n_order
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # manual_omit ------------------------------------------------------------------
  if(method == "manual_omit"){
    # check within range of summary length
    if(!all(n %in% 1:nrow(x$summary))) stop("select_rate: For 'manual' method: 'n' values are out of range of $summary data.frame rows...")
    message(glue::glue("select_rate: Selecting rows of the '$summary' table which are NOT in the 'manual_omit' input..."))
    n_order <- sort(n)
    keep <- (1:nrow(x$summary))[-n_order]
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }

  # density ------------------------------------------------------------------
  if(!is.null(n) && method == "density"){
    if(length(n) != 2) stop("select_rate: For 'density' method 'n' must be a vector of two values.")
    n_order <- sort(n)
    message(glue::glue("select_rate: Selecting rates with density values between {n[1]} and {n[2]}..."))
    keep <- sort(which(data.table::between(x$summary$density, n_order[1], n_order[2])))
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # duration ----------------------------------------------------------------
  if(method == "duration"){
    if(length(n) != 2) stop("select_rate: For 'duration' method 'n' must be a vector of two values.")
    n_order <- sort(n)
    message(glue::glue("select_rate: Selecting rates with duration between {n_order[1]} and {n_order[2]}..."))

    durations <- x$summary$endtime-x$summary$time

    keep1 <- which(durations >= n_order[1])
    keep2 <- which(durations <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
    summ <- x$summary
    reordered <- FALSE
  }


  # overlap -------------------------------------------------------------
  if(method == "overlap"){

    if(is.null(n)) {
      n <- 0
      message("select_rate: 'overlap' method - applying default 'n = 0', no overlapping permitted.")
    }
    if(n < 0 || n > 1)
      stop("select_rate: For 'overlap' method 'n' must be between 0 and 1 inclusive.")
    message(glue::glue("select_rate: The 'overlap' method can be computationally intensive and may take some time."))
    message(glue::glue("select_rate: Selecting rates which overlap by at least {n*100}% ..."))

    ## extract summary df
    df <- x$summary

    #### Remove overlapping

    ## add row width col
    df$row_width <- df$endrow - df$row

    overlaps <- TRUE # to get while loop started

    ## while at least one regression is overlapping,
    ## go from bottom row to top
    ## the one closest the bottom which is overlapping gets removed
    ## then repeat while loop
    while(any(overlaps)){
      ## object to save logical test results to
      overlaps <- rep(NA, nrow(df))
      ## loop from last row to first
      for(i in nrow(df):1) {
        start <- df$row[i] # start of reg in data
        end <- df$endrow[i] # end of reg in data
        width <- df$row_width[i] # width of reg
        overlap <- round(width * n) # allowed overlap
        overlaps[i] <- any(df$row[-i]+overlap <= end & df$endrow[-i]-overlap >= start)
      }

      ## if any are inside (contained), go to last one and remove it
      if(any(overlaps)) {
        remove <- tail(which(overlaps), 1)
        df <- df[-remove,]
      }

      keep <- which(x$summary$rank %in% df$rank)
      keep <- sort(keep)
      summ <- x$summary
      reordered <- FALSE
    }
  } ### end overlap

  # overlap_new -------------------------------------------------------------
  # Rewrote this. Can't remember why or what main difference is.
  # But doesn't seem to work quite as well.
  # Leaving here for now
  # if(method == "overlap_new"){
  #
  #   if(is.null(n)) {
  #     n <- 0
  #     message("select_rate: 'overlap' method applying default 'n = 0', no overlapping permitted.")
  #   }
  #   if(n < 0 || n > 1)
  #     stop("select_rate: For 'overlap' method 'n' must be between 0 and 1 inclusive.")
  #   message(glue::glue("select_rate: The 'overlap' method can be computationally intensive and may take some time."))
  #   message(glue::glue("select_rate: Selecting rates which overlap by at least {n*100}% ..."))
  #
  #   ##### First - remove contained
  #
  #   ## extract summary df
  #   df <- x$summary
  #   df$row_width <- df$endrow - df$row
  #
  #   insides <- TRUE # to get while loop started
  #
  #   ## while at least one regression is inside another,
  #   ## go from bottom row to top
  #   ## the one closest the bottom which is contained gets removed
  #   ## then repeat while loop
  #   while(any(insides)){
  #     ## object to save logical test results to
  #     insides <- rep(NA, nrow(df))
  #     ## loop from last row to first
  #     for(i in nrow(df):1) {
  #       start <- df$row[i] # start of reg in data
  #       end <- df$endrow[i] # end of reg in data
  #       insides[i] <- any(start >= df$row[-i] & end <= df$endrow[-i])
  #     }
  #
  #     ## if any are inside (contained), go to last one and remove it
  #     if(any(insides)) {
  #       remove <- tail(which(insides), 1)
  #       df <- df[-remove,]
  #     }
  #   }
  #
  #   ## second
  #
  #   max_overlap <- data.frame(rank = NA,
  #                             max_overlap = 1)
  #
  #   while(any(max_overlap[[2]] > n)) {
  #
  #     max_overlap <- data.frame(rank = NA,
  #                             max_overlap = NA)
  #     for(i in 1:nrow(df)){
  #       rank <- df[i,1]
  #       start <- df$row[i] # start of reg in data
  #       end <- df$endrow[i] # end of reg in data
  #       width <- df$row_width[i]
  #
  #       overlap_props <- data.frame(t(apply(df[-i,], 1, function(z) {
  #         c(z[1],
  #           prop = length(intersect(start:end, z[6]:z[7]))/width)
  #       })))
  #       max_overlap[i,] <- cbind(rank,
  #                                max(overlap_props$prop))
  #
  #     }
  #     # because which.max returns only first result
  #     remove <- tail(which(max_overlap$max_overlap == max(max_overlap$max_overlap)),1)
  #
  #     df <- df[-remove,]
  #
  #     cat("done", nrow(max_overlap))
  #   }   # end of while loop
  #
  #     keep <- which(x$summary$rank %in% df$rank)
  #     keep <- sort(keep)
  #   }
  ### end overlap_new

  # Select object -------------------------------------------------

  output <- x
  output$summary <- summ[keep,]
  output$rate.output <- summ$rate.output[keep]

  ## save original convert_rate object if it isn't already there
  if(is.null(output$original)) output$original <- input_x

  ## save selection criteria
  if(is.null(output$select_calls)) {
    output$select_calls <- list()
    output$select_calls[[1]] <- call
  } else {
    output$select_calls[[(length(output$select_calls)+1)]] <- call
  }

  ## Add custom ADDITIONAL class
  ## Maybe different way of doing this, but:
  ## 1. for analysis documentation purposes new object should have indication it was an
  ## object that was manipulated, i.e. not original
  ## 2. can't replace class because generic S3 functions will stop working
  ## (unless we just duplicate these for new class)
  if(inherits(output, "convert_rate") && !("convert_rate_select" %in% class(output))) class(output) <- c(class(output), "convert_rate_select")
  if(inherits(output, "convert_rate.ft") && !("convert_rate.ft_select" %in% class(output))) class(output) <- c(class(output), "convert_rate.ft_select")

  ## Message
  if(reordered) message(glue::glue(
    "\n----- Reordering complete. {length(input_x$rate.output)} rate(s) reordered by '{method}' method -----\n\n")) else
      message(glue::glue(
        "\n----- Selection complete. {length(input_x$rate.output) - length(keep)} rate(s) removed, {length(keep)} rate(s) remaining -----\n\n"))

  ## Return
  return(invisible(output))
}

