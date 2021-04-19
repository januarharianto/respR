#'@title Subset auto_rate results based on a range of criteria
#'
#'@description The `auto_rate` function is powerful, but the output can be large
#'  and difficult to explore. In addition, it may identify linear data regions,
#'  but from areas of the data that are not of experimental interest.  As an
#'  advanced, machine learning based process, it is also somewhat fallible and
#'  on occasion may return questionable results.
#'
#'  The `subset_rate` function subsets `auto_rate` results according to various
#'  criteria. For example, extracting only positive or negative rates, only the
#'  highest or lowest rates, only those from certain data regions, and numerous
#'  other methods that allow advanced filtering of results so the rates
#'  extracted are well-defined towards the research question of interest. This
#'  also allows for highly consistent reporting of results and rate selection
#'  criteria.
#'
#'  Multiple subsetting criteria can be applied by assigning the output and
#'  processing it through the function multiple times with different criteria,
#'  or alternatively via `%>%` piping. See examples.
#'
#'  Note: when choosing a `method`, keep in mind that to remain mathematically
#'  consistent, `respR` outputs oxygen consumption (i.e. respiration) rates as
#'  negative values. This is particularly important in the difference between
#'  `highest/lowest` and `min/max` methods. See Details.
#'
#'  When a `$rate` result is omitted by the subsetting criteria, all associated
#'  data in `$summary` (i.e. the associated row) is removed. Summary results are
#'  not reordered in any way, only the non-matching rates removed. The `$peaks`
#'  component (if present) of the `auto_rate` object is adjusted, and a
#'  `$subset_regs` value is added to `$metadata` indicating the number of
#'  regressions remaining after subsetting. The resulting object retains the
#'  `auto_rate` class, however an additional `auto_rate_subset` class is
#'  applied. The original `auto_rate` object is saved in the output as
#'  `$original`. The history of subsetting operations to the object is saved in
#'  `$subset_calls`
#'
#'  Regardless of which `method` is chosen, the first operation conducted is
#'  removal of duplicate results. Due to the way it works, the `auto_rate`
#'  kernel-density analysis occasionally identifies linear regions that are
#'  identical in all regards, including start/end datapoints, and these appear
#'  as identical rows in the output `$summary`, except for different `$density`
#'  values. For all methods, only unique regressions are retained. To apply only
#'  this duplicate removal method, use the default `method = NULL` or `method =
#'  "unique"`.
#'
#'  Generally speaking, for most large datasets we advise using
#'  \code{\link{subset_data}} and then running `auto_rate` on the subset(s) of
#'  the data you are interested in, rather than run it on the whole dataset and
#'  relying on `subset_rate` to filter it afterwards.
#'
#'@details These are the current methods by which rates in `auto_rate` objects
#'  can be subset. Matching results are *retained* in the output:
#'
#'  \subsection{ }{`NULL`, `unique`} Subsets only unique results, that is
#'  removes any duplicate regressions from the input `$summary`. `n` is ignored.
#'
#'  \subsection{ }{`positive`, `negative`} Subsets all `positive` (>0) or
#'  `negative` (<0) rates. `n` is ignored. Useful in intermittent respirometry
#'  where `auto_rate` may identify linear regions of oxygen increase during
#'  flushes. Note, to keep mathematically consistent, `respR` outputs oxygen
#'  consumption (i.e. respiration) rates as *negative* values.
#'
#'  \subsection{ }{`nonzero`, `zero`} Retains all `nonzero` rates (i.e. removes
#'  any zero rates), or retains *only* `zero` rates (i.e. removes all rates with
#'  any value). `n` is ignored.
#'
#'  \subsection{ }{`lowest`, `highest`} Special note should be taken of these
#'  methods. They can only be used when rates all have the same sign (i.e. are
#'  all negative or all positive). Here, they subset the highest and lowest
#'  ***absolute*** rate values. That is, if rates are all negative, `method =
#'  'highest'` will retain the highest magnitude rates regardless of the sign,
#'  that is the *most negative*. Essentially, these options ignore the sign of
#'  the rate. `n` indicates how many of the lowest/highest rates to retain See
#'  `min` and `max` options for extracting numerical low/high rates.
#'
#'  \subsection{ }{`lowest_percentile`, `highest_percentile`} Again, special
#'  note should be taken of these methods, which can only be used when rates all
#'  have the same sign (i.e. all negative or all positive). These methods retain
#'  the `n`'th lowest or highest percentile of ***absolute*** rate values. That
#'  is, if rates are all negative, `method = 'highest_percentile'` will retain
#'  the highest `n`'th percentile of rates regardless of sign, that is the *most
#'  negative*. Essentially, these options ignore the sign of the rate. Here `n`
#'  should be a percentile value between 0 and 1. For example, to extract the
#'  lowest 10th percentile of rates regardless of sign, you would enter `method
#'  = 'lowest_percentile', n = 0.1`.
#'
#'  \subsection{ }{`minimum`, `maximum`} In contrast to `lowest` and `highest`,
#'  these are *strictly numerical* options which take full account of the sign
#'  of the rate, and can be used where rates are a mix of positive and negative.
#'  That is, `method = 'minimum'` will retain the minimum value numerical rates,
#'  including the most negative. `n` indicates how many of the min/max rates to
#'  retain.
#'
#'  \subsection{ }{`minimum_percentile`, `maximum_percentile`} Like `min` and
#'  `max` these are *strictly numerical* inputs which retain the `n`'th minimum
#'  or maximum percentile of the rates and take full account of the sign. Here
#'  `n` should be a percentile value between 0 and 1. For example, if rates are
#'  all negative (i.e. typical uptake rates), to extract the lowest 10th
#'  percentile of rates, you would enter `method = 'maximum_percentile', n =
#'  0.1`. This is because the *lowest* rates are the *highest numerically*
#'  (`highest/lowest` percentile methods would be a better option in this case
#'  however).
#'
#'  \subsection{ }{`rate`, `rsq`, `row`, `time`, `density`} These methods refer
#'  to the respective columns of the `$summary` data frame. For these, `n`
#'  should be a numeric vector of two values. Matching rates which fall within
#'  the `n` range (inclusive) are retained. For example, to retain only rates
#'  where the `rate` value is between 0.05 and 0.08: `method = 'rate', n =
#'  c(0.05, 0.08)`. To retain all rates with a R-Squared above 0.90: `method =
#'  'rsq', n = c(0.9, 1)`. The `row` and `time` ranges refer to the
#'  `$row`-`$endrow` or `$time`-`$endtime` columns and original data source
#'  (`$dataframe` element of the input), and can be used to constrain results to
#'  rates from particular regions of the data (although usually a better option
#'  is to \code{\link{subset_data}} prior to analysis). Note, `time` is not the
#'  same as `duration` - see later section.
#'
#'  \subsection{ }{`time_omit`, `row_omit`} These methods refer to the original
#'  data, and are intended to *exclude* rates determined over particular data
#'  regions. This is useful in the case of, for example, a data anomaly such as
#'  a spike or sensor dropout. For these, `n` can be a single value indicating a
#'  single data row or timepoint, or a numeric vector of two values indicating a
#'  row or time range. Only rates (i.e. regressions) which *do not* utilise that
#'  particular value or range are retained in the output. For example, if an
#'  anomaly occurs precisely at timepoint 3000,  `time_omit = 3000` will mean
#'  only rates determined solely over regions before and after this will be
#'  retained. Similarly, if it occurs over a time range this can be entered as,
#'  `time_omit = c(3000, 3200)`.
#'
#'  \subsection{ }{`duration`} This method allows subsetting of rates which
#'  occur within a duration range. Here, `n` should be a numeric vector of two
#'  values indicating the duration range you are interested in retaining. Use
#'  this to set minimum and maximum durations in the time units of the original
#'  data. For example, `n = c(0,500)` will retain only rates determined over a
#'  maximum of 500 time units. To retain rates over a minimum duration, set this
#'  using the minimum value plus the maximum duration (or simply infinity, e.g.
#'  `n = c(500,Inf)`).
#'
#'  \subsection{ }{`manual`} This method simply allows particular rows of the
#'  `$summary` data frame to be manually selected to be retained. For example,
#'  to keep only the top ranked result: `method = 'manual', n = 1`. To keep
#'  multiple rows use regular `R` selection syntax: `n = 1:3`, `n = c(1,2,3)`,
#'  `n = c(5,8,10)`, etc. No value of `n` should exceed the number of rows in
#'  the `$summary` data frame.
#'
#'  \subsection{ }{`overlap`} This method removes rates which overlap, that is,
#'  linear regions or regressions calculated by `auto_rate` which partly or
#'  completely share the same rows of the original data. The `auto_rate`
#'  `linear` method may identify multiple linear regions, some of which may
#'  substantially overlap, or even be completely contained within others. In
#'  such cases summary operations such as taking an average of the rate values
#'  may be questionable, as certain values will be weighted higher due to these
#'  multiple, overlapping results. This method removes overlapping rates, using
#'  `n` as a threshold to determine degree of permitted overlap. It is
#'  recommended this method be used after all other selection criteria have been
#'  applied, as it is quite aggressive about removing rates, including
#'  high-ranked ones, and can be computationally intensive when there are many
#'  results. While it can be used with `auto_rate` results determined via the
#'  `rolling`, `lowest`, or `highest` methods, by their nature these methods
#'  produce all possible overlapping regressions, ordered in the appropriate
#'  way, so other subsetting methods are probably more appropriate.
#'
#'  Permitted overlap is determined by `n`, which indicates the minimum
#'  proportion of each particular regression which must overlap with another for
#'  it to be regarded as overlapping.  For example, `n = 0.2` means a regression
#'  would have to overlap with another by 20% or more of its total length to be
#'  regarded as overlapping.
#'
#'  The `overlap` method does two separate operations. First, regardless of the
#'  `n` value, any rate regressions which are completely contained within
#'  another are removed. Secondly, for each regression in `$summary` how many
#'  other regressions it overlaps with (accounting for `n`) is determined. The
#'  one which overlaps with the most others is then removed. In the event of
#'  several regressions overlapping with equal numbers of others, the one lowest
#'  down in the `$summary` table is removed. This will be the least ranked one
#'  (i.e. lowest density value) in the case of the `auto_rate` `linear` method.
#'  In the case of `auto_rate` `rolling`, `lowest`, or `highest` methods, this
#'  order is determined by the method. In general, the `overlap` subsetting
#'  should only be used in combination with the `auto_rate` `linear` method,
#'  unless you have a specific reason for doing so.
#'
#'  This analysis is repeated iteratively until only non-overlapping rates
#'  (accounting for `n`) are retained. If `n = 0`, only rates which do not
#'  overlap at all are retained. If `n = 1`, only rates which are entirely
#'  contained within another are removed.
#'
#'@return The output of `subset_rate` is a `list` object which retains the
#'  `auto_rate` class, but has an additional `auto_rate_subset` class. It
#'  contains two additional elements: `$original` contains the original,
#'  unaltered `auto_rate` object, which will be retained unaltered through
#'  multiple subsetting operations, that is even after processing through the
#'  function multiple times. `$subset_calls` contains the calls for every
#'  subsetting operation that has been applied to the `$original` object, from
#'  the first to the most recent. Note, if using `%>%` piping the `x` input in
#'  these appears as `x = .` where it has been piped from the previous call.
#'  These additional elements ensure the output contains the complete,
#'  reproducible history of the `auto_rate` object having been subset.
#'
#'@md
#'@param x list. An object of class `auto_rate` or `auto_rate_subset`.
#'@param method string. Method by which to subset rate results. Matching results
#'  are *retained* in the output. See Details.
#'@param n numeric. Number, percentile, or range of results to return depending
#'  on `method`. See Details.
#'@param plot logical. Plots a summary of subset locations within data (up to a
#'  maximum of the first 9 ranked plots).
#'
#'@export
#'
#'@importFrom glue glue
#'@importFrom data.table between
#'@importFrom stats quantile
#'
#'@examples
#'  ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
#'  auto_rate(plot = FALSE)
#'  ar_subs_neg <- subset_rate(ar_obj, method = "negative", plot = FALSE)

subset_rate <- function(x, method = NULL, n = NULL, plot = TRUE){

  ## Save function call for output
  call <- match.call()
  ## Save unaltered auto_rate input for output
  input_x <- x

  # Checks ------------------------------------------------------------------

  if(!("auto_rate" %in% class(x))) stop("subset_rate: Input is not an 'auto_rate' object")

  ## Check for empty auto_rate object
  ## Can occur if previously subset by silly criteria
  if(nrow(x$summary) == 0) stop("subset_rate: No rates found in $summary")

  ## pos and neg rates found
  if(any(x$rate > 0) && any(x$rate < 0)) message("subset_rate: Object contains both negative and positive rates. Ensure the chosen `method` is appropriate.")

  ## Specify a method
  #if(is.null(method)) stop("subset_rate: Please specify a 'method'")

  ## Validate method
  if(!is.null(method) && !(method %in% c("unique",
                                         "overlap",
                                         "duration",
                                         "density",
                                         "manual",
                                         "time",
                                         "time_omit",
                                         "row",
                                         "row_omit",
                                         "rsq",
                                         "rate",
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
                                         "positive"))) stop("subset_rate: 'method' input not recognised")


  # Remove duplicates -------------------------------------------------------
  # This happens regardless of any other subsetting operation.
  # Could be moved to auto_rate function, so can be removed from here when it is.

  # index of rows which are unique
  ## note only columns 1:7 since 8 is 'density' if 'linear' method
  index_unique <- which(!duplicated(x$summary[,1:7]))

  x$summary <- x$summary[index_unique,]
  x$rate <- x$rate[index_unique]
  x$metadata$subset_regs <- length(index_unique)

  if(x$method == "linear"){
    x$peaks <- x$peaks[index_unique,]
  }

  if(method == "unique" || is.null(method)){
    message("subset_rate: Subsetting only unique regressions. `n` input ignored...")
    method <- "unique"
    keep <- 1:nrow(x$summary)
    keep <- sort(keep)
  }

  # Positive rates only -----------------------------------------------------
  if(method == "positive"){
    message("subset_rate: Subsetting all positive rate values. `n` input ignored...")
    keep <- which(x$rate > 0)
    keep <- sort(keep)
  }

  # Negative rates only -----------------------------------------------------
  if(method == "negative"){
    message("subset_rate: Subsetting all negative rate values. `n` input ignored...")
    keep <- which(x$rate < 0)
    keep <- sort(keep)
  }

  # Nonzero rates only -----------------------------------------------------
  if(method == "nonzero"){
    message("subset_rate: Subsetting all non-zero rate values. `n` input ignored...")
    keep <- which(x$rate != 0)
    keep <- sort(keep)
  }

  # Zero rates only ---------------------------------------------------------
  if(method == "zero"){
    message("subset_rate: Subsetting all zero rate values. `n` input ignored...")
    keep <- which(x$rate == 0)
    keep <- sort(keep)
  }

  # lowest rates ------------------------------------------------------------
  ## Note these are NOT lowest *numerical*, but lowest *absolute* values
  ## E.g. if all negative will return the highest/least negative n values

  if(method == "lowest"){
    if(any(x$rate > 0) && any(x$rate < 0)) stop("subset_rate: Object contains both negative and positive rates. \n'lowest' method is intended to find lowest rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to subset only positive or negative rates first, or 'min' option to find *numerical* lowest rates.")
    if(is.null(n) || n %% 1 != 0 || n < 0) stop("subset_rate: For 'lowest' method 'n' must be a positive integer value.")
    if(n > length(x$rate)) message("subset_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("subset_rate: Subsetting lowest {n} *absolute* rate values..."))

    ## if all negative return HIGHEST numerical n
    if(all(x$rate <= 0)) keep <- sort(tail(order(x$rate), n))
    ## if all positive return LOWEST numerical n
    if(all(x$rate >= 0)) keep <- sort(head(order(x$rate), n))

    keep <- sort(keep)
  }

  # highest rates -----------------------------------------------------------
  ## Note these are NOT highest *numerical*, but highest *absolute* values

  if(method == "highest"){
    if(any(x$rate > 0) && any(x$rate < 0)) stop("subset_rate: Object contains both negative and positive rates. \n'highest' method is intended to find highest rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to subset only positive or negative rates first, or 'max' option to find *numerical* highest rates.")
    if(is.null(n) || n %% 1 != 0 || n < 0) stop("subset_rate: For 'highest' method 'n' must be a positive integer value.")
    if(n > length(x$rate)) message("subset_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("subset_rate: Subsetting highest {n} *absolute* rate values..."))

    ## if all negative return LOWEST numerical n
    if(all(x$rate <= 0)) keep <- sort(head(order(x$rate), n))
    ## if all positive return HIGHEST numerical n
    if(all(x$rate >= 0)) keep <- sort(tail(order(x$rate), n))

    keep <- sort(keep)
  }


  # lowest percentile rates -------------------------------------------------
  if(method == "lowest_percentile") {
    if(any(x$rate > 0) && any(x$rate < 0)) stop("subset_rate: Object contains both negative and positive rates. \n'lowest_percentile' method is intended to find lowest percentile rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to subset only positive or negative rates first, or see 'minimum_percentile' and 'maximum_percentile' options to perform *strictly numerical* operations.")
    if(n <= 0 || n >= 1) stop("subset_rate: For 'percentile' methods 'n' must be between 0 and 1.")

    message(glue::glue("subset_rate: Subsetting lowest {n*100}th percentile of *absolute* rate values..."))

    ## if all negative return HIGHEST numerical nth percentile
    if(all(x$rate <= 0)) {
      cutoff <- stats::quantile(x$rate, 1-n)
      keep <- sort(which(x$rate %in% x$rate[x$rate >= cutoff]))
    }
    ## if all positive return LOWEST numerical nth percentile
    if(all(x$rate >= 0)) {
      cutoff <- stats::quantile(x$rate, n)
      keep <- sort(which(x$rate %in% x$rate[x$rate <= cutoff]))
    }

    keep <- sort(keep)
  }

  # highest percentile rates ------------------------------------------------
  if(method == "highest_percentile") {
    if(any(x$rate > 0) && any(x$rate < 0)) stop("subset_rate: Object contains both negative and positive rates. \n'highest_percentile' method is intended to find highest percentile rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to subset only positive or negative rates first, or see 'minimum_percentile' and 'maximum_percentile' options to perform *strictly numerical* operations.")
    if(n <= 0 || n >= 1) stop("subset_rate: For 'percentile' methods 'n' must be between 0 and 1.")

    message(glue::glue("subset_rate: Subsetting highest {n*100}th percentile of *absolute* rate values..."))

    ## if all negative return LOWEST numerical nth percentile
    if(all(x$rate <= 0)) {
      cutoff <- stats::quantile(x$rate, n)
      keep <- sort(which(x$rate %in% x$rate[x$rate <= cutoff]))
    }
    ## if all positive return LOWEST numerical nth percentile
    if(all(x$rate >= 0)) {
      cutoff <- stats::quantile(x$rate, 1-n)
      keep <- sort(which(x$rate %in% x$rate[x$rate >= cutoff]))
    }

    keep <- sort(keep)
  }

  # min n rates -------------------------------------------------------------
  ## These are lowest *numerical* values.
  ## i.e can mix -ve and +ve
  ## min = lowest/most negative
  if(method == "minimum") {
    if(is.null(n) || n %% 1 != 0 || n < 0) stop("subset_rate: For 'min' method 'n' must be a positive integer value.")
    if(n > length(x$rate)) message("subset_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("subset_rate: Subsetting minimum {n} *numerical* rate values..."))
    keep <- sort(head(order(x$rate), n))
    keep <- sort(keep)
  }

  # max n rates -------------------------------------------------------------
  if(method == "maximum") {
    if(is.null(n) || n %% 1 != 0 || n < 0) stop("subset_rate: For 'max' method 'n' must be a positive integer value.")
    if(n > length(x$rate)) message("subset_rate: 'n' input is greater than number of rates in $summary. Nothing to remove.")

    message(glue::glue("subset_rate: Subsetting maximum {n} *numerical* rate values..."))
    keep <- sort(tail(order(x$rate), n))
    keep <- sort(keep)
  }

  # min percentile rates ----------------------------------------------------
  if(method == "minimum_percentile") {
    if(n <= 0 || n >= 1) stop("subset_rate: For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("subset_rate: Subsetting minimum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate, n)
    keep <- sort(which(x$rate %in% x$rate[x$rate <= cutoff]))
    keep <- sort(keep)
  }

  # max percentile rates ----------------------------------------------------
  if(method == "maximum_percentile") {
    if(n <= 0 || n >= 1) stop("subset_rate: For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("subset_rate: Subsetting maximum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate, 1-n) ## NOTE DIFFERENCE TO ABOVE
    keep <- sort(which(x$rate %in% x$rate[x$rate >= cutoff]))
    keep <- sort(keep)
  }


  # rate range --------------------------------------------------------------
  if(method == "rate"){
    if(length(n) != 2) stop("subset_rate: For 'rate' method 'n' must be a vector of two values.")
    message(glue::glue("subset_rate: Subsetting rates with values between {n[1]} and {n[2]}..."))
    keep <- sort(which(data.table::between(x$rate, n[1], n[2])))
    keep <- sort(keep)
  }


  # rsq range ---------------------------------------------------------------
  if(method == "rsq"){
    if(length(n) != 2) stop("subset_rate: For 'rsq' method 'n' must be a vector of two values.")
    message(glue::glue("subset_rate: Subsetting rates with rsq values between {n[1]} and {n[2]}..."))
    keep <- sort(which(data.table::between(x$summary$rsq, n[1], n[2])))
    keep <- sort(keep)
  }

  # row range ---------------------------------------------------------------
  if(method == "row"){
    if(length(n) != 2) stop("subset_rate: For 'row' method 'n' must be a vector of two values.")
    if(any(n > dim(x$dataframe)[1])) stop("subset_rate: Input for 'n': row inputs out of data frame range.")

    message(glue::glue("subset_rate: Subsetting rates which occur only between original data rows {n[1]} and {n[2]}..."))

    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$row >= n_order[1])
    keep2 <- which(x$summary$endrow <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
  }

  # row_omit ----------------------------------------------------------------
  if(method == "row_omit"){
    if(length(n) > 2) stop("subset_rate: For 'row_omit' method 'n' must be a single value or vector of two values.")
    message(glue::glue("subset_rate: Subsetting rates which *DO NOT* use original data row(s) of {glue::glue_collapse(n, ", ", last = \" to \")}..."))

    if(length(n) == 1) n <- c(n,n) ## if single value make 2-vec for code simplicity
    if(any(n > dim(x$dataframe)[1])) stop("subset_rate: Input for 'n': row inputs out of data frame range.")

    # which regs span n?
    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$row >= n_order[2]) ## start is AFTER n
    keep2 <- which(x$summary$endrow <= n_order[1]) ## end is BEFORE n
    keep <- c(keep1, keep2)
    keep <- sort(keep)
  }

  # time range --------------------------------------------------------------
  if(method == "time"){
    if(length(n) != 2) stop("subset_rate: For 'time' method 'n' must be a vector of two values.")
    if(any(n < range(x$dataframe[[1]])[1]) || any(n > range(x$dataframe[[1]])[2])) stop("subset_rate: Input for 'n': time inputs out of time data range.")

    message(glue::glue("subset_rate: Subsetting rates which occur only between times {n[1]} and {n[2]}..."))

    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$time >= n_order[1])
    keep2 <- which(x$summary$endtime <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
  }


  # time_omit ---------------------------------------------------------------
  if(method == "time_omit"){
    if(length(n) > 2) stop("subset_rate: For 'time_omit' method 'n' must be a single value or vector of two values.")
    message(glue::glue("subset_rate: Subsetting rates which *DO NOT* use time value(s) of {glue::glue_collapse(n, ", ", last = \" to \")}..."))
    if(length(n) == 1) n <- c(n,n) ## if single value make 2-vec for code simplicity
    if(any(n < range(x$dataframe[[1]])[1]) || any(n > range(x$dataframe[[1]])[2])) stop("subset_rate: Input for 'n': time inputs out of time data range.")

    # which regs span n?
    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$time >= n_order[2]) ## start is AFTER n
    keep2 <- which(x$summary$endtime <= n_order[1]) ## end is BEFORE n
    keep <- c(keep1, keep2)
    keep <- sort(keep)
  }


  # manual range ------------------------------------------------------------
  if(method == "manual"){
    # check within range of summary length
    if(!all(n %in% 1:nrow(x$summary))) stop("subset_rate: For 'manual' method: 'n' values are out of range of $summary data.frame rows...")
    message(glue::glue("subset_rate: Subsetting selected rows of the '$summary' table..."))
    n_order <- sort(n) # in case entered wrong way round
    keep <- n_order
    keep <- sort(keep)
  }


  # density range ------------------------------------------------------------
  if(method == "density"){
    if(x$method != "linear") stop("subset_rate: The 'density' method can only be used with results determined via the auto_rate 'linear' method.")
    if(length(n) != 2) stop("subset_rate: For 'density' method 'n' must be a vector of two values.")
    n_order <- sort(n) # in case entered wrong way round
    message(glue::glue("subset_rate: Subsetting rates with density values between {n[1]} and {n[2]}..."))
    keep <- sort(which(data.table::between(x$summary$density, n[1], n[2])))
    keep <- sort(keep)
  }


  # duration ----------------------------------------------------------------
  if(method == "duration"){
    if(length(n) != 2) stop("subset_rate: For 'duration' method 'n' must be a vector of two values.")
    n_order <- sort(n) # in case entered wrong way round
    message(glue::glue("subset_rate: Subsetting rates with duration between {n_order[1]} and {n_order[2]}..."))

    durations <- x$summary$endtime-x$summary$time

    keep1 <- which(durations >= n_order[1])
    keep2 <- which(durations <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
  }


  # overlap -----------------------------------------------------------------
  if(method == "overlap") {
    if(is.null(n)) {
      n <- 0
      message("subset_rate: 'overlap' method applying default 'n = 0', no overlapping permitted.")
    }
    if(n < 0 || n > 1) stop("subset_rate: For 'overlap' method 'n' must be between 0 and 1 inclusive.")
    #if(x$method != "linear") stop("subset_rate: The 'overlap' method should only be used with results determined via the auto_rate 'linear' method.")
    message(glue::glue("subset_rate: The 'overlap' method can be computationally intensive and may take some time."))
    message(glue::glue("subset_rate: Subsetting rates which overlap by {n*100}% or less..."))

    ## extract summary df
    df <- x$summary
    ## reverse it, since we will process from lowest ranked to top ranked in loops
    df <- df[nrow(df):1]

    ## Add these to use later
    df$row_width <- df$endrow - df$row
    df$orig_row_no <- nrow(df):1


    ################# FIRST - remove contained #####################
    ## i.e. remove all regressions completely contained within another longer one

    ## Need two dfs here, one to do loop with, another to modify
    out_df <- df

    ## so for each row we see if that regression is contained within any others
    ## if so - it gets removed from the other df
    ## Have to match by original row number, as rows are being removed on each loop
    for(i in 1:nrow(df)){

      orig_row <- df$orig_row_no[i] # original row of reg in summary
      start <- df$row[i] # start of reg in data
      end <- df$endrow[i] # end of reg in data

      ## Which other regs is it contained within?
      inside <- which(start >= df$row & end <= df$endrow)
      # It will match to itself, so remove this one
      if(any(inside == i)) inside <- inside[-which(inside == i)]

      ## If 'inside' has anything in it now, then this reg is within at least one other reg
      ## Therefore we want to remove it.
      ## Find this reg within output df, using matching of original row no.,
      ## and remove that row
      if(length(inside) > 0) {
        row_to_remove <- match(orig_row, out_df$orig_row_no)
        out_df <- out_df[-row_to_remove,]
      }
    }

    ################# SECOND - remove partial overlaps #####################
    ## Now we remove any regressions that overlap each other

    ## Here we loop through the df multiple times (while loop), because the multiple
    ## overlapping relationships change everytime you remove one.
    ## It will change in place as we do this.
    ## For each row of the summary df (for loop) we check how many other regs that
    ## one overlaps with (accounting for n)
    ## Then we sort and rank these.
    ## The one which overlaps with the most others gets removed.
    ## If there are multiple ones with the same no. of overlaps, the lowest ranked
    ## one gets removed (i.e. the lowest in auto_rate summary df ordering)
    ## After this one is removed the analysis is repeated (next i of while loop).
    ## It only stops when there are no more overlaps remaining (accounting for n)
    ## This **should** leave relatively higher ranked ones which don't overlap.

    ## Create objects to be used in loop
    sort <- table(c(1,1)) ## so it's not 0 long, which would stop while loop
    top <- NULL ## which row to remove before next loop

    while(length(sort) > 0){

      ## exclude row identified in previous loop
      if(!is.null(top)) out_df <- out_df[-top,]

      ## empty list for results of for loop
      results <- list()

      ## loop
      for(i in 1:nrow(out_df)) {

        start <- out_df$row[i] # start of reg
        end <- out_df$endrow[i] # end of reg
        width <- out_df$row_width[i] # width of reg
        overlap <- round(width * n) # allowed overlap

        ## For each regression in df (df[i,]) - which *other* regressions overlap it?
        ## They need to have start row (plus overlap) BEFORE end row of i reg
        ## And end row (minus overlap) AFTER start row of i reg
        overlaps <- which(out_df$row+overlap <= end & out_df$endrow-overlap >= start)

        ## May match with itself due to rounding of overlap, so remove it
        if(any(overlaps == i)) overlaps <- overlaps[-which(overlaps == i)]

        ## Save
        results[[i]] <- overlaps

      }

      ## sort all results from into table
      ## this will order by
      ## first - total number of other regs each reg in out_df overlaps with
      ## second - by row number of summary df
      ## so in event of multiple rows have same number of overlaps, lower numbers are
      ## lower ranked regs (because we reversed the df earlier)
      sort <-sort(table(unlist(results)), decreasing = TRUE)

      ## So FIRST one is the one which overlaps the MOST other regressions
      ## and is lower ranked (i.e. lower row number)
      ## So we exclude this for next loop
      top <- as.numeric(names(sort)[1])

      ## Loop repeats, every time removing this most overlapping, least ranked regression
      ## until sort is empty, indicating none of the remaining overlap any others
      ## (after accounting for n overlap tolerance)
    }

    ## keep is simply the remaining original row numbers (reversed back)
    keep <- rev(out_df$orig_row_no)
  }


  # Subset auto_rate object -------------------------------------------------

  output <- x
  output$summary <- output$summary[keep,]
  output$rate <- output$rate[keep]
  output$metadata$subset_regs <- length(keep)

  if(x$method == "linear"){
    output$peaks <- output$peaks[keep,]
  }

  ## save original ar object if it isn't already there
  if(!("original" %in% names(output))) output$original <- input_x

  ## save subsetting criteria
  if(!("subset_calls" %in% names(output))){
    output$subset_calls <- list()
    output$subset_calls[[1]] <- call
  } else {
    output$subset_calls[[(length(output$subset_calls)+1)]] <- call
  }

  ## Add custom ADDITIONAL class
  ## Maybe different way of doing this, but:
  ## 1. for analysis documentation purposes new object should have indiciation it was an
  ## auto_rate object that was manipulated, i.e. not original
  ## 2. can't replace 'auto_rate' class because generic S3 functions will stop working
  ## (unless we just duplicate these for new class)
  if(!("auto_rate_subset" %in% class(output))) class(output) <- c(class(output), "auto_rate_subset")



  # Plot --------------------------------------------------------------------

  ## This is SLOOOWWWWWW AS F
  if(plot) plot_multi_ar(output, n = 9)

  ## Message
  message(glue::glue(
    "\n----- Subsetting complete. {length(input_x$rate) - length(keep)} rate(s) removed, {length(keep)} rate(s) remaining -----\n\n"))

  ## Return
  return(output)

}


## older plot - faster but needs work to make it look nice ....

# if(plot == TRUE){
#
#   parorig <- par(no.readonly = TRUE) # save original par settings
#   n <- length(output$rate)
#   if(n == 0) message("No matching plots found.")
#   if(n == 1) par(mfrow = c(1,1))
#   if(n == 2) par(mfrow = c(1,2))
#   if(n %in% c(3,4)) par(mfrow = c(2,2))
#   if(n %in% c(5,6)) par(mfrow = c(2,3))
#   if(n %in% c(7,8,9)) par(mfrow = c(3,3))
#   if(n %in% c(10:12)) par(mfrow = c(3,4))
#   if(n %in% c(13:16)) par(mfrow = c(4,4)) ## start to get margins too large errors
#   if(n %in% c(17:20)) par(mfrow = c(4,5))
#   if(n > 20){
#     par(mfrow = c(4,5))
#     message("Over 20 results found. Plotting first 20 only...")
#   }
#   par(mar = c(0.1,0.1,0.1,0.1))
#   if(n != 0) for(i in 1:n) capture.output(plot(output, pos = i, choose =1, axes = FALSE))
#   on.exit(par(parorig)) # revert par settings to original
# }





