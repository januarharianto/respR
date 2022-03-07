#' @title Subset auto_rate results based on a range of criteria
#'
#' @description The `auto_rate` function is powerful, but the output can be
#'   large and difficult to explore. In addition, it may identify linear data
#'   regions, but from areas of the data that are not of experimental interest.
#'   As an advanced, machine learning based process, it is also somewhat
#'   fallible and on occasion may return questionable results.
#'
#'   The `subset_rate` function helps explore and filter `auto_rate` results by
#'   subsetting according to various criteria. For example, extracting only
#'   positive or negative rates, only the highest or lowest rates, only those
#'   from certain data regions, and numerous other methods that allow advanced
#'   filtering of results so the rates extracted are well-defined towards the
#'   research question of interest. This also allows for highly consistent
#'   reporting of results and rate selection criteria.
#'
#'   Multiple subsetting criteria can be applied by assigning the output and
#'   processing it through the function multiple times with different criteria,
#'   or alternatively via `%>%` piping. See Examples.
#'
#'   *Note:* when choosing a `method`, keep in mind that to remain
#'   mathematically consistent, `respR` outputs oxygen consumption (i.e.
#'   respiration) rates as negative values. This is particularly important in
#'   the difference between `highest/lowest` and `minimum/maximum` methods. See
#'   Details.
#'
#'   When a `$rate` result is omitted by the subsetting criteria, all associated
#'   data in `$summary` (i.e. the associated row) is removed. Summary results
#'   are not reordered in any way, only the non-matching rates removed. See
#'   **Output** section for more details.
#'
#'   Generally speaking, for most large datasets we recommend using
#'   [`subset_data()`] and then running `auto_rate` on the subset(s) of the data
#'   you are interested in, rather than run it on the whole dataset and relying
#'   on `subset_rate` to filter it afterwards.
#'
#' @details These are the current methods by which rates in `auto_rate` objects
#'   can be subset. Matching results are *retained* in the output:
#'
#'   ## `positive`, `negative`
#'
#'   Subsets all `positive` (>0) or `negative` (<0) rates. `n` is ignored.
#'   Useful, for example, in intermittent respirometry where `auto_rate` may
#'   identify linear regions of oxygen increase during flushes. Note, `respR`
#'   outputs oxygen consumption (i.e. respiration) rates as *negative* values,
#'   production rates as *positive*.
#'
#'   ## `nonzero`, `zero`
#'
#'   Retains all `nonzero` rates (i.e. removes any zero rates), or retains
#'   *only* `zero` rates (i.e. removes all rates with any value). `n` is
#'   ignored.
#'
#'   ## `lowest`, `highest`
#'
#'   Special note should be taken of these methods. They can only be used when
#'   rates all have the same sign (i.e. are all negative or all positive). Here,
#'   they subset the highest and lowest ***absolute*** rate values. That is, if
#'   rates are all negative, `method = 'highest'` will retain the highest
#'   magnitude rates regardless of the sign, that is the *most negative*.
#'   Essentially, these options ignore the sign of the rate. `n` indicates how
#'   many of the lowest/highest rates to retain See `minimum` and `maximum`
#'   options for extracting *numerically* lowest and highest rates.
#'
#'   ## `lowest_percentile`, `highest_percentile`
#'
#'   Again, special note should be taken of these methods, which can only be
#'   used when rates all have the same sign (i.e. all negative or all positive).
#'   These methods retain the `n`'th lowest or highest percentile of
#'   ***absolute*** rate values. That is, if rates are all negative, `method =
#'   'highest_percentile'` will retain the highest `n`'th percentile of rates
#'   regardless of sign, that is the *most negative*. Essentially, these options
#'   ignore the sign of the rate. Here `n` should be a percentile value between
#'   0 and 1. For example, to extract the lowest 10th percentile of rates
#'   regardless of sign, you would enter `method = 'lowest_percentile', n =
#'   0.1`.
#'
#'   ## `minimum`, `maximum`
#'
#'   In contrast to `lowest` and `highest`, these are *strictly numerical*
#'   options which take full account of the sign of the rate, and can be used
#'   where rates are a mix of positive and negative. That is, `method =
#'   'minimum'` will retain the minimum value numerical rates, including the
#'   most negative. `n` indicates how many of the min/max rates to retain.
#'
#'   ## `minimum_percentile`, `maximum_percentile`
#'
#'   Like `min` and `max` these are *strictly numerical* inputs which retain the
#'   `n`'th minimum or maximum percentile of the rates and take full account of
#'   the sign. Here `n` should be a percentile value between 0 and 1. For
#'   example, if rates are all negative (i.e. typical uptake rates), to extract
#'   the lowest 10th percentile of rates, you would enter `method =
#'   'maximum_percentile', n = 0.1`. This is because the *lowest* rates are the
#'   *highest numerically* (`highest/lowest` percentile methods would be a
#'   better option in this case however).
#'
#'   ## `rate`, `rsq`, `row`, `time`, `density`
#'
#'   These methods refer to the respective columns of the `$summary` data frame.
#'   For these, `n` should be a vector of two values. Matching regressions in
#'   which the respective parameter falls within the `n` range (inclusive) are
#'   retained. For example, to retain only rates where the `rate` value is
#'   between 0.05 and 0.08: `method = 'rate', n = c(0.05, 0.08)`. To retain all
#'   rates with a R-Squared above 0.90: `method = 'rsq', n = c(0.9, 1)`. The
#'   `row` and `time` ranges refer to the `$row`-`$endrow` or `$time`-`$endtime`
#'   columns and original data source (`$dataframe` element of the input), and
#'   can be used to constrain results to rates from particular regions of the
#'   data (although usually a better option is to \code{\link{subset_data}}
#'   prior to analysis). Note, `time` is not the same as `duration` - see later
#'   section.
#'
#'   ## `time_omit`, `row_omit`
#'
#'   These methods refer to the original data, and are intended to *exclude*
#'   rates determined over particular data regions. This is useful in the case
#'   of, for example, a data anomaly such as a spike or sensor dropout. For
#'   these inputs, `n` are values (a single value or multiple) indicating data
#'   rows or timepoints of the original data to exclude. Only rates (i.e.
#'   regressions) which *do not* utilise those particular values are retained in
#'   the output. For example, if an anomaly occurs precisely at timepoint 3000,
#'   `time_omit = 3000` means only rates determined solely over regions before
#'   and after this will be retained. If it occurs over a range this can be
#'   entered as, `time_omit = c(3000:3200)`. If you want to exclude a regular
#'   occurrence, for example the flushes in intermittent-flow respirometry they
#'   can be entered as a vector, e.g. `row_omit = c(1000, 2000, 3000)`.
#'
#'   ## `duration`
#'
#'   This method allows subsetting of rates which occur within a duration range.
#'   Here, `n` should be a numeric vector of two values indicating the duration
#'   range you are interested in retaining. Use this to set minimum and maximum
#'   durations in the time units of the original data. For example, `n =
#'   c(0,500)` will retain only rates determined over a maximum of 500 time
#'   units. To retain rates over a minimum duration, set this using the minimum
#'   value plus the maximum duration (or simply infinity, e.g. `n =
#'   c(500,Inf)`).
#'
#'   ## `manual`
#'
#'   This method simply allows particular rows of the `$summary` data frame to
#'   be manually selected to be retained. For example, to keep only the top
#'   ranked result: `method = 'manual', n = 1`. To keep multiple rows use
#'   regular `R` selection syntax: `n = 1:3`, `n = c(1,2,3)`, `n = c(5,8,10)`,
#'   etc. No value of `n` should exceed the number of rows in the `$summary`
#'   data frame.
#'
#'   ## `overlap`
#'
#'   This method removes rates which overlap, that is, linear regions or
#'   regressions calculated by `auto_rate` which partly or completely share the
#'   same rows of the original data. The `auto_rate` `linear` method may
#'   identify multiple linear regions, some of which may substantially overlap,
#'   or even be completely contained within others. In such cases summary
#'   operations such as taking an average of the rate values may be
#'   questionable, as certain values will be weighted higher due to these
#'   multiple, overlapping results. This method removes overlapping rates, using
#'   `n` as a threshold to determine degree of permitted overlap. It is
#'   recommended this method be used after other selection criteria have been
#'   applied, as it is quite aggressive about removing rates, and can be *very*
#'   computationally intensive when there are many results.
#'
#'   While it can be used with `auto_rate` results determined via the `rolling`,
#'   `lowest`, or `highest` methods, by their nature these methods produce *all
#'   possible* overlapping regressions, ordered in various ways, so other
#'   subsetting methods are more appropriate. The `overlap` method should only
#'   be used in combination with the `auto_rate` `linear` results, unless you
#'   have a specific reason for doing so.
#'
#'   The [`plot_ar()`] function is very useful for plotting `auto_rate` objects,
#'   and the results of `subset_rate` operations upon them, to visualise where
#'   regression results in the summary table occur in relation to the original
#'   dataset. See Examples.
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
#'   Secondly, for each regression in `$summary` starting from the lowest ranked
#'   (i.e. bottom of the summary table), the function checks if it overlaps with
#'   any others (accounting for `n`). If not, the next lowest is checked, and
#'   the function progresses up the summary table until it finds one that does.
#'   The first to be found overlapping is then removed, and the process repeats
#'   starting again from the bottom of the summary table. This is repeated
#'   iteratively until only non-overlapping rates (accounting for `n`) are
#'   retained.
#'
#'   If `n = 0`, only rates which do not overlap at all, that is share *no*
#'   data, are retained. If `n = 1`, only rates which are 100% contained within
#'   at least one other are removed.
#'
#'   ## Plot
#'
#'   While output objects are plotted as normal `auto_rate` objects if used in
#'   `plot()`, `subset_rate` has its own plotting functionality. This simply
#'   plots a grid of the remaining rates in `$summary` up to the first 20. This
#'   is simple functionality to give the user an idea of how subsetting is
#'   reducing the number of rates, and where the remaining rates occur within
#'   the data. It is really only useful once rates are down to fewer than 20
#'   remaining, or for examining the effects of different subsetting options on
#'   a small selection of rates. Therefore, the default is `plot = FALSE` to
#'   prevent this being produced for every single subsetting operation.
#'
#'   ## More
#'
#'   For additional help, documentation, vignettes, and more visit the `respR`
#'   website at <https://januarharianto.github.io/respR>
#'
#' @return The output of `subset_rate` is a `list` object which retains the
#'   `auto_rate` class, with an additional `auto_rate_subset` class applied.
#'
#'   It contains two additional elements: `$original` contains the original,
#'   unaltered `auto_rate` object, which will be retained unaltered through
#'   multiple subsetting operations, that is even after processing through the
#'   function multiple times. `$subset_calls` contains the calls for every
#'   subsetting operation that has been applied to the `$original` object, from
#'   the first to the most recent. Note, if using piping (`%>%` or `|>`), the
#'   `x` input in these appears as `"x = ."` where it has been piped from the
#'   previous call. These additional elements ensure the output contains the
#'   complete, reproducible history of the `auto_rate` object having been
#'   subset.
#'
#'   The `$summary` table contains a `$rank` column and the *original* rank of
#'   each result is retained. The `$peaks` component (if present) of the
#'   `auto_rate` object is adjusted, and a `$subset_regs` value is added to
#'   `$metadata` indicating the number of regressions remaining after
#'   subsetting.
#'
#' @param x list. An object of class `auto_rate` or `auto_rate_subset`.
#' @param method string. Method by which to subset rate results. Matching
#'   results are *retained* in the output. See Details.
#' @param n numeric. Number, percentile, or range of results to return depending
#'   on `method`. See Details.
#' @param plot logical. Default FALSE. Plots a summary of subset locations
#'   within data (up to a maximum of the first 20 ranked results).
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom data.table between
#' @importFrom stats quantile
#'
#' @examples
#'  ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
#'  auto_rate(plot = FALSE)
#'  ar_subs_neg <- subset_rate(ar_obj, method = "negative", plot = FALSE)

subset_rate <- function(x, method = NULL, n = NULL, plot = FALSE){

  ## Save function call for output
  call <- match.call()
  ## Save unaltered auto_rate input for output
  input_x <- x

  # Checks ------------------------------------------------------------------

  ## Check for valid auto_rate object
  if(!("auto_rate" %in% class(x))) stop("subset_rate: Input is not an 'auto_rate' object")

  ## pos and neg rates found
  if(any(x$rate > 0) && any(x$rate < 0)) message("subset_rate: Object contains both negative and positive rates. Ensure the chosen `method` is appropriate.")

  ## Specify a method
  if(is.null(method)) stop("subset_rate: Please specify a 'method'")

  ## Validate method
  if(!is.null(method) && !(method %in% c("overlap",
                                         "overlap_new",
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
    n_order <- sort(n)
    keep <- sort(which(data.table::between(x$rate, n_order[1], n_order[2])))
    keep <- sort(keep)
  }


  # rsq range ---------------------------------------------------------------
  if(method == "rsq"){
    if(length(n) != 2) stop("subset_rate: For 'rsq' method 'n' must be a vector of two values.")
    message(glue::glue("subset_rate: Subsetting rates with rsq values between {n[1]} and {n[2]}..."))
    n_order <- sort(n)
    keep <- sort(which(data.table::between(x$summary$rsq, n_order[1], n_order[2])))
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
    if(!(is.numeric(n)) || any(n %% 1 != 0))
      stop("subset_rate: For 'row_omit' method 'n' must only contain integer values of row.")
    message(glue::glue("subset_rate: Subsetting rates which *DO NOT* use original data row(s) in 'n' input..."))
    if(any(n > dim(x$dataframe)[1])) stop("subset_rate: Input for 'n': row inputs out of data frame range.")

    n_order <- sort(n)
    remove <- c() # empty obj for loop results
    # for each n, go through summary table by row
    # if it occurs between row and endrow then that row of summary gets removed
    for(i in n_order){
      remove_i <- which(apply(x$summary, 1, function(z) any(i %in% z[6]:z[7])))
      remove <- sort(unique(c(remove, remove_i)))
    }
    # if nothing to remove, above outputs integer(0), so this is for that...
    if(length(remove) > 0) keep <- (1:nrow(x$summary))[-remove] else
      keep <- 1:nrow(x$summary)

    keep <- sort(keep)
  }

  # time range --------------------------------------------------------------
  if(method == "time"){
    if(length(n) != 2) stop("subset_rate: For 'time' method 'n' must be a vector of two values.")
    if(any(n < range(x$dataframe[[1]], na.rm = TRUE)[1]) || any(n > range(x$dataframe[[1]], na.rm = TRUE)[2]))
      stop("subset_rate: Input for 'n': time inputs out of time data range.")

    message(glue::glue("subset_rate: Subsetting rates which occur only between times {n[1]} and {n[2]}..."))

    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$time >= n_order[1])
    keep2 <- which(x$summary$endtime <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
    keep <- sort(keep)
  }


  # time_omit ---------------------------------------------------------------
  if(method == "time_omit"){
    if(!(is.numeric(n)))
      stop("subset_rate: For 'time_omit' method 'n' must contain only numeric values of time.")
    message(glue::glue("subset_rate: Subsetting rates which *DO NOT* use time value(s) in 'n' input..."))
    if(any(n < range(x$dataframe[[1]], na.rm = TRUE)[1]) || any(n > range(x$dataframe[[1]], na.rm = TRUE)[2]))
      stop("subset_rate: Input for 'n': time inputs out of time data range.")

    n_order <- sort(n)
    remove <- c() # empty obj for loop results
    # for each n, go through summary table by row
    # if it occurs between time and endtime
    # then that row of summary gets removed
    for(i in n_order){
      remove_i <- which(apply(x$summary, 1, function(z)
        any(i >= z[8] && i <= z[9])))
      remove <- sort(unique(c(remove, remove_i)))
    }
    # if nothing to remove, above outputs integer(0), so this is for that...
    if(length(remove) > 0) keep <- (1:nrow(x$summary))[-remove] else
      keep <- 1:nrow(x$summary)

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

  # overlap -------------------------------------------------------------
  if(method == "overlap"){

    if(is.null(n)) {
      n <- 0
      message("subset_rate: 'overlap' method applying default 'n = 0', no overlapping permitted.")
    }
    if(n < 0 || n > 1)
      stop("subset_rate: For 'overlap' method 'n' must be between 0 and 1 inclusive.")
    message(glue::glue("subset_rate: The 'overlap' method can be computationally intensive and may take some time."))
    message(glue::glue("subset_rate: Subsetting rates which overlap by at least {n*100}% ..."))

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
  #     message("subset_rate: 'overlap' method applying default 'n = 0', no overlapping permitted.")
  #   }
  #   if(n < 0 || n > 1)
  #     stop("subset_rate: For 'overlap' method 'n' must be between 0 and 1 inclusive.")
  #   message(glue::glue("subset_rate: The 'overlap' method can be computationally intensive and may take some time."))
  #   message(glue::glue("subset_rate: Subsetting rates which overlap by at least {n*100}% ..."))
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

  # Subset auto_rate object -------------------------------------------------

  output <- x
  output$summary <- output$summary[keep,]
  output$rate <- output$rate[keep]
  output$metadata$subset_regs <- length(keep)
  output$peaks <- output$peaks[keep,]

  ## save original auto_rate object if it isn't already there
  if(is.null(output$original)) output$original <- input_x

  ## save subsetting criteria
  if(is.null(output$subset_calls)) {
    output$subset_calls <- list()
    output$subset_calls[[1]] <- call
  } else {
    output$subset_calls[[(length(output$subset_calls)+1)]] <- call
  }

  ## Add custom ADDITIONAL class
  ## Maybe different way of doing this, but:
  ## 1. for analysis documentation purposes new object should have indication it was an
  ## auto_rate object that was manipulated, i.e. not original
  ## 2. can't replace 'auto_rate' class because generic S3 functions will stop working
  ## (unless we just duplicate these for new class)
  if(!("auto_rate_subset" %in% class(output))) class(output) <- c(class(output), "auto_rate_subset")

  # Plot --------------------------------------------------------------------
  if(plot) plot_ar_grid(output)

## Message
message(glue::glue(
  "\n----- Subsetting complete. {length(input_x$rate) - length(keep)} rate(s) removed, {length(keep)} rate(s) remaining -----\n\n"))

## Return
return(output)

}







