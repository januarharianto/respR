#'@title Filter auto_rate results based on a range of criteria
#'
#'@description The `auto_rate` function is a very powerful function, but the
#'  output can be large and difficult to explore. `filter_rate` filters
#'  `auto_rate` results according to various criteria. For example, extracting
#'  only positive or negative rates, or only the highest or lowest rates.
#'
#'  Note: when choosing a `method`, keep in mind that to keep mathematically
#'  consistent, `respR` outputs oxygen consumption (i.e. respiration) rates as
#'  negative values. This is particularly important in the difference between
#'  `highest/lowest` and `min/max` methods.
#'
#'  When a `$rate` is omitted by the filtering criteria, all associated data in
#'  `$summary` (i.e. the associated row) is removed. Summary results are not
#'  reordered in any way, only the non-matching rates removed. The `$peaks` and
#'  `$metadata$no_peaks` components of the `auto_rate` object are similarly
#'  adjusted. The resulting object retains the `auto_rate` class, however an
#'  additional `auto_rate_filt` class is applied.
#'
#'  Multiple filtering criteria can be applied by chaining (via `%>%` piping)
#'  the function multiple times, or assigning and running the output through it
#'  multiple times with the different criteria. See examples.
#'
#'@details These are the current methods by which `auto_rate` objects can be
#'  filtered:
#'
#'  \subsection{ }{`positive`, `negative`} Retains all `positive` (>0) or
#'  `negative` (<0) rates. `n` is ignored. Useful in intermittent respirometry
#'  where `auto_rate` may identify linear regions of oxygen increase during
#'  flushes. Note, to keep mathematically consistent, `respR` outputs oxygen
#'  consumption (i.e. respiration) rates as negative values.
#'
#'  \subsection{ }{`nonzero`, `zero`} Retains all `nonzero` rates (i.e. removes
#'  any zero rates), or retains *only* `zero` rates (i.e. removes all rates with
#'  any value). `n` is ignored.
#'
#'  \subsection{ }{`lowest`, `highest`} Special note should be taken of these
#'  methods. They can only be used when rates all have the same sign (i.e. are
#'  all negative or all positive). Here, they retain the highest and lowest
#'  ***absolute*** rate values. That is, if rates are all negative, `method =
#'  'highest'` will retain the highest magnitude rates regardless of sign, that
#'  is the *most negative*. Essentially, these options ignore the sign of the
#'  rate. `n` indicates how many of the lowest/highest rates to extract. See
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
#'  \subsection{ }{`min`, `max`} In contrast to `lowest` and `highest`, these
#'  are *strictly numerical* options which take full account of the sign of the
#'  rate, and can be used where rates are a mix of positive and negative. That
#'  is, `method = 'min'` will retain the minimum value numerical rates,
#'  including the most negative. `n` indicates how many of the min/max rates to
#'  extract.
#'
#'  \subsection{ }{`min_percentile`, `max_percentile`} Like `min` and `max`
#'  these are *strictly numerical* inputs which retain the `n`'th minimum or
#'  maximum percentile of the rates and take full account of the sign. Here `n`
#'  should be a percentile value between 0 and 1. For example, if rates are all
#'  negative (i.e. typical uptake rates), to extract the lowest 10th percentile
#'  of rates, you would enter `method = 'max_percentile', n = 0.1`. This is
#'  because the *lowest* rates are the *highest numerically* (i.e. least
#'  negative).
#'
#'  \subsection{ }{`rate`, `rsq`, `row`, `time`} These methods work on the
#'  respective columns of the `$summary` data frame. For these, `n` should be a
#'  numeric vecctor of two values. Matching rows which fall within that range
#'  (inclusive) are returned. For example, to retain only rows where rate is
#'  between 0.05 and 0.08: `method = 'rate', n = c(0.05, 0.08)`. To retain all
#'  rates with a R-Squared above 0.90: `method = 'rsq', n = c(0.9, 1)`. The
#'  `row` and `time` ranges refer to the original data source and can be used to
#'  constrain results to rates from particular regions of the data. Note, `time`
#'  is not the same as `duration` - see next section.
#'
#'  \subsection{ }{`duration`} This method allows filtering of rates which occur
#'  over a duration range. Here, `n` should be a numeric vector of two values
#'  indicating the duration range you are interested in retaining. Use this to
#'  set minimum and maximum durations in the time units of the original data.
#'  For example, `n = c(0,500)`, will retain only rates determined over a
#'  maximum of 500 time units. To retain rates over a minimum duration, set this
#'  using the minimum value plus the maximum duration (or simply infinity, e.g.
#'  `n = c(500,Inf)`).
#'
#'  \subsection{ }{`manual`} This method simply allows particular rows of the
#'  `$summary` data frame to be manually selected to be retained. For example,
#'  to keep only the first result: `method = 'manual', n = 1`. To keep multiple
#'  columns use regular `R` selection syntax: `n = 1:3`, `n = c(1,2,3)`, `n =
#'  c(5,8,10)`, etc. No value of `n` should exceed the number of rows in the
#'  `$summary` data frame.
#'
#'@md
#'@param x list. An object of class `auto_rate` or `auto_rate_filt`.
#'@param method string. Method by which to filter rate results. Matching results
#'  are *retained* in the output. See Details.
#'@param n numeric. Number, percentile, or range of results to return. See
#'  Details.
#'@param plot logical. Plots a summary of all filtered plot locations within
#'  data.
#'
#'@return A list object of class `auto_rate` and `auto_rate_filt`.
#'
#'@export
#'
#'@importFrom glue glue
#'
#'@examples
#'  ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
#'  auto_rate(plot = FALSE)
#'  ar_filt_neg <- filter_rate(ar_obj, method = "negative", plot = FALSE)

filter_rate <- function(x, method = NULL, n = 1, plot = TRUE){

  # Checks ------------------------------------------------------------------
  ## Also for `auto_rate_filt` if we decide to use that as class
  if(!("auto_rate" %in% class(x))) stop("Input is not an 'auto_rate' object")

  ## Check for empty auto_rate object?
  ## can occur if previously filtered by silly criteria
  ## what to do/say if so?

  ## pos and neg rates found
  if(any(x$rate > 0) && any(x$rate < 0)) warning("Object contains both negative and positive rates. \nEnsure the chosen `method` is appropriate.")

  ## Specify a method (non-null check)
  if(is.null(method)) stop("Please specify a 'method'")

  ## validate method

  ## OTHER METHODS??
    ## - some way of excluding rates which overlap. Statistically should not use these in stats
    ## if they largely overlap. Falsely inflating stat strength of that result. But how to pick
    ## which one to exclude?

  if(!(method %in% c("duration",
                     "manual",
                     "time",
                     "row",
                     "rsq",
                     "rate",
                     "max_percentile",
                     "min_percentile",
                     "max",
                     "min",
                     "lowest_percentile",
                     "highest_percentile",
                     "lowest",
                     "highest",
                     "zero",
                     "nonzero",
                     "negative",
                     "positive"))) stop("'method' input not recognised")


  # Positive rates only -----------------------------------------------------
  if(method == "positive"){
    message("Filtering all positive rate values. `n` input ignored.")
    keep <- which(x$rate > 0)
  }

  # Negative rates only -----------------------------------------------------
  if(method == "negative"){
    message("Filtering all negative rate values. `n` input ignored.")
    keep <- which(x$rate < 0)
  }

  # Nonzero rates only -----------------------------------------------------
  if(method == "nonzero"){
    message("Filtering all non-zero rate values. `n` input ignored.")
    keep <- which(x$rate != 0)
  }

  # Zero rates only ---------------------------------------------------------
  if(method == "zero"){
    message("Filtering all zero rate values. `n` input ignored.")
    keep <- which(x$rate == 0)
  }

  # lowest rates ------------------------------------------------------------
  ## Note these are NOT lowest *numerical*, but lowest *absolute* values
  ## E.g. if all negative will return the highest/least negative n values

  if(method == "lowest"){
    if(any(x$rate > 0) && any(x$rate < 0)) stop("Object contains both negative and positive rates. \n'lowest' method is intended to find lowest rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to filter out only positive or negative rates first, or 'min' option to find *numerical* lowest rates.")

    message(glue::glue("Filtering lowest {n} *absolute* rate values..."))

    ## if all negative return HIGHEST numerical n
    if(all(x$rate <= 0)) keep <- sort(tail(order(x$rate), n))
    ## if all positive return LOWEST numerical n
    if(all(x$rate >= 0)) keep <- sort(head(order(x$rate), n))
  }

  # highest rates -----------------------------------------------------------
  ## Note these are NOT highest *numerical*, but highest *absolute* values

  if(method == "highest"){
    if(any(x$rate > 0) && any(x$rate < 0)) stop("Object contains both negative and positive rates. \n'highest' method is intended to find highest rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to filter out only positive or negative rates first, or 'max' option to find *numerical* highest rates.")

    message(glue::glue("Filtering highest {n} *absolute* rate values..."))

    ## if all negative return LOWEST numerical n
    if(all(x$rate <= 0)) keep <- sort(head(order(x$rate), n))
    ## if all positive return HIGHEST numerical n
    if(all(x$rate >= 0)) keep <- sort(tail(order(x$rate), n))
    ## if a mix of neg and pos stop
  }


  # lowest percentile rates -------------------------------------------------
  if(method == "lowest_percentile") {
    if(any(x$rate > 0) && any(x$rate < 0)) stop("Object contains both negative and positive rates. \n'lowest_percentile' method is intended to find lowest percentile rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to filter out only positive or negative rates first, or see 'min_percentile' and 'max_percentile' options to perform *strictly numerical* operations.")
    if(n <= 0 || n >= 1) stop("For 'percentile' methods 'n' must be between 0 and 1.")

    message(glue::glue("Filtering lowest {n*100}th percentile of *absolute* rate values..."))

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
  }

  # highest percentile rates ------------------------------------------------
  if(method == "highest_percentile") {
    if(any(x$rate > 0) && any(x$rate < 0)) stop("Object contains both negative and positive rates. \n'highest_percentile' method is intended to find highest percentile rate in absolute terms amongst rates all having the same sign. \nUse 'positive' or 'negative' method to filter out only positive or negative rates first, or see 'min_percentile' and 'max_percentile' options to perform *strictly numerical* operations.")
    if(n <= 0 || n >= 1) stop("For 'percentile' methods 'n' must be between 0 and 1.")

    message(glue::glue("Filtering highest {n*100}th percentile of *absolute* rate values..."))

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
  }

  # max percentile rates ----------------------------------------------------
  if(method == "max_percentile") {
    if(n <= 0 || n >= 1) stop("For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("Filtering maximum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate, 1-n) ## NOTE DIFFERENCE TO ABOVE
    keep <- sort(which(x$rate %in% x$rate[x$rate >= cutoff]))
  }


  # min n rates -------------------------------------------------------------
  ## These are lowest *numerical* values.
  ## i.e can mix -ve and +ve
  ## min = lowest/most negative
  if(method == "min") {
    message(glue::glue("Filtering minimum {n} *numerical* rate values..."))
    keep <- sort(head(order(x$rate), n))
  }

  # max n rates -------------------------------------------------------------
  if(method == "max") {
    message(glue::glue("Filtering maximum {n} *numerical* rate values..."))
    keep <- sort(tail(order(x$rate), n))
  }

  # min percentile rates ----------------------------------------------------
  if(method == "min_percentile") {
    if(n <= 0 || n >= 1) stop("For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("Filtering minimum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate, n)
    keep <- sort(which(x$rate %in% x$rate[x$rate <= cutoff]))
  }

  # max percentile rates ----------------------------------------------------
  if(method == "max_percentile") {
    if(n <= 0 || n >= 1) stop("For 'percentile' methods 'n' must be between 0 and 1.")
    message(glue::glue("Filtering maximum {n*100}th percentile *numerical* rate values..."))
    cutoff <- stats::quantile(x$rate, 1-n) ## NOTE DIFFERENCE TO ABOVE
    keep <- sort(which(x$rate %in% x$rate[x$rate >= cutoff]))
  }


  # rate range --------------------------------------------------------------
  if(method == "rate"){
    if(length(n) != 2) stop("For 'rate' method 'n' must be a vector of two values.")
    message(glue::glue("Filtering rates with values between {n[1]} and {n[2]}..."))
    keep <- sort(which(dplyr::between(x$rate, n[1], n[2])))
  }

  # rsq range ---------------------------------------------------------------
  if(method == "rsq"){
    if(length(n) != 2) stop("For 'rsq' method 'n' must be a vector of two values.")
    message(glue::glue("Filtering rates with rsq values between {n[1]} and {n[2]}..."))
    keep <- sort(which(dplyr::between(x$summary$rsq, n[1], n[2])))
  }

  # row range ---------------------------------------------------------------
  if(method == "row"){
    if(length(n) != 2) stop("For 'row' method 'n' must be a vector of two values.")
    message(glue::glue("Filtering rates which occur only between rows {n[1]} and {n[2]}..."))
    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$row >= n_order[1])
    keep2 <- which(x$summary$endrow <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
  }


  # time range --------------------------------------------------------------
  if(method == "time"){
    if(length(n) != 2) stop("For 'time' method 'n' must be a vector of two values.")
    message(glue::glue("Filtering rates which occur only between times {n[1]} and {n[2]}..."))
    n_order <- sort(n) # in case entered wrong way round
    keep1 <- which(x$summary$time >= n_order[1])
    keep2 <- which(x$summary$endtime <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
  }


  # manual range ------------------------------------------------------------
  if(method == "manual"){
    # check within range of summary length
    if(!all(n %in% 1:nrow(x$summary))) stop("For 'manual' method: 'n' values are out of range of $summary data.frame rows...")
    message(glue::glue("Filtering selected rows of the $summary data.frame..."))
    n_order <- sort(n) # in case entered wrong way round
    keep <- n_order
  }


  # duration ----------------------------------------------------------------
  if(method == "duration"){
    if(length(n) != 2) stop("For 'duration' method 'n' must be a vector of two values.")
    n_order <- sort(n) # in case entered wrong way round
    message(glue::glue("Filtering rates with duration between {n_order[1]} and {n_order[2]}..."))

    durations <- x$summary$endtime-x$summary$time

    keep1 <- which(durations >= n_order[1])
    keep2 <- which(durations <= n_order[2])
    keep <- keep1[keep1 %in% keep2]
  }



  # Filter ar object --------------------------------------------------------

  ## Options
  ## Save call or filtering criteria?
  ## Save excluded rows in separate element?
  ## Save original auto_rate object, i.e. x?
  ##    - will get big v quickly, especially if chaining together multiple times.

  output <- x
  output$summary <- output$summary[keep,]
  output$rate <- output$rate[keep]
  output$peaks <- output$peaks[keep,]
  output$metadata$no_peaks <- length(keep)

  ## Add custom ADDITIONAL class
  ## Maybe different way of doing this, but:
  ## 1. for analysis documentation purposes new object should have indiciation it was an
  ## auto_rate object that was manipulated, i.e. not original
  ## 2. can't replace 'auto_rate' class because generic S3 functions will stop working
  ## (unless we just duplicate these for new class)
  if(!("auto_rate_filt" %in% class(output))) class(output) <- c(class(output), "auto_rate_filt")



  # Plot --------------------------------------------------------------------

  ## This plotting is a horrible hack. Margins much too wide. Also very slow. Maybe reduce to max of 12.
  ## Must be a better way of determining grid too
  ## (cowplot has plot_grid but don't know how to make it work with these)

  ## ToDO
  ## narrow margins
  ## add box with value of rate in each plot - replace title so it doesn't overlap data

  if(plot == TRUE){

    parorig <- par(no.readonly = TRUE) # save original par settings
    n = length(output$rate)
    if(n == 0) message("No matching plots found.")
    if(n == 1) par(mfrow = c(1,1))
    if(n == 2) par(mfrow = c(1,2))
    if(n %in% c(3,4)) par(mfrow = c(2,2))
    if(n %in% c(5,6)) par(mfrow = c(2,3))
    if(n %in% c(7,8,9)) par(mfrow = c(3,3))
    if(n %in% c(10:12)) par(mfrow = c(3,4))
    if(n %in% c(13:16)) par(mfrow = c(4,4)) ## start to get margins too large errors
    if(n %in% c(17:20)) par(mfrow = c(4,5))
    if(n > 20){
      par(mfrow = c(4,5))
      message("Over 20 results found. Plotting first 20 only...")
    }
    if(n != 0) for(i in 1:n) capture.output(plot(output, pos = i, choose =1))
    on.exit(par(parorig)) # revert par settings to original
  }

  ## Message here like "Filtering complete: 'n' matching rates/rows returned" ???
  return(output)

}

##  shouldn't need these as long as auto_rate class is retained

##  HOWEVER - maybe should be different print out if filtered object

#' @export
# print.adjust_rate <- function(x, pos = 1, ...) {
#   cat("\n# adjust_rate # -------------------------\n")
#   cat("Note: please consider the sign of the value while correcting the rate.\n")
#   cat("\nRank/position", pos, "result shown. To see all results use summary().")
#   if (length(x) == 3) {
#     cat("\nInput rate:", x$input[pos])
#   } else cat("\nInput rate:", x$input.rate[pos])
#   cat("\nAdjustment:", x$adjustment[pos])
#   cat("\nAdj. rate:", x$corrected[pos], "\n")
#   return(invisible(x))
# }

#' @export
# summary.adjust_rate <- function(object, ...) {
#   cat("\n# summary.adjust_rate # -----------------\n")
#   if (length(object) == 3) {
#     rate <- object$input
#   } else rate <- object$input.rate
#   out <- data.table(rate, adjustment = x$adjustment, "adjusted rate" = object$corrected)
#   print(out)
#   return(invisible(object))
# }
