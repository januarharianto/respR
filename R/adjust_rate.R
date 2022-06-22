#' Adjust rates to account for background respiration or oxygen flux.
#'
#' The `adjust_rate` function adjusts oxygen uptake or production rates (for
#' example, as determined in [`calc_rate()`], [`calc_rate.int()`], or
#' [`auto_rate()`]) for background oxygen use by microbial organisms, or for
#' other removal or input of oxygen during a respirometry experiment. The
#' function accepts numeric values, as well as regular `respR` objects, and data
#' frames. See [`calc_rate.bg()`] for determining background rates, which is the
#' recommended way of passing background rates to `adjust_rate`. Rates
#' determined in `calc_rate` are also accepted as background rates.
#'
#' `adjust_rate` allows the rate, or multiple rates, in `x` to be adjusted in a
#' number of ways, as detailed below. Note that for those methods which accept
#' them, `by` and `by2` inputs of class `calc_rate`, `calc_rate.bg`,
#' `data.frame` or `inspect` can contain multiple columns of background oxygen
#' data, as long as they share the same numeric time data in column 1. In this
#' case, the mean of all rates calculated for all oxygen columns is used to
#' perform adjustments (see [`inspect()`] and [`calc_rate.bg()`] to coerce data
#' to this form). The exception to this is the `"paired"` method, where each
#' rate in `by` (i.e. rate in each oxygen column) is paired with the rate at the
#' same position in `x` and used to adjust it.
#'
#' ***Note:*** take special care with the *sign* of the rate used for
#' adjustments. In `respR` oxygen uptake rates are negative, as they represent a
#' negative slope of oxygen against time. Background rates will normally also be
#' a negative value, while any input of oxygen would be positive. See Examples.
#'
#' ***Methods***
#'
#' There are six methods of adjustment, briefly summarised here, with more
#' detail below:
#'
#' `"value"` - All experimental rates in `x` are adjusted by a single background
#' rate value in `by`.
#'
#' `"mean"` - This is the default method. All experimental rates in `x` are
#' adjusted by the mean of all background rate values in `by`.
#'
#' `"paired"` - Experimental rates in `x` are adjusted by the background rate
#' value at the same position in `by`. Therefore requires `x` and `by` to have
#' the same number of rates.
#'
#' `"concurrent"` - Experimental rates in `x` are adjusted by a background rate
#' calculated over the same time window in the data in `by`. Therefore requires
#' `x` and `by` to share the same time data and length (broadly speaking).
#'
#' `"linear"` - The time values for experimental rates in `x` are used to
#' calculate an adjustment value based on a background rate that changes
#' *linearly* with respect to time over the course of an experiment. Requires
#' two background recordings or values (`by`, `by2`), and that all data share
#' the same time data or scale.
#'
#' `"exponential"` - The time values for experimental rates in `x` are used to
#' calculate an adjustment value based on a background rate that changes
#' *exponentially* with respect to time over the course of an experiment.
#' Requires two background recordings or values (`by`, `by2`), and that all data
#' share the same time data or scale.
#'
#' ***More Detail***
#'
#' `"value"` - For experiments in which the rate from a single background
#' experiment (or any single background value) is being used to adjust one or
#' more specimen rates. Each rate in `x` is adjusted by the subtracting the
#' single value in `by`. `x` can be a numeric value, numeric vector,
#' `auto_rate`, `calc_rate.int`, or `calc_rate` object. `by` can be a single
#' numeric value, a `calc_rate.bg` object containing a single `$rate.bg` (i.e.
#' calculated from a 2-column data frame of time~oxygen), or a `calc_rate`
#' object containing a single `$rate`. All other inputs should be `NULL`.
#'
#' `"mean"` - For experiments in which the mean rate from multiple background
#' experiments is being used to adjust one or more specimen rates. Each rate in
#' `x` is adjusted by subtracting the *mean* of all background rates in `by`.
#' `x` can be a numeric value, numeric vector, `auto_rate`, `calc_rate.int`, or
#' `calc_rate` object. `by` can be a numeric value, numeric vector,
#' `calc_rate.bg` object containing multiple `$rate.bg`, or a `calc_rate` object
#' containing multiple `$rate`. All other inputs should be `NULL`. If `by` is a
#' single value, this will obviously have the same output as the `"value"`
#' method.
#'
#' `"paired"` - For experiments where multiple specimen experiments are being
#' adjusted by multiple different background rates. This is a vectorised
#' adjustment operation: rates in `x` are adjusted by the background rates at
#' the same position in `by`. That is, the first `x` adjusted by the first `by`,
#' second `x` by second `by`, etc. `x` can be a numeric value, numeric vector,
#' `auto_rate`, `calc_rate.int`, or `calc_rate` object. `by` can be a numeric
#' vector *of the same length*, a `calc_rate.bg` or `calc_rate` object where the
#' `$rate.bg` or `$rate` element is the *same length* as the rates in `x` to be
#' adjusted. All other inputs should be `NULL`.
#'
#' `"concurrent"` - For experiments in which one or more concurrent "blanks" or
#' background experiments are run alongside specimen experiments. Rates in `x`
#' are adjusted by a background rate calculated over the same time window in the
#' data in `by`. That is, the start and end time of each `x` rate is used to fit
#' a linear regression and calculate a background rate in the `$dataframe` in
#' `by`. `x` must be an `auto_rate`, `calc_rate.int`, or `calc_rate` object.
#' `by` must be a `data.frame`, `inspect`, `calc_rate.bg`, or `calc_rate` object
#' containing time~oxygen data. If there are multiple columns of background
#' oxygen the mean rate across the same time window in all columns is used. In
#' `calc_rate.bg` and `calc_rate` objects the `$rate.bg` or `$rate` element is
#' not used, only the `$dataframe`. The `x` and `by` data must share (broadly)
#' the *same time data or scale in the same units*. If the `x` and `by` data
#' differ in length by more than 5% or some time values are not shared between
#' the two datasets, a warning is given, but the adjustment is nevertheless
#' performed using the available data, by using the closest matching time window
#' in the background data.
#'
#' `"linear"` - This is a dynamic adjustment, intended for experiments in which
#' the background oxygen rate *changes* over the course of the experiment
#' *linearly* with respect to time. This is typical of long duration
#' respirometry experiments in high temperatures, where a "blank" is conducted
#' at the start of the experiment before the specimen is put in, and again at
#' the end after it is taken out. It requires therefore two background
#' recordings sharing the same numeric *time data* or *time scale*, in the same
#' units as the experiment to be adjusted. These can also be entered as two rate
#' *values* with associated *timestamps*, which again must share the same time
#' scale and units as the rate to be adjusted. This method can also be used in
#' experiments in which a concurrent blank experiment is conducted alongside
#' specimen experiments (as described in the `concurrent` method above), but in
#' which the background data is deemed too noisy to fit reliable regressions
#' over the short timescales specimen rates are determined. In this case, *any*
#' two reliable segments of the background data of any duration can be used to
#' determine how the background rate changes over the course of the experiment,
#' and then this used to adjust specimen rates using the appropriate rate
#' timestamps. The *time~background rate* linear relationship is calculated
#' using the midpoint of the time range of the `by` and `by2` rate regressions
#' (or values plus timestamps). The adjustments to `x` rates are calculated by
#' taking the midpoint of the time range over which it was determined and
#' applying it to the `by~by2` linear relationship. The `x` input can be a
#' numeric value, numeric vector, or a `calc_rate`, `calc_rate.int`, or
#' `auto_rate` object containing single or multiple rates. The `by` input is the
#' first background recording or rate value, and `by2` the second background
#' recording or rate value.
#'
#' While it is typical, the `x` rates do not necessarily need to be at
#' intermediate timepoints to the `by/by2` times. these are used only to
#' establish a *time~background rate* linear relationship, which can be
#' extrapolated before or after the time values used to calculate it. The `by`
#' and `by2` inputs can be a `data.frame`, `inspect` or `calc_rate.bg` object
#' containing background time~oxygen data. Alternatively, the rate `x`, and
#' background rates `by` and `by2` can be entered as values, in which case the
#' associated timepoints at which these were determined (generally the midpoint
#' of the time range over which the linear regression was fit) must be entered
#' as `time_x`, `time_by`, and `time_by2` (these timepoints are otherwise
#' automatically extracted from the input objects). Multiple `x` rates with
#' multiple `time_x` timepoints can be entered and adjusted, but only one linear
#' background rate relationship applied, that is `by`, `by2`, `time_by`, and
#' `time_by2` must be single numeric values in the correct units.
#'
#' `"exponential"` - This is a dynamic adjustment, intended for experiments in
#' which the background oxygen rate *changes* over the course of the experiment
#' *exponentially* with respect to time. This is typical of long duration
#' respirometry experiments in high temperatures, where a "blank" is conducted
#' at the start of the experiment before the specimen is put in, and again at
#' the end after it is taken out, and the background rate is found to increase
#' exponentially. This is identical to the `"linear"` method (see above for
#' requirements), except the adjustment is calculated as an exponential
#' relationship of the form - `lm(log(c(by, by2)) ~ c(time_by, time_by2))`.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints a single result, by default the first adjusted rate.
#' Others can be printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all results and metadata, or those
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The summary can
#' be exported as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all adjusted rates, or those specified by
#' the `pos` input. e.g. `mean(x, pos = 1:5)` The mean can be exported as a
#' separate value by passing `export = TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return Output is a list object of class `adjust_rate` containing all inputs,
#'   input rates, adjustment values, adjustment method and model (if relevant),
#'   and the primary output of interest `$rate.adjusted`.
#'
#' @param x numeric. A single numeric value, numeric vector, or object of class
#'   `calc_rate`, `calc_rate.int`,or `auto_rate`. This contains the experimental
#'   rate value(s) to be adjusted.
#' @param by numeric. A single numeric value, numeric vector, or object of class
#'   `calc_rate.bg` or `calc_rate`. This is the background rate(s) used to
#'   perform the adjustment to `x`. Can also be a `data.frame` or `inspect`
#'   object for `"concurrent"`, `"linear"` or `"exponential"` adjustments. See
#'   Details.
#' @param method string. Method of background adjustment. Defaults to `"mean"`.
#'   Other inputs are: `"value"`, `"paired"`, `"concurrent"`, `"linear"`,
#'   `"exponential"`. See Details.
#' @param by2 numeric. Either a single numeric value, a `calc_rate.bg` or
#'   `calc_rate` object, a `data.frame`, or `inspect` object. This is the source
#'   of the second background adjustment rate, and used only for dynamic
#'   adjustments (`"linear"` or `"exponential"`). See Details.
#' @param time_x numeric. The timestamp(s) for the rate(s) in `x`, if it was
#'   entered as a numeric (otherwise it is extracted from the `x` input object).
#'   Generally this is the midpoint of the time range over which each `x` rate
#'   was calculated. Used only in dynamic adjustments (`"linear"` or
#'   `"exponential"`). See Details.
#' @param time_by numeric. The timestamp of the background correction rate in
#'   `by`, if it was entered as a numeric (otherwise it is extracted from the
#'   `by` input object). Generally the midpoint of the time range over which it
#'   was calculated. Used only in dynamic adjustments (`"linear"` or
#'   `"exponential"`). See Details.
#' @param time_by2 numeric. The timestamp of the background correction rate in
#'   `by2`, if it was entered as a numeric (otherwise it is extracted from the
#'   `by2` input object). Generally the midpoint of the time range over which it
#'   was calculated. Used only in dynamic adjustments (`"linear"` or
#'   `"exponential"`). See Details.
#'
#' @export
#'
#' @examples
#' # Note that oxygen uptake rates are negative in respR since they represent a
#' # decrease in dissolved oxygen and negative slope. Typically both
#' # specimen rate and background rate values are negative.
#'
#' # Simple background adjustment to a single rate
#' # This is (-7.44) - (-0.04) = -7.40
#' adjust_rate(x = -7.44, by = -0.04, method = "value")
#'
#' # Oxygen input adjustment
#' # This is (-7.44) - (0.1) = -7.54
#' adjust_rate(x = -7.44, by = 0.1, method = "value")
#'
#' # Mean background respiration correction to a single rate.
#' adjust_rate(x = -7.44, by = c(-0.04, -0.05, -0.06),
#'             method = "mean")
#'
#' # Mean background respiration correction to multiple rates.
#' out <- adjust_rate(x = c(-7.44, -7.20, -7.67),
#'                    by = c(-0.04, -0.05, -0.06),
#'                    method = "mean")
#' summary(out)
#'
#' # Paired background respiration correction to multiple rates.
#' out <- adjust_rate(x = c(-7.44, -7.20, -7.67),
#'                    by = c(-0.04, -0.05, -0.06),
#'                    method = "paired")
#' summary(out)
#'
#' # Dynamic linear adjustment
#' # With a linear relationship between the 'by' and 'by2' rates,
#' # at the midpoint time value the adjustment to 'x' should be -0.5
#' adjust_rate(x = -10,
#'             time_x = 500,
#'             by = 0, by2 = -1,
#'             time_by = 0, time_by2 = 1000,
#'             method = "linear")
#'
#' # Same operation to multiple rates
#' out <- adjust_rate(x = c(-10, -11, -12),
#'                    time_x = c(500, 600, 700),
#'                    by = 0, by2 = -1,
#'                    time_by = 0, time_by2 = 1000,
#'                    method = "linear")
#' summary(out)
#'
#' # A complete workflow using objects instead of values.
#'
#' # Extract a single replicate from the middle of the zebrafish data
#' # and calculate rates
#' zeb_rate <- subset_data(zeb_intermittent.rd,
#'                         from = 38300,
#'                         to = 38720,
#'                         by = "time") %>%
#'   inspect() %>%
#'   auto_rate()
#'
#' # Calculate background rate at start of experiment
#' bg_start <- subset_data(zeb_intermittent.rd, 1, 4999, "time") %>%
#'   inspect() %>%
#'   calc_rate.bg() %>%
#'   print()
#'
#' # Calculate background rate at end of experiment
#' bg_end <- subset_data(zeb_intermittent.rd, 75140, 79251, "time") %>%
#'   inspect() %>%
#'   calc_rate.bg() %>%
#'   print()
#'
#' # Perform a dynamic linear adjustment
#' adjust_rate(zeb_rate, by = bg_start, by2 = bg_end,
#'             method = "linear") %>%
#'   summary()
#'
#' # Note the adjustment values applied are somewhere between the
#' # start and end background rate values

adjust_rate <- function(x, by, method = NULL, by2 = NULL,
                        time_x = NULL, time_by = NULL, time_by2 = NULL) {

  ## Save function call for output
  call <- match.call()

  # Apply default method ----------------------------------------------------
  if(is.null(method)) method <- "mean"

  # Validate inputs ---------------------------------------------------------

  ## Validate and classify method
  dynamic <- val_meth(method)


  # "value" checks ---------------------------------------------------------
  ## 'x' can be anything
  ## 'by' must be single numeric value or calc_rate.bg or calc_rate with one value in $rate.bg
  ## all others should be NULL
  if(method == "value"){

    if(!(class.val(x, num = TRUE, cr = TRUE, ar = TRUE, cr.int = TRUE)))
      stop("adjust_rate: for method = 'value' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', or 'auto_rate'.")

    if(!(class.val(by, num.sing = TRUE, crbg.sing = TRUE, cr.sing = TRUE)))
      stop("adjust_rate: for method = 'value' the 'by' input must be a single numeric value, 'calc_rate.bg' object with one value in '$rate.bg', or `calc_rate` object with one value in '$rate'")

    if(!is.null(by2)) stop("adjust_rate: for method = 'value' the 'by2' input should be NULL.")
    if(!is.null(time_x)) stop("adjust_rate: for method = 'value' the 'time_x' input should be NULL.")
    if(!is.null(time_by)) stop("adjust_rate: for method = 'value' the 'time_by' input should be NULL.")
    if(!is.null(time_by2)) stop("adjust_rate: for method = 'value' the 'time_by2' input should be NULL.")
  }

  # "mean" checks -----------------------------------------------------------

  ## 'x' and 'by' can be anything
  ## all others should be NULL
  if(method == "mean"){

    if(!(class.val(x, num = TRUE, cr = TRUE, ar = TRUE, cr.int = TRUE)))
      stop("adjust_rate: for method = 'mean' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', or 'auto_rate'.")

    if(!(class.val(by, num = TRUE, crbg = TRUE, cr = TRUE)))
      stop("adjust_rate: for method = 'mean' the 'by' input must be numeric, object of class 'calc_rate.bg', or object of class 'calc_rate'.")

    if(!is.null(by2)) stop("adjust_rate: for method = 'mean' the 'by2' input should be NULL.")
    if(!is.null(time_x)) stop("adjust_rate: for method = 'mean' the 'time_x' input should be NULL.")
    if(!is.null(time_by)) stop("adjust_rate: for method = 'mean' the 'time_by' input should be NULL.")
    if(!is.null(time_by2)) stop("adjust_rate: for method = 'mean' the 'time_by2' input should be NULL.")
  }


  # "paired" checks ---------------------------------------------------------

  ## 'x' and 'by' can be anything BUT MUST BE SAME LENGTH
  ## all others should be NULL
  ## same length check in code, after rate and rate.bg extracted
  if(method == "paired"){

    if(!(class.val(x, num = TRUE, cr = TRUE, ar = TRUE, cr.int = TRUE)))
      stop("adjust_rate: for method = 'paired' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', or 'auto_rate'.")

    if(!(class.val(by, num = TRUE, crbg = TRUE, cr = TRUE)))
      stop("adjust_rate: for method = 'paired' the 'by' input must be numeric, object of class 'calc_rate.bg', or object of class 'calc_rate'.")

    if(!is.null(by2)) stop("adjust_rate: for method = 'paired' the 'by2' input should be NULL.")
    if(!is.null(time_x)) stop("adjust_rate: for method = 'paired' the 'time_x' input should be NULL.")
    if(!is.null(time_by)) stop("adjust_rate: for method = 'paired' the 'time_by' input should be NULL.")
    if(!is.null(time_by2)) stop("adjust_rate: for method = 'paired' the 'time_by2' input should be NULL.")
  }


  # "concurrent" checks -----------------------------------------------------

  ## 'x' should be calc_rate/auto_rate and 'by' should be dataframe, or the
  ## dataframe within an inspect or calc.rate.bg or calc_rate object
  ## all other inputs should be NULL
  if(method == "concurrent") {

    if(!(class.val(x, cr = TRUE, ar = TRUE, cr.int = TRUE)))
      stop("adjust_rate: For method = \"concurrent\" the 'x' input must be a calc_rate, 'calc_rate.int', or auto_rate object.")

    if(!(class.val(by, df = TRUE, insp = TRUE, crbg = TRUE, cr = TRUE)))
      stop("adjust_rate: For method = \"concurrent\" the 'by' input must be a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object.")

    if(!is.null(by2)) stop("adjust_rate: for method = 'concurrent' the 'by2' input should be NULL.")
    if(!is.null(time_x)) stop("adjust_rate: for method = 'concurrent' the 'time_x' input should be NULL.")
    if(!is.null(time_by)) stop("adjust_rate: for method = 'concurrent' the 'time_by' input should be NULL.")
    if(!is.null(time_by2)) stop("adjust_rate: for method = 'concurrent' the 'time_by2' input should be NULL.")

  }



  # "linear" & "exponential" checks -----------------------------------------

  # "linear" and "exponential" methods are identical except for formula applied, so checks are same
  if(dynamic) {

    ## 'x'  - cannot be NULL - can be anything
    if(!(class.val(x, num = TRUE, cr = TRUE, ar = TRUE, cr.int = TRUE)))
      stop(glue::glue("adjust_rate: For method = \"{method}\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int' or 'auto_rate'."))
    ## 'time_x' - if 'x' is value or vector, 'time_x' must be numeric of same length
    if(is.numeric(x))
      if(!is.numeric(time_x) || length(x) != length(time_x))
        stop(glue::glue("adjust_rate: For method = \"{method}\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length (i.e. timestamp(s) for all rates in 'x')."))
    ## 'time_x' - if 'x' is calc_rate or auto_rate, 'time_x' must be NULL
    if(class.val(x, cr = TRUE, ar = TRUE, cr.int = TRUE))
      if(!is.null(time_x))
        stop(glue::glue("adjust_rate: For method = \"{method}\" and a calc_rate or auto_rate 'x' input, the 'time_x' input must be NULL."))

    ## by & by2 do not necessarily have to match in terms of class...
    ## no reason why you can't have value for one, calc_rate.bg for other....?

    ## 'by' - cannot be NULL or VECTOR - can be single value or df, inspect or calc_rate.bg or calc_rate
    if(!(class.val(by, num.sing = TRUE, df = TRUE, insp = TRUE, crbg = TRUE, cr = TRUE)))
      stop(glue::glue("adjust_rate: For method = \"{method}\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data."))
    ## if by is sn, then time_by must be sn
    if(class.val(by, num.sing = TRUE))
      if(!class.val(time_by, num.sing = TRUE))
        stop(glue::glue("adjust_rate: For method = \"{method}\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value (i.e. a timestamp for 'by' background rate)."))
    ## if by is anything else acceptable, time_by must be NULL
    if(class.val(by, df = TRUE, insp = TRUE, crbg = TRUE, cr = TRUE))
      if(!is.null(time_by))
        stop(glue::glue("adjust_rate: For method = \"{method}\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL."))

    ## 'by2' - cannot be NULL or VECTOR - can be single value or df, inspect or calc_rate.bg or calc_rate
    if(!class.val(by2, num.sing = TRUE, df = TRUE, insp = TRUE, crbg = TRUE, cr = TRUE))
      stop(glue::glue("adjust_rate: For method = \"{method}\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data."))
    ## if by2 is sn, then time_by2 must be sn
    if(class.val(by2, num.sing = TRUE))
      if(!class.val(time_by2, num.sing = TRUE))
        stop(glue::glue("adjust_rate: For method = \"{method}\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value (i.e. a timestamp for 'by2' background rate)."))
    ## if by2 is anything else acceptable, time_by2 must be NULL
    if(class.val(by2, df = TRUE, insp = TRUE, crbg = TRUE, cr = TRUE))
      if(!is.null(time_by2))
        stop(glue::glue("adjust_rate: For method = \"{method}\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL."))
  }


  # method = "value" -------------------------------------------------------

  if(method == "value"){

    ## Extract bg rate
    if (any(class(by) %in% "calc_rate.bg")) bg1 <-  by$rate.bg else
      if (any(class(by) %in% "calc_rate")) bg1 <-  by$rate else
        bg1 <- by

      # Extract x rate
      if (any(class(x) %in% c("calc_rate", "calc_rate.int", "auto_rate"))) {
        rate <- x$rate
      } else rate <- x

      # Use value for correction
      adjustment <- unname(unlist(bg1))
      out_model <- NULL

      #  Perform Adjustment
      rate.adjusted <- unname(unlist(rate - adjustment))
  }

  # method = "mean" ---------------------------------------------------------

  if(method == "mean"){

    ## Extract bg rate
    if (any(class(by) %in% "calc_rate.bg")) bg1 <-  by$rate.bg else
      if (any(class(by) %in% "calc_rate")) bg1 <-  by$rate else
        bg1 <- by

      if(length(bg1) > 1)
        message("adjust_rate: The 'by' input contains multiple background rates. The mean value will be used to perform adjustments.")


      # Extract x rate
      if (any(class(x) %in% c("calc_rate", "calc_rate.int", "auto_rate"))) {
        rate <- x$rate
      } else rate <- x

      # Use mean value of rate.bg for correction
      adjustment <- unname(unlist(mean(bg1)))
      out_model <- NULL

      #  Perform Adjustment
      rate.adjusted <- unname(unlist(rate - adjustment))
  }

  # method = "paired" -------------------------------------------------------

  if(method == "paired"){

    ## Extract bg rate
    if (any(class(by) %in% "calc_rate.bg")) bg1 <-  by$rate.bg else
      if (any(class(by) %in% "calc_rate")) bg1 <-  by$rate else
        bg1 <- by

      # Extract x rate
      if (any(class(x) %in% c("calc_rate", "calc_rate.int", "auto_rate"))) {
        rate <- x$rate
      } else rate <- x

      if(length(rate) != length(bg1))
        stop("adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")

      # Use paired values of rate.bg for correction
      adjustment <- unname(unlist(bg1))
      out_model <- NULL

      #  Perform Adjustment
      rate.adjusted <- unname(unlist(rate - adjustment))
  }



  # method = "concurrent" -------------------------------------------------------

  if(method == "concurrent"){

    # Extract x rate
    rate <- x$rate
    # locations of rate regressions
    starts <- x$summary$time
    ends <- x$summary$endtime

    ## background df
    if(is.data.frame(by)) bg_df <- by else
      if(any(class(by) %in% c("inspect", "calc_rate.bg", "calc_rate"))) bg_df <- as.data.frame(by$dataframe)

    ## If data lengths differ by more than 5% warn
    lx <- nrow(x$dataframe)
    lby <- nrow(bg_df)
    if(abs(diff(c(lx, lby)))/lx > 0.05)
      warning("adjust_rate: 'x' and 'by' inputs differ in length by more than 5%. \nmethod = \"concurrent\" is intended for background experiments that have been run in parallel and so should be approximately the same length, and share the same 'time' data. \nAdjustments have been attempted anyway, using shared or closest time values in the 'x' and 'by' inputs.")

    ## Check all bg regression time values within range of those in x rates
    if(any(!(data.table::between(starts, min(bg_df[[1]]), max(bg_df[[1]])))) ||
       any(!(data.table::between(ends, min(bg_df[[1]]), max(bg_df[[1]])))))
      warning("adjust_rate: Some time values used in 'x' rate calculations not present in 'by' background data.\n This probably comes from using datasets of different length for 'x' and 'by'.\n Background adjustments have been applied using available data, but this may have unintended consequences.\n Check results carefully.")

    ## Calc background over same region in each O2 column in bg data
    ## Use calc_rate.bg as by default it uses all columns, and will also fall back to closest matching behaviour
    ## if datasets don't have exactly the same time values.

    invisible(capture.output( # to suppress subset_rate console output
      out_model <- suppressMessages( # to suppress calc_rate.bg msg
        mapply(function(p,q){
          subset_data(bg_df, from = p, to = q, by = "time") %>% # subset concurrent section of bg data
            as.data.frame() %>% # because of subset_rate/calc_rate.bg error with data.table. can remove when fixed
            calc_rate.bg(plot = FALSE)
        },
        p = starts, q = ends, SIMPLIFY = FALSE))))

    # mean bg rate - using new S3 function support
    invisible(capture.output(adjustment <- sapply(out_model,
                                                  function(x) suppressMessages(mean(x, export = TRUE)))))
    adjustment <- unname(unlist(adjustment))

    ## check rate and adjustment are equal lengths
    ## (should not happen now, after lots of debugging, but you never know)
    if(length(rate) != length(adjustment))
      stop("adjust_rate: Error applying adjustment. Input rates and adjustment are different lengths. Please report to developers.")

    rate.adjusted <- unname(unlist(rate - adjustment))

  }

  # dynamic corrections -----------------------------------------------------

  if(dynamic) {

    ## Extract rate input
    if (any(class(x) %in% c("calc_rate", "calc_rate.int", "auto_rate"))) {
      rate <- x$rate
    } else {
      rate <- x
    }

    # Extract rate timestamp(s)
    if (any(class(x) %in% c("calc_rate", "calc_rate.int", "auto_rate"))) {
      t_rate <- (x$summary$time + x$summary$endtime)/2
    } else {
      t_rate <- time_x
    }

    ## Extract bg rates and timestamps
    # bg1
    ## as.data.frame() because of bug/failure of calc_rate.bg with data.tables. Can possibly remove when fixed.
    if (any(class(by) %in% c("calc_rate.bg", "inspect", "calc_rate"))){
      bg1 <-  mean(calc_rate.bg(as.data.frame(by$dataframe), time = 1, oxygen = 2:length(by$dataframe), plot = FALSE)$rate.bg)
      t_bg1 <- midpt(by$dataframe[[1]])
    } else if (is.data.frame(by)) {
      bg1 <- mean(calc_rate.bg(as.data.frame(by), time = 1, oxygen = 2:length(by), plot = FALSE)$rate.bg)
      t_bg1 <- midpt(by[[1]])
    } else {
      bg1 <- by
      t_bg1 <- time_by
    }
    # bg2
    if (any(class(by2) %in% c("calc_rate.bg", "inspect", "calc_rate"))){
      bg2 <-  mean(calc_rate.bg(as.data.frame(by2$dataframe), time = 1, oxygen = 2:length(by2$dataframe), plot = FALSE)$rate.bg)
      t_bg2 <- midpt(by2$dataframe[[1]])
    } else if (is.data.frame(by2)) {
      bg2 <- mean(calc_rate.bg(as.data.frame(by2), time = 1, oxygen = 2:length(by2), plot = FALSE)$rate.bg)
      t_bg2 <- midpt(by2[[1]])
    } else {
      bg2 <- by2
      t_bg2 <- time_by2
    }

    # More checks after all rates and timestamps have been extracted

    ## time_by2 should be greater than time_by1
    if(t_bg2 < t_bg1)
      stop("adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")

    ## t_rate - should not HAVE to be within range of t_bg and t_bg2, but maybe warn if outside
    if(any(!(data.table::between(t_rate, t_bg1, t_bg2))))
      warning("adjust_rate: One or more of the timestamps for the rate(s) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates. \nEnsure this is correct. The adjustment value has been calculated regardless by extrapolating outside the background rates time window.")

    ## sign(0) is different, so need exception. If either is zero it doesn't matter
    if(method == "linear" && sign(bg1) != 0 && sign(bg2) != 0 && sign(bg1) != sign(bg2))
      warning("adjust_rate: background rates in 'by' and 'by2' differ in sign (i.e. one is +ve, one is -ve). \nEnsure this is correct. The 'linear' adjustment has been performed regardless.")

    ## can't fit exponential if a bg rate is zero
    if(method == "exponential" && (bg1 == 0 || bg2 == 0))
      stop("adjust_rate: method = \"exponential\" cannot be used when a 'by' or 'by2' background rate is zero.")

    ## can't fit exponential if bg rates are different signs
    if(method == "exponential" && sign(bg1) != sign(bg2))
      stop("adjust_rate: method = \"exponential\" cannot be used when 'by' and 'by2' background rates are not the same sign (i.e. one is +ve, one is -ve).")

    # method = "linear" -------------------------------------------------------

    if(method == "linear"){

      ## fit lm
      bg_lm <- lm(c(bg1, bg2) ~ c(t_bg1, t_bg2))

      ## extract slope and intercept
      bg_lm_int <- coef(bg_lm)[1]
      bg_lm_slp <- coef(bg_lm)[2]

      ## fix slope - if NA make it zero
      if(is.na(bg_lm_slp)) bg_lm_slp <- 0

      ## apply to timestamp of rates to get adjustment value
      adjustment <- unname(unlist(t_rate * bg_lm_slp + bg_lm_int))
      out_model <- bg_lm

      ## check rate and adjustment are equal lengths
      ## (should not happen now, after lots of debugging, but you never know)
      if(length(rate) != length(adjustment))
        stop("adjust_rate: Error applying adjustment. Input rates and adjustment are different lengths. Please report to developers.")

      rate.adjusted <- unname(unlist(rate - adjustment))

    }

    # method = "exponential" --------------------------------------------------

    if(method == "exponential"){

      ## if negative, can't fit an exponential model
      ## so convert to positive
      if(bg1 < 0 && bg2 < 0) {
        both_neg <- TRUE
        bg1 <- bg1 * -1
        bg2 <- bg2 * -1
      } else {
        both_neg <- FALSE
        bg1 <- bg1
        bg2 <- bg2
      }

      ## fit exponential lm
      bg_exp <- lm(log(c(bg1, bg2)) ~ c(t_bg1, t_bg2))

      ## extract slope and intercept
      ## needs to convert back from log
      bg_exp_int <- exp(coef(bg_exp)[1])
      bg_exp_slp <- exp(coef(bg_exp)[2])

      ## apply to timestamp_rate to get adjustment
      adjustment <- unname(unlist(bg_exp_int * bg_exp_slp ^ t_rate))
      out_model <- bg_exp

      ## if both bg rates negative, convert back to negative
      if(both_neg) adjustment <- adjustment * -1

      ## check rate and adjustment are equal lengths
      ## (should not happen now, after lots of debugging, but you never know)
      if(length(rate) != length(adjustment))
        stop("adjust_rate: Error applying adjustment. Input rates and adjustment are different lengths. Please report to developers.")

      rate.adjusted <- unname(unlist(rate - adjustment))
    }
  }


  # Output ------------------------------------------------------------------

  # is adjustment is a single value, make it same length as rates
  # makes for easier printing etc.
  if(length(adjustment) == 1) adjustment <- rep(adjustment, length(rate.adjusted))

  inputs <- list(x = x,
                 by = by,
                 by2 = by2,
                 time_x = time_x,
                 time_by = time_by,
                 time_by2 = time_by2)

  if(any(class(x) %in% c("calc_rate", "calc_rate.int", "auto_rate"))) {
    summary <- cbind(x$summary,
                     adjustment = adjustment,
                     rate.adjusted = rate.adjusted)
  } else {
    summary <- data.table::data.table(rank = 1:length(rate.adjusted),
                                      rate = rate,
                                      adjustment = adjustment,
                                      rate.adjusted = rate.adjusted)
  }

  # Append the results to the object
  out <- list(call = call,
              inputs = inputs,
              summary = summary,
              adjustment.method = method,
              adjustment.model = out_model,
              rate = rate,
              adjustment = adjustment,
              rate.adjusted = rate.adjusted)

  class(out) <- "adjust_rate"
  message(glue::glue("adjust_rate: Rate adjustments applied using \"{method}\" method. \nUse print() or summary() on output for more info."))
  return(out)
}


# S3 Generic functions ----------------------------------------------------

#' Print adjust_rate objects
#' @param x adjust_rate object
#' @param pos integer. Which result to print.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
print.adjust_rate <- function(x, pos = 1, ...) {
  cat("\n# print.adjust_rate # -------------------\n")
  if(length(pos) > 1)
    stop("print.adjust_rate: 'pos' must be a single value. To examine multiple results use summary().")
  cat("NOTE: Consider the sign of the adjustment value when adjusting the rate.\n")
  if(pos > length(x$rate.adjusted)) stop("print.adjust_rate: Invalid 'pos' rank: only ", length(x$rate.adjusted), " adjusted rates found.")
  cat("\nAdjustment was applied using the '", x$adjustment.method, "' method.", sep = "")
  cat("\n")
  cat("\nRank", pos, "of", length(x$rate.adjusted), "adjusted rate(s):")
  cat("\nRate          :", x$rate[pos])
  cat("\nAdjustment    :", x$adjustment[pos])
  cat("\nAdjusted Rate :", x$rate.adjusted[pos], "\n")
  cat("\n")
  if(length(x$rate.adjusted) > 1) cat("To see other results use 'pos' input.\n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Summarise adjust_rate objects
#' @param object adjust_rate object
#' @param pos integer(s). Which summary row(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
#' @importFrom data.table data.table
summary.adjust_rate <- function(object, pos = NULL, export = FALSE, ...) {
  if(!is.null(pos) && any(pos > length(object$rate)))
    stop("summary.adjust_rate: Invalid 'pos' rank: only ", length(object$rate.adjusted), " rates found.")

  cat("\n# summary.adjust_rate # -----------------\n")
  cat("\nAdjustment was applied using '", object$adjustment.method, "' method.\n", sep = "")
  if(is.null(pos)) {
    pos <- 1:length(object$rate)
    cat("Summary of all rate results:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of rate results from entered 'pos' rank(s):")
    cat("\n")
    cat("\n")
  }

  out <- object$summary[pos,]

  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' Average adjust_rate object rates
#' @param x adjust_rate object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
mean.adjust_rate <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.adjust_rate # --------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate.adjusted)))
    stop("mean.adjust_rate: Invalid 'pos' rank: only ", length(x$rate.adjusted), " adjusted rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate.adjusted)
    cat("Mean of all adjusted rate results:")
    cat("\n")
  } else{
    cat("Mean of adjusted rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate.adjusted[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate.adjusted[pos])
  out <- mean(x$rate.adjusted[pos])
  cat("Mean of", n, "adjusted rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}

#' Plot adjust_rate objects
#' @param x calc_rate.bg object
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.adjust_rate <- function(x, ...){
  message("adjust_rate: plot() is not available for 'adjust_rate' objects.")
  return(invisible(x))
}


# Internal functions ------------------------------------------------------

#' Get midpoint between two values or within a vector
#' For getting midpoint timestamps from Time data
#' @keywords internal
midpt <- function(p) {
  p1 <- min(p)
  p2 <- max(p)
  return((p1 + p2) / 2)
}


