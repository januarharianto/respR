#' Adjust rates in flowthrough respirometry to account for background
#' respiration or oxygen flux.
#'
#' The `adjust_rate.ft` function adjusts an oxygen uptake or production rate
#' (for example, as determined in [`calc_rate.ft()`]) for background oxygen use
#' by microbial organisms, or other removal or input of oxygen during
#' *flowthrough* respirometry experiments. The function accepts numeric values,
#' as well as `calc_rate.ft` objects. Numeric `x` and `by` inputs should be
#' rates calculated as the **oxygen delta * flowrate**. Units will be specified
#' in [`convert_rate.ft()`] when rates are converted to specific output units.
#'
#' `adjust_rate.ft` allows the rate, or multiple rates, in `x` to be adjusted by
#' the background rate in `by`. There are several ways of determining the
#' background rate, or performing background corrections depending on the setup
#' of the experiment.
#'
#' For experiments in which an empty "blank" experiment has been run, and the
#' background rate generally does not change over the course of the experiment
#' (that is, the oxygen delta between inflow and outflow concentrations remains
#' consistent), it is recommended the rate be determined and saved via the
#' [`inspect.ft()`] and [`calc_rate.ft()`] functions and then entered as the
#' `by` input as either a value or the saved `calc_rate.ft` object. In this
#' case, the `$rate` element of the `calc_rate.ft` object is used to adjust all
#' rates in `x`. If there are multiple background rates in `$rate`, the mean
#' value is used. In this way, a single blank experiment can be applied to
#' several specimen experiments. Alternatively, the rate from several blank
#' experiments can be averaged to provide a single adjustment value, and this
#' entered via `by` as a numeric value.
#'
#' For experiments in which an empty "blank" experiment has been run alongside
#' actual experiments in parallel, and background rate may increase or decrease
#' over time (or there may be other variations for example in the inflow oxygen
#' concentrations), it is recommended you *NOT* use this function. Instead, the
#' paired blank oxygen concentration data should be used in [`inspect.ft`] as
#' the `in.oxy` input. In this way, the calculated specimen delta oxygen values
#' take account of whatever background or other variation in oxygen is occurring
#' in the blank chamber with respect to time. See examples in the vignettes on
#' the website.
#'
#' For adjustments, all rates in `x`, whether entered as values or as a
#' `calc_rate.ft` object, are adjusted by subtracting the mean of all background
#' rates in `by`.
#'
#' **Note:** take special care with the *sign* of the rate used for adjustments.
#' In `respR` oxygen uptake rates are negative, as they represent a negative
#' slope of oxygen against time. Background rates will normally also be a
#' negative value (though not always). See Examples.
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
#' @return **Output**: If the `x` input is a `calc_rate.ft` object, the output will be identical
#'   in structure, but of class `adjust_rate.ft` and containing the additional
#'   elements `$adjustment` and `$rate.adjusted`, with these also added to
#'   `$summary` metadata.
#'
#'   If `x` is a numeric value or vector, the output is a `list` object of class
#'   `adjust_rate.ft` containing four elements: a `$summary` table, `$rate`,
#'   `$adjustment`, and `$rate.adjusted`.
#'
#'   For all outputs, the `$rate.adjusted` element will be the one converted
#'   when the object is passed to `convert_rate.ft`.
#'
#' @param x numeric. A single numeric value, numeric vector, or object of class
#'   `calc_rate.ft`. This is the experimental rate value(s) to be adjusted.
#' @param by numeric. A numeric value, numeric vector, or object of class
#'   `calc_rate.ft`. This contains the background rate used to perform the
#'   adjustment to `x`. If the vector or `calc_rate.ft` object contains multiple
#'   rates, they will be averaged to produce a single adjustment value.
#'
#' @export
#'
#' @examples
#' # Note that oxygen uptake rates are negative in respR
#' # since they represent a decrease in dissolved oxygen
#' # and negative slope. Typically both specimen rate and
#' # background rate values are negative.
#'
#' # ----------------------------------------------------
#' # Simple background respiration correction to a single
#' # rate.
#'
#' # Note, 'x' and 'by' should both be rates calculated as
#' # the delta oxygen value, the difference between inflow
#' # and outflow oxygen, multiplied by the flowrate.
#'
#' # This is (-0.98) - (-0.04) = -0.94
#' adjust_rate.ft(x = -0.98, by = -0.04)
#'
#' # ----------------------------------------------------
#' # Mean background adjustment to a single rate.
#' adjust_rate.ft(x = -0.98, by = c(-0.04, -0.05, -0.06))
#'
#' # ----------------------------------------------------
#' # Mean background adjustment to multiple rates.
#' out <- adjust_rate.ft(x = c(-0.98, -0.87, -0.91),
#'                       by = c(-0.04, -0.05, -0.06))
#' summary(out)
#'
#' # ----------------------------------------------------
#' # Adjustment using calc_rate.ft objects
#' # Specimen rate
#' sp_rate <- flowthrough_mult.rd %>%
#'   inspect.ft(time = 1, out.oxy = 2, in.oxy = 6) %>%
#'   calc_rate.ft(from = 30, flowrate = 0.1)
#'
#' # Background rate
#' bg_rate <- flowthrough_mult.rd %>%
#'   inspect.ft(time = 1, out.oxy = 5, in.oxy = 9) %>%
#'   calc_rate.ft(flowrate = 0.1)
#'
#' # Perform adjustment
#' adj_rate <- adjust_rate.ft(sp_rate, by = bg_rate)
#' print(adj_rate)
#' summary(adj_rate)
#' # ----------------------------------------------------

adjust_rate.ft <- function(x, by) {

  ## Save function call for output
  call <- match.call()

  # Validate inputs ---------------------------------------------------------

  if(!is.numeric(x) && class(x) != "calc_rate.ft")
    stop("adjust_rate.ft: 'x' must be numeric or 'calc_rate.ft' object.")

  if(!is.numeric(by) && class(by) != "calc_rate.ft")
    stop("adjust_rate.ft: 'by' must be numeric or 'calc_rate.ft' object.")

  # if both crft objs, check flowrate is equal
  if(class(x) ==  "calc_rate.ft" && class(by) == "calc_rate.ft"){
    flowrate_equal <- x$inputs$flowrate == by$inputs$flowrate
    if(!flowrate_equal)
      stop("adjust_rate.ft: 'x' and by' input rates have been calculated using different 'flowrates'! \nBackground adjustments should be determined at the same flowrate. \nIf you still want to proceed, you can enter the 'by' adjustment as a value.")
  }

  # Extract adjustment ------------------------------------------------------
  if (any(class(by) %in% "calc_rate.ft")) adjustment <-  by$rate else
    adjustment <- by

  # Make single mean value
  if(length(adjustment) > 1) {
    message("adjust_rate.ft: the 'by' input contains multiple background rates. The mean value will be used to perform adjustments.")
    adjustment <- mean(adjustment)
  }

  # Extract rates to be adjusted --------------------------------------------
  if (any(class(x) %in% "calc_rate.ft")) rate <- x$rate else
    rate <- x

  # if multiple rates in x - message that all will be adjusted by same amount in by
  if(length(rate) > 1)
    message("adjust_rate.ft: The 'x' input contains multiple rates. Each will be adjusted by the same value determined via 'by'.")

  # Perform adjustment ------------------------------------------------------
  rate.adjusted <- rate - adjustment

  # Output ------------------------------------------------------------------

  # is adjustment is a single value, make it same length as rates
  # makes for easier printing etc.
  if(length(adjustment) == 1) adjustment <- rep(adjustment, length(rate.adjusted))

  # Append results to input object
  if(class(x) == "calc_rate.ft") {
    out <- list(call = call,
                inputs = list(x = x,
                              by = by,
                              from = x$from,
                              to = x$to,
                              by = x$by,
                              width = x$width,
                              flowrate = x$flowrate),
                dataframe = x$dataframe,
                data = x$data,
                subsets = x$subsets,
                delta.oxy = x$delta.oxy,
                input_type = x$input_type,
                summary = cbind(x$summary,
                                adjustment = adjustment,
                                rate.adjusted = rate.adjusted),
                rate = rate,
                adjustment = adjustment,
                rate.adjusted = rate.adjusted)
    # or make new one
  } else {
    out <- list(call = call,
                inputs = list(x = x,
                              by = by),
                summary = data.table::data.table(rank = 1:length(rate.adjusted),
                                                 rate = rate,
                                                 adjustment = adjustment,
                                                 rate.adjusted = rate.adjusted),
                rate = rate,
                adjustment = adjustment,
                rate.adjusted = rate.adjusted)
  }

  class(out) <- "adjust_rate.ft"

  message(glue::glue("adjust_rate.ft: Rate adjustments applied. Use print() or summary() on output for more info."))
  return(out)
}



# S3 Generics -------------------------------------------------------------

#' Print adjust_rate.ft objects
#' @return Print to console. No returned value.
#' @export
print.adjust_rate.ft <- function(x, pos = 1, ...) {
  if(length(pos) > 1)
    stop("print.adjust_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate.adjusted))
    stop("print.adjust_rate.ft: Invalid 'pos' rank: only ", length(x$rate.adjusted), " adjusted rates found.")
  cat("\n# print.adjust_rate.ft # ----------------\n")
  cat("NOTE: Consider the sign of the adjustment value when adjusting the rate.\n")
  cat("\nRank", pos, "of", length(x$rate.adjusted), "adjusted rate(s):")
  cat("\nRate          :", x$summary$rate[pos])
  cat("\nAdjustment    :", x$summary$adjustment[pos])
  cat("\nAdjusted Rate :", x$summary$rate.adjusted[pos], "\n")
  cat("\n")
  if(length(x$rate) > 1) cat("To see other results use 'pos' input.\n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' Summarise adjust_rate.ft objects
#' @return Print to console. No returned value.
#' @export
#' @importFrom data.table data.table
summary.adjust_rate.ft <- function(object, pos = NULL, export = FALSE, ...) {
  if(!is.null(pos) && any(pos > length(object$rate)))
    stop("summary.adjust_rate.ft: Invalid 'pos' rank: only ", length(object$rate), " rates found.")

  cat("\n# summary.adjust_rate.ft # --------------\n")
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

#' Average adjust_rate.ft rates
#' @return Print to console. No returned value.
#' @export
mean.adjust_rate.ft <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.adjust_rate.ft # -----------------\n")

  if(!is.null(pos) && any(pos > length(x$rate.adjusted)))
    stop("mean.adjust_rate.ft: Invalid 'pos' rank: only ", length(x$rate.adjusted), " adjusted rates found.")
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

#' Plot adjust_rate.ft objects
#' @return A plot. No returned value.
#' @export
plot.adjust_rate.ft <- function(x, ...){
  message("adjust_rate.ft: plot() is not available for 'adjust_rate.ft' objects.")
  return(invisible(x))
}
