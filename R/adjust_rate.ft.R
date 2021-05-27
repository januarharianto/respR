#' Adjust rates in flowthrough respirometry to account for background
#' respiration or oxygen flux.
#'
#' The `adjust_rate.ft` function adjusts an oxygen uptake or production rate
#' (for example, as determined in [`calc_rate.ft`]) for background oxygen use by
#' microbial organisms, or other removal (or input) of oxygen during
#' *flowthrough* respirometry experiments. The function accepts numeric values,
#' as well as regular `calc_rate.ft` objects.
#'
#' ***Note:*** take special care with the **sign** of the rate used for
#' adjustments. In `respR` oxygen uptake rates are negative values, as they
#' represent a negative slope of oxygen against time. Background rates will
#' normally also be a negative value. See examples.
#'
#' `adjust_rate.ft` allows the rate (or multiple rates) in `x` to be adjusted by
#' the background rate in `by`. There are several ways of determining the
#' background rate, or performing background corrections depending on the setup
#' of the experiment.
#'
#' For experiments in which an empty "blank" experiment has been run, and the
#' background rate generally does not change over the course of the experiment
#' (that is, the oxygen delta between inflow and outflow concentrations remains
#' consistent), it is recommended the rate be determined and saved via the
#' `[inspect.ft]` and `[calc_rate.ft]` functions and then entered as the `by`
#' input as either a value or the saved `calc_rate.ft` object. In this case, the
#' `$rate` element of the `calc_rate.ft` object is used to adjust all rates in
#' `x`. If there are multiple rates in `$rate`, the mean value of all rates
#' found is used. In this way, a single blank experiment can be applied to
#' numerous actual experiments, or several blank experiments can be averaged to
#' provide a single average adjustment rate. Alternatively, once blank
#' experiments have been analysed to produce a background correction value, can
#' be and this entered via `by` as a numeric value.
#'
#' For experiments in which an empty "blank" experiment has been run alongside
#' actual experiments in parallel, and background rate may increase or decrease
#' over time (or there may be other variations such as in the inflow oxygen
#' concentrations), it is recommended you *NOT* use this function. Instead, the
#' paired blank oxygen concentration data should be used in `[inspect.ft]` as
#' the `in.o2` input. In this way, the calculated delta oxygen values take
#' account of whatever background or other variation in oxygen is occurring in
#' the blank chamber with respect to time.
#'
#' For all background operations, it is important the same `flowrate` is used.
#'
#' For adjustments, all rates in `x`, whether entered as values or as a
#' `calc_rate.ft` object, are adjusted by subtracting the mean of all background
#' rates in `by`. Again, take special note of the *sign* of these rates. See
#' examples.
#'
#' ## Output
#'
#' If the `x` input is numeric, the output will be a numeric of the same length
#' containing adjusted rates.
#'
#' If the `x` input is a `calc_rate.ft` object, the output will be identical in
#' structure, but of class `adjust_rate.ft` and containing the additional
#' elements `$adjustment` and `$rate.adjusted` and these also added to summary
#' metadata. If `x` is a value or vector, the output is a list object of class
#' `adjust_rate.ft` containing three elements: `$rate`, `$adjustment`, and
#' `$rate.adjusted`. The `$rate.adjusted` element will be the one converted when
#' the object is passed to `convert_rate.ft`.
#'
#' @md
#'
#' @param x numeric. A single numeric value, numeric vector, or object of class
#'   `calc_rate.ft`. This is the experimental rate value(s) to be adjusted.
#' @param by numeric. A numeric value, numeric vector or object of class
#'   `calc_rate.ft`. This contains the background rate used to perform the
#'   adjustment to `x`. If the vector or `calc_rate.ft` contains multiple rates,
#'   they will be averaged to produce a single adjustment value.
#'
#' @return Output is a list object of class `adjust_rate.ft`
#'
#' @export
#'
#' @examples
#' # Note that oxygen uptake rates are negative by default (since it represents a
#' # decrease in dissolved oxygen and negative slope). Therefore, typically both
#' # rate and background rate values are negative.
#'
#' # Simple background respiration correction to a single rate.
#' # This is (-7.44) - (-0.04) = -7.40
#' adjust_rate.ft(x = -7.44, by = -0.04)
#'
#' # Oxygen input adjustment
#' # This is (-7.44) - (0.1) = -7.54
#' adjust_rate.ft(x = -7.44, by = 0.1)
#'
#' # Mean background respiration correction to a single rate.
#' adjust_rate.ft(x = -7.44, by = c(-0.04, -0.05, -0.06))
#'
#' # Mean background respiration correction to multiple rates.
#' out <- adjust_rate.ft(x = c(-7.44, -7.20, -7.67),
#'                    by = c(-0.04, -0.05, -0.06))
#' summary(out)
#'
#' # Paired background respiration correction to multiple rates.
#' out <- adjust_rate.ft(x = c(-7.44, -7.20, -7.67),
#'                    by = c(-0.04, -0.05, -0.06))
#' summary(out)

adjust_rate.ft <- function(x, by) {

  # Validate inputs ---------------------------------------------------------

  if(!is.numeric(x) && class(x) != "calc_rate.ft")
    stop("adjust_rate.ft: 'x' must be numeric or 'calc_rate.ft' object.")

  if(!is.numeric(by) && class(by) != "calc_rate.ft")
    stop("adjust_rate.ft: 'by' must be numeric or 'calc_rate.ft' object.")

  # if both crft objs, check flowrate is equal
  if(class(x) ==  "calc_rate.ft" && class(by) == "calc_rate.ft"){
    flowrate_equal <- x$flowrate == by$flowrate
    if(!flowrate_equal)
      stop("adjust_rate.ft: 'x' and by' input rates have been calculated using different 'flowrates'! \nBackground adjustments should be determined at the same flowrate. \nIf you still want to proceed, you can enter the 'by' adjustment as a value.")
  }

  # Extract adjustment ------------------------------------------------------
  if (any(class(by) %in% "calc_rate.ft")) adjustment <-  by$rate else
    adjustment <- by

  # Make single mean value
  if(length(adjustment) > 1) {
    warning("adjust_rate.ft: the 'by' input contains multiple rates. The mean value will be used to perform adjustments.")
    adjustment <- mean(adjustment)
  }

  # Extract rates to be adjusted --------------------------------------------
  if (any(class(x) %in% "calc_rate.ft")) rate <- x$rate else
    rate <- x

  # if multiple rates in x - message that all will be adjusted by same amount in by
  if(length(rate) > 1)
    message("adjust_rate.ft: The 'x' input contains multiple rates. Each will be adjusted by the same value in 'by'.")

  # Perform adjustment ------------------------------------------------------
  rate.adjusted <- rate - adjustment

  # Output ------------------------------------------------------------------

  # Make adjustment same length as rates for summary, printing etc.
  adjustment_long <- rep(adjustment, length(rate.adjusted))

  # Append results to input object
  if(class(x) == "calc_rate.ft") {
    out <- list(call = x$call,
                input_data = x$input_data,
                input_type = x$input_type,
                dataframe = x$dataframe,
                subsets = x$subsets,
                summary = cbind(x$summary, adjustment = adjustment_long,
                                rate.adjusted = rate.adjusted),
                from = x$from,
                to = x$to,
                by = x$by,
                width = x$width,
                flowrate = x$flowrate,
                delta.o2 = x$delta.o2,
                rate = rate,
                adjustment = adjustment,
                rate.adjusted = rate.adjusted)
    # or make new one
  } else {
    out <- list(
      summary = data.table::data.table(rate = rate, adjustment = adjustment_long,
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

#' @export
print.adjust_rate.ft <- function(x, pos = 1, ...) {
  if(length(pos) > 1)
    stop("print.adjust_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate.adjusted))
    stop("print.adjust_rate.ft: Invalid 'pos' rank: only ", length(x$rate.adjusted), " adjusted rates found.")
  cat("\n# print.adjust_rate.ft # ----------------\n")
  cat("Note: please consider the sign of the adjustment value when adjusting the rate.\n")
  cat("\nRank", pos, "of", length(x$rate.adjusted), "adjusted rate(s):")
  cat("\nRate          :", x$summary$rate[pos])
  cat("\nAdjustment    :", x$summary$adjustment[pos])
  cat("\nAdjusted Rate :", x$summary$rate.adjusted[pos], "\n")
  cat("\n")
  if(length(x$rate) > 1) cat("To see other results use 'pos' input. ")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")
  return(invisible(x))
}


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

  out <- cbind(rank = pos, object$summary[pos,])

  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}


#' @export
mean.adjust_rate.ft <- function(x, export = FALSE, ...){

  cat("\n# mean.adjust_rate.ft # -----------------\n")
  if(length(x$rate.adjusted) == 1)
    message("Only 1 rate found in input. Returning mean rate anyway...")
  n <- length(x$rate.adjusted)
  out <- mean(x$rate.adjusted)
  cat("Mean of", n, "adjusted rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}


