#' Calculate rate of change in oxygen in flowthrough respirometry
#'
#' Calculates rate of oxygen uptake in flowthrough respirometry given a
#' flow-rate and both inflow (upstream of the chamber) and outflow (downstream
#' of the chamber) oxygen concentrations. Can return a single rate value, or
#' multiple and mean values based on continuous data.
#'
#' Input can be numeric values or vectors - see examples. In addition, a
#' `data.frame` or `inspect` object can be entered. In this case `outflow.o2`
#' and `inflow.o2` are indices indicting the columns containing these data.
#' Note, an `inflow.o2` column *must* be present in the data.frame, even if this
#' is a single fixed value repeated to the correct length. Note, a numeric time
#' elapsed column can be present, but is not required. In flowthrough data,
#' uptake rate calculations use O2 delta and the flow rate.
#'
#' There are no units involved in `calc_rate.ft`. This is a deliberate decision.
#' Units are called in a later function when volumetric and/or mass-specific
#' rates of oxygen use are computed in [convert_rate()] and [convert_DO()].
#'
#' @param x data frame or object of class `inspect`.
#' @param inflow.o2 numeric. Defaults to NULL. This specifies the inflow O2
#'   column if a data frame or `inspect` object is provided. Otherwise, this is
#'   a numeric vector for inflow oxygen concentration.
#' @param outflow.o2 numeric. Defaults to NULL. This specifies the outflow O2
#'   column if a data frame or `inspect` object is provided. Otherwise, this is
#'   a numeric vector for outflow oxygen concentration.
#' @param flowrate numeric vector. The flow rate. Must be in *Litres* per unit
#'   time (s,m,h), for example L/s. The units are not required to be entered in
#'   this function; you will specify them when you perform conversions later in
#'   `convert_rate`.
#' @param plot logical. Defaults to TRUE. Plots the data.
#'
#' @return An object of class "calc_rate.ft".
#' @export
#'
#' @examples
#' # Single numeric values
#' calc_rate.ft(inflow.o2 = 8.88, outflow.o2 = 8.17, flowrate = 0.000039)
#' # Numeric values and vector
#' data("flowthrough.rd")
#' calc_rate.ft(inflow.o2 = 8.88, outflow.o2 = flowthrough.rd$o2.out,
#'   flowrate = 0.000039)
#' # Vectors
#' data("flowthrough.rd")
#' calc_rate.ft(inflow.o2 = flowthrough.rd$o2.in,
#'   outflow.o2 = flowthrough.rd$o2.out, flowrate = 0.000039)
#' # A data frame
#' data("flowthrough.rd")
#' calc_rate.ft(flowthrough.rd, outflow.o2 = 2, inflow.o2 = 3, flowrate = 0.00039)
calc_rate.ft <- function(x = NULL, outflow.o2 = NULL, inflow.o2 = NULL, 
  flowrate = NULL, plot = TRUE) {

  # Validate inputs
  # flowrate required
  if (!is.numeric(flowrate)) stop("numeric 'flowrate' value must be provided.")
  # if df or inspect - both inflow.o2 and outflow.o2 required
  if ((is.data.frame(x) || (any(class(x) %in% "inspect"))) 
      && (is.null(inflow.o2) || is.null(outflow.o2)))
    stop("Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")


# inspect_data ------------------------------------------------------------

  if (any(class(x) %in% "inspect_data")) {

    # Object is of class `inspect_data`. Validate.
    if (length(x$df) < 3)
      stop("Looks like you used the wrong `inspect_data` object here.")

    # Extract data from object. 
    df      <- x$df
    inflow.o2  <- df$inflow
    outflow.o2 <- df$outflow
    message("NOTE: `inspect_data` has been deprecated. Please use `inspect` instead.")
    message("object of class `inspect_data` detected. 
            Any `inflow.o2` or `outflow.o2` inputs entered here ignored.")
    delta <- outflow.o2 - inflow.o2


# inspect -----------------------------------------------------------------

  } else if (any(class(x) %in% "inspect")) {
    
    # Extract data from object.
    df      <- x$dataframe
    inflow.o2  <- df[[inflow.o2]]
    outflow.o2 <- df[[outflow.o2]]
    message("object of class `inspect` detected. Values in `inflow.o2' and 'outflow.o2' are used as 
            column indices to extract data from the data frame.")
    delta <- outflow.o2 - inflow.o2


# data.frame --------------------------------------------------------------

  } else if (is.data.frame(x)) {
    
    # Object is data frame. Extract data from data frame.
    df <- x
    inflow.o2 <- df[[inflow.o2]]
    outflow.o2 <- df[[outflow.o2]]
    message("'data.frame' object detected. Values in 'inflow.o2' and 'outflow.o2' are used as column 
            indices to to extract data from the data frame.")
    delta <- outflow.o2 - inflow.o2


# numeric inputs ----------------------------------------------------------

  } else if (is.null(x) && is.numeric(inflow.o2) &&
      is.numeric(outflow.o2)) {

    # No data frame is provided. 
    df <- data.table::data.table(inflow.o2 = inflow.o2, outflow.o2 = outflow.o2)
    message("calculating from numeric rate values provided in 'inflow.o2' and 'outflow.o2'.")
    delta <- outflow.o2 - inflow.o2

  } else if ((is.null(inflow.o2) && !is.null(outflow.o2)) |
      (!is.null(inflow.o2) && is.null(outflow.o2))) {

    # If inflow.o2 is provided, outflow.o2 should be provided too. And vice versa.
    stop("Both 'inflow.o2' and 'outflow.o2' inputs should have a value.")

  } else stop("Unable to process data. Please double check input arguments.")


# Calculate rate ----------------------------------------------------------

  # Calculate rate
  rate <- delta * flowrate
  mean <- mean(rate)


# Summary and output ------------------------------------------------------

  # Generate summary
  input <- df
  summary <-  data.table::data.table(inflow.o2, outflow.o2, flowrate, rate)

  # Generate output
  out <-
    list(
      input = input,
      flowrate = flowrate,
      summary = summary,
      rate = rate,
      mean = mean)
  class(out) <- "calc_rate.ft"


# Plot --------------------------------------------------------------------

  # Plot
  if (plot) {
    if (length(rate) == 1) NULL else plot(out)
  }

# Return ------------------------------------------------------------------

  return(out)
}


# Print fn ----------------------------------------------------------------

#' @export
print.calc_rate.ft <- function(x, ...) {

  if (length(x$rate) < 6) {
    cat("Rate:\n")
    print(x$rate)
  } else {
    cat("Rate (first 6):\n")
    print(head(x$rate, 6))
  }

  if (length(x$rate) > 1) {
    cat("\nMean:\n")
    print(x$mean)
  }
}


# Summary fn --------------------------------------------------------------

#' @export
summary.calc_rate.ft <- function(object, ...) {
  out <- object$summary
  print(out)
  return(invisible(out))
}


# Plot fn -----------------------------------------------------------------

#' @export
plot.calc_rate.ft <- function(x, ...) {
  plot(x$rate, xlab = "", ylab = "", col = r1, pch = 16, cex = .7,
    panel.first = grid(lwd = .7))
  abline(h = x$mean, lwd = 1.5, lty = 2)
  title(main = "Row Index ~ Rate", line = 0.3)
}

