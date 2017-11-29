#' Calculate flowthrough rate of change in dissolved oxygen
#'
#' Calculates rate of $O_2$ uptake in flowthrough respirometry given a flow-rate
#' and both inflow and outflow oxygen concentrations.
#'
#' Can return a single value, or multiple and mean values based on continuous
#' data.
#'
#' @param x data frame or object of class `inspect_data`. Defaults to NULL. Will
#'   process a data frame if it is provided here.
#' @param time numeric. Defaults to NULL. This selects the time column if
#'   a data frame ('df') is provided. Otherwise, this is a numeric vector for
#'   time data.
#' @param inflow numeric. Defaults to NULL. This selects the incurrent column if
#'   a data frame ('df') is provided. Otherwise, this is a numeric vector for
#'   incurrent oxygen concentration.
#' @param outflow numeric. Defaults to NULL. This selects the outcurrent column
#'   if a data frame ('df') is provided.  Otherwise, this is a numeric vector
#'   for extcurrent oxygen concentration.
#' @param flowrate numeric vector. The flow rate. No unit of measurement is
#'   expected; you will specify it when you perform conversions later on.
#' @param plot logical. Defaults to TRUE. Plots the data.
#'
#' @return An object of class "calc_rate.ft".
#' @export
#'
#' @examples
#' NULL
calc_rate.ft <- function(x = NULL, time = NULL, inflow = NULL, outflow = NULL,
  flowrate = NULL, plot = TRUE) {

  # Validate inputs
  if (!is.numeric(flowrate)) stop("numeric 'flowrate' value must be provided.")
  if (is.data.frame(x) && is.null(inflow) && is.null(outflow))
    stop("You must provide subset locations (row number) for 'inflow' and 'outflow'.")

  # Extract individual data
  if (any(class(x) %in% "inspect_data")) {

    # Object is of class `inspect_data`. Extract data from object.
    df      <- x$df
    time    <- df$time
    inflow  <- df$inflow
    outflow <- df$outflow
    message("object of class `inspect_data` detected.")
    delta <- inflow - outflow

  } else if (is.data.frame(x) && is.null(time)) {

    # Object is data frame. Time is not provided. Extract data from data frame.
    df <- x
    inflow <- df[[inflow]]
    outflow <- df[[outflow]]
    message("'data.frame' object detected. Values in 'inflow' and 'outflow' are used as column indices to to extract data from the data frame.")
    delta <- inflow - outflow

  } else if (is.data.frame(x) && !is.null(time)) {

    # Object is data frame. Time is provided. Extract data from data frame.
    df <- x
    time <- df[[time]]
    inflow <- df[[inflow]]
    outflow <- df[[outflow]]
    message("'data.frame' object detected. Values in 'time', inflow' and 'outflow' are used as column indices to extract data from the data frame.")
    delta <- outflow - inflow

  } else if (is.null(x) && is.null(time) && is.numeric(inflow) &&
      is.numeric(outflow)) {

    # No data frame is provided. Time is not provided.
    df <- data.table::data.table(inflow = inflow, outflow = outflow)
    message("calculating from numeric rate values provided in 'inflow' and 'outflow'.")
    delta <- outflow - inflow

  } else if (is.null(x) && !is.null(time) && is.numeric(inflow) &&
      is.numeric(outflow)) {

    # No data frame is provided. Time is provided. Essentiall same as above.
    df <- data.table::data.table(time = time, inflow = inflow, outflow = outflow)
    message("calculating from numeric rate values provided in 'inflow' and 'outflow'.")
    delta <- outflow - inflow

  } else if ((is.null(inflow) && !is.null(outflow)) |
      (!is.null(inflow) && is.null(outflow))) {

    # If inflow is provided, outflow should be provided too. And vice versa.
    stop("Both 'inflow' and 'outflow' inputs should have a value.")

  } else stop("Unable to process data. Please double check input arguments.")

  # Calculate rate
  rate <- delta * flowrate
  mean <- mean(rate)

  # Generate summary
  input <- df
  summary <-  data.table::data.table(inflow, outflow, flowrate, rate)

  # Generate output
  out <-
    list(
      input = input,
      flowrate = flowrate,
      summary = summary,
      rate = rate,
      mean = mean)
  class(out) <- "calc_rate.ft"

  # Plot
  if (plot) {
    if (length(rate) == 1) NULL else plot(out)
  }

  return(out)
}


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


#' @export
summary.calc_rate.ft <- function(object, ...) {
  out <- object$summary
  print(out)
  return(invisible(out))
}


#' @export
plot.calc_rate.ft <- function(x, ...) {
  plot(x$rate, xlab = "", ylab = "", col = r1, pch = 16,
    panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
  abline(h = x$mean, lwd = 1.5, lty = 2)
  title(main = "Row Index ~ Rate", line = 0.3)
}

