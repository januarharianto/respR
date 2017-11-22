#' Calculate flowthrough rate of change in dissolved oxygen
#'
#' Calculates rate of O₂ uptake in flowthrough respirometry given either:
#'
#' 1. A flow-rate and both inflow and outflow oxygen concentrations 2. A
#' flow-rate and an O₂ differential between inflow and outflow concentrations
#'
#' Can return a single value, or multiple and mean values based on continuous
#' data.
#'
#' @param df data frame. Defaults to NULL. Will process a data frame if it is
#'   provided here.
#' @param from numeric. Defaults to NULL. Defines the upper bound of the data
#'   frame to subset. Subsetting is based on the argument: `by`.
#' @param to numeric. Defaults to NULL. Defines the lower bound of the data
#'   frame to subset. Subsetting is based on the argument: `by`.
#' @param by character. Defaults to "row".
#' @param inflow.col numeric. Defaults to NULL. This selects the incurrent
#'   column if a data frame ('df') is provided.
#' @param outflow.col numeric. Defaults to NULL. This selects the outcurrent
#'   column if a data frame ('df') is provided.
#' @param inflow.o2 numeric. Single value or vector. Incurrent oxygen
#'   concentration. If outflow.o2 is a vector, this must be a single value or a
#'   vector of equal length.
#' @param outflow.o2 numeric. Single value or vector. Excurrent oxygen
#'   concentration.
#' @param delta.o2 numeric. Single value or vector. An O2 differential between
#'   inflow and outflow O₂. Used only if inflow.o2 and outflow.o2 are NULL.
#' @param flowrate numeric vector. The flow rate. No unit of measurement is
#'   expected; you will specify it when you perform conversions later on.
#' @param plot logical. Defaults to TRUE. Plots the data.
#'
#' @return An object of class "calc_rate.ft".
#' @export
#'
#' @examples
#' NULL
calc_rate.ft <- function(df = NULL, from = NULL, to = NULL, by = "row",
  inflow.col = NULL, outflow.col = NULL, inflow.o2 = NULL, outflow.o2 = NULL,
  delta.o2 = NULL, flowrate = NULL, plot = TRUE) {

  # Validate inputs
  has.df  <- is.data.frame(df)
  has.incol <- is.numeric(inflow.col)
  has.outcol <- is.numeric(outflow.col)
  has.in  <- is.numeric(inflow.o2)
  has.out <- is.numeric(outflow.o2)
  has.delta   <- is.numeric(delta.o2)

  has.from <- is.numeric(from)
  has.to   <- is.numeric(to)

  # Is flowrate numeric?
  if(!is.numeric(flowrate)) stop("The argument 'flowrate' must be numeric.")

  # Subset data if necessary:

  # Check that subset is only by time or row since the others don't make sense.
  if (!(by %in% c("row", "time")))
    stop("You can only subset by 'time' or 'row.")

  # Check that if by time, the first column contains sequential time data.
  if (has.df && has.from && has.to && by == "time") {
    if (test_seq(df[[1]])$check)
      stop("Is the first column of the data frame sequential time data?")
  }

  # Ok, good to subset, make sure 'from' and 'to' arguments are supplied.
  if (has.df && has.from && has.to) {
    df <- subset_data(df, from, to, by)
  }

  # Extrat O2 differential ('o2.delta') if it is not provided.
  # Nested choices (first come, first serve)
  if (has.df && has.incol && has.outcol) {
    delta.o2 <- -abs(df[[inflow.col]] - df[[outflow.col]])
  } else if (has.df && has.incol && has.out) {
    delta.o2 <- -abs(df[[inflow.col]] - outflow.o2)
  } else if (has.df && has.outcol && has.in) {
    delta.o2 <- -abs(inflow.o2 - df[[outflow.col]])
  } else if (has.in && has.out) {
    delta.o2 <- -abs(inflow.o2 - outflow.o2)
  } else if (has.delta) {
    NULL # this just checks that delta is numeric and passes it on
  } else stop("Input values cannot be identified. Please check documentation.")

  # Calculate rate
  rate <- delta.o2 * flowrate

  # Generate output
  out <- list(df = df,
    inflow.o2 = inflow.o2,
    outflow.o2 = outflow.o2,
    delta.o2 = delta.o2,
    flowrate = flowrate,
    rate = rate,
    mean = mean(rate))
  class(out) <- "calc_rate.ft"
  if(plot) plot(out)
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
  if (length(object$delta.o2) < 6) {
    cat("O2 diff.:\n")
    print(object$delta.o2)
  } else {
    cat("\nO2.diff. (first 6):\n")
    print(head(object$delta.o2, 6))
  }

  if (length(object$rate) < 6) {
    cat("Rate:\n")
    print(object$rate)
  } else {
    cat("\nRate (first 6):\n")
    print(head(object$rate, 6))
  }

  if (length(object$rate) > 1) {
    cat("\nMean:\n")
    print(object$mean)
  }

  out <- object$rate
  return(invisible(out))
}


#' @export
plot.calc_rate.ft <- function(x, ...) {
  if (length(x$rate) == 1) cat("Only 1 data point available.")
  plot(x$rate, xlab = "", ylab = "", col = r1, pch = 16,
    panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
  abline(h = x$mean, lwd = 1.5, lty = 2)
  title(main = "Row Index ~ Rate", line = 0.3)
}

