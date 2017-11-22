#' Calculate flowthrough rate of change in dissolved oxygen
#'
#' Calculates rate of O₂ uptake in flowthrough respirometry given either;
#' 
#' 1. A flow-rate and both inflow and outflow oxygen concentrations
#' 2. A flow-rate and an O₂ differential between inflow and outflow concentrations
#' 
#' Can return a single value, or multiple and mean values based on continuous data.
#'
#' @param df data frame.
#' @param in.col numeric. Used only if df is not NULL. Selects the inflow.o2 data
#'   by column number.
#' @param out.col numeric. Used only if df is not NULL. Selects the outflow.o2 data
#'   by column number.
#' @param inflow.o2 numeric. Single value or vector. Incurrent oxygen concentration.
#'   If outflow.o2 is a vector, this must be a single value or a vector of equal length.
#' @param outflow.o2 numeric. Single value or vector. Excurrent oxygen concentration.
#' @param delta.o2 numeric. Single value or vector. An O2 differential between inflow 
#'   and outflow O₂. Used only if inflow.o2 and outflow.o2 are NULL. 
#' @param flowrate numeric vector. The flow rate. No unit of measurement is
#'   expected; you will specify it when you perform conversions later on. 
#'
#' @return An object of class "calc_rate.ft".
#' @export
#'
#' @examples
#'
calc_rate.ft <- function(df = NULL, in.col = 1, out.col = 2, inflow.o2 = NULL,
  outflow.o2 = NULL, delta.o2 = NULL, flowrate = NULL) {

  # Validate inputs
  has.df    <- is.data.frame(df)
  has.in    <- is.numeric(inflow.o2)
  has.out   <- is.numeric(outflow.o2)
  has.delta <- is.numeric(delta.o2)

  if (has.df) {
    delta.o2 <- -(abs(df[[in.col]] - df[[out.col]]))
  } else if (has.in && has.out) {
    delta.o2 <- -(abs(inflow.o2 - outflow.o2))
  } else if (is.numeric(delta.o2)) {
    NULL
  } else stop("Input values cannot be identified. Please check documentation.")

  # Is flowrate numeric?
  if(!is.numeric(flowrate)) stop("The argument 'flowrate' must be numeric.")

  # Calculate rate
  rate <- delta.o2 * flowrate
  out <- list(df = df,
    inflow.o2 = inflow.o2,
    outflow.o2 = outflow.o2,
    delta.o2 = delta.o2,
    flowrate = flowrate,
    rate = rate,
    mean = mean(rate))

  class(out) <- "calc_rate.ft"
  return(out)
}


#' @export
print.calc_rate.ft <- function(x, ...) {

  if (length(x$rate) < 6) {
    cat("\nRate:\n")
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

