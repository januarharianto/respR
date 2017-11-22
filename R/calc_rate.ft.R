#' Calculate flowthrough rate of change in dissolved oxygen
#'
#' I need a description here.
#'
#' @param df data frame.
#' @param in.col numeric. Used only if df is not NULL. Selects the inflow data
#'   by column number.
#' @param out.col numeric. Used only if df is not NULL. Selects the outflow data
#'   by column number.
#' @param inflow numeric vector. Incurrent oxygen concentration.
#' @param outflow numeric vector. Excurrent oxygen concentration.
#' @param delta numeric vector. The O2 differential between inflow and outflow.
#' @param flowrate numeric vector. The flow rate. No unit of measurement is
#'   expected, although you will specify it when you perform conversions later
#'   on.
#'
#' @return An object of class "calc_rate.ft".
#' @export
#'
#' @examples
#'
calc_rate.ft <- function(df = NULL, in.col = 1, out.col = 2, inflow = NULL,
  outflow = NULL, delta = NULL, flowrate = NULL) {

  # Validate inputs
  has.df    <- is.data.frame(df)
  has.in    <- is.numeric(inflow)
  has.out   <- is.numeric(outflow)
  has.delta <- is.numeric(delta)

  if (has.df) {
    delta <- -(abs(df[[in.col]] - df[[out.col]]))
  } else if (has.in && has.out) {
    delta <- -(abs(inflow - outflow))
  } else if (is.numeric(delta)) {
    NULL
  } else stop("Input values cannot be identified. Please check documentation.")

  # Is flowrate numeric?
  if(!is.numeric(flowrate)) stop("The argument 'flowrate' must be numeric.")

  # Calculate rate
  rate <- delta * flowrate
  out <- list(df = df,
    inflow = inflow,
    outflow = outflow,
    delta = delta,
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
  if (length(object$delta) < 6) {
    cat("O2 diff.:\n")
    print(object$delta)
  } else {
    cat("\nO2.diff. (first 6):\n")
    print(head(object$delta, 6))
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

