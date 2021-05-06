#' Calculate rate of change in oxygen in flowthrough respirometry
#'
#' Calculates rate of oxygen uptake in flowthrough respirometry given a
#' flowrate and a delta oxygen value. Can return a single rate value, or
#' multiple and mean values based on continuous data.
#'
#' The `x` input can be a single numeric value, a vector of length 2, or an
#' object of class `inspect.ft()`.
#'
#' For continuous data recordings, it is recommended a `data.frame` containing
#' the data be prepared via [`inspect.ft()`], and entered as the `x` input.
#'
#' Alternatively, if `x` is a single value it is assumed this is a delta oxygen
#' value (outflow oxygen concentration minus inflow oxygen concentration in the
#' same units). Alternatively, `x` can be a vector of two numeric values of
#' outflow and inflow oxygen concentrations in that order. In these cases the
#' `from`, `to`, and `by` inputs are unnecessary and ignored.
#'
#' The `from`, `to`, and `by` inputs determine the region of the `x` input over
#' which a rate is determined. All delta oxygen values within this region are
#' averaged to produce a final rate.
#'
#' Alternatively a `width` input can be entered, which along with `by` means a
#' rolling rate of that width is calculated and returned.
#'
#' There are no units involved in `calc_rate.ft`. This is a deliberate decision.
#' Units are called in a later function when absolute and/or mass-specific rates
#' of oxygen use are computed in [convert_rate.ft()].
#'
#' @param x numeric value, numeric vector of two values, or object of class
#'   `inspect.ft`.
#' @param flowrate numeric value. The flow rate. Must be in *Litres* per unit
#'   time (s,m,h) of the original data, for example L/s. The units are not
#'   required to be entered in this function; you will specify them when you
#'   perform conversions later in `convert_rate.ft`.
#' @param from numeric value.
#' @param to numeric value.
#' @param by string. "time" or "row".
#' @param plot logical. Defaults to TRUE. Plots the data.
#'
#' @return An object of class "calc_rate.ft".
#' @export
#'
#' @examples
#' # Single numeric values
#' calc_rate.ft(in.o2 = 8.88, out.o2 = 8.17, flowrate = 0.000039)
#' # Numeric values and vector
#' data("flowthrough.rd")
#' calc_rate.ft(in.o2 = 8.88, out.o2 = flowthrough.rd$o2.out,
#'   flowrate = 0.000039)
#' # Vectors
#' data("flowthrough.rd")
#' calc_rate.ft(in.o2 = flowthrough.rd$o2.in,
#'   out.o2 = flowthrough.rd$o2.out, flowrate = 0.000039)
#' # A data frame
#' data("flowthrough.rd")
#' calc_rate.ft(flowthrough.rd, out.o2 = 2, in.o2 = 3, flowrate = 0.00039)

calc_rate.ft <- function(x = NULL,
                         flowrate = NULL,
                         from = NULL,
                         to = NULL,
                         by = NULL,
                         width = NULL,
                         plot = TRUE) {


  # Input checks ------------------------------------------------------------

  # flowrate
  # - required, single value, must be numeric
  respR:::numeric.val(flowrate, req = TRUE, msg = "calc_rate.ft: 'flowrate'")

  # classify x
  if(is.numeric(x) && is.vector(x) && length(x) == 1){
    xtype <- "val"
  } else if(is.numeric(x) && is.vector(x) && length(x) == 2){
    xtype <- "vec"
  } else if(any(class(x) == "inspect.ft")){
    xtype <- "insp"
  } else if(any(class(x) == "inspect_data")){ #DEPRECATED - remove?
    stop("calc_rate.ft: function does not accept 'inspect_data' objects. Please process the data via 'inspect.ft' instead.")
  } else if(any(class(x) == "inspect")){
    stop("calc_rate.ft: function does not accept 'inspect' objects. Please process the data via 'inspect.ft' instead.")
  } else {
    stop("calc_rate.ft: 'x' must be an `inspect.ft` object, a single value, or a vector of two values. See Help.")
  }


  # single value calculation ------------------------------------------------
  if(xtype == "val") {
    message("calc_rate.ft: calculating rate from single delta oxygen value.")

    # other inputs should be NULL
    if(any(sapply(c(from, to, by, width), function(z) !is.null(z))))
      message("calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")

    # rate calc
    delta <- x

    # no need to plot
    if(plot) {
      message("calc_rate.ft: plot not available for single value rate calculations.")
      plot <- FALSE
    }

    # dual value calculation ------------------------------------------------
  } else if(xtype == "vec") {
    message("calc_rate.ft: calculating rate from outflow and inflow oxygen values.")

    if(any(sapply(c(from, to, by, width), function(z) !is.null(z))))
      message("calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")

    delta <- x[1] - x[2]

    if(plot) {
      message("calc_rate.ft: plot not available for dual value rate calculations.")
      plot <- FALSE
    }


    # inspect.ft input --------------------------------------------------------
  } else if(xtype == "insp") {
    message("calc_rate.ft: calculating rate from 'inspect.ft' object.")
    if(length(x$input_data$delta) > 1)
      warning("calc_rate.ft: multiple columns of delta O2 data found in input. \n  Rate(s) will be calculated from first column only! \n  To extract rates from other columns, use inspect.ft to save them as separate objects.")

    ## put data in dt
    time <- x$input_data$time[[1]]
    delta <- x$input_data$delta.o2[[1]]
    dt <- data.table::data.table(time = time, delta = delta)

    # ranges
    t_range <- range(time)
    r_range <- range(1:length(time))

    # rolling width
    if(!is.null(width)){
      message(glue::glue("calc_rate.ft: rates determined using a rolling 'width' of {width} {by} values."))
      # if width is not null, by and from should be NULL
      if(!is.null(from) || !is.null(to))
        message("calc_rate.ft: a rolling 'width' has been specified, therefore 'from' and 'to' inputs will be ignored.")


      # rolling reg of width
      dt_new <- dt
      data.table::setnames(dt_new, 1:2, c("x", "y"))
      roll_res <- respR:::rolling_reg(dt_new, by = "time", width = 100, method = "linear")
      # rename rate_b1 to slope
      names(roll_res$roll)[2] <- "slope_b1"
      # add mean rate (delta) value for each subset using row numbers
      roll_res$roll$rate <- mapply(function(p,q) mean(delta[p:q]),
             p = roll_res$roll$row,
             q = roll_res$roll$endrow)

    }

    # by
    # Apply default
    if(is.null(by)) by <- "time"
    # verify_by
    by <- respR:::verify_by(by, which = c("t", "r"), msg = "calc_rate.ft:")

    # from
    # - not required, numeric, within correct range of data
    if(by == "time") sapply(from, function(z) respR:::numeric.val(z, req = FALSE,
                                                                  range = t_range,
                                                                  msg = "calc_rate.ft: 'from' - "))

    if(by == "row") sapply(from, function(z) respR:::numeric.val(z, req = FALSE,
                                                                 range = r_range,
                                                                 msg = "calc_rate.ft: 'from' - "))

    # to
    # - not required, numeric, within correct range of data
    if(by == "time") sapply(to, function(z) respR:::numeric.val(z, req = FALSE,
                                                                range = t_range,
                                                                msg = "calc_rate.ft: 'to' - "))

    if(by == "row") sapply(to, function(z) respR:::numeric.val(z, req = FALSE,
                                                               range = r_range,
                                                               msg = "calc_rate.ft: 'to' - "))

    ## if from and to NULL assume all data
    if(by == "time"){
      if(is.null(from) && is.null(to)) {
        message("calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
        from <- t_range[1]
        to <- t_range[2]
      }
      if(is.null(from)) {
        message("calc_rate.ft: 'from' input NULL. Applying default 'from' of first time value.")
        from <- t_range[1]
      }
      if(is.null(to)) {
        message("calc_rate.ft: 'to' input NULL. Applying default 'to' of last time value.")
        to <- t_range[2]
      }
    }
    if(by == "row"){
      if(is.null(from) && is.null(to)) {
        message("calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
        from <- r_range[1]
        to <- r_range[2]
      }
      if(is.null(from)) {
        message("calc_rate.ft: 'from' input NULL. Applying default 'from' of first row.")
        from <- r_range[1]
      }
      if(is.null(to)) {
        message("calc_rate.ft: 'to' input NULL. Applying default 'to' of last row.")
        to <- r_range[2]
      }
    }

    # Ensure "from" and "to" are same length:
    if (length(from) != length(to)) stop("calc_rate.ft: 'from' and 'to' have unequal lengths.")

    # paired values of "from" and "to" can't be equal:
    if(any(mapply(function(p,q) p == q,
                  p = from,
                  q = to))) stop("calc_rate.ft: some 'from' values are equal to the paired values in 'to'.")

    ## all 'from' should be less than its paired 'to'
    if(any(mapply(function(p,q) p > q,
                  p = from,
                  q = to))) stop("calc_rate.ft: some 'from' values are greater than the paired values in 'to'.")




    ## determine starts and ends
    # row <-
    # endrow <-
    # time <-
    # endtime <-


    df <- lapply(1:length(from), function(z) respR:::truncate_data(dt, from[z], to[z], by))
    indices <- lapply(1:length(df), function(z) extract_indices(dt, df, z))

    subset_data(dt)

  }



  # Calculate rate ----------------------------------------------------------

  # Calculate rate
  all.rates <- delta * flowrate
  rate <- mean(all.rates)


  # Summary and output ------------------------------------------------------

  # Generate summary
  input <- x
  summary <-  list(from = from,
                   to = to,
                   by = by,
                   width = width,
                   flowrate = flowrate,
                   rate = rate)

  # Generate output
  out <-
    list(
      call = list(x = x,
                  flowrate = flowrate,
                  from = from,
                  to = to,
                  by = by,
                  width = width,
                  plot = plot),
      flowrate = flowrate,
      delta = delta,
      all.rates = all.rates,
      rate = rate,
      summary = summary)
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
print.calc_rate.ft <- function(x, pos = NULL, ...) {

  if(!is.null(pos) && pos > length(x$rate))
    stop("Invalid 'pos' rank: only ", length(x$rate), " rates found.")

  if(is.null(pos)) {
    if (length(x$rate) <= 20) {
      cat("Rate:\n")
      print(x$rate)
    } else {
      cat("Rate (first 20 shown):\n")
      print(head(x$rate, 20))
    }
  } else {
    cat("Rate:\n")
    print(x$rate[pos])
  }

  if (length(x$rate) > 1) {
    cat("\nMean (of all rates in x):\n")
    print(x$mean)
  }
  return(invisible(x))
}


# Summary fn --------------------------------------------------------------

#' @export
summary.calc_rate.ft <- function(object, export = FALSE, ...) {

  out <- object$summary
  print(out)

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}


# Plot fn -----------------------------------------------------------------

#' @export
plot.calc_rate.ft <- function(x, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  plot(x$rate, xlab = "", ylab = "", col = r1, pch = 16, cex = .7,
       panel.first = grid(lwd = .7))
  abline(h = x$mean, lwd = 1.5, lty = 2)
  legend("Mean (of all rates)", x = "topright", lwd = 1.5,
         lty = 2, bg = "white", adj = 0.2, cex = 0.8, x.intersp = 2)
  title(main = "Row Index ~ Rate", line = 0.3)
  return(invisible(x))
}

