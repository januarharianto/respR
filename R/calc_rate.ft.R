#' Calculate rate of change in oxygen from flowthrough respirometry data
#'
#' Calculates rate of oxygen uptake or production in flowthrough respirometry
#' data given a `flowrate` and delta oxygen values, which can either be directly
#' entered, or be calculated from inflow and outflow oxygen. The function
#' returns a single rate value from the whole dataset or a subset of it, by
#' averaging delta oxygen values. Alternatively, multiple rate values can be
#' returned from different regions of continuous data, or a rolling rate of a
#' specific window size performed across the whole dataset.
#'
#' The function calculates rates by averaging the delta oxygen values across the
#' dataset, or from specified subsets of the data, and then using the `flowrate`
#' to convert these to rates. There are no units involved in `calc_rate.ft`.
#' This is a deliberate decision. The units of oxygen concentration and flowrate
#' will be specified later in [`convert_rate.ft()`] when rates are converted to
#' specific output units.
#'
#' For continuous data recordings, it is recommended a `data.frame` containing
#' the data be prepared via [`inspect.ft()`], and entered as the `x` input.
#'
#' For data not prepared like this, `x` can be a 2-column `data.frame`
#' containing numeric values of outflow (col 1) and inflow (col 2) oxygen
#' concentrations. Alternatively, `x` can be a numeric value or vector
#' representing delta oxygen values (outflow oxygen concentration minus inflow
#' oxygen concentration in the same units). In these cases, the `from`, `to`,
#' and `by` inputs are unnecessary and are ignored.
#'
#' ## Specifying regions
#'
#' For calculating rates over specific regions of the data, the `from` and `to`
#' inputs in the `by` units of `"time"` (the default) or `"row"`) can be used
#' for `inspect.ft()` inputs. All delta oxygen values within this region are
#' converted to rates, and averaged to produce a overall rate for the region
#' (`$rate` in the output). Multiple regions can be examined within the same
#' dataset by entering `from` and `to` as vectors of paired values to specify
#' different regions. In this case, `$rate` in the output will be a vector of
#' multiple rates with each result corresponding to the position of the paired
#' `from` and `to` inputs. If `from` and `to` are `NULL` (the default), the rate
#' is determined over the entire dataset.
#'
#' Alternatively a `width` input can be specified, in which case a rolling rate
#' is calculated using this window size (in the relevant `by` units) across the
#' entire dataset, and returned as a vector of rate values in `$rate`.
#'
#' ## Flowrate
#'
#' In order to convert delta oxygen values to a oxygen uptake or production
#' rate, the `flowrate` input is required. This must be in a volume (L, ml, or
#' ul) per unit time (s,m,h,d), for example in `L/s`. The units are not required
#' to be entered in here; you will specify them in [`convert_rate.ft()`] to
#' convert rates to specific units of oxygen uptake or production.
#'
#' ## Plot
#'
#' For rates calculated from `inspect.ft` inputs, a plot is produced (provided
#' `plot = TRUE`) showing the original data timeseries of inflow and outflow
#' oxygen (if present, top plot), oxygen delta values (middle or top plot) with
#' the region specified via the `from` and `to` inputs highlighted, and a
#' close-up of this region with calculated rate value (bottom plot). If multiple
#' rates have been calculated, by default the first is plotted. Others can be
#' plotted by changing the `pos` argument, e.g. `plot(object, pos = 2)`.
#'
#' ***Important:*** Since `respR` is primarily used to examine oxygen
#' consumption, the delta oxygen and rate plots are by default plotted on a
#' reverse y-axis. In `respR` oxygen uptake rates are negative since they
#' represent a negative slope of oxygen against time. In these plots the axis is
#' reversed so that higher uptake rates (i.e. more negative rates) will be
#' higher on these plots. If you are interested instead in oxygen production
#' rates, which are positive, the `rate.rev = FALSE` argument can be passed in
#' either the `inspect.ft` call, or when using `plot()` on the output object. In
#' this case, the delta and rate values will be plotted numerically, with higher
#' oxygen *production* rates higher on the plot.
#'
#' If the legend or labels obscure part of the plot, they can be suppressed via
#' `legend = FALSE` in either the `inspect.ft` call, or when using `plot()` on
#' the output object. Console output messages can be suppressed using `message =
#' FALSE`.
#'
#' ## Background control or "blank" experiments
#'
#' `calc_rate.ft` can also be used to determine background rates from empty
#' control experiments in the same way as specimen rates are determined. The
#' saved objects can be used as the `by` input in [`adjust_rate.ft`]. For
#' experiments in which the specimen data is to be corrected by a
#' concurrently-run control experiment, best option is to use this as the
#' `in.o2` input in [`inspect.ft()`]. See help file for that function, or the
#' vignettes on the website for examples.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints a single result, by default the first rate. Others can be
#' printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all results and metadata, or those
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The output can be
#' saved as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all rates, or those specified by the `pos`
#' input. e.g. `mean(x, pos = 1:5)` The output can be saved as a separate value
#' by passing `export = TRUE`.
#'
#' ## Output
#'
#' Output is a `list` object of class `calc_rate.ft` containing input parameters
#' and data, various summary data, metadata, and the primary output of interest
#' `$rate`, which can be background adjusted in [`adjust_rate.ft`] or converted
#' to units in [`convert_rate.ft`]. Note the `$summary` table contains linear
#' regression coefficients alongside other metadata. These *should not be
#' confused* with those in other functions such as `calc_rate` where slopes
#' represent rates and coefficients such as a high r-squared are important.
#' Here, they represent the stability of the data region, in that the closer the
#' slope is to zero, and lower the r-squared, the less the delta oxygen values
#' in that region vary. They are included to enable possible future
#' functionality where stable regions may be automatically identified, and
#' should generally be ignored.
#'
#' @param x numeric value or vector of delta oxygen values, a 2-column
#'   `data.frame` of outflow (col 1) and inflow (col 2) oxygen values, or an
#'   object of class `inspect.ft`.
#' @param flowrate numeric value. The flow rate through the respirometer in
#'   volume (ul,ml,L) per unit time (s,m,h,d). The units are not necessary here,
#'   but will be specified in [`convert_rate.ft`].
#' @param from numeric value or vector. Defaults to `NULL`. The start of the
#'   region(s) over which you want to calculate the rate in either time or row
#'   units. If a vector, each value must have a paired value in `to`. For use
#'   with `inspect.ft` inputs only.
#' @param to numeric value or vector. Defaults to `NULL`. The end of the
#'   region(s) over which you want to calculate the rate in either time or row
#'   units. If a vector, each value must have a paired value in `from`. For use
#'   with `inspect.ft` inputs only.
#' @param by `"time"` or `"row"`. Defaults to `"time"`. Specifies the units of
#'   the `from` and `by`, or `width` value. For use with `inspect.ft` inputs
#'   only.
#' @param width numeric. Calculates a rolling rate across the whole dataset of
#'   the specified width in the units specified in `by`. For use with
#'   `inspect.ft` inputs only.
#' @param plot logical. Defaults to TRUE. Plots the data.
#' @param ... Allows additional plotting controls to be passed, such as `legend
#'   = FALSE`, `rate.rev = FALSE`, and `pos`.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' # Single numeric value
#'
#'
#' # Numeric vector of two values
#'
#'
#' # inspect.ft object
#' #single delta
#'
#' #multiple from/to
#'
#' #width
#'

calc_rate.ft <- function(x = NULL, flowrate = NULL, from = NULL, to = NULL,
                         by = NULL, width = NULL, plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()

  # Input checks ------------------------------------------------------------

  # flowrate
  # - required, single value, must be numeric
  input.val(flowrate, num = TRUE, int = FALSE, req = TRUE,
                      max = 1, min = 1, range = c(-Inf,Inf),
                      msg = "calc_rate.ft: 'flowrate'")

  # classify x
  if(is.numeric(x)){
    xtype <- "vec"
  } else if(is.data.frame(x) && ncol(x) == 2){
    xtype <- "df"
  } else if(any(class(x) == "inspect.ft")){
    xtype <- "insp"
  } else if(any(class(x) == "inspect_data")){ #DEPRECATED - remove?
    stop("calc_rate.ft: function does not accept 'inspect_data' objects. Please process the data via 'inspect.ft' instead.")
  } else if(any(class(x) == "inspect")){
    stop("calc_rate.ft: function does not accept 'inspect' objects. Please process the data via 'inspect.ft' instead.")
  } else {
    stop("calc_rate.ft: 'x' must be an `inspect.ft` object, a numeric value or vector, or 2-column data.frame. See Help.")
  }


  # vector calculation ------------------------------------------------
  if(xtype == "vec") {
    message("calc_rate.ft: calculating rate from delta oxygen value(s).")

    # other inputs should be NULL
    if(any(sapply(c(from, to, by, width), function(z) !is.null(z))))
      message("calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")

    # rate calc
    delta <- x

    ## elements for output
    dfs <- NULL
    dt <- data.table::data.table(time = NA,
                                 delta.o2 = delta)
    data <- data.frame(delta.o2 = x)

    ## summary for output
    summary <- data.frame(out.o2 = NA, in.o2 = NA, delta.o2 = delta)

    # no need to plot
    if(plot) {
      message("calc_rate.ft: plot only available for 'inspect.ft' inputs.")
      plot <- FALSE
    }

    # dataframe calculation ------------------------------------------------
  } else if(xtype == "df") {
    message("calc_rate.ft: calculating rate from outflow (col 1) and inflow (col 2) oxygen values.")

    if(any(sapply(c(from, to, by, width), function(z) !is.null(z))))
      message("calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")

    delta <- x[[1]] - x[[2]]

    ## empty elements for output
    dfs <- NULL
    dt <- data.table::data.table(time = NA,
                                 delta.o2 = delta)
    data <- data.frame(out.o2 = x[[1]],
                             in.o2 = x[[2]])

    ## summary for output
    summary <- data.frame(out.o2 = x[[1]], in.o2 = x[[2]], delta.o2 = delta)

    if(plot) {
      message("calc_rate.ft: plot only available for 'inspect.ft' inputs.")
      plot <- FALSE
    }


    # inspect.ft input --------------------------------------------------------
  } else if(xtype == "insp") {
    message("calc_rate.ft: calculating rate from 'inspect.ft' object.")

    data <- x$data

    if(length(data$delta) > 1)
      warning("calc_rate.ft: Multiple columns of delta O2 data found in input. \n  Rate(s) will be calculated from first column only! \n  To extract rates from other columns, use inspect.ft to save them as separate objects.")

    ## put data in dt
    time <- data$time[[1]]
    delta.o2 <- data$delta.o2[[1]]
    dt <- data.table::data.table(time = time, delta.o2 = delta.o2)

    # ranges
    t_range <- range(time, na.rm = TRUE)
    r_range <- range(1:length(time))


    # Validate 'by' -----------------------------------------------------------
    # Apply default
    if(is.null(by)) by <- "time"
    # verify_by
    by <- verify_by(by, which = c("t", "r"), msg = "calc_rate.ft:")


    # if 'from' and 'to' NULL assume all data ---------------------------------
    if(by == "time" && is.null(width)){
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
    if(by == "row" && is.null(width)){
      if(is.null(from) && is.null(to) && is.null(width)) {
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

    # Validate 'from' and 'to' ------------------------------------------------
    # - numeric, within correct range of data (integer if by = row)
    # from
    if(by == "time") sapply(from, function(z)
      input.val(z, num = TRUE, int = FALSE,
                          range = t_range,
                          msg = "calc_rate.ft: 'from' - "))

    if(by == "row") sapply(from, function(z)
      input.val(z, num = TRUE, int = TRUE,
                          range = r_range,
                          msg = "calc_rate.ft: 'from' - "))

    # to
    if(by == "time") sapply(to, function(z)
      input.val(z, num = TRUE, int = FALSE,
                          range = t_range,
                          msg = "calc_rate.ft: 'to' - "))

    if(by == "row") sapply(to, function(z)
      input.val(z, num = TRUE, int = TRUE,
                          range = r_range,
                          msg = "calc_rate.ft: 'to' - "))
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


    # Rolling width -----------------------------------------------------------
    if(!is.null(width)){
      # width should be single numeric
      input.val(width, num = TRUE, int = FALSE, req = TRUE,
                          max = 1, min = 1, range = c(-Inf, Inf),
                          msg =  "calc_rate.ft: 'width'")

      # if width is not null, by and from should be NULL
      if(!is.null(from) || !is.null(to))
        message("calc_rate.ft: a rolling 'width' has been specified, therefore 'from' and 'to' inputs will be ignored.")
      message(glue::glue("calc_rate.ft: rates determined using a rolling 'width' of {width} {by} values."))

      # ## empty elements for output
      # - too much to save every subset in output for rolling width
      dfs <- NULL

      # rolling reg of width
      new_dt <- dt
      names(new_dt) <- c("x", "y")

      ## determine window size from width
      win <- calc_window(new_dt, width, by)

      summary <- rolling_reg(new_dt, by = by, width = win, method = "linear")$roll
      # rename rate_b1 to slope, since it's not rate yet
      # We might use these coefficients later to find regions of stable rates (i.e. slope ~ 0)
      # so may as well save them
      names(summary)[2] <- "slope_b1"
      # add mean rate (delta) value for each subset using row numbers
      summary$delta_mean <- mapply(function(p,q) mean(delta.o2[p:q]),
                                   p = summary$row,
                                   q = summary$endrow)
      delta <- summary$delta_mean

    } else {

      # from-to inputs ----------------------------------------------------------
      # for other cases - i.e. not rolling width
      # subset data for each from-to pair
      dfs <- lapply(1:length(from), function(z) truncate_data(dt, from[z], to[z], by))

      indices <- lapply(1:length(dfs), function(z) respR::extract_indices(dt, dfs, z))

      summary <- Reduce(rbind, indices)

      summary <- cbind(t(mapply(function(p,q,r) unlist(time_lm(r, start = p$time, end = q$endtime)),
                                p = indices,
                                q = indices,
                                r = dfs)),
                       summary[,1:4])
      names(summary)[2] <- "slope_b1"

      summary$delta_mean <- mapply(function(p,q) mean(delta.o2[p:q]),
                                   p = summary$row,
                                   q = summary$endrow)
      delta <- summary$delta_mean

    }
  }

  # Calculate rate ----------------------------------------------------------

  # Calculate rate
  rate <- delta * flowrate

  # Summary and output ------------------------------------------------------

  # Generate output
  out <-
    list(
      call = call,
      dataframe = as.data.frame(dt),
      data = data,
      input_type = xtype,
      subsets = dfs,
      summary = data.table(cbind(rank = 1:nrow(summary),
        summary, flowrate = flowrate, rate = rate)),
      from = from,
      to = to,
      by = by,
      width = width,
      flowrate = flowrate,
      delta.o2 = delta,
      rate = rate)
  class(out) <- "calc_rate.ft"


  # Plot --------------------------------------------------------------------
  if (plot) plot(out, ...)

  # Return ------------------------------------------------------------------

  return(out)
}



# S3 Generics -------------------------------------------------------------

#' @export
print.calc_rate.ft <- function(x, pos = 1, ...) {
  cat("\n# print.calc_rate.ft # ------------------")
  if(length(pos) > 1)
    stop("print.calc_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate))
    stop("print.calc_rate.ft: Invalid 'pos' rank: only ", length(x$rate), " rates found.")
  cat("\nRank", pos, "of", length(x$rate), "rates:")
  cat("\nRate:", x$rate[pos], "\n")
  cat("\n")
  if(length(x$rate) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")

  return(invisible(x))
}

#' @export
#' @importFrom data.table data.table
summary.calc_rate.ft <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate)))
    stop("summary.calc_rate.ft: Invalid 'pos' rank: only ", length(object$rate), " rates found.")

  cat("\n# summary.calc_rate.ft # ----------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
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

#' @export
mean.calc_rate.ft <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.calc_rate.ft # -------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate)))
    stop("mean.calc_rate.ft: Invalid 'pos' rank: only ", length(x$rate), " rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate)
    cat("Mean of all rate results:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate[pos])
  out <- mean(x$rate[pos])
  cat("Mean of", n, "output rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}

#' @export
plot.calc_rate.ft <- function(x, pos = NULL, message = TRUE,
                              legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if(x$input_type != "insp")
    stop("calc_rate.ft: plot only available for 'inspect.ft' inputs.")

  if (message)
    cat("\n# plot.calc_rate.ft # -------------------\n")

  # extract data
  # only first columns
  time <- unlist(x$data$time)
  out.o2 <- x$data$out.o2[[1]]
  in.o2 <- x$data$in.o2[[1]]
  del.o2 <- x$data$delta.o2[[1]]
  nres <- length(x$rate) # number of rates
  # is it delta only rates?
  delta_only <-
    is.null(x$data$out.o2) && is.null(x$data$in.o2)
  if(!(delta_only)) y_range <- range(in.o2, out.o2, na.rm = TRUE) # for plotting rate region rectangle
  rate_mean <- signif(x$rate, digits = 5) # rounded mean rate for inclusion in plot

  # validate pos input
  if(is.null(pos)) pos <- 1
  if(length(pos) > 1)
    stop("calc_rate: 'pos' should be a single value.")
  if(pos > nres || pos < 1)
    stop("calc_rate.ft: Invalid 'pos' input: only ", nres, " rates found.")

  pos_rate <- signif(x$rate[pos], digits = 5) # rate for this pos
  pos_from_row <- x$summary$row[pos] # for subsetting rate region
  pos_to_row <- x$summary$endrow[pos] # for subsetting rate region
  pos_from_time <- x$summary$row[pos] # # for plotting rate subset region
  pos_to_time <- x$summary$endrow[pos] # # for plotting rate subset region

  pos_x_data <- time[pos_from_row:pos_to_row]
  pos_y_data_delta <- del.o2[pos_from_row:pos_to_row]

  if(message && pos == 1 && nres == 1)
    cat(glue::glue("calc_rate.ft: Plotting rate from position {pos} of {nres} ..."), sep="\n")
  if(message && pos == 1 && nres > 1)
    cat(glue::glue("calc_rate.ft: Plotting rate from position {pos} of {nres} ... \nTo plot others use 'pos'"), sep="\n")
  if(message && pos > 1)
    cat(glue::glue('calc_rate.ft: Plotting rate from position {pos} of {nres} ...'), sep="\n")

  # delta only --------------------------------------------------------------

  ## if only delta o2 plot, it takes top two thirds,
  ## otherwise 3 plots
  if (delta_only) m <- rbind(c(1,1,1), c(1,1,1), c(2,2,2)) else
    m <- rbind(c(1,1,1), c(2,2,2), c(3,3,3))
  ## set layout
  layout(m)

  ## general settings
  ## margins
  bt <- 0
  lf <- 0.5
  tp <- 0.6
  rt <- 0.3
  ## general plot settings
  par(mai = c(bt, lf, tp, rt),
      ps = 10,
      cex = 1,
      cex.main = 1,
      mgp = c(0, 0.5, 0))


  # in.o2 - out.o2 plot -----------------------------------------------------
  if (!delta_only) {

    ## ylim for outflow and inflow plots - plus 10%
    ylim <- range(range(out.o2), range(in.o2), na.rm = TRUE) ## so all on same axes
    buffer <- diff(ylim)*0.1
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    plot(time,
         out.o2,
         xlab = "",
         ylab = "",
         ylim = ylim,
         pch = 16,
         cex = .5,
         axes = FALSE,
         col.lab = "blue",
         col.axis = "blue",
         panel.first = grid())

    axis(side = 2, las = 1, tck = 0)
    points(time,
           in.o2,
           xlab = "",
           ylab = "",
           ylim = ylim,
           pch = 16,
           cex = .5,
           col = "grey")
    # plot this invisibly - to add row index x-axis
    par(new = TRUE)
    plot(
      seq(1, length(time)),
      out.o2,
      xlab = "",
      ylab = "",
      pch = "",
      cex = .5,
      axes = FALSE
    )
    axis(side = 3,
         col.axis = "red",
         tck = -0.02)
    box()

    ## green box for rate region
    abline(v = pos_from_time,
           col = rgb(15/255,245/255,53/255,  alpha = 0.3),
           lty = 1,
           lwd = 3)

    abline(v = pos_to_time,
           col = rgb(15/255,245/255,53/255,  alpha = 0.3),
           lty = 1,
           lwd = 3)

    rect(xleft = pos_from_time,
         ybottom = y_range[1],
         xright = pos_to_time,
         ytop = y_range[2],
         col = rgb(15/255,245/255,53/255,  alpha = 0.15),
         lty = 0)

    if(legend) legend("topright",
                      "Row Index",
                      text.col = "red",
                      bg = "gray90",
                      cex = 0.5)

    if(legend) legend("right",
                      legend=c("Inflow O2", "Outflow O2"),
                      col=c("grey", "black"),
                      pch=16,
                      cex=0.4)

    title(main = "Outflow - Inflow O2", line = 1.8)
  }

  # Delta plot --------------------------------------------------------------

  # if this is the top plot, set margins to have more space for axis
  # otherwise less space needed
  if(delta_only) par(mai = c(0.4, lf, tp, rt)) else
    par(mai = c(0.4, lf, 0.2, rt))

  ## ylim  - plus 10%
  ylim <- range(na.omit(del.o2)) ## so all on same axes
  buffer <- diff(ylim)*0.1
  ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

  if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

  plot(
    time,
    del.o2,
    xlab = "",
    ylab = "",
    ylim = ylim,
    pch = 16,
    cex = .5,
    axes = FALSE,
    panel.first = grid()
  )

  ## If delta only plot add legend and top axis here instead
  if(delta_only){
    axis(side = 3, col.axis = "red", tck = -0.02)

    if(legend) legend("topright",
                      "Row Index",
                      text.col = "red",
                      bg = "gray90",
                      cex = 0.5)
  }

  axis(side = 2, las = 1, tck = 0) # simply to put yaxis lab colour back to black
  axis(side = 1, col.lab = "blue", col.axis = "blue")

  box()

  ## Title
  if(delta_only) title(main = glue::glue("Delta O2"), line = 1.8) else
    title(main = glue::glue("Delta O2"), line = 0.3)

  ## This will have bottom legend regardless
  if(legend) legend("bottomright",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.5)

  ## add coloured points of rate region
  points(pos_y_data_delta ~ pos_x_data, col = "lightgreen",pch = 16,
         cex = .5)
  clip(min(na.omit(pos_x_data)),
       max(na.omit(pos_x_data)),
       min(na.omit(pos_y_data_delta)),
       max(na.omit(pos_y_data_delta)))
  abline(lm(pos_y_data_delta ~ pos_x_data), lwd = 1.2, lty = 3)


  # Close up plot -----------------------------------------------------------

  ## NOTE we switch y axis to rate values for each subset not delta values
  all_rates <- x$dataframe$delta[pos_from_row:pos_to_row] * x$flowrate

  par(mai = c(0.4, lf, 0.2, rt))
  ylim <- range(na.omit(all_rates))
  buffer <- diff(ylim)*0.1
  ylim <- c(ylim[1] - buffer, ylim[2] + buffer)

  if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

  plot(
    pos_x_data,
    all_rates,
    col = "lightgreen",
    xlab = "",
    ylab = "",
    ylim = ylim,
    pch = 16,
    cex = .5,
    axes = FALSE,
    panel.first = grid()
  )

  axis(side = 2, las = 1, tck = 0)
  axis(side = 1,col.lab = "blue",
       col.axis = "blue")

  box()

  if(legend) legend("bottomright",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.5)


  title(main = glue::glue("Close-up of Position {pos} of {nres}: Rate =  {pos_rate}"), line = 0.3)

  ## add lm trendline
  clip(min(na.omit(pos_x_data)),
       max(na.omit(pos_x_data)),
       min(na.omit(all_rates)),
       max(na.omit(all_rates)))
  abline(lm(all_rates ~ pos_x_data), lwd = 1.2, lty = 3)

  if (message){
    cat("-----------------------------------------\n")
  }

  return(invisible(x))
}

