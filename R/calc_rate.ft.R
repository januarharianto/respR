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
#' `calc_rate.ft` calculates rates by averaging delta oxygen values across the
#' whole dataset, or from specified subsets of the data. The `flowrate` is then
#' used to convert these average delta values to rates. There are no units
#' involved in `calc_rate.ft`. This is a deliberate decision. The units of
#' oxygen concentration and flowrate will be specified later in
#' [`convert_rate.ft()`] when rates are converted to specific output units.
#'
#' For continuous data recordings, it is recommended a `data.frame` containing
#' the data be prepared via [`inspect.ft()`], and entered as the `x` input.
#'
#' For data not prepared like this, `x` can be a 2-column `data.frame`
#' containing numeric values of outflow (col 1) and inflow (col 2) oxygen
#' concentrations in that order. Alternatively, if `x` is a numeric value or
#' vector it is treated as delta oxygen values (outflow oxygen concentration
#' minus inflow oxygen concentration in the same units). In both these cases,
#' the `from`, `to`, and `by` inputs are are ignored, and all delta oxygen
#' values whether as entered or calculated from the inflow and outflow oxygen
#' columns are converted to rates.
#'
#' ## Specifying regions
#'
#' For calculating rates over specific regions of the data, the `from` and `to`
#' inputs in the `by` units of `"time"` (the default) or `"row"` can be used for
#' [`inspect.ft()`] inputs. All delta oxygen values within this region are
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
#' entire dataset, and returned as a vector of rate values in `$rate`. See
#' [here](https://januarharianto.github.io/respR/articles/flowthrough.html#case-8-rolling-rate)
#' for how this might be used.
#'
#' ## Flowrate
#'
#' In order to convert delta oxygen values to a oxygen uptake or production
#' rate, the `flowrate` input is required. This must be in a volume (L, ml, or
#' ul) per unit time (s,m,h,d), for example in `L/s`. The units are not required
#' to be entered here; they will be specified in `[convert_rate.ft()`] to
#' convert rates to specific units of oxygen uptake or production.
#'
#' ## Plot
#'
#' For rates calculated from `inspect.ft` inputs, a plot is produced (provided
#' `plot = TRUE`) showing the original data timeseries of inflow and outflow
#' oxygen (if present, top plot), oxygen delta values (middle or top plot) with
#' the region specified via the `from` and `to` inputs highlighted in orange,
#' and a close-up of this region with calculated rate value (bottom plot). If
#' multiple rates have been calculated, by default the first is plotted. Others
#' can be plotted by changing the `pos` input, e.g. `plot(object, pos = 2)`.
#'
#' ***Important:*** Since `respR` is primarily used to examine oxygen
#' consumption, the delta oxygen and rate plots are by default plotted on a
#' reverse y-axis. In `respR` oxygen uptake rates are negative since they
#' represent a negative slope of oxygen against time. In these plots the axis is
#' reversed so that higher uptake rates (i.e. more negative rates) will be
#' higher on these plots. If you are interested instead in oxygen production
#' rates, which are positive, the `rate.rev = FALSE` input can be passed in
#' either the `inspect.ft` call, or when using `plot()` on the output object. In
#' this case, the delta and rate values will be plotted numerically, with higher
#' oxygen *production* rates higher on the plot.
#'
#' ## Additional plotting options
#'
#' If the legend or labels obscure part of the plot, they can be suppressed via
#' `legend = FALSE` in either the `inspect.ft` call, or when using `plot()` on
#' the output object. Console output messages can be suppressed using `quiet =
#' TRUE`. Console output messages can be suppressed using `quiet = TRUE`. If
#' axis labels or other text boxes obscure parts of the plot they can be
#' suppressed using `legend = FALSE`. If axis labels (particularly y-axis) are
#' difficult to read, `las = 2` can be passed to make axis labels horizontal,
#' and`oma` (outer margins, default `oma = c(0.4, 1, 1.5, 0.4)`), and `mai`
#' (inner margins, default `mai = c(0.3, 0.15, 0.35, 0.15)`) used to adjust plot
#' margins.
#'
#' ## Background control or "blank" experiments
#'
#' `calc_rate.ft` can also be used to determine background rates from empty
#' control experiments in the same way specimen rates are determined. The saved
#' objects can be used as the `by` input in [`adjust_rate.ft()`]. For
#' experiments in which the specimen data is to be corrected by a
#' concurrently-run control experiment, best option is to use this as the
#' `in.oxy` input in [`inspect.ft()`]. See help file for that function, or the
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
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The summary can
#' be exported as a separate data frame by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all rates, or those specified by the `pos`
#' input. e.g. `mean(x, pos = 1:5)` The mean can be exported as a separate value
#' by passing `export = TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return Output is a `list` object of class `calc_rate.ft` containing input
#'   parameters and data, various summary data, metadata, and the primary output
#'   of interest `$rate`, which can be background adjusted in [`adjust_rate.ft`]
#'   or converted to units in [`convert_rate.ft`]. Note the `$summary` table
#'   contains linear regression coefficients alongside other metadata. These
#'   *should not be confused* with those in other functions such as `calc_rate`
#'   where slopes represent rates and coefficients such as a high r-squared are
#'   important. Here, they represent the stability of the data region, in that
#'   the closer the slope is to zero the less the delta oxygen values, and
#'   therefore rates, in that region vary. These are included to enable possible
#'   future functionality where stable regions may be automatically identified.
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
#' @param ... Allows additional plotting controls to be passed such as `pos`,
#'   `quiet = TRUE`, `legend = FALSE`, and `rate.rev = FALSE`.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' # Single numeric delta oxygen value. The delta oxygen is the difference
#' # between inflow and outflow oxygen.
#' calc_rate.ft(-0.8, flowrate = 1.6)
#'
#' # Numeric vector of multiple delta oxygen values
#' ft_rates <- calc_rate.ft(c(-0.8, -0.88, -0.9, -0.76), flowrate = 1.6)
#' print(ft_rates)
#' summary(ft_rates)
#'
#' # Calculate rate from entire dataset
#' inspect.ft(flowthrough.rd, time = 1, out.oxy = 2, in.oxy = 3, ) %>%
#'   calc_rate.ft(flowrate = 2.34)
#'
#' # Calculate rate from a region based on time
#' inspect.ft(flowthrough.rd, time = 1, out.oxy = 2, in.oxy = 3, ) %>%
#'   calc_rate.ft(flowrate = 2.34, from = 200, to = 400, by = "time")
#'
#' # Calculate rate from multiple regions
#' inspect.ft(flowthrough.rd, time = 1, out.oxy = 2, in.oxy = 3, ) %>%
#'   calc_rate.ft(flowrate = 2.34,
#'                from = c(200, 400, 600),
#'                to = c(300, 500, 700),
#'                by = "row") %>%
#'   summary()
#'
#' # Calculate rate from existing delta oxygen values
#' inspect.ft(flowthrough.rd, time = 1, delta.oxy = 4) %>%
#'   calc_rate.ft(flowrate = 2.34, from = 200, to = 400, by = "time")
#'
#' # Calculate rate from a background recording
#' inspect.ft(flowthrough_mult.rd,
#'            time = 1,
#'            out.oxy = 5,
#'            in.oxy = 9) %>%
#'   calc_rate.ft(flowrate = 0.1, from = 20, to = 40, by = "time") %>%
#'   summary()
#'
#' # Calculate a rolling rate
#' inspect.ft(flowthrough_mult.rd,
#'            time = 1,
#'            out.oxy = 2,
#'            in.oxy = 6) %>%
#'   calc_rate.ft(flowrate = 0.1, width = 500, by = "row") %>%
#'   summary()

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
  } else if(any(class(x) == "inspect")){
    stop("calc_rate.ft: Function does not accept 'inspect' objects. Please process the data via 'inspect.ft' instead.")
  } else {
    stop("calc_rate.ft: 'x' must be an `inspect.ft` object, a numeric value or vector, or 2-column data.frame. See Help.")
  }


  # vector calculation ------------------------------------------------
  if(xtype == "vec") {
    message("calc_rate.ft: Calculating rate from delta oxygen value(s).")

    # other inputs should be NULL
    if(any(sapply(c(from, to, by, width), function(z) !is.null(z))))
      message("calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")

    # rate calc
    delta <- x

    ## elements for output
    dfs <- NULL
    dt <- data.table::data.table(time = NA,
                                 delta.oxy = delta)
    data <- data.frame(delta.oxy = x)

    ## summary for output
    summary <- data.frame(intercept_b0 = NA, slope_b1 = NA, rsq = NA,
                          row = NA, endrow = NA, time = NA, endtime = NA,
                          oxy = NA, endoxy = NA, delta_mean = delta)

    # no need to plot
    if(plot) {
      message("calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
      plot <- FALSE
    }

    # dataframe calculation ------------------------------------------------
  } else if(xtype == "df") {
    message("calc_rate.ft: Calculating rate from outflow (col 1) and inflow (col 2) oxygen values.")

    if(any(sapply(c(from, to, by, width), function(z) !is.null(z))))
      message("calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")

    delta <- x[[1]] - x[[2]]

    ## empty elements for output
    dfs <- NULL
    dt <- data.table::data.table(time = NA,
                                 delta.oxy = delta)
    data <- data.frame(out.oxy = x[[1]],
                       in.oxy = x[[2]])

    ## summary for output
    summary <- data.frame(intercept_b0 = NA, slope_b1 = NA, rsq = NA,
                          row = NA, endrow = NA, time = NA, endtime = NA,
                          oxy = NA, endoxy = NA, delta_mean = delta)

    if(plot) {
      message("calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
      plot <- FALSE
    }


    # inspect.ft input --------------------------------------------------------
  } else if(xtype == "insp") {
    message("calc_rate.ft: Calculating rate from 'inspect.ft' object.")

    data <- x$data

    if(length(data$delta) > 1)
      warning("calc_rate.ft: Multiple columns of delta oxygen data found in input. \n  Rate(s) will be calculated from first column only! \n  To extract rates from other columns, use inspect.ft to save them as separate objects.")

    ## put data in dt
    time <- data$time[[1]]
    delta.oxy <- data$delta.oxy[[1]]
    dt <- data.table::data.table(time = time, delta.oxy = delta.oxy)

    # ranges
    t_range <- range(time, na.rm = TRUE)
    r_range <- range(1:length(time))


    # Validate 'by' -----------------------------------------------------------
    # Apply default
    if(is.null(by)) by <- "time"
    # by.val
    by <- by.val(by, which = c("t", "r"), msg = "calc_rate.ft")


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

    # from should not be higher than highest time
    # to (time) should not be lower than lowest time
    if(by == "time") if(any(sapply(from, function(z) z > t_range[2])))
      stop("calc_rate.ft: Some 'from' time values are higher than the values present in 'x'.")
    if(by == "time") if(any(sapply(to, function(z) z < t_range[1])))
      stop("calc_rate.ft: Some 'to' time values are lower than the values present in 'x'.")
    # from (row) should not be higher than last row
    if(by == "row") if(any(sapply(from, function(z) z > r_range[2])))
      stop("calc_rate.ft: Some 'from' row numbers are beyond the number of rows present in 'x'.")

    # if any are beyond start/end of data assume first/last value
    if(by == "time") if(any(sapply(from, function(z) z < t_range[1])))
      message("calc_rate.ft: Some 'from' time values are lower than the values present in 'x'. The lowest time value will be used instead.")
    if(by == "row") if(any(sapply(from, function(z) z < r_range[1])))
      message("calc_rate.ft: Some 'from' rows are lower than the values present in 'x'. The first row will be used instead.")
    if(by == "time") if(any(sapply(to, function(z) z > t_range[2])))
      message("calc_rate.ft: Some 'to' time values are higher than the values present in 'x'. The highest time value will be used instead.")
    if(by == "row") if(any(sapply(to, function(z) z > r_range[2])))
      message("calc_rate.ft: Some 'to' rows are higher than the values present in 'x'. The last row will be used instead.")

    # - numeric, within correct range of data (integer if by = row)
    # from
    if(by == "time") sapply(from, function(z)
      input.val(z, num = TRUE, int = FALSE,
                msg = "calc_rate.ft: 'from' - "))

    if(by == "row") sapply(from, function(z)
      input.val(z, num = TRUE, int = TRUE,
                msg = "calc_rate.ft: 'from' - "))

    # to
    if(by == "time") sapply(to, function(z)
      input.val(z, num = TRUE, int = FALSE,
                msg = "calc_rate.ft: 'to' - "))

    if(by == "row") sapply(to, function(z)
      input.val(z, num = TRUE, int = TRUE,
                msg = "calc_rate.ft: 'to' - "))
    # Ensure "from" and "to" are same length:
    if (length(from) != length(to)) stop("calc_rate.ft: 'from' and 'to' have unequal lengths.")

    # This breaks conversion of single delta values, so removed for now
    # paired values of "from" and "to" can't be equal:
    # if(any(mapply(function(p,q) p == q,
    #               p = from,
    #               q = to))) stop("calc_rate.ft: some 'from' values are equal to the paired values in 'to'.")

    ## all 'from' should be less than its paired 'to'
    if(any(mapply(function(p,q) p > q,
                  p = from,
                  q = to))) stop("calc_rate.ft: Some 'from' values are greater than the paired values in 'to'.")


    # Rolling width -----------------------------------------------------------
    if(!is.null(width)){
      # width should only be used with by = "row"
      if(by == "time")
        stop("calc_rate.ft: 'width' can only be used with 'by = \"row\"'.")

      # width should be single numeric
      input.val(width, num = TRUE, int = TRUE, req = TRUE,
                max = 1, min = 1, range = c(1, length(unlist(time))),
                msg =  "calc_rate.ft: 'width' -")

      # if width is not null, by and from should be NULL
      if(!is.null(from) || !is.null(to))
        message("calc_rate.ft: A rolling 'width' has been specified, therefore 'from' and 'to' inputs will be ignored.")
      message(glue::glue("calc_rate.ft: Rates determined using a rolling 'width' of {width} {by} values."))

      # ## empty elements for output
      # - too much to save every subset in output for rolling width
      dfs <- NULL

      # rolling reg of width
      new_dt <- dt
      names(new_dt) <- c("x", "y")

      ## determine window size from width
      win <- calc_win(new_dt, width, by, "calc_rate.ft")

      summary <- rolling_reg_row(new_dt, width = win)
      # rename slope_b1 to slope, since it's not the rate
      # We might use these coefficients later to find regions of stable rates (i.e. slope ~ 0)
      # so may as well save them
      names(summary)[8] <- "slope_b1"
      # add mean rate (delta) value for each subset using row numbers
      summary$delta_mean <- mapply(function(p,q) mean(delta.oxy[p:q]),
                                   p = summary$row,
                                   q = summary$endrow)
      # reorganise summary table
      summary <- summary[,c(7:9,1:6,10)]

      delta <- summary$delta_mean

    } else {

      # from-to inputs ----------------------------------------------------------
      # for other cases - i.e. not rolling width
      # subset data for each from-to pair
      dfs <- lapply(1:length(from), function(z) truncate_data(dt, from[z], to[z], by))

      indices <- lapply(1:length(dfs), function(z) extract_indices(dt, dfs, z))

      summary <- Reduce(rbind, indices)

      summary <- cbind(t(mapply(function(p,q,r) unlist(time_lm(r, start = p$time, end = q$endtime)),
                                p = indices,
                                q = indices,
                                r = dfs)),
                       summary[,1:6])
      names(summary)[2] <- "slope_b1"

      summary$delta_mean <- mapply(function(p,q) mean(delta.oxy[p:q]),
                                   p = summary$row,
                                   q = summary$endrow)
      delta <- summary$delta_mean

    }
  }

  # Calculate rate ----------------------------------------------------------

  # Calculate rate
  rate <- delta * flowrate

  # Summary and output ------------------------------------------------------

  # save inputs for output
  inputs <- list(x = x, flowrate = flowrate, from = from, to = to,
                 by = by, width = width, plot = plot)

  # Generate output
  out <- list(call = call,
              inputs = inputs,
              dataframe = as.data.frame(dt),
              data = data,
              subsets = dfs,
              delta.oxy = delta,
              input_type = xtype,
              summary = data.table(cbind(rep = NA,
                                         rank = 1:nrow(summary),
                                         summary,
                                         flowrate = flowrate,
                                         rate = rate)),
              rate = rate)

  class(out) <- "calc_rate.ft"

  # Plot --------------------------------------------------------------------
  if (plot) plot(out, quiet = TRUE,  ...)

  # Return ------------------------------------------------------------------

  return(out)
}



# S3 Generics -------------------------------------------------------------

#' Print calc_rate.ft objects
#' @param x calc_rate.ft object
#' @param pos integer. Which result to print.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
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

#' Summarise calc_rate.ft objects
#' @param object calc_rate.ft object
#' @param pos integer(s). Which summary row(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
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
  print(out, nrows = 50, class = FALSE)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' Average calc_rate.ft object rates
#' @param x calc_rate.ft object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
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

#' Plot calc_rate.ft objects
#' @param x calc_rate.bg object
#' @param pos integer. Which result to plot.
#' @param quiet logical. Suppress console output.
#' @param legend logical. Suppress labels and legends.
#' @param rate.rev logical. Control direction of y-axis in rolling rate plot.
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.calc_rate.ft <- function(x, pos = NULL, quiet = FALSE,
                              legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if(x$input_type != "insp")
    stop("calc_rate.ft: Plot only available for 'inspect.ft' inputs.")

  # is it delta only rates?
  delta_only <-
    is.null(x$data$out.oxy) && is.null(x$data$in.oxy)
  # number of rates
  nres <- length(x$rate)

  if (!quiet)
    cat("\n# plot.calc_rate.ft # -------------------\n")

  # validate pos input
  if(is.null(pos)) pos <- 1
  if(length(pos) > 1)
    stop("calc_rate: 'pos' should be a single value.")
  if(pos > nres || pos < 1)
    stop("calc_rate.ft: Invalid 'pos' input: only ", nres, " rates found.")

  if(!quiet && pos == 1 && nres == 1)
    cat(glue::glue("calc_rate.ft: Plotting rate from position {pos} of {nres} ..."), sep="\n")
  if(!quiet && pos == 1 && nres > 1)
    cat(glue::glue("calc_rate.ft: Plotting rate from position {pos} of {nres} ... \nTo plot others use 'pos'"), sep="\n")
  if(!quiet && pos > 1)
    cat(glue::glue('calc_rate.ft: Plotting rate from position {pos} of {nres} ...'), sep="\n")

  # plot layout --------------------------------------------------------------

  ## if only delta oxygen plot, it takes top two thirds,
  ## otherwise 3 plots
  if (delta_only) m <- rbind(c(1,1,1), c(1,1,1), c(2,2,2)) else
    m <- rbind(c(1,1,1), c(2,2,2), c(3,3,3))
  ## set layout
  layout(m)

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = 1,
      cex.main = 1,
      ps = 10)
  par(...)

  # in.oxy - out.oxy plot -----------------------------------------------------
  if(!delta_only)  in.out.p(x,
                            pos = pos,
                            legend = legend,
                            ...)

  # # Delta plot --------------------------------------------------------------
  delta.p(x, delta_only = delta_only, legend = legend, rate.rev = rate.rev, pos = pos, ...)

  # Close up plot -----------------------------------------------------------
  pos.ft.p(x, pos = pos, legend = legend, rate.rev = rate.rev, ...)

  if (!quiet){
    cat("-----------------------------------------\n")
  }

  return(invisible(x))
}

