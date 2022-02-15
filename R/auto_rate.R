#' Automatically determine most linear, highest, lowest and rolling oxygen
#' uptake or production rates
#'
#' `auto_rate` performs rolling regressions on a dataset to determine the *most
#' linear, highest, lowest, maximum, minimum, rolling*, and *interval* rates of
#' change in oxygen against time. A rolling regression of the specified `width`
#' is performed on the entire dataset, then based on the "`method`" input, the
#' resulting regressions are ranked or ordered, and the output summarised.
#'
#' ## Ranking and ordering algorithms
#'
#' Currently, `auto_rate` contains seven ranking and ordering algorithms that
#' can be applied using the `method` input:
#'
#' - `linear`: Uses kernel density estimation (KDE) to learn the shape of the
#' entire dataset and *automatically identify* the most linear regions of the
#' timeseries. This is achieved by using the smoothing bandwidth of the KDE to
#' re-sample the "peaks" in the KDE to determine linear regions of the data. The
#' summary output will contain only the regressions identified as coming from
#' linear regions of the data, ranked by order of the KDE density analysis. This
#' is present in the `$summary` component of the output as `$density`. Under
#' this method, the `width` input is used as a starting seed value, but the
#' resulting regressions may be of any width. See
#' \href{https://github.com/januarharianto/respR}{here} for full details.
#'
#' - `highest`: Every regression of the specified `width` across the entire
#' timeseries is calculated, then ordered using ***absolute*** rate values from
#' highest to lowest. Essentially, this option ignores the sign of the rate, and
#' can only be used when rates all have the same sign. Rates will be ordered
#' from highest to lowest in the `$summary` table regardless of if they are
#' oxygen uptake or oxygen production rates.
#'
#' - `lowest`: Every regression of the specified `width` across the entire
#' timeseries is calculated, then ordered using ***absolute*** rate values from
#' lowest to highest. Essentially, this option ignores the sign of the rate, and
#' can only be used when rates all have the same sign. Rates will be ordered
#' from lowest to highest in the `$summary` table regardless of if they are
#' oxygen uptake or oxygen production rates.
#'
#' - `maximum`: Every regression of the specified `width` across the entire
#' timeseries is calculated, then ordered using ***numerical*** rate values from
#' maximum to minimum. Takes ***full account of the sign of the rate***.
#' Therefore, oxygen uptake rates, which in `respR` are negative, would be
#' ordered from lowest (least negative), to highest (most negative) in the
#' summary table in numerical order. Therefore, generally this method should
#' only be used when rates are a mix of oxygen consumption and production rates,
#' such as when positive rates may result from regressions fit over flush
#' periods in intermittent-flow respirometry. Generally, for most analyses where
#' maximum or minimum rates are of interest the `"highest"` or `"lowest"`
#' methods should be used.
#'
#' - `minimum`: Every regression of the specified `width` across the entire
#' timeseries is calculated, then ordered using ***numerical*** rate values from
#' minimum to maximum. Takes ***full account of the sign of the rate***.
#' Therefore, oxygen uptake rates, which in `respR` are negative, would be
#' ordered from highest (most negative) to lowest (least negative) in the
#' summary table in numerical order. Therefore, generally this method should
#' only be used when rates are a mix of oxygen consumption and production rates,
#' such as when positive rates may result from regressions fit over flush
#' periods in intermittent-flow respirometry. Generally, for most analyses where
#' maximum or minimum rates are of interest the `"highest"` or `"lowest"`
#' methods should be used.
#'
#' - `rolling`: A rolling regression of the specified `width` is performed
#' across the entire timeseries. No reordering of results is performed.
#'
#' - `interval`: multiple, successive, non-overlapping regressions of the
#' specified `width` are extracted from the rolling regressions, ordered by
#' time.
#'
#' - NOTE: `max`, `min`: These methods were used in previous versions of `respR`
#' but have been deprecated. They were intended to order oxygen uptake
#' (negative) rates by magnitude, but this resulted in incorrect ordering of
#' oxygen production (positive) rates. They have been retained for code
#' compatibility, but will be removed in a future version of `respR`, and so
#' *should not be used*.
#'
#' ## Further selection and filtering of results
#'
#' For further selection or subsetting of `auto_rate` results, see the dedicated
#' [`subset_rate()`] function, which allows subsetting of rates by various
#' criteria, including r-squared, data region, percentiles, and more.
#'
#' ## Units
#'
#' There are no units involved in `auto_rate`. This is a deliberate decision.
#' The units of oxygen concentration and time will be specified later in
#' [`convert_rate()`] when rates are converted to specific output units.
#'
#' ## The `width` and `by` inputs
#'
#' If `by = "time"`, the `width` input represents a time window in the units of
#' the time data.
#'
#' If `by = "row"` and between 0 and 1, `width` represents a proportion of the
#' total data length, as in the equation `floor(width * number of data rows)`.
#' For example, 0.2 represents a rolling window of 20% of the data width.
#' Otherwise, if entered as an integer of 2 or greater, the `width` represents
#' the number of rows.
#'
#' For both `by` inputs, if left as `width = NULL` it defaults to 0.2 or a
#' window of 20% of the data length.
#'
#' In most cases, `by` should be left as the default `"row"`, and the `width`
#' chosen with this in mind, as it is considerably more computationally
#' efficient. Changing to `"time"` causes the function to perform checks for
#' irregular time intervals at every iteration of the rolling regression, which
#' adds to computation time. This is to ensure the specified `width` input is
#' honoured in the time units and rates correctly calculated, even if the data
#' is unevenly spaced or has gaps.
#'
#' ## Plot
#'
#' A plot is produced (provided `plot = TRUE`) showing the original data
#' timeseries of oxygen against time (bottom blue axis) and row index (top red
#' axis), with the rate result region highlighted. Second panel is a close-up of
#' the rate region with linear model coefficients. Third panel is a rolling rate
#' plot (note the reversed y-axis so that higher oxygen uptake rates are plotted
#' higher), of a rolling rate of the input `width` across the whole dataset.
#' Each rate is plotted against the middle of the time and row range used to
#' calculate it. The dashed line indicates the value of the current rate result
#' plotted in panels 1 and 2. The fourth and fifth panels are summary plots of
#' fit and residuals, and for the `linear` method the sisth panel the results of
#' the kernel density analysis, with the dashed line again indicating the value
#' of the current rate result plotted in panels 1 and 2.
#'
#' ## Additional plotting options
#'
#' If multiple rates have been calculated, by default the first (`pos = 1`) is
#' plotted. Others can be plotted by changing the `pos` input either in the main
#' function call, or by plotting the output, e.g. `plot(object, pos = 2)`. In
#' addition, each sub-panel can be examined individually by using the `panel`
#' input, e.g. `plot(object, panel = 2)`.
#'
#' Console output messages can be suppressed using `quiet = TRUE`. If axis
#' labels or other text boxes obscure parts of the plot they can be suppressed
#' using `legend = FALSE`. The rate in the rolling rate plot can be plotted
#' *not* reversed by passing `rate.rev = FALSE`, for instance when examining
#' oxygen production rates so that higher production rates appear higher. If
#' axis labels (particularly y-axis) are difficult to read, `las = 2` can be
#' passed to make axis labels horizontal, and `oma` (outer margins, default `oma
#' = c(0.4, 1, 1.5, 0.4)`), and `mai` (inner margins, default `mai = c(0.3,
#' 0.15, 0.35, 0.15)`) used to adjust plot margins.
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
#' @return Output is a `list` object of class `auto_rate` containing input
#'   parameters and data, various summary data, metadata, linear models, and the
#'   primary output of interest `$rate`, which can be background adjusted in
#'   [`adjust_rate`] or converted to units in [`convert_rate`].
#'
#' @param x data frame, or object of class `inspect` containing oxygen~time
#'   data.
#' @param method string. `"linear"`, `"highest"`, `"lowest"`, `"maximum"`,
#'   `"minimum"`, `"rolling"` or `"interval"`. Defaults to `"linear"`. See
#'   Details.
#' @param width numeric. Width of the rolling regression. For `by = "row"`,
#'   either a value between 0 and 1 representing a proportion of the data
#'   length, or an integer of 2 or greater representing an exact number of rows.
#'   If `by = "time"` it represents a time window in the units of the time data.
#'   If `NULL`, it defaults to 0.2 or a window of 20% of the data length. See
#'   Details.
#' @param by string. `"row"` or `"time"`. Defaults to `"row"`. Metric by which
#'   to apply the `width` input if it is above 1.
#' @param plot logical. Defaults to TRUE. Plot the results.
#' @param ... Allows additional plotting controls to be passed, such as `pos`,
#'   `panel`, and `quiet = TRUE`.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Most linear section of an entire dataset
#' inspect(sardine.rd, time = 1, oxygen =2) %>%
#'  auto_rate()
#'
#' # What is the lowest oxygen consumption rate over a 10 minute (600s) period?
#' inspect(sardine.rd, time = 1, oxygen =2) %>%
#'  auto_rate(method = "lowest", width = 600, by = "time") %>%
#'  summary()
#'
#' # What is the highest oxygen consumption rate over a 10 minute (600s) period?
#' inspect(sardine.rd, time = 1, oxygen =2) %>%
#'  auto_rate(method = "highest", width = 600, by = "time") %>%
#'  summary()
#'
#' # What is the NUMERICAL minimum oxygen consumption rate over a 5 minute (300s)
#' # period in intermittent-flow respirometry data?
#' # NOTE: because uptake rates are negative, this would actually be
#' # the HIGHEST uptake rate.
#' auto_rate(intermittent.rd, method = "minimum", width = 600, by = "time") %>%
#'  summary()
#'  }

auto_rate <- function(x, method = "linear", width = NULL,
                      by = "row", plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()
  ## Save inputs for output
  inputs <- list(x = x,
                 method = method,
                 width = width,
                 by = by,
                 plot = plot)

  # perform checks
  checks <- validate_auto_rate(x, by, method)
  dt <- checks$df  # extract df from validation check
  by <- checks$by

  # prepare data
  data.table::setnames(dt, 1:2, c("x", "y")) # rename data columns
  win <- calc_win(dt, width, by, "auto_rate: ")  # determine width

  # verify & apply methods

  # max method --------------------------------------------------------------
  ## OLD METHOD
  ## WE WILL DEPRECATE THIS IN LATER VERSION
  if (method == 'max') {
    warning("auto_rate: the 'min' and 'max' methods have been deprecated, as they resulted in incorrect ordering of oxygen production rates. \n They have been retained for code compatibility, but will be removed in a future version of respR. \n Please use 'highest/lowest' for ordering by absolute rate value, or 'maximum/minimum' for strict numerical ordering of rates. ")
    output <- auto_rate_min(dt, win, by) ## note "wrong" method - but matches old behaviour
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA

    # min method --------------------------------------------------------------
    ## OLD METHOD
    ## WE WILL DEPRECATE THIS IN LATER VERSION
  } else if (method == 'min') {
    warning("auto_rate: the 'min' and 'max' methods have been deprecated, as they resulted in incorrect ordering of oxygen production rates. \n They have been retained for code compatibility, but will be removed in a future version of respR. \n Please use 'highest/lowest' for ordering by absolute rate value, or 'maximum/minimum' for strict numerical ordering of rates. ")
    output <- auto_rate_max(dt, win, by) ## note "wrong" method - but matches old behaviour
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA


    # maximum method ----------------------------------------------------------
    ### NOW ORDERS BY NUMERICAL VALUE I.E. CONSIDERS SIGN
  } else if (method == 'maximum') {
    output <- auto_rate_max(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA

    # minimum method ----------------------------------------------------------
    ### NOW ORDERS BY NUMERICAL VALUE I.E. CONSIDERS SIGN
  } else if (method == 'minimum') {
    output <- auto_rate_min(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA


    # interval method ---------------------------------------------------------
  } else if (method == 'interval') {
    output <- auto_rate_interval(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA


    # rolling method ----------------------------------------------------------
  } else if (method == 'rolling') {
    output <- auto_rate_rolling(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA


    # highest method ----------------------------------------------------------
  } else if (method == 'highest') {
    output <- auto_rate_highest(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA


    # lowest method -----------------------------------------------------------
  } else if (method == 'lowest') {
    output <- auto_rate_lowest(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)
    ### Add density to summary table
    ### (empty, but keeps column refs right later)
    out$summary$density <- NA


    # linear method -----------------------------------------------------------
  } else if (method == 'linear') {
    output <- auto_rate_linear(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll),
                           total_peaks = nrow(output$peaks),
                           kde_bw = output$density$bw)
    out <- list(call = call,
                inputs = inputs,
                dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                density = output$density,
                peaks   = output$peaks,
                bandwidth = output$density$bw,
                metadata  = metadata)

    ### Add density to summary table
    out$summary$density <- out$peaks$density

    ## Remove duplicate results
    ### Index of unique rows. This keeps the first (top) row of any duplicates
    ### so lower density ranked ones are removed - which is what we want
    index_unique <- which(!duplicated(out$summary[,1:7]))

    # now remove these from relevant places
    out$summary <- out$summary[index_unique,]
    out$rate <- out$rate[index_unique]
    out$metadata$subset_regs <- length(index_unique)
    out$peaks <- out$peaks[index_unique,]

  } else stop("auto_rate: 'method' input not recognised")

  # Assemble final output object --------------------------------------------

  ## Reorder output summary table
  out$summary <- data.table::data.table(rank = 1:nrow(out$summary),
                                        intercept_b0 = out$summary$intercept_b0,
                                        rate_b1 = out$summary$rate_b1,
                                        rsq = out$summary$rsq,
                                        density = out$summary$density,
                                        row = out$summary$row,
                                        endrow = out$summary$endrow,
                                        time = out$summary$time,
                                        endtime = out$summary$endtime,
                                        oxy = out$summary$oxy,
                                        endoxy = out$summary$endoxy,
                                        rate = out$summary$rate_b1)

  class(out) <- 'auto_rate'

  if (plot) plot(out, quiet = TRUE, ...)

  return(out)
}


#' @export
print.auto_rate <- function(x, pos = 1, ...) {

  ## warning if empty
  if(length(x$rate) == 0) stop("print.auto_rate: No rates found in 'auto_rate' object.")

  cat("\n# print.auto_rate # ---------------------\n")

  if(is.null(pos)) pos <- 1
  if(length(pos) > 1)
    stop("print.auto_rate: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate))
    stop("print.auto_rate: Invalid 'pos' rank: only ", length(x$rate), " rates found.")

  method <- x$method
  cat("Data is subset by '", x$by, "' using 'width' of ", x$width, ".\n", sep = "")
  cat(sprintf("Rates were computed using '%s' method.\n", x$ method))
  if (method == "linear")
    cat(nrow(x$summary), "linear regions detected in the kernel density estimate.\n")
  cat("To see all results use summary().\n")

  if (method %in% c("max", "min", "maximum", "minimum", "highest", "lowest", "linear", "rolling")) {
    cat("\nRank", pos, "of", nrow(x$summary), ":\n")
    cat("Rate:", x$summary$rate[pos], "\n")
    cat("R.sq:", signif(x$summary$rsq[pos], 5), "\n")
    cat("Rows:", x$summary$row[pos], "to", x$summary$endrow[pos], "\n")
    cat("Time:", x$summary$time[pos], "to", x$summary$endtime[pos], "\n")
  } else if (method == "interval") {
    cat("\n=== All", nrow(x$summary), "results of", nrow(x$summary), "===\n")
    print(x$summary)
  }
  cat("-----------------------------------------\n")
  return(invisible(x)) # this lets us continue with dplyr pipes
}

#' @export
plot.auto_rate <- function(x, pos = 1, panel = FALSE, quiet = FALSE,
                           legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # how many rates
  nres <- length(x$rate)

  ## warning if empty
  if(nres == 0) stop("plot.auto_rate: Nothing to plot! No rates found in 'auto_rate' object.")

  ## pos 1 by default
  if(is.null(pos)) pos <-1
  ## warning if pos too low
  if(pos > nres)
    stop("plot.auto_rate: Invalid 'pos' rank: only ", nres, " rates found.")

  if(!quiet) {
    cat("\n# plot.auto_rate # ----------------------\n")
    if(pos == 1 && nres == 1)
      cat(glue::glue("plot.auto_rate: Plotting rate from position {pos} of {nres} ..."), sep="\n")
    if(pos == 1 && nres > 1)
      cat(glue::glue("plot.auto_rate: Plotting rate from position {pos} of {nres} ... \nTo plot others use 'pos'"), sep="\n")
    if(pos > 1)
      cat(glue::glue('plot.auto_rate: Plotting rate from position {pos} of {nres} ...'), sep="\n")
  }

  # DEFINE OBJECTS
  dt <- x$dataframe
  start <- x$summary$row[pos]
  end <- x$summary$endrow[pos]
  rownums_sub <- start:end
  rownums <- 1:nrow(dt)
  sdt <- dt[start:end]
  rolldt <- data.table::data.table(x = (x$roll$endtime+x$roll$time)/2,
                                   y = x$roll$rate)
  xlim <- range(nainf.omit(x$dataframe$x))
  # middle row of each regression for roll rate plot
  rate <- x$summary$rate_b1[pos]
  rsq <- signif(x$summary$rsq[pos],3)
  fit <- lm(sdt[[2]] ~ sdt[[1]], sdt) # lm of subset
  interval <- x$summary$endrow
  dens <- x$density
  peaks <- x$peaks[, 2:3]

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = cex_def)

  # PLOT BASED ON METHOD
  if (x$method %in% c("max", "min", "maximum", "minimum",
                      "highest", "lowest", "rolling", "interval")) {
    if (panel == FALSE) {

      mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5),
                    nrow = 2, byrow = TRUE)
      layout(mat)

      par(ps = 10,
          cex = 1,
          cex.main = 1,
          ...)

      multi.p(dt, sdt, legend = legend)
      if(x$method == "interval") abline(v = 1, lty = 3)
      if(x$method == "interval") abline(v = interval, lty = 3)
      sub.p(sdt, rsq = rsq, rownums = rownums_sub, legend = legend)
      rollreg.p(rolldt, rate, rownums = rownums, xlim = xlim, rate.rev)
      residual.p(fit)
      qq.p(fit)
      layout(1)
    }
  } else if (x$method == "linear") {
    if (panel == FALSE) {

      par(mfrow = c(2, 3),
          ps = 10,
          cex = 1,
          cex.main = 1,
          ...)

      multi.p(dt, sdt, legend = legend) # full timeseries with lmfit
      sub.p(sdt, rsq = rsq, rownums = rownums_sub, legend = legend) # closed-up (subset timeseries)
      rollreg.p(rolldt, rate, rownums = rownums, xlim = xlim, rate.rev) # rolling regression series with markline
      residual.p(fit) # residual plot
      qq.p(fit) # qq plot
      density.p(dens, peaks, pos) # density plot
    }
  }

  if(!isFALSE(panel))

    par(ps = 10,
        cex = 1,
        cex.main = 1,
        ...)

  if (panel == 1) {
    multi.p(dt, sdt, legend = legend) # full timeseries with lmfit
    if (x$method == "interval") {
      abline(v = 1, lty = 3)
      abline(v = interval, lty = 3)
    }
  }
  if (panel == 2)
    sub.p(sdt, rsq = rsq, rownums = rownums_sub, legend = legend)  # subset plot
  if (panel == 3)
    rollreg.p(rolldt, rate, rownums = rownums, xlim = xlim, rate.rev)  # rolling regression
  if (panel == 4)
    residual.p(fit) # residual plot
  if (panel == 5)
    qq.p(fit)  #qq plot
  if (panel == 6) {
    if (x$method != 'linear') {
      stop('plot.auto_rate: density plot only available for "linear" method.')
    } else {
      density.p(dens, peaks, pos)  # density
    }
  }

  mtext(glue::glue("auto.rate: Rank {pos} of {nres} Total Rates"),
        outer = TRUE, cex = 1.2, line = 0.3, font = 2)

  if (!quiet) cat("-----------------------------------------\n")


  return(invisible(x))
}

#' @export
summary.auto_rate <- function(object, pos = NULL, export = FALSE, ...) {

  ## warning if empty
  if(length(object$rate) == 0) stop("summary.auto_rate: No rates found in 'auto_rate' object.")

  cat("\n# summary.auto_rate # -------------------\n")

  if(!is.null(pos) && pos > length(object$rate))
    stop("summary.auto_rate: Invalid 'pos' rank: only ", length(object$rate), " rates found.")

  ########### Summary Table ###################
  if (is.null(pos)) {
    # if no row is specified, return all results
    if (object$method == "linear") cat("\n=== Summary of Results by Kernel Density Rank ===\n")
    if (object$method == "highest") cat("\n=== Summary of Results by Highest Rate ===\n")
    if (object$method == "lowest") cat("\n=== Summary of Results by Lowest Rate ===\n")
    if (object$method == "maximum") cat("\n=== Summary of Results by Maximum Rate ===\n")
    if (object$method == "minimum") cat("\n=== Summary of Results by Minimum Rate ===\n")
    if (object$method == "rolling") cat("\n=== Summary of Results by Rolling Order ===\n")
    if (object$method == "interval") cat("\n=== Summary of Results by Interval Order ===\n")
    if (object$method == "max") cat("\n=== Summary of Results by Maximum NEGATIVE Rate ===\n")
    if (object$method == "min") cat("\n=== Summary of Results by Minimum NEGATIVE Rate ===\n")
    out <- data.table(object$summary)
    print(out, nrows = 20)
  } else {
    # otherwise, return row specified by `pos`
    cat("\n=== Summary of results from entered 'pos' rank(s)", "===\n\n")
    out <- data.table::data.table(object$summary)[pos]
    print(out)
  }

  ########### Regressions summary line ########
  cat("\nRegressions :", nrow(object$roll))
  cat(" | Results :", nrow(object$summary))
  cat(" | Method :", object$method)
  cat(" | Roll width :", object$width)
  cat(" | Roll type :", object$by, "\n")


  ########### Kernel Density summary ##########
  if (object$method == "linear") {
    cat("\n=== Kernel Density Summary ===")
    print_dens(object$density)
  }

  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.auto_rate <- function(x, pos = NULL, export = FALSE, ...){

  ## warning if empty
  if(length(x$rate) == 0) stop("mean.auto_rate: No rates found in 'auto_rate' object.")

  cat("\n# mean.auto_rate # ----------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate)))
    stop("mean.auto_rate: Invalid 'pos' rank: only ", length(x$rate), " rates found.")
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


