#' Automatically determine most linear, maximum and minimum oxygen uptake or
#' production rates
#'
#' `auto_rate` performs rolling regressions on a dataset to determine the *most
#' linear, highest, lowest, maximum, minimum, rolling*, and *interval* rates of
#' change in oxygen concentration over time. Initially, a rolling regression of
#' the specified `width` is performed on the entire dataset. Then, based on the
#' "`method`" argument, the resulting regressions are ranked or ordered, and the
#' output is summarised.
#'
#' ## Ranking and ordering algorithms
#'
#' Currently, `auto_rate` contains seven ranking and ordering algorithms that
#' can be applied using the `method` argument:
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
#' \href{https://januarharianto.github.io/respR/articles/auto_rate.html}{here}
#' for full details.
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
#' specified 'width' are extracted from the rolled regressions, ordered by time.
#'
#' - NOTE: `max`, `min`: These methods were used in previous versions of `respR`
#' but have been deprecated. They were intended to order oxygen uptake
#' (negative) rates by magnitude, but this resulted in incorrect ordering of
#' oxygen production (positive) rates. They have been retained for code
#' compatibility, but may be removed in a future version of `respR`, and so
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
#' The `width` input defaults to 0.2. If it is a value between 0 and 1, it
#' represents a proportion of the total data length, as in the equation
#' `floor(width * number of data rows)`. For example, 0.2 represents a
#' regression rolling window of 20% of the data. Otherwise, if 1 or greater, the
#' width is an exact value in either number of rows or in the units of time
#' data, as specified via the `by` argument.
#'
#' In most cases, `by` should be left as the default `"row"`, and the `width`
#' chosen with this in mind, as it is considerably more computationally
#' efficient. Changing to `"time"` causes the function to perform checks for
#' irregular time intervals at every iteration of the rolling regression, which
#' adds to computation time. This is to ensure the specified `width` input is
#' honoured in the time units and rates correctly calculated, even if there are
#' gaps in the data.
#'
#' ## Plot
#'
#' A plot is produced (provided `plot = TRUE`) showing the original data
#' timeseries of oxygen against time, with the rate result region highlighted, a
#' close-up of this region with linear model coefficients, a rolling rate plot
#' (note the reversed y-axis so that higher oxygen consumption rates are
#' higher), various summary plots of fit and residuals, and for the `linear`
#' method the results of the kernel density analysis. If multiple rates have
#' been calculated, by default the first is plotted. Others can be plotted by
#' changing the `pos` argument either in the main function call, or by plotting
#' the output, e.g. `plot(object, pos = 2)`. In addition, each sub-panel can be
#' examined individually by using the `choose` input, e.g. `plot(object, choose
#' = 2)`. Console output messages can be suppressed using `quiet = TRUE`. The
#' rate in the rolling rate plot can be plotted *not* reversed by passing
#' `rate.rev = FALSE`, for instance if examining oxygen production rates so that
#' higher production rates appear higher.
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
#' @param x data frame, or object of class `inspect` containing oxygen~time
#'   data.
#' @param method string. `"linear"`, `"highest"`, `"lowest"`, `"maximum"`,
#'   `"minimum"`, `"rolling"` or `"interval"`. Defaults to `"linear"`. See
#'   Details.
#' @param width numeric. Width of the rolling regression. Defaults to 0.2.
#'   Either a value between 0 and 1 representing a proportion of the data
#'   length, or a value above 1 representing an exact width in the `by` input.
#'   See Details.
#' @param by string. `"row"` or `"time"`. Defaults to `"row"`. Metric by which
#'   to apply the `width` input.
#' @param plot logical. Plot the results. Defaults to TRUE.
#' @param ... Allows additional plotting controls to be passed, such as `pos`,
#'   `choose`, and `quiet = TRUE`.
#'
#' @return A list object of class `auto_rate`.
#'
#' @md
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' # most linear section of the entire data
#' auto_rate(sardine.rd)
#'
#' \dontrun{
#' # What is the lowest oxygen consumption rate over a 10 minute (600s) period?
#' low_rates <- auto_rate(sardine.rd, method = "lowest", width = 600, by = "time")
#' # View summary of lowest rate results
#' summary(low_rates)
#'
#' # What is the highest oxygen consumption rate over a 10 minute (600s) period?
#' high_rates <- auto_rate(sardine.rd, method = "highest", width = 600, by = "time")
#' # View summary of lowest rate results
#' summary(high_rates)
#'
#' # What is the NUMERICAL minimum oxygen consumption rate over a 5 minute (300s)
#' # period in intermittent-flow respirometry data?
#' # NOTE: because uptake rates are negative, this would be the HIGHEST uptake rate.
#' auto_rate(intermittent.rd, method = "minimum", width = 600, by = "time")
#' }
#'
auto_rate <- function(x, method = 'linear', width = NULL, by = 'row',
                      plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()

  # perform checks
  checks <- validate_auto_rate(x, by, method)
  dt <- checks$df  # extract df from validation check
  by <- checks$by

  # prepare data
  data.table::setnames(dt, 1:2, c("x", "y")) # rename data columns
  win <- calc_rolling_win(dt, width, by)  # determine width automatically

  # verify & apply methods
  ## OLD METHOD
  if (method == 'max') {
    warning("auto_rate: the 'min' and 'max' methods have been deprecated, as they resulted in incorrect ordering of oxygen production rates. \n They have been retained for code compatibility, but may be removed in a future version of respR. \n Please use 'highest/lowest' for ordering by absolute rate value, or 'maximum/minimum' for strict numerical ordering of rates. ")
    output <- auto_rate_min(dt, win, by) ## note "wrong" method - but matches old behaviour
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

    ## OLD METHOD
  } else if (method == 'min') {
    warning("auto_rate: the 'min' and 'max' methods have been deprecated, as they resulted in incorrect ordering of oxygen production rates. \n They have been retained for code compatibility, but may be removed in a future version of respR. \n Please use 'highest/lowest' for ordering by absolute rate value, or 'maximum/minimum' for strict numerical ordering of rates. ")
    output <- auto_rate_max(dt, win, by) ## note "wrong" method - but matches old behaviour
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

    ### NOW ORDERS BY NUMERICAL VALUE I.E. CONSIDERS SIGN
  } else if (method == 'maximum') {
    output <- auto_rate_max(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

    ### NOW ORDERS BY NUMERICAL VALUE I.E. CONSIDERS SIGN
  } else if (method == 'minimum') {
    output <- auto_rate_min(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

  } else if (method == 'interval') {
    output <- auto_rate_interval(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

  } else if (method == 'rolling') {
    output <- auto_rate_rolling(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

  } else if (method == 'highest') {
    output <- auto_rate_highest(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

  } else if (method == 'lowest') {
    output <- auto_rate_lowest(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(call = call,
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

  } else if (method == 'linear') {
    output <- auto_rate_linear(dt, win)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll),
                           total_peaks = nrow(output$peaks),
                           kde_bw = output$density$bw)
    out <- list(call = call,
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

  } else stop("auto_rate: 'method' argument not recognised")

  ## reorder output summary
  out$summary <- data.table::data.table(rank = 1:nrow(out$summary),
                                        intercept_b0 = out$summary$intercept_b0,
                                        rate_b1 = out$summary$rate_b1,
                                        rsq = out$summary$rsq,
                                        density = out$summary$density,
                                        row = out$summary$row,
                                        endrow = out$summary$endrow,
                                        time = out$summary$time,
                                        endtime = out$summary$endtime,
                                        rate = out$summary$rate_b1)

  class(out) <- 'auto_rate'

  if (plot) plot(out, ...)

  return(out)
}


#' @export
print.auto_rate <- function(x, pos = 1, ...) {

  ## warning if empty
  if(length(x$rate) == 0) stop("print.auto_rate: No rates found in auto_rate x.")

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
plot.auto_rate <- function(x, pos = 1, choose = FALSE, quiet = FALSE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  nres <- length(x$rate)

  ## warning if empty
  if(nres == 0) stop("auto_rate: No rates found in auto_rate x.")

  ## pos 1 by default
  if(is.null(pos)) pos <-1
  ## warning if pos too low
  if(pos > nres)
    stop("auto_rate: Invalid 'pos' rank: only ", nres, " rates found.")

  if(!quiet) {
    cat("\n# plot.auto_rate # ----------------------\n")
    if(pos == 1 && nres == 1)
      cat(glue::glue("auto_rate: Plotting rate from position {pos} of {nres} ..."), sep="\n")
    if(pos == 1 && nres > 1)
      cat(glue::glue("auto_rate: Plotting rate from position {pos} of {nres} ... \nTo plot others use 'pos'"), sep="\n")
    if(pos > 1)
      cat(glue::glue('auto_rate: Plotting rate from position {pos} of {nres} ...'), sep="\n")
  }

  # DEFINE OBJECTS
  dt <- x$dataframe
  start <- x$summary$row[pos]
  end <- x$summary$endrow[pos]
  sdt <- dt[start:end]
  rolldt <- data.table::data.table(x = (x$roll$endtime+x$roll$time)/2,
                                   y = x$roll$rate)
  rate <- x$summary$rate_b1[pos]
  rsq <- signif(x$summary$rsq[pos],3)
  fit <- lm(sdt[[2]] ~ sdt[[1]], sdt) # lm of subset
  interval <- x$summary$endtime
  startint <- min(interval) - x$width
  dens <- x$density
  peaks <- x$peaks[, 2:3]

  ## plot params
  mai = c(0.3, 0.15, 0.2, 0.1)
  oma = c(1, 1, 1.5, 0)

  # PLOT BASED ON METHOD
  if (x$method %in% c("max", "min", "maximum", "minimum", "highest", "lowest", "rolling")) {
    if (choose == FALSE) {

      mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5), nrow = 2, byrow = TRUE)
      layout(mat)
      par(mai = mai,
          oma = oma,
          ps = 10, cex = 1, cex.main = 1)
      multi.p(dt, sdt)
      sub.p(sdt, rsq = rsq)
      rollreg.p(rolldt, rate, rate.rev)
      residual.p(fit)
      qq.p(fit)
      layout(1)
    }
  } else if (x$method == "interval") {
    if (choose == FALSE) {

      par(mfrow = c(2, 2),
          mai = mai,
          oma = oma,
          ps = 10, cex = 1, cex.main = 1)
      multi.p(dt, sdt)
      abline(v = startint, lty = 3)
      abline(v = interval, lty = 3)
      sub.p(sdt, rsq = rsq)
      residual.p(fit)
      qq.p(fit)
    }
  } else if (x$method == "linear") {
    if (choose == FALSE) {

      par(mfrow = c(2, 3),
          mai = mai,
          oma = oma,
          ps = 10, cex = 1, cex.main = 1)
      multi.p(dt, sdt) # full timeseries with lmfit
      sub.p(sdt, rsq = rsq) # closed-up (subset timeseries)
      rollreg.p(rolldt, rate, rate.rev = rate.rev) # rolling regression series with markline
      density.p(dens, peaks, pos) # density plot
      residual.p(fit) # residual plot
      qq.p(fit) # qq plot
    }
  }

  if(!isFALSE(choose))
    par(mai = c(0.3, 0.3, 0.2, 0.2),
        oma = c(0.4, 0.4, 0.3, 0.3))
  if (choose == 1) {
    multi.p(dt, sdt) # full timeseries with lmfit
    if (x$method == "interval") {
      abline(v = startint, lty = 3)
      abline(v = interval, lty = 3)
    }
  }
  if (choose == 2) sub.p(sdt, rsq = rsq)  # subset plot
  if (choose == 3) rollreg.p(rolldt, rate, rate.rev)  # rolling regression
  if (choose == 4) {
    if (x$method != 'linear') {
      stop('auto_rate: density plot not available for "highest", "lowest", "maximum", "minimum", "rolling" and "interval" methods')
    } else density.p(dens, peaks, pos)  # density
  }
  if (choose == 5) residual.p(fit)  # residual plot
  if (choose == 6) qq.p(fit)  #qq plot

  mtext(glue::glue("auto.rate: Rank {pos} of {nres} Total Rates"),
        outer = TRUE, cex = 1.2, line = 0.3, font = 2)

  if (!quiet) cat("-----------------------------------------\n")


  return(invisible(x))
}

#' @export
summary.auto_rate <- function(object, pos = NULL, export = FALSE, ...) {

  ## warning if empty
  if(length(object$rate) == 0) stop("auto_rate: No rates found in auto_rate object.")

  cat("\n# summary.auto_rate # -------------------\n")

  if(!is.null(pos) && pos > length(object$rate))
    stop("auto_rate: Invalid 'pos' rank: only ", length(object$rate), " rates found.")

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
  if(length(x$rate) == 0) stop("auto_rate: No rates found in auto_rate x.")

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


#' Normal rolling regression
#'
#' This is an internal function.
#'
#' @importFrom roll roll_lm
#' @keywords internal
#' @export
static_roll <- function(df, win) {
  roll <- roll_lm(matrix(df[[1]]), matrix(df[[2]]), win)
  roll <- na.omit(data.table(cbind(roll$coefficients, roll$r.squared)))
  setnames(roll, 1:3, c("intercept_b0", "rate_b1", "rsq"))
  return(roll)
}


#' Perform time-width rolling regression
#'
#' This is an internal function. Used by [auto_rate()].
#'
#' @keywords internal
#' @import parallel
#' @export
time_roll <- function(dt, width, parallel = FALSE) {
  future_lapply <- plan <- NULL # global variables hack (unfortunate)
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("V1", "V2"))

  # The cutoff specifies where to stop the rolling regression, based on width
  time_cutoff <- max(dt[,1]) - width
  row_cutoff <- max(dt[, which(V1 <= time_cutoff)])

  # if(parallel) {
  #   oplan <- plan()
  #   on.exit(plan(oplan), add = TRUE)
  #   if (os() == 'win') {
  #     plan(multicore)
  #   } else plan(multisession)
  #   out <- future_lapply(1:row_cutoff, function(x) time_lm(dt,
  #     dt[[1]][x], dt[[1]][x] + width))
  # } else {
  #   out <- lapply(1:row_cutoff, function(x) time_lm(dt,
  #     dt[[1]][x], dt[[1]][x] + width))
  # }

  # parallelisation
  if (parallel) {
    no_cores <- parallel::detectCores()  # calc the no. of cores available
    if (os() == "win") {
      cl <- parallel::makeCluster(no_cores)
    } else cl <- parallel::makeCluster(no_cores, type = "FORK")
    parallel::clusterExport(cl, "time_lm")
    out <- parallel::parLapply(cl, 1:row_cutoff, function(x) time_lm(dt,
                                                                     dt[[1]][x], dt[[1]][x] + width))
    parallel::stopCluster(cl)  # stop cluster (release cores)
  } else out <- lapply(1:row_cutoff, function(x) time_lm(dt,
                                                         dt[[1]][x], dt[[1]][x] + width))

  out <- data.table::rbindlist(out)
  return(out)
}


#' Subset data by time and perform a linear regression.
#'
#' This is an internal function. Used with [time_roll()] and [auto_rate()].
#'
#' @keywords internal
#' @export
time_lm <- function(df, start, end) {
  dt <- data.table::data.table(df)
  data.table::setnames(dt, 1:2, c("x", "y"))
  sdt <- dt[x >= start & x <= end]
  fit <- .lm.fit(cbind(1, sdt[[1]]), sdt[[2]])  # perform lm
  coef <- coef(fit)
  intercept_b0 <- coef[1]
  rate_b1 <- coef[2]
  # Calculate coef of determination (rsq) manually
  ybar <- sdt[, mean(y)]
  sst <- sum(sdt[, (y-ybar)*(y-ybar)])
  ssr <- sum((fit$residuals)*(fit$residuals))
  rsq <- 1-(ssr/sst)
  out <- data.table::data.table(intercept_b0, rate_b1, rsq)
  return(out)
}


#' Rolling regression
#'
#' @keywords internal
#' @export
rolling_reg <- function(dt, width, by, method) {
  # win <- calc_window(dt, width, by) # this was wrong hmm
  win <- width
  # Rolling regression
  if (by == "time") {
    roll <- time_roll(dt, win, parallel = FALSE) # TODO: fix parallel here
  } else if (by == "row") {
    roll <- static_roll(dt, win)
  }

  # Attach row and time indices to the roll data
  if (by == "time") {
    roll[, row := seq_len(.N)]
    endrow <- sapply(dt[roll[, row], x], function(z)
      max(dt[, which(x <= z + win)]))
    roll[, endrow := endrow]
    roll[, time := dt[roll[, row], x]]
    roll[, endtime := dt[roll[, endrow], x]]
  } else if (by == "row") {
    roll[, row := seq_len(.N)]
    roll[, endrow := roll[, row] + win - 1]
    roll[, time := dt[roll[, row], x]]
    roll[, endtime := dt[roll[, endrow], x]]
  }
  out <- list(roll = data.table(roll), win = win)
  return(out)
}

#' Kernel density estimation and fitting
#'
#' @keywords internal
#' @export
kde_fit <- function(dt, width, by, method, use = "all") {
  # perform rolling regression
  reg <- rolling_reg(dt, width, by, method)
  roll <- reg$roll
  win <- reg$win

  # If entire width of data frame is selected, there's nothing to detect!
  if (nrow(roll) == 1) {
    subsets <- list(truncate_data(dt, roll$row, roll$endrow, "row"))
    d <- NULL
    peaks <- roll$rate_b1
    bw <- NULL

  } else {
    d <- density(roll$rate_b1, na.rm = T, bw = "SJ-ste", adjust = .95) # KDE
    bw <- d$bw
    peaks <- which(diff(sign(diff(d$y))) == -2) + 1 # index modes
    peaks <- lapply(peaks, function(x)
      data.table(index = x, peak_b1 = d$x, density = d$y)[x, ]) # match to roll
    if (use == "all") {
      peaks <- rbindlist(peaks)[order(-rank(density))] # rank by size
    } else {
      peaks <- rbindlist(peaks)[order(-rank(density))][1]
    }
    # match peaks to roll data
    frags <- lapply(
      peaks$peak_b1,
      function(x) roll[rate_b1 <= (x + d$bw*.95)][rate_b1 >= (x - d$bw*.95)]
    )
    # split non-overlapping rolls by window size
    if (by == "row") {
      frags <- lapply(1:length(frags), function(x) split(
        frags[[x]], c(0, cumsum(abs(diff(frags[[x]]$row)) > win))
      ))
    } else {
      frags <- lapply(1:length(frags), function(x) split(
        frags[[x]], c(0, cumsum(abs(diff(frags[[x]]$time)) > win))
      ))
    }
    # select longest fragments
    i <- sapply(1:length(frags),
                function(x) which.max(sapply(frags[[x]], nrow)))
    frags <- unname(mapply(function(x, y) frags[[x]][y], 1:length(frags), i))
    frags <- frags[sapply(frags, nrow) > 0] # remove zero-length data
    # Convert fragments to subsets
    subsets <- lapply(1:length(frags), function(x)
      truncate_data(
        dt, min(frags[[x]]$row),
        max(frags[[x]]$endrow), "row"
      ))
  }
  out <- list(
    win = win, roll = roll, subsets = subsets, density = d,
    peaks = peaks, bandwidth = bw
  )
  return(out)
}

#' Calculate rolling window size for rolling regression
#'
#' @keywords internal
#' @export
calc_window <- function(dt, width, by) {
  # Determine rolling window from `width`
  if (is.null(width)) {
    width <- .2
    if (by == "time") win <- floor(width * max(dt[[1]]))
    if (by == "row") win <- floor(width * nrow(dt))
  } else if (is.numeric(width)) {
    if (width <= 1) {
      if (by == "time") win <- floor(width * max(dt[[1]]))
      if (by == "row") win <- floor(width * nrow(dt))
    } else if (width > 1) win <- width
  } else {
    stop("auto_rate: 'width' must be numeric.")
  }
  return(win)
}



#' Prints the density object for summary.auto_rate S3
#' Basically copied from stats:::print.density and edited to make
#' it more compact
#'
#' @keywords internal
#' @export
print_dens <- function (x, digits = NULL, ...){
  cat("\nCall: ", gsub("  ", "", deparse(x$call)), "\nData: ", x$data.name,
      " (", x$n, " obs.)", ", Bandwidth 'bw' = ", formatC(x$bw,
                                                          digits = digits), "\n", sep = "")
  print(summary(as.data.frame(x[c("x", "y")])), digits = digits,
        ...)
  invisible(x)
}

