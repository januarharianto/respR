#' Automatically determine most linear, maximum and minimum rates of change in
#' oxygen concentration in a dataset
#'
#' `auto_rate` performs a rolling regression on a dataset to determine the *most
#' linear, highest, lowest, maximum, minimum*, and *interval* rates of change in
#' oxygen concentration over time. Initially, a rolling regression of the
#' specified `width` is performed on the entire dataset to obtain all possible
#' rate values. Then, based on the "`method`" argument, the resulting
#' regressions are ranked or ordered, and the output is summarised.
#'
#' @details ***Ranking and ordering algorithms***
#'
#'   Currently, `auto_rate` contains six ranking and ordering algorithms that
#'   can be applied using the `method` argument:
#'
#'   - `linear`: Uses kernel density estimation (KDE) to learn the shape of the
#'   entire dataset and *automatically identify* the most linear regions of the
#'   timeseries. This is achieved by using the smoothing bandwidth of the KDE to
#'   re-sample the "peaks" in the KDE to determine linear regions of the data.
#'   See
#'   \href{https://januarharianto.github.io/respR/articles/auto_rate.html}{here}
#'   for full details. The summary output will contain only the regressions
#'   identified as coming from linear regions of the data, ranked by order of
#'   the KDE density analysis. This is present in the `$summary` component of
#'   the output as `$density`.
#'
#'   - `highest`: Every regression of the specified `width` across the entire
#'   timeseries is calculated, then ordered using ***absolute*** rate values
#'   from highest to lowest. Essentially, this option ignores the sign of the
#'   rate, and can only be used when rates all have the same sign. Regardless of
#'   if they are oxygen uptake or oxygen production rates, rates will be ordered
#'   from highest to lowest in the `$summary` table.
#'
#'   - `lowest`: Every regression of the specified `width` across the entire
#'   timeseries is calculated, then ordered using ***absolute*** rate values
#'   from lowest to highest. Essentially, this option ignores the sign of the
#'   rate, and can only be used when rates all have the same sign. Regardless of
#'   if they are oxygen uptake or oxygen production rates, rates will be ordered
#'   from lowest to highest in the `$summary` table.
#'
#'   - `maximum`: Every regression of the specified `width` across the entire
#'   timeseries is calculated, then ordered using ***numerical*** rate values
#'   from maximum to minimum. Takes ***full account of the sign of the rate***.
#'   Therefore, oxygen uptake rates, which in `respR` are negative, would be
#'   ordered from lowest (least negative), to highest (most negative) in the
#'   summary table in numerical order. Therefore, generally this method should
#'   only be used when rates are a mix of oxygen consumption and production
#'   rates, or when positive rates result from flushes such as in
#'   intermittent-flow respirometry. For most analyses where highest or lowest
#'   rates are of interest use the 'highest' or 'lowest' methods should be used.
#'
#'   - `minimum`: Every regression of the specified `width` across the entire
#'   timeseries is calculated, then ordered using ***numerical*** rate values
#'   from minimum to maximum. Takes ***full account of the sign of the rate***.
#'   Therefore, oxygen uptake rates, which in `respR` are negative, would be
#'   ordered from highest (most negative) to lowest (least negative) in the
#'   summary table in numerical order. Therefore, generally this method should
#'   only be used when rates are a mix of oxygen consumption and production
#'   rates, or when positive rates result from flushes such as in
#'   intermittent-flow respirometry. For most analyses where highest or lowest
#'   rates are of interest use the 'highest' or 'lowest' methods should be used.
#'
#'   - `interval`: multiple, successive, non-overlapping regressions of the
#'   specified 'width' are extracted from the rolled regressions, ordered by
#'   time.
#'
#'   - NOTE: `max`, `min`: These methods used in previous versions have been
#'   deprecated. They were intended to order oxygen uptake (negative) rates by
#'   magnitude, but this resulted in incorrect ordering of oxygen production
#'   (positive) rates. They have been retained for code compatibility, but may
#'   be removed in a future version of `respR`, and so *should not be used*.
#'
#'   ***Further selection of rates***
#'
#'   For further selection or subsetting of `auto_rate` results, see the
#'   dedicated \code{\link{subset_rate}} function, which allows subsetting of
#'   rates by various criteria, including r-squared, data region, percentiles,
#'   and more.
#'
#'   ***Units***
#'
#'   There are no units of measurement involved in `auto_rate()`. This is a
#'   deliberate decision. Units are called in a later function when absolute
#'   and/or mass-specific rates of oxygen use are computed in [convert_rate()]
#'   and [convert_DO()].
#'
#' @param df data frame, or object of class `inspect` containing oxygen~time
#'   data.
#' @param method string. `"linear"`, `"highest"`, `"lowest"`, `"maximum"`,
#'   `"minimum"` or `"interval"`. Defaults to `linear`. See Details.
#' @param width numeric. Width of the rolling regression. Defaults to
#'   `floor(0.2*length of data)` or 20% of the width of the data, i.e. `width =
#'   0.2`. In testing, this value performs well with the `linear` method. A
#'   value between 0 and 1 represents a proportion of the total data length, as
#'   applied in the equation `floor(width * length of data)`. Otherwise if 1 or
#'   greater, the width can be an exact value in either the time unit or in
#'   rows, as specified via the `by = ` argument.
#' @param by string. Metric to which to apply the `width` input applies if 1 or
#'   greater, either `"row"` or `"time"`. Defaults to `"row"`. However, if the
#'   function detects an irregular time series, a warning will be issued to
#'   consider changing this argument to `"time"`. In most cases `"row"` should
#'   be used by default as it is more computationally efficient. Switching to
#'   `"time"` causes the function to perform checks for irregular time intervals
#'   at every iteration of the rolling regression, even if time is evenly
#'   spaced, which adds to computation time. This is to ensure the specified
#'   `"width"` input is honoured.
#' @param plot logical. Plot the results. Defaults to TRUE.
#'
#' @return A list object of class `auto_rate`.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' # most linear section of the entire data
#' auto_rate(flowthrough.rd)
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
auto_rate <- function(df, method = 'linear', width = NULL, by = 'row',
                      plot = TRUE) {
  # perform checks
  checks <- validate_auto_rate(df, by, method)
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
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)

    ## OLD METHOD
  } else if (method == 'min') {
    warning("auto_rate: the 'min' and 'max' methods have been deprecated, as they resulted in incorrect ordering of oxygen production rates. \n They have been retained for code compatibility, but may be removed in a future version of respR. \n Please use 'highest/lowest' for ordering by absolute rate value, or 'maximum/minimum' for strict numerical ordering of rates. ")
    output <- auto_rate_max(dt, win, by) ## note "wrong" method - but matches old behaviour
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)

    ### NOW ORDERS BY NUMERICAL VALUE I.E. CONSIDERS SIGN
  } else if (method == 'maximum') {
    output <- auto_rate_max(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)

    ### NOW ORDERS BY NUMERICAL VALUE I.E. CONSIDERS SIGN
  } else if (method == 'minimum') {
    output <- auto_rate_min(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)


  } else if (method == 'interval') {
    output <- auto_rate_interval(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)

  } else if (method == 'highest') {
    output <- auto_rate_highest(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)

  } else if (method == 'lowest') {
    output <- auto_rate_lowest(dt, win, by)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll))
    out <- list(dataframe = dt,
                width   = win,
                by      = by,
                method  = method,
                roll    = output$roll,
                summary = output$results,
                rate    = output$results$rate_b1,
                metadata = metadata)


  } else if (method == 'linear') {
    output <- auto_rate_linear(dt, win)
    metadata <- data.table(width = win, by = by, method = method,
                           total_regs = nrow(output$roll),
                           total_peaks = nrow(output$peaks),
                           kde_bw = output$density$bw)
    out <- list(dataframe = dt,
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


  class(out) <- 'auto_rate'

  if (plot) plot(out, label = FALSE)

  return(out)
}


#' @export
print.auto_rate <- function(x, pos = 1, ...) {

  ## warning if empty
  if(length(x$rate) == 0) stop("auto_rate: No rates found in auto_rate x.")

  cat("\n# print.auto_rate # ---------------------\n")

  if(is.null(pos)) pos <- 1
  if(pos > length(x$rate))
    stop("auto_rate: Invalid 'pos' rank: only ", length(x$rate), " rates found.")

  method <- x$method
  cat("Data is subset by", x$by, "using width of", x$width, "\n")
  cat(sprintf("Rates were computed using '%s' method\n", x$ method))
  if (method == "linear")
    cat(nrow(x$summary), "linear regions detected in the kernel density estimate\n")
  cat("To see all results use summary()\n")

  if (method %in% c("max", "min", "maximum", "minimum", "highest", "lowest", "linear")) {
    cat("\nRank", pos, "of", nrow(x$summary), ":\n")
    cat("Rate:", x$summary$rate_b1[pos], "\n")
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

# OLD PLOTTING FUNCTION USING BASE PLOT.
# Don't delete -- take as a reminder that this has been attempted before.
#' @export
plot.auto_rate <- function(x, pos = 1, choose = FALSE, label = TRUE, ...) {

  ## warning if empty
  if(length(x$rate) == 0) stop("auto_rate: No rates found in auto_rate x.")

  ## pos 1 by default
  if(is.null(pos)) pos <-1
  ## warning if pos too low
  if(pos > length(x$rate))
    stop("auto_rate: Invalid 'pos' rank: only ", length(x$rate), " rates found.")

  parorig <- par(no.readonly = TRUE) # save original par settings

  if (label) {
    cat("\n# plot.auto_rate # ----------------------\n")
    cat('Plotting auto_rate result from position', pos, 'of', length(x$rate), '... \n')
  }

  # DEFINE OBJECTS
  dt <- x$dataframe
  start <- x$summary$row[pos]
  end <- x$summary$endrow[pos]
  sdt <- dt[start:end]
  rolldt <- data.table::data.table(x = x$roll$endtime, y = x$roll$rate)
  rate <- x$summary$rate_b1[pos]
  rsq <- signif(x$summary$rsq[pos],3)
  fit <- lm(sdt[[2]] ~ sdt[[1]], sdt) # lm of subset
  interval <- x$summary$endtime
  startint <- min(interval) - x$width
  dens <- x$density
  peaks <- x$peaks[, 2:3]

  # PLOT BASED ON METHOD
  if (x$method %in% c("max", "min", "maximum", "minimum", "highest", "lowest")) {
    if (choose == FALSE) {

      mat <- matrix(c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5), nrow = 2, byrow = TRUE)
      layout(mat)
      par(mai = c(0.4, 0.4, 0.3, 0.3), ps = 10, cex = 1, cex.main = 1)
      multi.p(dt, sdt)
      sub.p(sdt, rsq = rsq)
      rollreg.p(rolldt, rate)
      residual.p(fit)
      qq.p(fit)
      layout(1)
    }
  } else if (x$method == "interval") {
    if (choose == FALSE) {

      par(mfrow = c(2, 2), mai = c(.4, .4, .3, .3), ps = 10, cex = 1, cex.main = 1)
      multi.p(dt, sdt)
      abline(v = startint, lty = 3)
      abline(v = interval, lty = 3)
      sub.p(sdt, rsq = rsq)
      residual.p(fit)
      qq.p(fit)
    }
  } else if (x$method == "linear") {
    if (choose == FALSE) {

      par(mfrow = c(2, 3), mai = c(.4, .4, .3, .3), ps = 10, cex = 1, cex.main = 1)
      multi.p(dt, sdt) # full timeseries with lmfit
      sub.p(sdt, rsq = rsq) # closed-up (subset timeseries)
      rollreg.p(rolldt, rate) # rolling regression series with markline
      density.p(dens, peaks, pos) # density plot
      residual.p(fit) # residual plot
      qq.p(fit) # qq plot
    }
  }

  if (choose == 1) {
    multi.p(dt, sdt) # full timeseries with lmfit
    if (x$method == "interval") {
      abline(v = startint, lty = 3)
      abline(v = interval, lty = 3)
    }
  }
  if (choose == 2) sub.p(sdt, rsq = rsq)  # subset plot
  if (choose == 3) rollreg.p(rolldt, rate)  # rolling regression
  if (choose == 4) {
    if (x$method != 'linear') {
      stop('auto_rate: density plot not available for "highest", "lowest", "maximum", "minimum" and "interval" methods')
    } else density.p(dens, peaks, pos)  # density
  }
  if (choose == 5) residual.p(fit)  # residual plot
  if (choose == 6) qq.p(fit)  #qq plot

  if (label){
    cat("Done.\n")
    cat("-----------------------------------------\n")
  }

  return(invisible(x))
  on.exit(par(parorig)) # revert par settings to original

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
    if (object$method == "interval") cat("\n=== Summary of Results by Interval Order ===\n")
    if (object$method == "max") cat("\n=== Summary of Results by Maximum NEGATIVE Rate ===\n")
    if (object$method == "min") cat("\n=== Summary of Results by Minimum NEGATIVE Rate ===\n")
    out <- data.table(object$summary)
    print(out, nrows = 20)
  } else {
    # otherwise, return row specified by `pos`
    cat("\n=== Summary of Result: Rank ", pos, "of", length(object$rate), "===\n\n")
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


  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.auto_rate <- function(x, export = FALSE, ...){

  ## warning if empty
  if(length(x$rate) == 0) stop("auto_rate: No rates found in auto_rate x.")

  cat("\n# mean.auto_rate # ----------------------\n")
  if(length(x$rate) == 1) warning("auto_rate: Only 1 rate found in auto_rate x. Returning mean rate regardless...")
  n <- length(x$rate)
  out <- mean(x$rate)
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

