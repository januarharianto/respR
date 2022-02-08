#' Explore and visualise respirometry data and check for common errors
#'
#' `inspect()` is a data exploration and preparation function that visualises
#' respirometry data and checks it for errors that may affect the use of further
#' functions in `respR`. It also subsets specified columns into a new `list`
#' object that can be used in subsequent functions, reducing the need for
#' additional inputs. Note, use of `inspect` to prepare data for the subsequent
#' functions is optional. Functions in `respR` can accept regular `R` data
#' objects including data frames, data tables, tibbles, vectors, etc. It is a
#' quality control and exploratory step to help users view and prepare their
#' data prior to analysis.
#'
#' Given an input data frame, `x`, the function scans the `time` and `oxygen`
#' columns. If these are left `NULL`, by default it is assumed the first column
#' is time data, and all other columns are oxygen data. However, best practice
#' is to use the `time` and `oxygen` inputs to specify particular columns.
#'
#' ## Check for numeric data
#'
#' `respR` requires data be in the form of paired values of numeric time and
#' oxygen. All columns are checked that they contain numeric data before any
#' other checks are performed. If any of the inspected columns do not contain
#' numeric data the remaining checks for that column are skipped, and the
#' function exits returning `NULL`, printing the summary of the checks. No plot
#' is produced. Only when all inspected columns pass this numeric check can the
#' resulting output object be saved and passed to other `respR` functions.
#'
#' ## Other checks
#'
#' The `time` column is checked for missing (`NA/NaN`) values, infinite values
#' both positive and negative (`Inf/-Inf`), that values are sequential, that
#' there are no duplicate times, and that it is numerically evenly-spaced.
#' Oxygen columns are checked for missing (`NA/NaN`) and infinite values
#' (`Inf/-Inf`). See **Failed Checks** section for what it means for analyses if
#' these checks result in warnings. If the output is assigned, the specified
#' columns are saved to a `list` object for use in later functions such as
#' [`calc_rate()`] and [`auto_rate()`]. A plot is also produced.
#'
#' ## Plot
#'
#' A plot of the data is produced (unless `plot = FALSE`), of the data
#' timeseries, plus a rolling regression plot. This plot shows the rate of
#' change in oxygen across a rolling window specified using the `width` operator
#' (default is `width = 0.1`, or 10% of the entire dataset). This plot provides
#' a quick visual inspection of how the rate varies over the course of the
#' experiment. Regions of stable and consistent rates can be identified on this
#' plot as flat or level areas. This plot is for exploratory purposes only;
#' later functions allow rate to be calculated over specific regions. Each rate
#' value is plotted against the centre of the time window used to calculate it.
#'
#' ***Note:*** Since `respR` is primarily used to examine oxygen consumption,
#' the oxygen rate plot is by default plotted on a reverse y-axis. In `respR`
#' oxygen uptake rates are negative since they represent a negative slope of
#' oxygen against time. In these plots the axis is reversed so that higher
#' uptake rates (i.e. more negative) will be higher on these plots. If you are
#' interested instead in oxygen production rates, which are positive, the
#' `rate.rev = FALSE` input can be passed in either the `inspect` call, or when
#' using `plot()` on the output object. In this case, the rate values will be
#' plotted numerically, and higher oxygen *production* rates will be higher on
#' the plot.
#'
#' ## Plot an additional data source
#'
#' Using the `add.data` input an additional data source, for example
#' temperature, can be plotted alongside the oxygen timeseries. This input
#' should be an integer indicating a column in the input `x` data frame sharing
#' the same time data. None of the data checks are performed on this column; it
#' is simply to give a basic visual aid in the plot to, for example, help decide
#' if regions of the data should be used or not used because this parameter was
#' variable. It is saved in the output as a vector under `$add.data`. It is
#' plotted in blue on a separate y-axis on the main timeseries plot. It is *not*
#' plotted if multiple oxygen columns are inspected. See examples.
#'
#' ## Additional plotting options
#'
#' A different `width` value can be passed to see how it affects estimation of
#' the rolling rate. If axis labels obscure parts of the plot they can be
#' suppressed using `legend = FALSE`. Suppress console output messages with
#' `quiet = TRUE`. If multiple columns have been inspected, the `pos` input can
#' be used to examine each time~oxygen dataset. If axis labels (particularly
#' y-axis) are difficult to read, `las = 2` can be passed to make axis labels
#' horizontal, and  `oma` (outer margins, default `oma = c(0.4, 1, 1.5, 0.4)`)
#' or `mai` (inner margins, default `mai = c(0.3, 0.15, 0.35, 0.15)`) can be
#' used to adjust plot margins. See examples.
#'
#' ## Multiple Columns of Oxygen Data
#'
#' For a quick overview of larger datasets, multiple oxygen columns can be
#' inspected for errors and plotted by using the `oxygen` input to select
#' multiple columns (by default, all non-time columns are inspected as oxygen
#' columns if no `oxygen` input is specified). These must share the same `time`
#' column. In this case, data checks are performed, with a plot of each oxygen
#' time series, but no rolling rate plot is produced. All data are plotted on
#' the same axis range of both time and oxygen (total range of data). This is
#' chiefly exploratory functionality to allow for a quick overview of a dataset,
#' and it should be noted that while the output `inspect` object will contain
#' all columns in its `$dataframe` element, subsequent functions in `respR`
#' (`calc_rate`, `auto_rate`, etc.) will by default only use the first two
#' columns (`time`, and the first specified `oxygen` column). To analyse
#' multiple columns and determine rates, best practice is to inspect and assign
#' each time-oxygen column pair as separate `inspect` objects. See Examples.
#'
#' ## Flowthrough Respirometry Data
#'
#' For flowthrough respirometry data, see the specialised [`inspect.ft()`]
#' function.
#'
#' ## Failed Checks
#'
#' The most important data check in `inspect` is that all data columns are
#' numeric. If any column fails this check, the function skips the remaining
#' checks for that column, the function exits returning `NULL`, and no output
#' object or plot is produced.
#'
#' The other failed check that requires action is the check for infinite values
#' (`Inf/-Inf`). Some oxygen sensing systems add these in error when
#' interference or data dropouts occur. Infinite values will cause problems when
#' it comes to calculating rates, so need to be removed. If found, locations of
#' these are printed and can be found in the output object under `$locs`. Note,
#' these values are not plotted, so special note should be taken of the warnings
#' and console printout.
#'
#' The remaining data checks in `inspect` are mainly exploratory and help
#' diagnose and flag potential issues with the data that might affect rate
#' calculations. For instance, long experiments may have had sensor dropouts the
#' user is unaware of. Some might not be major issues. For instance, an uneven
#' time warning can result from using decimalised minutes, which is a completely
#' valid time metric, but happens to be numerically unevenly spaced. As an
#' additional check, if uneven time is found, the minimum and maximum intervals
#' in the time data are in the console output, so a user can see immediately if
#' there are large gaps in the data.
#'
#' If some of these checks produce warnings, it should *generally* not hinder
#' analysis of the data. `respR` has been coded to rely on linear regressions on
#' exact data values, and not make assumptions about data spacing or order.
#' Therefore issues such as missing or NA/NaN values, duplicate or
#' non-sequential time values, or uneven time spacing should not cause any
#' erroneous rate results, as long as they do not occur over large regions of
#' the data. `inspect` however outputs locations (row numbers) of where these
#' issues occur (located in the `$locs` element of the output), allowing users
#' to amend them before analysis. We would strongly recommend that to be
#' completely confident in any results from analysis of such data, and avoid
#' obscure errors, these issues be addressed before proceeding.
#'
#' ## Output
#'
#' Output is a `list` object of class `inspect`, with a `$dataframe` containing
#' the specified `time` and `oxygen` columns, inputs, and metadata which can be
#' passed to [`calc_rate()`] or [`auto_rate()`] to determine rates. If there are
#' failed checks or warnings, the row locations of the potentially problematic
#' data can be found in `$locs`.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `plot()`,
#' `print()` and `summary()`.
#'
#' - `plot()`: plots the result.
#'
#' - `print()`: prints a summary of the checks performed on the data. If issues
#' are found, locations (row numbers) are printed (up to first 20 occurrences).
#'
#' - `summary()`: simple wrapper for `print()` function. See above.
#'
#' @param x data.frame. Any object of class `data.frame` (incl. `data.table`,
#'   `tibble`, etc.).
#' @param time integer. Defaults to 1. Specifies the column number of the Time
#'   data.
#' @param oxygen integer or vector of integers. Defaults to all non-time columns
#'   if no other inputs given. Specifies the column number(s) of the Oxygen
#'   data.
#' @param width numeric, 0.01 to 1. Defaults to 0.1. Width used in the rolling
#'   regression plot as proportion of total length of data.
#' @param plot logical. Defaults to `TRUE`. Plots the data. If `time` and single
#'   `oxygen` columns selected, plots timeseries data, plus plot of rolling
#'   rate. If multiple `oxygen` columns, plots all timeseries data only.
#' @param add.data integer. Defaults to `NULL`. Specifies the column number of
#'   an optional additional data source that will be plotted in blue alongside
#'   the full oxygen timeseries.
#' @param ... Allows additional plotting controls to be passed, such as `legend
#'   = FALSE`, `quiet = TRUE`, `rate.rev = FALSE` and `pos`. A different `width`
#'   can also be passed in `plot()` commands on output objects.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' ## By default, inspects all columns, assuming time is in 1 and all others
#' ## are oxygen:
#' inspect(sardine.rd)
#'
#' ## Instead, specify time and oxygen columns
#' inspect(sardine.rd, time = 1, oxygen = 2)
#'
#' ## Use add.data input to plot an additional data type
#' ## (this column is not checked)
#' inspect(sardine.rd, time = 1, oxygen = 2, add.data = 3)
#'
#' ## Adjust the width of the rolling rate plot:
#' inspect(sardine.rd, 1, 2, width = 0.2)
#'
#' ## Inspect specific columns in multicolumn datasets:
#' inspect(urchins.rd, time = 1, oxygen = 4)
#'
#' ## Inspect multiple columns for a quick overview
#' ## of a large dataset:
#' inspect(urchins.rd, time = 1, oxygen = c(11:19))
#'
#' ## Inspect oxygen production data, use a width that gives
#' ## a better rolling rate, and use extra plotting options to
#' ## suppress legend, and ensure rates are plotted not reversed:
#' inspect(algae.rd, time = 1, oxygen = 2, width = 0.4,
#'         legend = FALSE, rate.rev = FALSE)
#'
#' ## Pass additional plotting inputs to override defaults and
#' ## allow better y-axis label visibility
#' inspect(sardine.rd, time = 1, oxygen = 2,
#'         las = 1, mai = c(0.3, 0.35, 0.35, 0.15))

inspect <- function(x, time = NULL, oxygen = NULL,
                    width = 0.1, plot = TRUE, add.data = NULL, ...) {

  ## Save function call for output
  call <- match.call()

  ## stop if not df
  if (!is.data.frame(x)) stop("inspect: 'x' must be data.frame object.")
  ## make sure df is df not dt
  df <- as.data.frame(x)

  ## time: apply default and validate
  if(is.null(time)) {
    message("inspect: Applying column default of 'time = 1'")
    time <- 1
  }
  column.val(time, req = TRUE, max = 1, min = 1,
             range = c(1,ncol(x)), conflicts = NULL,
             msg = "inspect: 'time' -")

  ## oxygen: Apply default
  if (is.null(oxygen)) {
    #message("inspect: Applying column default of 'oxygen = 2'")
    #oxygen <- 2
    message("inspect: Applying column default of all non-time column(s) as 'oxygen'")
    listcols <- seq.int(1, ncol(x))
    oxygen <- listcols[!listcols %in% time]
  }
  column.val(oxygen, req = TRUE, min = 1, max = Inf,
             range = c(1,ncol(x)), conflicts = time,
             msg = "inspect: 'oxygen' -")

  ## Multiple column message/warning
  if (length(oxygen) > 1)
    message("inspect: Multiple 'oxygen' columns selected. Note that subsequent functions in respR will by default use first oxygen column only.")

  ## width: apply default and validate
  if(is.null(width)) {
    width <- 0.1
    message("inspect: Applying default of 'width = 0.1'")
  }
  input.val(width, num = TRUE, int = FALSE, req = TRUE,
            max = 1, min = 1, range = c(0.01,1),
            msg = "inspect: 'width' -")

  # extract data
  xval <- lapply(1:length(df[time]), function(z) df[time][[z]])
  yval <- lapply(1:length(df[oxygen]), function(z) df[oxygen][[z]])
  if(!is.null(add.data)) {
    input.val(add.data, num = TRUE, int = TRUE, req = FALSE,
              max = 1, min = 1, range = c(1, ncol(df)),
              msg = "inspect: 'add.data' -")
    add.data <- df[[add.data]]
  }

  x_results <- check_timeseries(xval, "time")
  y_results <- check_timeseries(yval, "oxygen")

  # issue warnings
  if (any(unlist(x_results[[1]][1,])))
    warning("inspect: Time column not numeric. Other column checks skipped. \nData cannot be analysed by respR functions if not numeric. \nNo output returned.", call. = F)
  if (any(unlist(x_results[[1]][2,]) == "TRUE"))
    warning("inspect: Inf/-Inf values detected in Time column. Remove or replace before proceeding.", call. = F)
  if (any(unlist(x_results[[1]][3,]) == "TRUE"))
    warning("inspect: NA/NaN values detected in Time column.", call. = F)
  if (any(unlist(x_results[[1]][4,]) == "TRUE"))
    warning("inspect: Non-sequential Time values found.", call. = F)
  if (any(unlist(x_results[[1]][5,]) == "TRUE"))
    warning("inspect: Duplicate Time values found.", call. = F)
  if (any(unlist(x_results[[1]][6,]) == "TRUE"))
    warning("inspect: Time values are not evenly-spaced (numerically).", call. = F)

  if (any(unlist(y_results[[1]][1,])))
    warning("inspect: Oxygen column(s) not numeric. Other column checks skipped. \nData cannot be analysed by respR functions if not numeric. \nNo output returned.", call. = F)
  if (any(unlist(y_results[[1]][2,]) == "TRUE"))
    warning("inspect: Inf/-Inf values detected in Oxygen column(s). Remove or replace before proceeding.", call. = F)
  if (any(unlist(y_results[[1]][3,]) == "TRUE"))
    warning("inspect: NA/NaN values detected in Oxygen column(s).", call. = F)

  # combine results
  checks <- cbind(x_results[[1]], y_results[[1]])
  locs_raw <- cbind(x_results[[2]], y_results[[2]])

  # output
  ## rename columns:
  colnames(checks) <- c(names(df[time]), names(df[oxygen]))
  locs <- lapply(1:ncol(locs_raw), function(z) locs_raw[, z])
  names(locs) <- c(names(df[time]), names(df[oxygen]))

  # save new data frame and create output object
  dataframe <- data.table::data.table(cbind(df[time], df[oxygen]))

  out <- list(call = call,
              dataframe = dataframe,
              add.data = add.data,
              inputs = list(x = x,
                            time = time,
                            oxygen = oxygen,
                            width = width,
                            plot = plot,
                            add.data = add.data),
              checks = checks,
              locs_raw = locs_raw,
              locs = locs)

  class(out) <- "inspect"

  # if no errors occur, send out a good message :D
  if (!any(na.omit(unlist(checks) == "TRUE")))
    message("inspect: No issues detected while inspecting data frame.") else
      message("inspect: Data issues detected. For more information use print().")

  # Not all functions should print on assigning, but this one should
  print(out)

  # If any inspected columns found to be non-numeric, print, but don't return object
  # If this is the case, it can't be used in later fns anyway, so no point
  # And skip plotting - avoids awkward code in plot() to handle the object having non-numeric data
  # Warnings issued above.
  if(any(unlist(checks[1,]))) {
    return(invisible(NULL))
  } else {
    if (plot) plot(out, quiet = TRUE, width = width, ...)
    ## invisible prevents it printing twice on assigning
    return(invisible(out))
  }
}

#' @export
print.inspect <- function(x, ...) {
  cat("\n# print.inspect # -----------------------\n")
  checks <- x$checks
  locs <- x$locs_raw

  # rename content:
  tab <- checks
  tab[tab == "TRUE"] <- "WARN"
  tab[tab == "FALSE"] <- "pass"
  tab[is.na(tab)] <- "-"

  # print table
  print(as.data.frame(tab), quote = FALSE)
  cat("\n")

  # highlight locations that did not pass the tests:
  # No need to do this for non-numeric check
  # - if one is, they all are
  # Maybe we add a location
  #   - apply() as.numeric to each value to see if it works?
  # if (checks[, 1][[1]]) {
  #   xnan <- locs[, 1][[1]]
  #   cat("Non-numeric Time data locations: ")
  #   if (length(xnan) > 20) cat("(first 20 shown) ")
  #   cat("in column:", names(x$dataframe)[1], "\n")
  #   print(head(xnan, 20))
  # }
  if (checks[, 1][[2]] != "skip" && checks[, 1][[2]]) {
    xinf <- locs[, 1][[2]]
    cat("Inf/-Inf Time data locations: ")
    if (length(xinf) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xinf, 20))
  }
  if (checks[, 1][[3]] != "skip" && checks[, 1][[3]]) {
    xnan <- locs[, 1][[3]]
    cat("NA/NaN Time data locations: ")
    if (length(xnan) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xnan, 20))
  }
  if (checks[, 1][[4]] != "skip" && checks[, 1][[4]]) {
    xseq <- locs[, 1][[4]]
    cat("Non-sequential Time data locations ")
    if (length(xseq) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xseq, 20))
  }
  if (checks[, 1][[5]] != "skip" && checks[, 1][[5]]) {
    xdup <- locs[, 1][[5]]
    cat("Duplicate Time data locations ")
    if (length(xdup) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xdup, 20))
  }
  if (checks[, 1][[6]] != "skip" && checks[, 1][[6]]) {
    xevn <- locs[, 1][[6]]
    cat("Uneven Time data locations ")
    if (length(xevn) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xevn, 20))
    cat("Minimum and Maximum intervals in uneven Time data: \n")
    print(range(diff(nainf.omit(x$dataframe[[1]]))))
  }

  for(i in 2:ncol(checks)) { ## for multiple columns
    #Again, no need to do this for non-numeric - all or none
    #Can delete this in future
    # if (checks[, i][[1]]) {
    #   ynan <- locs[, i][[1]]
    #   cat("Non-numeric locations ")
    #   if (length(ynan) > 20) cat("(first 20 shown) ")
    #   cat("in Oxygen column:", names(x$dataframe)[i], "\n")
    #   print(head(ynan, 20))
    # }
    if (checks[, i][[2]] != "skip" && checks[, i][[2]]) {
      yinf <- locs[, i][[2]]
      cat("Inf/-Inf locations ")
      if (length(yinf) > 20) cat("(first 20 shown) ")
      cat("in Oxygen column:", names(x$dataframe)[i], "\n")
      print(head(yinf, 20))
    }
    if (checks[, i][[3]] != "skip" && checks[, i][[3]]) {
      ynan <- locs[, i][[3]]
      cat("NA/NaN locations ")
      if (length(ynan) > 20) cat("(first 20 shown) ")
      cat("in Oxygen column:", names(x$dataframe)[i], "\n")
      print(head(ynan, 20))
    }
  }

  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' @export
summary.inspect <- function(object, ...) {
  print(object)
}

#' @export
plot.inspect <- function(x, width = NULL, pos = NULL, quiet = FALSE,
                         legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # number of oxygen columns
  nres <- ncol(x$dataframe)-1

  ## if pos = NULL, plot all oxygen columns
  if(is.null(pos)) pos <- 1:nres
  if(any(pos > nres))
    stop("plot.inspect: Invalid 'pos' rank: only ", nres, " oxygen columns found.")

  if (!quiet){
    cat("\n# plot.inspect # ------------------------\n")
    cat("Plotting inspected columns ...\n")
  }

  # extract data frame - pos columns (plus 1 as time is always col 1)
  dt <- as.data.frame(x$dataframe)
  dt <- dt[,c(1,pos+1)]

  # if width is NULL use original one
  if(is.null(width)) width <- x$inputs$width

  # Apply default plotting params
  par(oma = oma_def,
      # this one needs more space at top for two panel plot
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = cex_def)

  # single dataset ----------------------------------------------------------
  if (length(dt) == 2) {

    par(mfrow = c(2, 1),
        ps = 10,
        cex = 1,
        cex.main = 1,
        ...)

    plot(dt[[1]],
         dt[[2]],
         xlab = "",
         ylab = "",
         ylim = grDevices::extendrange(nainf.omit(dt[[2]]), f = 0.05),
         cex = .5,
         axes = FALSE,
         col.lab = "blue",
         col.axis = "blue",
         panel.first = grid())

    axis(side = 2)

    ## add row index axis
    par(new = TRUE, ...)
    plot(seq(1, nrow(dt)),
         dt[[2]],
         xlab = "",
         ylab = "",
         pch = "",
         cex = .5,
         axes = FALSE)
    axis(side = 3, col.axis = "red")

    # if add.data plot it too
    if(!is.null(x$add.data)) {
      par(new = TRUE, mgp = c(0, 0.13, 0))
      plot(seq(1, nrow(dt)),
           x$add.data,
           ylim = grDevices::extendrange(x$add.data, f = 1),
           xlab = "",
           ylab = "",
           pch = ".",
           cex = .5,
           axes = FALSE,
           col = "blue")
      axis(side = 4, col.axis = "blue")
      par(mgp = mgp_def) # set back default mgp
      par(...) # or allow custom one to overwrite it
    }

    box()
    if(legend) legend("topright",
                      "Row Index",
                      text.col = "red",
                      bg = "gray90",
                      cex = 0.7)
    mtext("Full Timeseries",
          outer = TRUE, cex = 1.2, line = 0.3, font = 2)

    ## Adding this fn here to avoid using static_roll
    roll_reg_plot <- function(df, width) {
      roll_width <- floor(width * nrow(df))
      ## Calc all rates, even there is a min_obs of only 1 datapoint
      ## This means rate is returned even if there are NA in data
      ## Also replace Inf with NA of ylim in plot fails
      df[[1]][which(is.infinite(df[[1]]))] <- NA
      df[[2]][which(is.infinite(df[[2]]))] <- NA
      rates <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]),
                             roll_width, min_obs = 1)$coefficients[,2]
      ## However this means rates are ALSO calculated at the start of the data
      ## before the width is even reached, so we remove these.
      rates <- rates[-(1:(roll_width-1))]
      return(rates)
    }

    ## Rolling reg plot
    ## Width needs to be half on each side
    rates <- roll_reg_plot(dt, width)
    half_width <- width/2
    xdt <- dt[[1]]
    xlim <- range(nainf.omit(xdt))

    ## now to get ylim, if width is higher than 0.1 we run roll_reg_plot with
    ## 0.1 width to get y range of rates, and plot against these. This means if
    ## a higher width is used, and rates have very little variation the plots
    ## don't look weird - i.e. should get flatter with higher widths
    if(width > 0.1) {
      rates01 <- roll_reg_plot(dt, 0.1)
      y_lim <- range(nainf.omit(rates01))
      buffer <- diff(y_lim)*0.05
      y_lim <- c(y_lim[1] - buffer, y_lim[2] + buffer) ## add a little more space
    } else {
      y_lim <- range(nainf.omit(rates))
      buffer <- diff(y_lim)*0.05
      y_lim <- c(y_lim[1] - buffer, y_lim[2] + buffer) ## add a little more space
    }

    if(rate.rev) y_lim <- rev(y_lim) ## reverse y-axis

    plot((rates) ~ xdt[floor(half_width * length(xdt)):(floor(half_width * length(xdt)) + (length(rates) - 1))],
         xlim = xlim,
         ylim = y_lim,
         # reversed axis
         xlab = "",
         ylab = "",
         cex = .5,
         col = r2,
         axes = FALSE,)
    axis(side = 2, cex.axis = 0.9)
    # to put yaxis label colour back to black
    axis(side = 1, col.lab = "blue", col.axis = "blue")
    ## Added dashed line at rate = 0 - for when rates are +ve and -ve
    abline(h = 0, lty = 2)
    grid()
    box()
    if(legend) legend("bottomleft",
                      "Time",
                      text.col = "blue",
                      bg = "gray90",
                      cex = 0.7)
    mtext("Rolling Rate",
          outer = FALSE, cex = 1.2, line = 1.2, font = 2)
    mtext(glue::glue("(moving window of {width} width)"),
          outer = FALSE, cex = 1, line = 0.3, font = 2)

    # Multi column plot -------------------------------------------------------

  } else {
    ## plot every column anyway - without rate plot
    if(!quiet)
      message("plot.inspect: Rolling Regression plot is only avalilable for a 2-column dataframe output.")
    if(!quiet && !is.null(x$add.data))
      message("plot.inspect: Additional data source cannot be plotted for multiple columns.")

    par(mfrow = n2mfrow(length(dt) - 1),
        # put back to default mai for multi plot so column titles are visible
        mai = mai_def,
        ps = 10,
        cex = 1,
        cex.main = 1,
        ...)

    sapply(1:(length(dt) - 1), function(z) {

      plot(data.frame(dt[[1]], dt[[z + 1]]),
           ylim = grDevices::extendrange(nainf.omit(x$dataframe[,-1]), f = 0.05),
           xlab = "",
           ylab = "",
           cex = 0.5,
           col.lab = "blue",
           col.axis = "blue",
           panel.first = grid(),
           axes = FALSE)

      box()
      axis(side = 1, col.axis = "blue")
      axis(side = 2, col.axis = "black")

      # plot invisibly - to add row index x-axis
      par(new = TRUE, ...)
      plot(data.frame(1:length(dt[[1]]), dt[[z + 1]]),
           xlab = "",
           ylab = "",
           pch = "",
           cex = .5,
           axes = FALSE
      )
      axis(side = 3, col.axis = "red")

      title(main = glue::glue("Column: {names(dt)[z+1]}"), line = 1.2,
            adj = 0)
    }
    )
    mtext("inspect: Inspecting Selected Columns",
          outer = TRUE, cex = 1.2, line = 0.3, font = 2)
  }

  if (!quiet){
    cat("-----------------------------------------\n")
  }

  return(invisible(x))
}
