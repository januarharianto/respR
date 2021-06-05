#' Explore and visualise respirometry data and check for common errors
#'
#' `inspect` is a data exploration and preparation function that visualises
#' respirometry data and scans it for errors that may affect the use of further
#' functions in `respR`. It also subsets specified columns into a new `list`
#' object that can be used in subsequent functions, reducing the need for
#' additional inputs. Note, use of `inspect` to prepare data for the subsequent
#' functions is optional. All functions in `respR` can accept regular `R` data
#' objects including data frames, data tables, tibbles, vectors, etc. It is a
#' quality control and exploratory step to help users explore and prepare their
#' data prior to analysis.
#'
#' @details Given an input data frame, `x`, the function scans the `time` and
#'   `oxygen` columns. By default, it is assumed the first column is time data,
#'   and second is oxygen data, but the `time` and `oxygen` inputs can be used
#'   to specify different columns.
#'
#'   Time columns are checked for NA/NaN values, that values are sequential, for
#'   duplicate values, and that they are numerically evenly-spaced. Oxygen
#'   columns are only checked for NA/NaN data. See **Failed Checks** section for
#'   what it means for analyses if these checks produce warnings. Once data
#'   checks are complete, the function produces a list object which may be
#'   directly loaded (or `%>%` piped) into [calc_rate()], [calc_rate.bg()], and
#'   [auto_rate()] for further analyses.
#'
#' @section Plotting: A plot of the data is also produced (unless `plot =
#'   FALSE`), including a rolling regression plot, which calculates the rate of
#'   change in oxygen across a rolling window set using the `width` operator
#'   (default is `width = 0.1`, or 10% of the entire dataset). This plot
#'   provides a quick visual inspection of how the rate varies over the course
#'   of the experiment. This is for exploratory purposes only; later functions
#'   allow rate to be calculated over specific regions. Each rate value is
#'   plotted against the centre of the time window used to calculate them. Note
#'   that by default the rolling rates are plotted on a reverse y-axis, because
#'   oxygen uptake rates are returned as negative by `respR`. Therefore, higher
#'   oxygen uptake rates occur higher on this plot (more negative). If you are
#'   interested instead in oxygen production rates, which are positive, the
#'   `rate.rev = FALSE` argument can be passed in either the `inspect` call, or
#'   when using `plot()` on the output object. In this case, the rolling rate
#'   values will be plotted not-reversed, with higher oxygen *production* rates
#'   higher on the plot.
#'
#'   If axis labels obscure parts of the plot they can be suppressed using
#'   `legend = FALSE`. To suppress console output messages use `message =
#'   FALSE`. Lastly, if using `plot()` on the output object, a different
#'   `width` value can be passed than that used in the original call.
#'
#' @section Multiple Columns of Oxygen Data: For quick overview of larger
#'   experiments, multiple oxygen columns can be scanned for errors and plotted
#'   by using the `oxygen` argument to select multiple columns. These must
#'   *share the same time column*. In this case, data checks are performed,
#'   and a plot of each oxygen time series is produced, but no rolling rate plot
#'   is produced. All data are plotted on the same axis range of both time and
#'   oxygen (total range of data). This is chiefly exploratory functionality to
#'   allow for a quick overview of a dataset, and it should be noted that while
#'   the output `list` object will contain all columns in its `$dataframe`
#'   element, subsequent functions in `respR` (`calc_rate`, `auto_rate`, etc.)
#'   will by default only use the first two columns (`time`, and the first
#'   specified `oxygen` column). To analyse multiple columns and determine
#'   rates, best practice is to inspect and assign each time-oxygen column pair
#'   as separate `inspect` objects. See examples.
#'
#' @section Flowthrough Data: For flowthrough respirometry data, see the
#'   specialised [`inspect.ft()`] function.
#'
#' @section Failed Checks: It should be noted the data checks in `inspect` are
#'   mainly for exploratory purposes; they help diagnose and flag potential
#'   issues with the data. For instance, long experiments may have had sensor
#'   dropouts the user is unaware of. Others are not really issues at all. For
#'   instance, an uneven time warning can result from using decimalised minutes,
#'   which is a completely valid time metric, but happens to be numerically
#'   unevenly spaced. As an additional check, if uneven time is found, the
#'   minimum and maximum intervals in the time data are in the console output,
#'   so a user can see immediately if there are large gaps in the data.
#'
#'   If some of these checks fail, it should *generally* not hinder analysis of
#'   the data. `respR` has been coded to rely on linear regressions on exact
#'   data values, and not make assumptions about data spacing or order.
#'   Therefore issues such as missing or NA/NaN values, duplicate or
#'   non-sequential time values, or uneven time spacing should not cause any
#'   erroneous results, as long as they do not occur over large regions of the
#'   data. `inspect` however outputs locations (row numbers) of where these
#'   issues occur (located in the `$locs` element of the output), allowing users
#'   to amend them before analysis. We would recommend that to be completely
#'   confident in any results from analysis of such data, and avoid obscure
#'   errors, these issues be addressed before proceeding.
#'
#' @param x data.frame. Accepts any object of class `data.frame` (incl.
#'   `data.table`, `tibble`, etc.).
#' @param time numeric integer. Defaults to 1. Specifies the column number of
#'   the Time data.
#' @param oxygen numeric vector of integers. Defaults to 2. Specifies the column
#'   number(s) of the Oxygen data.
#' @param width numeric. Defaults to 0.1. Width used in the rolling regression
#'   plot as proportion of total length of data (0.01 to 1).
#' @param plot logical. Defaults to TRUE. Plots the data. If 2 columns selected,
#'   plots timeseries data, plus plot of rolling rate. If multiple columns,
#'   plots all timeseries data only.
#' @param ... Allows additional plotting controls to be passed. See **Plotting**
#'   section.
#'
#' @return Output is a `list` object of class `inspect`.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' ## By default, inspects first 2 columns:
#' inspect(sardine.rd)
#'
#' ## Adjust the width of the rolling rate plot:
#' inspect(sardine.rd, width = 0.2)
#'
#' ## Inspect specific columns in multicolumn datasets:
#' inspect(urchins.rd, time = 1, oxygen = 4)
#'
#' ## Inspect multiple columns in multicolumn datasets:
#' inspect(urchins.rd, time = 1, oxygen = c(11:19))
#'
#' ## Inspect oxygen production data, choose a width that gives
#' ## a better rolling rate, and use extra plotting options to
#' ## suppress legend, and ensure rates are *not* plotted reversed:
#' inspect(algae.rd, time = 1, oxygen = 2, width = 0.4,
#'         legend = FALSE, rate.rev = FALSE)
#'

inspect <- function(x, time = NULL, oxygen = NULL,
                    width = 0.1, plot = TRUE, ...) {

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
    message("inspect: Applying column default of 'oxygen = 2'")
    oxygen <- 2
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

  x_results <- check_timeseries(xval, "time")
  y_results <- check_timeseries(yval, "oxygen")

  # issue warnings
  if (any(unlist(x_results[[1]][1,])))
    warning("NA/NaN values detected in Time column.", call. = F)
  if (any(unlist(x_results[[1]][2,])))
    warning("Non-sequential Time values found.", call. = F)
  if (any(unlist(x_results[[1]][3,])))
    warning("Duplicate Time values found.", call. = F)
  if (any(unlist(x_results[[1]][4,])))
    warning("Time values are not evenly-spaced (numerically).", call. = F)
  if (any(unlist(y_results[[1]][1,])))
    warning("NA/NaN values detected in Oxygen column(s).", call. = F)

  # combine results
  checks <- cbind(x_results[[1]], y_results[[1]])
  locs <- cbind(x_results[[2]], y_results[[2]])

  # output
  ## rename columns:
  colnames(checks) <- c(names(df[time]), names(df[oxygen]))
  list <- lapply(1:ncol(locs), function(z) locs[, z])
  names(list) <- c(names(df[time]), names(df[oxygen]))

  # save new data frame and create output object
  dataframe <- data.table::data.table(cbind(df[time], df[oxygen]))

  out <- list(dataframe = dataframe,
              call = call,
              inputs = list(x = x,
                            time = time,
                            oxygen = oxygen,
                            width = width),
              checks = checks,
              locs_raw = locs,
              locs = list)

  class(out) <- "inspect"

  # if no errors occur, send out a good message :D
  if (!any(na.omit(unlist(checks)))){
    message("inspect: No issues detected while inspecting data frame.")
  } else {
    message("inspect: Data issues detected:")
    print(out)
  }

  if (plot) plot(out, message = FALSE, width = width, ...)

  return(out)
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

  # highlight locations that did not pass the tests (but only for 2-col dfs):
  if (checks[, 1][[1]]) {
    xnan <- locs[, 1][[1]]
    cat("NA/NaN Time data locations: ")
    if (length(xnan) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xnan, 20))
  }
  if (checks[, 1][[2]]) {
    xseq <- locs[, 1][[2]]
    cat("Non-sequential Time data locations ")
    if (length(xseq) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xseq, 20))
  }
  if (checks[, 1][[3]]) {
    xdup <- locs[, 1][[3]]
    cat("Duplicate Time data locations ")
    if (length(xdup) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xdup, 20))
  }
  if (checks[, 1][[4]]) {
    xevn <- locs[, 1][[4]]
    cat("Uneven Time data locations ")
    if (length(xevn) > 20) cat("(first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xevn, 20))
    cat("Minimum and Maximum intervals in uneven Time data: \n")
    print(range(diff(na.omit(x$dataframe[[1]]))))
  }

  for(i in 2:length(x$dataframe)) { ## for multiple columns
    if (checks[, i][[1]]) {
      ynan <- locs[, i][[1]]
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
plot.inspect <- function(x, message = TRUE, width = NULL,
                         legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if (message)
    cat("\n# plot.inspect # ------------------------\n")

  # extract data frame
  dt <- x$dataframe

  # if width is NULL use original one
  if(is.null(width)) width <- x$inputs$width

  if (length(dt) == 2) {

    ## general settings
    ## margins
    bt <- 0
    lf <- 0.5
    tp <- 0.6
    rt <- 0.3

    par(mfrow = c(2, 1),
        mai = c(bt, lf, tp, rt),
        ps = 10,
        cex = 1,
        cex.main = 1,
        mgp = c(0, 0.5, 0))

    ylim <- range(na.omit(dt[[2]]))
    buffer <- diff(ylim)*0.05
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    plot(dt[[1]],
         dt[[2]],
         xlab = "",
         ylab = "",
         ylim = ylim,
         pch = 16,
         cex = .5,
         axes = FALSE,
         col.lab = "blue",
         col.axis = "blue",
         panel.first = grid())

    axis(side = 2, las = 1, tck = 0, mgp = c(0, 0.3, 0))
    #title(xlab = "Time", line = 1)
    ## add row index axis
    par(new = TRUE)
    plot(seq(1, nrow(dt)),
         dt[[2]],
         xlab = "",
         ylab = "",
         pch = "",
         cex = .5,
         axes = FALSE)
    axis(side = 3, col.axis = "red")
    box()
    if(legend) legend("topright",
                      "Row Index",
                      text.col = "red",
                      bg = "gray90",
                      cex = 0.7)
    title(main = "Full Timeseries", line = 2)

    ## Adding this fn here to avoid using static_roll
    roll_reg_plot <- function(df, width) {
      roll_width <- floor(width * nrow(df))
      ## Calc all rates, even there is a min_obs of only 1 datapoint
      ## This means rate is returned even if there are NA in data
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
    xlim <- range(na.omit(xdt))

    ## now to get ylim, if width is higher than 0.1 we run roll_reg_plot with
    ## 0.1 width to get y range of rates, and plot against these. This means if
    ## a higher width is used, and rates have very little variation the plots
    ## don't look weird - i.e. should get flatter with higher widths
    if(width > 0.1) {
    rates01 <- roll_reg_plot(dt, 0.1)
    ylim <- range(na.omit(rates01))
    buffer <- diff(ylim)*0.05
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space
    } else {
      ylim <- range(na.omit(rates))
      buffer <- diff(ylim)*0.05
      ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space
    }

    if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

    par(mai = c(0.4, lf, 0.3, rt))
    plot((rates) ~ xdt[floor(half_width * length(xdt)):(floor(half_width * length(xdt)) + (length(rates) - 1))],
         xlim = xlim,
         ylim = ylim,
         # reversed axis
         xlab = "",
         ylab = "",
         pch = 16,
         cex = .5,
         axes = FALSE,)
    axis(side = 2, las = 1, tck = 0, mgp = c(0, 0.3, 0), cex.axis = 0.9)
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
    title(main = glue::glue("Rolling Rate  (Moving Window of {width} Width)"),
          line = 0.4)

    # Multi column plot -------------------------------------------------------

  } else {
    ## plot every column anyway - without rate plot
    message("inspect: Rolling Regression plot is only avalilable for a 2-column dataframe output.")
    par(mfrow = n2mfrow(length(dt) - 1),
        mai = c(0.3, 0.3, 0.2, 0.1),
        ps = 10,
        pch = 20,
        cex = 1,
        cex.main = 1,
        tck = -.05)

    ylim <- range(na.omit(x$dataframe[,-1])) ## so all on same axes
    buffer <- diff(ylim)*0.05
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    sapply(1:(length(dt) - 1), function(z) {
      plot(
        data.frame(dt[[1]], dt[[z + 1]]),
        ylim = ylim,
        xlab = "",
        ylab = "",
        cex = 0.5,
        col.lab = "blue",
        col.axis = "blue",
        panel.first = grid(),
        axes = FALSE
      )
      axis(side = 1, las = 1, tck = 0, col.axis = "blue", mgp = c(0, 0.3, 0))
      axis(side = 2, las = 1, tck = 0, col.axis = "black", mgp = c(0, 0.3, 0))
      box()
      title(main = glue::glue("Column: {names(dt)[z+1]}"), line = 0.3)}
    )
  }

  if (message){
    cat("Done.\n")
    cat("-----------------------------------------\n")
  }

  return(invisible(x))
}
