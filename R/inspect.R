#' Explore and visualise respirometry data and check for common errors
#'
#' `inspect` is a data exploration and preparation function that visualises
#' respirometry data and scans it for errors that may affect the use of various
#' functions in `respR`. It also subsets specified columns into a new `list`
#' object that can be used in subsequent functions, reducing the need for
#' additional inputs. Note, use of `inspect` to prepare data for the subsequent
#' functions is *completely optional*. All functions in `respR` can accept
#' regular `R`` data objects including data frames, data tables, tibbles,
#' vectors, etc. It is a quality control and exploratory step to help users
#' explore and prepare their data prior to analysis.
#'
#' For the input data frame, the function scans the specified `time` and
#' `oxygen` columns. By default, it is assumed the first column is time data,
#' and second is oxygen data, but different columns can be specified using the
#' `time` and `oxygen` column identification inputs. A plot of the data is also
#' produced (unless `plot = FALSE`), including a rolling regression plot, which
#' calculates the rate of change in oxygen across a rolling window set using the
#' `width` operator (default is `width = 0.1`, or 10% of the entire dataset).
#' This plot provides a quick visual inspection of the how the rate changes (or
#' is stable) over the course of the experiment. The `width` can be changed to
#' examine how this affects rate calculations. Rates are plotted against the
#' middle timepoint of the respective regression. Note that rates are plotted on
#' a reverse axis because oxygen uptake rates are returned as negative by
#' `respR`. Therefore, higher oxygen uptake rates are higher on the rate plot
#' (more negative). Note, if examining oxygen *production*, higher rates will be
#' *lower* on this plot.
#'
#' Time columns are checked for NA/NaN values, sequential time, duplicate time
#' and evenly-spaced time data. Oxygen columns are simply checked for NA/NaN
#' data. See **Failed Checks** section for what it means for analyses if these
#' checks are failed. Once data checks are complete, the function produces a
#' list object which may be directly loaded into [calc_rate()],
#' [calc_rate.bg()], [calc_rate.ft()], and [auto_rate()] for further analyses.
#'
#' @section Multiple Columns of Oxygen Data: For quick overview of larger
#'   experiments, multiple oxygen columns can be scanned for errors and plotted
#'   using the `oxygen` argument to select multiple columns of a data frame (or
#'   `NULL` for all columns). These must *share the same `time` column*. In this
#'   case a plot of each oxygen time series is produced, but no rolling rate
#'   plot is produced. All data are plotted on the same axis range of both time
#'   and oxygen (total range of data). This is chiefly exploratory functionality
#'   to give a quick overview of a dataset, and it should be noted that while
#'   the output `list` object will contain all columns in its `$dataframe`
#'   element, subsequent functions in `respR` (`calc_rate`, `auto_rate`, etc.)
#'   will by default only use the first two columns (`time`, and the first
#'   specified `oxygen` column). Other columns of oxygen data contained in
#'   `inspect` output objects can only be used in these functions by calling the
#'   data frame columns directly as the input (e.g. `data$dataframe[,c(1,3)])`).
#'   Best practice is to inspect and assign each oxygen column individually as
#'   separate `inspect` objects. See examples.
#'
#' @section Flowthrough Data: Flowthrough respirometry data, which usually
#'   contains oxygen values for inflow and outflow channels, can also be
#'   inspected by specifying column IDs, e.g. `oxygen = c(2,3)`. See examples.
#'
#' @section Failed Checks: It should be noted the data checks in `inspect` are
#'   mainly for exploratory purposes; they help diagnose and flag potential
#'   issues with the data. For instance, very long experiments could have had
#'   sensor dropouts the user is completely unaware of. Others are not really
#'   issues at all. For instance, an uneven time warning can result from using
#'   decimalised minutes, which is a completely valid time metric, but happens
#'   to be numerically unevenly spaced. As an additional check, if uneven time
#'   is found, the minimum and maximum intervals in the time data are in the
#'   console output, so a user can see immediately if there are large gaps in
#'   the data.
#'
#'   If some of these checks fail, it should *generally* not hinder analysis of
#'   the data. `respR` has been coded to rely on linear regressions on exact
#'   data values, and not make assumptions about data spacing or order.
#'   Therefore issues such as missing or NA/NaN values, duplicate or
#'   non-sequential time values, or uneven time spacing should not cause any
#'   erroneous results, as long as they do not occur over large regions of the
#'   data. `inspect` however outputs locations (row numbers) of where these
#'   issues occur, allowing users to amend them before analysis. We would
#'   recommend that to be completely confident in any results from analysis of
#'   such data, these issues be addressed before proceeding.
#'
#' @param df data.frame object. Accepts any object of class `data.frame`.
#' @param time numeric integer. Defaults to 1. Specifies the column number of
#'   the Time data.
#' @param oxygen numeric vector of integers. Defaults to 2. Specifies the column
#'   number(s) of the Oxygen data. If NULL all columns will be inspected.
#' @param width numeric. Defaults to 0.1. Width used in the rolling regression
#'   plot as proportion of total length of data (0 to 1).
#' @param plot logical. Defaults to TRUE. Plots the data. If 2 columns selected,
#'   plots timeseries data plus a plot of rolling rate. If multiple columns,
#'   plots all timeseries data only.
#'
#' @return A list object of class `inspect`.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' # By default, inspects first 2 columns:
#' inspect(sardine.rd)
#'
#' # Adjust the width of the rolling rate plot:
#' inspect(sardine.rd, width = 0.2)
#'
#' # Inspect specific columns in multicolumn datasets:
#' inspect(urchins.rd, time = 1, oxygen = 4)
#'
#' # Inspect and assign multiple columns in multicolumn datasets:
#' x <- inspect(urchins.rd, time = 1, oxygen = c(2:9))
#' print(x)
#'
#' # Check position of errors in data frame
#' x$list$time.min
#'
#' # Inspect flowthrough data
#' x <- inspect(flowthrough.rd, 1, c(2,3))
#' x
inspect <- function(df, time = 1, oxygen = 2, width = 0.1, plot = TRUE) {

  #"%!in%" <- function(x, y) !("%in%"(x, y))

  # validate inputs
  ## set default values if NULL, which selects first column as time, and
  ## everything else as oxygen
  if (is.null(time)) time <- 1

  ## if only time is provided, assume check is for all data
  if (is.null(oxygen)) {
    listcols <- seq.int(1, ncol(df))
    oxygen <- listcols[!listcols %in% 1]
  }

  if (length(oxygen) > 2) message("Multiple `oxygen` columns selected. Note subsequent functions will by default use only first oxygen column.")
  if (!is.data.frame(df)) stop("`df` must be data.frame object.")
  if (!(time %% 1 == 0)) stop("`time` column: must be numeric integer.")
  if (any(!(oxygen %% 1 == 0))) stop("`oxygen` column(s): must be numeric integers.")
  if (width <= 0 || width >= 1) stop("`width` must be between 0 and 1.")
  # more validations in a bit

  df <- as.data.frame(df)

  # extract data
  x <- lapply(1:length(df[time]), function(y) df[time][[y]])
  y <- lapply(1:length(df[oxygen]), function(y) df[oxygen][[y]])

  # validate data by type
  x_results <- respR:::check_timeseries(x, "time")
  y_results <- respR:::check_timeseries(y, "oxygen")

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

  # if no errors occur, send out a good message :D
  if (!any(na.omit(unlist(checks)))) {
    message("No issues detected while inspecting data frame.")
  }

  # save new data frame and create output object
  dataframe <- data.table(cbind(df[time], df[oxygen]))
  out <-
    list(dataframe = dataframe, checks = checks, list_raw = locs, list = list)
  class(out) <- "inspect"

  if (plot) plot(out, label = FALSE, width = width)

  return(out)
}

#' @export
print.inspect <- function(x, ...) {
  cat("\n# inspect # -----------------------------\n")
  checks <- x$checks
  locs <- x$list_raw

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
    if (length(xnan) > 20) cat(" (first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xnan, 20))
  }
  if (checks[, 1][[2]]) {
    xseq <- locs[, 1][[2]]
    cat("Non-sequential Time data locations ")
    if (length(xseq) > 20) cat(" (first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xseq, 20))
  }
  if (checks[, 1][[3]]) {
    xdup <- locs[, 1][[3]]
    cat("Duplicate Time data locations ")
    if (length(xdup) > 20) cat(" (first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xdup, 20))
  }
  if (checks[, 1][[4]]) {
    xevn <- locs[, 1][[4]]
    cat("Uneven Time data locations")
    if (length(xevn) > 20) cat(" (first 20 shown) ")
    cat("in column:", names(x$dataframe)[1], "\n")
    print(head(xevn, 20))
    cat("Minimum and Maximum intervals in uneven Time data: \n")
    print(range(diff(na.omit(x$dataframe[[1]]))))
  }
  if ((length(x$dataframe)) == 2) {
    if (checks[, 2][[1]]) {
      ynan <- locs[, 2][[1]]
      cat("NA/NaN locations ")
      if (length(ynan) > 20) cat(" (only first 20 shown) ")
      cat("in Oxygen column:", names(x$dataframe)[2], "\n")
      print(head(ynan, 20))
    }
  }
  return(invisible(x))
}



#' @export
plot.inspect <- function(x, label = TRUE, width = 0.1, ...) {
  if (label)
    cat("\n# plot.inspect # ------------------------\n")

  pardefault <- par(no.readonly = T) # save original par settings

  # extract data frame
  dt <- x$dataframe
  # perform rolling regression (quick one)
  if (ncol(dt) == 2) {
    ## changed here 0.2 to 0.1
    roll <- static_roll(dt, floor(width * nrow(dt)))$rate_b1
  }

  if (length(x$dataframe) == 2) {
    par(
      mfrow = c(2, 1),
      mai = c(0.4, 0.4, 0.6, 0.3),
      ps = 10,
      cex = 1,
      cex.main = 1,
      mgp = c(0, 0.5, 0)
    ) # mid = to put tick labels closer to ticks
    plot(
      dt[[1]],
      dt[[2]],
      xlab = "",
      ylab = "",
      pch = 16,
      cex = .5,
      col.lab = "blue",
      col.axis = "blue",
      panel.first = grid()
    )
    axis(side = 2) # simply to put yaxis label colour back to black
    #title(xlab = "Time", line = 1)
    ## add row index axis
    par(new = TRUE)
    plot(
      seq(1, nrow(dt)),
      dt[[2]],
      xlab = "",
      ylab = "",
      pch = 16,
      cex = .5,
      axes = FALSE
    )
    axis(side = 3, col.axis = "red")
    legend(
      "bottomleft",
      "Time",
      text.col = "blue",
      bg = "gray90",
      cex = 0.7
    )
    legend(
      "topright",
      "Row Index",
      text.col = "red",
      bg = "gray90",
      cex = 0.7
    )
    title(main = "Full Timeseries", line = 2)

    ## Rolling reg plot
    ## Width needs to be half on each side

    ## putting this here to stop using static_roll
    roll_reg_plot <- function(df, width) {

      roll_width <- floor(width * nrow(df))
      roll <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]), roll_width, min_obs = 1)$coefficients[,2]

      roll <- roll[-(1:roll_width)]
      #length((rev(roll[-(1:roll_width)])))

      #plot(rev(roll[-(1:roll_width)]))

      return(roll)
    }

    roll <- roll_reg_plot(x$dataframe, width)
    half_width <- width/2
    xdt <- dt[[1]]
    plot((roll) ~ xdt[floor(half_width * length(xdt)):(floor(half_width * length(xdt)) + (length(roll) - 1))],
         xlim = range(na.omit(xdt)),
         ylim = rev(range(roll)),
         # reversed axis
         xlab = "",
         ylab = "",
         pch = 16,
         cex = .5,
         col.lab = "blue",
         col.axis = "blue"
    )
    axis(side = 2) # simply to put yaxis label colour back to black
    ## Added dashed line at rate = 0
    abline(h = 0, lty = 2)
    par(new = TRUE)
    plot(
      seq(1, nrow(dt)),
      dt[[2]],
      xlab = "",
      ylab = "",
      pch = "",
      cex = .5,
      axes = FALSE,
      col = "white"
    ) # plot invisibly
    axis(side = 3, col.axis = "red")
    legend(
      "bottomleft",
      "Time",
      text.col = "blue",
      bg = "gray90",
      cex = 0.7
    )
    legend(
      "topright",
      "Row Index",
      text.col = "red",
      bg = "gray90",
      cex = 0.7
    )
    title(main = glue::glue("Rolling Regression of Rate ({width} Rolling Window)"), line = 2)

  } else {
    ## plot every column anyway - without rate plot
    message("inspect: Rolling Regression plot is only avalilable for a 2-column dataframe output.")
    par(
      mfrow = n2mfrow(length(x$dataframe) - 1),
      mai = c(0.3, 0.3, 0.2, 0.1),
      ps = 10,
      pch = 20,
      cex = 1,
      cex.main = 1,
      tck = -.05
    )

    ylim <- range(x$dataframe[,-1]) ## so all on same axes
    buffer <- diff(ylim)*0.05
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    lapply(1:(length(x$dataframe) - 1), function(z)
    {plot(
      data.frame(x$dataframe[[1]], x$dataframe[[z + 1]]),
      mgp = c(0, 0.5, 0),
      ylim = ylim,
      xlab = "",
      ylab = "",
      cex = 0.5,
      col.lab = "blue",
      col.axis = "blue",
      panel.first = grid()
    )
      axis(side = 2, col = "black") # simply to put yaxis label colour back to black
      title(main = glue::glue("Column: {names(x$dataframe)[z+1]}"), line = 0.3)}

    )
  }
  on.exit(par(pardefault))  # revert par settings to original

  if (label)
    cat("Done.\n")
  return(invisible(x))
}
