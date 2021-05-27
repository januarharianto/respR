#' Explore and visualise flowthrough respirometry data and check for errors
#'
#' `inspect.ft` is a data exploration and preparation function that visualises
#' flowthrough respirometry data, scans it for common issues, and prepares it
#' for use in later functions in `respR`, such as [calc_rate.ft()].
#'
#' `inspect.ft` is intended to be specific to *flowthrough* respirometry data.
#' In flowthrough respirometry (also known as 'open flow' 'open system' or
#' 'continuous flow' respirometry) rather than calculating a rate from a
#' changing oxygen concentration recording in a sealed chamber, instead the
#' difference (i.e. 'delta') between the inflow and outflow concentrations of a
#' respirometer receiving water at a constant flow rate is used to calculate an
#' oxygen consumption or production rate, typically after it has reached a
#' steady state. Therefore, in general, regions of stable oxygen delta values
#' (difference between outflow and inflow oxygen) are of interest. `inspect.ft`
#' inspects, visualises, and prepares the data for use in [calc_rate.ft()]. By
#' specifying data types in this function and saving the output, they do not
#' need to be specified in later functions.
#'
#' `inspect.ft` requires at least two data inputs; a single column of numeric
#' `time` data, with *either* a column of paired `out.o2` concentrations (i.e.
#' the exhalent or 'downstream' concentrations), *or* a column of already
#' calculated `delta.o2` values, that is the difference between outflow and
#' inflow concentrations, or the outflow concentration corrected by a background
#' recording from a 'blank' or empty chamber. If an `out.o2` column has been
#' specified, in order to calculate the oxygen delta (and therefore a rate in
#' [calc_rate.ft()]) there must also be an inflow oxygen concentration input
#' (i.e. the inhalent or 'upstream' concentration). This will generally be a
#' column of paired `in.o2` concentrations, in which case the paired values of
#' `out.o2` and `in.o2` are used to calculate the oxygen `delta.o2`, which is
#' saved in the output, and used to determine a rate in [calc_rate.ft()].
#' Alternatively, if the inflow oxygen concentration is a known, generally
#' constant concentration (such as fully air-saturated water from a header tank)
#' this can be entered as a single value via `in.o2.value` and this is used to
#' calculate the `delta.o2`.
#'
#' ***Data processing and validation checks***
#'
#' `inspect.ft` subsets and classifies the specified columns into a new `list`
#' object that can be used in subsequent functions, reducing the need for
#' additional inputs. Note, use of `inspect.ft` to prepare data for the
#' subsequent functions is optional. Most functions in `respR` accept regular
#' `R`` data objects including data frames, data tables, tibbles, vectors, etc.
#' Its purpose is quality control, data visualisation and exploration to assist
#' users in exploring and preparing their data prior to analysis.
#'
#' Given an input data frame (`df`), the function scans the columns specified
#' via the `time`, `out.o2`, `in.o2` or `delta.o2` inputs. If no columns are
#' specified, by default the functions assumes the first column is `time`, and
#' all others are `delta.o2` oxygen data.
#'
#' The `time` column is checked for missing (NA/NaN) values, that values are
#' sequential, that there are no duplicate times, and that it is numerically
#' evenly-spaced. Oxygen columns are simply checked for NA/NaN data. See
#' **Failed Checks** section for what it means for analyses if these checks
#' result in warnings. The function outputs a `list` object which, if saved, can
#' be directly loaded (or `%>%` piped) into [calc_rate.ft()] for determining
#' rates.
#'
#' ***Plot***
#'
#' A plot of the data is produced (unless `plot = FALSE`), depending on the
#' inputs:
#'
#' - a single `out.o2` column with either a paired `in.o2` column or a
#' `in.o2.value`: a two panel plot. The top plot is both outflow (black points)
#' and inflow (grey points) oxygen against both time (bottom, blue axis) and row
#' index (top, red axis). The bottom plot is the oxygen delta between outflow
#' and inflow oxygen, a proxy for oxygen uptake or production rate.
#'
#' - a single `delta.o2` column: a one panel plot of oxygen delta values against
#' time.
#'
#' - multiple `out.o2` or `delta.o2` columns: a grid plot of all `delta.o2` data
#' (either as entered or calculated from `out.o2` and `in.o2`). Specific delta
#' plots can be examined as a single plot by using the `pos` input. Y-axes are
#' not equal. Chiefly intended to give a quick overview of large datasets. For
#' further analyses, we strongly recommend inspecting and saving each particular
#' experiment individually. See examples.
#'
#' - unspecified columns: all columns are plotted assuming `time` is in column
#' 1, and all others are oxygen `delta.o2` data. Y-axes are not equal.
#'
#' In delta plots, that is those plotting `delta.o2` values, either directly
#' entered or calculated, consistent oxygen uptake or production rates will be
#' seen as flat or level regions.
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
#' the output object.
#'
#' ***Multiple data columns***
#'
#' For a quick overview of larger experiments, multiple columns of `out.o2`,
#' `in.o2` and `delta.o2` can be inspected, but must share the same numeric time
#' data column specified by the `time` input. Note, multiple column inspection
#' is chiefly intended to give a quick overview larger datasets. Subsequent
#' functions such as [calc_rate.ft()] will use only the first `delta.o2` columns
#' for calculating rates. For further analyses, we strongly recommend inspecting
#' and saving each particular experiment individually. See examples.
#'
#' If multiple `out.o2` columns are specified, `in.o2` can be a single column
#' (if for example all chambers are supplied from the same header tank), in
#' which case it is used to calculate an oxygen delta for all `out.o2` columns.
#' A single `in.o2.value` value in the same units as `out.o2` can also be
#' specified. There can also be multiple `in.o2` columns, in which case it is
#' assumed each `out.o2` column is paired with each `in.o2` at the same
#' position, and used to calculate the oxygen `delta.o2`. Therefore, they must
#' have the same number of columns.
#'
#' Regardless of which inputs are used, multiple columns results in a plot of
#' each `delta.o2` time series. This is chiefly exploratory functionality to
#' give a quick overview of a dataset. While the output will contain all data
#' columns in `$dataframe` and `$input_data`, subsequent functions such as
#' [calc_rate.ft()], will only use the first inspected data set (`time`, and the
#' first `delta.o2`) to calculate rates. Best practice is to inspect and assign
#' each individual experiment or column pair as separate `inspect.ft` objects.
#' See examples.
#'
#' ***Failed Checks***
#'
#' Note that the data checks in `inspect.ft` are mainly for exploratory
#' purposes; they help diagnose and flag potential issues with the data. For
#' instance, very long experiments could have had sensor dropouts the user is
#' unaware of. Some issues, are not really consequential. For instance, an
#' uneven time warning can result from using decimalised minutes as the time
#' metric, which happens to be numerically unevenly spaced, but works perfectly
#' fine as the time data. If gaps in time data are found, the minimum and
#' maximum intervals are shown in the console output, so a user can see
#' immediately if there are large gaps in the data.
#'
#' If some of these checks fail, it should *generally* not hinder analysis of
#' the data. `respR` has been coded to rely on linear regressions on exact data
#' values, and not make assumptions about data spacing or order. Therefore
#' issues such as missing or NA/NaN values, duplicate or non-sequential time
#' values, or uneven time spacing should not cause any erroneous results, as
#' long as they are genuine datapoints and do not occur over large regions of
#' the data. `inspect.ft` however outputs locations (row numbers) of where these
#' issues occur, allowing users to amend or examine them before analysis. We
#' would recommend that to be completely confident in any results from analysis
#' of such data, these issues be considered before proceeding.
#'
#' ***Background control or "blank" experiments***
#'
#' For experiments in which the specimen data is to be background corrected by a
#' concurrently-run control experiment, `inspect.ft` can be used by specifying
#' the specimen experiment as `out.o2`, and the "blank" as the `in.o2` input. In
#' this way, any variations in oxygen in the specimen data due to background
#' microbial activity, or for any other reason such as fluctuations in inflow
#' oxygen, are accounted for in the delta oxygen calculations, and therefore in
#' the rate calculated in `[calc_rate.ft()]`. See the vignettes on the website
#' for examples.
#'
#' If the background recordings are experiments with their own outflow and
#' inflow recordings, which show a generally consistent oxygen delta due to
#' microbial activity, this can be saved as a separate `inspect.ft` object, a
#' background rate calculated in `[calc_rate.ft()]`, and this used in
#' `[adjust_rate.ft()]` as the `by` input to perform background adjustments to
#' specimen rates.
#'
#' **Note**: All background calculations should be from experiments done at the
#' **same flow rate** as the specimen experiments to be corrected.
#'
#' @param df any object of class `data.frame` containing columns of `time` and
#'   `out.o2` or `delta.o2` concentrations, and optionally `in.o2`.
#' @param time numeric integer. Defaults to 1. Specifies the column number of
#'   the time data.
#' @param out.o2 numeric value or vector of integers. Defaults to NULL.
#'   Specifies the column number(s) of outflow O2 data.
#' @param in.o2 numeric value or vector of integers. Defaults to NULL. Specifies
#'   the column number(s) of inflow O2 data.
#' @param in.o2.value numeric value. Defaults to NULL. If there is no continuous
#'   `in.o2` data in `df`, this specifies a fixed value of oxygen for inflowing
#'   water in same units as `out.o2`, and is used to calculate a `delta.o2`.
#' @param delta.o2 numeric value or vector of integers. Specifies the column
#'   number(s) of delta O2 data, for when the user has already calculated the
#'   difference between outflow and inflow oxygen (should be negative values for
#'   oxygen uptake). If this is used, `out.o2` and `in.o2` should be NULL.
#'   Defaults to all non-time columns if no other inputs given.
#' @param plot logical. Defaults to TRUE. Plots the data. See Details.
#' @param ... Allows additional plotting controls to be passed, such as `legend
#'   = FALSE` and `pos`.
#'
#' @return Output is a `list` object of class `inspect.ft` containing input
#'   parameters and data, data checks summaries, and metadata, which can be used
#'   in [`calc_rate.ft`] to determine a rate.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#'
#' # inspect outflow and inflow O2 data
#' x <- inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
#'                 in.o2 = 3)
#' print(x)
#' plot(x)
#'
#' # inspect outflow O2 data with inflow as a known value
#' x <- inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
#'                 in.o2.value = 8.9)
#' print(x)
#' plot(x)
#'
#' # inspect already calculated delta O2 data
#' inspect.ft(flowthrough.rd, time = 1, delta.o2 = 4)
#'
#' # inspect multiple columns for a quick overview
#' inspect.ft(flowthrough_mult.rd, time = 1, delta.o2 = 8:10)
#'
#' # Inspect outflow and use a blank control chamber as background
#' # correction
#' #
#' # This experiment has increasing background respiration over time.
#' # Inspecting outflow O2 with inflow header tank concentrations
#' # suggests specimen rates (bottom delta.o2 plot) are increasing.
#' inspect.ft(flowthrough_nlbg.rd, time = 1,
#'            out.o2 = 2, in.o2 = 4)
#'
#' # However, inspecting with recordings from a concurrent blank
#' # control accounts for this and shows specimen rates are level
#' # when background is taken into account.
#' inspect.ft(flowthrough_nlbg.rd, time = 1,
#'            out.o2 = 2, in.o2 = 3)

inspect.ft <- function(df, time = NULL, out.o2 = NULL, in.o2 = NULL,
                       in.o2.value = NULL, delta.o2 = NULL, plot = TRUE, ...) {

  # Input checks ------------------------------------------------------------

  ## stop if not df
  if (!is.data.frame(df)) stop("inspect.ft: 'df' must be data.frame object.")

  ## Apply time column default
  if(is.null(time)) {
    message("inspect.ft: Applying column default of 'time = 1'")
    time <- 1
  }

  ## only one of in.o2 or in.o2.value should be entered
  if(!is.null(in.o2) && !is.null(in.o2.value))
    stop("inspect.ft: Only one of 'in.o2' or 'in.o2.value' can be entered.")

  ## if in.o2 entered, out.o2 must be entered
  if(!is.null(in.o2) && (is.null(out.o2)))
    stop("inspect.ft: An 'in.o2' input requires paired 'out.o2' column(s).")

  ## if out.o2 entered, one of in.o2 or in.o2.value must be entered
  if(!is.null(out.o2) && (is.null(in.o2) && is.null(in.o2.value)))
    stop("inspect.ft: With 'out.o2' data, paired 'in.o2' columns or an 'in.o2.value' is required.")

  ## if out.o2 entered, in.o2 must be either same no. of columns or a single column
  if(!is.null(out.o2) && !(is.null(in.o2))){
    ncol_o <- length(out.o2)
    ncol_i <- length(in.o2)
    if(ncol_o != ncol_i && ncol_i != 1)
      stop("inspect.ft: With 'out.o2' data, 'in.o2' must be a single column or an equal number of paired columns.")
  }

  # if delta.o2 entered, out.o2, in.o2 & in.o2.value should be NULL
  if(!is.null(delta.o2) && (!is.null(out.o2) || !is.null(in.o2) || !is.null(in.o2.value)))
    stop("inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")

  # Apply input defaults ----------------------------------------------------

  ## After all that, if these are still NULL assume all non-time are delta.o2
  if (is.null(out.o2) && is.null(delta.o2)) {
    message("inspect.ft: Applying column default of all non-time column(s) as 'delta.o2'")
    listcols <- seq.int(1, ncol(df))
    delta.o2 <- listcols[!listcols %in% time]
  }


  # Final input checks ------------------------------------------------------

  ## check for duplicate column numbers
  inputs <- list(time, out.o2, in.o2, delta.o2)
  if(column.conflict(inputs)) {
    dupe_cols <- column.conflict(inputs, id = TRUE)
    stop(glue::glue("inspect.ft: Input columns conflict. Column(s) {glue::glue_collapse(dupe_cols, \", \", last = \" and \")} are entered more than once."))
  }
  ## check column inputs are valid
  column.val(time, int = TRUE, req = TRUE, max = 1, range = c(1, ncol(df)),
                     msg = "inspect.ft: 'time' input - ")
  column.val(out.o2, int = TRUE, req = FALSE, max = ncol(df)-1, range = c(1, ncol(df)),
                     msg = "inspect.ft: 'out.o2' input - ")
  column.val(in.o2, int = TRUE, req = FALSE, max = ncol(df)-1, range = c(0, ncol(df)),
                     msg = "inspect.ft: 'in.o2' input - ")
  column.val(delta.o2, int = TRUE, req = FALSE, max = ncol(df)-1, range = c(0, ncol(df)),
                     msg = "inspect.ft: 'delta.o2' input - ")

  # Extract data ------------------------------------------------------------

  df <- as.data.frame(df)

  ## time
  time.all <- lapply(1:length(df[time]), function(z) df[time][[z]])
  names(time.all) <- names(df[time])

  ## out o2
  # if no input, null
  if(is.null(out.o2)) {
    out.o2.all <- NULL
  } else {
    out.o2.all <- lapply(1:length(df[out.o2]), function(z) df[out.o2][[z]])
    names(out.o2.all) <- names(df[out.o2])
  }

  ## in.o2
  # if no input, null
  if(is.null(in.o2)) {
    in.o2.all <- NULL
  } else {
    in.o2.all <- lapply(1:length(df[in.o2]), function(z) df[in.o2][[z]])
    names(in.o2.all) <- names(df[in.o2])
  }

  ## in.o2 conc value - if present, make into vector same length as out.o2
  ## and same number of cols????
  if(!is.null(in.o2.value)) {
    in.o2.all <- list(rep(in.o2.value, nrow(df)))

    names(in.o2.all) <- "in.o2.value"
    #names(in.o2.all) <- sapply(1:length(in.o2.all), function(p) glue::glue("in.o2.value.{p}"))
  }

  ## if no delta input, calculate it
  if(is.null(delta.o2)) {
    del.o2.all <- mapply(function(p,q) p-q,
                         p = out.o2.all,
                         q = in.o2.all,
                         SIMPLIFY = FALSE)
    names(del.o2.all) <- sapply(1:length(del.o2.all), function(p) glue::glue("delta.o2.calc.{p}"))
    ## otherwise extract it
  } else {
    del.o2.all <- lapply(1:length(df[delta.o2]), function(z) df[delta.o2][[z]])
    names(del.o2.all) <- names(df[delta.o2])
  }

  ## Do Data Checks
  time_results <- check_timeseries(time.all, "time")

  if(is.null(out.o2)) out.o2_results <- NULL else
    out.o2_results <- check_timeseries(out.o2.all, "oxygen")

  if(is.null(in.o2)) in.o2_results <- NULL else
    in.o2_results <- check_timeseries(in.o2.all, "oxygen")

  if(is.null(delta.o2)) del.o2_results <- NULL else
    del.o2_results <- check_timeseries(del.o2.all, "oxygen")

  # issue warnings
  if (any(unlist(time_results[[1]][1,])))
    warning("NA/NaN values detected in Time column.", call. = F)
  if (any(unlist(time_results[[1]][2,])))
    warning("Non-sequential Time values found.", call. = F)
  if (any(unlist(time_results[[1]][3,])))
    warning("Duplicate Time values found.", call. = F)
  if (any(unlist(time_results[[1]][4,])))
    warning("Time values are not evenly-spaced (numerically).", call. = F)
  if (any(unlist(out.o2_results[[1]][1,])))
    warning("NA/NaN values detected in Oxygen column(s).", call. = F)
  if (any(unlist(in.o2_results[[1]][1,])))
    warning("NA/NaN values detected in Oxygen column(s).", call. = F)
  if (any(unlist(del.o2_results[[1]][1,])))
    warning("NA/NaN values detected in Oxygen column(s).", call. = F)

  # combine results
  checks <- cbind(time_results[[1]], out.o2_results[[1]], in.o2_results[[1]], del.o2_results[[1]])
  locs <- cbind(time_results[[2]], out.o2_results[[2]], in.o2_results[[2]], del.o2_results[[2]])

  # output
  ## rename columns:
  ## ncol SHOULD leave only out o2 names
  #colnames(checks) <- c(names(time.all), names(out.o2.all), names(in.o2.all), names(del.o2.all))

  ##
  locations <- lapply(1:ncol(locs), function(z) locs[, z])
  names(locations) <- colnames(locs)
  ##

  # save new data frame and create output object
  if(is.null(out.o2.all) && is.null(in.o2.all)) {
    dataframe <- data.table::data.table(cbind(data.frame(time.all),
                                              del.o2.all))
  } else {
    dataframe <- data.table::data.table(cbind(data.frame(time.all),
                                              out.o2.all,
                                              in.o2.all,
                                              del.o2.all))
  }

  out <- list(dataframe = dataframe,
              input_data = list(time = time.all,
                                out.o2 = out.o2.all,
                                in.o2 = in.o2.all,
                                delta.o2 = del.o2.all),
              checks = checks,
              locations_raw = locs,
              locations = locations,
              call = list(df = df,
                          time = time,
                          out.o2 = out.o2,
                          in.o2 = in.o2,
                          in.o2.all = in.o2.all,
                          delta.o2 = delta.o2,
                          plot = plot))

  class(out) <- "inspect.ft"

  # if no errors occur, send out a good message :D
  if (!any(na.omit(unlist(checks))))
    message("inspect.ft: No issues detected while inspecting data frame.") else
      message("inspect.ft: Data issues detected. For more information use print().")

  if (plot) plot(out, message = FALSE, ...)

  # Not all functions should print on assigning, but this one definitely should
  print(out)

  return(out)
}

#' @export
print.inspect.ft <- function(x, ...) {
  cat("\n# print.inspect.ft # --------------------\n")
  checks <- x$checks
  locs <- x$locations_raw

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

  #### CHANGE from length df to ncol checks
  for(i in 2:ncol(x$checks)) { ## for multiple columns
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
plot.inspect.ft <- function(x, pos = NULL, message = TRUE,
                            legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings

  if (message)
    cat("\n# plot.inspect.ft # ---------------------\n")

  # extract data
  dt <- x$dataframe
  time <- x$input_data$time
  out.o2 <- x$input_data$out.o2
  in.o2 <- x$input_data$in.o2
  if(length(in.o2) == 1) in.o2 <- rep(in.o2, length(out.o2)) # makes plotting easier
  del.o2 <- x$input_data$delta.o2

  # Multi column plot -------------------------------------------------------

  ## if multiple columns or only delta.o2 data inspected, just plot grid of all delta.o2
  if (length(del.o2) > 1 || is.null(out.o2)){

    if(message && is.null(pos) && length(del.o2) > 1)
      cat("inspect.ft: Plotting all delta oxygen columns. To plot a specific dataset use 'pos'.\n")

    if(is.null(pos))
      pos <- seq.int(1, length(del.o2))

    if(any(pos > length(del.o2)))
      stop("inspect.ft: Invalid 'pos' input: only ", length(del.o2), " data inputs found.")

    if (message && length(pos) == 1)
      cat('inspect.ft: Plotting delta.o2 data from position', pos, 'of', length(del.o2), '... \n')

    par(
      mfrow = n2mfrow(length(pos)),
      mai = c(0.3, 0.3, 0.2, 0.1),
      ps = 10,
      pch = 20,
      cex = 1,
      cex.main = 1,
      tck = -.05
    )

    # Plot all on same y axis range?????
    #ylim <- range(na.omit(del.o2[pos])) ## so all on same axes
    #buffer <- diff(ylim)*0.1
    #ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space



    lapply(pos, function(z) {

      ylim <- grDevices::extendrange(r = range(del.o2[[z]]), f = 0.05) ## add a little more space
      if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

      plot(data.frame(time, del.o2[[z]]),
           mgp = c(0, 0.5, 0),
           ylim = ylim,
           xlab = "",
           ylab = "",
           tck = -0.02,
           cex = 0.5,
           col.lab = "blue",
           col.axis = "blue",
           panel.first = grid())

      title(main = glue::glue("Column: {names(del.o2)[z]}"), line = 0.3)}
    )

    # Single dataset ----------------------------------------------------------
  } else if (length(x$input_data$delta.o2) == 1 || !is.null(pos)) {

    if(is.null(pos)) pos <- 1
    if(pos > length(x$input_data$delta.o2))
      stop("plot.inspect.ft: Invalid 'pos' input: only ", length(x$input_data$out.o2), " data inputs found.")

    if (message)
      cat('Plotting inspect.ft dataset from position', pos, 'of', length(x$input_data$delta), '... \n')

    ## general settings
    ## margins
    bt <- 0
    lf <- 0.5
    tp <- 0.6
    rt <- 0.3

    m <- rbind(c(1,1,1), c(1,1,1), c(2,2,2))
    layout(m)
    par(mai = c(bt, lf, tp, rt),
        ps = 10,
        cex = 1,
        cex.main = 1,
        mgp = c(0, 0.5, 0))

    ## ylim for outflow and inflow plots - plus 10%
    ylim <- range(range(out.o2[[pos]]), range(in.o2[[pos]])) ## so all on same axes
    buffer <- diff(ylim)*0.1
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space


    # Outflow plot ------------------------------------------------------------
    plot(unlist(time),
         out.o2[[pos]],
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
    points(unlist(time),
           in.o2[[pos]],
           xlab = "",
           ylab = "",
           ylim = ylim,
           pch = 16,
           cex = .5,
           col = "grey")
    # plot invisibly - to add row index x-axis
    par(new = TRUE)
    plot(
      seq(1, nrow(dt)),
      out.o2[[pos]],
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
    if(legend) legend(x = "topright",
                      "Row Index",
                      text.col = "red",
                      bg = "gray90",
                      cex = 0.6)

    if(legend) legend(x = "right",
                      legend=c("Inflow O2", "Outflow O2"),
                      col=c("grey", "black"),
                      pch=16,
                      cex=0.8)

    title(main = "Outflow ~ Inflow O2", line = 1.8)

    # Delta plot --------------------------------------------------------------
    par(mai = c(0.4, lf, 0.2, rt))
    ## ylim  - plus 10%
    ylim <- range(na.omit(del.o2[[pos]])) ## so all on same axes
    buffer <- diff(ylim)*0.1
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

    plot(
      unlist(time),
      del.o2[[pos]],
      xlab = "",
      ylab = "",
      ylim = ylim, # reverse y axis
      pch = 16,
      cex = .5,
      axes = FALSE,
      panel.first = grid()
    )

    axis(side = 2, las = 1, tck = 0) # simply to put yaxis lab colour back to black
    axis(side = 1,col.lab = "blue",
         col.axis = "blue") # simply to put yaxis lab colour back to black

    box()

    if(legend) legend("bottomright",
                      "Time",
                      text.col = "blue",
                      bg = "gray90",
                      cex = 0.6)

    title(main = glue::glue("Delta O2 (i.e. Unitless Rate)"), line = 0.3)

  }

  if (message){
    cat("Done.\n")
    cat("-----------------------------------------\n")
  }

  on.exit(par(parorig)) # revert par settings to original

  return(invisible(x))
}
