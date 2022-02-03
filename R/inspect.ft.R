#' Explore and visualise flowthrough respirometry data and check for errors
#'
#' `inspect.ft` is a data exploration and preparation function that visualises
#' flowthrough respirometry data, checks it for common issues, and prepares it
#' for use in later functions in `respR`, such as [calc_rate.ft()].
#'
#' `inspect.ft` is intended to be specific to *flowthrough respirometry* data.
#' In flowthrough respirometry (also known as 'open flow' or 'continuous flow'
#' respirometry) rather than calculating a rate from a changing oxygen
#' concentration recording in a sealed chamber, instead the difference (i.e.
#' 'oxygen delta') between the inflowing and outflowing oxygen concentrations of
#' a respirometer receiving water at a constant flow rate is used to calculate
#' an oxygen consumption or production rate, typically after it has reached a
#' steady state. Therefore, in general, regions of stable oxygen delta values
#' (difference between outflow and inflow oxygen) are of interest. `inspect.ft`
#' visualises and prepares the data for use in [calc_rate.ft()]. By specifying
#' data types in this function and saving the output, they do not need to be
#' specified in later functions.
#'
#' ## Inputs
#'
#' `inspect.ft` requires at least two data inputs; a single column of numeric
#' `time` data, with *either* a column of paired `out.o2` concentrations (i.e.
#' the exhalent or 'downstream' concentrations), *or* a column of already
#' calculated `delta.o2` values, that is the difference between outflow and
#' inflow concentrations, or the outflow concentration corrected by a background
#' recording from a 'blank' or empty chamber.
#'
#' *out.o2 input option*: If an `out.o2` column has been specified, in order to
#' calculate the oxygen delta (and therefore a rate in [calc_rate.ft()]) there
#' must also be an inflow oxygen concentration input (i.e. the inhalent or
#' 'upstream' concentration). This will generally be a column of paired `in.o2`
#' concentrations, in which case the paired values of `out.o2` and `in.o2` are
#' used to calculate the oxygen `delta.o2`, which is saved in the output, and
#' used to determine a rate in [calc_rate.ft()]. Alternatively, if the inflow
#' oxygen concentration is a known, generally unvarying concentration (such as
#' fully air-saturated water from a header tank) this can be entered as a single
#' value via `in.o2.value` and this is used to calculate the `delta.o2`.
#'
#' *delta.o2 input option*: If delta oxygen values have already been calculated,
#' these can be entered via the `delta.o2` input, and these are prepared and
#' saved for rate calculations in `calc_rate.ft`.
#'
#' Given an input data frame `x`, the function scans the columns specified via
#' the `time`, `out.o2`, `in.o2` or `delta.o2` inputs. If no columns are
#' specified, by default the functions assumes the first column is `time`, and
#' all others are `delta.o2` oxygen data.
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
#' [`calc_rate.ft()`]. A plot is also produced.
#'
#' ## Plot
#'
#' If `plot = TRUE`, entered data is plotted against both time (bottom, blue
#' axis) and row index (top, red axis), depending on the inputs:
#'
#' - a single `out.o2` column with either a paired `in.o2` column or
#' `in.o2.value`: a two panel plot. The top plot is both outflow (green points)
#' and inflow (turquoise points) oxygen. The bottom plot is the oxygen delta
#' (black points) between outflow and inflow oxygen, essentially the oxygen
#' uptake or production rate.
#'
#' - a single `delta.o2` column: a one panel plot of oxygen delta values.
#'
#' - multiple `out.o2` or `delta.o2` columns: a grid plot of all `delta.o2` data
#' (either as entered or calculated from `out.o2` and `in.o2`). Specific delta
#' plots can be examined individually by using the `pos` input (e.g. `plot(x,
#' pos = 2)`). Y-axes are not equal.
#'
#' - unspecified columns: all columns are plotted assuming `time` is in column
#' 1, and all others are oxygen `delta.o2` data. Y-axes are not equal.
#'
#' In delta plots, that is those plotting `delta.o2` values, either directly
#' entered or calculated, consistent oxygen uptake or production rates will be
#' represented by flat or level regions.
#'
#' ***Note:*** Since `respR` is primarily used to examine oxygen consumption,
#' the delta oxygen and rate plots are by default plotted on a reverse y-axis.
#' In `respR` oxygen uptake rates are negative since they represent a negative
#' slope of oxygen against time. In these plots the axis is reversed so that
#' higher uptake rates (i.e. more negative) will be higher on these plots. If
#' you are interested instead in oxygen production rates, which are positive,
#' the `rate.rev = FALSE` argument can be passed in either the `inspect.ft`
#' call, or when using `plot()` on the output object. In this case, the delta
#' and rate values will be plotted numerically, and higher oxygen *production*
#' rates will be higher on the plot.
#'
#' ## Additional plotting options
#'
#' If the legend or labels obscure part of the plot, they can be suppressed via
#' `legend = FALSE` in either the `inspect.ft` call, or when using `plot()` on
#' the output object. Suppress console output messages with `quiet = TRUE`. If
#' multiple columns have been inspected, the `pos` input can be used to examine
#' each `out.o2`~`in.o2`~`del.o2` dataset. If axis labels (particularly y-axis)
#' are difficult to read, `las = 2` can be passed to make axis labels
#' horizontal. In addition, `oma` (outer margins, default `oma = c(0.4, 1, 1.5,
#' 0.4)`), and `mai` (inner margins, default `mai = c(0.3, 0.15, 0.35, 0.15)`)
#' can be used to adjust plot margins.
#'
#' ## Multiple data columns
#'
#' For a quick overview of larger experiments, multiple columns of `out.o2`,
#' `in.o2` and `delta.o2` can be inspected, but must share the same numeric time
#' data column specified by the `time` input. Note, multiple column inspection
#' is chiefly intended to be exploratory functionality to provide a quick
#' overview of larger datasets. While the output will contain all data columns
#' in `$dataframe` and `$data`, subsequent functions such as [calc_rate.ft()]
#' will use only the first `delta.o2` column for calculating rates. Best
#' practice is to inspect and assign each individual experiment or column pair
#' as separate `inspect.ft` objects. See Examples.
#'
#' If multiple `out.o2` columns are specified, `in.o2` can be a single column
#' (if for example all chambers are supplied from the same header tank), in
#' which case it is used to calculate an oxygen delta for all `out.o2` columns.
#' A single `in.o2.value` in the same units as `out.o2` can also be specified.
#' There can also be multiple `in.o2` columns, in which case it is assumed each
#' `out.o2` column is paired with each `in.o2` at the same position, and used to
#' calculate the oxygen `delta.o2`. Therefore, `out.o2` and `in.o2` must have
#' equal numbers of columns.
#'
#' ## Failed Checks
#'
#' The most important data check in `inspect.ft` is that all data columns are
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
#' The remaining data checks in `inspect.ft` are mainly exploratory and help
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
#' erroneous results, as long as they do not occur over large regions of the
#' data. `inspect.ft` however outputs locations (row numbers) of where these
#' issues occur (located in the `$locs` element of the output), allowing users
#' to amend them before analysis. We would recommend that to be completely
#' confident in any results from analysis of such data, and avoid obscure
#' errors, these issues be addressed before proceeding.
#'
#' ## Background control or "blank" experiments
#'
#' For experiments in which the specimen data is to be background corrected by a
#' concurrently-run control experiment, `inspect.ft` can be used by specifying
#' the specimen experiment as `out.o2`, and the "blank" as the `in.o2` input. In
#' this way, any variations in oxygen in the specimen data due to background
#' microbial activity, or for any other reason such as fluctuations in inflow
#' oxygen, are accounted for in the delta oxygen calculations, and therefore in
#' the rate calculated in [`calc_rate.ft()`]. See the vignettes on the website
#' for examples.
#'
#' If the background recordings are experiments with their own outflow and
#' inflow recordings, which show a generally consistent oxygen delta due to
#' microbial activity, this can be saved as a separate `inspect.ft` object, a
#' background rate calculated in [`calc_rate.ft()`], and this used in
#' [`adjust_rate.ft()`] as the `by` input to perform background adjustments to
#' specimen rates.
#'
#' **Note**: All background calculations should be from experiments done at the
#' *same flow rate* as the specimen experiments to be corrected.
#'
#' ## Output
#'
#' Output is a `list` object of class `inspect.ft` containing input parameters
#' and data, data check summaries, and metadata, which can be passed to
#' [`calc_rate.ft()`] to determine rates. If there are failed checks or
#' warnings, the row locations of the potentially problematic data can be found
#' in `$locs`.
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
#' @param x `data.frame` containing columns of `time` and `out.o2` or `delta.o2`
#'   concentrations, and optionally `in.o2`.
#' @param time integer. Defaults to 1. Specifies the column number of the time
#'   data.
#' @param out.o2 integer(s). Defaults to `NULL`. Specifies the column number(s)
#'   of outflow O2 data.
#' @param in.o2 integer(s). Defaults to `NULL`. Specifies the column number(s)
#'   of inflow O2 data.
#' @param in.o2.value numeric value. Defaults to `NULL`. If there is no
#'   continuous `in.o2` data, this specifies a fixed value of oxygen
#'   concentration for inflowing water in same units as `out.o2`, and is used
#'   with `out.o2` to calculate a `delta.o2`.
#' @param delta.o2 integer(s). Defaults to all non-time columns if no other
#'   inputs given. Specifies the column number(s) of delta O2 data, for when the
#'   user has already calculated the difference between outflow and inflow
#'   oxygen (should be negative values for oxygen uptake). If this is used,
#'   `out.o2` and `in.o2` should be NULL.
#' @param plot logical. Defaults to TRUE. Plots the data. See Details.
#' @param ... Allows additional plotting controls to be passed, such as `legend
#'   = FALSE`, `quiet = TRUE`, `rate.rev = FALSE` and `pos`.
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
#' inspect.ft(flowthrough_sim.rd, time = 1,
#'            out.o2 = 2, in.o2 = 4)
#'
#' # However, inspecting with recordings from a concurrent blank
#' # control accounts for this and shows specimen rates are level
#' # when background is taken into account.
#' inspect.ft(flowthrough_sim.rd, time = 1,
#'            out.o2 = 2, in.o2 = 3)

inspect.ft <- function(x, time = NULL, out.o2 = NULL, in.o2 = NULL,
                       in.o2.value = NULL, delta.o2 = NULL, plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()

  # Input checks ------------------------------------------------------------

  ## stop if not df
  if (!is.data.frame(x)) stop("inspect.ft: 'x' must be data.frame object.")

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
    listcols <- seq.int(1, ncol(x))
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
  column.val(time, req = TRUE, max = 1, range = c(1, ncol(x)),
             msg = "inspect.ft: 'time' -")
  column.val(out.o2, req = FALSE, max = ncol(x)-1, range = c(1, ncol(x)),
             msg = "inspect.ft: 'out.o2' -")
  column.val(in.o2, req = FALSE, max = ncol(x)-1, range = c(0, ncol(x)),
             msg = "inspect.ft: 'in.o2' -")
  column.val(delta.o2, req = FALSE, max = ncol(x)-1, range = c(0, ncol(x)),
             msg = "inspect.ft: 'delta.o2' -")

  # Extract data ------------------------------------------------------------

  df <- as.data.frame(x)

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
  if(!is.null(in.o2.value)) {
    in.o2.all <- list(rep(in.o2.value, nrow(df)))

    names(in.o2.all) <- "in.o2.value"
  }

  ## If no delta input - make it NULL for now,
  ## We will calculate it after checks if out.o2 and in.out non-numeric check passes.
  ## Otherwise failures here if non-numeric data
  if(is.null(delta.o2)) {
    del.o2.all <- NULL
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
  # time
  if (any(unlist(time_results[[1]][1,])))
    warning("inspect.ft: Time column not numeric. Other column checks skipped. \nData cannot be analysed by respR functions if not numeric. \nNo output returned.", call. = F)
  if (any(unlist(time_results[[1]][2,]) == "TRUE"))
    warning("inspect.ft: Inf/-Inf values detected in Time column. Remove or replace before proceeding.", call. = F)
  if (any(unlist(time_results[[1]][3,]) == "TRUE"))
    warning("inspect.ft: NA/NaN values detected in Time column.", call. = F)
  if (any(unlist(time_results[[1]][4,]) == "TRUE"))
    warning("inspect.ft: Non-sequential Time values found.", call. = F)
  if (any(unlist(time_results[[1]][5,]) == "TRUE"))
    warning("inspect.ft: Duplicate Time values found.", call. = F)
  if (any(unlist(time_results[[1]][6,]) == "TRUE"))
    warning("inspect.ft: Time values are not evenly-spaced (numerically).", call. = F)

  # out.o2
  if (any(unlist(out.o2_results[[1]][1,])))
    warning("inspect.ft: Oxygen column(s) not numeric. Other column checks skipped. \nData cannot be analysed by respR functions if not numeric. \nNo output returned.", call. = F)
  if (any(unlist(out.o2_results[[1]][2,]) == "TRUE"))
    warning("inspect.ft: Inf/-Inf values detected in Oxygen column(s). Remove or replace before proceeding.", call. = F)
  if (any(unlist(out.o2_results[[1]][3,]) == "TRUE"))
    warning("inspect.ft: NA/NaN values detected in Oxygen column(s).", call. = F)
  # in.o2
  if (any(unlist(in.o2_results[[1]][1,])))
    warning("inspect.ft: Oxygen column(s) not numeric. Other column checks skipped. \nData cannot be analysed by respR functions if not numeric. \nNo input returned.", call. = F)
  if (any(unlist(in.o2_results[[1]][2,]) == "TRUE"))
    warning("inspect.ft: Inf/-Inf values detected in Oxygen column(s). Remove or replace before proceeding.", call. = F)
  if (any(unlist(in.o2_results[[1]][3,]) == "TRUE"))
    warning("inspect.ft: NA/NaN values detected in Oxygen column(s).", call. = F)
  # del.o2
  if (any(unlist(del.o2_results[[1]][1,])))
    warning("inspect.ft: Oxygen column(s) not numeric. Other column checks skipped. \nData cannot be analysed by respR functions if not numeric. \nNo output returned.", call. = F)
  if (any(unlist(del.o2_results[[1]][2,]) == "TRUE"))
    warning("inspect.ft: Inf/-Inf values detected in Oxygen column(s). Remove or replace before proceeding.", call. = F)
  if (any(unlist(del.o2_results[[1]][3,]) == "TRUE"))
    warning("inspect.ft: NA/NaN values detected in Oxygen column(s).", call. = F)

  # combine results
  checks <- cbind(time_results[[1]], out.o2_results[[1]], in.o2_results[[1]], del.o2_results[[1]])
  locs_raw <- cbind(time_results[[2]], out.o2_results[[2]], in.o2_results[[2]], del.o2_results[[2]])

  # output
  locs <- lapply(1:ncol(locs_raw), function(z) locs_raw[, z])
  names(locs) <- colnames(locs_raw)

  # if in.o2 and out.o2 non-numeric checks have passed, now it's ok to calculate delta.o2
  if(is.null(delta.o2) && !(any(unlist(out.o2_results[[1]][1,]))) && !(any(unlist(in.o2_results[[1]][1,])))) {
    del.o2.all <- mapply(function(p,q) p-q,
                         p = out.o2.all,
                         q = in.o2.all,
                         SIMPLIFY = FALSE)
    names(del.o2.all) <- sapply(1:length(del.o2.all), function(p) glue::glue("delta.o2.calc.{p}"))
    ## otherwise make dummy list so next part doesn't error.
    ## It won't be returned anyway
  } else if (is.null(delta.o2)) {
    del.o2.all <- mapply(function(p,q) rep(NA, length(p)),
                         p = out.o2.all,
                         q = in.o2.all,
                         SIMPLIFY = FALSE)
    #names(del.o2.all) <- sapply(1:length(del.o2.all), function(p) glue::glue("delta.o2.calc.{p}"))
  }

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

  out <- list(call = call,
              dataframe = dataframe,
              inputs = list(df = x,
                            time = time,
                            out.o2 = out.o2,
                            in.o2 = in.o2,
                            in.o2.value = in.o2.value,
                            delta.o2 = delta.o2,
                            plot = plot),
              data = list(time = time.all,
                          out.o2 = out.o2.all,
                          in.o2 = in.o2.all,
                          delta.o2 = del.o2.all),
              checks = checks,
              locs_raw = locs_raw,
              locs = locs)

  class(out) <- "inspect.ft"

  # if no errors occur, send out a good message :D
  if (!any(na.omit(unlist(checks) == "TRUE")))
    message("inspect.ft: No issues detected while inspecting data frame.") else
      message("inspect.ft: Data issues detected. For more information use print().")

  # Not all functions should print on assigning, but this one should
  print(out)

  # If any inspected columns found to be non-numeric, print, but don't return object
  # If this is the case, it can't be used in later fns anyway, so no point
  # And skip plotting - avoids awkward code in plot() to handle the object having non-numeric data
  # Warnings issued above.
  if(any(unlist(checks[1,]))) {
    return(invisible(NULL))
  } else {
    if (plot) plot(out, quiet = TRUE, ...)
    ## invisible prevents it printing twice on assigning
    return(invisible(out))
  }
}

#' @export
print.inspect.ft <- function(x, ...) {
  cat("\n# print.inspect.ft # --------------------\n")
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
  # No need to do this for numeric check - it's all or none
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
    # use ncol(checks) because x$dataframe may have extra delta oxy calculated columns added
    if (checks[, i][[2]] != "skip" && checks[, i][[2]]) {
      yinf <- locs[,i][[2]]
      cat("Inf/-Inf locations ")
      if (length(yinf) > 20) cat("(first 20 shown) ")
      cat("in Oxygen column:", names(x$dataframe)[i], "\n")
      print(head(yinf, 20))
    }
    if (checks[, i][[3]] != "skip" && checks[, i][[3]]) {
      ynan <- locs[,i][[3]]
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
summary.inspect.ft <- function(object, ...) {
  print(object)
}

#' @export
plot.inspect.ft <- function(x, pos = NULL, quiet = FALSE,
                            legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if (!quiet)
    cat("\n# plot.inspect.ft # ---------------------\n")

  # extract data
  dt <- x$dataframe
  time <- x$data$time
  out.o2 <- x$data$out.o2
  in.o2 <- x$data$in.o2
  if(length(in.o2) == 1) in.o2 <- rep(in.o2, length(out.o2)) # makes plotting easier
  del.o2 <- x$data$delta.o2

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = cex_def)

  # Multi column plot -------------------------------------------------------

  ## if multiple columns or only delta.o2 data inspected, just plot grid of all delta.o2
  if (length(del.o2) > 1 || is.null(out.o2)){

    if(!quiet && is.null(pos) && length(del.o2) > 1)
      cat("inspect.ft: Plotting all delta oxygen columns. To plot a specific dataset use 'pos'.\n")

    if(is.null(pos))
      pos <- seq.int(1, length(del.o2))

    if(any(pos > length(del.o2)))
      stop("inspect.ft: Invalid 'pos' input: only ", length(del.o2), " data inputs found.")

    if (!quiet && length(pos) == 1)
      cat('inspect.ft: Plotting delta.o2 data from position', pos, 'of', length(del.o2), '... \n')

    par(mfrow = n2mfrow(length(pos)),
        ps = 10,
        cex = 1,
        cex.main = 1,
        ...)

    # Plot all on same y axis range?????
    #ylim <- range(na.omit(del.o2[pos])) ## so all on same axes
    #buffer <- diff(ylim)*0.1
    #ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    lapply(pos, function(z) {

      ylim <- grDevices::extendrange(r = range(del.o2[[z]], na.rm = TRUE), f = 0.05) ## add a little more space
      if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

      plot(data.frame(time, del.o2[[z]]),
           axes = FALSE,
           ylim = ylim,
           xlab = "",
           ylab = "",
           cex = 0.5,
           col = ftcol_del,
           panel.first = grid())

      box()
      axis(side = 2)
      axis(side = 1, col.lab = "blue", col.axis = "blue")

      # plot invisibly - to add row index x-axis
      par(new = TRUE, ...)
      plot(data.frame(1:length(unlist(time)), del.o2[[z]]),
           xlab = "",
           ylab = "",
           pch = "",
           cex = .5,
           axes = FALSE
      )
      axis(side = 3, col.axis = "red")
      title(main = glue::glue("Column: {names(del.o2)[z]}"), line = 1.2,
            adj = 0)
    })

    if(legend && length(pos) == 1) legend("topright",
                                          "Row Index",
                                          text.col = "red",
                                          bg = "gray90",
                                          cex = 0.6)

    if(legend && length(pos) == 1) legend("bottomright",
                                          "Time",
                                          text.col = "blue",
                                          bg = "gray90",
                                          cex = 0.6)
    mtext("inspect.ft: Inspecting Selected Columns",
          outer = TRUE, cex = 1.2, line = 0.3, font = 2)

    # Single dataset ----------------------------------------------------------
  } else if (length(x$data$delta.o2) == 1 || !is.null(pos)) {

    if(is.null(pos)) pos <- 1
    if(pos > length(x$data$delta.o2))
      stop("plot.inspect.ft: Invalid 'pos' input: only ", length(x$data$out.o2), " data inputs found.")

    if (!quiet)
      cat('Plotting inspect.ft dataset from position', pos, 'of', length(x$data$delta), '... \n')

    m <- rbind(c(1,1,1), c(1,1,1), c(2,2,2))
    layout(m)

    par(mai = mai_def_top_ext, # this one needs extra space
        ps = 10,
        cex = 1,
        cex.main = 1,
        ...)
    par(...) # to allow oma and mai to be overridden

    ## ylim for outflow and inflow plots - plus 10%
    ylim <- range(range(nainf.omit(out.o2[[pos]])), range(nainf.omit(in.o2[[pos]]))) ## so all on same axes
    buffer <- diff(ylim)*0.1
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    # Outflow plot ------------------------------------------------------------
    plot(unlist(time),
         out.o2[[pos]],
         xlab = "",
         ylab = "",
         ylim = ylim,
         cex = .5,
         col = ftcol_out,
         axes = FALSE,
         col.lab = "blue",
         col.axis = "blue",
         panel.first = grid())

    axis(side = 2)
    points(unlist(time),
           in.o2[[pos]],
           xlab = "",
           ylab = "",
           ylim = ylim,
           cex = .5,
           col = ftcol_in)
    # plot invisibly - to add row index x-axis

    par(new = TRUE)

    plot(seq(1, nrow(dt)),
         out.o2[[pos]],
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
                      cex = 0.6)
    if(legend) legend("right",
                      legend = c("Inflow O2", "Outflow O2"),
                      pch = pch_def,
                      col = c(ftcol_in, ftcol_out),
                      bg = "white",
                      cex = 0.8)

    mtext("Outflow ~ Inflow O2",
          outer = TRUE, cex = 1.2, line = 0.3, font = 2)

    # Delta plot --------------------------------------------------------------

    ## ylim  - plus 10%
    ylim <- range(nainf.omit(del.o2[[pos]])) ## so all on same axes
    buffer <- diff(ylim)*0.1
    ylim <- c(ylim[1] - buffer, ylim[2] + buffer) ## add a little more space

    if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

    plot(unlist(time),
         del.o2[[pos]],
         xlab = "",
         ylab = "",
         ylim = ylim, # reverse y axis
         cex = .5,
         col = ftcol_del,
         axes = FALSE,
         panel.first = grid())

    axis(side = 2) # simply to put yaxis lab colour back to black
    axis(side = 1,
         col.lab = "blue",
         col.axis = "blue")

    box()

    if(legend) legend("bottomright",
                      "Time",
                      text.col = "blue",
                      bg = "gray90",
                      cex = 0.6)

    mtext("Delta O2",
          outer = FALSE, cex = 1.2, line = 1.2, font = 2)
    mtext(glue::glue("(i.e. Unitless Rate)"),
          outer = FALSE, cex = 1, line = 0.3, font = 2)

  }

  if (!quiet){
    cat("-----------------------------------------\n")
  }

  return(invisible(x))
}
