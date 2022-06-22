#' Run calc_rate on multiple replicates in intermittent-flow respirometry data
#'
#' `calc_rate.int` allows you to run the `calc_rate()` function on multiple
#' replicates in intermittent-flow respirometry. This allows you to use
#' consistent selection parameters, for example a time range, row range, or
#' oxygen range to extract a single rate from each replicate.
#'
#' `calc_rate.int` uses the `starts` and optional `ends` inputs to subset each
#' replicate, then run `calc_rate` on the subset, saving the result, and
#' extracting the rate and other data to a summary table.
#'
#' The `x` Input should be an`inspect` object or two-column data frame
#' containing paired values of time and oxygen from an intermittent-flow
#' experiment in columns 1 and 2 respectively. If a multiple column dataset is
#' entered as `x` the first two columns are selected by default. If these are
#' not the intended data use `inspect` to select the correct time and oxygen
#' columns.
#'
#' ## Specifying replicate structure
#'
#' The `starts` input specifies the locations of the start of each replicate in
#' the data in `x`. This can be in one of two ways:
#'
#' - A single numeric value representing a regular interval in rows starting at
#' row 1 indicating how replicates are spaced. This option should only be used
#' when replicates cycle at regular intervals of row numbers. If the first
#' replicate does not start at row 1, the data should be subset so that it does
#' (see `subset_data()`) and example [here]().
#'
#' - A numeric vector of row locations indicating the start row of each
#' individual replicate. The first replicate does not have to start at row 1.
#' This option can also be used to specify a regular replicate structure (as
#' above) using regular `R` syntax such as `seq()` or `1:10`, etc.
#'
#' The `ends` input is optional and can be used to specify the end row of each
#' replicate, for example to exclude flush periods. If `ends = NULL` (the
#' default) it is assumed each replicate end row is the row preceding the start
#' of the next replicate as specified in the `starts` input, or in the case of
#' the last replicate the final row of the dataset. If you specify replicate
#' `ends` you can do it in two ways which must match how `starts` has been
#' entered:
#'
#' - If `starts` is a single numeric value specifying the row interval of
#' replicates, this is a numeric value specifying the *length* in rows of each
#' replicate. This is added to each `starts` location internally to get the end
#' locations of each replicate.
#'
#' - If `starts` is a vector of row locations of the start of each replicate,
#' this should be a vector of equal length of the end row locations of each
#' individual replicate.
#'
#' ## Rate region selection within replicates
#'
#' Once the function subsets each replicate it calculates a rate within it using
#' `calc_rate`. By default the rate is calculated over the whole replicate,
#' however the `from`, `to`, and `by` inputs can be used to specify a region
#' within each replicate in terms of row, time, or oxygen ranges.
#'
#' For `by = "time"` and `by = "row"` the `from` and `to` inputs are *relative*
#' to the start of the subset replicate, not the original input data values. For
#' example, `from = 2, to = 10, by = "time"` will determine rate from the actual
#' replicate `start time + 2` to `start time + 10` within every replicate,
#' regardless of the actual time values. See Examples.
#'
#' See `calc_rate()` for full details of how selection inputs are applied.
#' Briefly, for all methods if any `from` or `to` values lie outside the range
#' of the replicate data or do not match exactly to a value within it, the
#' closest value is used instead (accounting for the fact that `from` and `to`
#' inputs are treated as relative within each replicate).
#'
#' ## More details
#'
#' Only a single rate can be extracted from each replicate. If you need to
#' extract multiple rates from single replicates use `subset_data()` and
#' `calc_rate()` which will accept multiple `from` and `to` inputs. Similarly,
#' the rate is extracted from the same region of each replicate as determined
#' via the `from` and `to` inputs. See vignettes on the website for examples of
#' alternative ways of iterating `calc_rate` across multiple replicates if you
#' need to get around these constraints.
#'
#' ## Plot
#'
#' If `plot = TRUE` (the default), the result for each replicate is plotted on a
#' grid up to a maximum of 20. Which replicates are plotted can be selected
#' using the `pos` input (default is 1:20), either in the main function call or
#' when calling `plot()` on output objects.
#'
#' There are three ways of plotting the results, which can be selected using the
#' `type` input:
#'
#' - `type = "rep"`: The default. Each individual replicate is plotted with the
#' rate region highlighted.
#'
#' - `type = "full"`: Each replicate rate is highlighted in the context of the
#' whole dataset. May be quite difficult to interpret if dataset is large.
#'
#' - `type = "cr"`: Plots individual replicate results as `calc_rate` objects.
#'
#' For all types `pos` can be used to select which replicate(s) to plot.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `plot()`,
#' `print()`, `summary()`, and `mean()`.
#'
#' - `plot()`: plots the result. See Plot section above.
#'
#' - `print()`: prints the result of a single replicate, by default the first.
#' Others can be printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all results and metadata, or those
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The `$rank`
#' column numbers represent the replicate number. The summary table can be
#' exported as a separate data frame by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of the rates from every replicate, or those
#' specified by the `pos` input. e.g. `mean(x, pos = 1:5)` The mean can be
#' exported as a numeric value by passing `export = TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return Output is a `list` object of class `calc_rate.int` containing a
#'   `calc_rate` object for each replicate in `$results`. The output also
#'   contains a `$summary` table which includes the full rate regression results
#'   from each replicate with replicate number indicated by the `$rank` column.
#'   It also contains a `$rate` element which contains the rate values from each
#'   replicate. The function call, inputs, and other metadata are also included.
#'   Note, that if you have many replicates this object can be rather large
#'   (several MB).
#'
#' @param x object of class `inspect` or `data.frame`. This is the timeseries of
#'   paired values of oxygen against time containing multiple replicates from
#'   which to calculate rates.
#' @param starts Integer(s). Row locations of the start of each replicate, or if
#'   a single value a regular interval in rows of how replicates are spaced
#'   starting from row 1.
#' @param ends Integer(s). Locations of the end of each replicate. Optional.
#'   Should be same length as `starts`. If a vector, this is the row number of
#'   the end of each replicate, if a single value it represents the length in
#'   rows of the replicates. If not entered, the function assumes each replicate
#'   ends at the row preceding the start of the next or for the final replicate
#'   the final row of the dataset.
#' @param from numeric. Start time, row, or oxygen value within each replicate
#'   over which to calculate rate. See `calc_rate()`. Time and row values are
#'   relative to each replicate as subset using `starts` and `ends`. See
#'   Details.
#' @param to numeric. End time, row, or oxygen value within each replicate over
#'   which to calculate rate. See `calc_rate()`. Time and row values are
#'   relative to each replicate as subset using `starts` and `ends`. See
#'   Details.
#' @param by string. `"time"`, `"row"`, or `"oxygen"`. Defaults to `"time"`.
#'   Method by which `from` and `to` are applied within each replicate. See
#'   `calc_rate()`.
#' @param plot logical. Default is `TRUE`. Plots the results. See 'Plotting'
#'   section for details.
#' @param ... Allows additional plotting controls to be passed, such as `type`,
#'   `pos`, `legend`, and `quiet`.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Irregular replicate structures ------------------------------------------
#'
#' # Inspect the data to use in examples
#' urch_insp <- inspect(intermittent.rd)
#'
#' # Calculate rate across each entire replicate
#' # This leads to erroneous rates because the flush is included
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3751))
#'
#' # So instead we also specify the replicate end rows
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3751),
#'               ends = c(1900, 3550, 4831))
#'
#' # However, we usually don't want to use the entire replicate.
#' # So we can specify a rate region over which to extract a rate in every
#' # replicate in one of three ways:
#' #
#' # By time
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3751),
#'               ends = c(1900, 3550, 4831),
#'               from = 300,
#'               to = 1200,
#'               by = "time")
#' # By row
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3751),
#'               ends = c(1900, 3550, 4831),
#'               from = 100,
#'               to = 500,
#'               by = "row")
#' # By oxygen
#' urch_res <- calc_rate.int(urch_insp,
#'                           starts = c(1, 2101, 3751),
#'                           ends = c(1900, 3550, 4831),
#'                           from = 7.0,
#'                           to = 6.7,
#'                           by = "oxygen")
#'
#' # Regular replicate structures --------------------------------------------
#'
#' # If replicates cycle at regular intervals, 'starts' can be used to specify the
#' # spacing in rows, starting at row 1. Therefore data must be subset first so
#' # that the first replicate starts at row 1.
#' #
#' # Subset and inspect data
#' zeb_insp <- zeb_intermittent.rd |>
#'   subset_data(from = 5840,
#'               to = 75139,
#'               by = "row",
#'               quiet = TRUE) |>
#'   inspect()
#'
#' # Calculate a rate from same 6-minute time region in every replicate.
#' # Replicates start every 660 rows.
#' # 'ends' is used to exclude the flush by specifying each is 540 rows long
#' calc_rate.int(zeb_insp,
#'               starts = 660,
#'               ends = 540,
#'               from = 120,
#'               to = 480,
#'               by = "time",
#'               plot = TRUE)
#'
#' # We don't necessarily need to specify the 'ends', because in this case our
#' # time region excludes the flush anyway.
#' zeb_res <- calc_rate.int(zeb_insp,
#'                          starts = 660,
#'                          from = 120,
#'                          to = 480,
#'                          by = "time",
#'                          plot = TRUE)
#'
#' # S3 functions ------------------------------------------------------------
#'
#' # Outputs can be used in print(), summary(), and mean().
#' # 'pos' can be used to select replicate ranges
#' summary(zeb_res)
#' mean(zeb_res, pos = 1:5)
#'
#' # There are three ways by which the results can be plotted.
#' # 'pos' can be used to select replicates to be plotted.
#' #
#' # type = "replicate" - the default. Each replicate plotted on a grid with rate
#' # region highlighted (up to a maximum of 20).
#' plot(urch_res)
#'
#' # type = "full" - each replicate rate region plotted on entire data series.
#' plot(urch_res, pos = 1:2, type = "full")
#' # Of limited utility when datset is large
#' plot(zeb_res, pos = 10, type = "full")
#'
#' # type = "cr" - the 'calc_rate' object for selected replicates in 'pos' is plotted
#' plot(urch_res, pos = 2, type = "cr")
#'
#' # See vignettes on website for how to adjust and convert rates from calc_rate.int
#' }

calc_rate.int <- function(x,
                          starts = NULL,
                          ends = NULL,
                          from = NULL,
                          to = NULL,
                          by = "time",
                          plot = TRUE,
                          ...)
{

  ## Save function call for output
  call <- match.call()
  ## Save inputs for output
  inputs <- list(x = x,
                 starts = starts,
                 ends = ends,
                 from = from,
                 to = to,
                 by = by,
                 plot = plot)

  # Checks ------------------------------------------------------------------

  # x
  if(!(class.val(x, df = TRUE, insp = TRUE)))
    stop("calc_rate.int: Input must be a 'data.frame' or 'inspect' object.")

  # Extract data
  if(any(class(x) %in% "inspect")) df <- x$dataframe else
    df <- x

  # Format as data.table
  df <- data.table::data.table(df)
  if (length(df) > 2) {
    warning("calc_rate.int: Multi-column dataset detected in input. Selecting first two columns by default.\n  If these are not the intended data, inspect() or subset the data frame columns appropriately before running calc_rate.int()")
    df <- df[, 1:2]
  }

  # starts
  #  - required, numeric, integer, within df row range
  input.val(starts, num = TRUE, int = TRUE, req = TRUE,
            range = c(1,nrow(df)),
            msg = "calc_rate.int: 'starts' -")

  # ends
  #  - not required, but if entered numeric, integer, within df row range
  # plus same length as 'starts'
  input.val(ends, num = TRUE, int = TRUE, req = FALSE,
            range = c(1,nrow(df)),
            msg = "calc_rate.int: 'ends' -")
  if(!is.null(ends) && length(ends) != length(starts))
    stop("calc_rate.int: The 'ends' input should be the same length as the 'starts' input.")
  # if length > 1 each 'ends' should be larger than the respective 'starts'
  if(!is.null(ends) && length(ends) > 1 && any(mapply(function(p, q) p >= q,
                                                      p = starts,
                                                      q = ends)))
    stop("calc_rate.int: One or more 'ends' inputs are before or equal to the corresponding 'starts' input!")

  # from, to, by
  # Handled by checks in calc_rate
  # Only one needed here is 'from' and 'to' should be single values
  if(length(from) > 1)
    stop("calc_rate.int: The 'from' input should be a single value.")
  if(length(to) > 1)
    stop("calc_rate.int: The 'to' input should be a single value.")

  ## verify by input
  ## - this is also done in calc_rate but we want to disallow "proportion"
  by <- verify_by(by, req = FALSE, default = "time",
                  which = c("t", "o", "r"),
                  msg = "calc_rate.int:")

  # Format starts -----------------------------------------------------------
  if(length(starts) == 1) starts <- seq(1, nrow(df), starts)

  # Format ends -------------------------------------------------------------
  if(is.null(ends)) ends <- c(starts[2:length(starts)]-1, nrow(df)) else
    if(length(ends) == 1) ends <- starts + ends

  # Subset replicates -------------------------------------------------------
  # use starts and ends to subset each replicate into a list
  reps <- mapply(function(p,q) subset_data(df, p, q, "row", quiet = TRUE),
                 p = starts,
                 q = ends,
                 SIMPLIFY = FALSE)

  # Run calc_rate on reps ---------------------------------------------------

  if(by == "row" || by == "oxygen")
    res <- lapply(reps, function(z) calc_rate(z,
                                              from = from,
                                              to = to,
                                              by = by,
                                              plot = FALSE))

  if(by == "time")
    res <- lapply(reps, function(z) {
      if(!is.null(from)) from_rel <- from + z[[1,1]] else # time relative to this rep
        from_rel <- NULL
      if(!is.null(to)) to_rel <- to + z[[1,1]] else
        to_rel <- NULL

      calc_rate(z,
                from = from_rel,
                to = to_rel,
                by = "time",
                plot = FALSE)
    })

  # Extract summary tables --------------------------------------------------

  summ <- lapply(res, function(x) x$summary) # extract
  summ <- do.call(rbind.data.frame, summ) # bind
  summ$rank <- 1:nrow(summ) # modify rank col
  row_width <- summ$endrow - summ$row # row width for amending row and endrow
  summ$row <- starts + summ$row - 1 # amended cos these refer to subsets
  summ$endrow <- summ$row + row_width

  # Construct output --------------------------------------------------------
  out <- list(
    call = call,
    inputs = inputs,
    dataframe = df,
    subsets = reps,
    results = res,
    summary = summ,
    rate = summ$rate
  )

  class(out) <- "calc_rate.int"


  # Plot --------------------------------------------------------------------
  if(plot) plot(out, ...)

  # Return ------------------------------------------------------------------
  return(out)
}

#' Print calc_rate.int objects
#' @param x calc_rate.int object
#' @param pos integer. Which replicate to print.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
print.calc_rate.int <- function(x, pos = NULL, ...) {
  cat("\n# print.calc_rate.int # -----------------")

  if(is.null(pos)) pos <- 1

  if(length(pos) > 1)
    stop("print.calc_rate.int: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate))
    stop("print.calc_rate.int: Invalid 'pos' input: only ", length(x$rate), " replicates found.")

  cat("\nReplicate", pos, "of", length(x$rate), ":")
  cat("\nRate:", x$rate[pos], "\n")
  cat("\n")
  if(length(x$rate) > 1) cat("To see other replicate results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Summarise calc_rate.int objects
#' @param object calc_rate.int object
#' @param pos integer(s). Which replicate(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
#' @importFrom data.table data.table
summary.calc_rate.int <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate)))
    stop("summary.calc_rate.int: Invalid 'pos' input: only ", length(object$rate), " replicates found.")

  cat("\n# summary.calc_rate.int # ---------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all replicate results:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of results from entered 'pos' replicate(s):")
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

#' Plot calc_rate.int objects
#' @param x calc_rate.int object
#' @param pos integer. Which replicate(s) to plot up to a maximum of 20.
#'   Defaults to 1:20.
#' @param quiet logical. Suppress console output.
#' @param legend logical. Suppress labels and legends.
#' @param type logical. Type of plot to produce. `"rep"`, `"full"`, or
#'   `"cr"`. Defaults to `"rep"`. See Details.
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.calc_rate.int <- function(x, pos = NULL, quiet = FALSE,
                               legend = FALSE,
                               type = "rep", ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if(!(type %in% c("rep", "full", "cr")))
    stop("plot.calc_rate.int: 'type' input not recognised.")

  nreps <- length(x$rate) # number of reps

  if(is.null(pos)) pos <- 1:nreps
  if(any(pos > nreps))
    stop("plot.calc_rate.int: Invalid 'pos' input: only ", nreps, " replicates found.")

  if(!quiet) {
    cat("\n# plot.calc_rate.int # ------------------\n")
    if(length(pos) == nreps) cat(glue::glue("plot.calc_rate.int: Plotting rate from all replicates ..."), sep="\n") else
      cat(glue::glue("plot.calc_rate.int: Plotting rate from selected replicates... \nTo plot others modify 'pos' input."), sep="\n")
  }

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      ps = 10,
      cex = 1,
      cex.main = 1)
  # allows params overriding defaults to be passed
  par(...)

  if(length(pos) == 1)          par(mfrow = c(1,1))
  if(length(pos) == 2)          par(mfrow = c(1,2))
  if(length(pos) %in% c(3,4))   par(mfrow = c(2,2))
  if(length(pos) %in% c(5,6))   par(mfrow = c(2,3))
  if(length(pos) %in% c(7,8,9)) par(mfrow = c(3,3))
  if(length(pos) %in% c(10:12)) par(mfrow = c(3,4))
  if(length(pos) %in% c(13:16)) par(mfrow = c(4,4))
  if(length(pos) %in% c(17:20)) par(mfrow = c(4,5))

  if(length(pos) > 20){
    if(!quiet) message("plot.calc_rate.int: Plotting first 20 selected reps only. To plot others modify 'pos' input.")
    pos <- pos[1:20]
    par(mfrow = c(4,5))
  }

  if(type == "rep") sapply(pos, function(z)
    multi.p(x$results[[z]]$dataframe,
            x$results[[z]]$subsets[[1]],
            legend = legend,
            title = glue::glue("Replicate {z} of {nreps}"),
            tck = -0.005,
            mgp = c(0, 0.1, 0),
            las = 0, ...))
  else if(type == "full") sapply(pos, function(z)
    multi.p(x$dataframe,
            x$results[[z]]$subsets[[1]],
            legend = legend,
            title = glue::glue("Replicate {z} of {nreps}"),
            tck = -0.005,
            mgp = c(0, 0.1, 0),
            las = 0, ...))
  else if(type == "cr") sapply(pos, function(z)
    plot(x$results[[z]], quiet = TRUE, ...))

  if(!quiet) cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Average calc_rate.int object rates
#' @param x calc_rate.int object
#' @param pos integer(s). Which replicate rates to average.
#' @param export logical. Export averaged rate as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
mean.calc_rate.int <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.calc_rate.int # ------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate)))
    stop("mean.calc_rate.int: Invalid 'pos' input: only ", length(x$rate), " replicates found.")

  if(is.null(pos)) {
    pos <- 1:length(x$rate)
    cat("Mean of all replicate rates:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' replicates:")
    cat("\n")
  }
  if(length(x$rate[pos]) == 1)
    message("Only 1 replicate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate[pos])
  out <- mean(x$rate[pos])
  cat("Mean of", n, "replicate rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}

