#' Extract rates from multiple replicates in intermittent-flow respirometry data
#'
#' `calc_rate.int` allows you to extract an oxygen uptake or production rate
#' from multiple replicates in intermittent-flow respirometry. It allows you to
#' easily use consistent selection parameters to extract a single rate from each
#' replicate, for example a specific time range or row range.
#'
#' `calc_rate.int` uses the `starts` input to subset each replicate. The `wait`
#' and `measure` inputs control which parts of each replicate data are excluded
#' and included from the rate calculation. It extracts a rate from each
#' replicate using these, and saves it and other data to a summary table.
#'
#' The `x` input should be an`inspect` object. Alternatively, it can be a
#' two-column data frame containing paired values of time and oxygen from an
#' intermittent-flow experiment in columns 1 and 2 respectively (though we
#' always recommend processing such data in `inspect()` first). If a multiple
#' column dataset is entered as `x` the first two columns are selected by
#' default. If these are not the intended data use `inspect` to select the
#' correct time and oxygen columns.
#'
#' ## Specifying replicate structure
#'
#' The `starts` input specifies the locations of the start of each replicate in
#' the data in `x`. This can be in one of two ways:
#'
#' - A single numeric value specifying how replicates are spaced starting from
#' the data in the first row. This option should only be used when replicates
#' cycle at regular intervals. This can be a regular row or time interval, as
#' specified via the `by` input. If the first replicate does not start at row 1,
#' the data should be subset so that it does (see `subset_data()`) and example
#' [here](https://januarharianto.github.io/respR/articles/intermittent_long.html).
#'
#' - A numeric vector of row locations or times, as specified via the `by`
#' input, of the start of each individual replicate. The first replicate does
#' not have to start at the first row of the data, and all data after the last
#' entry is assumed to be part of the final replicate. Regular `R` syntax such
#' as `seq()`, `1:10`, etc. is also accepted, so can be used to specify both
#' regular and irregular replicate spacing.
#'
#' For both methods it is assumed each replicate ends at the row preceding the
#' start of the next replicate, or in the case of the last replicate the final
#' row of the dataset.
#'
#' ## Specifying rate region
#'
#' The `wait` and `measure` inputs are used to specify the region from which to
#' extract a rate and exclude flush periods. They can be entered as row
#' intervals or time values in the units of the input data. The `wait` phase
#' controls the amount of data at the start of each replicate to be ignored,
#' that is not used in rate calculations. The `measure` phase determines the
#' region after this over which a rate is calculated. There is no `flush` phase
#' input since this is assumed to be from the end of the `measure` phase to the
#' end of the replicate.
#'
#' Both `wait` and `measure` can be entered in one of two ways:
#'
#' - Single numeric values specifying a row width or a time period, as specified
#' via the `by` input. Use this if you want to use the *same* `wait` and
#' `measure` phases in every replicate, that is extract a rate from the same
#' region of each.
#'
#' - If `starts` is a vector of locations of the start of each replicate, these
#' inputs can also be vectors of equal length of row lengths or time periods as
#' specified via the `by` input. This is only useful if you want to use
#' *different* `wait` and/or `measure` phases in different replicates.
#'
#' If `wait = NULL` no wait phase is applied. If `measure = NULL` the rate is
#' extracted from the start of the replicate or end of the `wait` phase to the
#' last row of the replicate. This will typically include the flush period, so
#' is rarely what you would want. Similarly if any `measure` input is beyond the
#' available values in the replicate the closest value (row or time) is used
#' instead, which again would typically be the last row of the replicate.
#'
#' ## Example
#'
#' See examples below for actual code, but here is a simple example. An
#' experiment comprises replicates which cycle at ten minute intervals with data
#' recorded every second. Therefore each replicate will be 600 rows long.
#' Flushes of the respirometer take 3 minutes at the end of each replicate. We
#' want to exclude the first 2 minutes (120 rows) of data in each, and measure
#' an oxygen uptake rate for five minutes (300 rows), leaving the three minutes
#' of flushing (180 rows) excluded. The inputs for this would be:
#'
#' `starts = 600, wait = 120, measure = 300, by = "row"`
#'
#' ## More details
#'
#' Only a single rate can be extracted from each replicate. If for some reason
#' you need to extract multiple rates from single replicates use `subset_data()`
#' and `calc_rate()` which accepts multiple `from` and `to` inputs. Similarly,
#' the `calc_rate` method of `by = "oxygen"` is not supported in
#' `calc_rate.int`. See vignettes on the website for examples of alternative
#' ways of iterating `calc_rate` across multiple replicates if you need to get
#' around these constraints.
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
#' rate region (i.e. `measure` phase) highlighted in yellow. The `wait` and
#' `measure` phases are also highlighted with red and green backgrounds
#' respectively. These are also labelled if `legend = TRUE`.
#'
#' - `type = "full"`: Each replicate rate (i.e. `measure` phase) is highlighted
#' in the context of the whole dataset. May be quite difficult to interpret if
#' dataset is large.
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
#' - `summary()`: prints summary table of all results and metadata, or the rows
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The `$rep` column
#' indicates the replicate number. The summary table can be exported as a
#' separate data frame by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of the rates from every replicate, or the
#' rows specified by the `pos` input. e.g. `mean(x, pos = 1:5)` The mean can be
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
#'   from each replicate with replicate number indicated by the `$rep` column.
#'   Output also contains a `$rate` element which contains the rate values from
#'   each replicate in order. The function call, inputs, and other metadata are
#'   also included. Note, that if you have many replicates this object can be
#'   rather large (several MB).
#'
#' @param x Object of class `inspect` or `data.frame`. This is the timeseries of
#'   paired values of oxygen against time containing multiple replicates from
#'   which to calculate rates.
#' @param starts Integer(s). Row locations or times of the start of each
#'   replicate. A single value input indicates a regular interval in rows or
#'   time units starting at the first row of the data in `x`. If replicates do
#'   not cycle at a regular interval, a vector of the row or time of each
#'   replicate can be entered. The function assumes each replicate ends at the
#'   row preceding the start of the next replicate, or for the final replicate
#'   the final row of the dataset.
#' @param wait Numeric. Rows or time period to exclude at the start of each
#'   replicate. Default is `NULL` in which case no wait phase is applied. See
#'   Details.
#' @param measure Numeric. Rows or time period over which to calculate rate in
#'   each replicate. Applied directly after `wait` phase. Default is `NULL` in
#'   which case the entire replicate is used. See Details.
#' @param by String. `"row"` or `"time"`. Defaults to `"row"`. Method by which
#'   `starts`, `wait` and `measure` are applied.
#' @param plot Logical. Default is `TRUE`. Plots the results. See 'Plotting'
#'   section for details.
#' @param ... Allows additional plotting controls to be passed, such as `type`,
#'   `pos`, `legend`, and `quiet`.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Irregular replicate structure ------------------------------------------
#'
#' # Prepare the data to use in examples
#' # Note in this dataset each replicate is a different length!
#' data <- intermittent.rd
#' # Convert time to minutes (to show different options below)
#' data[[1]] <- round(data[[1]]/60, 2)
#' # Inspect
#' urch_insp <- inspect(data)
#'
#' # Calculate rate across each entire replicate
#' # This leads to erroneous rates because the flush is included
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3901))
#'
#' # So instead we also specify a 'measure' phase
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3901),
#'               measure = 1000)
#'
#' # You can even specify different 'measure' phases in each rep
#' calc_rate.int(urch_insp,
#'               starts = c(1, 2101, 3901),
#'               measure = c(1500, 1200, 200))
#'
#' # We usually don't want to use the start of a replicate just after the flush,
#' # so we specify a 'wait' phase. We can also specify 'starts', 'wait' and
#' # 'measure' in units of time instead of rows.
#' #
#' # By time
#' # (this time we save the result)
#' urch_res <- calc_rate.int(urch_insp,
#'                           starts = c(0, 35, 65), # start locations in minutes
#'                           wait = 2,              # wait for 2 mins
#'                           measure = 10,          # measure for 10 mins
#'                           by = "time")
#'
#' # Regular replicate structure --------------------------------------------
#'
#' # If replicates cycle at regular intervals, 'starts' can be used to specify
#' # the spacing in rows or time, starting at row 1. Therefore data must be
#' # subset first so that the first replicate starts at row 1.
#' #
#' # Subset and inspect data
#' zeb_insp <- zeb_intermittent.rd |>
#'   subset_data(from = 5840,
#'               to = 75139,
#'               by = "row",
#'               quiet = TRUE) |>
#'   inspect()
#'
#' # Calculate a rate from same 6-minute region in every replicate.
#' # Replicates cycle at every 660 rows.
#' zeb_res <- calc_rate.int(zeb_insp,
#'                          starts = 660,
#'                          wait = 120, # exclude first 2 mins
#'                          measure = 360, # rate from 6 mins after 'wait'
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
#' # type = "rep" - the default. Each replicate plotted on a grid with rate
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
                          wait = NULL,
                          measure = NULL,
                          by = "row",
                          plot = TRUE,
                          ...)
{

  ## Save function call for output
  call <- match.call()
  ## Save inputs for output
  inputs <- list(x = x,
                 starts = starts,
                 wait = wait,
                 measure = measure,
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
    message("calc_rate.int: Multi-column dataset detected in input. Selecting first two columns by default.\n  If these are not the intended data, inspect() or subset the data frame columns appropriately before running calc_rate.int()")
    df <- df[, 1:2]
  }

  # starts
  #  - required, numeric, integer, within df row range
  if(by == "row") input.val(starts, num = TRUE, int = TRUE, req = TRUE,
                            range = c(1,nrow(df)),
                            msg = "calc_rate.int: 'starts' -")
  #  - required, numeric, within df units time range
  if(by == "time") input.val(starts, num = TRUE, req = TRUE,
                            range = c(range(df)[[1]], range(df)[[2]]),
                             msg = "calc_rate.int: 'starts' -")

  # wait
  # - not required, but if entered numeric, integer
  if(by == "row") input.val(wait, num = TRUE, int = TRUE, req = FALSE,
                            range = c(1,nrow(df)),
                            msg = "calc_rate.int: 'wait' -")
  if(by == "time") input.val(wait, num = TRUE, req = FALSE,
                             msg = "calc_rate.int: 'wait' -")
  # plus if not null and not length == 1, must be same length as 'starts'
  if(!is.null(wait) && length(wait) > 1 && length(wait) != length(starts))
    stop("calc_rate.int: For a vector input 'wait' should be the same length as 'starts'.")

  # measure
  # - not required, but if entered numeric, integer
  if(by == "row") input.val(measure, num = TRUE, int = TRUE, req = FALSE,
                            range = c(1,nrow(df)),
                            msg = "calc_rate.int: 'measure' -")
  if(by == "time") input.val(measure, num = TRUE, req = FALSE,
                             msg = "calc_rate.int: 'measure' -")
  # plus if not null and not length == 1, must be same length as 'starts'
  if(!is.null(measure) && length(measure) > 1 && length(measure) != length(starts))
    stop("calc_rate.int: For a vector input 'measure' should be the same length as 'starts'.")

  ## verify by input
  ## - this is also done in calc_rate but we want to disallow "oxygen"
  by <- by.val(by, req = TRUE, default = "row",
                  which = c("t", "r"),
                  msg = "calc_rate.int")

  # Format starts -----------------------------------------------------------
  if(length(starts) == 1 && by == "row") starts <- seq(1, nrow(df), starts)
  if(length(starts) == 1 && by == "time") starts <- seq(df[[1]][1], tail(df[[1]], 1), starts)


  # Format ends -------------------------------------------------------------
  # keep this, but internal - need it for subsetting
  if(by == "row") ends <- c(starts[2:length(starts)]-1, nrow(df))
  if(by == "time") {
    # convert starts times to row locations since we will subset by row later to avoid data
    # overlapping between reps
    starts <- sapply(starts, function(z) which.min(abs(df[[1]] - z)))
    ends <- c(starts[2:length(starts)]-1, nrow(df))
  }

  # Format wait and measure -------------------------------------------------

  # If wait and measure NULL
  if(is.null(wait)) wait <- 0
  # Could make this an error and force input
  # For now make it whole replicate
  # These will cover any eventuality and make it max value
  if(is.null(measure)) {
    message("calc_rate.int: The `measure` input is NULL. Calculating rate to the end of the replicate.")
    if(by == "row") measure <- nrow(df)
    if(by == "time") measure <- max(df[[1]])
  }

  # Convert to 'from' and 'to'
  if(by == "row") {
    from <- wait + 1
    to <- wait + measure
  }
  if(by == "time") {
    from <- wait
    to <- wait + measure
  }

  # Subset replicates -------------------------------------------------------

  # use starts and ends to subset each replicate into a list
  reps <- mapply(function(p,q) truncate_data(df, p, q, by = "row"),
                 p = starts,
                 q = ends,
                 SIMPLIFY = FALSE)

  # Run calc_rate on reps ---------------------------------------------------

  if(by == "row")
    res <- mapply(function(p,q,r,s) calc_rate.rep(p,
                                                  from = r,
                                                  to = s,
                                                  by = by,
                                                  plot = FALSE,
                                                  rep = q),
                  p = reps,
                  q = 1:length(reps),
                  r = from, # this in mapply call to handle vector inputs
                  s = to,
                  SIMPLIFY = FALSE)

  if(by == "time")
    res <- mapply(function(p,q,r,s) {

      if(!is.null(from)) from_rel <- r + p[[1,1]] else # time relative to this rep
        from_rel <- NULL
      if(!is.null(to)) to_rel <- s + p[[1,1]] else
        to_rel <- NULL

      calc_rate.rep(p,
                    from = from_rel,
                    to = to_rel,
                    by = "time",
                    plot = FALSE,
                    rep = q)},
      p = reps,
      q = 1:length(reps),
      r = from,
      s = to,
      SIMPLIFY = FALSE)

  # Extract summary tables --------------------------------------------------

  summ <- lapply(res, function(x) x$summary) # extract
  summ <- do.call(rbind.data.frame, summ) # bind
  row_width <- summ$endrow - summ$row # row width for amending row and endrow
  if(by == "row"){
    summ$row <- starts + summ$row - 1 # amended cos these refer to subsets
    summ$endrow <- summ$row + row_width
  }
  if(by == "time"){
    summ$row <- c(0, cumsum(sapply(reps, nrow))[1:(length(reps)-1)]) + summ$row
    summ$endrow <- summ$row + row_width
  }

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
  print(out, nrows = 50, class = FALSE)
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

  if(type == "rep") sapply(pos, function(z) {
    multi.p(x$results[[z]]$dataframe,
            x$results[[z]]$subsets[[1]],
            legend = legend,
            title = glue::glue("Replicate {z} of {nreps}"),
            tck = -0.005,
            mgp = c(0, 0.1, 0),
            las = 0)
    # wait region shaded
    # only if not NULL
    if(!is.null(x$inputs$wait)){
      rect(xleft = 0,
           ybottom = min(x$results[[z]]$dataframe[[2]]),
           xright = x$results[[z]]$summary$row,
           ytop = max(x$results[[z]]$dataframe[[2]]),
           col = rgb(255/255,0/255,0/255,  alpha = 0.18),
           lty = 0)
      # wait region title
      if(legend){
        xpos <- mean(c(0, x$results[[z]]$summary$row))
        ypos <- max(x$results[[z]]$dataframe[[2]])-(diff(range(x$results[[z]]$dataframe[[2]]))*0.02)
        text(xpos, ypos, labels = 'wait', col = "red", font = 2)
      }
    }
    # measure region shaded
    rect(xleft = x$results[[z]]$summary$row,
         ybottom = min(x$results[[z]]$dataframe[[2]]),
         xright = x$results[[z]]$summary$endrow,
         ytop = max(x$results[[z]]$dataframe[[2]]),
         col = rgb(15/255,245/255,53/255,  alpha = 0.2),
         lty = 0)
    # measure region title
    if(legend){
      xpos <- mean(c(x$results[[z]]$summary$row, x$results[[z]]$summary$endrow))
      ypos <- max(x$results[[z]]$dataframe[[2]])-(diff(range(x$results[[z]]$dataframe[[2]]))*0.02)
      text(xpos, ypos, labels = 'measure', col = "darkgreen", font = 2)
    }

  })
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




# Util fns ----------------------------------------------------------------

#' Function for calc_rate with replicate number in summary
#'
#' This is an internal function for `calc_rate.int()`. Runs `calc_rate`
#' and replaces the `$rep` column in `$summary` with the `rep`
#' input.
#'
#' @param x df or inspect obj
#' @param from calc_rate from
#' @param to calc_rate to
#' @param by calc_rate by
#' @param plot calc_rate plot
#' @param rep The replicate
#' @param supp.mess Suppress messages?
#'
#' @return a `calc_rate` object with `summary$rep` filled in with appropriate
#'   replicate number
#'
#' @keywords internal
calc_rate.rep <- function(x,
                          from = NULL,
                          to = NULL,
                          by = "time",
                          plot = TRUE,
                          rep = 1,
                          supp.mess = TRUE,
                          ...){
  if(supp.mess) out <- suppressMessages(calc_rate(x,
                                                  from = from,
                                                  to = to,
                                                  by = by,
                                                  plot = plot,
                                                  ...))
  else out <- calc_rate(x,
                        from = from,
                        to = to,
                        by = by,
                        plot = plot,
                        ...)

  out$summary$rep <- rep

  return(out)
}
