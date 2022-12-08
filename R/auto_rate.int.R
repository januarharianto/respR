#' Run auto_rate on multiple replicates in intermittent-flow respirometry data
#'
#' `auto_rate.int` allows you to run the `auto_rate()` function on multiple
#' replicates in intermittent-flow respirometry. A `wait` and `measure` phase
#' can be specified for each replicate, and the `auto_rate` analysis is
#' performed within the `measure` region.
#'
#' @details
#'
#' `auto_rate.int` uses the `starts` input to subset each replicate. The `wait`
#' and `measure` inputs control which parts of each replicate data are excluded
#' and included from the rate calculation. It runs `auto_rate` on the `measure`
#' phase in each replicate saving the top `n` ranked results and extracting the
#' rate and other data to a summary table.
#'
#' The `x` input should be an`inspect` object. Alternatively, it can be a
#' two-column data frame containing paired values of time and oxygen from an
#' intermittent-flow experiment in columns 1 and 2 respectively (though we
#' always recommend processing such data in `inspect()` first). If a multiple
#' column dataset is entered as `x` the first two columns are selected by
#' default. If these are not the intended data use `inspect` to select the
#' correct time and oxygen columns.
#'
#' ## `auto_rate` inputs
#'
#' You should be familiar with how `auto_rate` works before using this function.
#' See `help("auto_rate")` and vignettes on the website for full details.
#'
#' The `auto_rate` inputs can be changed by entering different `method` and
#' `width` inputs. The `by` input controls how the `width` is applied. Note if
#' using a proportional `width` input (i.e. between 0 and 1 representing a
#' proportion of the data length) this applies to the length of the `measure`
#' phase of each particular replicate.
#'
#' The `n` input controls how many `auto_rate` results from each replicate to
#' return in the output. By default this is only the top ranked result for the
#' particular `method`, i.e. `n = 1`. This can be changed to return more,
#' however consider carefully if this is necessary as the output will
#' necessarily contain many more rate results which may make it difficult to
#' explore and select results (although see `select_rate()`).
#'
#' ## Specifying replicate structure
#'
#' The `starts` input specifies the locations of the start of each replicate in
#' the data in `x`. This can be in one of two ways:
#'
#' - A single numeric value specifying the number of rows in each replicate
#' starting from the data in the first row. This option should only be used when
#' replicates cycle at regular intervals. This can be a regular row or time
#' interval, as specified via the `by` input. If the first replicate does not
#' start at row 1, the data should be subset so that it does (see
#' `subset_data()`) and example
#' [here](https://januarharianto.github.io/respR/articles/intermittent_long.html).
#' For example, `starts = 600, by = "row"` means the first replicate starts at
#' row 1 and ends at row 600, the second starts at row 601 ends at 1200, and so
#' on.
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
#' row of the dataset. Also for both methods, `by = "time"` inputs do not need
#' to be exact; the closest matching values in the time data are used.
#'
#' Results are presented in the `summary` table with `rep` and `rank` columns to
#' distinguish those from different replicates and their ranking within
#' replicates (if multiple results per replicate have been returned by
#' increasing the `n` input).
#'
#' ## Specifying rate region
#'
#' The `wait` and `measure` inputs are used to specify the region from which to
#' extract a rate and exclude flush periods. They can be entered as row
#' intervals or time values in the units of the input data. The `wait` phase
#' controls the amount of data at the start of each replicate to be ignored,
#' that is excluded from any rate calculations. The `measure` phase determines
#' the region after this from which a rate is calculated. Unlike
#' `calc_rate.int()`, `auto_rate.int` will not necessarily use all of the data
#' in the `measure` phase, but will run the `auto_rate` analysis *within* it
#' using the `method`, `width` and `by` inputs. This may result in rates of
#' various widths depending on the inputs. See `auto_rate()` for defaults and
#' full details of how selection inputs are applied.
#'
#' There is no `flush` phase input since this is assumed to be from the end of
#' the `measure` phase to the end of the replicate.
#'
#' Both `wait` and `measure` can be entered in one of two ways:
#'
#' - Single numeric values specifying a row width or a time period, as specified
#' via the `by` input. Use this if you want to use the *same* `wait` and
#' `measure` phases in every replicate.
#'
#' - If `starts` is a vector of locations of the start of each replicate, these
#' inputs can also be vectors of equal length of row lengths or time periods as
#' specified via the `by` input. This is only useful if you want to use
#' *different* `wait` and/or `measure` phases in different replicates.
#'
#' If `wait = NULL` no wait phase is applied. If `measure = NULL` the data used
#' for analysis is from the start of the replicate or end of the `wait` phase to
#' the last row of the replicate. This will typically include the flush period,
#' so is rarely what you would want.
#'
#' ## Example
#'
#' See examples below for actual code, but here is a simple example. An
#' experiment comprises replicates which cycle at ten minute intervals with data
#' recorded every second. Therefore each replicate will be 600 rows long.
#' Flushes of the respirometer take 3 minutes at the end of each replicate. We
#' want to exclude the first 2 minutes (120 rows) of data in each, and run an
#' `auto_rate` analysis to get an oxygen uptake rate within the following five
#' minute period (300 rows), leaving the three minutes of flushing (180 rows)
#' excluded. The inputs for this would be:
#'
#' `starts = 600, wait = 120, measure = 300, by = "row"`
#'
#' ## Plot
#'
#' If `plot = TRUE` (the default), the result for each rate is plotted on a grid
#' up to a maximum of 20. There are three ways of plotting the results, which
#' can be selected using the `type` input:
#'
#' - `type = "rep"`: The default. Each individual replicate is plotted with the
#' rate region highlighted in yellow. The `wait` and `measure` phases are also
#' highlighted as shaded red and green regions respectively. These are also
#' labelled if `legend = TRUE`.
#'
#' - `type = "full"`: Each replicate rate is highlighted in the context of the
#' whole dataset. May be quite difficult to interpret if dataset is large.
#'
#' - `type = "ar"`: Plots individual replicate results as `auto_rate` objects.
#' Note, these will only show the `measure` phase of the data.
#'
#' For all plot types `pos` can be used to select which rate(s) to plot (default
#' is 1:20), where `pos` indicates rows of the `$summary` table (and hence which
#' `$rep` and `$rank`). This can be passed either in the main function call or
#' when calling `plot()` on output objects. Note for all plot types if `n` has
#' been changed to return more than one rate per replicate these will also be
#' plotted.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `plot()`,
#' `print()`, `summary()`, and `mean()`. For all of these `pos` selects rows of
#' the `$summary` table.
#'
#' - `plot()`: plots the result. See Plot section above.
#'
#' - `print()`: prints the result of a single rate, by default the first. Others
#' can be printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all results and metadata, or the rows
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The `$rep` column
#' indicates the replicate number, and `$rank` column the ranking of each rate
#' *within* each replicate (only used if a different `n` has been passed,
#' otherwise they are all `1`). The summary table (or `pos` rows) can be
#' exported as a separate data frame by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of the rates from every row or those
#' specified by the `pos` input. e.g. `mean(x, pos = 1:5)` Note if a different
#' `n` has been passed this may include multiple rates from each replicate. The
#' mean can be exported as a numeric value by passing `export = TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return Output is a `list` object of class `auto_rate.int` containing a
#'   `auto_rate` object for each replicate in `$results`. The output also
#'   contains a `$summary` table which includes the full rate regression results
#'   from each replicate with replicate number indicated by the `$rep` column.
#'   Output also contains a `$rate` element which contains the rate values from
#'   each replicate in order. The function call, inputs, and other metadata are
#'   also included. Note, that if you have many replicates this object can be
#'   rather large (several MB).
#'
#' @param x object of class `inspect` or `data.frame`. This is the timeseries of
#'   paired values of oxygen against time containing multiple replicates from
#'   which to calculate rates.
#' @param starts Numeric. Row locations or times (in the units of the data in
#'   `x`) of the start of each replicate. If a single value it indicates a
#'   regular interval in rows or time starting from row 1. If a vector, each
#'   entry is the start row or time of an individual replicate. Use of rows or
#'   time is controlled via `by`.
#' @param wait Numeric. A row length or time duration to be applied at the start
#'   of each replicate to *exclude* these data from any rate calculations. Can
#'   be a single value to apply the same wait phase to each replicate, or a
#'   vector of the same length as `starts` of different wait phases for each
#'   replicate. Optional.
#' @param measure Numeric. A row length or time duration to be applied at the
#'   end of the `wait` phase (if used), and used to exclude the flush period.
#'   This is the region within which the `auto_rate` analysis is conducted for
#'   each replicate. Can be a single value to apply the same measure phase to
#'   each replicate, or a vector of the same length as `starts` of different
#'   measure phases for each replicate. Default is `NULL` in which case the
#'   entire replicate is used (which is rarely what is wanted).
#' @param by String. `"row"` or `"time"`. Controls how `starts`, `wait` and
#'   `measure` are applied. It also controls how the `width` is applied in the
#'   `auto_rate` analysis - see `help("auto_rate")`. Default is `"row"`.
#' @param method string. The `auto_rate` `method` to use. Default is `"linear"`.
#'   Others include `"lowest"` and `"highest"`. See `help("auto_rate")` for
#'   descriptions and other methods.
#' @param width numeric. The `width` to use in the `auto_rate` analysis.
#'   Mandatory and should be entered in the correct units of the `by` input. See
#'   `help("auto_rate")` and vignettes on website for how width affects
#'   analyses.
#' @param n integer. How many `auto_rate` results to return for each replicate.
#'   Default is `1`.
#' @param plot logical. Default is `TRUE`. Plots the results. See 'Plotting'
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
#' # Calculate the most linear rate within each replicate
#' auto_rate.int(urch_insp,
#'               starts = c(1, 2101, 3901),
#'               by = "row",
#'               method = "linear",
#'               width = 400) |>
#'   summary()
#'
#' # Calculate the lowest rate within each replicate across
#' # 5 minutes (300 rows). For this we need to specify a 'measure' phase
#' # so that the flush is excluded.
#' auto_rate.int(urch_insp,
#'               starts = c(1, 2101, 3901),
#'               measure = 1000,
#'               by = "row",
#'               method = "lowest",
#'               width = 300) |>
#'   summary()
#'
#' # You can even specify different 'measure' phases in each rep
#' auto_rate.int(urch_insp,
#'               starts = c(1, 2101, 3901),
#'               measure = c(1000, 800, 600),
#'               by = "row",
#'               method = "lowest",
#'               width = 300) |>
#'   summary()
#'
#' # We usually don't want to use the start of a replicate just after the flush,
#' # so we can specify a 'wait' phase. We can also specify 'starts', 'wait',
#' # 'measure', and 'width' in units of time instead of rows.
#' #
#' # By time
#' # (this time we save the result)
#' urch_res <- auto_rate.int(urch_insp,
#'                           starts = c(0, 35, 65), # start locations in minutes
#'                           wait = 2,              # wait for 2 mins
#'                           measure = 10,          # measure phase of 10 mins
#'                           by = "time",           # apply inputs by time values
#'                           method = "lowest",     # get the 'lowest' rate...
#'                           width = 5) |>          #  ... of 5 minutes width
#'   summary()
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
#' # Calculate the most linear rate from the same 6-minute region in every
#' # replicate. Replicates cycle at every 660 rows.
#' zeb_res <- auto_rate.int(zeb_insp,
#'                          starts = 660,
#'                          wait = 120, # exclude first 2 mins
#'                          measure = 360, # measure period of 6 mins after 'wait'
#'                          method = "linear",
#'                          width = 200, # starting value for linear analysis
#'                          plot = TRUE) |>
#'   summary()
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
#' # type = "ar" - the 'auto_rate' object for selected replicates in 'pos' is plotted
#' # Note this shows the 'measure' phase only
#' plot(urch_res, pos = 2, type = "ar")
#'
#' # See vignettes on website for how to adjust and convert rates from auto_rate.int
#' }

auto_rate.int <- function(x,
                          starts = NULL,
                          wait = NULL,
                          measure = NULL,
                          by = "row",
                          method = "linear",
                          width = NULL,
                          n = 1,
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
                 method = method,
                 width = width,
                 n = n,
                 plot = plot)

  # Checks ------------------------------------------------------------------

  # x
  if(!(class.val(x, df = TRUE, insp = TRUE)))
    stop("auto_rate.int: Input must be a 'data.frame' or 'inspect' object.")

  # Extract data
  if(any(class(x) %in% "inspect")) df <- x$dataframe else
    df <- x

  # Format as data.table
  df <- data.table::data.table(df)
  if (length(df) > 2) {
    message("auto_rate.int: Multi-column dataset detected in input. Selecting first two columns by default.\n  If these are not the intended data, inspect() or subset the data frame columns appropriately before running auto_rate.int()")
    df <- df[, 1:2]
  }

  ## verify by input
  ## - this is also done in auto_rate
  by <- verify_by(by, req = TRUE, default = "row",
                  which = c("t", "r"),
                  msg = "auto_rate.int:")

  # starts
  #  - required, numeric, integer, within df row range
  if(by == "row") input.val(starts, num = TRUE, int = TRUE, req = TRUE,
                            range = c(1,nrow(df)),
                            msg = "auto_rate.int: 'starts' -")
  #  - required, numeric, within df units time range
  if(by == "time") input.val(starts, num = TRUE, req = TRUE,
                             #range = c(range(df)[[1]], range(df)[[2]]),
                             msg = "auto_rate.int: 'starts' -")

  # wait
  # - not required, but if entered numeric, integer, same length as starts
  if(by == "row") input.val(wait, num = TRUE, int = TRUE, req = FALSE,
                            range = c(0,nrow(df)),
                            msg = "auto_rate.int: 'wait' -")
  if(by == "time") input.val(wait, num = TRUE, req = FALSE,
                             msg = "auto_rate.int: 'wait' -")
  # plus if not null and not length == 1, must be same length as 'starts'
  if(!is.null(wait) && length(wait) > 1 && length(wait) != length(starts))
    stop("auto_rate.int: For a vector input 'wait' should be the same length as 'starts'.")

  # measure
  # - not required, but if entered numeric, integer
  if(by == "row") input.val(measure, num = TRUE, int = TRUE, req = FALSE,
                            range = c(1,nrow(df)),
                            msg = "auto_rate.int: 'measure' -")
  if(by == "time") input.val(measure, num = TRUE, req = FALSE,
                             msg = "auto_rate.int: 'measure' -")
  # plus if not null and not length == 1, must be same length as 'starts'
  if(!is.null(measure) && length(measure) > 1 && length(measure) != length(starts))
    stop("auto_rate.int: For a vector input 'measure' should be the same length as 'starts'.")


  # width
  # Force a width input. This is to stop the auto_rate default 0.2 being applied since it's rarely
  # appropriate for intermittent-flow reps
  if(is.null(width)){
    if(by == "row") stop("auto_rate.int: Please enter a 'width'. This should be in the 'by' input of number of rows.")
    if(by == "time") stop("auto_rate.int: Please enter a 'width'. This should be in the 'by' input of a time duration in the correct units.")
  }
  if(by == "row" && width < 1) input.val(width, num = TRUE, int = FALSE, req = TRUE,
                                         max = 1, min = 1, range = c(0.001, 0.999),
                                         msg = "auto_rate.int: 'width' -")
  if(by == "row" && width >= 1) input.val(width, num = TRUE, int = FALSE, req = TRUE,
                                          max = 1, min = 1, range = c(2, nrow(df)),
                                          msg = "auto_rate.int: 'width' -")
  if(by == "time") input.val(width, num = TRUE, int = FALSE, req = TRUE,
                             max = 1, min = 1, range = c(0,Inf),
                             msg = "auto_rate.int: 'width' -")
  if(by == "time" && width > 0 && width < 1)
    message("auto_rate.int: 'width' input is between 0 and 1. Check this value is what you intend.\n Proportional width inputs (as used in auto_rate) are not supported in auto_rate.int. \n The 'width' must be entered as a row width or a duration in the same units as the input time data.")

  # n
  input.val(n, num = TRUE, int = TRUE, req = TRUE,
            max = 1, min = 1, range = c(-Inf,Inf),
            msg = "auto_rate.int: 'n' -")

  # Format starts -----------------------------------------------------------
  if(length(starts) == 1 && by == "row") starts <- seq(1, nrow(df), starts)
  if(length(starts) == 1 && by == "time") starts <- seq(df[[1]][1], tail(df[[1]], 1), starts)

  # Format ends -------------------------------------------------------------
  # keep this, but internal - need it for subsetting
  if(by == "row") ends <- c(starts[2:length(starts)]-1, nrow(df))
  if(by == "time") {
    # convert starts and ends times to row locations since we will subset by row later
    # to avoid data overlapping between reps
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
    message("auto_rate.int: The `measure` input is NULL. Calculating rate to the end of the replicate.")
    if(by == "row") measure <- nrow(df)
    if(by == "time") measure <- max(df[[1]])
  }

  # Convert to 'from' and 'to'
  # We use these to do second subset
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
  reps <- mapply(function(p,q) subset_data(df, p, q, by = "row", quiet = TRUE),
                 p = starts,
                 q = ends,
                 SIMPLIFY = FALSE)
  # if by = "time", remove last row of each (except last rep) since this "belongs" to
  # the next replicate - this is because of how we constructed 'ends' above
  #if(by == "time") reps[1:(length(reps)-1)] <-
  #  lapply(reps[1:(length(reps)-1)], function(z) z[-nrow(z),])

  # Run auto_rate on reps ---------------------------------------------------

  if(by == "row")
    res <- mapply(function(p,q,r,s,t) {
      sub <- subset_data(p,
                         from = r,
                         to = s,
                         by = by,
                         quiet = TRUE)

      out <- auto_rate.rep(sub,
                           method = method,
                           width = width,
                           by = by,
                           plot = FALSE,
                           rep = q,
                           n = n,
                           rep_row = t,
                           meas_row = r,
                           meas_endrow = s,
                           rep_data = p)

      ## add actual start row of this rep
      ## for fixing row/endrow after this
      #out$summary$start_row <- starts[q]

      return(out)
    },
    p = reps,
    q = 1:length(reps),
    r = from,
    s = to,
    t = sort(c(0, cumsum(sapply(reps, nrow))[1:(length(reps)-1)])),
    SIMPLIFY = FALSE)

  #p=reps
  #p=p[[1]]
  #q=q[1]
  #r=r[1]
  #s=s[1]
  #t=t[1]

  if(by == "time")
    res <- mapply(function(p,q,r,s,t) {

      if(!is.null(from)) from_rel <- r + p[[1,1]] else # time relative to this rep
        from_rel <- NULL
      if(!is.null(to)) to_rel <- s + p[[1,1]] else
        to_rel <- NULL

      sub <- subset_data(p,
                         from = from_rel,
                         to = to_rel,
                         by = by,
                         quiet = TRUE)

      # need offset of sub start row from replicate start row
      # this is to correct summary row/endrow columns later
      # So we use the same code from truncate data to get this
      # Only need the 'from' value
      # row of the measure phase within rep (rep for n)
      meas_row <- which.min(abs(p[[1]] - from_rel)) - 1
      # this is for plotting
      meas_endrow <- which.min(abs(p[[1]] - to_rel)) - 1

      # if(to > rng[2]) to <- rng[2]
      # #out <- dt[dt[[1]] >= from & dt[[1]] <= to] # old method
      # # new method - finds closest value to each time
      # out <- dt[dt[[1]] >= dt[[1]][which.min(abs(p[[1]] - from))]
      #           & dt[[1]] <= dt[[1]][which.min(abs(dt[[1]] - to))]]

      out <- auto_rate.rep(sub,
                           method = method,
                           width = width,
                           by = by,
                           plot = FALSE,
                           rep = q,
                           n = n,
                           rep_row = t,
                           meas_row = meas_row,
                           meas_endrow = meas_endrow,
                           rep_data = p)

      return(out)
    },
    p = reps,
    q = 1:length(reps),
    r = from,
    s = to,
    # this is the row number of reps in the orig data (using n)
    t = sort(c(0, cumsum(sapply(reps, nrow))[1:(length(reps)-1)])),
    SIMPLIFY = FALSE)

  # Extract summary tables --------------------------------------------------

  summ <- lapply(res, function(x) x$summary) # extract
  summ <- do.call(rbind.data.frame, summ) # bind
  row_width <- summ$endrow - summ$row # row width for amending row and endrow
  if(by == "row"){
    summ$row <- summ$row + summ$rep_row + summ$meas_row - 1
    summ$endrow <- summ$row + row_width
  }
  if(by == "time"){
    summ$row <- summ$row + summ$rep_row + summ$meas_row
    summ$endrow <- summ$row + row_width
  }

  # Remove rep_row and meas_row from summary and summary in every auto_rate result in res
  summ <- summ[,-c(14,15,16)]
  # Put extra summary columns at top level of auto_rate results for each rep
  # This is for plotting and so object will still work as ar object
  res <- lapply(res, function(x) {
    x$rep_row <- x$summary$rep_row
    return(x)
  })
  res <- lapply(res, function(x) {
    x$meas_row <- x$summary$meas_row
    return(x)
  })
  res <- lapply(res, function(x) {
    x$meas_endrow <- x$summary$meas_endrow
    return(x)
  })
  # remove from summary
  res <- lapply(res, function(x) {
    x$summary <- x$summary[,-c(14,15,16)]
    return(x)
  })

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

  class(out) <- "auto_rate.int"

  # Plot --------------------------------------------------------------------
  if(plot) plot(out, ...)

  # Return ------------------------------------------------------------------
  return(out)
}

#' Print auto_rate.int objects
#' @param x auto_rate.int object
#' @param pos integer. Which replicate to print.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
print.auto_rate.int <- function(x, pos = NULL, ...) {
  cat("\n# print.auto_rate.int # -----------------")

  summ <- x$summary

  if(is.null(pos)) pos <- 1
  if(length(pos) > 1)
    stop("print.auto_rate.int: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > nrow(summ))
    stop("print.auto_rate.int: Invalid 'pos' input: only ", nrow(summ), " rates found.")


  reps <- unique(summ$rep) # all reps
  nreps <- length(reps) # number of reps
  rep <- summ$rep[pos] # this rep
  ranks <- summ$rank[summ$rep == rep] # all ranks in this rep
  nranks <- length(ranks) # number of ranks in this rep
  rank <- summ$rank[pos] # this rank in this rep


  cat("\n\nReplicate ", rep, " of ", nreps, ", Rank ", rank, " of ", nranks, ":", sep = "")
  cat("\nRate:", x$rate[pos], "\n")
  cat("\n")
  if(length(x$rate) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Summarise auto_rate.int objects
#' @param object auto_rate.int object
#' @param pos integer(s). Which replicate(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
#' @importFrom data.table data.table
summary.auto_rate.int <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate)))
    stop("summary.auto_rate.int: Invalid 'pos' input: only ", length(object$rate), " rates found.")

  cat("\n# summary.auto_rate.int # ---------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all rate results:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of results from entered 'pos' row(s):")
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

#' Plot auto_rate.int objects
#' @param x auto_rate.int object
#' @param pos integer. Which replicate(s) to plot up to a maximum of 20.
#'   Defaults to 1:20.
#' @param quiet logical. Suppress console output.
#' @param legend logical. Suppress labels and legends.
#' @param type logical. Type of plot to produce. `"rep"`, `"full"`, or
#'   `"ar"`. Defaults to `"rep"`. See Details.
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.auto_rate.int <- function(x, pos = NULL, quiet = FALSE,
                               legend = FALSE,
                               type = "rep", ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if(!(type %in% c("rep", "full", "ar")))
    stop("plot.auto_rate.int: 'type' input not recognised.")

  nrates <- length(x$rate) # number of reps

  if(is.null(pos)) pos <- 1:nrates
  if(any(pos > nrates))
    stop("plot.auto_rate.int: Invalid 'pos' input: only ", nrates, " rates found.")

  if(!quiet) {
    cat("\n# plot.auto_rate.int # ------------------\n")
    if(length(pos) == nrates) cat(glue::glue("plot.auto_rate.int: Plotting all rates ..."), sep="\n") else
      cat(glue::glue("plot.auto_rate.int: Plotting selected rate(s)... \nTo plot others modify 'pos' input."), sep="\n")
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
    if(!quiet) message("plot.auto_rate.int: Plotting first 20 selected rates only. To plot others modify 'pos' input.")
    pos <- pos[1:20]
    par(mfrow = c(4,5))
  }

  # reps and ranks for pos input
  # also row numbers for "full" type
  rep_ranks <- x$summary[pos, c(1:2, 7:8)]

  ## add wait (up to meas_row) and measure rows
  #rep_ranks$meas_row <- c(sapply(x$results, function(z) z$meas_row))
  #rep_ranks$meas_endrow <- c(sapply(x$results, function(z) z$meas_endrow))

  nreps <- length(unique(x$summary$rep))

  if(type == "rep") apply(rep_ranks, 1, function(z) {

    dt <- x$results[[z[1]]]$rep_data
    start <- x$results[[z[1]]]$summary$row[z[2]] + x$results[[z[1]]]$meas_row[z[2]]
    end <- x$results[[z[1]]]$summary$endrow[z[2]] + x$results[[z[1]]]$meas_row[z[2]]
    sdt <- dt[start:end]
    nranks <- max(x$summary$rank[x$summary$rep == z[1]])

    multi.p(dt,
            sdt,
            legend = legend,
            title = glue::glue("Rep {z[1]} of {nreps}, Rank {z[2]} of {nranks}"),
            tck = -0.005,
            mgp = c(0, 0.1, 0),
            las = 0, ...)

    # wait region shaded
    # only if not NULL
    if(!is.null(x$inputs$wait)){
      rect(xleft = 0,
           ybottom = min(dt[[2]]),
           xright = x$results[[z[1]]]$meas_row[z[2]],
           ytop = max(dt[[2]]),
           col = rgb(255/255,0/255,0/255,  alpha = 0.18),
           lty = 0)
      # wait region title
      if(legend){
        xpos <- mean(c(0, x$results[[z[1]]]$meas_row[z[2]]))
        ypos <- max(dt[[2]])-(diff(range(dt[[2]]))*0.02)
        text(xpos, ypos, labels = 'wait', col = "red", font = 2)
      }
    }
    # measure region shaded
    rect(xleft = x$results[[z[1]]]$meas_row[z[2]],
         ybottom = min(dt[[2]]),
         xright = x$results[[z[1]]]$meas_endrow[z[2]],
         ytop = max(dt[[2]]),
         col = rgb(15/255,245/255,53/255,  alpha = 0.2),
         lty = 0)
    # measure region title
    if(legend){
      xpos <- mean(c(x$results[[z[1]]]$meas_row[z[2]], x$results[[z[1]]]$meas_endrow[z[2]]))
      ypos <- max(dt[[2]])-(diff(range(dt[[2]]))*0.02)
      text(xpos, ypos, labels = 'measure', col = "darkgreen", font = 2)
    }
  })
  else if(type == "full") apply(rep_ranks, 1, function(z) {

    dt <- x$dataframe
    #start <- x$results[[z[1]]]$summary$row[z[2]]
    start <- z[3]
    #end <- x$results[[z[1]]]$summary$endrow[z[2]]
    end <- z[4]
    sdt <- dt[start:end]
    nranks <- max(x$summary$rank[x$summary$rep == z[1]])

    multi.p(dt,
            sdt,
            legend = legend,
            title = glue::glue("Rep {z[1]} of {nreps}, Rank {z[2]} of {nranks}"),
            tck = -0.005,
            mgp = c(0, 0.1, 0),
            las = 0, ...)
  })
  else if(type == "ar") apply(rep_ranks, 1, function(z)
    plot(x$results[[z[1]]], pos = z[2], quiet = TRUE, ...))

  if(!quiet) cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Average auto_rate.int object rates
#' @param x auto_rate.int object
#' @param pos integer(s). Which replicate rates to average.
#' @param export logical. Export averaged rate as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
mean.auto_rate.int <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.auto_rate.int # ------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate)))
    stop("mean.auto_rate.int: Invalid 'pos' input: only ", length(x$rate), " rates found.")

  if(is.null(pos)) {
    pos <- 1:length(x$rate)
    cat("Mean of all rates:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' rows:")
    cat("\n")
  }
  if(length(x$rate[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate[pos])
  out <- mean(x$rate[pos])
  cat("Mean of", n, "rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}




# Util fns ----------------------------------------------------------------

#' Function for auto_rate with replicate number in summary plus limit to n
#' results
#'
#' This is an internal function for `auto_rate.int()`. Runs `auto_rate` and
#' replaces the `$rep` column in `$summary` with the `rep` input. Also limits
#' number of results returned using `n`.
#'
#' @param x df or inspect obj
#' @param method auto_rate method
#' @param width auto_rate width
#' @param by auto_rate by
#' @param plot auto_rate plot
#' @param rep The replicate
#' @param n number of results to return
#' @param rep_row start row of this rep in the larger dataset
#' @param meas_row start row of the measure phase in *this* rep
#' @param rep_data df of the entire replicate. Used in plotting.
#' @param ... pass plot stuff
#'
#' @return a `auto_rate` object with `summary$rep` filled in with appropriate
#'   replicate number
#'
#' @keywords internal
auto_rate.rep <- function(x,
                          method = "linear",
                          width = NULL,
                          by = "row",
                          plot = TRUE,
                          rep = 1,
                          n = 1,
                          rep_row = NULL,
                          meas_row = NULL,
                          meas_endrow = NULL,
                          rep_data = NULL,
                          ...){

  # run auto_rate
  out <- suppressMessages(auto_rate(x,
                                    method = method,
                                    width = width,
                                    by = by,
                                    plot = plot,
                                    ...))
  # add rep
  out$summary$rep <- rep

  # subset to top n results
  # if n greater than results available, make it number available
  if(n > length(out$rate)) n <- length(out$rate)

  # subset relevant elements of auto_rate results
  out$summary <- out$summary[1:n,]
  out$rate <- out$rate[1:n]
  out$metadata$subset_regs <- n
  out$peaks <- out$peaks[1:n,]
  out$rep_data <- rep_data

  out$summary$rep_row <- rep_row
  out$summary$meas_row <- meas_row
  out$summary$meas_endrow <- meas_endrow

  class(x) <- c(class(x), "auto_rate.rep")

  return(out)
}
