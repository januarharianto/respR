#' Calculate background oxygen uptake or input rates
#'
#' This function calculates the rate of change of oxygen over time from "blank"
#' or control respirometry experiments, to allow for background adjustments of
#' experimental data. It accepts background oxygen~time data as data frames and
#' `inspect` objects. The data must be in the same time and oxygen units as the
#' data from which the rate which will be adjusted was extracted. Multiple
#' columns of background oxygen measurements can be entered as long as they
#' share the same time data. In this case the function returns rates for all
#' columns, and also calculates a mean rate.
#'
#' The main difference between `calc_rate.bg` and `calc_rate`, is that this
#' function allows a rate to be determined from the same region of multiple
#' oxygen data columns, whereas `calc_rate` allows multiple rates to be
#' determined from different regions of a single dataset.
#'
#' ## Units
#'
#' There are no units involved in `calc_rate.bg`. This is a deliberate decision.
#' The units of oxygen concentration and time will be specified later in
#' [`convert_rate()`] when rates are converted to specific output units. It is
#' important however, the background time~oxygen data is in the same time and
#' oxygen units as the data used to determine the rate which will be adjusted.
#'
#' ## Subsetting data regions
#'
#' `calc_rate.bg` does not have internal subsetting of data regions. If you need
#' to subset the data to specific regions you don't want to use, see
#' [`subset_data()`], which allows for easy passing (or piping) of subsets to
#' `calc_rate.bg`.
#'
#' ## Background respiration vs background input of oxygen
#'
#' Most users will be using this function to account for background oxygen
#' consumption rates from microbial activity that need to be quantified and
#' their effects removed from experimental specimen rates. However, there are
#' some experiments where oxygen *input* rates may be of interest, for example
#' in open tank or open arena respirometry where the input of oxygen from the
#' water surface has been calculated or quantified. There are also cases in
#' closed respirometry where there may be an input of oxygen via leaks or oxygen
#' production from photosynthesis which need to be quantified. `calc_rate.bg` is
#' readily capable of quantifying production rates as well as consumption, and
#' these can also be used for adjustments in [`adjust_rate()`].
#'
#' ## Plot
#'
#' A plot is produced (provided `plot = TRUE`) showing all examined columns of
#' oxygen against time (bottom blue axis) and row index (top red axis), with the
#' rate and linear model coefficients. Single rates can be plotted by changing
#' the `pos` input either in the main function call, or by plotting the output,
#' e.g. `plot(object, pos = 2)`. Console output messages can be suppressed using
#' `quiet = TRUE`. If equations obscure the plot they can be suppressed using
#' `legend = FALSE`.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints all background rates, plus the mean background rate.
#'
#' - `summary()`: prints summary table of all results and metadata, or those
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The summary can
#' be exported as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all rates, or those specified by the `pos`
#' input. e.g. `mean(x, pos = 1:5)` The mean can be exported as a separate value
#' by passing `export = TRUE`.
#'
#' @return Output is a `list` object of class `calc_rate.bg` containing original
#'   data, linear models, summary information, and the primary output of
#'   interest `$rate.bg`, which contains a rate for each oxygen column present
#'   in the input data. There is also `$rate.bg.mean` containing the mean of all
#'   background rates. Note, this is not used in `adjust_rate`, where the
#'   `method` input there determines how `$rate.bg` is applied, but can easily
#'   be extracted and applied as an adjustment value if desired.
#'
#' @param x `data.frame` or `inspect` object. This is the data to extract
#'   background rate(s) from.
#' @param time integer. Defaults to 1. This specifies the column number of the
#'   time data.
#' @param oxygen integer value or vector. This specifies the column number(s) of
#'   the oxygen data. Multiple columns of oxygen can be specified. If NULL,
#'   function assumes oxygen data are in *all* columns of the data frame except
#'   the `time` column.
#' @param plot logical. Defaults to TRUE. Plots the data. See Details.
#' @param ... Allows additional plotting controls to be passed, such as `pos`,
#'   `legend = FALSE`, and `quiet = TRUE`.
#'
#' @importFrom data.table data.table
#'
#' @export
#'
#' @examples
#' # Inspect and calculate background rate from two columns
#' inspect(urchins.rd, time = 1, oxygen = 18:19) %>%
#'   calc_rate.bg()
#'
#' # Same example but enter as a data frame, save as an object and use
#' # in adjust_rate
#' bg_rate <- calc_rate.bg(urchins.rd,
#'                         time = 1,
#'                         oxygen = 18:19,
#'                         plot = FALSE)
#'
#' inspect(urchins.rd, 1, 2, plot = FALSE) %>%
#'   calc_rate(from = 10, to = 30, by = "time", plot = FALSE) %>%
#'   adjust_rate(by = bg_rate)
#'
#' # Subset single column data first before calculating background rate
#' subset_data(background_con.rd, from = 5000, to = 20000, by = "time") %>%
#'   calc_rate.bg()

calc_rate.bg <- function(x, time = NULL, oxygen = NULL, plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()
  inputs <- list(x = x,
                 time = time,
                 oxygen = oxygen,
                 plot = plot)

  # Import x from inspect function. We convert to data.frame object here as
  # data.table doesn't like subsetting columns by variable names.
  if(any(class(x) %in% "inspect")) {
    x <- data.frame(x$dataframe)
  } else {
    x <- data.frame(x)}

  ## if NULL use col1 for time, all other cols for oxygen
  if(is.null(time)) {
    time <- 1
    message("calc_rate.bg: Applying column default of 'time = 1'")
  }
  if(is.null(oxygen)) {
    listcols <- seq.int(1, ncol(x))
    oxygen <- listcols[!listcols %in% time]
    message("calc_rate.bg: Applying column default of all non-time column(s) as 'oxygen'")
  }
  if (any(time %in% oxygen)) stop("calc_rate.bg: 'time' and 'oxygen' columns conflict.")

  # Extract data:
  if(any(time > length(x))) stop("calc_rate.bg: Selected 'time' column not present in the input.") else
    xval <- x[time]
  if(any(oxygen > length(x))) stop("calc_rate.bg: Selected 'oxygen' column(s) not present in the input.") else
    yval <- x[oxygen]

  # Ok, convert back to data.table object
  dt <- data.table(xval, yval)

  # number of results
  nres <- length(oxygen)

  # Perform lm fit on each column:
  fit <- lapply(1:nres, function(x) lm(dt[[x + 1]] ~ dt[[1]]))
  # Extract coefficients:
  summary <- data.table(rank = 1:nres,
                        t(sapply(1:length(fit), function(x) coef(fit[[x]]))),
                        (sapply(1:length(fit), function(x) summary(fit[[x]])$r.squared)),
                        rep(1,nres), #row
                        rep(nrow(dt),nres), #endrow
                        rep(dt[[1,1]],nres), #time
                        rep(dt[[nrow(dt),1]],nres), #endtime
                        unlist(dt[1,-1]), #oxy
                        unlist(dt[nrow(dt),-1]) #endoxy
  )
  summary[[11]] <- summary[[3]] # rate

  names(summary) <- c("rank", "intercept_b0", "rate_b1", "rsq",
                      "row", "endrow", "time", "endtime",
                      "oxy", "endoxy", "rate.bg")

  # Generate output:
  bg <- summary$rate_b1
  out <- list(call = call,
              inputs = inputs,
              dataframe = dt,
              lm = fit,
              summary = summary,
              rate.bg = bg,
              rate.bg.mean = mean(bg))
  class(out) <- "calc_rate.bg"

  # Plot data:
  if (plot) plot(out, ...)
  return(out)
}

#' Print calc_rate.bg objects
#' @param x calc_rate.bg object
#' @param ... Pass additional inputs
#' @return Print to console. No returned value.
#' @export
print.calc_rate.bg <- function(x, ...) {

  cat("\n# print.calc_rate.bg # ------------------\n")
  cat("Background rate(s):\n")
  print(x$rate.bg)
  cat("Mean background rate:\n")
  print(x$rate.bg.mean)
  cat("-----------------------------------------\n")
  return(invisible(x))

}

#' Summarise calc_rate.bg objects
#' @param object calc_rate.bg object
#' @param pos integer(s). Which summary row(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @return Print to console. No returned value.
#' @export
summary.calc_rate.bg <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate.bg)))
    stop("summary.calc_rate.bg: Invalid 'pos' rank: only ", length(object$rate.bg), " background rates found.")

  cat("\n# summary.calc_rate.bg # ----------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all background rate results:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of background rate results from entered 'pos' rank(s):")
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

#' Plot calc_rate.bg objects
#' @param x calc_rate.bg object
#' @param pos integer. Which result to plot.
#' @param quiet logical. Suppress console output.
#' @param legend logical. Suppress labels and legends.
#' @param ... Pass additional plotting parameters
#' @return A plot. No returned value.
#' @export
plot.calc_rate.bg <- function(x, pos = NULL, quiet = FALSE, legend = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  nres <- length(x$rate.bg)
  rownums <- 1:nrow(x$dataframe)
  if(!is.null(pos) && length(pos) > 1)
    stop("plot.calc_rate.bg: 'pos' should be a single value.")
  if(!is.null(pos) && pos > nres)
    stop("plot.calc_rate.bg: Invalid 'pos' rank: only ", nres, " rates found.")
  if(!is.null(pos)) nplot <- 1 else
    nplot <- nres

  if(!quiet) cat("\n# plot.calc_rate.bg # -------------------\n")

  # Apply default plotting params
  par(mfrow = n2mfrow(nplot),
      oma = oma_def,
      mai = mai_def,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      ps = 10,
      cex = 1,
      cex.main = 1)  # replace par settings
  # allows params overriding defaults to be passed
  par(...)

  if(length(pos) == 1) {
    sub.p(data.frame(x$dataframe[[1]],
                     x$dataframe[[pos + 1]]),
          rsq = NULL,
          rownums = rownums,
          title = F,
          legend = legend)
    title(main = glue::glue("Column: {names(x$dataframe)[pos+1]}"), line = 1.2,
          adj = 0)

  } else lapply(1:nres, function(z) {
    sub.p(data.frame(x$dataframe[[1]],
                     x$dataframe[[z + 1]]),
          rsq = NULL,
          rownums = rownums,
          title = F,
          legend = legend)
    title(main = glue::glue("Column: {names(x$dataframe)[z+1]}"), line = 1.2,
          adj = 0)
  })

  if(length(pos) == 1) mtext(glue::glue("calc.rate.bg: Rank {pos} of {nres} Background Rates"),
                             outer = TRUE, cex = 1.2, line = 0.3, font = 2) else
                               mtext(glue::glue("calc.rate.bg: All {nres} of {nres} Background Rates"),
                                     outer = TRUE, cex = 1.2, line = 0.3, font = 2)

  if(!quiet && length(pos) == 1)
    cat(glue::glue("plot.calc_rate.bg: Plotting background rate from position {pos} of {nres} ..."), sep="\n")
  else if(!quiet)
    cat(glue::glue("plot.calc_rate.bg: Plotting all {nres} background rates ..."), sep="\n")

  if(!quiet) cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Average calc_rate.bg object rates
#' @param x calc_rate.bg object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @return Print to console. No returned value.
#' @export
mean.calc_rate.bg <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.calc_rate.bg # -------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate.bg)))
    stop("mean.calc_rate.bg: Invalid 'pos' rank: only ", length(x$rate.bg), " background rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate.bg)
    cat("Mean of all background rate results:")
    cat("\n")
  } else{
    cat("Mean of background rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate.bg[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate.bg[pos])
  out <- mean(x$rate.bg[pos])
  cat("Mean of", n, "background rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}
