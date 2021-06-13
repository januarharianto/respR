#' Calculate background oxygen uptake or input rates
#'
#' This function calculates the rate of change of oxygen over time from "blank"
#' or control respirometry experiments, to allow for background adjustments of
#' experimental data. It accepts background oxygen~time data as data frames and
#' `inspect` objects. The data must be in the same time and oxygen units as the
#' data used from which the rate to be adjusted was extracted. Multiple columns
#' of background oxygen measurements can be examined as long as they share the
#' same time data. The function returns rates for all columns, and also
#' calculates a mean rate.
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
#' `calc_rate.bg` can easily quantify oxygen *input* rates as well as
#' consumption rates. Most users will be interested in this function because
#' background rates are typically consumption rates from microbial activity,
#' that need to be quantified and their effects removed from experimental
#' specimen rates. However, there are some experiments where oxygen input is of
#' interest, for example open tank or open arena respirometry where the input of
#' oxygen from the open water surface is of interest. There are also cases in
#' closed respirometry where there may be an input of oxygen via leaks or
#' production from photosynthesis, which need to be quantified. `calc_rate.bg`
#' is capable of quantifying production rates as well as consumption, and these
#' can be used for adjustments in [`adjust_rate()`].
#'
#' ## Output
#'
#' Output is a `list` object of class `calc_rate.bg` containing original data,
#' linear models, summary information, and the primary output of interest
#' `$rate.bg`, which contains a rate for each oxygen column present in the input
#' data. There is also `$rate.bg.mean` containing the mean of all background
#' rates. Note, this is not used in `adjust_rate`, where the `method` input
#' there determines how `$rate.bg` are applied, but can easily be extracted and
#' applied as an adjustment value if desired.
#'
#' ## Plot
#'
#' A plot is produced (provided `plot = TRUE`) showing all examined columns of
#' oxygen against time, with the rate and linear model coefficients. Single
#' rates can be plotted by changing the `pos` argument either in the main
#' function call, or by plotting the output, e.g. `plot(object, pos = 2)`.
#' Console output messages can be suppressed using `message = FALSE`.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints all background rates, plus the mean background rate.
#'
#' - `summary()`: prints summary table of all results and metadata, or those
#' specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The output can be
#' saved as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all rates, or those specified by the `pos`
#' input. e.g. `mean(x, pos = 1:5)` The output can be saved as a separate value
#' by passing `export = TRUE`.
#'
#' @param x `data.frame` or `inspect` object. This is the data to process.
#' @param time integer. Defaults to 1. This specifies the column number of the
#'   time data.
#' @param oxygen integer value or vector. This specifies the column number(s) of
#'   the oxygen data. Multiple columns of oxygen can be specified. If NULL,
#'   function assumes oxygen data are in all columns of the data frame except
#'   the `time` column.
#' @param plot logical. Defaults to TRUE. Defaults to TRUE. Plots the data. See
#'   Details.
#' @param ... Allows additional plotting controls to be passed, such as `pos`
#'   and `message = FALSE`.
#'
#' @importFrom data.table data.table
#'
#' @return A list object of class `calc_bg.rate`.
#' @export
#'
#' @examples
#' data("urchins.rd")
#' calc_rate.bg(urchins.rd, time = 1, oxygen = 18:19)

calc_rate.bg <- function(x, time = NULL, oxygen = NULL, plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()

  # Import x from inspect function. We convert to data.frame object here as
  # data.table doesn't like subsetting columns by variable names.
  if(any(class(x) %in% "inspect")) {
    x <- data.frame(x$dataframe)
  } else if(any(class(x) %in% "inspect_data")) {
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

#' @export
plot.calc_rate.bg <- function(x, pos = NULL, message = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  nres <- length(x$rate.bg)
  if(!is.null(pos) && length(pos) > 1)
    stop("calc_rate: 'pos' should be a single value.")
  if(!is.null(pos) && pos > nres)
    stop("calc_rate.bg: Invalid 'pos' rank: only ", nres, " rates found.")
  if(!is.null(pos)) nplot <- 1 else
    nplot <- nres

  if(message) cat("\n# plot.calc_rate.bg # -------------------\n")
  par(mfrow = n2mfrow(nplot),
      oma = c(1, 1, 1.5, 0),
      mai = c(0.2, 0.2, 0.1, 0.1),
      ps = 10, cex = 1, cex.main = 1)  # replace par settings

  if(length(pos) == 1) sub.p(data.frame(x$dataframe[[1]],
                                        x$dataframe[[pos + 1]]), rsq = NULL, title = F)
  else lapply(1:nres, function(z) sub.p(data.frame(x$dataframe[[1]],
                                                   x$dataframe[[z + 1]]), rsq = NULL, title = F))

  if(length(pos) == 1) mtext(glue::glue("calc.rate.bg: Rank {pos} of {nres} Background Rates"),
                             outer = TRUE, cex = 1.2, line = 0.3, font = 2) else
                               mtext(glue::glue("calc.rate.bg: All Background Rates"),
                                     outer = TRUE, cex = 1.2, line = 0.3, font = 2)

  if(message && length(pos) == 1)
    cat(glue::glue("calc_rate.bg: Plotting background rate from position {pos} of {nres} ..."), sep="\n")
  else if(message)
    cat(glue::glue("calc_rate.bg: Plotting all {nres} background rates ..."), sep="\n")

  if(message) cat("-----------------------------------------\n")

  return(invisible(x))
}

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
