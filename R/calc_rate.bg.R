#' Calculate background respiration rates
#'
#' This function uses simple linear regression to calculate the rate of change
#' of oxygen over time for background corrections of the main data. The
#' background data must be in the same time and oxygen units as the data to be
#' corrected. Multiple columns of background oxygen measures, as long as the
#' time data are identical between measurements.The function returns rates for
#' all columns, and also calculates an average rate.
#'
#' If you need to subset the data to remove regions you don't want to use, see
#' \code{\link{subset_data}}.
#'
#' There are no units involved in `calc_rate.bg()`. This is a deliberate
#' decision. Units are called in a later function when absolute and/or
#' mass-specific rates of oxygen use are computed in [convert_rate()] and
#' [convert_DO()].
#'
#' @param x data frame, `inspect` or `inspect_data` object. This is the data to
#'   process.
#' @param time integer. Defaults to NULL. This specifies the column number of
#'   the time data. If NULL, function assumes time data are in Column 1.
#' @param oxygen integer vector. Defaults to NULL. This specifies the column
#'   number(s) of the oxygen data. Multiple columns of oxygen can be specified.
#'   If NULL, function assumes oxygen data are in all columns of the data frame
#'   except Column 1.
#' @param plot logical. Defaults to TRUE. Will plot the results for visual
#'   inspection.
#'
#' @importFrom data.table data.table
#'
#' @return A list object of class `calc_bg.rate`.
#' @export
#'
#' @examples
#' data("urchins.rd")
#' calc_rate.bg(urchins.rd, time = 1, oxygen = 18:19)
calc_rate.bg <- function(x, time = NULL, oxygen = NULL, plot = TRUE) {

  # Import x from inspect function. We convert to data.frame object here as
  # data.table doesn't like subsetting columns by variable names.
  if(any(class(x) %in% "inspect")) {
    message("`inspect` input detected")
    x <- data.frame(x$dataframe)
  } else if(any(class(x) %in% "inspect_data")) {
    message("`inspect_data` input detected")
    x <- data.frame(x$df)
  } else {
    message("`data.frame` input detected")
    x <- data.frame(x)}

  ## if NULL use col1 for time, all other cols for oxygen
  if(is.null(time)) {
    time <- 1
    message("Using Column 1 as `time`...")}
  if(is.null(oxygen)) {
    oxygen <- seq(2, ncol(x))
    message("Using Column 2 onwards as `oxygen`...")}

  # Extract data:
  xval <- x[time]
  yval <- x[oxygen]
  # Ok, convert back to data.table object
  dt <- data.table(xval, yval)

  # Perform lm fit on each column:
  fit <- lapply(1:length(oxygen), function(x) lm(dt[[x + 1]] ~ dt[[1]]))
  # Extract coefficients:
  coefs <- sapply(1:length(fit), function(x) coef(fit[[x]]))
  rownames(coefs) <- c("Intercept", "Rate")
  # Generate output:
  bg <- unname(coefs[2, ])
  out <- list(data = dt, lm = fit, results = coefs, bgrate = bg,
    mean = mean(bg))
  class(out) <- "calc_rate.bg"
  # Plot data:
  if (plot) plot(out)
  return(out)
}


#' @export
print.calc_rate.bg <- function(x, ...) {
  cat("Rate(s):\n")
  print(x$bgrate)
  cat("Average bg rate:\n")
  print(mean(x$bgrate))
}

#' @export
plot.calc_rate.bg <- function(x, ...) {
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow = n2mfrow(length(x$bgrate)), mai = c(0.4, 0.4, 0.1, 0.1),
    ps = 10, cex = 1, cex.main = 1)  # replace par settings
  lapply(1:length(x$bgrate), function(z) sub.p(data.frame(x$data[[1]],
    x$data[[z + 1]]), rsq = NULL, title = F))
  par(pardefault)  # revert par settings to original

}

