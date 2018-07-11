#' Calculate background respiration rates
#'
#' This function uses simple linear regression to automatically calculate the
#' rate of change of oxygen over time for background corrections of the main
#' data. Can be used on multiple datasets of background measures, as long as the
#' time data are identical between measurements. In addition, the data must be
#' in the same time and oxygen units as the data to be corrected. Data can be
#' subset using the `from`, `to`, and `by` arguments. If multiple datasets are
#' included, all are subset by the same criteria.
#'
#' There are no units involved in `calc_rate.bg()`. This is a deliberate
#' decision. Units are called in a later function when volumetric and/or
#' mass-specific rates of oxygen use are computed in
#' [convert_rate()] and [convert_DO()].
#'
#' @param x data frame. This is the data to process.
#' @param xcol numeric. Defaults to `1`. This is the time column.
#' @param ycol numeric vector. Defaults to `2`. Can have length > 1. This is the
#'   O2 data.
#' @param from numeric. Defaults to NULL. Defines the lower bound(s) of the data
#'   frame to subset. Subsetting is based on the argument: `by`.
#' @param to numeric. Defaults to NULL. Defines the upper bound(s) of the data
#'   frame to subset. Subsetting is based on the argument: `by`.
#' @param by string. `"time"` or `"row"` Defaults to `"time"`. This is the
#'   method used to subset the data.
#' @param plot logical. Defaults to TRUE. Will plot the results for visual
#'   inspection.
#'
#' @importFrom data.table data.table
#'
#' @return A list object of class `calc_bg.rate`.
#' @export
#'
#' @examples
#' calc_rate.bg(urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "time")
calc_rate.bg <- function(x, xcol = 1, ycol = 2, from = NULL,
  to = NULL, by = "time", plot = TRUE) {

  # Import x from inspect function
  if(any(class(x) %in% "inspect")) x <- as.data.table(x$dataframe)
  if(any(class(x) %in% "inspect_data")) x <- as.data.table(x$df)
  # x <- as.data.table(x)

  # Extract data:
  xval <- x[xcol]
  yval <- x[ycol]
  dt <- data.table(xval, yval)
  # Subset data if needed:
  if (!is.null(from) && !is.null(to))
    dt <- subset_data(dt, from, to, by)
  # Perform lm fit on each column:
  fit <- lapply(1:length(ycol), function(x) lm(dt[[x + 1]] ~ dt[[1]]))
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

