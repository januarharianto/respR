#' Calculate background respiration rates
#'
#' This function uses simple linear regression to automatically calculate the
#' rate of change of oxygen over time for background corrections of the main
#' data. Can be used on multiple datasets of background measures, as long as the
#' time data are identical between measurements.
#'
#' There are no units involved in `calc_rate.bg()`. This is a deliberate
#' decision. Units are called in a later function when volume- and/or
#' weight-specific rates of oxygen concentration are computed in
#' [convert_rate()] and [convert_DO()].
#'
#' @param x data frame.
#' @param xcol numeric. Defaults to `1`.
#' @param ycol numeric vector. Defaults to `2`. Can have length > 1.
#' @param from numeric.
#' @param to numeric.
#' @param by string. "time" or "row". Defaults to "time".
#' @param plot logical. Defaults to TRUE.
#'
#' @importFrom data.table data.table
#'
#' @return A list object of class `calc_bg.rate`.
#' @export
#'
#' @examples
#' calc_rate.bg(urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "time")
calc_rate.bg <- function(x, xcol = 1, ycol = 2, from = NULL,
  to = NULL, by = "time", plot = T) {
  # Extract data:
  dt <- data.table(x[c(xcol, ycol)])
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
  out <- list(data = dt, lm = fit, results = coefs, bgrate = bg)
  class(out) <- "calc_rate.bg"
  # Plot data:
  if (plot) plot(out)
  return(out)
}


#' @export
print.calc_rate.bg <- function(x, ...) {
  cat("Rate(s):\n")
  print(x$bgrate)
}

#' @export
plot.calc_rate.bg <- function(x, ...) {
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow = n2mfrow(length(x$bgrate)), mai = c(0.4, 0.4, 0.1, 0.1),
    ps = 10, cex = 1, cex.main = 1)  # replace par settings
  lapply(1:length(x$bgrate), function(z) sub.p(data.frame(x$data[[1]],
    x$data[[z + 1]]), title = F))
  par(pardefault)  # revert par settings to original

}




#' Truncate a data frame to create a subset of the data
#'
#' This is an internal function. This function extracts a subset data frame
#' based on a given set of rules.
#'
#' @param x data frame.
#' @param from numeric.
#' @param to numeric.
#' @param by string. "time", "row", "o2" or "proportion".
#'
#' @keywords internal
#'
#' @return A `data.table` object.
#' @export
subset_data <- function(x, from, to, by) {
  dt <- data.table::as.data.table(x)
  if (by == "time") {
    out <- dt[dt[[1]] >= from & dt[[1]] <= to]
  }
  if (by == "row") {
    out <- dt[from:to]
  }
  if (by == "o2" & length(x) == 2) {
    top <- Position(function(z) z <= from, dt[[2]])
    bot <- Position(function(z) z <= to, dt[[2]])
    out <- dt[top:bot]
  }
  if (by == "proportion") {
    mx <- max(dt[[2]])
    mn <- min(dt[[2]])
    top <- Position(function(z) z <= (from * (mx - mn) + mn), dt[[2]])
    bot <- Position(function(z) z <= (to * (mx - mn) + mn), dt[[2]])
    out <- dt[top:bot]
  }
  return(out)
}

