#' Calculate rate over time. Need a better title.
#'
#' @param df Data frame.
#' @param from Numeric.
#' @param to Numeric.
#' @param by Character.
#' @param expt.type Character. Not yet active.
#' @param ... Additional parameters can be added here, if needed.
#'
#' @return A list.
#' @import ggplot2
#' @export
#'
#' @examples
#' # Load data
#' data(sardine)
#' sardine.full <- calcRate(sardine) # if you want to run the entire dataset
#' plot(sardine.full)
#'
#' # if you want to specify range, include from and to arguments.
#' sardine.subset <- calcRate(sardine, from = 0.7, to = 0.2)
#' plot(sardine.subset)
#'
calcRate <- function(df, from = NULL, to = NULL, by = "proportion", expt.type = "closed") {
  # Subset data -----
  if (is.null(from) && is.null(to)) {  # if these arguments are not used, don't subset the data
    subdf     <- df
    from.row  <- 1
    to.row    <- df[, 1][length(df[, 1])]
  } else if (is.numeric(from) && is.numeric(to)) {  # this checks that the arguments are numeric (two birds one stone)
    if (by == "time") {
      from.row <- which(df[, 1] == from)
      to.row <- which(df[, 1] == to)
      subdf <- df[df[, 1] >= from & df[, 1] <= to, ]
    }
    if (by == "row") {
      subdf <- df[from:to, ]
      from.row <- from
      to.row <- to
    }
    if (by == "o2") {
      from.row <- Position(function(x) x <= from, df[, 2])
      to.row <- Position(function(x) x < to, df[, 2])
      subdf <- df[from.row:to.row, ]
    }
    if (by == "proportion") {
      from.row <- Position(function(x) x <= (from * (max(df[, 2]) - min(df[, 2])) + min(df[, 2])), df[, 2])
      to.row <- Position(function(x) x < (to * (max(df[, 2]) - min(df[, 2])) + min(df[, 2])), df[, 2])
      subdf <- df[from.row:to.row, ]
    }
  }
  # Check that the subset meets minimum requirements -----
  if (nrow(subdf) <= 3) stop("Data subset is too narrow. Please select a larger threshold.")
  if (max(subdf[, 2]) == min(subdf[, 2])) stop("Min/max O2 in data subset are too close. Please select a larger threshold.")

  # Looks good. Perform regression -----
  lmfit <- lm(subdf[[2]] ~ subdf[[1]], subdf)  # perform the lm
  # Generate output -----
  beta <- coef(lmfit)[[2]]  # slope
  r.sq <- summary(lmfit)$r.squared  # r-square
  calcrateout <- list(data = df, subset = subdf, model = lmfit, slope = beta, r.square = r.sq)
  class(calcrateout) <- append(class(calcrateout), "calcRate")
  summ <- data.frame(slope = beta, r.sq, from.row, to.row)
  print(summ)
  return(invisible(calcrateout))
}



#' Plot for class calcRate
#'
#' @param x An object of class 'calcRate'.
#' @param ... Additional arguments may be passed into the function.
#'
#' @return ggplot2 objects, arranged in grids
#' @export
#'
#' @examples
#' # Load data
#' data(sardine)
#' sardine.full <- calcRate(sardine) # if you want to run the entire dataset
#' plot(sardine.full)
#'
plot.calcRate <- function(x, ...) {
  lm <- x$model
  dfdata <- x$data
  dfsubset <- x$subset

  # Timeseries Plot
  tseries.plot <- ggplot() +
    geom_point(data = dfdata, aes(dfdata[, 1], dfdata[, 2]), shape = 21, size = 3,
      fill = 'darkorchid3', colour = 'darkorchid4', alpha = .4) +
    geom_point(data = dfsubset, aes(dfsubset[, 1], dfsubset[, 2]), shape = 21,
      size = 3, fill = 'gold', colour = 'gold2') +
    stat_smooth(data = dfsubset, aes(dfsubset[, 1], dfsubset[, 2]), method = 'lm',
      colour = 'black', linetype = 4) +
    labs(x = "Time", y = "DO2")

  # Residual Plot
  residual.plot <- ggplot(lm, aes(.fitted, .resid)) +
    geom_point(shape = 21, size = 1, fill = 'darkorchid3',
      colour = 'darkorchid4', alpha = .4) +
    labs(x = "Fitted values", y = "Residuals") +
    stat_smooth(method = 'loess')

  # QQ Plot
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  qq.plot <- ggplot(data=d, aes(sample=std.resid)) +
    stat_qq(shape = 21, size = 3, fill = 'darkorchid3', colour = 'darkorchid4',
      alpha = .4) +
    labs(x="Theoretical Quantiles", y="Standardized Residuals") +
    geom_abline(slope = slope, intercept = int, linetype="dashed")

  #Output
  cowplot::plot_grid(tseries.plot, cowplot::plot_grid(residual.plot, qq.plot),
            ncol = 1, align = 'h')
}
