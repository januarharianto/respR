# This file contains functions used to plot all the figures in the package. We have grouped all of these together so that we can edit the functions easily. They are not to be used on their own, and will not be exported for use in the package.

# We have also deliberately decided to use base graphics, as ggplot2 was too slow when plotting large datasets. Unless ggplot2 improves, we will probably not use that. Maybe skip to ggvis once it's mature?

# define colours
r1 <- adjustcolor("darkslateblue", alpha.f = 0.4)  # primary colour
r2 <- adjustcolor("goldenrod1", alpha.f = 0.7)  # secondary colour
r3 <- adjustcolor("lavenderblush", alpha.f = 0.8)  # plot background colour
d1 <- adjustcolor("tomato", alpha.f = 0.75)
d2 <- adjustcolor("darkslateblue", alpha.f = 0.75)



#' Plot the main data, and the subset data, in the same window.
#'
#' @param df data frame.
#' @param sdf data frame.
#' @param title logical.
#'
#' @return A plot.
#'
multi.p <- function(df, sdf, title = T) {
  names(df) <- c("x", "y")
  if (!is.null(nrow(sdf)))
    sdf <- list(sdf)
  plot(df, xlab = "", ylab = "", col = r1, pch = 16, panel.first = c(rect(par("usr")[1],
    par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
    lty = 1, lwd = 1.5)))
  invisible(lapply(sdf, function(x) points(x, col = r2, pch = 16)))
  invisible(lapply(sdf, function(z) {
    names(z) <- c("x", "y")  # rename columns, in case they're not x and y
    clip(min(z$x), max(z$x), min(z$y), max(z$y))
    abline(lm(y ~ x, z), lwd = 2, lty = 3)
  }))
  if (title == T)
    title(main = "Full Timeseries", line = 0.3)
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1, title = T) {
  if (is.null(nrow(sdf)))
    sdf <- sdf[[rep]]
  names(sdf) <- c("x", "y")
  fit <- lm(y ~ x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1], ifelse(sign(cf[2]) == 1, " + ", " - "), abs(cf[2]),
    " x ")
  # plot the graph
  plot(sdf, xlab = "", ylab = "", col = r2, pch = 16, panel.first = c(rect(par("usr")[1],
    par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
    lty = 1, lwd = 1.5)))
  abline(fit, lwd = 1.5, lty = 2)
  if (title == T) title(main = "Close-up Region", line = 0.3)
  title(main = eq, line = -1.5, font.main = 1)
}

# a plot of residuals
residual.p <- function(fit) {
  plot(fit$fitted.values, fit$residuals, xlab = "", ylab = "",
    col = r2, pch = 16, panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
  lines(loess.smooth(fit$fitted.values, fit$residuals), col = "black", lwd = 3)
  title(main = "Std. Residuals vs Fitted Values", line = 0.3)
  abline(0, 0, lty = 3, lwd = 1.5)
}

# a q-q plot
qq.p <- function(fit) {
  qqnorm(rstandard(fit), main = "", xlab = "", ylab = "", col = r2,
    pch = 16, panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
  title(main = "Theoretical Q. vs Std. Residuals", line = 0.3)
  qqline(rstandard(fit), lty = 3, lwd = 1.5)
}

# kernel density plot
density.p <- function(dens, peaks, rank = 1) {
  plot(dens, main = "", xlab = "", ylab = "", col = r1, pch = 16, panel.first = c(rect(par("usr")[1],
    par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
    lty = 1, lwd = 1.5)))
  polygon(dens, col = r2)
  title(main = "Density vs Rate (b1)", line = 0.3)
  abline(v = peaks[rank, ][1], lty = 2)  # indicate position on density plot
}

# rolling regression
rollreg.p <- function(rolldf, ranked.b1) {
  plot(rolldf, type = "l", xlab = "Time", ylab = "Rate", col = "black", pch = 16,
    lwd = 1, panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
      par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
  abline(h = ranked.b1, lty = 3)
  title(main = "Rolling Regressions (Rate)", line = 0.3)
}

# pcrit plot
pcrit.p <- function(x, rank = 1) {
  data <- x$do.mr  # main plot dataset
  ref <- x$pcritRanked[1]  # reference list to create subsets
  set1 <- dplyr::filter(data, do <= as.numeric(ref[rank, ]))
  lm1 <- lm(set1[[2]] ~ set1[[1]], set1)
  set2 <- dplyr::filter(data, do > as.numeric(ref[rank, ]))
  lm2 <- lm(set2[[2]] ~ set2[[1]], set2)
  pc.intercept <- x$pcritRanked[5][rank, ]
  pc.midpoint <- x$pcritRanked[6][rank, ]
  # plot:
  plot(data, col = r2, pch = 16, xlab = "DO", ylab = "MR",
    lwd = 2, xlim = c(min(data[[1]]), max(data[[1]], pc.intercept, pc.midpoint)),
    ylim = c(min(data[[2]]), ((max(data[[2]]) - min(data[[2]])) * 1.2 + min(data[[2]]))),
    panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
      col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
  abline(lm1, lwd = 1.5, lty = 4)
  abline(lm2, lwd = 1.5, lty = 4)
  abline(v = pc.intercept, col = d1, lwd = 3)
  # text(pc.intercept, min(data[2]), signif(pc.intercept, 3))
  abline(v = pc.midpoint, col = d2, lwd = 3)
  # text(pc.midpoint, max(data[2]), signif(pc.midpoint, 3))
  # title(main = "Pcrit")
  legend("topright", c("Intercept", "Mid-point"), col = c(d1, d2), lty = 1, lwd = 3,
    bty = "n")
}