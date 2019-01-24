# This file contains functions used to plot all the figures in the package. We
# have grouped all of these together so that we can edit the functions easily.
# They are not to be used on their own, and will not be exported for use in the
# package.

# We have also deliberately decided to use base graphics, as ggplot2 was too
# slow when plotting large datasets. Unless ggplot2 improves, we will probably
# not switch to it.

# define colours
r1 <- adjustcolor("black", alpha.f = .9)  # primary colour
r2 <- adjustcolor("goldenrod1", alpha.f = 1)  # secondary colour
r3 <- adjustcolor("darkgrey", alpha.f = .2)  # plot background colour
d1 <- adjustcolor("tomato", alpha.f = .75)
d2 <- adjustcolor("darkslateblue", alpha.f = .75)

# panel <- c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
#   par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5))
pch <- 21
cex <- .5

multi.p <- function(df, sdf, rsq, title = TRUE, xl = '', yl = '') {
  names(df) <- c("x", "y")
  if (!is.null(nrow(sdf)))
    sdf <- list(sdf)
  plot(df, xlab = xl, ylab = yl, bg = r1, col = r1, pch = pch, cex = .3,
    panel.first = grid(lwd = .7))
  invisible(lapply(sdf, function(x) points(x, pch = pch, bg = r2, col = r2,
    cex = cex)))
  invisible(lapply(sdf, function(z) {
    names(z) <- c("x", "y")  # rename columns, in case they're not x and y
    clip(min(z$x), max(z$x), min(z$y), max(z$y))
    abline(lm(y ~ x, z), lwd = 1.2, lty = 3)
  }))
  if (title == T)
    title(main = expression("Full Timeseries"), line = 0.5)
  # title(main = paste0("r2 = ", signif(rsq, 3)), line = -1.5, font.main = 1)
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1, rsq, title = T) {
  if (is.null(nrow(sdf)))
    sdf <- sdf[[rep]]
  names(sdf) <- c("x", "y")
  fit <- lm(y ~ x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1], ifelse(sign(cf[2]) == 1, " + ", " - "),
    abs(cf[2]), " x ")
  # plot the graph
  plot(sdf, xlab = "", ylab = "", pch = pch, bg = r2, col = r2, cex = cex,
    panel.first = grid(lwd = .7))
  abline(fit, lwd = 1.5, lty = 2)
  if (title == T) title(main = expression("Close-up Region"), line = 0.5)
  title(main = eq, line = -1.5, font.main = 1)
  if (!is.null(rsq)) title(main = paste0("r2 = ", rsq), line = -2.5, font.main = 1)
}

# a plot of residuals
residual.p <- function(fit) {
  plot(fit$fitted.values, fit$residuals, xlab = "", ylab = "", bg = r2,
    col = r2, ylim = c(max(fit$residuals), -max(fit$residuals)),
    pch = pch, cex = cex, panel.first = grid(lwd = .7))
  lines(suppressWarnings(loess.smooth(fit$fitted.values, fit$residuals)),
    col = "black", lwd = 2)
  title(main = expression("Std. Residuals vs Fitted Values"), line = 0.5)
  abline(0, 0, lty = 3, lwd = 1.5)
}

# a q-q plot
qq.p <- function(fit) {
  qqnorm(rstandard(fit), main = "", xlab = "", ylab = "", bg = r2, col = r2,
    pch = pch, cex = cex, panel.first = grid(lwd = .7))
  title(main = expression("Theoretical Q. vs Std. Residuals"), line = 0.5)
  qqline(rstandard(fit), lty = 3, lwd = 1.5)
}

# kernel density plot
density.p <- function(dens, peaks, rank = 1) {
  plot(dens, main = "", xlab = "", ylab = "", panel.first = grid(lwd = .7))
  polygon(dens, col = r2, border = r2)
  title(main = expression("Density of Rolling"~beta[1]), line = 0.5)
  abline(v = peaks[rank, ][1][,1], lty = 2)  # indicate position on density plot
}

# rolling regression
rollreg.p <- function(rolldf, ranked.b1) {
  plot(rolldf, xlab = "Time", ylab = "Rate", bg = r2, col = r2,
    pch = pch,
    lwd = 1, cex = cex, panel.first = grid(lwd = .7))
  # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = r3)
  abline(h = ranked.b1, lty = 2)
  title(main = expression("Rolling Regression"~beta[1]), line = 0.5)
}

# Unused, but maybe for next time?
# # pcrit plot
# pcrit.p <- function(x, rank = 1) {
#   data <- x$do.mr  # main plot dataset
#   ref <- x$pcritRanked[1]  # reference list to create subsets
#   set1 <- dplyr::filter(data, do <= as.numeric(ref[rank, ]))
#   lm1 <- lm(set1[[2]] ~ set1[[1]], set1)
#   set2 <- dplyr::filter(data, do > as.numeric(ref[rank, ]))
#   lm2 <- lm(set2[[2]] ~ set2[[1]], set2)
#   pc.intercept <- x$pcritRanked[5][rank, ]
#   pc.midpoint <- x$pcritRanked[6][rank, ]
#   # plot:
#   plot(data, col = r2, pch = 16, xlab = "DO", ylab = "MR",
#     lwd = 2, xlim = c(min(data[[1]]), max(data[[1]], pc.intercept, pc.midpoint)),
#     ylim = c(min(data[[2]]), ((max(data[[2]]) - min(data[[2]])) * 1.2 + min(data[[2]]))),
#     panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
#       col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
#   abline(lm1, lwd = 1.5, lty = 4)
#   abline(lm2, lwd = 1.5, lty = 4)
#   abline(v = pc.intercept, col = d1, lwd = 3)
#   # text(pc.intercept, min(data[2]), signif(pc.intercept, 3))
#   abline(v = pc.midpoint, col = d2, lwd = 3)
#   # text(pc.midpoint, max(data[2]), signif(pc.midpoint, 3))
#   # title(main = "Pcrit")
#   legend("topright", c("Intercept", "Mid-point"), col = c(d1, d2), lty = 1, lwd = 3,
#     bty = "n")
# }
