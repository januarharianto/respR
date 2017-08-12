# define colours
r1 <- adjustcolor("black", alpha.f = 0.3)        # primary colour
r2 <- adjustcolor("goldenrod1", alpha.f = 0.7)   # secondary colour
r3 <- adjustcolor("lavenderblush", alpha.f = 1)  # plot background colour

# plot of main series with subset (highlighted)
multi.p <- function(df, sdf) {
  names(df) <- c("x", "y")
  if (!is.null(nrow(sdf)))
    sdf <- list(sdf)
  plot(df, xlab = "Time", ylab = "DO", col = r1, pch = 16,
    font.lab = 2, panel.first = c(rect(par("usr")[1], par("usr")[3],
      par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  invisible(lapply(sdf, function(x) points(x, col = r2, pch = 16)))
  invisible(lapply(sdf, function(z) {
    names(z) <- c("x", "y")  # rename columns, in case they're not x and y
    clip(min(z$x), max(z$x), min(z$y), max(z$y))
    abline(lm(y ~ x, z), lwd = 2, lty = 2)
  }))
  title(main = "Full Timeseries")
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1) {
  if (is.null(nrow(sdf)))
    sdf <- sdf[[rep]]
  names(sdf) <- c("x", "y")
  fit <- lm(y ~ x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1], ifelse(sign(cf[2]) == 1, " + ", " - "),
    abs(cf[2]), " x ")
  plot(sdf, xlab = "Time", ylab = "DO", col = r2, pch = 16,
    font.lab = 2, panel.first = c(rect(par("usr")[1], par("usr")[3],
      par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  abline(fit, lwd = 1.5, lty = 3)
  title(main = "Closed-up Region")
  title(main = eq, line = 0.5)
}

# a plot of residuals
residual.p <- function(fit) {
  plot(fit$fitted.values, fit$residuals, xlab = "Fitted Values", ylab = "Standardised Residuals",
    col = r2, pch = 16, font.lab = 2, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  lines(loess.smooth(fit$fitted.values, fit$residuals), col = "black",
    lwd = 3)
  abline(0, 0, lty = 3, lwd = 1.5)
  title(main = "Residual")
}

# a q-q plot
qq.p <- function(fit) {
  qqnorm(rstandard(fit), main = "Q-Q", ylab = "Standardised Residuals",
    col = r2, pch = 16, font.lab = 2, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  qqline(rstandard(fit), lty = 3, lwd = 2)
}

# kernel density plot
density.p <- function(dens, peaks, rank = 1) {
  plot(dens, main = "Peak Density", xlab = "Rate", ylab = "Density",
    col = r1, pch = 16, font.lab = 2, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  polygon(dens, col = r2)
  abline(v = peaks[rank, ][1], lty = 2)  # indicate position on density plot
}

# rolling regression
rollreg.p <- function(rolldf, peaks, ranked.b1) {
  plot(rolldf, type = "l", main = "Rolling Regression", xlab = "Time", ylab = "Rate",
    col = "black", pch = 16, lwd = 1, font.lab = 2, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  abline(h = ranked.b1, lty = 3)
}

