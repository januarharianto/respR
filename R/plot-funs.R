# define colours
r1 <- adjustcolor("darkslateblue", alpha.f = 0.4)  # primary colour
r2 <- adjustcolor("goldenrod1", alpha.f = 0.7)  # secondary colour
r3 <- adjustcolor("lavenderblush", alpha.f = .8)  # plot background colour

# plot of main series with subset (highlighted)
multi.p <- function(df, sdf, title = T) {
  names(df) <- c("x", "y")
  if (!is.null(nrow(sdf)))
    sdf <- list(sdf)
  plot(df, xlab = "Time", ylab = "DO", col = r1, pch = 16,
    panel.first = c(rect(par("usr")[1], par("usr")[3],
      par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  invisible(lapply(sdf, function(x) points(x, col = r2, pch = 16)))
  invisible(lapply(sdf, function(z) {
    names(z) <- c("x", "y")  # rename columns, in case they're not x and y
    clip(min(z$x), max(z$x), min(z$y), max(z$y))
    abline(lm(y ~ x, z), lwd = 2, lty = 3)
  }))
  if (title == T) title(main = "Full Timeseries", line = .5)
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1, title = T) {
  if (is.null(nrow(sdf)))
    sdf <- sdf[[rep]]
  names(sdf) <- c("x", "y")
  fit <- lm(y ~ x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1], ifelse(sign(cf[2]) == 1, " + ", " - "),
    abs(cf[2]), " x ")
  # plot the graph
  plot(sdf, xlab = "Time", ylab = "DO", col = r2, pch = 16,
    panel.first = c(rect(par("usr")[1], par("usr")[3],
      par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  abline(fit, lwd = 1.5, lty = 2)
  if (title == T) title(main = "Closed-up Region", line = .5)
  title(main = eq, line = -1.5, font.main = 1)
}

# a plot of residuals
residual.p <- function(fit) {
  plot(fit$fitted.values, fit$residuals, xlab = "Fitted Values", ylab = "Standardised Residuals",
    col = r2, pch = 16, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  lines(loess.smooth(fit$fitted.values, fit$residuals), col = "black",
    lwd = 3)
  title(main = "Residual", line = .5)
  abline(0, 0, lty = 3, lwd = 1.5)
}

# a q-q plot
qq.p <- function(fit) {
  qqnorm(rstandard(fit), main = "", ylab = "Standardised Residuals",
    col = r2, pch = 16, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  title(main = "Q-Q", line = .5)
  qqline(rstandard(fit), lty = 3, lwd = 1.5)
}

# kernel density plot
density.p <- function(dens, peaks, rank = 1) {
  plot(dens, main = "", xlab = "Rate", ylab = "Density",
    col = r1, pch = 16, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  polygon(dens, col = r2)
  title(main = "Peak Density", line = .5)
  abline(v = peaks[rank, ][1], lty = 2)  # indicate position on density plot
}

# rolling regression
rollreg.p <- function(rolldf, peaks, ranked.b1) {
  plot(rolldf, type = "l", xlab = "Time", ylab = "Rate",
    col = "black", pch = 16, lwd = 1, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
  abline(h = ranked.b1, lty = 3)
  title(main = "Rolling Regression", line = .5)
}

