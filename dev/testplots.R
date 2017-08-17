
c1 <- adjustcolor("black", alpha.f=0.3)
c2 <- adjustcolor("goldenrod1", alpha.f=0.7)
b3 <- adjustcolor("linen", alpha.f=1)
usr <- par("usr")

# a plot of the main timeseries and its subset(if a subset is
# performed)
multi.p <- function(df, sdf) {
  if (!is.null(nrow(sdf))) sdf <- list(sdf)
  plot(df, xlab="Time", ylab="Dissolved Oxygen", col=c1, pch=16,
    panel.first=c(rect(usr[1], usr[3], usr[2], usr[4], col=b3),
                  grid(col="white", lty=1, lwd=1.5)))
  invisible(lapply(sdf, function(x) points(x, col=c2, pch=16)))
  invisible(lapply(sdf, function(z) {
    names(z) <- c('x', 'y') # rename columns, in case they're not x and y
    clip(min(z$x), max(z$x), min(z$y), max(z$y))
    abline(lm(y~x, z), lwd=2, lty=2)
  }))
  title(main="Full Timeseries")
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1) {
  if (is.null(nrow(sdf))) sdf <- sdf[[rep]]
  names(sdf) <- c('x', 'y')
  fit <- lm(y~x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1],
    ifelse(sign(cf[2])==1, " + ", " - "), abs(cf[2]), " x ")
  plot(sdf, xlab="Time", ylab="Dissolved Oxygen", col=c2, pch=16)
  abline(fit, lwd=3, lty=3)
  mtext(eq, 3)
  title(main="Closed-up Region")
}

# a plot of residuals
residual.p <- function(fit) {
  plot(fit$residuals~fit$fitted.values, xlab="Fitted Values", ylab="Standardised Residuals", col=c2, pch=16)
  lines(loess.smooth(fit$fitted.values, fit$residuals), col='black', lwd=3)
  abline(0, 0, lty=3, lwd=2)
  title(main="Residual Plot")
}

# a q-q plot
qq.p <- function(fit) {
  qqnorm(rstandard(fit), main="Q-Q Plot", ylab="Standardised Residuals", col=c2, pch=16)
  qqline(rstandard(fit), lty=3, lwd=2)
}



# test data
data(intermittent)
df <- intermittent
d1 <- intermittent[250:1800,]
d2 <- intermittent[2300:3200,]
d3 <- intermittent[4000:4700,]
sdf <- list(d1,d2,d3)
fit <- lm(d1[[2]]~d1[[1]], intermittent)

plot(df, xlab="Time", ylab="Dissolved Oxygen", col=c1, pch=16,
  panel.first=c(rect(usr[1], usr[3], usr[2], usr[4], col=b3),
                grid(col="white", lty=1, lwd=1.5)))
invisible(lapply(sdf, function(x) points(x, col=c2, pch=16)))
invisible(lapply(sdf, function(z) {
  names(z) <- c('x', 'y') # rename columns, in case they're not x and y
  clip(min(z$x), max(z$x), min(z$y), max(z$y))
  abline(lm(y~x, z), lwd=2, lty=2)
}))
title(main="Complete Dataset")




#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
#test functions
pardefault <- par(no.readonly = T)
par(mfrow=c(2,2))
calc.rate(sardine)
par(pardefault)
multi.p(intermittent, sdf)
sub.p(sdf)
residual.p(fit)
qq.p(fit)
