# by: 'time' (default) or 'row' or 'o2' or 'proportion'
# expt.type: 'closed' (default) or 'intermittent' or 'flowthrough' or 'open' or 'two-point'
calcRate <- function(df, from = NULL, to = NULL, by = 'time', expt.type = 'closed', ...) {
  # Subset data -----
  if (is.null(from) && is.null(to)) subdf <- df
  else {
    if (by == 'time') subdf <- df[df[, 1] >= from & df[, 1] <= to, ]
    if (by == 'row') subdf <- df[from:to, ]
    if (by == 'o2') {
      fromPos <- Position(function(x) x <= from, df[, 2])
      toPos   <- Position(function(x) x < to, df[, 2])
      subdf   <- df[fromPos:toPos, ]
    }
    if (by == 'proportion') {
      fromPos <- Position(function(x) x <= (from * (max(df[, 2]) - min(df[, 2])) + min(df[, 2])), df[, 2])
      toPos   <- Position(function(x) x < (to * (max(df[, 2]) - min(df[, 2])) + min(df[, 2])), df[, 2])
      subdf   <- df[fromPos:toPos, ]
    }
  }
  # Check that the subset meets minimum requirements -----
  if (nrow(subdf) <= 3)
    stop('Data subset is too small. Please select a larger threshold.')
  if (max(subdf[, 2]) == min(subdf[, 2]))
    stop('Min/max O2 in data subset are too close. Please select a larger threshold.')
  # Perform regression -----
  lmfit <- lm(subdf[[2]] ~ subdf[[1]], subdf) # perform the lm
  beta  <- coef(lmfit)[[2]] # slope
  r.sq  <- summary(lmfit)$r.squared # r-square
  output <- list( data = df,
                  subset = subdf,
                  model = lmfit,
                  slope = beta,
                  r.square = r.sq)
  class(output) <- append(class(output),"calcRate")
  summ <- data.frame(beta, r.sq)
  print(summ)
  return(invisible(output))
}



plot.calcRate <- function(x, ...) {
  lm <- x$model
  resi <- ggRF(lm)
  quan <- ggQQ(lm)
  plot_grid(resi, quan)
}


sard <- calcRate(sardine, from = 2500, to = 6500)
sord <- calcRate(sardine)
plot(sard)
plot(sord)


# Residuals vs Fitted Values
ggRF <- function(lm) {
  ggplot(lm, aes(.fitted, .resid)) +
    geom_point() +
    stat_smooth(method = 'loess')
}

# Standardised vs Theoretical Quantiles (Normal QQ)
# Modified from source: https://stackoverflow.com/a/19990107
ggQQ <-  function(lm) {
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lm))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  p <- ggplot(data=d, aes(sample=std.resid)) +
    stat_qq(shape = 21, size = 3, fill = 'darkorchid3', colour = 'darkorchid4', alpha = .4) +
    labs(x="Theoretical Quantiles", y="Standardized Residuals") +
    geom_abline(slope = slope, intercept = int, linetype="dashed")  # dashed reference line
  return(p)
}


