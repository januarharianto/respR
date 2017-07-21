# plot functions

main_plot <- function(df, sdf) {
  ggplot() +
    geom_point(data = df, aes(df[[1]], df[[2]]), alpha = .6, colour = 'navy') +
    geom_point(data = sdf, aes(sdf[[1]], sdf[[2]]), size = 3, colour = 'goldenrod') +
    stat_smooth(data = sdf, aes(sdf[[1]], sdf[[2]]), method ='lm', colour = 'black', linetype = 6) +
    labs(x = 'Time', y = 'DO') +
    theme_respr() +
    geom_blank()
}

sub_plot <- function(sdf) {
  ggplot(sdf, aes(sdf[[1]], sdf[[2]])) +
    geom_point(size = 3, colour = 'goldenrod') +
    stat_smooth(method = 'lm', colour = 'black', linetype = 6) +
    ggpmisc::stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
      aes(label = paste(..eq.label.., sep = "*plain(\",\")~")), label.x.npc = .6, parse = T) +
    labs(x = 'Time', y = 'DO') +
    theme_respr() +
    geom_blank()
}

residual_plot <- function(lmfit) {
  ggplot(lmfit, aes(.fitted, .resid)) +
    geom_point(size = 2, colour = 'goldenrod', alpha = .6) +
    labs(x = "Fitted values", y = "Residuals") +
    stat_smooth(method = 'loess', colour = 'black') +
    theme_respr() +
    geom_blank()
}

qq_plot <- function(lmfit) {
  d <- data.frame(std.resid = rstandard(lmfit))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p4 <- ggplot(data = d, aes(sample = std.resid)) +
    stat_qq(size = 3, colour = 'goldenrod', alpha = .6) +
    labs(x="Theoretical Quantiles", y="Standardised Residuals") +
    geom_abline(slope = slope, intercept = int, size = 1, linetype = 5, colour = 'black') +
    theme_respr() +
    geom_blank()
}
