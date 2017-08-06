# This script contains functions uset to plot diagnostics.
# If we decide to switch to base R graphics at some point we can change them
# here and they should update automatically in the main functions.
# Note - partially implemented. plot_grid still occurs in functions and will
# have to be removed if a swap occurs.

# Define colours
c1 <- 'black'
c2 <- 'navy'
c3 <- 'goldenrod'

# Define theme
theme_respr <- function() {
  theme_bw(base_size = 12) %+replace%
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      #plot.margin = unit(c(10,5,5,5),"mm"),

      panel.border = element_rect(fill = 'transparent', size = 1),
      # panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      axis.text	= element_text(size = rel(1)),
      axis.title = element_text(face = 'bold', size = rel(1)),
      # axis.title.y = element_text(angle = 90, margin = margin(0, 25, 0, 0)),
      # axis.title.x = element_text(margin = margin(25, 0, 0, 0)),
      axis.ticks = element_line(),

      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.2, "cm"),
      legend.title = element_text(face="italic"),

      strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
      strip.text = element_text(face="bold")
    )
}

# a plot of the main timeseries, with subset superimposed
main_plot <- function(df, sdf) {
  names(df) <- c('x', 'y')
  names(sdf) <- c('x', 'y')
  ggplot() +
    geom_point(data = df, aes(x, y), alpha = .6, colour = c1) +
    geom_point(data = sdf, aes(x, y), size = 3, colour = c3) +
    stat_smooth(data = sdf, aes(x, y), method ='lm', colour = c2, linetype = 6) +
    labs(x = 'Time', y = 'DO', title = 'Complete Dataset') +
    theme_respr() +
    geom_blank()
}

# a plot of the subset timeseries, with linear smoothing and equation
sub_plot <- function(sdf) {
  ggplot(sdf, aes(sdf[[1]], sdf[[2]])) +
    geom_point(size = 3, colour = c3) +
    stat_smooth(method = 'lm', colour = c1, linetype = 6) +
    ggpmisc::stat_poly_eq(formula = y ~ x, eq.with.lhs = "italic(hat(y))~`=`~",
      aes(label = paste(..eq.label.., sep = "*plain(\",\")~")), label.x.npc = .6, parse = T) +
    labs(x = 'Time', y = 'DO', title = 'Closed-up Region') +
    theme_respr() +
    geom_blank()
}

# a plot of residuals vs fitted values
residual_plot <- function(lmfit) {
  ggplot(lmfit, aes(.fitted, .resid)) +
    geom_point(size = 2, colour = c3, alpha = .6) +
    labs(x = "Fitted values", y = "Residuals", title = 'Residual Plot') +
    stat_smooth(method = 'loess', colour = c1) +
    theme_respr() +
    geom_blank()
}

# a plot showing standardised quantiles vs theoratical quantiles (qq plot)
qq_plot <- function(lmfit) {
  d <- data.frame(std.resid = rstandard(lmfit))
  # calculate 1Q/4Q line
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  ggplot(data = d, aes(sample = std.resid)) +
    stat_qq(size = 3, colour = c3, alpha = .6) +
    labs(x="Theoretical Quantiles", y="Standardised Residuals", title = 'QQ Plot') +
    geom_abline(slope = slope, intercept = int, size = 1, linetype = 5, colour = c1) +
    theme_respr() +
    geom_blank()
}

