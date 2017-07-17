autoRate <- function(df, span = 0.2) {
  win <- nrow(df) * span # define window
  reg <- movingReg(df, span) # perform rolling regression on data
  names(df) <- c('time', 'DO')
  p0 <- ggplot(df, aes(time, DO)) + geom_point(shape = 46)
  # OK ANALYSE
  r2 <- c(rep(NA, win - 1), reg$r.sq) # take r2 and append NA values to match x
  slope <- c(rep(NA, win - 1), reg$beta) # extract slope values from regression

  time.slope <- data.frame(df[[1]], slope)
  names(time.slope) <- c('time', 'slope')
  p1 <- ggplot(time.slope, aes(time, slope)) + geom_line(na.rm = T)

  time.r2 <- data.frame(df[[1]], r2)
  names(time.r2) <- c('time', 'r.square')
  p2 <- ggplot(time.r2, aes(time, r.square)) + geom_line(na.rm = T)

  p3 <- ggplot(data.frame(slope = slope), aes(x = slope)) + geom_line(stat = 'density', na.rm = T)
  p4 <- ggplot(data.frame(r2 = r2), aes(x = r2)) + geom_line(stat = 'density', na.rm = T)

  plot_grid(p0,p1,p2, plot_grid(p3, p4, ncol = 2), ncol = 1)
}


autoRate(squid)
autoRate(sardine)
autoRate(read.csv('~/Documents/Dev/lepcte11.csv'))
autoRate(read.csv('~/Documents/Dev/hery.csv'))
autoRate(read.csv('~/Documents/Dev/RESPA05.1.csv'))

urchin <- read.csv('~/Documents/Dev/hery.csv')
