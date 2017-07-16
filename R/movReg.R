#'
#'@export
movReg <- function(df, span = 0.1) {
  win   <- floor(span*NROW(df))  # compute window size (rounded down)
  x <- df[[1]]
  y <- df[[2]]
  # Calculate b1 (slope) here
  xyBar <-  roll_mean(x * y, win)  # mean of xy for each window
  xBar  <-  roll_mean(x, win)  # mean of x
  yBar  <-  roll_mean(y, win)  # mean of y
  xxBar <-  roll_mean(x * x, win)  # mean of x^2
  b1 <- c(rep(NA, win - 1), (xyBar - xBar * yBar) / (xxBar - xBar * xBar))
  return(b1)
}
