#' Moving regression version 2
#'
#' @import RcppRoll
#' @export
movReg <- function(df, span = 0.1) {
  message("Performing rolling regressions...")
  benchStart <- Sys.time()  # grab current time (for simple benchmark)
  win   <- floor(span*NROW(df))  # compute window size (rounded down)
  x <- df[[1]]
  y <- df[[2]]
  # Calculate b1 (slope) here
  xyBar <-  roll_mean(x * y, win)  # mean of xy for each window
  xBar  <-  roll_mean(x, win)  # mean of x
  yBar  <-  roll_mean(y, win)  # mean of y
  xxBar <-  roll_mean(x * x, win)  # mean of x^2
  # b1 <- c(rep(NA, win - 1), (xyBar - xBar * yBar) / (xxBar - xBar * xBar))
  b1 <- (xyBar - xBar * yBar) / (xxBar - xBar * xBar)

  benchEnd <- round(unclass(Sys.time() - benchStart)[1], 1)
  message(sprintf("%d regressions fitted ", NROW(b1)),
    sprintf("in %g seconds", benchEnd), "\n")

  return(b1)
}
