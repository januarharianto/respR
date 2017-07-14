#' @export
hockeyLm <- function(indx, df) {
  # generate windows
  x1 <- df[, 1][indx[1]:indx[2]]
  y1 <- df[, 2][indx[1]:indx[2]]
  x2 <- df[, 1][indx[3]:indx[4]]
  y2 <- df[, 2][indx[3]:indx[4]]
  # design matrix of dimension n * p for .lm.fit
  mx1 <- matrix(cbind(1, x1), ncol = 2)
  mx2 <- matrix(cbind(1, x2), ncol = 2)
  # extract index
  start1 <- x1[1]
  end1   <- x1[length(x1)]
  start2 <- x2[1]
  end2   <- x2[length(x2)]
  # lm.fit
  reg1 <- .lm.fit(mx1, y1)
  reg2 <- .lm.fit(mx2, y2)
  # coefficients
  coef1 <- coef(reg1)
  coef2 <- coef(reg2)
  b1a   <- coef1[2]
  b1b   <- coef2[2]
  # calculate RSS (residual sum of squares)
  res1   <- reg1$residuals
  res2   <- reg2$residuals
  rss1   <- sum(res1 * res1)
  rss2   <- sum(res2 * res2)
  sumRSS <- rss1 + rss2
  #intersect
  cm        <- rbind(coef1, coef2)
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
  xint      <- intersect[1]
  yint      <- intersect[2]
  # output
  out <- data.frame(  # need to improve output
                    # start1       = start1,
                    splitpoint     = end1,
                    slope1         = b1a,
                    # start2       = start2,
                    # end2         = end2,
                    slope2         = b1b,
                    sumRSS         = sumRSS,
                    # xint         = yint,
                    pcrit.lm       = xint,
                    pcrit.mpoint   = (end1 + start2) / 2)
  out
}
