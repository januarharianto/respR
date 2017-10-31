#' Calculate critical oxygen tension, \eqn{P_{crit}}{P[crit]}
#'
#' Critical oxygen tension can be calculated either by the "broken-stick" regression (BSR) approach, adopted from Yeager and Ultsch (1989), or the segmented regression approach, presented by Muggeo (2003).
#'
#' To determine P_crit, the rate of change in oxygen, RO_2 (or its mass-specific variation, MO_2) and ambient oxygen concentration (PO_2) must be provided. If RO_2 has not been calculated, `pcrit()`` can automatically generate the data by performing a rolling regression of the PO_2 values.
#'
#' @md
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @param df data frame.
#' @param width numeric.
#' @param has.rate logical.
#' @param plot logical.
#'
#' @return A list object.
#' @export
#'
#' @examples
#' NULL
pcrit2 <- function(df, width = floor(0.1*nrow(df)), has.rate = F, plot = T) {
  # Check inputs:
  if (any(class(df) == "data.frame") == F)
    stop("Input must be data.frame object.")
  if (width > nrow(df))
    stop("Width input is too large. Please choose a smaller value.")
  # If the user only has raw data in the df, we use it to generate rate data
  if (has.rate == F) {
    # Extract columns
    x <- df[, 1]
    y <- df[, 2]
    # Check that x is numeric
    if (any(class(x) == "POSIXct") | any(class(x) == "POSIXt"))
      x <- as.numeric(x) - min(as.numeric(x))
    x <- roll::roll_mean(matrix(y), width)  # Perform rolling mean for new x
    y <- rollfit(df, width)$b1  # Perform rolling regression for new y
    counts <- length(y)
    # Create the new df for analysis
    mr.df <- na.omit(data.frame(x, y = abs(y)))
    mr.df <- dplyr::arrange(mr.df, x)
  } else {
    mr.df <- df    # re-assign df to mr.df
    width <- NULL  # remove number since it's not used
    df    <- NULL  # set as NULL since it's not the usual pO2/time data
  }
  # ---------------------
  # Broken stick RSS method
  message("Performing broken-stick analysis...")
  index <- gen.index(nrow(mr.df))  # create matrix for sampling
  no_cores <- parallel::detectCores() - 1  # use n-1 cores
  cl <- parallel::makeCluster(no_cores)  # initiate cluster and use those cores
  reg <- parallel::parApply(cl, index, 1, broken.stick, df = mr.df)
  parallel::stopCluster(cl)  # release cores
  reg <- dplyr::bind_rows(reg)
  reg <- dplyr::arrange(reg, sumRSS)
  message(counts, " iterative broken-stick regressions completed.")
  # Grab the best results (has lowest sum RSS):
  best <- dplyr::filter(reg, sumRSS == min(sumRSS))
  # ---------------------
  # Breakpoint gap method
  message("Performing segmented (breakpoint) analysis...")
  lmfit <- lm(y ~ x, mr.df)
  seg <- segmented::segmented(lmfit, seg.Z = ~ x)
  message("Convergence attained in ", seg$it, " iterations.")
  fit <- fitted(seg)
  bpfit <- data.frame(x = mr.df$x, y = fit)
  # Prepare output
  out <- list(df    = df,
              mr.df = mr.df,
              width = width,
              bstick.summary   = reg,
              bstick.intercept = best$pcrit.intercept,
              bstick.midpoint  = best$pcrit.mpoint,
              bpoint.summary   = seg,
              bpoint.fit.df    = bpfit,
              bpoint.result    = seg$psi[2]
  )
  class(out) <- "pcrit2"
  if (plot) plot(out)
  return(out)
}

print.pcrit2 <- function(x) {
  cat("--Broken stick (Yeager & Ultsch 1989)--\n")
  cat(sprintf("Sum RSS     %g\n", x$bstick.summary$sumRSS[1]))
  cat(sprintf("Intercept   %g\n", x$bstick.intercept))
  cat(sprintf("Midpoint    %g\n", x$bstick.midpoint))

  cat("\n--Segmented (Muggeo 2003)--\n")
  cat(sprintf("Std. Err.   %g\n", x$bpoint.summary$psi[3]))
  cat(sprintf("Breakpoint  %g\n", x$bpoint.result))
}


plot.pcrit2 <- function(x) {
  # Prepare data
  cutoff <- x$bstick.summary$splitpoint[1]
  segment1 <- dplyr::filter(x$mr.df, x <= cutoff)
  segment2 <- dplyr::filter(x$mr.df, x > cutoff)
  intercept <- x$bstick.intercept

  # Plot settings
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow = c(2, 2), mar = c(4, 4, 0.5, 0.5), oma = c(1.5, 1, 1, 1))

  # Plot original data if available
  if (is.null(x$df)) {
    plot.new()
  } else {
    plot(x$df, col = r2, pch = 16, xlab = "Time", ylab = "O2", lwd = 2, panel.first = c(rect(par("usr")[1],
      par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white",
        lty = 1, lwd = 1.5)))
    abline(h = x$bstick.intercept, col = "forestgreen", lwd = 3, lty = 1)
    abline(h = x$bstick.midpoint, col = "steelblue", lwd = 3, lty = 1)
    abline(h = x$bpoint.result, col = "red", lwd = 3, lty = 1)
  }

  # plot within
  aps <- c(x$bstick.intercept, x$bstick.midpoint, x$bpoint.result)
  subdf <- dplyr::filter(x$mr.df, x > min(aps) * 0.99 & x < max(aps) * 1.01)
  plot(subdf, col = r2, pch = 16, xlab = "PO2", ylab = "Rate", lwd = 2, panel.first = c(rect(par("usr")[1],
    par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white", lty = 1,
      lwd = 1.5)))
  abline(v = x$bstick.intercept, col = "forestgreen", lwd = 3, lty = 1)
  abline(v = x$bstick.midpoint, col = "steelblue", lwd = 3, lty = 1)
  abline(v = x$bpoint.result, col = "red", lwd = 3, lty = 1)

  # Plot for broken-stick
  plot(x$mr.df, col = r2, pch = 16, xlab = "PO2", ylab = "Rate", lwd = 2, panel.first = c(rect(par("usr")[1],
    par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white", lty = 1,
      lwd = 1.5)))
  abline(lm(y ~ x, segment1), lwd = 1.5, lty = 4, col = "gray35")
  abline(lm(y ~ x, segment2), lwd = 1.5, lty = 4, col = "gray35")
  abline(v = x$bstick.intercept, col = "forestgreen", lwd = 3, lty = 1)
  abline(v = x$bstick.midpoint, col = "steelblue", lwd = 3, lty = 1)
  legend("bottom", c(sprintf("Intercept, %g", signif(x$bstick.intercept, 3)), sprintf("Midpoint, %g",
    signif(x$bstick.midpoint, 3))), col = c("darkolivegreen", "steelblue"), lty = 1, lwd = 2, bty = "o",
    cex = 0.8, horiz = F)

  # Plot for segmented
  plot(x$mr.df, col = r2, pch = 16, xlab = "PO2", ylab = "Rate", lwd = 2, panel.first = c(rect(par("usr")[1],
    par("usr")[3], par("usr")[2], par("usr")[4], col = r3), grid(col = "white", lty = 1,
      lwd = 1.5)))
  lines(x$bpoint.fit.df, lwd = 2, col = "gray35")
  abline(v = x$bpoint.result, col = "red", lwd = 3, lty = 1)
  legend("bottom", sprintf("Breakpoint, %g", signif(x$bpoint.result, 3)), col = "red",
    lty = 1, lwd = 2, bty = "o", cex = 0.8, horiz = F)

  par(pardefault)  # revert par settings to original
}




# generate an index for hockeyLm
gen.index <- function(x, min = 3) {
  seq1 <- data.frame(1, (seq.int(min, (x-min))))
  seq2 <- data.frame((seq.int(min, (x-min)) + 1), x)
  seqs <- unname(as.matrix(cbind(seq1, seq2)))
  return(seqs)
}

# perform broken-stick regressions
broken.stick <- function(indx, df) {
  # generate windows
  x1 <- df[, 1][indx[1]:indx[2]] # x-coordinate of line 1
  y1 <- df[, 2][indx[1]:indx[2]] # y-coordinate of line 1
  x2 <- df[, 1][indx[3]:indx[4]] # x-coordinate of line 2
  y2 <- df[, 2][indx[3]:indx[4]] # y-coordinate of line 2
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
  # b1a   <- coef1[2]
  # b1b   <- coef2[2]
  # calculate RSS (residual sum of squares)
  res1   <- reg1$residuals
  res2   <- reg2$residuals
  rss1   <- sum(res1 * res1)
  rss2   <- sum(res2 * res2)
  sumRSS <- rss1 + rss2
  # calculate intersection between 2 lines
  cm        <- rbind(coef1, coef2)
  # https://stackoverflow.com/a/7114961:
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
  xint      <- intersect[1]
  # yint      <- intersect[2]
  # Output
  out <- data.frame(
    # start1          = start1,
    splitpoint      = end1,
    # slope1          = b1a,
    # start2          = start2,
    # end2            = end2,
    # slope2          = b1b,
    sumRSS          = sumRSS,
    # xint            = yint,
    pcrit.intercept = xint,
    pcrit.mpoint    = (end1 + start2) / 2)
  return(out)
}
