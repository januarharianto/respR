#' Calculate critical oxygen tension, \eqn{P_{crit}}{P[crit]}
#'
#' A function to calculate the critical oxygen tension, or the O2 concentration
#' below which uptake rate becomes dependent upon oxygen concentration. It is
#' calculated by both the "broken-stick" regression (BSR) approach, adopted from
#' Yeager and Ultsch (1989), and the segmented regression approach, presented by
#' Muggeo (2003).
#'
#' To calculate Pcrit, the function requires data in the form of the rate of
#' oxygen uptake against dissolved oxygen (DO) concentration. The default data
#' input is a data frame of DO against time (e.g. `squid.rd`). The function
#' performs a rolling regression to determine rates, and matches these against a
#' rolling mean of the DO data. The function then performs the two
#' \eqn{P_{crit}}{P[crit]} analyses methods on these data. The width of the
#' rolling regression is determined by the `width` argument. In most cases, the
#' default width (10\% of the data length) works well, but this may vary with
#' data that has abrupt changes in rate, or is particularly noisy.
#'
#' Alternatively, existing rate~DO data may be used, with the argument `has.rate
#' = "TRUE"`, in which case the function performs the two
#' \eqn{P_{crit}}{P[crit]} analyses on these data directly without any
#' processing.
#'
#' @param df data frame or object of class `inspect_data`. This is the data to
#'   analyse.
#' @param width numeric. Number of rows over which to perform the rolling
#'   regression. Defaults to `floor(0.1*nrow(df))`, or 10\% of total rows.
#' @param has.rate logical. Defaults to FALSE. If you already possess a rate~DO
#'   dataset, change this to TRUE to process it without performing rolling
#'   regressions.
#' @param plot logical. Defaults to TRUE.
#' @param parallel logical. Defaults to TRUE. Should parallel processing be
#'   used?
#'
#' @return A list object of class `pcrit`.
#'
#' @importFrom data.table data.table setnames setorder rbindlist
#' @importFrom parallel detectCores makeCluster clusterExport parLapply
#'   stopCluster
#'
#' @export
#'
#' @references Yeager DP, Ultsch GR (1989) Physiological regulation and
#'   conformation: A BASIC program for the determination of critical points.
#'   Physiological Zoology 62:888–907. doi: 10.1086/physzool.62.4.30157935
#'
#'   Muggeo V (2008) Segmented: an R package to fit regression models with
#'   broken-line relationships. R News 8:20–25.
#'
#' @examples
#' # Intensive, and a large dataset, so may take some time.
#' \dontrun{
#' pcrit(squid.rd)
#' }
pcrit <- function(df, width = floor(0.1*nrow(df)), has.rate = FALSE,
  plot = TRUE, parallel = TRUE) {

  # Data validation.
  if (any(class(df) %in% "inspect.data")) df <- df$df
  if (!is.data.frame(df)) stop("Input must be data.frame object.")
  if (width > nrow(df)) stop("`width` input is bigger than  length of data.")

  # Format data.
  dt <- data.table::data.table(df)
  data.table::setnames(dt, 1:2, c("x", "y"))

  # Check if rate is provided in "has.rate".
  if (!has.rate) {
    rdt <- generate_mrdf(dt, width)
  } else rdt <- dt

  # Arrange the dataset in ascending order by x to prep for broken-stick model.
  data.table::setorder(rdt, "x")

  # BROKEN-STICK
  message("Performing broken-stick analysis...")

  # We can speed up really large datasets by subsampling them first.
  limit <- 1000
  if (nrow(rdt) > limit) {
    srdt <- subsample(rdt, n = round(nrow(rdt)/limit), plot = F)
  } else srdt <- rdt

  # Generate index for iterative sampling.
  lseq <- seq.int(3, nrow(srdt) - 2) # generate sequence for lm

  # Then, perform broken-stick estimates.
  if (parallel) {
    no_cores <- parallel::detectCores() - 1  # use n-1 cores
    cl <- parallel::makeCluster(no_cores)  # initiate cluster and use those cores
    parallel::clusterExport(cl, "broken_stick") # import function to use
    brstick <- parallel::parLapply(cl, lseq, function(z) broken_stick(srdt, z))
    parallel::stopCluster(cl)  # release cores
  } else {
    brstick <- lapply(lseq, function(z) broken_stick(srdt,z))
  }

  brstick <- data.table::rbindlist(brstick)

  # Arrange by increasing total sum of squares of residuals
  data.table::setorder(brstick, sumRSS)
  best <- brstick[1]

  # SEGMENTED (BREAKPOINT)
  message("Performing segmented (breakpoint) analysis...")
  lmfit <- lm(y ~ x, srdt)
  seg <- segmented::segmented(lmfit, seg.Z = ~ x)
  message("Convergence attained in ", seg$it, " iterations.")
  fit <- fitted(seg)
  bpfit <- data.table::data.table(x = srdt$x, y = fit)

  # Generate output

  out <- list(
    df = df,
    mr.df = rdt,
    has.rate = has.rate,
    width = width,
    bstick.summary = brstick,
    bpoint.summary = seg,
    bpoint.fit.df = bpfit,
    result.intercept = best$pcrit.intercept,
    result.midpoint = best$pcrit.mpoint,
    result.segmented = seg$psi[2]
  )

  class(out) <- "pcrit"

  # Plot, if true
  if (plot) plot(out)

  return(out)
}





#' @export
print.pcrit <- function(x, ...) {
  cat("--Broken stick (Yeager & Ultsch 1989)--\n")
  cat(sprintf("Sum RSS     %g\n", x$bstick.summary$sumRSS[1]))
  cat(sprintf("Intercept   %g\n", x$result.intercept))
  cat(sprintf("Midpoint    %g\n", x$result.midpoint))

  cat("\n--Segmented (Muggeo 2003)--\n")
  cat(sprintf("Std. Err.   %g\n", x$bpoint.summary$psi[3]))
  cat(sprintf("Breakpoint  %g\n", x$result.segmented))
  return(invisible(x))

}

#' @export
summary.pcrit <- function(object, ...) {
  cat("Top Result for all Methods:\n")
  summ <- cbind(
    object$bstick.summary[1],
    pcrit.segmented = object$result.segmented)

  print(summ)
  return(invisible(summ))
}




#' @export
plot.pcrit <- function(x, ...) {
  # Prepare data
  cutoff <- x$bstick.summary$splitpoint[1]
  segment1 <- x$mr.df[x <= cutoff]
  segment2 <- x$mr.df[x > cutoff]
  intercept <- x$result.intercept

  # Plot settings
  c1 <- adjustcolor("orange", alpha.f = 1)
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow = c(2, 2), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)

  # Plot original data if available
  if (x$has.rate) {
    # No plot here :D
  } else {
    plot(x$df, col = c1, pch = 21, xlab = "Time", ylab = "Oxygen", cex = .8,
      panel.first = grid(lwd = .7))
    abline(h = x$result.intercept, col = "forestgreen", lwd = 2, lty = 2)
    abline(h = x$result.midpoint, col = "steelblue", lwd = 2, lty = 3)
    abline(h = x$result.segmented, col = "red", lwd = 2, lty = 4)
    legend("top", c(sprintf("Breakpoint, %g", signif(x$result.segmented, 3)),
      sprintf("Intercept, %g", signif(x$result.intercept, 3)),
      sprintf("Midpoint, %g", signif(x$result.midpoint, 3))),
      col = c("red", "darkolivegreen", "steelblue"), lty = 1, lwd = 2,
      bty = "n", cex = 0.8, horiz = F)
    title(main = expression("Original Series"), line = 0.5)
  }

  # Plot for broken-stick
  plot(x$mr.df, col = c1, pch = 21, xlab = "Oxygen", ylab = "Rate", cex = .8,
    panel.first = grid(lwd = .7))
  abline(lm(y ~ x, segment1), lwd = 1, lty = 4)
  abline(lm(y ~ x, segment2), lwd = 1, lty = 4)
  abline(v = x$result.intercept, col = "forestgreen", lwd = 2, lty = 2)
  abline(v = x$result.midpoint, col = "steelblue", lwd = 2, lty = 3)
  legend("bottom", c(sprintf("Intercept, %g", signif(x$result.intercept, 3)),
    sprintf("Midpoint, %g", signif(x$result.midpoint, 3))),
    col = c("darkolivegreen", "steelblue"), lty = 1, lwd = 2, bty = "n",
    cex = 0.8, horiz = F)
  title(main = expression('Rate vs PO'[2] * ', Broken-Stick'), line = 0.5)

  # Plot for segmented (breakpoint)
  plot(x$mr.df, col = c1, pch = 21, xlab = "Oxygen", ylab = "Rate", lwd = 2, cex = .8,
    panel.first = grid(lwd = .7))
  lines(x$bpoint.fit.df, lwd = 1, lty = 4)
  abline(v = x$result.segmented, col = "red", lwd = 2, lty = 2)
  legend("bottom", sprintf("Breakpoint, %g", signif(x$result.segmented, 3)),
    col = "red", lty = 1, lwd = 2, bty = "n", cex = 0.8, horiz = F)
  title(main = expression('Rate vs PO'[2] * ', Segmented'), line = 0.5)

  # plot within
  aps <- c(x$result.intercept, x$result.midpoint, x$result.segmented)
  subdf <- x$mr.df[x > min(aps) * 0.99][x < max(aps) *1.01]
  plot(subdf, col = c1, pch = 21, xlab = "Oxygen", ylab = "Rate", cex = 2,
    panel.first = grid(lwd = .7))
  abline(v = x$result.intercept, col = "forestgreen", lwd = 2, lty = 2)
  abline(v = x$result.midpoint, col = "steelblue", lwd = 2, lty = 2)
  abline(v = x$result.segmented, col = "red", lwd = 2, lty = 2)
  title(main = expression('Rate vs PO'[2]*', Close-Up (All)'), line = 0.5)

  par(pardefault)  # revert par settings to original
  return(invisible(x))
}


#' Perform broken-stick regressions
#'
#' @keywords internal
#'
#' @export
broken_stick <- function(dt, n) {
  # Cut data into 2
  dta <- dt[1:n]
  dtb <- dt[(n + 1):nrow(dt)]

  # Perform lm
  lma <- .lm.fit(cbind(1, dta[[1]]), dta[[2]])
  lmb <- .lm.fit(cbind(1, dtb[[1]]), dtb[[2]])

  # Extract coefficients
  coefa <- coef(lma)
  coefb <- coef(lmb)

  # Calculate residual sum of squares
  trss <- sum(lma$residuals*lma$residuals) + sum(lmb$residuals*lmb$residuals)

  # Also, calculate intersect
  cm <- rbind(coefa, coefb)
  # https://stackoverflow.com/a/7114961
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])[1]

  # Calculate midpoint
  midpoint <-  (dta[,x][nrow(dta)] + dtb[,x][1]) / 2

  # Generate output
  out <- data.table::data.table(
    splitpoint = dta[,x][nrow(dta)],
    sumRSS = trss,
    pcrit.intercept = intersect,
    pcrit.mpoint = midpoint
  )
  return(out)
}


#' Generate a DO ~ PO2 data table from a DO timeseries
#'
#' @keywords internal
#'
#' @export
generate_mrdf <- function(dt, width) {
  # Ensure that dt is a data.table
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("x", "y"))

  # Extract columns
  x <- as.matrix(dt[,1])
  y <- as.matrix(dt[,2])

  # Then, perform rolling mean and lm
  rollx <- na.omit(roll::roll_mean(y, width))
  rolly <- static_roll(dt, width)

  # Then, combine into new data.table
  rdt <- data.table::data.table(rollx, abs(rolly$rate_b1))
  data.table::setnames(rdt, 1:2, c("x", "y"))
  return(rdt)
}

