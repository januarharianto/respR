#' Calculate critical oxygen tensions
#'
#' A function to calculate the critical oxygen tension, or the O2 concentration
#' below which uptake rate becomes dependent upon oxygen concentration. It is
#' calculated by both the "broken-stick" regression (BSR) approach, adopted from
#' Yeager and Ultsch (1989), and the segmented regression approach, presented by
#' Muggeo (2003).
#'
#' The default data input is an `inspect` object, or `data.frame` containing
#' time~dissolved oxygen data (e.g. `squid.rd`). Columns of each can be
#' specified, the default being they are the first two columns (i.e. `time = 1`,
#' `oxygen = 2`). If an `inspect` object is used the data frame is extracted
#' automatically and column identifiers are not required since these were
#' already identified and extracted in those functions.
#'
#' To calculate Pcrit, the function requires data in the form of oxygen uptake
#' rate against dissolved oxygen (DO) concentration. The function performs a
#' rolling regression on time~O2 data to determine rates, and matches these
#' against a rolling mean of the DO data. The function then performs the two
#' \eqn{P_{crit}}{P[crit]} analyses methods on these data. The width of the
#' rolling regression is determined by the `width` argument. In most cases, the
#' default width (10% of the data length) works well, but this may vary with
#' data that has abrupt changes in rate, or is particularly noisy.
#'
#' Alternatively, existing rate~DO data may be used, with the `rate` input
#' identifying the column with the rate data. In this case the function performs
#' the two \eqn{P_{crit}}{P[crit]} analyses on these data directly without any
#' processing.
#'
#' @param x data frame or object of class `inspect`. This is the data to
#'   analyse.
#' @param time numeric vector. Defaults to NULL. This specifies the column
#'   number of the time data.
#' @param oxygen numeric vector. Defaults to NULL. This specifies the column
#'   number(s) of the oxygen data.
#' @param rate numeric vector. Defaults to NULL. This specifies the column
#'   number(s) of rate data.
#' @param width numeric. Determines the width of the rolling regression used to
#'   determine rolling rate. Proportion of the length of data. Defaults to 0.1,
#'   or 10% of total rows.
#' @param plot logical. Defaults to TRUE.
#' @param parallel logical. Defaults to FALSE. Should parallel processing be
#'   used?
#'
#' @return A list object of class `oxy_crit`.
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
#' data("squid.rd")
#' pcrit(squid.rd)
#' }

oxy_crit <- function(x, time = NULL, oxygen = NULL, rate = NULL,
                       width = 0.1, plot = TRUE, parallel = FALSE) {

  # data validation
  if (any(class(x) %in% "inspect")) df <- x$dataframe else
    df <- x
  if (!is.data.frame(df)) stop("oxy_crit: Input must be data.frame object.")
  if (width > nrow(df)) stop("oxy_crit: 'width' input is bigger than length of data.")

  # conditions must be met before we continue
  if (!is.null(oxygen) & !is.null(rate)) {
    # one of "rate" or "oxygen" must be NULL
    stop("oxy_crit: Choose either an 'oxygen' or 'rate' column, cannot enter both.")
  }
  # if "time" is not provided, check if we can still automate on the assumption
  # that the first column is "time":
  if (is.null(time) & !is.null(oxygen) & is.null(rate)) {
    if (oxygen == 1) stop("oxy_crit: Please specify a 'time' argument.")
  } else if (is.null(time) & is.null(oxygen) & !is.null(rate)) {
    if (rate == 1) stop("oxy_crit: Please specify a 'time' argument.")
  }

  # identify data -----
  # if nothing is provided other than data frame, use assumption that first
  # column is "time" and second column is "oxygen"
  if (is.null(time) & is.null(oxygen) & is.null(rate)) {
    col1 <- 1
    col2 <- 2
    convert <- TRUE
    message('oxy_crit: No inputs for data types. Using column 1 as "time" and column 2 as "oxygen". If you want to change this behaviour, please specify "time", "oxygen" or "rate" arguments.')
  } else if (is.numeric(time) & is.numeric(oxygen) & is.null(rate)) {
    # "time" and "oxygen" are used
    col1 <- time
    col2 <- oxygen
    convert <- TRUE
    message("oxy_crit: Performing analysis using raw oxygen data.")
  } else if (is.numeric(time) &
             is.null(oxygen) & is.numeric(rate)) {
    # "time" and "rate" are used
    col1 <- time
    col2 <- rate
    convert <- FALSE
    message("oxy_crit: Performing analysis using existing rate data.")
  } else if (is.null(time) & is.numeric(oxygen) & is.null(rate)) {
    # only "oxygen" is used
    col1 <- 1 # automatically assume column 1 is time column
    col2 <- oxygen
    convert <- TRUE
    message("oxy_crit: Performing analysis using raw oxygen data.")
    message('Using column 1 as "time".')
  } else if (is.null(time) & is.null(oxygen) & is.numeric(rate)) {
    # only "rate" ise used
    col1 <- 1 # automatically assume column 1 is time column
    col2 <- rate
    convert <- FALSE
    message("oxy_crit: Performing analysis using existing rate data.")
    message("Using column 1 as 'time'.")
  }

  # begin analysis -----
  # extract data first:
  if (any(class(df) %in% "data.table")) {
    dt <- subset(df, select = c(col1, col2))
  } else dt <- data.table(df[, c(col1, col2)])

  setnames(dt, 1:2, c("x", "y"))  # rename columns
  # if raw oxygen data, automatically generate rate data
  if (convert) {
    win <- floor(width*nrow(dt))
    dt_mr <- generate_mrdf(dt, win)
    message("Using rolling regression to convert raw oxygen data to rate...")
  } else dt_mr <- dt
  # Arrange the dataset in ascending order by x to prep for broken-stick model:
  data.table::setorder(dt_mr, "x")

  # broken-stick -----
  message("Performing broken-stick analysis (Yeager and Ultsch 1989)...")
  # speed up large data by subsampling:
  limit <- 1000
  if (nrow(dt_mr) > limit) {
    sdt <- subsample(dt_mr, n = round(nrow(dt_mr)/limit), plot = F)
  } else sdt <- dt_mr
  # generate index for iterative sampling.
  lseq <- seq.int(3, nrow(sdt) - 2) # generate sequence for lm
  # perform analysis:
  if (parallel) {
    no_cores <- parallel::detectCores() - 1  # use n-1 cores
    cl <- parallel::makeCluster(no_cores)  # initiate cluster and use those cores
    # parallel::clusterExport(cl, "broken_stick") # import function to use
    brstick <- parallel::parLapply(cl, lseq, function(z) respR::broken_stick(sdt, z))
    parallel::stopCluster(cl)  # release cores
  } else brstick <- lapply(lseq, function(z) respR::broken_stick(sdt,z))
  # convert output to data.table:
  brstick <- data.table::rbindlist(brstick)
  # arrange by increasing total sum of squares of residuals
  data.table::setorder(brstick, sumRSS)
  best <- brstick[1]

  # segmented -----
  message("Performing nonlinear breakpoint analysis (Muggeo 2003)...")
  lmfit <- lm(y ~ x, sdt)
  seg <- segmented::segmented(lmfit, seg.Z = ~ x)
  message("Convergence attained in ", seg$it, " iterations.")
  fit <- fitted(seg)
  bpfit <- data.table::data.table(x = sdt$x, y = fit)

  # output -----
  out <- list(
    dataframe = dt,
    df_rate_oxygen = dt_mr,
    width = width,
    bstick.summary = brstick,
    bpoint.summary = seg,
    bpoint.fit.df = bpfit,
    result.intercept = best$pcrit.intercept,
    result.midpoint = best$pcrit.mpoint,
    result.segmented = seg$psi[2],
    convert = convert
  )
  class(out) <- "oxy_crit"

  # Plot, if true
  if (plot) plot(out)
  return(out)
}





#' @export
print.oxy_crit <- function(x, ...) {
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
summary.oxy_crit <- function(object, export = FALSE, ...) {
  cat("Top Result for all Methods:\n")

  out <- cbind(
    object$bstick.summary[1],
    pcrit.segmented = object$result.segmented)

  print(out)

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
plot.oxy_crit <- function(x, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # Prepare data
  cutoff <- x$bstick.summary$splitpoint[1]
  segment1 <- x$df_rate_oxygen[x <= cutoff]
  segment2 <- x$df_rate_oxygen[x > cutoff]
  intercept <- x$result.intercept

  # Plot settings
  c1 <- adjustcolor("orange", alpha.f = 1)
  par(mfrow = c(2, 2), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)

  # Plot original data if available
  if (!x$convert) {
    message("Plotting...") # dummy text (make this better next time)
  } else {
    plot(x$dataframe, col = c1, pch = 21, xlab = "Time", ylab = "Oxygen", cex = .8,
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
  plot(x$df_rate_oxygen, col = c1, pch = 21, xlab = "Oxygen", ylab = "Rate", cex = .8,
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
  plot(x$df_rate_oxygen, col = c1, pch = 21, xlab = "Oxygen", ylab = "Rate", lwd = 2, cex = .8,
       panel.first = grid(lwd = .7))
  lines(x$bpoint.fit.df, lwd = 1, lty = 4)
  abline(v = x$result.segmented, col = "red", lwd = 2, lty = 2)
  legend("bottom", sprintf("Breakpoint, %g", signif(x$result.segmented, 3)),
         col = "red", lty = 1, lwd = 2, bty = "n", cex = 0.8, horiz = F)
  title(main = expression('Rate vs PO'[2] * ', Segmented'), line = 0.5)

  # plot within
  aps <- c(x$result.intercept, x$result.midpoint, x$result.segmented)
  srow <- which.min(abs(x$df_rate_oxygen[[1]] - (min(aps) * 0.99))) -1
  erow <- which.min(abs(x$df_rate_oxygen[[1]] - (max(aps) * 1.10))) +1
  subdf <- x$df_rate_oxygen[srow:erow,]
  plot(subdf, col = c1, pch = 21, xlab = "Oxygen", ylab = "Rate", cex = 2,
       panel.first = grid(lwd = .7))
  abline(v = x$result.intercept, col = "forestgreen", lwd = 2, lty = 2)
  abline(v = x$result.midpoint, col = "steelblue", lwd = 2, lty = 2)
  abline(v = x$result.segmented, col = "red", lwd = 2, lty = 2)
  title(main = expression('Rate vs PO'[2]*', Close-Up (All)'), line = 0.5)

  return(invisible(x))
}
