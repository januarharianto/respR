#' Calculate critical oxygen values, such as PCrit
#'
#' A function to calculate the critical oxygen value, the oxygen tension or
#' concentration below which an uptake rate transitions from independent to
#' dependent on the oxygen supply, typically known as *PCrit*.
#'
#' In earlier versions of `respR`, this function was known as `calc_pcrit`. It
#' was renamed to avoid conflicts with a function of the same name in another
#' package, and also because technically the *P* in  *PCrit* stands for the
#' partial *pressure* of oxygen. Since the function returns the value in the
#' units of the data as entered, whether they are concentration or pressure
#' units, this terminology can be technically in error. Instead, for the
#' purposes of the documentation we refer to this as the *Critical Oxygen
#' Value*, or "*COV*". If the units of oxygen are partial pressure units (e.g.
#' kPa), this is equivalent to PCrit, otherwise they should be reported with
#' this in mind.
#'
#' ## Methods
#'
#' The `oxy_crit()` function provides two methods (one of which outputs two
#' results) to calculate the *COV*. These are selected using the `method` input.
#'
#' ### Broken Stick Regression:    `method = "bsr"`
#'
#' This is the default method, adapted from the “Broken-Stick” regression
#' (*BSR*) approach, of Yeager & Ultsch (1989), in which two segments of the
#' data are iteratively fitted and the intersection with the smallest sum of the
#' residual sum of squares between the two linear models is the estimated *COV*.
#' Two slightly different ways of reporting this breakpoint are detailed by
#' Yeager & Ultsch (1989); the *intercept* and *midpoint*. These are usually
#' very close in value, and the function returns both.
#'
#' The `thin` input influences the *BSR* analysis. The method is very
#' computationally intensive, so to speed up analyses the `thin` input will
#' subsample datasets longer than this input to this number or rows before
#' analysis. The default value of 5000 has in testing provided a good balance
#' between speed and results accuracy and repeatability. However, results may
#' vary with different datasets, so users should experiment with varying the
#' value. To perform no subsampling and use the entire dataset enter `thin =
#' NULL`. It has no effect on datasets shorter than the `thin` input.
#'
#' ### Segmented Regression:    `method = "segmented"`
#'
#' The second method is a wrapper for the "Segmented" regression approach,
#' available as part of the `segmented` R package (Muggeo 2008), which estimates
#' the *COV* by iteratively fitting two intersecting models and selecting the
#' value that minimises the “gap” between the fitted lines.
#'
#' ## Inputs
#'
#' The data input `x` should be an `inspect` object or `data.frame` containing
#' oxygen~time data, *or* a `data.frame` containing rate~oxygen data.
#'
#' ### Oxygen ~ Time data
#'
#' This is the typical input, where a timeseries of oxygen concentrations or
#' partial pressures against time has been recorded, generally down to a very
#' low value of oxygen. A column of `time` and a column of `oxygen` should be
#' specified. The function defaults to `time = 1` and `oxygen = 2` if no other
#' inputs are entered. If an `inspect` object is entered as the `x` input, the
#' data frame is extracted automatically and column identifiers are not required
#' since these were already entered in `inspect`. Note, if multiple `oxygen`
#' columns were entered in `inspect` only the first entered one will be used in
#' `oxy_crit`.
#'
#' To calculate the *COV*, the function requires data in the form of oxygen
#' uptake rate against oxygen value. Therefore, the function performs a rolling
#' regression on the oxygen~time data to determine rates, and pairs these
#' against a rolling mean of the oxygen data. The function then performs the
#' selected analysis `method` on these data. The width of the rolling regression
#' and rolling mean is determined by the `width` argument. The default is 0.1,
#' representing 10% of the length of the data. This performs well in testing,
#' however performance may vary with data that has abrupt changes in rate, or is
#' particularly noisy. Users should experiment with different `width` values to
#' see how it affects results, and report this with their results and analysis
#' parameters.
#'
#' ### Rate ~ Oxygen data
#'
#' Alternatively, if existing rolling oxygen uptake rates have been calculated,
#' and have appropriate paired oxygen concentration or partial pressure values,
#' these can be entered with the `rate` and `oxygen` inputs specifying the
#' respective columns. In this case the function performs the selected analysis
#' `method` on these data directly without any processing. The `width` input in
#' this case is not relevant and is ignored.
#'
#' This option can only be used with data frame `x` inputs. Note, other columns
#' such as time data may be present in the input, but are not required so need
#' not be specified.
#'
#' ## Plot
#'
#' A plot is produced (provided `plot = TRUE`) of the input data and results.
#' The top panel is the input data, either the oxygen~time timeseries, or the
#' rate~oxygen series, depending on what was entered in `x`. If the former, the
#' critical oxygen value is indicated by a horizontal line, or two lines in the
#' case of the Broken-Stick analysis. Note, since the two *BSR* results are
#' usually close in value these may overlay each other.
#'
#' The bottom plot is the rate~oxygen series upon which the analysis was
#' conducted, either as input or as calculated. Critical oxygen values are
#' indicated by vertical lines, and regression fits upon which the analysis was
#' based by black dashed lines.
#'
#' Note, that in `respR` oxygen uptake rates are negative since they represent a
#' negative slope of oxygen against time, therefore by default rates are plotted
#' on a reverse y-axis so higher rates appear higher on the plot. If analysing
#' already calculated rates which are positive values this behaviour can be
#' reversed by passing `rate.rev = FALSE` in either the main function call or
#' when calling `plot()` on the output object. There is no issue with using
#' positive rate values; they will give identical critical value results in the
#' analysis.
#'
#' ### Additional plotting options
#'
#' If the legend obscures parts of the plot they can be suppressed using `legend
#' = FALSE`. Suppress console output messages with `quiet = TRUE`. Each panel
#' can be plotted on its own using `choose = 1` or `choose = 2`. If using
#' already-calculated, positive rate values to identify critical oxygen values,
#' the y-axis of the rolling rate plot can be plotted *not* reversed by passing
#' `rate.rev = FALSE` These inputs can be passed in either the main `oxy_crit`
#' call or when calling `plot()` on the output object. If axis labels
#' (particularly y-axis) are difficult to read, `las = 2` can be passed to make
#' axis labels horizontal. In addition, `oma` (outer margins, default `oma =
#' c(0.4, 1, 1.5, 0.4)`), and `mai` (inner margins, default `mai = c(0.3, 0.15,
#' 0.35, 0.15)`) can be used to adjust plot margins.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()` and
#' `summary()`.
#'
#' - `print()`: prints the critical oxygen value for the particular `method`
#' used.
#'
#' - `summary()`: prints critical oxygen value, plus additional coefficients and
#' metadata for the particular `method` used. See Yeager & Ultsch (1989) and
#' Muggeo (2008) for what these represent. The summary can be exported as a
#' separate data frame by passing `export = TRUE`.
#'
#' ## Output
#'
#' Output is a `list` object of class `oxy_crit` containing input parameters and
#' data, various summary data, metadata, and the primary output of interest
#' `$crit`, which is the critical oxygen value in the units of the oxygen data
#' as entered. This can be converted to additional units using `convert_DO()`.
#' Note, if the Broken-Stick analysis (`method == "bsr"`) has been used, `$crit`
#' will contain two results; `$crit.intercept` and `$crit.midpoint`. For full
#' explanation of the difference between these see Yeager & Ultsch (1989),
#' however they are generally very close in value.
#'
#' @param x object of class `inspect` or a `data.frame` containing either paired
#'   oxygen~time values, or paired rate~oxygen values. See Details.
#' @param method string. Defaults to `"bsr"`. Critical oxygen value analysis
#'   method. Either `"bsr"` or `"segmented"`. See Details.
#' @param time integer. Defaults to 1. Specifies column number of the time data.
#' @param oxygen integer. Defaults to 2. Specifies column number of the oxygen
#'   data.
#' @param rate integer. Defaults to NULL. Specifies column number of the rate
#'   data.
#' @param width numeric value between 0 and 1 representing proportion of the
#'   total data length. Determines the width of the rolling regression used to
#'   determine the rolling rate and the rolling mean of oxygen values the rate
#'   is paired with. Defaults to 0.1, representing 10% of total rows.
#' @param parallel logical. Defaults to FALSE. Enables parallel processing for
#'   computationally intensive analyses of large datasets.
#' @param thin integer. Defaults to 5000. Number of rows to subsample `x` data
#'   to before running `"bsr"` analysis. No effect on datasets smaller than this
#'   value or with `"segmented"` method. To perform no subsampling enter as
#'   `NULL`. See Details.
#' @param plot logical. Defaults to TRUE.
#' @param ... Allows additional plotting controls to be passed, such as `legend
#'   = FALSE`, `quiet = TRUE`, `rate.rev = FALSE`, and `choose`. See Plotting
#'   section.
#'
#' @importFrom data.table data.table as.data.table setnames setorder rbindlist
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
#' # Intensive, and a large dataset, so these may take some time.
#' \dontrun{
#'
#' ## Run on oxygen~time data.frame with default inputs
#' oxy_crit(squid.rd)
#'
#' ## Try a lower 'thin' input to speed up analysis
#' oxy_crit(squid.rd, thin = 1000)
#'
#' ## Use the Segmented method instead
#' oxy_crit(squid.rd, method = "segmented")
#'
#' ## Experiment with different 'width' input
#' # Higher widths tend to oversmooth data
#' oxy_crit(squid.rd, method = "segmented", width = 0.2)
#' # Lower width in this case gives very similar result to default 0.1
#' oxy_crit(squid.rd, method = "segmented", width = 0.05)
#'
#' ## Run on oxygen~time data in 'inspect' object
#' insp <- inspect(squid.rd, time = 1, oxygen = 2)
#' oxy_crit(insp)
#'
#' ## Run on already calculated rate~oxygen data
#' # Generate using internal function
#' o2.v.rates <- generate_mrdf(squid.rd, 0.1*nrow(squid.rd))
#' # Analyse
#' oxy_crit(o2.v.rates, oxygen = 1, rate = 2)
#' }

oxy_crit <- function(x, method = "bsr", time = NULL, oxygen = NULL, rate = NULL,
                     width = 0.1, parallel = FALSE, thin = 5000, plot = TRUE, ...) {

  ## Save function call for output
  call <- match.call()

  # data validation
  if (any(class(x) %in% "inspect")) df <- x$dataframe else
    df <- x
  if (!is.data.frame(df)) stop("oxy_crit: Input must be an 'inspect' or data.frame object.")
  if (!(dplyr::between(width, 0.001, 0.999)))
    stop("oxy_crit: 'width' input should be between 0.001 to 0.999, representing a proportion of the total data length.")

  ## validate method
  if(!(method %in% c("bsr", "segmented")))
    stop("oxy_crit: 'method' input not recognised.")
  ## validate thin
  input.val(thin, num = TRUE, int = TRUE, req = FALSE,
            max = 1, min = 1, range = c(1,Inf), msg = "oxy_crit: 'thin' -")
  ## validate column inputs
  column.val(time, req = FALSE, min = 1, max = 1,
             range = c(1,ncol(df)), conflicts = c(oxygen, rate), msg = "oxy_crit: 'time' -")
  column.val(oxygen, req = FALSE, min = 1, max = 1,
             range = c(1,ncol(df)), conflicts = c(time, rate), msg = "oxy_crit: 'oxygen' -")
  column.val(rate, req = FALSE, min = 1, max = 1,
             range = c(1,ncol(df)), conflicts = c(time, oxygen), msg = "oxy_crit: 'rate' -")

  ## Rate~Oxygen analysis can only be done with dfs not inspect
  if("inspect" %in% class(x) && !is.null(oxygen) && !is.null(rate))
    stop("oxy_crit: 'inspect' input detected. Rate~Oxygen analyses cannot be conducted with 'inspect' objects!")

  ## if inspect - any other inputs ignored - should be NULL
  if("inspect" %in% class(x) && (!is.null(time) | !is.null(oxygen) | !is.null(rate))) {
    warning("oxy_crit: 'inspect' input detected. Column inputs ignored. These will have been specified in 'inspect'.")
    time <- NULL
    oxygen <- NULL
    rate <- NULL
  }

  ## if inspect - multiple columns warning
  if("inspect" %in% class(x) && ncol(x$dataframe) > 2)
    warning("oxy_crit: Multiple columns of oxygen data found in 'inspect' input.\n  Analysis will use first column only!\n  To analyse other oxygen columns, use 'inspect()' to save them as separate objects.")

  ## if inputs NULL, apply defaults
  if(is.null(time) && is.null(oxygen) && is.null(rate)) {
    time = 1
    oxygen = 2
    col1 <- time
    col2 <- oxygen
    convert = TRUE
    message("oxy_crit: Applying column defaults of 'time = 1' and 'oxygen = 2'.")
    ## time and oxygen columns
  } else if (!is.null(time) && !is.null(oxygen) && is.null(rate)) {
    col1 <- time
    col2 <- oxygen
    convert = TRUE
    message("oxy_crit: Performing analysis using Oxygen ~ Time data.")
  } else if (is.null(time) && !is.null(oxygen) && !is.null(rate)) {
    col1 <- oxygen
    col2 <- rate
    convert = FALSE
    message("oxy_crit: Performing analysis using Rate ~ Oxygen data.")
  } else {
    stop("oxy_crit: Inputs should be 'time' and 'oxygen' columns, or 'oxygen' and 'rate' columns.")
  }

  # begin analysis -----
  # extract data first:
  if (any(class(df) %in% "data.table")) {
    dt <- subset(df, select = c(col1, col2))
  } else {
    dt <- data.table(df[, c(col1, col2)])
  }

  data.table::setnames(dt, 1:2, c("x", "y"))  # rename columns

  # if raw oxygen data, automatically generate rate data
  if (convert) {
    win <- floor(width*nrow(dt))
    dt_mr <- generate_mrdf(dt, win)
    # else format existing rate data as same structure
  } else {
    dt_mr <- dt
  }
  # Arrange the dataset in ascending order by x to prep for broken-stick model:
  data.table::setorder(dt_mr, "x")

  # broken-stick -----
  if(method == "bsr"){
    message("oxy_crit: Performing Broken-Stick analysis (Yeager and Ultsch 1989)...")
    # for Done message
    st.tm <- Sys.time()

    # speed up large data by subsampling:
    if (!is.null(thin) && nrow(dt_mr) > thin) {
      sdt <- subsample(dt_mr, length.out = thin, plot = F)
    } else sdt <- dt_mr
    # generate index for iterative sampling.
    lseq <- seq.int(3, nrow(sdt) - 2) # generate sequence for lm
    # perform analysis:
    if (parallel) {
      no_cores <- parallel::detectCores() - 1  # use n-1 cores
      cl <- parallel::makeCluster(no_cores)  # initiate cluster and use those cores
      # parallel::clusterExport(cl, "broken_stick") # import function to use
      results <- parallel::parLapply(cl, lseq, function(z) broken_stick(sdt, z))
      parallel::stopCluster(cl)  # release cores
    } else results <- lapply(lseq, function(z) broken_stick(sdt,z))
    # convert output to data.table:
    results <- data.table::rbindlist(results)
    # arrange by increasing total sum of squares of residuals
    data.table::setorder(results, sumRSS)
    # reorder columns so 2 results are last columns
    # and rename
    results <- results[,c(1,2,5:8,3,4)]
    names(results)[7:8] <- c("crit.intercept", "crit.midpoint")

    summary <- results[1]
    crit <- list(crit.intercept = summary$crit.intercept,
                 crit.midpoint = summary$crit.midpoint)

    en.tm <- Sys.time()
    dur <- difftime(en.tm, st.tm, units = "secs")
    dur <- round(as.numeric((dur)), 1)
    message(glue::glue("oxy_crit: Broken-Stick analysis completed in {dur} seconds."))
  }

  # segmented -----
  if(method == "segmented"){
    message("oxy_crit: Performing Segmented breakpoint analysis (Muggeo 2003)...")
    lmfit <- lm(y ~ x, dt_mr)
    results <- segmented::segmented(lmfit, seg.Z = ~ x)
    message("oxy_crit: Segmented analysis convergence attained in ", results$it, " iterations.")
    fit <- fitted(results)
    seg_fit <- data.table::data.table(x = dt_mr$x, y = fit)
    results$seg_fit <- seg_fit
    crit <- results$psi[2]

    # not actually sure what to put in this, but there has to be something
    summary <- c(results$coefficients,
                 std.err = results$psi[3],
                 crit.segmented = crit)
    summary <- as.data.table(t(summary))
    names(summary) <- c("Intercept", "x", "U1.x", "psi1.x", "std.err", "crit.segmented")
  }

  ## rename df columns for output
  if(convert) names(dt) <- c("time", "oxygen") else
    names(dt) <- c("oxygen", "rate")
  names(dt_mr) <- c("oxygen", "rate")

  # output -----
  out <- list(
    call = call,
    inputs = list(x = x, method = method, time = time,
                  oxygen = oxygen, rate = rate,
                  width = width, plot = plot,
                  thin = thin,
                  parallel = parallel),
    dataframe = dt,
    df_rate_oxygen = dt_mr,
    width = width,
    convert = convert,
    method = method,
    results = results,
    summary = summary,
    crit = crit
  )

  class(out) <- "oxy_crit"

  # Plot, if true
  if (plot) plot(out, ...)
  return(out)
}

#' @export
print.oxy_crit <- function(x, ...) {

  cat("\n# print.oxy_crit # ----------------------\n")
  cat("\n")

  if(x$method == "bsr") {
    cat("Broken-Stick (Yeager & Ultsch 1989):\n\n")
    cat("Sum RSS     ", x$summary$sumRSS, "\n")
    cat("Intercept   ", x$summary$crit.intercept, "\n")
    cat("Midpoint    ", x$summary$crit.midpoint, "\n")
  }

  if(x$method == "segmented"){
    cat("Segmented (Muggeo 2003):\n\n")
    cat("Std. Err.   ", x$summary$std.err, "\n")
    cat("Breakpoint  ", x$summary$crit.segmented, "\n")
  }

  cat("\n-----------------------------------------\n")

  return(invisible(x))
}

#' @export
summary.oxy_crit <- function(object, export = FALSE, ...) {
  cat("\n# summary.oxy_crit # --------------------\n\n")

  ## result
  summ <- unlist(object$summary)

  out <- as.data.table(t(summ))

  if(object$method == "bsr") {
    cat("--Broken-Stick Analysis Summary--\n")
    cat("Top ranked result shown. Others available in '$results' element of output.\n\n")
  }
  if(object$method == "segmented") {
    cat("--Segmented Analysis Summary--\n")
  }

  print(out)

  cat("\n-----------------------------------------\n")

  if(export == TRUE) return(invisible(out))
  return(invisible(object))
}

#' @export
plot.oxy_crit <- function(x, legend = TRUE, quiet = FALSE, choose = NULL,
                          rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if(!quiet && !x$convert)
    message("plot.oxy_crit: Plotting Rate ~ Oxygen derived critical oxygen results.")
  if(!quiet && x$convert)
    message("plot.oxy_crit: Plotting Oxygen ~ Time derived critical oxygen results.")

  # Prepare data
  # bsr
  if(x$method == "bsr") {
    bsr.intercept <- x$crit$crit.intercept
    bsr.midpoint <- x$crit$crit.midpoint
    cutoff <- x$summary$splitpoint[1]
    segment1 <- x$df_rate_oxygen[which(x$df_rate_oxygen[[1]] <= cutoff)]
    segment2 <- x$df_rate_oxygen[which(x$df_rate_oxygen[[1]] > cutoff)]
  }

  # seg
  if(x$method == "segmented") {
    seg.breakpoint <- x$crit
  }

  # Plot settings -----------------------------------------------------------
  # Point character colour and type
  c1 <- adjustcolor("lightgreen", alpha.f = 1)

  # point size
  ptcex <- 0.5
  # legend text size
  legcex <- 0.8
  # legend text vertical spacing
  leg.y.intersp <- 0.7
  # result line colours
  # in BSR Intercept, BSR Midpoint, Seg Breakpoint order
  line_cols_bsr <- c("red", "orange")
  line_cols_seg <- c("steelblue")
  line_types_bsr <- c(2,3)
  line_types_seg <- c(4)
  line_wt <- 3
  # additional lines type, weight and colour
  line_type_add <- 4
  line_wt_add <- 1
  line_col_add <- "black"

  # set plot layout based on 'choose'
  if(is.null(choose)) {
    choose <- 1:2
    mfrow = c(2,1)
  } else {
    mfrow = c(1,1)
  }
  if(any(choose > 2))
    stop("plot.oxy_crit: 'choose' input should be 1 to 2 or 'NULL' for both.")

  ## general plot settings
  par(mfrow = mfrow,
      oma = oma_def,
      mai = mai_def_top_ext,
      mgp = mgp_def,
      tck = tck_def,
      las = las_def,
      pch = pch_def,
      ps = 10,
      cex = 1,
      cex.main = 1)
  par(...)

  # Plot 1 - Timeseries -----------------------------------------------------
  if(1 %in% choose) {
    if (!x$convert) {
      ylim <- grDevices::extendrange(
        r = range(x$df_rate_oxygen$rate, na.rm = TRUE), f = 0.05) ## add a little more space
      if(rate.rev) ylim <- rev(ylim) ## reverse y-axis
      plot(x$dataframe, col = c1, xlab = "", ylab = "", cex = ptcex,
           panel.first = grid(lwd = .7), ylim=ylim)
      mtext("Rate~Oxygen Timeseries",
            outer = TRUE, cex = 1.2, line = 0, font = 2)
    } else {
      plot(x$dataframe, col = c1, xlab = "", ylab = "", cex = ptcex,
           panel.first = grid(lwd = .7))
      if(x$method == "bsr")
        abline(h = bsr.intercept, col = line_cols_bsr[1], lwd = line_wt, lty = line_types_bsr[1])
      if(x$method == "bsr")
        abline(h = bsr.midpoint, col = line_cols_bsr[2], lwd = line_wt, lty = line_types_bsr[2])
      if(x$method == "segmented")
        abline(h = seg.breakpoint, col = line_cols_seg, lwd = line_wt, lty = line_types_seg)

      # Legend
      if(x$method == "bsr"){
        leg <- c(glue::glue("Intercept (BSR): ", signif(bsr.intercept, 4)),
                 glue::glue("Midpoint (BSR): ", signif(bsr.midpoint, 4)))
        line_cols <- line_cols_bsr
        line_types <- line_types_bsr
      }

      if(x$method == "segmented"){
        leg <- c(glue::glue("Breakpoint (Seg): ", signif(seg.breakpoint, 4)))
        line_cols <- line_cols_seg
        line_types <- line_types_seg
      }

      if(legend) legend("topright",
                        leg,
                        col = line_cols,
                        lty = line_types,
                        lwd = line_wt,
                        xjust = 1,
                        yjust = 1,
                        bty = "n",
                        horiz = F,
                        cex = legcex,
                        y.intersp = leg.y.intersp)

      mtext("Oxygen~Time Timeseries",
            outer = TRUE, cex = 1.2, line = 0, font = 2)
    }
  }


  # Plot 2 - Broken Stick ---------------------------------------------------
  if(x$method == "bsr" && 2 %in% choose) {
    ylim <- grDevices::extendrange(
      r = range(x$df_rate_oxygen$rate, na.rm = TRUE), f = 0.05) ## add a little more space
    if(rate.rev) ylim <- rev(ylim) ## reverse y-axis
    plot(x$df_rate_oxygen, col = c1, xlab = "", ylab = "", cex = ptcex,
         panel.first = grid(lwd = .7), ylim = ylim)
    abline(lm(rate ~ oxygen, segment1), lwd = line_wt_add, lty = line_type_add, col = line_col_add)
    abline(lm(rate ~ oxygen, segment2), lwd = line_wt_add, lty = line_type_add, col = line_col_add)
    abline(v = bsr.intercept, col = line_cols_bsr[1], lwd = line_wt, lty = line_types_bsr[1])
    abline(v = bsr.midpoint, col = line_cols_bsr[2], lwd = line_wt, lty = line_types_bsr[2])
    if(legend) legend("bottomright",
                      c(glue::glue("Intercept (BSR): ", signif(bsr.intercept, 4)),
                        glue::glue("Midpoint (BSR): ", signif(bsr.midpoint, 4))),
                      col = line_cols_bsr,
                      lty = line_types_bsr,
                      lwd = line_wt,
                      #bg = "gray90",
                      xjust = 1, yjust = 1,
                      bty = "n", horiz = F,
                      cex = legcex,
                      y.intersp = leg.y.intersp)
    title(main = "Broken-Stick Result (Rate~Oxygen)", line = 0.5)
  }

  # Plot 2 - Segmented ------------------------------------------------------
  if(x$method == "segmented" && 2 %in% choose) {
    ylim <- grDevices::extendrange(
      r = range(x$df_rate_oxygen$rate, na.rm = TRUE), f = 0.05) ## add a little more space
    if(rate.rev) ylim <- rev(ylim) ## reverse y-axis
    plot(x$df_rate_oxygen, col = c1, xlab = "", ylab = "", lwd = 2, cex = ptcex,
         panel.first = grid(lwd = .7), ylim=ylim)
    # subsample fit model otherwise dahsed line type is too dense to see
    if (nrow(x$results$seg) > 1000)
      fitsub <- subsample(x$results$seg, length.out = 1000, plot = F)
    else fitsub <- x$results$seg

    lines(fitsub, type = "l", lwd = line_wt_add, lty = line_type_add, col = line_col_add)
    abline(v = seg.breakpoint, col = line_cols_seg, lwd = line_wt, lty = line_types_seg)
    if(legend) legend("bottomright",
                      glue::glue("Breakpoint (Seg): ", signif(seg.breakpoint, 4)),
                      col = line_cols_seg,
                      lty = line_types_seg,
                      lwd = line_wt,
                      #bg = "gray90",
                      xjust = 1, yjust = 1,
                      bty = "n", horiz = F,
                      cex = legcex,
                      y.intersp = leg.y.intersp)
    title(main = "Segmented Result (Rate~Oxygen)", line = 0.5)
  }

  return(invisible(x))
}
