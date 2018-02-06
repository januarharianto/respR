#' Perform `auto_rate()` iteratively and extract performance metrics
#'
#' Randomly generate a dataset and runs `auto_rate()` on the data to detect
#' linear regions (with `method = "linear"`). This is an internal function not
#' meant for public use. The function plots 4 exploratory graphs and outputs the
#' results of a linear regression between detected rate and true (known) rate,
#' which can demonstrate how much the function is able to predict true rate.
#'
#' @param reps numeric. Number of times to iterate `auto_rate()` on a randomly
#'   generated dataset. Defaults to 1.
#' @param len numeric. Length (number of observations) of the dataset to test
#'   `auto_rate()` on. Defaults to 300.
#' @param sd numeric. Noise to add to the data. Defaults to .05 standard
#'   difference.
#' @param type character. Use "default", "corrupted" or "segmented" to pick one
#'   of the three different kinds of data to generate.
#' @param preview logical. This will show the randomly-generated data in your
#'   plot window at every iteration. **Note: will slow the function down.**
#'   Useful to see the shape of the data. Defaults to FALSE.
#' @param plot logical. This will show the diagnostic plots of `auto_rate()` at
#'   every iteration. **Note: will severely slow the function down.** Useful to
#'   visualise what's being detected at every step. Defaults to FALSE.
#'
#' @return An object of class `test_lin`. Contains linear regressin results, and
#'   data required to plot diagnostics.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' # run using default values:
#' test_lin(plot = TRUE)
#'
#' # run 5 iterations (please run at least 1000 times for more reliable visuals)
#' x <- test_lin(reps = 5)
#' plot(x)
#' plot(x, "a")  # view only plot "A"
#' plot(x, "d")  # view only plot "D". You know what to do (for other plots)..
#'
#' # run using randomly-generated "corrupted" datasets
#' x <- test_lin(reps = 5, type = "corrupted")
#' plot(x)
test_lin <- function(reps = 1, len = 300, sd = .05, type = "default",
                     preview = FALSE, plot = FALSE) {
  # define the procedure to repeat
  run_once <- function(preview = FALSE, plot = TRUE, ...) {
    # generate data
    gen <- sim_data(preview = preview, ...)
    # extract data
    coef_sim <- gen$coef
    df <- gen$df
    len_main <- gen$len_main
    # run auto_rate
    autorate <- auto_rate(df, plot = plot)
    coef_meas <- autorate$rate[1]
    len_detect <- autorate$summary$rowlength[1]

    # results <- data.frame(sim = coef_sim, meas = coef_meas) # save
    results <- c(coef_sim, coef_meas, len_main, len_detect)
    # rm(coef_meas) # is this needed for apply? check
    out <- list(
      df = df,
      results = results
    )
    return(out)
  }

  # now repeat the procedure
  runs <- replicate(
    reps,
    run_once(
      len = len, sd = sd, type = type, preview = preview,
      plot = plot
    )$results
  )
  # convert to data frame, then rename
  df <- data.frame(t(runs))
  names(df) <- c("real", "measured", "length_line", "length_detected")
  # test accuracy of the data with linear regression
  test <- lm(real ~ measured, df)
  # export
  out <- list(
    df = df,
    results = test
  )
  class(out) <- "test_lin"
  return(out)
}


#' Plot output of `test_lin()`
#'
#' This is an internal generic. Produces 4 plots.
#'
#' @export
#' @keywords internal
plot.test_lin <- function(x, show = c("all", "a", "b", "c", "d"), ...) {
  df <- x$df
  bw <- "nrd0" # "nrd" "SJ-ste"
  c1 <- adjustcolor("peru", alpha.f = .4)

  # Calculate density based on length of subset detected as a percentage of
  # known linear region. Note that it does not represent proportion of linear
  # section detected (since it may detect other regions). The plot from this is
  # most useful to see if over-detection occurs, since under-detecting the
  # linear region is fine since the rate should be the same.
  d1 <- density(df$length_detected / df$length_line * 100, bw = bw)

  # Calculate the percentage difference of the detected rate from the true
  # (known) rate. The plot from this will show how spread out the detected rate
  # is from the true rate.
  d2 <- density((df$real - df$measured)/df$real * 100, bw = bw)

  # Linear regression results a calculated from `sim_data()`:
  ls <- x$results

  # Calculate the percentage of measured values that are within 5 percent of the
  # true rate.
  devp <- data.table::data.table(x = (x$df$real - x$df$measured)/x$df$real*100)
  dev5 <- nrow(devp[x <= 2.5][x >= -2.5])/nrow(devp)

  options(scipen = 5) # adjust threshold for scientific notation
  pardefault <- par(no.readonly = T) # save original par settings
  if (any(show %in% "all")) {
    mat <- matrix(c(1,2,5,3,4,5), nrow = 2, byrow = TRUE)
    pardefault <- par(no.readonly = T)  # save original par settings
    layout(mat)
    par(mai=c(0.7,0.7,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
  }

  # plot A: length of subset as a proportion of full linear region
  if (any(show %in% c("all", "a"))) {
    plot(d1, main = "", xlab = "", ylab = "")
    polygon(d1, col = c1)
    abline(v = 100, lty = 2)
    # abline(v = d1df[d1df$y == max(d1df$y),]$x)
    title(xlab = expression(Delta*n*"%"), ylab = "Density", line = 2)
    title(main = "A", cex.main = 1.8, adj = 0)
    # title(main = "% segment")
  }

  # plot B: distribution density of detected rate around the true rate
  if (any(show %in% c("all", "b"))) {
    plot(d2, main = "", xlab = "", ylab = "", xlim = c(-50,50))
    polygon(d2, col = c1)
    abline(v = 0, lty = 2)
    title(
      xlab = expression("% difference"~(beta[true]*","~beta[detected])),
      ylab = "Density", line = 2
    )
    title(main = "B", cex.main = 1.8, adj = 0)
    # title(main = "% difference")
  }

  # plot D: true rate (x) against detected rate (y), with linear fit
  if (any(show %in% c("all", "c"))) {
    plot(
      df$real, df$measured, main = "", xlab = "", ylab = "", pch = 21,
      bg = c1, col = c1
    )
    abline(ls, lty = 2)
    title(
      xlab = expression("Rate ("~beta[true]*")"),
      ylab = expression("Rate ("~beta[detected]*")"), line = 2
    )
    title(main = "D", cex.main = 1.8, adj = 0)
    # title(main = "True v. detected")
  }

  # plot E: difference of detected rate from true rate
  if (any(show %in% c("all", "d"))) {
    plot(
      df$real, (df$real - df$measured), main = "", xlab = "", ylab = "",
      pch = 21, bg = c1, col = c1, cex = .8
    )
    abline(h = 0, lty = 2)
    lines(
      suppressWarnings(loess.smooth(df$real, (df$real - df$measured))),
      col = "black", lwd = 1.5
    )
    title(
      xlab = expression("Rate ("~beta[true]*")"),
      ylab = expression("d"
      ~(beta[true]*","~beta[detected])), line = 2
    )
    title(main = "E", cex.main = 1.8, adj = 0)
    # title(main = "Difference v. true")
  }

  # plot C: proportional difference of detected rate from true rate
  if (any(show %in% c("all", "d"))) {
    plot(
      df$real, ((df$real - df$measured)/df$real*100), main = "", xlab = "",
      ylab = "", pch = 21, bg = c1, col = c1, cex = .8, ylim = c(-25,25)
    )
    abline(h = 2.5, lty = 3)
    abline(h = -2.5, lty = 3)
    text(min(x$df$real)*0.7, 4, dev5, cex = 1)
    title(
      xlab = expression("Rate ("~beta[true]*")"),
      ylab = expression("% d"
        ~(beta[true]*","~beta[detected])), line = 2
    )
    title(main = "C", cex.main = 1.8, adj = 0)
    # title(main = "Proportion")
  }

  par(pardefault) # revert par settings to original
  return(invisible(x))
}
