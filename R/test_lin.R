#' Perform `auto_rate()` iteratively and extract performance metrics
#'
#' Randomly generate a dataset and runs `auto_rate()` on the data to detect
#' linear regions (with `method = "linear"`). The function plots 4 exploratory
#' graphs and outputs the results of a linear regression between detected rate
#' and true (known) rate, which can demonstrate how much the function is able to
#' predict true rate.
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
#' @return An object of class `test_lin`. Contains linear regression results, and
#'   data required to plot diagnostics.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' # run 3 iterations (please run at least 1000 times for more reliable visuals)
#' x <- test_lin(reps = 3)
#' # plot(x)
#' # plot(x, "a")  # view only plot "A"
#' # plot(x, "d")  # view only plot "D". You know what to do (for other plots).

test_lin <- function(reps = 1, len = 300, sd = .05, type = "default",
                     preview = FALSE, plot = FALSE) {

  # define the procedure to repeat
  run_once <- function(preview = FALSE, plot = TRUE, ...) {
    # generate data
    dt <- sim_data(preview = preview, ...)
    # extract segment info from data
    coef_sim <- dt$coef # the slope of the segment (calculated post-noise)
    df <- dt$df # the simulated dataset as a data frame object
    len_main <- dt$len_main # length of the main segment
    seg_index <- dt$seg_index # position index of main segment, vectorised

    # run auto_rate
    autorate <- auto_rate(df, plot = plot)
    coef_meas <- autorate$rate[1]  # extract the rate determined by auto_rate
    sttrow <- autorate$summary[1]$row # grab start row
    endrow <- autorate$summary[1]$endrow # grab end row
    seg_detec <- seq.int(sttrow, endrow) # index df from start to end row

    # determine length of detected segment within true segment
    ins <- length(which(seg_detec %in% seg_index))

    # determine length of samples which are NOT in the true segment
    outs <- length(which(!seg_detec %in% seg_index))

    # # proportion identified (correct + incorrect):
    # identified <- (ins + outs)/len_main

    # results <- data.frame(sim = coef_sim, meas = coef_meas) # save
    results <- c(
      coef_sim,  # rate, simulated
      coef_meas, # rate, measured by auto_rate()
      len_main,            # length of true segment
      outs,                # length of incorrectly detected segment(s)
      ins)                 # length of correctly detected segment
    # rm(coef_meas) # is this needed for apply? check
    out <- list(
      df = df,
      results = results
    )
    return(out)
  }

  # now repeat this [reps] times!
  runs <- replicate(
    reps,
    run_once(
      len = len, sd = sd, type = type, preview = preview,
      plot = plot
    )$results
  )
  # convert to data frame, then rename
  df <- data.frame(t(runs))
  names(df) <- c("real",
                 "measured",
                 "length_line",
                 "length_incorrect",
                 "length_detected")
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


#' Plot output of test_lin
#'
#' This is an internal generic. Produces 4 plots.
#' @param x test_lin object
#' @param show string. Which result to show.
#' @param ... Pass additional plotting parameters
#' @return A plot. No returned value.
#' @export
#' @keywords internal
plot.test_lin <- function(x, show = c("all", "a", "b", "c", "d"), ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  df <- x$df
  # bw <- "nrd0" # "nrd" "SJ-ste"
  c1 <- adjustcolor("peru", alpha.f = .4)

  # proportion of true linear region that was correctly sampled (detected):
  d1 <- density(df$length_detected / df$length_line)

  # proportion of the detected segment that contains incorrect data:
  incorrect <- x$df$length_incorrect / x$df$length_line
  incorrect[is.infinite(incorrect)] <- 0
  d2 <- density(incorrect)

  # Linear regression results a calculated from `sim_data()`:
  ls <- x$results

  options(scipen = 5) # adjust threshold for scientific notation
  parorig <- par(no.readonly = TRUE) # save original par settings
  if (any(show %in% "all")) {
    # mat <- matrix(c(1,2,5,3,4,5), nrow = 2, byrow = TRUE)
    # pardefault <- par(no.readonly = T)  # save original par settings
    # layout(mat)
    # par(mai=c(0.6,0.6,0.3,0.2), ps = 10, cex = 1, cex.main = 1)
    parorig <- par(no.readonly = TRUE) # save original par settings
    par(mfrow = c(2,2), mai = c(.5,.5,.3,.3), ps = 10, cex = 1, cex.main = 1)
  }

  # plot A: length of subset as a proportion of full linear region
  if (any(show %in% c("all", "a"))) {
    plot(d1, main = "", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         # xlim = c(min(d1$x), max(d1$x)),
         panel.first = grid())
    axis(2, mgp=c(3, .5, 0))
    axis(1, mgp=c(3, .5, 0))
    polygon(d1, col = c1, border = c1)
    abline(v = 1, lty = 2)
    # abline(v = d1df[d1df$y == max(d1df$y),]$x)
    title(
      xlab = "Proportion",
      # xlab = expression(Delta*n*"%"),
      ylab = "Density", line = 1.5)
    title(main = "A", cex.main = 1.8, adj = 0)
    title(main = "Proportion of lin. seg. correctly identified",
          cex.main = .8)
  }

  # plot B: density of incorrectly sampled data as a proportion of the data
  if (any(show %in% c("all", "b"))) {
    plot(d2, main = "", xlab = "", ylab = "", xaxt = "n", yaxt = "n",
         xlim = c(min(d2$x),
           if (max(d2$x > 3)) 3 else max(d2$x)
           ),
         panel.first = grid())
    axis(2, mgp=c(3, .5, 0))
    axis(1, mgp=c(3, .5, 0))
    polygon(d2, col = c1, border = c1)
    abline(v = 0, lty = 2)
    title(
      # xlab = expression("Proportion"~(beta[true]*"-"*beta[detected])*"/"*beta[true]),
      xlab = "Proportion",
      ylab = "Density", line = 1.5
    )
    title(main = "B", cex.main = 1.8, adj = 0)
    title(main = "Proportion of incorrectly sampled data", cex.main = .8)
  }



  # plot C: true rate (x) against detected rate (y), with linear fit
  if (any(show %in% c("all", "c"))) {
    plot(
      signif(df$real,3), signif(df$measured,3), main = "", xlab = "", ylab = "",
      xaxt = "n", yaxt = "n",
      pch = pch_def,
      cex = .6,
      bg = c1,
      col = c1
    )
    axis(2, mgp=c(3, .5, 0))
    axis(1, mgp=c(3, .5, 0))
    abline(ls, lty = 2)
    title(
      xlab = expression("Rate ("~beta[true]*")"),
      ylab = expression("Rate ("~beta[detected]*")"), line = 1.5
    )
    title(main = "C", cex.main = 1.8, adj = 0)
    title(main = "True v. detected", cex.main = .8)
  }

  # plot D: difference of detected rate from true rate (like a residual plot)
  if (any(show %in% c("all", "d"))) {
    plot(
      df$real, (df$real - df$measured), main = "", xlab = "", ylab = "",
      xlim = c(max(abs(df$real)), -max(abs(df$real))),
      ylim = c(max(abs((df$real-df$measured))),-max(abs((df$real-df$measured)))),
      xaxt = "n", yaxt = "n",
      pch = pch_def, bg = c1, col = c1, cex = .6
    )
    axis(2, mgp=c(3, .5, 0))
    axis(1, mgp=c(3, .5, 0))
    abline(h = 0, lty = 2)
    if(length(df$real) > 5)
      lines(
      suppressWarnings(loess.smooth(df$real, (df$real - df$measured))),
      col = "black", lwd = 1.5
    )
    title(
      xlab = expression("Rate ("~beta[true]*")"),
      ylab = expression("d"
      ~(beta[true]*" -"~beta[detected])), line = 1.5
    )
    title(main = "D", cex.main = 1.8, adj = 0)
    title(main = "Deviation from true rate", cex.main = .8)
  }

  if (any(show %in% "all"))   on.exit(par(parorig)) # revert par settings to original
  return(invisible(x))
}
