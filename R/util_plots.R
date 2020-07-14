# This file contains functions used to plot all the figures in the package. We
# have grouped all of these together so that we can edit the functions easily.
# They are not to be used on their own, and will not be exported for use in the
# package.

# We have also deliberately decided to use base graphics, as ggplot2 was too
# slow when plotting large datasets. Unless ggplot2 improves, we will probably
# not switch to it.

# define colours
r1 <- adjustcolor("black", alpha.f = .9)  # primary colour
r2 <- adjustcolor("goldenrod1", alpha.f = 1)  # secondary colour
r3 <- adjustcolor("darkgrey", alpha.f = .2)  # plot background colour
d1 <- adjustcolor("tomato", alpha.f = .75)
d2 <- adjustcolor("darkslateblue", alpha.f = .75)

# panel <- c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
#   par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5))
pch <- 21
cex <- .5

multi.p <- function(df, sdf, rsq, title = TRUE, xl = '', yl = '') {
  names(df) <- c("x", "y")
  if (!is.null(nrow(sdf)))
    sdf <- list(sdf)
  plot(df, xlab = xl, ylab = yl, bg = r1, col = r1, pch = pch, cex = .3,
       panel.first = grid(lwd = .7))
  invisible(lapply(sdf, function(x) points(x, pch = pch, bg = r2, col = r2,
                                           cex = cex)))
  invisible(lapply(sdf, function(z) {
    names(z) <- c("x", "y")  # rename columns, in case they're not x and y
    ## This fails and breaks return if the z data happens to contain an NA
    ## Rare, but i have seen it happen...
    clip(min(na.omit(z$x)),
         max(na.omit(z$x)),
         min(na.omit(z$y)),
         max(na.omit(z$y)))
    abline(lm(y ~ x, z), lwd = 1.2, lty = 3)
  }))
  if (title == T)
    title(main = expression("Full Timeseries"), line = 0.5)
  # title(main = paste0("r2 = ", signif(rsq, 3)), line = -1.5, font.main = 1)
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1, rsq, title = T) {
  if (is.null(nrow(sdf)))
    sdf <- sdf[[rep]]
  names(sdf) <- c("x", "y")
  fit <- lm(y ~ x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1], ifelse(sign(cf[2]) == 1, " + ", " - "),
               abs(cf[2]), " x ")
  # plot the graph
  plot(sdf, xlab = "", ylab = "", pch = pch, bg = r2, col = r2, cex = cex,
       panel.first = grid(lwd = .7))
  abline(fit, lwd = 1.5, lty = 2)
  if (title == T) title(main = expression("Close-up Region"), line = 0.5)
  title(main = eq, line = -1.5, font.main = 1)
  if (!is.null(rsq)) title(main = paste0("r2 = ", rsq), line = -2.5, font.main = 1)
}

# a plot of residuals
residual.p <- function(fit) {
  plot(fit$fitted.values, fit$residuals, xlab = "", ylab = "", bg = r2,
       col = r2, ylim = c(max(fit$residuals), -max(fit$residuals)),
       pch = pch, cex = cex, panel.first = grid(lwd = .7))
  lines(suppressWarnings(loess.smooth(fit$fitted.values, fit$residuals)),
        col = "black", lwd = 2)
  title(main = expression("Std. Residuals vs Fitted Values"), line = 0.5)
  abline(0, 0, lty = 3, lwd = 1.5)
}

# a q-q plot
qq.p <- function(fit) {
  qqnorm(rstandard(fit), main = "", xlab = "", ylab = "", bg = r2, col = r2,
         pch = pch, cex = cex, panel.first = grid(lwd = .7))
  title(main = expression("Theoretical Q. vs Std. Residuals"), line = 0.5)
  qqline(rstandard(fit), lty = 3, lwd = 1.5)
}

# kernel density plot
density.p <- function(dens, peaks, rank = 1) {
  plot(dens, main = "", xlab = "", ylab = "", panel.first = grid(lwd = .7))
  polygon(dens, col = r2, border = r2)
  title(main = expression("Density of Rolling"~beta[1]), line = 0.5)
  abline(v = peaks[rank, ][1][,1], lty = 2)  # indicate position on density plot
}

# rolling regression
rollreg.p <- function(rolldf, ranked.b1) {
  plot(rolldf, xlab = "Time", ylab = "Rate", bg = r2, col = r2,
       pch = pch,
       lwd = 1, cex = cex, panel.first = grid(lwd = .7))
  # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = r3)
  abline(h = ranked.b1, lty = 2)
  title(main = expression("Rolling Regression"~beta[1]), line = 0.5)
}

# Unused, but maybe for next time?
# # pcrit plot
# pcrit.p <- function(x, rank = 1) {
#   data <- x$do.mr  # main plot dataset
#   ref <- x$pcritRanked[1]  # reference list to create subsets
#   set1 <- dplyr::filter(data, do <= as.numeric(ref[rank, ]))
#   lm1 <- lm(set1[[2]] ~ set1[[1]], set1)
#   set2 <- dplyr::filter(data, do > as.numeric(ref[rank, ]))
#   lm2 <- lm(set2[[2]] ~ set2[[1]], set2)
#   pc.intercept <- x$pcritRanked[5][rank, ]
#   pc.midpoint <- x$pcritRanked[6][rank, ]
#   # plot:
#   plot(data, col = r2, pch = 16, xlab = "DO", ylab = "MR",
#     lwd = 2, xlim = c(min(data[[1]]), max(data[[1]], pc.intercept, pc.midpoint)),
#     ylim = c(min(data[[2]]), ((max(data[[2]]) - min(data[[2]])) * 1.2 + min(data[[2]]))),
#     panel.first = c(rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
#       col = r3), grid(col = "white", lty = 1, lwd = 1.5)))
#   abline(lm1, lwd = 1.5, lty = 4)
#   abline(lm2, lwd = 1.5, lty = 4)
#   abline(v = pc.intercept, col = d1, lwd = 3)
#   # text(pc.intercept, min(data[2]), signif(pc.intercept, 3))
#   abline(v = pc.midpoint, col = d2, lwd = 3)
#   # text(pc.midpoint, max(data[2]), signif(pc.midpoint, 3))
#   # title(main = "Pcrit")
#   legend("topright", c("Intercept", "Mid-point"), col = c(d1, d2), lty = 1, lwd = 3,
#     bty = "n")
# }



#' Plots multiple auto_rate results in a nice way
#' x = auto_rate or auto_rate_filt object
#' n = max no. of plots
#' THIS IS SLOOOOOOOW
#' Will probably have to revert to base plotting
#'
#' @importFrom cowplot plot_grid
#' @importFrom methods show
#' @import ggplot2
#'
#' @keywords internal

plot_multi_ar <- function(x, n = 9){
  parorig <- par(no.readonly = TRUE) # save original par settings

  nres <- length(x$rate) ## no. of results
  df <- x$dataframe
  summ <- as.data.frame(x$summary)
  # summary table rank column for creating title
  # this replaces $density if linear auto_rate method
  # but we don't use it here anyway.
  summ[[8]] <- 1:nrow(summ)

  if(nres == 0) {
    message("subset_rate: No rates to plot...")
    return()}
  if(nres > n) message(glue::glue("subset_rate: Plotting first {n} of {nres} subset rate results only..."))
  if(nres < n) n <- nres

  ## save all ggplot2 plots to list
  all_plots <- apply(summ[1:n,], 1, function(q) {

    start <- q[1]
    end <- q[2]
    rate <- q[6]
    rank <- q[8]

    rdf <- df[start:end]
    slope <- q[6]
    intercept <- q[5]

    plt <-
      ggplot() +
      theme(plot.title = element_text(hjust = 1, family = "mono", size = 10)) +
      theme(plot.margin = margin(0.1, 0.1, 0, 0, "cm")) +
      geom_point(aes(x = df$x,
                     y = df$y),
                 color="darkgrey",) +
      labs(x="", y="") +

      geom_point(aes(x = rdf$x, y = rdf$y),
                 color = "yellow1") +
      stat_smooth(method = "lm") +
      ggtitle(glue::glue("Rank {rank} of {nres}\nRate = {signif(rate, digits = 3)}"))

    ## turns out clipping an lm in ggplot is a PITA...
    plt <- plt + geom_segment(
      aes(x = rdf$x[1], y = rdf$x[1]*slope+intercept,
          xend = tail(rdf$x, 1), yend = tail(rdf$x, 1)*slope+intercept, ),
      linetype = "dashed",
      color = "black",
      lwd = 0.8)

    return(plt)
  })

  grd <- cowplot::plot_grid(plotlist = all_plots)
  show(grd)
  on.exit(suppressWarnings(par(parorig))) # revert par settings to original
}


#' Plots auto_rate regressions in a way you can see how they
#' overlap. If it's an auto_rate_subset object, it will plot
#' the subset on the axes of the original results, so you can compare
#' the subset and original easily.
#' You can also specify an auto_rate $summary df directly, but then the axes
#' limits will be drawn from the summary df not the original data, so may be
#' somewhat arbitrary.
#' Possibly we incorporate this into a user exploration function
#' @keywords internal
plot_overlaps <- function(x){

  if(is.data.frame(x)) df <- x else
    df <- x$summary

  if(is.data.frame(x))maxy <- nrow(x) else
    if(("original" %in% names(x))) maxy <- nrow(x$original$summary) else
      maxy <- nrow(x$summary)

    miny <- 0
    minx <- 0
    if(is.data.frame(x))maxx <- max(x$endrow) else
      maxx <- nrow(x$dataframe)

    if(("original" %in% names(x))) {
      o_df <- x$original$summary

      colsToUse <- intersect(colnames(df), colnames(o_df))
      o_rows <- match(do.call("paste", as.data.frame(df)[, colsToUse]),
                      do.call("paste", as.data.frame(o_df)[, colsToUse]))

      o_df$row_no <- 1:nrow(o_df)
    }

    df$row_no <- 1:nrow(df)

    plot(minx:maxx, seq(miny, maxy, length.out=length(minx:maxx)),
         ylim = c(maxy,miny),
         col = "white",
         ylab="$summary table ranking",
         xlab="orginal data rows")

    if(("original" %in% names(x))) {for(i in 1:nrow(df)) {
      segments(df$row[i], o_rows[i], x1 = df$endrow[i], y1 = o_rows[i],
               lwd=3, col = "blue")}
    } else {for(i in 1:nrow(df)) {
      segments(df$row[i], df$row_no[i], x1 = df$endrow[i], y1 = df$row_no[i],
               lwd=3, col = "blue")}}

    invisible(return(x)) ## to allow it to be used within pipes - still prints though...
}

