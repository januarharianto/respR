# This file contains functions used to plot all the figures in the package. We
# have grouped all of these together so that we can edit the functions easily.
# They are not to be used on their own, and will not be exported for use in the
# package.

# We have also deliberately decided to use base graphics, as ggplot2 was too
# slow when plotting large datasets. Unless ggplot2 improves, we will probably
# not switch to it.


# Plotting defaults -------------------------------------------------------

# define colours
r1 <- adjustcolor("black", alpha.f = .9)  # primary colour
r2 <- adjustcolor("goldenrod1", alpha.f = 1)  # secondary colour
r3 <- adjustcolor("darkgrey", alpha.f = .2)  # plot background colour
d1 <- adjustcolor("tomato", alpha.f = .75)
d2 <- adjustcolor("darkslateblue", alpha.f = .75)
# colours for flowthrough plots
ftcol_in <- adjustcolor("turquoise", alpha.f = 1) # inflow oxy
ftcol_out <- adjustcolor("lightgreen", alpha.f = 1) # outflow oxy
ftcol_del <- adjustcolor("black", alpha.f = 0.9) # delta oxy
ftcol_rate_pt <- adjustcolor("goldenrod1", alpha.f = 1) # highlighted rate points
ftcol_rate_bx <- adjustcolor("goldenrod1",  alpha.f = 0.2) # highlighted rate region box bg
ftcol_rate_ln <- adjustcolor("goldenrod1",  alpha.f = 0.25) # highlighted rate region line

# panel <- c(rect(par("usr")[1], par("usr")[3], par("usr")[2],
#   par("usr")[4], col = r3), grid(col = "white", lty = 1, lwd = 1.5))

# default plotting character
pch_def <- 16

# not used very much....
cex_def <- 0.5

# default tick size
tck_def <- -0.015 # size of axis ticks

# default axis distances
# 1 = distance of axis label from axis
# 2 = distance of tick labels from ticks
# 3 = distance of ticks from axis
mgp_def <- c(0, 0.2, 0)

# default margins
# outer
oma_def <- c(0.4, 1, 1.5, 0.4)
# inner
mai_def <- c(0.3, 0.15, 0.35, 0.15)
# To give slightly more space for plots without two titles
# currently applied in:
#   - inspect
#   - inspect.ft
#   - calc_rate.ft
mai_def_top_ext <- c(0.3, 0.15, 0.2, 0.15)


# default axis rotation
las_def <- 0


# Plotting functions ------------------------------------------------------

# axes = which axes to draw
# mgp_bt etc = separate mgp for axes if needed
multi.p <- function(df, sdf, rsq, title = "Full Timeseries", xl = '', yl = '',
                    axes = c(1,2,3), legend = TRUE,
                    bt_mgp = NULL,
                    lf_mgp = NULL,
                    tp_mgp = NULL,
                    rt_mgp = NULL,
                    ...) {

  par(...)
  names(df) <- c("x", "y")
  if (!is.null(nrow(sdf)))
    sdf <- list(sdf)
  plot(df, xlab = xl, ylab = yl, bg = r1, col = r1, cex = .3,
       panel.first = grid(lwd = .7),
       axes = FALSE)
  invisible(lapply(sdf, function(x) points(x, bg = r2, col = r2)))
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

  if(1 %in% axes) axis(side = 1, col.axis = "blue", mgp = bt_mgp)
  if(2 %in% axes) axis(side = 2, col.axis = "black", mgp = lf_mgp)

  box()

  if(legend && 1 %in% axes) legend("bottomleft",
                                   "Time",
                                   text.col = "blue",
                                   bg = "gray90",
                                   cex = 0.5)
  if(legend && 3 %in% axes) legend("topright",
                                   "Row",
                                   text.col = "red",
                                   bg = "gray90",
                                   cex = 0.5)
  ## add row index axis
  par(new = TRUE, ...)
  plot(seq(1, nrow(df)),
       df[[2]],
       xlab = "",
       ylab = "",
       pch = "",
       cex = .5,
       axes = FALSE)
  if(3 %in% axes) axis(side = 3, col.axis = "red", mgp = tp_mgp)

  title(main = glue::glue("{title}"), line = 1.2, font = 2)
}

# a plot of the subset only
sub.p <- function(sdf, rep = 1, rsq, rownums, title = TRUE, legend = TRUE, ...) {
  par(...)
  if (is.null(nrow(sdf)))
    sdf <- sdf[[rep]]
  names(sdf) <- c("x", "y")
  fit <- lm(y ~ x, sdf)
  # generate equation to paste into plot
  cf <- signif(coef(fit), 3)
  eq <- paste0("y = ", cf[1], ifelse(sign(cf[2]) == 1, " + ", " - "),
               abs(cf[2]), " x ")
  # plot the graph
  plot(sdf, xlab = "", ylab = "", bg = r2, col = r2,
       panel.first = grid(lwd = .7),
       axes = FALSE)
  abline(fit, lwd = 1.5, lty = 2)

  axis(side = 1, col.axis = "blue")
  axis(side = 2, col.axis = "black")
  box()

  ## add row index axis
  par(new = TRUE, ...)
  plot(rownums,
       sdf[[2]],
       xlab = "",
       ylab = "",
       pch = "",
       cex = .5,
       axes = FALSE)
  axis(side = 3, col.axis = "red")

  if(title) title(main = ("Close-up Region"), line = 1.2, font = 2)
  if(legend) title(main = eq, line = -1.5, font.main = 1)
  if(legend && !is.null(rsq)) title(main = paste0("r2 = ", rsq), line = -2.5, font.main = 1)
}

# a plot of residuals
residual.p <- function(fit, ...) {
  par(...)
  plot(fit$fitted.values, fit$residuals, xlab = "", ylab = "", bg = r2,
       col = r2, ylim = c(max(fit$residuals), -max(fit$residuals)),
       panel.first = grid(lwd = .7))
  ## fails with values 5 or less, so just don't bother drawing it
  if(length(fit$fitted.values) > 5)
    lines(suppressWarnings(loess.smooth(fit$fitted.values, fit$residuals)),
          col = "black", lwd = 2)
  title(main = ("Std. Residuals \nvs Fitted Values"), line = 0.3, font = 2)
  abline(0, 0, lty = 3, lwd = 1.5)
}

# a q-q plot
qq.p <- function(fit, ...) {
  par(...)
  # problems with rstandard if only 2 values, so do this
  if(length(fit$fitted.values) > 2) vals <- rstandard(fit) else
    vals <- fit$fitted.values

  qqnorm(vals, main = "", xlab = "", ylab = "", bg = r2, col = r2,
         panel.first = grid(lwd = .7))
  title(main = ("Theoretical Q. \nvs Std. Residuals"), line = 0.3, font = 2)
  qqline(vals, lty = 3, lwd = 1.5)
}

# kernel density plot
density.p <- function(dens, peaks, rank = 1, ...) {
  par(...)
  plot(dens, main = "", xlab = "", ylab = "", panel.first = grid(lwd = .7))
  polygon(dens, col = r2, border = r2)
  title(main = expression(bold("Density of Rolling"~beta[1])), line = 0.5)
  abline(v = peaks[rank, ][1][,1], lty = 2)  # indicate position on density plot
}

# rolling regression
rollreg.p <- function(rolldf, ranked.b1, rownums, xlim, rate.rev = TRUE, ...) {
  par(...)
  ylim <- grDevices::extendrange(nainf.omit(rolldf[[2]]), f = 0.05)
  if(rate.rev) ylim <- rev(ylim) ## reverse y-axis
  plot(rolldf, xlab = "", ylab = "", bg = r2, col = r2,
       ylim = ylim,
       xlim = xlim,
       lwd = 1, panel.first = grid(lwd = .7),
       axes = FALSE)

  axis(side = 1, col.axis = "blue")
  axis(side = 2, col.axis = "black")
  abline(h = ranked.b1, lty = 2)
  box()

  ## add row index axis
  par(new = TRUE, ...)
  plot(rownums,
       xlab = "",
       ylab = "",
       pch = "",
       cex = .5,
       axes = FALSE)
  axis(side = 3, col.axis = "red")
  title(main = ("Rolling Rate"), line = 1.2, font = 2)
}

# rolling regression 2
# this is a version especially for inspect roll reg plot
# Can't quite remember why i didn't use the above, but there was a reason...
# Maybe just better control over appearance...
rollreg2.p <- function(df, width) {
  roll_width <- floor(width * nrow(df))
  ## Calc all rates, even there is a min_obs of only 1 datapoint
  ## This means rate is returned even if there are NA in data
  ## Also replace Inf with NA of ylim in plot fails
  df[[1]][which(is.infinite(df[[1]]))] <- NA
  df[[2]][which(is.infinite(df[[2]]))] <- NA
  rates <- roll::roll_lm(matrix(df[[1]]), matrix(df[[2]]),
                         roll_width, min_obs = 1)$coefficients[,2]
  ## However this means rates are ALSO calculated at the start of the data
  ## before the width is even reached, so we remove these.
  rates <- rates[-(1:(roll_width-1))]
  rates <- na.omit(rates)
  return(rates)
}

# plots summary tabel as overlaps
# used in convert_rate and convert_rate.ft
over.p <- function(x, pos, legend, highlight){

  ## Extract data
  dt <- data.table::as.data.table(x$dataframe)
  summ <- x$summary

  if(is.null(highlight)) highlight <- 1

  # Axis limits
  ## how many summary rows to plot.
  miny <- 1
  maxy <- nrow(summ)
  minx <- 0
  maxx <- nrow(dt)

  plot(minx:maxx,
       seq(miny, maxy, length.out=length(minx:maxx)),
       ylim = c(maxy,miny),
       col = "white",
       ylab="",
       xlab="",
       axes = FALSE,
       panel.first = grid(lwd = .7))
  box()
  for(i in pos)
    segments(x0 = summ$row[i],
             y0 = i,
             x1 = summ$endrow[i],
             y1 = i,
             lwd = 3,
             col = r1)
  axis(side = 2, col.axis = "black")
  segments(x0 = summ$row[highlight],
           y0 = highlight,
           x1 = summ$endrow[highlight],
           y1 = highlight,
           lwd = 3,
           col = r2)
  mtext("Summary Table Row (Descending)",
        outer = FALSE, cex = 1, line = 0.3, font = 2)
  # invisible plot to get time axis
  par(new=TRUE)
  plot(dt[[1]], dt[[2]], pch = "", xlab = "", ylab = "", axes = FALSE)
  axis(side = 1, col.axis = "blue")
  if(legend) legend("bottomleft",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.5)
}

# Plots multiple auto_rate or convert_rate results in a nice way
# using base plot
# x = auto_rate or convert_rate object
grid.p <- function(x, pos = NULL, msg = "grid.p",
                   title = "Rank", quiet = FALSE,
                   rate.rev = TRUE, ...){

  # Apply default plotting params
  par(oma = c(2, 2, 2, 0.5),
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = 1,
      cex.main = 1,
      ps = 10)
  par(mar = c(0.1, 0.1, 2, 0.1))
  # allows params overriding defaults to be passed
  par(...)

  # total number of results
  totres <- nrow(x$summary)

  # pos messages
  if(!quiet){
    if(length(pos) <= 20 && length(pos) == totres) message(glue::glue("{msg}: Plotting all rate(s)...")) else
      if(length(pos) <= 20) message(glue::glue("{msg}: Plotting rate(s) from selected 'pos' rows...")) else
        message(glue::glue("{msg}: Plotting first 20 selected rates only. To plot others use 'pos' input."))
  }

  plot.sub.grid <- function(x, pos, bt, lf, tp) {
    for(i in pos) {
      dt <- x$dataframe
      rate <- x$rate.output[i]
      rank <- x$summary$rank[i]
      res <- i
      units <- x$summary$output.unit[1]
      start <- x$summary$row[i]
      end <- x$summary$endrow[i]
      sdt <- dt[start:end]
      ax <- c()
      if(i %in% bt) ax <- c(ax, 1)
      if(i %in% lf) ax <- c(ax, 2)
      if(i %in% tp) ax <- c(ax, 3)
      multi.p(dt, sdt, legend = F, title = "", axes = ax,
              tck = -0.005,
              mgp = c(0, 0.2, 0),
              tp_mgp = c(0, 2, 0),
              las = 1)
      #title(glue::glue("tmp title"))
      title(glue::glue("{title} {res} of {totres} Rates:\n{signif(rate, digits = 3)} {units}"),
            cex.main = 0.9)
    }
  }
  plot.sub.grid.delta <- function(x, pos, bt, lf, tp, rate.rev = TRUE, ...) {
    for(i in pos) {
      dt <- data.table::as.data.table(x$dataframe)
      rate <- x$rate.output[i]
      rank <- x$summary$rank[i]
      res <- i
      units <- x$summary$output.unit[1]
      start <- x$summary$row[i]
      end <- x$summary$endrow[i]
      sdt <- dt[start:end]
      ax <- c()
      if(i %in% bt) ax <- c(ax, 1)
      if(i %in% lf) ax <- c(ax, 2)
      if(i %in% tp) ax <- c(ax, 3)
      delta.p(x, delta_only = TRUE, legend = FALSE, pos = i,
              rate.rev = rate.rev,
              title = "",
              axes = ax,
              tck = -0.005,
              mgp = c(0, 0.2, 0),
              tp_mgp = c(0, 2, 0),
              las = 1,
              ...)
      #title(glue::glue("tmp title"))
      title(glue::glue("{title} {res} of {totres} Rates:\n{signif(rate, digits = 3)} {units}"),
            cex.main = 0.9)
    }
  }

  if(length(pos) == 1)          {
    par(mfrow = c(1,1))
    bt <- 1
    lf <- 1
    tp <- 1
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) == 2)          {
    par(mfrow = c(1,2))
    bt <- pos[1:2]
    lf <- pos[1]
    tp <- pos[1:2]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(3,4))   {
    par(mfrow = c(2,2))
    bt <- pos[3:4]
    lf <- pos[c(1,3)]
    tp <- pos[1:2]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(5,6))   {
    par(mfrow = c(2,3))
    bt <- pos[4:6]
    lf <- pos[c(1,4)]
    tp <- pos[1:3]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(7,8,9)) {
    par(mfrow = c(3,3))
    bt <- pos[7:9]
    lf <- pos[c(1,4,7)]
    tp <- pos[1:3]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(10:12)) {
    par(mfrow = c(3,4))
    bt <- pos[9:12]
    lf <- pos[c(1,5,9)]
    tp <- pos[1:4]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(13:16)) {
    par(mfrow = c(4,4))
    bt <- pos[13:16]
    lf <- pos[c(1,5,9,13)]
    tp <- pos[1:4]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  } ## start to get margins too large errors
  if(length(pos) %in% c(17:20)) {
    par(mfrow = c(4,5))
    bt <- pos[16:20]
    lf <- pos[c(1,6,11,16)]
    tp <- pos[1:5]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) > 20){
    pos <- 1:20
    par(mfrow = c(4,5))
    bt <- pos[16:20]
    lf <- pos[c(1,6,11,16)]
    tp <- pos[1:5]
    if(inherits(x, "convert_rate.ft")) plot.sub.grid.delta(x, pos, bt, lf, tp, rate.rev = rate.rev, ...) else
      plot.sub.grid(x, pos, bt, lf, tp)
  }
}



#' Plot convert_rate and auto_rate summary tables
#'
#' Plots `convert_rate` and `auto_rate` summary table regressions in a way that
#' visualises how they are positioned within the data timeseries.
#'
#' @param x `convert_rate`, `convert_rate_select`, or `auto_rate` object
#' @param highlight integer. Which result in the summary table to highlight on
#'   the plots. Defaults to 1. If it is outside the range of the summary rows it
#'   will default to 1.
#' @param legend logical. Suppress plot legends.
#' @param quiet logical. Suppress console output.
#' @param msg string. For adding custom text to start of messages.
#' @param pos integer(s). Choose which summary table rows to plot.
#' @param ... Allows additional plotting controls to be passed.
#'
#' @return A plot of the auto_rate object results
#'
#' @keywords internal
overlap.p <- function(x, highlight = NULL, pos = NULL, legend = TRUE, quiet = FALSE,
                      msg = "overlap.p", ...){

  # for now, not bothering with plotting against original column numbers
  # Very complicated - can no longer use rank column as tracker because of possible reordering

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # Needs to be this way as class(x) might have two classes and %in% check fails others way around
  if(!("convert_rate" %in% class(x) || "auto_rate" %in% class(x)))
    stop(glue::glue("{msg}: 'x' should be an 'auto_rate' or 'convert_rate' object."))
  if("convert_rate" %in% class(x) && inherits(x$inputs$x, "calc_rate.bg"))
    stop(glue::glue("{msg}: Plot is not available for converted 'calc_rate.bg' objects because rates may come from different columns of the dataframe."))
  if("convert_rate" %in% class(x) && is.null(x$dataframe))
    stop(glue::glue("{msg}: Plot is not available for 'convert_rate' objects containing rates converted from numeric values."))

  ## warning if empty - but return to allow piping
  if(length(x$summary$rate) == 0){
    message(glue::glue("{msg}: Nothing to plot! No rates found in object."))
    return(invisible(x))
  }

  if(is.null(pos)) pos <- 1:length(x$summary$rate)

  if(!quiet){
    if(length(pos) == length(x$summary$rate)) message(glue::glue("{msg}: Plotting all rate(s)...")) else
      if(length(pos) < length(x$summary$rate)) message(glue::glue("{msg}: Plotting rate(s) from selected 'pos' rows..."))
  }

  ## set layout
  m <- rbind(c(1,1,1), c(2,2,2), c(2,2,2))
  layout(m)

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = 1,
      cex.main = 1,
      ps = 10)
  # allows params overriding defaults to be passed
  par(...)

  ## Extract data
  dt <- data.table::as.data.table(x$dataframe)
  summ <- x$summary

  # apply default pos
  if(is.null(pos)) pos <- 1:nrow(summ)

  # apply default if pos too high
  if(any(pos > nrow(summ))) {
    message(glue::glue("{msg}: One or more 'pos' inputs higher than number of rows in '$summary'. Applying default of all rows."))
    pos <- 1:nrow(summ)
  }

  # apply default of highlight being the highest rank pos
  if(is.null(highlight)) highlight <- pos[1]

  # If highlight isn't in pos ranks set it to highest rank one
  if(!(highlight %in% pos)) {
    message(glue::glue("{msg}: 'highlight' not within 'pos' input. Applying default of first 'pos' entry."))
    highlight <- pos[1]
  }

  # highlight subset
  start <- summ$row[highlight]
  end <- summ$endrow[highlight]
  sub_dt <- dt[start:end]

  # timeseries --------------------------------------------------------------
  multi.p(dt, sub_dt, axes = c(2,3), legend = legend)
  mtext("Full Timeseries",
        outer = TRUE, cex = 1.2, line = 0, font = 2)

  # Overlap plot ------------------------------------------------------------
  over.p(x, pos, legend, highlight)

  return(invisible(x))
}

#' Plot convert_rate.ft summary tables
#'
#' Plots `convert_rate.ft` summary table regressions in a way that
#' visualises how they are positioned within the data timeseries.
#'
#' @param x `convert_rate.ft`, `convert_rate.ft_select` object
#' @param highlight integer. Which result in the summary table to highlight on
#'   the plots. Defaults to 1. If it is outside the range of the summary rows it
#'   will default to 1.
#' @param legend logical. Suppress plot legends.
#' @param quiet logical. Suppress console output.
#' @param msg string. For adding custom text to start of messages.
#' @param pos integer(s). Choose which summary table rows to plot.
#' @param ... Allows additional plotting controls to be passed.
#'
#' @return A plot of the auto_rate object results
#'
#' @keywords internal
overlap.ft.p <- function(x, highlight = NULL, pos = NULL, legend = TRUE, quiet = FALSE,
                         rate.rev= TRUE, msg = "overlap.ft.p", ...){

  # apply default of highlight being the highest rank pos
  if(is.null(highlight)) highlight <- pos[1]

  # If highlight isn't in pos ranks set it to highest rank one
  if(!(highlight %in% pos)) {
    message(glue::glue("{msg}: 'highlight' not within 'pos' input. Applying default of first 'pos' entry."))
    highlight <- pos[1]
  }

  # extract data
  time <- x$data$time[[1]]
  # extract rates
  rt <- x$rate.output[pos]
  # extract times of rates
  tm <- (x$summary$time[pos] + x$summary$endtime[pos])/2
  # extract units
  un <- x$summary$output.unit[1]

  # messages
  if(!quiet){
    if(length(pos) == length(x$summary$rate)) message(glue::glue("{msg}: Plotting all rate(s)...")) else
      if(length(pos) < length(x$summary$rate)) message(glue::glue("{msg}: Plotting rate(s) from selected 'pos' rows..."))
  }

  # number of rates
  nrt <- length(x$rate.output)
  # delta only plot?
  delta_only <- is.null(x$data$out.oxy) && is.null(x$data$in.oxy)
  #if(!(delta_only)) y_range <- range(in.oxy, out.oxy, na.rm = TRUE) # for plotting rate region rectangle

  ## if only delta oxygen plot, it takes top two thirds,
  ## otherwise 3 plots
  if (delta_only) m <- rbind(c(1,1,1), c(1,1,1), c(2,2,2)) else
    m <- rbind(c(1,1,1), c(2,2,2), c(3,3,3))
  ## set layout
  layout(m)

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = 1,
      cex.main = 1,
      ps = 10)
  par(...)

  # in.oxy - out.oxy plot -----------------------------------------------------
  if(!delta_only)  in.out.p(x, pos = NULL, legend = legend)

  # Delta plot --------------------------------------------------------------
  delta.p(x, delta_only = delta_only, legend = legend, rate.rev = rate.rev, pos = NULL)

  # Overlap plot ------------------------------------------------------------
  over.p(x, pos, legend = FALSE, highlight)

  return(invisible(x))
}

# Plots convert_rate objects
# Very similar to plot.inspect
# top plot is same full timeseries
# But bottom plot is output rate values against middle of their time range
outrate.p <- function(x, pos = NULL, quiet = FALSE, msg = "rate.p",
                      legend = TRUE, rate.rev = TRUE, ...){

  ## apply default pos
  if(is.null(pos)) pos <- 1:length(x$summary$rate)

  # extract data frame
  dt <- as.data.frame(x$dataframe)
  # extract rates
  rt <- x$rate.output[pos]
  # extract times of rates
  tm <- (x$summary$time[pos] + x$summary$endtime[pos]) /2
  # extract units
  un <- x$summary$output.unit[1]

  # messages
  if(!quiet){
    if(length(pos) == length(x$summary$rate)) message(glue::glue("{msg}: Plotting all rate(s)...")) else
      if(length(pos) < length(x$summary$rate)) message(glue::glue("{msg}: Plotting rate(s) from selected 'pos' rows..."))
  }

  # Apply default plotting params
  par(oma = oma_def,
      # this one needs more space at top for two panel plot
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = cex_def)

  # plot timeseries ----------------------------------------------------------

  par(mfrow = c(2, 1),
      ps = 10,
      cex = 1,
      cex.main = 1)
  # allows defaults to be changed
  par(...)

  plot(dt[[1]],
       dt[[2]],
       xlab = "",
       ylab = "",
       ylim = grDevices::extendrange(nainf.omit(dt[[2]]), f = 0.05),
       cex = .5,
       axes = FALSE,
       col.lab = "blue",
       col.axis = "blue",
       panel.first = grid())

  axis(side = 2)

  ## add row index axis
  par(new = TRUE, ...)
  plot(seq(1, nrow(dt)),
       dt[[2]],
       xlab = "",
       ylab = "",
       pch = "",
       cex = .5,
       axes = FALSE)
  axis(side = 3, col.axis = "red")

  box()
  if(legend) legend("topright",
                    "Row Index",
                    text.col = "red",
                    bg = "gray90",
                    cex = 0.7)
  mtext("Full Timeseries",
        outer = TRUE, cex = 1.2, line = 0.3, font = 2)


  # plot rates --------------------------------------------------------------

  xlim <- range(nainf.omit(dt[[1]]))
  y_lim <- grDevices::extendrange(nainf.omit(x$rate.output[pos]), f = 0.05)
  if(rate.rev) y_lim <- rev(y_lim) ## reverse y-axis

  ## dynamically resize point size to between 0.5 and 1 based on how many there are
  int <- lm(c(1,0.5) ~c(1,6000))$coefficients[[1]] # int
  slp <- lm(c(1,0.5) ~c(1,6000))$coefficients[[2]] # slp

  plot(rt ~ tm,
       xlim = xlim,
       ylim = y_lim,
       xlab = "",
       ylab = "",
       cex = length(rt) * slp + int, # dynamic size of points
       col = r2,
       axes = FALSE,)
  axis(side = 2, cex.axis = 0.9)
  # to put yaxis label colour back to black
  axis(side = 1, col.lab = "blue", col.axis = "blue")
  ## Added dashed line at rate = 0 - for when rates are +ve and -ve
  abline(h = 0, lty = 2)
  grid()
  box()
  if(legend) legend("bottomleft",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.7)
  mtext("Output Rate",
        outer = FALSE, cex = 1.2, line = 1.2, font = 2)
  mtext(glue::glue("(units: {un})"),
        outer = FALSE, cex = 1, line = 0.3, font = 2)
}


# Plots convert_rate.ft objects
# Very similar to plot.inspect.ft
# top plot is in-out oxy (if present) and delta
# But bottom plot is output rate values against middle of their time range

outrate.ft.p <- function(x, pos = NULL, quiet = FALSE, msg = "rate.p",
                         legend = TRUE, rate.rev = TRUE, ...){

  # extract data
  time <- x$data$time[[1]]
  # extract rates
  rt <- x$rate.output[pos]
  # extract times of rates
  tm <- (x$summary$time[pos] + x$summary$endtime[pos])/2
  # extract units
  un <- x$summary$output.unit[1]

  # messages
  if(!quiet){
    if(length(pos) == length(x$summary$rate)) message(glue::glue("{msg}: Plotting all rate(s)...")) else
      if(length(pos) < length(x$summary$rate)) message(glue::glue("{msg}: Plotting rate(s) from selected 'pos' rows..."))
  }

  # number of rates
  nrt <- length(x$rate.output)
  # delta only plot?
  delta_only <- is.null(x$data$out.oxy) && is.null(x$data$in.oxy)
  #if(!(delta_only)) y_range <- range(in.oxy, out.oxy, na.rm = TRUE) # for plotting rate region rectangle

  ## if only delta oxygen plot, it takes top two thirds,
  ## otherwise 3 plots
  if (delta_only) m <- rbind(c(1,1,1), c(1,1,1), c(2,2,2)) else
    m <- rbind(c(1,1,1), c(2,2,2), c(3,3,3))
  ## set layout
  layout(m)

  # Apply default plotting params
  par(oma = oma_def,
      mai = mai_def_top_ext,
      las = las_def,
      mgp = mgp_def,
      tck = tck_def,
      pch = pch_def,
      cex = 1,
      cex.main = 1,
      ps = 10)
  par(...)

  # in.oxy - out.oxy plot -----------------------------------------------------
  if(!delta_only)  in.out.p(x,
                                    pos = NULL,
                                    legend = legend)

  # # Delta plot --------------------------------------------------------------
  delta.p(x, delta_only = delta_only, legend = legend, rate.rev = rate.rev, pos = NULL)

  # Rates plot -----------------------------------------------------------

  x_lim <- range(nainf.omit(time))
  y_lim <- grDevices::extendrange(nainf.omit(x$rate.output[pos]), f = 0.05)
  if(rate.rev) y_lim <- rev(y_lim) ## reverse y-axis

  ## dynamically resize point size to between 0.5 and 1 based on how many there are
  int <- lm(c(1,0.5) ~c(1,6000))$coefficients[[1]] # int
  slp <- lm(c(1,0.5) ~c(1,6000))$coefficients[[2]] # slp

  plot(rt ~ tm,
       xlim = x_lim,
       ylim = y_lim,
       xlab = "",
       ylab = "",
       cex = length(rt) * slp + int, # dynamic size of points
       col = r2,
       axes = FALSE,)
  axis(side = 2, cex.axis = 0.9)
  # to put yaxis label colour back to black
  axis(side = 1, col.lab = "blue", col.axis = "blue")
  ## Added dashed line at rate = 0 - for when rates are +ve and -ve
  abline(h = 0, lty = 2)
  grid()
  box()
  if(legend) legend("bottomleft",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.7)
  # mtext("Output Rate",
  #       outer = FALSE, cex = 1.2, line = 1.2, font = 2)
  mtext(glue::glue("Output Rate (units: {un})"),
        outer = FALSE, cex = 1, line = 0.3, font = 2)
}


# Delta plot --------------------------------------------------------------

# Plots delta oxygen in flowthrough data
# Used in calc_rate.ft and convert_rate.ft
# x = calc_rate.ft or convert_rate.ft object
#   (containing the time data generally in unlist(x$data$time), and
#   delta.oxy data generally in x$delta.oxy)
# delta_only = is this being plotted by itself or with a inflow/outflow plot above?
# legend = time/row labels
# pos = which rate to plot
delta.p <- function(x, delta_only = TRUE, legend = TRUE, rate.rev = TRUE, pos = 1,
                    title = "Delta Oxygen",
                    axes = c(1,2,3),
                    bt_mgp = NULL,
                    lf_mgp = NULL,
                    tp_mgp = NULL,
                    rt_mgp = NULL, ...) {

  # Extract data
  time <- x$data$time[[1]]
  delta.oxy <- x$data$delta.oxy[[1]]
  if(inherits(x, "calc_rate.ft")) rate <- x$rate else
    rate <- x$rate.input

  # Extract pos data
  if(!is.null(pos)){
    pos_from_row <- x$summary$row[pos] # for subsetting rate region
    pos_to_row <- x$summary$endrow[pos] # for subsetting rate region
    pos_x_data <- time[pos_from_row:pos_to_row] # subset of rate region
    pos_y_data_delta <- delta.oxy[pos_from_row:pos_to_row]
  }

  ## ylim  - plus 10%
  ylim <- grDevices::extendrange(na.omit(delta.oxy), f = 0.1)
  if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

  ## Plot
  plot(time,
       delta.oxy,
       xlab = "",
       ylab = "",
       ylim = ylim,
       cex = .5,
       col = ftcol_del,
       axes = FALSE,
       panel.first = grid())

  if(1 %in% axes) axis(side = 1, col.axis = "blue", mgp = bt_mgp)
  if(2 %in% axes) axis(side = 2, col.axis = "black", mgp = lf_mgp)

  box()

  ## Title
  if(delta_only) mtext(title, outer = TRUE, cex = 1, line = 0, font = 2) else
    title(main = title, line = 0.3)

  ## This will have bottom legend regardless
  if(legend) legend("bottomright",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.5)

  ## add coloured points of rate region
  ## No need to do this if data are single points
  ## Which happens when single delta value is converted
  if(!is.null(pos)){
    if(length(pos_y_data_delta) > 1){
      points(pos_y_data_delta ~ pos_x_data,
             col = ftcol_rate_pt,
             cex = .5)
      clip(min(na.omit(pos_x_data)),
           max(na.omit(pos_x_data)),
           min(na.omit(pos_y_data_delta)),
           max(na.omit(pos_y_data_delta)))
      abline(lm(pos_y_data_delta ~ pos_x_data), lwd = 1.2, lty = 3)
    }
  }

  ## If delta only plot add legend and top axis here instead
  if(delta_only){
    # plot this invisibly - to add row index x-axis
    par(new = TRUE)
    plot(seq(1, length(time)),
         delta.oxy,
         xlab = "",
         ylab = "",
         pch = "",
         cex = .5,
         axes = FALSE)

    if(3 %in% axes) axis(side = 3, col.axis = "red", mgp = tp_mgp)

    if(legend) legend("topright",
                      "Row Index",
                      text.col = "red",
                      bg = "gray90",
                      cex = 0.5)
  }
}

# Plots inflow and outflow oxygen on same plot in flowthrough data
# Used in calc_rate.ft and convert_rate.ft
# x = calc_rate.ft or convert_rate.ft object
#   (containing the time data generally in unlist(x$data$time), and
#   in and out oxy data in x$data)
# legend = time/row labels
# pos = which rate to highlight with a shaded box
# box = draw box?
# quiet
# row.axis - add red row axis?
# time.axis - add blue time axis?
# title
in.out.p <- function(x,
                     pos = 1,
                     legend = TRUE,
                     ...){

  # Extract data
  time <- x$data$time[[1]]
  out.oxy <- x$data$out.oxy[[1]]
  in.oxy <- x$data$in.oxy[[1]]
  if(inherits(x, "calc_rate.ft")) rate <- x$rate else
    rate <- x$rate.input
  y_range <- range(in.oxy, out.oxy, na.rm = TRUE) # for plotting rate region rectangle

  # Extract pos data
  if(!is.null(pos)){
    pos_rate <- signif(rate[pos], digits = 5) # rate for this pos
    pos_from_time <- x$summary$row[pos] # # for plotting rate subset region
    pos_to_time <- x$summary$endrow[pos] # # for plotting rate subset region
  }

  ## ylim for outflow and inflow plots - plus 10%
  ylim <- grDevices::extendrange(nainf.omit(range(range(out.oxy), range(in.oxy), na.rm = TRUE)), f = 0.1)

  ## Plot
  plot(time,
       out.oxy,
       xlab = "",
       ylab = "",
       ylim = ylim,
       cex = .5,
       col = ftcol_out,
       axes = FALSE,
       col.lab = "blue",
       col.axis = "blue",
       panel.first = grid())

  axis(side = 2)
  points(time,
         in.oxy,
         xlab = "",
         ylab = "",
         ylim = ylim,
         cex = .5,
         col = ftcol_in)
  # plot this invisibly - to add row index x-axis
  par(new = TRUE)
  plot(seq(1, length(time)),
       out.oxy,
       xlab = "",
       ylab = "",
       pch = "",
       cex = .5,
       axes = FALSE)

  axis(side = 3, col.axis = "red")
  box()

  if(!is.null(pos)){
    ## box for rate region
    abline(v = pos_from_time,
           col = ftcol_rate_ln,
           lty = 1,
           lwd = 3)

    abline(v = pos_to_time,
           col = ftcol_rate_ln,
           lty = 1,
           lwd = 3)

    rect(xleft = pos_from_time,
         ybottom = y_range[1],
         xright = pos_to_time,
         ytop = y_range[2],
         col = ftcol_rate_bx,
         lty = 0)
  }

  if(legend) legend("topright",
                    "Row Index",
                    text.col = "red",
                    bg = "gray90",
                    cex = 0.5)

  if(legend) legend("right",
                    legend = c("Inflow Oxygen", "Outflow Oxygen"),
                    col = c(ftcol_in, ftcol_out),
                    pch = pch_def,
                    cex = 0.4)

  mtext("Outflow - Inflow O2",
        outer = TRUE, cex = 1, line = 0, font = 2)
}

# Plots close-up of 'pos' delta data
# Used in calc_rate.ft
# x = calc_rate.ft or convert_rate.ft object
#   (containing the time data generally in unlist(x$data$time), and
#   in and out oxy data in x$data)
# legend = time/row labels
# pos = which rate to highlight with a shaded box
# box = draw box?
# quiet
# row.axis - add red row axis?
# time.axis - add blue time axis?
# title
pos.ft.p <- function(x, pos = 1, rate.rev = TRUE, legend = TRUE, ...){

  nres <- length(x$rate) # number of rates
  pos_rate <- signif(x$rate[pos], digits = 5) # rate for this pos
  pos_from_row <- x$summary$row[pos] # for subsetting rate region
  pos_to_row <- x$summary$endrow[pos] # for subsetting rate region

  time <- x$data$time[[1]]
  pos_x_data <- time[pos_from_row:pos_to_row]
  pos_y_data <- x$data$delta[[1]][pos_from_row:pos_to_row] * x$inputs$flowrate

  ylim <- grDevices::extendrange(nainf.omit(pos_y_data), f = 0.1)
  if(rate.rev) ylim <- rev(ylim) ## reverse y-axis

  plot(pos_x_data,
       pos_y_data,
       col = ftcol_rate_pt,
       xlab = "",
       ylab = "",
       ylim = ylim,
       cex = .5,
       axes = FALSE,
       panel.first = grid())

  axis(side = 2)
  axis(side = 1, col.lab = "blue", col.axis = "blue")

  box()

  if(legend) legend("bottomright",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.5)

  title(main = glue::glue("Close-up of Position {pos} of {nres}: Rate =  {pos_rate}"), line = 0.3)

  ## add lm trendline
  ## ## No need to do this if data are single points
  ## Which happens when single delta value is converted
  if(length(pos_x_data) > 1){
    clip(min(na.omit(pos_x_data)),
         max(na.omit(pos_x_data)),
         min(na.omit(pos_y_data)),
         max(na.omit(pos_y_data)))
    abline(lm(pos_y_data ~ pos_x_data), lwd = 1.2, lty = 3)
  }

}

# Misc --------------------------------------------------------------------

# These are for possibly replacing use of 'legend' for time and row axis labels
# Still can't get them looking nice, so unused and leaving for now
#
# adds "time" axis label to plots
# xdata
# ydata
# rate.rev
lab.time <- function(xdata,
                     ydata,
                     xlim = NULL,
                     ylim = NULL,
                     rate.rev = TRUE){

  if(is.null(ylim)) yrange <- range(ydata) else
    yrange <- sort(ylim)
  ydiff <- diff(yrange)
  if(rate.rev) ypos <- yrange[2]-(ydiff*0.04) else
    ypos <- yrange[1]+(ydiff*0.04)

  xrange <- range(xdata)
  xdiff <- diff(xrange)
  xpos <- xrange[1]+(xdiff*0.02)

  text(x = xpos,
       y = ypos,
       labels = "Time",
       col = "blue",
       font = 2)
}

# adds "row" axis label to plots
# xdata
# ydata
# rate.rev
lab.row <- function(xdata,
                    ydata){

  yrange <- range(ydata)
  ydiff <- diff(yrange)
  ypos <- yrange[2]-(ydiff*0.02)

  xrange <- range(xdata)
  xdiff <- diff(xrange)
  xpos <- xrange[2]-(xdiff*0.02)

  text(x = xpos,
       y = ypos,
       "Row Index",
       col = "red",
       font = 2,
       adj = c(1,1))
}

