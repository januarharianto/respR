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

# Plots multiple auto_rate or convert_rate results in a nice way
# using base plot
# x = auto_rate or convert_rate object
grid.p <- function(x, pos = NULL, msg = "grid.p",
                   title = "Rank", quiet = FALSE, ...){

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  ## warning if empty - but return to allow piping
  if(length(x$summary$rate.output) == 0){
    message(glue::glue("{msg}: Nothing to plot! No rates found in object."))
    return(invisible(x))
  }

  # total number of results
  totres <- nrow(x$summary)
  # set pos
  if(is.null(pos)) pos <- 1:totres
  if(any(pos > totres)){
    message(glue::glue("{msg}: One or more 'pos' inputs higher than number of rows in '$summary'. Applying default of all rows."))
    pos <- 1:nrow(x$summary)
  }

  if(!quiet){
    if(length(pos) <= 20 && length(pos) == totres) message(glue::glue("{msg}: Plotting all rate(s)...")) else
      if(length(pos) <= 20) message(glue::glue("{msg}: Plotting rate(s) from selected 'pos' rows...")) else
        message(glue::glue("{msg}: Plotting first 20 selected rates only. To plot others use 'pos' input."))
  }

  par(oma = c(2, 2, 2, 0.5), mar = c(0.1, 0.1, 2, 0.1))
  # allows params overriding defaults to be passed
  par(...)

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

  if(length(pos) == 1)          {
    par(mfrow = c(1,1))
    bt <- 1
    lf <- 1
    tp <- 1
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) == 2)          {
    par(mfrow = c(1,2))
    bt <- pos[1:2]
    lf <- pos[1]
    tp <- pos[1:2]
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(3,4))   {
    par(mfrow = c(2,2))
    bt <- pos[3:4]
    lf <- pos[c(1,3)]
    tp <- pos[1:2]
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(5,6))   {
    par(mfrow = c(2,3))
    bt <- pos[4:6]
    lf <- pos[c(1,4)]
    tp <- pos[1:3]
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(7,8,9)) {
    par(mfrow = c(3,3))
    bt <- pos[7:9]
    lf <- pos[c(1,4,7)]
    tp <- pos[1:3]
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(10:12)) {
    par(mfrow = c(3,4))
    bt <- pos[9:12]
    lf <- pos[c(1,5,9)]
    tp <- pos[1:4]
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) %in% c(13:16)) {
    par(mfrow = c(4,4))
    bt <- pos[13:16]
    lf <- pos[c(1,5,9,13)]
    tp <- pos[1:4]
    plot.sub.grid(x, pos, bt, lf, tp)
  } ## start to get margins too large errors
  if(length(pos) %in% c(17:20)) {
    par(mfrow = c(4,5))
    bt <- pos[16:20]
    lf <- pos[c(1,6,11,16)]
    tp <- pos[1:5]
    plot.sub.grid(x, pos, bt, lf, tp)
  }
  if(length(pos) > 20){
    pos <- 1:20
    par(mfrow = c(4,5))
    bt <- pos[16:20]
    lf <- pos[c(1,6,11,16)]
    tp <- pos[1:5]
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
  dt <- x$dataframe
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

  multi.p(dt, sub_dt, axes = c(2,3), legend = legend)
  mtext("Full Timeseries",
        outer = TRUE, cex = 1.2, line = 0, font = 2)

  # Overlap plot ------------------------------------------------------------

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
        outer = FALSE, cex = 1.2, line = 0.8, font = 2)
  # invisible plot to get time axis
  par(new=TRUE)
  plot(dt[[1]], dt[[2]], pch = "", xlab = "", ylab = "", axes = FALSE)
  axis(side = 1, col.axis = "blue")
  if(legend) legend("bottomleft",
                    "Time",
                    text.col = "blue",
                    bg = "gray90",
                    cex = 0.5)

  return(invisible(x))
}

# Plots convert_rate objects
# Very similar to plot.inspect
# top plot is same fill timeseries
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
  y_lim <- grDevices::extendrange(nainf.omit(x$rate.output), f = 0.05)
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
