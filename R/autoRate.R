#' @import ggplot2 ggpmisc
#' @importFrom roll roll_lm
#' @export
autoRate <- function(df, span = 0.1) {
  # prepare the data for analysis
  row.names(df) <- NULL               # reset row numbers if it's a subset
  width <- floor(span * nrow(df))     # calculate width from span value
  slopes <- rollreg(df, span)         # perform rolling regression and output slopes
  rdf <- data.frame(slopes)           # convert rolling regression output to dataframe for later analysis

  # now that rolling regression is done we can analyse its output.
  # perform kernel density estimation on regression data:
  dens <- density(slopes, na.rm = T, bw = 'SJ', n = length(slopes))
  densPeaks <- which.peaks(dens)                                 # find peaks in density
  rankP <- densPeaks[order(densPeaks$density, decreasing = T), ] # rank density peaks from highest to lowest
  allslopes <- rankP[, 1]                                        # extract just the slope values
  densBW <- dens$bw # extract bin width from kernel density estimation

  # we now define a method to identify the rows in the original dataset that
  #   correspond to the bin width. This is done for every single peak detected
  #   as above.
  makeIndex <- function(rdf, x, densBW, width) {
    # segments of the rolling windows that produced those slopes are
    #   first identified:
    picks <- which(rdf <= (x + densBW/2) & rdf >= (x - densBW/2))
    # the segments may be non-continuous; they are separated into groups:
    allWins <- unname(split(picks, cumsum(c(TRUE, diff(picks) >= width))))
    # index values (which can identify the first and last rows of the original
    #   dataframe) are generated:
    indx <- do.call(rbind, lapply(allWins, function(l) c((min(l) - width + 1), max(l))))
    # the index with the largest width is selected; the rest are discarded.
    # a better method would be to analyse each width (upgrade in future?):
    best <- which((indx[, 2] - indx[, 1]) == max((indx[, 2] - indx[, 1])), arr.ind = TRUE)[1]
    return(indx[best, ]) # the best index is passed on for further analysis below
  }
  # link each peak with the best index by applying makeIndex on all density peak results:
  rankedIndices <- lapply(allslopes, makeIndex, densBW = densBW, rdf = rdf, width = width)
  rankedIndices <- do.call(rbind, rankedIndices) # bind the results

  # now that we have identified the start/end rows in the original dataset for
  #   each peak, we extract the subsets of the original dataframe that
  #   correspond to each peak and store them all in a list.

  # first, all data needed for the extraction are aggregated into a list:
  input <- list(df = df,
                rankP = rankP,
                slopes = slopes,
                rdf    = rdf,
                rankedIndices = rankedIndices)
  # we also create an index to specify how many times we need to subset the data:
  glist <- c(1:NROW(rankedIndices))
  # extract subsets and perform a linear regression on each subset. We use these
  #   linear values for our results instead of the averaged slopes used
  #   initially to detect the subsets. This gives us greater statistical power.
  allsubs <- lapply(glist, rankedOutput, input = input)
  # rename the lists to 'ranks' so that they are easily selected in the output:
  names(allsubs)<-sprintf("Rank%i",1:length(allsubs))

  # DONE! Now do cleanup and output:
  # bind all lm results into table:
  allSummarylms <- lapply(glist, function(x) allsubs[[x]][[8]])
  allSummarylms <- do.call(rbind, allSummarylms)
  # generate summary:
  summary <- cbind(rankP, bin = densBW, allSummarylms)
  print(head(summary))
  class(allsubs) <- append(class(allsubs), "autoRate")
  message('Please wait while we generate diagnostic plots...')
  print(plot.autoRate(allsubs))
  message("Done.")
  return(invisible(allsubs))
}



#' @export
# plot of class autoRate
plot.autoRate <- function(x, rank = 1) {
  set <- x[[rank]]  # grab the data based on rank
  df <- set$df      # the original dataframe
  subdf <-set$subdf # the subset
  rdf <- set$rdf    # rolling dataframe of b1 (slope) data
  bestb1 <- signif(set$rankP$b1.reference[1], 3) # best b1, rounded to 3 s.f.
  ggPeaks <- unclass(set$rankP$b1.reference)     # peaks, to label density plot
  rPeak <- ggPeaks[rank]                         # the above is ranked
  slopesdf <- data.frame(x = set$df$Time, y = set$rdf)  # create dataframe of slope data vs time data
  allslopes <- set$rankP[, 1][rank]              # the best slope
  lmfit <- set$lmfit                             # lmfit
  # define plotting constants
  c1 <- 'navy'; c2 <- 'black'; c3 <- 'goldenrod'; a <- 0.6

  # main plot
  pltdf <- ggplot(df, aes(x = df[[1]], y = df[[2]])) +
    geom_point(size = 1.5, colour = c1, alpha = a) +
    geom_point(data = subdf, aes(x, y), colour = c3) +
    geom_smooth(data = subdf, aes(x, y), method = 'lm', se = F, colour = c2, linetype = 2) +
    labs(x = 'Time', y = 'DO') +
    theme_respr()

  # subset plot
  my.formula <- y ~ x
  pltsub <- ggplot(subdf, aes(x, y)) +
    geom_point(colour = c3) +
    geom_smooth(method = 'lm', se = F, colour = c2, linetype = 2) +
    ggpmisc::stat_poly_eq(formula = my.formula,
      eq.with.lhs = "italic(hat(y))~`=`~",
      aes(label = paste(..eq.label.., sep = "*plain(\",\")~")),
      label.x.npc = .6,
      parse = TRUE) +
    theme_respr()

  # density plot
  pdens <- ggplot(rdf, aes(slopes)) +
    stat_density(fill = c1, alpha = a, na.rm = T) +
    geom_line(stat = 'density', colour = c2, alpha = a, na.rm = T) +
    geom_vline(xintercept = ggPeaks, size = .2, linetype = 2) +
    geom_vline(xintercept = rPeak, size = 2, colour = c1) +
    labs(x = 'Time', y = 'MO2') +
    theme_respr()

  # rolling regression plot
  pslope <- ggplot(slopesdf, aes(slopesdf[, 1], slopesdf[, 2])) +
    geom_line(size = 1, colour = c1, na.rm = T) +
    geom_hline(yintercept = allslopes) +
    annotate(geom="text", label = signif(allslopes, 3), x = 0.1*max(slopesdf[1],
      na.rm = T), y = allslopes, vjust = 2) +
    labs(x = 'Time', y = 'MO2') +
    theme_respr()

  # residual plot
  presi <- ggplot(lmfit, aes(.fitted, .resid)) +
    geom_point(size = 2, colour = c1, alpha = .2) +
    labs(x = "Fitted values", y = "Residuals") +
    stat_smooth(method = 'loess', colour = c2) +
    theme_respr()

  # QQ Plot
  # extract standardized residuals from the fit
  d <- data.frame(std.resid = rstandard(lmfit))
  # calculate 1Q/4Q line
  yx <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  xx <- qnorm(c(0.25, 0.75))
  slope <- diff(yx)/diff(xx)
  int <- yx[1L] - slope * xx[1L]
  pqq <- ggplot(data = d, aes(sample = std.resid)) +
    stat_qq(shape = 21, size = 3, fill = c1, colour = c1, alpha = .4) +
    labs(x="Theoretical Quantiles", y="Standardised Residuals") +
    geom_abline(slope = slope, intercept = int, linetype = "dashed", colour = c3) +
    theme_respr()

  cowplot::plot_grid(pltdf, pltsub, pslope, pdens, presi, pqq, nrow = 3, ncol = 2, align = 'hv')
}


# functions ----------------------------------------------
rankedOutput <- function(input, rank) {
  # dissect input
  df <- input$df
  rankP <- input$rankP
  slopes <- input$slopes
  rdf <- input$rdf
  indx <- input$rankedIndices
  # grab list number
  rnk <- indx[rank, ]
  x <- df[[1]][rnk[1]:rnk[2]]
  y <- df[[2]][rnk[1]:rnk[2]]
  initial.x <- x[1]
  final.x <- x[length(x)]
  subdf <- data.frame(x, y)  # generate subset df
  lmfit <- lm(y ~ x, subdf)
  b0 <- coef(lmfit)[[1]]  # intercept
  b1 <- coef(lmfit)[[2]]  # slope
  r2 <- summary(lmfit)$r.square  # r-square
  lmsumm <- data.frame(initial.x = initial.x,
    final.x = final.x,
    b0 = b0,
    b1 = b1,
    r.squared = r2)
  out <- list(df = df,
    subdf = subdf,
    rankP = rankP,
    slopes = slopes,
    rdf = rdf,
    indx = indx,
    lmfit = lmfit,
    summarylm = lmsumm)
  return(out)
}

rollreg <- function(df, span, intercept = F) {
  x <- matrix(df[[1]])
  y <- matrix(df[[2]])
  width <- floor(span * nrow(df))
  lm <- roll_lm(x, y, width)
  slopes <- lm$coefficients[, 2]
  nfits <- length(slopes[!is.na(slopes)])
  message(sprintf('%d regressions fitted.', nfits))
  slopes
}

which.peaks <- function(ds) {
  d.x <- ds$x
  d.y <- ds$y
  df <- data.frame(slope = d.x, density = d.y)
  allPeaks <- which(diff(sign(diff(d.y)))==-2)+1
  listVals <- lapply(allPeaks, function(x) df[x, ]) # This works, doesn't need function
  out <- as.data.frame(do.call(rbind, listVals))
  names(out) <- c('b1.reference', 'density')
  out
}


