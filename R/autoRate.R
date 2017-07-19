#' @import ggplot2
#' @export
autoRate <- function(df, span = 0.3) {
  row.names(df) <- NULL # reset row numbers (keep if I'm using indexing later)
  width <- floor(span * nrow(df))
  slopes <- rollreg(df, span)  # perform rolling regression and output slopes
  #hist(slopes)
  dens <- density(slopes, na.rm = T, n = length(slopes)) # estimate density
  densBW <- dens$bw  # extract bin width
  densPeaks <- which.peaks(dens) # find peaks in density
  rankP <- densPeaks[order(densPeaks$density, decreasing = T), ] # rank results
  print(rankP)
  bestP <- rankP$slope[1]
  rdf <- data.frame(slopes)  # convert rollReg to dataframe

  # pick windows with slopes within the bin range
  # picks <- which(rdf <= (bestP + densBW) & rdf >= (bestP - densBW))
  # allWins <- unname(split(picks, cumsum(c(TRUE, diff(picks) > width))))
  # create index for main dataframe
  # indx <- data.frame(x=do.call(cbind, lapply(allWins, function(l) c((min(l) - width + 1), max(l)))))
  # create new dataframes based on index
  #allsets <- lapply(indx, function(x) df[x[1]:x[2],]) # breaks sometimes. Need to do this better.
  # indx

  # define plotting constants
  pri <- 'navy'; sec <- 'black'; a <- 0.6

  pdf <- ggplot(df, aes(df[, 1], df[, 2])) +
    geom_point(size = 1.5, colour = pri, alpha = a) +
    labs(x = 'Time', y = 'DO') +
    theme_respr()

  # plot density
  pdens <- ggplot(rdf, aes(slopes)) +
      stat_density(fill = pri, alpha = a, na.rm = T) +
      geom_line(stat = 'density', colour = sec, alpha = a, na.rm = T) +
      labs(x = 'MO2', y = 'Density') +
      theme_respr()

  # plot slopes
  slopesdf <- data.frame(x = df[,1], y = slopes)
  pslope <- ggplot(slopesdf, aes(x,y)) +
      geom_line(size = 1, colour = pri, na.rm = T) +
      labs(x = 'MO2', y = 'Time') +
      theme_respr()

  cowplot::plot_grid(pdf, pslope, pdens, nrow = 2, ncol = 2, align = 'hv')
}


# functions ----------------------------------------------
rollreg <- function(df, span, intercept = F) {
  x <- matrix(df[[1]])
  y <- matrix(df[[2]])
  width <- floor(span * nrow(df))
  lm <- roll::roll_lm(x, y, width)
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
  out
  }

