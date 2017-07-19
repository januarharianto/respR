#' Calculate Pcrit
#'
#' @param df Data frame.
#' @param span Numeric.
#' @param MR Logical.
#'
#' @return A value of Pcrit.
#' @export
#'
#'
pcrit <- function(df, span = 0.05, MR = FALSE) {
  if (MR == T) {
    mrDo <- df
  } # end if

  if(MR == F) {
    names(df) <- c("x", "y")
    # convert time to integer, if necessary
    if (any(inherits(df$x, "POSIXct"), (inherits(df$x, "POSIXt"))) == T) {
      df$x   <- as.integer(df$x - df$x[1])
    } # end if

    # otherwise, carry on
    width    <- floor(span * nrow(df))
    rollreg  <- movReg(df, span)
    rollmean <- RcppRoll::roll_mean(df[[2]], width)
    counts   <- length(rollmean) # for benchmark
    mrDo     <- data.frame(rollmean, abs(rollreg))
  } # end if

  # calculate pcrit here
  names(mrDo) <- c('do', 'mr')
  mrDo        <- mrDo[order(mrDo$do), ]  # sort (ascending)
  indices     <- spawnIndices(nrow(mrDo))  # create matrix for hockey method
  message("Performing rolling 'hockey' regressions...")
  old         <- Sys.time()  # grab current time (for simple benchmark)
  reg         <- apply(indices, 1, hockeyLm, df = mrDo)
  reg         <- do.call(rbind.data.frame, reg)  # bind into a dataframe
  pcrit       <- reg[order(reg$sumRSS), ]
  pcritRanked <- pcrit[order(pcrit$sumRSS), ] # rank results by RSS
  best        <- pcritRanked[1, ] # best result
  new         <- round(unclass(Sys.time() - old)[1], 1)
  # save data for plots
  time   <- df[[1]]
  do     <- df[[2]]
  rolldo <- mrDo[[1]]
  rollmr <- mrDo[[2]]

  out <- list(
    do.mr = mrDo,
    pcrit = pcrit,
    pcritRanked = pcritRanked,
    best = best)

  cat(sprintf("%d 'hockey' regressions fitted ",  NROW(pcrit)),
    sprintf("in %g seconds", new), "\n\n")
  cat('Top 6 results:\n')
  print(head(out$pcritRanked))

  class(out) <- "pcrit"
  print(plot(out, rank = 1))

  return(invisible(out))
}



#' @export
plot.pcrit <- function(x, rank = 1, ...) {
  data <- x$do.mr  # main plot dataset
  ref <- x$pcritRanked[1] # reference list to create subsets

  lm1 <- subset(data, do <= as.numeric(ref[rank, ]))
  lm2 <- subset(data, do >  as.numeric(ref[rank, ]))

  p <-
    ggplot(data, aes(do, mr)) +
      geom_line(size = 1, colour = 'navy') +
      stat_smooth(data = lm1, aes(do, mr),  na.rm=TRUE, se = F, method = 'lm', fullrange = T, size = .25, linetype = 2, colour = 'black', alpha = .1) +
      stat_smooth(data = lm2, aes(do, mr), na.rm=TRUE, se = F, method = 'lm', fullrange = T, size = .25, linetype = 2, colour = 'black', alpha = .1) +
      stat_smooth(data = lm1, aes(do, mr),  na.rm=TRUE, se = F, method = 'lm', fullrange = F, size = .5, colour = 'firebrick') +
      stat_smooth(data = lm2, aes(do, mr), na.rm=TRUE, se = F, method = 'lm', fullrange = F, size = .5, colour = 'firebrick') +
      geom_vline(xintercept = x$pcritRanked[5][rank, ], size = .5, linetype = 3, colour = 'darkorchid2') + # pcrit intercept
      geom_vline(xintercept = x$pcritRanked[6][rank, ], size = .5, linetype = 3, colour = 'coral2') + # pcrit midpoint
      annotate('text', x = x$pcritRanked[5][rank, ], y = max(data$mr), label = signif(x$pcritRanked[5][rank, ], 3), angle = 90) +
      annotate('text', x = x$pcritRanked[6][rank, ], y = min(data$mr), label = signif(x$pcritRanked[6][rank, ], 3), angle = 90) +
      ylab('Metabolic rate') +
      xlab('Oxygen concentration') +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 1.1 * max((data[2])))) +
      # labs(colour = "Regression") +
      theme_respr() +
      geom_blank()

  return(p)
}


