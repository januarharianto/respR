#' @import ggplot2
#' @import zoo
#' @export
pcrit <- function(df, span = 0.05, MR = FALSE) {
  if (MR == T) {
    mrDo <- df
  }
  if(MR == F) {
  # ordf <- df
  names(df) <- c("x", "y")
  # convert time to integer, if necessary
  if (any(inherits(df$x, "POSIXct"), (inherits(df$x, "POSIXt"))) ==
    T)
    df$x <- as.integer(df$x - df$x[1])
  width <- floor(span * nrow(df))
  # Rolling regression and mean
  message("Performing regressions to generate MO2...")
  old <- Sys.time()  # grab current time (for simple benchmark)
  reg <- function(x) coef(.lm.fit(cbind(Intercept = 1, x[,
    1]), x[, 2]))
  rollreg <- zoo::rollapply(df, width, reg, by.column = F)
  rollmean <- zoo::rollmean(df[, 2], width)
  counts <- length(rollmean)
  # bundle into dataframe and re-order
  mrDo <- data.frame(rollmean, abs(rollreg[, 2]))
  }

  names(mrDo) <- c("do", "mr")
  mrDo <- mrDo[order(mrDo$do), ]  # sort (ascending)
  indices <- spawnIndices(nrow(mrDo))  # create matrix of hockey combinations
  message("Performing rolling 'hockey' regressions...")
  reg <- apply(indices, 1, hockeyLm, df = mrDo)
  reg <- do.call(rbind.data.frame, reg)  # bind into a dataframe
  pcrit <- reg[order(reg$sumRSS), ]
  pcritRanked <- pcrit[order(pcrit$sumRSS), ] # rank results by RSS
  best <- pcritRanked[1, ] # best result

  new <- round(unclass(Sys.time() - old)[1], 1)
  # save data for plots
  time <- df[[1]]
  do <- df[[2]]
  rolldo <- mrDo[[1]]
  rollmr <- mrDo[[2]]

  out <- list(
    # df = ordf,
    do.mr = mrDo,
    pcrit = pcrit,
    pcritRanked = pcritRanked,
    best = best)
  message(sprintf("%d regressions fitted ", counts + nrow(mrDo)),
    sprintf("in %g seconds", new), "\n")
  class(out) <- "pcrit"
  return(out)
}



#' @export
plot.pcrit <- function(x, rank = 1, ...) {
  data <- x$do.mr  # main plot dataset
  ref <- x$pcritRanked[1] # reference list to create subsets

  lm1 <- subset(data, do <= as.numeric(ref[rank, ]))
  lm2 <- subset(data, do >  as.numeric(ref[rank, ]))

  p <-
    ggplot(data, aes(do, mr)) +
      geom_point(size = 1, colour = 'yellow2', alpha = .1) +
      stat_smooth(data = lm1, aes(do, mr),  na.rm=TRUE, se = F, method = 'lm', fullrange = T, size = .5, linetype = 6, colour = 'black') +
      stat_smooth(data = lm2, aes(do, mr), na.rm=TRUE, se = F, method = 'lm', fullrange = T, size = .5, linetype = 6, colour = 'black') +
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



theme_respr <- function() {
  theme_bw(base_size = 14) %+replace%
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      plot.margin = unit(c(10,5,5,5),"mm"),

      panel.border = element_rect(fill = 'transparent', size = 1),
      # panel.grid.major = element_line(colour="#f0f0f0"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      axis.text	= element_text(size = rel(1)),
      axis.title = element_text(face = 'bold', size = rel(1)),
      axis.title.y = element_text(angle = 90, margin = margin(0, 25, 0, 0)),
      axis.title.x = element_text(margin = margin(25, 0, 0, 0)),
      axis.ticks = element_line(),

      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.size = unit(0.2, "cm"),
      legend.title = element_text(face="italic"),

      strip.background = element_rect(colour="#f0f0f0",fill="#f0f0f0"),
      strip.text = element_text(face="bold")
    )
}

