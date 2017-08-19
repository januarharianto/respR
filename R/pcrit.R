#' Calculate pcrit
#'
#' I need a description for this...
#'
#' @param df data frame object
#' @param span numeric
#' @param MR logical
#'
#' @return NULL
#' @import parallel
#' @export
#'
#' @examples NULL
pcrit <- function(df, span = 0.05, datmr = FALSE, plot = T) {
  if (datmr == T) {
    mrDo <- na.omit(df)
  }
  if (datmr == F) {
    names(df) <- c("x", "y")
    # Convert time to integer, if necessary. We do this because even though
    # check.input ensures that a numeric input is in the time column, it is
    # inevitable that many pcrit experiments use real time (instead of time
    # elapsed) due to the extended nature of pcrit experiments (hours to more
    # than a day). We do want to analyse those experiments... :D
    if (any(inherits(df$x, "POSIXct"), (inherits(df$x, "POSIXt"))) == T) {
      df$x   <- as.integer(df$x - df$x[1])
    }
    # otherwise, carry on
    width    <- floor(span * nrow(df))
    rollreg <- roll.reg(df, width)$b1
    rollmean <- roll::roll_mean(matrix(df[[2]]), width)
    counts   <- length(rollmean) # for benchmark
    mrDo     <- na.omit(data.frame(rollmean, abs(rollreg)))
  }
  # calculate pcrit here
  names(mrDo) <- c('do', 'mr')
  mrDo        <- mrDo[order(mrDo$do), ]  # sort (ascending)
  indices     <- spawnIndices(nrow(mrDo))  # create matrix for hockey method
  message("Performing rolling 'hockey' regressions...")
  old         <- Sys.time()  # grab current time (for simple benchmark)
  # some parallel computing here (new trick I learned):
  no_cores <- detectCores() - 1   # calculate the number of cores available
  cl <- makeCluster(no_cores)   # initiate cluster and use those cores
  reg <- parApply(cl, indices, 1, hockeyLm, df = mrDo) # perform regressions
  stopCluster(cl)  # release cores

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

  cat(sprintf("%d 'hockey' regressions fitted",  NROW(pcrit)),
    sprintf("in %g seconds", new), "\n\n")
  # cat('Top 6 results:\n')
  # print(head(out$pcritRanked))
  class(out) <- append(class(out),"pcrit")
  if (plot == T) plot(out, rank = 1)
  return(out)
}

#' @export
print.pcrit <- function(x) {
  cat("Rank 1 result:\n")
  print(x$best)
}

#' @export
plot.pcrit <- function(x, rank = 1, ...) {
  pcrit.p(x, rank)
}

# ------------------------------------------------------------------------------
# Internal functons
# ------------------------------------------------------------------------------

# generate an index for hockeyLm
spawnIndices <- function(x, min = 3) {
  seq1 <- data.frame(1, (seq.int(min, (x-min))))
  seq2 <- data.frame((seq.int(min, (x-min)) + 1), x)
  seqs <- unname(as.matrix(cbind(seq1, seq2)))
  seqs
}

# perform hockey-stick regressions
hockeyLm <- function(indx, df) {
  # generate windows
  x1 <- df[, 1][indx[1]:indx[2]] # x-coordinate of line 1
  y1 <- df[, 2][indx[1]:indx[2]] # y-coordinate of line 1

  x2 <- df[, 1][indx[3]:indx[4]] # x-coordinate of line 2
  y2 <- df[, 2][indx[3]:indx[4]] # y-coordinate of line 2
  # design matrix of dimension n * p for .lm.fit
  mx1 <- matrix(cbind(1, x1), ncol = 2)
  mx2 <- matrix(cbind(1, x2), ncol = 2)
  # extract index
  start1 <- x1[1]
  end1   <- x1[length(x1)]
  start2 <- x2[1]
  end2   <- x2[length(x2)]
  # lm.fit
  reg1 <- .lm.fit(mx1, y1)
  reg2 <- .lm.fit(mx2, y2)
  # coefficients
  coef1 <- coef(reg1)
  coef2 <- coef(reg2)
  b1a   <- coef1[2]
  b1b   <- coef2[2]
  # calculate RSS (residual sum of squares)
  res1   <- reg1$residuals
  res2   <- reg2$residuals
  rss1   <- sum(res1 * res1)
  rss2   <- sum(res2 * res2)
  sumRSS <- rss1 + rss2
  # calculate intersection between 2 lines
  cm        <- rbind(coef1, coef2)
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1]) # https://stackoverflow.com/a/7114961
  xint      <- intersect[1]
  yint      <- intersect[2]
  # output
  out <- data.frame(  # need to improve output
    # start1       = start1,
    splitpoint     = end1,
    slope1         = b1a,
    # start2       = start2,
    # end2         = end2,
    slope2         = b1b,
    sumRSS         = sumRSS,
    # xint         = yint,
    pcrit.lm       = xint,
    pcrit.mpoint   = (end1 + start2) / 2)
  out
}



# # fishpcrit <- fmr
# # devtools::use_data(fishpcrit)
# data("fishpcrit")
# df <- fishpcrit
# span <- 0.05
#
# width <- floor(span * nrow(df))
# rollreg <- roll.reg(df, width)$b1
# rollmean <- roll::roll_mean(matrix(df[[2]]), width)
# counts <- length(rollmean) # for benchmark
# # create data frame
# mrDo <- na.omit(data.frame(rollmean, abs(rollreg)))
#
# # now we need to perform hockey regression
# # first generate the index
# indices <- spawnIndices(nrow(mrDo))
# seq1 <- data.frame(1, (seq.int(3, (nrow(mrDo)-3))))
# seq2 <- data.frame((seq.int(3, (nrow(mrDo)-3)) + 1), nrow(mrDo))
#
# # we use the index to generate subsets of the data for lm fits
# # how do we do this FAST?
#
# #
#

