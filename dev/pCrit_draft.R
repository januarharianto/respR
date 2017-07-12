# Functions-----------
spawnIndices <- function(x, min = 3) {
  seq1 <- data.frame(1, (seq.int(min, (x-min))))
  seq2 <- data.frame((seq.int(min, (x-min)) + 1), x)
  seqs <- unname(as.matrix(cbind(seq1, seq2)))
  seqs
}

regLm <- function(indx, df) {
  # generate windows
  x1 <- df[, 1][indx[1]:indx[2]]
  y1 <- df[, 2][indx[1]:indx[2]]
  x2 <- df[, 1][indx[3]:indx[4]]
  y2 <- df[, 2][indx[3]:indx[4]]
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
  #intersect
  cm        <- rbind(coef1, coef2)
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
  xint      <- intersect[1]
  yint      <- intersect[2]
  # output
  out <- data.frame(
    start1 = start1,
    end1   = end1,
    slope1 = b1a,
    start2 = start2,
    end2   = end2,
    slope2 = b1b,
    sumRSS = sumRSS,
    xint   = yint,
    yint   = xint)
  out
}
#--------------

library(zoo) # this is needed
library(ggplot2) # not needed if we use plot()

#---------------------------------
# workflof for pCrit
# load data
sample <- read.csv('~/Documents/Dev/pcrit.csv')
sample$elapsed <- as.numeric(sample$x - sample$x[1]) # convert date to integer
sample <- data.frame(x = sample$elapsed, y = sample$y) # rename columns

# perform rolling regression
# First, width needs to be defined
# let's default it to 0.1 (but editable)
span <- 0.1
width <- floor(span * nrow(sample))
# now perform the rolling regression
reg <- function(x) coef(.lm.fit(cbind(Intercept = 1, x[, 1]), x[, 2]))
rollreg <- rollapply(sample, width, reg, by.column = F)
# nrow(rollreg)
# now perform rolling means
rollmean <- rollmean(sample[, 2], width)
# length(rollmean)
# bundle into dataframe and re-order. Also, make slopes absolute integers
mrDO <- data.frame(DO = as.numeric(rollmean), MR = abs(as.numeric(rollreg[, 2])))
# sort ascending by DO
mrDO <- mrDO[order(mrDO$DO), ]

# let's start the hockey!
nr <- nrow(mrDO)
indices <- spawnIndices(nr)
reg <- apply(indices, 1, regLm, df = mrDO) # perform all regressions
reg <- do.call(rbind.data.frame, reg) # bind all results by row into a dataframe
pcrit <- subset(reg, sumRSS == min(sumRSS)) # filters out the row with the lowest combined RSS

line1 <- subset(mrDO, DO <= as.numeric(pcrit[2]))
line2 <- subset(mrDO, DO > as.numeric(pcrit[2]))

lm(MR~DO, line1)
lm(MR~DO, line2)

ggplot(mrDO, aes(DO, MR)) + geom_point(size = .1) + # this is the basis plot
  geom_smooth(data = line1, aes(DO,MR), method = 'lm') +
  geom_smooth(data = line2, aes(DO,MR), method = 'lm') +
  geom_vline(xintercept = as.numeric(pcrit[9]), colour = 'purple', size = .2) + # intercept
  geom_vline(xintercept = ((as.numeric(pcrit[2]) + as.numeric(pcrit[4])) / 2), colour = 'yellow', size = .2) + # 'midpoint approximation' (yeager & ultsch 1989)
  theme_bw()

as.numeric(pcrit[9])
((as.numeric(pcrit[2]) + as.numeric(pcrit[4])) / 2)
