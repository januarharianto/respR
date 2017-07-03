# Load my usual packages and functions
source("~/Dropbox/R/functions/allFunctions.R")
loadPackage("tidyverse")  # better code
loadPackage("cowplot")    # scientific theme for ggplot2
loadPackage("viridis")    # colour scheme for ggplot2

# Set working directory


# Load additional libraries if needed

loadPackage("LoLinR")

# Load data
df <- readr::read_csv("data/hery.csv") %>%
  dplyr::select(time=1,o2=4) %>%
  print()
df

# Workflow ---------------------------------------------------------------------

# prepData function ------------------------------------------------------------

prepData <- function(df) {
  df <- dplyr::rename_(df, x = names(df)[1], y = names(df)[2])

  # Grab units
  o2time <- as.character(readline("Unit of time (e.g. s, min):"))
  o2unit <- as.character(readline("Unit of O2 conc, (e.g. mgL-1, ug/kg:"))
  # save values
  resprdf <<- df %>% print()
  o2time <<- o2time
  o2unit <<- o2unit
  # Plot
  ggplot(df, aes(x, y)) + geom_point()
  message("Table generated. Please run calcRO2() to process data.")
}

prepData(df)
resprdf
# calcRate function -------------------------------------------------------------

calcRO2 <- function(start, end, df = resprdf) {
  # load andsubset data
  df <- resprdf
  win  <- dplyr::filter(df, x >= start, x <= end)
  # perform regression
  reg  <- lm(y ~ x, win)
  # Grab coefficients
  slope     <- round(coef(reg)[2], 4)
  intercept <- round(coef(reg)[1], 4)
  rsq       <- round(summary(reg)$r.squared, 5)
  arsq      <- round(summary(reg)$adj.r.squared, 5)
  # Delta method
  dvars <- slice(win, c(1,n()))  # grab first and last row
  dRO2  <- round(((dvars$y[2] - dvars$y[1]) / (dvars$x[2] - dvars$x[1])), 4)
  # Generate tables
  cat('\n')
  out <- matrix(c(dRO2, slope, intercept, rsq, arsq,
                  dvars$x[1], dvars$x[2]), ncol = 7, byrow = F) %>%
    `colnames<-`(c("Delta", "Slope", "Intercept", "R-sq", "Adj R-sq",
                   "Time start", "Time end")) %>%
    `rownames<-`("") %>%
    as.table() %>%
    print()
  # Copy to clipboard
  clip <- pipe("pbcopy", "w")
  write.table(out, file=clip, col.names = F, row.names = F,
              sep = "\t", eol = "\r")
  close(clip)
  # cat('\n')
  # vrs <- matrix(c(start, end), ncol = 2, byrow = T) %>%
  #   `colnames<-`(c("Start", "End")) %>%
  #   `rownames<-`("Time") %>%
  #   as.table() %>% print()
  # Plot
  plotFull <- ggplot(df, aes(x, y)) + ggtitle("Full data") +
                geom_point() +
                geom_point(data = win, aes(x ,y), colour = "seagreen3") +
                xlab("Time") +
                ylab("O2")
  plotWin <- ggplot(win, aes(x, y)) + ggtitle("Selected") +
               geom_point(colour = "seagreen4") +
               geom_smooth(method = 'lm', colour = "black", size = .5, se = F) +
               xlab("Time") +
               ylab("O2")
  plot_grid(plotFull, plotWin, nrow = 2)
}

# Test
prepData(df)
calcRO2(10.1, 30)


# Others -----------------------------------------------------------------------



nonmon <- read_csv("data/non_monotonic.csv") %>% print()

# Check for NA and NaN ---------------------------------------------------------
# TRUE - there are NA values
# FALSE - no NA values
hasNA <- function(dt, summary = F) {
  check <- any(is.na(dt))
  if (check == T) {
    check %>% print()
  }
  if (check == F) {
    check %>% print()
  }
  if (summary == T) {
    which(is.na(dt), arr.ind = T) %>% print()
  }
}

# Test
hasNA(nonmon, T)

# Check for duplicate time -----------------------------------------------------
?duplicated.array()
anyDuplicated(nonmon[1])
nonmon[duplicated(nonmon[,1]),]

dupes <- function(df, summary = F) {
  check <- duplicated(df[[1]])
  if (check == T) {

  }
}
duplicated(nonmon[[1]])
which(duplicated(nonmon[[1]]) == T)

# Check for monotonically increasing time --------------------------------------
# Are all the values in x equal to the cumulative maximum of the same values?
# https://stackoverflow.com/a/13094801

monotonic <- function(df, summary = F) {
  x <- df
  x <- x[[1]]
  check <- all(x == cummax(x))
  if (check == T) {
    check %>% print()
  }
  if (check == F) {
    check %>% print()
    which(diff(x) < 0) %>% print() # ID rows that return FALSE
  }
}

# Test
monotonic(nonmon)

# Check for evenly spaced time -------------------------------------------------
# https://stackoverflow.com/a/4752580

evenSpaced <- function(df, col = 1, tol = .Machine$double.eps * 100,
                       summary = F) {
  x <- df
  x <- diff(x[[col]])
  cond <- range(x) / mean(x)
  check <- isTRUE(all.equal(cond[1], cond[2], tolerance = tol))
  if (check == T) {
    check %>% print()
  }
  if (check == F) {
    check %>% print()
    # message("The following rows differ from the median interval time:")
    which(x != median(x)) %>% print() # ID rows that return FALSE
  }
}

# Test
evenSpaced(nonmon, 1)
evenSpaced(nonmon, 2)

# RespirE
getRO2closed(df, volume = 200, by = "time", start = 7, end = 30)
datatest(df)
