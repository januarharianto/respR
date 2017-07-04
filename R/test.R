
# Reset everything before we begin
closeAllConnections()   # self-explanatory
rm(list=ls())           # remove all variables in global environment
cat("\014")             # clear the console

# Load my usual packages and functions
source("~/Dropbox/R/functions/allFunctions.R")
loadPackage("tidyverse")  # better code
loadPackage("cowplot")    # scientific theme for ggplot2
loadPackage("viridis")    # colour scheme for ggplot2

loadPackage("LoLinR")
# Check working directory
setwd("~/Documents/Dev/respiration-R-package/")

# Load data --------------------------------------------------------------------
df <- readr::read_csv("data/smallset.csv")
df
ggplot(df, aes(`time (min)`,`1`)) + geom_point()
nonmon <- read_csv("data/non_monotonic.csv") %>% print()

# just testing lolinr
urchinRegs  <-  rankLocReg(xall=df$`time (min)`, yall=df$`1`, alpha=0.8,
  method="eq", verbose=TRUE)
urchinRegs2 <- reRank(urchinRegs, newMethod="pc")
summary(urchinRegs)
summary(urchinRegs2)

plot(urchinRegs)

# Create new environment -------------------------------------------------------
# This creates a new environment to store variables that are hidden to the user
.myenv <- new.env()

# prepData function ------------------------------------------------------------

prepData <- function(df, plot=F) {
  # rename for better ID
  names(df) <- c("x", "y")
  # prompt user for input
  message("Input dataframe units:")
  o2time <<- as.character(readline("Unit of time (e.g. s, min):"))
  o2unit <<- as.character(readline("Unit of O2 conc, (e.g. mgL-1, ug/kg:"))
  # Checks
  message("Performing tests:")
  cat("Checking for missing data...\n")
  xNA <- hasNA(df,1)
  yNA <- hasNA(df,2)
  cat("Checking time data for duplicates...\n")
  dup <- dupes(df)
  cat("Checking if O2 data are monotonic...\n")
  mon <- monotonic(df)
  cat("Checking if time data are not evenly spaced...\n")
  spa <- evenSpaced(df)
  # Summary
  results <- list(c(xNA, yNA, dup, mon, spa))
  # All checks passed
  .resprdf <<- df
  message("New dataframe generated.")
  #results
  cat("Summary:\n")
# if (plot)
  #   return(ggplot(df, aes(x, y)) + geom_point())
}

prepData(df)
prepData(nonmon)
.resprdf

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


# RespirE
getRO2closed(df, volume = 200, by = "time", start = 7, end = 30)
datatest(df)

#
