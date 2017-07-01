# Load my usual packages and functions
source("~/Dropbox/R/functions/allFunctions.R")
loadPackage("tidyverse")  # better code
loadPackage("lubridate")  # handles datetime
loadPackage("ggplot2")    # grammar of graphics (plots)
loadPackage("cowplot")    # scientific theme for ggplot2
loadPackage("viridis")    # colour scheme for ggplot2

# Set working directory


# Load additional libraries if needed

loadPackage("LoLinR")

# Load data
df <- read_csv("data/hery.csv") %>%
  select(time=1,o2=4) %>%
  print()
#
# Workflow ---------------------------------------------------------------------

start <- 7
end <- 30
# :oad and  subset data
full <- dplyr::rename_(df, x = names(df)[1], y = names(df)[2])
win  <- dplyr::filter(full, x >= start, x <= end)
# perform regression
reg  <- lm(y ~ x, win)

# Grab coefficients
slope     <- round(coef(reg)[2], 4)
intercept <- round(coef(reg)[1], 4)
rsq       <- round(summary(reg)$r.squared, 3)
arsq      <- round(summary(reg)$adj.r.squared, 3)

# Generate tables
vrs <- matrix(c(start, end), ncol = 2, byrow = T) %>%
  `colnames<-`(c("Start", "End")) %>%
  `rownames<-`("Time") %>%
  as.table() %>% print()

cat('\n')

out <- matrix(c(slope, intercept, rsq, arsq),ncol = 4, byrow = F) %>%
  `colnames<-`(c("Slope", "Intercept", "R-sq.", "Adj. R-sq")) %>%
  `rownames<-`("") %>%
  as.table() %>% print()

# Plot
plotFull <- ggplot(full, aes(x, y)) +
  geom_point() +
  geom_point(data = win, aes(x ,y), colour = "yellow")
plotWin <- ggplot(win, aes(x, y)) +
  geom_point() +
  geom_label(data = subset(win, x == min(x) | x == max(x)),
            aes(x, y, label = x))
plot_grid(plotFull, plotWin, nrow = 2)


# prepData function ------------------------------------------------------------

prepData <- function(df) {
  df <- dplyr::rename_(df, x = names(df)[1], y = names(df)[2])
  # Plot
  ggplot(df, aes(x, y)) + geom_point()

  rtime <- as.character(readline("Please specify unit of time, e.g. s, min, h:"))
  o2unit <- as.character(readline("Please specify O2 unit (e.g. mgL-1, ug/kg:"))
  # spit out values
  resprdf <<- df
  rtime <<- rtime
  o2unit <<- o2unit
  df
}

getRate <- function(start, end, df = resprdf) {
  win  <- dplyr::filter(df, x >= start, x <= end)
  win
}


prepData(df)
getRate(10,50)

# getRate function -------------------------------------------------------------

getRate <- function(start, end, df = resprdf) {
  # load andsubset data
  win  <- dplyr::filter(df, x >= start, x <= end)
  # perform regression
  reg  <- lm(y ~ x, win)
  reg

  # Grab coefficients
  slope     <- round(coef(reg)[2], 4)
  intercept <- round(coef(reg)[1], 4)
  rsq       <- round(summary(reg)$r.squared, 3)
  arsq      <- round(summary(reg)$adj.r.squared, 3)

  # Generate tables
  vrs <- matrix(c(start, end), ncol = 2, byrow = T) %>%
    `colnames<-`(c("Start", "End")) %>%
    `rownames<-`("Time") %>%
    as.table() %>% print()

  cat('\n')

  out <- matrix(c(slope, intercept, rsq, arsq),ncol = 4, byrow = F) %>%
    `colnames<-`(c("Slope", "Intercept", "R-sq", "Adj. R-sq")) %>%
    `rownames<-`("") %>%
    as.table() %>% print()

  # Plot
  plotFull <- ggplot(df, aes(x, y)) +
              geom_point() +
              geom_point(data = win, aes(x ,y), colour = "yellow")
  plotWin <- ggplot(win, aes(x, y)) +
             geom_point()
  plot_grid(plotFull, plotWin, nrow = 2)
}

# Others -----------------------------------------------------------------------
getRate(10,30)

# lolinr stuff


#get RO2 stuff

getRO2closed(df, volume = 200, by = "time", start = 7, end = 30)
