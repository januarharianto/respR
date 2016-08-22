# Moving Average
# Authors: Januar Harianto, Nicholas Carey
#
# This script performs a moving average on a metabolic rate dataset, useful
# to clean up messy data.
#


# Load required libraries in script
# -----------------------------------------------------------------------------
library(readr)  # read tabular data properly - for difficult .csv files
library(ggplot2)  # graphing package
library(cowplot)  # produce print-ready plots
library(zoo)  # indexing and averaging

# Configuration settings
# -----------------------------------------------------------------------------
width <- 10  # smoothing factor
raw.plot <- read.csv("~/Documents/RESPA05.1.csv")  # load file into data frame

# Perform moving average
# -----------------------------------------------------------------------------
# align options : "left", "right", "center"
# 'fill = NA' automatically pads missing head/tail of data range with 'NA'
smoothed.plot <- data.frame(Time = raw.plot$Time,
                            O2 = rollmean(  # this is from the zoo package
                                  x = raw.plot[, 2],
                                  k = width,  # smoothing factor
                                  fill = NA,
                                  align = "left"))
# Plot graphs
# -----------------------------------------------------------------------------
rp <-  # define raw plot
  ggplot(raw.plot, aes(Time, O2)) + 
    geom_point(size = 0.1, na.rm=TRUE, colour = "purple") + 
    ggtitle("Raw Plot")

sp <-  # define smoothed plot
  ggplot(smoothed.plot, aes(Time, O2)) + 
    geom_point(size = 0.1, na.rm=TRUE) + 
    ggtitle("Smoothed Plot")

cp <-  # define combined raw + smoothed plot
  ggplot(NULL, aes(Time, O2)) +
    geom_point(data = raw.plot,
               na.rm = TRUE,
               colour = "purple") +
    geom_point(data = smoothed.plot,
               na.rm = TRUE,
               size = 0.1,
               colour = "black") +
    ggtitle("Combined Plot")

plot_grid(rp, sp, cp, ncol = 1, nrow = 3)  # grid plots to one screen