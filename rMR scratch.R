library(rMR)
library(respR)

## rMR
## load data ##
data(fishMR)
## create time variable in POSIXct format ##
fishMR$std.time <- as.POSIXct(fishMR$Date.time,
                              format = "%d/%m/%Y %I:%M:%S %p")

## set start and end of each data region to calculate rate over
starts <- c("2015-07-03 01:15:00", "2015-07-03 02:13:00",
            "2015-07-03 03:02:00", "2015-07-03 03:50:00",
            "2015-07-03 04:50:00")
stops <- c("2015-07-03 01:44:00", "2015-07-03 02:35:30",
           "2015-07-03 03:25:00", "2015-07-03 04:16:00",
           "2015-07-03 05:12:00")

## calculate and convert rates
metR <- MR.loops(data = fishMR, DO.var.name ="DO.mgL",
                 start.idx = starts, time.units = "hr",
                 stop.idx = stops, time.var.name = "std.time",
                 temp.C = "temp.C", elevation.m = 1180,
                 bar.press = NULL, in.DO.meas = "mg/L",
                 ylim=c(6, 8))

## respR
# extract time and o2 column, and format to numeric time starting at zero
fishMR_respR <- fishMR[, c(2,6)]
fishMR_respR[,1] <- fishMR_respR[,1] - fishMR_respR[1,1]

## calculate rates (same regions, expressed as row numbers)
rates_respR <- calc_rate(fishMR_respR,
                         from = c(33400, 36873, 39807, 42683, 46277),
                         to = c(35136, 38221, 41186, 44240, 47596),
                         by = "row")

## convert rates
rates_respR_conv <- convert_rate(rates_respR,
                                 time.unit = "s",
                                 o2.unit = "mg/L",
                                 volume = 1,
                                 output.unit = "mg/h")

## compare rates - both in mg/h
rates_respR_conv$output
metR$MR.summary$MR

