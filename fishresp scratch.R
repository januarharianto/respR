library(respR)
library(FishResp)
library(dplyr)


# FishResp ----------------------------------------------------------------

## load data
data(SMR.raw)
data(info)
data(pre)
data(post)

## apply background
SMR.clean <- correct.meas(info.data = info,
                          pre.data = pre,
                          meas.data = SMR.raw,
                          method = "pre.test")

QC.meas(SMR.clean, "Temperature")
QC.meas(SMR.clean, "Total.O2.phases")
QC.meas(SMR.clean, "Corrected.O2.phases")
QC.meas(SMR.clean, "Total.O2.chambers")
QC.activity(SMR.clean, compare = TRUE)

## calculate slopes
SMR.slope <- extract.slope(SMR.clean,
                           method = "all",
                           n.slope = 3,
                           r2=0.95,
                           length = 1200)

## convert
SMR <- calculate.MR(SMR.slope,
                    density = 1000,
                    plot.BR = F,
                    plot.MR.abs = F,
                    plot.MR.mass = F)

## reorder
smr_all <- dplyr::arrange(SMR, Ind, Phase)

head(smr_all)

# respR -------------------------------------------------------------------

## rearrange background data
bg_pre <- data.frame(pre[0001:1200,3],
                     pre[0001:1200,6])

## calculate bg rate
bg_r_pre <- calc_rate.bg(bg_pre, xcol = 1, ycol = 2)

## Extract specimen 1 data
spec_1_all <- SMR.raw[,c(13,4)]

## calculate rates for each replicate
spec_1_all_r <- calc_rate(spec_1_all,
                        from = seq(1, 27601, 1200),
                        to = seq(1200, 28800, 1200),
                        by = "row")

## adjust for background
spec_1_all_r_adj <- adjust_rate(spec_1_all_r, bg_r_pre)

## convert
spec_1_all_r_conv <- convert_rate(spec_1_all_r_adj,
                      time.unit = "s",
                      o2.unit = "mg/L",
                      output.unit = "mg/h/kg",
                      mass = 0.00186,
                      volume = 0.24814)


# Compare results ---------------------------------------------------------

# compare slopes - just specimen 1
# = match exactly
spec_1_all_r$rate - smr_all$Slope.with.BR[1:24]

# compare rates
# = very different
spec_1_all_r_conv$output
smr_all$MR.mass[1:24]

# ratio - consistent x3.57 difference
spec_1_all_r_conv$output/smr_all$MR.mass[1:24]






# Importing ---------------------------------------------------------------

library(respR)
library(FishResp)

SMR.path = system.file("extdata/stickleback/SMR_raw.txt.xz", package = "FishResp")

input.info(ID = c(NA, NA, NA, NA, NA, NA, NA, NA),
           Mass = c(NA, NA, NA, NA, NA, NA, NA, NA),
           Volume = c(NA, NA, NA, NA, NA, NA, NA, NA),
           DO.unit = c("mg/L", "mmol/L", "ml/L"))


## create experimental metadata
info <- input.info(ID = c("spec1", NA, NA, NA, NA, NA, NA, NA),
                   Mass = c(3, NA, NA, NA,  NA, NA, NA, NA),
                   Volume = c(1, NA, NA, NA,  NA, NA, NA, NA),
                   DO.unit = c("mg/L"))

## import raw data file
SMR.raw <- import.meas(file = "~/Documents/Work/Projects/respR ms/Example Files/AutoResp/Anchovy/anch01_raw_test.txt",
                       info.data = info,
                       logger = "AutoResp",
                       n.chamber = 8,
                       date.format = "DMY",
                       start.measure = "10:10:00",
                       stop.measure = "16:00:00",
                       plot.temperature = F,
                       plot.oxygen = F)

raw_respR <- import_file(
  "/Users/nicholascarey/Documents/Work/Projects/respR ms/Example Files/AutoResp/Squid/squid4_022017_raw.txt")

raw_respR <- import_file(
  "/Users/nicholascarey/Documents/Work/Projects/respR ms/Example Files/AutoResp/Anchovy/anch01_raw_test.txt")




# comparing subsetting of reps --------------------------------------------

temp<- data.frame(SMR.raw$Date.Time, SMR.raw$Ox.1)
temp2 <- respR::format_time(temp, format = "ymdHMS")
names(temp2) <- c("Time", "O2")
inspect_data(temp2)
temp2[1190:1210,]

tail(temp2)
tail(SMR.raw)


## last 600
respr_r <-  calc_rate(temp2,
                      from = seq(601, 28201, 1200),
                      to = seq(1200, 28800, 1200),
                      by = "row")
## first 600
respr_r <-  calc_rate(temp2,
                      from = seq(1, 27601, 1200),
                      to = seq(600, 28200, 1200),
                      by = "row")

## all 1200
respr_r <-  calc_rate(temp2,
                      from = seq(1, 27601, 1200),
                      to = seq(1200, 28800, 1200),
                      by = "row")


# respr -------------------------------------------------------------------


## calculate rates for each replicate
respr_r <- calc_rate(temp2,
                     from = seq(1, 27601, 1200),
                     to = seq(1200, 28800, 1200),
                     by = "row")

## adjust for background
respr_r_adj <- adjust_rate(respr_r, bg_r_pre)

## convert
respr_r_adj_conv <- convert_rate(respr_r_adj,
                                 time.unit = "s",
                                 o2.unit = "mg/L",
                                 output.unit = "mg/h/kg",
                                 mass = 0.00186,
                                 volume = 0.25)


##
## apply background
SMR.clean <- correct.meas(info.data = info,
                          pre.data = pre,
                          meas.data = SMR.raw,
                          method = "pre.test")

## calculate slopes
SMR.slope <- extract.slope(SMR.clean,
                           method = "all",
                           n.slope = 3,
                           r2=0.95,
                           length = 600)

## convert
SMR <- calculate.MR(SMR.slope,
                    density = 1000,
                    plot.BR = F,
                    plot.MR.abs = F,
                    plot.MR.mass = F)

## reorder
smr_all <- dplyr::arrange(SMR, Ind, Phase)


round(respr_r$rate - smr_all$Slope.with.BR[1:24],10)


respR::unit_args()

## 10% quantile cutoff value
c <- quantile(rmr_rates$rmr_adj, 0.9)
## rates above cutoff value
r <- rmr_rates$rmr_adj[rmr_rates$rmr_adj > c]
## RMR
zeb_rmr_adj <- mean(r)

convert_rate(zeb_rmr_adj,
                          o2.unit = "mg/l",
                          time.unit = "s",
                          output.unit = "mg/h/g",
                          mass = 1,
                          volume = 0.1)
rmr_final
