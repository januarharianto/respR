### Load data

rawdata <- read.csv("RESPA05.1.csv")

plot(rawdata[,1], rawdata[,2])

### set exp parameters

exptype <- "intermittent" #also open, closed, flowthrough
timeunit <- "s" #also min, hr
O2unit <- "mg/L" #also umol, %, mL ...
massunit <- "g" #also mg, kg
masstype <- "ash-free dry" #also total, wet, dry, tissue
volunit <- "L" #also ml, μl?
flowunit <- "ml/s" #also ml/min, ml/h, L/s, L/min, L/h, only for flowthrough
fluxunit <- "mg/h" #only for open.

## set exp data

specimen <- "HC14" #unique specimen ID
mass <- 0.3197
volume <- 0.257
reps <- 3 #replicates within these data #REQUIRED
    ## if reps >1, have to enter start and end times for each rep
    repstarts <- c(0,2100,3750)
    repends <- c(1800,3500,4800)
flowrate <- NULL #only for flowthrough
flux <- NULL #only for open. Estimated input of O2 at surface of open containers 

