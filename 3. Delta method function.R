## to do - time must match exactly - no entering intermediate times - add warning - DONE
        ## warning for chosen time and/or O₂ start and end not being in bounds - DONE
        ## warning for if chosen time start/end not within the repID - DONE
        ## DONE BUT CHECK - yes, seems to work ok

## x = df of Time and O2
## start, end = interval to calculate uptake over - either oxygen or time
## by = sets if intervals are oxygen or time
## repID = current rep being calculated - function ignores it if reps = 1 or left NULL
## plot - plots results for rough guide


## define function, and default arguments
deltamethod <- function(x, start = 100, end = 90, by = "oxygen", repID=NULL, plot=TRUE){
  
  ## Error checking
  
  ## check 'by' has been defined correctly
  if(by != "time" && by != "oxygen") {
    warning("The 'by' operator has not been correctly defined")
    return("NA")
  }
  
  ## check start and end are in correct order
  if(by == "time"){
    if(start > end){
      warning("End time is earlier than start time")
      return("NA")}}
  if(by == "oxygen"){
    if(start < end){
      warning("End O2 value is greater than start O2 value")
      return("NA")}}
  
  ## check 'reps' exists (required even if 1 for single rep) or code breaks
  if(exists("reps") == FALSE) {
    warning("Number of reps has not been defined. Must be defined even if 1")
    return("NA")
  }
  
  ## check repID is not greater than reps
  if(repID > reps){
    warning("Current repID is greater than defined number of reps")
    return("NA")
  }
  
  ## check for duplicate times
  if(anyDuplicated(x$Time)){
    warning("There are duplicate time values. Function requires all time values to be unique")
    return("NA")
  }
  
  ## check for multiple reps but no current rep defined, and if so display a warning and return "NA"
  if(reps > 1 && is.null(repID)){
    warning("Multiple replicates have been defined, therefore repID operator is required.")
    return("NA")} 
  
  ## check if entered times are within range 
    ## in range of all data for reps = 1, or repID = NULL
    if(by == "time") {if(reps == 1 || is.null(repID)){
      if((start > max(x$Time) || start < min(x$Time)) || (end > max(x$Time) || end < min(x$Time))){
        warning("Selected start and/or end times are out of bounds of data")
        return("NA")}
    ## or in range of the selected rep
      } else {
        if(reps > 1){
            startcheck <- repstarts[repID]
            endcheck <- repends[repID]
            if((start > endcheck || start < startcheck) || (end > endcheck || end < startcheck)){
              warning("Selected start and/or end times are out of bounds of this rep")
              return("NA")  
            }}}} 
      
  ## check if entered O2 values are within range    
      if(by == "oxygen") {if(reps == 1 || is.null(repID)){
        if((start > max(x$O2) || start < min(x$O2)) || (end > max(x$O2) || end < min(x$O2))){
          warning("Selected start and/or end O2 values are out of bounds of data")
          return("NA")}
        } else {
          if(reps > 1){
              O2range <- range(x$O2[repstarts[repID]:repends[repID]])
              #startcheck <- repstarts[i]
              #endcheck <- repends[i]
              if((start > O2range[2] || start < O2range[1]) || (end > O2range[2] || end < O2range[1])){
                warning("Selected start and/or end O2 values are out of bounds of this rep")
                return("NA")  
              }}}}
  
  ## If all above checks passed - then proceed to calculations
  
  ## check if repID left as NULL - i.e. default - only one rep
  if(reps == 1 || is.null(repID)) {
  ## if so - carry on
      if(by == "oxygen") {
        ## Finds closest matching O2 value and returns Time and ACTUAL O2 conc at that time
        starttime <- x[min(which(abs(x$O2-start)==min(abs(x$O2-start)))), 1]
        endtime <- x[min(which(abs(x$O2-end)==min(abs(x$O2-end)))), 1]
        startconc <- x[min(which(abs(x$O2-start)==min(abs(x$O2-start)))), 2]
        endconc <- x[min(which(abs(x$O2-end)==min(abs(x$O2-end)))), 2]
        
        ## rate calculation - currently output unit is O2 unit/time unit
        output <- ((startconc*volume)-(endconc*volume))/(endtime-starttime)
        
      } else if(by == "time"){
        ## these two only here for plotting function below
        starttime <- start
        endtime <- end
        ## Finds O2 conc at start & end times
        startconc <- x[match(start, x$Time),2]
        endconc <-x[match(end, x$Time),2]
        
        output <- ((startconc*volume)-(endconc*volume))/(endtime-starttime)
      }
    ## if multiple reps
  } else if(reps > 1){
      ## subset current rep into new df
      repdata <- subset(x, Time >= repstarts[repID] & Time <= repends[repID])
      
      if(by == "oxygen") {
        starttime <- repdata[min(which(abs(repdata$O2-start)==min(abs(repdata$O2-start)))), 1]
        endtime <- repdata[min(which(abs(repdata$O2-end)==min(abs(repdata$O2-end)))), 1]
        startconc <- repdata[min(which(abs(repdata$O2-start)==min(abs(repdata$O2-start)))), 2]
        endconc <- repdata[min(which(abs(repdata$O2-end)==min(abs(repdata$O2-end)))), 2]
        
        output <- ((startconc*volume)-(endconc*volume))/(endtime-starttime)
        
      } else if(by == "time"){
        ## these two only here for plotting function below
        starttime <- start
        endtime <- end
        ## match start time and return O2 conc at that time
        startconc <- repdata[match(start, repdata$Time),2]
        endconc <-repdata[match(end, repdata$Time),2]
        
        output <- ((startconc*volume)-(endconc*volume))/(endtime-starttime)
      }
  }  
  ## plot original data with delta range used for this calculation
  if(plot == TRUE){
    plot(x[,1], x[,2], pch=".", panel.first = segments(starttime, startconc, endtime, endconc, col="yellow", lwd=10), 
         xlab=colnames(x[1]), ylab=colnames(x[2]))
  }
  
  ## this is here because the plot above hijacks the output somehow
  
  ## Check output is ok
  if(is.na(output)) {
    warning("Output not defined. Check input parameters, particularly that 'start' and 'end' are in the correct units for 'by'")
    return("NA")
  }
  
  ## return output
  return(output)
  }
  

## Test
temp <- deltamethod(data, start = 100, end = 1000, by = "time", repID = 1)

