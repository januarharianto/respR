#' Check data for issues
#'@export
prepareData <- function(df, plot=F) {
  names(df) <- c("x", "y")  # rename for better ID
  # Checks
  # message("Performing tests:")
  # cat("Checking for missing data...\n")
  xNA    <- hasNA(df,1)
  yNA    <- hasNA(df,2)
  # cat("Checking time data for duplicates...\n")
  dup    <- dupes(df)
  # cat("Checking if O2 data are monotonic...\n")
  mono   <- monotonic(df)
  # cat("Checking if time data are not evenly spaced...\n")
  spaced <- evenSpaced(df)
  # Summary
  message("Summary:")
  cat("Missing values in time data (x):    ", xNA, "\n")
  cat("Missing values in O2 data (y):      ", yNA,"\n")
  cat("Duplicates in time data (x):        ", dup, "\n")
  cat("O2 data (y)is monotonic throughout: ", mono, "\n")
  cat("Time data (x) is evenly spaced:     ", spaced, "\n")
if(dup == T)
  stop('There are duplicate time values.
    Functions require all time values to be unique.')
# All checks passed
  #results
  # if (plot)
  #   return(ggplot(df, aes(x, y)) + geom_point())
  else
  message("New dataframe generated.")
  return(df)
}
