#' @export
prepData <- function(df, plot=F) {
  names(df) <- c("x", "y")  # rename for better ID
  # prompt user for input
  # message("Input dataframe units:")
  # o2time <<- as.character(readline("Unit of time (e.g. s, min):"))
  # o2unit <<- as.character(readline("Unit of O2 conc, (e.g. mgL-1, ug/kg:"))
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
  out <- list(c(xNA, yNA, dup, mon, spa))
  # All checks passed
  .resprdf <<- df
  message("New dataframe generated.")
  #results
  cat("Summary:\n")
  # if (plot)
  #   return(ggplot(df, aes(x, y)) + geom_point())
}
