#' Check data for issues
#'@export
prepareData <- function(df, plot = F) {
  names(df) <- c("x", "y")  # rename for better ID
  # Checks
  # message("Performing tests:")
  # cat("Checking for missing data...\n")
  xNA    <- hasNA(df, 1)
  yNA    <- hasNA(df, 2)
  if (xNA == T | yNA == T)  # automatically remove NAs if detected
  df <- na.omit(df)
  n <- length(attributes(df)$na.action)
  m <- length(attributes(df)$row.names)

  dup    <- dupes(df)        # duplicated time
  mono   <- monotonic(df)    # monotonic
  spaced <- evenSpaced(df)   # evenly-spaced

  # Summary
  message("Summary:")
  cat("Missing values in time data (x):    ", xNA, "\n")
  cat("Missing values in O2 data (y):      ", yNA, "\n")
  cat("Duplicates in time data (x):        ", dup, "\n")
  cat("O2 data (y)is monotonic throughout: ", mono, "\n")
  cat("Time data (x) is evenly spaced:     ", spaced, "\n")
  if (dup == T)
    stop('There are duplicate time values.
      Functions require all time values to be unique. Process aborted.')
  if (xNA == T | yNA == T)
    message(sprintf('%i rows removed due to missing values. ', n),
      sprintf('Data reduced to %i ', m),
      sprintf('rows (from %i).', m + n))
  else
    message("New dataframe generated.")
  return(df)
}
