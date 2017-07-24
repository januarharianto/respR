#' @export
prepare.data <- function(df, col1 = 1, col2 = 2, plot = F){
  # select the columns and bind to dataframe
  # if columns are not specified, pick column 1 and 2
  x <- df[[col1]]
  y <- df[[col2]]
  df <- data.frame(x, y)
  # perform checks:
  c.length <- check.length(x, y)     # check equal length for x and y
  cat("Columns are of equal length:  ", c.length$check, "\n")
  if (c.length$check == F) stop("Processing stopped. Data columns are not of equal length.\nDataframe NOT generated.", call. = F)

  c.class <- numeric.datetime(x, y)  # check that x is numeric/time data, and y is numeric data
  cat("Data are formatted correctly: ", any(c.class), "\n")
  if (sum(c.class) < 2) stop("Processing stopped. Time must be numeric/date, and DO must be numeric.\nDataframe NOT generated.", call. = F)

  c.NA <- has.NA(df)                 # check for NA
  cat("No missing values (NA):       ", !c.NA$check, "\n")

  c.dupe <- dupes(df)                # check for duplicated x
  cat("No duplicated time data:      ", !c.dupe$check, "\n")
  if (!c.dupe$check == F) stop("Processing stopped. Duplicate time data detected.\nDataframe NOT generated.", call. = F)

  # c.mono <- monotonic(y)  # check for monotonic y. not really working. Spearman's test will check better?
  c.evensp <- even.spaced(x)         # check for evenly-spaced x
  cat("Time is evenly spaced:        ", c.evensp, "\n")

  # warning - data might still be useful:
  if (!c.NA$check == F)    message("Warning: NA values detected and automatically removed.")
  if (c.evensp == F)       message("Warning: Time data are not evenly-spaced.")

  # message output
  if (!c.NA$check == F | c.evensp == F) {
    cat("Subsetting by 'row' may cause errors. Please subset by method = 'time' when using calcRate.\n")
    message("Dataframe generated.")
    df <- na.omit(df) # remove NA values
  } else message("Dataframe generated.")

  # time to plot
  p1 <- ggplot(df, aes(x, y)) +
    geom_point() +
    theme_respr()
  print(p1)
  return(invisible(df))
}
