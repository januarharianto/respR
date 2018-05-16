#' Import from sensor output file
#'
#' Automatically read and clean data from sensor output. The aim is to work with
#' most commercial DO sensors available in the market with little input from the
#' user. This is done by performing simple regular expressions to identify the
#' file source.
#'
#' Time data is also automatically formatted.
#'
#' @param path string. Path to file.
#'
#' @return a data frame object of time (absolute)
#'
#' @importFrom data.table data.table fread
#'
#' @export
#'
#' @examples
#' NULL
import_file <- function(path, summarise = FALSE) {

  # Extract metadata to identify file type
  con <- file(path, "r")
  id <- readLines(con, n = 1)
  close(con)

  if (grepl("MiniDOT", id, fixed = TRUE)) {
    #################
    # MiniDOT sensor
    #################
    # first read the file, and remove first row after column headers
    raw_df <- fread(path)[-1]
    # create timestamp
    timestamp <- data.table(time = format_time(raw_df[[2]]))
    # append timestamp
    df <- cbind(timestamp, raw_df)
    # if summarise is TRUE, select only time (absolute) and DO columns
    if (summarise) df <- df[, c(1, 7)]
  } else if (grepl("OXY10v3", id, fixed = TRUE)) {
    #################
    # OXY10v3 sensor
    #################
    raw_df <- fread(path, autostart = 18)
    # create timestamp
    datetime <- paste(raw_df[[1]], raw_df[[2]])
    timestamp <- data.table(time = format_time(datetime, " mdyHMSp!"))
    df <- cbind(timestamp, raw_df)
    # if summarise is TRUE, select only time (absolute) and DO columns
    if (summarise) df <- df[, c(1, 5)]
  } else {
    warning("Source file cannot be identified. Applying generic import.")
    id <- NULL
    df <- fread(path)
  }

  #####
  # Other sensors will follow!
  #####

  return(df)
}
