#' Import from sensor output file
#'
#' Automatically read and clean data from sensor output. The aim is to work with
#' most commercial DO sensors available in the market with little input from the
#' user. This is done by performing simple regular expressions to identify the
#' file source by extracting the first line of the file and matching unique
#' strings.
#'
#' @param path string. Path to file.
#'
#' @return a data frame object of time (absolute)
#'
#' @importFrom data.table data.table fread
#' @importFrom readr read_excel
#'
#' @export
#'
#' @examples
#' NULL
import_file <- function(path) {

  # Extract metadata to identify file type
  if (tools::file_ext(path) %in% c("txt", "TXT")) {
    con <- file(path, "r")
    id <- readLines(con, n = 1)
    close(con)
  } else if (tools::file_ext(path) %in% c("xls", "xlsx")) {
    id <- names(readxl::read_excel(path)[1])
  }

  # Create data.table object based on file detected
  if (grepl("MiniDOT", id, fixed = TRUE)) {
    #################
    # MiniDOT sensor
    #################
    cat("MiniDOT sensor detected.\n")
    # first read the file, and remove first row after column headers
    raw_df <- fread(path)[-1]
    # create timestamp
    timestamp <- data.table(time = format_time(raw_df[[2]]))
    # append timestamp
    df <- cbind(timestamp, raw_df)
    # if summarise is TRUE, select only time (absolute) and DO columns
    # if (summarise) df <- df[, c(1, 7)]
  } else if (grepl("OXY10v3", id, fixed = TRUE)) {
    #################
    # OXY10v3 sensor
    #################
    cat("PRESENS OXY10 detected.\n")
    raw_df <- fread(path, skip = 18)
    # create timestamp
    datetime <- paste(raw_df[[1]], raw_df[[2]])
    timestamp <- data.table(time = format_time(datetime, " mdyHMSp!"))
    df <- cbind(timestamp, raw_df)
    # if summarise is TRUE, select only time (absolute) and DO columns
    # if (summarise) df <- df[, c(1, 5)]
  } else if (grepl("Measurement Name", line1, fixed = TRUE)) {
    ###################################
    # Loligo microplate sensor (maybe)
    ###################################
    cat("Loligo Microplate Systwm detected.\n")
    df <- as.data.table(readxl::read_excel(path, skip = 12))
    # timestamp is already available in min
    # if (summarise) df <- df[, c()]
  } else {
    stop("Source file cannot be identified. Process stopped.")
  }

  #####
  # Other sensors will follow!
  #####

  return(df)
}
