#' Import from sensor output file
#'
#' Automatically read and clean data from sensor output. The aim is to work with
#' most commercial DO sensors available in the market with little input from the
#' user. This is done by performing simple regular expressions to identify the
#' file source by extracting the first line of the file and matching unique
#' strings. It's a simple procedure for now, but once we have a large database
#' of files we will optimise the code.
#'
#' @param path string. Path to file.
#'
#' @return a data frame object of time (absolute)
#'
#' @importFrom data.table data.table fread
#' @importFrom readxl read_excel
#'
#' @export
#'
#' @examples
#' NULL
import_file <- function(path) {

  # Extract metadata to identify file type
  # Once file is identified, extract the first line for ID
  if (tools::file_ext(path) %in% c("txt", "TXT")) {
    ## support for text files - maybe .log files fit in here too?
    # TODO
    con <- file(path, "r")
    id <- readLines(con, n = 1)
    close(con)
  } else if (tools::file_ext(path) %in% c("xls", "xlsx")) {
    ## support for microsoft excel files
    id <- names(read_excel(path)[1])
  }

  # Create data.table object based on text in first line
  if (grepl("MiniDOT", id, fixed = TRUE)) {
    ###########################################
    # PME MiniDOT sensor (TXT file)
    ###########################################
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
    ###########################################
    # PRESENS OXY10v3 sensor (TXT file)
    ###########################################
    cat("PRESENS OXY10 detected.\n")
    raw_df <- fread(path, skip = 18)
    # create timestamp
    datetime <- paste(raw_df[[1]], raw_df[[2]])
    timestamp <- data.table(time = format_time(datetime, " mdyHMSp!"))
    df <- cbind(timestamp, raw_df)
    # if summarise is TRUE, select only time (absolute) and DO columns
    # if (summarise) df <- df[, c(1, 5)]
  } else if (grepl("Measurement Name", id, fixed = TRUE)) {
    ###########################################
    # Loligo microplate sensor (EXCEL OUTPUT)
    ###########################################
    cat("Loligo Microplate Systwm detected.\n")
    df <- as.data.table(read_excel(path, skip = 12))
    # timestamp is already available in min
    # if (summarise) df <- df[, c()]
  } else {
    stop("Source file cannot be identified. Please contact the developers with a sample of your file. Process stopped.")
  }

  #####
  # Other sensors will follow!
  # TODO
  #
  # Firesting
  # Vernier
  # More Loligo
  #
  #####

  return(df)
}




# MiniDOT
parse_minidot <- function(path) {
  txt <- readLines(path)
  raw <- fread(path, fill = TRUE)
  colstart <- suppressWarnings(raw[raw$V1 %like% "Unix", which = TRUE])
  colnames <- as.matrix(raw[colstart])[1,]
  rdt <- fread(path, skip = colstart-1)[-1]
  elapsed <- format_time(rdt[[2]])
  out <- data.table(elapsed, rdt)
  return(out)
}


# PRESENS OXY10
parse_oxy10 <- function(path) {
  txt <- readLines(path)
  raw <- fread(path, fill = TRUE)
  colstart <- suppressWarnings(raw[raw$V1 %like% "Date/", which = TRUE])
  rdt <- fread(path, fill = TRUE, skip = colstart-1, header = TRUE)[,1:4]
  out <- setnames(rdt, 1:4, c("date", "time", "elapsed", "o2"))
  return(out)
}

# Firesting
parse_firesting <- function(path) {
  txt <- readLines(path)
  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE)
  startdate <- raw[16]$V2                                                       # Extract start date
  colstart <- which(raw$V1 == "Time")[1]                                        # Identify main column names
  rdt <- fread(path, fill = TRUE, skip = colstart+1, header = TRUE)             # Import and clean data
  rdt <- Filter(function(x)!all(is.na(x)||is.null(x)||x == ""||x == 0), rdt)    # Remove empty columns
  # Remove unnecessary rows containing extra headers and metadata
  # We use "Time" as our landmark since it is a header, and delete around it
  landmark <- which(rdt$Time == "Time")
  cull <- unlist(lapply(landmark, function(i) seq((i-18), i)))                  # Mark rows to cull
  rdt <- rdt[-cull]                                                             # Delete those rows
  # Now we need to deal with the time.
  rdt <- data.table(startdate, rdt)                                             # First, we add a start date
  rdt <- data.table(timestamp = with(rdt, as.POSIXct(paste(as.Date(startdate,   # We convert time column to datetime
                                                                   "%d/%m/%Y"), Time))), rdt[,-c(1,2)])
  # Now we adjust the dates properly since they're all one date
  diffday <- c(0, which(diff(rdt$timestamp) < 0), nrow(rdt))                    # create index
  newdates <- do.call("c", sapply(1:(length(diffday) - 1), function(x) {        # create new dates
    rdt[(diffday[x] + 1):diffday[x + 1]][[1]] + days(x - 1)
  }))
  # rdt[, timestamp := newdates]                                                  # replace timestamp column with new dates
  elapsed <- format_time(as.character(newdates))
  out <- data.table(timestamp = newdates, elapsed, rdt[,-1])
  return(out)
}

parse_pyro <- function(path) {
  txt <- readLines(path)
  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE)
  startdate <- raw[16]$V2                                                       # Extract start date
  colstart <- which(raw$V1 == "Date")[1]                                        # Identify main column names
  rdt <- fread(path, fill = TRUE, skip = colstart+1, header = TRUE)             # Import and clean data
  rdt <- rdt[, which(unlist(lapply(rdt, function(x) !all(is.na(x)||x == ""||x == "---")))), with = FALSE]
  out <- data.table(rdt)                                                        # need to figure out why I did this
  rdt
}