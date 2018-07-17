#' Import from sensor output file
#'
#' Automatically read and clean data from sensor output. The aim is to work with
#' most commercial DO sensors available in the market with little input from the
#' user. This is done by performing simple regular expressions to identify the
#' file source by extracting the first line of the file and matching unique
#' strings. It's a simple procedure for now, but once we have a large database
#' of files we will optimise the code.
#'
#' Currently works for: Firesting Logger | Pyro Oxygen Logger (also Firesting) |
#' PRESENS OXY10 | PRESENS (generic) | MiniDOT | Loligo Witrox Logger | Loligo
#' AutoResp (software output)
#'
#' While the devices listed above are supported, the import functionality may
#' not be complete due to limited access to output files. This will improve over
#' time as users provide feedback. We are releasing this as it is, without any
#' warranty, so that some people can still benefit from the functionality as it
#' gets better. Users should still be expected to be able to import data by
#' themselves since it is a fundamental skill in R.
#'
#' @param path string. Path to file.
#' @param export logical. If TRUE, saves the file in the same directory,
#'   determined by the path parameter above.
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
import_file <- function(path, export = FALSE) {
  raw <- readLines(path)

  # Identify source of file
  if (suppressWarnings(any(grepl("Firesting", raw[1:20])))) {
    cat("Firesting Logger Detected\n\n")
    out <- parse_firesting(path)
  } else if (suppressWarnings(any(grepl("Pyro", raw[1:20])))) {
    cat("Pyro Oxygen Logger Detected\n\n")
    out <- parse_pyro(path)
  } else if (suppressWarnings(any(grepl("OXY10", raw[1:20])))) {
    cat("PRESENS OXY10 Detected\n\n")
    out <- parse_oxy10(path)
  } else if (suppressWarnings(any(grepl("MiniDOT", raw[1:20])))) {
    cat("MiniDOT Logger Detected\n\n")
    out <- parse_minidot(path)
  } else if (suppressWarnings(any(grepl("CALIBRATION DATA", raw[1:20])))) {
    cat("Loligo Witrox Logger Detected\n\n")
    out <- parse_witrox(path)
  } else if (suppressWarnings(any(grepl("Fractional error", raw[1:20])))) {
    cat("Loligo AutoResp Output Detected\n\n")
    out <- parse_autoresp(path)
  } else if(suppressWarnings(any(grepl("MUX channel", raw[10:30]))) &&
            suppressWarnings(any(grepl("PARAMETERS", raw[10:30]))) &&
            suppressWarnings(any(grepl("FIRMWARE", raw[30:50])))) {
    cat("PRESENS Generic file detected\n\n")
    out <- parse_presens(path)
  } else stop("Source file cannot be identified. Please contact the developers with a sample of your file. Process stopped.")

  if(export) {
    newpath <- paste(normalizePath(dirname(path)),"/", "parsed-",
                     basename(path), sep = "")
    write.csv(out, newpath)
  }

  return(out)
}



# Invividual device functions ----------

# Loligo AutoResp
parse_autoresp <- function(path) {
  raw <- fread(path, fill = TRUE, header = FALSE)
  colstart <- tail(suppressWarnings(raw[raw$V1 %like% "Date", which = TRUE]), 1)
  rdt <- fread(path, fill = TRUE, skip = colstart)
  timestamp <- rdt[[1]]
  rdt <- rdt[, which(unlist(lapply(rdt, function(x)
    !all(is.na(x)||x == ""||x == "---"||is.character(x))))), with = FALSE]
  rdt <- setnames(rdt, c("time", "loop", "phase", "slope", "r.squared", "max.o2", "min.o2", "avg.o2", "temp"))
  rdt[, time := rdt$time - min((rdt$time))]
  out <- data.table(rdt, timestamp)
  return(out)
}

# Witrox
parse_witrox <- function(path) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE, header = FALSE)
  # detect start column:
  colstart <- tail(suppressWarnings(raw[raw$V1 %like% "Date", which = TRUE]), 1)
  rdt <- fread(path, fill = TRUE, skip = colstart, colClasses = c(V2 = "character"))
  rdt <- setnames(rdt, c("datetime", "time", "pressure", "phase", "temp", "oxygen"))
  rdt[, time := as.numeric((rdt$time)) - min(as.numeric((rdt$time)))]
  data.table::setcolorder(rdt, 2) # set time as first column
  out <- data.table(rdt)
  return(out)
}


# MiniDOT
parse_minidot <- function(path) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE)
  colstart <- suppressWarnings(raw[raw$V1 %like% "Unix", which = TRUE])
  # colnames <- as.matrix(raw[colstart])[1,]
  rdt <- fread(path, skip = colstart-1)[-1]
  elapsed <- format_time(rdt[[2]])
  out <- data.table(elapsed, rdt)
  return(out)
}


# PRESENS OXY10
parse_oxy10 <- function(path) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE)
  # colstart <- suppressWarnings(raw[raw[,1] %like% "Date/", which = TRUE])
  rdt <- fread(path, fill = TRUE, skip = 37, header = TRUE)[,1:4]
  out <- setnames(rdt, 1:4, c("date", "time", "elapsed", "o2"))
  data.table::setcolorder(out, 3)
  return(out)
}

# Firesting
parse_firesting <- function(path) {
  # txt <- readLines(path)
  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE)
  startdate <- raw[16]$V2 # Extract start date
  colstart <- which(raw$V1 == "Time")[1] # Identify main column names
  rdt <- fread(path, fill = TRUE, skip = colstart+1, header = TRUE)
  rdt <- rdt[, which(unlist(lapply(rdt, function(x)
    !all(is.na(x)||x == ""||x == "---")))), with = FALSE]
  # Remove unnecessary rows containing extra headers and metadata
  # We use "Time" as our landmark since it is a header, and delete around it
  landmark <- which(rdt$Time == "Time")
  # Mark rows to cull
  cull <- unlist(lapply(landmark, function(i) seq((i-18), i)))
  rdt <- rdt[-cull] # Delete those rows
  # Now we need to deal with the time.
  rdt <- data.table(startdate, rdt) # First, we add a start date
  # We convert time column to datetime
  rdt <- data.table(timestamp = with(rdt, as.POSIXct(paste(as.Date(startdate,
                      "%d/%m/%Y"), Time))), rdt[,-c(1,2)])
  # Now we adjust the dates properly since they're all one date
  diffday <- c(0, which(diff(rdt$timestamp) < 0), nrow(rdt)) # create index
  # create new dates
  newdates <- do.call("c", sapply(1:(length(diffday) - 1), function(x) {
    rdt[(diffday[x] + 1):diffday[x + 1]][[1]] + lubridate::days(x - 1)
  }))
  elapsed <- format_time(as.character(newdates))
  out <- data.table(timestamp = newdates, elapsed, rdt[,-1])
  return(out)
}

# Firesting Pyro
parse_pyro <- function(path) {
  # txt <- readLines(path)
  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE)
  startdate <- raw[16]$V2
  colstart <- which(raw$V1 == "Date")[1]
  rdt <- fread(path, fill = TRUE, skip = colstart+1, header = TRUE)
  rdt <- rdt[, which(unlist(lapply(rdt, function(x)
    !all(is.na(x)||x == ""||x == "---")))), with = FALSE]
  out <- data.table(rdt)
  data.table::setcolorder(out, 3)
  return(out)
}

# Generic PRESENS file
parse_presens <- function(path) {
  raw <- fread(path, fill = TRUE, header = FALSE)
  colstart <- suppressWarnings(raw[raw$V1 %like% "Date/", which = TRUE])
  rdt <- fread(path, skip = colstart)
  # Identify columns with numbers
  valids <- rdt[, which(unlist(lapply(rdt, function(x)
    !all(is.na(x)||x == ""||x == "---"||is.character(x)))))]

  # Identify column names
  colid <- unlist(raw[colstart])
  colid <- colid[valids]
  colid <- gsub(";", "", colid)
  # Remove empty channels
  rdt <- rdt[, which(unlist(lapply(rdt, function(x)
    !all(is.na(x)||x == ""||x == "---"||is.character(x))))), with = FALSE]
  out <- setnames(rdt, colid) # rename column headers
  return(out)
}
