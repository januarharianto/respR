#' Import from sensor output file
#'
#' Automatically read and clean data from sensor output. The aim is to work with
#' most commercial DO sensors available in the market with little input from the
#' user. This is done by performing simple regular expressions to identify the
#' file source by extracting the first line of the file and matching unique
#' strings. It's a simple procedure for now, but once we have a large database
#' of files we will optimise the code.
#'
#' Currently works for:
#'  - Firesting Logger
#'  - Pyro Oxygen Logger (another name for Firesting)
#'  - PRESENS OXY10
#'  - PRESENS (generic)
#'  - MiniDOT
#'  - Loligo Witrox Logger
#'  - Loligo AutoResp (software output)
#'  - Loligo & Presens 24-Well Multiplate System (txt and Excel files)
#'  - Vernier (raw gmbl/qmbl, csv, txt)
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
#' @importFrom xml2 xml_text
#' @importFrom xml2 read_html
#'
#' @export
#'
#' @examples
#' NULL
import_file <- function(path, export = FALSE) {

  ## readLines doesn't work on xlsx files Have to do Excel import here - may not
  ## be just for multiplate system - probably we will support other systems that
  ## output xl files

  if(grepl(".xls", path)) {
    raw <- suppressMessages(read_excel(path, n_max = 20))
    raw <- as.character(raw)
  } else {raw <- readLines(path)}

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
  } else if (suppressWarnings(any(grepl("gmbl", raw[1:20])))) {
    cat("Vernier Logger Raw File Detected\n\n")
    out <- parse_vernier_raw(path)
  } else if (suppressWarnings(any(grepl("qmbl", raw[1:20])))) {
    cat("Vernier Logger Raw File Detected\n\n")
    out <- parse_vernier_raw(path)
  } else if (suppressWarnings(any(grepl(": Time \\(", raw[1])))) {
    cat("Vernier Logger .csv File Detected\n\n")
    out <- parse_vernier_csv(path)
  } else if (suppressWarnings(any(grepl("Vernier Format", raw[1:20])))) {
    cat("Vernier Logger .txt File Detected\n\n")
    warning("NOTE: Vernier files exported as .txt may have data columns in a different order to
            original data, and have no clear indication of which data came from which probes!
            We strongly recommend exporting as .csv or importing raw qmbl/gmbl files.")
    out <- parse_vernier_txt(path)
  ## This next one is also a multiplate file, but exported as text rather than
  ## Excel. Should probably revise to keep empty Well columns, so there's always
  ## 24 total.
  } else if(suppressWarnings(any(grepl("MUX channel", raw[10:30]))) &&
            suppressWarnings(any(grepl("PARAMETERS", raw[10:30]))) &&
            suppressWarnings(any(grepl("FIRMWARE", raw[30:50])))) {
    cat("PRESENS Generic file detected\n\n")
    out <- parse_presens(path)
  } else if(suppressWarnings(any(grepl("SDR Serial No.", raw[1:20])))) {
    cat("Loligo/Presens 24-well Multiplate Excel file detected\n\n")
    out <- parse_multiplate_excel(path)
  } else stop("Source file cannot be identified. Please contact the developers with a sample of your file. Process stopped.")

  if(export) {
    newpath <- paste(normalizePath(dirname(path)),"/", "parsed-",
                     basename(path), sep = "")
    write.csv(out, newpath)
  }

  return(out)
}



# Invividual device functions ---------------------------------------------

# Loligo/Presens multiplate system ----------------------------------------

parse_multiplate_excel <- function(path){
  raw <- suppressMessages(read_excel(path, col_names = TRUE))
  ## which row has "Date/Time"
  start_row <- which(grepl("^Date/Time$", raw[[1]]))
  ## inport from that row on
  raw <- suppressMessages(read_excel(path, skip = start_row-1))
  ## remove columns which are empty (all NA)
  ## EXCEPT the 24 Wells columns (and keep date-time, and num.time)
  cols1 <- raw[,1:26]
  cols2 <- raw[,-(1:26)]
  cols2 <- cols2[ , ! apply( cols2 , 2 , function(x) all(is.na(x)) ) ]
  raw <- cbind(cols1, cols2)
  out <- data.table(raw)
}

# Vernier csv files -------------------------------------------------------

parse_vernier_csv <- function(path) {
  raw <- fread(path, fill = TRUE, header = TRUE)
  out <- data.table(raw)
  return(out)
}


# Vernier txt files -------------------------------------------------------

parse_vernier_txt <- function(path) {

  ## read in raw data
  raw <- fread(path, fill = TRUE, header = FALSE, skip = 7)
  if(all(is.na(raw[[ncol(raw)]])))
    raw <- raw[,1:(ncol(raw)-1)] # remove extra column of NAs

  ## get metadata
  all <- readLines(path) # read in everything
  meta_index <- which(grepl("Vernier", all)) # where do each Run start?
  meta <- lapply(meta_index, function(x) all[x:(x+7)]) # read in 7 rows of metadata for each run
  runs <- sapply(meta, function(x) x[3]) # extract Run or exp name

  ## column names
  cols <- suppressWarnings(fread(path, fill = FALSE, header = FALSE, nrows = 7)) # read in first chunk of metadata
  if(all(is.na(cols[[ncol(cols)]])))
    cols <- cols[,1:(ncol(cols)-1)] # remove extra column of NAs

  col_nms <- c() # loop to construct col names
  for(i in 1:ncol(cols)){
    col_nms[i] <- paste0(cols[1,i], " (", cols[3,i], ")")
  }

  ## if more than one Run in file, split
  if(any(grepl("Vernier", raw))){
    ## sequence of Run row locations
    seq <- which(grepl("Vernier", raw[[1]])) # metadata starts
    seq <- c(seq-1, seq+7) # Add run data starts
    seq <- sort(seq) # reorder
    seq <- c(1, seq, length(raw[[1]])) ## sequence of Run data row locations
    seq <- matrix(seq, nrow = length(seq)/2, ncol = 2, byrow = T) # matrix for loop

    nrows <- max(seq[,2]-seq[,1])+1 # nrows of data
    ncols <- length(col_nms) # ncols of data in each run

    ## df with max no. of rows
    assembled <- data.frame(a = rep(NA, nrows))

    ## loop
    lp <- nrow(seq)

    for (i in 1:lp) {

      out <- raw[seq[i,1]:seq[i,2],]
      ## fill if too short
      if(nrow(out) < nrows){
        r_add <- nrows - nrow(out)
        empty <- matrix(NA, nrow = r_add, ncol = ncols)
        out <- rbind(out, empty)
      }

      assembled <- cbind(assembled, out)
    }
    raw <- assembled[,-1] # remove initialising column
  }

  all_col_nms <- rep(col_nms, ncol(raw)/length(col_nms)) ## rep col nms to size of df
  all_col_nms <- paste0(sapply(runs, function(x) rep(x, times = length(col_nms))), ": ", all_col_nms) # append run name to each

  raw <- apply(raw, 2, function(x) x <- as.numeric(x)) # make numeric

  out <- data.table(raw)
  names(out) <- all_col_nms

  return(out)
}


# Vernier gmbl/qmbl files -------------------------------------------------

parse_vernier_raw <- function(path){

  raw <- data.table::fread(path, fill = TRUE)

  ## collapse all columns into one column and remove added commas
  if(ncol(raw) > 1){
    raw <- data.table::data.table(apply(raw, 1, toString))
    raw[[1]] <- gsub(",", "", raw[[1]])
  }

  ## Remove "NA NA" from end of strings
  ## Only seen this once so far, not sure why)
  raw[[1]] <- gsub("NA NA", "", raw[[1]])

  ## Extract any notes, then copy over them to keep metadata pattern intact
  ## Should probably do a "notes detected" message and return these somehow
  if(any(grep("<TextText>", raw[[1]]))){
    str <- grep("<TextText>", raw[[1]])
    enr <- grep("</TextText>", raw[[1]])
    nr <- enr-str+1
    notes <- raw[str:enr]
    raw[[1]][(str:enr)] <- rep("<tmp>", nr)
  }

  # Metadata ----------------------------------------------------------------

  meta_index <- grep("<", raw[[1]]) ## metadata rows

  meta_starts <- c(1, meta_index[(which(diff(meta_index) !=1))+1])
  meta_ends <- c(meta_index[which(diff(meta_index) !=1)], nrow(raw))
  meta_locs <- data.frame(starts = meta_starts,
                          ends = meta_ends)
  ## metadata in list as separate elements
  meta <- apply(meta_locs, 1, function(x) raw[x[1]:x[2]])


  # Data --------------------------------------------------------------------

  data_index <- seq(1:length(raw[[1]]))
  data_index <- data_index[data_index %in% meta_index == F] # data rows

  data_starts <- c(data_index[1], data_index[(which(diff(data_index) !=1))+1])
  data_ends <- c(data_index[which(diff(data_index) !=1)], tail(data_index, 1))
  data_locs <- data.frame(starts = data_starts,
                          ends = data_ends)

  data <- apply(data_locs, 1, function(x) raw[x[1]:x[2]])
  data <- lapply(data, function(x) x[[1]]) ## make vector (cos problems later)
  data <- suppressWarnings(lapply(data, function(x) as.numeric(x)))  # make numeric

  # Run names ---------------------------------------------------------------

  runs <- raw[grep("<DataSetName>", raw[[1]])]
  runs <- sapply(runs, function (x) gsub("<DataSetName>", "", x, fixed = TRUE))
  runs <- sapply(runs, function (x) gsub("</DataSetName>", "", x, fixed = TRUE))
  runs <- trimws(runs, "right")

  n_runs <- length(runs)


  # Channels ----------------------------------------------------------------

  channels <- meta[[length(meta)]][[1]][grep("<MBLChannelIndex>", meta[[length(meta)]][[1]])]
  channels <- sapply(channels, function (x) gsub("<MBLChannelIndex>", "", x, fixed = TRUE))
  channels <- sapply(channels, function (x) gsub("</MBLChannelIndex>", "", x, fixed = TRUE))
  channels <- trimws(channels, "right")
  channels <- c(NA, channels) ## add NA channel for Time

  if(anyDuplicated(channels))
    warning("Duplicate Channel ID numbers found. This can result from wireless probes.")

  if(!identical(as.vector(na.omit(channels)), as.vector(na.omit(channels[order(channels)]))))
    warning("NOTE: Data Channels may not be in numerical order. Columns returned in the order
            reported by the Vernier software.")

  n_channels <- length(channels)

  if(n_channels != length(data)/n_runs){
    warning("Channel information unable to be extracted. Column names will be units
            and data type only.")
    channels <- rep(NA, length(data))
  }

  # Column names ------------------------------------------------------------

  ## column names and units fn
  col_nm <- function(x){
    ## column name
    col <- as.character(x[grep("<DataObjectName>", x[[1]])])
    col <- gsub("<DataObjectName>", "", col, fixed = TRUE)
    col <- gsub("</DataObjectName>", "", col, fixed = TRUE)
    col <- gsub(" ", "", col, fixed = TRUE)
    ## column units
    un <- as.character(x[grep("<ColumnUnits>", x[[1]])])
    un <- gsub("<ColumnUnits>", "", un, fixed = TRUE)
    un <- gsub("</ColumnUnits>", "", un, fixed = TRUE)
    un <- gsub(" ", "", un, fixed = TRUE)
    ## replace html degrees character and space
    un <- gsub("&#176;", xml2::xml_text(xml2::read_html("<x>&#176;</x>")), un)
    ## final col name
    nm <- paste0(col, " (", un, ")")
    return(nm)}

  col_nms <- sapply(meta[-length(meta)], function (x) col_nm(x))

  ## matrix of additions to column names
  mat <- expand.grid(runs, channels)
  mat <- mat[order(factor(mat[,1], levels = runs)),]
  mat[,3] <- col_nms
  mat <- as.matrix(mat)

  col_nms <- apply(mat, 1, function(x) paste0(x[1], ": Ch.", x[2], ": ", x[3]))


  # Data --------------------------------------------------------------------

  ## nrow of final df
  nrf <- max(sapply(data, length))

  ## pad shorter data to max length
  data <- lapply(data, function(x) {
    r_add <- nrf - length(x)
    empty <- rep(NA, r_add)
    x <- c(x, empty)
    return(x)
  })

  ## convert to df
  data <- as.data.frame(data)
  ## rename
  names(data) <- col_nms
  ## return
  data <- data.table(data)
  return(data)
}


# Loligo AutoResp ---------------------------------------------------------

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


# Witrox ------------------------------------------------------------------

parse_witrox <- function(path) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE, header = FALSE)
  # detect start column:
  colstart <- tail(suppressWarnings(raw[raw$V1 %like% "Date", which = TRUE]), 1)
  rdt <- fread(path, fill = TRUE, skip = colstart, colClasses = c(V2 = "character"))

  ## column names - this can differ A LOT depending on what is connected and
  ## number of channels. Best to use original names
  nms <- fread(path, skip = colstart - 1, nrows = 1, header = F)
  nms <- as.character(nms)

  ## need to strip all special characters - units etc. Encoding/hex code problems
  ## Found this regex online - no idea how it works.....
  nms <- gsub("[^[:alnum:]///' ]", "", nms)
  nms <- gsub("  ", "", nms) # to remove multiple spaces
  nms <- gsub(" ", "_", nms)

  rdt <- setnames(rdt, nms[1:ncol(rdt)])
  #rdt[, time := as.numeric((rdt$time)) - min(as.numeric((rdt$time)))]
  #data.table::setcolorder(rdt, 2) # set time as first column
  out <- data.table(rdt)
  return(out)
}


# MiniDOT -----------------------------------------------------------------

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


# PRESENS OXY10 -----------------------------------------------------------

parse_oxy10 <- function(path) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE)
  # colstart <- suppressWarnings(raw[raw[,1] %like% "Date/", which = TRUE])
  rdt <- fread(path, fill = TRUE, skip = 37, header = TRUE)[,1:4]
  out <- setnames(rdt, 1:4, c("date", "time", "elapsed", "o2"))
  data.table::setcolorder(out, 3)
  return(out)
}

# Firesting ---------------------------------------------------------------

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
  # convert to numeric if read as characters:
  rdt <- rdt[, -1][, lapply(.SD, as.numeric)]
  # avengers, assemble!:
  out <- data.table(elapsed,timestamp = newdates, rdt)
  return(out)
}

# Firesting Pyro ----------------------------------------------------------

parse_pyro <- function(path) {
  # txt <- readLines(path)
  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE)
  startdate <- raw[16]$V2
  colstart <- which(raw$V1 == "Date")[1]
  # Extract column header names:
  headers <- raw[colstart]
  rdt <- tail(fread(path, fill = TRUE),-colstart)
  # Insert column header names:
  setnames(rdt, make.unique(as.character(unlist(headers))))
  rdt <- rdt[, which(unlist(lapply(rdt, function(x)
    !all(is.na(x)||x == ""||x == "---")))), with = FALSE]
  out <- data.table(rdt)
  data.table::setcolorder(out, 3)
  # convert character to numeric
  index <- c(1, 4:length(names(out)))
  # wrapped this because of "NAs introduced by coercion" warning
  suppressWarnings(out[, names(out)[index] := lapply(.SD, as.numeric) , .SDcols = index])

  return(out)
}

# Generic PRESENS file ----------------------------------------------------

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
