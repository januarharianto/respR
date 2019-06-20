#' Import respirometry system output files
#'
#' Automatically read data from different respirometry hardware and software
#' systems. The aim is to work with most commercial DO sensors available in the
#' market with little input from the user. The function extracts data columns
#' from the file and generally cleans up column names to make the data easier to
#' work with.
#'
#' Currently works for:
#'  - Firesting Logger
#'  - Pyro Oxygen Logger (another name for Firesting)
#'  - PRESENS OXY10
#'  - PRESENS (generic, including multiplate systems)
#'  - MiniDOT
#'  - Loligo Witrox Logger
#'  - Loligo AutoResp ('_raw' files output, not metadata files)
#'  - Loligo/Presens 24-Well Multiplate System (Excel files)
#'  - Vernier (raw qmbl, csv, txt, (not yet gmbl))
#'  - PRESENS OXY4 (working on it)
#'  - Guppy (working on it)
#'  - Qbox Aqua (working on it)
#'
#' We are always looking for sample files to improve the function. Please send
#' them to us via [email](mailto:nicholascarey@gmail.com), or via a [Github
#' issue](https://github.com/januarharianto/respR/issues).
#'
#' While the devices listed above are supported, the import functionality may
#' not be complete due to limited access to output files. This will improve over
#' time as users provide feedback. We are releasing this as it is, without any
#' warranty, so that some people can still benefit from the functionality as it
#' gets better. Users should be careful using this function, and be prepared to
#' to import data by themselves since it is a fundamental skill in R.
#'
#' @param path string. Path to file.
#' @param export logical. If TRUE, exports the data to the same directory,
#'   determined by the path parameter above.
#'
#' @return a data frame object of all columned data
#'
#' @importFrom data.table data.table fread
#' @importFrom readxl read_excel
#' @importFrom xml2 xml_text
#' @importFrom xml2 read_html
#'
#' @export
#'
#' @examples

import_file <- function(path, export = FALSE) {

  # Don't even start if file doesn't exist:
  if (!file.exists(path)) {
    stop("File does not exist - please check file path.")
  }

  ## readLines doesn't work on xlsx files Have to do Excel import here - may not
  ## be just for multiplate system - probably we will support other systems that
  ## output xl files

  if(grepl(".xls", path)) {
    raw <- suppressMessages(read_excel(path, n_max = 20))
    raw <- as.character(raw)
  } else {
    raw <- suppressWarnings(readLines(path))
  }

  # Identify source of file
  if (suppressWarnings(any(grepl("Pyro", raw[1:20])))) {
    cat("Firesting-Pyro Oxygen Logger Detected\n\n")
    out <- parse_pyro(path)
  } else if (suppressWarnings(any(grepl("OXY10", raw[1:20])))) {
    cat("PRESENS OXY10 Detected\n\n")
    out <- parse_oxy10(path)
  } else if (suppressWarnings(any(grepl("MiniDOT", raw[1:20])))) {
    cat("MiniDOT Logger Detected\n\n")
    out <- parse_minidot(path)
  } else if (suppressWarnings(any(grepl("CALIBRATION DATA", raw[1:20])))) {
    cat("Loligo AutoResp/Witrox Logger Detected\n\n")
    out <- parse_autoresp_witrox(path)
  } else if (suppressWarnings(any(grepl(": Time \\(", raw[1])))) {
    cat("Vernier Logger .csv File Detected\n\n")
    out <- parse_vernier_csv(path)
  } else if (suppressWarnings(any(grepl("Vernier Format", raw[1:20])))) {
    cat("Vernier Logger .txt File Detected\n\n")
    warning("NOTE: Vernier files exported as .txt may have data columns in a different order to
    original data, and have no clear indication of which data came from which probes!
    We strongly recommend exporting as .csv or importing raw qmbl files.")
    out <- parse_vernier_txt(path)
  } else if (suppressWarnings(any(grepl("gmbl", raw[1:20])))) {
    stop("Vernier gmbl files not yet supported.\n\n")
    ## gmbl used to work with parse_vernier_raw but not any longer
    ## Some problem wih fread
  } else if (suppressWarnings(any(grepl("qmbl", raw[1:20])))) {
    cat("Vernier Logger Raw File Detected\n\n")
    out <- parse_vernier_raw(path)
    ## This one needs to go here, because the next (Presens Generic) will also match to
    ## Oxyview files
  } else if(suppressWarnings(any(grepl("OxyView", raw[1:100])))) {
    cat("PreSens OxyView file detected\n\n")
    out <- parse_oxyview(path)
    ## This next one is also a multiplate file, but exported as text rather than
    ## Excel.
  } else if(suppressWarnings(any(grepl("MUX channel", raw[1:80]))) &&
            suppressWarnings(any(grepl("PARAMETERS", raw[1:80]))) &&
            suppressWarnings(any(grepl("FIRMWARE", raw[1:80])))) {
    cat("PRESENS Generic file detected\n\n")
    out <- parse_presens(path)
  } else if(suppressWarnings(any(grepl("SDR Serial No.", raw[1:20])))) {
    cat("Loligo/Presens 24-well Multiplate Excel file detected\n\n")
    out <- parse_multiplate_excel(path)
  } else if(suppressWarnings(any(grepl("Tau - Phase Method", raw[1])))) {
    cat("NeoFox file detected\n\n")
    out <- parse_neofox(path)
    ## Loligo Metadata files
  } else if (suppressWarnings(any(grepl("Fractional error", raw[1:20])))) {
    cat("Loligo AutoResp Metadata File Detected\n\n")
    stop("Currently these files are unsupported in respR.
  This is an AutoResp metadata file, and contains no raw time~O2 data.
  It may contain other experimentally useful values (e.g. mass and volume).
  Please import the associated file appended with \"_raw\" which contains time~O2 data." )

  } else stop("Source file cannot be identified. Please contact the developers with a sample of your file. Import stopped.")

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
  ## remove column 27 - empty
  raw <- raw[,-27]
  out <- data.table(raw)
  return(out)
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
  ## make df
  setDF(cols)

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
  all_col_nms <- paste0(sapply(runs, function(x)
    rep(x, times = length(col_nms))), ": ", all_col_nms) # append run name to each

  raw <- apply(raw, 2, function(x) x <- as.numeric(x)) # make numeric

  out <- data.table(raw)
  names(out) <- all_col_nms

  return(out)
}


# Vernier gmbl/qmbl files -------------------------------------------------

# This used to work with gmbl, but no longer
# Problem with fread on first line

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

# Witrox ------------------------------------------------------------------

parse_autoresp_witrox <- function(path) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE, header = FALSE)
  # detect start column:
  rowstart <- tail(suppressWarnings(raw[raw$V1 %like% "Date", which = TRUE]), 1)
  rdt <- fread(path, fill = TRUE, skip = rowstart, colClasses = c(V2 = "character"))

  ## column names - this can differ A LOT depending on what is connected and
  ## number of channels. Best to use original names
  nms <- fread(path, skip = rowstart - 1, nrows = 1, header = F)
  nms <- as.character(nms)

  ## need to strip all special characters - units etc. Encoding/hex code problems
  nms <- gsub("%", "perc", nms) ## because it gets removed in next line
  ## Found this regex online - no idea how it works.....
  nms <- gsub("[^[:alnum:]///' ]", "", nms)
  nms <- gsub("  ", "", nms) # to remove multiple spaces
  nms <- gsub("  ", "", nms) # and again
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
  rowstart <- suppressWarnings(raw[raw$V1 %like% "Unix", which = TRUE])
  # colnames <- as.matrix(raw[rowstart])[1,]
  rdt <- fread(path, skip = rowstart-1)[-1]
  col_nms1 <- colnames(rdt)
  col_nms2 <- fread(path, skip = rowstart-1, nrows = 1)
  col_nms <- paste(col_nms1, col_nms2)
  colnames(rdt) <- col_nms
  out <- rdt
  return(out)
}


# PRESENS OXY10 -----------------------------------------------------------

parse_oxy10 <- function(path) {
  rdt <- fread(path, fill = TRUE, skip = 37, header = TRUE)
  nms <- colnames(rdt)
  nms <- gsub("%", "perc", nms) ## because it gets removed in next line
  nms <- gsub("[^[:alnum:]///' ]", "", nms) ## removes weird characters
  nms <- gsub("/", " ", nms)
  colnames(rdt) <- nms
  out <- rdt
  return(out)
}


# Firesting Pyro ----------------------------------------------------------

parse_pyro <- function(path) {
  # txt <- readLines(path)
  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE)
  #startdate <- raw[16]$V2
  rowstart <- grep("^Date$|^Time$", raw$V1)
  # deals with files with multiple datasets (e.g. pyro06)
  # dunno how common this is
  if(length(rowstart) != 1){
    message("Data file appears to have multiple datasets starting at these rows: ")
    cat(rowstart)
    message("\n")
    stop("It will have to imported manually or edited to contain only one dataset.")
  }
  # Extract column header names:
  headers <- raw[rowstart]
  rdt <- tail(fread(path, fill = TRUE),-rowstart)
  # Insert column header names:
  setnames(rdt, make.unique(as.character(unlist(headers))))

  # Remove empty columns
  # Previous code here removed comments column in pyro08 where there was a single
  # comment. No idea why
  rdt <- Filter(function(x) !all(is.na(x)), rdt)
  rdt <- Filter(function(x) !all(x == ""), rdt)
  rdt <- Filter(function(x) !all(x == "---"), rdt)

  out <- data.table(rdt)
  # Convert character to numeric
  # Find columns where / or : are used. These are dates/times
  # And Comments column if present
  index <- c(grep("/", out[1,]), grep(":", out[1,]), grep("Comment", colnames(out)))
  # Index of columns except these
  all_cols <- 1:ncol(out)
  index <- Filter(function(x) all(x != index), all_cols)

  # wrapped this because of "NAs introduced by coercion" warning
  suppressWarnings(out[, names(out)[index] := lapply(.SD, as.numeric) , .SDcols = index])

  return(out)
}

# Generic PRESENS file ----------------------------------------------------

parse_presens <- function(path) {

  raw <- fread(path, fill = TRUE, header = FALSE)
  rowstart <- suppressWarnings(raw[raw$V1 %like% "^Date/", which = TRUE])

  ## if generic version with ; delimit
  if(grepl("^Measurement$", raw[1,1])){
    rdt <- fread(path, skip = rowstart)
    nms <- fread(path, skip = rowstart-1, nrow = 1)
  } else if (grepl("^Measurement Name :$", raw_x[1,1])){
    rdt <- raw[-(1:(rowstart)),]
    nms <- fread(path, nrow = rowstart, fill = TRUE, header = FALSE)
    nms <- nms[rowstart,]
  } else {stop("Parsing Error. Please import manually.")}

  nms <- gsub("%", "perc", nms) ## because it gets removed in next line
  nms <- gsub("[^[:alnum:]///' ]", " ", nms) ## removes weird characters
  nms <- gsub("/", " ", nms) # replace slashes
  nms <- trimws(nms) # remove trailing spaces
  nms <- gsub("  ", " ", nms) # to remove multiple spaces
  nms <- gsub("  ", " ", nms) # and again
  nms <- gsub(" ", "_", nms) # now make all spaces underscores
  colnames(rdt) <- nms[1:ncol(rdt)]
  if("NA" %in% colnames(rdt)){rdt$'NA' <- NULL} # remove added empty column if present
  out <- rdt
  return(out)

}



# NeoFox ------------------------------------------------------------------

## super simple for now - decide later if we do anything more complicated
parse_neofox <- function(path) {
  rdt <- fread(path, fill = TRUE, sep = ",")
  out <- rdt
  return(out)
}


# PreSens OxyView ---------------------------------------------------------

## Very similar to parse oxy10

parse_oxyview <- function(path) {

  raw <- fread(path, fill = TRUE)
  rowstart <- suppressWarnings(raw[raw$V1 %like% "date\\(", which = TRUE])
  rdt <- raw[-(1:rowstart),]
  #rdt <- fread(path, fill = TRUE, skip = rowstart-1, header = TRUE)
  #nms <- colnames(rdt)
  nms <- raw[rowstart,]
  nms <- gsub("%", "perc", nms) ## because it gets removed in next line
  nms <- gsub("[^[:alnum:]///' ]", " ", nms) ## removes weird characters
  nms <- gsub("/", " ", nms)
  nms <- trimws(nms) # remove trailing spaces
  nms <- gsub("  ", "", nms) # to remove multiple spaces
  nms <- gsub(" ", "_", nms) # now make all spaces underscores
  colnames(rdt) <- nms
  if("NA" %in% colnames(rdt)){rdt$'NA' <- NULL} # remove added empty column if present
  out <- rdt
  return(out)
}
