#' Import respirometry system output files
#'
#' Automatically import data from different respirometry hardware and software
#' systems. The aim is to work with most commercial DO sensors available in the
#' market with little input from the user. The function extracts data columns
#' from the file and generally cleans up column names (e.g. removes whitespace
#' and characters which cause text encoding issues) to make the data easier to
#' work with. Files should be raw output files where possible; files opened and
#' re-saved in a different format or filetype will likely fail to import.
#'
#' Currently works for:
#'  - Firesting
#'  - Pyro (another name for Firesting)
#'  - PreSens OXY10
#'  - PreSens OXY4
#'  - PreSens (OxyView generic, including multiplate systems)
#'  - PreSens/Loligo 24-Well Multiplate System (Excel files)
#'  - MiniDOT
#'  - Loligo AutoResp ('_raw' files output, not metadata files)
#'  - Loligo Witrox (same as AutoResp, without metadata)
#'  - Vernier (raw qmbl, csv, txt, (not yet gmbl))
#'  - NeoFox
#'  - Qbox Aqua
#'
#' We are always looking for sample files to improve the function. Please send
#' them to us via [email](mailto:nicholascarey@gmail.com), or via a [Github
#' issue](https://github.com/januarharianto/respR/issues).
#'
#' Files with European formatting (i.e. commas instead of points to denote
#' decimals) are supported, and will be converted to point-decimals on import.
#' This is experimental functionality, so please provide feedback for any files
#' for which this might fail.
#'
#' While the devices listed above are supported, the import functionality is
#' experimental due to limited access to output files. This will improve over
#' time as users provide feedback and smaple files. We are releasing this as it
#' is, without any warranty, so that some people can still benefit from the
#' functionality as it gets better. Users should be careful using this function,
#' check the imported data, and be prepared to import data by themselves since
#' it is a fundamental skill in R.
#'
#' @param path string. Path to file.
#' @param export logical. If TRUE, exports the data as a `csv` to the same
#'   directory, as determined by the `path` parameter.
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
#' @examples NULL
import_file <- function(path, export = FALSE) {

  cat("\n# import_file # -------------------------\n")

  # Don't even start if file doesn't exist:
  if (!file.exists(path)) {
    stop("File does not exist - please check file path.")
  }

  ## readLines doesn't work on xlsx files Have to do Excel import here - may not
  ## be just for multiplate system - probably we will support other systems that
  ## output xl files

  if (grepl(".xls", path)) {
    message("Excel file detected. Only Loligo/Presens Multiplate Excel files currently supported. \nImport will fail for others saved as Excel format. Use raw output files only.")
    raw <- suppressMessages(read_excel(path, n_max = 20))
    raw <- as.character(raw)
  } else if (grepl("gmbl", path)) {
    raw <- suppressWarnings(readLines(path))
  } else {
    raw <- suppressWarnings(readLines(path))
    ## get decimal character
    dec <- get_dec(path)
  }

  ## file extensions helps with some
  ext <- substr(path, nchar(path)-3, nchar(path))

  # Identify source of file
  if (suppressWarnings(any(grepl("Pyro", raw[1:20])))) {
    cat("Firesting-Pyro file detected\n")
    out <- parse_pyro(path, dec = dec)
  } else if (suppressWarnings(any(grepl("MiniDOT", raw[1:20])))) {
    cat("MiniDOT file detected\n")
    out <- parse_minidot(path, dec = dec)
  } else if (suppressWarnings(any(grepl("CALIBRATION DATA", raw[1:20])))) {
    cat("Loligo AutoResp/Witrox file detected\n")
    out <- parse_autoresp_witrox(path, dec = dec)
  } else if (suppressWarnings(any(grepl(": Time \\(", raw[1])))) {
    cat("Vernier or QBox Aqua csv file detected\n")
    out <- parse_vernier_csv(path, dec = dec)
  } else if (suppressWarnings(any(grepl("Vernier Format", raw[1:20])))) {
    cat("Vernier txt file detected\n")
    warning("NOTE: Vernier files exported as .txt may have data columns in a different order to
    original data, and have no clear indication of which data came from which probes!
    We strongly recommend exporting as .csv or importing raw qmbl files.")
    out <- parse_vernier_txt(path, dec = dec)
  } else if (suppressWarnings(any(grepl("gmbl", raw[1:20])))) {
    stop("Vernier gmbl files not yet supported.\n")
    ## gmbl used to work with parse_vernier_raw but not any longer
    ## Some problem wih fread
  } else if (suppressWarnings(any(grepl("qmbl", raw[1:20])))) {
    cat("Vernier raw qmbl file detected\n")
    out <- parse_vernier_raw(path, dec = dec)
    ## These need to go here, because the next (Presens Generic) will also match
  } else if (suppressWarnings(any(grepl("OXY10", raw[1:20])))) {
    cat("PreSens OXY10 file detected\n")
    out <- parse_oxy10(path, dec = dec)
  } else if (suppressWarnings(any(grepl("OxyView", raw[1:100]))) && ext == ".txt") {
    cat("PreSens OxyView .txt file detected\n")
    out <- parse_oxyview_txt(path, dec = dec)
  } else if (suppressWarnings(any(grepl("OxyView", raw[1:100]))) && ext == ".csv") {
    cat("PreSens OxyView .csv file detected\n")
    out <- parse_oxyview_csv(path, dec = dec)
  # } else if (suppressWarnings(any(grepl("OxyView", raw[1:100])))) {
  #   cat("PreSens OxyView file detected\n")
  #   out <- parse_oxyview(path, dec = dec)
  } else if (suppressWarnings(any(grepl("OXY4", raw[1:100])))) {
    cat("PreSens OXY4 file detected\n")
    out <- parse_oxy4(path, dec = dec)
    ## This next one is also a multiplate file, but exported as text rather than
    ## Excel.
  } else if (suppressWarnings(any(grepl("MUX channel", raw[1:80]))) &&
             suppressWarnings(any(grepl("PARAMETERS", raw[1:80]))) &&
             suppressWarnings(any(grepl("FIRMWARE", raw[1:80])))) {
    cat("PreSens Generic file detected\n")
    out <- parse_presens(path, dec = dec)
  } else if (suppressWarnings(any(grepl("SDR Serial No.", raw[1:20])))) {
    cat("Loligo/PreSens 24-well multiplate Excel file detected\n")
    out <- parse_multiplate_excel(path, dec = dec)
  } else if (suppressWarnings(any(grepl("Tau - Phase Method", raw[1])))) {
    cat("NeoFox file detected\n")
    out <- parse_neofox(path, dec = dec)
    ## Loligo Metadata files
  } else if (suppressWarnings(any(grepl("Fractional error", raw[1:20])))) {
    cat("Loligo AutoResp metadata file detected\n")
    stop("Currently these files are unsupported in respR.
  This is an AutoResp metadata file, and contains no raw time~O2 data.
  It may contain other experimentally useful values (e.g. mass and volume).
  Please import the associated file appended with \"_raw\" which contains time~O2 data." )
    ## PreSens Datamanager
  } else if (suppressWarnings(any(grepl("PreSens Datamanager", raw[1:20])))) {
    cat("PreSens Datamanager output file detected\n")
    out <- parse_datamanager(path, dec = dec)
  } else stop("Source file cannot be identified.
              Please contact the developers with a sample of your file.
              Import halted.")

  if (export) {
    newpath <- paste(normalizePath(dirname(path)),"/", "parsed-",
                     basename(path), sep = "")
    write.csv(out, newpath)
  }

  cat("-----------------------------------------\n")
  return(out)
}



# Invividual device functions ---------------------------------------------

# Loligo/Presens multiplate system ----------------------------------------

parse_multiplate_excel <- function(path, dec = dec){
  raw <- suppressMessages(read_excel(path, col_names = TRUE))
  ## which row has "Date/Time"
  start_row <- which(grepl("^Date/Time$", raw[[1]]))
  ## inport from that row on
  raw <- suppressMessages(read_excel(path, skip = start_row - 1))
  ## remove column 27 - empty
  raw <- raw[,-27]
  out <- data.table(raw)
  return(out)
}

# Vernier csv files -------------------------------------------------------

parse_vernier_csv <- function(path, dec = dec) {
  raw <- fread(path, fill = TRUE, header = TRUE, dec = dec, showProgress = FALSE)
  out <- data.table(raw)
  return(out)
}


# Vernier txt files -------------------------------------------------------

parse_vernier_txt <- function(path, dec = dec) {

  ## read in raw data
  raw <- fread(path, fill = TRUE, header = FALSE, skip = 7, dec = dec)
  if (all(is.na(raw[[ncol(raw)]])))
    raw <- raw[,1:(ncol(raw) - 1)] # remove extra column of NAs

  ## get metadata
  all <- readLines(path) # read in everything
  meta_index <- which(grepl("Vernier", all)) # where do each Run start?
  meta <- lapply(meta_index, function(x) all[x:(x + 7)]) # read in 7 rows of metadata for each run
  runs <- sapply(meta, function(x) x[3]) # extract Run or exp name

  ## column names
  cols <- suppressWarnings(fread(path, fill = FALSE, header = FALSE, nrows = 7,
                                 dec = dec, showProgress = FALSE)) # read in first chunk of metadata
  if (all(is.na(cols[[ncol(cols)]])))
    cols <- cols[,1:(ncol(cols) - 1)] # remove extra column of NAs
  ## make df
  setDF(cols)

  col_nms <- c() # loop to construct col names
  for (i in 1:ncol(cols)){
    col_nms[i] <- paste0(cols[1,i], " (", cols[3,i], ")")
  }

  ## if more than one Run in file, split
  if(any(grepl("Vernier", raw))){
    ## sequence of Run row locations
    seq <- which(grepl("Vernier", raw[[1]])) # metadata starts
    seq <- c(seq - 1, seq + 7) # Add run data starts
    seq <- sort(seq) # reorder
    seq <- c(1, seq, length(raw[[1]])) ## sequence of Run data row locations
    seq <- matrix(seq, nrow = length(seq)/2, ncol = 2, byrow = T) # matrix for loop

    nrows <- max(seq[,2] - seq[,1])+1 # nrows of data
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

parse_vernier_raw <- function(path, dec = dec){

  raw <- data.table::fread(path, fill = TRUE, dec = dec, showProgress = FALSE)

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

parse_autoresp_witrox <- function(path, dec = dec) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE, header = FALSE, dec = dec, showProgress = FALSE)
  # detect start column:
  rowstart <- tail(suppressWarnings(raw[raw$V1 %like% "Date", which = TRUE]), 1)
  rdt <- fread(path, fill = TRUE, skip = rowstart, colClasses = c(V2 = "character"),
               dec = dec, showProgress = FALSE)

  ## column names - this can differ A LOT depending on what is connected and
  ## number of channels. Best to use original names
  nms <- fread(path, skip = rowstart - 1, nrows = 1, header = F, dec = dec,
               showProgress = FALSE)
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

parse_minidot <- function(path, dec = dec) {
  # txt <- readLines(path)
  raw <- fread(path, fill = TRUE, dec = dec, showProgress = FALSE)
  rowstart <- suppressWarnings(raw[raw$V1 %like% "Unix", which = TRUE])
  # colnames <- as.matrix(raw[rowstart])[1,]
  rdt <- fread(path, skip = rowstart+1, dec = dec, showProgress = FALSE)
  col_nms1 <- fread(path, skip = rowstart-1, nrows = 3, dec = dec, showProgress = FALSE)[-1]
  col_nms1 <- colnames(col_nms1)
  col_nms2 <- fread(path, skip = rowstart-1, nrows = 1, dec = dec, showProgress = FALSE)
  col_nms <- paste(col_nms1, col_nms2)
  colnames(rdt) <- col_nms
  out <- rdt
  return(out)
}


# PRESENS OXY10 -----------------------------------------------------------

## identical to parse_oxy10 - could merge
parse_oxy10 <- function(path, dec = dec) {
  rdt <- fread(path, fill = TRUE, skip = 37, header = TRUE,
               dec = dec, showProgress = FALSE)
  nms <- colnames(rdt)
  nms <- gsub("%", "perc", nms) ## because it gets removed in next line
  nms <- gsub("[^[:alnum:]///' ]", "", nms) ## removes weird characters
  nms <- gsub("/", " ", nms)
  colnames(rdt) <- nms
  out <- rdt
  return(out)
}

# PRESENS OXY4 ------------------------------------------------------------

## identical to parse_oxy10 - could merge
parse_oxy4 <- function(path, dec = dec) {
  rdt <- fread(path, fill = TRUE, skip = 37, header = TRUE,
               dec = dec, showProgress = FALSE)
  nms <- colnames(rdt)
  nms <- gsub("%", "perc", nms) ## because it gets removed in next line
  nms <- gsub("[^[:alnum:]///' ]", "", nms) ## removes weird characters
  nms <- gsub("/", " ", nms)
  colnames(rdt) <- nms
  out <- rdt
  return(out)
}


# Firesting Pyro ----------------------------------------------------------

parse_pyro <- function(path, dec = dec) {

  # Convert text to data.table object so that it's fast to deal with
  raw <- fread(path, fill = TRUE, dec = dec, header = FALSE,
               encoding = "UTF-8", na.strings=c("","NA","---"),
               showProgress = FALSE)

  # deals with files with multiple datasets (e.g. pyro06)
  # dunno how common this is
  dbl_data <- grep("Settings", raw[[1]])
  if(length(dbl_data) > 1) {
    message("Data file appears to have multiple datasets starting at these rows: ")
    cat(dbl_data)
    message("\nIt will have to imported manually or edited to contain only one dataset.")
    stop("Import stopped.")
  }

  raw_sub <- raw[1:50,]
  ## finds row with all Channels - SLOW
  rowstart <- apply(raw_sub, 1, function(x) {
    stringr::str_detect(x, "Ch 1")
    stringr::str_detect(x, "Ch1")
    stringr::str_detect(x, "Ch 2")
    stringr::str_detect(x, "Ch2")
    stringr::str_detect(x, "Ch 1")
    stringr::str_detect(x, "Ch1")
    stringr::str_detect(x, "Ch 3")
    stringr::str_detect(x, "Ch3")
  })

  rowstart <- which(rowstart, arr.ind = TRUE)[,2]
  rowstart <- unique(rowstart)

  # Extract column header names and clean them up

  ## Read in the 2 rows of column names
  col_nms <- as.data.frame(raw_sub[(rowstart-1):(rowstart),])

  ## Where does each set of 4 channels start
  ch1_locs <- which("Ch1" == col_nms[2,])
  ch1_locs <- sort(c(ch1_locs, which("Ch 1" == col_nms[2,])))

  ## Data type of each set of channels
  ## This collapses them to one if all the same, concatenates if split across several cells
  ch_type <- sapply(ch1_locs, function(x)
    paste(unique(paste(col_nms[1,x:(x+3)][!is.na(col_nms[1,x:(x+3)])])),collapse=" "))

  ## put locations and data types together
  ch <- data.frame(a=ch1_locs, b = ch_type)

  ## for each, copy data type to all 4 channel positions in row 1
  for(i in 1:nrow(ch)){
    col_nms[1, ch[i,1]:(ch[i,1]+3)] <- ch[i,2]
  }

  col_nms <- gsub("%", "perc", col_nms) ## This also collapses to one string
  col_nms <- gsub("c\\(\"", "", col_nms)
  col_nms <- gsub("c\\(NA, \"", "", col_nms)
  col_nms <- gsub("xb0", "", col_nms)
  col_nms <- gsub("[^[:alnum:]///' ]", " ", col_nms) ## removes weird characters
  col_nms <- gsub("'", " ", col_nms) # replace appostrophes
  col_nms <- trimws(col_nms) # remove trailing spaces
  col_nms <- gsub("  ", " ", col_nms) # to remove multiple spaces
  col_nms <- gsub("  ", " ", col_nms) # and again
  col_nms <- gsub("  ", " ", col_nms) # and again
  col_nms <- gsub(" ", "_", col_nms) # now make all spaces underscores
  ## fix overly long one
  col_nms[which(grepl("Advanced_", col_nms))] <- substr(col_nms[which(grepl("Advanced_", col_nms))], 1, 19)


  ## Ok, this is the only way i could get this to work
  ## skip in fread is incredible inconsistent. Sometimes works, sometimes does not and
  ## output includes 1-2 rows above skip row. Drove me up the wall trying to figure it out.
  ## This works but data comes out non-numeric
  rdt <- tail(fread(path, fill = TRUE, dec = dec,
                    header = FALSE,
                    showProgress = FALSE,
                    encoding = "UTF-8", na.strings=c("","NA","---")),-(rowstart))

  # Insert column header names:
  setnames(rdt, make.unique(as.character(unlist(col_nms))))

  ## this smartly converts different data types without changing strings to NA
  ## or to factors (via as.is). Also thank F accepts dec
  rdt <- type.convert(rdt, as.is = TRUE, dec = dec)

  ## removed code to remove empty columns

  out <- data.table(rdt)

  return(out)
}

# Generic PRESENS file ----------------------------------------------------

parse_presens <- function(path, dec = dec) {

  raw <- fread(path, fill = TRUE, header = FALSE, dec = dec, showProgress = FALSE)
  rowstart <- suppressWarnings(raw[raw$V1 %like% "^Date/", which = TRUE])

  rdt <- fread(path, skip = rowstart, dec = dec, showProgress = FALSE)
  nms <- fread(path, skip = rowstart-1, nrows = 1, dec = dec, showProgress = FALSE)

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


# Presens datamanager ----------------------------------------------------------

parse_datamanager <- function(path, dec = dec) {
  # import raw data:
  raw <- data.table::fread(path, fill = TRUE, skip = 1, dec = dec, showProgress = FALSE)

  # below is code to clean the data file
  # TODO perhaps a clean function could be used hmm
  # # clean if necessary:
  # if (clean) {
  #   out <- raw[, c(
  #     "Date",
  #     "Time",
  #     "delta_t",
  #     "Value",
  #     "Phase",
  #     "Amplitude",
  #     "Temp",
  #     "patm",
  #     "Salinity"
  #   )]
  # } else
  #   out <- raw
  out <- raw
  return(out)
}


# NeoFox ------------------------------------------------------------------

## super simple for now - decide later if we do anything more complicated
parse_neofox <- function(path, dec = dec) {
  rdt <- fread(path, fill = TRUE, sep = ",", dec = dec, showProgress = FALSE)
  out <- rdt
  return(out)
}


# PreSens OxyView ---------------------------------------------------------

## Very similar to parse oxy10

parse_oxyview_csv <- function(path, dec = dec) {

  raw <- fread(path, fill = TRUE, dec = dec, showProgress = FALSE)
  rowstart <- suppressWarnings(raw[raw$V1 %like% "date\\(", which = TRUE])
  rdt <- fread(path, fill = TRUE, skip = rowstart, dec = dec, showProgress = FALSE)
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

parse_oxyview_txt <- function(path, dec = dec) {

  raw <- fread(path, fill = TRUE, dec = dec, showProgress = FALSE)
  rowstart <- suppressWarnings(raw[raw$V1 %like% "date\\(", which = TRUE])
  rdt <- fread(path, fill = TRUE, skip = "date", dec = dec,
               showProgress = FALSE) ## only difference to above, otherwise columns are characters
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



# Internal Functions ------------------------------------------------------

#' Identify decimal character
#' For European formatted files with commas as decimal separator
#'
#' @keywords internal
#' @export
get_dec <- function(path){

  ## approx nrows of file, then read in at bottom
  ## This is better than guessing the number of lines to skip, in case of datasets with only
  ## a few rows.
  ## readLines faster, but problems with files with alternating blank lines
  #nrw <- length(suppressWarnings(readLines(path)))
  nrw <- nrow(suppressWarnings(fread(path, fill = TRUE, header = TRUE,
                                     showProgress = FALSE)))

  ## read in 5 rows starting 10 back from last row (to avoid any weirdness in last rows)
  ## read in with points as dec
  pnt <- suppressWarnings(fread(path, fill = FALSE, header = FALSE, nrows = 5,
                                skip = nrw-10, dec = ".", showProgress = FALSE))
  ## read in with commas as dec
  com <- suppressWarnings(fread(path, fill = FALSE, header = FALSE, nrows = 5,
                                skip = nrw-10, dec = ",", showProgress = FALSE))

  ## which has more numeric columns?
  pnt_num <- unlist(lapply(pnt, is.numeric))
  pnt_l <- length(pnt_num[pnt_num==TRUE])
  com_num <- unlist(lapply(com, is.numeric))
  com_l <- length(com_num[com_num==TRUE])

  if(pnt_l > com_l) dec<- "." else
    dec <- ","

  return(dec)
}
