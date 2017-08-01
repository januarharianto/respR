#' @export
check.input <- function(df, x = NULL, y = NULL, plot = T) {
  df <- tibble::as_tibble(df)
  # if no columns are selected
  if (is.null(x) && is.null(y)) {
    df <- df
  } else if (is.numeric(x) && is.numeric(y)) {
    # otherwise, select columns
    x <- df[,x]
    y <- df[,y]
    df <- data.frame(x, y)
  } else stop("The arguments 'x' and/or 'y' must be numeric.")

  print(ls.str(df))

  # perform checks
  # check that columns are of equal lengths
  summary.lens <- sapply(1:length(df), function(x) length(df[[x]]))
  test.lens <- equal.lengths(summary.lens)

  # check that all columns are numeric or integer
  test.class <- sapply(df, class)
  summary.class <- sapply(test.class, function (x) x == 'numeric' | x == 'integer')
  if (any(summary.class == F, na.rm = T)) {
    # if not, terminate the function since more errors will pop up
    message("Non-numeric data detected. Are there typos in your dataset? Check:")
    print(test.class)
    #    show.notanumber <- lapply(1:length(df), function(x) which(df[x] != 'numeric' && df[x] != 'integer'))
    stop("Function terminated since non-numeric data cannot be processed. ",call. = F)
  }

  # check for NA
  test.na.absent <- !any(is.na(df))
  summary.na <- !sapply(1:length(df), function(x) anyNA(df[[x]]))
  show.na <- lapply(1:length(df), function(x) which(is.na(df[x])))

  # check for duplicates
  summary.dupes <- sapply(1:length(df), function(x) !any(duplicated(df[[x]])))
  test.dupes.absent <- !any(summary.dupes == FALSE)
  show.duplicated <- lapply(1:length(df), function(x) which(duplicated(df[x]) == T))

  # check for evenly-spaced data
  summary.espace <- sapply(1:length(df), function(x) equal.lengths(diff(df[[x]])))
  test.space <- any(summary.espace == T)

  # summarise the results
  results <- noquote(rbind(as.character(summary.lens), summary.class, summary.na, summary.dupes, summary.espace))
  rownames(results) <- c('Length of data',
    'Data are numeric',
    'NA/NaN',
    'Duplicates',
    'Evenly-spaced')
  results[results == 'TRUE'] <- 'o'
  results[results == 'FALSE'] <- 'x'
  print(results)

  # this activates when a 2-column data is selected, or if x and y are NOT NULL
  # the function will process the data frame i.e. convert numeric, remove NAs
  if (length(df) == 2 | !is.null(x) | !is.null(y)) {
    if (test.lens == F) stop("Data columns are not of equal length.\nDataframe NOT generated.", call. = F)

    if(test.class[1] == 'integer' | test.class[2] == 'integer') {
      df[1] <- as.numeric(df[[1]])
      df[2] <- as.numeric(df[[2]])
      test.class <- sapply(df, class)
      message("Note: Integer columns have been converted to numeric.")
    } #

    if (test.class[1] != 'numeric' && test.class[2] != 'numeric') {
      stop("Data must be numeric.\nDataframe NOT generated.", call. = F)
    } #

    if (test.na.absent == F) {
      df <- na.omit(df)
      message("Warning: NA values detected. Rows containing NA values have been automatically removed (see below).")
      print(show.na)
    } #

    if (summary.dupes[1] == F) {
      message("Duplicate time data found at rows:")
      print(show.duplicated[[1]])
      stop("Time data must not contain duplicates.\nDataframe NOT generated.", call. = F)
    }
    if (summary.espace[1] == F) message("Warning: Time data is unevenly spaced. If using `calc.rate`, please avoid subsetting\nby = 'row' unless you understand the range of data you are selecting using this method.")
    message("New dataframe generated.")

    # plot
    if (plot == T) {
      p1 <- ggplot(df, aes(df[[1]], df[[2]])) +
        geom_point(size = 2, alpha = .6) +
        labs(x = 'Time', y = 'DO') +
        theme_respr()
      print(p1)
    }
    return(invisible(df))
  }
}





# internal function
equal.lengths <- function(x) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2]))
}
