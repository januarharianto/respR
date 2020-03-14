#' Subsample a data frame object
#'
#' A simple function that subsamples a data frame of time~oxygen data in order
#' to "thin" large datasets. It also works on vectors of any data type. Two
#' subsampling methods are provided. Using the `n` argument selects every n
#' elements or rows. The `length.out` argument uniformly subsamples the input to
#' the desired length.
#'
#' @param df data frame or vector. The data to process.
#' @param n numeric. Subsample every `n` rows.
#' @param length.out numeric. Subsample to a specific length or number of rows.
#' @param random_start logical. Defaults to FALSE. If TRUE, randomises the start
#'   row of the data (applies to `n` input only).
#' @param plot logical. Defaults to TRUE. Plots the data.
#'
#' @return A data frame or vector object (depending on input).
#' @export
#'
#' @examples
#' # Subsample every 10 rows:
#' subsample(squid.rd, n = 10)
#'
#' # Subsample to 1000 rows
#' subsample(squid.rd, n = 10)
#'
#' # Subsample with random starting position
#' subsample(sardine.rd, n = 3, random_start = TRUE)

subsample <- function(df, n = NULL, length.out = NULL, random_start = FALSE, plot = TRUE) {

  if(is.null(n) && is.null(length.out))
    stop("One of 'n' or 'length.out' is required.")
  if(!is.null(n) && !is.null(length.out))
    stop("Only one of 'n' or 'length.out' should be entered.")

  # df or vector
  if(is.data.frame(df)) index <- 1:nrow(df) else
    index <- 1:length(df)

  # If random_start randomise start location for n
  if(!is.null(n) && random_start) start <- sample(1:n, 1) else
    start <- 1

  end <- length(index)

  # if n - sample every [n] rows from start
  # if length.out - sample to correct length
  if(!is.null(n)) index <- index[seq.int(start, end, by = n)] else
    index <- index[seq.int(start, end, length.out = length.out)]

  ## apply index
  if(is.data.frame(df)) subset <- df[index,] else
    subset <- df[index]

  # Create the plots
  if (plot) {

    if(is.data.frame(df) && ncol(df) > 2) message("subsample: plotting first column of data only.")

    if(is.data.frame(df)){
      xlab <- "Time"
      ylab <- "Oxygen"
      data <- df[,1:2]
      sub_data <- subset[,1:2]
    } else {
      xlab <- "Row"
      ylab <- "Data"
      data <- df
      sub_data <- subset
    }

    parorig <- par(no.readonly = TRUE) # save original par settings
    on.exit(par(parorig)) # revert par settings to original

    par(mfrow=c(1,2))  # replace par settings
    plot(data, xlab = xlab, ylab = ylab, col = r1, pch = 16,
         panel.first = c(rect(par("usr")[1],
                              par("usr")[3],
                              par("usr")[2],
                              par("usr")[4], col = r3), grid(col = "white",
                                                             lty = 1, lwd = 1.5)))
    title(main = "Full Dataset", line = 0.5)
    plot(sub_data, xlab = xlab, ylab = ylab, col = r1, pch = 16,
         panel.first = c(rect(par("usr")[1],
                              par("usr")[3],
                              par("usr")[2],
                              par("usr")[4], col = r3), grid(col = "white",
                                                             lty = 1, lwd = 1.5)))
    title(main = "Subsample Dataset", line = 0.5)
  }
  return(invisible(subset))
}
