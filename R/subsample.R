#' Subsample a data frame object
#'
#' A simple function that subsamples a data frame of time~oxygen data in order
#' to "thin" large datasets. It also works on vectors of any data type. Two
#' subsampling methods are provided. Using the `n` argument selects every n
#' elements or rows. The `length.out` argument uniformly subsamples the input to
#' the desired length.
#'
#' ## Output
#' Returns a data frame or vector object depending on input.
#'
#' @param x data frame or vector. The data to subsample.
#' @param n numeric. Subsample every `n` rows.
#' @param length.out numeric. Subsample to a specific length or number of rows.
#' @param random_start logical. Defaults to FALSE. If TRUE, randomises the start
#'   row of the data (applies to `n` input only).
#' @param plot logical. Defaults to TRUE. Plots the data.
#'
#' @export
#'
#' @examples
#' # Subsample by every 10th row:
#' subsample(squid.rd, n = 10)
#'
#' # Subsample to 1000 rows:
#' subsample(squid.rd, length.out = 1000)
#'
#' # Subsample with random starting position:
#' subsample(sardine.rd, n = 3, random_start = TRUE)

subsample <- function(x, n = NULL, length.out = NULL, random_start = FALSE, plot = TRUE) {

  if(is.null(n) && is.null(length.out))
    stop("One of 'n' or 'length.out' is required.")
  if(!is.null(n) && !is.null(length.out))
    stop("Only one of 'n' or 'length.out' should be entered.")

  # df or vector
  if(is.data.frame(x)) index <- 1:nrow(x) else
    index <- 1:length(x)

  # If random_start randomise start location for n
  if(!is.null(n) && random_start) start <- sample(1:n, 1) else
    start <- 1

  end <- length(index)

  # if n - sample every [n] rows from start
  # if length.out - sample to correct length
  if(!is.null(n)) index <- index[seq.int(start, end, by = n)] else
    index <- index[seq.int(start, end, length.out = length.out)]

  ## apply index
  if(is.data.frame(x)) subset <- x[index,] else
    subset <- x[index]

  # Create the plots
  if (plot) plot.sub(x, subset)

  # Return
  return(invisible(subset))
}

#o_data <- x
plot.sub <- function(o_data, subset){

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  if(is.data.frame(o_data) && ncol(o_data) > 2)
    message("subsample: plotting first column of data only.")

  if(is.data.frame(o_data)){
    o_data <- o_data[,1:2]
    sub_data <- subset[,1:2]
    xlab <- names(o_data)[1]
    ylab <- names(o_data)[2]
  } else {
    o_data <- o_data
    sub_data <- subset
    xlab <- "Row"
    ylab <- "Data"
  }

  par(mfrow=c(1,2),
      oma = c(1, 1, 1, 0.2),
      mai = c(0.3, 0.3, 0.2, 0.2),
      mgp = c(0, 0.3, 0),
      tck = tck)

  plot(o_data, axes = FALSE, xlab = "", ylab = "",
       panel.first = c(rect(par("usr")[1],
                            par("usr")[3],
                            par("usr")[2],
                            par("usr")[4], col = r3), grid(col = "white",
                                                           lty = 1, lwd = 1.5)))
  box()
  axis(1, col = r1, pch = 16)
  mtext(xlab, line = 1.2, side = 1)
  axis(2, col = r1, pch = 16)
  mtext(ylab, line = 1.2, side = 2)
  mtext("Full Data", line = 0.5)

  plot(sub_data, axes = FALSE, xlab = "", ylab = "",
       panel.first = c(rect(par("usr")[1],
                            par("usr")[3],
                            par("usr")[2],
                            par("usr")[4], col = r3), grid(col = "white",
                                                           lty = 1, lwd = 1.5)))
  box()
  axis(1, col = r1, pch = 16)
  mtext(xlab, line = 1.2, side = 1)
  axis(2, col = r1, pch = 16)
  mtext(ylab, line = 1.2, side = 2)
  mtext("Subsample Data", line = 0.5)
}


