#' Subsample a data frame object
#'
#' A simple function that subsamples a data frame or numeric vector in order to
#' "thin" large datasets.
#'
#' Two subsampling methods are provided. The `n` input selects every n'th
#' element or row, or alternatively the `length.out` input uniformly subsamples
#' the data to the desired length.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR>
#'
#' @return Returns a subsampled data frame or vector object depending on input.
#'
#' @param x data frame or vector. The data to subsample.
#' @param n numeric. Subsample every `n` elements or rows.
#' @param length.out numeric. Subsample to a specific length or number of rows.
#' @param random_start logical. Defaults to FALSE. If TRUE, randomises the start
#'   position from which to start the subsample (applies to `n` input only).
#' @param plot logical. Defaults to TRUE. Plots the data. If there are multiple
#'   columns in the data frame, only the first two are plotted. Vectors are
#'   plotted against a position index.
#'
#' @export
#'
#' @examples
#' # Subsample by every 200th row:
#' subsample(squid.rd, n = 200)
#'
#' # Subsample to 100 rows:
#' subsample(sardine.rd, length.out = 100)
#'
#' # Subsample with random starting position:
#' subsample(sardine.rd, n = 20, random_start = TRUE)
#'
#' # Subsample a vector
#' subsample(sardine.rd[[2]], n = 20)

subsample <- function(x, n = NULL, length.out = NULL, random_start = FALSE, plot = TRUE) {

  if(is.null(n) && is.null(length.out))
    stop("subsample: One of 'n' or 'length.out' is required.")
  if(!is.null(n) && !is.null(length.out))
    stop("subsample: Only one of 'n' or 'length.out' should be entered.")

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
  } else {
    o_data <- o_data
    sub_data <- subset
  }

  par(mfrow=c(1,2),
      oma = oma_def,
      mai = mai_def,
      mgp = mgp_def,
      tck = tck_def)

  plot(o_data, axes = FALSE, xlab = "", ylab = "",
       panel.first = c(rect(par("usr")[1],
                            par("usr")[3],
                            par("usr")[2],
                            par("usr")[4], col = r3), grid(col = "white",
                                                           lty = 1, lwd = 1.5)))
  box()
  axis(1, col = r1, pch = pch_def)
  axis(2, col = r1, pch = pch_def)
  mtext("Full Data", line = 0.5)

  plot(sub_data, axes = FALSE, xlab = "", ylab = "",
       panel.first = c(rect(par("usr")[1],
                            par("usr")[3],
                            par("usr")[2],
                            par("usr")[4], col = r3), grid(col = "white",
                                                           lty = 1, lwd = 1.5)))
  box()
  axis(1, col = r1, pch = pch_def)
  axis(2, col = r1, pch = pch_def)
  mtext("Subsample Data", line = 0.5)
}


