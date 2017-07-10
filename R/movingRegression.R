#' @title Beginner implementation of a moving regression
#' @description Runs multiple regressions of width (span * length of data)
#' across the entire dataset from beginning to end. Extremely fast
#' implementation - can run 10k regressions in 3 seconds on a laptop (eyeball
#' benchmark). You can easily plot the regressions. I still need to figure out
#' how to pluck the relevant datasets associated with each regression. With some
#' luck, I may use this to determine basal and max RO2?
#' @param df Dataframe. Needs to be formatted by prepData().
#' @param span Numeric. Width of window, as a proportion of the length of the
#' entire dataset. Defaults to 0.1. Must be between 0 and 1.
#' @return NULL
#' @author Januar Harianto
#' @export
#' @examples
#' data("sardine")                 # load data
#' plot(sardine)                   # preview dataset
#' reg <- movingReg(sardine, .2)   # perform rolling regression
#' plot(reg$beta)                  # plot change in rate over index of time
#'                                 # max and min rates can be visualised
#' plot(density(reg$beta))         # density plot of rate

movingReg <- function(df, span = .1) {
  names(df) <- c("x", "y")  # rename for better ID
  old <- Sys.time()                   # grab current time (for simple benchmark)
  reg <- lm(y ~ x, df, x = T, y = T)  # define the regression
  x <- reg$x                  # x as vector
  y <- reg$y                  # y as vector
  nr <- dim(matrix(y))[1]     # count no. of rows in dataframe
  width <- floor(span * nr)   # size of window based on span (rounded down)
  ncoef <- length(coef(reg))
  stopifnot(width >= ncoef, width <= nr)  # stop conditions
  terms <- reg$terms                         # grab terms from lm function
  inds <- embed(1:nr, width)[, width:1]      # create matrix of indexes at width
  sumrys <- apply(inds, 1, function(ix) {
    fit <- lm.fit(x[ix, , drop = F], y[ix])  # run regression based on matrix
    fit$terms <- terms                       # pass terms to lm structure
    class(fit) <- "lm"                       # make sure terms can be called
    # summary(fit) # (alt)
    summary(fit)[c("coefficients", "sigma", "r.squared")]
  })
  # compile terms into matrices
  coeff     <- sapply(sumrys, function(sm) coef(sm)[, "Estimate"])
  intercept <- t(coeff)[, 1]  # don't need sapply here cos it grabs from coeff
  beta      <- t(coeff)[, 2]
  std.err   <- sapply(sumrys, function(sm) coef(sm)[, "Std. Error"])
  sigma     <- sapply(sumrys, "[[", "sigma")
  r.sq      <- sapply(sumrys, "[[", "r.squared")
  countr    <- NROW(r.sq)
  # generate output
  output <-
    list(# coeff = coeff,
         intercept = intercept,
         beta = beta,
         sigma = sigma,
         r.sq = r.sq,
         std.err = std.err)
  new <- Sys.time() - old                          # calculate time elapsed
  new <- round(unclass(new)[1], 1)                 # convert to number
   cat(sprintf('%d regressions fitted', countr),   # print # regressions done
       sprintf('in %g seconds', new), '\n')        # print time taken
   class(output) <- 'movingReg'
   invisible(output)
}
