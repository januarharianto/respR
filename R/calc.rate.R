#' Calculate rate via local linear regression or 2-point slope.
#'
#' \code{calc.rate} parses a dataframe, or its subset(s) to calculate the rate of change of oxygen concentration over time.
#'
#' @param df Dataframe.
#' @param from Number, or a list.
#' @param to Number, or a list.
#' @param by Character. Select method of subsetting. If by \code{'time'}(default) or \code{'row'}, \code{calc.rate} selects the values inclusive. If by \code{'o2'}, the first occurrence of the O2 value in \code{'from'}, and the last occurrence of the O2 value in \code{'to'}, are selected. If by \code{'proportion'}, he proportion, based on the minimum and maximum oxygen concentration. At 1, this selects the highest known oxygen value. At 0, this selects the lowest known oxygen value. Note that due to the noisy nature of respiration data, the lowest known oxygen value may not represent the last row of the dataframe.
#' @param background Number, or an object of class \code{calc.bg.rate}.
#' @param plot
#'
#' @return A summary list.
#' @export
#'
#' @examples
#' data(sardine)
#' calc.rate(sardine, from = 200, to = 1800)     # default subset by 'time'
#' calc.rate(sardine, 93, 92, by = 'o2')         # subset by O2
#' calc.rate(sardine, 200, 1800, by = 'row')     # subset by row
#' calc.rate(sardine, .8, .2, by = 'proportion') # subset by proportion
#'
#' # Using a list in 'from' and 'to' calculates multiple regressions:
#' data(intermittent)
#' calc.rate(intermittent, c(200,2300,4100), c(1800,3200,4600), by = 'time')
#'
#' # calculating rate with background adjustments of known value
#' calc.rate(sardine, 200, 800, background = -0.00002)
#'
#' # calculating rate with background adjustments made from calc.bg.rate
#' bg <- calc.bg.rate(urchin2013, 1, 21)
#' ureg <- check.input(urchin2013, 1, 2)
#' calc.rate(ureg, 20, 40)  # without bg adjustment
#' calc.rate(ureg, 20, 40, background = bg) # with bg adjustment
#'
calc.rate <- function(df, from = NULL, to = NULL, by = 'time', background = NULL, plot = T) {
  # perform lm on entire dataset if subset is not defined:
  if (is.null(from) && is.null(to)) {
    from <- 1; to <- nrow(df); by <- 'row'
  }
  # perform some error checks:
  # check subset inputs are numeric:
  if (!is.numeric(from) | !is.numeric(to)) stop("'to' and 'from' arguments must be numeric.")
  index <- cbind(from, to)   # create matrix of dataframe indices
  # for each row in the matrix, apply the function linReg and list the output:
  out <- apply(index, 1, linReg, df = df, by = by)
  names(out) <- sprintf('rep%i', 1:length(out)) # a list must have a name

  # extract tables and bind them if necessary
  results <- lapply(out, '[[', 'results') # extract summary sublist from output for all reps
  results <- do.call(rbind, results) # bind the sublistslist into a dataframe
  subsets <- lapply(out, '[[', 'subsets')
  subsets <- do.call(rbind, subsets)

  # now check if there is background rate
  if (!is.null(background)) {  # if bg is not null, process bg data
    if (class(background) == 'calc.bg.rate') bg <- background$average
    if (class(background) == 'numeric') bg <- background
    results$bg <- bg
    results$bg.adj.b1 <- results$b1 - bg
  }

  av <- mean(results$b1)  # calculate average
  w.av <- weighted.mean(results$b1, subsets$time.width)  # calculate weighted average - based on time

  # correct average and weighted average to background, if needed
  if (!is.null(background)) {
    av <- av - bg
    w.av <- w.av - bg
  }
  # plot the result (this is separate from the print call)
  allsets <- lapply(out, '[[', 'subdf') # extract specific sublist from output
  #allsets <- dplyr::bind_rows(allsets, .id = "rep") # bind into one dataframe, and group them
  if (plot == T) multi.p(df, allsets, title = F) # print out the plot
  out <- list(alldata = out, results = results, subsets = subsets, average = av, weighted.average = w.av)
  class(out) <- 'calc.rate'
  return(out)
}

#' @export
print.calc.rate <- function(x) {
  cat('Coefficients, R-square:\n')
  print(x$results)
  cat('\nData location(s):\n')
  print(x$subsets)
  # Now summarise:
  av <- x$average
  w.av <- x$weighted.average
  # before printing summary, indicate if results are background-corrected
  cat('\nResult:\n')
  if (length(x$results) > 3) {
    cat('(Results have been background-corrected.)\n')
  }
  # summary results
  result <- data.frame(Rate = c(av, w.av))
  row.names(result) <- c("Mean", "Weighted mean")
  print(result)
}

#' @export
plot.calc.rate <- function(x, rep = 1, method = 'plot') {
  message('Plotting...this may take a while for large datasets.')
  sub <- x$alldata[[rep]] # extract list group
  df <- sub$df # main df
  sdf <- sub$subdf # sub df
  lmfit <- sub$lmfit # previous lm call
  # let's plot:
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow=c(2,2))  # replace par settings
  multi.p(df, sdf)
  sub.p(sdf)
  residual.p(lmfit)
  qq.p(lmfit)
  par(pardefault)  # revert par settings to original
}


# ------------------------------------------------------------------------------
# Internal functions
# ------------------------------------------------------------------------------
# perform linear regression for calc.rate.
linReg <- function(df, lookup, by) {
  # dissect matrix
  from <- lookup[[1]]
  to <- lookup[[2]]
  # how are we subsetting the data?
  if (by == 'time') {
    from.row <- Position(function(x) x >= from, df[[1]])
    to.row   <- Position(function(x) x <= to, df[[1]], right = T)
    # generate data frame
    subdf    <- df[df[, 1] >= from & df[, 1] <= to, ]
  }
  if (by == 'row') {
    from.row  <- from
    to.row    <- to
    subdf     <- df[from:to, ] # subset data directly by row
  }
  if (by == 'o2') {
    from.row <- Position(function(x) x <= from, df[[2]])
    to.row   <- Position(function(x) x <= to, df[[2]])
    # to.row   <- Position(function(x) x >= to, df[[2]], right = T) # alternative
    subdf    <- df[from.row:to.row, ]
  }
  if (by == 'proportion') {
    max.x <- max(df[2])
    min.x <- min(df[2])
    from.row <- Position(function(x) x <= (from * (max.x - min.x) + min.x), df[[2]])
    to.row   <- Position(function(x) x <= (to * (max.x - min.x) + min.x), df[[2]])
    # to.row   <- Position(function(x) x >= (to * (max(df[[2]]) - min(df[[2]])) + min(df[[2]])), df[[2]], right = T) # alternative
    subdf    <- df[from.row:to.row, ]
  }
  # check that the subset meets minimum requirements
  if (nrow(subdf) <= 3 | max(subdf[, 2]) == min(subdf[, 2]))
    stop('Data subset is too narrow. Please select a larger threshold.')
  # check 'from' is earlier than 'to'
  if (to.row - from.row <= 0) stop("End time/row is earlier than or equal to start time.")
  # extract 'from' and 'to' values associated with 'by'
  from.o2 <- df[from.row, ][[2]]
  to.o2   <- df[to.row, ][[2]]
  from.time <- df[from.row, ][[1]]
  to.time <- df[to.row, ][[1]]

  # check lengths
  row.width <- nrow(subdf)
  time.width <- to.time - from.time
  o2.width <- from.o2 - to.o2

  # perform regression
  lmfit <- lm(subdf[[2]] ~ subdf[[1]], subdf)
  # generate output:
  b0   <- coef(lmfit)[[1]]
  b1   <- coef(lmfit)[[2]]  # slope
  r.sq <- summary(lmfit)$r.squared  # r-square
  results <- data.frame(b0 = b0, b1 = b1, r.sq)
  subsets <- data.frame(from.row, to.row,
    from.time, to.time,
    from.o2, to.o2,
    row.width, time.width, o2.width)
  out  <- list(df = df, subdf = subdf, lmfit = lmfit, results = results, subsets = subsets)
  return(out)
}

