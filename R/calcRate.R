#' Calculate rate via local linear regression or 2-point slope.
#'
#' \code{calcRate} parses a dataframe, or its subset(s) to calculate the rate of change of oxygen concentration over time.
#'
#' @param df Dataframe.
#' @param from Number, or a list.
#' @param to Number, or a list.
#' @param by Character. Select method of subsetting. If by \code{'time'}(default) or \code{'row'}, \code{calcRate} selects the values inclusive. If by \code{'o2'}, the first occurrence of the O2 value in \code{'from'}, and the last occurrence of the O2 value in \code{'to'}, are selected. If by \code{'proportion'}, he proportion, based on the minimum and maximum oxygen concentration. At 1, this selects the highest known oxygen value. At 0, this selects the lowest known oxygen value. Note that due to the noisy nature of respiration data, the lowest known oxygen value may not represent the last row of the dataframe.
#'
#' @return A summary list.
#' @export
#'
#' @examples
#' data(sardine)
#' calcRate(sardine, 200, 1800)                 # default subset by 'time'
#' calcRate(sardine, 93, 92, by = 'o2')         # subset by O2
#' calcRate(sardine, 200, 1800, by = 'row')     # subset by row
#' calcRate(sardine, .8, .2, by = 'proportion') # subset by proportion
#'
#' # Using a list in 'from' and 'to' calculates multiple regressions:
#' data(intermittent)
#' calcRate(intermittent, c(200,2300,4100), c(1800,3200,4600), by = 'time')
#'
calcRate <- function(df, from = NULL, to = NULL, by = 'time') {
  # perform lm on entire dataset if subset is not defined:
  if(is.null(from) && is.null(to)) {
    from <- 1
    to   <- df[, 1][length(df[, 1])]
    by   <- 'row'
  }
  # perform some error checks:
  if (length(from) != length(to)) # check inputs are of same length
    stop('Please check \'to\' and \'from\' arguments.')
  if (!is.numeric(from) | !is.numeric(to)) # check subset inputs are numeric
    stop('\'to\' and \'from\' arguments must be numeric.')
  index <- cbind(from, to)   # create matrix of dataframe indices
  # for each row in the matrix, apply the function linReg and list the output:
  out <- apply(index, 1, linReg, df = df, by = by)
  names(out) <- sprintf('rep%i', 1:length(out)) # a list must have a name
  summ <- lapply(out, '[[', 'wrap') # extract specific sublist from output
  summ <- do.call(rbind, summ) # bind the list into a dataframe
  print(summ)
  # print the rate, or weighted average of the rate:
  w.sum <- (summ$to.time - summ$from.time) # weighted sum
  av <- mean(summ$b1) # average of slopes
  w.av <- sum(summ$b1 * w.sum) / sum(w.sum) # weighted average of slopes
  cat('\n', sprintf('Rate          %g', av))  # print results
  cat('\n', sprintf('Weighted rate %g', w.av))
  # plot the subset for location check:
  allsets <- lapply(out, '[[', 'subdf') # extract specific sublist from output
  allsets <- do.call(rbind, allsets) # combine subset lists into one dataframe
  dplot <- ggplot() +
    geom_point(data = df, aes(df[[1]], df[[2]])) +
    geom_point(data = allsets, aes(allsets[[1]], allsets[[2]]), colour ='goldenrod') +
    labs(x = 'Time', y = 'DO') +
    theme_respr() +
    geom_blank()
  print(dplot)
  out <- list(alldata = out, summary = summ, average = av, weighted.average = w.av)
  class(out) <- 'calcRate'
  return(invisible(out))
}

#' @export
plot.calcRate <- function(x, rep = 1) {
  message('Plotting...this may take a while for large datasets.')
  sub <- x[[rep]] # extract list group
  df <- sub$df # main df
  sdf <- sub$subdf # sub df
  lmfit <- sub$lmfit # previous lm call
  # let's plot:
  p1 <- main_plot(df, sdf) # main + subset plot
  p2 <- sub_plot(sdf) # subset plot
  p3 <- residual_plot(lmfit)
  p4 <- qq_plot(lmfit)
  cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2, align = 'hv')
}



# Internal function to perform linear regression for calcRate.
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
    from.row <- Position(function(x) x <= (from * (max(df[[2]]) - min(df[[2]])) + min(df[[2]])), df[[2]])
    to.row   <- Position(function(x) x <= (to * (max(df[[2]]) - min(df[[2]])) + min(df[[2]])), df[[2]], right = T)
    # to.row   <- Position(function(x) x >= (to * (max(df[[2]]) - min(df[[2]])) + min(df[[2]])), df[[2]], right = T) # alternative
    subdf    <- df[from.row:to.row, ]
  }
  # check that the subset meets minimum requirements
  if (nrow(subdf) <= 3 | max(subdf[, 2]) == min(subdf[, 2]))
    stop('Data subset is too narrow. Please select a larger threshold.')
  # extract o2 values associated with 'from' and 'to'
  from.o2 <- df[from.row, ][[2]]
  to.o2   <- df[to.row, ][[2]]
  from.time <- df[from.row, ][[1]]
  to.time <- df[to.row, ][[1]]
  row.width <- nrow(subdf)
  # perform regression
  lmfit <- lm(subdf[[2]] ~ subdf[[1]], subdf)
  # generate output:
  b0   <- coef(lmfit)[[1]]
  b1   <- coef(lmfit)[[2]]  # slope
  r.sq <- summary(lmfit)$r.squared  # r-square
  wrap <- data.frame(b0 = b0,
    b1 = b1,
    r.sq,
    from.row,
    to.row,
    row.width,
    from.time,
    to.time,
    from.o2,
    to.o2)
  out  <- list(df = df, subdf = subdf, lmfit = lmfit, wrap = wrap)
  return(out)
}
