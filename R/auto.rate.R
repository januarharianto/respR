#' Determine min, max or 'ideal' rate
#'
#' @param df Data frame.
#' @param width Numeric.
#' @param logic Character.
#' @param background Numeric, or an object of class \code{'calc.bg.rate'}
#' @param plot Character.
#'
#' @return A summary.
#' @importFrom dplyr mutate select arrange filter
#' @export
#'
#' @examples
#' TBC
auto.rate <- function(df, width = round(0.1 * nrow(df)), logic = 'automatic', background = NULL) {
  # STEP 1: perform rolling regressions
  message(sprintf("Performing rolling regressions of width %d (rows)...", width))
  allregs <- roll.reg(df, width)  # perform rolling regression
  allregs <- mutate(allregs, to.row = row(allregs)[,1], from.row = to.row-width+1)  # identify row indices per regression
  allregs <- na.omit(allregs) # remove NA results, as they are not useful
  allregs <- mutate(allregs, from.time = df[from.row,][[1]], to.time = df[to.row, ][[1]],
    from.o2 = df[from.row,][[2]], to.o2 = df[to.row,][[2]],
    row.width = to.row-from.row+1, time.width = to.time-from.time,
    o2.width = from.o2-to.o2)  ## append time and o2 indices, and all index widths
  allregs <- select(allregs, 1:3, 5, 4, 6:12)  # slightly rearrange the data frame
  message(sprintf("%d regressions fitted,", nrow(allregs)))

  # STEP 3: determine method to analyse roll outputs, based on 'logic' argument
  if (logic == 'max') { allregs.ranked <- arrange(allregs, b1)
  } else if (logic == 'min') { allregs.ranked <- arrange(allregs, desc(b1))
  } else if (logic == 'automatic') {
    # EXPERIMENTAL: use rolling regression and density kernel estimation to compute 'best' rate
    # 1. perform kernel density estimation on regression data:
    dens <- density(allregs$b1, na.rm = T, n = length(allregs$b1))
    # 2. grab peak values and rank them in order of decreasing density
    peaks <- which.peaks(dens) # see below for function usage
    peaks.ranked <- peaks[order(peaks$density, decreasing = T),]
    # 4. also grab the binwidth used to calculate density:
    bw <- dens$bw
    # 5. using the binwidth, gather all regressions that were used to determine each peak
    match.peaks <- lapply(peaks.ranked[,1], function(x) filter(allregs, b1 <= (x+bw/2) & b1 >= (x-bw/2)))
    # 6. the matched areas of the data frame may be fragmented, we list them out
    frags <- lapply(1:length(match.peaks), function(x)
      split(match.peaks[[x]], c(0, cumsum(abs(diff(match.peaks[[x]]$from.time)) > allregs$time.width[1]))))
    # 7. the largest fragment is retained
    best <- lapply(1:length(frags), function(x) which.max(do.call(rbind, lapply(frags[[x]], nrow))))
    best <- do.call(rbind, best)
    best.frags <- unname(mapply(function(x, y) frags[[x]][y], 1:length(frags), best))
    # 8. perform calc.rate best fragments
    # here we reanalyse the data again based on the matched max/min indices in each fragment
    allregs.ranked <- lapply(1:length(frags), function(x)
      calc.rate(df, min(best.frags[[x]]$from.row), max(best.frags[[x]]$to.row), by = 'row', background = background, plot = F))
    message(sprintf("%d kernel density peaks in rates detected and ranked.", length(allregs.ranked)))
  } else stop("The 'logic' argument is incorrect. Please check that it is either 'max', 'min' or 'automatic'.", call. = F)

  if (logic == 'max' | logic == 'min') {
    # split the dataframe
    allregs.results <- select(allregs.ranked, 1:3)
    rownames(allregs.results) <- sprintf('rank%i', 1:nrow(allregs.results))
    allregs.subsets <- select(allregs.ranked, 4:12)
    rownames(allregs.subsets) <- sprintf('rank%i', 1:nrow(allregs.results))
    # generate the ranked outputs
    allregs.ranked <-
      lapply(1:nrow(allregs), function(x)
        list(results = allregs.results[x,],
          subsets = allregs.subsets[x,],
          average = allregs.results[x,]$b1,
          weighted.average = allregs.results[x,]$b1))
    names(allregs.ranked) <- sprintf('rank%i', 1:nrow(allregs))
  }
  if (logic == 'automatic'){
    out <- list(dataframe = df, output = allregs.ranked,
      ranked.rollregs = allregs.ranked, ranked.peaks = peaks.ranked, binwidth = bw)
  } else out <- list(dataframe = df, output = allregs.ranked, binwidth = NULL)
  class(out) <- 'auto.rate'
  return(out)
}


#' @export
print.auto.rate <- function(x, rank = 1) {
  top.r <- x$output[[rank]]$results
  top.sub <- x$output[[rank]]$subsets
  if (is.null(x$binwidth)) {
    cat(sprintf("Rank %d result:\n", rank))
  } else {
    cat(sprintf("Rank %d result based on kernel density",
      rank), sprintf("estimation of bin width %f (b1):\n",
        x$binwidth))
    top.r <- data.frame(top.r, density = x$ranked.peaks[rank,][2])
  }
  print(top.r)
  cat(sprintf("\nData location(s) for rank %d result:\n", rank))
  print(top.sub)
}


#' @export
summary.auto.rate <- function(x, rank = 1) {
  top5.results <- do.call(rbind, lapply(1:5, function(y) x$output[[y]]$results))
  rownames(top5.results) <- sprintf('rank%i', 1:5)
  top5.subsets <- do.call(rbind, lapply(1:5, function(y) x$output[[y]]$subsets))
  rownames(top5.subsets) <- sprintf('rank%i', 1:5)
  if (is.null(x$binwidth)) {
    cat("Top 1-5 results:\n")
  } else {
    cat(sprintf("Top 1-6 results, based on kernel density estimation of bin width %f (b1):\n", x$binwidth))
    top5.results <- cbind(top5.results, density = x$ranked.peaks[1:5,][2])
  }
  print(top5.results)
  cat("\nData location(s) for top 1-5 results:\n")
  print(top5.subsets)
}

plot.auto.rate <- function(x, rank = 1) {
  message("Generating diagnostic plots...")
  if (is.null(x$binwidth)) {
    from <- x$output[[rank]]$subsets$from.row
    to <- x$output[[rank]]$subsets$to.row
    a.df <- x$dataframe
    a.sdf <- x$dataframe[from:to, ]
    lmfit <- lm(a.sdf[[2]] ~ a.sdf[[1]], a.sdf)
  } else {
    a.df <- x$output[[rank]]$alldata$rep1$df
    a.sdf <- x$output[[rank]]$alldata$rep1$subdf
    lmfit <- x$output[[rank]]$alldata$rep1$lmfit
  }
  p1 <- main_plot(a.df, a.sdf)
  p2 <- sub_plot(a.sdf)
  p3 <- residual_plot(lmfit)
  p4 <- qq_plot(lmfit)
  # INCOMPLETE - need rolling reg and density plots
  output <- cowplot::plot_grid(p1, p2, p3, p4, align = "hv")
  print(output)
  message("Done.")
}


# internal function
# perform rolling regressions
roll.reg <- function(df, win) {
  x <- matrix(df[[1]])
  y <- matrix(df[[2]])
  lmfit <- roll::roll_lm(x, y, win)
  c   <- lmfit$coefficients[,1]
  b1  <- lmfit$coefficients[,2]
  rsq <- lmfit$r.squared[,1]
  # output:
  out <- data.frame(b0 = c, b1 = b1, r.sq = rsq)
  return(out)
}

# internal function
# identify peaks in density
which.peaks <- function(dens) {
  d.x <- dens$x
  d.y <- dens$y
  df <- data.frame(slope = d.x, density = d.y)
  all.peaks <- which(diff(sign(diff(d.y)))==-2)+1
  list.vals <- lapply(all.peaks, function(x) df[x, ])
  out <- as.data.frame(do.call(rbind, list.vals))
  names(out) <- c('b1.reference', 'density')
  out
}
