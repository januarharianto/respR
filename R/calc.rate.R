#' Calculate rate of change in oxygen over time
#'
#' `calc.rate` calculates the rate of change in oxygen concentration over time
#' in a data frame. You can perform single or multiple regressions on subsets of
#' the data frame by calling the `from` and `to` arguments.
#'
#' There are no units involved in `calc.rate`. This is a deliberate decision.
#' Units are called in a later function when volume- and/or weight-specific
#' rates of oxygen concentration are computed in [calc.mo2()].
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#'
#' @param df data frame. Should contain time (column 1) vs oxygen concentration
#'   (column 2) data.
#' @param from numeric vector. Defines the upper bound(s) of the data frame
#'   subset. Defaults to "`NULL`".
#' @param to numeric vector. Defines the lower bound(s) of the data frame
#'   subset. Defaults to "`NULL`".
#' @param by string. Describes the method used to subset the data frame.
#'   Defaults to `"time"`. Available: "`time`", "`o2`", "`proportion`", "`row`".
#' @param bg numeric, or an output of class `calc.bg.rate`. Value for backgroung
#'   respiration.
#' @param plot logical. When set to "`TRUE`" (default), will plot a quick visual
#'   of the data and its subset(s).
#' @param verbose logical. Set to FALSE to surpress messages. Defaults to TRUE.
#'
#' @return An object of class `calc.rate` containing a list of outputs:
#' \describe{
#' \item{`data.frame`}{The original data frame.}
#' \item{`subset.df`}{A list of subset data frames used for computations.}
#' \item{`by`}{The subsetting method. Possible outputs: "`time`", "`o2`",
#' "`proportion`" or "`row`".}
#' \item{`from`}{a numeric vector. Defines the upper bound(s) of the data frame
#' subset.}
#' \item{`to`}{A numeric vector. Defines the lower bound(s) of the data frame
#' subset.}
#' \item{`results`}{A data frame summary of the result(s), which include
#' regression coefficients and subset locations.}
#' \item{`background`}{Appears only if the argument `bg` is not `NULL`. The
#' background rate of change.}
#' \item{`rate`}{A value for the calculated rate of change in oxygen
#' concentration, averaged if multiple values exist. The average is weighted to
#' time.}
#' \item{`adj.rate`}{Same as `rate` above, but includes background correction.}
#' }
#'
#' @importFrom dplyr bind_rows mutate select everything
#' @importFrom tibble as.tibble
#' @export
#'
#' @seealso [calc.bg.rate()]
#' @examples
#' data(sardine)
#' calc.rate(sardine, from = 200, to = 1800)     # default subset by 'time'
#' calc.rate(sardine, 93, 92, by = 'o2')         # subset by O2
#' calc.rate(sardine, 200, 1800, by = 'row')     # subset by row
#' x <- calc.rate(sardine, .8, .2, by = 'proportion') # subset by proportion
#' x
#' summary(x)
#' plot(x)
#'
#' # Using a list in 'from' and 'to' calculates multiple regressions:
#' data(intermittent)
#' calc.rate(intermittent, c(200,2300,4100), c(1800,3200,4600), by = 'time')
#'
#' # calculating rate with background adjustments of known value
#' calc.rate(sardine, 200, 800, bg = -0.00002)
#'
#' # calculating rate with background adjustments made from calc.bg.rate
#' bg <- calc.bg.rate(urchin2013, 1, 19)
#' ureg <- check.input(urchin2013, 1, 2)
#' calc.rate(ureg, 20, 40)  # without bg adjustment
#' calc.rate(ureg, 20, 40, bg = bg) # with bg adjustment
#'
calc.rate <- function(df, from = NULL, to = NULL, by = 'time', bg = NULL,
  plot = T, verbose = T) {
  # ======================
  # Catch input errors
  # There's probably a better way to do this... but here goes:
  # Check that df is a data.frame object:
  if (any(class(df) == "data.frame") == FALSE)
    stop("Input must be a data.frame object.")
  # Check `from` and `to` are correct:
  if (is.numeric(from) && is.numeric(to)) {
    if (by == "time" | by == "row") {
      if (any(from > to))
        stop("The `from` argument must not be bigger than the `to` argument.")
    }
  }
  # Make sure that `by` is correct:
  if (!(by == "time" | by == "row" | by == "o2" | by == "proportion"))
    stop("The `by` argument can only be 'time', 'row', 'o2' or 'proportion'.")

  # ======================
  # Inform user that lm will be performed on entire dataset if "start" and "end"
  # are not defined:
  if (is.null(from) && is.null(to)) {
    if (verbose) {
    message("Data bounds not set. Performing analysis on entire data frame.")
    }
    from <- 1; to <- nrow(df); by <- 'row'
  }
  # Tibbles mess with apply functions, so we convert them into data frames:
  if (any(class(df) == "tbl")) class(df) <- "data.frame"
  if (any(class(df) == "data.table")) class(df) <- "data.frame"
  # Error check: ensure that subset inputs are numeric:
  if (!is.numeric(from) | !is.numeric(to))
    stop("'to' and 'from' arguments must be numeric.")
  # ----------------------------------------------------------------------------
  # Here we perform the regression and generate the summary results.
  index <- cbind(from, to)  # identify subset locations
  locs <- apply(index, 1, locate.subdfs, df = df, by = by)
  # Perform linear regressions and add data index locations:
  fits <- lapply(1:length(locs), function(x)
    data.frame(lmfit(locs[[x]][[3]]),
      row     = locs[[x]][[1]],
      endrow  = locs[[x]][[2]],
      time    = df[locs[[x]][[1]],][,1],
      endtime = df[locs[[x]][[2]],][,1],
      oxy     = df[locs[[x]][[1]],][,2],
      endoxy  = df[locs[[x]][[2]],][,2]))
  fits <- bind_rows(fits)  # bind into data.frame
  # Add additional calculations:
  fits <- mutate(fits,
    row.len  = endrow - row + 1,
    time.len = endtime - time,
    rate.2pt = (endoxy - oxy) / time.len)
  # ----------------------------------------------------------------------------
  # If background (bg) argument is provided, correct for bg and update summary.
  # Also calculate mean and weighted mean if multiple regressions were made:
  if (is.null(bg)) {
    results <- fits
    rate    <- mean(results$b1)
    w.rate  <- weighted.mean(results$b1, results$time.len)
    adj.w.rate <- NULL
  } else if (class(bg) == "calc.bg.rate") {
    bg <- bg$rate
    results <- mutate(fits, bg = bg, "b1-bg" = b1 - bg)
    results <- select(results, b0, b1, bg, `b1-bg`, everything())
    rate    <- mean(results$b1)
    w.rate  <- weighted.mean(results$b1, results$time.len)
    adj.w.rate <- w.rate - results$bg[[1]]  # bg-adjust, if needed
  } else if (class(bg) == "numeric") {
    results <- mutate(fits, bg = bg, "b1-bg" = b1 - bg)
    results <- select(results, b0, b1, bg, `b1-bg`, everything())
    rate    <- mean(results$b1)
    w.rate  <- weighted.mean(results$b1, results$time.len)
    adj.w.rate <- w.rate - results$bg[[1]]  # bg-adjust, if needed
  }
  # ----------------------------------------------------------------------------
  # Plot the result, if set to TRUE
  alldf <- lapply(1:length(locs), function(x) as.tibble(locs[[x]][[3]]))
  if (plot) multi.p(df, alldf, title = F)
  # ----------------------------------------------------------------------------
  # Generate output data:
  if (is.null(bg)) {
    # This is generated if there is no "bg" argument:
    out <- list(
      id         = "calc.rate",
      data.frame = df,
      subset.df  = alldf,
      by         = by,
      from       = from,
      to         = to,
      results    = results,
      rate       = rate,
      w.rate     = w.rate,
      adj.w.rate = adj.w.rate)
  } else {
    # This is generated if "bg" argument is included:
    out <- list(
      id         = "calc.rate",
      data.frame = df,
      subset.df  = alldf,
      from       = from,
      to         = to,
      by         = by,
      results    = results,
      rate       = rate,
      w.rate     = w.rate,
      background = bg,
      adj.w.rate = adj.w.rate)
  }
  if(verbose) {
    message(sprintf("Data subset is by %s.", out$by))
    if(!is.null(bg)) message("Background correction recognised and applied.")
  }
  if (nrow(out$results) > 1) {
    message(sprintf("Result are averaged across %g data subsets, and weighted to time.",
      nrow(out$results)))
  }
  class(out) <- 'calc.rate'  # classy stuff :D
  return(out)
}









#' @export
print.calc.rate <- function(x) {
  if(is.null(x$adj.rate)) {
    cat(sprintf("Rate (b1): %f\n", x$rate))
  } else {
    cat(sprintf("Background (bg)  : %f\n", x$background))
    cat(sprintf("Rate (b1)        : %f\n", x$rate))
    cat(sprintf("Adj. Rate (b1-bg): %f\n", x$adj.rate))
  }
}








#' @export
summary.calc.rate <- function(x) {
  # cat("Summary\n")
  print(x$results)
}









#' @export
plot.calc.rate <- function(x, rep = 1) {
  message('Plotting...this may take a while for large datasets.')
  sdf <- x$subset.df[[rep]] # extract data from list
  df <- x$data.frame # main df
  lmfit  <- lm(sdf[[2]] ~ sdf[[1]], sdf)
  # let's plot:
  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow=c(2,2))  # replace par settings
  multi.p(df, sdf)  # full timeseries with lmfit
  sub.p(sdf)  # subset timeseries
  residual.p(lmfit)  # residual plot
  qq.p(lmfit)  # qqplot
  par(pardefault)  # revert par settings to original
}








# ==============================================================================
# Internal functions


#' Subset the main data based on the "by" argument.
#'
#' Also saves row index data in the output.
#'
#' This is an internal function. The typical user should not see this.
#'
#' @md
#' @param df data frame.
#' @param index data frame.
#' @param by string.
#' @return A list object.
#'
locate.subdfs <- function(df, index, by) {
  # Extract index
  from <- index[[1]]
  to <- index[[2]]
  # How are we subsetting the data?
  if (by == "time") {
    row  <- Position(function(x) x >= from, df[[1]])
    endrow    <- Position(function(x) x <= to, df[[1]], right = T)
    subdf     <- df[df[,1] >= from & df[,1] <= to, ]
  } else if (by == 'row') {
    row  <- from
    endrow    <- to
    subdf     <- df[from:to, ] # subset data directly by row
  } else if (by == 'o2') {
    row  <- Position(function(x) x <= from, df[[2]])
    endrow    <- Position(function(x) x <= to, df[[2]])
    subdf     <- df[row:endrow, ]
  } else if (by == 'proportion') {
    max.x <- max(df[2])
    min.x <- min(df[2])
    row  <- Position(function(x) x <= (from*(max.x-min.x)+min.x), df[[2]])
    endrow    <- Position(function(x) x <= (to*(max.x-min.x)+min.x), df[[2]])
    subdf     <- df[row:endrow, ]
  }
  # Error checks:
  # Check that data length is appropriate for further analysis:
  if (nrow(subdf) <= 3 | max(subdf[, 2]) == min(subdf[, 2]))
    stop('Data subset is too narrow. Select a larger threshold.', call. = F)
  # Ensure that end time is later than start time:
  if (endrow - row <= 0)
    stop("End time/row is earlier than or equal to start time.", call. = F)
  output <- list(row, endrow, subdf)
  return(output)
}









#' Perform OLS regression on subset and save b0, b1 and rsq.
#'
#' Used within `calc.rate()`.
#'
#' This is an internal function. The typical user should not see this.
#'
#' @param x data frame.
#' @return a data frame.
#'
lmfit <- function(x) {
  time <- x[[1]]
  o2   <- x[[2]]
  fit  <- lm(o2 ~ time, x)
  b0   <- coef(fit)[[1]]
  b1   <- coef(fit)[[2]]  # slope
  rsq  <- signif(summary(fit)$r.squared, 3) # r-square
  out  <- data.frame(b0, b1, rsq)
  return(out)
}
