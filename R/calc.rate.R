
#' Title
#'
#' @param x
#' @param from
#' @param to
#' @param by
#' @param plot
#'
#' @importFrom data.table data.table rbindlist
#'
#' @return
#' @export
#'
#' @examples
calc.rate <- function(x, from = NULL, to = NULL, by = 'time', plot = T) {

  # Validate inputs
  # Will migrate to assertive package when I get used to it..
  # Ensure "from" and "to" are same length:
  if (length(from) != length(to)) stop("'from' and 'to' have unequal lengths.")

  # Extract data.frame if from object inspect.data
  if(any(class(df) %in% "inspect.data")) x <- x$df

  # Format as data.table
  x <- data.table::data.table(x)

  # If 'from' and 'to' are NULL, we assume that the user is analysing all data
  if (all(sapply(list(from, to), is.null))) {
    from <- 1; to <- nrow(x); by <- "row"
  }

  # Subset the data:
  dt <- lapply(1:length(from), function(z) subset.data(x, from[z], to[z], by))

  # Perform lm on data and extract coefficients
  coefs <- lapply(1:length(to), function(z) linear.fit(dt[[z]]))

    # Extract row, time and DO indices from subsets
  indices <- lapply(1:length(dt), function(z) extract.indices(x, dt, z))

  # Extract row, time and DO indices from subsets and add to results
  rdt <- data.table::rbindlist(lapply(1:length(to), function(x)
    cbind(coefs[[x]], indices[[x]])))

  # Include row and time lengths, and twopoint method in table

  rdt[, rowlength := endrow - row]
  rdt[, timelength := endtime - time]
  rdt[, rate_twopoint := (endoxy - oxy / timelength)]

  # Extract rate_b1
  rate <- rdt[,rate_b1]

  # Plot if TRUE
  if (plot) multi.p(x, dt, title = F)

  # Generate output
  out <- list(
    data = x,
    from = from, to = to, by = by,
    subsets = dt,
    summary = rdt,
    rate = rate
    )

  class(out) <- "calc.rate"
  return(out)
}


# print -------------------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.calc.rate <- function(x) {
  cat("Rate(s):\n")
  print(x$rate)
}


# summary -----------------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
summary.calc.rate <- function(x) {
  cat("Summary:\n")
  print(x$summary)
}



# plot --------------------------------------------------------------------

plot.calc.rate <- function(x, rep = 1) {
  message('Plotting...this may take a while for large datasets.')
  df  <- x$data
  sdf <- x$subsets[[rep]]
  fit <- lm(sdf[[2]] ~ sdf[[1]], sdf)

  pardefault <- par(no.readonly = T)  # save original par settings
  par(mfrow = c(2, 2), mai=c(0.4,0.4,0.3,0.3), ps = 10, cex = 1, cex.main = 1)
  multi.p(df, sdf)  # full timeseries with lmfit
  sub.p(sdf)  # subset timeseries
  residual.p(fit)  # residual plot
  qq.p(fit)  # qqplot
  par(pardefault)  # revert par settings to original
}
# linear.fit --------------------------------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
linear.fit <- function(dt) {
  fit <- lm(dt[[2]] ~ dt[[1]], dt)
  b0   <- coef(fit)[[1]]
  b1   <- coef(fit)[[2]]  # slope
  rsq  <- signif(summary(fit)$r.squared, 3) # r-square
  out  <- data.frame(intercept_b0 = b0, rate_b1 = b1, rsq)
  return(out)
}


# extract.indices ---------------------------------------------------------

#' Title
#'
#' @param x
#' @param subsets
#' @param n
#'
#' @return
#' @export
#'
#' @examples
extract.indices <- function(x, subsets, n) {
  # This grabs the first and last-row data
  fl <- subsets[[n]][, .SD[c(1, .N)]]
  # Add row indices while flattening data into a row:
  out <- data.table::data.table(
    row = match(fl[[1]][1], x[[1]]), endrow = match(fl[[1]][2], x[[1]]),
    time = fl[[1]][1], endtime =  fl[[1]][2],
    oxy =fl[[2]][1], endoxy = fl[[2]][2])
  return(out)
}
