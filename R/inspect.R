#' Check for common errors in respirometry data
#'
#' `inspect()` scans and subsets a data.frame object for errors that may affect
#' the use of various functions in `respR`. By default, the function scans only
#' the first 2 columns of a data frame and assumes that the first column is time
#' data. A plot of the data is also produced, including a rolling regression
#' plot using a width of `floor(0.1 * nrow([data frame])` for a quick visual
#' inspection of the rate pattern (or stability) of the data. Note that rates
#' for oxygen uptake are returned as negative and plotted on a reverse axis;
#' higher oxygen uptake rates are higher on the rate plot (more negative).
#'
#' Time columns are checked for NA/NaN values, sequential time, duplicate time
#' and evenly-spaced time data. Oxygen columns are simply checked for NA/NaN
#' data. Once data checks are complete, the function produces a list object
#' which may be directly loaded into [calc_rate()], [calc_rate.bg()],
#' [calc_rate.ft()], and [auto_rate()] for further analyses.
#'
#' If you wish to scan more than two columns, you can do so by specifying the
#' `time` and `oxygen` arguments to select specific columns of a large data
#' frame. However, the function will not produce a plot. Thus, you may inspect
#' flowthrough respirometry data, which usually contains oxygen values for
#' inflow and outflow, by specifying a vector of column numbers, e.g. `oxygen =
#' c(2,3)`.
#'
#' @section Failed Checks: It should be noted most of these checks are for
#'   exploratory purposes only; they help diagnose potential issues with the
#'   data. For instance, very long experiments could have had sensor dropouts
#'   the user is completely unaware of. Other issues are not issues at all - for
#'   instance, an uneven time warning can result from using decimalised minutes,
#'   which is a completely valid time metric.
#'
#'   If some of these checks fail, it should generally not hinder analysis of
#'   the data. respR has been coded to rely on linear regression on exact data
#'   values, and not make assumptions about data spacing. Therefore issues such
#'   as missing or NA/NaN values, duplicate values, or uneven time spacing
#'   should not cause any erroneous results, as long as they do not occur over
#'   large regions of the data. The only major potential issue is if time data
#'   are not sequential. This could cause unknown results and incorrect rates to
#'   be returned.
#'
#' @param df data.frame object. Accepts any object of class `data.frame`.
#' @param time numeric vector. Defaults to NULL. This specifies the column
#'   number(s) of the time data to subset.
#' @param oxygen numeric vector. Defaults to NULL. This specifies the column
#'   number(s) of the oxygen data to subset.
#' @param plot logical. Defaults to TRUE. Plots the data for quick visual
#'   diagnosis. Works only when the subset dataframe contains exactly 2 columns.
#'
#' @return A list object of class `inspect`.
#'
#' @importFrom data.table data.table
#' @export
#'
#' @examples
#' # automatically inspect first 2 columns:
#' inspect(sardine.rd)
#' inspect(urchins.rd)
#'
#' # inspect specific columns:
#' inspect(urchins.rd, time = 1, oxygen = 4)
#'
#' # inspect multiple columns
#' x <- inspect(urchins.rd, time = 1, oxygen = c(2:12))
#' print(x)
#' x$list$time.min  # check position of errors in data frame
#'
#' # inspect flowthrough data
#' x <- inspect(flowthrough.rd, 1, c(2,3))
#' x
inspect <- function(df, time = NULL, oxygen = NULL, plot = TRUE) {

  "%!in%" <- function(x, y) !("%in%"(x, y))

  # validate inputs
  ## set default values if NULL, which selects first column as time, and
  ## everything else as oxygen
  if (is.null(time) & is.null(oxygen)) {
    time <- 1
    listcols <- seq.int(1, ncol(df))
    oxygen <- listcols[!listcols %in% 1]
  }
  ## if only time is provided, assume check is for all data
  if (is.numeric(time) & is.null(oxygen)) {
    time <- time
    listcols <- seq.int(1, ncol(df))
    oxygen <- listcols[!listcols %in% time]
  }

  if (!is.data.frame(df)) stop("`df` must be data.frame object.")
  if (!is.numeric(time)) stop("`time` must be numeric integer.")
  if (!is.numeric(oxygen)) stop("`oxygen` must be numeric integer.")
  # more validations in a bit

  df <- as.data.frame(df)

  # extract data
  x <- lapply(1:length(df[time]), function(y) df[time][[y]])
  y <- lapply(1:length(df[oxygen]), function(y) df[oxygen][[y]])

  # validate data by type
  x_results <- check_timeseries(x, "time")
  y_results <- check_timeseries(y, "oxygen")

  # issue warnings
  if (any(unlist(x_results[[1]][1,])))
    warning("NA/NaN values detected in time columns.", call. = F)
  if (any(unlist(x_results[[1]][2,])))
    warning("Non-sequential time found.", call. = F)
  if (any(unlist(x_results[[1]][3,])))
    warning("Duplicate time values found.", call. = F)
  if (any(unlist(x_results[[1]][4,])))
    warning("Time values are not evenly-spaced.", call. = F)
  if (any(unlist(y_results[[1]][1,])))
    warning("NA/NaN values detected in oxygen columns.", call. = F)

  # combine results
  checks <- cbind(x_results[[1]], y_results[[1]])
  locs <- cbind(x_results[[2]], y_results[[2]])

  # output
  ## rename columns:
  colnames(checks) <- c(names(df[time]), names(df[oxygen]))
  list <- lapply(1:ncol(locs), function(z) locs[, z])
  names(list) <- c(names(df[time]), names(df[oxygen]))

  # if no errors occur, send out a good message :D
  if (!any(na.omit(unlist(checks)))) {
    message("No issues detected while inspecting data frame.")
  }

  # save new data frame and create output object
  dataframe <- data.table(cbind(df[time], df[oxygen]))
  out <-
    list(dataframe = dataframe, checks = checks, list_raw = locs, list = list)
  class(out) <- "inspect"

  if (plot) plot(out, label = FALSE)

  return(out)
}

#' @export
print.inspect <- function(x, ...) {
  cat("\n# inspect # -----------------------------\n")
  checks <- x$checks
  locs <- x$list_raw

  # rename content:
  tab <- checks
  tab[tab == "TRUE"] <- "WARN"
  tab[tab == "FALSE"] <- "pass"
  tab[is.na(tab)] <- "-"

  # print table
  print(as.data.frame(tab), quote = FALSE)
  # cat("\n")

  # highlight locations that did not pass the tests (but only for 2-col dfs):
  if ((length(x$dataframe)) == 2) {
    if (checks[, 1][[1]]) {
      xnan <- locs[, 1][[1]]
      cat("NA/NaN data locations")
      if (length(xnan) > 20) cat(" (first 20 shown) ")
      cat("in column:", names(x$dataframe[1]), "\n")
      print(head(xnan, 20))
    }
    if (checks[, 1][[2]]) {
      xseq <- locs[, 1][[2]]
      cat("Non-sequential time data locations")
      if (length(xseq) > 20) cat(" (first 20 shown) ")
      cat("in column:", names(x$dataframe[1]), "\n")
      print(head(xseq, 20))
    }
    if (checks[, 1][[3]]) {
      xdup <- locs[, 1][[3]]
      cat("Duplicate time data locations")
      if (length(xdup) > 20) cat(" (first 20 shown) ")
      cat("in column:", names(x$dataframe[1]), "\n")
      print(head(xdup, 20))
    }
    if (checks[, 1][[4]]) {
      xevn <- locs[, 1][[4]]
      cat("Uneven time data locations")
      if (length(xevn) > 20) cat(" (first 20 shown) ")
      cat("in column:", names(x$dataframe[1]), "\n")
      print(head(xevn, 20))
    }
    if (checks[, 2][[1]]) {
      ynan <- locs[, 2][[1]]
      cat("NA/NaN locations")
      if (length(ynan) > 20) cat(" (only first 20 shown) ")
      cat("in column:", names(x$dataframe[2]), "\n")
      print(head(ynan, 20))
    }
  }
  return(invisible(x))
}




#' @export
plot.inspect <- function(x, label = TRUE, ...) {
  if (label) cat("\n# plot.inspect # ------------------------\n")
  # extract data frame
  dt <- x$dataframe
  # perform rolling regression (quick one)
  if(ncol(dt) == 2) {
    ## changed here 0.2 to 0.1
    roll <- static_roll(dt, floor(0.1 * nrow(dt)))$rate_b1
  }

  if (length(x$dataframe) == 2) {

    pardefault <- par(no.readonly = T) # save original par settings
    par(
      mfrow = c(2, 1), mai = c(0.4, 0.4, 0.6, 0.3), ps = 10,
      cex = 1, cex.main = 1,
      mgp=c(0, 0.5, 0) # middle = to put tick labels closer to ticks
    )
    
    plot(
      dt[[1]], dt[[2]], xlab = "", ylab = "", pch = 16, cex =.5,
      col.lab = "blue", col.axis = "blue",
      panel.first = grid())
    axis(side = 2) # simply to put yaxis label colour back to black
    
    #title(xlab = "Time", line = 1)
    ## add row index axis
    par(new=TRUE)
    plot(
      seq(1, nrow(dt)), dt[[2]], xlab = "", ylab = "", pch = 16, cex =.5,
      axes = FALSE)
    axis(side=3, col.axis = "red")
    
    legend("bottomleft", "Time", text.col = "blue", bg = "gray90", 
           cex = 0.7) 
    legend("topright", "Row Index", text.col = "red", bg = "gray90", 
           cex = 0.7)
    
    title(main = "Full Timeseries", line = 2)
    
    plot(
      ## changed this to 10% width for now (0.05 each side here)
      ## Removed -1* before roll - now actual negative rates
      (roll) ~ dt[[1]][floor(0.05 * length(dt[[1]])):(floor(0.05 *
          length(dt[[1]])) + (length(roll) - 1))],
      xlim = range(dt[[1]]),
      ylim = rev(range(roll)), # reversed axis
      xlab = "", ylab = "", pch = 16, cex = .5,
      col.lab = "blue", col.axis = "blue")
    axis(side = 2) # simply to put yaxis label colour back to black

    ## Added dashed line at rate = 0
    abline(h = 0, lty = 2)
    
    par(new=TRUE)
    plot(
      seq(1, nrow(dt)), dt[[2]], xlab = "", ylab = "", pch = "", cex =.5,
      axes = FALSE, col = "white") # plot invisibly
    axis(side=3, col.axis = "red")
    
    legend("bottomleft", "Time", text.col = "blue", bg = "gray90", 
           cex = 0.7)
    legend("topright", "Row Index", text.col = "red", bg = "gray90", 
           cex = 0.7)
    
    title(
      ## UPDATED TITLE
      ## Updated again!!!!
      main = "Rolling Regression of Rate (0.1 Rolling Window)",
      line = 2
    )
    
    par(pardefault) # revert par settings to original
  } else {
    
    ## plot every column anyway - without rate plot
    message("inspect: Full plot is only avalilable for a 2-column dataframe output.")

    pardefault <- par(no.readonly = T)  # save original par settings
    par(mfrow = n2mfrow(length(x$dataframe)-1), mai = c(0.4, 0.4, 0.1, 0.1),
        ps = 10, cex = 1, cex.main = 1, pch =".")  # replace par settings
    
    lapply(1:(length(x$dataframe)-1), function(z) plot(data.frame(x$dataframe[[1]],
                                                                  x$dataframe[[z+1]]),
                                                       xlab = "", ylab = ""))
    
    par(pardefault)  # revert par settings to original 
    }   
  
  
  if (label) cat("Done.\n")
  return(invisible(x))
}
