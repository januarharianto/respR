#' Simple rate adjustment function (background or oxygen flux)
#'
#' A simple rate correction function. Please note that the **sign** of the
#' number must be considered in the correction. Background respiration is
#' normally a negative number, while oxygen flux may or may not be negative.
#'
#' To account for background respiration and/or net oxygen flux in open respirometry
#' experiments, we have provided a simple function to perform either, or both,
#' corrections. As a separate function we may develop this to support more
#' complex correction methods (e.g. non-linear) in the future.
#'
#' @md
#' @param x numeric. A single value, or any object of class `calc_rate` or `auto_rate`.
#'   This object contains the rate value that needs to be adjusted.
#' @param by either a numeric, or an object of class `calc_rate.bg`. This is the
#'   value that is used to perform the correction/adjustment.
#'
#' @return A list object of class `adjust_rate`.
#'
#' @export
#'
#' @examples
#' # Note that respiration is negative by default (since it represents a
#' # decrease in dissolved oxygen), so both values are negative!
#' # Background respiration correction
#' adjust_rate(-7.44, -0.04) # this is (-7.44) - (-0.04) = -7.40
#' # Oxygen flux correction
#' adjust_rate(-7.44, 0.1) # this is (-7.44) - (0.1) = -7.54

adjust_rate <- function(x, by, by2 = NULL, method = "mean", t_x = NULL, t_by = NULL, t_by2 = NULL) {

  # Validate inputs ---------------------------------------------------------

  ## validate x - rate(s) to be adjusted
  if(!(is.nv.cr.ar(x)))
    stop("adjust_rate: 'x' must be numeric or an object of class 'calc_rate' or 'auto_rate'")

  ## validate by and by2 - bg rate(s)
  if(!(is.nv.crbg(by)))
    stop("adjust_rate: 'by' must be numeric or object of class 'calc_rate.bg'")
  if(!(is.null(by2) | is.sn(by2) | is.crbg.2col(by2)))
    stop("adjust_rate: 'by2' must be a single numeric value or 'calc_rate.bg' object containing one column of oxygen data.")

  ## Validate and classify method
  dynamic <- val_meth(method)

  ## if "paired" x should be calc_rate/auto_rate and by should be calc.rate.bg
  if(method == "paired") {
    if(!(is.cr.ar(x)))
      stop("adjust_rate: For method = \"paired\" the 'x' input must be a calc_rate or auto_rate object.")
    if(!(is.crbg.2col(by)))
      stop("adjust_rate: For method = \"paired\" the 'by' input must be a calc_rate.bg object containing one column of oxygen data.")

    ## if lengths differ by more than 5% warn
    lx <- nrow(x$dataframe)
    lby <- nrow(by$dataframe)
    if(abs(diff(c(lx, lby)))/lx > 0.05)
      warning("adjust_rate: 'x' and 'by' inputs differ in length by more than 5%.
method = \"paired\" is intended for background experiments that have been run in parallel and so should be approximately the same length, and share the same 'time' data.
Adjustments have been applied anyway, using shared time values found in the 'x' and 'by' inputs.")
  }

  if(dynamic){
  ## if dynamic - t_x (if numeric/not null) can have multiple values, but must be same lengths as x or x$rate
    if(is.numeric(x) && is.numeric(t_x) && length(t_x) != length(x))
      stop("adjust_rate: 't_x' input should be the same length as the 'x' input.")
    if(!is.crbg.2col(by) && !is.sn(by))
      stop("adjust_rate: For \"linear\" and \"exponential\" methods the 'by' input must be a single numeric value or calc_rate.bg object containing one column of oxygen data.")
    if(!is.crbg.2col(by2) && !is.sn(by2))
      stop("adjust_rate: For \"linear\" and \"exponential\" methods the 'by2' input must be a single numeric value or calc_rate.bg object containing one column of oxygen data.")
  }

  ## if x is calc_rate/auto_rate t_x should be NULL (or ignored)
  ## converse - if x is numeric AND method =linear/exponental, t_x should have values, and be same length

  ## if method = "mean" by2 should be NULL

  ## if exponential - by and by2 can't be different signs?
  ## test this with linear

  ## t_x - should not HAVE to be within range of t_by and t_by2, but maybe warn if outside?


  # Extract background rate values ------------------------------------------

  ## Extract bg rate vector(s)
  if (class(by) %in% "calc_rate.bg") bg1 <-  by$bgrate else
    bg1 <- by

  ## these are only needed if methods are dynamic
  if(dynamic) {
    if (class(by2) %in% "calc_rate.bg") bg2 <-  by2$bgrate else
      bg2 <- by2

    ## bg1 and bg2 should now be single numeric values
    if(!is.numeric(bg1) || length(bg1) != 1)
      stop("adjust_rate: the 'by' input must a single numeric value or object of class calc_rate.bg")
    if(!is.numeric(bg2) || length(bg2) != 1)
      stop("adjust_rate: the 'by' input must a single numeric value or object of class calc_rate.bg")

    ## Extract bg rate timestamp(s)
    ## If calc_rate.bg objects, timestamp is midpoint of regression time values
    if(class(by) %in% "calc_rate.bg"){
      t_by <- respR:::midpt(by$lm[[1]]$model$`dt[[1]]`)
      message("adjust_rate: 't_by' extracted from 'by' object. Any 't_by' input ignored.")
    }

    if(class(by2) %in% "calc_rate.bg"){
      t_by2 <- respR:::midpt(by2$lm[[1]]$model$`dt[[1]]`)
      message("adjust_rate: 't_by2' extracted from 'by2' object. Any 't_by2' input ignored.")
    }

    ## t_by and t_by2 should now be single numeric values
    if(!is.numeric(t_by) || length(t_by) != 1)
      stop("adjust_rate: the 't_by' input must a single numeric value.")
    if(!is.numeric(t_by2) || length(t_by2) != 1)
      stop("adjust_rate: the 't_by2' input must a single numeric value.")

    ## t_by2 should be greater than t_by1
    if(t_by2 < t_by)
      stop("adjust_rate: Error in inputs. Timestamp in 'by2' less than in 'by' suggesting they are in the wrong order or come from different datasets.")
  }

  # Extract rate values -----------------------------------------------------

  # Extract rate input
  if (class(x) %in% c("calc_rate", "auto_rate")) {
    rate <- x$rate
  } else rate <- x

  # Extract rate timestamp(s)
  # (only used for dynamic correction)
  if (dynamic && class(x) %in% c("calc_rate", "auto_rate"))
    t_x <- (x$summary$time + x$summary$endtime)/2

  ## CHECK length(x) == length(t_x) - NO - CHECK DONE ABOVE IF NUMERIC


  # method = "mean" ---------------------------------------------------------

  if(method == "mean"){

    # Use mean value of bgrate for correction
    adjustment <- mean(bg1)
    out_model <- NULL

  }

  # method = "paired" -------------------------------------------------------

  if(method == "paired"){

    ## background experiment df
    bg_data <- as.data.frame(by$dataframe)

    ## locations of auto_rate/calc_rate regressions
    starts <- x$summary$time
    ends <- x$summary$endtime

    ## Use calc rate so we can use exact times, and will also fall back to closest matching behaviour if
    ## datasets don't have exactly the same time values.
    out_model <- mapply(function(p,q) calc_rate(bg_data, from = p, to = q, by = "time", plot = FALSE),
                        p = starts, q = ends, SIMPLIFY = FALSE)

    ## adjustment values
    adjustment <- unlist(lapply(out_model, function(q) q$rate))

    ## check all bg regression time values within range of those in x rates
    if(any(!(data.table::between(starts, min(bg_data[[1]]), max(bg_data[[1]])))) ||
       any(!(data.table::between(ends, min(bg_data[[1]]), max(bg_data[[1]])))))
      warning("adjust_rate: Some time values used in 'x' rate calculations not found in 'by' background data.
This probably stems from using datasets of different length for 'x' and 'by'.
Background adjustments have been applied using available data, but this may have unintended consequences.
Check results carefully.")

  }

  # method = "linear" -------------------------------------------------------

  if(method == "linear"){

    ## fit lm
    bg_lm <- lm(c(bg1, bg2) ~ c(t_by, t_by2))

    ## extract slope and intercept
    bg_lm_int <- coef(bg_lm)[1]
    bg_lm_slp <- coef(bg_lm)[2]

    ## apply to timestamp_rate to get adjustment
    adjustment <- unname(t_x * bg_lm_slp + bg_lm_int)
    out_model <- bg_lm

  }

  # method = "exponential" --------------------------------------------------

  if(method == "exponential"){

    ## if negative, can't fit an exponential
    ## so convert to positive
    if(by < 0 && by2 < 0) {
      both_neg <- TRUE
      by_e <- by * -1
      by2_e <- by2 * -1
    } else {
      both_neg <- FALSE
      by_e <- by
      by2_e <- by2
    }

    ## fit exponential lm
    bg_exp <- lm(log(c(by_e, by2_e)) ~ c(t_by, t_by2))

    # y = AB^x
    # A = 0.0000729564
    # B = 1.000006605

    # A*B^2500.0
    # A*B^77195.5

    exp(bg_exp_int)
    exp(bg_exp_slp)

    ## extract slope and intercept
    ## needs to convert back from log
    bg_exp_int <- exp(coef(bg_exp)[1])
    bg_exp_slp <- exp(coef(bg_exp)[2])

    ## apply to timestamp_rate to get adjustment
    adjustment <- bg_exp_int * bg_exp_slp ^ t_x

    ## if both bg rates negative, convert back to negative
    if(both_neg) adjustment <- adjustment * -1

  }


  # Apply -------------------------------------------------------------------

  # Perform correction
  adjusted.rate <- unname(unlist(rate - adjustment))

  # Output ------------------------------------------------------------------

  # Append the results to the object
  out <- c(input = list(x),
           input.rate = list(rate),
           list(
             adjustment.model = out_model,
             adjustment.type = method,
             adjusted.rate = adjusted.rate,
             adjustment = adjustment
           ))

  class(out) <- "adjust_rate"
  message("\nadjust_rate: Rate adjustments applied. Use print() on output for more info.")
  return(out)
}




#' @export
print.adjust_rate <- function(object, pos = 1, ...) {
  cat("\n# print.adjust_rate # -------------------\n")
  cat("Note: please consider the sign of the correction value when adjusting the rate.\n")
  if(pos > length(object$adjusted.rate)) stop("Invalid 'pos' rank: only ", length(object$adjusted.rate), " adjusted rates found.")
  cat("\nRank", pos, "of", length(object$adjusted.rate), "adjusted rate(s) shown. To see all results use summary().")
  cat("\nInput rate    :", object$input.rate[pos])
  cat("\nAdjustment    :", object$adjustment[pos])
  cat("\nAdjusted rate :", object$adjusted.rate[pos], "\n")
  cat("-----------------------------------------\n")

  return(invisible(object))
}

#' @export
#' @importFrom data.table data.table
summary.adjust_rate <- function(object, export = FALSE, ...) {
  cat("\n# summary.adjust_rate # -----------------\n")
  if (length(object) == 3) {
    rate <- object$input
  } else rate <- object$input.rate
  out <- data.table::data.table(rate, adjustment = object$adjustment, "adjusted rate" = object$adjusted.rate)
  print(out)

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.adjust_rate <- function(object, export = FALSE, ...){

  cat("\n# mean.adjust_rate # --------------------\n")
  if(length(object$adjusted.rate) == 1) warning("Only 1 rate found in adjust_rate object. Returning mean rate anyway...")
  n <- length(object$adjusted.rate)
  out <- mean(object$adjusted.rate)
  cat("Mean of", n, "adjusted rates:\n")
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}



# Get midpoint between two values
# For getting timestamps from Time data
midpt <- function(p) {
  p1 <- min(p)
  p2 <- max(p)
  return((p1 + p2) / 2)
}

## validate that an input is a single numeric value
is.sn <- function(x){
  if(is.numeric(x) && length(x) == 1) return(TRUE) else
    return(FALSE)
  }

## validate that an input is a single numeric value OR calc_rate/auto_rate object
is.sn.cr.ar <- function(x){
  if(((is.numeric(x) && length(x) == 1)) ||  class(x) %in% c("calc_rate", "auto_rate")) return(TRUE) else
    return(FALSE)
  }

## validate that an input is numeric vector OR calc_rate/auto_rate object
is.nv.cr.ar <- function(x){
  if((is.numeric(x)) ||  class(x) %in% c("calc_rate", "auto_rate")) return(TRUE) else
    return(FALSE)
  }

## validate that an input is numeric vector OR calc_rate.bg object
is.nv.crbg <- function(x){
  if((is.numeric(x)) ||  class(x) %in% c("calc_rate.bg")) return(TRUE) else
    return(FALSE)
  }

## validate that an input is a single numeric value OR calc_rate.bg object
is.sn.crbg <- function(x){
  if(((is.numeric(x) && length(x) == 1)) ||  class(x) %in% c("calc_rate.bg")) return(TRUE) else
    return(FALSE)
  }

## validate that an input is a calc_rate/auto_rate object
is.cr.ar <- function(x){
  if(class(x) %in% c("calc_rate", "auto_rate")) return(TRUE) else
    return(FALSE)
  }

## validate that an input is a calc_rate.bg object with 2 column dataframe
is.crbg.2col <- function(x){
  if(class(x) %in% c("calc_rate.bg") && length(x$dataframe) == 2) return(TRUE) else
    return(FALSE)
  }
## validate that an input is a calc_rate.bg object with multiple column dataframe
is.crbg.multi.col <- function(x){
  if(class(x) %in% c("calc_rate.bg") && length(x$dataframe) > 2) return(TRUE) else
    return(FALSE)
  }

## validate method
val_meth <- function(method){
  if(!(method %in% c("mean", "paired", "linear", "exponential")))
    stop("adjust_rate: 'method' input not recognised")
  ## create logical for linear/exp methods
  if(method == "linear" | method == "exponential") dynamic <- TRUE else
    dynamic <- FALSE
  return(dynamic)
}


