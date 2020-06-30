#' Convert a unitless rate value to absolute, mass-specific or area-specific
#' rate
#'
#' This is a conversion function. It can convert a unitless rate derived from
#' `calc_rate`, `calc_rate.ft`, `calc_rate.bg`, `auto_rate`, or `adjust_rate`
#' into an absolute volume-adjusted (i.e. to the container) rate, mass-specific
#' rate (i.e. normalised by specimen mass), or area-specific rate (i.e.
#' normailsed by specimen surface area).
#'
#' Unless other values are specifically called as `x`, the function converts the
#' primary `$rate` from `calc_rate` and `auto_rate` objects, the
#' `$adjusted.rate` rate from `adjust_rate` objects, and the `$mean` rate from
#' `calc_rate.ft` and `calc_rate.bg` objects. Values or vectors of other rates
#' within these obects can be converted by calling them as `x` using `$`.
#'
#' Note, for rates from flowthrough experiments, the `volume` and `time` inputs
#' should be set with reference to the *flow rate* in L per unit time. E.g. for
#' a flow rate in L/s `volume = 1, time = "s"`. With these rates `volume` does
#' *NOT* represent the volume of the respirometer, and `time` does *NOT*
#' represent the resolution of the original data."
#'
#' The function uses an internal database and a fuzzy string matching algorithm
#' to accept various unit formatting styles.
#'
#' For example, `'mg/l', 'mg/L', 'mgL-1', 'mg l-1', 'mg.l-1'` are all the same.
#' Use [unit_args()] to view a list of usable unit strings.
#'
#' Output units (`output.unit`) must be in the sequence O2-Time (e.g. mg/h,
#' mg-h) for absolute rates, and for mass-specific rates O2-Time-Mass (e.g.
#' mg/h/kg).
#'
#' Some units also require temperature (`t`), salinity (`S`), and atmospheric
#' pressure (`P`) to be specified. See [unit_args()] for details. For freshwater
#' experiments, salinity should be set to zero (i.e. `S = 0`).
#'
#' @param x numeric, or objects of class [calc_rate()], [calc_rate.ft()],
#'   [auto_rate()] or [adjust_rate()].
#' @param o2.unit string. The dissolved oxygen unit of the data used to
#'   determine rate. Check [unit_args()].
#' @param time.unit string. The time unit of the data used to determine rate.
#'   Check [unit_args()].
#' @param output.unit string. The output unit to convert rate to. Check
#'   [unit_args()].
#' @param volume numeric. Volume in litres. This is the volume of fluid in the
#'   respirometry chamber, not the specimen volume.
#' @param mass numeric. Mass/weight in kg. This is the mass of the specimen if
#'   you wish to calculate mass-specific rates.
#' @param area numeric. Surface area in m^2. This is the surface area of the
#'   specimen if you wish to calculate surface area-specific rates.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Used only in conversion
#'   of some units. See [unit_args()] for details.
#' @param t numeric. Temperature(°C). Defaults to NULL. Used only in conversion
#'   of some units. See [unit_args()] for details.
#' @param P numeric. Pressure (bar). Defaults to NULL. Used only in conversion
#'   of some units. If left NULL, default value of 1.013253 is applied in
#'   conversions. See [unit_args()] for details.
#'
#' @return A list object.
#'
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' # Manually enter values
#' convert_rate(7.5, o2.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/kg', volume = 1.2, mass = 0.5)
#'
#' # Use example data
#' x <- calc_rate(sardine.rd, from = 200, to = 1800, by = "time")
#' convert_rate(x, o2.unit = '%', time.unit = 's',
#'   output.unit = 'mg/h/g', volume = 12.3, mass = 0.05,
#'   S =35, t = 15, P = 1.013)
convert_rate <- function(x,
                         o2.unit = NULL,
                         time.unit = NULL,
                         output.unit = NULL,
                         volume = NULL,
                         mass = NULL,
                         area = NULL,
                         S = NULL,
                         t = NULL,
                         P = NULL)
  {

  # Validate inputs If units are set to NULL, use default values.
  if (is.null(o2.unit)) {
    warning("convert_rate: the 'o2.unit' is not provided, using 'mg/L'.", call. = F)
    o2.unit <- "mg/L"
  }
  if (is.null(time.unit)) {
    warning("convert_rate: the 'time.unit' is not provided, using 's'.", call. = F)
    time.unit <- "s"
  }
  if (is.null(output.unit)) {
    warning("convert_rate: the 'output.unit' is not provided, using 'mg/h'.",
      call. = F)
    output.unit <- "mg/h"
  }

  # Volume must not be NULL
  if (is.null(volume))
    stop("convert_rate: Input argument for 'volume' is required.")

  # Can't have both 'mass' and 'area' inputs
  if (!is.null(mass) && !is.null(area))
    stop("convert_rate: Cannot have inputs for both 'mass' and 'area'.")

  # Validate rate value based on object class
  if (is.numeric(x)) {
    rate <- x
  } else if (class(x) %in% c("calc_rate", "auto_rate")) {
    rate <- x$rate
  } else if (class(x) %in% "adjust_rate") {
    rate <- x$adjusted.rate
  } else if (class(x) %in% "calc_rate.ft") {
    rate <- x$mean
    message("object of class `calc_rate.ft` detected. Automatically using mean value.")
    ## possibly here we automatically fill volume = 1
    ## Or at least warn if volume != 1
    warning("NOTE: In flowthrough experiments `volume` and `time` inputs should be set
      with reference to the flow rate in L per unit time.
      E.g. for a flow rate in L/s `volume = 1, time = \"s\"`.
      `volume` does NOT represent the volume of the respirometer.
      `time` does NOT represent the resolution of the original data.")
  } else if (class(x) %in% "calc_rate.bg") {
    ## possible warning if mass entered - no reason to have mass with bg data
    rate <- x$mean
    message("object of class `calc_rate.bg` detected. Automatically using mean value.")
  } else stop("`x` input is not valid.")

  # Validate o2.unit & time.unit
  oxy <- verify_units(o2.unit, "o2")
  time <- verify_units(time.unit, "time")

  # Validate output.unit
  ou <- as.matrix(read.table(text = gsub("(?:-1|[/.[:space:]])+",
    " ", output.unit), header = FALSE))

  ## is it a specific rate (mass or area)?
  is.spec <- length(ou) == 3

  ## Is output unit mass or area specific rate?
  if(is.spec){
    if(verify_units(ou[3], "mass") %in% c("ug.mass", "mg.mass", "g.mass", "kg.mass")){
      is.mass.spec <- TRUE
      is.area.spec <- FALSE
    } else if(verify_units(ou[3], "area") %in% c("mm.sq", "cm.sq", "m.sq", "km.sq")){
      is.mass.spec <- FALSE
      is.area.spec <- TRUE
    }
  } else {
    is.area.spec <- FALSE
    is.mass.spec <- FALSE
  }

  A <- verify_units(ou[1], "o1")
  B <- verify_units(ou[2], "time")
  if(is.spec){
    if (is.mass.spec) {
      C <- verify_units(ou[3], "mass")
      ou <- as.matrix(data.frame(A, B, C))
    } else if (is.area.spec) {
      C <- verify_units(ou[3], "area")
      ou <- as.matrix(data.frame(A, B, C))
    }
  } else ou <- as.matrix(data.frame(A, B))

  # Verify 'mass' input
  if (is.mass.spec && is.null(mass))
    stop("convert_rate: 'output.unit' requires a value for 'mass'.")
  if (!is.mass.spec && is.numeric(mass))
    stop("convert_rate: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")

  # Verify 'area' input
  if (is.area.spec && is.null(area))
    stop("convert_rate: 'output.unit' requires a value for 'area'.")
  if (!is.area.spec && is.numeric(area))
    stop("convert_rate: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")

  # Format unit strings to look nicer
  o2.unit <- stringr::str_replace(oxy, "\\..*", "")
  time.unit <- stringr::str_replace(time, "\\..*", "")
  output.unit <- stringr::str_replace(ou, "\\..*", "")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("mmol.o2", "umol.o2")) {
    RO2 <- convert_DO(rate, oxy, "mmol/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- convert_DO(rate, oxy, "mg/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "mg.o2", A)
  } else if (A == "ml.o2") {
    RO2 <- convert_DO(rate, oxy, "mL/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "ml.o2", A)
  }

  # Then, convert time unit
  RO2 <- adjust_scale(RO2, time, B)

  # Then, scale to volume
  VO2 <- RO2 * volume

  # Then, scale to mass or area
  if (is.mass.spec) {
    # adjust mass multiplier
    multm <- adjust_scale(mass, "kg.mass", C)
    VO2.mass.spec <- VO2/multm # ok
  }
  if (is.area.spec) {
    # adjust area multiplier
    multm <- adjust_scale_area(area, "m.2", C)
    VO2.area.spec <- VO2/multm # ok
  }

  # Generate output
  summary <- data.frame(input.rate = rate, converted.rate = RO2,
    absolute.rate = VO2)
  if (is.mass.spec) {
    summary <- data.frame(summary, mass.specific.rate = VO2.mass.spec)
    converted.rate <- VO2.mass.spec
  } else if (is.area.spec) {
    summary <- data.frame(summary, area.specific.rate = VO2.area.spec)
    converted.rate <- VO2.area.spec
  } else converted.rate <- VO2

  out <- list(input.rate = rate,
              output.rate = converted.rate,
              summary = summary,
              input.o2.unit = o2.unit,
              input.time.unit = time.unit,
              output.unit = output.unit)

  class(out) <- "convert_rate"
  return(out)
}

#' @export
print.convert_rate <- function(object, pos = NULL, ...) {
  cat("\n# print.convert_rate # ------------------\n")

  if (is.null(pos)) {
    cat("Rank/position 1 of", length(object$output.rate), "result(s) shown. To see all results use summary().\n")
    cat("Input:\n")
    print(object$input.rate[1])
  } else if(pos > length(object$output.rate)) {
    stop("Invalid 'pos' rank: only ",
         length(object$output.rate), " rates found.")
  } else if (is.numeric(pos)) {
    cat("Position", pos, "result\n")
    cat("Input:\n")
    print(object$input.rate[pos])
  } else {
    cat("Input:\n")
    print(object$input.rate)
  }
  print(c(object$input.o2.unit, object$input.time.unit))
  cat("Converted:\n")
  if (is.null(pos)) {
    print(object$output.rate[1])
  } else if (is.numeric(pos)) {
    print(object$output.rate[pos])
  } else {
    print(object$output.rate)
  }
  print(object$output.unit)
  cat("-----------------------------------------\n")
  return(invisible(object))
}

#' @export
summary.convert_rate <- function(object, export = FALSE, ...) {
  cat("\n# summary.convert_rate # ----------------\n")

  out <- data.table(object$summary,
                    input.o2.unit = object$input.o2.unit,
                    input.time.unit = object$input.time.unit,
                    output.unit = object$output.unit)

  print(out)

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.convert_rate <- function(object, export = FALSE, ...){

  cat("\n# mean.convert_rate # -------------------\n")
  if(length(object$output.rate) == 1) warning("Only 1 rate found in convert_rate object. Returning mean rate regardless...")
  n <- length(object$output.rate)
  out <- mean(object$output.rate)
  cat("Mean of", n, "output rates:\n")
  print(out)
  print(object$output.unit)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}


#' Convert between multipliers of the same unit, e.g. mg to kg
#'
#' This is an internal function. Converts units of the same scale, e.g. mg to
#' kg, or mL to L.
#'
#' @param x numeric.
#' @param input string.
#' @param output string.
#'
#' @keywords internal
#'
#' @return A numeric.
#'
#' @importFrom stringr str_replace
#' @export
adjust_scale <- function(x, input, output) {
  # Create database of terms for matching
  prefix <- c("n", "u", "m", "", "k", "sec", "min", "hour")
  suffix <- c("mol", "g", "L", "l", "")
  multip <- c(1e-19, 1e-06, 0.001, 1, 1000, 3600, 60, 1)
  string <- "^(n|u|m||k|sec|min|hour)?(mol|g|L|l|)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")  # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef)))  # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")  # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft)))  # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("Units do not match and cannot be converted.", call. = F)
  # Convert!
  a <- multip[match(bef[2], prefix)]  # get multiplier from input
  b <- multip[match(aft[2], prefix)]  # get multiplier from output
  out <- x * (a/b)  # convert
  return(out)
}

#' Convert between multipliers of the same AREA unit, e.g. mm2 to km2
#'
#' This is an internal function. Converts units of area. Could be combined with
#' adjust_scale, but didn't know how....
#'
#' @param x numeric.
#' @param input string.
#' @param output string.
#'
#' @keywords internal
#'
#' @return A numeric.
#'
#' @importFrom stringr str_replace
#' @export
adjust_scale_area <- function(x, input, output) {
  # Create database of terms for matching
  prefix <- c("m", "c", "", "k")
  suffix <- c("m.2")
  multip <- c(1e+6, 10000, 1, 1e-6)
  string <- "^(m|c||k)?(m.2)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")  # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef)))  # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")  # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft)))  # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("Units do not match and cannot be converted.", call. = F)
  # Convert!
  a <- multip[match(bef[2], prefix)]  # get multiplier from input
  b <- multip[match(aft[2], prefix)]  # get multiplier from output
  out <- x * (a/b)  # convert
  return(out)
}

x = 1
input = "m^2"
output = "mm^2"
