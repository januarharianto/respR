#' Convert a unitless oxygen rate value to absolute, mass-specific or
#' area-specific rate
#'
#' `convert_rate` converts a unitless rate derived from [`calc_rate()`],
#' [`calc_rate.ft()`], [`calc_rate.bg()`], [`auto_rate()`], or [`adjust_rate()`]
#' into an absolute volume-adjusted (i.e. to the container) rate, or
#' mass-specific rate (i.e. normalised by specimen mass), or area-specific rate
#' (i.e. normalised by specimen surface area) in any common unit.
#'
#' By default, `convert_rate` converts the primary `$rate` element from
#' `calc_rate` and `auto_rate` objects, the `$adjusted.rate` from `adjust_rate`
#' objects, and the `$mean` rate from `calc_rate.ft` and `calc_rate.bg` objects.
#' Different rates within these objects can be converted by calling them
#' specifically as the `x` input using `$`. Additionally, any numeric value or
#' vector can be input as `x`.
#'
#' NOTE: for converting rates from flowthrough experiments, the `volume` and
#' `time.unit` inputs should be set with reference to the `flowrate` units **in
#' L per unit time** used in `calc_rate.ft` to determine the rates. For example.
#' if the flow rate was entered via the `flowrate` operator in `L/s` enter
#' `volume = 1, time.unit = "s"`. With flowthrough rates `volume` does *NOT*
#' represent the volume of the respirometer, and `time.unit` does *NOT*
#' represent the resolution of the original data.
#'
#' The function uses an internal database and a fuzzy string matching algorithm
#' to accept various unit formatting styles. For example, `'mg/l', 'mg/L',
#' 'mgL-1', 'mg l-1', 'mg.l-1'` are all parsed the same. Use [unit_args()] to
#' view a list of usable unit strings.
#'
#' Output units (`output.unit`) must be in the sequence *O2-Time* (e.g.
#' `"mg/h"`) for absolute rates, *O2-Time-Mass* (e.g. `"mg/h/kg"`) for
#' mass-specific rates, and *O2-Time-Area* (e.g. `"mg/h/cm2"`) for surface
#' area-specific rates.
#'
#' Some oxygen units require temperature (`t`), salinity (`S`), and atmospheric
#' pressure (`P`) to be specified. See [unit_args()] for details. For freshwater
#' experiments, salinity should be entered as zero (i.e. `S = 0`).
#'
#' @param x numeric value or vector, or object of class [auto_rate()],
#'   [calc_rate()], [calc_rate.ft()], [calc_rate.bg()] or [adjust_rate()]. The
#'   rate of change in oxygen.
#' @param o2.unit string. The dissolved oxygen unit of the original data used to
#'   determine the rate in 'x'. See [unit_args()].
#' @param time.unit string. The time unit of the original data used to determine
#'   the rate in 'x'. See [unit_args()]. When converting rates from *flowthrough
#'   experiments* this should be the time unit of the *flow rate*. See Details.
#' @param output.unit string. The output unit to which to convert the input rate
#'   in 'x'. Should be in the correct order: "O2/Time" or "O2/Time/Mass" or
#'   "O2/Time/Area". See [unit_args()].
#' @param volume numeric. Volume of water in litres. This is the *effective
#'   volume* of the respirometer, that is volume of fluid in the respirometry
#'   chamber, not the total respirometer volume or specimen volume.
#'   \href{https://github.com/nicholascarey/respfun#eff_vol}{See here} for
#'   calculating effective volumes. Converting rates from *flowthrough
#'   experiments* is a special case: see Details.
#' @param mass numeric. Mass/weight in kg. This is the mass of the specimen if
#'   you wish to calculate mass-specific rates.
#' @param area numeric. Surface area in m^2. This is the surface area of the
#'   specimen if you wish to calculate surface area-specific rates.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Used in conversion of
#'   some oxygen units. See [unit_args()] for details. Fresh water should be
#'   entered as `S = 0`.
#' @param t numeric. Temperature(°C). Defaults to NULL. Used in conversion of
#'   some oxygen units. See [unit_args()] for details.
#' @param P numeric. Pressure (bar). Defaults to 1.013253. Used in conversion of
#'   some oxygen units. If NULL, a standard value of 1.013253 is applied in
#'   conversions. See [unit_args()] for details.
#'
#' @return Returns a `list` object containing the `$input.rate`, and
#'   `$output.rate` (converted) rate in the `$output.unit`, as well as inputs
#'   and summary elements.
#'
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' # Convert a single numeric rate to an absolute rate
#' convert_rate(0.09, o2.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min', volume = 1.2)
#'
#' # Convert a single numeric rate to a mass-specific rate
#' convert_rate(0.09, o2.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/kg', volume = 1.2, mass = 0.5)
#'
#' # Convert a single numeric rate to an area-specific rate
#' convert_rate(0.09, o2.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/cm2', volume = 1.2, area = 0.0002)
#'
#' # Convert a single rate derived via calc_rate to mass-specific
#' x <- calc_rate(sardine.rd, from = 200, to = 1800, by = "time")
#' convert_rate(x, o2.unit = '%Air', time.unit = 's',
#'   output.unit = 'mg/h/g', volume = 12.3, mass = 0.05,
#'   S =35, t = 15, P = 1.013)
#'
#' # Convert multiple rates derived via auto_rate to area-specific
#' x <- auto_rate(sardine.rd)
#' rates <- convert_rate(x, o2.unit = '%Air', time.unit = 's',
#'   output.unit = 'mg/h/cm2', volume = 12.3, area = 0.00005,
#'   S =35, t = 15, P = 1.013)
#' summary(rates)

convert_rate <- function(x, o2.unit = NULL, time.unit = NULL, output.unit = NULL,
                         volume = NULL, mass = NULL, area = NULL,
                         S = NULL, t = NULL, P = 1.013253)
  {

  # Validate inputs If units are set to NULL, use default values.
  if (is.null(o2.unit) || is.numeric(o2.unit)) {
    stop("convert_rate: the 'o2.unit' of the original data is required.")
  }
  if (is.null(time.unit) || is.numeric(time.unit)) {
    stop("convert_rate: the 'time.unit' of the original data is required.")
  }

  ## Apply output unit defaults
  if (is.null(output.unit) && is.null(mass) && is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided, using 'mgO2/h'.",
      call. = F)
    output.unit <- "mg/h"
  }
  if (is.null(output.unit) && !is.null(mass) && is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided, using 'mgO2/h/kg'.",
      call. = F)
    output.unit <- "mg/h/kg"
  }
  if (is.null(output.unit) && is.null(mass) && !is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided, using 'mgO2/h/m2'.",
      call. = F)
    output.unit <- "mg/h/m2"
  }

  # Volume must not be NULL
  if (is.null(volume) || !is.numeric(volume))
    stop("convert_rate: the 'volume' input is required.")

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
    message("convert_rate: object of class `calc_rate.ft` detected. Automatically using mean rate value.")
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
    message("convert_rate: object of class `calc_rate.bg` detected. Automatically using mean value.")
  } else stop("convert_rate: 'x' input is not valid.")

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
    if(!is.null(mass) && is.null(area)){
      is.mass.spec <- TRUE
      is.area.spec <- FALSE
    } else if(!is.null(area) && is.null(mass)){
      is.mass.spec <- FALSE
      is.area.spec <- TRUE
    } else if(is.null(area) && is.null(mass)){
      stop("convert_rate: 'output.unit' requires a value for 'mass' or 'area'")
    }
  } else {
    is.mass.spec <- FALSE
    is.area.spec <- FALSE
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
  if (!is.mass.spec && is.numeric(mass))
    stop("convert_rate: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")

  # Verify 'area' input
  if (!is.area.spec && is.numeric(area))
    stop("convert_rate: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")

  # Format unit strings to look nicer
  o2.unit <- stringr::str_replace(oxy, "\\..*", "")
  time.unit <- stringr::str_replace(time, "\\..*", "")

  ## Add "O2" to output O2 unit string for clarity
  output.unit <- stringr::str_replace(ou, "\\..*", "")
  output.unit[1] <- paste0(output.unit[1], "O2")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("mmol.o2", "umol.o2", "mol.o2")) {
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
    multm <- adjust_scale_area(area, "m2.area", C)
    VO2.area.spec <- VO2/multm # ok
  }

  # Generate output
  if (is.mass.spec) {
    summary <- data.frame(input.rate = rate, output.rate = VO2.mass.spec,
                          absolute.rate = VO2, mass.specific.rate = VO2.mass.spec)
  } else if (is.area.spec) {
    summary <- data.frame(input.rate = rate, output.rate = VO2.area.spec,
                          absolute.rate = VO2, area.specific.rate = VO2.area.spec)
  } else {
    summary <- data.frame(input.rate = rate, output.rate = VO2,
                          absolute.rate = VO2)
  }

  out <- list(input.rate = rate,
              output.rate = summary$output.rate,
              summary = summary,
              input.o2.unit = o2.unit,
              input.time.unit = time.unit,
              output.unit = output.unit,
              input.volume = volume,
              input.mass = mass,
              input.area = area)

  class(out) <- "convert_rate"
  return(out)
}

#' @export
print.convert_rate <- function(x, pos = NULL, ...) {
  cat("\n# print.convert_rate # ------------------\n")

  if (is.null(pos)) {
    cat("Rank/position 1 of", length(x$output.rate), "result(s) shown. To see all results use summary().\n")
    cat("Input:\n")
    print(x$input.rate[1])
  } else if(pos > length(x$output.rate)) {
    stop("Invalid 'pos' rank: only ",
         length(x$output.rate), " rates found.")
  } else if (is.numeric(pos)) {
    cat("Position", pos, "result\n")
    cat("Input:\n")
    print(x$input.rate[pos])
  } else {
    cat("Input:\n")
    print(x$input.rate)
  }
  print(c(x$input.o2.unit, x$input.time.unit))
  cat("Converted:\n")
  if (is.null(pos)) {
    print(x$output.rate[1])
  } else if (is.numeric(pos)) {
    print(x$output.rate[pos])
  } else {
    print(x$output.rate)
  }
  print(x$output.unit)
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' @export
summary.convert_rate <- function(object, export = FALSE, ...) {
  cat("\n# summary.convert_rate # ----------------\n")

  out <- data.table(object$summary,
                    input.o2.unit = object$input.o2.unit,
                    input.time.unit = object$input.time.unit,
                    output.unit = object$output.unit,
                    input.volume = object$input.volume,
                    input.mass = object$input.mass,
                    input.area = object$input.area)

  print(out)

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.convert_rate <- function(x, export = FALSE, ...){

  cat("\n# mean.convert_rate # -------------------\n")
  if(length(x$output.rate) == 1) warning("Only 1 rate found in convert_rate object. Returning mean rate regardless...")
  n <- length(x$output.rate)
  out <- mean(x$output.rate)
  cat("Mean of", n, "output rates:\n")
  print(out)
  print(x$output.unit)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
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
  prefix <- c("n", "u", "m", "", "k", "sec", "min", "hour", "day")
  suffix <- c("mol", "g", "L", "l", "")
  multip <- c(1e-19, 1e-06, 0.001, 1, 1000, 3600, 60, 1, 1/24)
  string <- "^(n|u|m||k|sec|min|hour|day)?(mol|g|L|l|)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")  # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef)))  # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")  # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft)))  # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("adjust_scale: Units do not match and cannot be converted.", call. = F)
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
  suffix <- c("m2")
  multip <- c(1e-06, 0.0001, 1, 1e+06)
  string <- "^(m|c||k)?(m2)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")  # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef)))  # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")  # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft)))  # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("adjust_scale_area: Units do not match and cannot be converted.", call. = F)
  # Convert!
  a <- multip[match(bef[2], prefix)]  # get multiplier from input
  b <- multip[match(aft[2], prefix)]  # get multiplier from output
  out <- x * (a/b)  # convert
  return(out)
}

# x = area
# input = "m.sq"
# output = "cm.sq"
