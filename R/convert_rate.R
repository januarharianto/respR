#' Convert a unitless oxygen rate value to absolute, mass-specific or
#' area-specific rate
#'
#' Converts a unitless rate derived from [`calc_rate()`], [`auto_rate()`],
#' [`adjust_rate()`], or [`calc_rate.bg()`] into an absolute (i.e. whole chamber
#' or whole speciemn) rate, or mass-specific rate (i.e. normalised by specimen
#' mass), or area-specific rate (i.e. normalised by specimen surface area) in
#' any common unit.
#'
#' By default, `convert_rate` converts the primary `$rate` element from
#' `calc_rate` and `auto_rate` objects, the `$rate.adjusted` from `adjust_rate`
#' objects, and the `$rate.bg` from `calc_rate.bg` objects. Additionally, any
#' numeric value or vector of rates can be input as `x`.
#'
#' ## Respirometer volume
#'
#' The `volume` of the respirometer is required and should be in litres (`L`).
#' Note, the `volume` represents the *effective volume* of the respirometer,
#' that is *volume of water* in the respirometry chamber. This is not
#' necessarily the same as the volume of the respirometer. Typically, it is the
#' volume of the respirometer *minus* the volume of the specimen.
#' \href{https://github.com/nicholascarey/respfun#eff_vol}{See here} for help
#' with calculating effective volumes. It also does not refer to the specimen
#' volume.
#'
#' ## Units
#'
#' The `oxy.unit` of the original raw data used to calculate the rate is
#' required. Concentration units should use only SI units (`L` or `kg`) for the
#' denominator, e.g. `"mg/L"`, `"mmol/kg"`. Percentage saturation of air or
#' oxygen is accepted, as are oxygen pressure units. See [`unit_args()`] for
#' details.
#'
#' The `time.unit` of the original raw data used to calculate the rate is also
#' required.
#'
#' An `output.unit` is also required. If left `NULL`, The default of `"mgO2/h"`
#' is used, or `"mgO2/h/kg"` or `"mgO2/h/m2"` if a `mass` or `area` respectively
#' has been entered. The `output.unit` must be in the sequence *Oxygen-Time*
#' (e.g. `"mg/h"`) for absolute rates, *Oxygen-Time-Mass* (e.g. `"mg/h/kg"`) for
#' mass-specific rates, and *Oxygen-Time-Area* (e.g. `"mg/h/cm2"`) for surface
#' area-specific rates.
#'
#' Note, some oxygen input or output units require temperature (`t`) and
#' salinity (`S`) to perform conversions. For freshwater experiments, salinity
#' should be entered as zero (i.e. `S = 0`).
#'
#' Strictly speaking the atmospheric pressure (`P`) should also be supplied. If
#' not, the default value of 1.013253 bar (standard pressure at sea level) is
#' used. In most locations which have a normal range (outside extreme weather
#' events) of around 20 millibars, any variability in pressure will have a
#' relatively minor effect on dissolved oxygen, and even less on calculated
#' rates. However, we would encourage users to enter the actual value if they
#' know it, or use historical weather data to find out what it was on the day.
#' See [unit_args()] for details.
#'
#' The function uses an internal database and a fuzzy string matching algorithm
#' to accept various unit formatting styles. For example, `"mg/l"`, `"mg/L"`,
#' `"mgL-1"`, `"mg l-1"`, `"mg.l-1"` are all parsed the same. See
#' [`unit_args()`] for details of accepted units and their formatting. See also
#' [`convert_val()`] for simple conversion between non-oxygen units.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints a single result, by default the first converted rate.
#' Others can be printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all converted rates and metadata, or
#' those specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The summary
#' can be exported as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all converted rates, or those specified by
#' the `pos` input. e.g. `mean(x, pos = 1:5)` The mean can be exported as a
#' separate value by passing `export = TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR>
#'
#' @return Output is a `list` object containing the `$rate.input`, and converted
#'   rate(s) in `$rate.output` in the `$output.unit`, as well as inputs and
#'   summary elements.
#'
#' @param x numeric value or vector, or object of class `auto_rate`,
#'   `calc_rate`, `adjust_rate`, or `calc_rate.bg.` Contains the rate(s) to be
#'   converted.
#' @param oxy.unit string. The dissolved oxygen unit of the original raw data
#'   used to determine the rate in `x`.
#' @param time.unit string. The time unit of the original raw data used to
#'   determine the rate in `x`.
#' @param output.unit string. The output unit to convert the input rate to.
#'   Should be in the correct order: "Oxygen/Time" or "Oxygen/Time/Mass" or
#'   "Oxygen/Time/Area".
#' @param volume numeric. Volume of water in ***litres*** in the respirometer or
#'   respirometer loop.
#' @param mass numeric. Mass/weight in ***kg***. This is the mass of the
#'   specimen if you wish to calculate mass-specific rates.
#' @param area numeric. Surface area in ***m^2***. This is the surface area of
#'   the specimen if you wish to calculate surface area-specific rates.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Used in conversion of
#'   some oxygen units. Freshwater should be entered as `S = 0`.
#' @param t numeric. Temperature(°C). Defaults to NULL. Used in conversion of
#'   some oxygen units.
#' @param P numeric. Pressure (bar). Used in conversion of some oxygen units.
#'   Defaults to a standard value of 1.013253 bar.
#'
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' # Convert a single numeric rate to an absolute rate
#' convert_rate(0.09, oxy.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min', volume = 1.2)
#'
#' # Convert a single numeric rate to a mass-specific rate
#' convert_rate(0.09, oxy.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/kg', volume = 1.2, mass = 0.5)
#'
#' # Convert a single numeric rate to an area-specific rate
#' convert_rate(0.09, oxy.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/cm2', volume = 1.2, area = 0.0002)
#'
#' # Convert a single rate derived via calc_rate to mass-specific
#' x <- calc_rate(sardine.rd, from = 200, to = 1800, by = "time")
#' convert_rate(x, oxy.unit = '%Air', time.unit = 's',
#'   output.unit = 'mg/h/g', volume = 12.3, mass = 0.05,
#'   S =35, t = 15, P = 1.013)
#'
#' # Convert multiple rates derived via auto_rate to area-specific
#' x <- auto_rate(sardine.rd)
#' rates <- convert_rate(x, oxy.unit = '%Air', time.unit = 's',
#'   output.unit = 'mg/h/cm2', volume = 12.3, area = 0.00005,
#'   S =35, t = 15, P = 1.013)
#' summary(rates)

convert_rate <- function(x, oxy.unit = NULL, time.unit = NULL, output.unit = NULL,
                         volume = NULL, mass = NULL, area = NULL,
                         S = NULL, t = NULL, P = 1.013253) {

  ## Save function call for output
  call <- match.call()

  # Validate inputs If units are set to NULL, use default values.
  if (is.null(oxy.unit) || is.numeric(oxy.unit))
    stop("convert_rate: the 'oxy.unit' of the original data is required.")
  if (is.null(time.unit) || is.numeric(time.unit))
    stop("convert_rate: the 'time.unit' of the original data is required.")

  ## Apply output unit defaults
  if (is.null(output.unit) && is.null(mass) && is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h'",
            call. = F)
    output.unit <- "mg/h"
  }
  if (is.null(output.unit) && !is.null(mass) && is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h/kg'",
            call. = F)
    output.unit <- "mg/h/kg"
  }
  if (is.null(output.unit) && is.null(mass) && !is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h/m2'",
            call. = F)
    output.unit <- "mg/h/m2"
  }

  # Volume must not be NULL
  if (is.null(volume) || !is.numeric(volume))
    stop("convert_rate: the 'volume' input is required.")

  # Can't have both 'mass' and 'area' inputs
  if (!is.null(mass) && !is.null(area))
    stop("convert_rate: cannot have inputs for both 'mass' and 'area'.")

  # Validate rate value based on object class
  if (is.numeric(x)) {
    rate <- x
  } else if (class(x) %in% c("calc_rate")) {
    rate <- x$rate
    message("convert_rate: object of class `calc_rate` detected. Converting all rates in '$rate'.")
  } else if (class(x) %in% c("auto_rate")) {
    rate <- x$rate
    message("convert_rate: object of class `auto_rate` detected. Converting all rates in '$rate'.")
  } else if (class(x) %in% "adjust_rate") {
    rate <- x$rate.adjusted
    message("convert_rate: object of class `adjust_rate` detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (class(x) %in% "calc_rate.ft") {
    stop("convert_rate: object of class `calc_rate.ft` detected. \nPlease use 'convert_rate.ft' to convert the rate.")
  } else if (class(x) %in% "calc_rate.bg") {
    ## possible warning if mass entered - no reason to have mass with bg data
    rate <- x$rate.bg
    message("convert_rate: object of class `calc_rate.bg` detected. Converting all background rates in '$rate.bg'.")
  } else stop("convert_rate: 'x' input is not valid.")

  # Validate oxy.unit & time.unit
  oxy <- verify_units(oxy.unit, "o2")
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
  oxy.unit <- stringr::str_replace(oxy, "\\..*", "")
  time.unit <- stringr::str_replace(time, "\\..*", "")

  ## Add "O2" to output O2 unit string for clarity
  output.unit <- stringr::str_replace(ou, "\\..*", "")
  output.unit[1] <- paste0(output.unit[1], "O2")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("mmol.o2", "umol.o2", "mol.o2")) {
    RO2 <- convert_DO(rate, oxy, "mmol/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- convert_DO(rate, oxy, "mg/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mg.o2", A)
  } else if (A == "ml.o2") {
    RO2 <- convert_DO(rate, oxy, "mL/L", S, t, P)
    RO2 <- adjust_scale(RO2, "ml.o2", A)
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
    summary <- data.table(rank = 1:length(rate),
                          rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = mass,
                          area = NA,
                          rate.abs = VO2,
                          rate.m.spec = VO2.mass.spec,
                          rate.a.spec = NA,
                          output.unit = output.unit,
                          rate.output = VO2.mass.spec)

  } else if (is.area.spec) {
    summary <- data.table(rank = 1:length(rate),
                          rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = NA,
                          area = area,
                          rate.abs = VO2,
                          rate.m.spec = NA,
                          rate.a.spec = VO2.area.spec,
                          output.unit = output.unit,
                          rate.output = VO2.area.spec)
  } else {
    summary <- data.table(rank = 1:length(rate),
                          rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = NA,
                          area = NA,
                          rate.abs = VO2,
                          rate.m.spec = NA,
                          rate.a.spec = NA,
                          output.unit = output.unit,
                          rate.output = VO2)
  }


  # Assemble output ---------------------------------------------------------

  ## Save inputs
  inputs <- list(x = x, oxy.unit = oxy.unit, time.unit = time.unit, output.unit = output.unit,
                 volume = volume, mass = mass, area = area,
                 S = S, t = t, P = P)

  out <- list(call = call,
              inputs = inputs,
              summary = summary,
              rate.input = rate,
              output.unit = output.unit,
              rate.output = summary$rate.output)

  class(out) <- "convert_rate"
  return(out)
}

#' Print convert_rate objects
#' @param x convert_rate object
#' @param pos integer. Which result to print.
#' @param ... Pass additional inputs
#' @return Print to console. No returned value.
#' @export
print.convert_rate <- function(x, pos = 1, ...) {
  cat("\n# print.convert_rate # ------------------\n")

  if(length(pos) > 1)
    stop("print.convert_rate: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate.input))
    stop("print.convert_rate: Invalid 'pos' rank: only ", length(x$rate.input), " rates found.")

  cat("Rank", pos, "of", length(x$rate.output), "rates:\n")
  cat("\n")
  cat("Input:\n")
  print(x$rate.input[pos])
  print(c(x$inputs$oxy.unit, x$inputs$time.unit))
  cat("Converted:\n")
  print(x$rate.output[pos])
  print(x$output.unit)
  cat("\n")
  if(length(x$rate.input) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' Summarise convert_rate objects
#' @param object convert_rate object
#' @param pos integer(s). Which summary row(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @return Print to console. No returned value.
#' @export
summary.convert_rate <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate.input)))
    stop("summary.convert_rate: Invalid 'pos' rank: only ", length(object$rate.input), " rates found.")

  cat("\n# summary.convert_rate # ----------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all converted rates:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of converted rates from entered 'pos' rank(s):")
    cat("\n")
    cat("\n")
  }

  out <- data.table(object$summary[pos,])

  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' Average convert_rate object rates
#' @param x convert_rate object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @return Print to console. No returned value.
#' @export
mean.convert_rate <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.convert_rate # -------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate.output)))
    stop("mean.convert_rate: Invalid 'pos' rank: only ", length(x$rate.output), " rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate.output)
    cat("Mean of all rate results:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate.output[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate.output[pos])
  out <- mean(x$rate.output[pos])
  cat("Mean of", n, "output rates:\n")
  print(out)
  print(x$output.unit)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}

#' Plot convert_rate objects
#' @param x convert_rate object
#' @param ... Pass additional plotting parameters
#' @return A plot. No returned value.
#' @export
plot.convert_rate <- function(x, ...) {
  message("convert_rate: plot() is not available for 'convert_rate' objects.")
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
