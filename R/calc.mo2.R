#' Calculate volume and/or mass-specific rate of oxygen flux
#'
#' @param x Number, or an object of class \code{'calc.rate'}
#' @param unit.in Character.
#' @param unit.out Character.
#' @param volume Numeric. Volume of water in L.
#' @param mass Numeric. Mass/weight of specimen in kg.
#' @param S Numeric. Salinity in PSU. Defaults to 35.
#' @param t Numeric. temperature in degrees C. Defaults to 25.
#' @param P Numeric. Pressure. Defaults to 1.013253.
#'
#' @return A conversion from raw rate to volume and/or mass-specific rate.
#' @importFrom stringr str_extract
#' @export
#'
#' @examples
#' TBC
calc.mo2 <- function(x, unit.in, unit.out, volume = NULL, mass = NULL, rank = 1,
  S = 35, t = 25, P = 1.013253) {
  # check that the user has the right inputs
  if (is.null(volume))
    stop("Input argument for 'volume' must not be empty.", call. = F)
  # check if input is class 'calc.rate' - if it is, we extract the results
  if (class(x) == "calc.rate") {
    b1 <- x$results$b1
  } else if (class(x) == "auto.rate") {
    b1 <- x$output[[rank]]$results$b1
  } else b1 <- x  # otherwise, use the input given

  # let's convert the DO/time to MO2 first we identify the units based on whether
  # they are input or output:
  u.in <- id.unit(unit.in, "input")  # should identify 2 units
  u.out <- id.unit(unit.out, "output")  # should identify 2-3 units
  # for output:
  o2.unit <- paste(u.in, collapse = "/")
  mo2.unit <- paste(u.out, collapse = "/")

  # check if 'u.out' requires 'mass' argument, or not
  if (is.null(mass) && length(u.out) == 3)
    stop("Please include 'mass' argument.", call. = F)
  if (is.numeric(mass) && length(u.out) == 2)
    stop("Mass input detected, but 'to' argument is missing a unit.", call. = F)

  # now check that the unit types match:
  s <- "[.][:alnum:]*"  # this is the string to match
  o2.match <- all.equal(str_extract(u.in[1], s), str_extract(u.out[1], s))
  time.match <- all.equal(str_extract(u.in[2], s), str_extract(u.out[2], s))
  if (o2.match == FALSE)
    stop("O2 units don't match.")
  if (time.match == FALSE)
    stop("Time units don't match.")

  # convert to 'standard' units first based on input
  if (u.out[1] == "mmol.o2" | u.out[1] == "umol.o2") {
    result <- convert.do(b1, u.in[1], "mmol/L", S, t, P) * volume
    result <- scale.unit(result, "mmol.o2", u.out[1])
  } else if (u.out[1] == "mg.o2" | u.out[1] == "ug.o2") {
    result <- convert.do(b1, u.in[1], "mg/L", S, t, P) * volume
    result <- scale.unit(result, "mg.o2", u.out[1])
  } else if (u.out[1] == "ml.o2") {
    result <- convert.do(b1, u.in[1], "mL/L", S, t, P) * volume
    result <- scale.unit(result, "ml.o2", u.out[1])
  } else stop("Units of pressure are currently not supported.", call. = F)

  # scale time unit (e.g. sec to min)
  result <- scale.unit(result, u.in[2], u.out[2])
  # scale mass unit, if provided (note, mass MUST be in kg)
  if (!is.null(mass))
    result <- scale.unit(result/mass, "kg", u.out[3])

  # check if multiple values are calculated - if so, also calculate average
  if (length(result) > 1) {
    mean <- mean(result)
    w.mean <- weighted.mean(result, x$subsets$row.width)
  } else {mean = NULL; w.mean = NULL}
  out <- list(input.rate = b1,
    input.unit = o2.unit,
    output.rate = result,
    output.unit = mo2.unit,
    mean = mean,
    weighted.mean = w.mean)
  class(out) <- "calc.mo2"
  return(out)
}

#' @export
print.calc.mo2 <- function(x) {
  results <- data.frame(input = x$input.rate, converted = x$output.rate)
  units <- data.frame(x$input.unit, x$output.unit)
  if (nrow(results) > 1) {
    mean <- x$mean
    w.mean <- x$weighted.mean
  }
  cat("MO2:\n")
  print(results)
  cat(sprintf("\nInput units:     %s", x$input.unit))
  cat(sprintf("\nConverted units: %s \n", x$output.unit))

  if (nrow(results) > 1) {
    cat(sprintf("\nMean:           %f",mean))
    cat(sprintf("\nWeighted mean:  %f \n", w.mean))
  }
}




# internal function use for calc.mo2 only!!!  This function calculates a 'scale
# factor' for a unit, based on the prefix of both 'unit.in' and 'unit.out'
# strings. Then, it uses the scale factor to convert a measurement number that
# may consist of 2-3 dimensional units. It's just something that can convert mg/s
# to mg/hr, or mmol/s to umol/hr, and so on, in calc.mo2.

scale.unit <- function(b1, unit.in, unit.out) {
  # create database of terms for matching
  prefix <- c("n", "u", "m", "", "k", "sec", "min", "hour")
  factor <- c(1e-19, 1e-06, 0.001, 1, 1000, 3600, 60, 1)
  suffix <- c("mol", "g", "L", "l", "")
  string <- "^(n|u|m||k|sec|min|hour)?(mol|g|L|l|)$"

  # clean input strings to extract units
  from <- stringr::str_replace(unit.in, "\\..*", "")
  from <- unlist(regmatches(from, regexec(string, from)))
  to <- stringr::str_replace(unit.out, "\\..*", "")
  to <- unlist(regmatches(to, regexec(string, to)))

  if (from[3] != to[3])
    stop("Units do not match and cannot be converted.", call. = F)

  # output conversion
  factor.in <- factor[match(from[2], prefix)]
  factor.out <- factor[match(to[2], prefix)]
  return(b1 * (factor.in/factor.out))
}



# internal function use for calc.mo2 only!!!  This function parses a character
# string e.g. mg/l/s and identifies the units in the string. The 'cond' argument
# checks whether the string is an input or output string. Generally, an input
# string is 2-dimensional. However the DO2 units may itself consist of 2 units,
# e.g. mg/l, ml/kg. Thus if the string is an 'input', and 3 units are detected,
# the function merges the first 2 units, before it tries to identify it e.g.
# mg/l/s will be identified as mg/l and s.  For an 'output' string, units are
# always 3-dimensional, so no string manipulation is needed before processing.
# cond: 'input', 'output'
id.unit <- function(string, cond) {
  units <- read.table(text = gsub("(?:-1|[/.[:space:]])+", " ", string), header = FALSE)
  units <- matrix(as.matrix(units), ncol = ncol(units))
  # This deals with 'mg/l/s', 'ug l-1 s-1'
  if (cond == "input" && length(units) == 3) {
    unit1 <- paste0(units[1], "/", units[2])
    unit1 <- checkUnits(unit1, "o2")[2]
    unit2 <- units[3]
    unit2 <- checkUnits(unit2, "time")[2]
    out <- data.frame(unit1, unit2)
    # this deals with '%/s'
  } else if (cond == "input" && length(units) == 2) {
    unit1 <- units[1]
    unit1 <- checkUnits(unit1, "o2")[2]
    unit2 <- units[2]
    unit2 <- checkUnits(unit2, "time")[2]
    out <- data.frame(unit1, unit2)
  } else if (cond == "output" && length(units) == 3) {
    unit1 <- units[1]
    unit1 <- checkUnits(unit1, "o1")[2]
    unit2 <- units[2]
    unit2 <- checkUnits(unit2, "time")[2]
    unit3 <- units[3]
    unit3 <- checkUnits(unit3, "mass")[2]
    out <- data.frame(unit1, unit2, unit3)
  } else if (cond == "output" && length(units) == 2) {
    unit1 <- units[1]
    unit1 <- checkUnits(unit1, "o1")[2]
    unit2 <- units[2]
    unit2 <- checkUnits(unit2, "time")[2]
    out <- data.frame(unit1, unit2)
  }
  return(as.matrix(out))
}
