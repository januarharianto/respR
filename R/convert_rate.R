#' Scale rate value to volume and/or mass
#'
#' This is a conversion function. It can convert a unit of rate, RO_2, into
#' volume-adjusted (e.g to the container), VO_2 or mass specific (i.e. to the
#' specimen), MO_2 rate.
#'
#' The function uses an inernal database and a fuzzy string matching algorithm
#' to accept various unit formatting styles.
#'
#' For example, `'mg/l', 'mg/L', 'mgL-1', 'mg l-1', 'mg.l-1'` are all the same.
#' Use [unit_args()] to view a list of usable unit strings.
#'
#' @param x numeric, or objects of class [calc_rate()], [auto_rate()] or
#'   [adjust_rate()].
#' @param o2.unit string. The dissolved oxygen unit to convert from. Check
#'   [unit_args()].
#' @param time.unit string. The time unit to convert from. Check [unit_args()].
#' @param output.unit string. The output unit to convert to. Check
#'   [unit_args()].
#' @param volume numeric. Volume in litres. This is the volume of the
#'   respirometry chamber, not the specimen(s).
#' @param mass numeric. Mass/weight in kg. This is the mass of the specimen if
#'   you wish to calculate mass-specific rates.
#' @param S numeric. Salinity. Defaults to 35.
#' @param t numeric. Temperature. Defaults to 25 (degress celcius).
#' @param P numeric. Air pressure. Defaults to 1.013253 (kPa).
#'
#' @return A list object.
#'
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' convert_rate(7.5, o2.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/kg', volume = 1.2, mass = 0.5)
convert_rate <- function(x, o2.unit = NULL, time.unit = NULL,
  output.unit = NULL, volume = NULL, mass = NULL, S = 35, t = 25, P = 1.013253)
  {

  # Validate inputs If units are set to NULL, use default values.
  if (is.null(o2.unit)) {
    warning("`o2.unit` is not provided, using `mg/L`.", call. = F)
    o2.unit <- "mg/L"
  }
  if (is.null(time.unit)) {
    warning("'time.unit' is not provided, using 's'.", call. = F)
    time.unit <- "s"
  }
  if (is.null(output.unit)) {
    warning("'output.unit' is not provided, using 'mg/h/kg`.",
      call. = F)
    output.unit <- "mg/h/kg"
  }

  # Volume must not be NULL
  if (is.null(volume))
    stop("Input argument for 'volume' must not be empty.")

  # Validate rate value based on object class
  if (is.numeric(x)) {
    rate <- x
  } else if (class(x) %in% c("calc_rate", "auto_rate")) {
    rate <- x$rate
  } else if (class(x) %in% "adjust_rate") {
    rate <- x$corrected
  } else stop("`x` input is not valid.")

  # Validate o2.unit & time.unit
  oxy <- verify_units(o2.unit, "o2")
  time <- verify_units(time.unit, "time")

  # Validate output.unit
  ou <- as.matrix(read.table(text = gsub("(?:-1|[/.[:space:]])+",
    " ", output.unit), header = FALSE))
  is.MO2 <- length(ou) == 3
  A <- verify_units(ou[1], "o1")
  B <- verify_units(ou[2], "time")
  if (is.MO2) {
    C <- verify_units(ou[3], "mass")
    ou <- as.matrix(data.frame(A, B, C))
  } else ou <- as.matrix(data.frame(A, B))

  # Verify if mass is needed
  if (is.MO2 && is.null(mass))
    stop("'output.unit' needs a value for 'mass'.")
  if (!is.MO2 && is.numeric(mass))
    warning("mass' is ignored as `output.unit` does not require it.",
      call. = F)

  # Format unit strings to look nicer
  o2.unit <- stringr::str_replace(oxy, "\\..*", "")
  time.unit <- stringr::str_replace(time, "\\..*", "")
  output.unit <- stringr::str_replace(ou, "\\..*", "")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("mmol.o2", "umol.o2")) {
    RO2 <- convert_rate(rate, oxy, "mmol/L", S, t, P)
    RO2 <- adjust_scale(RO2$converted, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- convert_DO(rate, oxy, "mg/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "mg.o2", A)
  } else if (A == "ml.o2") {
    RO2 <- convert_DO(rate, oxy, "mL/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "ml.o2", A)
  }

  # Then, convert time unit
  RO2 <- adjust_scale(RO2, time, B)

  # Then, scale to volune
  VO2 <- RO2 * volume

  # Then, scale to mass
  if (is.MO2) {
    MO2 <- VO2/mass
    MO2 <- adjust_scale(MO2, "kg.mass", C)
  }

  # Generate output
  summary <- data.frame(input.rate = rate, converted.rate = RO2,
    volumetric = VO2)
  if (is.MO2) {
    summary <- data.frame(summary, mass.specific = MO2)
    converted <- MO2
  } else converted <- VO2

  out <- list(input = rate, output = converted, summary = summary,
    input.o2.unit = o2.unit, input.time.unit = time.unit,
    output.unit = output.unit)

  class(out) <- "convert_rate"
  return(out)
}

#' @export
print.convert_rate <- function(x, ...) {
  cat("Input:\n")
  print(x$input)
  print(c(x$input.o2.unit, x$input.time.unit))
  cat("Output:\n")
  print(x$output)
  print(x$output.unit)
}


#' @export
summary.convert_rate <- function(object, ...) {
  out <- (data.frame(t(c(object$summary,
    input.o2.unit = object$input.o2.unit,
    input.time.unit = object$input.time.unit,
    output.unit = object$output.unit)))
    )
  return(out)
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