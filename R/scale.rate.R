#' @export
scale.rate <- function(x, o2.unit = NULL, time.unit = NULL, output.unit = NULL,
  volume = NULL, mass = NULL, S = 35, t = 25, P = 1.013253) {
  # Validate inputs
  # ----------------------------------------------------------------------------
  if (is.null(o2.unit)) {
    warning("`o2.unit` is not provided, using `mg/L`.", call. = F)
    o2.unit <- "mg/L"
  }
  if (is.null(time.unit)) {
    warning("'time.unit' is not provided, using 's'.", call. = F)
    time.unit <- "s"
  }
  if (is.null(volume)) stop("Input argument for 'volume' must not be empty.")
  # if x is an object of previous function, grab the rate value only
  if (class(x) %in% c("calc.rate", "auto_rate", "auto.rate")) x <- x$rate
  # check o2.unit and time.unit
  oxy  <- verify.units(o2.unit, "o2")
  time <- verify.units(time.unit, "time")
  if (oxy[1] == FALSE) stop("'o2.unit' not recognised.")
  if (time[1] == FALSE) stop("'time.unit' not recognised.")
  if (is.null(output.unit)) {
    warning("`output.unit' is not provided, using 'mg/h/kg.'", call. = F)
    output.unit <- "mg/h/kg"
  }
 # check output.unit
  uo <- id.unit(output.unit, "output")  # should identify 2-3 units
  if (length(uo) == 3 && is.null(mass))
    stop("Output units require mass input, however 'mass' is NULL.")
  if (length(uo) == 2 && !is.null(mass))
    stop("'mass' input provided, but output units do not seem to need it.")
  # End input validation
  # ----------------------------------------------------------------------------

  # Format the units (for display later):
  o2.unit <- stringr::str_replace(oxy[2],"\\..*","")
  time.unit <- stringr::str_replace(time[2],"\\..*","")
  end.unit <- stringr::str_replace(uo,"\\..*","")
  end.unit <- paste(end.unit, collapse = "/")

  s <- "[.][:alnum:]*"  # the string to match
  o2.match <- all.equal(stringr::str_extract(oxy[2], s),
                        stringr::str_extract(uo[1], s))
  time.match <- all.equal(stringr::str_extract(time[2], s),
                          stringr::str_extract(uo[2], s))
  if (!o2.match) stop("Input and output O2 units cannot be converted!")
  if (!time.match) stop("Input and output time units don't match.")

  # 1. Convert the O2 unit
  if (uo[1] %in% c("mmol.o2", "umol.o2")) {
    rate <- convert.rate(x, oxy[2], "mmol/L", S, t, P)
    rate <- adjust.multiplier(rate, "mmol.o2", uo[1])
  } else if (uo[1] %in% c("mg.o2", "ug.o2")) {
    rate <- convert.rate(x, oxy[2], "mg/L", S, t, P)
    rate <- adjust.multiplier(rate, "mg.o2", uo[1])
  } else if (uo[1] == "ml.o2") {
    rate <- convert.rate(x, oxy[2], "mL/L", S, t, P)
    rate <- adjust.multiplier(rate, "ml.o2", uo[1])
  }
  # 2. Convert the Time unit
  rate <- adjust.multiplier(rate, time[2], uo[2])
  # 3. Scale to volume
  VO2 <- rate * volume
  # 4. Scale to mass
  if (!is.null(mass)) {
    MO2 <- VO2 / mass
    MO2 <- adjust.multiplier(MO2, "kg", uo[3])
  }
  # Output
  out <- list(VO2 = VO2)
  if (!is.null(mass)) {
    MO2 <- list(MO2 = MO2)
    out <- c(out, MO2)
  }
  unit <- list(input.o2.unit = o2.unit, input.time.unit = time.unit,
    output.unit = end.unit)
  out <- c(out, unit)
  return(out)
}



# Convert between multipliers of the same unit, e.g. mg to kg
adjust.multiplier <- function(x, input, output) {
  # Create database of terms for matching
  prefix <- c("n", "u", "m", "", "k", "sec", "min", "hour")
  suffix <- c("mol", "g", "L", "l", "")
  multip <- c(1e-19, 1e-06, 0.001, 1, 1000, 3600, 60, 1)
  string <- "^(n|u|m||k|sec|min|hour)?(mol|g|L|l|)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")      # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef))) # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")     # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft))) # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("Units do not match and cannot be converted.", call. = F)
  # Convert!
  a   <- multip[match(bef[2], prefix)] # get multiplier from input
  b   <- multip[match(aft[2], prefix)]   # get multiplier from output
  out <- x * (a / b)                    # convert
  return(out)
}



# Also unused, now
# id.unit <- function(string, cond) {
#   units <- read.table(text = gsub("(?:-1|[/.[:space:]])+", " ", string), header = FALSE)
#   units <- matrix(as.matrix(units), ncol = ncol(units))
#   # This deals with 'mg/l/s', 'ug l-1 s-1'
#   if (cond == "input" && length(units) == 3) {
#     unit1 <- paste0(units[1], "/", units[2])
#     unit1 <- checkUnits(unit1, "o2")[2]
#     unit2 <- units[3]
#     unit2 <- checkUnits(unit2, "time")[2]
#     out <- data.frame(unit1, unit2)
#     # this deals with '%/s'
#   } else if (cond == "input" && length(units) == 2) {
#     unit1 <- units[1]
#     unit1 <- checkUnits(unit1, "o2")[2]
#     unit2 <- units[2]
#     unit2 <- checkUnits(unit2, "time")[2]
#     out <- data.frame(unit1, unit2)
#   } else if (cond == "output" && length(units) == 3) {
#     unit1 <- units[1]
#     unit1 <- checkUnits(unit1, "o1")[2]
#     unit2 <- units[2]
#     unit2 <- checkUnits(unit2, "time")[2]
#     unit3 <- units[3]
#     unit3 <- checkUnits(unit3, "mass")[2]
#     out <- data.frame(unit1, unit2, unit3)
#   } else if (cond == "output" && length(units) == 2) {
#     unit1 <- units[1]
#     unit1 <- checkUnits(unit1, "o1")[2]
#     unit2 <- units[2]
#     unit2 <- checkUnits(unit2, "time")[2]
#     out <- data.frame(unit1, unit2)
#   }
#   return(as.matrix(out))
# }


# unused (may use later)
# identify.units <- function(x) {
#   out <- as.matrix(read.table(text = gsub("(?:-1|[/.[:space:]])+", " ", x),
#     header = FALSE))
#   out
# }
