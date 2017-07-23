#' Calcurate MO2
#'
#' Still working out a good description for this...
#' @param input A number, or an object of class \code{calcRate}. You may directly parse the output of a previous \code{calcRate} result.
#' @param from Character string. This are the units used to calculate the rate in \code{input}. Must include the precise O2 unit and the time unit, e.g. "mg L-1 s-1" or "mg/l/s". A space, "/" or "." must separate each unit in the character string.
#' @param to Character string. Your intended output units. Mass, if included, must be positioned at the end of the string e.g. "mg/l/kg", NOT "mg/kg/l". A space, "/" or "." must separate each unit in the character string.
#' @param volume Numeric. Volume of the seawater used, in litre/liter (L).
#' @param mass Numeric. Defaults to NULL. Mass/weight of the animal, in kilograms (kg).
#' @param ... for s, P, t. I'll probably convert this soon (just testing the ... method but it gets unclear)
#'
#' @return I'll put something in here soon.
#' @export
#'
#' @examples
#' # Basic conversion, with no specimen weight:
#' calcMO2(0.5, 'mg l-1s-1', 'mg/s', volume = 1)
#'
#' # Below, will output an error as the "to" argument is wrongly formatted.
#' # Instead of 'ug/kg/h", the mass should be last, i.e. 'ug/h/kg'
#' calcMO2(0.0007, from = '%/s', to = 'ug/kg/h', volume = 0.74, mass = 0.33)
#' calcMO2(0.0007, '%/s', 'ug/h/kg', volume = 0.74, mass = 0.33) # this works
#'
#' # We can use this function directly on an output of "calcRate"
#' data("sardine")
#' sard <- calcRate(sardine)
#' sard_mo2 <- calcMO2(sard, '% s-1', 'ug/h/kg', volume = 1.1, mass = 0.21)
#'
#' # Intermittent data is also supported
#' data("intermittent")
#' intermm <- calcRate(intermittent, c(200,2300,4100), c(1800,3200,4600), by = 'time')
#' intermm_mo2 <- calcMO2(intermm, from = 'mg/l/s', to = 'mg/s/kg', volume = 0.8, mass = 0.3)

calcMO2 <- function(input, from, to, volume, mass = NULL, ...) {
  # check if input is class 'calcRate'
  if (class(input) == 'calcRate') {
    summtab <- input$summary # extract summary table
    b1 <- summtab$b1 # extract rates
    mo2 <- convMO2(b1, from, to, volume, mass) # convert rates to MO2
    out <- data.frame(summtab, mo2 = mo2$mo2, units.mo2 = paste(mo2$units, collapse = "/"))
    print(out)
    # recalculate average and intermittent MO2
    w.sum <- (out$to.time - out$from.time) # weighted sum
    av <- mean(out$mo2) # average of slopes
    w.av <- sum(out$mo2 * w.sum) / sum(w.sum) # weighted average of slopes
    cat('\n', sprintf('MO2          %g ', av))  # print results
    cat('\n', sprintf('Weighted MO2 %g ', w.av))


  } else {
    # otherwise, input is a number, so let's do normal processing
    result <- convMO2(input, from, to, volume, mass, ...)
    out <- data.frame(mo2 = result$mo2, units= paste(result$units.mo2, collapse = "/"))
    print(out)
  }
  return(invisible(out))
}


# Internal
# Convert do2 to mo2 units
convMO2 <- function(x, from, to, volume, mass = NULL, ...) {
  # let's dissect out the units
  f.un <- parseunit(from, 'input') # extract units and ID them
  t.un <- parseunit(to, 'output')
  # quick checks before we continue
  if (is.null(mass) && length(t.un) == 3) stop("Please include 'mass' argument.") # check if non mass-specific
  if (is.numeric(mass) && length(t.un) == 2) stop("'to' argument is missing a unit.") # check if mass-specific
  # chceck that the suffixes match
  suffix <- '[.][:alnum:]*' # this is the unit to match
  checkO2 <- all.equal(stringr::str_extract(f.un[1], suffix), stringr::str_extract(t.un[1], suffix))
  checkTime <- all.equal(stringr::str_extract(f.un[2], suffix), stringr::str_extract(t.un[2], suffix))
  if (checkO2 == FALSE) stop("O2 units don't match.")
  if (checkTime == FALSE) stop("Time units don't match.")
  # let's do conversions
  # first, the DO unit will either be in 'x/kg s-1' or 'x/l s-1' e.g. 'mg/kg s-1'.
  # so that needs to be converted, let's pick conversion to mg/L since we will be using L to convert MO2
  out <-  convUnits(x, f.un[1], 'mg/L', S = 35, t = 25, P = 1.013253) * volume
  # so now it could be either mg/s or mg/min or mg/h
  # output is ug/h/g
  # so convert first unit, then second unit
  out <- unitconv(out, 'mg.o2', t.un[1])
  out <- unitconv(out, f.un[2], t.un[2])

  # If mass is provided
  if (is.numeric(mass) && length(t.un) == 3) {
    out <- out / mass
    out <- unitconv(out, 'kg', t.un[3])
  }
  out <- list(mo2 = out,
    units.mo2 = stringr::str_split(to, "(?:-1|[/.[:space:]])+")[[1]])
  return(out)
}

# internal
# convert metric units based on prefix, and time in h, min and s.
# does not work for units of pressure (not yet)
unitconv <- function(x, from, to) {
  # create database of terms for matching
  prefix <- c('n', 'u', 'm', '', 'k', 'sec', 'min', 'hour')
  factor <- c(1e-19, 1e-6, 1e-3, 1, 1e3, 3600, 60, 1)
  suffix <- c('mol', 'g', 'L', 'l', '')
  string <- "^(n|u|m||k|sec|min|hour)?(mol|g|L|l|)$"

  # clean input strings to extract units
  from <- str_replace(from, "\\..*", '')
  from <- str_replace(from, "\\/.*", '')
  from <- unlist(regmatches(from, regexec(string, from)))
  to <- str_replace(to, "\\..*", '')
  to <- str_replace(to, "\\/.*", '')
  to <- unlist(regmatches(to, regexec(string, to)))

  if (from[3] != to[3]) stop("Base units don't match")

  # output conversion
  factor.from <- factor[match(from[2], prefix)]
  factor.to <- factor[match(to[2], prefix)]
  return(x * (factor.from / factor.to))
}

# Internal.
# This function is able to extract units from 2d and 3d unit strings.
# Just setting it apart since it's a hack and I should make it better.
# parseunit('mg/s', 'output')
# parseunit('ug/min', 'output')
# parseunit('mg/l/s', 'input')
# parseunit('%/s', 'input')
# parseunit('torr/s', 'input')
# parseunit('mg / l / s', 'input')
parseunit <- function(string, cond){ # cond: 'input', 'output'
  units <- read.table(text=gsub("(?:-1|[/.[:space:]])+", " ", string), header=FALSE)
  units <- matrix(as.matrix(units), ncol = ncol(units))
  # This deals with 'mg/l/s', 'ug l-1 s-1'
  if (cond == 'input' && length(units) == 3) {
    unit1 <- paste0(units[1], "/", units[2])
    unit1 <- checkUnits(unit1, 'o2')[2]
    unit2 <- units[3]
    unit2 <- checkUnits(unit2, 'time')[2]
    out <- data.frame(unit1, unit2)
    # this deals with '%/s' and possibly the pressure units (which I have not implemented)
  } else if (cond == 'input' && length(units) == 2) {
    unit1 <- units[1]
    unit1 <- checkUnits(unit1, 'o2')[2]
    unit2 <- units[2]
    unit2 <- checkUnits(unit2, 'time')[2]
    out <- data.frame(unit1, unit2)
  } else if (cond == 'output' && length(units) == 3) {
    unit1 <- units[1]
    unit1 <- checkUnits(unit1, 'o1')[2]
    unit2 <- units[2]
    unit2 <- checkUnits(unit2, 'time')[2]
    unit3 <- units[3]
    unit3 <- checkUnits(unit3, 'mass')[2]
    out <- data.frame(unit1, unit2, unit3)
  } else if (cond == 'output' && length(units) == 2) {
    unit1 <- units[1]
    unit1 <- checkUnits(unit1, 'o1')[2]
    unit2 <- units[2]
    unit2 <- checkUnits(unit2, 'time')[2]
    out <- data.frame(unit1, unit2)
  }
  return(as.matrix(out))
}
