#' @export
calc.mo2 <- function(x, unit.in, unit.out, volume = NULL, mass = NULL, S = 35, t = 25, P = 1.013253) {
  # check that the user has the right inputs
  if(is.null(volume)) stop("Input argument for 'volume' must not be empty.", call. = F)
  # check if input is class "calc.rate" - if it is, we extract the results
  if (class(x) == 'calc.rate') {
    b1 <- x$results$b1
  } else b1 <- x # otherwise, use the input given
  #conversion <- convert.to.mo2(b1, unit.in, unit.out, volume, mass, S, t, P)

  # let's convert the DO/time to MO2
  # first we identify the units based on whether they are input or output:
  u.in  <- id.unit(unit.in, 'input') # should identify 2 units
  u.out <- id.unit(unit.out, 'output') # should identify 2-3 units

  # check if 'u.out' requires 'mass' argument, or not
  if (is.null(mass) && length(u.out) == 3)
    stop("Please include 'mass' argument.", call. = F)
  if (is.numeric(mass) && length(u.out) == 2)
    stop("Mass input detected, but 'to' argument is missing a unit.", call. = F)

  # now check that the unit types match (if they don't match, conversion is not possible):
  group.id   <- '[.][:alnum:]*' # this is the string to match
  o2.match   <- all.equal(stringr::str_extract(u.in[1], group.id),
                          stringr::str_extract(u.out[1], group.id))
  time.match <- all.equal(stringr::str_extract(u.in[2], group.id),
                          stringr::str_extract(u.out[2], group.id))
  if (o2.match   == FALSE) stop("O2 units don't match.")
  if (time.match == FALSE) stop("Time units don't match.")

  # if checks are good - let's do conversions
  # first, convert o2 unit to mg/l, and then scale to volume:
  # NO. must convert to xx/l where xx is equal to the output! # massive changes... omg
  #result <- convert.do(b1, u.in[1], 'mg/L', S = S, t = t, P = P) * volume

  # 1. convert o2 units based on output o2 unit, and scale to volume
  # 2. adjust scale of o2 unit
  if (u.out[1] == 'mmol.o2' | u.out[1] == 'umol.o2') {
    result <- convert.do(b1, u.in[1], 'mmol/L', S = S, t = t, P = P) * volume
    result <- scale.unit(result, 'mmol.o2', u.out[1])
  } else if (u.out[1] == 'mg.o2' | u.out[1] == 'ug.o2') {
    result <- convert.do(b1, u.in[1], 'mg/L', S = S, t = t, P = P) * volume
    result <- scale.unit(result, 'mg.o2', u.out[1])
  } else if (u.out[1] == 'ml.o2') {
    result <- convert.do(b1, u.in[1], 'mL/L', S = S, t = t, P = P) * volume
    result <- scale.unit(result, 'ml.o2', u.out[1])
  } else stop("Units of pressure are currently not supported.",  call. = F)
  # 3. adjust scale of time unit
  result <- scale.unit(result, u.in[2], u.out[2])
  # 4. scale to mass, if provided (note, mass MUST be in kg)
  if (!is.null(mass)) result <- scale.unit(result / mass, 'kg', u.out[3])

  # final mo2 units (for output):
  mo2.unit <- paste(u.out, collapse = "/")
  # calculations are complete - if input is class 'calc.rate', we need to create a modified output
  if (class(x) == 'calc.rate') {
    out <- data.frame(x$results, b1.adj = result[[1]])
    # the average and weighted average need to be calculated:
    av.mo2 <- mean(result)
    w.av.mo2 <- weighted.mean(result, x$subsets$time.width)
    means <- data.frame(Rate = c(x$average, x$weighted.average), MO2 = c(av.mo2, w.av.mo2))
    row.names(means) <- c("Rate", "Weighted rate")
    out <- list(alldata = x, mo2s = result, mean.mo2 = means, unit.out = mo2.unit)
    class(out) <- 'calc.rate.mo2'
  } else {
    out <- list(result = result, unit.out = mo2.unit)
    class(out) <- 'calc.num.mo2' # assign output to different class if input is not of class 'calc.rate'
  }
  return(out)
}

#' @export
print.calc.rate.mo2 <- function(x) {
  cat('Results:\n')
  print(data.frame(x$alldata$results, mo2 = x$mo2s))
  cat('\nSubsetting locations:\n')
  print(x$alldata$subsets)
  cat('\nMeans:\n')
  if (length(x$alldata$results) > 3) {
    cat('(Results have been background-corrected.)\n')
  }
  print(x$mean.mo2)
}


#' @export
print.calc.num.mo2 <- function(x) {
  result <- x$result
  units <- x$unit.out
  cat(sprintf('MO2:    %g', result), '\n')
  cat(sprintf('Units:  %s', units))
}



# internal function
# use for calc.mo2 only!!!
# This function calculates a "scale factor" for a unit, based on the prefix of
#   both "unit.in" and "unit.out" strings. Then, it uses the scale factor to
#   convert a measurement number that may consist of 2-3 dimensional units. It's
#   just something that can convert mg/s to mg/hr, or mmol/s to umol/hr, and so
#   on, in calc.mo2.

scale.unit <- function(b1, unit.in, unit.out) {
  # create database of terms for matching
  prefix <- c('n', 'u', 'm', '', 'k', 'sec', 'min', 'hour')
  factor <- c(1e-19, 1e-6, 1e-3, 1, 1e3, 3600, 60, 1)
  suffix <- c('mol', 'g', 'L', 'l', '')
  string <- "^(n|u|m||k|sec|min|hour)?(mol|g|L|l|)$"

  # clean input strings to extract units
  from <- stringr::str_replace(unit.in, "\\..*", '')
  from <- unlist(regmatches(from, regexec(string, from)))
  to <- stringr::str_replace(unit.out, "\\..*", '')
  to <- unlist(regmatches(to, regexec(string, to)))

  if (from[3] != to[3]) stop("Units do not match and cannot be converted.", call. = F)

  # output conversion
  factor.in <- factor[match(from[2], prefix)]
  factor.out <- factor[match(to[2], prefix)]
  return(b1 * (factor.in / factor.out))
}



# internal function
# use for calc.mo2 only!!!
# This function parses a character string e.g. mg/l/s and identifies the units
#   in the string. The 'cond' argument checks whether the string is an input or
#   output string. Generally, an input string is 2-dimensional. However the DO2
#   units may itself consist of 2 units, e.g. mg/l, ml/kg. Thus if the string is
#   an "input", and 3 units are detected, the function merges the first 2 units,
#   before it tries to identify it e.g. mg/l/s will be identified as mg/l and s.
#   For an 'output' string, units are always 3-dimensional, so no string
#   manipulation is needed before processing.
id.unit <- function(string, cond){ # cond: 'input', 'output'
  units <- read.table(text=gsub("(?:-1|[/.[:space:]])+", " ", string), header=FALSE)
  units <- matrix(as.matrix(units), ncol = ncol(units))
  # This deals with 'mg/l/s', 'ug l-1 s-1'
  if (cond == 'input' && length(units) == 3) {
    unit1 <- paste0(units[1], "/", units[2])
    unit1 <- checkUnits(unit1, 'o2')[2]
    unit2 <- units[3]
    unit2 <- checkUnits(unit2, 'time')[2]
    out <- data.frame(unit1, unit2)
    # this deals with '%/s'
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
