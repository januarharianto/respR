#' Calculate volume and/or mass-specific rate of change in oxygen concentration
#'
#' This is a conversion function that can convert value(s) of rate of change in oxygen concentration to volume-specific, and/or volume- and mass-specific rate of change in oxygen concentration.
#'
#' `calc.mo2` integrates well with output objects from [calc.rate()] and [auto.rate()]. Alternatively, the user may convert any numeric vector.
#'
#' @author Januar Harianto & Nicholas Carey
#'
#' @md
#' @param x numeric, or an object of class `calc.rate` or `auto.rate`. The input object to calculate the conversion.
#' @param unit.in character string. The units to convert from. Units can be separated by a space, e.g. "`mg l-1 s-1`", a slash, e.g. "mg/l/s". More information about unit strings in [convert.do()].
#' @param unit.out character string. The units to convert into. Units can be separated by a space, e.g. "`mg l-1 s-1`", a slash, e.g. "mg/l/s". More information about unit strings in [convert.do()].
#' @param volume numeric. The volume of medium to correct to. Must be in litres (L).
#' @param mass numeric (optional). The mass of the speciment to correct to. Must be in kilograms (kg).
#' @param rank numeric (optional). If the input is of class `auto.rate`, the user may convert any of the ranked outputs by specifying the rank here. Note: not used for other input types.
#' @param S numeric. Salinity, defaults to 35.
#' @param t numeric. Temperature in degrees C, defaults to 25.
#' @param P numeric. Pressure unit in bar, defaults to 1.013253.
#'
#' @return An object containing a list of outputs:
#' \describe{
#' \item{`input.class`}{String. Helps the user identify the source of the input
#'   `x`. Possible values: `calc.rate`, `auto.rate`, `numeric`.}
#' \item{`input.id`}{String. If the source of the input `x` is `auto.rate`,
#'   identifies it's ranking method. Possible values: `maxmin`, `interval`,
#'   `automatic`, `calc.rate`, `numeric`.}
#' \item{`volume`}{Numeric. Same as input parameter.}
#' \item{`mass`}{Numeric. Same as input parameter.}
#' \item{`rank`}{Numeric. Same as input parameter.}
#' \item{`salinity`}{Numeric. Same as input parameter.}
#' \item{`temperature`}{Numeric. Same as input parameter.}
#' \item{`pressure`}{Numeric. Same as input parameter.}
#' \item{`summary`}{Data frame. Contains all inputs and converted outputs.}
#' \item{`unit.in`}{String. Input units identified by the function.}
#' \item{`unit.out`}{String. Output units identified by the function.}
#' \item{`converted`}{Numeric (vector). Converted value(s).}
#' \item{`weighted`}{Numeric. The weighted average of conversions (useful for
#'   intermittent data).}
#' }
#'
#' @seealso [calc.rate] auto.rate
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract str_replace
#' @export
#'
#' @examples
#' x <- calc.rate(sardine, bg = 0.0001234)
#' calc.mo2(x, "%/s", "mg/s", volume = 1.2)
#'
#' x <- calc.rate(intermittent, c(200,2300,4100), c(1800,3200,4600), by = 'time')
#' calc.mo2(x, "mg/l/s", "mg/s/kg", volume = 1, mass = 1)
#'
#' x <- auto.rate(squid, logic = "interval")
#' calc.mo2(x, "%/s", "mg/s/g", volume = 1, mass = .5)
calc.mo2 <- function(x, unit.in, unit.out, volume = NULL, mass = NULL,
  rank = 1, S = 35, t = 25, P = 1.013253) {
  # Extract the object class for use later:
  input.class <- class(x)
  # Check that the user has the right inputs:
  if (is.null(volume))
    stop("Input argument for 'volume' must not be empty.", call. = F)
  # Save input ID if it is from calc.rate or auto.rate
  if (class(x) == "numeric") {
    input.id <- "numeric"
  } else input.id <- x$id
  # ----------------------------------------------------------------------------
  # Extract the rate(s)
  # First, check if x is numeric. If it is, just use it:
  if (class(x) == "numeric") {
    b1 <- x
    # ----------------------------------------------------------------------------
    # Otherwise, check if x is class "calc.rate" or "auto.rate":
  } else if (class(x) == "calc.rate" | class(x) == "auto.rate") {
    # Extract rate(s) based on object 'id' and if outputs contain bg rates.
    # These were previously determined in their respective functions, so it
    # is just a matter of identifying them:
    if (x$id == "calc.rate" | x$id == "automatic") {
      if (length(x$results) == 11) b1 <- x$results$b1
      if (length(x$results) == 13) b1 <- x$results$`b1-bg`
    } else if (x$id == "maxmin" | x$id == "interval") {
      if (length(x$results) == 12) b1 <- x$results$b1
      if (length(x$results) == 14) b1 <- x$results$`b1-bg`
    }
    # ----------------------------------------------------------------------------
    # In the worst case scenario, input is not right; stop running:
  } else {
    stop("Input a numeric, `calc.rate` or `auto.rate` object, for `x`.",
      call. = F)
  }
  # ----------------------------------------------------------------------------
  # Let's convert the DO/time to MO2 first.
  # We identify the input units:
  u.in <- id.unit(unit.in, "input")  # should identify 2 units
  # Then we identify the output units:
  u.out <- id.unit(unit.out, "output")  # should identify 2-3 units
  if (length(u.out) == 3 && is.null(mass))
    stop("Output units specify mass, however mass is not defined (i.e. NULL)",
      call. = F)
  if (length(u.out) == 2 && !is.null(mass))
    stop("Output units did not specify mass, but a mass value was detected.",
      call. = F)
  # Format the units (for display later):
  o2.unit <- paste(u.in, collapse = "/")
  mo2.unit <- paste(u.out, collapse = "/")
  # ----------------------------------------------------------------------------
  # Error checks.
  # If output unit requires mass, check if user has that input:
  has.mass <- length(u.out) == 3
  if (is.null(mass) && has.mass)
    stop("Please include 'mass' argument.", call. = F)
  if (is.numeric(mass) && has.mass == F)
    stop("Mass input detected, but 'to' argument is missing a unit.", call. = F)
  # Now check that the incoming and outgoing unit types are in the same group
  # This is an internal check. We want to ensure that we're not trying to
  # convert something unexpected.
  s <- "[.][:alnum:]*"  # the string to match
  o2.match <- all.equal(str_extract(u.in[1], s), str_extract(u.out[1], s))
  time.match <- all.equal(str_extract(u.in[2], s), str_extract(u.out[2], s))
  if (o2.match == FALSE)
    stop("Input and output O2 units cannot be converted!", call = F)
  if (time.match == FALSE)
    stop("Input and output time units don't match! This shouldn't happen.",
      call = F)
  # ----------------------------------------------------------------------------
  # Time to do the conversions.
  # First convert to 'standard' units first based on input. This makes the
  # second conversion easier. Also, multiply by volume.
  if (u.out[1] == "mmol.o2" | u.out[1] == "umol.o2") {
    result <- convert.do(b1, u.in[1], "mmol/L", S, t, P) * volume
    result <- scale.unit(result, "mmol.o2", u.out[1])
  } else if (u.out[1] == "mg.o2" | u.out[1] == "ug.o2") {
    result <- convert.do(b1, u.in[1], "mg/L", S, t, P) * volume
    result <- scale.unit(result, "mg.o2", u.out[1])
  } else if (u.out[1] == "ml.o2") {
    result <- convert.do(b1, u.in[1], "mL/L", S, t, P) * volume
    result <- scale.unit(result, "ml.o2", u.out[1])
  } else stop("Units of pressure are currently not supported. Sorry!",
    call. = F)
  # ----------------------------------------------------------------------------
  # Scale other units
  # Convert time unit (e.g. sec to min)
  vo2 <- scale.unit(result, u.in[2], u.out[2])
  m.vo2 <- mean(vo2)
  if (!class(x) == "numeric") wm.vo2 <- weighted.mean(vo2, x$results$time.len)

  # If output is mass-specific, then scale to mass as well:
  if (has.mass) {
    mo2 <- scale.unit((result/mass), "kg", u.out[3])
    m.mo2 <- mean(mo2)
    if (!class(x) == "numeric") wm.mo2 <- weighted.mean(mo2, x$results$time.len)
  }
  # CONVERSION COMPLETE

  # ----------------------------------------------------------------------------
  # Prepare data for output:
  if (!class(x) == "numeric") {
    ro2 <- x$results$b1  # grab the original input(s)
  } else ro2 <- x; w.rate = NULL
  # Generate the summary table and rates:
  if (has.mass) {
    summary <- data.frame(RO2 = ro2, VO2 = vo2, MO2 = mo2)
    rate   <- mo2
    if (!class(x) == "numeric") w.rate <- wm.mo2
  } else {
    summary <- data.frame(RO2 = ro2, VO2 = vo2)
    rate   <- vo2
    if (!class(x) == "numeric") w.rate <- wm.vo2
  }
  # If input if of class "auto.rate", pick the ranked result:
  if (class(x) == "auto.rate") {
    if (has.mass) rate <- mo2
    if (has.mass == F) rate <- vo2
  }
  # ----------------------------------------------------------------------------
  # Generate output:
  if (class(x) == "auto.rate") {
    out <- list(
      input.class = input.class,
      input.id    = input.id,
      volume      = volume,
      mass        = mass,
      rank        = rank,
      salinity    = S,
      temperature = t,
      pressure    = P,
      summary     = summary,
      unit.in     = o2.unit,
      unit.out    = mo2.unit,
      converted   = rate
    )
    # For all other outputs, we just need to figure if the rate is mass-specific
    # or not.
  } else {
    out <- list(
      input.class = input.class,
      input.id    = input.id,
      volume      = volume,
      mass        = mass,
      rank        = rank,
      salinity    = S,
      temperature = t,
      pressure    = P,
      summary     = summary,
      unit.in     = o2.unit,
      unit.out    = mo2.unit,
      converted   = rate,
      weighted    = w.rate
    )
  }
  class(out) <- "calc.mo2"
  return(out)
}










#' Print output with rank argument
#'
#' @param x object of rank `calc.mo2`.
#' @param rank numeric.
#'
#' @export
print.calc.mo2 <- function(x, rank = 1) {
  if (length(x$summary) == 2) Output <- x$summary$VO2
  if (length(x$summary) == 3) Output <- x$summary$MO2
  # output for ranked results in auto.rate
  if (x$input.class == "auto.rate" && !x$input.id == "interval") {
    message(sprintf("Rank %g result", rank))
    cat(sprintf("Input : %f\n", x$summary$RO2[rank]))
    cat(sprintf("Output: %f\n", x$converted[rank]))
  } else {
    result <- data.frame(Input = x$summary$RO2, Output)
    if(nrow(x$summary) > 6) {
      cat("Showing only first 6 results:\n")
      print(head(result))
    } else {
      print(result)
      cat(sprintf("\nMean (weighted) : %f", x$weighted))
    }
  }
  cat(sprintf("\nInput unit : %s", x$unit.in))
  cat(sprintf("\nOutput unit: %s", x$unit.out))
}








#' @export
summary.calc.mo2 <- function(x) {
  cat(sprintf("Data was converted from object of class %s.\n", x$input.class))
  if (x$input.class == "auto.rate")
    cat("Ranking method:", x$input.id, "\n")
  cat("\nRO2: rate before conversion\n")
  if(length(x$summary) > 2)
    cat("VO2: rate after volume and time conversion\n")
  cat("MO2: rate after volume, time and mass conversion\n\n")
  if (nrow(x$summary) > 6) {
    cat("Showing on the first 6 results in summary:\n")
    print(head(x$summary))
  } else print(x$summary)
  cat(sprintf("\nInput unit : %s", x$unit.in))
  cat(sprintf("\nOutput unit: %s", x$unit.out))
}










# ==============================================================================
# Internal Functions

#
#' Scale units of the same base.
#'
#' This function performs simple conversion in units with the same base, e.g. x/s to x/h, or mmol/x to umol/x.
#'
#' This is an internal function. The typical user should not see this.
#'
#' @md
#' @param b1 numeric vector.
#' @param unit.in character string.
#' @param unit.out character string.
#' @return numeric vector.
#'
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








#' Identify units in a character string for conversions.
#'
#' Checks strings and match them to a known database. The database is highly specific - and is divided into "input" or "output" groups.
#'
#' This is an internal function. The typical user should not see this.
#'
#' @md
#' @param string character string.
#' @param cond string. `"input"` or `"output"`.
#' @return A character string.
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
