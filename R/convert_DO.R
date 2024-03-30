#' Convert between units of dissolved oxygen
#'
#' This is a conversion function that performs conversions between concentration
#' and pressure units of dissolved oxygen (DO).
#'
#' The function uses a fuzzy string matching algorithm to accept various unit
#' formatting styles. For example, `"mg/l"`, `"mg/L"`, `"mgL-1"`, `"mg l-1"`,
#' `"mg.l-1"` are all parsed the same. See `[unit_args()]` for details of
#' accepted units.
#'
#' Oxygen concentration units should use SI units (`L` or `kg`) for the
#' denominator.
#'
#' Some DO units require temperature (`t`), salinity (`S`), and atmospheric
#' pressure (`P`) to be specified; if this is the case the function will stop
#' and prompt for them. For the atmospheric pressure input (P), a default value
#' of 1.013 bar (standard pressure at sea level) is applied if not otherwise
#' entered. For freshwater experiments, salinity should be set to zero (i.e. `S
#' = 0`).
#'
#' ## S3 Generic Functions
#'
#' Saved output objects (if `simplify = FALSE` is used) can be entered in the
#' generic S3 functions `print()` and `summary()`.
#'
#' - `print()`: prints input and converted values (up to first 20), plus input
#' and output units.
#'
#' - `summary()`: simple wrapper for `print()` function. See above.
#'
#' @return By default (`simplify = TRUE`) the output is a numeric vector of
#'   converted values. If `simplify = FALSE` output is a `list` object of class
#'   `convert_DO` containing five elements: `$call` the function call, `$input`
#'   values, `$output` converted values, `$input.unit` and `$output.unit`.
#'
#'   ## More
#'
#'   For additional help, documentation, vignettes, and more visit the `respR`
#'   website at <https://januarharianto.github.io/respR/>
#'
#' @param x numeric. The dissolved oxygen (DO) value(s) to be converted.
#' @param from string. The DO unit to convert *from*. See [unit_args()] for
#'   details.
#' @param to string. The DO unit to convert *to*. See [unit_args()] for details.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Required for conversion
#'   of some units. See [unit_args()] for details.
#' @param t numeric. Temperature(°C). Defaults to NULL. Required for conversion
#'   of some units. See [unit_args()] for details.
#' @param P numeric. Pressure (bar). Defaults to 1.013253. Required for
#'   conversion of some units. See [unit_args()] for details.
#' @param simplify logical. Defaults to `TRUE` in which case the converted
#'   values are returned as a numeric vector. if `FALSE` a list object of class
#'   `convert_DO` is returned.
#'
#' @importFrom marelac molvol molweight gas_satconc sw_dens vapor atmComp
#' @export
#'
#' @examples
#' # Convert a numeric value to units which do not require t, S and P
#' convert_DO(8.21, from = "mg/L", to = "umol/L")
#'
#' # Convert a numeric value to units which require t, S and P
#' convert_DO(100, from = "%Air", to = "mg L-1", S = 33, t = 18)
#' convert_DO(214, from = "hPa", to = "mL/kg", S = 33, t = 18)
#'
#' # Convert a vector of values
#' convert_DO(urchins.rd[[5]], from = "mg/L", to = "umol/L")
#' convert_DO(c(8.01, 8.03, 8.05), from = "mg per litre", to = "%Air",
#'   t = 15, S = 35)
#' convert_DO(sardine.rd[[2]], from = "%Air", to = "torr",
#'   t = 15, S = 35)

convert_DO <- function(x, from = NULL, to = NULL, S = NULL, t = NULL,
                       P = NULL, simplify = TRUE) {

  ## Save function call for output
  call <- match.call()

  # Verify the units:
  fru <- units.val(from, 'o2', msg = "convert_DO")
  tou <- units.val(to, 'o2', msg = "convert_DO")

  # Check t, S and P needed for units, issue errors
  # and apply default P
  # This will apply it if either in or out unit requires it.
  P <- StP.val(fru, "oxy", S, t, P, P.chk = FALSE, msg = "convert_DO")
  P <- StP.val(tou, "oxy", S, t, P, P.chk = TRUE, msg = "convert_DO")
  # Should be either single value or same length as rate
  # S, t, P vector inputs should be same length as rate or single value
  if (length(S) > 1 && length(S) != length(x))
    stop("convert_DO: The 'S' input must be a single value or the same length as the rates to be converted.")
  if (length(t) > 1 && length(t) != length(x))
    stop("convert_DO: The 't' input must be a single value or the same length as the rates to be converted.")
  if (length(P) > 1 && length(P) != length(x))
    stop("convert_DO: The 'P' input must be a single value or the same length as the rates to be converted.")

  # Do either on of input or output units require StP?
  StPreq <- any(StP.check(fru, "oxy"), StP.check(tou, "oxy"))

  # Constants/formula data using data taken from 'marelac' (gsw removed atm).
  # Conversion factors between pressure units are obtained from the udunits2
  # C library: https://www.unidata.ucar.edu/software/udunits/

  if(StPreq) omVl <- unname(marelac::molvol(t, P, species = "O2"))  # moles O2 in 1L vol
  omWt <- unname(marelac::molweight('O2'))  # molecular weight of O2 in g/mol
  if(StPreq) oGas <- unname(marelac::gas_satconc(S, t, P, species = "O2")) # gas sat conc.
  if(StPreq) swDn <- marelac::sw_dens(S = S, t = t, P = P) # seawater density in kg/m^3
  if(StPreq) vpor <- marelac::vapor(S = S, t = t)  # sat. pressure of water vapour (au)
  oAtm <- unname(marelac::atmComp('O2'))  # atmospheric composition of O2 (%)

  # Validate x input:
  if (!is.numeric(x)) stop("convert_DO: input 'x' must be a numeric value or vector.", call. = FALSE)
  if (is.numeric(x)) z <- x

  # Perform conversions
  # First we convert all values to a standard unit, mg/L:
  if (fru == units.val('mg/L',   'o2', msg = "convert_DO")) {c <-  z}
  if (fru == units.val('ug/L',   'o2', msg = "convert_DO")) {c <-  z / 1e3}
  if (fru == units.val('mol/L',  'o2', msg = "convert_DO")) {c <-  z * omWt * 1e3}
  if (fru == units.val('mmol/L', 'o2', msg = "convert_DO")) {c <-  z * omWt}
  if (fru == units.val('umol/L', 'o2', msg = "convert_DO")) {c <-  z * omWt / 1e3}
  if (fru == units.val('nmol/L', 'o2', msg = "convert_DO")) {c <-  z * omWt / 1e6}
  if (fru == units.val('pmol/L', 'o2', msg = "convert_DO")) {c <-  z * omWt / 1e9}
  if (fru == units.val('mL/L',   'o2', msg = "convert_DO")) {c <-  z * omWt / omVl}
  if (fru == units.val('uL/L',   'o2', msg = "convert_DO")) {c <-  z * omWt / omVl / 1e3}
  if (fru == units.val('cm3/L',  'o2', msg = "convert_DO")) {c <-  z * omWt / omVl}
  if (fru == units.val('mm3/L',  'o2', msg = "convert_DO")) {c <-  z * omWt / omVl / 1e3}
  if (fru == units.val('mg/kg',  'o2', msg = "convert_DO")) {c <-  z * swDn / 1e3}
  if (fru == units.val('ug/kg',  'o2', msg = "convert_DO")) {c <-  z * swDn / 1e6}
  if (fru == units.val('mol/kg', 'o2', msg = "convert_DO")) {c <-  z * swDn * omWt}
  if (fru == units.val('mmol/kg','o2', msg = "convert_DO")) {c <-  z * swDn * omWt / 1e3}
  if (fru == units.val('umol/kg','o2', msg = "convert_DO")) {c <-  z * swDn * omWt / 1e6}
  if (fru == units.val('nmol/kg','o2', msg = "convert_DO")) {c <-  z * swDn * omWt / 1e9}
  if (fru == units.val('pmol/kg','o2', msg = "convert_DO")) {c <-  z * swDn * omWt / 1e12}
  if (fru == units.val('%Air',   'o2', msg = "convert_DO")) {c <-  z * oGas * omWt / 1e3 / 100}
  if (fru == units.val('%Oxy',   'o2', msg = "convert_DO")) {c <-  z * oGas * omWt / oAtm / 1e3 / 100}
  if (fru == units.val('mL/kg',  'o2', msg = "convert_DO")) {c <-  z * omWt / omVl * swDn / 1e3}
  if (fru == units.val('uL/kg',  'o2', msg = "convert_DO")) {c <-  z * omWt / omVl * swDn / 1e6}
  if (fru == units.val('cm3/kg', 'o2', msg = "convert_DO")) {c <-  z * omWt / omVl * swDn / 1e3}
  if (fru == units.val('mm3/kg', 'o2', msg = "convert_DO")) {c <-  z * omWt / omVl * swDn / 1e6}
  if (fru == units.val('Torr',   'o2', msg = "convert_DO")) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 760.000066005}
  if (fru == units.val('hPa',    'o2', msg = "convert_DO")) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 1013.235}
  if (fru == units.val('kPa',    'o2', msg = "convert_DO")) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 101.3235}
  if (fru == units.val('mmHg',   'o2', msg = "convert_DO")) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 759.999951996}
  if (fru == units.val('inHg',   'o2', msg = "convert_DO")) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 29.9212583001}

  # Then we convert mg/L to the final desired unit:
  if(tou == units.val('mg/L',   'o2', msg = "convert_DO")) {out <- c}
  if(tou == units.val('ug/L',   'o2', msg = "convert_DO")) {out <- c * 1e3}
  if(tou == units.val('mol/L',  'o2', msg = "convert_DO")) {out <- c / omWt / 1e3}
  if(tou == units.val('mmol/L', 'o2', msg = "convert_DO")) {out <- c / omWt}
  if(tou == units.val('umol/L', 'o2', msg = "convert_DO")) {out <- c / omWt * 1e3}
  if(tou == units.val('nmol/L', 'o2', msg = "convert_DO")) {out <- c / omWt * 1e6}
  if(tou == units.val('pmol/L', 'o2', msg = "convert_DO")) {out <- c / omWt * 1e9}
  if(tou == units.val('mL/L',   'o2', msg = "convert_DO")) {out <- c / omWt * omVl}
  if(tou == units.val('uL/L',   'o2', msg = "convert_DO")) {out <- c / omWt * omVl * 1e3}
  if(tou == units.val('cm3/L',  'o2', msg = "convert_DO")) {out <- c / omWt * omVl}
  if(tou == units.val('mm3/L',  'o2', msg = "convert_DO")) {out <- c / omWt * omVl * 1e3}
  if(tou == units.val('mg/kg',  'o2', msg = "convert_DO")) {out <- c / swDn * 1e3}
  if(tou == units.val('ug/kg',  'o2', msg = "convert_DO")) {out <- c / swDn * 1e6}
  if(tou == units.val('mol/kg', 'o2', msg = "convert_DO")) {out <- c / omWt / swDn}
  if(tou == units.val('mmol/kg','o2', msg = "convert_DO")) {out <- c / omWt / swDn * 1e3}
  if(tou == units.val('umol/kg','o2', msg = "convert_DO")) {out <- c / omWt / swDn * 1e6}
  if(tou == units.val('nmol/kg','o2', msg = "convert_DO")) {out <- c / omWt / swDn * 1e9}
  if(tou == units.val('pmol/kg','o2', msg = "convert_DO")) {out <- c / omWt / swDn * 1e12}
  if(tou == units.val('%Air',   'o2', msg = "convert_DO")) {out <- c / omWt / oGas * 1e3 * 100}
  if(tou == units.val('%Oxy',   'o2', msg = "convert_DO")) {out <- c / omWt / oGas * oAtm * 1e3 * 100}
  if(tou == units.val('mL/kg',  'o2', msg = "convert_DO")) {out <- c / swDn * omVl / omWt * 1e3}
  if(tou == units.val('uL/kg',  'o2', msg = "convert_DO")) {out <- c / swDn * omVl / omWt * 1e6}
  if(tou == units.val('cm3/kg', 'o2', msg = "convert_DO")) {out <- c / swDn * omVl / omWt * 1e3}
  if(tou == units.val('mm3/kg', 'o2', msg = "convert_DO")) {out <- c / swDn * omVl / omWt * 1e6}
  if(tou == units.val('Torr',   'o2', msg = "convert_DO")) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 760.000066005}
  if(tou == units.val('hPa',    'o2', msg = "convert_DO")) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 1013.253}
  if(tou == units.val('kPa',    'o2', msg = "convert_DO")) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 101.3253}
  if(tou == units.val('mmHg',   'o2', msg = "convert_DO")) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 759.999951996}
  if(tou == units.val('inHg',   'o2', msg = "convert_DO")) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 29.9212583001}

  # Clean units for output
  fru_clean <- units.clean(fru, 'o2')
  tou_clean <- units.clean(tou, 'o2')

  # Generate output
  out <- list(call = call,
              input = z,
              output = out,
              input.unit = fru_clean,
              output.unit = tou_clean)

  class(out) <- "convert_DO"

  if(simplify) return(out$output) else
    return(out)
}

#' Print convert_DO objects
#' @param x convert_DO object
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
print.convert_DO <- function(x, ...) {

  cat("\n# print.convert_DO # --------------------\n")
  if(length(x$input) >= 20) cat("Showing only the first 20 conversions:\n")

  cat("\nInput values:\n")
  if(length(x$input) >= 20) {
    print(head(x$input, 20))
  } else print(x$input)

  cat("Output values:\n")
  if(length(x$output) >= 20) {
    print(head(x$output, 20))
  } else print(x$output)
  cat("\nInput unit: ", x$input.unit)
  cat("\nOutput unit:", x$output.unit)
  cat("\n")
  cat("-----------------------------------------\n")
}

#' Summarise convert_DO objects
#' @param object convert_DO object
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
summary.convert_DO <- function(object, ...) {
  print(object)
}

#' Plot convert_DO objects
#' @param x convert_DO object
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.convert_DO <- function(x, ...) {
  message("convert_DO: plot is not available for 'convert_DO' objects.")
  return(invisible(x))
}

#' Average convert_DO object values
#' @param x convert_DO object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
mean.convert_DO <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.convert_DO # ---------------------\n")
  if(!is.null(pos) && any(pos > length(x$output)))
    stop("mean.convert_DO: Invalid 'pos' rank: only ", length(x$output), " rates found.", call. = FALSE)
  if(is.null(pos)) {
    pos <- 1:length(x$output)
    cat("Averaging all converted oxygen values.")
    cat("\n")
  } else{
    cat("Averaging converted oxygen values from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$output[pos]) == 1)
    message("Only 1 converted oxygen value found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$output[pos])
  out <- mean(x$output[pos])
  cat("Mean of", n, "converted oxygen values:\n")
  print(out)
  print(x$output.unit)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}




