#' Print examples of unit arguments for use in [convert_DO()] and
#' [convert_rate()]
#'
#' This is a simple function, with no arguments. It shows available units that
#' can be used in the functions [convert_DO()] and [convert_rate()]. Some oxygen
#' unit conversions also require temperature (`t`), salinity (`S`), and
#' atmospheric pressure (`P`) to be specified.
#'
#' **[convert_DO()]**
#'
#' **Oxygen concentration or pressure units for `from` and `to` arguments:**
#'
#' Oxygen concentration units should use SI units (`L` or `kg`) for the
#' denominator.
#'
#' Do *NOT* require `t`, `S` and `P` for conversions:
#'
#' `"mg/L", "ug/L", "mmol/L", "umol/L"`
#'
#' Require `t`, `S` and `P` for conversions:
#'
#' `"mL/L", "mg/kg", "ug/kg", "mmol/kg", "umol/kg", "mL/kg",` `"%" (Note this is
#' % Air Saturation), "Torr", "hPa", "kPa",` `"mmHg", "inHg"`
#'
#' **[convert_rate()]**
#'
#' **Oxygen concentration or pressure units for `o2.unit` argument:**
#'
#' Oxygen concentration units should use SI units (`L` or `kg`) for the
#' denominator.
#'
#' Do *NOT* require `t`, `S` and `P` for conversions:
#'
#' `"mg/L", "ug/L", "mmol/L", "umol/L"`
#'
#' Require `t`, `S` and `P` for conversions:
#'
#' `"mL/L", "mg/kg", "ug/kg", "mmol/kg", "umol/kg", "mL/kg",` `"%" (Note this is
#' % Air Saturation), "Torr", "hPa", "kPa",` `"mmHg", "inHg"`
#'
#' **Time units for `time.unit` argument:**
#'
#' `"sec", "min", "hour", "day"`
#'
#' **Combining units for `output.unit` argument:**
#'
#' Must be in correct order, with no special characters other than the
#' separator:
#'
#' - Absolute rates: `O2/Time` e.g. `"mg/s"`, `"umol/min"`, `"mL/h"`
#'
#' - Mass-specific rates: `O2/Time/Mass` e.g. `"mg/s/ug"`, `"umol/min/g"`,
#' `"mL/h/kg"`
#'
#' - Area-specific rates: `O2/Time/Area` e.g. `"mg/s/mm2"`, `"umol/min/cm2"`,
#' `"mL/h/m2"`
#'
#' **Oxygen amount units for use in `output.unit` argument:**
#'
#' `"ug", "mg", "umol", "mmol", "ml"`
#'
#' **Time units for use in `output.unit` argument:**
#'
#' `"sec", "min", "hour", "day"`
#'
#' **Mass units for use in `output.unit` argument in mass-specific rates:**
#'
#' `"ug", "mg", "g", "kg"`
#'
#' **Area units for use in `output.unit` argument in area-specific rates:**
#'
#' `"mm2", "cm2", "m2", "km2"`
#'
#' @export
#'
#' @examples
#' unit_args()
#'
unit_args <- function() {

  o2unit <- c("mg/L", "ug/L", "mmol/L", "umol/L")
  o2unit_tsp <- c("mL/L", "mg/kg",
                  "ug/kg", "mmol/kg", "umol/kg", "mL/kg", "%", "Torr",
                  "hPa", "kPa", "mmHg", "inHg")
  o2unit_out <- c("ug", "mg", "umol", "mmol", "ml")
  timeunit <- c("sec", "min", "hour", "day")
  massunit <- c("ug", "mg", "g", "kg")
  areaunit <- c("mm2", "cm2", "m2", "km2")

  cat("Note: A string-matching algorithm is used to identify units. \n")
  cat("Example 1: These are recognised as the same: 'mg/L', 'mg/l', 'mg L-1', 'mg per litre', 'mg.L-1'\n")
  cat("Example 2: These are recognised as the same: 'hour', 'hr', 'h'\n")

  cat("\n# Input Units # --------------------------------------\n")
  cat("Oxygen concentration units should use SI units (`L` or `kg`) for the denominator.\n\n")
  cat("Oxygen Concentration or Pressure Units - Do not require t, S and P\n")
  print(o2unit)
  cat("Oxygen Concentration or Pressure Units - Require t, S and P\n")
  print(o2unit_tsp)
  cat("\nTime units\n")
  print(timeunit)
  cat("\nMass units\n")
  print(massunit)
  cat("\nArea units\n")
  print(areaunit)

  cat("\n# Output Units in 'convert_rate' # -------------------\n")
  cat("Must be in correct order, with no special characters other than the separator:\n\n")
  cat("Absolute rates:        O2/Time       e.g. 'mg/sec',     'umol/min',     'mL/h'\n")
  cat("Mass-specific rates:   O2/Time/Mass  e.g. 'mg/sec/ug',  'umol/min/g',   'mL/h/kg'\n")
  cat("Area-specific rates:   O2/Time/Area  e.g. 'mg/sec/mm2', 'umol/min/cm2', 'mL/h/m2'\n")
  cat("\nOutput Oxygen amount units\n")
  print(o2unit_out)
  cat("\nOutput Time units\n")
  print(timeunit)
  cat("\nOutput Mass units for mass-specific rates\n")
  print(massunit)
  cat("\nOutput Area units for surface area-specific rates\n")
  print(areaunit)
}
