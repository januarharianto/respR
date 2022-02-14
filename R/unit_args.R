#' Print examples of unit inputs for use in convert_DO, convert_rate, and
#' convert_rate.ft
#'
#' This is a basic function with no inputs. It prints to the console the units
#' that can be used in the functions [convert_DO()], [convert_rate()], and
#' [convert_rate.ft()].
#'
#' Note that some oxygen unit conversions require temperature (`t`), salinity
#' (`S`), and atmospheric pressure (`P`) to be specified.
#'
#' Note the difference between percent air saturation (`%Air`), where air
#' saturated water is ~100%, and percent oxygen saturation (`%Oxy`), where air
#' saturated water is ~20.946% *oxygen* saturated. In other words, `%Oxy = %Air
#' x 0.20946`.
#'
#' For most units a fuzzy string matching algorithm is used to accept different
#' formatting styles. For example, `"mg/l"`, `"mg/L"`, `"mgL-1"`, `"mg l-1"`,
#' `"mg.l-1"` are all parsed the same.
#'
#' # [convert_DO()]
#'
#' ## Oxygen concentration or pressure units for `from` and `to`s:
#'
#' Oxygen concentration units should use SI units (`L` or `kg`) for the
#' denominator.
#'
#' Do *NOT* require `t`, `S` and `P` for conversions:
#'
#' - `"mg/L", "ug/L", "mol/L", "mmol/L", "umol/L"`
#'
#' Require `t`, `S` and `P` for conversions:
#'
#' - `"mL/L", "cm3/L", "mg/kg", "ug/kg", "mol/kg", "mmol/kg", "umol/kg",
#' "mL/kg", "%Air"` (i.e. % Air Saturation), `"%Oxy"` (i.e. % Oxygen
#' Saturation), `"Torr", "hPa", "kPa",` `"mmHg", "inHg"`
#'
#' # [convert_rate()] and [convert_rate.ft()]
#'
#' ## Oxygen concentration or pressure units for `oxy.unit`:
#'
#' Oxygen concentration units should use SI units (`L` or `kg`) for the
#' denominator.
#'
#' Do *NOT* require `t`, `S` and `P` for conversions:
#'
#' - `"mg/L", "ug/L", "mmol/L", "umol/L"`
#'
#' Require `t`, `S` and `P` for conversions:
#'
#' - `"mL/L", "cm3/L", "mg/kg", "ug/kg", "mmol/kg", "umol/kg", "mL/kg",`
#' `"%Air"` (i.e. % Air Saturation), `"%Oxy"` (i.e. % Oxygen Saturation),
#' `"Torr", "hPa", "kPa",` `"mmHg", "inHg"`
#'
#' ## Time units for `time.unit` or as part of `flowrate.unit`:
#'
#' - `"sec", "min", "hour", "day"`
#'
#' ## Volume units for use as part of `flowrate.unit` (`convert_rate.ft` only):
#'
#' For example, in `'ml/min'`, `'L/s'`, etc.
#'
#' - `"uL", "mL", "L"`
#'
#' ## Combining units for `output.unit`:
#'
#' Must be in correct order, with no special characters other than the
#' separator:
#'
#' - Absolute rates: `Oxygen/Time` e.g. `"mg/s"`, `"umol/min"`, `"mL/h"`
#'
#' - Mass-specific rates: `Oxygen/Time/Mass` e.g. `"mg/s/ug"`, `"umol/min/g"`,
#' `"mL/h/kg"`
#'
#' - Area-specific rates: `Oxygen/Time/Area` e.g. `"mg/s/mm2"`,
#' `"umol/min/cm2"`, `"mL/h/m2"`
#'
#' **Oxygen amount units for use in `output.unit`:**
#'
#' - `"ug", "mg", "umol", "mmol", "mol", "mL"`
#'
#' **Time units for use in `output.unit`:**
#'
#' - `"sec", "min", "hour", "day"`
#'
#' **Mass units for use in `output.unit` in mass-specific rates:**
#'
#' - `"ug", "mg", "g", "kg"`
#'
#' **Area units for use in `output.unit` in area-specific rates:**
#'
#' - `"mm2", "cm2", "m2", "km2"`
#'
#' @export
#'
#' @examples
#' unit_args()

unit_args <- function() {

  oxyunit <- c("mg/L", "ug/L", "mol/L", "mmol/L", "umol/L")
  oxyunit_tsp <- c("mL/L", "cm3/L",
                   "mg/kg", "ug/kg",
                   "mol/kg", "mmol/kg", "umol/kg",
                   "mL/kg",
                   "%Air", "%Oxy",
                   "Torr", "hPa", "kPa", "mmHg", "inHg")
  oxyunit_out <- c("ug", "mg", "umol", "mmol", "mol", "mL")
  timeunit <- c("sec", "min", "hour", "day")
  massunit <- c("ug", "mg", "g", "kg")
  areaunit <- c("mm2", "cm2", "m2", "km2")
  flowunit <- c("uL", "mL", "L")

  cat("Note: A string-matching algorithm is used to identify units. \n")
  cat("Example 1: These are recognised as the same: 'mg/L', 'mg/l', 'mg L-1', 'mg per litre', 'mg.L-1'\n")
  cat("Example 2: These are recognised as the same: 'hour', 'hr', 'h'\n")

  cat("\n# Input Units # --------------------------------------\n")
  cat("Oxygen concentration units should use SI units (`L` or `kg`) for the denominator.\n\n")
  cat("Oxygen Concentration or Pressure Units - Do not require t, S and P\n")
  print(oxyunit)
  cat("Oxygen Concentration or Pressure Units - Require t, S and P\n")
  print(oxyunit_tsp)
  cat("\nVolume units for use in flow rates in calc_rate.ft and convert_rate.ft")
  cat("\n(e.g. as in 'ml/min', 'L/s', etc.)\n")
  print(flowunit)
  cat("\nTime units (for 'time.unit' or as part of 'flowrate.unit')\n")
  print(timeunit)
  cat("\nMass units\n")
  print(massunit)
  cat("\nArea units\n")
  print(areaunit)

  cat("\n# Output Units in 'convert_rate' & 'convert_rate.ft' #\n")
  cat("Must be in correct order, with no special characters other than the separator:\n\n")
  cat("Absolute rates:        Oxygen/Time       e.g. 'mg/sec',     'umol/min',     'mL/h'\n")
  cat("Mass-specific rates:   Oxygen/Time/Mass  e.g. 'mg/sec/ug',  'umol/min/g',   'mL/h/kg'\n")
  cat("Area-specific rates:   Oxygen/Time/Area  e.g. 'mg/sec/mm2', 'umol/min/cm2', 'mL/h/m2'\n")
  cat("\nOutput Oxygen amount units\n")
  print(oxyunit_out)
  cat("\nOutput Time units\n")
  print(timeunit)
  cat("\nOutput Mass units for mass-specific rates\n")
  print(massunit)
  cat("\nOutput Area units for surface area-specific rates\n")
  print(areaunit)
}
