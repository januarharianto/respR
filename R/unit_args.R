#' Print examples of unit inputs
#'
#' This is a basic function with no inputs. It prints to the console the units
#' that can be used in the functions [convert_DO()], [convert_MR()],
#' [convert_rate()], and [convert_rate.ft()].
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
#' ## [convert_DO()]
#'
#' ### Oxygen concentration or pressure units for `from` and `to`:
#'
#' Oxygen concentration units. Should use SI units (`L` or `kg`) for the
#' denominator.
#'
#' Do *NOT* require `t`, `S` and `P` for conversions:
#'
#' - `"mg/L"`, `"ug/L"`, `"mol/L"`, `"mmol/L"`, `"umol/L"`, `"nmol/L"`, `"pmol/L"`
#'
#' Require `t`, `S` and `P` for conversions:
#'
#' - `"uL/L"`, `"mL/L"`, `"mm3/L"`, `"cm3/L"`, `"mg/kg"`, `"ug/kg"`, `"mol/kg"`, `"mmol/kg"`, `"umol/kg"`,
#' `"nmol/kg"`, `"pmol/kg"`, `"uL/kg"`, `"mL/kg"`, `"ppm"` (i.e. parts per
#' million, equivalent to `mg/kg`).
#'
#' Percentage saturations (require `t`, `S` and `P`):
#'
#' `"%Air"` (i.e. % Air Saturation), `"%Oxy"` (i.e. % Oxygen Saturation)
#'
#' Pressure units (require `t`, `S` and `P`):
#'
#' `"Torr"`, `"hPa"`, `"kPa"`, `"mmHg"`, `"inHg"`
#'
#'
#' ## [convert_rate()] and [convert_rate.ft()]
#'
#' ### Oxygen concentration or pressure units for `oxy.unit`:
#'
#' See above.
#'
#' ### Time units for `time.unit` or as part of `flowrate.unit`:
#'
#' - `"sec", "min", "hour", "day"`
#'
#' ### Volume units for use as part of `flowrate.unit` (`convert_rate.ft` only):
#'
#' For example, in `'ml/min'`, `'L/s'`, etc.
#'
#' - `"uL"`, `"mL"`, `"L"`
#'
#' ## Metabolic rate units
#'
#' Combining units for `output.unit` in [convert_rate()] and
#' [convert_rate.ft()], or for use in [`convert_MR()`], must follow these
#' orders:
#'
#' - Absolute rates: `Oxygen/Time` e.g. `"mg/s"`, `"umol/min"`, `"mL/h"`
#'
#' - Mass-specific rates: `Oxygen/Time/Mass` e.g. `"mg/s/ug"`, `"umol/min/g"`,
#' `"mL/h/kg"`
#'
#' - Area-specific rates: `Oxygen/Time/Area` e.g. `"mg/s/mm2"`,
#' `"umol/min/cm2"`, `"mL/h/m2"`
#'
#' **Oxygen amount units:**
#'
#' - `"ug"`, `"mg"`, `"pmol"`, `"nmol"`, `"umol"`, `"mmol"`, `"mol"`, `"uL"`, `"mL"`, `"mm3"`, `"cm3"`
#'
#' Note `"mm3"` and `"cm3"` (i.e. `cc`) are used in some older publications.
#' These are equivalent to `"uL"` and `"mL"` respectively.
#'
#' **Time units:**
#'
#' - `"sec"`, `"min"`, `"hour"`, `"day"`
#'
#' **Mass units for mass-specific rates:**
#'
#' - `"ug"`, `"mg"`, `"g"`, `"kg"`
#'
#' **Area units for area-specific rates:**
#'
#' - `"mm2"`, `"cm2"`, `"m2"`, `"km2"`
#'
#' @return A print out to the console of accepted units
#'
#' @export
#'
#' @examples
#'
#' # Run the function:
#' unit_args()

unit_args <- function() {

  oxyunit <- c("mg/L", "ug/L", "mol/L", "mmol/L", "umol/L", "nmol/L", "pmol/L")
  oxyunit_tsp <- c("uL/L", "mL/L",
                   "mm3/L", "cm3/L", "cc/L",
                   "mg/kg", "ug/kg",
                   "ppm",
                   "mol/kg", "mmol/kg", "umol/kg", "nmol/kg", "pmol/kg",
                   "uL/kg", "mL/kg",
                   "%Air", "%Oxy",
                   "Torr", "hPa", "kPa", "mmHg", "inHg")
  oxyunit_out <- c("ug", "mg", "pmol", "nmol", "umol", "mmol", "mol", "uL", "mL", "mm3", "cm3")
  timeunit <- c("sec", "min", "hour", "day")
  massunit <- c("ug", "mg", "g", "kg")
  areaunit <- c("mm2", "cm2", "m2", "km2")
  flowunit <- c("uL", "mL", "L")

  cat("Note: A string-matching algorithm is used to identify units. \n")
  cat("Example 1: These are recognised as the same: 'mg/L', 'mg/l', 'mg L-1', 'mg per litre', 'mg.L-1'\n")
  cat("Example 2: These are recognised as the same: 'Hour', 'hr', 'h'\n")

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

  cat("\n# Metabolic Rate Units # -----------------------------")
  cat("\nFor use in 'convert_rate', 'convert_rate.ft', 'convert_MR'\n")
  cat("\nMust be in correct order:\n")
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
