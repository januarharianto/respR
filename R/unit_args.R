#' Print examples of unit arguments for use in [convert_DO()] and
#' [convert_rate()]
#'
#' This is a simple function, requiring no argument. It shows available units
#' that can be used in the functions [convert_DO()] and [convert_rate()]. Some
#' units also require temperature (`t`), salinity (`S`), and atmospheric
#' pressure (`P`) to be specified.
#'
#' **Available units for [convert_DO()]:**
#'
#' Do *NOT* require t, S and P:
#'
#' "mg/L", "ug/L", "mmol/L", "umol/L"
#'
#' Require t, S and P:
#'
#' "mL/L", "mg/kg", "ug/kg", "mmol/kg",
#' "umol/kg", "mL/kg", "\%" (N.B. = \% Air Saturation), "Torr", "hPa", "kPa",
#' "mmHg", "inHg"
#'
#' **Available units for [convert_rate()]:**
#'
#' **O2**
#'
#' Do *NOT* require t, S and P:
#'
#' "mg/L", "ug/L", "mmol/L", "umol/L"
#'
#' Require t, S and P:
#'
#' "mL/L", "mg/kg", "ug/kg", "mmol/kg",
#' "umol/kg", "mL/kg", "\%" (N.B. = \% Air Saturation), "Torr", "hPa", "kPa",
#' "mmHg", "inHg"
#'
#' **Time**
#'
#' "s", "m", "h"
#'
#' **Output mass**
#'
#' "ug", "mg", "g", "kg"
#'
#' @return A list.
#' @export
#'
#' @examples
#' unit_args()
#'
unit_args <- function() {
  cat("Note: A string-matching algorithm is used to identify units. \nE.g. all of these are the same: ")
  cat("mg/L; mg/l, mg L-1, mgL-1, mg per litre, mg.l-1, mg.L-1\n\n")

  o2unit <- c("mg/L", "ug/L", "mmol/L", "umol/L")

  o2unit_tsp <- c("mL/L", "mg/kg",
                  "ug/kg", "mmol/kg", "umol/kg", "mL/kg", "%", "Torr",
                  "hPa", "kPa", "mmHg", "inHg")

  timeunit <- c("s", "m", "h")
  massunit <- c("ug", "mg", "g", "kg")
  areaunit <- c("mm^2", "cm^2", "m^2", "km^2")

  cat("O2 Units - Do not require t, S and P\n")
  print(o2unit)
  cat("\nO2 Units - Require t, S and P\n")
  print(o2unit_tsp)
  cat("\nTime units\n")
  print(timeunit)
  cat("\nOutput mass-specific units\n")
  print(massunit)
  cat("\nOutput surface area-specific units\n")
  print(areaunit)
}
