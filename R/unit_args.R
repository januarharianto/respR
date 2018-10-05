#' Print examples of unit arguments for use in [convert_DO()] and
#' [convert_rate()]
#'
#' This is a simple function, requiring no argument. It shows available units
#' that can be used in the functions [convert_DO()] and [convert_rate()].
#'
#' Available units for [convert_DO()]:
#'
#' "mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg", "ug/kg", "mmol/kg",
#' "umol/kg", "mL/kg", "\% Air Saturation", "Torr", "hPa", "kPa", "mmHg", "inHg"
#'
#' Available units for [convert_rate()]:
#'
#' **O2**
#'
#' "mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg", "ug/kg", "mmol/kg",
#' "umol/kg", "mL/kg", "\% Air Saturation", "Torr", "hPa", "kPa", "mmHg", "inHg"
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
  cat("Note: A string-matching algorithm is used to identify units. E.g. all of these are the same: ")
  cat("mg/L; mg/l, mg L-1, mgL-1, mg per litre, mg.l-1, mg.L-1\n\n")

  o2unit <- c("mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg",
              "ug/kg", "mmol/kg", "umol/kg", "mL/kg", "% Air Saturation", "Torr",
              "hPa", "kPa", "mmHg", "inHg")

  timeunit <- c("s", "m", "h")
  massunit <- c("ug", "mg", "g", "kg")
  cat("O2 Units\n")
  print(o2unit)
  cat("\nTime units\n")
  print(timeunit)
  cat("\nOutput mass units\n")
  print(massunit)
}
