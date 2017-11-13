#' Print examples of unit arguments for use in [convert.rate()] and
#' [scale.rate()]
#'
#' This is a simple function, requiring no argument. It shows available units
#' that can be used in the functions [convert.rate()] and [scale.rate()].
#'
#' Available units for [convert.rate()]:
#'
#' "mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg", "ug/kg", "mmol/kg",
#' "umol/kg", "mL/kg", "%", "Torr", "hPa", "kPa", "mmHg", "inHg"
#'
#' Available units for [scale.rate()]:
#'
#' **O2**
#'
#' "mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg", "ug/kg", "mmol/kg",
#' "umol/kg", "mL/kg", "Torr", "hPa", "kPa", "mmHg", "inHg"
#'
#' **time**
#'
#' "s", "m", "h"
#'
#' **mass**
#'
#' "ug", "mg", "g", "kg"
#'
#' @return A list.
#' @export
#'
#' @examples
#' unit.args()
#'
unit.args <- function() {
  cat("Note: A string-matchin algorithm is used to identify units. e.g. all of these are the same: ")
  cat("mg/L; mg/l, mg L-1, mgL-1, mg per litre, mg.l-1, mg.L-1\n\n")

  o2unit <- c("mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg",
    "ug/kg", "mmol/kg", "umol/kg", "mL/kg", "%", "Torr",
    "hPa", "kPa", "mmHg", "inHg")

  timeunit <- c("s", "m", "h")
  massunit <- c("ug", "mg", "g", "kg")
  cat("O2 Units\n")
  print(o2unit)
  cat("\nTime units\n")
  print(timeunit)
  cat("\nMass units\n")
  print(massunit)
}
