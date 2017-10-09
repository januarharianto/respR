#' Print examples for unit arguments in `calc.mo2()`
#'
#' @md
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

  o2unit <- c("mg/L", "ug/L", "mmol/L", "umol/L", "mL/L", "mg/kg", "ug/kg", "mmol/kg", "umol/kg", "mL/kg", "%")
  timeunit <- c("s", "m", "h")
  massunit <- c("ug", "mg", "g", "kg")
  cat("O2 Units\n")
  print(o2unit)
  cat("\nTime units\n")
  print(timeunit)
  cat("\nMass units\n")
  print(massunit)
}
