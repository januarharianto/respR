#' @title Define units and experimental conditions for the current session
#' @description This function is run first to establish measurement units that
#' will be used for all calculations in subsequent functions.
#' @param Time Character.
#' @param O2 Character.
#' @param Vol Character.
#' @param Mass Character.
#' @return NULL
#' @export
#' @examples
#' storeUnits()
#' storeUnits('min', 'mgl-1', 'l', 'kg')
storeUnits <- function(Time, O2, Vol, Mass) {
  # Prompt if no input
  if(missing(Time)) Time <- .validateInput(1, "Unit of time: ")[2]
  if(missing(O2)) O2 <- .validateInput(2, "Unit of O2 concentration: ")[2]
  if(missing(Vol)) Vol  <- .validateInput(3, "Unit of volume: ")[2]
  if(missing(Mass)) Mass <- .validateInput(4, "Unit of mass: ")[2]

  # Validate input
  if(checkUnits(Time, 1)[1] == F)
    Time <- .validateInput(1, "Time unit invalid. Unit of time: ")[2]
  if(checkUnits(O2, 2)[1] == F)
    O2 <- .validateInput(2, "O2 unit invalid. Unit of O2 concentration: ")[2]
  if(checkUnits(Vol, 3)[1] == F)
    Vol  <- .validateInput(3, "Volume unit invalid. Unit of volume: ")[2]
  if(checkUnits(Mass, 4)[1] == F)
    Mass <- .validateInput(4, "Mass unit invalid. Unit of mass: ")[2]

  Time <- checkUnits(Time, 1)[2]
  O2 <- checkUnits(O2,2)[2]
  Vol <- checkUnits(Vol,3)[2]
  Mass <- checkUnits(Mass,4)[2]

  # # Generate output
  cat("Time unit:   ", Time,"\n")
  cat("O2 unit:     ", O2,"\n")
  cat("Volume unit: ", Vol,"\n")
  cat("Mass unit:   ", Mass,"\n")
  out <- list(time = Time, O2 = O2, volume = Vol, mass = Mass)
  message("Unit of measurements saved and will be used for the session.")
  class(out) <- "storeUnits"
  return(out)
}


# sub-function?
.validateInput <- function(grp, string) {
  x <- as.character(readline(string))
  x <- checkUnits(x, grp)
  if(x[1] == F) {
    x <- .validateInput(grp, string)
  }
  return(x)
}
