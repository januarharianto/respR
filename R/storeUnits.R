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
  # prompt user for input
  if(missing(Time)) {
  Time <- as.character(readline("Unit of time: "))
  if(checkUnit(Time, 1)[1] == F) {
    message("Time unit not valid, please re-enter.")
    Time <- as.character(readline("Unit of time: "))
    }
  }
  if(missing(O2)) {
    O2 <- as.character(readline("Unit of O2 conc: "))
    if(checkUnit(O2, 2)[1] == F) {
      message("O2 unit not valid, please re-enter.")
      runit <- as.character(readline("Unit of O2 conc.: "))
    }
  }
  if(missing(Vol)) {
    Vol <- as.character(readline("Unit of volume :"))
    if(checkUnit(Vol, 3)[1] == F) {
      message("Vol unit not valid, please re-enter.")
      runit <- as.character(readline("Unit of volume:"))
    }
  }
  if(missing(Mass)) {
    Mass <- as.character(readline("Unit of mass :"))
    if(checkUnit(Mass, 4)[1] == F) {
      message("Mass unit not valid, please re-enter.")
      runit <- as.character(readline("Unit of mass:"))
    }
  }
  # rmass <- as.character(readline("Unit of specimen mass (if used) "))
  # Summary
  Time <- checkUnit(Time, 1)[2]
  O2   <- checkUnit(O2, 2)[2]
  Vol  <- checkUnit(Vol, 3)[2]
  Mass <- checkUnit(Mass, 4)[2]
  .runits <<- list(time = Time, o2 = O2, vol = Vol, mass = Mass)
  out <- matrix(c(Time, O2, Vol, Mass), ncol = 4)
  colnames(out) <- c('Time', 'O2', 'Vol', 'Mass')
  rownames(out) <- 'Unit'
  # unames <- c('Time', 'O2', 'Vol', 'Mass')
  # units  <- c(Time, O2, Vol, Mass)
  # out <- data.frame(unames, units)
  message("Unit of measurements saved and will be used for the session.")
  return(out)
}

