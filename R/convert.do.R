#' @title Convert units or oxygen concentration or respiration
#' @description This function is able to pick up whether the data that needs to
#' be converted is a unit of [O2] or a unit of VO2 or MO2 (NOT YET IMPLEMENTED).
#' @param x Numeric vector of measurement(s).
#' @param from String. The unit to convert from.
#' @param to String. The unit to convert into.
#' @param S Numeric. Salinity, default at S = 35.
#' @param t Numeric. Temperature, default at 25 &deg;C.
#' @param P Numeric. Pressure. Default at 1.013253 bar.
#' @details While you could parse an entire raw dataset with this, it is highly
#' recommended that unit conversion is conducted at the end of analysis (i.e.
#' convert one value once, rather than convert an entire data frame at start),
#' OR MAYBE NOT -- just filler text for now.
#' @author Januar Harianto
#' @author Nicholas Carey
#' @return NULL
#' @export
#' @examples
#' convert.do(90, from = '%', to = 'mg/l')
#' convert.do(7.7, from = 'mgl-1', to = 'umol/l', S = 30, t = 20)
#'
#' # convert a vector, if necessary
#' convert.do(sardine[2], from = '%', to = 'mg/l')
#'
convert.do <- function(x, from, to, S = 35, t = 25, P = 1.013253) {
  # ----------------------------------------------------------------------------
  # Constants/formula data using data taken from 'marelac' (gsw removed atm).
  # Conversion factors between pressure units are obtained from the udunits2
  # C library: https://www.unidata.ucar.edu/software/udunits/
  omVl <- unname(marelac::molvol(t, P, species = "O2"))  # moles O2 in 1L vol
  omWt <- unname(marelac::molweight('O2'))  # molecular weight of O2 in g/mol
  oGas <- unname(marelac::gas_satconc(S, t, P, species = "O2")) # gas sat conc.
  swDn <- marelac::sw_dens(S = S, t = t, P = P) # seawater density in kg/m^3
  #swDn <- gsw::gsw_rho_t_exact(S, t, (P * 10))  # seawater density in kg/m^3
  vpor <- marelac::vapor(S = S, t = t)  # sat. pressure of water vapour (au)
  oAtm <- unname(marelac::atmComp('O2'))  # atmospheric composition of O2 (%)
  # ----------------------------------------------------------------------------
  f <- checkUnits(from, 'o2')[2]
  # Before conversion, standardise all units to mg/L
  # Step-by-step, verbose conversions (for easier debugging later):
  if(f == checkUnits('mg/L',   'o2')[2]) {c <-  x}
  if(f == checkUnits('ug/L',   'o2')[2]) {c <-  x / 1e3}
  if(f == checkUnits('mmol/L', 'o2')[2]) {c <-  x * omWt}
  if(f == checkUnits('umol/L', 'o2')[2]) {c <-  x * omWt / 1e3}
  if(f == checkUnits('mL/L',   'o2')[2]) {c <-  x * omWt / omVl}
  if(f == checkUnits('mg/kg',  'o2')[2]) {c <-  x * swDn / 1e3}
  if(f == checkUnits('ug/kg',  'o2')[2]) {c <-  x * swDn / 1e6}
  if(f == checkUnits('mmol/kg','o2')[2]) {c <-  x * swDn * omWt / 1e3}
  if(f == checkUnits('umol/kg','o2')[2]) {c <-  x * swDn * omWt / 1e6}
  if(f == checkUnits('%',      'o2')[2]) {c <-  x * oGas * omWt / 1e3 / 100}
  if(f == checkUnits('mL/kg',  'o2')[2]) {c <-  x * omWt / omVl * swDn / 1e3}
  if(f == checkUnits('Torr',   'o2')[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 760.000066005}
  if(f == checkUnits('hPa',    'o2')[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 1013.235}
  if(f == checkUnits('kPa',    'o2')[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 101.3235}
  if(f == checkUnits('mmHg',   'o2')[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 759.999951996}
  if(f == checkUnits('inHg',   'o2')[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 29.9212583001}

  t <- checkUnits(to,'o2')[2]
  # Step-by-step, verbose conversions (for easier debugging later):
  if(t == checkUnits('mg/L',   'o2')[2]) {out <- c}
  if(t == checkUnits('ug/L',   'o2')[2]) {out <- c * 1e3}
  if(t == checkUnits('mmol/L', 'o2')[2]) {out <- c / omWt}
  if(t == checkUnits('umol/L', 'o2')[2]) {out <- c / omWt * 1e3}
  if(t == checkUnits('mL/L',   'o2')[2]) {out <- c / omWt * omVl}
  if(t == checkUnits('mg/kg',  'o2')[2]) {out <- c / swDn * 1e3}
  if(t == checkUnits('ug/kg',  'o2')[2]) {out <- c / swDn * 1e6}
  if(t == checkUnits('mmol/kg','o2')[2]) {out <- c / omWt / swDn * 1e3}
  if(t == checkUnits('umol/kg','o2')[2]) {out <- c / omWt / swDn * 1e6}
  if(t == checkUnits('%',      'o2')[2]) {out <- c / omWt / oGas * 1e3 * 100}
  if(t == checkUnits('mL/kg',  'o2')[2]) {out <- c / swDn * omVl / omWt * 1e3}
  if(t == checkUnits('Torr',   'o2')[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 760.000066005}
  if(t == checkUnits('hPa',    'o2')[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 1013.253}
  if(t == checkUnits('kPa',    'o2')[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 101.3253}
  if(t == checkUnits('mmHg',   'o2')[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 759.999951996}
  if(t == checkUnits('inHg',   'o2')[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 29.9212583001}

  return(out)
}
