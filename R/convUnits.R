#' @title Convert units or oxygen concentration or respiration
#' @description This function is able to pick up whether the data that needs to
#' be converted is a unit of [O2] or a unit of VO2 or MO2.
#' @param x Numeric vector of measurement(s).
#' @param from String. The unit to convert from.
#' @param to String. The unit to convert into.
#' @param S Numeric. Salinity, default at S = 35.
#' @param t Numeric. Temperature, default at 25 &deg;C.
#' @param P Numeric. Pressure. Default at 1.013253 bar.
#' @details While you could parse an entire raw dataset with this, it is highly
#' recommended that unit conversion is conducted at the end of analysis (i.e.
#' convert one value once, rather than convert an entire data frame at start),
#' @return NULL
#' @export
#' @examples
#' convUnits(df, from = '%', to = 'mg/l')
#' convUnits(df, from = 'mgl-1', to = 'umol/l', S = 30, t = 20)
convUnits <- function(x, from, to, S = 35, t = 25, P = 1.013253) {
  # ----------------------------------------------------------------------------
  # Constants/formula data using data taken from 'marelac' and 'gsw'.
  # Conversion factors between pressure units are obtained from the udunits2
  # C library: https://www.unidata.ucar.edu/software/udunits/
  omVl <- unname(marelac::molvol(t, P, species = "O2"))  # moles O2 in 1L gas
  omWt <- unname(marelac::molweight('O2'))  # molecular weight of O2
  oGas <- unname(marelac::gas_satconc(S, t, P, species = "O2")) # gas sat conc.
  swDn <- gsw::gsw_rho_t_exact(S, t, (P * 10))  # seawater density
  vpor <- marelac::vapor(S = S, t = t)  # sat. pressure of water vapour
  oAtm <- unname(marelac::atmComp('O2'))  # atmospheric composition of O2
  # ----------------------------------------------------------------------------
  f <- checkUnit(from, 2)[2]
  # Before conversion, standardise all units to mg/L
  # Step-by-step, verbose conversions (for easier debugging later):
  if(f == checkUnit('mg/L',    2)[2]) {c <-  x}
  if(f == checkUnit('ug/L',    2)[2]) {c <-  x / 1e3}
  if(f == checkUnit('mmol/L',  2)[2]) {c <-  x * omWt}
  if(f == checkUnit('umol/L',  2)[2]) {c <-  x * omWt / 1e3}
  if(f == checkUnit('mL/L',    2)[2]) {c <-  x * omWt / omVl}
  if(f == checkUnit('mg/kg',   2)[2]) {c <-  x * swDn / 1e3}
  if(f == checkUnit('ug/kg',   2)[2]) {c <-  x * swDn / 1e6}
  if(f == checkUnit('mmol/kg', 2)[2]) {c <-  x * swDn * omWt / 1e3}
  if(f == checkUnit('umol/kg', 2)[2]) {c <-  x * swDn * omWt / 1e6}
  if(f == checkUnit('%',       2)[2]) {c <-  x * oGas * omWt / 1e3 / 100}
  if(f == checkUnit('mL/kg',   2)[2]) {c <-  x * omWt / omVl * swDn / 1e3}
  if(f == checkUnit('Torr',    2)[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 760.000066005}
  if(f == checkUnit('hPa',     2)[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 1013.235}
  if(f == checkUnit('kPa',     2)[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 101.3235}
  if(f == checkUnit('mmHg',    2)[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 759.999951996}
  if(f == checkUnit('inHg',    2)[2]) {c <-  x / (P - vpor) / oAtm * oGas * omWt / 1e3 / 29.9212583001}

  t <- checkUnit(to, 2)[2]
  # Step-by-step, verbose conversions (for easier debugging later):
  if(t == checkUnit('mg/L',    2)[2]) {out <- c}
  if(t == checkUnit('ug/L',    2)[2]) {out <- c * 1e3}
  if(t == checkUnit('mmol/L',  2)[2]) {out <- c / omWt}
  if(t == checkUnit('umol/L',  2)[2]) {out <- c / omWt * 1e3}
  if(t == checkUnit('mL/L',    2)[2]) {out <- c / omWt * omVl}
  if(t == checkUnit('mg/kg',   2)[2]) {out <- c / swDn * 1e3}
  if(t == checkUnit('ug/kg',   2)[2]) {out <- c / swDn * 1e6}
  if(t == checkUnit('mmol/kg', 2)[2]) {out <- c / omWt / swDn * 1e3}
  if(t == checkUnit('umol/kg', 2)[2]) {out <- c / omWt / swDn * 1e6}
  if(t == checkUnit('%',       2)[2]) {out <- c / omWt / oGas * 1e3 * 100}
  if(t == checkUnit('mL/kg',   2)[2]) {out <- c / swDn * omVl / omWt * 1e3}
  if(t == checkUnit('Torr',    2)[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 760.000066005}
  if(t == checkUnit('hPa',     2)[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 1013.253}
  if(t == checkUnit('kPa',     2)[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 101.3253}
  if(t == checkUnit('mmHg',    2)[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 759.999951996}
  if(t == checkUnit('inHg',    2)[2]) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 29.9212583001}

  return(out)
}
