#' @title Measurement unit check and identification
#' @description Checks input string against a known database and returns a
#' logical output (i.e. TRUE or FALSE). If TRUE, also prints out the identified
#' string. Works for time (e.g min, sec), unit of O2 measurement (e.g. %, mg/L),
#' volume (e.g. mL, L) and weight/mass (e.g. kg).
#' @param unit Character string. A unit of measurement.
#' @param type Numeric. Identifies the type of unit to check. 1: time; 2: O~2~
#' 3: volume; 4: mass.
#' @details This function makes it easier to parse and understand the plethora
#' of units that a user may provide as inputs while using our functions. The
#' user may deploy a 'fuzzy' approach during the input of measurement units
#' (e.g. 'mg/l', 'mgL-1', 'mgl-1' and 'mg per litre' all work).
#' @author Januar Harianto
#' @return NULL
#' @examples
#' checkUnits('min', 'time')
#' checkUnits('notatunit', 'time')
#'
#' checkUnits('hpa', 'o2')
#' checkUnits('notatunit', 'o2')
checkUnits <- function(unit, type) {
  # not sure if worth ID'ing some of these using regex (too many variations)
  # ok it's worth it but I've come too far.... will fix in future version
  # doing it the stupid way:
  # time units
  if (type == 'time') {
    allUnits <- list(
      hour.time = c('hour', 'hr', 'h'),
      min.time  = c('minute', 'min', 'm'),
      sec.time  = c('second', 'sec', 's'))
  }
  # 2-dimensional o2 units, and pressure
  if(type == 'o2') {
    allUnits <- list(
      '%.o2'       = c('%.o2', '%', 'percent', 'percentage', '%o2', '%O2'),
      'ug/L.o2'    = c('ug/L.o2','ug/L', 'ug/l', 'ug / L', 'ug / l', 'ugL-1', 'ugl-1', 'ug L-1', 'ug l -1', 'ug per liter', 'ug per litre'),
      'mmol/L.o2'  = c('mmol/L.o2', 'mmol/L', 'mmol/l', 'mmol / L', 'mmol / l',
                       'mmolL-1,', 'mmoll-1', 'mmol L-1,', 'mmol l-1', 'mmol per liter', 'mmol per litre'),
      'umol/L.o2'  = c('umol/L.o2','umol/L', 'umol/l', 'umolL-1', 'umoll-1',
                       'umol / L', 'umol / l', 'umol L-1', 'umol l-1', 'umol per litre', 'umol per liter'),
      'mL/L.o2'    = c('mL/L.o2', 'ml/L', 'mL/L', 'mL/l', 'ml/l', 'mll-1', 'mLl-1', 'mLL-1', 'mlL-1',
                       'ml / L', 'mL / L', 'mL / l', 'ml / l', 'ml l-1', 'mL l-1', 'mL L-1', 'ml L-1', 'ml per l', 'mL per L', 'ml per L'),
      'mg/L.o2'    = c('mg/L.o2', 'mg/L', 'mg/l', 'mg / l', 'mg / L', 'mgL-1', 'mgl-1', 'mg L-1', 'mg l-1', 'mg per litre', 'mg per liter'),
      'mg/kg.o2'   = c('mg/kg.o2', 'mg/kg', 'mg / kg', 'mgkg-1', 'mg kg-1', 'mg per kg'),
      'ug/kg.o2'   = c('ug/kg.o2', 'ug/kg', 'ugkg-1', 'ug / kg', 'ug kg-1', 'ug per kg'),
      'mL/kg.o2'   = c('mL/kg.o2', 'ml/kg', 'mL/kg', 'mlkg-1', 'mLkg-1', 'ml / kg', 'mL / kg', 'ml kg-1', 'mL kg-1', 'ml per kg'),
      'mmol/kg.o2' = c('mmol/kg.o2', 'mmol/kg', 'mmol/Kg', 'mmolkg-1', 'mmolKg-1',
                       'mmol / kg', 'mmol / Kg', 'mmol kg-1', 'mmol Kg-1', 'mmol per kg', 'mmol per Kg'),
      'umol/kg.o2' = c('umol/kg.o2', 'umol/kg', 'umol/Kg', 'umolkg-1,', 'umolKg-1',
                       'umol / kg', 'umol / Kg', 'umol kg-1,', 'umol Kg-1', 'umol per kg', 'umol per Kg'),
      'Torr.o2p'   = c('Torr.o2p', 'torr', 'TORR', 'Torr', 'Tor', 'tor'),
      'hPa.o2p'    = c('hPa.o2p', 'hPa', 'hpa', 'Hpa', 'HPA', 'HPa', 'hectopascal', 'hpascal'),
      'kPa.o2p'    = c('kPa.o2p', 'kPa', 'kpa', 'Kpa', 'KPA', 'KPa', 'kilopascal', 'kpascal'),
      'mmHg.o2p'   = c('mmHg.o2p', 'mmHg', 'mm Hg', 'mmhg', 'mm hg', 'MMHG', 'MM HG', 'millimeter of mercury', 'mm mercury'),
      'inHg.o2p'   = c('inHg.o2p', 'inHg', 'in Hg', 'inhg', 'in hg', 'INHG', 'IN HG', 'inch of mercury', 'inch mercury'))
  }
  if(type == 'vol') {
    allUnits <- list(
      uL.vol = c('ul.vol', 'ul', 'uL', 'microlitre', 'microliter', 'micro litre', 'micro liter'),
      mL.vol = c('mL.vol', 'ml', 'mL', 'millilitre', 'milli litre', 'milliliter', 'milli liter'),
      L.vol  = c('L.vol', 'l', 'L', 'liter', 'litre', 'Litre', 'Liter'))
  }
  if(type == 'mass') {
    allUnits <- list(
      ug.mass  = c('ug.mass', 'ug', 'UG', 'µg', 'ugram', 'microgram'),
      mg.mass  = c('mg.mass', 'mg', 'MG', 'mgram', 'milligram'),
      g.mass   = c('g.mass', 'g', 'G', 'gram'),
      kg.mass  = c('kg.mass', 'kg', 'KG', 'kilogram', 'kgram'))
  }
  if(type == 'o1') {
    allUnits <-  list(
    'mg.o2'   = c('mg.o2', 'mg', 'milligram'),
    'ug.o2'   = c('ug.o2', 'ug', 'microgram'),
    'mmol.o2' = c('mmol.o2', 'mmol', 'millimol'),
    'umol.o2' = c('umol.o2', 'umol', 'micromol'),
    'ml.o2'   = c('ml.o2', 'ml', 'mL', 'millil'))
  }
  # look for match
  unit <- paste0('^', unit, '$')  # for exact matching
  chk <- lapply(allUnits, function(x) grep(unit, x))
  chk <- sapply(chk, function(x) length(x) > 0)
  result <- any(chk == T)  # did a match occur?
  name <- names(chk)[which(chk)]  # print unit name
  out <- c(result, name)  # print output
  return(out)
}
