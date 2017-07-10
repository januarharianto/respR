#' @title Measurement unit check and identification
#' @description Checks input string against a known database and returns a
#' logical output (i.e. TRUE or FALSE). If TRUE, also prints out the identified
#' string. Works for time (e.g min, sec), unit of O2 measurement (e.g. %, mg/L),
#' volume (e.g. mL, L) and mass/weight (e.g. kg).
#' @param unit Character string. A unit of measurement.
#' @param type Numeric. Identifies the type of unit to check. 1: time; 2: O~2~
#' @details This function makes it easier to parse and understand the plethora
#' of units that a user may provide as inputs while using our functions. The
#' user may deploy a 'fuzzy' approach during the input of measurement units
#' (e.g. 'mg/l', 'mgL-1', 'mgl-1' and 'mg per litre' all work).
#' @author Januar Harianto
#' @return NULL
#' @export
#' @examples
#' checkUnits('min', 1)
#' checkUnits('notatunit', 1)
#'
#' checkUnits('hpa', 2)
#' checkUnits('notatunit', 2)
checkUnits <- function(unit, type) {
  if (type == 1) {
    allUnits <- list(
      hour = c('hour', 'hr', 'h'),
      min  = c('minute', 'min', 'm'),
      sec  = c('second', 'sec', 's'))
  }
  if(type == 2) {
    allUnits <- list(
      'pct %'   = c('%', 'percent', 'percentage', '%o2', '%O2'),
      'ug/L'    = c('ug/L', 'ug/l', 'ugL-1', 'ugl-1', 'ug per liter',
                    'ug per litre', 'µg/L', 'µg/l', 'µgL-1', 'µgl-1',
                    'µg per liter', 'µg per litre'),
      'mmol/L'  = c('mmol/L', 'mmol/l', 'mmolL-1,', 'mmoll-1', 'mmol per liter',
                    'mmol per litre'),
      'umol/L'  = c('umol/L', 'umol/l', 'umolL-1', 'umoll-1', 'µmol/L',
                    'µmol/l', 'µmolL-1', 'µmoll-1', 'umol per litre',
                    'umol per liter', 'µmol per litre', 'µmol per liter'),
      'mL/L'    = c('ml/L', 'mL/L', 'ml/l', 'mll-1', 'mLl-1', 'mLL-1', 'mlL-1',
                    'ml per l', 'mL per L', 'ml per L'),
      'mg/L'    = c('mg/L', 'mg/l', 'mgL-1', 'mgl-1', 'mg per litre',
                    'mg per liter'),
      'mg/kg'   = c('mg/kg', 'mgkg-1', 'mg per kg'),
      'ug/kg'   = c('ug/kg', 'ugkg-1', 'ug per kg', 'µg/kg', 'µgkg-1',
                    'µg per kg'),
      'mL/kg'   = c('ml/kg', 'mL/kg', 'mlkg-1', 'mLkg-1', 'ml per kg'),
      'mmol/kg' = c('mmol/kg', 'mmol/Kg', 'mmolkg-1', 'mmolKg-1', 'mmol per kg',
                    'mmol per Kg'),
      'umol/kg' = c('umol/kg', 'umol/Kg', 'umolkg-1,', 'umolKg-1',
                    'umol per kg', 'umol per Kg', 'µmol/kg', 'µmol/Kg',
                    'µmolkg-1,', 'µmolKg-1', 'µmol per kg', 'µmol per Kg'),
      'Torr'    = c('torr', 'TORR', 'Torr', 'Tor', 'tor'),
      'hPa'     = c('hPa', 'hpa', 'Hpa', 'HPA', 'HPa', 'hectopascal',
                    'hpascal'),
      'kPa'     = c('kPa', 'kpa', 'Kpa', 'KPA', 'KPa', 'kilopascal', 'kpascal'),
      'mmHg'    = c('mmHg', 'mm Hg', 'mmhg', 'mm hg', 'MMHG', 'MM HG',
                    'millimeter of mercury', 'mm mercury'),
      'inHg'    = c('inHg', 'in Hg', 'inhg', 'in hg', 'INHG', 'IN HG',
                    'inch of mercury', 'inch mercury'))
  }
  if(type == 3) {
    allUnits <- list(
      uL = c('ul', 'uL'),
      mL = c('ml', 'mL'),
      L  = c('l', 'L'))
  }
  if(type == 4) {
    allUnits <- list(
      ug = c('ug', 'UG', 'µg', 'ugram'),
      mg = c('mg', 'MG', 'mgram', 'milligram'),
      g  = c('g', 'G', 'gram'),
      kg = c('kg', 'KG', 'kilogram', 'kgram'))
  }
    # look for match
    chk <- lapply(allUnits, function(x) grep(unit, x))
    chk <- sapply(chk, function(x) length(x) > 0)
    result <- any(chk == T)
    name <- names(chk)[which(chk)]

    # print list
    out <- c(result, name)
    return(out)
    }
