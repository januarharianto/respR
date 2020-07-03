#' Convert between units of dissolved oxygen.
#'
#' This is a conversion function that performs basic conversions between
#' concentration or pressure units of dissolved oxygen (DO). Concentration units
#' should use SI units (`L` or `kg`) for the denominator. Some DO units require
#' temperature (`t`), salinity (`S`), and atmospheric pressure (`P`) to be
#' specified. For freshwater experiments, salinity should be set to zero (i.e.
#' `S = 0`). See [unit_args()] for details of accepted units.
#'
#' @param x numeric value or vector. This is the dissolved oxygen (DO) value(s)
#'   that you want to convert.
#' @param from string. The DO unit to convert *from*. See [unit_args()] for
#'   details.
#' @param to string. The unit to convert *to*. See [unit_args()] for details.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Required for conversion
#'   of some units. See [unit_args()] for details.
#' @param t numeric. Temperature(°C). Defaults to NULL. Required for conversion
#'   of some units. See [unit_args()] for details.
#' @param P numeric. Pressure (bar). Defaults to 1.013253. Required for
#'   conversion of some units. See [unit_args()] for details.
#'
#' @return Returns a `list` object containing four elements: `$input` values,
#'   `$output` (converted) values, `$input.unit` and `$output.unit`
#'
#' @importFrom marelac molvol molweight gas_satconc sw_dens vapor atmComp
#' @export
#'
#' @examples
#' # Convert a numeric value from/to units which do not require t, S and P
#' convert_DO(8.21, from = "mg/L", to = "umol/L")
#'
#' # Convert a numeric value from/to units which require t, S and P
#' convert_DO(100, from = "percent", to = "mg L-1", S = 33, t = 18)
#' convert_DO(214, from = "hPa", to = "mL/kg", S = 33, t = 18)
#'
#' # Convert a vector of values
#' convert_DO(urchins.rd[[5]], from = "mg/L", to = "umol/L")
#' convert_DO(c(8.01, 8.03, 8.05), from = "mg per litre", to = "%",
#'   t = 15, S = 35)
#' convert_DO(sardine.rd[[2]], from = "percent", to = "torr",
#'   t = 15, S = 35)

convert_DO <- function(x, from = NULL, to = NULL, S = NULL, t = NULL,
                         P = 1.013253) {

  # Validate input:
  if (!is.character(from)) stop("convert_DO: 'from' unit should be a character string.")
  if (!is.character(to)) stop("convert_DO: 'to' unit should be a character string.")

  # Verify the units:
  fru <- verify_units(from, 'o2')
  tou <- verify_units(to, 'o2')

  # Units requiring t, S and/or P (all same for now)
  tsp_req <- c("mL/L.o2", "mL/kg.o2", "%.o2", "Torr.o2p", "hPa.o2p", "kPa.o2p",
    "inHg.o2p", "mmHg.o2p", "mg/kg.o2", "ug/kg.o2", "mmol/kg.o2", "umol/kg.o2",
    "mL/kg.o2")

  # Check t, S and P needed for units

  ## t and S - could combine these to one check
  if(is.null(S) && (fru %in% tsp_req || tou %in% tsp_req))
    stop("convert_DO: Input or output units require Salinity input (i.e. S = ??)")

  if(is.null(t) && (fru %in% tsp_req || tou %in% tsp_req))
    stop("convert_DO: Input or output units require Temperature input (i.e. t = ??)")

  ## Set default P if not provided
  if(is.null(P) && (fru %in% tsp_req || tou %in% tsp_req))
    message("convert_DO: Input or output units require Atmospheric Pressure input (i.e. P = ??). \n Default value of P = 1.013253 bar has been used.")
  if(is.null(P)) P <- 1.013253

  # Constants/formula data using data taken from 'marelac' (gsw removed atm).
  # Conversion factors between pressure units are obtained from the udunits2
  # C library: https://www.unidata.ucar.edu/software/udunits/

  if(!is.null(t)) omVl <- unname(marelac::molvol(t, P, species = "O2"))  # moles O2 in 1L vol
  omWt <- unname(marelac::molweight('O2'))  # molecular weight of O2 in g/mol
  if(!is.null(t) && !is.null(S)) oGas <- unname(marelac::gas_satconc(S, t, P, species = "O2")) # gas sat conc.
  if(!is.null(t) && !is.null(S)) swDn <- marelac::sw_dens(S = S, t = t, P = P) # seawater density in kg/m^3
  #swDn <- gsw::gsw_rho_t_exact(S, t, (P * 10))  # seawater density in kg/m^3
  if(!is.null(t) && !is.null(S)) vpor <- marelac::vapor(S = S, t = t)  # sat. pressure of water vapour (au)
  oAtm <- unname(marelac::atmComp('O2'))  # atmospheric composition of O2 (%)

  # Import from other functions
  # This made no sense! These are not DO units but DO/time rates
  # However this would be useful functionality to have - conversion of rates between units
  # if (class(x) %in% c("calc_rate","auto_rate")) z <- x$rate
  # if (class(x) %in% "adjust_rate") z <- x$adjusted.rate

  if (is.numeric(x)) z <- x

  # Validate x input:
  if (!is.numeric(z)) stop("convert_DO: input 'x' must be a numeric value or vector.")

  # Perform conversions
  # First we convert all values to a standard unit, mg/L:
  if (fru == verify_units('mg/L',   'o2')) {c <-  z}
  if (fru == verify_units('ug/L',   'o2')) {c <-  z / 1e3}
  if (fru == verify_units('mmol/L', 'o2')) {c <-  z * omWt}
  if (fru == verify_units('umol/L', 'o2')) {c <-  z * omWt / 1e3}
  if (fru == verify_units('mL/L',   'o2')) {c <-  z * omWt / omVl}
  if (fru == verify_units('mg/kg',  'o2')) {c <-  z * swDn / 1e3}
  if (fru == verify_units('ug/kg',  'o2')) {c <-  z * swDn / 1e6}
  if (fru == verify_units('mmol/kg','o2')) {c <-  z * swDn * omWt / 1e3}
  if (fru == verify_units('umol/kg','o2')) {c <-  z * swDn * omWt / 1e6}
  if (fru == verify_units('%',      'o2')) {c <-  z * oGas * omWt / 1e3 / 100}
  if (fru == verify_units('mL/kg',  'o2')) {c <-  z * omWt / omVl * swDn / 1e3}
  if (fru == verify_units('Torr',   'o2')) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 760.000066005}
  if (fru == verify_units('hPa',    'o2')) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 1013.235}
  if (fru == verify_units('kPa',    'o2')) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 101.3235}
  if (fru == verify_units('mmHg',   'o2')) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 759.999951996}
  if (fru == verify_units('inHg',   'o2')) {c <-  z / (P - vpor) / oAtm * oGas * omWt / 1e3 / 29.9212583001}

  # Then we convert mg/L to the final desired unit:
  if(tou == verify_units('mg/L',   'o2')) {out <- c}
  if(tou == verify_units('ug/L',   'o2')) {out <- c * 1e3}
  if(tou == verify_units('mmol/L', 'o2')) {out <- c / omWt}
  if(tou == verify_units('umol/L', 'o2')) {out <- c / omWt * 1e3}
  if(tou == verify_units('mL/L',   'o2')) {out <- c / omWt * omVl}
  if(tou == verify_units('mg/kg',  'o2')) {out <- c / swDn * 1e3}
  if(tou == verify_units('ug/kg',  'o2')) {out <- c / swDn * 1e6}
  if(tou == verify_units('mmol/kg','o2')) {out <- c / omWt / swDn * 1e3}
  if(tou == verify_units('umol/kg','o2')) {out <- c / omWt / swDn * 1e6}
  if(tou == verify_units('%',      'o2')) {out <- c / omWt / oGas * 1e3 * 100}
  if(tou == verify_units('mL/kg',  'o2')) {out <- c / swDn * omVl / omWt * 1e3}
  if(tou == verify_units('Torr',   'o2')) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 760.000066005}
  if(tou == verify_units('hPa',    'o2')) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 1013.253}
  if(tou == verify_units('kPa',    'o2')) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 101.3253}
  if(tou == verify_units('mmHg',   'o2')) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 759.999951996}
  if(tou == verify_units('inHg',   'o2')) {out <- c / omWt / oGas * oAtm * (P - vpor) * 1e3 * 29.9212583001}

  # Generate output
  out <- list(input = z, output = out, input.unit = from, output.unit = to)
  # if (is.numeric(x)) {
  #   out <- c(input = x, out)
  #   } else out <- c(x, out)

  class(out) <- "convert_DO"
  return(out)
}



#' @export
print.convert_DO <- function(x, ...) {
  if(length(x$output) >= 20) {
    cat("Showing only the first 20 conversions:\n")
    print(head(x$output, 20))
  } else print(x$output)
  cat("\n Input unit:", x$input.unit)
  cat("\nOutput unit:", x$output.unit)
  cat("\n")
}

#' Check unit string against a known database
#'
#' @keywords internal
#' @export
verify_units <- function(unit, is) {
  # Not sure if worth ID'ing some of these using regex (too many variations)
  # EDIT: ok it's worth it, but I've come too far.... will fix in future version
  # Doing it the stupid way:
  # time units
  if (is == 'time') {
    all.units <- list(
      hour.time = c('hour', 'hr', 'h'),
      min.time  = c('minute', 'min', 'm'),
      sec.time  = c('second', 'sec', 's'))
  }
  # 2-dimensional o2 units, and pressure
  if (is == 'o2') {
    all.units <- list(
      '%.o2' = c('%.o2','%','percent','percentage','%o2','%O2'),

      'ug/L.o2' = c('ug/L.o2','ug/L','ug/l','ug / L','ug / l','ugL-1',
                    'ugl-1','ug L-1','ug l -1','ug per liter','ug per litre'),

      'mmol/L.o2' = c('mmol/L.o2','mmol/L','mmol/l','mmol / L','mmol / l',
                      'mmolL-1,','mmoll-1','mmol L-1,','mmol l-1',
                      'mmol per liter','mmol per litre'),

      'umol/L.o2' = c('umol/L.o2','umol/L','umol/l','umolL-1','umoll-1',
                      'umol / L','umol / l','umol L-1','umol l-1',
                      'umol per litre','umol per liter'),

      'mL/L.o2' = c('mL/L.o2','ml/L','mL/L','mL/l','ml/l','mll-1','mLl-1',
                    'mLL-1','mlL-1','ml / L','mL / L','mL / l','ml / l',
                    'ml l-1','mL l-1','mL L-1','ml L-1','ml per l','mL per L',
                    'ml per L'),

      'mg/L.o2' = c('mg/L.o2','mg/L','mg/l','mg / l','mg / L','mgL-1','mgl-1',
                    'mg L-1','mg l-1','mg per litre','mg per liter'),

      'mg/kg.o2' = c('mg/kg.o2','mg/kg','mg / kg','mgkg-1','mg kg-1',
                     'mg per kg'),

      'ug/kg.o2' = c('ug/kg.o2','ug/kg','ugkg-1','ug / kg','ug kg-1',
                     'ug per kg'),

      'mL/kg.o2' = c('mL/kg.o2','ml/kg','mL/kg','mlkg-1','mLkg-1','ml / kg',
                     'mL / kg','ml kg-1','mL kg-1','ml per kg'),

      'mmol/kg.o2' = c('mmol/kg.o2','mmol/kg','mmol/Kg','mmolkg-1','mmolKg-1',
                       'mmol / kg','mmol / Kg','mmol kg-1','mmol Kg-1',
                       'mmol per kg','mmol per Kg'),

      'umol/kg.o2' = c('umol/kg.o2','umol/kg','umol/Kg','umolkg-1,','umolKg-1',
                       'umol / kg','umol / Kg','umol kg-1,','umol Kg-1',
                       'umol per kg','umol per Kg'),

      'Torr.o2p' = c('Torr.o2p','torr','TORR','Torr','Tor','tor'),

      'hPa.o2p' = c('hPa.o2p','hPa','hpa','Hpa','HPA','HPa','hectopascal',
                    'hpascal'),

      'kPa.o2p' = c('kPa.o2p','kPa','kpa','Kpa','KPA','KPa','kilopascal',
                    'kpascal'),

      'mmHg.o2p' = c('mmHg.o2p','mmHg','mm Hg','mmhg','mm hg','MMHG','MM HG',
                     'millimeter of mercury','mm mercury'),

      'inHg.o2p' = c('inHg.o2p','inHg','in Hg','inhg','in hg','INHG','IN HG',
                     'inch of mercury','inch mercury'))
  }
  if (is == 'vol') {
    all.units <- list(
      uL.vol = c('ul.vol','ul','uL','microlitre','microliter',
                 'micro litre','micro liter'),
      mL.vol = c('mL.vol','ml','mL','millilitre','milli litre','milliliter',
                 'milli liter'),
      L.vol  = c('L.vol','l','L','liter','litre','Litre','Liter'))
  }
  if (is == 'mass') {
    all.units <- list(
      ug.mass  = c('ug.mass','ug','UG','ugram','microgram'),
      mg.mass  = c('mg.mass','mg','MG','mgram','milligram'),
      g.mass   = c('g.mass','g','G','gram'),
      kg.mass  = c('kg.mass','kg','KG','kilogram','kgram'))
  }
  if (is == 'area') {
    all.units <- list(
      mm2.area  = c('mm2.area','mmsq','mm2','sqmm'),
      cm2.area  = c('cm2.area','cmsq','cm2','sqcm'),
      m2.area   = c('m2.area','msq','m2','sqm'),
      km2.area  = c('km2.area','kmsq','km2','sqkm'))
  }
  if (is == 'o1') {
    all.units <-  list(
      'mg.o2'   = c('mg.o2','mg','milligram'),
      'ug.o2'   = c('ug.o2','ug','microgram'),
      'mmol.o2' = c('mmol.o2','mmol','millimol'),
      'umol.o2' = c('umol.o2','umol','micromol'),
      'ml.o2'   = c('ml.o2','ml','mL','millil'))
  }
  # Look for match
  string <- paste0('^', unit, '$')  # for exact matching
  chk <- lapply(all.units, function(x) grep(string, x))
  chk <- sapply(chk, function(x) length(x) > 0)
  result <- any(chk == T)  # did a match occur?
  if (result == FALSE)
    stop("verify_units: unit '", unit, "' not recognised. Check it is valid for the input or output type. \nOutput rate unit strings should be in correct order: O2/Time or O2/Time/Mass or O2/Time/Area.\nSee unit_args() for details.", call. = F)
  out <- names(chk)[which(chk)]  # print unit name
  return(out)
}



