#' Convert values to a different unit
#'
#' This is a basic function that converts values of temperature, volume, mass,
#' area, and atmospheric pressure to different units. This can be useful in
#' [convert_DO()], [convert_rate()], and [convert_rate.ft()] in which some
#' inputs must be in specific units (e.g. atmospheric pressure in bar, area in
#' m2). See examples.
#'
#' @details
#' If the `'to'` input is left `NULL`, these defaults are applied depending on
#' the unit type of the `from` input: volume "L", temperature "C", mass "kg",
#' area "m2", pressure "bar".
#'
#' A fuzzy string matching algorithm is used to accept different unit formatting
#' styles. For example, `'msq', 'm2', 'M2', 'sqm'` are all parsed as metres
#' squared of area.
#'
#' **Units Accepted**
#'
#' *Temperature:*
#'
#' - "C", "K", "F"
#'
#' *Pressure:*
#'
#' - "kPa", "hPa", "Pa", "ubar", "mbar", "bar", "atm" (standard atmospheres),
#' "Torr"
#'
#' *Volume:*
#'
#' - "uL", "mL", "L"
#'
#' *Mass:*
#'
#' - "ug", "mg", "g", "kg"
#'
#' *Area:*
#'
#' - "mm2", "cm2", "m2", "km2"
#'
#' @param x numeric value or vector. Values to be converted to a different unit.
#' @param from string. Unit of the original values.
#' @param to string. Unit to be converted to. These defaults are applied if left
#'   NULL: volume "L", temperature "C", mass "kg", area "m2", pressure "bar".
#'
#' @return Output is a numeric vector of converted values.
#'
#' @importFrom marelac convert_p convert_T
#' @export
#'
#' @examples
#' # Convert volume
#' convert_val(10, "ml", "L")
#' convert_val(10:15, "ml", "L")
#'
#' # Convert temperature
#' convert_val(-273.15, "K", "C")
#' convert_val(-40, "F", "C")
#' convert_val(c(2,4,6,8), "C", "F")
#'
#' # Convert pressure
#' convert_val(1, "atm", "bar")
#' convert_val(1010, "hpa", "bar")
#' convert_val(735, "torr", "kpa")
#'
#' # Convert area
#' convert_val(100, "cm2", "m2")
#' convert_val(10000, "mm2", "cm2")
#'
#' # Convert mass
#' convert_val(200, "g", "kg")
#' convert_val(10000, "ug", "mg")
#'
#' # Use directly in a respR function which requires inputs to be
#' # in a specific unit. For example, in convert_rate() pressure
#' # must be in 'bar' and respirometer volume in 'L'.
#' # Here, chamber volume is 200 ml, pressure measured in mbar.
#' x <- suppressWarnings(inspect(urchins.rd, 1, 2))
#' rate <- calc_rate(x, from = 20, to = 30)
#' convert_rate(rate, o2.unit = "ml/l", time.unit = "min",
#'              output.unit = "mg/h",
#'              volume = convert_val(200, "ml", "L"),
#'              S = 35, t = 15,
#'              P = convert_val(1010, "mbar", "bar"))
#'
#' # Note, the default 'to' units are set to those respR requires in
#' # these functions ("L" and "bar" here), so do not necessarily need
#' # to be specified:
#' x <- suppressWarnings(inspect(urchins.rd, 1, 2))
#' rate <- calc_rate(x, from = 20, to = 30)
#' convert_rate(rate, o2.unit = "ml/l", time.unit = "min",
#'              output.unit = "mg/h",
#'              volume = convert_val(200, "ml"),
#'              S = 35, t = 15,
#'              P = convert_val(1010, "mbar"))

convert_val <- function(x, from = NULL, to = NULL) {

  # Validate inputs ---------------------------------------------------------
  ## validate from and to input
  input.val(from, num = FALSE, req = TRUE,
            msg = "convert_val: 'from'")
  input.val(to, num = FALSE, req = FALSE,
            msg = "convert_val: 'to'")

  ## check from unit type
  fr_unit <- unit_type(from, msg = "convert_val: ")
  if(!(fr_unit %in% c("vol", "temperature", "mass", "area", "pressure")))
    stop("convert_val: 'from' unit is not one of the accepted unit types.")

  ## apply defaults
  if(is.null(to)) {
    if(fr_unit == "vol") to <- "L"
    else if(fr_unit == "temperature") to <- "C"
    else if(fr_unit == "mass") to <- "kg"
    else if(fr_unit == "area") to <- "m2"
    else if(fr_unit == "pressure") to <- "bar"
  }
  ## check to unit type
  to_unit <- unit_type(to, msg = "convert_val: ")
  if(!(to_unit %in% c("vol", "temperature", "mass", "area", "pressure")))
    stop("convert_val: 'to' unit is not one of the accepted unit types.")

  ## warn if different
  if(fr_unit != to_unit)
    stop("convert_val: 'from' and 'to' appear to be different unit types.")

  # Perform conversion ------------------------------------------------------
  if(to_unit == "vol"){
    unit.fr <- verify_units(from, fr_unit)
    unit.to <- verify_units(to, to_unit)
    out <- adjust_scale(x, unit.fr, unit.to)
  }
  if(to_unit == "mass"){
    unit.fr <- verify_units(from, fr_unit)
    unit.to <- verify_units(to, to_unit)
    out <- adjust_scale(x, unit.fr, unit.to)
  }
  if(to_unit == "area"){
    unit.fr <- verify_units(from, fr_unit)
    unit.to <- verify_units(to, to_unit)
    out <- adjust_scale_area(x, unit.fr, unit.to)
  }
  if(to_unit == "pressure"){
    unit.fr <- verify_units(from, fr_unit)
    unit.to <- verify_units(to, to_unit)
    uns <- c("kpa.p", "hpa.p", "pa.p", "ubar.p", "mbar.p",
             "bar.p", "atm.p", "torr.p")
    # from 1 bar
    multiplier <- c(100, 1000, 100000, 1000000, 1000,
                    1, 0.98692, 750.06)
    mult.fr <- multiplier[match(unit.fr, uns)]
    mult.to <- multiplier[match(unit.to, uns)]
    out <- x * (mult.to/mult.fr)
  }
  if(to_unit == "temperature"){
    unit.fr <- verify_units(from, fr_unit)
    unit.to <- verify_units(to, to_unit)

    if(unit.fr == "c.temp"){
      if(unit.to == "k.temp") out <- x + 273.15
      else if(unit.to == "f.temp") out <- x * 9/5 + 32
      else out <- x
    }

    if(unit.fr == "k.temp"){
      if(unit.to == "c.temp") out <- x - 273.15
      else if(unit.to == "f.temp") out <- x * 9/5 - 459.67
      else out <- x
    }

    if(unit.fr == "f.temp"){
      if(unit.to == "c.temp") out <- (x - 32) * 5/9
      else if(unit.to == "k.temp") out <- (x + 459.67) * 5/9
      else out <- x
    }
  }


  # Return result -----------------------------------------------------------
  return(out)

}

#' Search for and classify units
#'
#' @keywords internal
#' @export
unit_type <- function(unit, msg = ""){

  all.units <- list(
    time = c(
      c('days', 'day', 'dy', 'dys', 'd',
        'Days', 'Day', 'Dy', 'Dys', 'D'),
      c('hours', 'hour', 'hr', 'hrs', 'h',
        'Hours', 'Hour', 'Hr', 'Hrs', 'H'),
      c('minutes', 'minute', 'min', 'mins', 'm',
        'Minutes', 'Minute', 'Min', 'Mins', 'M'),
      c('seconds', 'second', 'sec', 'secs', 's',
        'Seconds', 'Second', 'Sec', 'Secs', 'S')),
    vol = c(
      c('ul.vol','ul','uL','microlitre','microliter',
        'micro litre','micro liter'),
      c('mL.vol','ml','mL','millilitre','milli litre','milliliter',
        'milli liter'),
      c('L.vol','l','L','liter','litre','Litre','Liter')),
    mass = c(
      c('ug.mass','ug','UG','ugram','microgram'),
      c('mg.mass','mg','MG','mgram','milligram'),
      c('g.mass','g','G','gram'),
      c('kg.mass','kg','KG','kilogram','kgram')),
    area = c(
      c('mm2.area','mmsq','mm2','sqmm'),
      c('cm2.area','cmsq','cm2','sqcm'),
      c('m2.area','msq','m2','sqm'),
      c('km2.area','kmsq','km2','sqkm')),
    pressure = c(
      c('kPa','kpa', 'KPA'),
      c('hPa','hpa', 'HPA'),
      c('Pa','pa', 'PA'),
      c('ub', 'ubar', 'Ubar', 'UBAR', 'uBar', 'ubr', 'UBR'),
      c('mb', 'mbar', 'Mbar', 'MBAR', 'mBar', 'mbr', 'MBR'),
      c('b', 'bar', 'bar', 'BAR', 'Bar', 'br', 'BR'),
      c('atm', 'Atm', 'ATM', 'Atmos', 'ATMOS'),
      c('torr','TORR','Torr','Tor','tor'),
      c('mmHg','mm Hg','mmhg','mm hg','MMHG','MM HG'),
      c('inHg','in Hg','inhg','in hg','INHG','IN HG')),
    temperature = c(
      c('C','c', 'dgrc', 'DGRC', 'dgr c', 'DGR C',
        'degrees c', 'DEGREES C',
        'celsius', 'Celsius', 'CELSIUS',
        'centigrade', 'Centigrade'),
      c('K','k', 'dgrk', 'DGRK', 'dgr k', 'DGR K',
        'degrees k', 'DEGREES K',
        'kelvin', 'Kelvin', 'KELVIN'),
      c('F','f', 'dgrf', 'DGRF', 'dgr f', 'DGR F',
        'degrees f', 'DEGREES F',
        'fahrenheit', 'Fahrenheit', 'FAHRENHEIT'))
  )

  #  look for match ---------------------------------------------------------
  string <- paste0('^', unit, '$')  # for exact matching
  chk <- lapply(all.units, function(y) grep(string, y))
  chk <- sapply(chk, function(y) length(y) > 0)
  result <- any(chk == T)  # did a match occur?
  if(!result)
    stop(glue::glue("{msg}\"{unit}\" unit not recognised."))
  out <- names(chk)[which(chk)]  # print unit name
  return(out)
}
