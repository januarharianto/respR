#' Convert values of temperature, volume, mass, area, and atmospheric pressure
#' to different units
#'
#' This is a basic function that converts values of temperature, volume, mass,
#' area, and atmospheric pressure to different units. This can be useful in
#' [convert_DO()], [convert_rate()], and [convert_rate.ft()] where some inputs
#' must be in specific units (e.g. temperature in °C, atmospheric pressure in
#' bar, area in m2). See Examples.
#'
#' Note the type of unit does not need to be specified. The function will
#' automatically recognise it using the `from` unit.
#'
#' If the `'to'` input is left `NULL`, the following defaults are applied
#' depending on the unit type of the `from` input:
#'
#' - volume:        `"L"`
#'
#' - temperature:   `"C"`
#'
#' - mass:          `"kg"`
#'
#' - area:          `"m2"`
#'
#' - pressure:      `"bar"`
#'
#' A fuzzy string matching algorithm is used to accept different unit formatting
#' styles. For example, `"msq"` `"m2"`, `"M2"`, `"sqm"` are all parsed as metres
#' squared of area.
#'
#' ## Accepted Units
#'
#' *Temperature:*
#'
#' - `"C"`, `"K"`, `"F"`
#'
#' *Pressure:*
#'
#' - `"kPa"`, `"hPa"`, `"Pa"`, `"ubar"`, `"mbar"`, `"bar"`, `"Torr"`, `"atm"`
#' (note, this is standard atmospheres).
#'
#' *Volume:*
#'
#' - `"uL"`, `"mL"`, `"L"`
#'
#' *Mass:*
#'
#' - `"ug"`, `"mg"`, `"g"`, `"kg"`
#'
#' *Area:*
#'
#' - `"mm2"`, `"cm2"`, `"m2"`, `"km2"`
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return Output is a numeric vector of converted values.
#'
#' @param x numeric value or vector. Values to be converted to a different unit.
#' @param from string. Unit of the original values.
#' @param to string. Unit to be converted to. These defaults are applied if left
#'   `NULL`: volume `"L"`, temperature `"C"`, mass `"kg"`, area `"m2"`, pressure
#'   `"bar"`.
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
#' convert_val(-273.15, "C", "K")
#' convert_val(-40, "C", "F")
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
#' # Here, we know chamber volume is 200 ml, and pressure measured in mbar.
#' x <- suppressWarnings(inspect(urchins.rd, 1, 2))
#'
#' rate <- calc_rate(x, from = 20, to = 30)
#'
#' convert_rate(rate,
#'              oxy.unit = "ml/l",
#'              time.unit = "min",
#'              output.unit = "mg/h",
#'              volume = convert_val(200, "ml", "L"),
#'              S = 35,
#'              t = 15,
#'              P = convert_val(1010, "mbar", "bar"))
#'
#' # Note, the default 'to' units are set to those respR requires in
#' # these functions ('L' and 'bar' here), so do not necessarily need
#' # to be specified:
#' convert_rate(rate,
#'              oxy.unit = "ml/l",
#'              time.unit = "min",
#'              output.unit = "mg/h",
#'              volume = convert_val(200, "ml"),
#'              S = 35,
#'              t = 15,
#'              P = convert_val(1010, "mbar"))

convert_val <- function(x, from = NULL, to = NULL) {

  # Validate inputs ---------------------------------------------------------
  ## validate from and to input
  input.val(from, num = FALSE, req = TRUE,
            msg = "convert_val: 'from'")
  input.val(to, num = FALSE, req = FALSE,
            msg = "convert_val: 'to'")

  ## check from unit type
  fr_unit <- unit_type(from, msg = "convert_val")
  if(!(fr_unit %in% c("vol", "temperature", "mass", "area", "pressure")))
    stop("convert_val: 'from' unit is not one of the accepted unit types.", call. = FALSE)

  ## apply defaults
  if(is.null(to)) {
    if(fr_unit == "vol") to <- "L"
    else if(fr_unit == "temperature") to <- "C"
    else if(fr_unit == "mass") to <- "kg"
    else if(fr_unit == "area") to <- "m2"
    else if(fr_unit == "pressure") to <- "bar"
  }
  ## check to unit type
  to_unit <- unit_type(to, msg = "convert_val")
  if(!(to_unit %in% c("vol", "temperature", "mass", "area", "pressure")))
    stop("convert_val: 'to' unit is not one of the accepted unit types.", call. = FALSE)

  ## warn if different
  if(fr_unit != to_unit)
    stop("convert_val: 'from' and 'to' appear to be different unit types.", call. = FALSE)

  # Perform conversion ------------------------------------------------------
  if(to_unit == "vol"){
    unit.fr <- units.val(from, fr_unit)
    unit.to <- units.val(to, to_unit)
    out <- adjust_scale(x, unit.fr, unit.to)
  }
  if(to_unit == "mass"){
    unit.fr <- units.val(from, fr_unit)
    unit.to <- units.val(to, to_unit)
    out <- adjust_scale(x, unit.fr, unit.to)
  }
  if(to_unit == "area"){
    unit.fr <- units.val(from, fr_unit)
    unit.to <- units.val(to, to_unit)
    out <- adjust_scale_area(x, unit.fr, unit.to)
  }
  if(to_unit == "pressure"){
    unit.fr <- units.val(from, fr_unit)
    unit.to <- units.val(to, to_unit)
    uns <- c("kPa.p", "hPa.p", "Pa.p", "uBar.p", "mBar.p",
             "Bar.p", "atm.p", "Torr.p")
    # from 1 bar
    multiplier <- c(100, 1000, 100000, 1000000, 1000,
                    1, 0.98692, 750.06)
    mult.fr <- multiplier[match(unit.fr, uns)]
    mult.to <- multiplier[match(unit.to, uns)]
    out <- x * (mult.to/mult.fr)
  }
  if(to_unit == "temperature"){
    unit.fr <- units.val(from, fr_unit)
    unit.to <- units.val(to, to_unit)

    if(unit.fr == "C.temp"){
      if(unit.to == "K.temp") out <- x + 273.15
      else if(unit.to == "F.temp") out <- x * 9/5 + 32
      else out <- x
    }

    if(unit.fr == "K.temp"){
      if(unit.to == "C.temp") out <- x - 273.15
      else if(unit.to == "F.temp") out <- x * 9/5 - 459.67
      else out <- x
    }

    if(unit.fr == "F.temp"){
      if(unit.to == "C.temp") out <- (x - 32) * 5/9
      else if(unit.to == "K.temp") out <- (x + 459.67) * 5/9
      else out <- x
    }
  }


  # Return result -----------------------------------------------------------
  return(out)

}

#' Search for and classify units
#'
#' @keywords internal
unit_type <- function(unit, msg = ""){

  all.units <- list(
    time = min.time.rgx,
    time = sec.time.rgx,
    time = hr.time.rgx,
    time = day.time.rgx,
    vol = L.vol.rgx,
    vol = mL.vol.rgx,
    vol = uL.vol.rgx,
    mass = kg.mass.rgx,
    mass = g.mass.rgx,
    mass = mg.mass.rgx,
    mass = ug.mass.rgx,
    area = km2.area.rgx,
    area = m2.area.rgx,
    area = cm2.area.rgx,
    area = mm2.area.rgx,
    pressure = kPa.p.rgx,
    pressure = hPa.p.rgx,
    pressure = Pa.p.rgx,
    pressure = uBar.p.rgx,
    pressure = mBar.p.rgx,
    pressure = Bar.p.rgx,
    pressure = atm.p.rgx,
    pressure = Torr.p.rgx,
    pressure = mmHg.p.rgx,
    pressure = inHg.p.rgx,
    temperature = C.temp.rgx,
    temperature = K.temp.rgx,
    temperature = F.temp.rgx
  )

  #  look for match ---------------------------------------------------------
  string <- gsub(" ", "", stringr::str_squish(unit))
  chk <- lapply(all.units, function(x) grepl(x, string))
  chk <- sapply(chk, function(x) isTRUE(any(x)))

  result <- any(chk == T)  # did a match occur?
  if(!result)
    stop(glue::glue("{msg}: '{unit}' unit not recognised."), call. = FALSE)
  out <- names(chk)[which(chk)]  # print unit name
  return(out)
}

#' Search for and classify oxygen units as used in concentration or rates
#'
#' @keywords internal
unit_type_o1 <- function(unit, msg = ""){

  all.units <- list(
    o1 = mg.o2.rgx,
    o1 = ug.o2.rgx,
    o1 = mol.o2.rgx,
    o1 = mmol.o2.rgx,
    o1 = umol.o2.rgx,
    o1 = nmol.o2.rgx,
    o1 = pmol.o2.rgx,
    o1 = mL.o2.rgx,
    o1 = uL.o2.rgx,
    o1 = cm3.o2.rgx,
    o1 = mm3.o2.rgx
    )

  #  look for match ---------------------------------------------------------
  string <- gsub(" ", "", stringr::str_squish(unit))
  chk <- lapply(all.units, function(x) grepl(x, string))
  chk <- sapply(chk, function(x) isTRUE(any(x)))

  result <- any(chk == T)  # did a match occur?
  if(!result)
    stop(glue::glue("{msg}: '{unit}' unit not recognised as an oxygen unit that can be used for rates or concentrations.", call. = FALSE))
  out <- names(chk)[which(chk)]  # print unit name
  return(out)
}

