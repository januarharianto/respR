# Contains functions which validate inputs

# Looks for column name and returns column number
# strings = column name(s) to search for
# df = the df
# msg = fn name to report failure from
column.id <- function(strings, df, msg){
  names <- names(df)

  if(any(!(strings %in% names)))
    stop(glue::glue("{msg}: One or more column names not found in data frame."))
  index <- as.vector(sapply(strings, function(z) which(z == names)))

  return(index)
}

# Checks that inputs which accept column numbers have no conflicts.
# Enter inputs as a list. e.g. inputs = list(time, oxygen, etc.).
# If id = FALSE returns simple logical of if a conflict is found.
# If id = TRUE returns conflicting column numbers.
column.conflict <- function(inputs, id = FALSE){
  cols <- unlist(inputs) # vector of all input column numbers
  dupe <- any(duplicated(cols)) # any duplicates found?
  dupe_no <- cols[which(duplicated(cols))] # which cols are duplicated

  if(dupe && !id) return(TRUE) else
    if(dupe && id) return(dupe_no) else
      if(!dupe && !id) return(FALSE) else
        if(!dupe && id) return(NULL)
}

# Verify the 'by' input for calc_rate etc.
# req - indicates if a 'by' input is required
# default - what should be the default if by is NULL?
# which - which 'by' methods does the function support?
# msg - start of message text, typically the function name
by_val <- function(by, req = TRUE, default = NULL,
                   which = c("t", "o", "r"),
                   msg = ""){

  # stop if required and not entered
  if (req && is.null(by)) stop(glue::glue("{msg}: 'by' input is required."))
  # if not required, and entered as NULL, apply default
  if (!req && is.null(by) && !is.null(default)) {
    by <- default
    message(glue::glue("{msg}: Applying default 'by' input of {default}."))}

  # acceptable variations
  time_variations <- "^(?i)\\b(time|t)\\b$"
  ox_variations   <- "^(?i)(o|o2|ox|oxy|oxygen)$"
  row_variations  <- "^(?i)\\b(row|r)\\b$"

  if("t" %in% which && grepl(time_variations, by)) by <- "time" else
    if("o" %in% which && grepl(ox_variations, by)) by <- "oxygen" else
      if("r" %in% which && grepl(row_variations, by)) by <- "row" else
        stop(glue::glue("{msg}: 'by' input not valid or not recognised."))

  return(by)
}

## Column input validation
## - req = input is required (i.e. can't be NULL)
## - min = for min total number of column inputs allowed
## - max = for max total number of column inputs allowed
##      (e.g. for time, max = 1, as should never have more than 1 time column)
## - range = for specific column range allowed (e.g. c(1,ncol(df)))
## - conflicts = does it conflict with these other column inputs?
##      (e.g. for time and oxygen should never be same column)
## - msg = string to add custom message
column.val <- function(input, req = FALSE, min = 1, max = Inf,
                       range = c(-Inf,Inf), conflicts = NULL, msg = ""){

  ## check if NULL
  is_null <- is.null(input)
  if(req && is_null) stop(glue::glue("{msg} column input is required."), call. = FALSE)

  ## check if numeric
  is_num <- is.numeric(input)
  if(req && !is_num) stop(glue::glue("{msg} column input is not numeric."), call. = FALSE)

  ## check all integers
  if(is_num) are_int <- all(sapply(input, function(z) z %% 1 == 0))
  ## check not greater than max
  below_max <-  length(input) <= max
  ## check not less than min
  above_min <-  length(input) >= min
  ## check within range
  if(is_num) in_range <- all(sapply(input, function(z) dplyr::between(z, range[1], range[2])))
  ## check if conflicts
  conflicts <- any(input %in% conflicts)

  # Check and messages
  if(!is_null){
    if(is_num && !are_int) stop(glue::glue("{msg} some column inputs are not integers."), call. = FALSE)
    if(!below_max) stop(glue::glue("{msg} cannot enter more than {max} column(s) with this input or this dataset."), call. = FALSE)
    if(!above_min) stop(glue::glue("{msg} at least {min} column inputs required."), call. = FALSE)
    if(is_num && !in_range) stop(glue::glue("{msg} one or more column inputs are out of range of allowed data columns."), call. = FALSE)
    if(conflicts) stop(glue::glue("{msg} one or more column inputs conflicts with other inputs."), call. = FALSE)
  }
}


## Validation of general numeric type inputs
## - num = input is numeric?
## - req = input is required (i.e. can't be NULL)
## - int = should only be integers?
## - max = for max total number of inputs allowed (e.g. flowrate should only be 1 value)
## - min = for min total number of inputs allowed
## - range = for specific range of values allowed (e.g. c(0,1))
## - msg = string to prepend failure message
input.val <- function(input, num = TRUE, int = FALSE, req = FALSE,
                      max = Inf, min = 1, range = c(-Inf,Inf), msg = ""){

  ## check if numeric
  is_num <- is.numeric(input)
  ## if is numeric, but should not be - stop
  if(is_num && !num) stop(glue::glue("{msg} input is numeric and should not be."))

  ## check if an input required
  is_null <- is.null(input)
  ## check integer
  if(is_num) are_int <- all(input %% 1 == 0)
  ## check length not greater than max allowed
  below_max <-  length(input) <= max
  ## check length not less than min allowed
  above_min <-  length(input) >= min
  ## check actual values within range
  if(is_num) in_range <- all(dplyr::between(as.numeric(input), range[1], range[2]))

  if(req && is_null) stop(glue::glue("{msg} input is required."))

  if(!is_null){
    if(num && !is_num) stop(glue::glue("{msg} input is not numeric."))
    if(num && int && !are_int) stop(glue::glue("{msg} one or more inputs are not integers."))
    if(num && !below_max) stop(glue::glue("{msg} only {max} inputs allowed."))
    if(num && !above_min) stop(glue::glue("{msg} at least {min} inputs required."))
    if(num && !in_range) stop(glue::glue("{msg} one or more inputs are outside the range of allowed values."))
  }
}

#' Validate adjust_rate or auto_rate or oxy_crit method input
#' @keywords internal
method.val <- function(method, source = "adjust_rate"){

  if(source == "adjust_rate") {
    if(!(method %in% c("value", "mean", "paired", "concurrent", "linear", "exponential")))
      stop("adjust_rate: 'method' input not recognised.", call. = F)
    #' create logical for linear/exp methods
    if(method == "linear" | method == "exponential") dynamic <- TRUE else
      dynamic <- FALSE
    return(dynamic)
  }

  if(source == "auto_rate") {
    if (!(method %in% c("linear", "max", "min", "interval",
                        "rolling", "highest", "lowest", "maximum", "minimum")))
      stop("auto_rate: 'method' input not recognised.", call. = F)
  }

  if(source == "oxy_crit") {
    if(!(method %in% c("bsr", "segmented")))
      stop("oxy_crit: 'method' input not recognised.", call. = F)
  }
}

#' Validates acceptable classes of inputs
#' Set single or multiple inputs to TRUE. This is what the fn accepts.
#' If x is *any* one of these a single TRUE will be returned
#' @keywords internal
class.val <- function(x,
                      int = FALSE,            # int of any length
                      int.sing = FALSE,       # int single
                      int.mult = FALSE,       # int multiple
                      num = FALSE,            # numeric
                      num.sing = FALSE,       # numeric single
                      num.mult = FALSE,       # numeric multiple
                      df = FALSE,             # data.frame
                      cr = FALSE,             # calc_rate any
                      cr.sing = FALSE,        # calc_rate w/ single rate
                      cr.mult = FALSE,        # calc_rate w/ multiple rates
                      cr.int = FALSE,         # calc_rate.int any
                      cr.int.sing = FALSE,    # calc_rate.int w/ single rate
                      cr.int.mult = FALSE,    # calc_rate.int w/ multiple rates
                      ar = FALSE,             # auto_rate any
                      ar.sing = FALSE,        # auto_rate w/ single rate
                      ar.mult = FALSE,        # auto_rate w/ multiple rates
                      ar.int = FALSE,         # auto_rate.int any
                      ar.int.sing = FALSE,    # auto_rate.int w/ single rate
                      ar.int.mult = FALSE,    # auto_rate.int w/ multiple rates
                      crbg = FALSE,           # calc_rate.bg any
                      crbg.sing = FALSE,      # calc_rate.bg w/ single rate
                      crbg.mult = FALSE,      # calc_rate.bg w/ multiple rates
                      cnvr = FALSE,           # convert_rate any
                      cnvr.sing = FALSE,      # convert_rate w/ single rate
                      cnvr.mult = FALSE,      # convert_rate w/ multiple rates
                      cnvr.ft = FALSE,        # convert_rate.ft any
                      cnvr.ft.sing = FALSE,   # convert_rate.ft w/ single rate
                      cnvr.ft.mult = FALSE,   # convert_rate.ft w/ multiple rates
                      insp = FALSE){          # inspect

  # any length integer
  if(int) int.chk <- (x %% 1 == 0) else
    int.chk <- NULL
  # single integer
  if(int.sing) int.sing.chk <- (x %% 1 == 0 && length(x) == 1) else
    int.sing.chk <- NULL
  # multiple integer
  if(int.mult) int.mult.chk <- (all(x %% 1 == 0) && length(x) > 1) else
    int.mult.chk <- NULL
  # any length numeric
  if(num) num.chk <- (is.numeric(x)) else
    num.chk <- NULL
  # single numeric value
  if(num.sing) num.sing.chk <- (is.numeric(x) && length(x) == 1) else
    num.sing.chk <- NULL
  # multiple numeric value
  if(num.mult) num.mult.chk <- (is.numeric(x) && length(x) > 1) else
    num.mult.chk <- NULL
  # data frame
  if(df) df.chk <- is.data.frame(x) else
    df.chk <- NULL
  # calc_rate
  if(cr) cr.chk <- any(class(x) %in% "calc_rate") else
    cr.chk <- NULL
  # calc_rate single rate
  if(cr.sing) cr.sing.chk <- (any(class(x) %in% "calc_rate") && length(x$rate) == 1) else
    cr.sing.chk <- NULL
  # calc_rate multiple rate
  if(cr.mult) cr.mult.chk <- (any(class(x) %in% "calc_rate") && length(x$rate) > 1) else
    cr.mult.chk <- NULL
  # calc_rate.int
  if(cr.int) cr.int.chk <- any(class(x) %in% "calc_rate.int") else
    cr.int.chk <- NULL
  # calc_rate.int single rate
  if(cr.int.sing) cr.int.sing.chk <- (any(class(x) %in% "calc_rate.int") && length(x$rate) == 1) else
    cr.int.sing.chk <- NULL
  # calc_rate.int multiple rate
  if(cr.int.mult) cr.int.mult.chk <- (any(class(x) %in% "calc_rate.int") && length(x$rate) > 1) else
    cr.int.mult.chk <- NULL
  # auto_rate
  if(ar) ar.chk <- any(class(x) %in% "auto_rate") else
    ar.chk <- NULL
  # auto_rate single rate
  if(ar.sing) ar.sing.chk <- (any(class(x) %in% "auto_rate") && length(x$rate) == 1) else
    ar.sing.chk <- NULL
  # auto_rate multiple rate
  if(ar.mult) ar.mult.chk <- (any(class(x) %in% "auto_rate") && length(x$rate) > 1) else
    ar.mult.chk <- NULL
  # auto_rate.int
  if(ar.int) ar.int.chk <- any(class(x) %in% "auto_rate.int") else
    ar.int.chk <- NULL
  # auto_rate.int single rate
  if(ar.int.sing) ar.int.sing.chk <- (any(class(x) %in% "auto_rate.int") && length(x$rate) == 1) else
    ar.int.sing.chk <- NULL
  # auto_rate.int multiple rate
  if(ar.int.mult) ar.int.mult.chk <- (any(class(x) %in% "auto_rate.int") && length(x$rate) > 1) else
    ar.int.mult.chk <- NULL
  # calc_rate.bg
  if(crbg) crbg.chk <- any(class(x) %in% "calc_rate.bg") else
    crbg.chk <- NULL
  # calc_rate.bg single rate
  if(crbg.sing) crbg.sing.chk <- (any(class(x) %in% "calc_rate.bg") && length(x$rate.bg) == 1) else
    crbg.sing.chk <- NULL
  # calc_rate.bg multiple rate
  if(crbg.mult) crbg.mult.chk <- (any(class(x) %in% "calc_rate.bg") && length(x$rate.bg) > 1) else
    crbg.mult.chk <- NULL
  # convert_rate any
  if(cnvr) cnvr.chk <- any(class(x) %in% "convert_rate") else
    cnvr.chk <- NULL
  # convert_rate single rate
  if(cnvr.sing) cnvr.sing.chk <- (any(class(x) %in% "convert_rate") && length(x$rate.output) == 1) else
    cnvr.sing.chk <- NULL
  # convert_rate multiple rate
  if(cnvr.mult) cnvr.mult.chk <- (any(class(x) %in% "convert_rate") && length(x$rate.output) > 1) else
    cnvr.mult.chk <- NULL
  # convert_rate.ft any
  if(cnvr.ft) cnvr.ft.chk <- any(class(x) %in% "convert_rate.ft") else
    cnvr.ft.chk <- NULL
  # convert_rate.ft single rate
  if(cnvr.ft.sing) cnvr.ft.sing.chk <- (any(class(x) %in% "convert_rate.ft") && length(x$rate.output) == 1) else
    cnvr.ft.sing.chk <- NULL
  # convert_rate.ft multiple rate
  if(cnvr.ft.mult) cnvr.ft.mult.chk <- (any(class(x) %in% "convert_rate.ft") && length(x$rate.output) > 1) else
    cnvr.ft.mult.chk <- NULL
  # inspect
  if(insp) insp.chk <- any(class(x) %in% "inspect") else
    insp.chk <- NULL

  # Assemble test results
  # (handily this only collects TRUE, NULL are omitted)
  res <- c(int.chk,
           int.sing.chk,
           int.mult.chk,
           num.chk,
           num.sing.chk,
           num.mult.chk,
           df.chk,
           cr.chk,
           cr.sing.chk,
           cr.mult.chk,
           cr.int.chk,
           cr.int.sing.chk,
           cr.int.mult.chk,
           ar.chk,
           ar.sing.chk,
           ar.mult.chk,
           ar.int.chk,
           ar.int.sing.chk,
           ar.int.mult.chk,
           crbg.chk,
           crbg.sing.chk,
           crbg.mult.chk,
           cnvr.chk,
           cnvr.sing.chk,
           cnvr.mult.chk,
           cnvr.ft.chk,
           cnvr.ft.sing.chk,
           cnvr.ft.mult.chk,
           insp.chk)

  final_res <- any(res)

  return(final_res)
}


#' Check unit string against allowed values. See util_fns.R file for regex
#' patterns
#'
#' These names (before the .o2, .vol, etc.) are the 'clean' or parsed names we
#' want to use in outputs. All inputs units get parsed to these in units.clean
#'
#' @keywords internal
units.val <- function(unit, is, msg = "units.val") {

  # time --------------------------------------------------------------------
  if (is == 'time') all.units <- list(min.time = min.time.rgx,
                                      sec.time = sec.time.rgx,
                                      hr.time = hr.time.rgx,
                                      day.time = day.time.rgx)

  # o2 ----------------------------------------------------------------------
  # 2-dimensional o2 units, and pressure
  if (is == 'o2') {

    if(unit %in% c("%", "perc", "percent","percentage"))
      stop(glue::glue("{msg}: unit \"%\" has been deprecated. Please use \"%Air\" or \"%Oxy\" instead. See unit_args()."), call. = FALSE)

    all.units <- list('%Air.o2'    = PercAir.o2.rgx,
                      '%Oxy.o2'    = PercOxy.o2.rgx,
                      'mg/L.o2'    = mgperL.o2.rgx,
                      'ug/L.o2'    = ugperL.o2.rgx,
                      'mol/L.o2'   = molperL.o2.rgx,
                      'mmol/L.o2'  = mmolperL.o2.rgx,
                      'umol/L.o2'  = umolperL.o2.rgx,
                      'nmol/L.o2'  = nmolperL.o2.rgx,
                      'pmol/L.o2'  = pmolperL.o2.rgx,
                      'mL/L.o2'    = mLperL.o2.rgx,
                      'uL/L.o2'    = uLperL.o2.rgx,
                      'cm3/L.o2'   = cm3perL.o2.rgx,
                      'mm3/L.o2'   = mm3perL.o2.rgx,
                      'mg/kg.o2'   = mgperkg.o2.rgx,
                      'mg/kg.o2'   = ppm.o2.rgx,
                      'ug/kg.o2'   = ugperkg.o2.rgx,
                      'mL/kg.o2'   = mLperkg.o2.rgx,
                      'uL/kg.o2'   = uLperkg.o2.rgx,
                      'mol/kg.o2'  = molperkg.o2.rgx,
                      'mmol/kg.o2' = mmolperkg.o2.rgx,
                      'umol/kg.o2' = umolperkg.o2.rgx,
                      'nmol/kg.o2' = nmolperkg.o2.rgx,
                      'pmol/kg.o2' = pmolperkg.o2.rgx,
                      'cm3/kg.o2'  = cm3perkg.o2.rgx,
                      'mm3/kg.o2'  = mm3perkg.o2.rgx,
                      'Torr.o2p'   = Torr.o2p.rgx,
                      'hPa.o2p'    = hPa.o2p.rgx,
                      'kPa.o2p'    = kPa.o2p.rgx,
                      'mmHg.o2p'   = mmHg.o2p.rgx,
                      'inHg.o2p'   = inHg.o2p.rgx)
  }

  # vol ---------------------------------------------------------------------
  if (is == 'vol') all.units <- list(L.vol  = L.vol.rgx,
                                     mL.vol = mL.vol.rgx,
                                     uL.vol = uL.vol.rgx)

  # mass --------------------------------------------------------------------
  if (is == 'mass') all.units <- list(kg.mass  = kg.mass.rgx,
                                      g.mass   = g.mass.rgx,
                                      mg.mass  = mg.mass.rgx,
                                      ug.mass  = ug.mass.rgx)

  # area --------------------------------------------------------------------
  if (is == 'area') all.units <- list(km2.area  = km2.area.rgx,
                                      m2.area   = m2.area.rgx,
                                      cm2.area  = cm2.area.rgx,
                                      mm2.area  = mm2.area.rgx)

  # o1 ----------------------------------------------------------------------
  if (is == 'o1') all.units <-  list(mg.o2   = mg.o2.rgx,
                                     ug.o2   = ug.o2.rgx,
                                     mol.o2  = mol.o2.rgx,
                                     mmol.o2 = mmol.o2.rgx,
                                     umol.o2 = umol.o2.rgx,
                                     nmol.o2 = nmol.o2.rgx,
                                     pmol.o2 = pmol.o2.rgx,
                                     mL.o2   = mL.o2.rgx,
                                     uL.o2   = uL.o2.rgx,
                                     cm3.o2  = cm3.o2.rgx,
                                     mm3.o2  = mm3.o2.rgx)

  # flow --------------------------------------------------------------------
  if (is == 'flow') all.units <- list('uL/sec.flow' = uLpersec.flow.rgx,
                                      'mL/sec.flow' = mLpersec.flow.rgx,
                                      'L/sec.flow'  = Lpersec.flow.rgx,
                                      'uL/min.flow' = uLpermin.flow.rgx,
                                      'mL/min.flow' = mLpermin.flow.rgx,
                                      'L/min.flow'  = Lpermin.flow.rgx,
                                      'uL/hr.flow'  = uLperhr.flow.rgx,
                                      'mL/hr.flow'  = mLperhr.flow.rgx,
                                      'L/hr.flow'   = Lperhr.flow.rgx,
                                      'uL/day.flow' = uLperday.flow.rgx,
                                      'mL/day.flow' = mLperday.flow.rgx,
                                      'L/day.flow'  = Lperday.flow.rgx)

  # pressure ----------------------------------------------------------------
  if (is == 'pressure') all.units <- list(kPa.p  = kPa.p.rgx,
                                          hPa.p  = hPa.p.rgx,
                                          Pa.p   = Pa.p.rgx,
                                          uBar.p = uBar.p.rgx,
                                          mBar.p = mBar.p.rgx,
                                          Bar.p  = Bar.p.rgx,
                                          atm.p  = atm.p.rgx,
                                          Torr.p = Torr.p.rgx,
                                          mmHg.p = mmHg.p.rgx,
                                          inHg.p = inHg.p.rgx)

  # temperature -------------------------------------------------------------
  if (is == 'temperature') all.units <- list(C.temp = C.temp.rgx,
                                             K.temp = K.temp.rgx,
                                             F.temp = F.temp.rgx)

  # Look for match ----------------------------------------------------------
  # remove all whitespace
  string <- gsub(" ", "", stringr::str_squish(unit))
  chk <- lapply(all.units, function(x) grepl(x, string))
  chk <- sapply(chk, function(x) isTRUE(any(x)))

  ## Message if no match found
  result <- any(chk == TRUE)  # did a match occur?
  if (result == FALSE)
    stop(glue::glue("{msg}: unit '{unit}' not recognised. Check it is valid for the input or output type. \nOutput rate unit strings should be in correct order: O2/Time or O2/Time/Mass or O2/Time/Area.\nSee unit_args() for details.", call. = F))

  # print unit name
  out <- names(chk)[which(chk)]
  return(out)
}

#' Cleans units from units.val to remove the suffix (.o2, .flow, etc)
#'
#' These are the 'clean' or parsed names we want to use in outputs. All input
#' units get parsed to these.
#'
#' Could make code much simpler with a regex for everything after the "." but i
#' like the specificity of this.
#'
#' @keywords internal
units.clean <- function(unit, is) {

  if (is == 'o2') out <- unit %>%
      gsub(".o2p", ".o2", x = .) %>%
      gsub(".o2", "", x = .)

  if (is == 'time')        out <- unit %>% gsub(".time", "", x = .)
  if (is == 'vol')         out <- unit %>% gsub(".vol",  "", x = .)
  if (is == 'mass')        out <- unit %>% gsub(".mass", "", x = .)
  if (is == 'area')        out <- unit %>% gsub(".area", "", x = .)
  if (is == 'o1')          out <- unit %>% gsub(".o1",   "", x = .)
  if (is == 'flow')        out <- unit %>% gsub(".flow", "", x = .)
  if (is == 'pressure')    out <- unit %>% gsub(".p",    "", x = .)
  if (is == 'temperature') out <- unit %>% gsub(".temp", "", x = .)

  return(out)
}


#' Checks if an oxygen concentration/pressure unit or metabolic rate unit
#' requires temperature, salinity, and pressure to convert to another unit.
#'
#' `type` should be `"oxy"` or `"mr"`.
#'
#' Returns `TRUE` if it requires t,S,P or `FALSE` if not.
#'
#' @keywords internal
StP.check <- function(unit, type) {
  if(type == "oxy") check <- any(sapply(oxy.StP.req.rgx, function(z) grepl(z, unit)))
  if(type == "mr") check <- any(sapply(mr.StP.req.rgx, function(z) grepl(z, unit)))
  return(check)
}

#' Returns an error message if an oxygen concentration/pressure unit or
#' metabolic rate unit requires S or t and either is NULL, or a message if it
#' requires P and it is NULL.
#'
#' `type` should be `"oxy"` or `"mr"`.
#' `P.chk` - perform P out of normal range check. Only want to do this once per function.
#'
#' Also returns default value of P = 1.013253 if it is NULL.
#'
#' @keywords internal
StP.val <- function(unit, type, S, t, P, P.chk = TRUE, msg) {

  if(P.chk && all(!is.null(P)) && any(!dplyr::between(P, 0.9, 1.08)))
    warning(glue::glue("{msg}: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range. \nP values should not be outside the typical natural range of 0.9 to 1.1 except for special applications. \nPlease make sure it is entered in 'bar' units. Conversion performed regardless."),
            call. = FALSE)

  if(is.null(S)) {
    check.S <- StP.check(unit, type)
    if(check.S) stop(glue::glue("{msg}: Input or output units require Salinity input (i.e. S = ??)"), call. = FALSE)
  } else if(is.null(t)) {
    check.t <- StP.check(unit, type)
    if(check.t) stop(glue::glue("{msg}: Input or output units require Temperature input (i.e. t = ??)"), call. = FALSE)
  } else if(is.null(P)) {
    check.P <- StP.check(unit, type)
    if(check.P){
      message(glue::glue("{msg}: Input or output units require Atmospheric Pressure input (i.e. P = ??). \n Default value of P = 1.013253 bar has been used."))
      P <- 1.013253
    }
  }

  return(P)
}



