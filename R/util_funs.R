# General package default settings ------------------------------------------------

# Number of rows of summary tables to print before it prints a condensed version
# i.e. nrows in print.data.table
summ_rows <- 50

# Regex -------------------------------------------------------------------

# Regex patterns for units
# Put here so don't have to update them in multiple places
# Used in units.val and unit_type and unit_type_o1 etc.

# Allowed unit separators - used in splitting units up
unit.sep.rgx  <- "(?:-1|[_/.[:space:]]|per)+"
# time
min.time.rgx  <- "^(?i)\\b(minute|min|m)(s)?(.time)?\\b$"
sec.time.rgx  <- "^(?i)\\b(second|sec|s)(s)?(.time)?\\b$"
hr.time.rgx   <- "^(?i)\\b(hour|hr|h)(s)?(.time)?\\b$"
day.time.rgx  <- "^(?i)\\b(day|dy|d)(s)?(.time)?\\b$"
# o2
# DO NOT require S,t,P
mgperL.o2.rgx    <- "^(?i)\\b(mg|milligram|milligramme)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
ugperL.o2.rgx    <- "^(?i)\\b(ug|microgram|microgramme)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
molperL.o2.rgx   <- "^(?i)\\b(mol|mole)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
mmolperL.o2.rgx  <- "^(?i)\\b(mmol|mmole|millimol|millimole)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
umolperL.o2.rgx  <- "^(?i)\\b(umol|umole|micromol|micromole)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
nmolperL.o2.rgx  <- "^(?i)\\b(nmol|nmole|nanomol|nanomole)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
pmolperL.o2.rgx  <- "^(?i)\\b(pmol|pmole|picomol|picomole)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
# require S,t,P
molperkg.o2.rgx  <- "^(?i)\\b(mol|mole)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
mmolperkg.o2.rgx <- "^(?i)\\b(mmol|mmole|millimol|millimole)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
umolperkg.o2.rgx <- "^(?i)\\b(umol|umole|micromol|micromole)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
nmolperkg.o2.rgx <- "^(?i)\\b(nmol|nmole|nanomol|nanomole)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
pmolperkg.o2.rgx <- "^(?i)\\b(pmol|pmole|picomol|picomole)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
ugperkg.o2.rgx   <- "^(?i)\\b(ug|microgram|microgramme)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
mgperkg.o2.rgx   <- "^(?i)\\b(mg|milligram|milligramme)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
ppm.o2.rgx       <- "^(?i)\\b(p|parts)(.o2|O2)?(/|per|p|.|_)?(m|million)(.o2)?\\b$"
PercAir.o2.rgx   <- "^(?i)\\b(%|perc|percent|percentage)[._]*(air|a)(.o2)?\\b$"
PercOxy.o2.rgx   <- "^(?i)\\b(%|perc|percent|percentage)[._]*(oxygen|oxy|ox|o|o2)(.o2)?\\b$"
cm3perL.o2.rgx   <- "^(?i)\\b(cm3|cm[\\^]3|cc|ccm|cubiccm)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
mm3perL.o2.rgx   <- "^(?i)\\b(mm3|mm[\\^]3|cmm|cubicmm)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
cm3perkg.o2.rgx  <- "^(?i)\\b(cm3|cm[\\^]3|cc|ccm|cubiccm)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
mm3perkg.o2.rgx  <- "^(?i)\\b(mm3|mm[\\^]3|cmm|cubicmm)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
mLperL.o2.rgx    <- "^(?i)\\b(ml|millilitre|milliliter)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
uLperL.o2.rgx    <- "^(?i)\\b(ul|microlitre|microliter)(.o2|O2)?(/|per|.|_)?(l|liter|litre)(-1)?(.o2)?\\b$"
mLperkg.o2.rgx   <- "^(?i)\\b(ml|millilitre|milliliter)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
uLperkg.o2.rgx   <- "^(?i)\\b(ul|microlitre|microliter)(.o2|O2)?(/|per|.|_)?(kg|kilogram|kilogramme)(-1)?(.o2)?\\b$"
Torr.o2p.rgx     <- "^(?i)\\b(tor|torr)(.o2|O2)?(.o2p)?\\b$"
hPa.o2p.rgx      <- "^(?i)\\b(hpa|hectopascal|hpascal)(.o2|O2)?(.o2p)?\\b$"
kPa.o2p.rgx      <- "^(?i)\\b(kpa|kilopascal|kpascal)(.o2|O2)?(.o2p)?\\b$"
mmHg.o2p.rgx     <- "^(?i)\\b(mm|millimeter|millimetre)(of|.|_)?(hg|mercury)(.o2|O2)?(.o2p)?\\b$"
inHg.o2p.rgx     <- "^(?i)\\b(in|inch|inches)(of|.|_)?(hg|mercury)(.o2|O2)?(.o2p)?\\b$"

# all the above in one object
# Used in StP.check internal fn
oxy.StP.req.rgx <- c(molperkg.o2.rgx, mmolperkg.o2.rgx, umolperkg.o2.rgx, nmolperkg.o2.rgx, pmolperkg.o2.rgx,
                     ugperkg.o2.rgx, mgperkg.o2.rgx, ppm.o2.rgx, PercAir.o2.rgx, PercOxy.o2.rgx, cm3perL.o2.rgx,
                     mm3perL.o2.rgx, cm3perkg.o2.rgx, mm3perkg.o2.rgx, mLperL.o2.rgx, uLperL.o2.rgx,
                     mLperkg.o2.rgx, uLperkg.o2.rgx, Torr.o2p.rgx, hPa.o2p.rgx, kPa.o2p.rgx, mmHg.o2p.rgx,
                     inHg.o2p.rgx)
# Metabolic rate units which require StP
# Only "mL/time" "uL/time", "cm3/time", "mm3/time" and per mass
mr.StP.req.rgx <- c("^(?i)\\b(ml|millilitre|milliliter|ul|microlitre|microliter|cm3|cm[\\^]3|cc|ccm|cubiccm|mm3|mm[\\^]3|cmm|cubicmm)(.o2|O2)?(/|per|.|_)?(second|sec|s|minute|min|m|hour|hr|h|day|dy|d)(-1)?(/|per|.|_)?(ug|ugm|ugram|microgram|microgramme|mg|mgm|mgram|milligram|milligramme|g|gm|gram|gramme|kg|kgm|kgram|kilogram|kilogramme)?(-1)?\\b$")

# volume
L.vol.rgx  <- "^(?i)\\b(l|litre|liter)(s?)(.vol)?\\b$"
mL.vol.rgx <- "^(?i)\\b(ml|millilitre|milliliter)(s?)(.vol)?\\b$"
uL.vol.rgx <- "^(?i)\\b(ul|microlitre|microliter)(s?)(.vol)?\\b$"
# mass
kg.mass.rgx <- "^(?i)\\b(kg|kgm|kgram|kilogram|kilogramme)(s?)(.mass)?\\b$"
g.mass.rgx  <- "^(?i)\\b(g|gm|gram|gramme)(s?)(.mass)?\\b$"
mg.mass.rgx <- "^(?i)\\b(mg|mgm|mgram|milligram|milligramme)(s?)(.mass)?\\b$"
ug.mass.rgx <- "^(?i)\\b(ug|ugm|ugram|microgram|microgramme)(s?)(.mass)?\\b$"
# area
km2.area.rgx  <- "^(?i)\\b(km|kilometre|kilometer|km[\\^]|kilometre[\\^]|kilometer[\\^])(-2|2|sq)(.area)?\\b$"
m2.area.rgx   <- "^(?i)\\b(m|metre|meter|m[\\^]|metre[\\^]|meter[\\^])(-2|2|sq)(.area)?\\b$"
cm2.area.rgx  <- "^(?i)\\b(cm|centimetre|centimeter|cm[\\^]|centimetre[\\^]|centimeter[\\^])(-2|2|sq)(.area)?\\b$"
mm2.area.rgx  <- "^(?i)\\b(mm|millimetre|millimeter|mm[\\^]|millimetre[\\^]|millimeter[\\^])(-2|2|sq)(.area)?\\b$"
# o1
mg.o2.rgx   <- "^(?i)\\b(mg|milligram|milligramme)(s?)(O2)?(.o2)?\\b$"
ug.o2.rgx   <- "^(?i)\\b(ug|microgram|microgramme)(s?)(O2)?(.o2)?\\b$"
mol.o2.rgx  <- "^(?i)\\b(mol|mole)(s?)(O2)?(.o2)?\\b$"
mmol.o2.rgx <- "^(?i)\\b(mmol|mmole|millimol|millimole)(s?)(O2)?(.o2)?\\b$"
umol.o2.rgx <- "^(?i)\\b(umol|umole|micromol|micromole)(s?)(O2)?(.o2)?\\b$"
nmol.o2.rgx <- "^(?i)\\b(nmol|nmole|nanomol|nanomole)(s?)(O2)?(.o2)?\\b$"
pmol.o2.rgx <- "^(?i)\\b(pmol|pmole|picomol|picomole)(s?)(O2)?(.o2)?\\b$"
mL.o2.rgx   <- "^(?i)\\b(ml|millilitre|milliliter)(s?)(O2)?(.o2)?\\b$"
uL.o2.rgx   <- "^(?i)\\b(ul|microlitre|microliter)(s?)(O2)?(.o2)?\\b$"
cm3.o2.rgx  <- "^(?i)\\b(cm3|cm[\\^]3|cc|ccm|cubiccm)(s?)(O2)?(.o2)?\\b$"
mm3.o2.rgx  <- "^(?i)\\b(mm3|mm[\\^]3|cmm|cubicmm)(s?)(O2)?(.o2)?\\b$"
# flow
uLpersec.flow.rgx <- "^(?i)\\b(ul|microlitre|microliter)(s?)(/|per|.|_)?(second|sec|s)(s)?(-1)?(.flow)?\\b$"
mLpersec.flow.rgx <- "^(?i)\\b(ml|millilitre|milliliter)(s?)(/|per|.|_)?(second|sec|s)(s)?(-1)?(.flow)?\\b$"
Lpersec.flow.rgx  <- "^(?i)\\b(l|litre|liter)(s?)(/|per|.|_)?(second|sec|s)(s)?(-1)?(.flow)?\\b$"
uLpermin.flow.rgx <- "^(?i)\\b(ul|microlitre|microliter)(s?)(/|per|.|_)?(minute|min|m)(s)?(-1)?(.flow)?\\b$"
mLpermin.flow.rgx <- "^(?i)\\b(ml|millilitre|milliliter)(s?)(/|per|.|_)?(minute|min|m)(s)?(-1)?(.flow)?\\b$"
Lpermin.flow.rgx  <- "^(?i)\\b(l|litre|liter)(s?)(/|per|.|_)?(minute|min|m)(s)?(-1)?(.flow)?\\b$"
uLperhr.flow.rgx <- "^(?i)\\b(ul|microlitre|microliter)(s?)(/|per|.|_)?(hour|hr|h)(s)?(-1)?(.flow)?\\b$"
mLperhr.flow.rgx <- "^(?i)\\b(ml|millilitre|milliliter)(s?)(/|per|.|_)?(hour|hr|h)(s)?(-1)?(.flow)?\\b$"
Lperhr.flow.rgx  <- "^(?i)\\b(l|litre|liter)(s?)(/|per|.|_)?(hour|hr|h)(s)?(-1)?(.flow)?\\b$"
uLperday.flow.rgx <- "^(?i)\\b(ul|microlitre|microliter)(s?)(/|per|.|_)?(day|dy|d)(s)?(-1)?(.flow)?\\b$"
mLperday.flow.rgx <- "^(?i)\\b(ml|millilitre|milliliter)(s?)(/|per|.|_)?(day|dy|d)(s)?(-1)?(.flow)?\\b$"
Lperday.flow.rgx  <- "^(?i)\\b(l|litre|liter)(s?)(/|per|.|_)?(day|dy|d)(s)?(-1)?(.flow)?\\b$"
# pressure
kPa.p.rgx  <- "^(?i)\\b(kpa)(.p)?\\b$"
hPa.p.rgx  <- "^(?i)\\b(hpa)(.p)?\\b$"
Pa.p.rgx   <- "^(?i)\\b(pa)(.p)?\\b$"
uBar.p.rgx <- "^(?i)\\b(ub|ubar|ubr)(.p)?\\b$"
mBar.p.rgx <- "^(?i)\\b(mb|mbar|mbr)(.p)?\\b$"
Bar.p.rgx  <- "^(?i)\\b(bar|br)(.p)?\\b$"
atm.p.rgx  <- "^(?i)\\b(atm|atmos|atmosphere|atmospheres)(.p)?\\b$"
Torr.p.rgx <- "^(?i)\\b(tor|torr)(.p)?\\b$"
mmHg.p.rgx <- "^(?i)\\b(mmhg|millimetrehg|millimeterhg|millimetreshg|millimetershg)(.p)?\\b$"
inHg.p.rgx <- "^(?i)\\b(inhg|inchhg|incheshg)(.p)?\\b$"
# temperature
C.temp.rgx <- "^(?i)\\b(dgr|degree|degrees)?(c|cel|celcius|celsius|centigrade)(.temp)?\\b$"
K.temp.rgx <- "^(?i)\\b(dgr|degree|degrees)?(k|kelvin|kel)(.temp)?\\b$"
F.temp.rgx <- "^(?i)\\b(dgr|degree|degrees)?(f|fahrenheit)(.temp)?\\b$"

# -------------------------------------------------------------------------

#' Pipe graphics direct from tidyverse-related package
#' @importFrom magrittr %>%
#' @name %>%
#' @return No value returned
#' @keywords internal
#' @export
NULL

#' Select columns
#' @importFrom dplyr select
#' @name select
#' @return No value returned
#' @keywords internal
#' @export
NULL

# check os - useful for parallel functions
os <- function() {
  if (.Platform$OS.type == "windows")
    "win" else if (Sys.info()["sysname"] == "Darwin")
      "mac" else if (.Platform$OS.type == "unix")
        "unix" else stop("Unknown OS")
}

#' Convert between multipliers of the same unit, e.g. mg to kg
#'
#' Converts units of the same scale, e.g. mg to kg, or mL to L.
#'
#' @param x numeric.
#' @param input string.
#' @param output string.
#'
#' @keywords internal
#'
#' @return A numeric.
#'
#' @importFrom stringr str_replace
adjust_scale <- function(x, input, output) {
  # Create database of terms for matching
  prefix <- c("p", "n", "u", "m", "", "k", "sec", "min", "hr", "day")
  suffix <- c("mol", "g", "L", "l", "")
  multip <- c(1e-12, 1e-09, 1e-06, 0.001, 1, 1000, 3600, 60, 1, 1/24)
  string <- "^(p|n|u|m||k|sec|min|hr|day)?(mol|g|L|l|)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")  # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef)))  # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")  # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft)))  # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("adjust_scale: Units do not match and cannot be converted.", call. = F)
  # Convert!
  a <- multip[match(bef[2], prefix)]  # get multiplier from input
  b <- multip[match(aft[2], prefix)]  # get multiplier from output
  out <- x * (a/b)  # convert
  return(out)
}


#' Convert between multipliers of the same AREA unit, e.g. mm2 to km2
#'
#' This is an internal function. Converts units of area. Could be combined with
#' adjust_scale, but didn't know how....
#'
#' @param x numeric.
#' @param input string.
#' @param output string.
#'
#' @keywords internal
#'
#' @return A numeric.
#'
#' @importFrom stringr str_replace
adjust_scale_area <- function(x, input, output) {
  # Create database of terms for matching
  prefix <- c("m", "c", "", "k")
  suffix <- c("m2")
  multip <- c(1e-06, 0.0001, 1, 1e+06)
  string <- "^(m|c||k)?(m2)$"
  # Clean and extract input strings
  bef <- stringr::str_replace(input, "\\..*", "")  # remove .suffix
  bef <- unlist(regmatches(bef, regexec(string, bef)))  # split up
  # Clean and extract output strings
  aft <- stringr::str_replace(output, "\\..*", "")  # remove .suffix
  aft <- unlist(regmatches(aft, regexec(string, aft)))  # split up
  # Check that conversion is possible
  if (bef[3] != aft[3])
    stop("adjust_scale_area: Units do not match and cannot be converted.", call. = F)
  # Convert!
  a <- multip[match(bef[2], prefix)]  # get multiplier from input
  b <- multip[match(aft[2], prefix)]  # get multiplier from output
  out <- x * (a/b)  # convert
  return(out)
}

# checks for `inspect()` functions --------------------------------

## combined check:
check_timeseries <- function(x, type = "time") {
  if (type == "time") {
    num <- sapply(x, function(y) check_num(y))
    ## if not numeric, no point doing these checks
    ## So instead we 'skip'
    if(!num[[1]][1]) inf <- sapply(x, function(y) check_inf(y)) else
      inf <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) nan <- sapply(x, function(y) check_na(y)) else
      nan <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) seq <- sapply(x, function(y) check_seq(y)) else
      seq <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) dup <- sapply(x, function(y) check_dup(y)) else
      dup <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    if(!num[[1]][1]) evn <- sapply(x, function(y) check_evn(y)) else
      evn <- sapply(x, function(y) return(list(check = "skip", which = integer(0))))
    checks <- rbind(
      num[1, , drop = F],
      inf[1, , drop = F],
      nan[1, , drop = F],
      seq[1, , drop = F],
      dup[1, , drop = F],
      evn[1, , drop = F]
    )
    locs <- rbind(
      NA,
      inf[2, , drop = F],
      nan[2, , drop = F],
      seq[2, , drop = F],
      dup[2, , drop = F],
      evn[2, , drop = F]
    )
  } else if (type == "oxygen") {
    num <- sapply(x, function(y) check_num(y))

    # if oxy column has passed numeric check, do check
    # otherwise return "skip"
    # check inf
    inf <- sapply(1:length(x), function(y) {
      if(!num[,y][[1]]) check_inf(x[[y]]) else
        return(list(check = "skip", which = integer(0)))
    })
    # check nan
    nan <- sapply(1:length(x), function(y) {
      if(!num[,y][[1]]) check_na(x[[y]]) else
        return(list(check = "skip", which = integer(0)))
    })

    seq <- NA
    dup <- NA
    evn <- NA
    checks <- rbind(
      num[1, , drop = F],
      inf[1, , drop = F],
      nan[1, , drop = F],
      seq[1],
      dup[1],
      evn[1])
    locs <- rbind(
      NA,
      inf[2, , drop = F],
      nan[2, , drop = F],
      seq[1],
      dup[1],
      evn[1])
  }

  # rename rows
  rnames <- c("numeric", "Inf/-Inf", "NA/NaN", "sequential", "duplicated", "evenly-spaced  ")
  rownames(checks) <- rnames
  rownames(locs) <- rnames

  return(list(checks, locs))
}

## check for non-numeric values - used in the function `inspect()`
## Note - checks for NOT numeric
check_num <- function(x) {
  test <- !is.numeric(x)
  check <- any(test)
  highlight <- rep(check, length(x))
  out <- list(check = check, which = highlight)
  return(out)
}

## check for NA values - used in the function `inspect()`
check_na <- function(x) {
  test <- is.na(x)
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

## check for Inf/-Inf values - used in the function `inspect()`
check_inf <- function(x) {
  test <- is.infinite(x)
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

## check for sequential (monotonic) data - used in the function `inspect()`
check_seq <- function(x) {
  test <- diff(x) < 0
  test <- ifelse(is.na(test), FALSE, test)  # convert NA values to FALSE
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

# check for duplicate data (time) - used in the function `inspect()`
check_dup <- function(x) {
  test <- x %in% unique(x[duplicated(x, incomparables = NA)])
  check <- any(test)
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

## calculate mode - used in the function `inspect()`
calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## check for evenly-spaced data (time) - used in the function `inspect()`
check_evn <- function(x) {
  spacing <- diff(as.numeric(x))
  mod <- calc_mode(spacing)

  test <- spacing != mod
  # If spacing is even, there should only be 1 interval detected:
  check <- length(unique(spacing)) > 1

  test <- ifelse(is.na(test), TRUE, test)  # convert NA values to FALSE
  highlight <- which(test)
  out <- list(check = check, which = highlight)
  return(out)
}

# Internal truncate (similar to subset_data)
truncate_data <- function(x, from, to, by) {

  # import from other respR functions
  if (any(class(x) %in% "inspect")) x <- x$dataframe
  if (any(class(x) %in% "inspect.ft")) x <- x$dataframe

  dt <- data.table::as.data.table(x)

  ## replace NULL inputs with defaults
  if (is.null(by)) by <- "time"

  ## verify by just in case
  by <- by.val(by)

  ## replace NULL inputs with defaults
  if(is.null(from)){
    if(by == "time") from <- min(dt[[1]], na.rm = TRUE)
    if(by == "row") from <- 1
    if(by == "oxygen") from <- dt[[2]][1] # first oxygen value
  }
  if(is.null(to)){
    if(by == "time") to <- max(dt[[1]], na.rm = TRUE)
    if(by == "row") to <- nrow(dt)
    if(by == "oxygen") to <- dt[[2]][nrow(dt)] # last oxygen value
  }

  ## time is ok, since it is always increasing
  if (by == "time") {
    # if values out of range use lowest/highest available
    rng <- range(dt[[1]], na.rm = TRUE)
    if(from < rng[1]) from <- rng[1]
    if(to > rng[2]) to <- rng[2]
    #out <- dt[dt[[1]] >= from & dt[[1]] <= to] # old method
    # new method - finds closest value to each time
    out <- dt[dt[[1]] >= dt[[1]][which.min(abs(dt[[1]] - from))]
              & dt[[1]] <= dt[[1]][which.min(abs(dt[[1]] - to))]]
  }
  ## row is ok, since it is always increasing
  if (by == "row") {
    # if to value out of range use highest available
    if(to > nrow(dt)) to <- nrow(dt)
    out <- dt[from:to]
  }
  ## oxygen could be increasing or decreasing
  if (by == "oxygen") {

    # data range
    o_range <- range(dt[[2]], na.rm = TRUE)

    # use highest/lowest values if out of range
    if(from > o_range[2]) from <- o_range[2] else
      if(from < o_range[1]) from <- o_range[1]
      if(to > o_range[2]) to <- o_range[2] else
        if(to < o_range[1]) to <- o_range[1]

        ## dplyr::between needs them in low-high order
        lower <- sort(c(from, to))[1]
        upper <- sort(c(from, to))[2]
        # indices of data between these
        start <- min(which(dplyr::between(dt[[2]], lower, upper)), na.rm = TRUE)
        end <- max(which(dplyr::between(dt[[2]], lower, upper)), na.rm = TRUE)

        out <- dt[start:end]
  }
  return(out)
}


# FUNCTIONS for P_crit----------------------------

#' Perform broken-stick regressions
#' @return a data.table object
#' @keywords internal

broken_stick <- function(dt, n) {
  # Cut data into 2
  dta <- dt[1:n]
  dtb <- dt[(n + 1):nrow(dt)]

  # Perform lm
  lma <- .lm.fit(cbind(1, dta[[1]]), dta[[2]])
  lmb <- .lm.fit(cbind(1, dtb[[1]]), dtb[[2]])

  # Extract coefficients
  coefa <- coef(lma)
  coefb <- coef(lmb)

  # Calculate residual sum of squares
  trss <- sum(lma$residuals*lma$residuals) + sum(lmb$residuals*lmb$residuals)

  # Also, calculate intersect
  cm <- rbind(coefa, coefb)
  # https://stackoverflow.com/a/7114961
  intersect <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])[1]

  # Calculate midpoint
  midpoint <-  (dta[,x][nrow(dta)] + dtb[,x][1]) / 2

  # List coefficients
  line1 <- data.table(rbind(coefa))
  names(line1) <- c("b0", "b1")
  line2 <- data.table(rbind(coefb))
  names(line2) <- c("b0", "b1")

  # Generate output
  out <- data.table::data.table(
    splitpoint = dta[,x][nrow(dta)],
    sumRSS = trss,
    pcrit.intercept = intersect,
    pcrit.midpoint = midpoint,
    l1_coef = line1,
    l2_coef = line2

  )
  return(out)
}


#' Generate a DO ~ PO2 data table from a DO timeseries
#' @return a data.table object
#' @keywords internal
generate_mrdf <- function(dt, width) {
  # Ensure that dt is a data.table
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, 1:2, c("x", "y"))

  # Extract columns
  x <- as.matrix(dt[,1])
  y <- as.matrix(dt[,2])

  # Then, perform rolling mean and lm
  rollx <- na.omit(roll::roll_mean(y, width))
  rolly <- static_roll(dt, width)

  # Then, combine into new data.table
  rdt <- data.table::data.table(rollx, rolly$slope_b1)
  data.table::setnames(rdt, 1:2, c("x", "y"))
  return(rdt)
}

#' Omit NA, NaN, Inf and -Inf from a vector or dataframe columns
#'
#' For using with, for example, range to get axis range values in 'inspect'.
#' Previously, na.omit was used, then discovered data files with Inf values.
#' This causes axis limit range to be Inf, and xlim/ylim don't accept infinite
#' axes!
#'
#' If x is dataframe, it returns a vector of all columns appended together.
#' Only useful for getting range in this case, don't use for anything else.
#'
#' @return original vector without NA or Inf values or df all cols appended without these
#' @keywords internal
nainf.omit <- function(x) {
  if (is.vector(x)) z <- na.omit(x[is.finite(x)])

  if (is.data.frame(x)) {
    z <- lapply(x, function(y) na.omit(y[is.finite(y)]))
    z <- as.vector(unlist(z))
  }
  return(z)
}

# Deal with pesky "no visible binding for global variable.." checks
x = NULL; endtime = NULL; row.len = NULL; time.len = NULL
rowlength = NULL; endrow = NULL; timelength = NULL; rate.2pt = NULL
endoxy = NULL; oxy = NULL; sumRSS = NULL; do = NULL; y = NULL; V1 = NULL
..xcol = NULL; ..ycol = NULL; multicore = NULL; multisession = NULL
rsq = NULL; rate = NULL; rate.output = NULL; start_row = NULL;
intercept_b0 = NULL; slope_b1 = NULL


