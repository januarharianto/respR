## For storing functions which validate inputs

# Verify the 'by' input for calc_rate etc.
# req - indicates if a 'by' input is required
# default - what should be the default if by is NULL?
# which - which 'by' methods does the function support?
# msg - start of message text, typically the function name
verify_by <- function(by, req = TRUE, default = NULL,
                      which = c("t", "o", "r", "p"),
                      msg = ""){
  ## no doubt this is easier with regex
  time_variations <- c("time", "Time", "TIME",
                       "tim", "Tim", "TIM",
                       "tm", "Tm", "TM",
                       "t", "T")
  ox_variations <- c("o2", "O2",
                     "oxygen", "Oxygen", "OXYGEN",
                     "oxy", "Oxy", "OXY",
                     "ox", "Ox", "OX",
                     "o", "O")
  row_variations <- c("row", "Row", "ROW",
                      "rw", "Rw", "RW",
                      "r", "R")
  prop_variations <- c("proportion", "Proportion", "PROPORTION",
                       "proport", "Proport", "PROPORT",
                       "prop", "Prop", "PROP",
                       "prp", "Prp", "PRP",
                       "pr", "Pr", "PR",
                       "p", "P")

  # stop if required and not entered
  if (req && is.null(by)) stop(glue::glue("{msg} 'by' input is required."))

  # if not required, and entered as NULL, apply default
  if (!req && is.null(by) && !is.null(default)) {
    by <- default
    message(glue::glue("{msg} applying default 'by' input of {default}."))}

  if("t" %in% which && by %in% time_variations) by <- "time"
  else if("o" %in% which && by %in% ox_variations) by <- "oxygen"
  else if("r" %in% which && by %in% row_variations) by <- "row"
  else if("p" %in% which && by %in% prop_variations) by <- "proportion"
  else stop(glue::glue("{msg} 'by' input not valid or not recognised."))

  return(by)
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
  if(req && is_null) stop(glue::glue("{msg} column input is required."))

  ## check if numeric
  is_num <- is.numeric(input)
  if(req && !is_num) stop(glue::glue("{msg} column input is not numeric."))

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
    if(is_num && !are_int) stop(glue::glue("{msg} some column inputs are not integers."))
    if(!below_max) stop(glue::glue("{msg} cannot enter more than {max} column(s) with this input or this dataset."))
    if(!above_min) stop(glue::glue("{msg} at least {min} column inputs required."))
    if(is_num && !in_range) stop(glue::glue("{msg} one or more column inputs are out of range of allowed data columns."))
    if(conflicts) stop(glue::glue("{msg} one or more column inputs conflicts with other inputs."))
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
  if(is_num) are_int <- all(sapply(input, function(z) z %% 1 == 0))
  ## check length not greater than max allowed
  below_max <-  length(input) <= max
  ## check length not less than min allowed
  above_min <-  length(input) >= min
  ## check actual values within range
  if(is_num) in_range <- all(sapply(input, function(z) dplyr::between(z, range[1], range[2])))

  if(req && is_null) stop(glue::glue("{msg} input is required."))

  if(!is_null){
    if(num && !is_num) stop(glue::glue("{msg} input is not numeric."))
    if(num && int && !are_int) stop(glue::glue("{msg} one or more inputs are not integers."))
    if(num && !below_max) stop(glue::glue("{msg} only {max} inputs allowed."))
    if(num && !above_min) stop(glue::glue("{msg} at least {min} inputs required."))
    if(num && !in_range) stop(glue::glue("{msg} one or more inputs are outside the range of allowed values."))
  }
}




#' Validate adjust_rate method input
#' @keywords internal
val_meth <- function(method){
  if(!(method %in% c("value", "mean", "paired", "concurrent", "linear", "exponential")))
    stop("adjust_rate: 'method' input not recognised")
  #' create logical for linear/exp methods
  if(method == "linear" | method == "exponential") dynamic <- TRUE else
    dynamic <- FALSE
  return(dynamic)
}


#' Validates acceptable classes of inputs
#' Set single or multiple inputs to TRUE. If x is any one of these a TRUE
#' will be returned
#' @keywords internal
class.val <- function(x,
                      int = FALSE,         # int of any length
                      int.sing = FALSE,    # int single
                      int.mult = FALSE,    # int multiple
                      num = FALSE,         # numeric
                      num.sing = FALSE,    # numeric single
                      num.mult = FALSE,    # numeric multiple
                      df = FALSE,          # data.frame
                      cr = FALSE,          # calc_rate any
                      cr.sing = FALSE,     # calc_rate w/ single rate
                      cr.mult = FALSE,     # calc_rate w/ multiple rates
                      cr.int = FALSE,      # calc_rate.int any
                      cr.int.sing = FALSE, # calc_rate.int w/ single rate
                      cr.int.mult = FALSE, # calc_rate.int w/ multiple rates
                      ar = FALSE,          # auto_rate any
                      ar.sing = FALSE,     # auto_rate w/ single rate
                      ar.mult = FALSE,     # auto_rate w/ multiple rates
                      crbg = FALSE,        # calc_rate.bg any
                      crbg.sing = FALSE,   # calc_rate.bg w/ single rate
                      crbg.mult = FALSE,   # calc_rate.bg w/ multiple rates
                      insp = FALSE){       # inspect

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
  # calc_rate.bg
  if(crbg) crbg.chk <- any(class(x) %in% "calc_rate.bg") else
    crbg.chk <- NULL
  # calc_rate.bg single rate
  if(crbg.sing) crbg.sing.chk <- (any(class(x) %in% "calc_rate.bg") && length(x$rate.bg) == 1) else
    crbg.sing.chk <- NULL
  # calc_rate.bg multiple rate
  if(crbg.mult) crbg.mult.chk <- (any(class(x) %in% "calc_rate.bg") && length(x$rate.bg) > 1) else
    crbg.mult.chk <- NULL
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
           crbg.chk,
           crbg.sing.chk,
           crbg.mult.chk,
           insp.chk)

  final_res <- any(res)

  return(final_res)
}
