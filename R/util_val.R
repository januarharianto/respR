## For storing functions which validate inputs

# Verify the 'by' input for calc_rate etc.
# req - indicates if a 'by' input is required
# which - which 'by' methods does the function support?
verify_by <- function(by, req = TRUE,
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

  if (req && is.null(by)) stop(glue::glue("{msg} 'by' input is NULL"))
  else if("t" %in% which && by %in% time_variations) by <- "time"
  else if("o" %in% which && by %in% ox_variations) by <- "o2"
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
## - int = should only be integers
## - max = for max total number of column inputs allowed (e.g. for time, max = 1, as should never have more than 1 time column)
## - range = for specific column range allowed (e.g. c(1,ncol(df)))
## - msg = string to add custom message

column.val <- function(input, int = TRUE, req = FALSE, max = 1, range = c(-Inf,Inf), msg = ""){

  ## check if an input required
  is_null <- is.null(input)
  ## check integer
  are_int <- all(sapply(input, function(z) z %% 1 == 0))
  ## check not greater than max
  below_max <-  length(input) <= max
  ## check within range
  in_range <- all(sapply(input, function(z) dplyr::between(z, range[1], range[2])))

  if(req && is_null) stop(glue::glue("{msg}input is required."))
  if(int && !are_int) stop(glue::glue("{msg}some column inputs are not integers."))
  if(!below_max) stop(glue::glue("{msg}input is greater than the maximum allowed number of columns."))
  if(!in_range) stop(glue::glue("{msg}some column inputs are out of range of allowed data columns."))

}

# df = flowthrough.rd
# input = time
# time = 1:2
# outflow.o2 = 3
# inflow.o2 = 4
# inflow.o2.conc = NULL
# delta.o2 = NULL
# plot = FALSE


## Validation of numeric inputs
## - req = input is required (i.e. can't be NULL)
## - int = should only be integers?
## - max = for max total number of inputs allowed (e.g. flowrate should only be 1 value)
## - min = for min total number of inputs allowed
## - range = for specific range allowed (e.g. c(0,1))
## - msg = string to add custom message

numeric.val <- function(input, num = TRUE, int = FALSE, req = FALSE,
                        max = 1, min = 1, range = c(-Inf,Inf), msg = ""){

  ## check numeric
  is_num <- is.numeric(input)
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
    if(int && !are_int) stop(glue::glue("{msg} one or more inputs are not integers."))
    if(!below_max) stop(glue::glue("{msg} only {max} inputs allowed."))
    if(!above_min) stop(glue::glue("{msg} at least {min} inputs required."))
    if(!in_range) stop(glue::glue("{msg} one or more inputs are outside the range of allowed values."))
  }
}
