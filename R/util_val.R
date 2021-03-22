## For storing functions which validate inputs

# Verify the 'by' input for calc_rate etc.
verify_by <- function(by){
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

  if (is.null(by)) stop("'by' input is NULL")
  else if(by %in% time_variations) by <- "time"
  else if(by %in% ox_variations) by <- "o2"
  else if(by %in% row_variations) by <- "row"
  else if(by %in% prop_variations) by <- "proportion"
  else stop("'by' input not recognised")

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

## Column input checks
## - req = input is required (i.e can't be NULL)
## - int = should only be integers
## - max = for max total number of column inputs allowed (e.g. for time, max = 1, as should never have more than 1 time column)
## - range = for specific column range allowed (e.g. c(1,ncol(df)))
## - msg = string to add custom message

column.val <- function(input, int = TRUE, req = FALSE, max = 1, range = NULL, msg = NULL){

## check if an input required
is_null <- is.null(input)
## check integer
are_int <- all(sapply(input, function(z) z %% 1 == 0))
## check not greater than max
below_max <-  length(input) <= max
## check within range
in_range <- all(sapply(input, function(z) dplyr::between(z, range[1], range[2])))

if(req && is_null) stop(glue::glue("{msg}Input is required."))
if(int && !are_int) stop(glue::glue("{msg}Some column inputs are not integers."))
if(!below_max) stop(glue::glue("{msg}Input is greater than the maximum allowed number of columns."))
if(!in_range) stop(glue::glue("{msg}Some column inputs are out of range of allowed data columns."))

}

# df = flowthrough.rd
# input = time
# time = 1:2
# outflow.o2 = 3
# inflow.o2 = 4
# inflow.o2.conc = NULL
# delta.o2 = NULL
# plot = FALSE
