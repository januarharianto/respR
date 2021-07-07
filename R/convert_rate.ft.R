#' Convert a unitless oxygen rate value from flowthrough respirometry to
#' absolute, mass-specific or area-specific rates
#'
#' `convert_rate.ft` converts a unitless rate derived from [`calc_rate.ft()`] or
#' [`adjust_rate.ft()`] into an absolute rate (i.e. whole specimen or whole
#' chamber), mass-specific rate (i.e. normalised by specimen mass), or
#' area-specific rate (i.e. normalised by specimen surface area) in any common
#' unit.
#'
#' By default, `convert_rate.ft` converts the `$rate` element from
#' `calc_rate.ft` objects, or the `$rate.adjusted` element from `adjust_rate.ft`
#' objects if these are entered as the `x` input. Alternatively, a numeric value
#' or vector of rates can be input as `x`.
#'
#' ## Units
#'
#' The `o2.unit` of the original data used to calculated the rate is required.
#' Concentration units should use only SI units (`L` or `kg`) for the
#' denominator, e.g. `"mg/L"`, `"mmol/kg"`. Percentage saturation of air or
#' oxygen is accepted, as are oxygen pressure units. See [`unit_args()`] for
#' details.
#'
#' An `output.unit` is also required. If left NULL, The default of `"mgO2/h"` is
#' used, or `"mgO2/h/kg"` or `"mgO2/h/m2"` if a `mass` or `area` respectively
#' has been entered. The `output.unit` must be in the sequence *O2-Time* (e.g.
#' `"mg/h"`) for absolute rates, *O2-Time-Mass* (e.g. `"mg/h/kg"`) for
#' mass-specific rates, and *O2-Time-Area* (e.g. `"mg/h/cm2"`) for surface
#' area-specific rates.
#'
#' Note, some oxygen input or output units require temperature (`t`) and
#' salinity (`S`) to perform conversions. For freshwater experiments, salinity
#' should be entered as zero (i.e. `S = 0`). Strictly speaking these also
#' require an atmospheric pressure (`P`) input. In reality, it has a relatively
#' minor effect within normal ranges, however the default value of 1.013253 bar
#' (standard pressure at sea level) can be changed if desired. See [unit_args()]
#' for details.
#'
#' The `flowrate.unit` is required and should be the units of the `flowrate`
#' used in `calc_rate.ft` to calculate the rate, and should be in the form of
#' volume (L, ml, or ul) per unit time (s,m,h,d), for example in `"L/s"`. Note,
#' the volume component does *NOT* represent the volume of the respirometer, and
#' the time component does *NOT* represent the units or recording interval of
#' the original data.
#'
#' The function uses an internal database and a fuzzy string matching algorithm
#' to accept various unit formatting styles. For example, `'mg/l', 'mg/L',
#' 'mgL-1', 'mg l-1', 'mg.l-1'` are all parsed the same. See [`unit_args()`] for
#' details of accepted units and their formatting. See also [`convert_val()`]
#' for simple conversion between units.
#'
#' ## Output
#'
#' Returns a `list` object containing the `$rate.input`, and converted rate(s)
#' in `$rate.output` in the `$output.unit`, as well as inputs and summary
#' elements.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints a single result, by default the first converted rate.
#' Others can be printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints summary table of all converted rates and metadata, or
#' those specified by the `pos` input. e.g. `summary(x, pos = 1:5)`. The summary
#' can be exported as a separate dataframe by passing `export = TRUE`.
#'
#' - `mean()`: calculates the mean of all converted rates, or those specified by
#' the `pos` input. e.g. `mean(x, pos = 1:5)` The mean can be exported as a
#' separate value by passing `export = TRUE`.
#'
#' @param x numeric value or vector, object of class [calc_rate.ft()] or
#'   [adjust_rate.ft()]. Contains the rate(s) to be converted.
#' @param o2.unit string. The dissolved oxygen unit of the original data used to
#'   determine the rate in `x`.
#' @param flowrate.unit string. The unit of the flowrate through the
#'   respirometer. See Details.
#' @param output.unit string. The output unit to convert the input rate to.
#'   Should be in the correct order: "O2/Time" or "O2/Time/Mass" or
#'   "O2/Time/Area".
#' @param mass numeric. Mass/weight in **kg**. This is the mass of the specimen
#'   if you wish to calculate mass-specific rates.
#' @param area numeric. Surface area in **m^2**. This is the surface area of the
#'   specimen if you wish to calculate surface area-specific rates.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Used in conversion of
#'   some oxygen units. Fresh water should be entered as `S = 0`.
#' @param t numeric. Temperature(°C). Defaults to NULL. Used in conversion of
#'   some oxygen units.
#' @param P numeric. Pressure (bar). Used in conversion of some oxygen units.
#'   Defaults to a standard value of 1.013253 bar.
#'
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' # Convert a single numeric rate to an absolute rate
#'
#' # Convert a single numeric rate to a mass-specific rate
#'
#' # Convert a single numeric rate to an area-specific rate
#'
#' # Convert a single rate derived via calc_rate to mass-specific
#'
#' # Convert multiple rates derived via auto_rate to area-specific
#'

convert_rate.ft <- function(x,
                            o2.unit = NULL,
                            flowrate.unit = NULL,
                            output.unit = NULL,
                            mass = NULL,
                            area = NULL,
                            S = NULL, t = NULL, P = 1.013253) {


  ## Save function call for output
  call <- match.call()

  # Validate inputs ---------------------------------------------------------

  # Validate x
  if (is.numeric(x)) {
    rate <- x
    message("convert_rate.ft: numeric input detected. Converting...")
  } else if ("calc_rate.ft" %in% class(x)) {
    rate <- x$rate
    message("convert_rate.ft: object of class 'calc_rate.ft' detected. Converting '$rate' element.")
  } else if ("adjust_rate.ft" %in% class(x)) {
    rate <- x$rate.adjusted
    message("convert_rate.ft: object of class 'adjust_rate.ft' detected. Converting '$rate.adjusted' element.")
  } else stop("convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")

  # o2.unit, flowrate.unit, required
  input.val(o2.unit, num = FALSE, req = TRUE,
                      msg = "convert_rate.ft: 'o2.unit'")
  input.val(flowrate.unit, num = FALSE, req = TRUE,
                      msg = "convert_rate.ft: 'flowrate.unit'")

  ## Apply output unit defaults
  if (is.null(output.unit) && is.null(mass) && is.null(area)) {
    warning("convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h'.",
            call. = F)
    output.unit <- "mg/h"
  }
  if (is.null(output.unit) && !is.null(mass) && is.null(area)) {
    warning("convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h/kg'.",
            call. = F)
    output.unit <- "mg/h/kg"
  }
  if (is.null(output.unit) && is.null(mass) && !is.null(area)) {
    warning("convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h/m2'.",
            call. = F)
    output.unit <- "mg/h/m2"
  }

  # Can't have both 'mass' and 'area' inputs
  if (!is.null(mass) && !is.null(area))
    stop("convert_rate.ft: Cannot have inputs for both 'mass' and 'area'.")

  # Validate o2.unit & flowrate.unit
  oxy <- verify_units(o2.unit, "o2")
  flow <- verify_units(flowrate.unit, "flow")

  # Validate output.unit
  out.unit <- as.matrix(read.table(text = gsub("(?:-1|[/.[:space:]])+",
                                               " ", output.unit), header = FALSE))

  ## is it a specific rate (mass or area)?
  is.spec <- length(out.unit) == 3

  ## Is output unit mass or area specific rate?
  if(is.spec){
    if(!is.null(mass) && is.null(area)){
      is.mass.spec <- TRUE
      is.area.spec <- FALSE
    } else if(!is.null(area) && is.null(mass)){
      is.mass.spec <- FALSE
      is.area.spec <- TRUE
    } else if(is.null(area) && is.null(mass)){
      stop("convert_rate.ft: 'output.unit' requires a value for 'mass' or 'area'")
    }
  } else {
    is.mass.spec <- FALSE
    is.area.spec <- FALSE
  }

  A <- verify_units(out.unit[1], "o1")
  B <- verify_units(out.unit[2], "time")
  if(is.spec){
    if (is.mass.spec) {
      C <- verify_units(out.unit[3], "mass")
      out.unit <- as.matrix(data.frame(A, B, C))
    } else if (is.area.spec) {
      C <- verify_units(out.unit[3], "area")
      out.unit <- as.matrix(data.frame(A, B, C))
    }
  } else out.unit <- as.matrix(data.frame(A, B))

  # Verify 'mass' input
  if (!is.mass.spec && is.numeric(mass))
    stop("convert_rate.ft: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")

  # Verify 'area' input
  if (!is.area.spec && is.numeric(area))
    stop("convert_rate.ft: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")

  # Format unit strings to look nicer
  o2.unit <- stringr::str_replace(oxy, "\\..*", "")
  flow.unit <- stringr::str_replace(flow, "\\..*", "")

  ## Add "O2" to output O2 unit string for clarity
  output.unit <- stringr::str_replace(out.unit, "\\..*", "")
  output.unit[1] <- paste0(output.unit[1], "O2")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("mmol.o2", "umol.o2", "mol.o2")) {
    RO2 <- convert_DO(rate, oxy, "mmol/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- convert_DO(rate, oxy, "mg/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "mg.o2", A)
  } else if (A == "ml.o2") {
    RO2 <- convert_DO(rate, oxy, "mL/L", S, t, P)
    RO2 <- adjust_scale(RO2$output, "ml.o2", A)
  }

  ## Approach here -
  ## Break down flow rate to volume and time components.
  ## e.g. L and s
  ## Then the rate is RO2 per 1s in 1L
  ##
  # Then, convert time unit
  # time of flow rate to time
  time_component <- flow_unit_parse(flow.unit, "time")
  RO2 <- adjust_scale(RO2, time_component, B)

  # Then, scale to volume
  # vol of flow rate to volume in L
  vol.component <- flow_unit_parse(flow.unit, "vol") # what is the flow vol unit?
  volume <- adjust_scale(1, vol.component, "l.vol") # adjust to 1 litres
  VO2 <- RO2 * volume

  # Then, scale to mass or area
  if (is.mass.spec) {
    # adjust mass multiplier
    multm <- adjust_scale(mass, "kg.mass", C)
    rate.m.spec <- VO2/multm
    rate.a.spec <- NA
    VO2.out <- rate.m.spec
  } else  if (is.area.spec) {
    # adjust area multiplier
    multm <- adjust_scale_area(area, "m2.area", C)
    rate.m.spec <- NA
    rate.a.spec <- VO2/multm
    VO2.out <- rate.a.spec
  } else {
    rate.m.spec <- NA
    rate.a.spec <- NA
    VO2.out <- VO2
  }

  # Generate output ---------------------------------------------------------

  ## so data.table will accept them
  if(is.null(mass)) mass <- NA
  if(is.null(area)) area <- NA

  summary <- data.table::data.table(rate.input = rate,
                                    o2.unit = o2.unit,
                                    flowrate.unit = flowrate.unit,
                                    mass = mass,
                                    area = area,
                                    rate.abs = VO2,
                                    rate.m.spec = rate.m.spec,
                                    rate.a.spec = rate.a.spec,
                                    output.unit = output.unit,
                                    rate.output = VO2.out)

  out <- list(call = call,
              inputs = list(x = x,
                            o2.unit = o2.unit,
                            flowrate.unit = flowrate.unit,
                            output.unit = output.unit,
                            mass = mass,
                            area = area,
                            S = S, t = t, P = P),
              summary = summary,
              rate.input = rate,
              output.unit = summary$output.unit[1],
              rate.output = summary$rate.output)

  class(out) <- "convert_rate.ft"
  return(out)
}


# S3 Generics -------------------------------------------------------------

#' @export
print.convert_rate.ft <- function(x, pos = NULL, ...) {
  cat("\n# print.convert_rate.ft # ---------------\n")
  if(is.null(pos)) pos <- 1
  if(length(pos) > 1)
    stop("print.convert_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate.output))
    stop("print.convert_rate.ft: Invalid 'pos' rank: only ", length(x$rate.output), " rates found.")
  cat("Rank", pos, "of", length(x$rate.output), "result(s)\n")
  cat("Input:\n")
  print(x$summary$rate.input[pos])
  print(c(x$summary$o2.unit[1], x$summary$flowrate.unit[1]))
  cat("Converted:\n")
  print(x$rate.output[pos])
  print(x$output.unit[1])
  cat("\n")
  if(length(x$rate.output) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' @export
summary.convert_rate.ft <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate.output)))
    stop("summary.convert_rate.ft: Invalid 'pos' rank: only ", length(object$rate.output), " rates found.")
  cat("\n# summary.convert_rate.ft # -------------\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all rate results:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of rate results from entered 'pos' rank(s):")
    cat("\n")
    cat("\n")
  }
  out <- cbind(rank = pos, object$summary[pos,])
  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' @export
mean.convert_rate.ft <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.convert_rate.ft # ----------------\n")
  if(!is.null(pos) && any(pos > length(x$rate.output)))
    stop("mean.convert_rate.ft: Invalid 'pos' rank: only ", length(x$rate.output), " rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate.output)
    cat("Mean of all rate results:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate.output[pos]) == 1)
    message("Only 1 rate found. Returning mean rate anyway...")
  cat("\n")

  n <- length(x$rate.output[pos])
  out <- mean(x$rate.output[pos])
  cat("Mean of", n, "output rates:\n")
  print(out)
  print(x$output.unit)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(x))
}



#' Extracts time and volume units from flowrate unit already parsed by verify_units
#'
#' @param unit flowrate unit input to be parsed
#' @param which parse which component of unit? "time" or "vol"
#' @keywords internal
#' @export
flow_unit_parse <- function(unit, which){

  if(which == "vol"){
    if(unit %in% c("ul/s", "ul/m", "ul/h", "ul/d")) out <- "ul.vol" else
      if(unit %in% c("ml/s", "ml/m", "ml/h", "ml/d")) out <- "ml.vol" else
        if(unit %in% c("l/s", "l/m", "l/h", "l/d")) out <- "l.vol"
  }

  if(which == "time"){
    if(unit %in% c("ul/s", "ml/s", "l/s")) out <- "sec.time" else
      if(unit %in% c("ul/m", "ml/m", "l/m")) out <- "min.time" else
        if(unit %in% c("ul/h", "ml/h", "l/h")) out <- "hour.time" else
          if(unit %in% c("ul/d", "ml/d", "l/d")) out <- "day.time"
  }
  return(out)
}

