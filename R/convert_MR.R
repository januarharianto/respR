#' Convert between units of absolute, mass-specific, or area-specific metabolic
#' rates
#'
#' Converts metabolic rates to a different unit. These can be absolute rates
#' (i.e. whole chamber or whole specimen e.g. `mg/h`), mass-specific rates (i.e.
#' normalised by specimen mass  e.g. `mg/h/kg`), or area-specific rates (i.e.
#' normalised by specimen surface area e.g. `mg/h/cm2`). Input rates can be a
#' numeric value or vector, in which case the converted rates are output as a
#' numeric of the same length in the new units. Alternatively, input can be an
#' object of class `convert_rate()` or `convert_rate.ft()`, in which case a new
#' `convert_rate` or `convert_rate.ft` object is returned with all rates in the
#' `$summary` and other elements converted to the new units. This allows you to
#' convert results of analyses to a different unit without having to repeat the
#' entire analysis.
#'
#' ## Units
#'
#' Units are specified using `from` and `to`. These should be in the sequence
#' *Oxygen-Time* (e.g. `"mg/h"`) for absolute rates, *Oxygen-Time-Mass* (e.g.
#' `"mg/h/kg"`) for mass-specific rates, and *Oxygen-Time-Area* (e.g.
#' `"mg/h/cm2"`) for surface area-specific rates. If `x` is a `convert_rate` or
#' `convert_rate.ft` object the `from` unit is extracted automatically.
#'
#' A fuzzy string matching algorithm is used to accept various unit formatting
#' styles. For example, `"mg/h"`, `"mg/H"`, `"mg hr-1"`, `"milligram per hour"`
#' are all parsed the same. See `unit_args()` for details of accepted units and
#' their formatting.
#'
#' Note some units require salinity (`S`) and temperature (`t`) to perform the
#' conversion. For freshwater experiments, salinity should be entered as zero
#' (i.e. `S = 0`). These conversions also require the atmospheric pressure
#' (`P`). If not entered the default value of 1.013253 bar (standard pressure at
#' sea level) is used. If `x` is a `convert_rate` or `convert_rate.ft` object,
#' `S`, `t`, and `P` are extracted automatically if they are present (they may
#' not be if the original rate conversion did not require them). They are also
#' saved to the `$inputs` element of the output object.
#'
#' ## More details
#'
#' For `convert_rate` or `convert_rate.ft` inputs the primary `$rate.output`
#' element is converted to the new unit and the `$output.unit` also updated to
#' this new unit. These columns are also updated in the `$summary` table, and in
#' addition the `rate.abs` column and, if relevant, the `rate.m.spec` or
#' `rate.a.spec` column. Note, the `$call` element is updated to the
#' `convert_MR` call and the original call to `convert_rate` or
#' `convert_rate.ft` replaced. The `$inputs` element will still contain the
#' original inputs, with the `output.unit` updated to the new `to` unit. In
#' addition `S`, `t`, and `P` are added if they weren't already present.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return If the `x` input rates are a numeric value or vector, output is a
#'   numeric value or vector of the same length. If `x` is a `convert_rate` or
#'   `convert_rate.ft` object, output is a new `convert_rate` or
#'   `convert_rate.ft` object with all rates in the `$summary` table,
#'   `$rate.output` and `$output.unit` elements converted to the new units. This
#'   allows you to convert results of analyses to a different unit without
#'   having to repeat the entire analysis.
#'
#' @param x numeric value or vector, or object of class `convert_rate` or
#'   `convert_rate.ft`. Contains the rate(s) to be converted.
#' @param from string. The unit of the input metabolic rate(s). Should be in the
#'   correct order: Oxygen/Time, Oxygen/Time/Mass or Oxygen/Time/Area. If `x` is
#'   a `convert_rate` or `convert_rate.ft` object this is extracted
#'   automatically and any input specified via `from` is ignored.
#' @param to string. The unit to convert the metabolic rate(s) to. Should be in
#'   the correct order: Oxygen/Time, Oxygen/Time/Mass or Oxygen/Time/Area. See
#'   `unit_args()`.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Used in conversion of
#'   some oxygen units. Freshwater should be entered as `S = 0`. If `x` is a
#'   `convert_rate` or `convert_rate.ft` object this is extracted automatically
#'   if present and any other input ignored.
#' @param t numeric. Temperature(°C). Defaults to NULL. Used in conversion of
#'   some oxygen units. If `x` is a `convert_rate` or `convert_rate.ft` object
#'   this is extracted automatically if present and any other input ignored.
#' @param P numeric. Pressure (bar). Used in conversion of some oxygen units.
#'   Defaults to a standard value of 1.013253 bar. If `x` is a `convert_rate` or
#'   `convert_rate.ft` object this is extracted automatically if present and any
#'   other input ignored.
#' @param quiet logical. Suppresses the summary of the converted rates printed
#'   to the console. Default is `FALSE`.
#'
#' @export
#'
#' @examples
#' # Convert a numeric absolute rate to a different unit
#' convert_MR(-0.09, from = 'mg/min', to = 'umol/hr')
#'
#' # Convert a vector of absolute rates to a different unit
#' convert_MR(c(-0.090, -0.081, -0.098),
#'            from = 'mg/min', to = 'umol/hr')
#'
#' # Convert to a unit which requires S, t, & P
#' convert_MR(-0.09, from = 'mg/min', to = 'ml/hour',
#'            S = 0, t = 20, P = 1.01)
#'
#' # Convert mass-specific rates
#' convert_MR(-0.09, from = 'mg/min/g', to = 'ml/hour/kg',
#'            S = 0, t = 20, P = 1.01)
#'
#' # Convert area-specific rates
#' convert_MR(-0.09, from = 'mg/min/mm2', to = 'ml/hour/cm2',
#'            S = 0, t = 20, P = 1.01)
#'
#' # Convert from units largely only used in older papers.
#' # E.g. cubic cm (e.g. Tang 1933, Head 1962)
#' convert_MR(0.1, from = 'cc/hr/gm', to = 'mg/hr/g',
#'            S = 30, t = 20, P = 1.01)
#' convert_MR(0.6, from = 'cm3/hr', to = 'mg/hr',
#'            S = 28, t = 12, P = 1.01)
#' # uL (e.g. Zeuthen 1953, Newell & Northcroft 1967)
#' convert_MR(400, from = 'ul/hr', to = 'mg/hr',
#'            S = 30, t = 15, P = 1.01)
#' convert_MR(0.5, from = 'ul/hr/mg', to = 'mg/hr/g',
#'            S = 0, t = 20, P = 1.01)
#' # mm3 (e.g. Newell & Roy 1973)
#' convert_MR(2.7, from = 'mm3/hr', to = 'mg/hr',
#'            S = 30, t = 15, P = 1.01)
#'
#' # Convert rates in a 'convert_rate' object. This avoids having to repeat
#' # an entire analysis to see the output in different units.
#'
#' # Make a convert_rate object
#' cnv_rt.obj <- urchins.rd %>%
#'   auto_rate() %>%
#'   convert_rate(oxy.unit = "mg/L",
#'                time.unit = "min",
#'                output.unit = "mg/hr/kg",
#'                volume = 1,
#'                mass = 0.05)
#'
#' # Now convert all results to "umol/min/g".
#' # The 'from' units are identified automatically from the object.
#' cnv_rt.obj.new <- convert_MR(cnv_rt.obj,
#'                              to = "umol/min/g")
#'
#' # Compare the two:
#' summary(cnv_rt.obj)
#' summary(cnv_rt.obj.new)

convert_MR <- function(x, from = NULL, to = NULL,
                       S = NULL, t = NULL, P = NULL,
                       quiet = FALSE) {

  ## Save function call for output
  call <- match.call()

  # Validate inputs ---------------------------------------------------------

  if(!(class.val(x, num = TRUE, cnvr = TRUE, cnvr.ft = TRUE)))
    stop("convert_MR: The 'x' input should be a numeric value or vector, or 'convert_rate' or 'convert_rate.ft' object.", call. = FALSE)

  if(is.null(to))
    stop("convert_MR: Please specify a 'to' unit.", call. = FALSE)

  # Extract rates ------------------------------------------------------------

  if(any(class(x) %in% c("convert_rate", "convert_rate.ft"))) {

    # stop if empty
    if(length(x$rate.output) == 0) stop("convert_MR: No rates found in input object.", call. = FALSE)

    # ignore from input
    if(!(is.null(from))) message(glue::glue("convert_MR: 'from' input ignored. For '{class(x)}' objects the input unit is extracted automatically."))

    # extract t,S,P
    # if entered and in x but not same, then message ignored
    if(!(is.null(x$inputs$S)) && !(is.null(S)) && !(identical(x$inputs$S, S)))
      message(glue::glue("convert_MR: 'S' input ignored. For '{class(x)}' objects 'S' is extracted automatically if it is present."))
    # if in x then extract
    if(!(is.null(x$inputs$S))) S <- x$inputs$S

    if(!(is.null(x$inputs$t)) && !(is.null(t)) && !(identical(x$inputs$t, t)))
      message(glue::glue("convert_MR: 't' input ignored. For '{class(x)}' objects 't' is extracted automatically if it is present."))
    if(!(is.null(x$inputs$t))) t <- x$inputs$t

    if(!(is.null(x$inputs$P)) && !(is.null(P)) && !(identical(x$inputs$P, P)))
      message(glue::glue("convert_MR: 'P' input ignored. For '{class(x)}' objects 'P' is extracted automatically if it is present."))
    if(!(is.null(x$inputs$P))) P <- x$inputs$P

    obj <- TRUE
    input_class <- class(x) # class for console print out message
    from <- x$output.unit
    rates <- x$rate.output
  } else {
    obj <- FALSE
    input_class <- "numeric"
    rates <- x
  }

  if(is.null(from))
    stop("convert_MR: Please specify a 'from' unit.", call. = FALSE)

  P <- StP.val(from, "mr", S, t, P, P.chk = FALSE, msg = "convert_MR")
  P <- StP.val(to, "mr", S, t, P, P.chk = TRUE, msg = "convert_MR")

  # Extract and validate units ----------------------------------------------

  # Separate the units
  from.sep <- as.matrix(read.table(text = gsub(unit.sep.rgx,
                                               " ", from), header = FALSE))
  to.sep <- as.matrix(read.table(text = gsub(unit.sep.rgx,
                                             " ", to), header = FALSE))
  # Should be same length
  if(length(from.sep) != length(to.sep))
    stop("convert_MR: Parsing of 'from' and/or 'to' units has failed. Check unit_args() for accepted units and formatting. They should *both* be absolute, mass-specific, or area-specific rates and use same general formatting.", call. = FALSE)

  # Unit types
  from.types <- c()
  from.types[1] <- unit_type_o1(from.sep[1], msg = "convert_MR")
  from.types[2] <- unit_type(from.sep[2], msg = "convert_MR")
  if(length(from.sep) == 3) from.types[3] <- unit_type(from.sep[3], msg = "convert_MR")

  to.types <- c()
  to.types[1] <- unit_type_o1(to.sep[1], msg = "convert_MR")
  to.types[2] <- unit_type(to.sep[2], msg = "convert_MR")
  if(length(to.sep) == 3) to.types[3] <- unit_type(to.sep[3], msg = "convert_MR")

  # Should all match
  if(!(all(from.types == to.types)))
    stop("convert_MR: 'from' and 'to' unit types conflict. \nRate unit strings should be in correct order: O2/Time or O2/Time/Mass or O2/Time/Area.\nSee unit_args() for details.", call. = FALSE)

  # Overall unit type
  if(length(from.types) == 2) rate.type <- "abs" else
    if(from.types[3] == "mass") rate.type <- "mass.spec" else
      if(from.types[3] == "area") rate.type <- "area.spec" else
        stop("convert_MR: unit '", from.sep[3], "' not recognised as a mass or area unit. Check it is valid for the input or output type. \nOutput rate unit strings should be in correct order: O2/Time or O2/Time/Mass or O2/Time/Area.\nSee unit_args() for details.", call. = F)

  # verify
  from.sep[1] <- units.val(from.sep[1], "o1")
  from.sep[2] <- units.val(from.sep[2], "time")
  if(rate.type == "mass.spec") from.sep[3] <- units.val(from.sep[3], "mass") else
    if(rate.type == "area.spec") from.sep[3] <- units.val(from.sep[3], "area")

  to.sep[1] <- units.val(to.sep[1], "o1")
  to.sep[2] <- units.val(to.sep[2], "time")
  if(rate.type == "mass.spec") to.sep[3] <- units.val(to.sep[3], "mass") else
    if(rate.type == "area.spec") to.sep[3] <- units.val(to.sep[3], "area")


  # Convert -----------------------------------------------------------------

  if(rate.type == "abs"){
    # adjust rate to per hour
    rate.adj <- adjust_scale(rates, from.sep[2], "hr.time")

    # parse oxygen unit - make it a per l conc - then we use 1 l as volume
    ox.unit <- units.val(paste0(from.sep[1], "/l"), "o2")

    # S,t,P missing errors
    # this also applies default P if it is NULL
    #P <-  StP.val(ox.unit, "oxy", S, t, P, msg = "convert_MR")

    # adjust rate to mg/l oxygen
    rate.adj <- suppressMessages(suppressWarnings(convert_DO(rate.adj, ox.unit, "mg/l", S = S, t = t, P = P)))

    # now convert to output unit
    rate.out <- suppressMessages(suppressWarnings(convert_rate(rate.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = to,
                             volume = 1, mass = NULL, area = NULL,
                             S = S, t = t, P = P,
                             plot = FALSE)))
  }

  if(rate.type == "mass.spec"){
    # adjust rate to per hour
    rate.adj <- adjust_scale(rates, from.sep[2], "hr.time")

    # adjust rate to per kg
    # Really don't understand why this has to be opposite way round to the above
    # Otherwise rates are off by 1000x1000
    rate.adj <- adjust_scale(rate.adj, "kg.mass", from.sep[3])

    # parse oxygen unit - make it a per l conc - then we use 1 l as volume
    ox.unit <- units.val(paste0(from.sep[1], "/l"), "o2")

    # Check S,t,P
    #P <- StP.val(ox.unit, "oxy", S, t, P, msg = "convert_MR")

    # adjust rate to mg/l oxygen
    rate.adj <- suppressMessages(suppressWarnings(convert_DO(rate.adj, ox.unit, "mg/l", S = S, t = t, P = P)
    ))

    # now convert to output unit
    rate.out <- suppressMessages(suppressWarnings(convert_rate(rate.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = to,
                                                               volume = 1, mass = 1, area = NULL,
                                                               S = S, t = t, P = P,
                                                               plot = FALSE)))

    # abs rates for summary table
    if(obj) {
      rates.abs <- x$summary$rate.abs
      rate.abs.adj <- adjust_scale(rates.abs, from.sep[2], "hr.time")
      rate.abs.adj <- suppressMessages(suppressWarnings(convert_DO(rate.abs.adj, ox.unit, "mg/l", S = S, t = t, P = P)))
      unit.abs <- paste0(to.sep[1], "/", to.sep[2])
      unit.abs <- gsub(".o2", "", unit.abs)
      unit.abs <- gsub(".time", "", unit.abs)

      rate.abs.adj <- suppressMessages(suppressWarnings(convert_rate(rate.abs.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = unit.abs,
                                                                     volume = 1, mass = NULL, area = NULL,
                                                                     S = S, t = t, P = P,
                                                                     plot = FALSE)))$rate.output
    }
  }

  if(rate.type == "area.spec"){
    # adjust rate to per hour
    rate.adj <- adjust_scale(rates, from.sep[2], "hr.time")

    # adjust rate to per m2
    rate.adj <- adjust_scale_area(rate.adj, "m2.area", from.sep[3])

    # parse oxygen unit - make it a per l conc - then we use 1 l as volume
    ox.unit <- units.val(paste0(from.sep[1], "/l"), "o2")
    # errors
    #P <- StP.val(ox.unit, "oxy", S, t, P, msg = "convert_MR")
    # adjust rate to mg/l oxygen
    rate.adj <- suppressMessages(suppressWarnings(convert_DO(rate.adj, ox.unit, "mg/l", S = S, t = t, P = P)
    ))
    # now convert to output unit
    rate.out <- suppressMessages(suppressWarnings(convert_rate(rate.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = to,
                                                               volume = 1, mass = NULL, area = 1,
                                                               S = S, t = t, P = P,
                                                               plot = FALSE)))

    # abs rates for summary table
    if(obj) {
      rates.abs <- x$summary$rate.abs
      rate.abs.adj <- adjust_scale(rates.abs, from.sep[2], "hr.time")
      rate.abs.adj <- suppressMessages(suppressWarnings(convert_DO(rate.abs.adj, ox.unit, "mg/l", S = S, t = t, P = P)))
      unit.abs <- paste0(to.sep[1], "/", to.sep[2])
      unit.abs <- gsub(".o2", "", unit.abs)
      unit.abs <- gsub(".time", "", unit.abs)

      rate.abs.adj <- suppressMessages(suppressWarnings(convert_rate(rate.abs.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = unit.abs,
                                                                     volume = 1, mass = NULL, area = NULL,
                                                                     S = S, t = t, P = P,
                                                                     plot = FALSE)))$rate.output
    }
  }

  # Assemble output ---------------------------------------------------------

  rate.out.val <- rate.out$rate.output
  rate.out.un <- rate.out$output.unit

  if(obj) {
    # modify convert_rate/convert_rate.ft object
    out <- x

    # add t,S,P to inputs
    # - only if they are NULL
    if(!(is.null(S))) out$inputs$S <- S
    if(!(is.null(t))) out$inputs$t <- t
    if(!(is.null(P))) out$inputs$P <- P

    # rates and units
    out$rate.output <- rate.out.val
    out$output.unit <- rate.out.un
    out$summary$rate.output <- rate.out.val
    out$summary$output.unit <- rate.out.un
    out$inputs$output.unit <- rate.out.un

    if(rate.type == "abs"){
      out$summary$rate.abs <- rate.out.val
    } else if(rate.type == "mass.spec"){
      out$summary$rate.m.spec <- rate.out.val
      out$summary$rate.abs <- rate.abs.adj
    } else if(rate.type == "area.spec"){
      out$summary$rate.a.spec <- rate.out.val
      out$summary$rate.abs <- rate.abs.adj
    }

    # replace call
    out$call <- call

  } else {
    out <- rate.out.val
  }

  # clean format input unit
  rate.in.un <- from.sep
  rate.in.un <- sapply(rate.in.un, function(z) stringr::str_replace(z, "\\..*", ""))
  if(length(rate.in.un) == 2) rate.in.un <- paste0(rate.in.un[1], "O2", "/", rate.in.un[2]) else
    rate.in.un <- paste0(rate.in.un[1], "O2", "/", rate.in.un[2], "/", rate.in.un[3])

  # print result
  if(!(quiet)) convert_MR_print(rates, rate.out.val, rate.in.un, rate.out.un, input_class)

  # return
  return(invisible(out))

} #end fn


#' Print results of convert_MR
#' @param oldrates The original rates
#' @param newrates The converted rates
#' @param from The original unit
#' @param to The converted unit
#' @param input_class Class of the input (numeric, convert_rate, convert_rate.ft)
#' @keywords internal
convert_MR_print <- function(oldrates, newrates, from, to, input_class) {
  cat("\n# print.convert_MR # --------------------\n\n")
  cat(glue::glue("Converting rates from '{input_class}' input...\n\n"))

  ## print up to first 100
  if(length(oldrates) > 100) {
    message("Printing first 100 rates only...")
    oldrates <- oldrates[1:100]
    newrates <- newrates[1:100]
  }

  cat("\n")
  cat(paste0("Input: ", "\"", from, "\"", "\n"))
  print(oldrates)
  cat("\n")
  cat(paste0("Converted: ", "\"", to, "\"", "\n"))
  print(newrates)
  cat("\n")
  cat("-----------------------------------------\n")
}

