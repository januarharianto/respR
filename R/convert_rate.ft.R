#' Convert a unitless oxygen rate value from flowthrough respirometry to
#' absolute, mass-specific or area-specific rates
#'
#' `convert_rate.ft` converts a unitless rate derived from [`calc_rate.ft()`] or
#' [`adjust_rate.ft()`] into an absolute rate (i.e. whole specimen or whole
#' chamber), mass-specific rate (i.e. normalised by specimen mass), or
#' area-specific rate (i.e. normalised by specimen surface area) in any common
#' unit. These should be rates calculated as an oxygen delta (inflow minus
#' outflow oxygen) multiplied by the flowrate.
#'
#' By default, `convert_rate.ft` converts the `$rate` element from
#' `calc_rate.ft` objects, or the `$rate.adjusted` element from `adjust_rate.ft`
#' objects if these are entered as the `x` input. Alternatively, a numeric value
#' or vector of rates can be input as `x`.
#'
#' ## Units
#'
#' The `oxy.unit` of the original raw data used to calculated the rate is
#' required. Concentration units should use only SI units (`L` or `kg`) for the
#' denominator, e.g. `"mg/L"`, `"mmol/kg"`. Percentage saturation of air or
#' oxygen is accepted, as are oxygen pressure units. See [`unit_args()`] for
#' details.
#'
#' An `output.unit` is also required. If left `NULL`, The default of `"mgO2/h"`
#' is used, or `"mgO2/h/kg"` or `"mgO2/h/m2"` if a `mass` or `area` respectively
#' has been entered. The `output.unit` must be in the sequence *Oxygen-Time*
#' (e.g. `"mg/h"`) for absolute rates, *Oxygen-Time-Mass* (e.g. `"mg/h/kg"`) for
#' mass-specific rates, and *Oxygen-Time-Area* (e.g. `"mg/h/cm2"`) for surface
#' area-specific rates.
#'
#' Note, some oxygen input or output units require temperature (`t`) and
#' salinity (`S`) to perform conversions. For freshwater experiments, salinity
#' should be entered as zero (i.e. `S = 0`).
#'
#' Strictly speaking the atmospheric pressure (`P`) should also be supplied. If
#' not, the default value of 1.013253 bar (standard pressure at sea level) is
#' used. In most locations which have a normal range (outside extreme weather
#' events) of around 20 millibars, any variability in pressure will have a
#' relatively minor effect on dissolved oxygen, and even less on calculated
#' rates. However, we would encourage users to enter the actual value if they
#' know it, or use historical weather data to find out what it was on the day.
#' See [unit_args()] for details.
#'
#' The `flowrate.unit` is required and should be the units of the `flowrate`
#' used in `calc_rate.ft` to calculate the rate, and should be in the form of
#' volume (L, ml, or ul) per unit time (s,m,h,d), for example in `"L/s"`. Note,
#' the volume component does *NOT* represent the volume of the respirometer, and
#' the time component does *NOT* represent the units or recording interval of
#' the original raw data.
#'
#' The function uses a fuzzy string matching algorithm to accept various unit
#' formatting styles. For example, `"mg/l"`, `"mg/L"`, `"mgL-1"`, `"mg l-1"`,
#' `"mg.l-1"` are all parsed the same. See [`unit_args()`] for details of
#' accepted units and their formatting. See also [`convert_val()`] for simple
#' conversion between non-oxygen units.
#'
#' ## Plot
#'
#' Plotting provides three ways of visualising converted rates (or a selection
#' of them using `pos`), chosen using `type`. This is mostly useful only if you
#' have extracted multiple rates (see `calc_rate.ft()`). The default is `plot =
#' FALSE` to prevent plots being produced for every single conversion.
#' `convert_rate.ft` objects can only be plotted if and `inspect.ft` object was
#' used as the input in `calc_rate.ft`. In other words, converted rates from
#' numeric inputs cannot be plotted.
#'
#' `type = "full"` (the default) plots a grid of up to 20 plots with each rate
#' (i.e. region of averaged delta values) highlighted on a plot of delta oxygen
#' values, with the converted rate value in the title. Values on the axes - time
#' (bottom), row (top), and oxygen delta (left) - are in the units of the
#' original raw data. Rates are plotted in order of how they appear in the
#' summary table up to the first 20 rows, unless different rows have been
#' specified via `pos`.
#'
#' `type = "rate"` plots the entire data timeseries, that is the outflow and
#' inflow oxygen (if used) on the upper plot, with delta oxygen on the middle
#' plot or as the upper plot if delta oxygen values have been entered in
#' `inspect.ft`. The lower plot is the output rate values in the chosen output
#' units. Each rate is plotted against the middle of the region used to
#' determine it (i.e. region of averaged delta values). `pos` can be used to
#' select a range of rates (i.e. summary table rows) to show in the lower plot
#' (default is all).
#'
#' `type = "overlap"` visualises where regression results in the summary table
#' occur in relation to the original dataset to help understand how they are
#' distributed or may overlap. The top plot is the entire data timeseries, that
#' is the outflow and inflow oxygen (if used) on the upper plot, with delta
#' oxygen on the middle plot or as the upper plot if delta oxygen values have
#' been entered in `inspect.ft`. The bottom plot is the region of the data each
#' rate has been calculated over (i.e. region of averaged delta values). The
#' y-axis represents the position (i.e. row) of each in the summary table
#' descending from top to bottom. If no reordering or selection has been
#' performed, this will usually be equivalent to the `$rank` column, but note as
#' reordering or selection is performed rank and summary table position will not
#' necessarily be equivalent. One result (summary table row) can be highlighted,
#' the default being `highlight = 1`. `pos` can be used to select a range of
#' summary rows to plot in the lower overlap plot.
#'
#' Other options:
#'
#' `legend = FALSE` will suppress plot labels, `pos` selects summary rates to
#' plot, `quiet` suppresses console messages.
#'
#' ## S3 Generic Functions
#'
#' Saved output objects can be used in the generic S3 functions `print()`,
#' `summary()`, and `mean()`.
#'
#' - `print()`: prints a single result, by default the first converted rate.
#' Others can be printed by passing the `pos` input. e.g. `print(x, pos = 2)`
#'
#' - `summary()`: prints the output `$summary` table of converted rates and
#' metadata. Specific rows can be specified with the `pos` input. e.g.
#' `summary(x, pos = 1:5)`. This can be exported as a separate data frame by
#' passing `export = TRUE`, and includes all rate parameters, data locations,
#' adjustments if applied, units, and more. Note, the summary table contains
#' linear regression coefficients alongside other metadata. These should not be
#' confused with those in other functions such as `calc_rate` where slopes
#' represent rates and coefficients such as a high r-squared are important.
#' Here, slope represents the stability of the data region, in that the closer
#' the slope is to zero, the less the delta oxygen values in that region vary,
#' which is an indication of a region of stable rates. They are included to
#' enable possible future functionality where stable regions may be
#' automatically identified, and should generally be ignored. However, advanced
#' users can use regular R syntax to explore and subset the results using these
#' if they wish.
#'
#' - `mean()`: calculates the mean of all converted rates, or those specified by
#' the `pos` input. e.g. `mean(x, pos = 1:5)` The mean can be exported as a
#' separate value by passing `export = TRUE`.
#'
#' ## More
#'
#' For additional help, documentation, vignettes, and more visit the `respR`
#' website at <https://januarharianto.github.io/respR/>
#'
#' @return Output is a `list` object containing the `$rate.input`, and converted
#'   rate(s) in `$rate.output` in the `$output.unit`, as well as inputs and
#'   summary elements. Note, `$rate.abs` is the *absolute* rate in the output
#'   unit minus the mass- or area-specific component. The `$summary` table
#'   element contains all rate parameters and data locations (depending on what
#'   class of object was entered), adjustments (if applied), units, and more.
#'
#' @param x numeric value or vector, or object of class [calc_rate.ft()] or
#'   [adjust_rate.ft()]. Contains the rate(s) to be converted.
#' @param oxy.unit string. The dissolved oxygen units of the original raw data
#'   used to determine the rate in `x`.
#' @param flowrate.unit string. The units of the flowrate through the
#'   respirometer. See Details.
#' @param output.unit string. The output unit to convert the input rate to.
#'   Should be in the correct order: "Oxygen/Time" or "Oxygen/Time/Mass" or
#'   "Oxygen/Time/Area".
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
#' @param plot logical. Default is `FALSE`. Controls if a plot is produced. See
#'   Plot section.
#' @param ... Allows additional plotting controls to be passed. See Plot
#'   section.
#'
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' # Convert a single numeric rate to an absolute rate
#' convert_rate.ft(-0.09, oxy.unit = 'mg/l', flowrate.unit = 'L/s',
#'                 output.unit = 'mg/min')
#'
#' # Convert a single numeric rate to a mass-specific rate
#' convert_rate.ft(-0.09, oxy.unit = 'mg/l', flowrate.unit = 'L/s',
#'                 output.unit = 'mg/min/kg', mass = 0.5)
#'
#' # Convert a single numeric rate to an area-specific rate
#' convert_rate.ft(-0.09, oxy.unit = 'mg/l', flowrate.unit = 'L/s',
#'                 output.unit = 'mg/min/cm2', area = 0.0002)
#'
#' # Full object-oriented workflow
#' # Inspect, calculate rate, adjust rate, and convert
#' # to a final mass-specific rate
#' inspect.ft(flowthrough_mult.rd,
#'            time = 1,
#'            out.oxy = 2,
#'            in.oxy = 6) %>%
#'   calc_rate.ft(flowrate = 0.1,
#'                from = 30,
#'                to = 60,
#'                by = "time") %>%
#'   adjust_rate.ft(by = -0.032) %>%
#'   convert_rate.ft(oxy.unit = '%Air',
#'                   flowrate.unit = 'L/min',
#'                   output.unit = 'mg/h/g',
#'                   mass = 0.05,
#'                   S =35, t = 15, P = 1.013)

convert_rate.ft <- function(x,
                            oxy.unit = NULL,
                            flowrate.unit = NULL,
                            output.unit = NULL,
                            mass = NULL,
                            area = NULL,
                            S = NULL, t = NULL, P = 1.013253,
                            plot = FALSE, ...) {


  ## Save function call for output
  call <- match.call()

  # Validate inputs ---------------------------------------------------------

  # Create extended summary table
  summ.ext <- data.table(rank = NA,
                         intercept_b0 = NA,
                         slope_b1 = NA,
                         rsq = NA,
                         row = NA,
                         endrow = NA,
                         time = NA,
                         endtime = NA,
                         oxy = NA,
                         endoxy = NA,
                         delta_mean = NA,
                         flowrate = NA,
                         rate = NA,
                         adjustment = NA,
                         rate.adjusted = NA)

  # Validate x
  if (is.numeric(x)) {
    rate <- x
    # input type (needed to allow/disallow plotting and not necessarily found in x
    # because x can be numerics)
    input_type <- "vec"
    summ.ext <- as.data.table(lapply(summ.ext, rep, length(rate)))
    summ.ext$rank <- 1:length(rate)
    summ.ext$rate <- rate
    message("convert_rate.ft: numeric input detected. Converting...")
  } else if ("calc_rate.ft" %in% class(x)) {
    rate <- x$rate
    input_type <- x$input_type
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    message("convert_rate.ft: object of class 'calc_rate.ft' detected. Converting '$rate' element.")
  } else if ("adjust_rate.ft" %in% class(x)) {
    rate <- x$rate.adjusted
    input_type <- x$input_type
    summ.ext <- x$summary
    message("convert_rate.ft: object of class 'adjust_rate.ft' detected. Converting '$rate.adjusted' element.")
  } else stop("convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")

  # oxy.unit, flowrate.unit, required
  input.val(oxy.unit, num = FALSE, req = TRUE,
            msg = "convert_rate.ft: 'oxy.unit'")
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

  # Validate oxy.unit & flowrate.unit
  oxy <- units.val(oxy.unit, "o2")
  flow <- units.val(flowrate.unit, "flow")

  # Validate output.unit
  out.unit <- as.matrix(read.table(text = gsub(unit.sep.rgx,
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

  A <- units.val(out.unit[1], "o1")
  B <- units.val(out.unit[2], "time")
  if(is.spec){
    if (is.mass.spec) {
      C <- units.val(out.unit[3], "mass")
      out.unit <- as.matrix(data.frame(A, B, C))
    } else if (is.area.spec) {
      C <- units.val(out.unit[3], "area")
      out.unit <- as.matrix(data.frame(A, B, C))
    }
  } else out.unit <- as.matrix(data.frame(A, B))

  # Verify 'mass' input
  if (!is.mass.spec && is.numeric(mass))
    stop("convert_rate.ft: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")

  # Verify 'area' input
  if (!is.area.spec && is.numeric(area))
    stop("convert_rate.ft: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")

  # Format unit strings to clean format
  oxy.unit <- units.clean(oxy, "o2")
  flowrate.unit <- units.clean(flow, "flow")

  ## Add "O2" to output O2 unit string for clarity
  output.unit <- stringr::str_replace(out.unit, "\\..*", "")
  output.unit[1] <- paste0(output.unit[1], "O2")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("pmol.o2", "nmol.o2", "umol.o2", "mmol.o2", "mol.o2")) {
    RO2 <- convert_DO(rate, oxy, "mmol/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- convert_DO(rate, oxy, "mg/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mg.o2", A)
  } else if (A %in% c("mL.o2", "uL.o2")) {
    RO2 <- convert_DO(rate, oxy, "mL/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mL.o2", A)
  } else if (A %in% c("cm3.o2")) {
    RO2 <- convert_DO(rate, oxy, "cm3/L", S, t, P)
  } else if (A %in% c("mm3.o2")) {
    RO2 <- convert_DO(rate, oxy, "cm3/L", S, t, P) * 1000
  }

  ## Approach here -
  ## Break down flow rate to volume and time components.
  ## e.g. L and s
  ## Then the rate is RO2 per 1s in 1L
  ##
  # Then, convert time unit
  # time of flow rate to time
  time_component <- flow_unit_parse(flowrate.unit, "time")
  RO2 <- adjust_scale(RO2, time_component, B)

  # Then, scale to volume
  # vol of flow rate to volume in L
  vol.component <- flow_unit_parse(flowrate.unit, "vol") # what is the flow vol unit?
  volume <- adjust_scale(1, vol.component, "L.vol") # adjust to 1 litres
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

  ## Save inputs for output
  inputs = list(x = x,
                oxy.unit = oxy.unit,
                flowrate.unit = flowrate.unit,
                output.unit = output.unit,
                mass = mass,
                area = area,
                S = S, t = t, P = P)

  ## extract dataframe
  if(any(class(x) %in% c("calc_rate.ft",
                         "adjust_rate.ft"))) df <- x$dataframe else
                           df <- NULL

  ## so data.table will accept them
  if(is.null(mass)) mass <- NA
  if(is.null(area)) area <- NA

  summary <- data.table::data.table(rate.input = rate,
                                    oxy.unit = oxy.unit,
                                    flowrate.unit = flowrate.unit,
                                    mass = mass,
                                    area = area,
                                    S = ifelse(is.null(S), NA, S),
                                    t = ifelse(is.null(t), NA, t),
                                    P = ifelse(is.null(P), NA, P),
                                    rate.abs = VO2,
                                    rate.m.spec = rate.m.spec,
                                    rate.a.spec = rate.a.spec,
                                    output.unit = output.unit,
                                    rate.output = VO2.out)
  summary <- cbind(summ.ext, summary)

  ## Assemble output based on input
  ## (i.e. object or numerics)
  if(class(x) %in% c("calc_rate.ft", "adjust_rate.ft"))
    out <- list(call = call,
                inputs = inputs,
                dataframe = df,
                data = x$data,
                subsets = x$subsets,
                delta.oxy = x$delta.oxy,
                input_type = input_type,
                summary = summary,
                rate.input = rate,
                output.unit = summary$output.unit[1],
                rate.output = summary$rate.output)
  else out <- list(call = call,
                   inputs = inputs,
                   dataframe = df,
                   data = NULL,
                   subsets = NULL,
                   delta.oxy = NULL,
                   input_type = input_type,
                   summary = summary,
                   rate.input = rate,
                   output.unit = summary$output.unit[1],
                   rate.output = summary$rate.output)


  class(out) <- "convert_rate.ft"

  # Plot if TRUE
  if(plot == TRUE) plot(out, ...)

  return(out)
}


# S3 Generics -------------------------------------------------------------

#' Print convert_rate.ft objects
#' @param x convert_rate.ft object
#' @param pos integer. Which result to print.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
print.convert_rate.ft <- function(x, pos = 1, ...) {
  cat("\n# print.convert_rate.ft # ---------------\n")
  if(length(pos) > 1)
    stop("print.convert_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate.input))
    stop("print.convert_rate.ft: Invalid 'pos' rank: only ", length(x$rate.output), " rates found.")
  cat("Rank", pos, "of", length(x$rate.output), "result(s)\n")
  cat("Input:\n")
  print(x$rate.input[pos])
  print(c(x$inputs$oxy.unit, x$inputs$flowrate.unit))
  cat("Converted:\n")
  print(x$rate.output[pos])
  print(x$output.unit[1])
  cat("\n")
  if(length(x$rate.output) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' Summarise convert_rate.ft objects
#' @param object convert_rate.ft object
#' @param pos integer(s). Which summary row(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
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

  out <- data.table(object$summary[pos,])

  print(out, nrows = 50, class = FALSE)

  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' Average convert_rate.ft object rates
#' @param x convert_rate.ft object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
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

#' Plot convert_rate.ft objects
#' @param x convert_rate.ft object
#' @param type "full", "rate", or "overlap"
#' @param pos Which summary rows to plot?
#' @param quiet logical. Suppress console output.
#' @param highlight Which summary row result to highlight in overlap plots.
#' @param legend logical. Suppress labels and legends.
#' @param rate.rev logical. Control direction of y-axis in rate plot.
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.convert_rate.ft <- function(x, type = "full", pos = NULL, quiet = FALSE,
                                 highlight = NULL, legend = TRUE, rate.rev = TRUE, ...) {

  # if numeric conversions, nothing to plot
  if(x$input_type != "insp")
    stop("plot.convert_rate.ft: Plot is not available for 'convert_rate.ft' objects containing rates converted from numeric values.")

  ## warning if empty - but return to allow piping
  if(length(x$summary$rate.output) == 0){
    message("plot.convert_rate.ft: Nothing to plot! No rates found in object.")
    return(invisible(x))
  }

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # Validate type
  if(!(type %in% c("full", "rate", "overlap")))
    stop(glue::glue("plot.convert_rate.ft: 'type' input not recognised."))

  # number of rates
  nrt <- length(x$rate.output)

  # pos checks
  if(is.null(pos)) pos <- 1:nrt
  if(any(pos > nrt)){
    message(glue::glue("plot.convert_rate.ft: One or more 'pos' inputs higher than number of rows in '$summary'. Applying default of all rows."))
    pos <- 1:nrt
  }

  #### if(!quiet) CONSOLE
  if(!quiet) cat("\n# plot.convert_rate.ft # ----------------\n")

  # Plot based on type
  # grid of delta plots
  if(type == "full") grid.p(x, pos = pos, quiet = quiet,
                            rate.rev = rate.rev,
                            msg = "plot.convert_rate.ft",
                            title = "", ...)

  # outflow/inflow
  if(type == "rate") outrate.ft.p(x, pos = pos, quiet = quiet, msg = "plot.convert_rate.ft",
                                  legend = legend, rate.rev = rate.rev, ...)

  # as above but with overlap plot as bottom
  if(type == "overlap") overlap.ft.p(x, highlight = highlight, pos = pos, legend = legend, quiet = quiet,
                                     rate.rev = rate.rev, msg = "plot.convert_rate.ft", ...)

  if(!quiet) cat("-----------------------------------------\n")

  return(invisible(x))
}


#' Extracts time and volume units from flowrate unit already parsed by units.val
#'
#' @param unit flowrate unit input to be parsed
#' @param which parse which component of unit? "time" or "vol"
#' @keywords internal
flow_unit_parse <- function(unit, which){

  if(which == "vol"){
    if(unit %in% c("uL/sec", "uL/min", "uL/hr", "uL/day")) out <- "uL.vol" else
      if(unit %in% c("mL/sec", "mL/min", "mL/hr", "mL/day")) out <- "mL.vol" else
        if(unit %in% c("L/sec", "L/min", "L/hr", "L/day")) out <- "L.vol"
  }

  if(which == "time"){
    if(unit %in% c("uL/sec", "mL/sec", "L/sec")) out <- "sec.time" else
      if(unit %in% c("uL/min", "mL/min", "L/min")) out <- "min.time" else
        if(unit %in% c("uL/hr", "mL/hr", "L/hr")) out <- "hr.time" else
          if(unit %in% c("uL/day", "mL/day", "L/day")) out <- "day.time"
  }
  return(out)
}

