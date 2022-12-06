#' Convert a unitless oxygen rate value to absolute, mass-specific or
#' area-specific rate
#'
#' Converts a unitless rate derived from [`calc_rate()`], [`calc_rate.int()`],
#' [`auto_rate()`], [`auto_rate.int()`], [`adjust_rate()`], or
#' [`calc_rate.bg()`] into an absolute rate (i.e. whole chamber or whole
#' specimen), or mass-specific rate (i.e. normalised by specimen mass), or
#' area-specific rate (i.e. normalised by specimen surface area) in any common
#' unit.
#'
#' By default, `convert_rate` converts the primary `$rate` element from
#' `calc_rate`, `calc_rate.int`, `auto_rate` and , `auto_rate.int` objects, the
#' `$rate.adjusted` from `adjust_rate` objects, and the `$rate.bg` from
#' `calc_rate.bg` objects. Additionally, any numeric value or vector of rates
#' can be input as `x`.
#'
#' ## Respirometer volume
#'
#' The `volume` of the respirometer is required and should be in litres (`L`).
#' Note, the `volume` represents the *effective volume* of the respirometer,
#' that is *volume of water* in the respirometry chamber. This is not
#' necessarily the same as the volume of the respirometer. Typically, it is the
#' volume of the respirometer *minus* the volume of the specimen.
#' \href{https://github.com/nicholascarey/respfun#eff_vol}{See here} for help
#' with calculating effective volumes. It also does not refer to the specimen
#' volume.
#'
#' ## Units
#'
#' The `oxy.unit` of the original raw data used to calculate the rate is
#' required. Concentration units should use only SI units (`L` or `kg`) for the
#' denominator, e.g. `"mg/L"`, `"mmol/kg"`. Percentage saturation of air
#' (`%Air`) or oxygen (`%Oxy`) is supported, as are oxygen pressure units. See
#' [`unit_args()`] for details.
#'
#' The `time.unit` of the original raw data used to calculate the rate is also
#' required (seconds, minutes, hours, or days).
#'
#' An `output.unit` is also required and must be in the sequence *Oxygen-Time*
#' (e.g. `"mg/h"`) for absolute rates, *Oxygen-Time-Mass* (e.g. `"mg/h/kg"`) for
#' mass-specific rates, and *Oxygen-Time-Area* (e.g. `"mg/h/cm2"`) for surface
#' area-specific rates. If left `NULL`, the default of `"mgO2/h"` is used, or
#' `"mgO2/h/kg"` or `"mgO2/h/m2"` if a `mass` or `area` respectively has been
#' entered.
#'
#' Note, some oxygen input or output units require temperature (`t`) and
#' salinity (`S`) to perform conversions. For freshwater experiments, salinity
#' should be entered as zero (i.e. `S = 0`).
#'
#' Strictly speaking, the atmospheric pressure (`P`) should also be entered. If
#' not, the default value of 1.013253 bar (standard pressure at sea level) is
#' used. In most locations which have a normal range (outside extreme weather
#' events) of around 20 millibars, any variability in pressure will have a
#' relatively minor effect on dissolved oxygen, and even less on calculated
#' rates. However, we would encourage users to enter the actual value if they
#' know it, or use historical weather data to find out what it was on the day.
#' See [unit_args()] for details.
#'
#' The function uses an internal database and a fuzzy string matching algorithm
#' to accept various unit formatting styles. For example, `"mg/l"`, `"mg/L"`,
#' `"mgL-1"`, `"mg l-1"`, `"mg.l-1"` are all parsed the same. See
#' [`unit_args()`] for details of accepted units and their formatting. See also
#' [`convert_val()`] for simple conversion between non-oxygen units.
#'
#' ## Plot
#'
#' Plotting provides three ways of visualising the rates (or a selection of them
#' using `pos`), chosen using `type`. The default is `plot = FALSE` to prevent
#' plots being produced for every single conversion.
#'
#' `type = "full"` (the default) plots a grid of up to 20 plots with each rate
#' highlighted on the full dataset, with the rate value in the title. Values on
#' the axes - time (bottom), row (top), and oxygen (left) - are in the units of
#' the original raw data. Rates are plotted in order of how they appear in the
#' summary table up to the first 20 rows, unless different rows have been
#' specified via `pos`.
#'
#' `type = "rate"` plots the entire data timeseries on the upper plot, and on
#' the lower plot the output rate values in the chosen output units. Each rate
#' is plotted against the middle of the region used to determine it. `pos` can
#' be used to select a range of rates (i.e. summary table rows) to show in the
#' lower plot (default is all).
#'
#' `type = "overlap"` visualises where regression results in the summary table
#' occur in relation to the original dataset to help understand how they are
#' distributed or may overlap, and is particularly useful for results from the
#' `auto_rate` `linear` method. The top plot is the entire data timeseries, the
#' bottom plot the region of the data each rate regression has been fit over.
#' The y-axis represents the position (i.e. row) of each in the summary table
#' descending from top to bottom. If no reordering or subsetting has been
#' performed, this will usually be equivalent to the `$rank` column, but note as
#' reordering or subsetting is performed rank and summary table position will
#' not necessarily be equivalent. One result (summary table row) can be
#' highlighted, the default being `highlight = 1`. `pos` can be used to select a
#' range of summary rows to plot in the lower overlap plot.
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
#' passing `export = TRUE` and includes all rate regression parameters, and data
#' locations, adjustments if applied, units, and more. The `$rep` and `$rank`
#' columns requires special notice depending on the type of experiment you have
#' analysed or the function you used to determine the rates. For the `$rank`
#' column if `calc_rate` was used, it is the order of rates as entered using
#' `from` and `to` (if multiple rates were determined). For `auto_rate` it
#' relates to the `method` input, for example it indicates kernel density
#' ranking if the `linear` method was used, or ordering by rate value if
#' `lowest` or `highest` were used. For intermittent-flow experiments analysed
#' via `calc_rate.int` or `auto_rate.int` it indicates the ranking *within* each
#' replicate as seen in the `$rep` column. Note that if `select_rate` has been
#' used the rows in the summary table may have been reordered, including the
#' `$rep` and `$rank` columns. The *original* rep and rank for each row is
#' retained if reordering occurred.
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
#' @return Output is a `list` object of class `convert_rate` containing the
#'   `$rate.input`, and converted rate(s) in `$rate.output` in the
#'   `$output.unit`, as well as inputs and summary elements. Note, `$rate.abs`
#'   is the *absolute* rate in the output unit minus the mass- or area-specific
#'   component. The `$summary` table element contains all rate regression
#'   parameters and data locations (depending on what class of object was
#'   entered), adjustments (if applied), units, and more. The `$rep` and `$rank`
#'   columns require special notice depending on the type of experiment you have
#'   analysed or the function you used to determine the rates. See the summary
#'   table description in **S3 Generic Functions** section aboce.
#'
#' @param x numeric value or vector, or object of class `calc_rate`,
#'   `calc_rate.int`, `auto_rate`, `auto_rate.int`, `adjust_rate`, or
#'   `calc_rate.bg.` Contains the rate(s) to be converted.
#' @param oxy.unit string. The dissolved oxygen unit of the original raw data
#'   used to determine the rates in `x`.
#' @param time.unit string. The time unit of the original raw data used to
#'   determine the rates in `x`.
#' @param output.unit string. The output units to convert the input rates to.
#'   Should be in the correct order: "Oxygen/Time" or "Oxygen/Time/Mass" or
#'   "Oxygen/Time/Area".
#' @param volume numeric. Volume of water in ***litres*** in the respirometer or
#'   respirometer loop.
#' @param mass numeric. Mass/weight in ***kg***. This is the mass of the
#'   specimen if you wish to calculate mass-specific rates.
#' @param area numeric. Surface area in ***m^2***. This is the surface area of
#'   the specimen if you wish to calculate surface area-specific rates.
#' @param S numeric. Salinity (ppt). Defaults to NULL. Used in conversion of
#'   some oxygen units. Freshwater should be entered as `S = 0`.
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
#' @importFrom data.table data.table as.data.table
#' @export
#'
#' @examples
#' # Convert a single numeric rate to an absolute rate
#' convert_rate(0.09, oxy.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min', volume = 1.2)
#'
#' # Convert a single numeric rate to a mass-specific rate
#' convert_rate(0.09, oxy.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/kg', volume = 1.2, mass = 0.5)
#'
#' # Convert a single numeric rate to an area-specific rate
#' convert_rate(0.09, oxy.unit = 'mg/l', time.unit = 's',
#'   output.unit = 'mg/min/cm2', volume = 1.2, area = 0.0002)
#'
#' # Convert a single rate derived via calc_rate to mass-specific
#' x <- calc_rate(sardine.rd, from = 200, to = 1800, by = "time")
#' convert_rate(x, oxy.unit = '%Air', time.unit = 's',
#'   output.unit = 'mg/h/g', volume = 12.3, mass = 0.05,
#'   S =35, t = 15, P = 1.013)
#'
#' # Convert multiple rates derived via auto_rate to area-specific
#' x <- auto_rate(sardine.rd)
#' rates <- convert_rate(x, oxy.unit = '%Air', time.unit = 's',
#'   output.unit = 'mg/h/cm2', volume = 12.3, area = 0.00005,
#'   S =35, t = 15, P = 1.013)
#' summary(rates)

convert_rate <- function(x, oxy.unit = NULL, time.unit = NULL, output.unit = NULL,
                         volume = NULL, mass = NULL, area = NULL,
                         S = NULL, t = NULL, P = 1.013253,
                         plot = FALSE, ...) {

  ## Save function call for output
  call <- match.call()

  # Validate inputs If units are set to NULL, use default values.
  if (is.null(oxy.unit) || is.numeric(oxy.unit))
    stop("convert_rate: the 'oxy.unit' of the original data is required.")
  if (is.null(time.unit) || is.numeric(time.unit))
    stop("convert_rate: the 'time.unit' of the original data is required.")

  ## Apply output unit defaults
  if (is.null(output.unit) && is.null(mass) && is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h'",
            call. = F)
    output.unit <- "mg/h"
  }
  if (is.null(output.unit) && !is.null(mass) && is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h/kg'",
            call. = F)
    output.unit <- "mg/h/kg"
  }
  if (is.null(output.unit) && is.null(mass) && !is.null(area)) {
    warning("convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h/m2'",
            call. = F)
    output.unit <- "mg/h/m2"
  }

  # Volume must not be NULL
  if (is.null(volume) || !is.numeric(volume))
    stop("convert_rate: the 'volume' input is required.")

  # Can't have both 'mass' and 'area' inputs
  if (!is.null(mass) && !is.null(area))
    stop("convert_rate: cannot have inputs for both 'mass' and 'area'.")


  # Extract values ----------------------------------------------------------

  # Create extended summary table
  summ.ext <- data.table(rep = NA,
                         rank = NA,
                         intercept_b0 = NA,
                         rate_b1 = NA,
                         rsq = NA,
                         density = NA,
                         row = NA,
                         endrow = NA,
                         time = NA,
                         endtime = NA,
                         oxy = NA,
                         endoxy = NA,
                         rate = NA,
                         adjustment = NA,
                         rate.adjusted = NA)

  # Validate rate value based on object class
  if (is.numeric(x)) {
    rate <- x
    summ.ext <- as.data.table(lapply(summ.ext, rep, length(rate)))
    summ.ext$rank <- 1:length(rate)
    summ.ext$rate <- rate
  } else if (inherits(x, "calc_rate")) {
    rate <- x$rate
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: object of class `calc_rate` detected. Converting all rates in '$rate'.")
  } else if (class(x) %in% c("calc_rate.int")) {
    rate <- x$rate
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: object of class `calc_rate.int` detected. Converting all rates in '$rate'.")
  } else if (class(x) %in% c("auto_rate")) {
    rate <- x$rate
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    message("convert_rate: object of class `auto_rate` detected. Converting all rates in '$rate'.")
  } else if (class(x) %in% c("auto_rate.int")) {
    rate <- x$rate
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    message("convert_rate: object of class `auto_rate.int` detected. Converting all rates in '$rate'.")
  } else if (class(x) %in% "adjust_rate" && is.numeric(x$inputs$x)) {
    rate <- x$rate.adjusted
    summ.ext <- as.data.table(lapply(summ.ext, rep, length(rate)))
    summ.ext$rank <- x$summary$rank
    summ.ext$rate <- x$summary$rate
    summ.ext$adjustment <- x$summary$adjustment
    summ.ext$rate.adjusted <- x$summary$rate.adjusted
    message("convert_rate: object of class `adjust_rate` detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (class(x) %in% "adjust_rate" && class(x$inputs$x) %in% "calc_rate") {
    rate <- x$rate.adjusted
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: object of class `adjust_rate` detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (class(x) %in% "adjust_rate" && class(x$inputs$x) %in% "calc_rate.int") {
    rate <- x$rate.adjusted
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: object of class `adjust_rate` detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (class(x) %in% "adjust_rate" && class(x$inputs$x) %in% "auto_rate") {
    rate <- x$rate.adjusted
    summ.ext <- x$summary
    message("convert_rate: object of class `adjust_rate` detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (class(x) %in% "adjust_rate" && class(x$inputs$x) %in% "auto_rate.int") {
    rate <- x$rate.adjusted
    summ.ext <- x$summary
    message("convert_rate: object of class `adjust_rate` detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (inherits(x, "calc_rate.bg")) {
    ## possible warning if mass entered - no reason to have mass with bg data
    rate <- x$rate.bg
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: object of class `calc_rate.bg` detected. Converting all background rates in '$rate.bg'.")
    if(!is.null(mass) || !is.null(area))
      warning("convert_rate: A `calc_rate.bg` (i.e. background) object is being converted, and a 'mass' or 'area' has been entered. Are you sure you want to do this?")
  } else if (class(x) %in% "calc_rate.ft") {
    stop("convert_rate: object of class `calc_rate.ft` detected. \nPlease use 'convert_rate.ft' to convert the rate.")
  } else stop("convert_rate: 'x' input is not valid.")


  # Validate oxy.unit & time.unit
  oxy <- verify_units(oxy.unit, "o2")
  time <- verify_units(time.unit, "time")

  # Validate output.unit
  ou <- as.matrix(read.table(text = gsub("(?:-1|[/.[:space:]])+",
                                         " ", output.unit), header = FALSE))

  ## is it a specific rate (mass or area)?
  is.spec <- length(ou) == 3

  ## Is output unit mass or area specific rate?
  if(is.spec){
    if(!is.null(mass) && is.null(area)){
      is.mass.spec <- TRUE
      is.area.spec <- FALSE
    } else if(!is.null(area) && is.null(mass)){
      is.mass.spec <- FALSE
      is.area.spec <- TRUE
    } else if(is.null(area) && is.null(mass)){
      stop("convert_rate: 'output.unit' requires a value for 'mass' or 'area'")
    }
  } else {
    is.mass.spec <- FALSE
    is.area.spec <- FALSE
  }

  A <- verify_units(ou[1], "o1")
  B <- verify_units(ou[2], "time")
  if(is.spec){
    if (is.mass.spec) {
      C <- verify_units(ou[3], "mass")
      ou <- as.matrix(data.frame(A, B, C))
    } else if (is.area.spec) {
      C <- verify_units(ou[3], "area")
      ou <- as.matrix(data.frame(A, B, C))
    }
  } else ou <- as.matrix(data.frame(A, B))

  # Verify 'mass' input
  if (!is.mass.spec && is.numeric(mass))
    stop("convert_rate: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")

  # Verify 'area' input
  if (!is.area.spec && is.numeric(area))
    stop("convert_rate: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")

  # Format unit strings to look nicer
  oxy.unit <- stringr::str_replace(oxy, "\\..*", "")
  time.unit <- stringr::str_replace(time, "\\..*", "")

  ## Add "O2" to output O2 unit string for clarity
  output.unit <- stringr::str_replace(ou, "\\..*", "")
  output.unit[1] <- paste0(output.unit[1], "O2")
  output.unit <- paste(output.unit, collapse = "/")

  # Convert DO unit first
  if (A %in% c("pmol.o2", "nmol.o2", "umol.o2", "mmol.o2", "mol.o2")) {
    RO2 <- convert_DO(rate, oxy, "mmol/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- convert_DO(rate, oxy, "mg/L", S, t, P)
    RO2 <- adjust_scale(RO2, "mg.o2", A)
  } else if (A == "ml.o2") {
    RO2 <- convert_DO(rate, oxy, "mL/L", S, t, P)
    RO2 <- adjust_scale(RO2, "ml.o2", A)
  }

  # Then, convert time unit
  RO2 <- adjust_scale(RO2, time, B)

  # Then, scale to volume
  VO2 <- RO2 * volume

  # Then, scale to mass or area
  if (is.mass.spec) {
    # adjust mass multiplier
    multm <- adjust_scale(mass, "kg.mass", C)
    VO2.mass.spec <- VO2/multm # ok
  }
  if (is.area.spec) {
    # adjust area multiplier
    multm <- adjust_scale_area(area, "m2.area", C)
    VO2.area.spec <- VO2/multm # ok
  }

  # Generate output
  if (is.mass.spec) {
    summary <- data.table(rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = mass,
                          area = NA,
                          rate.abs = VO2,
                          rate.m.spec = VO2.mass.spec,
                          rate.a.spec = NA,
                          output.unit = output.unit,
                          rate.output = VO2.mass.spec)
    summ.ext <- cbind(summ.ext, summary)

  } else if (is.area.spec) {
    summary <- data.table(rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = NA,
                          area = area,
                          rate.abs = VO2,
                          rate.m.spec = NA,
                          rate.a.spec = VO2.area.spec,
                          output.unit = output.unit,
                          rate.output = VO2.area.spec)
    summ.ext <- cbind(summ.ext, summary)
  } else {
    summary <- data.table(rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = NA,
                          area = NA,
                          rate.abs = VO2,
                          rate.m.spec = NA,
                          rate.a.spec = NA,
                          output.unit = output.unit,
                          rate.output = VO2)
    summ.ext <- cbind(summ.ext, summary)
  }


  # Assemble output ---------------------------------------------------------

  ## Save inputs
  inputs <- list(x = x, oxy.unit = oxy.unit, time.unit = time.unit, output.unit = output.unit,
                 volume = volume, mass = mass, area = area,
                 S = S, t = t, P = P)
  ## extract dataframe
  if(any(class(x) %in% c("calc_rate",
                         "calc_rate.bg",
                         "calc_rate.int",
                         "auto_rate",
                         "auto_rate.int",
                         "adjust_rate"))) df <- x$dataframe else
                           df <- NULL

  out <- list(call = call,
              inputs = inputs,
              dataframe = df,
              summary = summ.ext,
              rate.input = rate,
              output.unit = output.unit,
              rate.output = summ.ext$rate.output)

  class(out) <- "convert_rate"

  # Plot if TRUE
  if(plot == TRUE) plot(out, ...)

  return(out)
}

#' Print convert_rate objects
#' @param x convert_rate object
#' @param pos integer. Which result to print.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
print.convert_rate <- function(x, pos = 1, ...) {
  cat("\n# print.convert_rate # ------------------\n")

  ## message if empty
  if(length(x$rate.output) == 0) message("No rates found in convert_rate object.\n")

  if(length(pos) > 1)
    stop("print.convert_rate: 'pos' must be a single value. To examine multiple results use summary().")
  if(pos > length(x$rate.input))
    stop("print.convert_rate: Invalid 'pos' rank: only ", length(x$rate.input), " rates found.")

  cat("Rank", pos, "of", length(x$rate.output), "rates:\n")
  cat("\n")
  cat("Input:\n")
  print(x$rate.input[pos])
  print(c(x$inputs$oxy.unit, x$inputs$time.unit))
  cat("Converted:\n")
  print(x$rate.output[pos])
  print(x$output.unit)
  cat("\n")
  if(length(x$rate.input) > 1) cat("To see other results use 'pos' input. \n")
  cat("To see full results use summary().\n")
  cat("-----------------------------------------\n")
  return(invisible(x))
}

#' Summarise convert_rate objects
#' @param object convert_rate object
#' @param pos integer(s). Which summary row(s) to print.
#' @param export logical. Export summary table as data frame.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
summary.convert_rate <- function(object, pos = NULL, export = FALSE, ...) {

  if(!is.null(pos) && any(pos > length(object$rate.input)))
    stop("summary.convert_rate: Invalid 'pos' rank: only ", length(object$rate.input), " rates found.")

  cat("\n# summary.convert_rate # ----------------\n")
  ## message if empty
  if(length(object$rate.output) == 0) message("No rates found in convert_rate object.\n")
  if(is.null(pos)) {
    pos <- 1:nrow(object$summary)
    cat("Summary of all converted rates:")
    cat("\n")
    cat("\n")
  } else{
    cat("Summary of converted rates from entered 'pos' rank(s):")
    cat("\n")
    cat("\n")
  }

  out <- data.table(object$summary[pos,])

  print(out)
  cat("-----------------------------------------\n")

  if(export)
    return(invisible(out)) else
      return(invisible(object))
}

#' Average convert_rate object rates
#' @param x convert_rate object
#' @param pos integer(s). Which result(s) to average.
#' @param export logical. Export averaged values as single value.
#' @param ... Pass additional inputs
#' @keywords internal
#' @return Print to console. No returned value.
#' @export
mean.convert_rate <- function(x, pos = NULL, export = FALSE, ...){

  cat("\n# mean.convert_rate # -------------------\n")
  if(!is.null(pos) && any(pos > length(x$rate.output)))
    stop("mean.convert_rate: Invalid 'pos' rank: only ", length(x$rate.output), " rates found.")
  if(is.null(pos)) {
    pos <- 1:length(x$rate.output)
    cat("Mean of all rate results:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate.output) == 0)
    message("No rates found in convert_rate object.\n")
  if(length(x$rate.output[pos]) == 1)
    message("Only 1 rate found or selected. Returning mean rate anyway...")
  ## message if empty
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

#' Plot convert_rate objects
#' @param x convert_rate object
#' @param type "full", "rate", "overlap"
#' @param pos Which summary rows to plot?
#' @param quiet logical. Suppress console output.
#' @param highlight Which to highlight in overlap plots.
#' @param legend logical. Suppress labels and legends.
#' @param rate.rev logical. Control direction of y-axis in rate plot.
#' @param ... Pass additional plotting parameters
#' @keywords internal
#' @return A plot. No returned value.
#' @export
plot.convert_rate <- function(x, type = "full", pos = NULL, quiet = FALSE,
                              highlight = NULL, legend = TRUE, rate.rev = TRUE, ...) {

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # if numeric conversions, nothing to print
  if(is.null(x$dataframe))
    stop(glue::glue("plot.convert_rate: Plot is not available for 'convert_rate' objects containing rates converted from numeric values."))
  # Can't plot calc_rate.bg objects as multiple rates come from different df columns
  # (much too complicated and who would want to...)
  if(!(is.null(x$dataframe)) && inherits(x$inputs$x, "calc_rate.bg"))
    stop(glue::glue("plot.convert_rate: Plot is not available for converted 'calc_rate.bg' objects because rates may come from different columns of the dataframe."))
  # Validate type
  if(!(type %in% c("full", "rate", "overlap")))
    stop(glue::glue("plot.convert_rate: 'type' input not recognised."))

  # number of rates
  nrt <- length(x$rate.output)

  #### if(!quiet) CONSOLE
  if(!quiet) cat("\n# plot.convert_rate # -------------------\n")

  # Plot based on type
  if(type == "full") grid.p(x, pos = pos, msg = "plot.convert_rate",
                            title = "", ...)

  if(type == "overlap") overlap.p(x, highlight = highlight, pos = pos, legend = legend,
                                  msg = "plot.convert_rate", ...)

  if(type == "rate") outrate.p(x, pos = pos, quiet = quiet, msg = "plot.convert_rate",
                               legend = legend, rate.rev = rate.rev, ...)


  if(!quiet) cat("-----------------------------------------\n")

  return(invisible(x))
}

#' Convert between multipliers of the same unit, e.g. mg to kg
#'
#' This is an internal function. Converts units of the same scale, e.g. mg to
#' kg, or mL to L.
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
#' @export
adjust_scale <- function(x, input, output) {
  # Create database of terms for matching
  prefix <- c("p", "n", "u", "m", "", "k", "sec", "min", "hour", "day")
  suffix <- c("mol", "g", "L", "l", "")
  multip <- c(1e-12, 1e-09, 1e-06, 0.001, 1, 1000, 3600, 60, 1, 1/24)
  string <- "^(p|n|u|m||k|sec|min|hour|day)?(mol|g|L|l|)$"
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
#' @export
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
