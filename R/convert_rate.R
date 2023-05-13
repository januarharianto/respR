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
#' used. In most locations which have a normal range of around 20 millibars
#' (outside of extreme weather events), any variability in pressure will have a
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
#'   table description in **S3 Generic Functions** section above.
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
                         S = NULL, t = NULL, P = NULL,
                         plot = FALSE, ...) {

  ## Save function call for output
  call <- match.call()

  # Validate inputs If units are set to NULL, use default values.
  if (is.null(oxy.unit) || is.numeric(oxy.unit))
    stop("convert_rate: The 'oxy.unit' of the original data is required.", call. = FALSE)
  if (is.null(time.unit) || is.numeric(time.unit))
    stop("convert_rate: The 'time.unit' of the original data is required.", call. = FALSE)

  ## Apply output unit defaults
  if (is.null(output.unit) && is.null(mass) && is.null(area)) {
    warning("convert_rate: The 'output.unit' is not provided. Applying default: 'mg/h'",
            call. = FALSE)
    output.unit <- "mg/h"
  }
  if (is.null(output.unit) && !is.null(mass) && is.null(area)) {
    warning("convert_rate: The 'output.unit' is not provided. Applying default: 'mg/h/kg'",
            call. = FALSE)
    output.unit <- "mg/h/kg"
  }
  if (is.null(output.unit) && is.null(mass) && !is.null(area)) {
    warning("convert_rate: The 'output.unit' is not provided. Applying default: 'mg/h/m2'",
            call. = FALSE)
    output.unit <- "mg/h/m2"
  }

  # Volume must not be NULL
  if (is.null(volume) || !is.numeric(volume))
    stop("convert_rate: The 'volume' input is required.", call. = FALSE)

  # Can't have both 'mass' and 'area' inputs
  if (!is.null(mass) && !is.null(area))
    stop("convert_rate: Cannot have inputs for both 'mass' and 'area'.", call. = FALSE)


  # Extract values ----------------------------------------------------------

  # Create extended summary table
  summ.ext <- data.table(rep = NA,
                         rank = NA,
                         intercept_b0 = NA,
                         slope_b1 = NA,
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
    message("convert_rate: Numeric input detected. Converting all numeric rates.")
  } else if (inherits(x, "calc_rate")) {
    rate <- x$rate
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: Object of class 'calc_rate' detected. Converting all rates in '$rate'.")
  } else if (inherits(x, "calc_rate.int")) {
    rate <- x$rate
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: Object of class 'calc_rate.int' detected. Converting all rates in '$rate'.")
  } else if (inherits(x, "auto_rate")) {
    rate <- x$rate
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    message("convert_rate: Object of class 'auto_rate' detected. Converting all rates in '$rate'.")
  } else if (inherits(x, "auto_rate.int")) {
    rate <- x$rate
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    message("convert_rate: Object of class 'auto_rate.int' detected. Converting all rates in '$rate'.")
  } else if (inherits(x, "adjust_rate") && is.numeric(x$inputs$x)) {
    rate <- x$rate.adjusted
    summ.ext <- as.data.table(lapply(summ.ext, rep, length(rate)))
    summ.ext$rank <- x$summary$rank
    summ.ext$rate <- x$summary$rate
    summ.ext$adjustment <- x$summary$adjustment
    summ.ext$rate.adjusted <- x$summary$rate.adjusted
    message("convert_rate: Object of class 'adjust_rate' detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (inherits(x, "adjust_rate") && inherits(x$inputs$x, "calc_rate")) {
    rate <- x$rate.adjusted
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: Object of class 'adjust_rate' detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (inherits(x, "adjust_rate") && inherits(x$inputs$x, "calc_rate.int")) {
    rate <- x$rate.adjusted
    summ.ext <- x$summary[,-"rate.2pt"]
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: Object of class 'adjust_rate' detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (inherits(x, "adjust_rate") && inherits(x$inputs$x, "auto_rate")) {
    rate <- x$rate.adjusted
    summ.ext <- x$summary
    message("convert_rate: Object of class 'adjust_rate' detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (inherits(x, "adjust_rate") && inherits(x$inputs$x, "auto_rate.int")) {
    rate <- x$rate.adjusted
    summ.ext <- x$summary
    message("convert_rate: Object of class 'adjust_rate' detected. Converting all adjusted rates in '$rate.adjusted'.")
  } else if (inherits(x, "calc_rate.bg")) {
    ## possible warning if mass entered - no reason to have mass with bg data
    rate <- x$rate.bg
    summ.ext <- x$summary
    summ.ext$adjustment <- NA
    summ.ext$rate.adjusted <- NA
    summ.ext$density <- NA
    setcolorder(summ.ext, c(1:5, 15, 6:14))
    message("convert_rate: Object of class 'calc_rate.bg' detected. Converting all background rates in '$rate.bg'.")
    if(!is.null(mass) || !is.null(area))
      warning("convert_rate: A 'calc_rate.bg' (i.e. background) object is being converted, and a 'mass' or 'area' has been entered. Are you sure you want to do this?",
              call. = FALSE)
  } else if (inherits(x, "calc_rate.ft")) {
    stop("convert_rate: Object of class 'calc_rate.ft' detected. \nPlease use 'convert_rate.ft' to convert the rate.",
         call. = FALSE)
  } else stop("convert_rate: 'x' is not an accepted input.", call. = FALSE)


  # Validate oxy.unit & time.unit
  oxy <- units.val(oxy.unit, "o2")
  time <- units.val(time.unit, "time")

  # Validate output.unit
  ou <- as.matrix(read.table(text = gsub(unit.sep.rgx,
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
      stop("convert_rate: 'output.unit' requires a value for 'mass' or 'area'", call. = FALSE)
    }
  } else {
    is.mass.spec <- FALSE
    is.area.spec <- FALSE
  }

  A <- units.val(ou[1], "o1")
  B <- units.val(ou[2], "time")
  if(is.spec){
    if (is.mass.spec) {
      C <- units.val(ou[3], "mass")
      ou <- as.matrix(data.frame(A, B, C))
    } else if (is.area.spec) {
      C <- units.val(ou[3], "area")
      ou <- as.matrix(data.frame(A, B, C))
    }
  } else ou <- as.matrix(data.frame(A, B))

  # Verify 'mass' input
  if (!is.mass.spec && is.numeric(mass))
    stop("convert_rate: A 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.",
         call. = FALSE)

  # Verify 'area' input
  if (!is.area.spec && is.numeric(area))
    stop("convert_rate: An 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.",
         call. = FALSE)

  # Format unit strings to clean format
  oxy.unit <- units.clean(oxy, "o2")
  time.unit <- units.clean(time, "time")

  ## Add "O2" to output O2 unit string for clarity
  output.unit <- stringr::str_replace(ou, "\\..*", "")
  output.unit[1] <- paste0(output.unit[1], "O2")
  output.unit <- paste(output.unit, collapse = "/")

  # Check t, S and P needed for units, issue errors
  # and apply default P
  # This will apply it if either in or out unit requires it.
  P <- StP.val(oxy.unit, "oxy", S, t, P, P.chk = FALSE, msg = "convert_rate")
  P <- StP.val(output.unit, "mr", S, t, P, P.chk = TRUE, msg = "convert_rate")

  # Convert -----------------------------------------------------------------

  # Convert DO unit first
  if (A %in% c("pmol.o2", "nmol.o2", "umol.o2", "mmol.o2", "mol.o2")) {
    RO2 <- suppressMessages(suppressWarnings(convert_DO(rate, oxy, "mmol/L", S, t, P)))
    RO2 <- adjust_scale(RO2, "mmol.o2", A)
  } else if (A %in% c("mg.o2", "ug.o2")) {
    RO2 <- suppressMessages(suppressWarnings(convert_DO(rate, oxy, "mg/L", S, t, P)))
    RO2 <- adjust_scale(RO2, "mg.o2", A)
  } else if (A %in% c("mL.o2", "uL.o2")) {
    RO2 <- suppressMessages(suppressWarnings(convert_DO(rate, oxy, "mL/L", S, t, P)))
    RO2 <- adjust_scale(RO2, "mL.o2", A)
  } else if (A %in% c("cm3.o2")) {
    RO2 <- suppressMessages(suppressWarnings(convert_DO(rate, oxy, "cm3/L", S, t, P)))
  } else if (A %in% c("mm3.o2")) {
    RO2 <- suppressMessages(suppressWarnings(convert_DO(rate, oxy, "cm3/L", S, t, P))) * 1000
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

  # Generate summary table --------------------------------------------------

  if (is.mass.spec) {
    summary <- data.table(rate.input = rate,
                          oxy.unit = oxy.unit,
                          time.unit = time.unit,
                          volume = volume,
                          mass = mass,
                          area = NA,
                          S = ifelse(is.null(S), NA, S),
                          t = ifelse(is.null(t), NA, t),
                          P = ifelse(is.null(P), NA, P),
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
                          S = ifelse(is.null(S), NA, S),
                          t = ifelse(is.null(t), NA, t),
                          P = ifelse(is.null(P), NA, P),
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
                          S = ifelse(is.null(S), NA, S),
                          t = ifelse(is.null(t), NA, t),
                          P = ifelse(is.null(P), NA, P),
                          rate.abs = VO2,
                          rate.m.spec = NA,
                          rate.a.spec = NA,
                          output.unit = output.unit,
                          rate.output = VO2)
    summ.ext <- cbind(summ.ext, summary)
  }


  # Assemble output ---------------------------------------------------------

  ## Save inputs
  inputs <- list(x = x,
                 oxy.unit = oxy.unit,
                 time.unit = time.unit,
                 output.unit = output.unit,
                 volume = volume,
                 mass = mass,
                 area = area,
                 S = S,
                 t = t,
                 P = P)
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
    stop("print.convert_rate: 'pos' must be a single value. To examine multiple results use summary().",
         call. = FALSE)
  if(pos > length(x$rate.input))
    stop("print.convert_rate: Invalid 'pos' rank: only ", length(x$rate.input), " rates found.",
         call. = FALSE)

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
    stop("summary.convert_rate: Invalid 'pos' rank: only ", length(object$rate.input), " rates found.",
         call. = FALSE)

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

  print(out, nrows = 50, class = FALSE)
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
    stop("mean.convert_rate: Invalid 'pos' rank: only ", length(x$rate.output), " rates found.",
         call. = FALSE)
  if(is.null(pos)) {
    pos <- 1:length(x$rate.output)
    cat("Mean of all rate results:")
    cat("\n")
  } else{
    cat("Mean of rate results from entered 'pos' ranks:")
    cat("\n")
  }
  if(length(x$rate.output) == 0)
    message("mean.convert_rate: No rates found in convert_rate object.\n")
  if(length(x$rate.output[pos]) == 1)
    message("mean.convert_rate: Only 1 rate found or selected. Returning mean rate anyway...")
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

  ## warning if empty - but return to allow piping
  if(length(x$summary$rate.output) == 0){
    message("plot.convert_rate: Nothing to plot! No rates found in object.")
    return(invisible(x))
  }

  parorig <- par(no.readonly = TRUE) # save original par settings
  on.exit(par(parorig)) # revert par settings to original

  # if numeric conversions, nothing to plot
  if(is.null(x$dataframe))
    stop(glue::glue("plot.convert_rate: Plot is not available for 'convert_rate' objects containing rates converted from numeric values."),
         call. = FALSE)
  # Can't plot calc_rate.bg objects as multiple rates come from different df columns
  # (much too complicated and who would want to...)
  if(!(is.null(x$dataframe)) && inherits(x$inputs$x, "calc_rate.bg"))
    stop(glue::glue("plot.convert_rate: Plot is not available for converted 'calc_rate.bg' objects because rates may come from different columns of the dataframe."),
         call. = FALSE)
  # Validate type
  if(!(type %in% c("full", "rate", "overlap")))
    stop(glue::glue("plot.convert_rate: 'type' input not recognised."), call. = FALSE)

  # number of rates
  nrt <- length(x$rate.output)

  # pos checks
  if(is.null(pos)) pos <- 1:nrt
  if(any(pos > nrt)){
    message(glue::glue("plot.convert_rate: One or more 'pos' inputs higher than number of rows in '$summary'. Applying default of all rows."))
    pos <- 1:nrt
  }

  #### if(!quiet) CONSOLE
  if(!quiet) cat("\n# plot.convert_rate # -------------------\n")

  # Plot based on type
  if(type == "full") grid.p(x, pos = pos, msg = "plot.convert_rate",
                            title = "", ...)

  if(type == "rate") outrate.p(x, pos = pos, quiet = quiet, msg = "plot.convert_rate",
                               legend = legend, rate.rev = rate.rev, ...)

  if(type == "overlap") overlap.p(x, highlight = highlight, pos = pos, legend = legend, quiet = quiet,
                                  msg = "plot.convert_rate", ...)



  if(!quiet) cat("-----------------------------------------\n")

  return(invisible(x))
}

