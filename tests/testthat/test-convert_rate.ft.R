# library(testthat)
# rm(list=ls())
# testthat::test_file("tests/testthat/test-convert_rate.ft.R")
# covr::file_coverage("R/convert_rate.ft.R", "tests/testthat/test-convert_rate.ft.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
  # Create testing objects --------------------------------------------------
  {
    insp.ft_obj <- inspect.ft(flowthrough.rd, delta.oxy = 4, plot = FALSE)
    ## oxygen production data
    insp.ft_obj_prod <- inspect.ft(cbind(flowthrough.rd[,1], -1*(flowthrough.rd[,4])),
                                   time = 1, delta.oxy = 2, plot = FALSE)

    # x objects
    crft1 <- calc_rate.ft(insp.ft_obj,
                          flowrate = 2, from = 200, to = 500,
                          by = NULL, width = NULL, plot = FALSE)
    crftmany <- calc_rate.ft(insp.ft_obj,
                             flowrate = 2, from = 200:300, to = 500:600,
                             by = NULL, width = NULL, plot = FALSE)
    crftwidth <- calc_rate.ft(insp.ft_obj,
                              flowrate = 2,
                              by = "row", width = 300, plot = FALSE)
    crftprod <- calc_rate.ft(insp.ft_obj_prod,
                             flowrate = 2, from = 200, to = 500,
                             by = NULL, width = NULL, plot = FALSE)
    crftval <- calc_rate.ft(-1.4,
                            flowrate = 2, from = 200, to = 500,
                            by = NULL, width = NULL, plot = FALSE)
    crftvec <- calc_rate.ft(seq(-0.9, -1.8, -0.1),
                            flowrate = 2, from = 200, to = 500,
                            by = NULL, width = NULL, plot = FALSE)

    by_val <- -0.7

    adjft.1 <- adjust_rate.ft(crft1, by = by_val)
    adjft.many <- adjust_rate.ft(crftmany, by = by_val)
    adjft.width <- adjust_rate.ft(crftwidth, by = by_val)
    adjft.prod <- adjust_rate.ft(crftprod, by = 0.7)
    adjft.vec <- adjust_rate.ft(crftmany$rate, by = by_val)
    adjft.val <- adjust_rate.ft(-1.5, by = by_val)

    S=30
    t=15
    P=1.01

    conv.crft1 <- convert_rate.ft(crft1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                  output.unit = "mg/h",
                                  mass = NULL, area = NULL,
                                  S=S, t=t, P=P)
    conv.crft1.ms <- convert_rate.ft(crft1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                     output.unit = "mg/h/g",
                                     mass = 0.4, area = NULL,
                                     S=S, t=t, P=P)
    conv.crft1.as <- convert_rate.ft(crft1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                     output.unit = "mg/h/cm2",
                                     mass = NULL, area = 0.01,
                                     S=S, t=t, P=P)
    conv.crftmany <- convert_rate.ft(crftmany, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                     output.unit = "mg/h",
                                     mass = NULL, area = NULL,
                                     S=S, t=t, P=P)
    conv.crftmany.ms <- convert_rate.ft(crftmany, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                        output.unit = "mg/h/g",
                                        mass = 0.4, area = NULL,
                                        S=S, t=t, P=P)
    conv.crftmany.as <- convert_rate.ft(crftmany, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                        output.unit = "mg/h/cm2",
                                        mass = NULL, area = 0.01,
                                        S=S, t=t, P=P)
    conv.crftwidth <- convert_rate.ft(crftwidth, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                      output.unit = "mg/h",
                                      mass = NULL, area = NULL,
                                      S=S, t=t, P=P)
    conv.crftwidth.ms <- convert_rate.ft(crftwidth, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                         output.unit = "mg/h/g",
                                         mass = 0.4, area = NULL,
                                         S=S, t=t, P=P)
    conv.crftwidth.as <- convert_rate.ft(crftwidth, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                         output.unit = "mg/h/cm2",
                                         mass = NULL, area = 0.01,
                                         S=S, t=t, P=P)

    conv.adjft.1 <- convert_rate.ft(adjft.1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                    output.unit = "mg/h",
                                    mass = NULL, area = NULL,
                                    S=S, t=t, P=P)
    conv.adjft.1.ms <- convert_rate.ft(adjft.1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                       output.unit = "mg/h/g",
                                       mass = 0.4, area = NULL,
                                       S=S, t=t, P=P)
    conv.adjft.1.as <- convert_rate.ft(adjft.1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                       output.unit = "mg/h/cm2",
                                       mass = NULL, area = 0.01,
                                       S=S, t=t, P=P)
    conv.adjft.many <- convert_rate.ft(adjft.many, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                       output.unit = "mg/h",
                                       mass = NULL, area = NULL,
                                       S=S, t=t, P=P)
    conv.adjft.many.ms <- convert_rate.ft(adjft.many, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                          output.unit = "mg/h/g",
                                          mass = 0.4, area = NULL,
                                          S=S, t=t, P=P)
    conv.adjft.many.as <- convert_rate.ft(adjft.many, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                          output.unit = "mg/h/cm2",
                                          mass = NULL, area = 0.01,
                                          S=S, t=t, P=P)
    conv.adjft.width <- convert_rate.ft(adjft.width, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                        output.unit = "mg/h",
                                        mass = NULL, area = NULL,
                                        S=S, t=t, P=P)
    conv.adjft.width.ms <- convert_rate.ft(adjft.width, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                           output.unit = "mg/h/g",
                                           mass = 0.4, area = NULL,
                                           S=S, t=t, P=P)
    conv.adjft.width.as <- convert_rate.ft(adjft.width, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                           output.unit = "mg/h/cm2",
                                           mass = NULL, area = 0.01,
                                           S=S, t=t, P=P)
  }

  # validate 'x' inputs -----------------------------------------------------

  test_that("convert_rate.ft - stops if 'x' input not numeric, calc_rate.ft, or adjust_rate.ft", {
    expect_error(convert_rate.ft("string",
                                 oxy.unit = "mg/l", flowrate.unit = "l/s"),
                 regexp = "convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")
    expect_error(convert_rate.ft(flowthrough.rd,
                                 oxy.unit = "mg/l", flowrate.unit = "l/s"),
                 regexp = "convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")
    expect_error(convert_rate.ft(inspect(flowthrough.rd, plot = FALSE),
                                 oxy.unit = "mg/l", flowrate.unit = "l/s"),
                 regexp = "convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")
  })

  test_that("convert_rate.ft - stops if 'oxy.unit' is missing", {
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = NULL, flowrate.unit = "l/s"),
                 regexp = "convert_rate.ft: 'oxy.unit' input is required.")
  })

  test_that("convert_rate.ft - stops if 'flowrate.unit' is missing", {
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mg/l", flowrate.unit = NULL),
                 regexp = "convert_rate.ft: 'flowrate.unit' input is required.")
  })

  test_that("convert_rate.ft - `output.unit` defaults successfully applied if missing", {
    # absolute
    expect_warning(convert_rate.ft(-1.4,
                                   oxy.unit = "mg/l", flowrate.unit = "l/s",
                                   output.unit = NULL),
                   regexp = "convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h'.")
    expect_identical(suppressWarnings(convert_rate.ft(-1.4,
                                                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                                                      output.unit = NULL))$output.unit,
                     "mgO2/hr")
    # mass-spec
    expect_warning(convert_rate.ft(-1.4,
                                   oxy.unit = "mg/l", flowrate.unit = "l/s",
                                   output.unit = NULL, mass = 2),
                   regexp = "convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h/kg'.")
    expect_identical(suppressWarnings(convert_rate.ft(-1.4,
                                                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                                                      output.unit = NULL, mass = 2))$output.unit,
                     "mgO2/hr/kg")
    # area-spec
    expect_warning(convert_rate.ft(-1.4,
                                   oxy.unit = "mg/l", flowrate.unit = "l/s",
                                   output.unit = NULL, area = 2),
                   regexp = "convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h/m2'.")
    expect_identical(suppressWarnings(convert_rate.ft(-1.4,
                                                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                                                      output.unit = NULL, area = 2))$output.unit,
                     "mgO2/hr/m2")
  })

  test_that("convert_rate.ft - stops is both 'mass' and 'area' have inputs.", {
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = NULL, area = 2, mass = 3),
                 regexp = "convert_rate.ft: Cannot have inputs for both 'mass' and 'area'.")
  })

  test_that("convert_rate.ft - stops if 'output.unit' requires a 'mass' or 'area' input and neither is provided", {
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = "mg/h/kg", area = NULL, mass = NULL),
                 "convert_rate.ft: 'output.unit' requires a value for 'mass' or 'area'")
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = "mg/h/m2", area = NULL, mass = NULL),
                 "convert_rate.ft: 'output.unit' requires a value for 'mass' or 'area'")
  })

  test_that("convert_rate.ft - stops if a 'mass' or 'area' input entered but output.unit is not mass- or area-specific", {
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = "mg/h", area = NULL, mass = 2),
                 "convert_rate.ft: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = "mg/h", area = 2, mass = NULL),
                 "convert_rate.ft: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")
  })

  test_that("convert_rate.ft - stops if units require t, S and P", {

    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mL/L", flowrate.unit = "l/s",
                                 output.unit = "mg/h/mm2", area = 2, mass = NULL,
                                 S = NULL, t = NULL, P = NULL),
                 "convert_rate.ft: Input or output units require Salinity input")
    expect_error(convert_rate.ft(-1.4,
                                 oxy.unit = "mL/L", flowrate.unit = "l/s",
                                 output.unit = "mg/h/mm2", area = 2, mass = NULL,
                                 S = 35, t = NULL, P = NULL),
                 "convert_rate.ft: Input or output units require Temperature input")
    expect_message(convert_rate.ft(-1.4,
                                   oxy.unit = "mL/L", flowrate.unit = "l/s",
                                   output.unit = "mg/h/mm2", area = 2, mass = NULL,
                                   S = 35, t = 15, P = NULL),
                   "convert_rate.ft: Input or output units require Atmospheric Pressure input")
  })

  test_that("convert_rate.ft - stops if S, t, and P are vectors", {

    expect_error(convert_rate.ft(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35:36,
                                 t= 12,
                                 P = 1.01),
                 "convert_rate.ft: The 'S' input should be a single value.")
    expect_error(convert_rate.ft(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12:13,
                                 P = 1.01),
                 "convert_rate.ft: The 't' input should be a single value.")
    expect_error(convert_rate.ft(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = c(1,1.01)),
                 "convert_rate.ft: The 'P' input should be a single value.")

  })

  test_that("convert_rate.ft - correct S, t, and P values saved to summary", {

    # single rate value
    expect_equal(convert_rate.ft(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = 1.01)$summary$S,
                 35)
    # multiple rate values
    expect_equal(convert_rate.ft(c(-0.001, -0.002), oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = 1.01)$summary$S,
                 c(35,35))
    # single rate value
    expect_equal(convert_rate.ft(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = 1.01)$summary$t,
                 12)
    # multiple rate values
    expect_equal(convert_rate.ft(c(-0.001, -0.002), oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = 1.01)$summary$t,
                 c(12,12))
    # single rate value
    expect_equal(convert_rate.ft(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = 1.01)$summary$P,
                 1.01)
    # multiple rate values
    expect_equal(convert_rate.ft(c(-0.001, -0.002), oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429,
                                 S = 35,
                                 t= 12,
                                 P = 1.01)$summary$P,
                 c(1.01,1.01))
    # NA when NULL
    expect_equal(convert_rate.ft(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429)$summary$S,
                 NA)
    expect_equal(convert_rate.ft(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429)$summary$t,
                 NA)
    # Default value for P
    expect_equal(convert_rate.ft(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429)$summary$P,
                 1.013253)
    # multiple rate values
    expect_equal(convert_rate.ft(c(-0.001, -0.002), oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                                 flowrate.unit = "l/m",
                                 area = 0.000001429)$summary$S,
                 c(NA,NA))
  })


  # Verify conversions ------------------------------------------------------

  # These are some simple conversions tested with known results.
  #
  # Further down is a skipped test which runs through masses of combinations and
  # compares results to convert_rate. It is skipped for now since it takes ages.
  # It can be run manually in the future or removed to somewhere else.
  #
  # Rationale behind these tests:
  #
  # convert_rate.ft results should be the same as convert_rate as long as the
  # 'time.unit' in cr is the same as the one used in the flowrate (e.g. if in
  # mL/s, time = "s"), and the volume used in flowrate is converted to L (e.g. if
  # mL/s, volume = 1/1000).

  test_that("convert_rate.ft - outputs same results as calc_rate", {
    expect_equal(
      convert_rate.ft(-1.4,
                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      convert_rate(-1.4,
                   oxy.unit = "mg/l", time.unit = "s", volume = 1,
                   output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
    )
    expect_equal(
      convert_rate.ft(-0.001,
                      oxy.unit = "mg/l", flowrate.unit = "ml/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      convert_rate(-0.001,
                   oxy.unit = "mg/l", time.unit = "s", volume = 1/1000,
                   output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
    )
    expect_equal(
      convert_rate.ft(-455,
                      oxy.unit = "mg/l", flowrate.unit = "ul/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      convert_rate(-455,
                   oxy.unit = "mg/l", time.unit = "s", volume = 1/1000000,
                   output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
    )
    expect_equal(
      convert_rate.ft(-455,
                      oxy.unit = "mg/l", flowrate.unit = "ul/d",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      convert_rate(-455,
                   oxy.unit = "mg/l", time.unit = "d", volume = 1/1000000,
                   output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
    )
    expect_equal(
      convert_rate.ft(crft1,
                      oxy.unit = "mg/l", flowrate.unit = "ml/min",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      convert_rate(crft1$rate,
                   oxy.unit = "mg/l", time.unit = "m", volume = 1/1000,
                   output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
    )
    expect_equal(
      convert_rate.ft(crftmany,
                      oxy.unit = "mg/l", flowrate.unit = "ml/min",
                      output.unit = "umol h-1", area = NULL, mass = NULL)$rate.output,
      convert_rate(crftmany$rate,
                   oxy.unit = "mg/l", time.unit = "m", volume = 1/1000,
                   output.unit = "umol h-1", area = NULL, mass = NULL)$rate.output
    )
    expect_equal(
      convert_rate.ft(crftvec,
                      oxy.unit = "mg/l", flowrate.unit = "ml/min",
                      output.unit = "mL s-1", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      convert_rate(crftvec$rate,
                   oxy.unit = "mg/l", time.unit = "m", volume = 1/1000,
                   output.unit = "mL s-1", area = NULL, mass = NULL,
                   S=S, t=t, P=P)$rate.output
    )
    expect_equal(
      convert_rate.ft(adjft.1,
                      oxy.unit = "mg/l", flowrate.unit = "ml/min",
                      output.unit = "mL s-1", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      convert_rate(adjft.1$rate.adjusted,
                   oxy.unit = "mg/l", time.unit = "m", volume = 1/1000,
                   output.unit = "mL s-1", area = NULL, mass = NULL,
                   S=S, t=t, P=P)$rate.output
    )
    expect_equal(
      convert_rate.ft(adjft.many,
                      oxy.unit = "%Air", flowrate.unit = "ml/min",
                      output.unit = "mL s-1", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      convert_rate(adjft.many$rate.adjusted,
                   oxy.unit = "%Air", time.unit = "m", volume = 1/1000,
                   output.unit = "mL s-1", area = NULL, mass = NULL,
                   S=S, t=t, P=P)$rate.output
    )
    expect_equal(
      convert_rate.ft(adjft.width,
                      oxy.unit = "%o2", flowrate.unit = "ml/min",
                      output.unit = "mol s-1", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      convert_rate(adjft.width$rate.adjusted,
                   oxy.unit = "%o2", time.unit = "m", volume = 1/1000,
                   output.unit = "mol s-1", area = NULL, mass = NULL,
                   S=S, t=t, P=P)$rate.output
    )
    expect_equal(
      convert_rate.ft(adjft.prod,
                      oxy.unit = "umol/kg", flowrate.unit = "l/h",
                      output.unit = "mol s-1", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      convert_rate(adjft.prod$rate.adjusted,
                   oxy.unit = "umol/kg", time.unit = "h", volume = 1/1,
                   output.unit = "mol s-1", area = NULL, mass = NULL,
                   S=S, t=t, P=P)$rate.output
    )
    expect_equal(
      convert_rate.ft(adjft.vec,
                      oxy.unit = "hPa", flowrate.unit = "l/h",
                      output.unit = "ml d-1", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      convert_rate(adjft.vec$rate.adjusted,
                   oxy.unit = "hPa", time.unit = "h", volume = 1/1,
                   output.unit = "ml d-1", area = NULL, mass = NULL,
                   S=S, t=t, P=P)$rate.output
    )
  })

  test_that("convert_rate.ft - outputs some known specific values", {
    # this is mostly to guard against future changes breaking the outputs
    expect_equal(
      # -1 mg in l/s should be -1*60*60  in mg/h
      convert_rate.ft(-1,
                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      -1*60*60)

    expect_equal(
      convert_rate.ft(-2,
                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      -2*60*60)

    expect_equal(
      convert_rate.ft(2,
                      oxy.unit = "mg/l", flowrate.unit = "l/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      2*60*60)
    expect_equal(
      convert_rate.ft(-0.01,
                      oxy.unit = "ug/l", flowrate.unit = "l/s",
                      output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
      -0.01*60*60 / 1000)
    expect_equal(
      convert_rate.ft(-0.01,
                      oxy.unit = "ml/l", flowrate.unit = "l/s",
                      output.unit = "mg/h", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      -48.6154742081153)
    expect_equal(
      convert_rate.ft(-1,
                      oxy.unit = "ml/l", flowrate.unit = "l/s",
                      output.unit = "umol/h", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      -151929.054239894)
    expect_equal(
      convert_rate.ft(-0.01,
                      oxy.unit = "umol/l", flowrate.unit = "l/s",
                      output.unit = "mg/s", area = NULL, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      -0.000319988)
    expect_equal(
      convert_rate.ft(-0.01,
                      oxy.unit = "umol/l", flowrate.unit = "l/s",
                      output.unit = "mg/s/kg", area = NULL, mass = 0.01,
                      S=S, t=t, P=P)$rate.output,
      -0.000319988/0.01)
    expect_equal(
      convert_rate.ft(-0.01,
                      oxy.unit = "umol/l", flowrate.unit = "l/s",
                      output.unit = "mg/s/m2", area = 0.001, mass = NULL,
                      S=S, t=t, P=P)$rate.output,
      -0.000319988/0.001)
  })

  test_that("convert_rate.ft: ul oxygen output unit correct values ", {
    # should be ml times 1000
    expect_equal(convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'ml/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output * 1000,
                 convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'ul/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output)
  })

  test_that("convert_rate.ft: cm3/mm3 oxygen output unit correct values ", {
    # cm3 should be same as ml
    expect_equal(convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'cm3/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output,
                 convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'ml/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output)
    # mm3 should be same as ul
    expect_equal(convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'mm3/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output,
                 convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'ul/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output)
    # mm3 should be cm3 times 1000
    expect_equal(convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'cm3/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output * 1000,
                 convert_rate.ft(0.9, oxy.unit = 'mg/l', time.unit = 's', flowrate.unit = "l/min",
                                 output.unit = 'mm3/h/kg',
                                 volume = 1.2, mass = 0.5, S = 35, t = 12, P = 1)$rate.output)
  })


  # S3 Generics -------------------------------------------------------------

  objs <- c("conv.crft1","conv.crft1.ms","conv.crft1.as","conv.crftmany",
            "conv.crftmany.ms","conv.crftmany.as","conv.crftwidth",
            "conv.crftwidth.ms","conv.crftwidth.as","conv.adjft.1",
            "conv.adjft.1.ms","conv.adjft.1.as","conv.adjft.many",
            "conv.adjft.many.ms","conv.adjft.many.as","conv.adjft.width",
            "conv.adjft.width.ms","conv.adjft.width.as")

  test_that("convert_rate.ft - objects can be printed.", {
    sapply(objs, function(z) {
      expect_error(print(eval(parse(text=z))),
                   regexp = NA)
    })
    sapply(objs, function(z) {
      expect_output(print(eval(parse(text=z))),
                    regexp = "Converted:")
    })

  })

  test_that("convert_rate.ft - objects can be printed with 'pos' input.", {

    expect_error(print(conv.adjft.many.as, pos = 2),
                 regexp = NA)
    expect_output(print(conv.adjft.many.as, pos = 2),
                  regexp = "Rank 2 of 101 result")

    expect_error(print(conv.crftmany.as, pos = 100),
                 regexp = NA)
    expect_output(print(conv.crftmany.as, pos = 100),
                  regexp = "Rank 100 of 101 result")

  })

  test_that("convert_rate.ft - print() stops with invalid 'pos' input.", {
    expect_error(print(conv.crft1, pos = 2),
                 regexp = "print.convert_rate.ft: Invalid 'pos' rank: only 1 rates found.")
    expect_error(print(conv.adjft.many.as, pos = 102),
                 regexp = "print.convert_rate.ft: Invalid 'pos' rank: only 101 rates found.")
    expect_error(print(conv.crftmany.as, pos = 10:20),
                 regexp = "print.convert_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")
  })

  test_that("convert_rate.ft - objects work with summary().", {
    sapply(objs, function(z) {
      expect_error(summary(eval(parse(text=z))),
                   regexp = NA)
    })
    sapply(objs, function(z) {
      expect_output(summary(eval(parse(text=z))))
    })

  })
  summary(conv.crft1)
  test_that("convert_rate.ft - objects work with summary() and 'pos' input", {

    expect_error(summary(conv.crftmany.ms, pos = 2),
                 regexp = NA)
    expect_output(summary(conv.crftmany.ms, pos = 2),
                  regexp = "Summary of rate results from entered 'pos' rank")
    expect_equal(nrow(summary(conv.crftmany.ms, pos = 2, export = TRUE)),
                 1)

    expect_error(summary(conv.crftmany, pos = 2:10),
                 regexp = NA)
    expect_output(summary(conv.crftmany, pos = 2:10),
                  regexp = "Summary of rate results from entered 'pos' rank")
    expect_equal(nrow(summary(conv.crftmany, pos = 2:10, export = TRUE)),
                 9)

    expect_error(summary(conv.adjft.width.as, pos = c(2,4,6,8)),
                 regexp = NA)
    expect_output(summary(conv.adjft.width.as, pos = c(2,4,6,8)),
                  regexp = "Summary of rate results from entered 'pos' rank")
    expect_equal(nrow(summary(conv.adjft.width.as, pos = c(2,4,6,8), export = TRUE)),
                 4)
    expect_equal(summary(conv.adjft.width.as, pos = c(2,4,6,8), export = TRUE)$rank,
                 c(2,4,6,8))

  })

  test_that("convert_rate.ft - summary() stops with invalid 'pos' input", {

    expect_error(summary(conv.crftmany.ms, pos = 102),
                 regexp = "summary.convert_rate.ft: Invalid 'pos' rank: only 101 rates found.")
  })

  test_that("convert_rate.ft - objects work with summary() and 'export' input", {
    expect_error(summary(conv.crftmany.ms, export = TRUE),
                 regexp = NA)
    expect_equal(nrow(summary(conv.crftmany.ms, export = TRUE)),
                 nrow(conv.crftmany.ms$summary))
  })

  test_that("convert_rate.ft - objects work with mean()", {

    expect_error(mean(conv.crft1),
                 regexp = NA)
    expect_output(mean(conv.crft1),
                  regexp = "Mean of 1 output rates:")
    expect_equal(mean(conv.crft1, export = TRUE),
                 mean(conv.crft1$rate.output))
    expect_message(mean(conv.crft1),
                   regexp = "Only 1 rate found. Returning mean rate anyway")

    expect_error(mean(conv.crft1.ms),
                 regexp = NA)
    expect_output(mean(conv.crft1.ms),
                  regexp = "Mean of 1 output rates:")
    expect_equal(mean(conv.crft1.ms, export = TRUE),
                 mean(conv.crft1.ms$rate.output))

    expect_error(mean(conv.crftmany.as),
                 regexp = NA)
    expect_output(mean(conv.crftmany.as),
                  regexp = "Mean of 101 output rates:")
    expect_equal(mean(conv.crftmany.as, export = TRUE),
                 mean(conv.crftmany.as$rate.output))

    expect_error(mean(conv.adjft.many),
                 regexp = NA)
    expect_output(mean(conv.adjft.many),
                  regexp = "Mean of 101 output rates:")
    expect_equal(mean(conv.adjft.many, export = TRUE),
                 mean(conv.adjft.many$rate.output))

    expect_error(mean(conv.adjft.many.ms),
                 regexp = NA)
    expect_output(mean(conv.adjft.many.ms),
                  regexp = "Mean of 101 output rates:")
    expect_equal(mean(conv.adjft.many.ms, export = TRUE),
                 mean(conv.adjft.many.ms$rate.output))

    expect_error(mean(conv.adjft.width.as),
                 regexp = NA)
    expect_output(mean(conv.adjft.width.as),
                  regexp = "Mean of 636 output rates:")
    expect_equal(mean(conv.adjft.width.as, export = TRUE),
                 mean(conv.adjft.width.as$rate.output))
  })

  test_that("convert_rate.ft - objects work with mean() and 'pos' input", {

    expect_error(mean(conv.adjft.many.ms, pos = 1:10),
                 regexp = NA)
    expect_output(mean(conv.adjft.many.ms, pos = 1:10),
                  regexp = "Mean of rate results from entered 'pos' ranks")
    expect_equal(mean(conv.adjft.many.ms, pos = 1:10,  export = TRUE),
                 mean(conv.adjft.many.ms$rate.output[1:10]))
    expect_error(mean(conv.adjft.many.ms, pos = 150),
                 regexp = "mean.convert_rate.ft: Invalid 'pos' rank: only 101 rates found.")
  })

  test_that("convert_rate.ft - plot is produced with converted calc_rate.ft objects", {
    expect_output(conv.adjft.1 <- convert_rate.ft(adjft.1, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                                  output.unit = "mg/h",
                                                  mass = NULL, area = NULL,
                                                  S=S, t=t, P=P, plot = TRUE))
    expect_output(plot(conv.adjft.many.ms))
    expect_output(plot(conv.adjft.many.ms, type = "full"))
    expect_output(plot(conv.adjft.many.ms, type = "rate"))
    expect_output(plot(conv.adjft.many.ms, type = "overlap"))
  })

  test_that("convert_rate.ft - plot pos and highlight correct messages", {
    expect_message(plot(conv.adjft.many.ms, pos = 200),
                   "convert_rate.ft: One or more 'pos' inputs higher than number of rows in '\\$summary'. Applying default of all rows.")
    expect_message(plot(conv.adjft.many.ms, pos = 200, type = "overlap"),
                   "convert_rate.ft: One or more 'pos' inputs higher than number of rows in '\\$summary'. Applying default of all rows.")
    expect_message(plot(conv.adjft.many.ms, highlight = 200, type = "overlap"),
                   "convert_rate.ft: 'highlight' not within 'pos' input. Applying default of first 'pos' entry.")
  })

  test_that("convert_rate.ft - plot errors with various disallowed inputs", {
    num <-
      c(1,2,3) %>%
      convert_rate.ft(oxy.unit = "mg/l",
                      time.unit = "min",
                      flowrate.unit = "l/s",
                      output.unit = "mg/h",
                      volume = 1.09)
    expect_error(plot(num),
                 "plot.convert_rate.ft: Plot is not available for 'convert_rate.ft' objects containing rates converted from numeric values.")
    expect_error(plot(conv.crft1, type = "text"),
                 "plot.convert_rate.ft: 'type' input not recognised.")
  })

  test_that("convert_rate.ft - correct message when plot is called on objects with zero rates", {

    # message with zero rates for both methods
    ar_mult_no_rts <-
      suppressWarnings(
        flowthrough.rd %>%
          inspect.ft(1,2,3, plot = FALSE) %>%
          calc_rate.ft(1, c(10, 300, 700), c(200, 500, 800), plot = FALSE) %>%
          convert_rate.ft(oxy.unit = "mg/l",
                          time.unit = "min",
                          flowrate.unit = "l/h",
                          output.unit = "mg/h/g",
                          volume = 1.09,
                          mass = 0.005))
    # remove
    ar_mult_no_rts$summary <- ar_mult_no_rts$summary[NULL,]
    ar_mult_no_rts$rate.output <- NULL

    expect_message(plot(ar_mult_no_rts),
                   "convert_rate.ft: Nothing to plot! No rates found in object.")
    expect_message(plot(ar_mult_no_rts, type = "rate"),
                   "convert_rate.ft: Nothing to plot! No rates found in object.")
    expect_message(plot(ar_mult_no_rts, type = "overlap"),
                   "convert_rate.ft: Nothing to plot! No rates found in object.")

  })

  test_that("convert_rate.ft - plot defaults are correctly restored", {

    # reset plotting first
    dev.off()
    # save par before
    parb4 <- par(no.readonly = TRUE)
    # now use a fn with plot
    plot(conv.adjft.many.ms)
    # save after
    paraft <- par(no.readonly = TRUE)
    # mai is something changed from the default,
    # so if par settings not restored properly this should fail
    expect_identical(parb4$mai,
                     paraft$mai)

  })

}) ## end capture.output
