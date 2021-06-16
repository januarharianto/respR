# library(testthat)
# rm(list=ls())
# testthat::test_file("tests/testthat/test-convert_rate.ft.R")
# covr::file_coverage("R/calc_rate.ft.R", "tests/testthat/test-convert_rate.ft.R")
# x <- covr::package_coverage()
# covr::report(x)

capture.output({  ## stops printing outputs on assigning

# Create testing objects --------------------------------------------------

insp.ft_obj <- inspect.ft(flowthrough.rd, delta.o2 = 4, plot = FALSE)
## o2 production data
insp.ft_obj_prod <- inspect.ft(cbind(flowthrough.rd[,1], -1*(flowthrough.rd[,4])),
                               time = 1, delta.o2 = 2, plot = FALSE)

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

conv.crft1 <- convert_rate.ft(crft1, o2.unit = "mg/l", flowrate.unit = "L/m",
                              output.unit = "mg/h",
                              mass = NULL, area = NULL,
                              S=S, t=t, P=P)
conv.crft1.ms <- convert_rate.ft(crft1, o2.unit = "mg/l", flowrate.unit = "L/m",
                                 output.unit = "mg/h/g",
                                 mass = 0.4, area = NULL,
                                 S=S, t=t, P=P)
conv.crft1.as <- convert_rate.ft(crft1, o2.unit = "mg/l", flowrate.unit = "L/m",
                                 output.unit = "mg/h/cm2",
                                 mass = NULL, area = 0.01,
                                 S=S, t=t, P=P)
conv.crftmany <- convert_rate.ft(crftmany, o2.unit = "mg/l", flowrate.unit = "L/m",
                                 output.unit = "mg/h",
                                 mass = NULL, area = NULL,
                                 S=S, t=t, P=P)
conv.crftmany.ms <- convert_rate.ft(crftmany, o2.unit = "mg/l", flowrate.unit = "L/m",
                                    output.unit = "mg/h/g",
                                    mass = 0.4, area = NULL,
                                    S=S, t=t, P=P)
conv.crftmany.as <- convert_rate.ft(crftmany, o2.unit = "mg/l", flowrate.unit = "L/m",
                                    output.unit = "mg/h/cm2",
                                    mass = NULL, area = 0.01,
                                    S=S, t=t, P=P)
conv.crftwidth <- convert_rate.ft(crftwidth, o2.unit = "mg/l", flowrate.unit = "L/m",
                                  output.unit = "mg/h",
                                  mass = NULL, area = NULL,
                                  S=S, t=t, P=P)
conv.crftwidth.ms <- convert_rate.ft(crftwidth, o2.unit = "mg/l", flowrate.unit = "L/m",
                                     output.unit = "mg/h/g",
                                     mass = 0.4, area = NULL,
                                     S=S, t=t, P=P)
conv.crftwidth.as <- convert_rate.ft(crftwidth, o2.unit = "mg/l", flowrate.unit = "L/m",
                                     output.unit = "mg/h/cm2",
                                     mass = NULL, area = 0.01,
                                     S=S, t=t, P=P)

conv.adjft.1 <- convert_rate.ft(adjft.1, o2.unit = "mg/l", flowrate.unit = "L/m",
                                output.unit = "mg/h",
                                mass = NULL, area = NULL,
                                S=S, t=t, P=P)
conv.adjft.1.ms <- convert_rate.ft(adjft.1, o2.unit = "mg/l", flowrate.unit = "L/m",
                                   output.unit = "mg/h/g",
                                   mass = 0.4, area = NULL,
                                   S=S, t=t, P=P)
conv.adjft.1.as <- convert_rate.ft(adjft.1, o2.unit = "mg/l", flowrate.unit = "L/m",
                                   output.unit = "mg/h/cm2",
                                   mass = NULL, area = 0.01,
                                   S=S, t=t, P=P)
conv.adjft.many <- convert_rate.ft(adjft.many, o2.unit = "mg/l", flowrate.unit = "L/m",
                                   output.unit = "mg/h",
                                   mass = NULL, area = NULL,
                                   S=S, t=t, P=P)
conv.adjft.many.ms <- convert_rate.ft(adjft.many, o2.unit = "mg/l", flowrate.unit = "L/m",
                                      output.unit = "mg/h/g",
                                      mass = 0.4, area = NULL,
                                      S=S, t=t, P=P)
conv.adjft.many.as <- convert_rate.ft(adjft.many, o2.unit = "mg/l", flowrate.unit = "L/m",
                                      output.unit = "mg/h/cm2",
                                      mass = NULL, area = 0.01,
                                      S=S, t=t, P=P)
conv.adjft.width <- convert_rate.ft(adjft.width, o2.unit = "mg/l", flowrate.unit = "L/m",
                                    output.unit = "mg/h",
                                    mass = NULL, area = NULL,
                                    S=S, t=t, P=P)
conv.adjft.width.ms <- convert_rate.ft(adjft.width, o2.unit = "mg/l", flowrate.unit = "L/m",
                                       output.unit = "mg/h/g",
                                       mass = 0.4, area = NULL,
                                       S=S, t=t, P=P)
conv.adjft.width.as <- convert_rate.ft(adjft.width, o2.unit = "mg/l", flowrate.unit = "L/m",
                                       output.unit = "mg/h/cm2",
                                       mass = NULL, area = 0.01,
                                       S=S, t=t, P=P)

# validate 'x' inputs -----------------------------------------------------

test_that("convert_rate.ft - stops if 'x' input not numeric, calc_rate.ft, or adjust_rate.ft", {
  expect_error(convert_rate.ft("string",
                               o2.unit = "mg/l", flowrate.unit = "l/s"),
               regexp = "convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")
  expect_error(convert_rate.ft(flowthrough.rd,
                               o2.unit = "mg/l", flowrate.unit = "l/s"),
               regexp = "convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")
  expect_error(convert_rate.ft(inspect(flowthrough.rd, plot = FALSE),
                               o2.unit = "mg/l", flowrate.unit = "l/s"),
               regexp = "convert_rate.ft: 'x' must be an `calc_rate.ft` or `adjust_rate.ft` object, or a numeric value or vector.")
})

test_that("convert_rate.ft - stops if 'o2.unit' is missing", {
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = NULL, flowrate.unit = "l/s"),
               regexp = "convert_rate.ft: 'o2.unit' input is required.")
})

test_that("convert_rate.ft - stops if 'flowrate.unit' is missing", {
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mg/l", flowrate.unit = NULL),
               regexp = "convert_rate.ft: 'flowrate.unit' input is required.")
})

test_that("convert_rate.ft - `output.unit` defaults successfully applied if missing", {
  # absolute
  expect_warning(convert_rate.ft(-1.4,
                                 o2.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = NULL),
                 regexp = "convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h'.")
  expect_identical(suppressWarnings(convert_rate.ft(-1.4,
                                                    o2.unit = "mg/l", flowrate.unit = "l/s",
                                                    output.unit = NULL))$output.unit,
                   "mgO2/hour")
  # mass-spec
  expect_warning(convert_rate.ft(-1.4,
                                 o2.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = NULL, mass = 2),
                 regexp = "convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h/kg'.")
  expect_identical(suppressWarnings(convert_rate.ft(-1.4,
                                                    o2.unit = "mg/l", flowrate.unit = "l/s",
                                                    output.unit = NULL, mass = 2))$output.unit,
                   "mgO2/hour/kg")
  # area-spec
  expect_warning(convert_rate.ft(-1.4,
                                 o2.unit = "mg/l", flowrate.unit = "l/s",
                                 output.unit = NULL, area = 2),
                 regexp = "convert_rate.ft: the 'output.unit' is not provided, applying default 'mgO2/h/m2'.")
  expect_identical(suppressWarnings(convert_rate.ft(-1.4,
                                                    o2.unit = "mg/l", flowrate.unit = "l/s",
                                                    output.unit = NULL, area = 2))$output.unit,
                   "mgO2/hour/m2")
})

test_that("convert_rate.ft - stops is both 'mass' and 'area' have inputs.", {
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mg/l", flowrate.unit = "l/s",
                               output.unit = NULL, area = 2, mass = 3),
               regexp = "convert_rate.ft: Cannot have inputs for both 'mass' and 'area'.")
})

test_that("convert_rate.ft - stops if 'output.unit' requires a 'mass' or 'area' input and neither is provided", {
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mg/l", flowrate.unit = "l/s",
                               output.unit = "mg/h/kg", area = NULL, mass = NULL),
               "convert_rate.ft: 'output.unit' requires a value for 'mass' or 'area'")
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mg/l", flowrate.unit = "l/s",
                               output.unit = "mg/h/m2", area = NULL, mass = NULL),
               "convert_rate.ft: 'output.unit' requires a value for 'mass' or 'area'")
})

test_that("convert_rate.ft - stops if a 'mass' or 'area' input entered but output.unit is not mass- or area-specific", {
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mg/l", flowrate.unit = "l/s",
                               output.unit = "mg/h", area = NULL, mass = 2),
               "convert_rate.ft: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mg/l", flowrate.unit = "l/s",
                               output.unit = "mg/h", area = 2, mass = NULL),
               "convert_rate.ft: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")
})

test_that("convert_rate.ft - stops if units require t, S and P", {

  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mL/L", flowrate.unit = "l/s",
                               output.unit = "mg/h/mm2", area = 2, mass = NULL,
                               S = NULL, t = NULL, P = NULL),
               "convert_DO: Input or output units require Salinity input")
  expect_error(convert_rate.ft(-1.4,
                               o2.unit = "mL/L", flowrate.unit = "l/s",
                               output.unit = "mg/h/mm2", area = 2, mass = NULL,
                               S = 35, t = NULL, P = NULL),
               "convert_DO: Input or output units require Temperature input")
  expect_message(convert_rate.ft(-1.4,
                                 o2.unit = "mL/L", flowrate.unit = "l/s",
                                 output.unit = "mg/h/mm2", area = 2, mass = NULL,
                                 S = 35, t = 15, P = NULL),
                 "convert_DO: Input or output units require Atmospheric Pressure input")
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
                    o2.unit = "mg/l", flowrate.unit = "l/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    convert_rate(-1.4,
                 o2.unit = "mg/l", time.unit = "s", volume = 1,
                 output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
  )
  expect_equal(
    convert_rate.ft(-0.001,
                    o2.unit = "mg/l", flowrate.unit = "ml/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    convert_rate(-0.001,
                 o2.unit = "mg/l", time.unit = "s", volume = 1/1000,
                 output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
  )
  expect_equal(
    convert_rate.ft(-455,
                    o2.unit = "mg/l", flowrate.unit = "ul/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    convert_rate(-455,
                 o2.unit = "mg/l", time.unit = "s", volume = 1/1000000,
                 output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
  )
  expect_equal(
    convert_rate.ft(-455,
                    o2.unit = "mg/l", flowrate.unit = "ul/d",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    convert_rate(-455,
                 o2.unit = "mg/l", time.unit = "d", volume = 1/1000000,
                 output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
  )
  expect_equal(
    convert_rate.ft(crft1,
                    o2.unit = "mg/l", flowrate.unit = "ml/min",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    convert_rate(crft1$rate,
                 o2.unit = "mg/l", time.unit = "m", volume = 1/1000,
                 output.unit = "mg/h", area = NULL, mass = NULL)$rate.output
  )
  expect_equal(
    convert_rate.ft(crftmany,
                    o2.unit = "mg/l", flowrate.unit = "ml/min",
                    output.unit = "umol h-1", area = NULL, mass = NULL)$rate.output,
    convert_rate(crftmany$rate,
                 o2.unit = "mg/l", time.unit = "m", volume = 1/1000,
                 output.unit = "umol h-1", area = NULL, mass = NULL)$rate.output
  )
  expect_equal(
    convert_rate.ft(crftvec,
                    o2.unit = "mg/l", flowrate.unit = "ml/min",
                    output.unit = "mL s-1", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    convert_rate(crftvec$rate,
                 o2.unit = "mg/l", time.unit = "m", volume = 1/1000,
                 output.unit = "mL s-1", area = NULL, mass = NULL,
                 S=S, t=t, P=P)$rate.output
  )
  expect_equal(
    convert_rate.ft(adjft.1,
                    o2.unit = "mg/l", flowrate.unit = "ml/min",
                    output.unit = "mL s-1", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    convert_rate(adjft.1$rate.adjusted,
                 o2.unit = "mg/l", time.unit = "m", volume = 1/1000,
                 output.unit = "mL s-1", area = NULL, mass = NULL,
                 S=S, t=t, P=P)$rate.output
  )
  expect_equal(
    convert_rate.ft(adjft.many,
                    o2.unit = "%Air", flowrate.unit = "ml/min",
                    output.unit = "mL s-1", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    convert_rate(adjft.many$rate.adjusted,
                 o2.unit = "%Air", time.unit = "m", volume = 1/1000,
                 output.unit = "mL s-1", area = NULL, mass = NULL,
                 S=S, t=t, P=P)$rate.output
  )
  expect_equal(
    convert_rate.ft(adjft.width,
                    o2.unit = "%o2", flowrate.unit = "ml/min",
                    output.unit = "mol s-1", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    convert_rate(adjft.width$rate.adjusted,
                 o2.unit = "%o2", time.unit = "m", volume = 1/1000,
                 output.unit = "mol s-1", area = NULL, mass = NULL,
                 S=S, t=t, P=P)$rate.output
  )
  expect_equal(
    convert_rate.ft(adjft.prod,
                    o2.unit = "umol/kg", flowrate.unit = "l/h",
                    output.unit = "mol s-1", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    convert_rate(adjft.prod$rate.adjusted,
                 o2.unit = "umol/kg", time.unit = "h", volume = 1/1,
                 output.unit = "mol s-1", area = NULL, mass = NULL,
                 S=S, t=t, P=P)$rate.output
  )
  expect_equal(
    convert_rate.ft(adjft.vec,
                    o2.unit = "hPa", flowrate.unit = "l/h",
                    output.unit = "ml d-1", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    convert_rate(adjft.vec$rate.adjusted,
                 o2.unit = "hPa", time.unit = "h", volume = 1/1,
                 output.unit = "ml d-1", area = NULL, mass = NULL,
                 S=S, t=t, P=P)$rate.output
  )
})

test_that("convert_rate.ft - outputs some known specific values", {
  # this is mostly to guard against future changes breaking the outputs
  expect_equal(
    # -1 mg in l/s should be -1*60*60  in mg/h
    convert_rate.ft(-1,
                    o2.unit = "mg/l", flowrate.unit = "l/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    -1*60*60)

  expect_equal(
    convert_rate.ft(-2,
                    o2.unit = "mg/l", flowrate.unit = "l/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    -2*60*60)

  expect_equal(
    convert_rate.ft(2,
                    o2.unit = "mg/l", flowrate.unit = "l/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    2*60*60)
  expect_equal(
    convert_rate.ft(-0.01,
                    o2.unit = "ug/l", flowrate.unit = "l/s",
                    output.unit = "mg/h", area = NULL, mass = NULL)$rate.output,
    -0.01*60*60 / 1000)
  expect_equal(
    convert_rate.ft(-0.01,
                    o2.unit = "ml/l", flowrate.unit = "l/s",
                    output.unit = "mg/h", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    -48.6154742081153)
  expect_equal(
    convert_rate.ft(-1,
                    o2.unit = "ml/l", flowrate.unit = "l/s",
                    output.unit = "umol/h", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    -151929.054239894)
  expect_equal(
    convert_rate.ft(-0.01,
                    o2.unit = "umol/l", flowrate.unit = "l/s",
                    output.unit = "mg/s", area = NULL, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    -0.000319988)
  expect_equal(
    convert_rate.ft(-0.01,
                    o2.unit = "umol/l", flowrate.unit = "l/s",
                    output.unit = "mg/s/kg", area = NULL, mass = 0.01,
                    S=S, t=t, P=P)$rate.output,
    -0.000319988/0.01)
  expect_equal(
    convert_rate.ft(-0.01,
                    o2.unit = "umol/l", flowrate.unit = "l/s",
                    output.unit = "mg/s/m2", area = 0.001, mass = NULL,
                    S=S, t=t, P=P)$rate.output,
    -0.000319988/0.001)
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
    expect_output(summary(eval(parse(text=z))),
                  regexp = "rank rate.input")
  })

})

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


# Extensive output tests - skipped ----------------------------------------

# This creates a matrix of every combination of input and output values and
# units, adds the appropriate divisor for the volume unit, and an iteration
# number. Then expect_equal compares the outputs of cr and crft, prints the
# inputs so you can see where it stops if it meets an error.

test_that("convert_rate and convert_rate.ft output same results - huge block of tests", {
  skip("skip - because these take forever")
  #job::job({
    # Absolute rates ----------------------------------------------------------

    inputs_abs <- list(
      # random rates
      o2.rates = c(-0.002755, -0.035, -0.88, -5.42, 0.00132, 0.0484, 0.5902, 6.4747),
      # input o2 units
      o2.units = c("mg/l", "hPa", "ug/l", "%Air", "mmol/L", "umol/kg", "inHg", "mL/L"),
      # flow units separated
      flow.vol.units = c("ul", "ml", "L"),
      flow.time.units = c("s", "m", "h", "d"),
      # output units
      out.units = c("ug/s", "mg/min", "umol/h", "mmol/day", "mL/min")
    )

    # all combinations
    grid_abs <- expand.grid(inputs_abs, stringsAsFactors = FALSE)
    # create flow units
    grid_abs$flow.units <- paste(grid_abs$flow.vol.units, grid_abs$flow.time.units, sep = "/")
    # add appropriate volume divisor
    grid_abs$vol.div <- apply(grid_abs, 1, function(z) {
      if(z[[3]] == "ul") return(1000000) else
        if(z[[3]] == "ml") return(1000) else
          if(z[[3]] == "L") return(1)
    })
    # add iteration
    grid_abs$iter <- 1:nrow(grid_abs)

    # S t P for units which require them
    S = 30
    t = 15
    P = 1

    test_that("convert_rate and convert_rate.ft output same results - ABSOLUTE RATES", {
      apply(grid_abs, 1, function(z) {
        expect_equal(suppressMessages(convert_rate.ft(as.numeric(z[[1]]),
                                                      o2.unit = z[[2]],
                                                      flowrate.unit = z[[6]],
                                                      output.unit = z[[5]],
                                                      area = NULL, mass = NULL,
                                                      S = S, t = t, P = P))$rate.output,

                     suppressMessages(convert_rate(as.numeric(z[[1]]),
                                                   o2.unit = z[[2]],
                                                   time.unit = z[[4]],
                                                   volume = 1/as.numeric(z[[7]]),
                                                   output.unit = z[[5]],
                                                   area = NULL, mass = NULL,
                                                   S = S, t = t, P = P))$rate.output,
                     label = glue::glue("FAILED on row {z[[8]]}"))
        #print(paste(z))
      })
    })

    # Mass-specific rates -----------------------------------------------------
    # not row indexes change because of extra mass/area columns
    inputs_ms <- list(
      # random rates
      o2.rates = c(-0.002755, -0.035, -0.88, -5.42, 0.00132, 0.0484, 0.5902, 6.4747),
      # input o2 units
      o2.units = c("mg/l", "hPa", "ug/l", "%Air", "mmol/L", "umol/kg", "inHg", "mL/L"),
      # flow units separated
      flow.vol.units = c("ul", "ml", "L"),
      flow.time.units = c("s", "m", "h", "d"),
      # output units - mass spec
      out.units = c("ug/s/ug", "mg/min/mg", "umol/h/g", "mmol/day/kg", "mL/min/g"),
      mass = c(0.0034, 0.065, 0.122, 2.78, 87.6) # all in kg
    )

    # all combinations
    grid_ms <- expand.grid(inputs_ms, stringsAsFactors = FALSE)
    # create flow units
    grid_ms$flow.units <- paste(grid_ms$flow.vol.units, grid_ms$flow.time.units, sep = "/")
    # add appropriate volume divisor
    grid_ms$vol.div <- apply(grid_ms, 1, function(z) {
      if(z[[3]] == "ul") return(1000000) else
        if(z[[3]] == "ml") return(1000) else
          if(z[[3]] == "L") return(1)
    })
    # add iteration
    grid_ms$iter <- 1:nrow(grid_ms)

    # S t P for units which require them
    S = 30
    t = 15
    P = 1

    test_that("convert_rate and convert_rate.ft output same results - MASS SPECIFIC", {
      apply(grid_ms, 1, function(z) {
        expect_equal(suppressMessages(convert_rate.ft(as.numeric(z[[1]]),
                                                      o2.unit = z[[2]],
                                                      flowrate.unit = z[[7]],
                                                      output.unit = z[[5]],
                                                      area = NULL,
                                                      mass = as.numeric(z[[6]]),
                                                      S = S, t = t, P = P))$rate.output,

                     suppressMessages(convert_rate(as.numeric(z[[1]]),
                                                   o2.unit = z[[2]],
                                                   time.unit = z[[4]],
                                                   volume = 1/as.numeric(z[[8]]),
                                                   output.unit = z[[5]],
                                                   area = NULL,
                                                   mass = as.numeric(z[[6]]),
                                                   S = S, t = t, P = P))$rate.output,
                     label = glue::glue("FAILED on row {z[[9]]}"))
        #print(paste(z))
      })
    })

    # Area-specific rates -----------------------------------------------------

    inputs_as <- list(
      # random rates
      o2.rates = c(-0.002755, -0.035, -0.88, -5.42, 0.00132, 0.0484, 0.5902, 6.4747),
      # input o2 units
      o2.units = c("mg/l", "hPa", "ug/l", "%Air", "mmol/L", "umol/kg", "inHg", "mL/L"),
      # flow units separated
      flow.vol.units = c("ul", "ml", "L"),
      flow.time.units = c("s", "m", "h", "d"),
      # output units - area spec
      out.units = c("ug/s/mm2", "mg/min/cm2", "umol/h/m2", "mmol/day/km2", "mL/min/mm2"),
      area = c(0.0034, 0.065, 0.122, 2.78, 87.6) # all in m2
    )

    # all combinations
    grid_as <- expand.grid(inputs_as, stringsAsFactors = FALSE)
    # create flow units
    grid_as$flow.units <- paste(grid_as$flow.vol.units, grid_as$flow.time.units, sep = "/")
    # add appropriate volume divisor
    grid_as$vol.div <- apply(grid_as, 1, function(z) {
      if(z[[3]] == "ul") return(1000000) else
        if(z[[3]] == "ml") return(1000) else
          if(z[[3]] == "L") return(1)
    })
    # add iteration
    grid_as$iter <- 1:nrow(grid_as)

    # S t P for units which require them
    S = 30
    t = 15
    P = 1

    test_that("convert_rate and convert_rate.ft output same results - AREA SPECIFIC", {
      apply(grid_as, 1, function(z) {
        expect_equal(suppressMessages(convert_rate.ft(as.numeric(z[[1]]),
                                                      o2.unit = z[[2]],
                                                      flowrate.unit = z[[7]],
                                                      output.unit = z[[5]],
                                                      mass = NULL,
                                                      area = as.numeric(z[[6]]),
                                                      S = S, t = t, P = P))$rate.output,

                     suppressMessages(convert_rate(as.numeric(z[[1]]),
                                                   o2.unit = z[[2]],
                                                   time.unit = z[[4]],
                                                   volume = 1/as.numeric(z[[8]]),
                                                   output.unit = z[[5]],
                                                   mass = NULL,
                                                   area = as.numeric(z[[6]]),
                                                   S = S, t = t, P = P))$rate.output,
                     label = glue::glue("FAILED on row {z[[9]]}"))
        #print(paste(z))
      })
    })
  #}) #job::job end
})


}) ## turns printing back on
