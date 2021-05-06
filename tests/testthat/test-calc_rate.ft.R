# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-calc_rate.ft.R")
# covr::file_coverage("R/calc_rate.ft.R", "tests/testthat/test-calc_rate.ft.R")
# x <- covr::package_coverage()
# covr::report(x)

insp.ft.obj <- suppressMessages(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2, in.o2 = 3, plot = FALSE))
insp.ft.mult.obj <- suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4, in.o2 = 5:7, plot = FALSE))


# 'flowrate' input --------------------------------------------------------

test_that("calc_rate.ft - stops if 'flowrate' input malformed", {
  # non-numeric
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = "string"),
               regexp = "calc_rate.ft: 'flowrate' input is not numeric.")
  # null
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = NULL),
               regexp = "calc_rate.ft: 'flowrate' input is required.")
  # too long
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = c(1,2)),
               regexp = "calc_rate.ft: 'flowrate' only 1 inputs allowed.")
})

test_that("calc_rate.ft - accepts numeric 'flowrate'", {
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.0111),
               regexp = NA)
})


# 'by' input --------------------------------------------------------------

test_that("calc_rate.ft - stops if 'by' input not 'time' or 'row'.", {
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = "string"),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = 123),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = "oxygen"),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = "proportion"),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
})

test_that("calc_rate.ft - accepts correct 'by' inputs", {
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = "time"),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = "Time"),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = "row"),
               regexp = NA)
})

test_that("calc_rate.ft - applies default 'by = 'time' correctly", {
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.1, from = 2, to = 10,
                            by = NULL)$rate,
               expected = -0.7763734,
               tolerance = 1e-3)
})

test_that("calc_rate.ft - default of 'by = 'time'' correctly applied", {
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5,
                            by = NULL, plot = FALSE)$call$by,
               "time")
})

# 'x' input ---------------------------------------------------------------

# single numeric ----------------------------------------------------------
test_that("calc_rate.ft - accepts single numeric 'x' input", {
  expect_error(calc_rate.ft(-0.8, flowrate = 1.5, plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(0.8, flowrate = 1.5, plot = FALSE),
               regexp = NA)
})

test_that("calc_rate.ft - expected output with single numeric 'x' input", {
  expect_equal(calc_rate.ft(-0.8, flowrate = 1.5, plot = FALSE)$rate,
               -1.2)
  expect_equal(calc_rate.ft(0.8, flowrate = 1.5, plot = FALSE)$rate,
               1.2)
})

test_that("calc_rate.ft - delta output is correct with single numeric 'x' input", {
  expect_equal(calc_rate.ft(-0.8, flowrate = 1.5, plot = FALSE)$delta,
               -0.8)
  expect_equal(calc_rate.ft(0.8, flowrate = 1.5, plot = FALSE)$delta,
               0.8)
})

test_that("calc_rate.ft - single numeric 'x' input, message that other inputs ignored.", {
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, to = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, by = "row",
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, width = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, width = 10, by = "row", from = 2,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
})

test_that("calc_rate.ft - single numeric 'x' input, message that 'plot = TRUE' input ignored and does not produce output.", {
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                              plot = TRUE),
                 regexp = "calc_rate.ft: plot not available for single value rate calculations.")
  expect_output(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})


# dual numeric - outflow, inflow order ----------------------------------
test_that("calc_rate.ft - accepts dual numeric 'x' input", {
  expect_error(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(c(90,100), flowrate = 1.5, plot = FALSE),
               regexp = NA)
})

test_that("calc_rate.ft - expected output with dual numeric 'x' input", {
  expect_equal(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = FALSE)$rate,
               -0.75)
  expect_equal(calc_rate.ft(c(90,100), flowrate = 1.5, plot = FALSE)$rate,
               -15)
})

test_that("calc_rate.ft - delta output is correct with dual numeric 'x' input", {
  expect_equal(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = FALSE)$delta,
               7.5 - 8.0)
  expect_equal(calc_rate.ft(c(90,100), flowrate = 1.5, plot = FALSE)$delta,
               90 - 100)
})

test_that("calc_rate.ft - dual numeric 'x' input, message that other inputs ignored.", {
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, to = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, by = "row",
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, width = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, width = 10, by = "row", from = 2,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
})

test_that("calc_rate.ft - dual numeric 'x' input, message that 'plot = TRUE' input ignored and does not produce output.", {
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                              plot = TRUE),
                 regexp = "calc_rate.ft: plot not available for dual value rate calculations.")
  expect_output(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})


# disallowed inputs -------------------------------------------------------

# stop if inspect_data obj
test_that("calc_rate.ft - stops with `inspect_data` 'x' input", {
  expect_error(calc_rate.ft(
    suppressWarnings(suppressMessages(inspect_data(flowthrough.rd, plot = F))), flowrate = 1.5, plot = FALSE),
    "calc_rate.ft: function does not accept 'inspect_data' objects. Please process the data via 'inspect.ft' instead.")
})

# stop if inspect obj
test_that("calc_rate.ft - stops with `inspect` 'x' input", {
  expect_error(calc_rate.ft(
    suppressWarnings(suppressMessages(inspect(flowthrough.rd, plot = F))), flowrate = 1.5, plot = FALSE),
    "calc_rate.ft: function does not accept 'inspect' objects. Please process the data via 'inspect.ft' instead.")
})

test_that("calc_rate.ft - stops with any other disallowed 'x' input", {
  # df
  expect_error(calc_rate.ft(flowthrough.rd, flowrate = 1.5, plot = FALSE),
               "calc_rate.ft: 'x' must be an `inspect.ft` object, a single value, or a vector of two values. See Help.")
  # longer than 2 vector
  expect_error(calc_rate.ft(1:10, flowrate = 1.5, plot = FALSE),
               "calc_rate.ft: 'x' must be an `inspect.ft` object, a single value, or a vector of two values. See Help.")
  # dt
  expect_error(calc_rate.ft(matrix(1:48, nrow = 4), flowrate = 1.5, plot = FALSE),
               "calc_rate.ft: 'x' must be an `inspect.ft` object, a single value, or a vector of two values. See Help.")
})



# inspect.ft input --------------------------------------------------------

test_that("calc_rate.ft - accepts 'inspect.ft' input, gives correct messages and warnings.", {
  expect_error(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE),
               NA)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE),
                 "calc_rate.ft: calculating rate from 'inspect.ft' object.")
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.mult.obj, flowrate = 1.5, plot = FALSE)),
               NA)
  expect_message(suppressWarnings(calc_rate.ft(insp.ft.mult.obj, flowrate = 1.5, plot = FALSE)),
                 "calc_rate.ft: calculating rate from 'inspect.ft' object.")
  expect_warning(calc_rate.ft(insp.ft.mult.obj, flowrate = 1.5, plot = FALSE),
                 "calc_rate.ft: multiple columns of delta O2 data found in input.")
})


test_that("calc_rate.ft - if 'from' and 'to' are NULL, defaults to using all data - by = 'time'.", {
  # from
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = 900, by = "time")$summary$from,
               0)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = 900, by = "time")$summary$from,
               "calc_rate.ft: 'from' input NULL. Applying default 'from' of first data value.")
  # to
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = 0, to = NULL, by = "time")$summary$to,
               934)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                              from = 0, to = NULL, by = "time")$summary$to,
               "calc_rate.ft: 'to' input NULL. Applying default 'to' of last data value.")
# both
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "time")$summary$from,
               0)
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "time")$summary$to,
               934)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                              from = NULL, to = NULL, by = "time")$summary$to,
                 "calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
})

test_that("calc_rate.ft - if 'from' and 'to' are NULL, defaults to using all data - by = 'time'.", {
  # from
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = 900, by = "time")$summary$from,
               0)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = 900, by = "time")$summary$from,
               "calc_rate.ft: 'from' input NULL. Applying default 'from' of first data value.")
  # to
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = 0, to = NULL, by = "time")$summary$to,
               934)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                              from = 0, to = NULL, by = "time")$summary$to,
               "calc_rate.ft: 'to' input NULL. Applying default 'to' of last data value.")
# both
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "time")$summary$from,
               0)
  expect_equal(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "time")$summary$to,
               934)
  expect_message(calc_rate.ft(insp.ft.obj, flowrate = 1.5, plot = FALSE,
                              from = NULL, to = NULL, by = "time")$summary$to,
                 "calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
})






#
#
# test_that("calc_rate.ft works with single values", {
#   expect_error(calc_rate.ft(inflow.o2 = 8.88, outflow.o2 = 8.17, flowrate =  0.00234,
#                             plot = FALSE),
#                regexp = NA)
# })
#
# test_that("calc_rate.ft works with single inflow and vector outflow", {
#   expect_error(calc_rate.ft(inflow.o2 = 8.88, outflow.o2 = flowthrough.rd$o2.out,
#                             flowrate =  0.00234, plot = FALSE),
#                regexp = NA)
# })
#
# test_that("calc_rate.ft works with multiple vectors", {
#   expect_error(calc_rate.ft(inflow.o2 = flowthrough.rd$o2.in,
#                             outflow.o2 = flowthrough.rd$o2.out, flowrate =  0.00234,
#                             plot = FALSE),
#                regexp = NA)
# })
#
# test_that("calc_rate.ft stops with x = NULL and one of inflow.o2 or outflow.o2 missing", {
#   expect_error(calc_rate.ft(inflow.o2 = 3.4,
#                             outflow.o2 = NULL,
#                             flowrate =  0.00234),
#                regexp = "Both 'inflow.o2' and 'outflow.o2' inputs should have a value.")
#   expect_error(calc_rate.ft(inflow.o2 = NULL,
#                             outflow.o2 = 3.4,
#                             flowrate =  0.00234),
#                regexp = "Both 'inflow.o2' and 'outflow.o2' inputs should have a value.")
# })
#
# test_that("calc_rate.ft works with data.frame", {
#   expect_error(calc_rate.ft(flowthrough.rd, outflow.o2 = 2,
#                             inflow.o2 = 3, flowrate =  0.00234,
#                             plot = FALSE),
#                regexp = NA)
# })
#
# test_that("calc_rate.ft works with `inspect` object", {
#   ftdat <- suppressWarnings(inspect(flowthrough.rd, oxygen = NULL, plot = FALSE))
#   expect_error(calc_rate.ft(ftdat, outflow.o2 = 2,
#                             inflow.o2 = 3, flowrate =  0.00234,
#                             plot = FALSE),
#                regexp = NA)
# })
#
# test_that("calc_rate.ft works with `inspect_data` object (OLD INSPECT FUNCTION)", {
#   sink("/dev/null") ## stops printing outputs on assigning
#   ftdat_oldinsp <- suppressWarnings(inspect_data(flowthrough.rd, time = 1, outflow.o2 = 2, inflow.o2 = 3,
#                                                  plot = FALSE))
#   expect_error(calc_rate.ft(ftdat_oldinsp, outflow.o2 = 2, inflow.o2 = 3, flowrate =  0.00234,
#                             plot = FALSE),
#                regexp = NA)
#   sink() ## turns printing back on
# })
#
# test_that("calc_rate.ft stops if flowrate missing", {
#   expect_error(calc_rate.ft(flowthrough.rd, outflow.o2 = 2,
#                             inflow.o2 = 3, flowrate = NULL),
#                regexp = "numeric 'flowrate' value must be provided.")
# })
#
# test_that("calc_rate.ft stops if df/inspect object and inflow.o2 OR outflow.o2 OR BOTH missing", {
#   expect_error(calc_rate.ft(flowthrough.rd, outflow.o2 = NULL,
#                             inflow.o2 = 3, flowrate =  0.00234),
#                regexp = "Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")
#
#   ftdat <- suppressWarnings(inspect(flowthrough.rd, oxygen = NULL, plot = FALSE))
#   expect_error(calc_rate.ft(ftdat, outflow.o2 = 2, # with inspect obj
#                             inflow.o2 = NULL, flowrate =  0.00234),
#                regexp = "Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")
#
#   expect_error(calc_rate.ft(ftdat, outflow.o2 = NULL,
#                             inflow.o2 = NULL, flowrate =  0.00234),
#                regexp = "Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")
# })
#
# test_that("calc_rate.ft output converts ok", {
#   ftdat <- suppressWarnings(inspect(flowthrough.rd, oxygen = NULL, plot = FALSE))
#   ftrt <- calc_rate.ft(ftdat, outflow.o2 = 2,
#                        inflow.o2 = 3, flowrate =  0.00234, plot = F)
#
#   ftrt_conv <- suppressWarnings(convert_rate(ftrt, o2.unit = "mg/l", time.unit = "m", output.unit = "mg/h",
#                                              volume = 1))
#   expect_equal(round(ftrt_conv$output.rate, 8),
#                -0.09909348)
#
#   ftrt_conv_ms <- suppressWarnings(convert_rate(ftrt, o2.unit = "mg/l", time.unit = "m", output.unit = "mg/h/g",
#                                                 volume = 1, mass = 0.000070))
#   expect_equal(round(ftrt_conv_ms$output.rate, 6),
#                -1.415621)
# })
#
# test_that("calc_rate.ft S3 generics work", {
#   ftdat <-suppressWarnings(inspect(flowthrough.rd, oxygen = 2:3, plot = FALSE))
#   ftrt <- calc_rate.ft(ftdat, outflow.o2 = 2,
#                        inflow.o2 = 3, flowrate =  0.00234, plot = FALSE)
#   expect_output(print(ftrt))
#   expect_output(summary(ftrt))
#   #expect_output(plot(ftrt)) ## this fails - don't know why - it plots
#   expect_error(plot(ftrt), regexp = NA) ## alternative to above?
# })
#
