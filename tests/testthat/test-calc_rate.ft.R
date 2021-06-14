# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-calc_rate.ft.R")
# covr::file_coverage("R/calc_rate.ft.R", "tests/testthat/test-calc_rate.ft.R")
# x <- covr::package_coverage()
# covr::report(x)

capture.output({  ## stops printing console outputs on assigning


# Create testing objects --------------------------------------------------
#
## df
df <-  data.frame(time = flowthrough.rd[[2]], delta = flowthrough.rd[[3]])

## inspect.ft objects
insp.ft.obj.outo2.1col.ino2.1col<- suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1,
                                                               out.o2 = 2, in.o2 = 5, plot = FALSE))
insp.ft.obj.outo2.1col.ino2.value <- suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2,
                                                                 in.o2.value = 9.05, delta.o2 = NULL, plot = F))
insp.ft.obj.delta.1col <- suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = NULL,
                                                      in.o2 = NULL, delta.o2 = 8, plot = F))

insp.ft.obj.outo2.multcols.ino2.multcols <-
  suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4, in.o2 = 5:7, plot = FALSE))

insp.ft.obj.outo2.multcols.ino2.value <-
  suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4, in.o2.value = 9.05, plot = FALSE))

insp.ft.obj.delta.multcols <-
  suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, delta.o2 = 8:10, plot = FALSE))

## calc_rate.ft objects for testing plotting, printing etc.
crft.obj.value <- calc_rate.ft(-0.8,
                               flowrate = 2, plot = FALSE)
crft.obj.vector <- calc_rate.ft(flowthrough.rd$o2.delta,
                               flowrate = 2, plot = FALSE)
crft.obj.df <- calc_rate.ft(data.frame(time = flowthrough.rd$o2.out,
                                       delta = flowthrough.rd$o2.in),
                               flowrate = 2, plot = FALSE)

crft.obj.1rate <- calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                               from = 5, to = 900,
                               flowrate = 2, plot = FALSE)
crft.obj.multrates <- calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                                   from = c(5, 1005, 2005), to = c(900, 1900, 2900),
                                   flowrate = 2, plot = FALSE)
crft.obj.multrates.many <- calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                                   from = c(100:201), to = c(1000:1101),
                                   flowrate = 2, plot = FALSE)
crft.obj.1rate.delta.only <- calc_rate.ft(insp.ft.obj.delta.1col,
                                          from = 5, to = 900,
                                          flowrate = 2, plot = FALSE)
crft.obj.multrate.delta.only <- calc_rate.ft(insp.ft.obj.delta.1col,
                                             from = c(5, 1005, 2005), to = c(900, 1900, 2900),
                                             flowrate = 2, plot = FALSE)


# 'flowrate' input --------------------------------------------------------

test_that("calc_rate.ft - stops if 'flowrate' input malformed", {
  # non-numeric
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            flowrate = "string", plot = FALSE),
               regexp = "calc_rate.ft: 'flowrate' input is not numeric.")
  # null
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            flowrate = NULL, plot = FALSE),
               regexp = "calc_rate.ft: 'flowrate' input is required.")
  # too long
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            flowrate = c(1,2), plot = FALSE),
               regexp = "calc_rate.ft: 'flowrate' only 1 inputs allowed.")
})

test_that("calc_rate.ft - accepts numeric 'flowrate'", {
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1, plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.0111, plot = FALSE),
               regexp = NA)
})



# 'x' input checks --------------------------------------------------------

# single numeric 'x' input ------------------------------------------------
test_that("calc_rate.ft - accepts single numeric 'x' input", {
  expect_error(calc_rate.ft(-0.8, flowrate = 1.5, plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(0.8, flowrate = 1.5, plot = FALSE),
               regexp = NA)
  expect_message(calc_rate.ft(0.8, flowrate = 1.5, plot = FALSE),
                 regexp = "calc_rate.ft: calculating rate from delta oxygen.")
})

test_that("calc_rate.ft - expected output with single numeric 'x' input", {
  expect_equal(calc_rate.ft(-0.8, flowrate = 1.5, plot = FALSE)$rate,
               -0.8 * 1.5)
  expect_equal(calc_rate.ft(0.8, flowrate = 1.5, plot = FALSE)$rate,
               0.8 * 1.5)
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
                 regexp = "calc_rate.ft: plot only available for 'inspect.ft' inputs.")
  expect_output(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})


# vector 'x' input --------------------------------------------------------
test_that("calc_rate.ft - accepts numeric vector 'x' input", {
  expect_error(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(c(90,100), flowrate = 1.5, plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(c(flowthrough.rd$o2.delta), flowrate = 1.5,
                            plot = FALSE),
               regexp = NA)
})

test_that("calc_rate.ft - expected output with dual numeric 'x' input", {
  expect_equal(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = FALSE)$rate,
               (c(7.5, 8.0)*1.5))
  expect_equal(calc_rate.ft(c(90, 100), flowrate = 1.5, plot = FALSE)$rate,
               (c(90, 100)*1.5))
  expect_equal(calc_rate.ft(c(flowthrough.rd$o2.delta), flowrate = 1.5,
                            plot = FALSE)$rate,
               (flowthrough.rd$o2.delta*1.5))
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
                 regexp = "calc_rate.ft: plot only available for 'inspect.ft' inputs.")
  expect_output(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})



# data.frame 'x' input --------------------------------------------------------
test_that("calc_rate.ft - accepts data.frame 'x' input", {
  expect_error(calc_rate.ft(df, flowrate = 1.5, plot = FALSE),
               regexp = NA)
})

test_that("calc_rate.ft - expected output with data.frame 'x' input", {
  expect_equal(calc_rate.ft(df, flowrate = 1.5, plot = FALSE)$rate,
               ((flowthrough.rd[[2]]-flowthrough.rd[[3]])*1.5))
})

test_that("calc_rate.ft - data.frame 'x' input, message that other inputs ignored.", {
  expect_message(calc_rate.ft(df, flowrate = 1.5, from = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, to = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, by = "row",
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, width = 10,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, width = 10, by = "row", from = 2,
                              plot = FALSE),
                 regexp = "calc_rate.ft: requires only 'x' and 'flowrate'. Additional inputs ignored.")
})

test_that("calc_rate.ft - data.frame 'x' input, message that 'plot = TRUE' input ignored and does not produce output.", {
  expect_message(calc_rate.ft(df, flowrate = 1.5, from = 10,
                              plot = TRUE),
                 regexp = "calc_rate.ft: plot only available for 'inspect.ft' inputs.")
  expect_output(calc_rate.ft(df, flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})


# disallowed inputs -------------------------------------------------------


# stop if inspect obj
test_that("calc_rate.ft - stops with `inspect` 'x' input", {
  expect_error(calc_rate.ft(
    suppressWarnings(suppressMessages(inspect(flowthrough.rd, plot = F))), flowrate = 1.5, plot = FALSE),
    "calc_rate.ft: function does not accept 'inspect' objects. Please process the data via 'inspect.ft' instead.")
})

test_that("calc_rate.ft - stops with any other disallowed 'x' input", {
  # df of 2 or more columns
  expect_error(calc_rate.ft(flowthrough.rd, flowrate = 1.5, plot = FALSE),
               "calc_rate.ft: 'x' must be an `inspect.ft` object, a numeric value or vector, or 2-column data.frame. See Help.")
  # string
  expect_error(calc_rate.ft("string", flowrate = 1.5, plot = T),
               "calc_rate.ft: 'x' must be an `inspect.ft` object, a numeric value or vector, or 2-column data.frame. See Help.")
  # string
  expect_error(calc_rate.ft(list(x = 1:10, y = 11:20), flowrate = 1.5, plot = T),
               "calc_rate.ft: 'x' must be an `inspect.ft` object, a numeric value or vector, or 2-column data.frame. See Help.")
})


# 'by' input --------------------------------------------------------------

test_that("calc_rate.ft - stops if 'by' input not 'time' or 'row'.", {
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "string", plot = FALSE),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = 123, plot = FALSE),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "oxygen", plot = FALSE),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "proportion", plot = FALSE),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
})

test_that("calc_rate.ft - accepts correct 'by' inputs", {
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "time", plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "Time", plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "row", plot = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "Row", plot = FALSE),
               regexp = NA)
})

test_that("calc_rate.ft - applies default 'by = 'time' correctly", {
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = NULL, plot = FALSE)$rate,
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[2:10]*1.5))
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            by = NULL, plot = FALSE)$by,
               "time")
})

# from/to input -----------------------------------------------------------

test_that("calc_rate.ft - stops if 'from' and 'to' are unequal lengths", {
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 2:3, to = 10,
                            by = "time", plot = FALSE),
               regexp = "calc_rate.ft: 'from' and 'to' have unequal lengths.")
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, from = 1:9, to = 10:40,
                            by = "time", plot = FALSE),
               regexp = "calc_rate.ft: 'from' and 'to' have unequal lengths.")
})

test_that("calc_rate.ft - stops if any paired 'from' and 'to' are of equal value", {
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            from = 2, to = 2,
                            by = "time", plot = FALSE),
               regexp = "calc_rate.ft: some 'from' values are equal to the paired values in 'to'.")
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            from = 1:10, to = c(8:16,10),
                            by = "time", plot = FALSE),
               regexp = "calc_rate.ft: some 'from' values are equal to the paired values in 'to'.")
})

test_that("calc_rate.ft - stops if any paired 'from' value is later than paired 'to' value", {
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            from = 3, to = 2,
                            by = "time", plot = FALSE),
               regexp = "calc_rate.ft: some 'from' values are greater than the paired values in 'to'.")
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            from = 20:29, to = 1:10,
                            by = "time", plot = FALSE),
               regexp = "calc_rate.ft: some 'from' values are greater than the paired values in 'to'.")
})



# width input -------------------------------------------------------------

test_that("calc_rate.ft - with 'width' input, correct message.", {
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                              width = 100,
                              by = "time",
                              plot = FALSE),
                 regexp = "calc_rate.ft: rates determined using a rolling 'width' of 100 time values.")
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                              width = 200,
                              by = "row",
                              plot = FALSE),
                 regexp = "calc_rate.ft: rates determined using a rolling 'width' of 200 row values.")
})

test_that("calc_rate.ft - with 'width' input and from/to inputs, correct message that they are ignored.", {
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                              width = 100,
                              from = 2, to = 90,
                              by = "time",
                              plot = FALSE),
                 regexp = "calc_rate.ft: a rolling 'width' has been specified, therefore 'from' and 'to' inputs will be ignored.")
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                              width = 100,
                              from = 2:5, to = 90:93,
                              by = "time",
                              plot = FALSE),
                 regexp = "calc_rate.ft: a rolling 'width' has been specified, therefore 'from' and 'to' inputs will be ignored.")
})

test_that("calc_rate.ft - 'width' input correctly verified.", {
  # numeric
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            width = "string",
                            by = "time",
                            plot = FALSE),
               regexp = "calc_rate.ft: 'width' input is not numeric.")
  # single value
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            width = 1:2,
                            by = "time",
                            plot = FALSE),
               regexp = "calc_rate.ft: 'width' only 1 inputs allowed.")
})


test_that("calc_rate.ft - 'width' outputs correct results.", {
  ## correct number of results in summary
  wd <- 100
  expect_equal(nrow(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                                 width = wd,
                                 by = "time",
                                 plot = FALSE)$summary),
               nrow(insp.ft.obj.outo2.1col.ino2.1col$dataframe)-wd)
  ## correct number of results in rate
  expect_equal(length(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                                   width = wd,
                                   by = "time",
                                   plot = FALSE)$rate),
               nrow(insp.ft.obj.outo2.1col.ino2.1col$dataframe)-wd)
  ## first result should equal this
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5,
                            width = wd,
                            by = "time",
                            plot = FALSE)$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[1:(wd+1)])*1.5)
})



# inspect.ft 'x' input --------------------------------------------------------

test_that("calc_rate.ft - accepts 'inspect.ft' input, gives correct messages and warnings.", {
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE),
               NA)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE),
                 "calc_rate.ft: calculating rate from 'inspect.ft' object.")
  # multiple columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE)),
               NA)
  expect_message(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE)),
                 "calc_rate.ft: calculating rate from 'inspect.ft' object.")
  expect_warning(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE),
                 "calc_rate.ft: Multiple columns of delta O2 data found in input.")
})


test_that("calc_rate.ft - if 'from' and 'to' are NULL, defaults to using all data - by = 'time'.", {
  # from
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = 900, by = "time")$from,
               1)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                              from = NULL, to = 900, by = "time")$summary$from,
                 "calc_rate.ft: 'from' input NULL. Applying default 'from' of first time value.")
  # to
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = 1, to = NULL, by = "time")$to,
               3740)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                              from = 1, to = NULL, by = "time")$summary$to,
                 "calc_rate.ft: 'to' input NULL. Applying default 'to' of last time value.")
  # both
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "time")$from,
               1)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "time")$to,
               3740)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                              from = NULL, to = NULL, by = "time")$summary$to,
                 "calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
})

test_that("calc_rate.ft - if 'from' and 'to' are NULL, defaults to using all data - by = 'row'.", {
  # from
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = 900, by = "row")$from,
               1)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                              from = NULL, to = 900, by = "row")$from,
                 "calc_rate.ft: 'from' input NULL. Applying default 'from' of first row.")
  # to
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = 1, to = NULL, by = "row")$to,
               3740)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                              from = 1, to = NULL, by = "row")$to,
                 "calc_rate.ft: 'to' input NULL. Applying default 'to' of last row.")
  # both
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "row")$from,
               1)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = NULL, to = NULL, by = "row")$to,
               3740)
  expect_message(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                              from = NULL, to = NULL, by = "row")$to,
                 "calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
})

# insp.ft.obj.outo2.1col.ino2.1col
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with single columns of in.o2 and out.o2.", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "row")$rate,
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               c(
                 mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5)
  )
  ## mean of all
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               (c(
                 mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.1col$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5)
               ))
})

# insp.ft.obj.outo2.1col.ino2.value
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with single column of out.o2 and in.o2.value.", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[from:to])*1.5)
  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "row")$rate,
               mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate[1],
               mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               c(
                 mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5)
  )
  ## mean of all
  expect_equal(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               (c(
                 mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.1col.ino2.value$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5)
               ))
})


# insp.ft.obj.delta.1col

test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with single column of delta oxygen data", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate[1],
               mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][from:to])*1.5)
  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from):(to)])*1.5)
  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "row")$rate,
               mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate[1],
               mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               c(
                 mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[3]):(to[3])])*1.5)
  )
  ## mean of all
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = FALSE,
                            from = from, to = to, by = "time")$rate,
               (c(
                 mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.o2[[1]][(from[3]):(to[3])])*1.5)
               ))
})


# insp.ft.obj.outo2.multcols.ino2.multcols
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with multiple columns of both in.o2 and out.o2 data", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")$rate[1]),
               mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[from:to])*1.5)
  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "row"))$rate,
               mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate[1],
               mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               c(
                 mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5))

  ## mean of all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               (c(
                 mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.multcols$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5)))
})

# insp.ft.obj.outo2.multcols.ino2.value
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with multiple columns of out.o2 data and in.o2.value", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")$rate[1]),
               mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[from:to])*1.5)
  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from):(to)])*1.5)
  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "row"))$rate,
               mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate[1],
               mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               c(
                 mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5))

  ## mean of all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outo2.multcols.ino2.value, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               (c(
                 mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outo2.multcols.ino2.value$dataframe$delta.o2.calc.1[(from[3]):(to[3])])*1.5)))
})

# insp.ft.obj.delta.multcols
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with multiple columns of delta.o2 data", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")$rate[1]),
               mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][from:to])*1.5)
  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from):(to)])*1.5)
  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "row"))$rate,
               mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate[1],
               mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               c(
                 mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[3]):(to[3])])*1.5))

  ## mean of all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = FALSE,
                                             from = from, to = to, by = "time"))$rate,
               (c(
                 mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.o2[[1]][(from[3]):(to[3])])*1.5)))
})



# S3 checks -----------------------------------------------------------

test_that("calc_rate.ft outputs plot", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            flowrate = 2, plot = TRUE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            flowrate = 2, plot = TRUE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.value,
                            flowrate = 2, plot = TRUE),
               regexp = NA)

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(plot(crft.obj.1rate),
               regexp = NA)
  expect_output(plot(crft.obj.1rate),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 1")
  expect_error(plot(crft.obj.multrates),
               regexp = NA)
  expect_output(plot(crft.obj.multrates),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3")
  expect_error(plot(crft.obj.multrates.many),
               regexp = NA)
  expect_output(plot(crft.obj.multrates.many),
                regexp = "calc_rate.ft: Plotting rate from position 1 of")
  expect_output(plot(crft.obj.multrates),
                regexp = "calc_rate.ft: Plotting rate from position 1 of")
  expect_error(plot(crft.obj.1rate.delta.only),
               regexp = NA)
  expect_output(plot(crft.obj.1rate.delta.only),
                regexp = "calc_rate.ft: Plotting rate from position 1 of")
  expect_error(plot(crft.obj.multrate.delta.only),
               regexp = NA)
  expect_output(plot(crft.obj.multrate.delta.only),
                regexp = "calc_rate.ft: Plotting rate from position 1 of")
  expect_output(plot(crft.obj.multrate.delta.only),
                regexp = "calc_rate.ft: Plotting rate from position 1 of")
})

test_that("calc_rate.ft output plot stops if input is not an inspect.ft object", {

  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  expect_error(plot(crft.obj.value),
               regexp = "calc_rate.ft: plot only available for 'inspect.ft' inputs.")
  expect_error(plot(crft.obj.vector),
               regexp = "calc_rate.ft: plot only available for 'inspect.ft' inputs.")
  expect_error(plot(crft.obj.df),
               regexp = "calc_rate.ft: plot only available for 'inspect.ft' inputs.")

})

test_that("calc_rate.ft output plots when 'pos' is used", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, pos = 2),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, pos = 2),
               regexp = NA)

  ## objects
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(plot(crft.obj.multrates, pos = 2),
               regexp = NA)
  expect_output(plot(crft.obj.multrates),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ...")
  expect_output(plot(crft.obj.multrates, pos = 2),
                regexp = "calc_rate.ft: Plotting rate from position 2 of 3 ...")
  expect_error(plot(crft.obj.multrate.delta.only, pos = 2),
               regexp = NA)
  expect_output(plot(crft.obj.multrate.delta.only),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ...")
  expect_output(plot(crft.obj.multrate.delta.only, pos = 2),
                regexp = "calc_rate.ft: Plotting rate from position 2 of 3 ...")
})

test_that("calc_rate.ft output plot stops when 'pos' is out of range", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, pos = 9),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 4 rates found.")
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, pos = 9),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 4 rates found.")
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, pos = 0),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 4 rates found.")

  ## objects
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(plot(crft.obj.multrates, pos = 4),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 3 rates found.")
  expect_error(plot(crft.obj.multrate.delta.only, pos = 4),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 3 rates found.")
  expect_error(plot(crft.obj.multrate.delta.only, pos = 4),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 3 rates found.")
  expect_error(plot(crft.obj.multrate.delta.only, pos = 4),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 3 rates found.")
  expect_error(plot(crft.obj.multrate.delta.only, pos = 0),
               regexp = "calc_rate.ft: Invalid 'pos' input: only 3 rates found.")
})

test_that("calc_rate.ft output plots when 'rate.rev' is used", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, rate.rev = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, rate.rev = FALSE),
               regexp = NA)

  ## objects
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(plot(crft.obj.multrates, rate.rev = FALSE),
               regexp = NA)
  expect_output(plot(crft.obj.multrates, rate.rev = FALSE),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ...")
  expect_output(plot(crft.obj.multrates, rate.rev = FALSE),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ...")
  expect_error(plot(crft.obj.multrate.delta.only, rate.rev = FALSE),
               regexp = NA)
  expect_output(plot(crft.obj.multrate.delta.only, rate.rev = FALSE),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ...")
  expect_output(plot(crft.obj.multrate.delta.only, rate.rev = FALSE),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ... \nTo plot others use 'pos'.")
})

test_that("calc_rate.ft output plots when 'legend' is used", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outo2.1col.ino2.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, legend = FALSE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, legend = FALSE),
               regexp = NA)

  ## objects
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(plot(crft.obj.multrates, legend = FALSE),
               regexp = NA)
  expect_output(plot(crft.obj.multrates, legend = FALSE),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ... \nTo plot others use 'pos'.")
  expect_error(plot(crft.obj.multrate.delta.only, legend = FALSE),
               regexp = NA)
  expect_output(plot(crft.obj.multrate.delta.only, legend = FALSE),
                regexp = "calc_rate.ft: Plotting rate from position 1 of 3 ... \nTo plot others use 'pos'.")
})

test_that("calc_rate.ft objects can be printed.", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(print(crft.obj.value),
               regexp = NA)
  expect_output(print(crft.obj.value),
                regexp = "Rate:")
  expect_error(print(crft.obj.vector),
               regexp = NA)
  expect_output(print(crft.obj.vector),
                regexp = "Rate:")
  expect_error(print(crft.obj.df),
               regexp = NA)
  expect_output(print(crft.obj.df),
                regexp = "Rate:")
  expect_error(print(crft.obj.1rate),
               regexp = NA)
  expect_output(print(crft.obj.1rate),
                regexp = "Rate:")
  expect_error(print(crft.obj.multrates),
               regexp = NA)
  expect_output(print(crft.obj.multrates),
                regexp = "Rank 1 of 3 rate")
  expect_error(print(crft.obj.multrates.many),
               regexp = NA)
  expect_output(print(crft.obj.multrates.many),
                regexp = "Rank 1 of 102 rate")
  expect_error(print(crft.obj.1rate.delta.only),
               regexp = NA)
  expect_output(print(crft.obj.1rate.delta.only),
                regexp = "Rate:")
  expect_error(print(crft.obj.multrate.delta.only),
               regexp = NA)
  expect_output(print(crft.obj.multrate.delta.only),
                regexp = "Rank 1 of 3 rate")
})

test_that("calc_rate.ft objects can be printed with 'pos' input.", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only

  expect_error(print(crft.obj.vector, pos = 10),
               regexp = NA)
  expect_output(print(crft.obj.vector, pos = 10),
                regexp = "Rank 10 of 935 rates:")
  expect_error(print(crft.obj.multrates, pos = 3),
               regexp = NA)
  expect_output(print(crft.obj.multrates, pos = 3),
                regexp = "Rank 3 of 3 rates:")
})

test_that("calc_rate.ft print() stops with invalid 'pos' input.", {

  expect_error(print(crft.obj.value, pos = 2),
               regexp = "print.calc_rate.ft: Invalid 'pos' rank: only 1 rates found.")
  expect_error(print(crft.obj.df, pos = 1000),
               regexp = "print.calc_rate.ft: Invalid 'pos' rank: only 935 rates found.")
  expect_error(print(crft.obj.df, pos = 10:20),
               regexp = "print.calc_rate.ft: 'pos' must be a single value. To examine multiple results use summary")

})

test_that("calc_rate.ft objects work with summary()", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only
  expect_error(summary(crft.obj.value),
               regexp = NA)
  expect_output(summary(crft.obj.value),
                regexp = "out.o2")
  expect_error(summary(crft.obj.vector),
               regexp = NA)
  expect_output(summary(crft.obj.vector),
                regexp = "out.o2")
  expect_error(summary(crft.obj.df),
               regexp = NA)
  expect_output(summary(crft.obj.df),
                regexp = "out.o2")

  expect_error(summary(crft.obj.1rate),
               regexp = NA)
  expect_output(summary(crft.obj.1rate),
                regexp = "   rank")
  expect_error(summary(crft.obj.multrates),
               regexp = NA)
  expect_output(summary(crft.obj.multrates),
                regexp = "   rank")
  expect_error(summary(crft.obj.multrates.many),
               regexp = NA)
  expect_output(summary(crft.obj.multrates.many),
                regexp = "   rank")
  expect_error(summary(crft.obj.1rate.delta.only),
               regexp = NA)
  expect_output(summary(crft.obj.1rate.delta.only),
                regexp = "   rank")
  expect_error(summary(crft.obj.multrate.delta.only),
               regexp = NA)
  expect_output(summary(crft.obj.multrate.delta.only),
                regexp = "   rank")
})

test_that("calc_rate.ft objects work with summary() and 'pos' input", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only
  expect_error(summary(crft.obj.multrates.many, pos = c(60,70,80,90)),
               regexp = NA)
  expect_error(summary(crft.obj.multrates.many, pos = 60:90),
               regexp = NA)
  expect_error(summary(crft.obj.vector, pos = 6:9),
               regexp = NA)
})

test_that("calc_rate.ft summary() stops with invalid 'pos' input", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only
  expect_error(summary(crft.obj.vector, pos = 1000),
               regexp = "summary.calc_rate.ft: Invalid 'pos' rank: only 935 rates found.")
})

test_that("calc_rate.ft objects work with summary() and 'export' input", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only
  expect_error(summary(crft.obj.vector, export = TRUE),
               regexp = NA)
  expect_equal(nrow(summary(crft.obj.vector, export = TRUE)),
               nrow(crft.obj.vector$summary))


})

test_that("calc_rate.ft objects work with mean()", {

  ## objects
  # crft.obj.value
  # crft.obj.vector
  # crft.obj.df
  # crft.obj.1rate
  # crft.obj.multrates
  # crft.obj.1rate.delta.only
  # crft.obj.multrate.delta.only
  expect_error(mean(crft.obj.value),
               regexp = NA)
  expect_output(mean(crft.obj.value),
                regexp = "Mean of 1 output rates:")
  expect_equal(mean(crft.obj.value, export = TRUE),
               mean(crft.obj.value$rate))
  expect_message(mean(crft.obj.value),
               regexp = "Only 1 rate found. Returning mean rate anyway")

  expect_error(mean(crft.obj.vector),
               regexp = NA)
  expect_output(mean(crft.obj.vector),
                regexp = "Mean of 935 output rates:")
  expect_equal(mean(crft.obj.vector, export = TRUE),
               mean(crft.obj.vector$rate))

  expect_error(mean(crft.obj.df),
               regexp = NA)
  expect_output(mean(crft.obj.df),
                regexp = "Mean of 935 output rates:")
  expect_equal(mean(crft.obj.df, export = TRUE),
               mean(crft.obj.df$rate))

  expect_error(mean(crft.obj.1rate),
               regexp = NA)
  expect_output(mean(crft.obj.1rate),
                regexp = "Mean of 1 output rates:")
  expect_equal(mean(crft.obj.1rate, export = TRUE),
               mean(crft.obj.1rate$rate))

  expect_error(mean(crft.obj.multrates),
               regexp = NA)
  expect_output(mean(crft.obj.multrates),
                regexp = "Mean of 3 output rates:")
  expect_equal(mean(crft.obj.multrates, export = TRUE),
               mean(crft.obj.multrates$rate))

  expect_error(mean(crft.obj.1rate.delta.only),
               regexp = NA)
  expect_output(mean(crft.obj.1rate.delta.only),
                regexp = "Mean of 1 output rates:")
  expect_equal(mean(crft.obj.1rate.delta.only, export = TRUE),
               mean(crft.obj.1rate.delta.only$rate))

  expect_error(mean(crft.obj.multrate.delta.only),
               regexp = NA)
  expect_output(mean(crft.obj.multrate.delta.only),
                regexp = "Mean of 3 output rates:")
  expect_equal(mean(crft.obj.multrate.delta.only, export = TRUE),
               mean(crft.obj.multrate.delta.only$rate))
})


test_that("calc_rate.ft - plot defaults are correctly restored", {

  # reset plotting first
  dev.off()
  # save par before
  parb4 <- par(no.readonly = TRUE)
  # now use a fn with plot
  calc_rate.ft(inspect.ft(flowthrough.rd, 1, delta.o2 = 4), flowrate = 1.5)
  # save after
  paraft <- par(no.readonly = TRUE)
  # mai is something changed from the default,
  # so if par settings not restored properly this should fail
  expect_identical(parb4$mai,
                   paraft$mai)

})
}) ## turns console summarying back on
