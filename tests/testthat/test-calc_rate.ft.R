# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-calc_rate.ft.R")
# covr::file_coverage("R/calc_rate.ft.R", "tests/testthat/test-calc_rate.ft.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)

capture.output({  ## stops printing console outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
# Create testing objects --------------------------------------------------
#
{
df <-  data.frame(time = flowthrough.rd[[2]], delta = flowthrough.rd[[3]])

## inspect.ft objects
insp.ft.obj.outO2.1col.inO2.1col<- suppressWarnings(suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1,
                                                               out.oxy = 2, in.oxy = 6, plot = F)))
insp.ft.obj.outO2.1col.inO2.value <- suppressWarnings(suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2,
                                                                 in.oxy.value = 9.05, delta.oxy = NULL, plot = F)))
insp.ft.obj.delta.1col <- suppressWarnings(suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = NULL,
                                                      in.oxy = NULL, delta.oxy = 10, plot = F)))

insp.ft.obj.outO2.multcols.inO2.multcols <-
  suppressWarnings(suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4, in.oxy = 6:8, plot = F)))

insp.ft.obj.outO2.multcols.inO2.value <-
  suppressWarnings(suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4, in.oxy.value = 9.05, plot = F)))

insp.ft.obj.delta.multcols <-
  suppressWarnings(suppressMessages(inspect.ft(flowthrough_mult.rd, time = 1, delta.oxy = 10:12, plot = F)))

## calc_rate.ft objects for testing plotting, printing etc.
crft.obj.value <- calc_rate.ft(-0.8,
                               flowrate = 2, plot = F)
crft.obj.vector <- calc_rate.ft(flowthrough.rd$oxy.delta,
                               flowrate = 2, plot = F)
crft.obj.df <- calc_rate.ft(data.frame(time = flowthrough.rd$oxy.out,
                                       delta = flowthrough.rd$oxy.in),
                               flowrate = 2, plot = F)

crft.obj.1rate <- calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                               from = 5, to = 900, by = "row",
                               flowrate = 2, plot = F)
crft.obj.multrates <- calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                                   from = c(5, 1005, 2005), to = c(900, 1900, 2900),by = "row",
                                   flowrate = 2, plot = F)
crft.obj.multrates.many <- calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                                   from = c(100:201), to = c(1000:1101),by = "row",
                                   flowrate = 2, plot = F)
crft.obj.1rate.delta.only <- calc_rate.ft(insp.ft.obj.delta.1col,
                                          from = 5, to = 900,by = "row",
                                          flowrate = 2, plot = F)
crft.obj.multrate.delta.only <- calc_rate.ft(insp.ft.obj.delta.1col,
                                             from = c(5, 1005, 2005), to = c(900, 1900, 2900),by = "row",
                                             flowrate = 2, plot = F)
}

# 'flowrate' input --------------------------------------------------------

test_that("calc_rate.ft - stops if 'flowrate' input malformed", {
  # non-numeric
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                            flowrate = "string", plot = F),
               regexp = "calc_rate.ft: 'flowrate' input is not numeric.")
  # null
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                            flowrate = NULL, plot = F),
               regexp = "calc_rate.ft: 'flowrate' input is required.")
  # too long
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                            flowrate = c(1,2), plot = F),
               regexp = "calc_rate.ft: 'flowrate' only 1 inputs allowed.")
})

test_that("calc_rate.ft - accepts numeric 'flowrate'", {
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1, plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.0111, plot = F),
               regexp = NA)
})



# 'x' input checks --------------------------------------------------------

# single numeric 'x' input ------------------------------------------------
test_that("calc_rate.ft - accepts single numeric 'x' input", {
  expect_error(calc_rate.ft(-0.8, flowrate = 1.5, plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(0.8, flowrate = 1.5, plot = F),
               regexp = NA)
  expect_message(calc_rate.ft(0.8, flowrate = 1.5, plot = F),
                 regexp = "calc_rate.ft: Calculating rate from delta oxygen.")
})

test_that("calc_rate.ft - expected output with single numeric 'x' input", {
  expect_equal(calc_rate.ft(-0.8, flowrate = 1.5, plot = F)$rate,
               -0.8 * 1.5)
  expect_equal(calc_rate.ft(0.8, flowrate = 1.5, plot = F)$rate,
               0.8 * 1.5)
})

test_that("calc_rate.ft - delta output is correct with single numeric 'x' input", {
  expect_equal(calc_rate.ft(-0.8, flowrate = 1.5, plot = F)$delta,
               -0.8)
  expect_equal(calc_rate.ft(0.8, flowrate = 1.5, plot = F)$delta,
               0.8)
})

test_that("calc_rate.ft - single numeric 'x' input, message that other inputs ignored.", {
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, to = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, by = "row",
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, width = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, width = 10, by = "row", from = 2,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
})

test_that("calc_rate.ft - single numeric 'x' input, message that 'plot = TRUE' input ignored and does not produce output.", {
  expect_message(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                              plot = TRUE),
                 regexp = "calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
  expect_output(calc_rate.ft(-0.8, flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})


# vector 'x' input --------------------------------------------------------
test_that("calc_rate.ft - accepts numeric vector 'x' input", {
  expect_error(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(c(90,100), flowrate = 1.5, plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(c(flowthrough.rd$oxy.delta), flowrate = 1.5,
                            plot = F),
               regexp = NA)
})

test_that("calc_rate.ft - expected output with dual numeric 'x' input", {
  expect_equal(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, plot = F)$rate,
               (c(7.5, 8.0)*1.5))
  expect_equal(calc_rate.ft(c(90, 100), flowrate = 1.5, plot = F)$rate,
               (c(90, 100)*1.5))
  expect_equal(calc_rate.ft(c(flowthrough.rd$oxy.delta), flowrate = 1.5,
                            plot = F)$rate,
               (flowthrough.rd$oxy.delta*1.5))
})

test_that("calc_rate.ft - dual numeric 'x' input, message that other inputs ignored.", {
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, to = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, by = "row",
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, width = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, width = 10, by = "row", from = 2,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
})

test_that("calc_rate.ft - dual numeric 'x' input, message that 'plot = TRUE' input ignored and does not produce output.", {
  expect_message(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                              plot = TRUE),
                 regexp = "calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
  expect_output(calc_rate.ft(c(7.5, 8.0), flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})



# data.frame 'x' input --------------------------------------------------------
test_that("calc_rate.ft - accepts data.frame 'x' input", {
  expect_error(calc_rate.ft(df, flowrate = 1.5, plot = F),
               regexp = NA)
})

test_that("calc_rate.ft - expected output with data.frame 'x' input", {
  expect_equal(calc_rate.ft(df, flowrate = 1.5, plot = F)$rate,
               ((flowthrough.rd[[2]]-flowthrough.rd[[3]])*1.5))
})

test_that("calc_rate.ft - data.frame 'x' input, message that other inputs ignored.", {
  expect_message(calc_rate.ft(df, flowrate = 1.5, from = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, to = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, by = "row",
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, width = 10,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
  expect_message(calc_rate.ft(df, flowrate = 1.5, width = 10, by = "row", from = 2,
                              plot = F),
                 regexp = "calc_rate.ft: Requires only 'x' and 'flowrate'. Additional inputs ignored.")
})

test_that("calc_rate.ft - data.frame 'x' input, message that 'plot = TRUE' input ignored and does not produce output.", {
  expect_message(calc_rate.ft(df, flowrate = 1.5, from = 10,
                              plot = TRUE),
                 regexp = "calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
  expect_output(calc_rate.ft(df, flowrate = 1.5, from = 10,
                             plot = TRUE),
                NA)
})


# disallowed inputs -------------------------------------------------------


# stop if inspect obj
test_that("calc_rate.ft - stops with `inspect` 'x' input", {
  expect_error(calc_rate.ft(
    suppressWarnings(suppressMessages(inspect(flowthrough.rd, plot = F))), flowrate = 1.5, plot = F),
    "calc_rate.ft: Function does not accept 'inspect' objects. Please process the data via 'inspect.ft' instead.")
})

test_that("calc_rate.ft - stops with any other disallowed 'x' input", {
  # df of 2 or more columns
  expect_error(calc_rate.ft(flowthrough.rd, flowrate = 1.5, plot = F),
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
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "string", plot = F),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = 123, plot = F),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "oxygen", plot = F),
               regexp = "calc_rate.ft: 'by' input not valid or not recognised.")
})

test_that("calc_rate.ft - accepts correct 'by' inputs", {
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "time", plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "Time", plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "row", plot = F),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 10,
                            by = "Row", plot = F),
               regexp = NA)
})

test_that("calc_rate.ft - applies default 'by = 'time' correctly", {
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0.05, to = 62.28,
                            by = NULL, plot = F)$rate,
               mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[3:3737]*1.5))
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                            by = NULL, plot = F)$inputs$by,
               "time")
})


# from/to input -----------------------------------------------------------

test_that("calc_rate.ft - stops if 'from' and 'to' are unequal lengths", {
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2:3, to = 10,
                            by = "time", plot = F),
               regexp = "calc_rate.ft: 'from' and 'to' have unequal lengths.")
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 1:9, to = 10:40,
                            by = "time", plot = F),
               regexp = "calc_rate.ft: 'from' and 'to' have unequal lengths.")
})

test_that("calc_rate.ft - stops if 'from' or 'to' values out of data ranges", {
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 1000, to = 10,
                            by = "time", plot = F),
               regexp = "calc_rate.ft: Some 'from' time values are higher than the values present in 'x'.")
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 0,
                            by = "time", plot = F),
               regexp = "calc_rate.ft: Some 'to' time values are lower than the values present in 'x'.")
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 4000, to = 5000,
                            by = "row", plot = F),
               regexp = "calc_rate.ft: Some 'from' row numbers are beyond the number of rows present in 'x'.")
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 200:210, to = 100:110,
                            by = "row", plot = F),
               regexp = "calc_rate.ft: Some 'from' values are greater than the paired values in 'to'.")
})

test_that("calc_rate.ft - if 'from' or 'to' values below or above data bounds, use lowest/highest vsalues instead", {
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0.01, to = 10,
                            by = "time", plot = F),
               regexp = "calc_rate.ft: Some 'from' time values are lower than the values present in 'x'. The lowest time value will be used instead.")
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0.01, to = 10,
                            by = "time", plot = F)$summary$time,
               0.02)

  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0, to = 10,
                            by = "row", plot = F),
               regexp = "calc_rate.ft: Some 'from' rows are lower than the values present in 'x'. The first row will be used instead.")
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0, to = 10,
                            by = "row", plot = F)$summary$row,
               1)

  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0.02, to = 70,
                            by = "time", plot = F),
               regexp = "calc_rate.ft: Some 'to' time values are higher than the values present in 'x'. The highest time value will be used instead.")
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 0.02, to = 70,
                            by = "time", plot = F)$summary$endtime,
               62.33)

  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 4000,
                            by = "row", plot = F),
               regexp = "calc_rate.ft: Some 'to' rows are higher than the values present in 'x'. The last row will be used instead.")
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, from = 2, to = 4000,
                            by = "row", plot = F)$summary$endrow,
               3740)

})

# width input -------------------------------------------------------------

test_that("calc_rate.ft - with 'width' input, correct message.", {
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                              width = 2,
                              by = "time",
                              plot = F),
                 regexp = "calc_rate.ft: 'width' can only be used with 'by = \"row\"'.")
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                              width = 2,
                              by = "row",
                              plot = F),
                 regexp = "calc_rate.ft: Rates determined using a rolling 'width' of 2 row values.")
})

test_that("calc_rate.ft - with 'width' input and from/to inputs, correct message that they are ignored.", {
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                              width = 2,
                              from = 2, to = 10,
                              by = "time",
                              plot = F),
                 regexp = "calc_rate.ft: 'width' can only be used with 'by = \"row\"'.")
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                              width = 2,
                              from = 2:5, to = 10:13,
                              by = "row",
                              plot = F),
                 regexp = "calc_rate.ft: A rolling 'width' has been specified, therefore 'from' and 'to' inputs will be ignored.")
})

test_that("calc_rate.ft - 'width' input correctly verified.", {
  # numeric
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                            width = "string",
                            by = "row",
                            plot = F),
               regexp = "calc_rate.ft: 'width' - input is not numeric.")
  # single value
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                            width = 1:2,
                            by = "row",
                            plot = F),
               regexp = "calc_rate.ft: 'width' - only 1 inputs allowed.")
})


test_that("calc_rate.ft - 'width' outputs correct results.", {
  ## correct number of results in summary
  wd <- 100
  expect_equal(nrow(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                                 width = wd,
                                 by = "row",
                                 plot = F)$summary),
               nrow(insp.ft.obj.outO2.1col.inO2.1col$dataframe)-wd+1)
  ## correct number of results in rate
  expect_equal(length(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                                   width = wd,
                                   by = "row",
                                   plot = F)$rate),
               nrow(insp.ft.obj.outO2.1col.inO2.1col$dataframe)-wd+1)
  ## first result should equal this
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5,
                            width = wd,
                            by = "row",
                            plot = F)$rate[1],
               mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[1:(wd)])*1.5)
})



# inspect.ft 'x' input --------------------------------------------------------

test_that("calc_rate.ft - accepts 'inspect.ft' input, gives correct messages and warnings.", {
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F),
               NA)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F),
                 "calc_rate.ft: Calculating rate from 'inspect.ft' object.")
  # multiple columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F)),
               NA)
  expect_message(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F)),
                 "calc_rate.ft: Calculating rate from 'inspect.ft' object.")
  expect_warning(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F),
                 "calc_rate.ft: Multiple columns of delta oxygen data found in input.")
})


test_that("calc_rate.ft - if 'from' and 'to' are NULL, defaults to using all data - by = 'time'.", {
  # from
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = NULL, to = 900, by = "time")$inputs$from,
               0.02)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                              from = NULL, to = 900, by = "time")$summary$from,
                 "calc_rate.ft: 'from' input NULL. Applying default 'from' of first time value.")
  # to
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = 1, to = NULL, by = "time")$inputs$to,
               62.33)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                              from = 1, to = NULL, by = "time")$summary$to,
                 "calc_rate.ft: 'to' input NULL. Applying default 'to' of last time value.")
  # both
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = NULL, to = NULL, by = "time")$inputs$from,
               0.02)
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = NULL, to = NULL, by = "time")$inputs$to,
               62.33)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                              from = NULL, to = NULL, by = "time")$summary$to,
                 "calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
})

test_that("calc_rate.ft - if 'from' and 'to' are NULL, defaults to using all data - by = 'row'.", {
  # from
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = NULL, to = 900, by = "row")$inputs$from,
               1)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                              from = NULL, to = 900, by = "row")$inputs$from,
                 "calc_rate.ft: 'from' input NULL. Applying default 'from' of first row.")
  # to
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = 1, to = NULL, by = "row")$inputs$to,
               3740)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                              from = 1, to = NULL, by = "row")$inputs$to,
                 "calc_rate.ft: 'to' input NULL. Applying default 'to' of last row.")
  # both
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = NULL, to = NULL, by = "row")$inputs$from,
               1)
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = NULL, to = NULL, by = "row")$inputs$to,
               3740)
  expect_message(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                              from = NULL, to = NULL, by = "row")$inputs$to,
                 "calc_rate.ft: 'from' and 'to' inputs NULL. Applying default of calculating rate from entire dataset.")
})

# insp.ft.obj.outO2.1col.inO2.1col
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with single columns of in.oxy and out.oxy.", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)

  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)
  # mean rate
  ## by time (+1 to from and to for time offset from row)

  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time (+1 to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               c(
                 mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5)
  )
  ## mean of all
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               (c(
                 mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.1col$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5)
               ))
})

# insp.ft.obj.outO2.1col.inO2.value
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with single column of out.oxy and in.oxy.value.", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row"),
               NA)
  ## check result should equal this
  ## all rates

  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)
  # mean rate

  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               c(
                 mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5)
  )
  ## mean of all
  expect_equal(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               (c(
                 mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.1col.inO2.value$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5)
               ))
})


# insp.ft.obj.delta.1col

test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with single column of delta oxygen data", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row"),
               NA)
  ## check result should equal this
  ## all rates

  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from):(to)])*1.5)
  # mean rate

  ## by row
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row"),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate[1],
               mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               c(
                 mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[3]):(to[3])])*1.5)
  )
  ## mean of all
  expect_equal(calc_rate.ft(insp.ft.obj.delta.1col, flowrate = 1.5, plot = F,
                            from = from, to = to, by = "row")$rate,
               (c(
                 mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.1col$data$delta.oxy[[1]][(from[3]):(to[3])])*1.5)
               ))
})


# insp.ft.obj.outO2.multcols.inO2.multcols
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with multiple columns of both in.oxy and out.oxy data", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row")),
               NA)
  ## check result should equal this
  ## all rates

  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)
  # mean rate

  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               c(
                 mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5))

  ## mean of all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               (c(
                 mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.multcols$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5)))
})

# insp.ft.obj.outO2.multcols.inO2.value
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with multiple columns of out.oxy data and in.oxy.value", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row")),
               NA)
  ## check result should equal this
  ## all rates

  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)
  # mean rate

  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               c(
                 mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5))

  ## mean of all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.outO2.multcols.inO2.value, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               (c(
                 mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.outO2.multcols.inO2.value$dataframe$delta.oxy.calc.1[(from[3]):(to[3])])*1.5)))
})

# insp.ft.obj.delta.multcols
test_that("calc_rate.ft - accepts and gives correct results for inspect.ft obj with multiple columns of delta.oxy data", {
  from = 100
  to = 400
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row")),
               NA)
  ## check result should equal this
  ## all rates

  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from):(to)])*1.5)
  # mean rate

  ## by row
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from):(to)])*1.5)

  ## multiple from and to
  from = c(100,200,300)
  to = c(400,500,600)
  # single columns in inspect.ft object
  expect_error(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row")),
               NA)
  ## check result should equal this
  ## all rates
  ## by time ( to from and to for time offset from row)
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate[1],
               mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[1]):(to[1])])*1.5)
  ## all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               c(
                 mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[3]):(to[3])])*1.5))

  ## mean of all
  expect_equal(suppressWarnings(calc_rate.ft(insp.ft.obj.delta.multcols, flowrate = 1.5, plot = F,
                                             from = from, to = to, by = "row"))$rate,
               (c(
                 mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[1]):(to[1])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[2]):(to[2])])*1.5,
                 mean(insp.ft.obj.delta.multcols$data$delta.oxy[[1]][(from[3]):(to[3])])*1.5)))
})



# S3 checks -----------------------------------------------------------

test_that("calc_rate.ft outputs plot", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                            flowrate = 2, plot = TRUE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.delta.1col,
                            flowrate = 2, plot = TRUE),
               regexp = NA)
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.value,
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
               regexp = "calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
  expect_error(plot(crft.obj.vector),
               regexp = "calc_rate.ft: Plot only available for 'inspect.ft' inputs.")
  expect_error(plot(crft.obj.df),
               regexp = "calc_rate.ft: Plot only available for 'inspect.ft' inputs.")

})

test_that("calc_rate.ft output plots when 'pos' is used", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
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
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
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

test_that("calc_rate.ft output plot stops when 'pos' more than 1 value", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
                            from = 2:5, to = 1002:1005,
                            flowrate = 2, plot = TRUE, pos = 9:10),
               regexp = "calc_rate: 'pos' should be a single value.")
})

test_that("calc_rate.ft output plots when 'rate.rev' is used", {
  ## plots when functions runs
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
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
  expect_error(calc_rate.ft(insp.ft.obj.outO2.1col.inO2.1col,
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
  expect_error(summary(crft.obj.vector),
               regexp = NA)
  expect_error(summary(crft.obj.df),
               regexp = NA)

  expect_error(summary(crft.obj.1rate),
               regexp = NA)
  expect_output(summary(crft.obj.1rate),
                regexp = "   rep")
  expect_error(summary(crft.obj.multrates),
               regexp = NA)
  expect_output(summary(crft.obj.multrates),
                regexp = "   rep")
  expect_error(summary(crft.obj.multrates.many),
               regexp = NA)
  expect_output(summary(crft.obj.multrates.many),
                regexp = "   rep")
  expect_error(summary(crft.obj.1rate.delta.only),
               regexp = NA)
  expect_output(summary(crft.obj.1rate.delta.only),
                regexp = "   rep")
  expect_error(summary(crft.obj.multrate.delta.only),
               regexp = NA)
  expect_output(summary(crft.obj.multrate.delta.only),
                regexp = "   rep")
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

test_that("calc_rate.ft objects work with mean() and 'pos' input", {

  expect_error(mean(crft.obj.vector, pos = 1:10),
               regexp = NA)
  expect_output(mean(crft.obj.vector, pos = 1:10),
                regexp = "Mean of rate results from entered 'pos' ranks:")
  expect_equal(mean(crft.obj.vector, pos = 1:10, export = TRUE),
               mean(crft.obj.vector$rate[1:10]))
})

test_that("calc_rate.ft - stops with mean() and invalid 'pos' input", {
  expect_error(mean(crft.obj.vector, pos = 1000),
               regexp = "mean.calc_rate.ft: Invalid 'pos' rank: only 935 rates found.")
})


test_that("calc_rate.ft - plot defaults are correctly restored", {

  # reset plotting first
  dev.off()
  # save par before
  parb4 <- par(no.readonly = TRUE)
  # now use a fn with plot
  calc_rate.ft(inspect.ft(flowthrough.rd, 1, delta.oxy = 4), flowrate = 1.5)
  # save after
  paraft <- par(no.readonly = TRUE)
  # mai is something changed from the default,
  # so if par settings not restored properly this should fail
  expect_identical(parb4$mai,
                   paraft$mai)

})
}) ## turns console summarying back on
