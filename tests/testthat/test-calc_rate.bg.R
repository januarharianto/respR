## library(testthat)
## test_file("tests/testthat/test-inspect.R")

sink("/dev/null") ## stops printing outputs on assigning in log

## Accepts data.frame
test_that("calc_rate.bg works using default arguments", {
  ## Analyses all columns by default
  urbg <- calc_rate.bg(urchins.rd,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
  expect_equal(ncol(urbg$dataframe), 19)
  expect_equal(length(urbg$bgrate), 18)
})

test_that("calc_rate.bg accepts 2 columns", {
  urbg <- calc_rate.bg(urchins.rd,  time = 1, oxygen = 18, plot = F)
  expect_equal(ncol(urbg$dataframe), 2)
})

test_that("calc_rate.bg accepts multiple columns", {
  urbg <- calc_rate.bg(urchins.rd,  time = 1, oxygen = c(18,19), plot = F)
  expect_equal(ncol(urbg$dataframe), 3)
})

test_that("calc_rate.bg accepts `inspect_data` objects", {
  ur <- suppressWarnings(inspect_data(urchins.rd, plot = F))
  urbg <- calc_rate.bg(ur,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
})

test_that("calc_rate.bg accepts `inspect` objects", {
  ur <- suppressWarnings(inspect(urchins.rd, plot = F))
  urbg <- calc_rate.bg(ur,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
})

test_that("calc_rate.bg correctly uses specified columns in `inspect` objects", {
  ur <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = NULL, plot = F))
  urbg <- calc_rate.bg(ur,  time = 1, oxygen = c(18,19), plot = F)
  expect_equal(ncol(urbg$data), 3)
  expect_equal(ur$dataframe$b1, urbg$data$b1)
  expect_equal(ur$dataframe$b2, urbg$data$b2)
})

test_that("calc_rate.bg S3 generics work", {
  ur <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = NULL, plot = F))
  urbg <- calc_rate.bg(ur,  time = 1, oxygen = c(18,19), plot = F)
  expect_output(print(urbg))
  expect_output(plot(urbg))
  expect_output(mean(urbg))
})

sink() ## turns printing back on
