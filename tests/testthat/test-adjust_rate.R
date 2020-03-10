## test_file("tests/testthat/test-adjust_rate.R")

test_that("adjust_rate accepts calc_rate.bg inputs", {
  bg <- calc_rate.bg(urchins.rd, time = 1, oxygen = 18:19, plot = F)
  expect_error(adjust_rate(100, bg),
               regexp = NA)
})

test_that("adjust_rate accepts numeric inputs", {
  adj <- adjust_rate(100, 20)
  expect_is(adj$input,
            "numeric")
})

test_that("adjust_rate stops with non-numeric or non calc_rate.bg object", {
  not_bg <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 18:19, plot=FALSE))
  expect_error(adjust_rate(100, not_bg),
               regexp = "'by' must be numeric or object of class 'calc_rate.bg'.")
})

test_that("adjust_rate works with input of class `auto_rate` or `calc_rate`", {
  cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
  expect_is(cr,
            "adjust_rate")
  cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
  ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
  expect_is(ar,
            "adjust_rate")
})


test_that("adjust_rate can be printed - if adjustment done to calc_rate or auto_rate object", {
  cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
  ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
  expect_output(print(cr))
  expect_output(print(ar))
})

test_that("adjust_rate can be printed - if adjustment done to numeric value", {
  nr <- adjust_rate(0.1, 0.001)
  expect_output(print(nr))
})

test_that("adjust_rate S3 generics work", {
  cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
  ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
  nr <- adjust_rate(0.1, 0.001)
  expect_output(summary(cr))
  expect_output(summary(ar))
  expect_output(summary(nr))
  expect_output(print(cr))
  expect_output(print(ar))
  expect_output(print(nr))
  expect_output(suppressWarnings(mean(cr)))
  expect_output(suppressWarnings(mean(ar)))
  expect_output(suppressWarnings(mean(nr)))
})

