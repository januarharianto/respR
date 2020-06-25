## test_file("tests/testthat/test-calc_rate.R")

test_that("calc_rate produces object of class calc_rate", {
  cr <- calc_rate(sardine.rd, plot = F)
  expect_is(cr,
            "calc_rate")
})

test_that("calc_rate default subsetting methods work", {
  cr <- calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)
  expect_is(cr,
            "calc_rate")
  cr <- calc_rate(sardine.rd, 2000, 4000, by = "row", plot = F)
  expect_is(cr,
            "calc_rate")
  cr <- calc_rate(sardine.rd, 94, 93, by = "o2", plot = F)
  expect_is(cr,
            "calc_rate")
  cr <- calc_rate(sardine.rd, 0.5, 0, by = "proportion", plot = F)
  expect_is(cr,
            "calc_rate")
})

test_that("calc_rate S3 generics work", {
  cr <- calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)

  expect_output(print(cr))
  expect_output(summary(cr))
  expect_output(plot(cr))
  expect_output(suppressWarnings(mean(cr)))
})

test_that("calc_rate calling linear_fit (calc_rate) produces coefficients", {

  expect_equal(c("intercept_b0", "rate_b1", "rsq"),
               names(linear_fit(sardine.rd)))
})

test_that("calc_rate works with variations of `by` input", {
  expect_error(calc_rate(sardine.rd, plot = F, by = "Time"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "T"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "Oxygen"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "O2"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "Row"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "r"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "Proportion"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "prop"), regexp = NA)
})

test_that("calc_rate errors with wrong by", {
  expect_error(calc_rate(sardine.rd, plot = F, by = "tttimmmeee"),
               "`by` input not recognised")
})

