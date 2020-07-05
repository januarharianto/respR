## test_file("tests/testthat/test-convert_rate.R")

test_that("convert_rate works with default values",
          expect_equal(suppressWarnings(convert_rate(10, volume = 1)$output.rate), 36000))

# use known warnings to do these checks
# maybe make this more specific later
test_that("convert_rate falls back to default arguments properly", {
  # specific warnings
  expect_warning(convert_rate(10, volume = 1, S = 35, t = 25),
                 "o2.unit` is not provided, using `mg/L`.")
  expect_warning(convert_rate(10, volume = 1, S = 35, t = 25),
                 "'time.unit' is not provided, using 's'.")
  expect_warning(convert_rate(10, volume = 1, S = 35, t = 25),
                 "'output.unit' is not provided, using 'mg/h`.")
})

#
test_that("convert_rate (adjust_scale) produces valid output", {
  expect_is(adjust_scale(10, "mg", "kg"), "numeric")
  expect_is(adjust_scale(10, "ml", "l"), "numeric")
})

