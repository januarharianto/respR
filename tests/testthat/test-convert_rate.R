## testthat::test_file("tests/testthat/test-convert_rate.R")

test_that("convert_rate works with default values",
          expect_equal(suppressWarnings(convert_rate(10, volume = 1)$output.rate), 36000))

# use known warnings to do these checks
# maybe make this more specific later
test_that("convert_rate falls back to default arguments properly", {
  # specific warnings
  expect_warning(convert_rate(10, volume = 1, S = 35, t = 25),
                 "convert_rate: the 'o2.unit' is not provided, using 'mg/L'.")
  expect_warning(convert_rate(10, volume = 1, S = 35, t = 25),
                 "convert_rate: the 'time.unit' is not provided, using 's'.")
  expect_warning(convert_rate(10, volume = 1, S = 35, t = 25),
                 "convert_rate: the 'output.unit' is not provided, using 'mg/h'.")
})

#
test_that("convert_rate (adjust_scale) produces valid output", {
  expect_is(adjust_scale(10, "mg", "kg"), "numeric")
  expect_is(adjust_scale(10, "ml", "l"), "numeric")
})


test_that("convert_rate outputs correct conversion values", {

  convert_rate(10, volume = 1, S = 35, t = 25)

  expect_equal(round(
    convert_rate(10, o2.unit = "%", time.unit = "s", output.unit = "mg/h", volume = 1, S = 35, t = 25, P = 1.013253)$output, 3), 0.675)
  expect_equal(round(convert_rate(10, "%", "ug/l", S = 35, t = 25, P = 1.013253)$output, 3), 675.11)
  expect_equal(round(convert_rate(10, "%", "mmol/l", S = 35, t = 25, P = 1.013253)$output, 3), 0.021)
  expect_equal(round(convert_rate(10, "%", "umol/l", S = 35, t = 25, P = 1.013253)$output, 3), 21.098)
  expect_equal(round(convert_rate(10, "%", "ml/l", S = 35, t = 25, P = 1.013253)$output, 3), 0.516)
  expect_equal(round(convert_rate(10, "%", "mg/kg", S = 35, t = 25, P = 1.013253)$output, 3), 0.66)
  expect_equal(round(convert_rate(10, "%", "ug/kg", S = 35, t = 25, P = 1.013253)$output, 3), 659.79)
  expect_equal(round(convert_rate(10, "%", "mmol/kg", S = 35, t = 25, P = 1.013253)$output, 3), 0.021)
  expect_equal(round(convert_rate(10, "%", "umol/kg", S = 35, t = 25, P = 1.013253)$output, 3), 20.619)
  expect_equal(round(convert_rate(10, "%", "ml/kg", S = 35, t = 25, P = 1.013253)$output, 3), 0.504)
  expect_equal(round(convert_rate(10, "%", "%", S = 35, t = 25, P = 1.013253)$output, 3), 10)
  expect_equal(round(convert_rate(10, "%", "hPa", S = 35, t = 25, P = 1.013253)$output, 3), 20.854)
  expect_equal(round(convert_rate(10, "%", "kPa", S = 35, t = 25, P = 1.013253)$output, 3), 2.085)
  expect_equal(round(convert_rate(10, "%", "mmHg", S = 35, t = 25, P = 1.013253)$output, 3), 15.642)
  expect_equal(round(convert_rate(10, "%", "inHg", S = 35, t = 25, P = 1.013253)$output, 3), 0.616)
  expect_equal(round(convert_rate(10, "%", "Torr", S = 35, t = 25, P = 1.013253)$output, 3), 15.642)

  })
