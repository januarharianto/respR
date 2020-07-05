## test_file("tests/testthat/test-convert_DO.R")

test_that("convert_DO output conversions, using %Air, have expected results", {
  expect_equal(round(convert_DO(10, "%Air", "mg/l", S = 35, t = 25, P = 1.013253)$output, 3), 0.675)
  expect_equal(round(convert_DO(10, "%Air", "ug/l", S = 35, t = 25, P = 1.013253)$output, 3), 675.11)
  expect_equal(round(convert_DO(10, "%Air", "mmol/l", S = 35, t = 25, P = 1.013253)$output, 3), 0.021)
  expect_equal(round(convert_DO(10, "%Air", "umol/l", S = 35, t = 25, P = 1.013253)$output, 3), 21.098)
  expect_equal(round(convert_DO(10, "%Air", "ml/l", S = 35, t = 25, P = 1.013253)$output, 3), 0.516)
  expect_equal(round(convert_DO(10, "%Air", "mg/kg", S = 35, t = 25, P = 1.013253)$output, 3), 0.66)
  expect_equal(round(convert_DO(10, "%Air", "ug/kg", S = 35, t = 25, P = 1.013253)$output, 3), 659.79)
  expect_equal(round(convert_DO(10, "%Air", "mmol/kg", S = 35, t = 25, P = 1.013253)$output, 3), 0.021)
  expect_equal(round(convert_DO(10, "%Air", "umol/kg", S = 35, t = 25, P = 1.013253)$output, 3), 20.619)
  expect_equal(round(convert_DO(10, "%Air", "ml/kg", S = 35, t = 25, P = 1.013253)$output, 3), 0.504)
  expect_equal(round(convert_DO(10, "%Air", "%Air", S = 35, t = 25, P = 1.013253)$output, 3), 10)
  expect_equal(round(convert_DO(10, "%Air", "hPa", S = 35, t = 25, P = 1.013253)$output, 3), 20.854)
  expect_equal(round(convert_DO(10, "%Air", "kPa", S = 35, t = 25, P = 1.013253)$output, 3), 2.085)
  expect_equal(round(convert_DO(10, "%Air", "mmHg", S = 35, t = 25, P = 1.013253)$output, 3), 15.642)
  expect_equal(round(convert_DO(10, "%Air", "inHg", S = 35, t = 25, P = 1.013253)$output, 3), 0.616)
  expect_equal(round(convert_DO(10, "%Air", "Torr", S = 35, t = 25, P = 1.013253)$output, 3), 15.642)
})
## check that `from` arguments do not produce error

## regexp: regular expression to test against. If omitted, just asserts that
## code produces some output, messsage, warning or error. Alternatively, you can
## specify NA to indicate that there should be no output, messages, warnings or
## errors.
test_that("convert_DO converts different units without error", {
  expect_error(convert_DO(10, "mg/l", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "ug/l", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "mmol/l", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "umol/l", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "ml/l", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "mg/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "ug/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "mmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "umol/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "ml/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "%Air", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "Torr", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "hPa", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "kPa", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "mmHg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
  expect_error(convert_DO(10, "inHg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
})

test_that("convert_DO produces the correct numeric output", {
  expect_is(convert_DO(10, "mg/l", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "ug/l", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "mmol/l", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "umol/l", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "ml/l", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "mg/kg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "ug/kg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "mmol/kg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "umol/kg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "ml/kg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "%Air", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "Torr", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "hPa", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "kPa", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "mmHg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
  expect_is(convert_DO(10, "inHg", "mg/l", S = 35, t = 25, P = 1.013253)$output, "numeric")
})

test_that("convert_DO conversion works with changing salinity value", {
  expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 35, t = 25, P = 1.013253)$output, 3), 0.506)
  expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 25, t = 25, P = 1.013253)$output, 3), 0.536)
  expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 15, t = 25, P = 1.013253)$output, 3), 0.567)
  expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 5, t = 25, P = 1.013253)$output, 3), 0.601)
  expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 0, t = 25, P = 1.013253)$output, 3), 0.618)
})

test_that("convert_DO conversion works with changing pressure value", {
  expect_equal(round(convert_DO(7.5, "%Air", "mg/l", P = 0.337, S = 35, t = 25)$output, 3), 0.168)
})

test_that("convert_DO conversion works with changing temperature", {
  expect_equal(round(convert_DO(100, "%Air", "mg/l", t = 25, P = 1.013253, S = 35)$output, 3), 6.751)
  expect_equal(round(convert_DO(100, "%Air", "mg/l", t = 20, S = 35)$output, 3), 7.377)
})

test_that("convert_DO verify_units internal functions works", {
  expect_is(verify_units("mg/l", "o2"), "character")
  expect_is(verify_units("ml", "vol"), "character")
  expect_is(verify_units("mg", "mass"), "character")
  expect_is(verify_units("mg", "o1"), "character")
})

test_that("convert_DO S3 generics work", {
  expect_output(print(convert_DO(10, "inHg", "mg/l", S = 35, t = 25, P = 1.013253)))
})

test_that("convert_DO stops if % operator (old one) is used", {
  expect_error(convert_DO(10, "%", "mg/l", S = 35, t = 25, P = 1.013253),
               regexp = "verify_units: unit \"%\" has been deprecated. Please use \"%Air\" or \"%O2\" instead. See unit_args().")
})

## checks against respirometry::conv_o2 results

test_that("convert_DO: %Air and %O2 return same results as respirometry::conv_o2", {

  ## variables
  PercAir_in <- c(seq(100,50,-10))
  PercO2_in <- c(seq(20,10,-2))
  t_in <- seq(0,20,5)
  S_in <- c(0,10,20,30)
  P_in <- c(0.5, 1.013253, 1.5)

  ## all combinations
  grid <- expand.grid(PercO2_in = PercO2_in,
                      t_in = t_in,
                      S_in = S_in,
                      P_in = P_in)
  grid[[5]] <- seq(1:nrow(grid))

  # %O2
  # respR results
  res_respR <- apply(grid, 1, function(x) {
      convert_DO(x = x[1], from = "%O2", to = "mg/L", t = x[2], S = x[3], P = x[4])$output
    })

  # respirometry results
  res_respirometry <- apply(grid, 1, function(x) {
      respirometry::conv_o2( o2 = x[1], from = "percent_o2", to = "mg_per_l",
                             temp = x[2], sal = x[3], atm_pres = x[4]*1000) ## nb diff pressure units
    })
  # check results same
  expect_true(all.equal(res_respR, res_respirometry))

  # %Air
  ## all combinations
  grid <- expand.grid(PercAir_in = PercAir_in,
                      t_in = t_in,
                      S_in = S_in,
                      P_in = P_in)
  grid[[5]] <- seq(1:nrow(grid))
  # respR results
  res_respR <- apply(grid, 1, function(x) {
      convert_DO(x = x[1], from = "%Air", to = "mg/L", t = x[2], S = x[3], P = x[4])$output
    })

  # respirometry results
  res_respirometry <- apply(grid, 1, function(x) {
      respirometry::conv_o2( o2 = x[1], from = "percent_a.s.", to = "mg_per_l",
                             temp = x[2], sal = x[3], atm_pres = x[4]*1000) ## nb diff pressure units
    })
  # check results same
  expect_true(all.equal(res_respR, res_respirometry))

  })




