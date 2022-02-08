## library(testthat)
## testthat::test_file("tests/testthat/test-convert_rate.R")

capture.output({  ## stops printing outputs on assigning

test_that("convert_rate works with default values",
          expect_equal(suppressWarnings(convert_rate(10, volume = 1, time.unit = "s", oxy.unit = "mg/l")$rate.output),
                       36000))

# use known warnings to do these checks
# maybe make this more specific later
test_that("convert_rate falls back to default inputs properly", {
  expect_warning(convert_rate(10, volume = 1,  time.unit = "s", oxy.unit = "mg/l"),
                 "convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h'")
  expect_equal(suppressWarnings(convert_rate(10, volume = 1,  time.unit = "s", oxy.unit = "mg/l")$output.unit),
               "mgO2/hour")

  expect_warning(convert_rate(10, volume = 1,  time.unit = "s", oxy.unit = "mg/l", mass = 1),
                 "convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h/kg'")
  expect_equal(suppressWarnings(convert_rate(10, volume = 1,  time.unit = "s", oxy.unit = "mg/l", mass = 1)$output.unit),
               "mgO2/hour/kg")

  expect_warning(convert_rate(10, volume = 1,  time.unit = "s", oxy.unit = "mg/l", area = 1),
                 "convert_rate: the 'output.unit' is not provided. Applying default: 'mg/h/m2'")
  expect_equal(suppressWarnings(convert_rate(10, volume = 1,  time.unit = "s", oxy.unit = "mg/l", area = 1)$output.unit),
               "mgO2/hour/m2")
})

#
test_that("convert_rate (adjust_scale) produces valid output", {
  expect_is(adjust_scale(10, "mg", "kg"), "numeric")
  expect_is(adjust_scale(10, "ml", "l"), "numeric")
})

##
test_that("convert_rate stops if 'oxy.unit' or 'time.unit' are NULL or numeric", {
  expect_error(convert_rate(10, volume = 1, oxy.unit = "umol/l", time.unit = NULL),
               "convert_rate: the 'time.unit' of the original data is required.")
  expect_error(convert_rate(10, volume = 1, oxy.unit = "umol/l", time.unit = 1.1),
               "convert_rate: the 'time.unit' of the original data is required.")

  expect_error(convert_rate(10, volume = 1, oxy.unit = NULL, time.unit = "s"),
               "convert_rate: the 'oxy.unit' of the original data is required.")
  expect_error(convert_rate(10, volume = 1, oxy.unit = 1.1, time.unit = "s"),
               "convert_rate: the 'oxy.unit' of the original data is required.")
})

test_that("convert_rate stops if 'volume' is NULL or NOT numeric", {
  expect_error(convert_rate(10, volume = NULL, oxy.unit = "umol/l", time.unit = "s", output.unit = "mg/h/g"),
               "convert_rate: the 'volume' input is required.")
  expect_error(convert_rate(10, volume = "text", oxy.unit = "umol/l", time.unit = "s", output.unit = "mg/h/g"),
               "convert_rate: the 'volume' input is required.")
})

test_that("convert_rate stops if both 'mass' and 'area' inputs are entered", {
  expect_error(convert_rate(10, volume = 1, oxy.unit = "umol/l", time.unit = "s", output.unit = "mg/h/g",
                            mass = 1,
                            area = 1),
               "convert_rate: cannot have inputs for both 'mass' and 'area'.")
})

test_that("convert_rate stops if 'output.unit' requires a 'mass' or 'area' input and neither is provided", {
  expect_error(convert_rate(10, volume = 1, oxy.unit = "mg/l", time.unit = "s", output.unit = "mg/h/g",
                            mass = NULL, area = NULL),
               "convert_rate: 'output.unit' requires a value for 'mass' or 'area'")
  expect_error(convert_rate(10, volume = 1, oxy.unit = "mg/l", time.unit = "s", output.unit = "mg/h/cm2",
                            mass = NULL, area = NULL),
               "convert_rate: 'output.unit' requires a value for 'mass' or 'area'")
})

test_that("convert_rate stops if a 'mass' or 'area' input entered but output.unit is not mass- or area-specific", {
  expect_error(convert_rate(10, volume = 1, oxy.unit = "mg/l", time.unit = "s", output.unit = "mg/h",
                            mass = 1, area = NULL),
               "convert_rate: a 'mass' has been entered, but a mass-specific unit has not been specified in 'output.unit'.")
  expect_error(convert_rate(10, volume = 1, oxy.unit = "mg/l", time.unit = "s", output.unit = "mg/h",
                            mass = NULL, area = 1),
               "convert_rate: an 'area' has been entered, but an area-specific unit has not been specified in 'output.unit'.")
})

test_that("convert_rate S3 generics work as expected", {
  res_abs <- convert_rate(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h",
                          volume = 0.0032245)
  res_ms <- convert_rate(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/g",
                         volume = 0.0032245, mass = 0.00534)
  res_as <- convert_rate(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                         volume = 0.0032245, area = 0.000001429)

  ## summary
  expect_output(summary(res_abs))
  expect_output(summary(res_ms))
  expect_output(summary(res_as))

  ## print
  expect_output(print(res_abs))
  expect_output(print(res_ms))
  expect_output(print(res_as))

  ## mean
  ## absolute
  expect_output(suppressWarnings(mean(res_abs)))
  expect_message(mean(res_abs),
                 "Only 1 rate found. Returning mean rate anyway...")
  expect_equal(suppressWarnings(mean(res_abs, export = TRUE)),
               mean(res_abs$rate.output))
  ## mass-spec
  expect_output(suppressWarnings(mean(res_ms)))
  expect_message(mean(res_ms),
                 "Only 1 rate found. Returning mean rate anyway...")
  expect_equal(suppressWarnings(mean(res_ms, export = TRUE)),
               mean(res_ms$rate.output))
  ## area-spec
  expect_output(suppressWarnings(mean(res_as)))
  expect_message(mean(res_as),
                 "Only 1 rate found. Returning mean rate anyway...")
  expect_equal(suppressWarnings(mean(res_as, export = TRUE)),
               mean(res_as$rate.output))

  ## check multiple rates have correct mean
  rate <- suppressWarnings(inspect(urchins.rd[,1:2], plot = F)) %>%
    auto_rate(plot = F) %>%
    convert_rate(oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                 volume = 0.0032245, area = 0.000001429)
  expect_output(suppressWarnings(mean(rate)))
  expect_equal(mean(rate, export = TRUE),
               mean(rate$rate.output))

})

test_that("convert_rate stops if units require t, S and P", {

  expect_error(convert_rate(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                            volume = 0.0032245,
                            area = 0.000001429),
               "Input or output units require Salinity input")

  expect_error(convert_rate(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                            volume = 0.0032245,
                            S = 35,
                            area = 0.000001429),
               "Input or output units require Temperature input")

  expect_message(convert_rate(-0.0001534657, oxy.unit = "mL/L", time.unit = "s", output.unit = "mg/h/mm2",
                              volume = 0.0032245,
                              S = 35, t = 31, P = NULL,
                              area = 0.000001429),
                 "Input or output units require Atmospheric Pressure input")

})


## Possibly update these in future with exact values we know are correct.
## For now, these just flag up if code changes cause output changes
test_that("convert_rate outputs correct conversion values - absolute rates", {

  res <- convert_rate(93.1, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 1, S = 35, t = 25, P = 1.013253)
  expect_equal(res$rate.output, 22626.99, tolerance = 0.01) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 93.1)
  expect_equal(res$inputs$oxy.unit, "%Air")
  expect_equal(res$inputs$time.unit, "sec")
  expect_equal(res$output.unit, "mgO2/hour")
  expect_equal(res$summary$rate.input, 93.1)
  expect_equal(res$summary$rate.output, 22626.99, tolerance = 0.01)
  expect_equal(res$summary$rate.abs, 22626.99, tolerance = 0.01)

  res <- convert_rate(4303, oxy.unit = "ug/l", time.unit = "hr", output.unit = "ml/minute", volume = 1, S = 35, t = 25, P = 1.013253)
  expect_equal(res$rate.output, 0.05477929, tolerance = 0.00001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 4303)
  expect_equal(res$inputs$oxy.unit, "ug/L")
  expect_equal(res$inputs$time.unit, "hour")
  expect_equal(res$output.unit, "mlO2/min")
  expect_equal(res$summary$rate.input, 4303)
  expect_equal(res$summary$rate.output, 0.05477929, tolerance = 0.00001)
  expect_equal(res$summary$rate.abs, 0.05477929, tolerance = 0.00001)

  res <- convert_rate(234, oxy.unit = "hPa", time.unit = "min", output.unit = "umol/min", volume = 1, S = 35, t = 25, P = 1.013253)
  expect_equal(res$rate.output, 236.739, tolerance = 0.001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 234)
  expect_equal(res$inputs$oxy.unit, "hPa")
  expect_equal(res$inputs$time.unit, "min")
  expect_equal(res$output.unit, "umolO2/min")
  expect_equal(res$summary$rate.input, 234)
  expect_equal(res$summary$rate.output, 236.739, tolerance = 0.001)
  expect_equal(res$summary$rate.abs, 236.739, tolerance = 0.001)

  # changing magnitude of inputs and outputs changes rate by right amount
  rate <- suppressWarnings(auto_rate(sardine.rd, plot = F))

  # sign of the rate
  expect_equal(
    convert_rate(-1*rate$rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output * -1
  )
  # time metric in output
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/m", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output*60
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/s", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output*60*60
  )
  # time metric in input
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "m", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output*60
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "h", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output*60*60
  )
  # volume
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 10, S = 35, t = 14, P = 1.013253)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h", volume = 1, S = 35, t = 14, P = 1.013253)$rate.output*10
  )

})


test_that("convert_rate outputs correct conversion values - mass-specific rates", {

  res <- convert_rate(93.1, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 1, S = 35, t = 25, P = 1.013253,
                      mass = 0.012)
  expect_equal(res$rate.output, 1885.583, tolerance = 0.001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 93.1)
  expect_equal(res$inputs$oxy.unit, "%Air")
  expect_equal(res$inputs$time.unit, "sec")
  expect_equal(res$output.unit, "mgO2/hour/g")
  expect_equal(res$summary$rate.input, 93.1)
  expect_equal(res$summary$rate.output, 1885.583, tolerance = 0.01)
  expect_equal(res$summary$rate.abs, 22626.99, tolerance = 0.01)
  expect_equal(res$summary$rate.m.spec, 1885.583, tolerance = 0.001)

  res <- convert_rate(4303, oxy.unit = "ug/l", time.unit = "hr", output.unit = "ml/minute/kg", volume = 1, S = 35, t = 25, P = 1.013253,
                      mass = 0.012)
  expect_equal(res$rate.output, 4.564941, tolerance = 0.00001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 4303)
  expect_equal(res$inputs$oxy.unit, "ug/L")
  expect_equal(res$inputs$time.unit, "hour")
  expect_equal(res$output.unit, "mlO2/min/kg")
  expect_equal(res$summary$rate.input, 4303)
  expect_equal(res$summary$rate.output, 4.564941, tolerance = 0.00001)
  expect_equal(res$summary$rate.abs, 0.05477929, tolerance = 0.00001)
  expect_equal(res$summary$rate.m.spec, 4.564941, tolerance = 0.00001)

  res <- convert_rate(234, oxy.unit = "hPa", time.unit = "min", output.unit = "umol/min/ug", volume = 1, S = 35, t = 25, P = 1.013253,
                      mass = 0.012)
  expect_equal(res$rate.output, 0.00001972825, tolerance = 0.000001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 234)
  expect_equal(res$inputs$oxy.unit, "hPa")
  expect_equal(res$inputs$time.unit, "min")
  expect_equal(res$output.unit, "umolO2/min/ug")
  expect_equal(res$summary$rate.input, 234)
  expect_equal(res$summary$rate.output, 0.00001972825, tolerance = 0.000001)
  expect_equal(res$summary$rate.abs, 236.739, tolerance = 0.001)
  expect_equal(res$summary$rate.m.spec, 0.00001972825, tolerance = 0.000001)

  # changing magnitude of inputs and outputs changes rate by right amount
  rate <- suppressWarnings(auto_rate(sardine.rd, plot = F))

  # sign of the rate
  expect_equal(
    convert_rate(-1*rate$rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output * -1
  )
  # time metric in output
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/m/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*60
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/s/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*60*60
  )
  # time metric in input
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "m", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*60
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "h", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*60*60
  )
  # volume
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 1, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*10
  )
  # mass
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/kg", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output/1000
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/mg", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*1000
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/g", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/ug", volume = 10, S = 35, t = 14, P = 1.013253,
                 mass = 0.012)$rate.output*1000000
  )

})


## Kate Quigley data
## These values checked in Excel - first one is right

test_that("convert_rate outputs correct conversion values - area-specific rates", {

  res <- convert_rate(-0.0001534657, oxy.unit = "mg/L", time.unit = "s", output.unit = "mg/h/mm2",
                      volume = 0.0032245,
                      S = 35, t = 31, P = 1.013253,
                      area = 0.000001429)

  expect_equal(res$rate.output, -0.001246648) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, -0.0001534657)
  expect_equal(res$inputs$oxy.unit, "mg/L")
  expect_equal(res$inputs$time.unit, "sec")
  expect_equal(res$output.unit, "mgO2/hour/mm2")
  expect_equal(res$summary$rate.input, -0.0001534657)
  expect_equal(res$summary$rate.output, -0.001246648)
  expect_equal(res$summary$rate.abs, -0.001781461)
  expect_equal(res$summary$rate.a.spec, -0.001246648)

  ## these made up
  res <- convert_rate(4303, oxy.unit = "ug/l", time.unit = "hr", output.unit = "ml/minute/m2",
                      volume = 1, S = 35, t = 25, P = 1.013253,
                      area = 0.012)
  expect_equal(res$rate.output, 4.564941, tolerance = 0.00001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 4303)
  expect_equal(res$inputs$oxy.unit, "ug/L")
  expect_equal(res$inputs$time.unit, "hour")
  expect_equal(res$output.unit, "mlO2/min/m2")
  expect_equal(res$summary$rate.input, 4303)
  expect_equal(res$summary$rate.output, 4.564941, tolerance = 0.00001)
  expect_equal(res$summary$rate.abs, 0.05477929, tolerance = 0.00001)
  expect_equal(res$summary$rate.a.spec, 4.564941, tolerance = 0.00001)

  res <- convert_rate(234, oxy.unit = "hPa", time.unit = "min", output.unit = "umol/min/cm2",
                      volume = 1, S = 35, t = 25, P = 1.013253,
                      area = 0.012)
  expect_equal(res$rate.output, 1.972825, tolerance = 0.000001) # exact value
  ## inputs and outputs are saved in object correctly
  expect_equal(res$rate.input, 234)
  expect_equal(res$inputs$oxy.unit, "hPa")
  expect_equal(res$inputs$time.unit, "min")
  expect_equal(res$output.unit, "umolO2/min/cm2")
  expect_equal(res$summary$rate.input, 234)
  expect_equal(res$summary$rate.output, 1.972825, tolerance = 0.000001)
  expect_equal(res$summary$rate.abs, 236.739, tolerance = 0.001)
  expect_equal(res$summary$rate.a.spec, 1.972825, tolerance = 0.000001)

  # changing magnitude of inputs and outputs changes rate by right amount
  rate <- suppressWarnings(auto_rate(sardine.rd, plot = F))
  # time metric in output
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/m/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output*60
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/s/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output*60*60
  )
  # time metric in input
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "m", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output*60
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "h", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output*60*60
  )
  # volume
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 1, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output*10
  )
  # area
  # mm2 > cm2 > m2 > km2
  # 1 x 100 x 10000 x 1000000
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/mm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/cm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output/100
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/mm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/m2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output/100/10000
  )
  expect_equal(
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/mm2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output,
    convert_rate(rate, oxy.unit = "%Air", time.unit = "s", output.unit = "mg/h/km2", volume = 10, S = 35, t = 14, P = 1.013253,
                 area = 0.012)$rate.output/100/10000/1000000
  )

})

test_that("convert_rate: changing 'time' inputs changes rates by correct magnitude multiplier", {
  # Outputs
  # s to m
  expect_equal(
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mg/s/kg',
                 volume = 1.2, mass = 0.5)$rate.output * 60,
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mg/m/kg',
                 volume = 1.2, mass = 0.5)$rate.output)
  # s to h
  expect_equal(
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mg/s/kg',
                 volume = 1.2, mass = 0.5)$rate.output * 60 * 60,
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mg/h/kg',
                 volume = 1.2, mass = 0.5)$rate.output)
  # s to d
  expect_equal(
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mg/s/kg',
                 volume = 1.2, mass = 0.5)$rate.output * 60 * 60 * 24,
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mg/d/kg',
                 volume = 1.2, mass = 0.5)$rate.output)

  # If inut data was different time
  # d to s
  expect_equal(
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 'day', output.unit = 'mg/s/kg',
                 volume = 1.2, mass = 0.5)$rate.output * 60 * 60 * 24,
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 'day', output.unit = 'mg/d/kg',
                 volume = 1.2, mass = 0.5)$rate.output)
  # d to m
  expect_equal(
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 'day', output.unit = 'mg/m/kg',
                 volume = 1.2, mass = 0.5)$rate.output * 60 * 24,
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 'day', output.unit = 'mg/d/kg',
                 volume = 1.2, mass = 0.5)$rate.output)
  # d to h
  expect_equal(
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 'day', output.unit = 'mg/h/kg',
                 volume = 1.2, mass = 0.5)$rate.output * 24,
    convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 'day', output.unit = 'mg/d/kg',
                 volume = 1.2, mass = 0.5)$rate.output)
})

test_that("convert_rate: different magnitudes of umol/mmol/mol changes rates by correct magnitude multiplier", {

  expect_equal(convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mol/s/kg',
                            volume = 1.2, mass = 0.5)$rate.output,
               convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mmol/s/kg',
                            volume = 1.2, mass = 0.5)$rate.output/1000)
  expect_equal(convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mol/s/kg',
                            volume = 1.2, mass = 0.5)$rate.output,
               convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'umol/s/kg',
                            volume = 1.2, mass = 0.5)$rate.output/1000/1000)
  expect_equal(convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'umol/s/kg',
                            volume = 1.2, mass = 0.5)$rate.output,
               convert_rate(0.9, oxy.unit = 'mg/l', time.unit = 's', output.unit = 'mmol/s/kg',
                            volume = 1.2, mass = 0.5)$rate.output*1000)
})

}) ## turns printing back on
