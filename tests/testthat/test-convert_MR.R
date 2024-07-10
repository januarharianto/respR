# library(testthat)
# testthat::test_file("tests/testthat/test-convert_MR.R")
# covr::file_coverage("R/convert_MR.R", "tests/testthat/test-convert_MR.R")
# cvr <- withr::with_envvar(c("NOT_CRAN" = "true"), covr::package_coverage())
# covr::report(cvr)

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
  # create testing objects
  {
    S <- 35
    t <- 12
    P <- 1.01

    # single unconverted rate to convert
    rate <- -1.82

    # multiple unconverted rates to convert
    rates <- c(-1.82, -2.3, -0.56, 5.677, 3.88)

    # convert_rate objects
    cnvrt_obj <- convert_rate(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "ml/m",
                              volume = 1.09,
                              S = S, t = t, P = P)
    cnvrt_obj_ms <- convert_rate(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "ml/m/g",
                              mass = 0.1,
                              volume = 1.09,
                              S = S, t = t, P = P)
    cnvrt_obj_as <- convert_rate(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "ml/m/cm2",
                              area = 0.1,
                              volume = 1.09,
                              S = S, t = t, P = P)
    cnvrt_obj_noStP <- convert_rate(rate,
                                    oxy.unit = "mg/l",
                                    time.unit = "min",
                                    output.unit = "mg/m",
                                    volume = 1.09,
                                    S = NULL, t = NULL, P = NULL)
    # convert_rate.ft object
    cnvrtft_obj <- convert_rate.ft(rate,
                                   oxy.unit = "mg/l",
                                   time.unit = "min",
                                   flowrate.unit = "l/m",
                                   output.unit = "ml/m",
                                   S = S, t = t, P = P)
    cnvrtft_obj_noStP <- convert_rate.ft(rate,
                                         oxy.unit = "mg/l",
                                         time.unit = "min",
                                         flowrate.unit = "l/m",
                                         output.unit = "mg/m",
                                         S = NULL, t = NULL, P = NULL)

    # EMPTY convert_rate object
    cnv_rt_empty <- calc_rate(sardine.rd[,1:2], 10:20, 1030:1040, plot = FALSE) %>%
      convert_rate("%Air", "s", "mg/h", 1, S = 30, t=10, P =1, plot = FALSE) %>%
      select_rate("duration", c(2000, 3000))

    # EMPTY convert_rate.ft object
    cnv_rt.ft_empty <- inspect.ft(flowthrough.rd, 1, 2, 3, plot = FALSE) %>%
      calc_rate.ft(1, 500:510, 900:910, plot = FALSE) %>%
      convert_rate.ft("mg/l", "l/m", "mg/h", S = 30, t=10, P =1, plot = FALSE) %>%
      select_rate.ft("duration", c(2000, 3000))
  }

  #  General checks ---------------------------------------------------------

  test_that("convert_MR - stops with incorrect inputs", {
    expect_error(convert_MR("string", "mg/h", ",mg/min"),
                 "convert_MR: The 'x' input should be a numeric value or vector, or 'convert_rate' or 'convert_rate.ft' object.")
    expect_error(convert_MR(suppressWarnings(inspect(urchins.rd, plot = F)), "mg/h", ",mg/min"),
                 "convert_MR: The 'x' input should be a numeric value or vector, or 'convert_rate' or 'convert_rate.ft' object.")
  })

  test_that("convert_MR - produces output of same class as input", {
    expect_is(convert_MR(10, "mg/h", to = "mg/min"),
              "numeric")
    expect_is(convert_MR(cnvrt_obj, to = "mg/min",
                         S = S, t = t, P = P),
              "convert_rate")
    expect_is(convert_MR(cnvrtft_obj, to = "mg/min",
                         S = S, t = t, P = P),
              "convert_rate.ft")
    expect_is(convert_MR(cnvrt_obj_ms, to = "mg/min/kg",
                         S = S, t = t, P = P),
              "convert_rate")
    expect_is(convert_MR(cnvrt_obj_as, to = "mg/min/m2",
                         S = S, t = t, P = P),
              "convert_rate")
  })

  test_that("convert_MR - stops if 'from' or 'to' parsing fails", {
    expect_error(convert_MR(10, from = 10, to = "mg/min"),
                 "convert_MR: Parsing of 'from' and/or 'to' units has failed.")
    expect_error(convert_MR(10, from = "mg/min", to = 10),
                 "convert_MR: Parsing of 'from' and/or 'to' units has failed.")
  })

  test_that("convert_MR - stops if 'from' and 'to' unit types are mismatched", {
    expect_error(convert_MR(10, from = "mg/s/kg", to = "mg/min"),
                 "convert_MR: Parsing of 'from' and/or 'to' units has failed.")
    expect_error(convert_MR(10, from = "mg/s", to = "mg/min/kg"),
                 "convert_MR: Parsing of 'from' and/or 'to' units has failed.")
    expect_error(convert_MR(10, from = "mg/s", to = "mg/min/m2"),
                 "convert_MR: Parsing of 'from' and/or 'to' units has failed.")
    expect_error(convert_MR(10, from = "mg/s/g", to = "mg/min/m2"),
                 "convert_MR: 'from' and 'to' unit types conflict.")
  })

  test_that("convert_MR - stops if 'to' is NULL", {
    expect_error(convert_MR(10, from = "mg/h", to = NULL),
                 "convert_MR: Please specify a 'to' unit.")
  })

  test_that("convert_MR - stops if 'from' is NULL", {
    expect_error(convert_MR(10, from = NULL, to = "mg/h"),
                 "convert_MR: Please specify a 'from' unit.")
  })

  test_that("convert_MR - stops if 'x' input contains no rates", {
    expect_error(convert_MR(cnv_rt_empty, from = NULL, to = "ml/s"),
                 "convert_MR: No rates found in input object.")
    expect_error(convert_MR(cnv_rt.ft_empty, from = NULL, to = "ml/s"),
                 "convert_MR: No rates found in input object.")
  })

  test_that("convert_MR - stops if oxygen amount unit not recognised", {
    expect_error(convert_MR(-0.01, from = "mg/h", to = "kg/min"),
                 regexp = "convert_MR: 'kg' unit not recognised as an oxygen unit that can be used for rates or concentrations.")
  })

  test_that("convert_MR - stops if mass or area amount unit not recognised", {
    expect_error(convert_MR(-0.01, from = "mg/h/mmHg", to = "ug/min/mmHg"),
                 regexp = "convert_MR: unit 'mmHg' not recognised as a mass or area unit.")
  })

  test_that("convert_MR - message that 'from' ignored with convert_rate or convert_rate.ft inputs", {
    expect_message(convert_MR(cnvrt_obj, from = "mg/h", to = "ml/min",
                              S = S, t = t, P = P),
                   "convert_MR: 'from' input ignored. For 'convert_rate' objects the input unit is extracted automatically.")
    expect_message(convert_MR(cnvrtft_obj, from = "mg/h", to = "ml/min",
                              S = S, t = t, P = P),
                   "convert_MR: 'from' input ignored. For 'convert_rate.ft' objects the input unit is extracted automatically.")
  })

  test_that("convert_MR - message that only first 100 conversions are printed", {
    expect_message(convert_MR(1:200, from = "mg/h", to = "ml/min",
                              S = S, t = t, P = P),
                   "Printing first 100 rates only...")
  })

  test_that("convert_MR - S,t,P are extracted from 'convert_rate' and 'convert_rate.ft' objects", {
    obj_no_StP <- convert_MR(cnvrt_obj, to = "mg/min")
    obj_StP <- convert_MR(cnvrt_obj, to = "mg/min", S = S, t = t, P = P)
    obj.ft_no_StP <- convert_MR(cnvrtft_obj, to = "mg/min")
    obj.ft_StP <- convert_MR(cnvrtft_obj, to = "mg/min", S = S, t = t, P = P)

    # same with or without actual StP input
    expect_equal(obj_no_StP$summary,
                 obj_StP$summary)
    expect_equal(obj_no_StP$inputs,
                 obj_StP$inputs)
    # output has same StP as input
    expect_equal(obj_no_StP$inputs[8:10],
                 cnvrt_obj$inputs[8:10])
    # same for flowthrough
    expect_equal(obj.ft_no_StP$summary,
                 obj.ft_StP$summary)
    expect_equal(obj.ft_no_StP$inputs,
                 obj.ft_StP$inputs)
    expect_equal(obj_no_StP$inputs[8:10],
                 cnvrtft_obj$inputs[7:9])
  })

  test_that("convert_MR - S,t,P are ignored if different in 'convert_rate' and 'convert_rate.ft' objects", {

    # same with or without actual StP input
    expect_message(convert_MR(cnvrt_obj, to = "mg/min", S = 30),
                   "convert_MR: 'S' input ignored. For 'convert_rate' objects 'S' is extracted automatically if it is present.")
    expect_message(convert_MR(cnvrt_obj, to = "mg/min", t = 30),
                   "convert_MR: 't' input ignored. For 'convert_rate' objects 't' is extracted automatically if it is present.")
    expect_message(convert_MR(cnvrt_obj, to = "mg/min", P = 1),
                   "convert_MR: 'P' input ignored. For 'convert_rate' objects 'P' is extracted automatically if it is present.")
    expect_message(convert_MR(cnvrtft_obj, to = "mg/min", S = 30),
                   "convert_MR: 'S' input ignored. For 'convert_rate.ft' objects 'S' is extracted automatically if it is present.")
    expect_message(convert_MR(cnvrtft_obj, to = "mg/min", t = 30),
                   "convert_MR: 't' input ignored. For 'convert_rate.ft' objects 't' is extracted automatically if it is present.")
    expect_message(convert_MR(cnvrtft_obj, to = "mg/min", P = 1),
                   "convert_MR: 'P' input ignored. For 'convert_rate.ft' objects 'P' is extracted automatically if it is present.")

    # output has same StP as input
    expect_equal(convert_MR(cnvrt_obj, to = "mg/min", S = 30)$inputs[8:10],
                 cnvrt_obj$inputs[8:10])
    expect_equal(convert_MR(cnvrtft_obj, to = "mg/min", S = 30)$inputs[7:9],
                 cnvrtft_obj$inputs[7:9])
  })

  test_that("convert_MR - no error or message if input StP are same as in 'convert_rate' and 'convert_rate.ft' objects", {
    expect_message(convert_MR(cnvrt_obj, to = "mg/min", S = 35, t = 12, P = 1.01),
                   NA)
    expect_error(convert_MR(cnvrt_obj, to = "mg/min", S = 35, t = 12, P = 1.01),
                 NA)
    expect_equal(convert_MR(cnvrt_obj, to = "mg/min", S = 30, t = 12, P = 1.01)$inputs[8:10],
                 list(S = 35, t = 12, P = 1.01))
    expect_message(convert_MR(cnvrtft_obj, to = "mg/min", S = 35, t = 12, P = 1.01),
                   NA)
    expect_error(convert_MR(cnvrtft_obj, to = "mg/min", S = 35, t = 12, P = 1.01),
                 NA)
    expect_equal(convert_MR(cnvrtft_obj, to = "mg/min", S = 30, t = 12, P = 1.01)$inputs[7:9],
                 list(S = 35, t = 12, P = 1.01))
  })

  test_that("convert_MR - StP error if StP required and input 'convert_rate' and 'convert_rate.ft' objects don't contain any", {
    expect_error(convert_MR(cnvrt_obj_noStP, to = "ml/min", S = NULL, t = NULL, P = NULL),
                 "convert_MR: Input or output units require Salinity input")
    expect_error(convert_MR(cnvrt_obj_noStP, to = "ml/min", S = 30, t = NULL, P = NULL),
                 "convert_MR: Input or output units require Temperature input")
    expect_message(convert_MR(cnvrt_obj_noStP, to = "ml/min", S = 30, t = 12, P = NULL),
                   "convert_MR: Input or output units require Atmospheric Pressure input")

    expect_error(convert_MR(cnvrtft_obj_noStP, to = "ml/min", S = NULL, t = NULL, P = NULL),
                 "convert_MR: Input or output units require Salinity input")
    expect_error(convert_MR(cnvrtft_obj_noStP, to = "ml/min", S = 30, t = NULL, P = NULL),
                 "convert_MR: Input or output units require Temperature input")
    expect_message(convert_MR(cnvrtft_obj_noStP, to = "ml/min", S = 30, t = 12, P = NULL),
                   "convert_MR: Input or output units require Atmospheric Pressure input")
  })

  test_that("convert_MR - StP correctly added to output if 'convert_rate' and 'convert_rate.ft' input doesn't contain them", {
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "ml/min", S = 30, t = 12, P = 1.01)$inputs$S,
                 30)
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "ml/min", S = 30, t = 12, P = 1.01)$inputs$t,
                 12)
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "ml/min", S = 30, t = 12, P = 1.01)$inputs$P,
                 1.01)
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "ml/min", S = 30, t = 12, P = 1.01)$inputs$S,
                 30)
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "ml/min", S = 30, t = 12, P = 1.01)$inputs$t,
                 12)
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "ml/min", S = 30, t = 12, P = 1.01)$inputs$P,
                 1.01)
  })

  test_that("convert_MR - warning if P out of range", {
    expect_warning(convert_MR(rate, from = "mg/h", to = "ml/min", S = 30, t = 12, P = 10),
                   "convert_MR: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range.")
    expect_warning(convert_MR(cnvrt_obj_noStP, from = "mg/h", to = "ml/min", S = 30, t = 12, P = 10),
                   "convert_MR: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range.")
    expect_warning(convert_MR(cnvrtft_obj_noStP, from = "mg/h", to = "ml/min", S = 30, t = 12, P = 10),
                   "convert_MR: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range.")
  })

  test_that("convert_MR - default P is used and added to output for 'convert_rate' and 'convert_rate.ft' inputs if they don't contain it", {
    expect_equal(convert_MR(cnvrt_obj_noStP, from = "mg/h", to = "ml/min", S = 30, t = 12, P = NULL)$inputs$P,
                 1.013253)
    expect_equal(convert_MR(cnvrtft_obj_noStP, from = "mg/h", to = "ml/min", S = 30, t = 12, P = NULL)$inputs$P,
                 1.013253)
  })

  test_that("convert_MR - StP inputs correctly added (OR NOT) to 'convert_rate' and 'convert_rate.ft' objects if they don't have them", {
    # NOT added if different
    expect_equal(convert_MR(cnvrt_obj, to = "mg/h", S = 20, t = 20, P = 1)$inputs[8:10],
                 list(S = 35, t = 12, P = 1.01))
    # added
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "mg/h", S = 30, t = 12, P = 1.01)$inputs[8:10],
                 list(S = 30, t = 12, P = 1.01))
    # NULL stays NULL
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "mg/h", S = 30, t = 12, P = NULL)$inputs[8:10],
                 list(S = 30, t = 12, P = NULL))
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "mg/h", S = NULL, t = 12, P = 1)$inputs[8:10],
                 list(S = NULL, t = 12, P = 1))
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "mg/h", S = NULL, t = NULL, P = 1)$inputs[8:10],
                 list(S = NULL, t = NULL, P = 1))
    expect_equal(convert_MR(cnvrt_obj_noStP, to = "mg/h", S = NULL, t = NULL, P = NULL)$inputs[8:10],
                 list(S = NULL, t = NULL, P = NULL))

    # NOT added if different
    expect_equal(convert_MR(cnvrtft_obj, to = "mg/h", S = 20, t = 20, P = 1)$inputs[7:9],
                 list(S = 35, t = 12, P = 1.01))
    # added
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "mg/h", S = 30, t = 12, P = 1.01)$inputs[7:9],
                 list(S = 30, t = 12, P = 1.01))
    # NULL stays NULL
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "mg/h", S = 30, t = 12, P = NULL)$inputs[7:9],
                 list(S = 30, t = 12, P = NULL))
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "mg/h", S = NULL, t = 12, P = 1)$inputs[7:9],
                 list(S = NULL, t = 12, P = 1))
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "mg/h", S = NULL, t = NULL, P = 1)$inputs[7:9],
                 list(S = NULL, t = NULL, P = 1))
    expect_equal(convert_MR(cnvrtft_obj_noStP, to = "mg/h", S = NULL, t = NULL, P = NULL)$inputs[7:9],
                 list(S = NULL, t = NULL, P = NULL))
  })


  # Known exact values tests ------------------------------------------------

  test_that("convert_MR - known values - absolute rates", {
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "ug/s",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "ug/m"  , S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "ug/h"  , S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "ug/d",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mg/s",   S=S, t=t, P=P, quiet = TRUE), -0.001)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mg/m"  , S=S, t=t, P=P, quiet = TRUE), -0.001 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mg/h"  , S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mg/d"  , S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "pmol/s", S=S, t=t, P=P, quiet = TRUE), -31251.171918)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "pmol/m", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "pmol/h", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "pmol/d", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "nmol/s", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "nmol/m", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "nmol/h", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "nmol/d", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "umol/s", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "umol/m", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "umol/h", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "umol/d", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mmol/s", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mmol/m", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mmol/h", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mmol/d", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mol/s",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mol/m" , S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mol/h",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mol/d",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "uL/s"  , S=S, t=t, P=P, quiet = TRUE), -0.7327680862)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "uL/m",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "uL/h",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "uL/d",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mL/s",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mL/m",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mL/h"  , S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mL/d",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mm3/s" , S=S, t=t, P=P, quiet = TRUE), -0.7327680862)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mm3/m" , S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mm3/h" , S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mm3/d",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "cm3/s" , S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "cm3/m",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "cm3/h" , S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "cm3/d",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24)
  })

  test_that("convert_MR - known values - mass-specific rates", {
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/s/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/m/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/h/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/d/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/s/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/m/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/h/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/d/ug",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/s/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/m/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/h/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/d/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/s/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/m/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/h/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/d/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/s/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/m/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/h/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/d/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/s/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/m/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/h/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/d/ug", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/s/ug",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/m/ug",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/h/ug",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/d/ug",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/s/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/m/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/h/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/d/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/s/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/m/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/h/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/d/ug",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/s/ug",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/m/ug",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/h/ug",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/d/ug",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/s/ug",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/m/ug",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/h/ug",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/d/ug",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e6)

    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/s/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/m/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/h/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/d/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/s/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/m/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/h/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/d/mg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/s/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/m/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/h/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/d/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/s/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/m/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/h/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/d/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/s/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/m/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/h/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/d/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/s/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/m/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/h/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/d/mg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/s/mg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/m/mg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/h/mg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/d/mg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/s/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/m/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/h/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/d/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/s/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/m/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/h/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/d/mg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/s/mg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/m/mg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/h/mg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/d/mg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/s/mg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/m/mg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/h/mg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/d/mg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e3)

    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/s/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/m/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/h/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/d/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/s/g",   S=S, t=t, P=P, quiet = TRUE), -0.001)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/m/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/h/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/d/g",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/s/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/m/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/h/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/d/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/s/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/m/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/h/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/d/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/s/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/m/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/h/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/d/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/s/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/m/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/h/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/d/g", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/s/g",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/m/g",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/h/g",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/d/g",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/s/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/m/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/h/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/d/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/s/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/m/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/h/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/d/g",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/s/g",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/m/g",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/h/g",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/d/g",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/s/g",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/m/g",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/h/g",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/d/g",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24)

    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/s/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/m/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/h/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ugO2/d/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/s/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/m/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/h/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mgO2/d/kg",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/s/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/m/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/h/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmolO2/d/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/s/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/m/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/h/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmolO2/d/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/s/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/m/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/h/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umolO2/d/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/s/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/m/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/h/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mmolO2/d/kg", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/s/kg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/m/kg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/h/kg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "molO2/d/kg",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/s/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/m/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/h/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uLO2/d/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/s/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/m/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/h/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mLO2/d/kg",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/s/kg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/m/kg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/h/kg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3O2/d/kg",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/s/kg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/m/kg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/h/kg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/d/kg",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 * 1e3)
  })

  test_that("convert_MR - known values - area-specific rates", {
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/s/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/m/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/h/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/d/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/s/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/m/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/h/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/d/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/s/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/m/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/h/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/d/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/s/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/m/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/h/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/d/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/s/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/m/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/h/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/d/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/s/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/m/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/h/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/d/mm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/s/mm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/m/mm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/h/mm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/d/mm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/s/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/m/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/h/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/d/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/s/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/m/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/h/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/d/mm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/s/mm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/m/mm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/h/mm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/d/mm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/s/mm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/m/mm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/h/mm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/d/mm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e6)

    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/s/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/m/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/h/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/d/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/s/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/m/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/h/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/d/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/s/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/m/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/h/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/d/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/s/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/m/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/h/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/d/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/s/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/m/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/h/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/d/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/s/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/m/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/h/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/d/cm2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/s/cm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/m/cm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/h/cm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/d/cm2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/s/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/m/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/h/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/d/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/s/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/m/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/h/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/d/cm2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/s/cm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/m/cm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/h/cm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/d/cm2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/s/cm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/m/cm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/h/cm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 / 1e4)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/d/cm2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 / 1e4)

    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/s/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/m/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/h/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/d/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/s/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/m/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/h/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/d/m2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/s/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/m/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/h/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/d/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/s/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/m/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/h/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/d/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/s/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/m/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/h/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/d/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/s/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/m/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/h/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/d/m2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/s/m2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/m/m2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/h/m2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/d/m2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/s/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/m/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/h/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/d/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/s/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/m/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/h/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/d/m2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/s/m2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/m/m2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/h/m2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/d/m2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/s/m2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/m/m2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/h/m2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/d/m2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24)

    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/s/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/m/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/h/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "ugO2/d/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e3 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/s/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/m/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/h/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mgO2/d/km2",   S=S, t=t, P=P, quiet = TRUE), -0.001 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/s/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/m/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/h/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "pmolO2/d/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/s/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/m/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/h/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "nmolO2/d/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e3 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/s/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/m/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/h/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "umolO2/d/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/s/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/m/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/h/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mmolO2/d/km2", S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e9 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/s/km2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/m/km2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/h/km2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "molO2/d/km2",  S=S, t=t, P=P, quiet = TRUE), -31251.171918 / 1e12 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/s/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/m/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/h/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "uLO2/d/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/s/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/m/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/h/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mLO2/d/km2",   S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/s/km2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/m/km2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/h/km2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "mm3O2/d/km2",  S=S, t=t, P=P, quiet = TRUE), -0.7327680862 * 60 * 60 * 24 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/s/km2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/m/km2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/h/km2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 1e6)
    expect_equal(convert_MR(-0.001, from = "mg/s/m2", to = "cm3/d/km2",    S=S, t=t, P=P, quiet = TRUE), -0.7327680862 / 1e3 * 60 * 60 * 24 * 1e6)
  })

  test_that("convert_MR - known values - different StP", {
    # mass amounts - different StP should NOT cause different rates - same as above
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "ug/s",   S=0, t=20, P=1, quiet = TRUE), -0.001 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "pmol/s", S=0, t=20, P=1, quiet = TRUE), -31251.171918)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "nmol/m", S=0, t=20, P=1, quiet = TRUE), -31251.171918 / 1e3 * 60)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "umol/h", S=0, t=20, P=1, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60)

    # vol amounts - different StP should cause different rates - this value checked with convert_rate
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "uL/s",  S=0, t=20, P=1, quiet = TRUE), -0.760940595)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mm3/s", S=0, t=20, P=1, quiet = TRUE), -0.760940595)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "mL/d",  S=0, t=20, P=1, quiet = TRUE), -0.760940595 / 1e3 * 60 * 60 * 24)
    expect_equal(convert_MR(-0.001, from = "mg/s", to = "cm3/d", S=0, t=20, P=1, quiet = TRUE), -0.760940595 / 1e3 * 60 * 60 * 24)

    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "ug/s/kg",   S=0, t=20, P=1, quiet = TRUE), -0.001 * 1e3 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "pmol/s/kg", S=0, t=20, P=1, quiet = TRUE), -31251.171918 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "nmol/m/kg", S=0, t=20, P=1, quiet = TRUE), -31251.171918 / 1e3 * 60 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "umol/h/kg", S=0, t=20, P=1, quiet = TRUE), -31251.171918 / 1e6 * 60 * 60 * 1e3)

    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "uL/s/kg" ,  S=0, t=20, P=1, quiet = TRUE), -0.760940595 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mm3/s/kg",  S=0, t=20, P=1, quiet = TRUE), -0.760940595 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "mL/d/kg",   S=0, t=20, P=1, quiet = TRUE), -0.760940595 / 1e3 * 60 * 60 * 24 * 1e3)
    expect_equal(convert_MR(-0.001, from = "mg/s/g", to = "cm3/d/kg",  S=0, t=20, P=1, quiet = TRUE), -0.760940595 / 1e3 * 60 * 60 * 24 * 1e3)

  })

}) ## end capture.output


