# library(testthat)
# rm(list=ls())
# testthat::test_file("tests/testthat/test-convert_DO.R")
# covr::file_coverage("R/convert_DO.R", "tests/testthat/test-convert_DO.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
  test_that("convert_DO stops if `x` not numeric", {
    expect_error(convert_DO("text", from = "%Air", to = "mg/l",
                            S = 35, t =10),
                 "convert_DO: input 'x' must be a numeric value or vector.")
  })

  test_that("convert_DO stops if t, S, or P required but not entered", {
    tsp_req <- c("uL/L.o2", "mm3/L.o2",
                 "mL/L.o2", "cm3/L.o2",
                 "uL/kg.o2", "mm3/kg.o2",
                 "mL/kg.o2", "cm3/kg.o2",
                 "%Air.o2", "%Oxy.o2",
                 "Torr.o2p",
                 "hPa.o2p", "kPa.o2p",
                 "inHg.o2p", "mmHg.o2p",
                 "mg/kg.o2", "ug/kg.o2",
                 "ppm.o2",
                 "mol/kg.o2", "mmol/kg.o2", "umol/kg.o2", "nmol/kg.o2", "pmol/kg.o2")
    for(i in tsp_req) expect_error(convert_DO(-0.1, from = i, to = "mg/l",
                                              S = 35, t = NULL),
                                   "convert_DO: Input or output units require Temperature input")
    for(i in tsp_req) expect_error(convert_DO(-0.1, from = i, to = "mg/l",
                                              S = NULL, t = 20),
                                   "convert_DO: Input or output units require Salinity input")
    for(i in tsp_req) expect_message(convert_DO(-0.1, from = i, to = "mg/l",
                                                S = 30, t = 20, P = NULL),
                                     "convert_DO: Input or output units require Atmospheric Pressure input")
  })

  test_that("convert_DO stops if t, S, or P not same length as x or single values", {

    expect_error(convert_DO(-0.1, from = "uL/L.o2", to = "mg/l",
                            S = 35:36, t = 12, P = 1),
                 "convert_DO: The 'S' input must be a single value or the same length as the rates to be converted.")
    expect_error(convert_DO(c(-0.1,-0.2,-0.3), from = "uL/L.o2", to = "mg/l",
                            S = 35:36, t = 12, P = 1),
                 "convert_DO: The 'S' input must be a single value or the same length as the rates to be converted.")
    expect_error(convert_DO(-0.1, from = "uL/L.o2", to = "mg/l",
                            S = 35, t = 12:13, P = 1),
                 "convert_DO: The 't' input must be a single value or the same length as the rates to be converted.")
    expect_error(convert_DO(c(-0.1,-0.2,-0.3), from = "uL/L.o2", to = "mg/l",
                            S = 35, t = 12:13, P = 1),
                 "convert_DO: The 't' input must be a single value or the same length as the rates to be converted.")
    expect_error(convert_DO(-0.1, from = "uL/L.o2", to = "mg/l",
                            S = 35, t = 12, P = c(1,1.01)),
                 "convert_DO: The 'P' input must be a single value or the same length as the rates to be converted.")
    expect_error(convert_DO(c(-0.1,-0.2,-0.3), from = "uL/L.o2", to = "mg/l",
                            S = 35, t = 12, P = c(1,1.01)),
                 "convert_DO: The 'P' input must be a single value or the same length as the rates to be converted.")

  })

  test_that("convert_DO accepts vectors of t, S, or P and produces correct values", {

    expect_error(convert_DO(c(-0.1,-0.2,-0.3), from = "uL/L.o2", to = "mg/l",
                            S = c(35,36,37), t = 12, P = 1),
                 NA)
    expect_error(convert_DO(c(-0.1,-0.2,-0.3), from = "uL/L.o2", to = "mg/l",
                            S = 35, t = c(12,13,14), P = 1),
                 NA)
    expect_error(convert_DO(c(-0.1,-0.2,-0.3), from = "uL/L.o2", to = "mg/l",
                            S = 35, t = 12, P = c(0.99, 1, 1.01)),
                 NA)

    # These should produce exact same results
    # fake data
    O <- 240:249
    S <- seq(30, 30.9, 0.1)
    t <- seq(10, 10.9, 0.1)
    P <- seq(0.9, 0.99, 0.01)

    df <- data.frame(O = O,
                     S = S,
                     t = t,
                     P = P)

    # empty vector for results
    conv <- c()
    # loop
    for(i in 1:nrow(df)) conv[i] <- convert_DO(x = df$O[i],
                                               from = "umol/kg",
                                               to = "mg/l",
                                               S = df$S[i],
                                               t = df$t[i],
                                               P = df$P[i])

    expect_equal(convert_DO(x = O,
                            from = "umol/kg",
                            to = "mg/l",
                            S = S,
                            t = t,
                            P = P),
                 conv)

  })

  test_that("convert_DO output conversions, using %Air, have expected results", {
    expect_equal(round(convert_DO(10, "%Air", "mg/l",    S = 35, t = 25, P = 1.013253), 3), 0.675)
    expect_equal(round(convert_DO(10, "%Air", "ug/l",    S = 35, t = 25, P = 1.013253), 3), 675.11)
    expect_equal(round(convert_DO(10, "%Air", "mol/l",   S = 35, t = 25, P = 1.013253), 9), 0.000021098)
    expect_equal(round(convert_DO(10, "%Air", "mmol/l",  S = 35, t = 25, P = 1.013253), 3), 0.021)
    expect_equal(round(convert_DO(10, "%Air", "umol/l",  S = 35, t = 25, P = 1.013253), 3), 21.098)
    expect_equal(round(convert_DO(10, "%Air", "nmol/l",  S = 35, t = 25, P = 1.013253), 3), 21097.984)
    expect_equal(round(convert_DO(10, "%Air", "pmol/l",  S = 35, t = 25, P = 1.013253), 3), 21097984.479)
    expect_equal(round(convert_DO(10, "%Air", "ml/l",    S = 35, t = 25, P = 1.013253), 3), 0.516)
    expect_equal(round(convert_DO(10, "%Air", "ul/l",    S = 35, t = 25, P = 1.013253), 3), 515.669)
    expect_equal(round(convert_DO(10, "%Air", "cm3/l",   S = 35, t = 25, P = 1.013253), 3), 0.516)
    expect_equal(round(convert_DO(10, "%Air", "mm3/l",   S = 35, t = 25, P = 1.013253), 3), 515.669)
    expect_equal(round(convert_DO(10, "%Air", "mg/kg",   S = 35, t = 25, P = 1.013253), 3), 0.66)
    expect_equal(round(convert_DO(10, "%Air", "ppm",     S = 35, t = 25, P = 1.013253), 3), 0.66)
    expect_equal(round(convert_DO(10, "%Air", "ug/kg",   S = 35, t = 25, P = 1.013253), 3), 659.79)
    expect_equal(round(convert_DO(10, "%Air", "mol/kg",  S = 35, t = 25, P = 1.013253), 9), 0.000020619)
    expect_equal(round(convert_DO(10, "%Air", "mmol/kg", S = 35, t = 25, P = 1.013253), 3), 0.021)
    expect_equal(round(convert_DO(10, "%Air", "umol/kg", S = 35, t = 25, P = 1.013253), 3), 20.619)
    expect_equal(round(convert_DO(10, "%Air", "nmol/kg", S = 35, t = 25, P = 1.013253), 3), 20619.216)
    expect_equal(round(convert_DO(10, "%Air", "pmol/kg", S = 35, t = 25, P = 1.013253), 3), 20619215.552)
    expect_equal(round(convert_DO(10, "%Air", "ml/kg",   S = 35, t = 25, P = 1.013253), 3), 0.504)
    expect_equal(round(convert_DO(10, "%Air", "ul/kg",   S = 35, t = 25, P = 1.013253), 3), 503.967)
    expect_equal(round(convert_DO(10, "%Air", "cm3/kg",  S = 35, t = 25, P = 1.013253), 3), 0.504)
    expect_equal(round(convert_DO(10, "%Air", "mm3/kg",  S = 35, t = 25, P = 1.013253), 3), 503.967)
    expect_equal(round(convert_DO(10, "%Air", "%Air",    S = 35, t = 25, P = 1.013253), 3), 10)
    expect_equal(round(convert_DO(10, "%Air", "%Oxy",    S = 35, t = 25, P = 1.013253), 3), 2.095)
    expect_equal(round(convert_DO(10, "%Air", "hPa",     S = 35, t = 25, P = 1.013253), 3), 20.854)
    expect_equal(round(convert_DO(10, "%Air", "kPa",     S = 35, t = 25, P = 1.013253), 3), 2.085)
    expect_equal(round(convert_DO(10, "%Air", "mmHg",    S = 35, t = 25, P = 1.013253), 3), 15.642)
    expect_equal(round(convert_DO(10, "%Air", "inHg",    S = 35, t = 25, P = 1.013253), 3), 0.616)
    expect_equal(round(convert_DO(10, "%Air", "Torr",    S = 35, t = 25, P = 1.013253), 3), 15.642)
  })

  test_that("convert_DO - various conversions to exact values", {
    expect_equal(convert_DO(10, "mg/l", "mg/l",    S = 35, t = 25, P = 1.013253), 10)
    expect_equal(convert_DO(10, "ug/l", "mg/l",    S = 35, t = 25, P = 1.013253), 10/1000)
    expect_equal(convert_DO(10, "mmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), 319.988)
    expect_equal(convert_DO(10, "umol/l", "mg/l",  S = 35, t = 25, P = 1.013253), 319.988/1000)
    expect_equal(convert_DO(10, "nmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), 319.988/1000/1000)
    expect_equal(convert_DO(10, "pmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), 319.988/1000/1000/1000)
    expect_equal(convert_DO(10, "ml/l", "mg/l",    S = 35, t = 25, P = 1.013253), 13.091931)
    expect_equal(convert_DO(10, "ul/l", "mg/l",    S = 35, t = 25, P = 1.013253), 13.091931/1000)
    expect_equal(convert_DO(10, "cm3/l", "mg/l",   S = 35, t = 25, P = 1.013253), 13.091931)
    expect_equal(convert_DO(10, "mm3/l", "mg/l",   S = 35, t = 25, P = 1.013253), 13.091931/1000)
    expect_equal(convert_DO(10, "mg/kg", "mg/l",   S = 35, t = 25, P = 1.013253), 10.2321955)
    expect_equal(convert_DO(10, "ppm", "mg/l",     S = 35, t = 25, P = 1.013253), 10.2321955)
    expect_equal(convert_DO(10, "ug/kg", "mg/l",   S = 35, t = 25, P = 1.013253), 10.2321955/1000)
    expect_equal(convert_DO(10, "mmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), 327.417977)
    expect_equal(convert_DO(10, "umol/kg", "mg/l", S = 35, t = 25, P = 1.013253), 327.417977/1000)
    expect_equal(convert_DO(10, "nmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), 327.417977/1000/1000)
    expect_equal(convert_DO(10, "pmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), 327.417977/1000/1000/1000)
    expect_equal(convert_DO(10, "ml/kg", "mg/l",   S = 35, t = 25, P = 1.013253), 13.3959197)
    expect_equal(convert_DO(10, "ul/kg", "mg/l",   S = 35, t = 25, P = 1.013253), 13.3959197/1000)
    expect_equal(convert_DO(10, "cm3/kg", "mg/l",  S = 35, t = 25, P = 1.013253), 13.3959197)
    expect_equal(convert_DO(10, "mm3/kg", "mg/l",  S = 35, t = 25, P = 1.013253), 13.3959197/1000)
    expect_equal(convert_DO(10, "%Air", "mg/l",    S = 35, t = 25, P = 1.013253), 0.6751102)
    expect_equal(convert_DO(10, "%Oxy", "mg/l",    S = 35, t = 25, P = 1.013253), 3.2230983756)
    expect_equal(convert_DO(10, "Torr", "mg/l",    S = 35, t = 25, P = 1.013253), 0.43160273)
    expect_equal(convert_DO(10, "hPa", "mg/l",     S = 35, t = 25, P = 1.013253), 0.32373349425)
    expect_equal(convert_DO(10, "kPa", "mg/l",     S = 35, t = 25, P = 1.013253), 0.32373349425*10)
    expect_equal(convert_DO(10, "mmHg", "mg/l",    S = 35, t = 25, P = 1.013253), 0.4316028)
    expect_equal(convert_DO(10, "inHg", "mg/l",    S = 35, t = 25, P = 1.013253), 10.96271098)
  })

  ## check that `from` inputs do not produce error
  test_that("convert_DO converts different units without error", {
    expect_error(convert_DO(10, "mg/l", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ug/l", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "mmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "umol/l", "mg/l",  S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "nmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "pmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ml/l", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ul/l", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "cm3/l", "mg/l",   S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "mm3/l", "mg/l",   S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "mg/kg", "mg/l",   S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ppm", "mg/l",     S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ug/kg", "mg/l",   S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "mmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "umol/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "nmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "pmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ml/kg", "mg/l",   S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "ul/kg", "mg/l",   S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "cm3/kg", "mg/l",  S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "mm3/kg", "mg/l",  S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "%Air", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "%Oxy", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "Torr", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "hPa", "mg/l",     S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "kPa", "mg/l",     S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "mmHg", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
    expect_error(convert_DO(10, "inHg", "mg/l",    S = 35, t = 25, P = 1.013253), regexp = NA)
  })

  test_that("convert_DO produces the correct numeric output", {
    expect_is(convert_DO(10, "mg/l", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "ug/l", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "mol/l", "mg/l",   S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "mmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "umol/l", "mg/l",  S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "nmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "pmol/l", "mg/l",  S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "ml/l", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "mg/kg", "mg/l",   S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "ug/kg", "mg/l",   S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "mol/kg", "mg/l",  S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "mmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "umol/kg", "mg/l", S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "nmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "pmol/kg", "mg/l", S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "ml/kg", "mg/l",   S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "%Air", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "%Oxy", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "Torr", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "hPa", "mg/l",     S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "kPa", "mg/l",     S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "mmHg", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
    expect_is(convert_DO(10, "inHg", "mg/l",    S = 35, t = 25, P = 1.013253), "numeric")
  })

  test_that("convert_DO conversion works with changing salinity value", {
    expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 35, t = 25, P = 1.013253), 3), 0.506)
    expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 25, t = 25, P = 1.013253), 3), 0.536)
    expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S = 15, t = 25, P = 1.013253), 3), 0.567)
    expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S =  5, t = 25, P = 1.013253), 3), 0.601)
    expect_equal(round(convert_DO(7.5, "%Air", "mg/l", S =  0, t = 25, P = 1.013253), 3), 0.618)
  })

  test_that("convert_DO conversion works with changing pressure value", {
    expect_equal(round(convert_DO(7.5, "%Air", "mg/l", P = 0.9, S = 35, t = 25), 3),
                 0.45)
  })

  test_that("convert_DO conversion works with changing temperature", {
    expect_equal(round(convert_DO(100, "%Air", "mg/l", t = 25, P = 1.013253, S = 35), 3), 6.751)
    expect_equal(round(convert_DO(100, "%Air", "mg/l", t = 20, S = 35), 3), 7.377)
  })

  test_that("convert_DO S3 generics work", {

    ob <- convert_DO(10, "inHg", "mg/l", S = 35, t = 25, P = 1.013253, simplify = FALSE)
    ob_many <- convert_DO(10:30, "inHg", "mg/l", S = 35, t = 25, P = 1.013253, simplify = FALSE)

    expect_error(print(ob),
                 NA)
    expect_error(print(ob_many),
                 NA)
    expect_output(print(ob),
                  "Input values:")
    expect_output(print(ob_many),
                  "Showing only the first 20 conversions:")

    expect_error(summary(ob),
                 NA)
    expect_error(summary(ob_many),
                 NA)
    expect_output(summary(ob),
                  "Input values:")
    expect_output(summary(ob_many),
                  "Showing only the first 20 conversions:")
  })

  test_that("convert_DO stops if % operator (old one) is used", {
    expect_error(convert_DO(10, "%", "mg/l", S = 35, t = 25, P = 1.013253),
                 regexp = "convert_DO: unit \"%\" has been deprecated. Please use \"%Air\" or \"%Oxy\" instead. See unit_args().")
  })

  test_that("convert_DO - stops if unit not recognised", {
    expect_error(convert_DO(10, "text", "mg/l", S = 35, t = 25, P = 1.013253),
                 regexp = "convert_DO: unit 'text' not recognised. Check it is valid for the input or output type.")
  })

  ## checks against respirometry::conv_o2 results

  test_that("convert_DO: %Air and %Oxy return same results as respirometry::conv_o2", {
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

    # %Oxy
    # respR results
    res_respR <- apply(grid, 1, function(x) {
      suppressWarnings(convert_DO(x = x[1], from = "%Oxy", to = "mg/L", t = x[2], S = x[3], P = x[4]))
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
      suppressWarnings(convert_DO(x = x[1], from = "%Air", to = "mg/L", t = x[2], S = x[3], P = x[4]))
    })

    # respirometry results
    res_respirometry <- apply(grid, 1, function(x) {
      respirometry::conv_o2( o2 = x[1], from = "percent_a.s.", to = "mg_per_l",
                             temp = x[2], sal = x[3], atm_pres = x[4]*1000) ## nb diff pressure units
    })
    # check results same
    expect_true(all.equal(res_respR, res_respirometry))

  })

  test_that("convert_DO: warning if P is outside realistic range", {
    expect_warning(convert_DO(x = 100, from = "%Air", to = "mg/L",
                              t = 12, S = 30, P = 1.5),
                   regexp = "convert_DO: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range.")
    expect_warning(convert_DO(x = 100, from = "%Air", to = "mg/L",
                              t = 12, S = 30, P = 1000),
                   regexp = "convert_DO: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range.")
    expect_warning(convert_DO(x = 100, from = "%Air", to = "mg/L",
                              t = 12, S = 30, P = 0.01),
                   regexp = "convert_DO: One or more of the Atmospheric Pressure inputs 'P' are outside the normal realistic range.")
    expect_warning(convert_DO(x = 100, from = "%Air", to = "mg/L",
                              t = 12, S = 30, P = 1),
                   regexp = NA)
  })

  test_that("convert_DO: message if plot is used", {
    obj <- convert_DO(x = 100, from = "%Air", to = "mg/L",
                      t = 12, S = 30, P = 1, simplify = FALSE)
    expect_message(plot(obj),
                   regexp = "convert_DO: plot is not available for 'convert_DO' objects.")
  })

  test_that("convert_DO: 'mean' works", {
    obj1<- convert_DO(x = 100, from = "%Air", to = "mg/L",
                      t = 12, S = 30, P = 1, simplify = FALSE)
    obj2<- convert_DO(x = c(100,95), from = "%Air", to = "mg/L",
                      t = 12, S = 30, P = 1, simplify = FALSE)
    obj4<- convert_DO(x = c(100,95,90,85), from = "%Air", to = "mg/L",
                      t = 12, S = 30, P = 1, simplify = FALSE)

    expect_error(mean(obj1, pos = 2),
                 regexp = "mean.convert_DO: Invalid 'pos' rank: only 1 rates found.")
    expect_message(mean(obj1),
                   regexp = "Only 1 converted oxygen value found. Returning mean rate anyway...")
    expect_output(mean(obj4),
                  regexp = "Averaging all converted oxygen values.")
    expect_output(mean(obj4, pos = 1:2),
                  regexp = "Averaging converted oxygen values from entered 'pos' ranks:")
    expect_equal(mean(obj4, export = TRUE),
                 8.14208432688129)
    expect_is(mean(obj4),
              "convert_DO")
  })



  # Unit variation tests ----------------------------------------------------

  # Added when switching from huge lists of units to regex matching
  test_that("convert_DO: %Air input variations pass regex", {

    un.vars <- c("%A", "%a", "% A",
                 "%Air", "%air", "% air",
                 "percair", "perc Air", "perc.air",
                 "percentair", "percent Air", "percent.air",
                 "percentageair", "percentage Air", "percentage.air")
    sapply(un.vars, function(z) {
      expect_equal(convert_DO(x = 100, from = z, to = "mg/L",
                              t = 12, S = 30, P = 1),
                   8.80225332635)
    })

  })


})
