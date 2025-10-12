# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-oxy_crit.R")
# covr::file_coverage("R/oxy_crit.ft.R", "tests/testthat/test-oxy_crit.ft.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)

capture.output({  ## stops printing of outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
  # small testing dataset - data.table
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  # pure data.frame
  squid_df <- as.data.frame(squid.rd[seq.int(1, nrow(squid.rd), 100), ])
  # rate dataset
  squid_rate <- generate_mrdf(squid, 30)
  # inspect object
  squid_insp <- inspect(squid)
  # inspect object - multiple columns
  urch_insp <- suppressWarnings(inspect(urchins.rd, 1, 2:6))


  test_that("oxy_crit - works with default values", {
    expect_error(suppressMessages(oxy_crit(squid, plot = F)),
                 regexp = NA)
  })

  test_that("oxy_crit - accepts inspect objects", {
    sq_i <- inspect(squid, plot = F)
    expect_error(oxy_crit(sq_i, plot = F),
                 regexp = NA)
  })

  test_that("oxy_crit - accepts data frame objects", {
    expect_error(oxy_crit(as.data.frame(squid), plot = F),
                 regexp = NA)
    expect_error(oxy_crit(squid_df, plot = F),
                 regexp = NA)
  })

  test_that("oxy_crit - accepts data table objects", {
    expect_error(oxy_crit(squid, plot = F),
                 regexp = NA)
  })

  test_that("oxy_crit - stops if not a dataframe", {
    expect_error(oxy_crit(as.matrix(squid), plot=F),
                 regexp = "oxy_crit: Input must be an 'inspect' or data.frame object.")
  })

  test_that("oxy_crit - stops if you try to do oxygen~rate analysis with inspect object", {
    expect_error(oxy_crit(squid_insp, oxygen = 1, rate = 2, plot=F),
                 regexp = "oxy_crit: 'inspect' input detected. Rate~Oxygen analyses cannot be conducted with 'inspect' objects!")
  })

  test_that("oxy_crit - issues warning with inspect object and column inputs", {
    expect_warning(oxy_crit(squid_insp, time = 1, oxygen = 2, rate = NULL, plot=F),
                   regexp = "oxy_crit: 'inspect' input detected. Column inputs ignored. These will have been specified in 'inspect'.")
    expect_warning(oxy_crit(squid_insp, time = 1, oxygen = NULL, rate = NULL, plot=F),
                   regexp = "oxy_crit: 'inspect' input detected. Column inputs ignored. These will have been specified in 'inspect'.")
    expect_warning(oxy_crit(squid_insp, time = NULL, oxygen = NULL, rate = 1, plot=F),
                   regexp = "oxy_crit: 'inspect' input detected. Column inputs ignored. These will have been specified in 'inspect'.")
    expect_warning(oxy_crit(squid_insp, time = 2, oxygen = NULL, rate = 1, plot=F),
                   regexp = "oxy_crit: 'inspect' input detected. Column inputs ignored. These will have been specified in 'inspect'.")
  })

  test_that("oxy_crit - issues warning with inspect object containing multiple columns", {
    expect_warning(oxy_crit(urch_insp, plot=F),
                   regexp = "oxy_crit: Multiple columns of oxygen data found in 'inspect' input.")
  })

  test_that("oxy_crit - stops if method not recognised", {
    expect_error(oxy_crit(squid, method = "text", plot=F),
                 regexp = "oxy_crit: 'method' input not recognised.")
  })

  test_that("oxy_crit - stops if width out of range", {
    expect_error(oxy_crit(squid, width = 1000, plot=F),
                 regexp = "oxy_crit: 'width' input should be between 0.001 to 0.999, representing a proportion of the total data length.")
  })

  test_that("oxy_crit - stops if time and rate columns entered", {
    expect_error(oxy_crit(squid, time = 1, rate = 2, plot=F),
                 regexp = "oxy_crit: Inputs should be 'time' and 'oxygen' columns, or 'oxygen' and 'rate' columns.")
  })

  test_that("oxy_crit - stops if oxygen/rate/time columns conflict or are out of range", {
    expect_error(oxy_crit(squid, time = 1, oxygen = 3, plot=F),
                 regexp = "one or more column inputs are out of range of allowed data columns.")
    expect_error(oxy_crit(squid, time = 3, oxygen = 2, plot=F),
                 regexp = "one or more column inputs are out of range of allowed data columns.")
    expect_error(oxy_crit(squid, rate = 3, oxygen = 2, plot=F),
                 regexp = "one or more column inputs are out of range of allowed data columns.")

    expect_error(oxy_crit(squid, time = 1, oxygen = 1, plot=F),
                 regexp = "one or more column inputs conflicts with other inputs.")
    expect_error(oxy_crit(squid, rate = 2, oxygen = 2, plot=F),
                 regexp = "one or more column inputs conflicts with other inputs.")

    expect_error(oxy_crit(urchins.rd, time = 1:2, oxygen = 3, plot=F),
                 regexp = "cannot enter more than 1 column\\(s) with this input or this dataset.")
    expect_error(oxy_crit(urchins.rd, time = 3, oxygen = 1:2, plot=F),
                 regexp = "cannot enter more than 1 column\\(s) with this input or this dataset.")
    expect_error(oxy_crit(urchins.rd, rate = 3:4, oxygen = 2, plot=F),
                 regexp = "cannot enter more than 1 column\\(s) with this input or this dataset.")
  })

  test_that("oxy_crit - applies defaults of time=1, oxygen=2", {
    expect_is(oxy_crit(squid, plot=F),
              "oxy_crit")
    expect_equal(oxy_crit(squid, plot=F)$result.intercept,
                 oxy_crit(squid, time = 1, oxygen = 2, plot=F)$result.intercept)
  })

  test_that("oxy_crit - correctly identifies column name strings", {
    # single time and oxygen columns work without error
    expect_error(oxy_crit(squid, time = "Time", oxygen = "Oxygen", plot=F),
                 NA)
    # same as using numbers
    # Everything except the $call and $inputs should be the same
    expect_equal(oxy_crit(squid, time = "Time", oxygen = "Oxygen", plot=F)[3:10],
                 oxy_crit(squid, time = 1, oxygen = 2, plot=F)[3:10])

    # rate
    expect_error(oxy_crit(squid_rate, oxygen = "x", rate = "y", plot = F),
                 NA)
    expect_equal(oxy_crit(squid_rate, oxygen = "x", rate = "y", plot = F)[3:10],
                 oxy_crit(squid_rate, oxygen = 1, rate = 2, plot = F)[3:10])
  })

  test_that("oxy_crit - thin input correctly applied in Broken-Stick", {
    thin <- 5000
    res <- oxy_crit(squid.rd, method = "bsr", thin = thin, plot=F)
    expect_equal(nrow(res$results),
                 thin - 4) # always minus 4 for some reason....
    thin <- 2000
    res <- oxy_crit(squid.rd, method = "bsr", thin = thin, plot=F)
    expect_equal(nrow(res$results),
                 thin - 4)
  })

  test_that("oxy_crit - works with segmented method", {
    expect_error(oxy_crit(squid, method = "segmented", plot=F),
                 NA)
  })

  test_that("oxy_crit - test exact value outputs", {
    # results as of July 2021, in case we do something that changes them
    expect_equal(round(oxy_crit(squid, plot=F)$crit$crit.intercept, 4),
                 2.6003)
    expect_equal(round(oxy_crit(squid, plot=F)$crit$crit.midpoint, 4),
                 2.5944)
    expect_equal(round(oxy_crit(squid, method = "segmented", plot=F)$crit, 2),
                 2.60)
  })

  test_that("oxy_crit - works with oxygen~rate data", {
    expect_error(oxy_crit(squid_rate, oxygen = 1, rate = 2, plot = F),
                 regexp = NA)
    expect_equal(oxy_crit(squid_rate, oxygen = 1, rate = 2, plot = F)$crit$crit.intercept,
                 2.598025, tolerance = 6)
  })

  test_that("oxy_crit - works with multi column data frames", {
    expect_error(oxy_crit(urchins.rd, time = 1, oxygen = 12, plot = F), regexp = NA)
    expect_error(oxy_crit(urchins.rd, time = 11, oxygen = 16, plot = F), regexp = NA)
  })

  test_that("oxy_crit - plot default works", {
    expect_error(oxy_crit(squid, plot = T),
                 NA)
  })

  test_that("oxy_crit - oxy_crit S3 generics work with both oxygen/time and rate/oxygen inputs,
            and both methods, export, and panel options", {
              # oxygen
              pcr <- oxy_crit(squid, plot = F)
              expect_output(print(pcr))
              expect_output(summary(pcr))
              expect_is(summary(pcr, export = TRUE),
                        "data.frame")
              expect_error(plot(pcr), regexp = NA)
              expect_error(plot(pcr, panel = 1), regexp = NA)
              expect_error(plot(pcr, panel = 2), regexp = NA)
              expect_error(plot(pcr, panel = 3),
                           "plot.oxy_crit: 'panel' input should be 1 to 2 or 'NULL' for both.")
              # rate
              pcr <- oxy_crit(squid_rate, oxygen = 1, rate = 2, plot = F)
              expect_output(print(pcr))
              expect_output(summary(pcr))
              expect_is(summary(pcr, export = TRUE),
                        "data.frame")
              expect_error(plot(pcr), regexp = NA)
              expect_error(plot(pcr, panel = 1), regexp = NA)
              expect_error(plot(pcr, panel = 2), regexp = NA)
              expect_error(plot(pcr, panel = 3),
                           "plot.oxy_crit: 'panel' input should be 1 to 2 or 'NULL' for both.")
              # segmented method
              # oxygen
              pcr <- oxy_crit(squid, method = "segmented", plot = F)
              expect_output(print(pcr))
              expect_output(summary(pcr))
              expect_is(summary(pcr, export = TRUE),
                        "data.frame")
              expect_error(plot(pcr), regexp = NA)
              expect_error(plot(pcr, panel = 1), regexp = NA)
              expect_error(plot(pcr, panel = 2), regexp = NA)
              expect_error(plot(pcr, panel = 3),
                           "plot.oxy_crit: 'panel' input should be 1 to 2 or 'NULL' for both.")
              # rate
              pcr <- oxy_crit(squid_rate, method = "segmented", oxygen = 1, rate = 2, plot = F)
              expect_output(print(pcr))
              expect_output(summary(pcr))
              expect_is(summary(pcr, export = TRUE),
                        "data.frame")
              expect_error(plot(pcr), regexp = NA)
              expect_error(plot(pcr, panel = 1), regexp = NA)
              expect_error(plot(pcr, panel = 2), regexp = NA)
              expect_error(plot(pcr, panel = 3),
                           "plot.oxy_crit: 'panel' input should be 1 to 2 or 'NULL' for both.")
            })

  test_that("oxy_crit - mean S3 returns message", {
    expect_message(mean(oxy_crit(squid, plot = F)),
                 "oxy_crit: mean\\() is not available for 'oxy_crit' objects.")
  })

  # This fails on R CMD CHK...
  # Pretty sure not allowed on CRAN
  # May also fail on Github
  # https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  # test_that("oxy_crit - parallel code works", {
  #   skip_on_cran()
  #   expect_error(oxy_crit(squid, parallel = T, plot=F),
  #                regexp = NA)
  # })

  test_that("oxy_crit - plot defaults are correctly restored", {

    # reset plotting first
    dev.off()
    # save par before
    parb4 <- par(no.readonly = TRUE)
    # now use a fn with plot
    suppressWarnings(oxy_crit(squid, plot = F))
    # save after
    paraft <- par(no.readonly = TRUE)
    # mai is something changed from the default,
    # so if par settings not restored properly this should fail
    expect_identical(parb4$mai,
                     paraft$mai)

  })

})

