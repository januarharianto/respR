# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-calc_rate.int.R")
# covr::file_coverage("R/calc_rate.int.R", "tests/testthat/test-calc_rate.int.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)
# covr::report(covr::package_coverage())

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
  dt <- intermittent.rd %>%
    subset_data(from = 1) # removes first value at 0 time because of annoying messages during adjustments
  dt.insp <- inspect(dt, plot = F)
  sts <- c(1,2100,3899) # different from help file because first row removed above
  ens.actual <- c(2099,3898,4830) #actual ends including flush
  ens.measure <- c(1899,3549,4830)  # ends excluding flush

  # regular reps - 10 reps from this dataset
  dt.reg.insp <- subset_data(zeb_intermittent.rd,
                             from = 5840,
                             to = 5840 + 6599,
                             by = "row") %>%
    inspect(legend = F, plot = F)

  # x input tests --------------------------------------------------

  # stops if x not df or inspect obj
  test_that("calc_rate.int - stops with wrong 'x' inputs", {
    expect_error(calc_rate.int(dt[[1]],
                               starts = sts,
                               plot = F),
                 "calc_rate.int: Input must be a 'data.frame' or 'inspect' object.")
  })

  test_that("calc_rate.int - stops with wrong 'x' inputs", {
    expect_error(calc_rate.int(inspect.ft(dt, plot = F),
                               starts = sts,
                               plot = F),
                 "calc_rate.int: Input must be a 'data.frame' or 'inspect' object.")
  })

  test_that("calc_rate.int - stops with wrong 'x' inputs", {
    expect_error(calc_rate.int(12,
                               starts = sts,
                               plot = F),
                 "calc_rate.int: Input must be a 'data.frame' or 'inspect' object.")
  })

  # accepts df and inspect objs
  test_that("calc_rate.int - accepts 'data.frame' 'x' inputs", {
    expect_error(calc_rate.int(dt,
                               starts = sts,
                               plot = F),
                 NA)
  })

  test_that("calc_rate.int - accepts 'inspect' 'x' inputs", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               plot = F),
                 NA)
  })

  test_that("calc_rate.int - correctly extracts dataframe from 'inspect' objects", {
    cr.int <- calc_rate.int(dt.insp,
                            starts = sts,
                            plot = F)
    expect_identical(cr.int$dataframe,
                     dt.insp$dataframe)
  })

  test_that("calc_rate.int - message with multicolumn 'x' inputs", {
    expect_message(calc_rate.int(sardine.rd,
                                 starts = sts,
                                 plot = F),
                   "calc_rate.int: Multi-column dataset detected in input. Selecting first two columns by default.")
  })


  # starts input tests -----------------------------------------------------

  # required, numeric, integer, within df row range
  test_that("calc_rate.int - stops with wrong 'starts' inputs", {
    # NULL
    expect_error(calc_rate.int(dt.insp,
                               starts = NULL,
                               plot = F),
                 "calc_rate.int: 'starts' - input is required.")
    # Non-integer(s)
    expect_error(calc_rate.int(dt.insp,
                               starts = c(1, 100.1, 500),
                               plot = F),
                 "calc_rate.int: 'starts' - one or more inputs are not integers.")
    # Outside range
    expect_error(calc_rate.int(dt.insp,
                               starts = c(1, 100, 5000),
                               plot = F),
                 "calc_rate.int: 'starts' - one or more inputs are outside the range of allowed values.")
    expect_error(calc_rate.int(dt.insp,
                               starts = c(0, 100, 5000),
                               plot = F),
                 "calc_rate.int: 'starts' - one or more inputs are outside the range of allowed values.")
    expect_error(calc_rate.int(dt.insp,
                               starts = c(0, 100, 5000),
                               by = "time",
                               plot = F),
                 "calc_rate.int: 'starts' - one or more inputs are outside the range of allowed values.")
  })

  # Accepts multiple and single inputs
  test_that("calc_rate.int - accepts correct 'starts' inputs", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               plot = F),
                 NA)
    expect_error(calc_rate.int(dt.insp,
                               starts = 1000,
                               plot = F),
                 NA)
  })


  # 'wait' tests ------------------------------------------------------------

  test_that("calc_rate.int - stops if 'wait' input malformed", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               wait = "text",
                               plot = F),
                 "calc_rate.int: 'wait' - input is not numeric.")
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               wait = 900000,
                               plot = F),
                 "calc_rate.int: 'wait' - one or more inputs are outside the range of allowed values.")
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               wait = 1:2,
                               plot = F),
                 "calc_rate.int: For a vector input 'wait' should be the same length as 'starts'.")
  })

  test_that("calc_rate.int - 'wait' accepts single and multiple inputs", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               wait = 200,
                               plot = F),
                 NA)
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               wait = 1:3,
                               plot = F),
                 NA)
  })




  # 'measure' tests ---------------------------------------------------------

  test_that("calc_rate.int - stops if 'measure' input malformed", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               measure = "text",
                               plot = F),
                 "calc_rate.int: 'measure' - input is not numeric.")
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               measure = 900000,
                               plot = F),
                 "calc_rate.int: 'measure' - one or more inputs are outside the range of allowed values.")
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               measure = 1:2,
                               plot = F),
                 "calc_rate.int: For a vector input 'measure' should be the same length as 'starts'.")
  })

  test_that("calc_rate.int - 'measure' accepts single and multiple inputs", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               measure = 200,
                               plot = F),
                 NA)
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               measure = c(200, 400, 600),
                               plot = F),
                 NA)
  })


  # 'by' tests --------------------------------------------------------------

  # Stops if you try to use "oxygen"
  test_that("calc_rate.int - stops with by = 'oxygen'", {
    expect_error(calc_rate.int(dt.insp,
                               starts = sts,
                               by = "oxygen",
                               plot = F),
                 "calc_rate.int: 'by' input not valid or not recognised.")
  })


  # Expected results ---------------------------------------
  #
  # Since it's essentially running calc_rate we use it to get results to test against

  # by = "time" tests -------------------------------------------------------

  test_that("calc_rate.int - expected results with by = 'time' over whole replicate", {
    # Whole replicate
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts,
                        to = ens.actual,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' using 'wait' and 'measure'", {
    # Part of replicate
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts + 500,
                        to = sts + 1000,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = 500,
                                measure = 500,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' using 'wait' only", {
    # Part of replicate - with ends not specified
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts + 500,
                        to = ens.actual,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = 500,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' over whole replicates of regularly spaced reps", {
    # Regular replicates - Whole replicate
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = 5839 + seq(1, 5941, 660),
                        to = 5839 + seq(660, 6600, 660),
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' using 'wait' and 'measure' and regularly spaced reps", {
    # Regular replicates - part of
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = 5839 + seq(1, 5941, 660) + 120,
                        to = 5839 + seq(1, 5941, 660) + 480,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                wait = 120,
                                measure = 360,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' and regularly spaced reps with 'measure' not specified", {
    # Regular replicates - part of
    # ends not specified
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = 5839 + seq(1, 5941, 660) + 120,
                        to = 5839 + seq(1, 5941, 660) + 659,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                wait = 120,
                                #to = 480,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' and regularly spaced reps with 'wait' not specified", {
    # Regular replicates - part of
    # ends not specified
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = 5839 + seq(1, 5941, 660),
                        to = 5839 + seq(1, 5941, 660) + 480,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                #wait = 120,
                                measure = 480,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'time' using vectors for 'wait' and 'measure' and irregularly spaced reps", {
    # IRREGULAR replicates
    # Both vectors
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts + c(200, 300, 400),
                        to = ens.measure - c(450, 350, 250),
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = c(200, 300, 400),
                                measure = c(1248, 799, 281),
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])

    #calc_rate object
    #wait vector, measure not
    cr.obj <- calc_rate(dt.insp,
                        from = sts + c(200, 300, 400),
                        to = sts + c(200, 300, 400) + 600,
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = c(200, 300, 400),
                                measure = 600,
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])

    #calc_rate object
    #measure vector, wait not
    cr.obj <- calc_rate(dt.insp,
                        from = sts,
                        to = sts + c(600, 700, 800),
                        by = "time",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = NULL,
                                measure = c(600, 700, 800),
                                by = "time",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })





  # by = "row" tests -------------------------------------------------------

  test_that("calc_rate.int - expected results with by = 'row' over whole replicate", {
    # Whole replicate
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts,
                        to = ens.actual,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' using 'wait' and 'measure'", {
    # Part of replicate
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts + 500,
                        to = sts + 999,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = 500,
                                measure = 500,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' using 'wait' only", {
    # Part of replicate - with ends not specified
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts + 500,
                        to = ens.actual,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = 500,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' over whole replicates of regularly spaced reps", {
    # Regular replicates - Whole replicate
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp$da,
                        from = seq(1, 6600, 660),
                        to = seq(660, 6600, 660),
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' using 'wait' and 'measure' and regularly spaced reps", {
    # Regular replicates - part of
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = seq(1, 6600, 660) + 120,
                        to = seq(1, 6600, 660) + 120 + 359,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                wait = 120,
                                measure = 360,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' and regularly spaced reps with 'measure' not specified", {
    # Regular replicates - part of
    # ends not specified
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = seq(1, 6600, 660) + 120,
                        to = seq(1, 6600, 660) + 120 + 539,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                wait = 120,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' and regularly spaced reps with 'wait' not specified", {
    # Regular replicates - part of
    # ends not specified
    #calc_rate object
    cr.obj <- calc_rate(dt.reg.insp,
                        from = seq(1, 6600, 660),
                        to = seq(1, 6600, 660) + 479,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.reg.insp,
                                starts = 660,
                                measure = 480,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })

  test_that("calc_rate.int - expected results with by = 'row' using vectors for 'wait' and 'measure' and irregularly spaced reps", {
    # IRREGULAR replicates
    # Both vectors
    #calc_rate object
    cr.obj <- calc_rate(dt.insp,
                        from = sts + c(200, 300, 400),
                        to = ens.measure - c(450, 350, 250),
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = c(200, 300, 400),
                                measure = c(1249, 800, 282),
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])

    #calc_rate object
    #wait vector, measure not
    cr.obj <- calc_rate(dt.insp,
                        from = sts + c(200, 300, 400),
                        to = sts + c(200, 300, 400) + 600,
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = c(200, 300, 400),
                                measure = 601,
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])

    #calc_rate object
    #measure vector, wait not
    cr.obj <- calc_rate(dt.insp,
                        from = sts,
                        to = sts + c(600, 700, 800),
                        by = "row",
                        plot = F)
    #calc_rate.int object
    cr.int.obj <- calc_rate.int(dt.insp,
                                starts = sts,
                                wait = NULL,
                                measure = c(601, 701, 801),
                                by = "row",
                                plot = F)
    # test
    expect_equal(cr.obj$summary[,3:ncol(cr.obj$summary)],
                 cr.int.obj$summary[,3:ncol(cr.obj$summary)])
  })


  # General tests -----------------------------------------------------------

  test_that("calc_rate.int - outputs object of class calc_rate.int", {
    cr.int <- calc_rate.int(dt.insp,
                            starts = sts,
                            plot = F)
    expect_is(cr.int,
              "calc_rate.int")
  })

  test_that("calc_rate.int - S3 generics work", {
    cr.int <- calc_rate.int(dt.insp,
                            starts = sts,
                            plot = F)
    expect_output(print(cr.int))
    expect_output(summary(cr.int))
    expect_output(plot(cr.int))
    expect_output(mean(cr.int))

    # multiple rates and 'pos'
    expect_output(print(cr.int, pos = 2))
    expect_error(print(cr.int, pos = 2:3),
                 "print.calc_rate.int: 'pos' must be a single value. To examine multiple results use summary().")
    expect_error(print(cr.int, pos = 30),
                 "print.calc_rate.int: Invalid 'pos' input: only 3 replicates found.")

    expect_output(summary(cr.int, pos = 2:3))
    expect_error(summary(cr.int, pos = 40),
                 "summary.calc_rate.int: Invalid 'pos' input: only 3 replicates found.")
    expect_is(summary(cr.int, pos = 2:3, export = TRUE),
              "data.frame")

    expect_output(mean(cr.int, pos = 2:3))
    expect_error(mean(cr.int, pos = 40),
                 "mean.calc_rate.int: Invalid 'pos' input: only 3 replicates found.")
    expect_is(mean(cr.int, pos = 2:3, export = TRUE),
              "numeric")
    expect_equal(mean(cr.int, pos = 2:3, export = TRUE),
                 mean(cr.int$rate[2:3]))

    # pos default applied
    expect_output(plot(cr.int, pos = NULL))
    expect_output(plot(cr.int, pos = 1))
    expect_output(plot(cr.int, pos = 3))
    expect_error(plot(cr.int, pos = 50),
                 "plot.calc_rate.int: Invalid 'pos' input: only 3 replicates found.")

    # plot types produce output
    expect_output(plot(cr.int, type = "rep"))
    expect_output(plot(cr.int, type = "rep", pos = 2:3))
    expect_output(plot(cr.int, type = "full"))
    expect_output(plot(cr.int, type = "full", pos = 2:3))
    expect_output(plot(cr.int, type = "cr"))
    expect_output(plot(cr.int, type = "cr", pos = 2:3))
    expect_error(plot(cr.int, type = "test"),
                 "plot.calc_rate.int: 'type' input not recognised.")
  })

  # Expected results with adjust_rate ---------------------------------------

  test_that("calc_rate.int - works as expected with adjust_rate method = 'value'", {

    cr.int <- calc_rate.int(dt,
                            starts = sts,
                            ends = ens.actual,
                            plot = F)
    # "value" method
    by <- -0.0001
    cr.int.adj <- adjust_rate(cr.int, by = by, method = "value")

    expect_equal(cr.int$rate - by,
                 cr.int.adj$rate.adjusted)
  })

  test_that("calc_rate.int - works as expected with adjust_rate method = 'mean'", {

    cr.int <- calc_rate.int(dt,
                            starts = sts,
                            ends = ens.actual,
                            plot = F)
    # "mean" method
    by <- c(-0.0001, -0.00008, -0.00005)
    cr.int.adj <- adjust_rate(cr.int, by = by, method = "mean")

    expect_equal(cr.int$rate - mean(by),
                 cr.int.adj$rate.adjusted)
  })

  test_that("calc_rate.int - works as expected with adjust_rate method = 'paired'", {

    cr.int <- calc_rate.int(dt,
                            starts = sts,
                            ends = ens.actual,
                            plot = F)
    # "paired" method
    by <- c(-0.0001, -0.00008, -0.00005)
    cr.int.adj <- adjust_rate(cr.int, by = by, method = "paired")

    # cr.int.adj$summary
    expect_equal(cr.int$rate - by,
                 cr.int.adj$rate.adjusted)
  })

  test_that("calc_rate.int - works as expected with adjust_rate method = 'concurrent'", {

    cr.int <- calc_rate.int(dt,
                            starts = sts,
                            ends = ens.actual,
                            plot = F)
    # "concurrent" method
    # subset these data to the same length
    by <- background_con.rd %>%
      subset_data(1, 4830, "time", quiet = TRUE) %>%
      calc_rate.bg(plot = F)

    cr.int.adj <- adjust_rate(cr.int, by = by, method = "concurrent")

    # cr.int.adj$summary
    rt <- calc_rate(by$dataframe, from = cr.int$summary$row, to = cr.int$summary$endrow, by = "row")$rate
    expect_equal(cr.int$rate - rt,
                 cr.int.adj$rate.adjusted)
  })

  test_that("calc_rate.int - works as expected with adjust_rate method = 'linear'", {

    cr.int <- calc_rate.int(dt,
                            starts = sts,
                            ends = ens.actual,
                            plot = F)
    # "linear" method
    # subset these data to the same length
    by1 <- background_con.rd %>%
      subset_data(1, 800, "time") %>%
      calc_rate.bg()
    by2 <- background_exp.rd %>%
      subset_data(5000, 15000, "time") %>%
      calc_rate.bg()

    cr.int.adj <- adjust_rate(cr.int, by = by1, by2 = by2, method = "linear")

    # runs without error
    expect_error(adjust_rate(cr.int, by = by1, by2 = by2, method = "linear"),
                 NA)

    # cr.int.adj$summary
    # cr.int.adj$summary$adjustment
    # For now just test the known values in case they change in future
    expect_equal(c(-0.00006356925, -0.00010351944, -0.00013150917),
                 cr.int.adj$summary$adjustment)
  })


  test_that("calc_rate.int - works as expected with adjust_rate method = 'exponential'", {

    cr.int <- calc_rate.int(dt,
                            starts = sts,
                            ends = ens.actual,
                            plot = F)
    # "exponential" method
    # subset these data to the same length
    by1 <- background_con.rd %>%
      subset_data(1, 800, "time") %>%
      calc_rate.bg()
    by2 <- background_exp.rd %>%
      subset_data(5000, 15000, "time") %>%
      calc_rate.bg()

    cr.int.adj <- adjust_rate(cr.int, by = by1, by2 = by2, method = "exponential")

    # runs without error
    expect_error(adjust_rate(cr.int, by = by1, by2 = by2, method = "exponential"),
                 NA)

    # cr.int.adj$summary
    # cr.int.adj$summary$adjustment
    # For now just test the known values in case they change in future
    expect_equal(c(-0.00005597288, -0.00007733651, -0.00009699645),
                 cr.int.adj$summary$adjustment)
  })

}) ## end capture output


