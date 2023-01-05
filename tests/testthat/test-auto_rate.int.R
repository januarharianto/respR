# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-auto_rate.int.R")
# covr::file_coverage("R/auto_rate.int.R", "tests/testthat/test-auto_rate.int.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)
# covr::report(covr::package_coverage())

capture.output({  ## stops printing outputs on assigning
  skip_on_cran()

  # create testing objects
  suppressWarnings({
    # in secs
    dt.sec <- intermittent.rd |>
      subset_data(from = 1) # removes first value at 0 time because of annoying messages during adjustments
    dt.sec.insp <- inspect(dt.sec, plot = F)

    # in mins - 2 dec places
    dt.min.2 <- intermittent.rd |>
      subset_data(from = 1)
    dt.min.2[[1]] <- round(dt.min.2[[1]]/60, 2)
    dt.min.2.insp <- inspect(dt.min.2, plot = F)

    # in mins - 1 dec places
    dt.min.1 <- intermittent.rd |>
      subset_data(from = 1)
    dt.min.1[[1]] <- round(dt.min.1[[1]]/60, 1)
    dt.min.1.insp <- inspect(dt.min.1, plot = F)

    # in hrs - 2 dec places
    dt.hr <- intermittent.rd |>
      subset_data(from = 1)
    dt.hr[[1]] <- round(dt.hr[[1]]/60/60, 3)
    dt.hr.insp <- inspect(dt.hr, plot = F)


    sts <- c(1,2100,3899) # different from help file because first row removed above
    sts.min <- c(0.02,35,65) # starts in minutes
    sts.hr <- round(c(0.02,35,65)/60, 3) # starts in hr
    ens.actual <- c(2099,3898,4830) #actual ends including flush
    ens.measure <- c(1899,3549,4830)  # ends excluding flush

    # regular reps - 10 reps from this dataset
    dt.reg.insp <- subset_data(zeb_intermittent.rd,
                               from = 5840,
                               to = 5840 + 6599,
                               by = "row") |>
      inspect(legend = F, plot = F)
  })

  # x input tests --------------------------------------------------

  # stops if x not df or inspect obj
  test_that("auto_rate.int - stops with wrong 'x' inputs", {
    expect_error(auto_rate.int(dt.sec[[1]],
                               starts = sts,
                               width = 100,
                               plot = F),
                 "auto_rate.int: Input must be a 'data.frame' or 'inspect' object.")

    expect_error(auto_rate.int(inspect.ft(dt.sec, plot = F),
                               starts = sts,
                               width = 100,
                               plot = F),
                 "auto_rate.int: Input must be a 'data.frame' or 'inspect' object.")

    expect_error(auto_rate.int(12,
                               starts = sts,
                               width = 100,
                               plot = F),
                 "auto_rate.int: Input must be a 'data.frame' or 'inspect' object.")
  })

  # accepts df and inspect objs
  test_that("auto_rate.int - accepts 'data.frame' 'x' inputs", {
    expect_error(auto_rate.int(dt.sec,
                               starts = sts,
                               width = 100,
                               plot = F),
                 NA)
  })

  test_that("auto_rate.int - accepts 'inspect' 'x' inputs", {
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               width = 100,
                               plot = F),
                 NA)
  })

  test_that("auto_rate.int - correctly extracts dataframe from 'inspect' objects", {
    ar.int <- auto_rate.int(dt.sec.insp,
                            starts = sts,
                            width = 100,
                            plot = F)
    expect_identical(ar.int$dataframe,
                     dt.sec.insp$dataframe)
    expect_is(ar.int$dataframe,
              "data.table")
  })

  test_that("auto_rate.int - message with multicolumn 'x' inputs", {
    expect_message(auto_rate.int(sardine.rd,
                                 starts = sts,
                                 width = 100,
                                 plot = F),
                   "auto_rate.int: Multi-column dataset detected in input. Selecting first two columns by default.")
  })


  # starts input tests -----------------------------------------------------

  # required, numeric, integer, within df row range
  test_that("auto_rate.int - stops with wrong 'starts' inputs", {
    # NULL
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = NULL,
                               width = 100,
                               plot = F),
                 "auto_rate.int: 'starts' - input is required.")
    # Non-integer(s)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = c(1, 100.1, 500),
                               width = 100,
                               plot = F),
                 "auto_rate.int: 'starts' - one or more inputs are not integers.")
  })

  # Accepts multiple and single inputs
  test_that("auto_rate.int - accepts correct 'starts' inputs", {
    skip_on_cran()
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = 1000,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               width = 100,
                               by = "time",
                               plot = F),
                 NA)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = 1000,
                               width = 100,
                               by = "time",
                               plot = F),
                 NA)
  })

  # wait input tests -----------------------------------------------------

  # required, numeric, integer, within df row range
  test_that("auto_rate.int - stops with wrong 'wait' inputs", {
    # NULL - is ok
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = NULL,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
    # string
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = "text",
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'wait' - input is not numeric.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = "text",
                               width = 100,
                               by = "time",
                               plot = F),
                 "auto_rate.int: 'wait' - input is not numeric.")

    # Non-integer(s) for row
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 100.1,
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'wait' - one or more inputs are not integers.")
    # Non-integer(s) for time is ok
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 100.1,
                               width = 100,
                               by = "time",
                               plot = F),
                 NA)

    # Outside range
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = c(300,400, 5000),
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'wait' - one or more inputs are outside the range of allowed values.")
    # by = "time" - there is no range test

    # Wrong length
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 100:103,
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: For a vector input 'wait' should be the same length as 'starts'.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 100:103,
                               width = 100,
                               by = "time",
                               plot = F),
                 "auto_rate.int: For a vector input 'wait' should be the same length as 'starts'.")
  })

  # Accepts multiple and single inputs
  test_that("auto_rate.int - accepts correct length 'wait' inputs", {
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50:52,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
  })

  # if wait = NULL all start rows should be equal to subset start rows
  test_that("auto_rate.int - properly parses 'wait = NULL'", {
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = NULL,
                                      measure = 1000,
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 1,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    expect_equal(sapply(ar.int.obj.3reps$subsets, function(z) z[[1,1]]),
                 ar.int.obj.3reps$summary$row)
    # ends should be this plus width
    expect_equal(sapply(ar.int.obj.3reps$subsets, function(z) z[[1,1]])+500-1,
                 ar.int.obj.3reps$summary$endrow)
    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses single 'wait' values", {
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = 100,
                                      measure = 1000,
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 1,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    expect_equal(sapply(ar.int.obj.3reps$subsets, function(z) z[[1,1]]),
                 ar.int.obj.3reps$summary$row - 100)
    # ends should be this plus width
    expect_equal(sapply(ar.int.obj.3reps$subsets, function(z) z[[1,1]])+500-1,
                 ar.int.obj.3reps$summary$endrow - 100)
    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses multiple 'wait' values", {
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = c(100, 120, 140),
                                      measure = 1000,
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 1,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    expect_equal(sapply(ar.int.obj.3reps$subsets, function(z) z[[1,1]]),
                 ar.int.obj.3reps$summary$row - c(100, 120, 140))
    # ends should be this plus width
    expect_equal(sapply(ar.int.obj.3reps$subsets, function(z) z[[1,1]])+500-1,
                 ar.int.obj.3reps$summary$endrow - c(100, 120, 140))
    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })


  # measure input tests -----------------------------------------------------

  test_that("auto_rate.int - message with NULL 'measure' input", {
    expect_message(auto_rate.int(dt.sec.insp,
                                 starts = sts,
                                 wait = 50,
                                 measure = NULL,
                                 width = 100,
                                 plot = F),
                   "auto_rate.int: The `measure` input is NULL. Calculating rate to the end of the replicate.")
  })

  # required, numeric, integer, within df row range
  test_that("auto_rate.int - stops with wrong 'measure' inputs", {

    # string
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = "text",
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'measure' - input is not numeric.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = "text",
                               width = 100,
                               by = "time",
                               plot = F),
                 "auto_rate.int: 'measure' - input is not numeric.")

    # Non-integer(s) for row
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 200.1,
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'measure' - one or more inputs are not integers.")
    # Non-integer(s) for time is ok
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 200.1,
                               width = 100,
                               by = "time",
                               plot = F),
                 NA)

    # Outside range
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = c(300,400, 5000),
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'measure' - one or more inputs are outside the range of allowed values.")
    # by = "time" - there is no range test

    # Wrong length
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 100:103,
                               width = 100,
                               by = "row",
                               plot = F),
                 "auto_rate.int: For a vector input 'measure' should be the same length as 'starts'.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 100:103,
                               width = 100,
                               by = "time",
                               plot = F),
                 "auto_rate.int: For a vector input 'measure' should be the same length as 'starts'.")
  })

  # Accepts multiple and single inputs
  test_that("auto_rate.int - accepts correct length 'measure' inputs", {
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500:502,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
  })

  test_that("auto_rate.int - properly parses 'measure = NULL'", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = NULL,
                                      measure = NULL,
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates of this width last endrow should be last row of data
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[2]-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[3]-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 3333)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses single 'measure' values", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = 100,
                                      measure = 1000,
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates with this measure last endrows should be starts + wait + measure
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[1]+100+1000-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[2]+100+1000-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 1335)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses multiple 'measure' values", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = 100,
                                      measure = c(800, 900, 1000),
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates with this measure last endrows should be starts + wait + measure
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[1]+100+800-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[2]+100+900-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 1035)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses multiple 'measure' values with multiple wait values", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = c(100, 150, 200),
                                      measure = c(800, 900, 1000),
                                      width = 500,
                                      by = "row",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates with this measure last endrows should be starts + wait + measure
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[1]+100+800-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[2]+150+900-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 935)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "row")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses 'measure = NULL' and by = 'time'", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = NULL,
                                      measure = NULL,
                                      width = 500,
                                      by = "time",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates of this width last endrow should be last row of data
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[2]-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[3]-1)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    # This is 1 less per rep than by = "row" prob for minor reasons
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 3330)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses single 'measure' values and by = 'time'", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = 100,
                                      measure = 1000,
                                      width = 500,
                                      by = "time",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates with this measure last endrows should be starts + wait + measure
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[1]+100+1000)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[2]+100+1000)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 1334)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses multiple 'measure' values and by = 'time'", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = 100,
                                      measure = c(800, 900, 1000),
                                      width = 500,
                                      by = "time",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates with this measure last endrows should be starts + wait + measure
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[1]+100+800)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[2]+100+900)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 1034)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })

  test_that("auto_rate.int - properly parses multiple 'measure' values with multiple wait values and by = 'time'", {
    skip_on_cran()
    # by row
    ar.int.obj.3reps <- auto_rate.int(x = dt.sec.insp,
                                      starts = sts,
                                      wait = c(100, 150, 200),
                                      measure = c(800, 900, 1000),
                                      width = 500,
                                      by = "time",
                                      method = "rolling",
                                      n = 10000,
                                      plot = F)
    #summary(ar.int.obj.3reps)
    # locs
    # If we calc all possible rates with this measure last endrows should be starts + wait + measure
    expect_equal(nrow(dt.sec.insp$dataframe),
                 ar.int.obj.3reps$summary$endrow[nrow(ar.int.obj.3reps$summary)])
    # last endrow of each rep should be last row of each rep
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 1], 1),
                 sts[1]+100+800)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 2], 1),
                 sts[2]+150+900)
    expect_equal(tail(ar.int.obj.3reps$summary$endrow[ar.int.obj.3reps$summary$rep == 3], 1),
                 nrow(dt.sec.insp$dataframe))
    # sould be this many result rows - just leaving this here in case this changes for some reason in future
    expect_equal(nrow(ar.int.obj.3reps$summary),
                 934)

    # check raw data matches for these rows
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$row,],
                      ar.int.obj.3reps$summary[,c(9,11)])
    expect_equivalent(dt.sec.insp$dataframe[ar.int.obj.3reps$summary$endrow,],
                      ar.int.obj.3reps$summary[,c(10,12)])
    # check rate calcs for these rows
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
    expect_equal(calc_rate(dt.sec.insp,
                           from = ar.int.obj.3reps$summary$row,
                           to = ar.int.obj.3reps$summary$endrow,
                           by = "time")$rate,
                 ar.int.obj.3reps$summary$rate)
  })


  # 'by' input tests -------------------------------------------------------

  test_that("auto_rate.int - accepts correct 'by' inputs", {
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "row",
                               plot = F),
                 NA)
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "time",
                               plot = F),
                 NA)
  })

  test_that("auto_rate.int - stops with incorrect 'by' inputs", {
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = NULL,
                               plot = F),
                 "auto_rate.int: 'by' input is required.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "oxygen",
                               plot = F),
                 "auto_rate.int: 'by' input not valid or not recognised.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "sometext",
                               plot = F),
                 "auto_rate.int: 'by' input not valid or not recognised.")
  })


  # 'width' input tests -----------------------------------------------------

  test_that("auto_rate.int - stops with NULL 'width' inputs", {
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = NULL,
                               by = "row",
                               plot = F),
                 "auto_rate.int: Please enter a 'width'. This should be in the 'by' input of number of rows.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = NULL,
                               by = "time",
                               plot = F),
                 "auto_rate.int: Please enter a 'width'. This should be in the 'by' input of a time duration in the correct units.")
  })

  test_that("auto_rate.int - stops with incorrect 'width' inputs", {
    # numeric
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = "text",
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'width' - input is not numeric.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = "text",
                               by = "time",
                               plot = F),
                 "auto_rate.int: 'width' - input is not numeric.")

    # integer of 1 if by = row
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 1,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'width' - one or more inputs are outside the range of allowed values.")
    # 0 to 1 non-integers ok with by = "row"
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 0.5,
                               by = "row",
                               plot = F),
                 NA)
    # non-integers ok with by = "time"
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100.1,
                               by = "time",
                               plot = F),
                 NA)

    # out of range
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 5000,
                               by = "row",
                               plot = F),
                 "auto_rate.int: 'width' - one or more inputs are outside the range of allowed values.")
    # no range test for by = "time"
  })

  test_that("auto_rate.int - message if 'width' input between 0 and 1 and by = 'time'", {
    expect_message(auto_rate.int(dt.hr,
                                 starts = sts.hr,
                                 wait = 0.01,
                                 measure = 0.4,
                                 width = 0.05,
                                 by = "time",
                                 plot = F),
                   "auto_rate.int: 'width' input is between 0 and 1. Check this value is what you intend.")
  })

  # 'n' input tests ---------------------------------------------------------

  test_that("auto_rate.int - stops with incorrect 'n' inputs", {
    # required
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "row",
                               n = NULL,
                               plot = F),
                 "auto_rate.int: 'n' - input is required.")
    # numeric
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "row",
                               n = "text",
                               plot = F),
                 "auto_rate.int: 'n' - input is not numeric.")
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "time",
                               n = "text",
                               plot = F),
                 "auto_rate.int: 'n' - input is not numeric.")

    # integer
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 100,
                               by = "row",
                               n = 1.1,
                               plot = F),
                 "auto_rate.int: 'n' - one or more inputs are not integers.")

    # single value
    expect_error(auto_rate.int(dt.sec.insp,
                               starts = sts,
                               wait = 50,
                               measure = 500,
                               width = 5000,
                               by = "time",
                               n = 1:2,
                               plot = F),
                 "auto_rate.int: 'n' - only 1 inputs allowed.")

  })


  # 'starts' correctly parsed -----------------------------------------------

  test_that("single 'starts' inputs are correctly parsed to correct number and locations of reps",{
    # by row
    # should be 5 reps
    ar.int.obj.5reps <- auto_rate.int(dt.sec.insp,
                                      starts = 1000,
                                      wait = 50,
                                      measure = 500,
                                      width = 100,
                                      by = "row",
                                      n = 1,
                                      plot = F)
    # n reps
    expect_equal(nrow(ar.int.obj.5reps$summary),
                 5)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[[1,1]]),
                 c(1,1001,2001,3001,4001))

    # by time
    # should be 5 reps
    ar.int.obj.5reps <- auto_rate.int(dt.sec.insp,
                                      starts = 1000,
                                      wait = 50,
                                      measure = 500,
                                      width = 100,
                                      by = "time",
                                      n = 1,
                                      plot = F)
    # n reps
    expect_equal(nrow(ar.int.obj.5reps$summary),
                 5)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[[1,1]]),
                 c(1,1001,2001,3001,4001))

    # by time - different units
    # should be 5 reps
    ar.int.obj.5reps <- auto_rate.int(dt.min.2.insp,
                                      starts = 1000/60,
                                      wait = 1,
                                      measure = 6,
                                      width = 1,
                                      by = "time",
                                      n = 1,
                                      plot = F)
    # n reps
    expect_equal(nrow(ar.int.obj.5reps$summary),
                 5)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[[1,1]]),
                 round(c(1,1001,2001,3001,4001)/60),2)

    # by time - different units and n greater than 1
    # should be 5 reps
    ar.int.obj.15reps <- auto_rate.int(dt.min.2.insp,
                                       starts = 1000/60,
                                       wait = 1,
                                       measure = 6,
                                       width = 1,
                                       by = "time",
                                       n = 3,
                                       plot = F)
    # n reps
    expect_equal(nrow(ar.int.obj.15reps$summary),
                 15)
    # locs - this should not change with different n
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[[1,1]]),
                 round(c(1,1001,2001,3001,4001)/60),2)

    # Regular reps
    # should be 10 reps
    ar.int.obj.10reps <- auto_rate.int(dt.reg.insp,
                                       starts = 660,
                                       wait = 50,
                                       measure = 300,
                                       width = 100,
                                       by = "row",
                                       n = 1,
                                       plot = F)
    # n reps
    expect_equal(nrow(ar.int.obj.10reps$summary),
                 10)
    # locs
    expect_equal(sapply(ar.int.obj.10reps$subsets, function(z) z[[1,1]]),
                 seq(5840, 5840 + 6599, 660))

    # Regular reps
    # should be 10 reps
    ar.int.obj.10reps <- auto_rate.int(dt.reg.insp,
                                       starts = 660,
                                       wait = 50,
                                       measure = 300,
                                       width = 100,
                                       by = "time",
                                       n = 1,
                                       plot = F)
    # n reps
    expect_equal(nrow(ar.int.obj.10reps$summary),
                 10)
    # locs
    expect_equal(sapply(ar.int.obj.10reps$subsets, function(z) z[[1,1]]),
                 seq(5840, 5840 + 6599, 660))

  })


  # rep ends correctly parsed -----------------------------------------------

  test_that("single 'starts' inputs are correctly parsed to correct number and locations of reps",{
    # by row
    # should be 5 reps
    ar.int.obj.5reps <- auto_rate.int(dt.sec.insp,
                                      starts = 1000,
                                      wait = 50,
                                      measure = 500,
                                      width = 100,
                                      by = "row",
                                      n = 1,
                                      plot = F)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[1,nrow(z)]),
                 c(1000,1000,1000,1000,830))

    # by time
    # should be 5 reps
    ar.int.obj.5reps <- auto_rate.int(dt.sec.insp,
                                      starts = 1000,
                                      wait = 50,
                                      measure = 500,
                                      width = 100,
                                      by = "time",
                                      n = 1,
                                      plot = F)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[1,nrow(z)]),
                 c(1000,1000,1000,1000,830))

    # by time - different units
    # should be 5 reps
    ar.int.obj.5reps <- auto_rate.int(dt.min.2.insp,
                                      starts = 1000/60,
                                      wait = 1,
                                      measure = 6,
                                      width = 1,
                                      by = "time",
                                      n = 1,
                                      plot = F)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[1,nrow(z)]),
                 c(1000,1000,1000,1000,830))

    # by time - different units and n greater than 1
    # should be 5 reps
    ar.int.obj.15reps <- auto_rate.int(dt.min.2.insp,
                                       starts = 1000/60,
                                       wait = 1,
                                       measure = 6,
                                       width = 1,
                                       by = "time",
                                       n = 3,
                                       plot = F)
    # locs
    expect_equal(sapply(ar.int.obj.5reps$subsets, function(z) z[1,nrow(z)]),
                 c(1000,1000,1000,1000,830))

    # Regular reps
    # should be 10 reps
    ar.int.obj.10reps <- auto_rate.int(dt.reg.insp,
                                       starts = 660,
                                       wait = 50,
                                       measure = 300,
                                       width = 100,
                                       by = "row",
                                       n = 1,
                                       plot = F)
    # locs
    expect_equal(sapply(ar.int.obj.10reps$subsets, function(z) z[1,nrow(z)]),
                 rep(660, 10))

    # Regular reps
    # should be 10 reps
    ar.int.obj.10reps <- auto_rate.int(dt.reg.insp,
                                       starts = 660,
                                       wait = 50,
                                       measure = 300,
                                       width = 100,
                                       by = "time",
                                       n = 1,
                                       plot = F)
    # locs
    expect_equal(sapply(ar.int.obj.10reps$subsets, function(z) z[1,nrow(z)]),
                 rep(660, 10))

  })




  # Expected results ---------------------------------------
  #
  # Since it's running auto_rate we use it to get results to test against

  # by = "time" tests -------------------------------------------------------

  test_that("auto_rate.int - expected results with method = 'linear'", {
    skip_on_cran()
    # Whole replicate
    #auto_rate object
    ar.obj.rep1 <- auto_rate(dt.sec[sts[1]:ens.actual[1],],
                             width = 400,
                             by = "row",
                             plot = F)
    ar.obj.rep2 <- auto_rate(dt.sec[sts[2]:ens.actual[2],],
                             width = 400,
                             by = "row",
                             plot = F)
    ar.obj.rep3 <- auto_rate(dt.sec[sts[3]:ens.actual[3],],
                             width = 400,
                             by = "row",
                             plot = F)
    #auto_rate.int object
    ar.int.obj <- auto_rate.int(dt.sec.insp,
                                starts = sts,
                                width = 400,
                                by = "row",
                                plot = F)
    # test
    # first rows of summary should match
    # except these columns: rep, rank, row, endrow
    # so just use final few columns
    expect_equal(ar.obj.rep1$summary[1,9:ncol(ar.obj.rep1$summary)],
                 ar.int.obj$summary[1,9:ncol(ar.int.obj$summary)])
    expect_equal(ar.obj.rep2$summary[1,9:ncol(ar.obj.rep2$summary)],
                 ar.int.obj$summary[2,9:ncol(ar.int.obj$summary)])
    expect_equal(ar.obj.rep3$summary[1,9:ncol(ar.obj.rep3$summary)],
                 ar.int.obj$summary[3,9:ncol(ar.int.obj$summary)])
  })

  test_that("auto_rate.int - expected results with method = 'lowest' and excluded flushes", {
    skip_on_cran()
    # Whole replicate
    #auto_rate object
    ar.obj.rep1 <- auto_rate(dt.sec[sts[1]:ens.measure[1],],
                             method = "lowest",
                             width = 300,
                             by = "time",
                             plot = F)
    ar.obj.rep2 <- auto_rate(dt.sec[sts[2]:ens.measure[2],],
                             method = "lowest",
                             width = 300,
                             by = "time",
                             plot = F)
    ar.obj.rep3 <- auto_rate(dt.sec[sts[3]:ens.measure[3],],
                             method = "lowest",
                             width = 300,
                             by = "time",
                             plot = F)
    #auto_rate.int object
    ar.int.obj <- auto_rate.int(dt.sec.insp,
                                starts = sts,
                                measure = ens.measure - sts +1,
                                method = "lowest",
                                width = 300,
                                by = "time",
                                plot = F)
    # test
    # first rows of summary should match
    # except these columns: rep, rank, row, endrow
    # so just use final few columns
    expect_equal(ar.obj.rep1$summary[1,9:ncol(ar.obj.rep1$summary)],
                 ar.int.obj$summary[1,9:ncol(ar.int.obj$summary)])
    expect_equal(ar.obj.rep2$summary[1,9:ncol(ar.obj.rep2$summary)],
                 ar.int.obj$summary[2,9:ncol(ar.int.obj$summary)])
    expect_equal(ar.obj.rep3$summary[1,9:ncol(ar.obj.rep3$summary)],
                 ar.int.obj$summary[3,9:ncol(ar.int.obj$summary)])
  })

  test_that("auto_rate.int - correctly populates $rep and $rank columns", {
    # We know this produces specific results, with different numbers of rresults in each rep
    # so we just test these 2 columns
    ar.int.obj <- auto_rate.int(dt.sec.insp,
                                starts = c(1, 2101, 3751),
                                measure = ens.measure - sts +1,
                                method = "linear",
                                width = 800,
                                by = "row",
                                n = 6,
                                plot = F)
    # test
    expect_equal(ar.int.obj$summary$rep,
                 c(rep(1, 6), rep (2, 5), rep(3, 3)))
    expect_equal(ar.int.obj$summary$rank,
                 c(1:6, 1:5, 1:3))
  })

  test_that("auto_rate.int - with replicates of regularly spaced reps", {
    # Regular replicates - Whole replicate
    #auto_rate object

    ar.int.obj <- auto_rate.int(dt.reg.insp,
                                starts = 660,
                                width = 400,
                                by = "row",
                                plot = F)

    #auto_rate.int object
    expect_error(auto_rate.int(dt.reg.insp,
                               starts = 660,
                               width = 400,
                               by = "row",
                               plot = F),
                 NA)
    # test
    # should be 10 rows
    expect_equal(nrow(ar.int.obj$summary),
                 10)
    # should be 20 rows when n = 2
    expect_equal(nrow(auto_rate.int(dt.reg.insp,
                                    starts = 660,
                                    width = 400,
                                    by = "row",
                                    n = 2,
                                    plot = F)$summary),
                 20)
  })



  # General tests -----------------------------------------------------------

  test_that("auto_rate.int - outputs object of class auto_rate.int", {
    skip_on_cran()
    ar.int <- auto_rate.int(dt.sec.insp,
                            starts = sts,
                            width = 400,
                            by = "row",
                            plot = F)
    expect_is(ar.int,
              "auto_rate.int")
  })

  test_that("auto_rate.int - S3 generics work", {
    skip_on_cran()
    ar.int <- auto_rate.int(dt.sec.insp,
                            starts = sts,
                            width = 400,
                            by = "row",
                            plot = F)
    expect_output(print(ar.int))
    expect_output(summary(ar.int))
    expect_output(plot(ar.int))
    expect_output(mean(ar.int))

    # multiple rates and 'pos'
    expect_output(print(ar.int, pos = 2))
    expect_error(print(ar.int, pos = 2:3),
                 "print.auto_rate.int: 'pos' must be a single value. To examine multiple results use summary().")
    expect_error(print(ar.int, pos = 30),
                 "print.auto_rate.int: Invalid 'pos' input: only 3 rates found.")

    expect_output(summary(ar.int, pos = 2:3))
    expect_error(summary(ar.int, pos = 40),
                 "summary.auto_rate.int: Invalid 'pos' input: only 3 rates found.")
    expect_is(summary(ar.int, pos = 2:3, export = TRUE),
              "data.frame")

    expect_output(mean(ar.int, pos = 2:3))
    expect_error(mean(ar.int, pos = 40),
                 "mean.auto_rate.int: Invalid 'pos' input: only 3 rates found.")
    expect_is(mean(ar.int, pos = 2:3, export = TRUE),
              "numeric")
    expect_equal(mean(ar.int, pos = 2:3, export = TRUE),
                 mean(ar.int$rate[2:3]))

    # pos default applied
    expect_output(plot(ar.int, pos = NULL))
    expect_output(plot(ar.int, pos = 1))
    expect_output(plot(ar.int, pos = 3))
    expect_error(plot(ar.int, pos = 50),
                 "plot.auto_rate.int: Invalid 'pos' input: only 3 rates found.")

    # plot types produce output
    expect_output(plot(ar.int, type = "rep"))
    expect_output(plot(ar.int, type = "rep", pos = 2:3))
    expect_output(plot(ar.int, type = "full"))
    expect_output(plot(ar.int, type = "full", pos = 2:3))
    expect_output(plot(ar.int, type = "ar"))
    expect_output(plot(ar.int, type = "ar", pos = 2:3))
    expect_error(plot(ar.int, type = "test"),
                 "plot.auto_rate.int: 'type' input not recognised.")
  })

  test_that("auto_rate.int - row numbers are output correctly", {

    #auto_rate.int object
    ar.int.obj <- auto_rate.int(x = dt.sec.insp,
                                starts = sts,
                                by = "time",
                                method = "linear",
                                width = 10,
                                wait = c(100,200,300),
                                n = 2,
                                measure = c(500,500,400),
                                plot = F,
                                type = "rep") |>
      summary()
    ar.int.obj$dataframe[ar.int.obj$summary$row,]
    # test - if we use row numbers in calc_rate results should match
    #calc_rate object
    cr.obj <- calc_rate(dt.sec.insp,
                        from = ar.int.obj$summary$row,
                        to = ar.int.obj$summary$endrow,
                        by = "row",
                        plot = F) |>
      summary()

    expect_equal(ar.int.obj$rate,
                 cr.obj$rate)
  })

  test_that("auto_rate.int - 'pos' inputs work", {
    ar.int <- auto_rate.int(x = dt.sec.insp,
                                starts = sts,
                                by = "time",
                                method = "linear",
                                width = 10,
                                wait = c(100,200,300),
                                n = 2,
                                measure = c(500,500,400),
                                plot = F,
                                type = "rep")

  # multiple rates and 'pos'
  expect_output(print(ar.int, pos = 2))
  expect_error(print(ar.int, pos = 2:3),
               "print.auto_rate.int: 'pos' must be a single value. To examine multiple results use summary().")
  expect_error(print(ar.int, pos = 30),
               "print.auto_rate.int: Invalid 'pos' input: only 5 rates found.")

  expect_output(summary(ar.int, pos = 2:3))
  expect_error(summary(ar.int, pos = 40),
               "summary.auto_rate.int: Invalid 'pos' input: only 5 rates found.")
  expect_is(summary(ar.int, pos = 2:3, export = TRUE),
            "data.frame")

  expect_output(mean(ar.int, pos = 2:3))
  expect_error(mean(ar.int, pos = 40),
               "mean.auto_rate.int: Invalid 'pos' input: only 5 rates found.")
  expect_is(mean(ar.int, pos = 2:3, export = TRUE),
            "numeric")
  expect_equal(mean(ar.int, pos = 2:3, export = TRUE),
               mean(ar.int$rate[2:3]))

  # pos default applied
  expect_output(plot(ar.int, pos = NULL))
  expect_output(plot(ar.int, pos = 1))
  expect_output(plot(ar.int, pos = 3))
  expect_error(plot(ar.int, pos = 50),
               "plot.auto_rate.int: Invalid 'pos' input: only 5 rates found.")

  # plot types produce output
  expect_output(plot(ar.int, type = "rep"))
  expect_output(plot(ar.int, type = "rep", pos = 2:3))
  expect_output(plot(ar.int, type = "full"))
  expect_output(plot(ar.int, type = "full", pos = 2:3))
  expect_output(plot(ar.int, type = "ar"))
  expect_output(plot(ar.int, type = "ar", pos = 2:3))
  expect_error(plot(ar.int, type = "test"),
               "plot.auto_rate.int: 'type' input not recognised.")
})


# Expected results with adjust_rate ---------------------------------------

test_that("auto_rate.int - works as expected with adjust_rate method = 'value'", {
  skip_on_cran()
  ar.int <- auto_rate.int(dt.sec,
                          starts = sts,
                          width = 100,
                          plot = F)
  # "value" method
  by <- -0.0001
  ar.int.adj <- adjust_rate(ar.int, by = by, method = "value")

  expect_equal(ar.int$rate - by,
               ar.int.adj$rate.adjusted)
})

test_that("auto_rate.int - works as expected with adjust_rate method = 'mean'", {
  skip_on_cran()
  ar.int <- auto_rate.int(dt.sec,
                          starts = sts,
                          width = 100,
                          plot = F)
  # "mean" method
  by <- c(-0.0001, -0.00008, -0.00005)
  ar.int.adj <- adjust_rate(ar.int, by = by, method = "mean")

  expect_equal(ar.int$rate - mean(by),
               ar.int.adj$rate.adjusted)
})

test_that("auto_rate.int - works as expected with adjust_rate method = 'paired'", {
  skip_on_cran()
  ar.int <- auto_rate.int(dt.sec,
                          starts = sts,
                          width = 100,
                          plot = F)
  # "paired" method
  by <- c(-0.0001, -0.00008, -0.00005)
  ar.int.adj <- adjust_rate(ar.int, by = by, method = "paired")

  # ar.int.adj$summary
  expect_equal(ar.int$rate - by,
               ar.int.adj$rate.adjusted)
})

test_that("auto_rate.int - works as expected with adjust_rate method = 'concurrent'", {
  skip_on_cran()
  ar.int <- auto_rate.int(dt.sec,
                          starts = sts,
                          width = 100,
                          plot = F)
  # "concurrent" method
  # subset these data to the same length
  by <- background_con.rd |>
    subset_data(1, 4830, "time", quiet = TRUE) |>
    calc_rate.bg(plot = F)

  ar.int.adj <- adjust_rate(ar.int, by = by, method = "concurrent")

  # ar.int.adj$summary
  rt <- calc_rate(by$dataframe, from = ar.int$summary$row, to = ar.int$summary$endrow, by = "row")$rate
  expect_equal(ar.int$rate - rt,
               ar.int.adj$rate.adjusted)
})

test_that("auto_rate.int - works as expected with adjust_rate method = 'linear'", {
  skip_on_cran()
  ar.int <- auto_rate.int(dt.sec,
                          starts = sts,
                          width = 100,
                          plot = F)
  #summary(ar.int)
  # "linear" method
  # subset these data to the same length
  by1 <- background_con.rd |>
    subset_data(1, 800, "time") |>
    calc_rate.bg()
  by2 <- background_exp.rd |>
    subset_data(5000, 15000, "time") |>
    calc_rate.bg()

  ar.int.adj <- adjust_rate(ar.int, by = by1, by2 = by2, method = "linear")

  # runs without error
  expect_error(adjust_rate(ar.int, by = by1, by2 = by2, method = "linear"),
               NA)

  # ar.int.adj$summary
  # ar.int.adj$summary$adjustment
  # For now just test the known values in case they change in future
  expect_equal(c(-0.00006102753, -0.00009158973, -0.00013826320),
               ar.int.adj$summary$adjustment)
})

test_that("auto_rate.int - works as expected with adjust_rate method = 'exponential'", {
  skip_on_cran()
  ar.int <- auto_rate.int(dt.sec,
                          starts = sts,
                          width = 100,
                          plot = F)
  # "exponential" method
  # subset these data to the same length
  by1 <- background_con.rd |>
    subset_data(1, 800, "time") |>
    calc_rate.bg()
  by2 <- background_exp.rd |>
    subset_data(5000, 15000, "time") |>
    calc_rate.bg()

  ar.int.adj <- adjust_rate(ar.int, by = by1, by2 = by2, method = "exponential")

  # runs without error
  expect_error(adjust_rate(ar.int, by = by1, by2 = by2, method = "exponential"),
               NA)

  # ar.int.adj$summary
  # ar.int.adj$summary$adjustment
  # For now just test the known values in case they change in future
  expect_equal(c(-0.00005483333, -0.00007021938, -0.00010244557),
               ar.int.adj$summary$adjustment)
})

}) ## end capture output


