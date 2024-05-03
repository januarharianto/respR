# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-subset_data.R")
# covr::file_coverage("R/subset_data.R", "tests/testthat/test-subset_data.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()

  test_that("subset_data - works with data.frame input", {
    # time
    sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "time")
    expect_is(sub,
              "data.frame")
    expect_is(sub,
              "data.table")
    expect_equal(nrow(sub),
                 1001)
    expect_equal(c(sardine.rd[2001,]),
                 c(sub[1,]))
    # row
    sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "row")
    expect_is(sub,
              "data.frame")
    expect_is(sub,
              "data.table")
    expect_equal(nrow(sub),
                 1001)
    expect_equal(c(sardine.rd[2000,]),
                 c(sub[1,]))

  })

  test_that("subset_data - test some exact values", {
    sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "time")
    expect_equal(sub$Oxygen[1],
                 93.8)
    expect_equal(tail(sub$Oxygen, 1),
                 93.1)
  })

  test_that("subset_data - works by row", {
    sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "row")
    expect_is(sub,
              "data.frame")
  })

  test_that("subset_data - works by O2", {
    sub <- subset_data(sardine.rd, from = 93, to = 92, by = "oxygen")
    expect_is(sub,
              "data.frame")
  })

  test_that("subset_data - works with variations of `by` input", {
    invisible({
      expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "Time"), regexp = NA)
      expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "T"), regexp = NA)
      expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "Row"), regexp = NA)
      expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "r"), regexp = NA)
      expect_error(subset_data(sardine.rd, from = 95, to = 94, by = "Oxygen"), regexp = NA)
      expect_error(subset_data(sardine.rd, from = 95, to = 94, by = "oxygen"), regexp = NA)
    })
  })

  test_that("subset_data - error with wrong by", {
    expect_error(subset_data(sardine.rd, by = "tttimmmeee"),
                 "subset_data: 'by' input not valid or not recognised.")
  })

  test_that("subset_data - output can be printed", {
    sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "time")
    expect_output(print(sub))
  })

  test_that("subset_data - works with inspect objects for each method", {
    urch <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 14:15))
    expect_error(subset_data(urch, from = 7.6, to = 7.4, by = "oxygen"),
                 regexp = NA)
    expect_error(subset_data(urch, from = 10, to = 30, by = "time"),
                 regexp = NA)
    expect_error(subset_data(urch, from = 70, to = 170, by = "row"),
                 regexp = NA)
  })

  test_that("subset_data - includes all columns when subsetting", {
    urch <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 10:15))
    expect_equal(ncol((subset_data(urch, from = 7.6, to = 7.4, by = "oxygen"))$dataframe),
                 7)
    expect_equal(ncol((subset_data(urch, from = 10, to = 30, by = "time"))$dataframe),
                 7)
    expect_equal(ncol((subset_data(urch, from = 70, to = 170, by = "row"))$dataframe),
                 7)
  })

  test_that("subset_data - works with inspect.ft objects", {
    fthr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                        delta.oxy =  10:12, plot = FALSE))

    expect_error(subset_data(fthr, from = -3, to = -5, by = "oxygen"),
                 regexp = NA)
    expect_output(subset_data(fthr, from = -3, to = -5, by = "oxygen", quiet = FALSE),
                  regexp = "Subset data:")
    expect_equal(nrow(subset_data(fthr, from = -3, to = -5, by = "oxygen")$dataframe),
                 553)
    expect_error(subset_data(fthr, from = 10, to = 20, by = "time"),
                 regexp = NA)
    expect_output(subset_data(fthr, from = 10, to = 20, by = "time", quiet = FALSE),
                  regexp = "Subset data:")
    expect_equal(nrow(subset_data(fthr, from = 10, to = 20, by = "time")$dataframe),
                 601)
    expect_error(subset_data(fthr, from = 70, to = 170, by = "row"),
                 regexp = NA)
    expect_output(subset_data(fthr, from = 70, to = 170, by = "row", quiet = FALSE),
                  regexp = "Subset data:")
    expect_equal(nrow(subset_data(fthr, from = 70, to = 170, by = "row")$dataframe),
                 101)
  })

  test_that("subset_data - class retained in output", {
    input <- sardine.rd
    expect_is(subset_data(input, from = 7.6, to = 7.4, by = "oxygen"),
              "data.frame")
    input <- inspect(sardine.rd)
    expect_is(subset_data(input, from = 7.6, to = 7.4, by = "oxygen"),
              "inspect")
    input <- inspect.ft(sardine.rd)
    expect_is(subset_data(input, from = 7.6, to = 7.4, by = "oxygen"),
              "inspect.ft")
  })

  test_that("subset_data - stops if 'from' or 'to' malformed", {
    fthr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                        delta.oxy =  10:12, plot = FALSE))

    expect_error(subset_data(fthr, from = 30, to = 20, by = "time"),
                 regexp = "subset_data: 'to' - one or more inputs are outside the range of allowed values.")
    expect_error(subset_data(fthr, from = 80, to = 20, by = "time"),
                 regexp = "subset_data: 'from' - one or more inputs are outside the range of allowed values.")
    expect_error(subset_data(fthr, from = 21:22, to = 30, by = "time"),
                 regexp = "subset_data: 'from' - only 1 inputs allowed.")

    expect_error(subset_data(fthr, from = 3000.2, to = 2000, by = "row"),
                 regexp = "subset_data: 'from' - one or more inputs are not integers.")
    expect_error(subset_data(fthr, from = 8000, to = 2000, by = "row"),
                 regexp = "subset_data: 'from' - one or more inputs are outside the range of allowed values.")
    expect_error(subset_data(fthr, from = 2001:2002, to = 3000, by = "row"),
                 regexp = "subset_data: 'from' - only 1 inputs allowed.")

    expect_error(subset_data(fthr, from = 2:3, to = 3800, by = "oxygen"),
                 regexp = "subset_data: 'from' - only 1 inputs allowed.")
  })

  # subset `inspect` and `inspect.ft`  objects work as expected in subsequent functions
  test_that("subset_data - `inspect` objects work as expected in subsequent functions.", {
    urch <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 4))
    sub <- subset_data(urch, from = 7.6, to = 7.4, by = "oxygen")
    expect_error(calc_rate(sub),
                 regexp = NA)
    # rate from sub should be same as if sub was done in fn
    expect_equal(calc_rate(urch, from = 7.6, to = 7.4, by = "oxygen")$rate,
                 calc_rate(sub)$rate)

    urch <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 4))
    sub <- subset_data(urch, from = 10, to = 20, by = "time")
    expect_error(calc_rate(sub),
                 regexp = NA)
    # rate from sub should be same as if sub was done in fn
    expect_equal(calc_rate(urch, from = 10, to = 20, by = "time")$rate,
                 calc_rate(sub)$rate)

    expect_error(auto_rate(sub),
                 regexp = NA)
    expect_equal(auto_rate(sub)$rate,
                 auto_rate(urchins.rd[61:121,c(1,4)])$rate)
  })

  test_that("subset_data - `inspect.ft` objects work as expected in subsequent functions.", {
    fltr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                        out.oxy = 2, in.oxy = 6))
    sub <- subset_data(fltr, from = 20, to = 30, by = "time")
    expect_error(calc_rate.ft(sub, flowrate = 1.5),
                 regexp = NA)
    # rate from sub should be same as if sub was done in fn
    expect_equal(calc_rate.ft(fltr, flowrate = 1.5, from = 20, to = 30)$rate,
                 calc_rate.ft(sub, flowrate = 1.5)$rate)

  })


  test_that("subset_data - `inspect.ft` $dataframe and $inputs elements have both been subset correctly", {

    fltr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                        out.oxy = 2, in.oxy = 6))
    sub <- subset_data(fltr, from = 20, to = 30, by = "time")
    dtt <- sapply(sub$data, function(z) rbind.data.frame(z))
    dtt <- as.data.table(dtt)
    names(dtt) <- names(sub$dataframe)
    expect_identical(dtt,
                     sub$dataframe)

    sub <- subset_data(fltr, from = 100, to = 867, by = "row")
    dtt <- sapply(sub$data, function(z) rbind.data.frame(z))
    dtt <- as.data.table(dtt)
    names(dtt) <- names(sub$dataframe)
    expect_identical(dtt,
                     sub$dataframe)

    sub <- subset_data(fltr, from = 8.5, to = 8.2, by = "ox")
    dtt <- sapply(sub$data, function(z) rbind.data.frame(z))
    dtt <- as.data.table(dtt)
    names(dtt) <- names(sub$dataframe)
    expect_identical(dtt,
                     sub$dataframe)

    fltr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                        delta.oxy = 8:10))
    sub <- subset_data(fltr, from = 15, to = 32, by = "time")
    dtt <- sapply(sub$data, function(z) rbind.data.frame(z))
    dtt <- as.data.table(dtt)
    names(dtt) <- names(sub$dataframe)
    expect_identical(dtt,
                     sub$dataframe)

    fltr <- suppressWarnings(inspect.ft(flowthrough.rd, time = 1,
                                        out.oxy = 2, in.oxy = 3))
    sub <- subset_data(fltr, from = 123, to = 654, by = "row")
    dtt <- sapply(sub$data, function(z) rbind.data.frame(z))
    dtt <- as.data.table(dtt)
    names(dtt) <- names(sub$dataframe)
    expect_identical(dtt,
                     sub$dataframe)
  })

  # this should now be impossible...
  # test_that("subset_data - warns if output is empty", {
  #   urch <- urchins.rd[20:200,]
  #   expect_warning(subset_data(urch, from = 0, to = 1, by = "time"),
  #                  regexp = "subset_data: subsetting criteria result in empty dataset!")
  #   expect_warning(subset_data(inspect(urch), from = 0, to = 1, by = "time"),
  #                  regexp = "subset_data: subsetting criteria result in empty dataset!")
  #   expect_warning(subset_data(inspect.ft(urch[,1:2]), from = 0, to = 1, by = "time"),
  #                  regexp = "subset_data: subsetting criteria result in empty dataset!")
  # })

  test_that("subset_data - correctly handles 'from' NULL", {
    urch <- urchins.rd[20:200,]
    expect_error(subset_data(urch, from = NULL, to = 20, by = "time"),
                 regexp = NA)
    expect_equal(as.numeric(subset_data(urch, from = NULL, to = 20, by = "time")[1,1]),
                 3.2)

    expect_error(subset_data(urch, from = NULL, to = 20, by = "row"),
                 regexp = NA)
    expect_equal(as.numeric(subset_data(urch, from = NULL, to = 20, by = "row")[1,1]),
                 3.2)

    expect_error(subset_data(urch, from = NULL, to = 7, by = "oxygen"),
                 regexp = NA)
    expect_equal(as.numeric(subset_data(urch, from = NULL, to = 20, by = "row")[1,2]),
                 7.75)

  })

  test_that("subset_data - correctly handles 'to' NULL", {
    urch <- urchins.rd[20:200,]
    expect_error(subset_data(urch, from = 5, to = NULL, by = "time"),
                 regexp = NA)
    expect_equal(tail(subset_data(urch, from = 5, to = NULL, by = "time")[[1]], 1),
                 33.2)

    expect_error(subset_data(urch, from = 5, to = NULL, by = "row"),
                 regexp = NA)
    expect_equal(tail(subset_data(urch, from = 5, to = NULL, by = "row")[[1]], 1),
                 33.2)

    expect_error(subset_data(urch, from = 7, to = NULL, by = "oxygen"),
                 regexp = NA)
    expect_equal(tail(subset_data(urch, from = 5, to = NULL, by = "oxygen")[[2]], 1),
                 6.90)

  })

  test_that("subset_data - correctly handles 'from' and 'to' NULL", {
    urch <- urchins.rd[20:200,]
    # all NULL - deafults - applies by= "time"
    expect_error(subset_data(urch),
                 regexp = NA)
    expect_equal(subset_data(urch)[[1]][1],
                 urch[[1]][1])
    expect_equal(tail(subset_data(urch)[[1]], 1),
                 urch[[1]][nrow(urch)])
    expect_equal(subset_data(urch)[[2]][1],
                 urch[[2]][1])
    expect_equal(tail(subset_data(urch)[[2]], 1),
                 urch[[2]][nrow(urch)])
    # by "row"
    expect_error(subset_data(urch, by = "row"),
                 regexp = NA)
    expect_equal(subset_data(urch, by = "row")[[1]][1],
                 urch[[1]][1])
    expect_equal(tail(subset_data(urch, by = "row")[[1]], 1),
                 urch[[1]][nrow(urch)])
    expect_equal(subset_data(urch, by = "row")[[2]][1],
                 urch[[2]][1])
    expect_equal(tail(subset_data(urch, by = "row")[[2]], 1),
                 urch[[2]][nrow(urch)])
    # by "oxygen"
    expect_error(subset_data(urch, by = "oxygen"),
                 regexp = NA)
    expect_equal(subset_data(urch, by = "oxygen")[[1]][1],
                 urch[[1]][1])
    expect_equal(tail(subset_data(urch, by = "oxygen")[[1]], 1),
                 urch[[1]][nrow(urch)])
    expect_equal(subset_data(urch, by = "oxygen")[[2]][1],
                 urch[[2]][1])
    expect_equal(tail(subset_data(urch, by = "oxygen")[[2]], 1),
                 urch[[2]][nrow(urch)])

  })

}) ## end capture.output

