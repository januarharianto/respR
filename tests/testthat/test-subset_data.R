# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-subset_data.R")
# covr::file_coverage("R/subset_data.R", "tests/testthat/test-subset_data.R")
# x <- covr::package_coverage()
# covr::report(x)

sink("/dev/null") ## stops printing outputs on assigning

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
  sub <- subset_data(sardine.rd, from = 93, to = 92, by = "o2")
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
    expect_error(subset_data(sardine.rd, from = 95, to = 94, by = "O2"), regexp = NA)
    expect_error(subset_data(sardine.rd, from = 0.8, to = 0.6, by = "Prop"), regexp = NA)
    expect_error(subset_data(sardine.rd, from = 0.8, to = 0.6, by = "p"), regexp = NA)
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
  expect_error(subset_data(urch, from = 0.8, to = 0.4, by = "prop"),
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
  expect_equal(ncol((subset_data(urch, from = 0.8, to = 0.4, by = "prop"))$dataframe),
               7)
})

test_that("subset_data - works with inspect.ft objects", {
  fthr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                      delta.o2 =  8:10, plot = FALSE))

  expect_error(subset_data(fthr, from = -0.3, to = -0.5, by = "oxygen"),
               regexp = NA)
  expect_output(subset_data(fthr, from = -0.3, to = -0.5, by = "oxygen"),
                regexp = "Subset data:")
  expect_equal(nrow(subset_data(fthr, from = -0.3, to = -0.5, by = "oxygen")$dataframe),
               590)
  expect_error(subset_data(fthr, from = 1000, to = 2000, by = "time"),
               regexp = NA)
  expect_output(subset_data(fthr, from = 1000, to = 2000, by = "time"),
                regexp = "Subset data:")
  expect_equal(nrow(subset_data(fthr, from = 1000, to = 2000, by = "time")$dataframe),
               1001)
  expect_error(subset_data(fthr, from = 70, to = 170, by = "row"),
               regexp = NA)
  expect_output(subset_data(fthr, from = 70, to = 170, by = "row"),
                regexp = "Subset data:")
  expect_equal(nrow(subset_data(fthr, from = 70, to = 170, by = "row")$dataframe),
               101)
  expect_error(subset_data(fthr, from = 0.8, to = 0.4, by = "prop"),
               regexp = NA)
  expect_output(subset_data(fthr, from = 0.8, to = 0.4, by = "prop"),
                regexp = "Subset data:")
  expect_equal(nrow(subset_data(fthr, from = 0.8, to = 0.4, by = "prop")$dataframe),
               673)
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
                                      delta.o2 =  8:10, plot = FALSE))

  expect_error(subset_data(fthr, from = 3000, to = 2000, by = "time"),
               regexp = "subset_data: 'to' - one or more inputs are outside the range of allowed values.")
  expect_error(subset_data(fthr, from = NULL, to = 3800, by = "time"),
               regexp = "subset_data: 'from' - input is required.")
  expect_error(subset_data(fthr, from = 8000, to = 2000, by = "time"),
               regexp = "subset_data: 'from' - one or more inputs are outside the range of allowed values.")
  expect_error(subset_data(fthr, from = 2001:2002, to = 3000, by = "time"),
               regexp = "subset_data: 'from' - only 1 inputs allowed.")

  expect_error(subset_data(fthr, from = 3000.2, to = 2000, by = "row"),
               regexp = "subset_data: 'from' - one or more inputs are not integers.")
  expect_error(subset_data(fthr, from = NULL, to = 3800, by = "row"),
               regexp = "subset_data: 'from' - input is required.")
  expect_error(subset_data(fthr, from = 8000, to = 2000, by = "row"),
               regexp = "subset_data: 'from' - one or more inputs are outside the range of allowed values.")
  expect_error(subset_data(fthr, from = 2001:2002, to = 3000, by = "row"),
               regexp = "subset_data: 'from' - only 1 inputs allowed.")

  expect_error(subset_data(fthr, from = NULL, to = 2000, by = "o2"),
               regexp = "subset_data: 'from' - input is required.")
  expect_error(subset_data(fthr, from = 2:3, to = 3800, by = "o2"),
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
                                      out.o2 = 2, in.o2 = 5))
  sub <- subset_data(fltr, from = 2000, to = 3000, by = "time")
  expect_error(calc_rate.ft(sub, flowrate = 1.5),
               regexp = NA)
  # rate from sub should be same as if sub was done in fn
  expect_equal(calc_rate.ft(fltr, flowrate = 1.5, from = 2000, to = 3000)$rate,
               calc_rate.ft(sub, flowrate = 1.5)$rate)

})


test_that("subset_data - `inspect.ft` $dataframe and $inputs elements have both been subset correctly", {

  fltr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                      out.o2 = 2, in.o2 = 5))
  sub <- subset_data(fltr, from = 2000, to = 3000, by = "time")
  dtt <- sapply(sub$input_data, function(z) rbind.data.frame(z))
  dtt <- as.data.table(dtt)
  names(dtt) <- names(sub$dataframe)
  expect_identical(dtt,
                   sub$dataframe)

  sub <- subset_data(fltr, from = 100, to = 867, by = "row")
  dtt <- sapply(sub$input_data, function(z) rbind.data.frame(z))
  dtt <- as.data.table(dtt)
  names(dtt) <- names(sub$dataframe)
  expect_identical(dtt,
                   sub$dataframe)

  sub <- subset_data(fltr, from = 8.5, to = 8.2, by = "ox")
  dtt <- sapply(sub$input_data, function(z) rbind.data.frame(z))
  dtt <- as.data.table(dtt)
  names(dtt) <- names(sub$dataframe)
  expect_identical(dtt,
                   sub$dataframe)

  fltr <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1,
                                      delta.o2 = 8:10))
  sub <- subset_data(fltr, from = 1567, to = 3200, by = "time")
  dtt <- sapply(sub$input_data, function(z) rbind.data.frame(z))
  dtt <- as.data.table(dtt)
  names(dtt) <- names(sub$dataframe)
  expect_identical(dtt,
                   sub$dataframe)

  fltr <- suppressWarnings(inspect.ft(flowthrough.rd, time = 1,
                                      out.o2 = 2, in.o2 = 3))
  sub <- subset_data(fltr, from = 123, to = 654, by = "row")
  dtt <- sapply(sub$input_data, function(z) rbind.data.frame(z))
  dtt <- as.data.table(dtt)
  names(dtt) <- names(sub$dataframe)
  expect_identical(dtt,
                   sub$dataframe)
})

test_that("subset_data - warns if output is empty", {
  urch <- urchins.rd[20:200,]
  expect_warning(subset_data(urch, from = 0, to = 1, by = "time"),
               regexp = "subset_data: subsetting criteria result in empty dataset!")
  expect_warning(subset_data(inspect(urch), from = 0, to = 1, by = "time"),
               regexp = "subset_data: subsetting criteria result in empty dataset!")
  expect_warning(subset_data(inspect.ft(urch[,1:2]), from = 0, to = 1, by = "time"),
               regexp = "subset_data: subsetting criteria result in empty dataset!")
})

sink() ## turns printing back on

