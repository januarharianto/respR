## library(testthat)
## test_file("tests/testthat/test-subset_data.R")

sink("/dev/null") ## stops printing outputs on assigning

test_that("subset_data works with and outputs df input", {
  sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "time")
  expect_is(sub,
            "data.frame")
})

test_that("subset_data exact values", {
  sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "time")
  expect_equal(sub$Oxygen[1],
               93.8)
  expect_equal(tail(sub$Oxygen, 1),
               93.1)
})

test_that("subset_data works by row", {
  sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "row")
  expect_is(sub,
            "data.frame")
})

test_that("subset_data works by O2", {
  sub <- subset_data(sardine.rd, from = 93, to = 92, by = "o2")
  expect_is(sub,
            "data.frame")
})

test_that("subset_data works with variations of `by` input", {
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

test_that("subset_data error with wrong by", {
  expect_error(subset_data(sardine.rd, by = "tttimmmeee"),
               "`by` input not recognised")
})

test_that("subset_data output can be printed", {
  sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "time")
  expect_output(print(sub))
})

test_that("subset_data works with inspect objects for each method", {
  urch <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 14:15))
  expect_error(subset_data(urch, from = 7.6, to = 7.4, by = "oxygen"), regexp = NA)
  expect_error(subset_data(urch, from = 10, to = 30, by = "time"), regexp = NA)
  expect_error(subset_data(urch, from = 70, to = 170, by = "row"), regexp = NA)
  expect_error(subset_data(urch, from = 0.8, to = 0.4, by = "prop"), regexp = NA)
})

test_that("subset_data includes all columns when subsetting", {
  urch <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 10:15))
  expect_equal(ncol((subset_data(urch, from = 7.6, to = 7.4, by = "oxygen"))$dataframe), 7)
  expect_equal(ncol((subset_data(urch, from = 10, to = 30, by = "time"))$dataframe), 7)
  expect_equal(ncol((subset_data(urch, from = 70, to = 170, by = "row"))$dataframe), 7)
  expect_equal(ncol((subset_data(urch, from = 0.8, to = 0.4, by = "prop"))$dataframe), 7)
})

sink() ## turns printing back on

