## testthat::test_file("tests/testthat/test-subsample.R")

if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
skip_on_cran()
skip_on_ci()
test_that("subsample stops if both n and length.out input", {
  expect_error(subsample(sardine.rd, n = 10, length.out = 200),
               "Only one of 'n' or 'length.out' should be entered.")
})

test_that("subsample stops if neither n or length.out input", {
  expect_error(subsample(sardine.rd, n = NULL, length.out = NULL),
               "One of 'n' or 'length.out' is required.")
})

test_that("subsample accepts data.frame input", {
  expect_error(subsample(sardine.rd, n = 10),
               NA)
})

test_that("subsample accepts vector input", {
  expect_error(subsample(sardine.rd[[1]], n = 10),
               NA)
})

test_that("subsample outputs data.frame if input is data.frame", {
  ssm <- subsample(sardine.rd, n = 10)
  expect_is(ssm, "data.frame")
})

test_that("subsample outputs vector if input is vector", {
  ssm <- subsample(sardine.rd[[1]], n = 10)
  expect_is(ssm, "integer")
})

test_that("subsample random.start input works as expected", {
  expect_error(subsample(sardine.rd, n = 10, random_start = T),
               regexp = NA)
})

test_that("subsample will not break when plot input is changed", {
  expect_error(subsample(sardine.rd, n = 10, plot = F),
               regexp = NA)
})

test_that("subsample output is correct lengths for data.frames", {
  expect_equal(nrow(subsample(sardine.rd, n = 10, plot = F)),
               752)
  expect_equal(nrow(subsample(sardine.rd, length.out = 752, plot = F)),
               752)
})

test_that("subsample works with and plots multi column df", {
  expect_error(subsample(urchins.rd, n = 10, plot = T),
               NA)
  expect_message(subsample(urchins.rd, n = 10, plot = T),
                 "subsample: plotting first column of data only.")
})

test_that("subsample output is correct lengths for vectors", {
  expect_equal(length(subsample(sardine.rd[[1]], n = 10, plot = F)),
               752)
  expect_equal(length(subsample(sardine.rd[[1]], length.out = 752, plot = F)),
               752)
})

