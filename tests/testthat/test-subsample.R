## test_file("tests/testthat/test-subsample.R")

test_that("subsample output is data.frame object", {
  ssm <- subsample(sardine.rd, n = 10)
  expect_is(ssm, "data.frame")
})

test_that("subsample random.start argument works as expected", {
  expect_error(subsample(sardine.rd, random_start = T), regexp = NA)
})

test_that("subsample will not break when plot argument is changed", {
  expect_error(subsample(sardine.rd, plot = F), regexp = NA)
})
