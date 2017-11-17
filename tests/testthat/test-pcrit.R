context("pcrit")

test_that("pcrit is of class `pcrit`", {
  expect_is(pcrit(subsample(squid.rd, 100)), "pcrit")
})

test_that("pcrit works with default settings", {
  expect_equal(length(pcrit(subsample(squid.rd, 100))), 10)
})
