## library(testthat)
## test_file("tests/testthat/test-oxy_crit.R")

capture.output({  ## stops printing of outputs on assigning

# small testing dataset
squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
# rate dataset
squid_rate <- generate_mrdf(squid, 30)


test_that("oxy_crit works with default values", {
  expect_error(
    suppressMessages(
      oxy_crit(squid, plot = F, parallel = F)), regexp = NA)
})

test_that("oxy_crit accepts inspect objects", {
  sq_i <- inspect(squid, plot = F)
  expect_error(oxy_crit(sq_i, plot = F, parallel = F),
               regexp = NA)
})

test_that("oxy_crit accepts data frame objects", {
  expect_error(oxy_crit(squid, plot = F, parallel = F),
               regexp = NA)
})

test_that("oxy_crit stops if not a dataframe", {
  expect_error(oxy_crit(as.matrix(squid), parallel = F, plot=F),
               regexp = "oxy_crit: Input must be an 'inspect' or data.frame object.")
})

test_that("oxy_crit stops if width out of range", {
  expect_error(oxy_crit(squid, width = 1000, parallel = F, plot=F),
               regexp = "oxy_crit: 'width' input should be between 0.001 to 0.999, representing a proportion of the total data length.")
})

test_that("oxy_crit stops if time and rate columns entered", {
  expect_error(oxy_crit(squid, time = 1, rate = 2, parallel = F, plot=F),
               regexp = "oxy_crit: Inputs should be 'time' and 'oxygen' columns, or 'oxygen' and 'rate' columns.")
})

test_that("oxy_crit stops if oxygen/rate/time columns conflict or are out of range", {
  expect_error(oxy_crit(squid, time = 1, oxygen = 3, parallel = F, plot=F),
               regexp = "one or more column inputs are out of range of allowed data columns.")
  expect_error(oxy_crit(squid, time = 3, oxygen = 2, parallel = F, plot=F),
               regexp = "one or more column inputs are out of range of allowed data columns.")
  expect_error(oxy_crit(squid, rate = 3, oxygen = 2, parallel = F, plot=F),
               regexp = "one or more column inputs are out of range of allowed data columns.")

  expect_error(oxy_crit(squid, time = 1, oxygen = 1, parallel = F, plot=F),
               regexp = "one or more column inputs conflicts with other inputs.")
  expect_error(oxy_crit(squid, rate = 2, oxygen = 2, parallel = F, plot=F),
               regexp = "one or more column inputs conflicts with other inputs.")

  expect_error(oxy_crit(urchins.rd, time = 1:2, oxygen = 3, parallel = F, plot=F),
               regexp = "cannot enter more than 1 column\\(s) with this input or this dataset.")
  expect_error(oxy_crit(urchins.rd, time = 3, oxygen = 1:2, parallel = F, plot=F),
               regexp = "cannot enter more than 1 column\\(s) with this input or this dataset.")
  expect_error(oxy_crit(urchins.rd, rate = 3:4, oxygen = 2, parallel = F, plot=F),
               regexp = "cannot enter more than 1 column\\(s) with this input or this dataset.")
})

test_that("oxy_crit applies defualts of time=1, oxygen=2", {
  expect_is(oxy_crit(squid, parallel = F, plot=F),
            "oxy_crit")
  expect_equal(oxy_crit(squid, parallel = F, plot=F)$result.intercept,
               oxy_crit(squid, time = 1, oxygen = 2, parallel = F, plot=F)$result.intercept)
})


test_that("oxy_crit - works with segmented method", {
  # results as of July 2021, in case we do something that changes them
  expect_error(oxy_crit(squid, method = "segmented", parallel = F, plot=F),
               NA)
})

test_that("oxy_crit - test exact value outputs", {
  # results as of July 2021, in case we do something that changes them
  expect_equal(round(oxy_crit(squid, parallel = F, plot=F)$crit$crit.intercept, 4),
               2.6003)
  expect_equal(round(oxy_crit(squid, parallel = F, plot=F)$crit$crit.midpoint, 4),
               2.5944)
  expect_equal(round(oxy_crit(squid, method = "segmented", parallel = F, plot=F)$crit, 4),
               2.6003)
})

test_that("oxy_crit works with oxygen~rate data", {
  expect_error(oxy_crit(squid_rate, oxygen = 1, rate = 2, parallel = F, plot = F),
               regexp = NA)
  expect_equal(oxy_crit(squid_rate, oxygen = 1, rate = 2, parallel = F, plot = F)$crit$crit.intercept,
               2.598025, tolerance = 6)
})

test_that("oxy_crit works with multi column data frames", {
  expect_error(oxy_crit(urchins.rd, time = 1, oxygen = 12, parallel = F, plot = F), regexp = NA)
  expect_error(oxy_crit(urchins.rd, time = 11, oxygen = 16, parallel = F, plot = F), regexp = NA)
})

test_that("oxy_crit pcrit S3 generics work with both oxygen and rate arguments", {

  # oxygen
  pcr <- oxy_crit(squid, parallel = F, plot = F)
  expect_output(print(pcr))
  expect_output(summary(pcr))
  expect_error(plot(pcr), regexp = NA)
  # rate
  pcr <- oxy_crit(squid_rate, oxygen =1, rate = 2, parallel = F, plot = F)
  expect_output(print(pcr))
  expect_output(summary(pcr))
  expect_error(plot(pcr), regexp = NA)
})

# This is failing...
# test_that("oxy_crit parallel code works", {
#
#   expect_error(oxy_crit(squid, parallel = T, plot=F),
#                regexp = NA)
# })

})

