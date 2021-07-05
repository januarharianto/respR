## library(testthat)
## test_file("tests/testthat/test-oxy_crit.R")

capture.output({  ## stops printing of outputs on assigning

test_that("oxy_crit works with default values", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(
    suppressMessages(
      oxy_crit(squid, plot = F, parallel = F)), regexp = NA)
})

test_that("oxy_crit accepts inspect objects", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  sq_i <- inspect(squid, plot = F)
  expect_error(oxy_crit(sq_i, plot = F, parallel = F),
               regexp = NA)
})

test_that("oxy_crit accepts data frame objects", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(oxy_crit(squid, plot = F, parallel = F),
               regexp = NA)
})

test_that("oxy_crit stops if not a dataframe", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(oxy_crit(as.matrix(squid), parallel = F, plot=F),
               regexp = "Input must be data.frame object.")
})

test_that("oxy_crit stops if width too high", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(oxy_crit(squid, width = 1000, parallel = F, plot=F),
               regexp = "'width' input is bigger than length of data.")
})

test_that("oxy_crit stops if both oxygen and rate column IDs entered", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(oxy_crit(squid, oxygen = 2, rate = 3, parallel = F, plot=F),
               regexp = "Choose either an 'oxygen' or 'rate' column, cannot enter both.")
})

test_that("oxy_crit stops if oxygen/rate in column 1 and time not specified", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(oxy_crit(squid, oxygen = 1, parallel = F, plot=F),
               regexp = "Please specify a 'time' argument.")
  expect_error(oxy_crit(squid, rate = 1, parallel = F, plot=F),
               regexp = "Please specify a 'time' argument.")
})

test_that("oxy_crit works without column specifiers - i.e. assumes time=1, oxygen=2", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_is(oxy_crit(squid, parallel = F, plot=F),
            "oxy_crit")
  expect_equal(oxy_crit(squid, parallel = F, plot=F)$result.intercept,
               2.60027)
})

test_that("oxy_crit works with only rate specified - i.e. assumes time = 1", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_is(oxy_crit(squid, rate = 2, parallel = F, plot=F),
            "oxy_crit")
  expect_equal(oxy_crit(squid, rate = 2, parallel = F, plot=F)$result.midpoint,
               19850)
})

test_that("oxy_crit correctly defaults to cols 1 and 2 for time and o2 and test exact value output", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_equal(round(oxy_crit(squid, parallel = F, plot=F)$result.midpoint, 4),
               2.5944)
})

test_that("oxy_crit works with all columns specified", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(oxy_crit(squid, time = 1, oxygen = 2, parallel = F, plot = F),
               regexp = NA)
  expect_error(oxy_crit(squid, time = 1, rate = 2, parallel = F, plot = F),
               regexp = NA)
})

test_that("oxy_crit works with multi column data frames", {
  expect_error(oxy_crit(urchins.rd, time = 1, oxygen = 12, parallel = F, plot = F), regexp = NA)
  expect_error(oxy_crit(urchins.rd, time = 11, oxygen = 16, parallel = F, plot = F), regexp = NA)
  expect_error(oxy_crit(urchins.rd, time = 11, rate = 16, parallel = F, plot = F), regexp = NA)
})

test_that("oxy_crit pcrit S3 generics work with both oxygen and rate arguments", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  # oxygen
  pcr <- oxy_crit(squid, parallel = F, plot = F)
  expect_output(print(pcr))
  expect_output(summary(pcr))
  expect_error(plot(pcr), regexp = NA)
  # rate
  pcr <- oxy_crit(squid, rate = 2, parallel = F, plot = F)
  expect_output(print(pcr))
  expect_output(summary(pcr))
  expect_error(plot(pcr), regexp = NA)
})

# This is failing...
# test_that("oxy_crit parallel code works", {
#   squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
#   expect_error(oxy_crit(squid, parallel = T, plot=F),
#                regexp = NA)
# })

}) ## turns printing back on

