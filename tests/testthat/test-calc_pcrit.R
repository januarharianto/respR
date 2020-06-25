## test_file("tests/testthat/test-calc_pcrit.R")

sink("/dev/null") ## stops printing of outputs on assigning

test_that("calc_pcrit works with default values", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(
    suppressMessages(
      calc_pcrit(squid, plot = F, parallel = F)), regexp = NA)
})

test_that("calc_pcrit accepts inspect objects", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  sq_i <- inspect(squid, plot = F)
  expect_error(calc_pcrit(sq_i, plot = F, parallel = F),
               regexp = NA)
})

test_that("calc_pcrit accepts inspect_data objects", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  sq_id <- suppressWarnings(inspect_data(squid, plot = F))
  expect_error(calc_pcrit(sq_id, plot = F, parallel = F),
               regexp = NA)
})

test_that("calc_pcrit accepts data frame objects", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(squid, plot = F, parallel = F),
               regexp = NA)
})

test_that("calc_pcrit stops if not a dataframe", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(as.matrix(squid), parallel = F, plot=F),
               regexp = "Input must be data.frame object.")
})

test_that("calc_pcrit stops if width too high", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(squid, width = 1000, parallel = F, plot=F),
               regexp = "'width' input is bigger than length of data.")
})

test_that("calc_pcrit stops if both oxygen and rate column IDs entered", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(squid, oxygen = 2, rate = 3, parallel = F, plot=F),
               regexp = "Choose either an 'oxygen' or 'rate' column, cannot enter both.")
})

test_that("calc_pcrit stops if oxygen/rate in column 1 and time not specified", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(squid, oxygen = 1, parallel = F, plot=F),
               regexp = "Please specify a 'time' argument.")
  expect_error(calc_pcrit(squid, rate = 1, parallel = F, plot=F),
               regexp = "Please specify a 'time' argument.")
})

test_that("calc_pcrit works without column specifiers - i.e. assumes time=1, oxygen=2", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_is(calc_pcrit(squid, parallel = F, plot=F),
            "calc_pcrit")
  expect_equal(calc_pcrit(squid, parallel = F, plot=F)$result.intercept,
               2.60027)
})

test_that("calc_pcrit works with only rate specified - i.e. assumes time = 1", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_is(calc_pcrit(squid, rate = 2, parallel = F, plot=F),
            "calc_pcrit")
  expect_equal(calc_pcrit(squid, rate = 2, parallel = F, plot=F)$result.midpoint,
               19850)
})

test_that("calc_pcrit correctly defaults to cols 1 and 2 for time and o2 and test exact value output", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_equal(round(calc_pcrit(squid, parallel = F, plot=F)$result.midpoint, 4),
               2.5944)
})

test_that("calc_pcrit works with all columns specified", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(squid, time = 1, oxygen = 2, parallel = F, plot = F),
               regexp = NA)
  expect_error(calc_pcrit(squid, time = 1, rate = 2, parallel = F, plot = F),
               regexp = NA)
})

test_that("calc_pcrit works with multi column data frames", {
  expect_error(calc_pcrit(urchins.rd, time = 1, oxygen = 12, parallel = F, plot = F), regexp = NA)
  expect_error(calc_pcrit(urchins.rd, time = 11, oxygen = 16, parallel = F, plot = F), regexp = NA)
  expect_error(calc_pcrit(urchins.rd, time = 11, rate = 16, parallel = F, plot = F), regexp = NA)
})

test_that("calc_pcrit pcrit S3 generics work with both oxygen and rate arguments", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  # oxygen
  pcr <- calc_pcrit(squid, parallel = F, plot = F)
  expect_output(print(pcr))
  expect_output(summary(pcr))
  expect_error(plot(pcr), regexp = NA)
  # rate
  pcr <- calc_pcrit(squid, rate = 2, parallel = F, plot = F)
  expect_output(print(pcr))
  expect_output(summary(pcr))
  expect_error(plot(pcr), regexp = NA)
})

test_that("calc_pcrit parallel code works", {
  squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
  expect_error(calc_pcrit(squid, parallel = T, plot=F),
               regexp = NA)
})

sink() ## turns printing back on

