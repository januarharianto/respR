## library(testthat)
## test_file("tests/testthat/test-calc_rate.bg.R")

#sink("/dev/null") ## stops printing outputs on assigning in log

## Accepts data.frame
test_that("calc_rate.bg works using default arguments", {
  ## Analyses all columns by default
  urbg <- calc_rate.bg(urchins.rd,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
  expect_equal(ncol(urbg$dataframe), 19)
  expect_equal(length(urbg$rate.bg), 18)
})

test_that("calc_rate.bg accepts 2 columns", {
  urbg <- calc_rate.bg(urchins.rd,  time = 1, oxygen = 18, plot = F)
  expect_equal(ncol(urbg$dataframe), 2)
})

test_that("calc_rate.bg accepts multiple columns", {
  expect_error(calc_rate.bg(urchins.rd,  plot = F),
               NA)
  urbg <- calc_rate.bg(urchins.rd,  time = 1, oxygen = c(18,19), plot = F)
  expect_equal(ncol(urbg$dataframe), 3)
})

test_that("calc_rate.bg accepts `inspect` objects", {
  ur <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 18, plot = F))
  expect_error(calc_rate.bg(ur,  plot = F),
               NA)
  urbg <- calc_rate.bg(ur,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
  expect_equal(urbg$rate.bg,
               -0.0007650013)
})

test_that("calc_rate.bg accepts `data.frame` objects", {
  ur <- as.data.frame(urchins.rd[,c(1,18)])
  expect_error(calc_rate.bg(ur,  plot = F),
            NA)
  urbg <- calc_rate.bg(ur,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
  expect_equal(urbg$rate.bg,
               -0.0007650013)
})

test_that("calc_rate.bg accepts `data.table` objects", {
  ur <- data.table::data.table(urchins.rd[,c(1,18)])
  expect_error(calc_rate.bg(ur,  plot = F),
               NA)
  urbg <- calc_rate.bg(ur,  plot = F)
  expect_is(urbg,
            "calc_rate.bg")
  expect_equal(urbg$rate.bg,
               -0.0007650013)
})

test_that("calc_rate.bg correctly uses specified columns in `inspect` objects", {
  ur <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 2:19, plot = F))
  urbg <- calc_rate.bg(ur,  time = 1, oxygen = c(18,19), plot = F)
  expect_equal(ncol(urbg$data), 3)
  expect_equal(ur$dataframe$b1, urbg$data$b1)
  expect_equal(ur$dataframe$b2, urbg$data$b2)
})

test_that("calc_rate.bg S3 generics work", {
  ur <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 2:19, plot = F))
  urbg <- calc_rate.bg(ur,  time = 1, oxygen = c(18,19), plot = F)
  expect_output(print(urbg))
  expect_output(plot(urbg))
  expect_output(mean(urbg))
})

test_that("calc_rate.bg - plot defaults are correctly restored", {

  # reset plotting first
  dev.off()
  # save par before
  parb4 <- par(no.readonly = TRUE)
  # now use a fn with plot
  calc_rate.bg(urchins.rd, 1, 19)
  # save after
  paraft <- par(no.readonly = TRUE)
  # mai is something changed from the default,
  # so if par settings not restored properly this should fail
  expect_identical(parb4$mai,
                   paraft$mai)

})


#sink() ## turns printing back on
