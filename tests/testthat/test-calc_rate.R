# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-calc_rate.R")
# covr::file_coverage("R/calc_rate.R", "tests/testthat/test-calc_rate.R")
# x <- covr::package_coverage()
# covr::report(x)
# covr::report(covr::package_coverage())


capture.output({  ## stops printing outputs on assigning

test_that("calc_rate - outputs object of class calc_rate", {
  cr <- calc_rate(sardine.rd, plot = F)
  expect_is(cr,
            "calc_rate")
})

test_that("calc_rate - subsetting methods work and produce correct outputs", {
  cr <- calc_rate(sardine.rd, from = 2000, to = 4000, by = "time", plot = F)
  expect_is(cr,
            "calc_rate")
  expect_equal(as.numeric(cr$subsets[[1]][1,1]),
               2000)
  expect_equal(as.numeric(cr$subsets[[1]][nrow(cr$subsets[[1]]),1]),
               4000)

  cr <- calc_rate(sardine.rd, 2000, 4000, by = "row", plot = F)
  expect_is(cr,
            "calc_rate")
  expect_equal(nrow(cr$subsets[[1]]),
               2001)

  cr <- calc_rate(sardine.rd, 94, 93, by = "oxygen", plot = F)
  expect_is(cr,
            "calc_rate")
  expect_equal(as.numeric(cr$subsets[[1]][1,2]),
               94)
  expect_equal(as.numeric(cr$subsets[[1]][nrow(cr$subsets[[1]]),2]),
               93)


  cr <- calc_rate(sardine.rd, 0.8, 0.5, by = "proportion", plot = F)
  expect_is(cr,
            "calc_rate")
  expect_equal(as.numeric(cr$subsets[[1]][1,2]),
               min(cr$dataframe[[2]]) + (diff((c(min(cr$dataframe[[2]]), max(cr$dataframe[[2]])))) * 0.8))
  expect_equal(as.numeric(cr$subsets[[1]][nrow(cr$subsets[[1]]),2]),
               (min(cr$dataframe[[2]]) + max(cr$dataframe[[2]]))/2)
})

test_that("calc_rate - S3 generics work", {
  cr <- calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)
  expect_output(print(cr))
  expect_output(summary(cr))
  expect_output(plot(cr))
  expect_output(suppressWarnings(mean(cr)))

  # multiple rates and 'pos'
  cr <- calc_rate(sardine.rd, from = 2000:2020, to = 4000:4020, plot = F)
  expect_output(print(cr, pos = 2))
  expect_error(print(cr, pos = 2:3),
                 "print.calc_rate: 'pos' must be a single value. To examine multiple results use summary().")
  expect_error(print(cr, pos = 30),
                 "print.calc_rate: Invalid 'pos' rank: only 21 rates found.")

  expect_output(summary(cr, pos = 2:3))
  expect_error(summary(cr, pos = 40),
                 "summary.calc_rate: Invalid 'pos' rank: only 21 rates found.")
  expect_is(summary(cr, pos = 2:3, export = TRUE),
                 "data.frame")

  expect_output(mean(cr, pos = 2:3))
  expect_error(mean(cr, pos = 40),
                 "mean.calc_rate: Invalid 'pos' rank: only 21 rates found.")
  expect_is(mean(cr, pos = 2:3, export = TRUE),
                 "numeric")
  expect_equal(mean(cr, pos = 2:3, export = TRUE),
               mean(cr$rate[2:3]))

  # pos default applied
  expect_output(plot(cr, pos = NULL))
  expect_output(plot(cr, pos = 1))
  expect_output(plot(cr, pos = 3))
  expect_error(plot(cr, pos = 50),
               "calc_rate: Invalid 'pos' rank: only 21 rates found.")
  expect_error(plot(cr, pos = 1:2),
               "calc_rate: 'pos' should be a single value.")

})

test_that("calc_rate - calling linear_fit (calc_rate) produces coefficients", {

  expect_equal(c("intercept_b0", "rate_b1", "rsq"),
               names(linear_fit(sardine.rd)))
})

test_that("calc_rate works with variations of `by` input", {
  expect_error(calc_rate(sardine.rd, plot = F, by = "Time"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "T"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "Oxygen"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "oxygen"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "Row"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "r"), regexp = NA)
  expect_error(calc_rate(sardine.rd, plot = F, by = "Proportion"),
               regexp = "calc_rate: please enter a proportion 'from' input.")
  expect_error(calc_rate(sardine.rd, plot = F, by = "prop"),
               regexp = "calc_rate: please enter a proportion 'from' input.")
})

test_that("calc_rate - stops with wrong 'by' inputs", {
  expect_error(calc_rate(sardine.rd, plot = F, by = "tttimmmeee"),
               "'by' input not valid or not recognised")
})

test_that("calc_rate - correctly extracts dataframe from 'inspect' objects", {
  insp <- inspect(sardine.rd[1000:2000,], plot = F)
  cr <- calc_rate(insp, plot = F)
  expect_identical(cr$dataframe,
                   insp$dataframe)

})

test_that("calc_rate - stops with non data.frame 'x' input", {
  expect_error(calc_rate(as.matrix(sardine.rd), plot = F),
               "calc_rate: Input must be a 'data.frame' or 'inspect' object.")
})

test_that("calc_rate - warns with multi column input", {
  expect_warning(calc_rate(urchins.rd, plot = F),
                 "calc_rate: Multiple columns of oxygen data found in input.")
})

test_that("calc_rate - calcs rate over all available data if to and from are NULL", {
  cr <- calc_rate(sardine.rd[1000:2000,], from = NULL, to = NULL, plot = F)

  expect_identical(cr$dataframe,
                   cr$subsets[[1]])
  expect_identical(nrow(cr$dataframe),
                   cr$summary$endrow)
})

test_that("calc_rate - correctly handles 'from' NULL", {
  urch <- urchins.rd[20:200,1:2]
  expect_error(calc_rate(urch, from = NULL, to = 20, by = "time", plot = FALSE),
               regexp = NA)
  expect_equal(calc_rate(urch, from = NULL, to = 20, by = "time", plot = FALSE)$summary$time,
               urch[[1]][1])

  expect_error(calc_rate(urch, from = NULL, to = 20, by = "row", plot = FALSE),
               regexp = NA)
  expect_equal(calc_rate(urch, from = NULL, to = 20, by = "row", plot = FALSE)$summary$row,
               1)

  expect_error(calc_rate(urch, from = NULL, to = 7, by = "oxygen", plot = FALSE),
               regexp = NA)
  expect_equal(calc_rate(urch, from = NULL, to = 7, by = "oxygen", plot = FALSE)$summary$oxy,
               urch[[2]][1])

  expect_error(calc_rate(urch, from = NULL, to = 0.8, by = "prop"),
               regexp = "calc_rate: please enter a proportion 'from' input.")
})

test_that("calc_rate - correctly handles 'to' NULL", {
  urch <- urchins.rd[20:200,1:2]
  expect_error(calc_rate(urch, from = 5, to = NULL, by = "time", plot = FALSE),
               regexp = NA)
  expect_equal(calc_rate(urch, from = 5, to = NULL, by = "time", plot = FALSE)$summary$endtime,
               urch[[1]][181])

  expect_error(calc_rate(urch, from = 5, to = NULL, by = "row", plot = FALSE),
               regexp = NA)
  expect_equal(calc_rate(urch, from = 5, to = NULL, by = "row", plot = FALSE)$summary$endrow,
               181)

  expect_error(calc_rate(urch, from = 7.5, to = NULL, by = "oxygen", plot = FALSE),
               regexp = NA)
  expect_equal(calc_rate(urch, from = 7.5, to = NULL, by = "oxygen", plot = FALSE)$summary$endoxy,
               urch[[2]][181])

  expect_error(calc_rate(urch, from = 0.2, to = NULL, by = "prop"),
               regexp = "calc_rate: please enter a proportion 'to' input.")
})


test_that("calc_rate - correctly handles 'from' and 'to' NULL", {
  urch <- urchins.rd[20:200,1:2]
  # all NULL - deafults - applies by= "time"
  expect_error(calc_rate(urch),
               regexp = NA)
  expect_equal(calc_rate(urch)$summary$time,
               urch[[1]][1])
  expect_equal(calc_rate(urch)$summary$endtime,
               urch[[1]][nrow(urch)])
  expect_equal(calc_rate(urch)$summary$oxy,
               urch[[2]][1])
  expect_equal(calc_rate(urch)$summary$endoxy,
               urch[[2]][nrow(urch)])
  # by "row"
  expect_error(calc_rate(urch, by = "row"),
               regexp = NA)
  expect_equal(calc_rate(urch, by = "row")$summary$row,
               1)
  expect_equal(calc_rate(urch, by = "row")$summary$endrow,
               181)
  expect_equal(calc_rate(urch, by = "row")$summary$oxy,
               urch[[2]][1])
  expect_equal(calc_rate(urch, by = "row")$summary$endoxy,
               urch[[2]][nrow(urch)])
  # by "oxygen"
  expect_error(calc_rate(urch, by = "oxygen"),
               regexp = NA)
  expect_equal(calc_rate(urch, by = "oxygen")$summary$row,
               1)
  expect_equal(calc_rate(urch, by = "oxygen")$summary$endrow,
               181)
  expect_equal(calc_rate(urch, by = "oxygen")$summary$oxy,
               urch[[2]][1])
  expect_equal(calc_rate(urch, by = "oxygen")$summary$endoxy,
               urch[[2]][nrow(urch)])


})

test_that("calc_rate - stops if 'from' and 'to' are unequal length", {
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(5,10,15), to = c(100,105), plot = F, by = "time"),
               "calc_rate: 'from' and 'to' have unequal lengths.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 5, to = c(100,105), plot = F, by = "oxy"),
               "calc_rate: 'from' and 'to' have unequal lengths.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(5,10,15), to = c(100,105), plot = F, by = "row"),
               "calc_rate: 'from' and 'to' have unequal lengths.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 5, to = c(100,105), plot = F, by = "prop"),
               "calc_rate: 'from' and 'to' have unequal lengths.")
})

test_that("calc_rate - stops if any paired 'from' and 'to' are of equal value", {
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 5, to = 5, plot = F, by = "time"),
               "some 'from' values are equal to the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(5,6,7), to = c(4,6,8), plot = F, by = "oxy"),
               "some 'from' values are equal to the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(5,6,7), to = c(4,6,8), plot = F, by = "row"),
               "some 'from' values are equal to the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(5,6,7), to = c(4,6,8), plot = F, by = "prop"),
               "some 'from' values are equal to the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(4,4,6), to = c(6,6,6), plot = F, by = "oxy"),
               "some 'from' values are equal to the paired values in 'to'.")
})


# by = "time" checks ------------------------------------------------------

test_that("calc_rate - by = 'time' stops with time values out of order", {
  by <- "time"
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 1600, to = 1500, plot = F, by = by),
               "calc_rate: some 'from' time values are later than the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(1200, 1600), to = c(1300, 1500), plot = F, by = by),
               "calc_rate: some 'from' time values are later than the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(1200, 1600, 1800), to = c(1300, 1500, 1900), plot = F, by = by),
               "calc_rate: some 'from' time values are later than the paired values in 'to'.")
})

test_that("calc_rate - by = 'time' stops with time values out of range of available data", {
  by <- "time"
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 2000, to = 2500, plot = F, by = by),
               "calc_rate: some 'from' time values are higher than the values present in 'x'")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(1500, 1600, 2100), to = c(1800, 1900, 2200), plot = F, by = by),
               "calc_rate: some 'from' time values are higher than the values present in 'x'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 400, to = 800, plot = F, by = by),
               "calc_rate: some 'to' time values are lower than the values present in 'x'")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(1500, 1600, 400), to = c(1800, 1900, 600), plot = F, by = by),
               "calc_rate: some 'to' time values are lower than the values present in 'x'")
})

test_that("calc_rate - by = 'time' warns with time values out of range, but uses closest available value", {
  by <- "time"

  expect_message(calc_rate(sardine.rd[1000:2000,], from = 900, to = 1500, plot = F, by = by),
                 "calc_rate: some 'from' time values are lower than the values present in 'x'. The lowest time value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd[1000:2000,], from = 900, to = 1500, plot = F, by = by)$summary$time[1]),
               999)
  expect_message(calc_rate(sardine.rd[1000:2000,], from = c(800, 900, 1000), to = c(1500, 1600, 1700), plot = F, by = by),
                 "calc_rate: some 'from' time values are lower than the values present in 'x'. The lowest time value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd[1000:2000,], from = c(800, 900, 1000), to = c(1500, 1600, 1700), plot = F, by = by)$summary$time[1:3]),
               c(999,999,1000))

  expect_message(calc_rate(sardine.rd[1000:2000,], from = 1500, to = 2200, plot = F, by = by),
                 "calc_rate: some 'to' time values are higher than the values present in 'x'. The highest time value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd[1000:2000,], from = 1500, to = 2200, plot = F, by = by)$summary$endtime[1]),
               1999)
  expect_message(calc_rate(sardine.rd[1000:2000,], from = c(1500, 1600, 1700), to = c(1900, 2200, 2500), plot = F, by = by),
                 "calc_rate: some 'to' time values are higher than the values present in 'x'. The highest time value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd[1000:2000,], from = c(1500, 1600, 1700), to = c(1900, 2200, 2500), plot = F, by = by)$summary$endtime[1:3]),
               c(1900,1999,1999))
})

test_that("calc_rate - by = 'time' outputs correct results", {
  by <- "time"

  from <- 1000
  to <- 1999
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sardine.rd$Oxygen[(from+1):(to+1)]~sardine.rd$Time[(from+1):(to+1)])$coefficients[2]))

  ## try a bunch of values
  from <- round(seq(1000, 5000, length.out = 20))
  to <- round(seq(2000, 7000, length.out = 20))
  mapply(function(p,q) expect_equal(calc_rate(sardine.rd, from = p, to = q, plot = F, by = by)$rate,
                                    as.numeric(lm(sardine.rd$Oxygen[(p+1):(q+1)]~sardine.rd$Time[(p+1):(q+1)])$coefficients[2])),
         p = from,
         q = to)

  ## as a multiple from/to input
  rates <- mapply(function(p,q) as.numeric(lm(sardine.rd$Oxygen[(p+1):(q+1)]~sardine.rd$Time[(p+1):(q+1)])$coefficients[2]),
                  p = from,
                  q = to)
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               rates)

})

test_that("calc_rate - by = 'time' outputs correct results with oxygen production data", {
  by <- "time"

  sardine_rev <- sardine.rd
  sardine_rev[[2]] <- rev(sardine_rev[[2]])

  from <- 1000
  to <- 1999
  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sardine_rev$Oxygen[(from+1):(to+1)]~sardine_rev$Time[(from+1):(to+1)])$coefficients[2]))

  ## try a bunch of values
  from <- round(seq(1000, 5000, length.out = 20))
  to <- round(seq(2000, 7000, length.out = 20))
  mapply(function(p,q) expect_equal(calc_rate(sardine_rev, from = p, to = q, plot = F, by = by)$rate,
                                    as.numeric(lm(sardine_rev$Oxygen[(p+1):(q+1)]~sardine_rev$Time[(p+1):(q+1)])$coefficients[2])),
         p = from,
         q = to)

  ## as a multiple from/to input
  rates <- mapply(function(p,q) as.numeric(lm(sardine_rev$Oxygen[(p+1):(q+1)]~sardine_rev$Time[(p+1):(q+1)])$coefficients[2]),
                  p = from,
                  q = to)
  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               rates)

  ## same region from reversed data outputs same rate
  from <- 1000
  to <- 1999
  from_rev <- nrow(sardine.rd)-to-1
  to_rev <- nrow(sardine.rd)-from-1
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               calc_rate(sardine_rev, from = from_rev, to = to_rev, plot = F, by = by)$rate * -1)
  ## try a bunch of values
  from <- round(seq(1000, 5000, length.out = 20))
  to <- round(seq(2000, 7000, length.out = 20))
  from_rev <- nrow(sardine.rd)-to-1
  to_rev <- nrow(sardine.rd)-from-1
  mapply(function(p,q,r,s) expect_equal(calc_rate(sardine.rd, from = p, to = q, plot = F, by = by)$rate,
                                    calc_rate(sardine_rev, from = r, to = s, plot = F, by = by)$rate * -1),
         p = from,
         q = to,
         r = from_rev,
         s = to_rev)

})

# by = "row" checks -------------------------------------------------------

test_that("calc_rate - by = 'row' errors with row values out of order", {
  by <- "row"

  expect_error(calc_rate(sardine.rd, from = 600, to = 500, plot = F, by = by),
               "calc_rate: some 'from' row numbers are higher than the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd, from = c(1200, 1600), to = c(1300, 1500), plot = F, by = by),
               "calc_rate: some 'from' row numbers are higher than the paired values in 'to'.")
  expect_error(calc_rate(sardine.rd, from = c(1200, 1600, 1800), to = c(1300, 1500, 1900), plot = F, by = by),
               "calc_rate: some 'from' row numbers are higher than the paired values in 'to'.")
})

test_that("calc_rate - by = 'row' stops with 'from' row numbers out of range of available data", {
  by <- "row"
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 2000, to = 2500, plot = F, by = by),
               "calc_rate: some 'from' row numbers are beyond the number of rows present in 'x'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(100, 200, 1100), to = c(300, 400, 1400), plot = F, by = by),
               "calc_rate: some 'from' row numbers are beyond the number of rows present in 'x'.")
})

test_that("calc_rate - by = 'row' warns with 'to' row values out of range, but uses last row value", {
  by <- "row"

  expect_message(calc_rate(sardine.rd[1000:2000,], from = 900, to = 1500, plot = F, by = by),
                 "calc_rate: some 'to' row numbers are higher than the number of rows present in 'x'. The final row number will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd[1000:2000,], from = 900, to = 1500, plot = F, by = by)$summary$endrow[1]),
               1001)
  expect_message(calc_rate(sardine.rd[1000:2000,], from = c(500, 600, 700), to = c(900, 1000, 1100), plot = F, by = by),
                 "calc_rate: some 'to' row numbers are higher than the number of rows present in 'x'. The final row number will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd[1000:2000,], from = c(500, 600, 700), to = c(900, 1000, 1100), plot = F, by = by)$summary$endrow[1:3]),
               c(900,1000,1001))
})

test_that("calc_rate - by = 'row' outputs correct results", {
  by <- "row"

  from <- 1000
  to <- 1999
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sardine.rd$Oxygen[(from):(to)]~sardine.rd$Time[(from):(to)])$coefficients[2]))

  ## try a bunch of values
  from <- round(seq(1000, 5000, length.out = 20))
  to <- round(seq(2000, 7000, length.out = 20))
  mapply(function(p,q) expect_equal(calc_rate(sardine.rd, from = p, to = q, plot = F, by = by)$rate,
                                    as.numeric(lm(sardine.rd$Oxygen[(p):(q)]~sardine.rd$Time[(p):(q)])$coefficients[2])),
         p = from,
         q = to)

  ## as a multiple from/to input
  rates <- mapply(function(p,q) as.numeric(lm(sardine.rd$Oxygen[(p):(q)]~sardine.rd$Time[(p):(q)])$coefficients[2]),
                  p = from,
                  q = to)
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               rates)

})

test_that("calc_rate - by = 'row' outputs correct results with oxygen production data", {
  by <- "row"

  sardine_rev <- sardine.rd
  sardine_rev[[2]] <- rev(sardine_rev[[2]])

  from <- 1000
  to <- 1999
  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sardine_rev$Oxygen[(from):(to)]~sardine_rev$Time[(from):(to)])$coefficients[2]))

  ## try a bunch of values
  from <- round(seq(1000, 5000, length.out = 20))
  to <- round(seq(2000, 7000, length.out = 20))
  mapply(function(p,q) expect_equal(calc_rate(sardine_rev, from = p, to = q, plot = F, by = by)$rate,
                                    as.numeric(lm(sardine_rev$Oxygen[(p):(q)]~sardine_rev$Time[(p):(q)])$coefficients[2])),
         p = from,
         q = to)

  ## as a multiple from/to input
  rates <- mapply(function(p,q) as.numeric(lm(sardine_rev$Oxygen[(p):(q)]~sardine_rev$Time[(p):(q)])$coefficients[2]),
                  p = from,
                  q = to)
  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               rates)

  ## same region from reversed data outputs same rate
  from <- 1000
  to <- 1999
  from_rev <- nrow(sardine.rd)-to+1
  to_rev <- nrow(sardine.rd)-from+1
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               calc_rate(sardine_rev, from = from_rev, to = to_rev, plot = F, by = by)$rate * -1)
  ## try a bunch of values
  from <- round(seq(1000, 5000, length.out = 20))
  to <- round(seq(2000, 7000, length.out = 20))
  from_rev <- nrow(sardine.rd)-to+1
  to_rev <- nrow(sardine.rd)-from+1
  mapply(function(p,q,r,s) expect_equal(calc_rate(sardine.rd, from = p, to = q, plot = F, by = by)$rate,
                                        calc_rate(sardine_rev, from = r, to = s, plot = F, by = by)$rate * -1),
         p = from,
         q = to,
         r = from_rev,
         s = to_rev)
})

# by = "oxygen" checks --------------------------------------------------------

test_that("calc_rate - by = 'oxygen' stops with paired values of from and to *both* below or *both* above oxygen data range", {
  by <- "oxygen"
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 90, to = 85, plot = F, by = by),
               "calc_rate: some paired 'from' and 'to' values are both below the range of oxygen data in 'x'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(90,93.9), to = c(85,94.4), plot = F, by = by),
               "calc_rate: some paired 'from' and 'to' values are both below the range of oxygen data in 'x'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = 100, to = 98, plot = F, by = by),
               "calc_rate: some paired 'from' and 'to' values are both above the range of oxygen data in 'x'.")
  expect_error(calc_rate(sardine.rd[1000:2000,], from = c(100,93.9), to = c(98,94.4), plot = F, by = by),
               "calc_rate: some paired 'from' and 'to' values are both above the range of oxygen data in 'x'.")
})

test_that("calc_rate - by = 'oxygen' warns with oxygen values out of range, but uses closest available value", {
  by <- "oxygen"
  # range is 89.7 to 95.7
  # single values, one above
  expect_message(calc_rate(sardine.rd, from = 100, to = 93, plot = F, by = by),
                 "calc_rate: some 'from' oxygen values are higher than the values in 'x'. The highest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 100, to = 93, plot = F, by = by)$summary$time[1]),
               0)
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 100, to = 93, plot = F, by = by)$summary$row[1]),
               1)
  expect_message(calc_rate(algae.rd, from = 92, to = 97, plot = F, by = by),
                 "calc_rate: some 'from' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(algae.rd, from = 92, to = 97, plot = F, by = by)$summary$time[1]),
               0.02)
  expect_equal(suppressWarnings(calc_rate(algae.rd, from = 92, to = 97, plot = F, by = by)$summary$row[1]),
               1)
  # single values, one below
  expect_message(calc_rate(sardine.rd, from = 95, to = 80, plot = F, by = by),
                 "calc_rate: some 'to' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 95, to = 80, plot = F, by = by)$summary$endtime[1]),
               sardine.rd$Time[nrow(sardine.rd)])
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 95, to = 80, plot = F, by = by)$summary$endrow[1]),
               nrow(sardine.rd))
  # single values, one above, one below
  expect_message(calc_rate(sardine.rd, from = 100, to = 80, plot = F, by = by),
                 "calc_rate: some 'to' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
  expect_message(calc_rate(sardine.rd, from = 100, to = 80, plot = F, by = by),
                 "calc_rate: some 'from' oxygen values are higher than the values in 'x'. The highest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 100, to = 80, plot = F, by = by)$summary$time[1]),
               0)
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 100, to = 80, plot = F, by = by)$summary$row[1]),
               1)
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 100, to = 80, plot = F, by = by)$summary$endtime[1]),
               sardine.rd$Time[nrow(sardine.rd)])
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = 100, to = 80, plot = F, by = by)$summary$endrow[1]),
               nrow(sardine.rd))

  # multiple values, one above
  expect_message(calc_rate(sardine.rd, from = c(100,95), to = c(93,92), plot = F, by = by),
                 "calc_rate: some 'from' oxygen values are higher than the values in 'x'. The highest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(100,95), to = c(93,92), plot = F, by = by)$summary$time[1]),
               0)
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(100,95), to = c(93,92), plot = F, by = by)$summary$row[1]),
               1)
  # multiple values, one below
  expect_message(calc_rate(sardine.rd, from = c(95,94), to = c(80,92), plot = F, by = by),
                 "calc_rate: some 'to' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(95,94), to = c(80,92), plot = F, by = by)$summary$endtime[1]),
               sardine.rd$Time[nrow(sardine.rd)])
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(95,94), to = c(80,92), plot = F, by = by)$summary$endrow[1]),
               nrow(sardine.rd))
  # multiple values, one above, one below
  expect_message(calc_rate(sardine.rd, from = c(100,94), to = c(80,92), plot = F, by = by),
                 "calc_rate: some 'to' oxygen values are lower than the values in 'x'. The lowest available value will be used instead.")
  expect_message(calc_rate(sardine.rd, from = c(100,94), to = c(80,92), plot = F, by = by),
                 "calc_rate: some 'from' oxygen values are higher than the values in 'x'. The highest available value will be used instead.")
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(100,94), to = c(80,92), plot = F, by = by)$summary$time[1]),
               0)
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(100,94), to = c(80,92), plot = F, by = by)$summary$row[1]),
               1)
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(100,94), to = c(80,92), plot = F, by = by)$summary$endtime[1]),
               sardine.rd$Time[nrow(sardine.rd)])
  expect_equal(suppressWarnings(calc_rate(sardine.rd, from = c(100,94), to = c(80,92), plot = F, by = by)$summary$endrow[1]),
               nrow(sardine.rd))
})

test_that("calc_rate - by = 'oxygen' outputs correct results", {
  by <- "oxygen"

  from <- 94
  to <- 91

  sub <- sardine.rd[min(which(dplyr::between(sardine.rd[[2]], to, from))):max(which(dplyr::between(sardine.rd[[2]], to, from))),]
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))

  ## try a bunch of values
  #from <- (seq(95, 91, length.out = 20))
  #to <- (seq(94, 90, length.out = 20))
  from <- runif(100, max = 96, min = 93)
  to <- runif(100, max = 93, min = 90)

  for(i in 1:length(from)){
    sub <- sardine.rd[min(which(dplyr::between(sardine.rd[[2]], to[i], from[i]))):max(which(dplyr::between(sardine.rd[[2]], to[i], from[i]))),]
    expect_equal(calc_rate(sardine.rd, from = from[i], to = to[i], plot = F, by = by)$rate,
                 as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))
  }

  ## as a multiple from/to input
  rates <- c()
  for(i in 1:length(from)){
    sub <- sardine.rd[min(which(dplyr::between(sardine.rd[[2]], to[i], from[i]))):max(which(dplyr::between(sardine.rd[[2]], to[i], from[i]))),]
    rates[i] <- as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2])
  }

  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               rates)

})

test_that("calc_rate - by = 'oxygen' - check 'from' and 'to' are interchangeable", {
  by <- "oxygen"

  from <- 94
  to <- 91

  sub <- sardine.rd[min(which(dplyr::between(sardine.rd[[2]], to, from))):max(which(dplyr::between(sardine.rd[[2]], to, from))),]
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               calc_rate(sardine.rd, from = to, to = from, plot = F, by = by)$rate)

  ## try a bunch of values
  from <- runif(100, max = 96, min = 93)
  to <- runif(100, max = 93, min = 90)

  for(i in 1:length(from)){
    sub <- sardine.rd[min(which(dplyr::between(sardine.rd[[2]], to[i], from[i]))):max(which(dplyr::between(sardine.rd[[2]], to[i], from[i]))),]
    expect_equal(calc_rate(sardine.rd, from = from[i], to = to[i], plot = F, by = by)$rate,
                 calc_rate(sardine.rd, from = to[i], to = from[i], plot = F, by = by)$rate)
  }

})

test_that("calc_rate - by = 'oxygen' outputs correct results with oxygen production data", {
  by <- "oxygen"

  sardine_rev <- sardine.rd
  sardine_rev[[2]] <- rev(sardine_rev[[2]])

  from <- 94
  to <- 91

  sub <- sardine_rev[min(which(dplyr::between(sardine_rev[[2]], to, from))):max(which(dplyr::between(sardine_rev[[2]], to, from))),]
  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))

  ## try a bunch of values
  from <- runif(100, max = 96, min = 93)
  to <- runif(100, max = 93, min = 90)

  for(i in 1:length(from)){
    sub <- sardine_rev[min(which(dplyr::between(sardine_rev[[2]], to[i], from[i]))):max(which(dplyr::between(sardine_rev[[2]], to[i], from[i]))),]
    expect_equal(calc_rate(sardine_rev, from = from[i], to = to[i], plot = F, by = by)$rate,
                 as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))
  }

  ## as a multiple from/to input
  rates <- c()
  for(i in 1:length(from)){
    sub <- sardine_rev[min(which(dplyr::between(sardine_rev[[2]], to[i], from[i]))):max(which(dplyr::between(sardine_rev[[2]], to[i], from[i]))),]
    rates[i] <- as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2])
  }

  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               rates)

  ## same region from reversed data outputs same rate
  from <- runif(100, max = 96, min = 93)
  to <- runif(100, max = 93, min = 90)
  from_rev <- to
  to_rev <- from

  mapply(function(p,q,r,s) expect_equal(calc_rate(sardine.rd, from = p, to = q, plot = F, by = by)$rate,
                                        calc_rate(sardine_rev, from = r, to = s, plot = F, by = by)$rate * -1),
         p = from,
         q = to,
         r = from_rev,
         s = to_rev)
})


# by = "proportion" checks ------------------------------------------------

test_that("calc_rate - by = 'proportion' errors with to or from not between 0 and 1", {
  by <- "proportion"

  expect_error(calc_rate(sardine.rd, from = 10, to = 0, plot = F, by = by),
               "calc_rate: for by = 'proportion' method, all 'from' values should be between 0 and 1.")
  expect_error(calc_rate(sardine.rd, from = 0.9, to = 10, plot = F, by = by),
               "calc_rate: for by = 'proportion' method, all 'to' values should be between 0 and 1.")
  expect_error(calc_rate(sardine.rd, from = 10, to = 11, plot = F, by = by),
               "calc_rate: for by = 'proportion' method, all 'from' values should be between 0 and 1.")

  expect_error(calc_rate(sardine.rd, from = c(10,0.6), to = c(0,0.3), plot = F, by = by),
               "calc_rate: for by = 'proportion' method, all 'from' values should be between 0 and 1.")
  expect_error(calc_rate(sardine.rd, from = c(0.9,0.6), to = c(10,0.3), plot = F, by = by),
               "calc_rate: for by = 'proportion' method, all 'to' values should be between 0 and 1.")
  expect_error(calc_rate(sardine.rd, from = c(10,0.6), to = c(12,0.3), plot = F, by = by),
               "calc_rate: for by = 'proportion' method, all 'from' values should be between 0 and 1.")
})

test_that("calc_rate - by = 'proportion' outputs correct results", {
  by <- "proportion"

  from <- 0.8
  to <- 0.2

  mx <- max(sardine.rd[[2]])
  mn <- min(sardine.rd[[2]])

  lower <- sort(c(from, to))[1]
  upper <- sort(c(from, to))[2]
  min <- min(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
  max <- max(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))

  sub <- sardine.rd[min:max,]
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))

  ## try a bunch of values
  #from <- (seq(95, 91, length.out = 20))
  #to <- (seq(94, 90, length.out = 20))
  from <- runif(100, max = 1, min = 0.5)
  to <- runif(100, max = 0.5, min = 0)

  for(i in 1:length(from)){
    lower <- sort(c(from[i], to[i]))[1]
    upper <- sort(c(from[i], to[i]))[2]
    min <- min(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    max <- max(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    sub <- sardine.rd[min:max,]
    expect_equal(calc_rate(sardine.rd, from = from[i], to = to[i], plot = F, by = by)$rate,
                 as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))
  }

  ## as a multiple from/to input
  rates <- c()
  for(i in 1:length(from)){
    lower <- sort(c(from[i], to[i]))[1]
    upper <- sort(c(from[i], to[i]))[2]
    min <- min(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    max <- max(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    sub <- sardine.rd[min:max,]
    rates[i] <- as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2])
  }

  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               rates)

})

test_that("calc_rate - by = 'proportion' - check 'from' and 'to' are interchangeable", {
  by <- "proportion"

  from <- 0.8
  to <- 0.2

  mx <- max(sardine.rd[[2]])
  mn <- min(sardine.rd[[2]])

  lower <- sort(c(from, to))[1]
  upper <- sort(c(from, to))[2]
  min <- min(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
  max <- max(which(dplyr::between(sardine.rd[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))

  sub <- sardine.rd[min:max,]
  expect_equal(calc_rate(sardine.rd, from = from, to = to, plot = F, by = by)$rate,
               calc_rate(sardine.rd, from = to, to = from, plot = F, by = by)$rate)

  ## try a bunch of values
  #from <- (seq(95, 91, length.out = 20))
  #to <- (seq(94, 90, length.out = 20))
  from <- runif(100, max = 1, min = 0.5)
  to <- runif(100, max = 0.5, min = 0)

  for(i in 1:length(from)){
    expect_equal(calc_rate(sardine.rd, from = from[i], to = to[i], plot = F, by = by)$rate,
                 calc_rate(sardine.rd, from = to[i], to = from[i], plot = F, by = by)$rate)
  }
})

test_that("calc_rate - by = 'proportion' outputs correct results", {
  by <- "proportion"

  sardine_rev <- sardine.rd
  sardine_rev[[2]] <- rev(sardine_rev[[2]])

  from <- 0.8
  to <- 0.2

  mx <- max(sardine_rev[[2]])
  mn <- min(sardine_rev[[2]])

  lower <- sort(c(from, to))[1]
  upper <- sort(c(from, to))[2]
  min <- min(which(dplyr::between(sardine_rev[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
  max <- max(which(dplyr::between(sardine_rev[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))

  sub <- sardine_rev[min:max,]
  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))

  ## try a bunch of values
  #from <- (seq(95, 91, length.out = 20))
  #to <- (seq(94, 90, length.out = 20))
  from <- runif(100, max = 1, min = 0.5)
  to <- runif(100, max = 0.5, min = 0)

  for(i in 1:length(from)){
    lower <- sort(c(from[i], to[i]))[1]
    upper <- sort(c(from[i], to[i]))[2]
    min <- min(which(dplyr::between(sardine_rev[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    max <- max(which(dplyr::between(sardine_rev[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    sub <- sardine_rev[min:max,]
    expect_equal(calc_rate(sardine_rev, from = from[i], to = to[i], plot = F, by = by)$rate,
                 as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2]))
  }

  ## as a multiple from/to input
  rates <- c()
  for(i in 1:length(from)){
    lower <- sort(c(from[i], to[i]))[1]
    upper <- sort(c(from[i], to[i]))[2]
    min <- min(which(dplyr::between(sardine_rev[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    max <- max(which(dplyr::between(sardine_rev[[2]], (lower * (mx - mn) + mn), (upper * (mx - mn) + mn))))
    sub <- sardine_rev[min:max,]
    rates[i] <- as.numeric(lm(sub[[2]]~sub[[1]])$coefficients[2])
  }

  expect_equal(calc_rate(sardine_rev, from = from, to = to, plot = F, by = by)$rate,
               rates)

  ## same region from reversed data outputs same rate
  from <- runif(100, max = 1, min = 0.5)
  to <- runif(100, max = 0.5, min = 0)
  from_rev <- to
  to_rev <- from

  mapply(function(p,q,r,s) expect_equal(calc_rate(sardine.rd, from = p, to = q, plot = F, by = by)$rate,
                                        calc_rate(sardine_rev, from = r, to = s, plot = F, by = by)$rate * -1),
         p = from,
         q = to,
         r = from_rev,
         s = to_rev)

})

test_that("calc_rate - plot defaults are correctly restored", {

  # reset plotting first
  dev.off()
  # save par before
  parb4 <- par(no.readonly = TRUE)
  # now use a fn with plot
  suppressWarnings(calc_rate(urchins.rd))
  # save after
  paraft <- par(no.readonly = TRUE)
  # mai is something changed from the default,
  # so if par settings not restored properly this should fail
  expect_identical(parb4$mai,
                   paraft$mai)

})

}) ## turns printing back on
