# library(testthat)
# test_file("tests/testthat/test-inspect.R")
# covr::file_coverage("R/inspect.R", "tests/testthat/test-inspect.R")
# x <- covr::package_coverage()
# covr::report(x)

sink("/dev/null") ## stops printing console outputs on assigning

test_that("inspect works on 2-column data",
          expect_error(inspect(sardine.rd, plot = F),
                       regexp = NA))

test_that("inspect works on multi-column data", {
  ## default first 2 columns
  expect_equal(ncol(suppressWarnings(inspect(urchins.rd, plot = F)$dataframe)),
               2)
  ## differnt 2 columns
  expect_equal(ncol(suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 3, plot = F)$dataframe)),
               2)
  ## multiple columns
  expect_equal(ncol(suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 3:6, plot = F)$dataframe)),
               5)
})

ur2c <- suppressWarnings(inspect(urchins.rd, plot = F))
test_that("inspect produces plot with 2-column data",
          expect_error(plot(ur2c),
                       regexp = NA))

ur3c <- suppressWarnings(inspect(urchins.rd, oxygen = 2:3, plot = F))
test_that("inspect produces plot with multi-column data",
          expect_error(plot(ur3c),
                       regexp = NA))

ur2c <- suppressWarnings(inspect(urchins.rd,
                                 width = 0.5, plot = F))
test_that("inspect produces plot with higher width values",
          expect_error(plot(ur2c),
                       regexp = NA))

test_that("inspect produces plot with extra options passed",
          expect_error(plot(ur2c, legend = FALSE, rate.rev = FALSE,
                            message = FALSE, width = 0.05),
                       regexp = NA))


# suppressWarnings(file.remove("Rplots.pdf"))

test_that("inspect objects can be printed", {
  expect_output(print(ur2c))
  expect_output(print(ur3c))
})

test_that("inspect objects with lots of results (20+) can be printed", {
  # time NA
  urch <- urchins.rd[,1:2]
  urch$time.min[c(2:4, 30:35, 120:127, 180:190, 200:204)] <- NA
  urch_insp <- suppressWarnings(inspect(urch))
  expect_output(print(urch_insp))
  # time non seq
  urch <- urchins.rd[,1:2]
  urch$time.min[c(2:4, 30:35, 120:127, 180:190, 200:204)] <-
    rev(urch$time.min[c(2:4, 30:35, 120:127, 180:190, 200:204)])
  urch_insp <- suppressWarnings(inspect(urch))
  expect_output(print(urch_insp))
  # time dup
  urch <- urchins.rd[,1:2]
  urch$time.min[c(2:4, 30:35, 120:127, 180:190, 200:204)] <- 44.3
  urch_insp <- suppressWarnings(inspect(urch))
  expect_output(print(urch_insp))
  # oxy NA
  urch <- urchins.rd[,1:2]
  urch$a[c(2:4, 30:35, 120:127, 180:190, 200:204)] <- NA
  urch_insp <- suppressWarnings(inspect(urch))
  expect_output(print(urch_insp))
})

test_that("inspect works with NULL inputs", {
  expect_error(inspect(intermittent.rd, time = NULL, oxygen = NULL, width = NULL, plot = F),
               regexp = NA)

  expect_error(inspect(intermittent.rd, time = NULL, plot = F),
               regexp = NA)
  expect_message(inspect(intermittent.rd, time = NULL, plot = F),
                 regexp = "inspect: Applying column default of 'time = 1'")
  expect_equal(inspect(intermittent.rd, time = NULL, plot = F)$inputs$time,
               1)

  expect_error(inspect(intermittent.rd, oxygen = NULL, plot = F),
               regexp = NA)
  expect_message(inspect(intermittent.rd, time = NULL, plot = F),
                 regexp = "inspect: Applying column default of 'oxygen = 2'")
  expect_equal(inspect(intermittent.rd, time = NULL, plot = F)$inputs$oxygen,
               2)

  expect_error(inspect(intermittent.rd, width = NULL, plot = F),
               regexp = NA)
  expect_message(inspect(intermittent.rd, width = NULL, plot = F),
                 regexp = "inspect: Applying default of 'width = 0.1'")
  expect_equal(inspect(intermittent.rd, width = NULL, plot = F)$inputs$width,
               0.1)
})


test_that("inspect stops if input not df", {
  expect_error(inspect(as.matrix(urchins.rd), plot = F),
               "inspect: 'x' must be data.frame object.")
  expect_error(inspect(urchins.rd[[1]], plot = F),
               "inspect: 'x' must be data.frame object.")
  expect_error(inspect(3435, plot = F),
               "inspect: 'x' must be data.frame object.")
})

test_that("inspect stops if time/oxygen/width column inputs malformed", {
  # not integer
  expect_error(inspect(urchins.rd, time = 0.2, plot = F),
               "inspect: 'time' - some column inputs are not integers.")
  # too many
  expect_error(inspect(urchins.rd, time = 1:2, plot = F),
               "inspect: 'time' - cannot enter more than 1 column\\(s\\) with this input or this dataset.")
  # out of range
  expect_error(inspect(urchins.rd, time = 20, plot = F),
               "inspect: 'time' - one or more column inputs are out of range of allowed data columns.")
  # not numeric
  expect_error(inspect(urchins.rd, time = "string", plot = F),
               "inspect: 'time' - column input is not numeric.")
  # conflicts
  expect_error(inspect(urchins.rd, time = 2, oxygen = NULL, plot = F),
               "inspect: 'oxygen' - one or more column inputs conflicts with other inputs.")

  expect_error(inspect(urchins.rd, time = 1, oxygen = 0.5, plot = F),
               "inspect: 'oxygen' - some column inputs are not integers.")
  expect_error(inspect(urchins.rd, time = 1, oxygen = 2:20, plot = F),
               "inspect: 'oxygen' - one or more column inputs are out of range of allowed data columns.")
  expect_error(inspect(urchins.rd, time = 1, oxygen = "string", plot = F),
               "inspect: 'oxygen' - column input is not numeric.")
  expect_error(inspect(urchins.rd, time = 3, oxygen = 3, plot = F),
               "inspect: 'oxygen' - one or more column inputs conflicts with other inputs.")

  expect_error(inspect(urchins.rd, width = 1.5, plot = F),
               "inspect: 'width' - one or more inputs are outside the range of allowed values.")
  expect_error(inspect(urchins.rd, width = 2:20, plot = F),
               "inspect: 'width' - only 1 inputs allowed.")
  expect_error(inspect(urchins.rd, width = "string", plot = F),
               "inspect: 'width' - input is not numeric.")
})


# -------------------------------------------------------------------------

test_that("inspect: unevenly spaced time detected message",
          expect_warning(inspect(urchins.rd, plot = F),
                         "Time values are not evenly-spaced \\(numerically)."))

base <- select(intermittent.rd, 1,2)
base[[3]] <- intermittent.rd[[2]]

input <- base
input[100,1] <- NA
input[200:205,1] <- NA

test_that("inspect: NA in time detected",
          expect_warning(inspect(input, plot = F),
                         "NA/NaN values detected in Time column."))

input <- base
input[100,2] <- NA
input[200:205,2] <- NA
test_that("inspect: NA in oxygen detected",
          expect_warning(inspect(input, plot = F),
                         "NA/NaN values detected in Oxygen column\\(s)."))


input <- base
input[100,2] <- NA
input[200:205,3] <- NA
test_that("inspect: NA in oxygen detected in multiple columns",
          expect_warning(inspect(input, plot = F),
                         "NA/NaN values detected in Oxygen column\\(s)."))

input <- base
input[9,1] <- 9
input[10,1] <- 8
input[325,1] <- 325
input[326,1] <- 324
test_that("inspect: non-sequential time detected",
          expect_warning(inspect(input, plot = F),
                         "Non-sequential Time values found."))

input <- base
input[12,1] <- 10
input[100,1] <- 98
test_that("inspect: non-sequential time detected",
          expect_warning(inspect(input, plot = F),
                         "Duplicate Time values found."))

input <- base
test_that("inspect: all good message if no errors",
          expect_message(suppressWarnings(inspect(input, plot = F)),
                         "No issues detected while inspecting data frame."))


sink() ## turns console printing back on
