## library(testthat)
## test_file("tests/testthat/test-inspect.R")
## covr::file_coverage("R/inspect.R", "tests/testthat/test-inspect.R")

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


# suppressWarnings(file.remove("Rplots.pdf"))


test_that("inspect objects can be printed", {
  expect_output(print(ur2c))
  expect_output(print(ur3c))
})

test_that("inspect works with NULL inputs", {
  expect_error(inspect(intermittent.rd, time = NULL, plot = F),
               regexp = NA)
  expect_error(inspect(intermittent.rd, oxygen = NULL, plot = F),
               regexp = NA)
})


test_that("inspect stops if input not df", {
  expect_error(inspect(as.matrix(urchins.rd), plot = F),
               "`df` must be data.frame object.")
  expect_error(inspect(urchins.rd[[1]], plot = F),
               "`df` must be data.frame object.")
  expect_error(inspect(3435, plot = F),
               "`df` must be data.frame object.")
})

test_that("inspect stops if time/oxygen/width inputs out of range", {
  expect_error(inspect(urchins.rd, time = 0.2, plot = F),
               "`time` column: must be numeric integer.")
  expect_error(inspect(urchins.rd, time = 1, oxygen = 0.5, plot = F),
               "`oxygen` column\\(s): must be numeric integers.")
  expect_error(inspect(urchins.rd, width = 1.5, plot = F),
               "`width` must be between 0 and 1.")
})


# -------------------------------------------------------------------------

test_that("inspect: unevenly spaced time detected",
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
          expect_message(inspect(input, plot = F),
                        "No issues detected while inspecting data frame."))


