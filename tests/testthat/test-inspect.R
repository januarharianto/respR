# library(testthat)
# test_file("tests/testthat/test-inspect.R")
# covr::file_coverage("R/inspect.R", "tests/testthat/test-inspect.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)
# covr::report(covr::package_coverage())

capture.output({  ## stops printing console outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()

  { # Testing objects
    base <- select(intermittent.rd, 1,2)
    base[[3]] <- intermittent.rd[[2]]
    names(base) <- c("Time", "oxy1", "oxy2")
  }

  test_that("inspect - works on 2-column data",
            expect_error(inspect(sardine.rd, plot = F),
                         regexp = NA))

  test_that("inspect - works on multi-column data", {
    ## defaults to columns 1:2
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
  test_that("inspect - produces plot with 2-column data", {
    expect_output(plot(ur2c))
    expect_error(plot(ur2c),
                 regexp = NA)
  })

  ur3c <- suppressWarnings(inspect(urchins.rd, oxygen = 2:3, plot = F))
  test_that("inspect - produces plot with multi-column data",
            expect_error(plot(ur3c),
                         regexp = NA))

  ur2c <- suppressWarnings(inspect(urchins.rd,
                                   width = 0.5, plot = F))
  test_that("inspect - produces plot with higher width values",
            expect_error(plot(ur2c),
                         regexp = NA))

  test_that("inspect - produces plot with extra options passed",
            expect_error(plot(ur2c, legend = FALSE, rate.rev = FALSE,
                              quiet = TRUE, width = 0.05),
                         regexp = NA))

  urmultrates <- suppressWarnings(inspect(urchins.rd, time =1, oxygen = 2:8,
                                          plot = F))
  test_that("inspect - gives multiple column message", {
    expect_message((plot(urmultrates)),
                   regexp = "inspect: Rolling Regression plot is only avalilable for a 2-column dataframe output.")
  })

  test_that("inspect - produces plot with 'pos' input passed", {
    expect_output(suppressWarnings(plot(urmultrates, pos = 2)))
    expect_error(suppressWarnings(plot(urmultrates, pos = 2)),
                 regexp = NA)
  })

  test_that("inspect - produces plot with 'add.data' input passed", {
    expect_output(suppressWarnings(inspect(urchins.rd, time =1, oxygen = 2, add.data = 3,
                                           plot = T)))
    expect_error(suppressWarnings(inspect(urchins.rd, time =1, oxygen = 2, add.data = 3)),
                 NA)
    expect_output(suppressWarnings(inspect(urchins.rd, time =1, oxygen = 2:3, add.data = 3,
                                           plot = T)))
    expect_message(suppressWarnings(inspect(urchins.rd, time =1, oxygen = 2:3, add.data = 3,
                                           plot = T)),
                   "plot.inspect: Additional data source cannot be plotted for multiple columns.")
  })

  test_that("inspect - gives error with plot and 'pos' too high", {
    expect_error((plot(urmultrates, pos = 100)),
                 regexp = "plot.inspect: Invalid 'pos' rank: only 7 oxygen columns found.")
  })


  # suppressWarnings(file.remove("Rplots.pdf"))

  test_that("inspect - objects can be printed", {
    expect_output(print(ur2c))
    expect_output(print(ur3c))
    expect_output(summary(ur2c))
    expect_output(summary(ur3c))
  })

  test_that("inspect - mean S3 returns message", {
    expect_message(mean(ur2c),
                   "inspect: mean\\() is not available for 'inspect' objects.")
  })

  test_that("inspect - objects with lots of results (20+) can be printed", {
    # time Inf
    urch <- urchins.rd[,1:2]
    urch$time.min[c(2:4, 30:35, 120:127, 180:190, 200:204)] <- Inf
    urch_insp <- suppressWarnings(inspect(urch, plot = F))
    expect_output(print(urch_insp))
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
    # oxy Inf
    urch <- urchins.rd[,1:2]
    urch$a[c(2:4, 30:35, 120:127, 180:190, 200:204)] <- Inf
    urch_insp <- suppressWarnings(inspect(urch))
    expect_output(print(urch_insp))
    # oxy NA
    urch <- urchins.rd[,1:2]
    urch$a[c(2:4, 30:35, 120:127, 180:190, 200:204)] <- NA
    urch_insp <- suppressWarnings(inspect(urch))
    expect_output(print(urch_insp))
  })

  test_that("inspect - works with NULL inputs", {
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

  test_that("inspect - stops if input not df", {
    expect_error(inspect(as.matrix(urchins.rd), plot = F),
                 "inspect: 'x' must be data.frame object.")
    expect_error(inspect(urchins.rd[[1]], plot = F),
                 "inspect: 'x' must be data.frame object.")
    expect_error(inspect(3435, plot = F),
                 "inspect: 'x' must be data.frame object.")
  })

  test_that("inspect - stops if time/oxygen/width column inputs malformed", {
    # not integer
    expect_error(inspect(urchins.rd, time = 0.2, plot = F),
                 "inspect: 'time' - some column inputs are not integers.")
    # too many
    expect_error(inspect(urchins.rd, time = 1:2, plot = F),
                 "inspect: 'time' - cannot enter more than 1 column\\(s\\) with this input or this dataset.")
    # out of range
    expect_error(inspect(urchins.rd, time = 20, plot = F),
                 "inspect: 'time' - one or more column inputs are out of range of allowed data columns.")

    # conflicts
    expect_error(inspect(urchins.rd, time = 2, oxygen = 2, plot = F),
                 "inspect: 'oxygen' - one or more column inputs conflicts with other inputs.")
    expect_error(inspect(urchins.rd, time = 1, oxygen = 0.5, plot = F),
                 "inspect: 'oxygen' - some column inputs are not integers.")
    expect_error(inspect(urchins.rd, time = 1, oxygen = 2:20, plot = F),
                 "inspect: 'oxygen' - one or more column inputs are out of range of allowed data columns.")
    expect_error(inspect(urchins.rd, time = 3, oxygen = 3, plot = F),
                 "inspect: 'oxygen' - one or more column inputs conflicts with other inputs.")
    expect_error(inspect(urchins.rd, width = 1.5, plot = F),
                 "inspect: 'width' - one or more inputs are outside the range of allowed values.")
    expect_error(inspect(urchins.rd, width = 2:20, plot = F),
                 "inspect: 'width' - only 1 inputs allowed.")
    expect_error(inspect(urchins.rd, width = "string", plot = F),
                 "inspect: 'width' - input is not numeric.")
  })

  test_that("inspect - correctly identifies column name strings", {
    # single time and oxygen columns work without error
    expect_error(inspect(base, time = "Time", oxygen = "oxy1", plot = FALSE),
                 NA)
    # same as using numbers
    # Everything except the $call should be the same
    expect_equal(inspect(base, time = "Time", oxygen = "oxy1", plot = FALSE)[2:7],
                 inspect(base, time = 1, oxygen = 2, plot = FALSE)[2:7])

    # multiple oxygen columns
    expect_error(inspect(base, time = "Time", oxygen = c("oxy1", "oxy2"), plot = FALSE),
                 NA)
    # same as using numbers
    # Everything except the $call should be the same
    expect_equal(inspect(base, time = "Time", oxygen = c("oxy1", "oxy2"), plot = FALSE)[2:7],
                 inspect(base, time = 1, oxygen = 2:3, plot = FALSE)[2:7])

    # add.data
    expect_error(inspect(base, time = 1, oxygen = 2, add.data = "oxy2", plot = FALSE),
                 NA)
    # same as using numbers
    # Everything except the $call should be the same
    expect_equal(inspect(base, time = 1, oxygen = 2, add.data = "oxy2", plot = FALSE)$add.data,
                 inspect(base, time = 1, oxygen = 2, add.data = 3, plot = FALSE)$add.data)

  })

  test_that("inspect - unevenly spaced time detected message", {
    expect_warning(inspect(urchins.rd, plot = F),
                   "Time values are not evenly-spaced \\(numerically).")
  })

  test_that("inspect - Non-numeric in time detected", {
    input <- as.data.frame(base)
    input[100,1] <- "99"

    expect_warning(inspect(input, plot = T),
                   "inspect: Time column not numeric. Other column checks skipped.")
  })

  test_that("inspect - Inf in time detected", {
    input <- as.data.frame(base)
    input[100,1] <- Inf
    input[200:205,1] <- -Inf

    expect_warning(inspect(input, plot = F),
                   "inspect: Inf/-Inf values detected in Time column. Remove or replace before proceeding.")
  })

  test_that("inspect - NA in time detected", {
    input <- base
    input[100,1] <- NA
    input[200:205,1] <- NA
    expect_warning(inspect(input, plot = F),
                   "NA/NaN values detected in Time column.")
  })

  test_that("inspect - Non-numeric in oxygen detected", {
    input <- as.data.frame(base)
    input[100,2] <- "7.09"
    expect_warning(inspect(input, plot = T),
                   "inspect: Oxygen column\\(s) not numeric. Other column checks skipped.")
  })

  test_that("inspect - Inf in oxygen detected", {
    input <- as.data.frame(base)
    input[100,2] <- Inf
    input[200:205,2] <- -Inf
    expect_warning(inspect(input, plot = F),
                   "inspect: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  })

  test_that("inspect - NA in oxygen detected", {
    input <- base
    input[100,2] <- NA
    input[200:205,2] <- NA
    expect_warning(inspect(input, plot = F),
                   "NA/NaN values detected in Oxygen column\\(s).")
  })

  test_that("inspect - NA in oxygen detected in multiple columns", {
    input <- base
    input[100,2] <- NA
    input[200:205,3] <- NA
    expect_warning(inspect(input, plot = F),
                   "NA/NaN values detected in Oxygen column\\(s).")
  })

  test_that("inspect - non-sequential time detected", {
    input <- base
    input[9,1] <- 9
    input[10,1] <- 8
    input[325,1] <- 325
    input[326,1] <- 324
    expect_warning(inspect(input, plot = F),
                   "Non-sequential Time values found.")
  })

  test_that("inspect - non-sequential time detected", {
    input <- base
    input[12,1] <- 10
    input[100,1] <- 98
    expect_warning(inspect(input, plot = F),
                   "Duplicate Time values found.")
  })

  test_that("inspect - all good message if no errors", {
    input <- base
    expect_message(suppressWarnings(inspect(input, plot = F)),
                   "No issues detected while inspecting data frame.")
  })

  test_that("inspect - plot defaults are correctly restored", {

    # reset plotting first
    dev.off()
    # save par before
    parb4 <- par(no.readonly = TRUE)
    # now use a fn with plot
    inspect(sardine.rd, 1, 2)
    # save after
    paraft <- par(no.readonly = TRUE)
    # mai is something changed from the default,
    # so if par settings not restored properly this should fail
    expect_identical(parb4$mai,
                     paraft$mai)

  })

}) ## turns console printing back on
