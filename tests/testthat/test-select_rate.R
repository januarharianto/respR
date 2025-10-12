# library(testthat)
# testthat::test_file("tests/testthat/test-select_rate.R")
# covr::file_coverage("R/select_rate.R", "tests/testthat/test-select_rate.R")
# Sys.setenv(NOT_CRAN = "true")
# cvr <- covr::package_coverage()
# covr::report(cvr)
# Sys.setenv(NOT_CRAN = "false")

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
  ## objects for testing
  ## convert_rate-from-auto_rate object
  ## contains both negative and positive rates
  {
    suppressWarnings(conv_rt_ar_obj <- inspect(intermittent.rd) %>%
                       auto_rate(plot = FALSE) %>%
                       convert_rate(oxy.unit = "mg/l",
                                    time.unit = "min",
                                    output.unit = "mg/h/g",
                                    volume = 2.379,
                                    mass = 0.006955))
    ## for testing with zero/non-zero change a couple to zero
    conv_rt_ar_obj_w_0 <- conv_rt_ar_obj
    conv_rt_ar_obj_w_0$rate[3] <- 0
    conv_rt_ar_obj_w_0$summary$rate[3] <- 0
    conv_rt_ar_obj_w_0$rate[9] <- 0
    conv_rt_ar_obj_w_0$summary$rate[9] <- 0

    ## make subset by method test objects
    suppressWarnings(conv_rt_ar_subset_pos <- select_rate(conv_rt_ar_obj, method = "positive"))
    suppressWarnings(conv_rt_ar_subset_neg <- select_rate(conv_rt_ar_obj, method = "negative"))
    suppressWarnings(conv_rt_ar_subset_nonzero <- select_rate(conv_rt_ar_obj_w_0, method = "nonzero"))
    suppressWarnings(conv_rt_ar_subset_zero <- select_rate(conv_rt_ar_obj_w_0, method = "zero"))


    ## objects to test subsetting of different auto_rate methods
    conv_rt_ar_obj_high <- auto_rate(urchins.rd[,1:2], method = "highest") %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)
    conv_rt_ar_obj_low <- auto_rate(urchins.rd[,1:2], method = "lowest") %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)
    conv_rt_ar_obj_maximum <- auto_rate(urchins.rd[,1:2], method = "maximum") %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)
    conv_rt_ar_obj_minimum <- auto_rate(urchins.rd[,1:2], method = "minimum") %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)
    conv_rt_ar_obj_max <- suppressWarnings(auto_rate(urchins.rd[,1:2], method = "max")) %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)
    conv_rt_ar_obj_min <- suppressWarnings(auto_rate(urchins.rd[,1:2], method = "min")) %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)
    conv_rt_ar_obj_int <- auto_rate(urchins.rd[,1:2], method = "interval") %>%
      convert_rate(oxy.unit = "mg/l", time.unit = "min", output.unit = "mg/h/g", volume = 2.379, mass = 0.006955)

    conv_rt_ar_obj_high_sub <- select_rate(conv_rt_ar_obj_high, method = "rsq", n = c(0.96,1))

    # large object
    conv_rt_ar_low_obj <- inspect(sardine.rd) %>%
      auto_rate(method = "lowest", plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 2.379,
                   mass = 0.006955)

    # intermittent-flow test data
    # regular reps - 10 reps from this dataset
    dt.reg.insp <- subset_data(zeb_intermittent.rd,
                               from = 5840,
                               to = 5840 + 6599,
                               by = "row") %>%
      inspect(legend = F, plot = T)

    # 10 reps, 1 rank in each
    conv_rt_ar.int_obj <- dt.reg.insp %>%
      calc_rate.int(starts = 660,
                    by = "time",
                    plot = F) %>%
      convert_rate(oxy.unit = "mg/L",
                   time.unit = "secs",
                   output.unit = "mg/h/g",
                   volume = 0.12,
                   mass = 0.0009)

    # 10 reps, multiple ranks in each
    conv_rt_ar.int_obj_mult_ranks <- dt.reg.insp %>%
      auto_rate.int(starts = 660,
                    wait = 120,
                    measure = 360,
                    by = "row",
                    plot = F,
                    n = 5,
                    width = 0.4) %>%
      convert_rate(oxy.unit = "mg/L",
                   time.unit = "secs",
                   output.unit = "mg/h/g",
                   volume = 0.12,
                   mass = 0.0009)
  }

  # General checks ----------------------------------------------------------

  test_that("select_rate - works with convert_rate-from-auto_rate input", {
    expect_error(select_rate(conv_rt_ar_obj, method = "positive"),
                 regexp = NA)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate input that has already been subset", {
    expect_error(select_rate(conv_rt_ar_subset_pos, method = "positive"),
                 regexp = NA)
  })

  test_that("select_rate - stops if input is not a convert_rate object", {
    expect_error(select_rate(calc_rate(intermittent.rd, from = 0, to = 1000,
                                       by = "row")),
                 regexp = "select_rate: Input is not a 'convert_rate' or 'convert_rate.ft' object")
    expect_error(select_rate(545),
                 regexp = "select_rate: Input is not a 'convert_rate' or 'convert_rate.ft' object")
  })

  test_that("select_rate - stops if wrong method", {
    expect_error(select_rate(conv_rt_ar_subset_pos, method = "blah"),
                 regexp = "select_rate: 'method' input not recognised")
  })

  test_that("select_rate - output inherits convert_rate and convert_rate_select class", {
    expect_is(conv_rt_ar_subset_pos,
              "convert_rate")
    expect_is(conv_rt_ar_subset_pos,
              "convert_rate_select")
  })

  test_that("select_rate - outputs can be plotted (as convert_rate objects)", {
    ##  (checking all here)
    for(i in 1:length(conv_rt_ar_subset_pos$rate.output)) expect_output(plot(conv_rt_ar_subset_pos, pos = i))
  })

  test_that("select_rate - summary and print generics work", {
    expect_output(summary(conv_rt_ar_subset_pos))
    expect_output(print(conv_rt_ar_subset_pos))
  })

  test_that("select_rate - works with pipes", {
    suppressMessages(piped_conv_rt_ar_subset_obj <- inspect(intermittent.rd) %>%
                       auto_rate(plot = FALSE) %>%
                       convert_rate(oxy.unit = "mg/l",
                                    time.unit = "min",
                                    output.unit = "mg/h/g",
                                    volume = 2.379,
                                    mass = 0.006955) %>%
                       select_rate(method = "positive"))

    expect_equal(piped_conv_rt_ar_subset_obj$rate, conv_rt_ar_subset_pos$rate)
  })

  test_that("select_rate - works chained by multiple pipes", {
    chain <- select_rate(conv_rt_ar_obj_w_0, method = "nonzero") %>%
      select_rate(method = "positive") %>%
      select_rate(method = "row", n = c(2000, 4100)) %>%
      select_rate(method = "highest", n = 2)

    expect_length(chain$rate.output, 2)
    expect_equal(chain$rate.output[1], 0.02212841)
  })

  test_that("select_rate - output has saved original convert_rate input", {
    expect_equal(conv_rt_ar_subset_neg$original,
                 conv_rt_ar_obj)
  })

  test_that("select_rate - output has saved subset calls", {
    expect_equal(as.character(conv_rt_ar_subset_neg$select_calls[[1]]),
                 c("select_rate", "conv_rt_ar_obj", "negative"))
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = highest object", {
    expect_error(select_rate(conv_rt_ar_obj_high, method = "rsq", n = c(0.96,1)),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(length(select_rate(conv_rt_ar_obj_high, method = "rsq", n = c(0.96,1))$rate.output),
                 38)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = lowest object", {
    expect_error(select_rate(conv_rt_ar_obj_low, method = "rsq", n = c(0.96,1)),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(length(select_rate(conv_rt_ar_obj_low, method = "rsq", n = c(0.96,1))$rate.output),
                 38)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = maximum object", {
    expect_error(select_rate(conv_rt_ar_obj_maximum, method = "rsq", n = c(0.96,1)),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(length(select_rate(conv_rt_ar_obj_maximum, method = "rsq", n = c(0.96,1))$rate.output),
                 38)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = minimum object", {
    expect_error(select_rate(conv_rt_ar_obj_minimum, method = "rsq", n = c(0.96,1)),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(length(select_rate(conv_rt_ar_obj_minimum, method = "rsq", n = c(0.96,1))$rate.output),
                 38)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = max object (OLD METHOD", {
    expect_error(select_rate(conv_rt_ar_obj_max, method = "rsq", n = c(0.96,1)),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(length(select_rate(conv_rt_ar_obj_max, method = "rsq", n = c(0.96,1))$rate.output),
                 38)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = min object (OLD METHOD)", {
    expect_error(select_rate(conv_rt_ar_obj_min, method = "rsq", n = c(0.96,1)),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(length(select_rate(conv_rt_ar_obj_min, method = "rsq", n = c(0.96,1))$rate.output),
                 38)
  })

  test_that("select_rate - works with convert_rate-from-auto_rate method = interval object (OLD METHOD", {
    expect_error(select_rate(conv_rt_ar_obj_int, method = "rsq", n = c(0.90,0.95)),
                 regexp = NA)
    ## should be 3 remaining - IN INTERVAL CASE ONLY
    expect_equal(length(select_rate(conv_rt_ar_obj_int, method = "rsq", n = c(0.90,0.95))$rate.output),
                 3)
  })

  test_that("select_rate - works with empty objects", {

    empty_obj <- select_rate(conv_rt_ar_obj, method = "rate", n = c(0.1, 1))
    empty_obj$summary
    empty_obj$rate.output
    # works with S3
    expect_output(print(empty_obj))
    expect_message(print(empty_obj),
                   "No rates found in convert_rate object.")
    expect_output(summary(empty_obj))
    expect_message(summary(empty_obj),
                   "No rates found in convert_rate object.")
    expect_output(mean(empty_obj))
    expect_message(mean(empty_obj),
                   "No rates found in convert_rate object.")

    # subsetting
    expect_error(select_rate(empty_obj, method = "rsq", n = c(0.90,0.95)),
                 regexp = NA)
    expect_message(select_rate(empty_obj, method = "rsq", n = c(0.90,0.95)),
                   regexp = "----- Selection complete. 0 rate\\(s) removed, 0 rate\\(s) remaining -----")
    # reordering
    expect_error(select_rate(empty_obj, method = "rsq", n = NULL),
                 regexp = NA)
    expect_message(select_rate(empty_obj, method = "rsq", n = NULL),
                   regexp = "----- Reordering complete. 0 rate\\(s) reordered by 'rsq' method -----")

  })

  # Check NULL stops fn --------------------------------------------
  test_that("select_rate - method = NULL   - correct message", {
    expect_error(conv_rt_ar_obj_uniq <- select_rate(conv_rt_ar_obj_high),
                 regexp = "select_rate: Please specify a 'method'")
  })

  # Check "positive" method -------------------------------------------------

  test_that("select_rate: method = positive  - all output rates are positive", {
    expect_true(all(conv_rt_ar_subset_pos$rate.output > 0))
  })

  test_that("select_rate: method = positive  - check n rates are subset", {
    expect_length(conv_rt_ar_subset_pos$rate.output, 11)
  })

  test_that("select_rate: method = positive  - check some exact values", {
    expect_equal(conv_rt_ar_subset_pos$rate.output[4],
                 convert_rate(0.0009992067,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "mg/h/g",
                              volume = 2.379,
                              mass = 0.006955)$rate.output)
  })

  test_that("select_rate: method = positive  - check message", {
    expect_message(select_rate(conv_rt_ar_obj, method = "positive"),
                   "select_rate: Selecting all positive rate values. 'n' input ignored...")
  })

  # Check "negative" method -------------------------------------------------

  test_that("select_rate: method = negative  - all output rates are negative", {
    expect_true(all(conv_rt_ar_subset_neg$rate.output < 0))
  })

  test_that("select_rate: method = negative  - check n rates are subset", {
    expect_length(conv_rt_ar_subset_neg$rate.output, 5)
  })

  test_that("select_rate: method = negative  - check some exact values", {
    expect_equal(conv_rt_ar_subset_neg$rate.output[4],
                 convert_rate(-0.0005949222,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "mg/h/g",
                              volume = 2.379,
                              mass = 0.006955)$rate.output)
  })

  test_that("select_rate: method = negative  - check message", {
    expect_message(select_rate(conv_rt_ar_obj, method = "negative"),
                   "Selecting all negative rate values. 'n' input ignored.")
  })


  # Check "nonzero" method --------------------------------------------------

  test_that("select_rate: method = nonzero  - NO rates should be zero", {
    expect_true(all(conv_rt_ar_subset_nonzero$rate.output != 0))
  })

  test_that("select_rate: method = nonzero  - check message", {
    expect_message(select_rate(conv_rt_ar_obj, method = "nonzero"),
                   "Selecting all non-zero rate values. 'n' input ignored.")
  })

  # Check "zero" method -----------------------------------------------------

  test_that("select_rate: method = zero  - ALL rates should be zero", {
    expect_true(all(conv_rt_ar_subset_zero$rate.output == 0))
  })

  test_that("select_rate: method = zero  - check message", {
    expect_message(select_rate(conv_rt_ar_obj, method = "zero"),
                   "Selecting all zero rate values. 'n' input ignored.")
  })


  # Check "lowest" method ---------------------------------------------------

  test_that("select_rate: method = lowest   - errors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "lowest", n = 7),
                 regexp = "Object contains both negative and positive rates.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "lowest", n = 7.1),
                 regexp = "select_rate: For 'lowest' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "lowest", n = -1),
                 regexp = "select_rate: For 'lowest' method 'n' must contain only positive integers.")
    expect_message(select_rate(conv_rt_ar_obj_min, method = "lowest", n = 1000),
                   regexp = "select_rate: 'n' input is greater than number of rates in \\$summary. Nothing to remove.")
  })

  test_that("select_rate: method = lowest   - check n rates extracted for positive rates", {
    conv_rt_ar_subset_low_pos <- select_rate(conv_rt_ar_subset_pos, method = "lowest", n = 3)
    expect_equal(length(conv_rt_ar_subset_low_pos$rate.output),
                 3)
  })

  test_that("select_rate: method = lowest   - check they are LOWEST numerically for positive rates", {
    conv_rt_ar_subset_low_pos <- select_rate(conv_rt_ar_subset_pos, method = "lowest", n = 3)
    expect_equal(head(sort(conv_rt_ar_subset_pos$rate.output), 3), ## head to get lowest
                 sort(conv_rt_ar_subset_low_pos$rate.output))
  })

  test_that("select_rate: method = lowest   - check n rates extracted for negative rates", {
    conv_rt_ar_subset_low_neg <- select_rate(conv_rt_ar_subset_neg, method = "lowest", n = 3)
    expect_equal(length(conv_rt_ar_subset_low_neg$rate.output),
                 3)
  })

  test_that("select_rate: method = lowest   - check they are HIGHEST numerically for negative rates", {
    conv_rt_ar_subset_low_neg <- select_rate(conv_rt_ar_subset_neg, method = "lowest", n = 3)
    expect_equal(tail(sort(conv_rt_ar_subset_neg$rate.output), 3), ## tail to get lowest
                 sort(conv_rt_ar_subset_low_neg$rate.output))
  })

  test_that("select_rate: method = lowest   - check message", {
    expect_message(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = 3),
                   "Selecting lowest")
  })


  # Check "highest" method --------------------------------------------------

  test_that("select_rate: method = highest   - errors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "highest", n = 7),
                 regexp = "Object contains both negative and positive rates.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "highest", n = 7.1),
                 regexp = "select_rate: For 'highest' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "highest", n = -1),
                 regexp = "select_rate: For 'highest' method 'n' must contain only positive integers.")
    expect_message(select_rate(conv_rt_ar_obj_min, method = "highest", n = 1000),
                   regexp = "select_rate: 'n' input is greater than number of rates in \\$summary. Nothing to remove.")
  })

  test_that("select_rate: method = highest   - check n rates extracted for positive rates", {
    conv_rt_ar_subset_high_pos <- select_rate(conv_rt_ar_subset_pos, method = "highest", n = 3)
    expect_equal(length(conv_rt_ar_subset_high_pos$rate.output),
                 3)
  })

  test_that("select_rate: method = highest   - check they are HIGHEST numerically for positive rates", {
    conv_rt_ar_subset_high_pos <- select_rate(conv_rt_ar_subset_pos, method = "highest", n = 3)
    expect_equal(tail(sort(conv_rt_ar_subset_pos$rate.output), 3), ## tail to get highest
                 sort(conv_rt_ar_subset_high_pos$rate.output))
  })

  test_that("select_rate: method = highest   - check n rates extracted for negative rates", {
    conv_rt_ar_subset_high_neg <- select_rate(conv_rt_ar_subset_neg, method = "highest", n = 3)
    expect_equal(length(conv_rt_ar_subset_high_neg$rate.output),
                 3)
  })

  test_that("select_rate: method = highest   - check they are LOWEST numerically for negative rates", {
    conv_rt_ar_subset_high_neg <- select_rate(conv_rt_ar_subset_neg, method = "highest", n = 3)
    expect_equal(head(sort(conv_rt_ar_subset_neg$rate.output), 3), ## head to get lowest
                 sort(conv_rt_ar_subset_high_neg$rate.output))
  })

  test_that("select_rate: method = highest   - check message", {
    expect_message(select_rate(conv_rt_ar_subset_pos, method = "highest", n = 3),
                   "Selecting highest")
  })


  # Check "lowest_percentile" method ----------------------------------------

  test_that("select_rate: method = lowest_percentile   - errors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "lowest_percentile", n = 7),
                 regexp = "Object contains both negative and positive rates.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "lowest_percentile", n = 7.1),
                 regexp = "select_rate: For 'percentile' methods 'n' must be between 0 and 1.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "lowest_percentile", n = -1),
                 regexp = "select_rate: For 'percentile' methods 'n' must be between 0 and 1.")
  })

  test_that("select_rate: method = lowest_percentile   - these should match", {
    conv_rt_ar_subset_pos_flip <- conv_rt_ar_subset_pos
    conv_rt_ar_subset_pos_flip$rate.output <- conv_rt_ar_subset_pos_flip$rate.output * -1
    conv_rt_ar_subset_pos_flip$summary$rate.output <- conv_rt_ar_subset_pos_flip$summary$rate.output * -1

    expect_equal(select_rate(conv_rt_ar_subset_pos, method = "lowest_percentile", n = 0.1)$rate.output,
                 select_rate(conv_rt_ar_subset_pos_flip, method = "lowest_percentile", n = 0.1)$rate.output *-1)
  })

  # Check "highest_percentile" method ---------------------------------------

  test_that("select_rate: method = highest_percentile   - errors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "highest_percentile", n = 7),
                 regexp = "Object contains both negative and positive rates.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "highest_percentile", n = 7.1),
                 regexp = "select_rate: For 'percentile' methods 'n' must be between 0 and 1.")
    expect_error(select_rate(conv_rt_ar_obj_min, method = "highest_percentile", n = -1),
                 regexp = "select_rate: For 'percentile' methods 'n' must be between 0 and 1.")
  })

  test_that("select_rate: method = highest_percentile   - these should match", {
    conv_rt_ar_subset_pos_flip <- conv_rt_ar_subset_pos
    conv_rt_ar_subset_pos_flip$rate.output <- conv_rt_ar_subset_pos_flip$rate.output * -1
    conv_rt_ar_subset_pos_flip$summary$rate.output <- conv_rt_ar_subset_pos_flip$summary$rate.output * -1

    expect_equal(select_rate(conv_rt_ar_subset_pos, method = "highest_percentile", n = 0.1)$rate.output,
                 select_rate(conv_rt_ar_subset_pos_flip, method = "highest_percentile", n = 0.1)$rate.output *-1)
  })


  # Check "minimum" method --------------------------------------------------

  conv_rt_ar_subset_min_n <- select_rate(conv_rt_ar_obj, method = "minimum", n = 4)

  test_that("select_rate: method = minimum   - errors", {
    expect_error(select_rate(conv_rt_ar_subset_min_n, method = "minimum", n = 7.1),
                 regexp = "select_rate: For 'minimum' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar_subset_min_n, method = "minimum", n = -1),
                 regexp = "select_rate: For 'minimum' method 'n' must contain only positive integers.")
    expect_message(select_rate(conv_rt_ar_subset_min_n, method = "minimum", n = 7000),
                   regexp = "select_rate: 'n' input is greater than number of rates in \\$summary. Nothing to remove.")
  })

  test_that("select_rate: method = minimum   - check n rates extracted", {
    expect_length(conv_rt_ar_subset_min_n$rate.output,
                  4)
  })

  test_that("select_rate: method = minimum   - check they are LOWEST n rates", {
    expect_equal(head(sort(conv_rt_ar_obj$rate.output), 4), ## head to get lowest
                 sort(conv_rt_ar_subset_min_n$rate.output))
  })

  test_that("select_rate: method = minimum   - check some exact values", {
    expect_equal(conv_rt_ar_subset_min_n$rate.output[2],
                 convert_rate(-0.0005968452,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "mg/h/g",
                              volume = 2.379,
                              mass = 0.006955)$rate.output)
  })

  test_that("select_rate: method = minimum   - check message", {
    expect_message(select_rate(conv_rt_ar_obj, method = "minimum", n = 4),
                   "Selecting minimum")
  })

  # Check "maximim" method --------------------------------------------------
  conv_rt_ar_subset_max_n <- select_rate(conv_rt_ar_obj, method = "maximum", n = 4)

  test_that("select_rate: method = maximum   - errors", {
    expect_error(select_rate(conv_rt_ar_subset_max_n, method = "maximum", n = 7.1),
                 regexp = "select_rate: For 'maximum' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar_subset_max_n, method = "maximum", n = -1),
                 regexp = "select_rate: For 'maximum' method 'n' must contain only positive integers.")
    expect_message(select_rate(conv_rt_ar_subset_max_n, method = "maximum", n = 7000),
                   regexp = "select_rate: 'n' input is greater than number of rates in \\$summary. Nothing to remove.")
  })

  test_that("select_rate: method = maximum   - check n rates extracted", {
    expect_length(conv_rt_ar_subset_max_n$rate.output,
                  4)
  })

  test_that("select_rate: method = maximum   - check they are HIGHEST n rates", {
    expect_equal(tail(sort(conv_rt_ar_obj$rate.output), 4),
                 sort(conv_rt_ar_subset_max_n$rate.output))
  })

  test_that("select_rate: method = maximum   - check some exact values", {
    expect_equal(conv_rt_ar_subset_max_n$rate.output[2],
                 convert_rate(0.001605693,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              output.unit = "mg/h/g",
                              volume = 2.379,
                              mass = 0.006955)$rate.output)
  })

  test_that("select_rate: method = maximum   - check message", {
    expect_message(select_rate(conv_rt_ar_obj, method = "maximum", n = 4),
                   "Selecting maximum")
  })

  # Check "minimum_percentile" method -------------------------------------------

  conv_rt_ar_subset_min_perc <- select_rate(conv_rt_ar_obj, method = "minimum_percentile", n = 0.1)

  test_that("select_rate: method = minimum_percentile   - these should match (in this case)", {
    expect_equal(conv_rt_ar_subset_min_perc$rate.output,
                 select_rate(conv_rt_ar_obj, method = "minimum", n = 2)$rate.output)
  })

  test_that("select_rate: method = minimum_percentile   - check stops with n not between 0-1", {
    expect_error(select_rate(conv_rt_ar_obj, method = "minimum_percentile", n = 4),
                 regexp = "For 'percentile' methods 'n' must be between 0 and 1.")
  })

  # Check "maximum_percentile" method -------------------------------------------

  conv_rt_ar_subset_max_perc <- select_rate(conv_rt_ar_obj, method = "maximum_percentile", n = 0.1)

  test_that("select_rate: method = maximum_percentile   - these should match (in this case)", {
    expect_equal(conv_rt_ar_subset_max_perc$rate.output,
                 select_rate(conv_rt_ar_obj, method = "maximum", n = 2)$rate.output)
  })

  test_that("select_rate: method = maximum_percentile   - check stops with n not between 0-1", {
    expect_error(select_rate(conv_rt_ar_obj, method = "maximum_percentile", n = 4),
                 regexp = "For 'percentile' methods 'n' must be between 0 and 1.")
  })

  # Check "rate" method -----------------------------------------------------

  conv_rt_ar_select_rate <- select_rate(conv_rt_ar_obj, method = "rate",
                                        n = c(-0.012, 0.02))

  test_that("select_rate: method = rate   - check all within n rate values", {
    sapply(conv_rt_ar_select_rate$rate.output, function(x) expect_gte(x, -0.012))
    sapply(conv_rt_ar_select_rate$rate.output, function(x) expect_lte(x, 0.02))
  })

  test_that("select_rate: method = rate   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "rate", n = 4),
                 regexp = "For 'rate' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "rate", n = c(1,2,3)),
                 regexp = "For 'rate' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: method = rate   - works if n entered either way round", {
    expect_equal(select_rate(conv_rt_ar_obj, method = "rate",
                             n = c(0.0010, 0.0015))$rate.output,
                 select_rate(conv_rt_ar_obj, method = "rate",
                             n = c(0.0015, 0.0010))$rate.output)
  })

  # Check "slope" method -----------------------------------------------------

  conv_rt_ar_select_slope <- select_rate(conv_rt_ar_obj, method = "slope",
                                         n = c(-0.0006, 0.001))

  test_that("select_slope: method = slope   - check all within n slope values", {
    sapply(conv_rt_ar_select_slope$summary$slope_b1, function(x) expect_gte(x, -0.0006))
    sapply(conv_rt_ar_select_slope$summary$slope_b1, function(x) expect_lte(x, 0.001))
  })

  test_that("select_slope: method = slope   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "slope", n = 4),
                 regexp = "For 'slope' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "slope", n = c(1,2,3)),
                 regexp = "For 'slope' method 'n' must be a vector of two values.")
  })

  test_that("select_slope: method = slope   - works if n entered either way round", {
    expect_equal(select_rate(conv_rt_ar_obj, method = "slope",
                             n = c(-0.0006, 0.001))$rate.output,
                 select_rate(conv_rt_ar_obj, method = "slope",
                             n = c(-0.0006, 0.001))$rate.output)
  })


  # Check "intercept" method -----------------------------------------------------

  conv_rt_ar_select_intercept <- select_rate(conv_rt_ar_obj, method = "intercept",
                                             n = c(3, 7.2))

  test_that("select_intercept: method = intercept   - check all within n intercept values", {
    sapply(conv_rt_ar_select_intercept$summary$intercept_b0, function(x) expect_gte(x, 3))
    sapply(conv_rt_ar_select_intercept$summary$intercept_b0, function(x) expect_lte(x, 7.2))
  })

  test_that("select_intercept: method = intercept   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "intercept", n = 4),
                 regexp = "For 'intercept' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "intercept", n = c(1,2,3)),
                 regexp = "For 'intercept' method 'n' must be a vector of two values.")
  })

  test_that("select_intercept: method = intercept   - works if n entered either way round", {
    expect_equal(select_rate(conv_rt_ar_obj, method = "intercept",
                             n = c(3, 7.2))$rate.output,
                 select_rate(conv_rt_ar_obj, method = "intercept",
                             n = c(3, 7.2))$rate.output)
  })



  # Check "rsq" method ------------------------------------------------------

  conv_rt_ar_subset_rsq <- select_rate(conv_rt_ar_obj, method = "rsq",
                                       n = c(0.7, 0.993))

  test_that("select_rate: method = rsq   - check all within rsq n range", {
    sapply(conv_rt_ar_subset_rsq$summary$rsq, function(x) expect_gte(x, 0.7))
    sapply(conv_rt_ar_subset_rsq$summary$rsq, function(x) expect_lte(x, 0.993))
  })

  test_that("select_rate: method = rsq   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "rsq", n = 4),
                 regexp = "For 'rsq' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "rsq", n = c(1,2,3)),
                 regexp = "For 'rsq' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: method = rsq   - works if n entered either way round", {
    expect_equal(select_rate(conv_rt_ar_obj, method = "rsq",
                             n = c(0.6, 0.7))$rate.output,
                 select_rate(conv_rt_ar_obj, method = "rsq",
                             n = c(0.7, 0.6))$rate.output)
  })

  # Check "row" method ------------------------------------------------------

  conv_rt_ar_subset_row <- select_rate(conv_rt_ar_obj, method = "row",
                                       n = c(2000, 4100))

  test_that("select_rate: method = row   - check all within row n range", {
    sapply(conv_rt_ar_subset_row$summary$row, function(x) expect_gte(x, 2000))
    sapply(conv_rt_ar_subset_row$summary$endrow, function(x) expect_lte(x, 4100))
  })

  test_that("select_rate: method = row   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "row", n = 4),
                 regexp = "For 'row' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "row", n = c(1,2,3)),
                 regexp = "For 'row' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: method = row   - check stops with n out of range", {
    expect_error(select_rate(conv_rt_ar_obj, method = "row",
                             n = c(1,5000)),
                 regexp = "select_rate: Input for 'n': row inputs out of data frame range.")
  })

  # Check "row_omit" method -------------------------------------------------

  test_that("select_rate: works with method = row_omit and a single n input", {
    expect_error(conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_obj, method = "row_omit",
                                                           n = 2201),
                 regexp = NA)
    expect_equal(nrow(select_rate(conv_rt_ar_obj, method = "row_omit",
                                  n = 2201)$summary),
                 9)
  })

  test_that("select_rate: with method = row_omit correctly omits rates with single n input", {
    conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_obj, method = "row_omit",
                                              n = 2201)

    ## check omitted row not within rows for each regression
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(2201 %in% x[6]:x[7]))
  })

  test_that("select_rate: works with method = row_omit and n input of multiple values", {
    expect_error(conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_obj, method = "row_omit",
                                                           n = c(1000,2000,3000)),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(1000 %in% x[6]:x[7]))
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(2000 %in% x[6]:x[7]))
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(3000 %in% x[6]:x[7]))
  })

  test_that("select_rate: works with method = row_omit and n input of range of values", {
    skip_on_cran()
    expect_error(conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_obj, method = "row_omit",
                                                           n = c(1000:3000)),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(1000 %in% x[6]:x[7]))
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(2000 %in% x[6]:x[7]))
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(3000 %in% x[6]:x[7]))
  })

  test_that("select_rate: works with method = row_omit and n input of two values", {
    skip_on_cran()
    expect_error(conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_obj, method = "row_omit",
                                                           n = c(2000,3000)),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(2000 %in% x[6]:x[7]))
    apply(conv_rt_ar_subset_row_omit$summary, 1, function(x)
      expect_false(3000 %in% x[6]:x[7]))
  })

  test_that("select_rate: works with method = row_omit and n input not used - i.e. does not remove any results", {
    skip_on_cran()
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "row_omit", n = 100)$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "row_omit", n = 98:100)$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
  })

  test_that("select_rate: stops with method = row_omit and n input malformed", {
    ## check stops with n not numeric or integer
    expect_error(select_rate(conv_rt_ar_obj, method = "row_omit",
                             n = "string"),
                 regexp = "select_rate: For 'row_omit' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar_obj, method = "row_omit",
                             n = 1.2),
                 regexp = "select_rate: For 'row_omit' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar_obj, method = "row_omit",
                             n = c(1.2, 2.5)),
                 regexp = "select_rate: For 'row_omit' method 'n' must contain only positive integers.")
    ## check stops when out of range
    expect_error(select_rate(conv_rt_ar_obj, method = "row_omit",
                             n = 5000),
                 regexp = "select_rate: Input for 'n': row inputs out of data frame range.")
  })



  # Check "time" method -----------------------------------------------------

  test_that("select_rate: method = time   - check all rates are within time range", {
    conv_rt_ar_subset_time <- select_rate(conv_rt_ar_obj, method = "time", n = c(2000,3500))
    sapply(conv_rt_ar_subset_time$summary$time, function(x) expect_gte(x, 2000))
    sapply(conv_rt_ar_subset_time$summary$endtime, function(x) expect_lte(x, 3500))
  })

  test_that("select_rate: method = time   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "time", n = 4),
                 regexp = "For 'time' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "time", n = c(1,2,3)),
                 regexp = "For 'time' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: stops with method = time and n input malformed", {
    ## check stops when out of range
    expect_error(select_rate(conv_rt_ar_obj, method = "time",
                             n = c(1,5000)),
                 regexp = "select_rate: Input for 'n': time inputs out of time data range.")
  })

  # Check "time_omit" method -------------------------------------------------

  test_that("select_rate: works with method = time_omit and a single n input", {
    expect_error(conv_rt_ar_subset_time_omit <- select_rate(conv_rt_ar_obj, method = "time_omit",
                                                            n = 2201),
                 regexp = NA)
    expect_equal(nrow(select_rate(conv_rt_ar_obj, method = "time_omit",
                                  n = 2201)$summary),
                 9)
  })

  test_that("select_rate: with method = time_omit correctly omits rates with single n input", {
    conv_rt_ar_subset_time_omit <- select_rate(conv_rt_ar_obj, method = "time_omit",
                                               n = 2201)

    ## check omitted row not within rows for each regression
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(2201 %in% x[8]:x[9]))
  })

  test_that("select_rate: works with method = time_omit and n input of multiple values", {
    expect_error(conv_rt_ar_subset_time_omit <- select_rate(conv_rt_ar_obj, method = "time_omit",
                                                            n = c(1000,2000,3000)),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(1000 %in% x[8]:x[9]))
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(2000 %in% x[8]:x[9]))
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(3000 %in% x[8]:x[9]))
  })

  test_that("select_rate: works with method = time_omit and n input of range of values", {
    skip_on_cran()
    expect_error(conv_rt_ar_subset_time_omit <- select_rate(conv_rt_ar_obj, method = "time_omit",
                                                            n = c(1000:3000)),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(1000 %in% x[8]:x[9]))
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(2000 %in% x[8]:x[9]))
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(3000 %in% x[8]:x[9]))
  })

  test_that("select_rate: works with method = time_omit and n input of two values", {
    expect_error(conv_rt_ar_subset_time_omit <- select_rate(conv_rt_ar_obj, method = "time_omit",
                                                            n = c(2000,3000)),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(2000 %in% x[8]:x[9]))
    apply(conv_rt_ar_subset_time_omit$summary, 1, function(x)
      expect_false(3000 %in% x[8]:x[9]))
  })

  test_that("select_rate: works with method = time_omit and n input not used - i.e. does not remove any results", {
    skip_on_cran()
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "time_omit", n = 6000)$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "time_omit", n = 5998:6000)$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
  })

  test_that("select_rate: stops with method = time_omit and n input malformed", {
    ## check stops with n not numeric
    expect_error(select_rate(conv_rt_ar_obj, method = "time_omit",
                             n = "string"),
                 regexp = "select_rate: For 'time_omit' method 'n' must contain only numeric values of time.")
    ## check stops when out of range
    expect_error(select_rate(conv_rt_ar_obj, method = "time_omit",
                             n = 5000),
                 regexp = "select_rate: Input for 'n': time inputs out of time data range.")
  })


  # Check "rank" method ------------------------------------------------------
  conv_rt_ar_subset_rank <- select_rate(conv_rt_ar_obj, method = "rank",
                                        n = c(4, 10))

  test_that("select_rate: method = rank   - messages", {
    expect_message(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank", n = 4),
                   "select_rate: Note there are multiple replicates present in these results, which may have multiple ranks \\*within\\* them.")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank", n = 7.1),
                 regexp = "select_rate: For 'rank' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank", n = 1000),
                 regexp = "select_rate: Input for 'n': One or more 'rank' inputs out of range of 'summary\\$rank' values.")
  })

  test_that("select_rate: method = rank   - check all within rank n range", {
    expect_equal(unique(conv_rt_ar_subset_rank$summary$rank),
                 c(4,10))
  })

  test_that("select_rate: method = rank   - correctly selects from intermittent-flow multiple ranks per rep ", {
    #conv_rt_ar.int_obj
    #conv_rt_ar.int_obj_mult_ranks

    # should not remove any rates
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj, method = "rank", n = 1)$summary),
                 10)
    # should remove all ranks except 1
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank", n = 1)$summary),
                 10)
    # another check
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank", n = 1:2)$summary),
                 20)
  })

  test_that("select_rate: method = rank   - accepts multiple length vectors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "rank", n = 4),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj, method = "rank", n = c(1,2)),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj, method = "rank", n = c(1,2,3)),
                 NA)
  })



  # Check "rank_omit" method ------------------------------------------------------
  conv_rt_ar_subset_rank <- select_rate(conv_rt_ar_obj, method = "rank_omit",
                                        n = c(4, 10))

  test_that("select_rate: method = rank   - messages", {
    expect_message(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank_omit", n = 4),
                   "select_rate: Note there are multiple replicates present in these results, which may have multiple ranks \\*within\\* them.")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank_omit", n = 7.1),
                 regexp = "select_rate: For 'rank_omit' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank_omit", n = 1000),
                 regexp = "select_rate: Input for 'n': One or more 'rank_omit' inputs out of range of 'summary\\$rank' values.")
  })

  test_that("select_rate: method = rank_omit   - check all within rank n range", {
    expect_equal(unique(conv_rt_ar_subset_rank$summary$rank),
                 c(1:3,5:9,11:16))
  })

  test_that("select_rate: method = rank_omit   - correctly selects from intermittent-flow multiple ranks per rep ", {
    #conv_rt_ar.int_obj
    #conv_rt_ar.int_obj_mult_ranks

    # should remove all rates
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj, method = "rank_omit", n = 1)$summary),
                 0)
    #
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank_omit", n = 1)$summary),
                 31)
    expect_equal(unique(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank_omit", n = 1)$summary$rank),
                 2:5)
    expect_equal(unique(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rank_omit", n = 1:3)$summary$rank),
                 4:5)
  })

  test_that("select_rate: method = rank   - accepts multiple length vectors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "rank_omit", n = 4),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj, method = "rank_omit", n = c(1,2)),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj, method = "rank_omit", n = c(1,2,3)),
                 NA)
  })


  # Check "rep" method ------------------------------------------------------

  #conv_rt_ar.int_obj
  #conv_rt_ar.int_obj_mult_ranks
  #
  conv_rt_ar_subset_rep <- select_rate(conv_rt_ar.int_obj, method = "rep",
                                       n = c(4, 10))

  test_that("select_rate: method = rep   - messages", {
    expect_error(select_rate(conv_rt_ar_obj, method = "rep", n = 1),
                 regexp = "select_rate: All 'rep' are NA so nothing to select!")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = 7.1),
                 regexp = "select_rate: For 'rep' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = 1000),
                 regexp = "select_rate: Input for 'n': One or more 'rep' inputs out of range of 'summary\\$rep' values.")
  })

  test_that("select_rate: method = rep   - check all within rep n range", {
    expect_equal(unique(conv_rt_ar_subset_rep$summary$rep),
                 c(4,10))
  })

  test_that("select_rate: method = rep   - correctly selects from intermittent-flow multiple reps per rep ", {

    #
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj, method = "rep", n = 1)$summary),
                 1)
    #
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = 1)$summary),
                 4)
    # another check
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = 1:2)$summary),
                 9)
  })

  test_that("select_rate: method = rep   - accepts multiple length vectors", {
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = 1),
                 NA)
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = c(1,2)),
                 NA)
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep", n = c(1,2,3)),
                 NA)
  })



  # Check "rep_omit" method ------------------------------------------------------
  #conv_rt_ar.int_obj
  #conv_rt_ar.int_obj_mult_ranks

  conv_rt_ar_subset_rep <- select_rate(conv_rt_ar.int_obj, method = "rep_omit",
                                       n = c(4, 10))

  test_that("select_rate: method = rep   - messages", {
    expect_error(select_rate(conv_rt_ar_obj, method = "rep_omit", n = 1),
                 regexp = "select_rate: All 'rep' are NA so nothing to select!")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = 7.1),
                 regexp = "select_rate: For 'rep_omit' method 'n' must contain only positive integers.")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = 1000),
                 regexp = "select_rate: Input for 'n': One or more 'rep_omit' inputs out of range of 'summary\\$rep' values.")
  })

  test_that("select_rate: method = rep_omit   - check all within rep n range", {
    expect_equal(unique(conv_rt_ar_subset_rep$summary$rep),
                 c(1:3,5:9))
  })

  test_that("select_rate: method = rep_omit   - correctly selects from intermittent-flow multiple reps per rep ", {
    #conv_rt_ar.int_obj
    #conv_rt_ar.int_obj_mult_ranks

    # should remove all rates
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj, method = "rep_omit", n = 1)$summary),
                 9)
    #
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = 1)$summary),
                 37)
    expect_equal(unique(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = 1)$summary$rep),
                 2:10)
    expect_equal(unique(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = 1:3)$summary$rep),
                 4:10)
  })

  test_that("select_rate: method = rep   - accepts multiple length vectors", {
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = 4),
                 NA)
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = c(1,2)),
                 NA)
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "rep_omit", n = c(1,2,3)),
                 NA)
  })


  # Check "oxygen" method ----------------------------------------------------
  conv_rt_ar_subset_oxy <- select_rate(conv_rt_ar_obj_high, method = "oxygen",
                                       c(7.35, 7))

  # this doesn't strictly check proper functonality, in that the method checks
  # *ALL* oxygen values in the original regression
  test_that("select_rate: method = oxygen   - check all within oxygen n range", {
    sapply(conv_rt_ar_subset_oxy$summary$endoxy, function(x) expect_gte(x, 7))
    sapply(conv_rt_ar_subset_oxy$summary$oxy, function(x) expect_lte(x, 7.35))
  })

  test_that("select_rate: method = oxygen   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "oxygen", n = 4),
                 regexp = "For 'oxygen' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "oxygen", n = c(1,2,3)),
                 regexp = "For 'oxygen' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: works with method = oxygen_omit and n input not used - i.e. does not remove any results", {
    skip_on_cran()
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "oxygen", n = c(3.5, 7.7))$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
  })


  # Check "oxygen_omit" method ----------------------------------------------------
  conv_rt_ar_subset_oxy_om <- select_rate(conv_rt_ar_obj, method = "oxygen_omit",
                                          c(7.00))

  # this doesn't strictly check proper functonality, in that the method checks
  # *ALL* oxygen_omit values in the original regression
  test_that("select_rate: method = oxygen_omit - no remaining results have ommitted value", {
    for(z in 1:nrow(conv_rt_ar_subset_oxy_om$summary)) {
      mapply(function(p,q) expect_false(any((conv_rt_ar_subset_oxy_om$dataframe$y[p:q] == 7.35))),
             p = conv_rt_ar_subset_oxy_om$summary$row,
             q = conv_rt_ar_subset_oxy_om$summary$endrow)
    }
  })

  test_that("select_rate: works with method = oxygen_omit and n input of multiple values", {
    expect_error(conv_rt_ar_subset_oxygen_omit <- select_rate(conv_rt_ar_obj, method = "oxygen_omit",
                                                              n = c(7, 6.9, 6.8)),
                 regexp = NA)
  })

  test_that("select_rate: works with method = oxygen_omit and n input of range of values", {
    expect_error(conv_rt_ar_subset_oxygen_omit <- select_rate(conv_rt_ar_obj, method = "oxygen_omit",
                                                              n = c(7:5)),
                 regexp = NA)
  })

  test_that("select_rate: works with method = oxygen_omit and n input of two values", {
    expect_error(conv_rt_ar_subset_oxygen_omit <- select_rate(conv_rt_ar_obj, method = "oxygen_omit",
                                                              n = c(7,6)),
                 regexp = NA)
  })


  test_that("select_rate: works with method = oxygen_omit and n input not used - i.e. does not remove any results", {
    skip_on_cran()
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "oxygen_omit", n = 3.5)$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
    expect_equal(nrow(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "oxygen_omit", n = c(3.2, 3.3, 3.5))$summary),
                 nrow(conv_rt_ar.int_obj_mult_ranks$summary))
  })

  test_that("select_rate: stops with method = oxygen_omit and n input malformed", {
    ## check stops with n not numeric
    expect_error(select_rate(conv_rt_ar_obj, method = "oxygen_omit",
                             n = "string"),
                 regexp = "select_rate: For 'oxygen_omit' method 'n' must contain only numeric values of oxygen.")
  })



  # Check "manual" method -----------------------------------------------------

  test_that("select_rate: method = manual   - subsets a single rate", {
    conv_rt_ar_subset_man <- select_rate(conv_rt_ar_obj, method = "manual",
                                         n = 1)

    expect_equal(length(conv_rt_ar_subset_man$rate.output),
                 1)
    expect_equal(conv_rt_ar_obj$rate.output[1],
                 conv_rt_ar_subset_man$rate.output[1])
  })

  test_that("select_rate: method = manual   - subsets multiple consecutive rates", {
    conv_rt_ar_subset_man <- select_rate(conv_rt_ar_obj, method = "manual",
                                         n = c(3,2))

    expect_equal(length(conv_rt_ar_subset_man$rate.output),
                 2)
    expect_equal(conv_rt_ar_obj$rate.output[2:3],
                 conv_rt_ar_subset_man$rate.output)
  })

  test_that("select_rate: method = manual   - subsets multiple non-consecutive rates", {
    conv_rt_ar_subset_man <- select_rate(conv_rt_ar_obj, method = "manual",
                                         n = c(1,5,9))

    expect_equal(length(conv_rt_ar_subset_man$rate.output),
                 3)
    expect_equal(conv_rt_ar_obj$rate.output[c(1,5,9)],
                 conv_rt_ar_subset_man$rate.output)
  })

  test_that("select_rate: method = manual   - check stops with n out of range", {
    expect_error(select_rate(conv_rt_ar_obj, method = "manual", n = 17),
                 regexp = "For 'manual' method: 'n' values are out of range of ")
  })

  # Check "manual_omit" method ------------------------------------------------------
  conv_rt_ar_subset_rank <- select_rate(conv_rt_ar_obj, method = "manual_omit",
                                        n = c(4, 10))

  test_that("select_rate: method = rank   - messages", {
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "manual_omit", n = 7.1),
                 regexp = "select_rate: For 'manual' method: 'n' values are out of range of \\$summary data.frame rows...")
    expect_error(select_rate(conv_rt_ar.int_obj_mult_ranks, method = "manual_omit", n = 1000),
                 regexp = "select_rate: For 'manual' method: 'n' values are out of range of \\$summary data.frame rows...")
  })

  test_that("select_rate: method = manual_omit   - check all within rank n range", {
    expect_equal(unique(conv_rt_ar_subset_rank$summary$rank),
                 c(1:3,5:9,11:16))
  })

  test_that("select_rate: method = rank   - accepts multiple length vectors", {
    expect_error(select_rate(conv_rt_ar_obj, method = "manual_omit", n = 4),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj, method = "manual_omit", n = c(1,2)),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj, method = "manual_omit", n = c(1,2,3)),
                 NA)
  })


  # Check "density" method --------------------------------------------------

  conv_rt_ar_subset_density <- select_rate(conv_rt_ar_obj, method = "density",
                                           n = c(300, 7500))

  test_that("select_rate: method = density   - check all within density n range", {
    sapply(conv_rt_ar_subset_density$summary$density, function(x) expect_gte(x, 300))
    sapply(conv_rt_ar_subset_density$summary$density, function(x) expect_lte(x, 7500))
  })

  test_that("select_rate: method = density   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "density", n = 4),
                 regexp = "For 'density' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "density", n = c(1,2,3)),
                 regexp = "For 'density' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: method = density   - check stops with auto_rate objects not of 'linear' KDE method", {
    skip_on_cran()
    obj <- inspect(intermittent.rd) %>%
      auto_rate(method = "maximum") %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 2.379,
                   mass = 0.006955)
    expect_error(select_rate(obj, method = "density", n = c(300,7500)),
                 regexp = "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    obj <- inspect(intermittent.rd) %>%
      auto_rate(method = "interval") %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 2.379,
                   mass = 0.006955)
    expect_error(select_rate(obj, method = "density", n = c(300,7500)),
                 regexp = "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")
  })



  # Check "duration" method -------------------------------------------------

  test_that("select_rate: method = duration   - check stops with n not length 2 vector", {
    expect_error(select_rate(conv_rt_ar_obj, method = "duration", n = 4),
                 regexp = "For 'duration' method 'n' must be a vector of two values.")
    expect_error(select_rate(conv_rt_ar_obj, method = "duration", n = c(1,2,3)),
                 regexp = "For 'duration' method 'n' must be a vector of two values.")
  })

  test_that("select_rate: method = duration   - subsets max duration (zero to something) correctly", {
    conv_rt_ar_subset_dur1 <- select_rate(conv_rt_ar_obj, method = "duration",
                                          n = c(0,1000))
    ## should be 11 matches
    expect_equal(length(conv_rt_ar_subset_dur1$rate.output),
                 11)
  })

  test_that("select_rate: method = duration   - min duration (something to something large) correctly", {
    conv_rt_ar_subset_dur2 <- select_rate(conv_rt_ar_obj, method = "duration",
                                          n = c(1000, 5000))
    ## should be 5 matches
    expect_equal(length(conv_rt_ar_subset_dur2$rate.output),
                 5)
  })

  test_that("select_rate: method = duration   - Test all possible durations match to original", {
    conv_rt_ar_subset_dur1 <- select_rate(conv_rt_ar_obj, method = "duration",
                                          n = c(0,999))
    conv_rt_ar_subset_dur2 <- select_rate(conv_rt_ar_obj, method = "duration",
                                          n = c(1000, Inf))
    expect_equal(length(conv_rt_ar_subset_dur1$rate.output) + length(conv_rt_ar_subset_dur2$rate.output),
                 length(conv_rt_ar_obj$rate.output))
  })

  # Check "overlap" method --------------------------------------------------

  test_that("select_rate: method = overlap   - applies n = NULL default of 0", {
    expect_message(select_rate(conv_rt_ar_obj, method = "overlap",
                             n = NULL),
                 "select_rate: 'overlap' method - applying default 'n = 0', no overlapping permitted.")
  })

  test_that("select_rate: method = overlap   - stops when n is outside 0 to 1", {
    expect_error(select_rate(conv_rt_ar_obj, method = "overlap",
                             n = 2),
                 "For 'overlap' method 'n' must be between 0 and 1 inclusive.")
  })

  test_that("select_rate: method = overlap   - outputs 4 rates with these inputs", {
    expect_equal(length(select_rate(conv_rt_ar_obj,
                                    method = "overlap", n = 0.5)$rate.output),
                 4)
  })

  test_that("select_rate: method = overlap   - outputs 4 rates with these inputs", {
    expect_equal(length(select_rate(conv_rt_ar_obj,
                                    method = "overlap", n = 0.5)$rate.output),
                 4)
  })

  # Reordering checks -------------------------------------------------------

  ## put conv_rt_ar_obj results in a random order
  conv_rt_ar_obj_mixed_lin <- conv_rt_ar_obj
  new_order_lin <- c(10, 9, 14, 13, 5, 2, 1 , 3, 8, 16, 6, 15, 7, 4, 12, 11)
  conv_rt_ar_obj_mixed_lin$summary <- conv_rt_ar_obj_mixed_lin$summary[new_order_lin,]
  conv_rt_ar_obj_mixed_lin$rate.output <- conv_rt_ar_obj_mixed_lin$summary$rate.output

  # same with other auto_rate method that's not linear
  conv_rt_ar_obj_mixed_high <- conv_rt_ar_obj_high
  new_order_high <- sample(1:nrow(conv_rt_ar_obj_mixed_high$summary))
  conv_rt_ar_obj_mixed_high$summary <- conv_rt_ar_obj_mixed_high$summary[new_order_high,]
  conv_rt_ar_obj_mixed_high$rate.output <- conv_rt_ar_obj_mixed_high$summary$rate.output

  # empty object
  empty_obj <- select_rate(conv_rt_ar_obj, method = "rate", n = c(0.1, 1))


  # method = "rolling" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = rolling - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL),
                   regexp = "select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = 1),
                   regexp = "select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = 1:10),
                   regexp = "select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL),
                   regexp = "select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = 1),
                   regexp = "select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = 1:10),
                   regexp = "select_rate: Reordering results by 'rolling' method. 'n' input ignored...")
  })

  test_that("select_rate: method = rolling - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "rolling", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "rolling", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'rolling' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL)$summary$row,
                     sort(conv_rt_ar_obj$summary$row))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL)$summary$row,
                     sort(conv_rt_ar_obj_high$summary$row))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, row)$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, row)$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "rolling", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "rolling", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })


  # method = "row" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = row - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL),
                   regexp = "select_rate: Reordering results by 'row' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL),
                   regexp = "select_rate: Reordering results by 'row' method.")
  })

  test_that("select_rate: method = row - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "row", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "row", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'row' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL)$summary$row,
                     sort(conv_rt_ar_obj$summary$row))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL)$summary$row,
                     sort(conv_rt_ar_obj_high$summary$row))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, row)$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, row)$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "row", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "row", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })


  # method = "time" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = time - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL),
                   regexp = "select_rate: Reordering results by 'time' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL),
                   regexp = "select_rate: Reordering results by 'time' method.")
  })

  test_that("select_rate: method = time - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "time", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "time", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'time' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL)$summary$time,
                     sort(conv_rt_ar_obj$summary$time))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL)$summary$time,
                     sort(conv_rt_ar_obj_high$summary$time))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, time)$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, time)$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "time", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "time", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })


  # method = "linear" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = linear - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL),
                   regexp = "select_rate: Reordering results by 'linear' method. 'n' input ignored...")
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = 1),
                   regexp = "select_rate: Reordering results by 'linear' method. 'n' input ignored...")
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = 1:10),
                   regexp = "select_rate: Reordering results by 'linear' method. 'n' input ignored...")
    # highest object
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "linear", n = NULL),
                 regexp = "select_rate: The 'linear' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")
  })

  test_that("select_rate: method = linear - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "linear", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "linear", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'linear' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL)$summary$density,
                     (conv_rt_ar_obj$summary$density))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, desc(density))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "linear", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))

  })




  # method = "density" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = density - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL),
                   regexp = "select_rate: Reordering results by 'density' method.")
  })

  test_that("select_rate: method = density - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "density", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "density", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'density' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL)$summary$density,
                     (conv_rt_ar_obj$summary$density))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, desc(density))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "density", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))

  })




  # method = "rank" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = rank - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL),
                   regexp = "select_rate: Reordering results by 'rank' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL),
                   regexp = "select_rate: Reordering results by 'rank' method.")
  })

  test_that("select_rate: method = rank - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "rank", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "rank", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'rank' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL)$summary$rank,
                     (conv_rt_ar_obj$summary$rank))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL)$summary$rank,
                     (conv_rt_ar_obj_high$summary$rank))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, rank)$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, rank)$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "rank", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "rank", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })

  # method = "rsq" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = rsq - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL),
                   regexp = "select_rate: Reordering results by 'rsq' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL),
                   regexp = "select_rate: Reordering results by 'rsq' method.")
  })

  test_that("select_rate: method = rsq - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "rsq", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "rsq", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'rsq' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL)$summary$rsq,
                     rev(sort(conv_rt_ar_obj$summary$rsq)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL)$summary$rsq,
                     rev(sort(conv_rt_ar_obj_high$summary$rsq)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL)$summary$rate.output)
    # expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL)$rate.output,
    #                  conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, desc(rsq))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, desc(rsq))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "rsq", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "rsq", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })

  # method = "slope" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = slope - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL),
                   regexp = "select_rate: Reordering results by 'slope' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL),
                   regexp = "select_rate: Reordering results by 'slope' method.")
  })

  test_that("select_rate: method = slope - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "slope", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "slope", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'slope' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL)$summary$slope,
                     (sort(conv_rt_ar_obj$summary$slope)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL)$summary$slope,
                     (sort(conv_rt_ar_obj_high$summary$slope)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL)$summary$rate.output)
    # expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL)$rate.output,
    #                  conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, desc(slope))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, (slope_b1))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "slope", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "slope", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })

  # method = "intercept" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = intercept - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL),
                   regexp = "select_rate: Reordering results by 'intercept' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL),
                   regexp = "select_rate: Reordering results by 'intercept' method.")
  })

  test_that("select_rate: method = intercept - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "intercept", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "intercept", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'intercept' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL)$summary$intercept,
                     (sort(conv_rt_ar_obj$summary$intercept)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL)$summary$intercept,
                     (sort(conv_rt_ar_obj_high$summary$intercept)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL)$summary$rate.output)
    # expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL)$rate.output,
    #                  conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, desc(intercept))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, (intercept_b0))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "intercept", n = NULL)$rate.output),
                     length(conv_rt_ar_obj$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "intercept", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })

  # method = "lowest" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = lowest - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL),
                   regexp = "select_rate: Reordering results by 'lowest' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL),
                   regexp = "select_rate: Reordering results by 'lowest' method.")
    # mixed sign object
    expect_error(select_rate(conv_rt_ar_obj, method = "lowest", n = NULL),
                 regexp = "select_rate: Object contains both negative and positive rates.")
  })

  test_that("select_rate: method = lowest - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "lowest", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "lowest", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'lowest' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL)$summary$lowest,
                     rev(sort(conv_rt_ar_obj$summary$lowest)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL)$summary$lowest,
                     rev(sort(conv_rt_ar_obj_high$summary$lowest)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL)$summary$rate.output)
    # expect_identical(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL)$rate.output,
    #                  conv_rt_ar_subset_pos$rate.output[dplyr::arrange(conv_rt_ar_subset_pos$summary, desc(rate))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, desc(rate))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_subset_pos, method = "lowest", n = NULL)$rate.output),
                     length(conv_rt_ar_subset_pos$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "lowest", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })




  # method = "highest" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = highest - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL),
                   regexp = "select_rate: Reordering results by 'highest' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL),
                   regexp = "select_rate: Reordering results by 'highest' method.")
    # mixed sign object
    expect_error(select_rate(conv_rt_ar_obj, method = "highest", n = NULL),
                 regexp = "select_rate: Object contains both negative and positive rates.")
  })

  test_that("select_rate: method = highest - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "highest", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "highest", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'highest' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL)$summary$highest,
                     rev(sort(conv_rt_ar_obj$summary$highest)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL)$summary$highest,
                     rev(sort(conv_rt_ar_obj_high$summary$highest)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL)$summary$rate.output)
    # expect_identical(select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL)$rate.output,
    #                  conv_rt_ar_subset_pos$rate.output[dplyr::arrange(conv_rt_ar_subset_pos$summary, desc(rate))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, (rate))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_subset_pos, method = "highest", n = NULL)$rate.output),
                     length(conv_rt_ar_subset_pos$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "highest", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })




  # method = "minimum" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = minimum - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_subset_pos, method = "minimum", n = NULL),
                   regexp = "select_rate: Reordering results by 'minimum' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL),
                   regexp = "select_rate: Reordering results by 'minimum' method.")
  })

  test_that("select_rate: method = minimum - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "minimum", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "minimum", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'minimum' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL)$summary$minimum,
                     rev(sort(conv_rt_ar_obj$summary$minimum)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL)$summary$minimum,
                     rev(sort(conv_rt_ar_obj_high$summary$minimum)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, (rate))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, (rate))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "minimum", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_mixed_lin$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "minimum", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })




  # method = "maximum" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("select_rate: method = maximum - correct message", {
    # linear object
    expect_message(select_rate(conv_rt_ar_subset_pos, method = "maximum", n = NULL),
                   regexp = "select_rate: Reordering results by 'maximum' method.")
    # highest object
    expect_message(select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL),
                   regexp = "select_rate: Reordering results by 'maximum' method.")
  })

  test_that("select_rate: method = maximum - works and correctly reorders results", {
    skip_on_cran()
    # works
    expect_error(select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL),
                 NA)
    expect_error(select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL),
                 NA)

    # can be plotted
    reorder_obj_lin <- select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(select_rate(empty_obj, method = "maximum", n = NULL),
                 NA)
    expect_message(select_rate(empty_obj, method = "maximum", n = NULL),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'maximum' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL)$summary$maximum,
                     rev(sort(conv_rt_ar_obj$summary$maximum)))
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL)$summary$maximum,
                     rev(sort(conv_rt_ar_obj_high$summary$maximum)))

    # $rate.output element correctly ordered
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL)$rate.output,
                     conv_rt_ar_obj$rate.output[dplyr::arrange(conv_rt_ar_obj$summary, desc(rate))$rank])
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL)$rate.output,
                     select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL)$summary$rate.output)
    expect_identical(select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL)$rate.output,
                     conv_rt_ar_obj_high$rate.output[dplyr::arrange(conv_rt_ar_obj_high$summary, desc(rate))$rank])

    # $subset_regs element correct length
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_lin, method = "maximum", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_mixed_lin$rate.output))
    expect_identical(length(select_rate(conv_rt_ar_obj_mixed_high, method = "maximum", n = NULL)$rate.output),
                     length(conv_rt_ar_obj_high$rate.output))

  })



  # Numerics checks ---------------------------------------------------------

  # Single numeric
  test_that("select_rate: single numerics work", {

    num_sing <- -0.03 %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should NOT remove the only rate
    expect_error(select_rate(num_sing, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(num_sing, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  1)
    # should remove the only rate
    expect_error(select_rate(num_sing, method = "rate", n = c(-0.3, -0.2)),
                 NA)
    expect_length(select_rate(num_sing, method = "rate", n = c(-0.3, -0.2))$rate.output,
                  0)

    # Works piped
    expect_error(
      num_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (num_sing %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # should stop
    expect_error(
      num_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rsq", n = NULL),
      "select_rate: The 'rsq' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "density", n = NULL),
      "select_rate: The 'density' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      "plot.convert_rate: Plot is not available for 'convert_rate' objects containing rates converted from numeric values.")
  })

  # Multiple numerics
  test_that("select_rate: multiple numerics work", {

    num_mult <- c(-0.03, -0.025, -0.021,- 0.051, -0.04) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should remove 3 of 5 rates
    expect_error(select_rate(num_mult, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(num_mult, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  2)
    # should remove 4 of 5 rates
    expect_error(select_rate(num_mult, method = "rate", n = c(-0.3, -0.2)),
                 NA)
    expect_length(select_rate(num_mult, method = "rate", n = c(-0.3, -0.2))$rate.output,
                  1)

    # Works piped
    expect_error(
      num_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (num_mult %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      num_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      num_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # should stop
    expect_error(
      num_mult %>%
        select_rate(method = "rsq", n = NULL),
      "select_rate: The 'rsq' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "oxygen", n = c(8,7)),
      "select_rate: The 'oxygen' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      "plot.convert_rate: Plot is not available for 'convert_rate' objects containing rates converted from numeric values.")
  })

  # Single numeric that has been adjusted
  test_that("select_rate: Single numeric that has been adjusted work", {

    num_sing_adj <- -0.03 %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove single rate
    expect_error(select_rate(num_sing_adj, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(num_sing_adj, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  1)
    # should remove single rates
    expect_error(select_rate(num_sing_adj, method = "rate", n = c(-0.3, -0.2)),
                 NA)
    expect_length(select_rate(num_sing_adj, method = "rate", n = c(-0.3, -0.2))$rate.output,
                  0)

    # Works piped
    expect_error(
      num_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (num_sing_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      num_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      num_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # should stop
    expect_error(
      num_sing_adj %>%
        select_rate(method = "rsq", n = NULL),
      "select_rate: The 'rsq' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "oxygen", n = c(8,7)),
      "select_rate: The 'oxygen' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      "plot.convert_rate: Plot is not available for 'convert_rate' objects containing rates converted from numeric values.")
  })

  # Multiple numeric that has been adjusted
  test_that("select_rate: Multiple numeric that has been adjusted work", {

    num_mult_adj <- c(-0.03, -0.025, -0.021,- 0.051, -0.04) %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should remove 2 of 5 rates
    expect_error(select_rate(num_mult_adj, method = "rate", n = c(-0.4, -0.2)),
                 NA)
    expect_length(select_rate(num_mult_adj, method = "rate", n = c(-0.4, -0.2))$rate.output,
                  3)

    # Works piped
    expect_error(
      num_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.2)) %>%
        select_rate(method = "rank", n = c(1,2)),
      NA)
    expect_length(
      (num_mult_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.2)) %>%
         select_rate(method = "rank", n = c(1,2)))$rate.output,
      2)

    # Works with reordering
    expect_error(
      num_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      num_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # should stop
    expect_error(
      num_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "linear", n = c(8,7)),
      "select_rate: The 'linear' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "oxygen", n = c(8,7)),
      "select_rate: The 'oxygen' method is not accepted for 'convert_rate' objects which have been created using numeric inputs.")

    # should stop
    expect_error(
      num_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      "plot.convert_rate: Plot is not available for 'convert_rate' objects containing rates converted from numeric values.")
  })



  # calc_rate-to-convert_rate object checks ---------------------------------

  # Single rate
  test_that("select_rate: calc_rate-to-convert_rate object with single rate works", {

    cr_sing <-
      urchins.rd[,1:2] %>%
      calc_rate(50, 150, "row", plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove single rate
    expect_error(select_rate(cr_sing, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(cr_sing, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  1)
    # should remove single rates
    expect_error(select_rate(cr_sing, method = "rate", n = c(-0.3, -0.2)),
                 NA)
    expect_length(select_rate(cr_sing, method = "rate", n = c(-0.3, -0.2))$rate.output,
                  0)

    # Works piped
    expect_error(
      cr_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr_sing %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr_sing %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr_sing %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      cr_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      NA)
    expect_output(
      cr_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot())
  })

  # Multiple rates
  test_that("select_rate: calc_rate-to-convert_rate object with multiple rates works", {

    cr_mult <-
      urchins.rd[,1:2] %>%
      calc_rate(seq(10, 100, 10), seq(110, 200, 10), "row", plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(cr_mult, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(cr_mult, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  10)
    # should remove all rates
    expect_error(select_rate(cr_mult, method = "rate", n = c(-0.3, -0.2)),
                 NA)
    expect_length(select_rate(cr_mult, method = "rate", n = c(-0.3, -0.2))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(cr_mult, method = "rate", n = c(-0.35, -0.381)),
                 NA)
    expect_length(select_rate(cr_mult, method = "rate", n = c(-0.35, -0.381))$rate.output,
                  4)

    # Works piped
    expect_error(
      cr_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr_mult %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr_mult %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr_mult %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      cr_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      NA)
    expect_output(
      cr_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot())
  })

  # Single adjusted rate
  test_that("select_rate: calc_rate-to-adjust_rate-to-convert_rate object with single rate works", {

    cr_sing_adj <-
      urchins.rd[,1:2] %>%
      calc_rate(50, 150, "row", plot = FALSE) %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove single rate
    expect_error(select_rate(cr_sing_adj, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(cr_sing_adj, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  1)
    # should remove single rates
    expect_error(select_rate(cr_sing_adj, method = "rate", n = c(-0.3, -0.2)),
                 NA)
    expect_length(select_rate(cr_sing_adj, method = "rate", n = c(-0.3, -0.2))$rate.output,
                  0)

    # Works piped
    expect_error(
      cr_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr_sing_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr_sing_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr_sing_adj %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      cr_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      NA)
    expect_output(
      cr_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot())
  })

  # Multiple adjusted rates
  test_that("select_rate: calc_rate-to-adjust_rate-to-convert_rate object with multiple rates works", {

    cr_mult_adj <-
      urchins.rd[,1:2] %>%
      calc_rate(seq(10, 100, 10), seq(110, 200, 10), "row", plot = FALSE) %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(cr_mult_adj, method = "rate", n = c(-0.4, -0.25)),
                 NA)
    expect_length(select_rate(cr_mult_adj, method = "rate", n = c(-0.4, -0.25))$rate.output,
                  10)
    # should remove all rates
    expect_error(select_rate(cr_mult_adj, method = "rate", n = c(-0.4, -0.5)),
                 NA)
    expect_length(select_rate(cr_mult_adj, method = "rate", n = c(-0.4, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(cr_mult_adj, method = "rate", n = c(-0.28, -0.316)),
                 NA)
    expect_length(select_rate(cr_mult_adj, method = "rate", n = c(-0.28, -0.316))$rate.output,
                  4)

    # Works piped
    expect_error(
      cr_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr_mult_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr_mult_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr_mult_adj %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      cr_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      NA)
    expect_output(
      cr_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot())
  })


  # calc_rate.int checks ----------------------------------------------------

  # Multiple adjusted rates
  test_that("select_rate: calc_rate.int-to-adjust_rate-to-convert_rate object with multiple rates works", {

    cr.int_mult_adj <-
      intermittent.rd %>%
      inspect(plot = FALSE)%>%
      calc_rate.int(starts = c(1, 2101, 3901),
                    wait = 500,
                    measure = 500,
                    by = "row",
                    plot = FALSE) %>%
      adjust_rate(by = -0.00005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "s",
                   output.unit = "mg/h/g",
                   mass = 0.006955,
                   volume = 2.379)

    # should not remove any rates
    expect_error(select_rate(cr.int_mult_adj, method = "rate", n = c(-0.6, -0.8)),
                 NA)
    expect_length(select_rate(cr.int_mult_adj, method = "rate", n = c(-0.6, -0.8))$rate.output,
                  3)
    # should remove all rates
    expect_error(select_rate(cr.int_mult_adj, method = "rate", n = c(-0.4, -0.5)),
                 NA)
    expect_length(select_rate(cr.int_mult_adj, method = "rate", n = c(-0.4, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(cr.int_mult_adj, method = "rate", n = c(-0.7, -0.8)),
                 NA)
    expect_length(select_rate(cr.int_mult_adj, method = "rate", n = c(-0.7, -0.8))$rate.output,
                  1)

    # Works piped
    expect_error(
      cr.int_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr.int_mult_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr.int_mult_adj %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      cr.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      cr.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      cr.int_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })


  # auto_rate.int checks ----------------------------------------------------

  # Multiple adjusted rates
  test_that("select_rate: auto_rate.int-to-adjust_rate-to-convert_rate object with multiple rates works", {

    ar.int_mult_adj <-
      intermittent.rd %>%
      inspect(plot = FALSE)%>%
      auto_rate.int(starts = c(1, 2101, 3901),
                    measure = c(1900, 3550, 4831) - c(1, 2101, 3901),
                    width = 500,
                    plot = FALSE) %>%
      adjust_rate(by = -0.00005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "s",
                   output.unit = "mg/h/g",
                   mass = 0.006955,
                   volume = 2.379)
    #summary(ar.int_mult_adj)
    # should not remove any rates
    expect_error(select_rate(ar.int_mult_adj, method = "rate", n = c(-0.6, -0.8)),
                 NA)
    expect_length(select_rate(ar.int_mult_adj, method = "rate", n = c(-0.6, -0.8))$rate.output,
                  3)
    # should remove all rates
    expect_error(select_rate(ar.int_mult_adj, method = "rate", n = c(-0.4, -0.5)),
                 NA)
    expect_length(select_rate(ar.int_mult_adj, method = "rate", n = c(-0.4, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(ar.int_mult_adj, method = "rate", n = c(-0.7, -0.8)),
                 NA)
    expect_length(select_rate(ar.int_mult_adj, method = "rate", n = c(-0.7, -0.8))$rate.output,
                  1)

    # Works piped
    expect_error(
      ar.int_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar.int_mult_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    ar.int_low_mult_adj <-
      intermittent.rd %>%
      inspect(plot = FALSE)%>%
      auto_rate.int(starts = c(1, 2101, 3901),
                    measure = c(1900, 3550, 4831) - c(1, 2101, 3901),
                    width = 500,
                    method = "lowest",
                    plot = FALSE) %>%
      adjust_rate(by = -0.00005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "s",
                   output.unit = "mg/h/g",
                   mass = 0.006955,
                   volume = 2.379)
    # should stop
    expect_error(
      ar.int_low_mult_adj %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      ar.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar.int_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar.int_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })


  # auto_rate checks --------------------------------------------------------

  # auto_rate-to-convert_rate object with single rate
  test_that("select_rate: auto_rate-to-convert_rate object with single rates works", {

    ar_sing <-
      urchins.rd[,1:2] %>%
      auto_rate(width = 220, plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(ar_sing, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(ar_sing, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  1)
    # should remove all rates
    expect_error(select_rate(ar_sing, method = "rate", n = c(-0.4, -0.5)),
                 NA)
    expect_length(select_rate(ar_sing, method = "rate", n = c(-0.4, -0.5))$rate.output,
                  0)
    # should remove some rates
    # expect_error(select_rate(ar_sing, method = "rate", n = c(-0.7, -0.8)),
    #              NA)
    # expect_length(select_rate(ar_sing, method = "rate", n = c(-0.7, -0.8))$rate.output,
    #               2)

    # Works piped
    expect_error(
      ar_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar_sing %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar_sing %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should NOT stop - this accepts density method
    expect_error(
      ar_sing %>%
        select_rate(method = "density", n = c(8,7)),
      NA)

    # should plot
    expect_error(
      ar_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar_sing %>%
        select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })

  # auto_rate-to-adjust_rate-to-convert_rate object with single rate
  test_that("select_rate: auto_rate-to-adjust_rate-to-convert_rate object with single rates works", {


    ar_sing_adj <-
      urchins.rd[,1:2] %>%
      auto_rate(width = 220, plot = FALSE) %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(ar_sing_adj, method = "rate", n = c(-0.2, -0.3)),
                 NA)
    expect_length(select_rate(ar_sing_adj, method = "rate", n = c(-0.2, -0.3))$rate.output,
                  1)
    # should remove all rates
    expect_error(select_rate(ar_sing_adj, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(ar_sing_adj, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # should remove some rates
    # expect_error(select_rate(ar_sing_adj, method = "rate", n = c(-0.7, -0.8)),
    #              NA)
    # expect_length(select_rate(ar_sing_adj, method = "rate", n = c(-0.7, -0.8))$rate.output,
    #               2)

    # Works piped
    expect_error(
      ar_sing_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar_sing_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar_sing_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar_sing_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar_sing_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should NOT stop - this accepts density method
    expect_error(
      ar_sing_adj %>%
        select_rate(method = "density", n = c(8,7)),
      NA)

    # should plot
    expect_error(
      ar_sing_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar_sing_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar_sing_adj %>%
        select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })

  # auto_rate-to-convert_rate object with multiple rates
  test_that("select_rate: auto_rate-to-convert_rate object with multiple rates works", {


    ar_mult <-
      urchins.rd[,1:2] %>%
      auto_rate(width = 0.2, plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(ar_mult, method = "rate", n = c(-0.5, -0.3)),
                 NA)
    expect_length(select_rate(ar_mult, method = "rate", n = c(-0.5, -0.3))$rate.output,
                  4)
    # should remove all rates
    expect_error(select_rate(ar_mult, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(ar_mult, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(ar_mult, method = "rate", n = c(-0.4, -0.3)),
                 NA)
    expect_length(select_rate(ar_mult, method = "rate", n = c(-0.4, -0.3))$rate.output,
                  2)

    # Works piped
    expect_error(
      ar_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar_mult %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar_mult %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should NOT stop - this accepts density method
    expect_error(
      ar_mult %>%
        select_rate(method = "density", n = c(8,7)),
      NA)

    # should plot
    expect_error(
      ar_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar_mult %>%
        select_rate(method = "rate", n = c(-0.9, -1)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })

  # auto_rate-to-adjust_rate-to-convert_rate object with multiple rates
  test_that("select_rate: auto_rate-to-adjust_rate-to-convert_rate object with multiple rates works", {


    ar_mult_adj <-
      urchins.rd[,1:2] %>%
      auto_rate(width = 0.2, plot = FALSE) %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(ar_mult_adj, method = "rate", n = c(-0.2, -0.4)),
                 NA)
    expect_length(select_rate(ar_mult_adj, method = "rate", n = c(-0.2, -0.4))$rate.output,
                  4)
    # should remove all rates
    expect_error(select_rate(ar_mult_adj, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(ar_mult_adj, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(ar_mult_adj, method = "rate", n = c(-0.32, -0.36)),
                 NA)
    expect_length(select_rate(ar_mult_adj, method = "rate", n = c(-0.32, -0.36))$rate.output,
                  2)

    # Works piped
    expect_error(
      ar_mult_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar_mult_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar_mult_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should NOT stop - this accepts density method
    expect_error(
      ar_mult_adj %>%
        select_rate(method = "density", n = c(8,7)),
      NA)

    # should plot
    expect_error(
      ar_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar_mult_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar_mult_adj %>%
        select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })

  # auto_rate-to-convert_rate object with multiple rates and NOT linear method
  test_that("select_rate: auto_rate-to-convert_rate object with multiple rates and NOT linear method works", {

    ar_mult_low <-
      urchins.rd[,1:2] %>%
      auto_rate(width = 0.2, method = "lowest", plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(ar_mult_low, method = "rate", n = c(-0.5, -0.3)),
                 NA)
    expect_length(select_rate(ar_mult_low, method = "rate", n = c(-0.5, -0.3))$rate.output,
                  218)
    # should remove all rates
    expect_error(select_rate(ar_mult_low, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(ar_mult_low, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(ar_mult_low, method = "rate", n = c(-0.4, -0.39)),
                 NA)
    expect_length(select_rate(ar_mult_low, method = "rate", n = c(-0.4, -0.39))$rate.output,
                  27)

    # Works piped
    expect_error(
      ar_mult_low %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar_mult_low %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar_mult_low %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar_mult_low %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar_mult_low %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      ar_mult_low %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      ar_mult_low %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar_mult_low %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar_mult_low %>%
        select_rate(method = "rate", n = c(-0.9, -1)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })

  # auto_rate-to-adjust_rate-to-convert_rate object with multiple rates and NOT linear method
  test_that("select_rate: auto_rate-to-adjust_rate-to-convert_rate object with multiple rates and NOT linear method works", {


    ar_mult_low_adj <-
      urchins.rd[,1:2] %>%
      auto_rate(width = 0.2, method = "lowest", plot = FALSE) %>%
      adjust_rate(by = -0.005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 1.09,
                   mass = 0.005)

    # should not remove any rates
    expect_error(select_rate(ar_mult_low_adj, method = "rate", n = c(-0.2, -0.4)),
                 NA)
    expect_length(select_rate(ar_mult_low_adj, method = "rate", n = c(-0.2, -0.4))$rate.output,
                  218)
    # should remove all rates
    expect_error(select_rate(ar_mult_low_adj, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(ar_mult_low_adj, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(ar_mult_low_adj, method = "rate", n = c(-0.3, -0.35)),
                 NA)
    expect_length(select_rate(ar_mult_low_adj, method = "rate", n = c(-0.3, -0.35))$rate.output,
                  113)

    # Works piped
    expect_error(
      ar_mult_low_adj %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (ar_mult_low_adj %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      ar_mult_low_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      ar_mult_low_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      ar_mult_low_adj %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      ar_mult_low_adj %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")

    # should plot
    expect_error(
      ar_mult_low_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      NA)
    expect_output(
      ar_mult_low_adj %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot())
    # should NOT plot
    expect_message(
      ar_mult_low_adj %>%
        select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
        plot(),
      "convert_rate: Nothing to plot! No rates found in object.")
  })



  # calc_rate.bg checks -----------------------------------------------------

  # calc_rate.bg-to-convert_rate object with single rate
  test_that("select_rate: calc_rate.bg-to-convert_rate object with single rate works", {

    cr.bg_sing <-
      urchins.rd[,c(1,18)] %>%
      calc_rate.bg(plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h",
                   volume = 1.09)

    # should not remove any rates
    expect_error(select_rate(cr.bg_sing, method = "rate", n = c(-0.04, -0.06)),
                 NA)
    expect_length(select_rate(cr.bg_sing, method = "rate", n = c(-0.04, -0.06))$rate.output,
                  1)
    # should remove all rates
    expect_error(select_rate(cr.bg_sing, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(cr.bg_sing, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # # should remove some rates
    # expect_error(select_rate(cr.bg_sing, method = "rate", n = c(-0.3, -0.35)),
    #              NA)
    # expect_length(select_rate(cr.bg_sing, method = "rate", n = c(-0.3, -0.35))$rate.output,
    #               113)

    # Works piped
    expect_error(
      cr.bg_sing %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr.bg_sing %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr.bg_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr.bg_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr.bg_sing %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr.bg_sing %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")
    expect_error(
      cr.bg_sing %>%
        select_rate(method = "oxygen", n = c(8,7)),
      "select_rate: The 'oxygen' method is not accepted for 'calc_rate.bg' objects because rates may come from different columns of the dataframe.")

    # should NOT plot
    expect_error(
      cr.bg_sing %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      "plot.convert_rate: Plot is not available for converted 'calc_rate.bg' objects")
    # expect_output(
    #   cr.bg_sing %>%
    #     select_rate(method = "rsq", n = NULL) %>%
    #     plot())
    # should NOT plot
    # expect_output(
    #   cr.bg_sing %>%
    #     select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
    #     plot())
    # expect_message(
    #   cr.bg_sing %>%
    #     select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
    #     plot(),
    #   "convert_rate: Nothing to plot! No rates found in object.")
  })

  # calc_rate.bg-to-convert_rate object with multiple rates
  test_that("select_rate: calc_rate.bg-to-convert_rate object with multiple rates works", {

    suppressWarnings(cr.bg_mult <-
                       urchins.rd %>%
                       inspect(1,18:19, plot = FALSE) %>%
                       calc_rate.bg(plot = FALSE) %>%
                       convert_rate(oxy.unit = "mg/l",
                                    time.unit = "min",
                                    output.unit = "mg/h",
                                    volume = 1.09))

    # should not remove any rates
    expect_error(select_rate(cr.bg_mult, method = "rate", n = c(-0.04, -0.06)),
                 NA)
    expect_length(select_rate(cr.bg_mult, method = "rate", n = c(-0.04, -0.06))$rate.output,
                  2)
    # should remove all rates
    expect_error(select_rate(cr.bg_mult, method = "rate", n = c(-0.6, -0.5)),
                 NA)
    expect_length(select_rate(cr.bg_mult, method = "rate", n = c(-0.6, -0.5))$rate.output,
                  0)
    # should remove some rates
    expect_error(select_rate(cr.bg_mult, method = "rate", n = c(-0.03, -0.052)),
                 NA)
    expect_length(select_rate(cr.bg_mult, method = "rate", n = c(-0.03, -0.052))$rate.output,
                  1)

    # Works piped
    expect_error(
      cr.bg_mult %>%
        select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
        select_rate(method = "rate", n = c(-0.3, -0.2)),
      NA)
    expect_length(
      (cr.bg_mult %>%
         select_rate(method = "rate", n = c(-0.4, -0.3)) %>%
         select_rate(method = "rate", n = c(-0.3, -0.2)))$rate.output,
      0)

    # Works with reordering
    expect_error(
      cr.bg_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      NA)
    expect_message(
      cr.bg_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        select_rate(method = "rank", n = NULL),
      "select_rate: Reordering results by 'rank' method.")

    # Works with a method that doesn't work on numerics
    expect_error(
      cr.bg_mult %>%
        select_rate(method = "rsq", n = NULL),
      NA)

    # should stop
    expect_error(
      cr.bg_mult %>%
        select_rate(method = "density", n = c(8,7)),
      "select_rate: The 'density' method is only accepted for rates determined in 'auto_rate' via the 'linear' method.")
    expect_error(
      cr.bg_mult %>%
        select_rate(method = "oxygen_omit", n = c(8,7)),
      "select_rate: The 'oxygen_omit' method is not accepted for 'calc_rate.bg' objects because rates may come from different columns of the dataframe.")

    # should NOT plot
    expect_error(
      cr.bg_mult %>%
        select_rate(method = "rsq", n = NULL) %>%
        plot(),
      "plot.convert_rate: Plot is not available for converted 'calc_rate.bg' objects")
    # expect_output(
    #   cr.bg_mult %>%
    #     select_rate(method = "rsq", n = NULL) %>%
    #     plot())
    # should NOT plot
    # expect_output(
    #   cr.bg_mult %>%
    #     select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
    #     plot())
    # expect_message(
    #   cr.bg_mult %>%
    #     select_rate(method = "rate", n = c(-0.5, -0.4)) %>%
    #     plot(),
    #   "convert_rate: Nothing to plot! No rates found in object.")
  })



  # Misc S3 Checks ----------------------------------------------------------

  test_that("select_rate: empty summary tables don't stop pipes during reordering", {

    cr.int_mult_adj <-
      intermittent.rd %>%
      inspect(plot = FALSE)%>%
      calc_rate.int(starts = c(1, 2101, 3901),
                    from = 500,
                    to = 1000,
                    by = "row",
                    plot = FALSE) %>%
      adjust_rate(by = -0.00005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "s",
                   output.unit = "mg/h/g",
                   mass = 0.006955,
                   volume = 2.379)

    # should not remove any rates
    expect_error(cr.int_mult_adj %>%
                   select_rate(method = "rate", n = c(-0.4, -0.5)) %>%
                   select_rate(method = "time", n = NULL) %>%
                   select_rate(method = "rsq", n = NULL),
                 NA)
    expect_length((cr.int_mult_adj %>%
                     select_rate(method = "rate", n = c(-0.4, -0.5)) %>%
                     select_rate(method = "time", n = NULL) %>%
                     select_rate(method = "rsq", n = NULL))$rate.output,
                  0)
  })

  test_that("select_rate: empty summary tables don't stop pipes during subsetting", {

    cr.int_mult_adj <-
      intermittent.rd %>%
      inspect(plot = FALSE)%>%
      calc_rate.int(starts = c(1, 2101, 3901),
                    from = 500,
                    to = 1000,
                    by = "row",
                    plot = FALSE) %>%
      adjust_rate(by = -0.00005) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "s",
                   output.unit = "mg/h/g",
                   mass = 0.006955,
                   volume = 2.379)

    # should not remove any rates
    expect_error(cr.int_mult_adj %>%
                   select_rate(method = "rate", n = c(-0.4, -0.5)) %>%
                   select_rate(method = "time", n = c(1,100)) %>%
                   select_rate(method = "rsq", n = c(0.95,1)),
                 NA)
    expect_length((cr.int_mult_adj %>%
                     select_rate(method = "rate", n = c(-0.4, -0.5)) %>%
                     select_rate(method = "time", n = c(1,100)) %>%
                     select_rate(method = "rsq", n = c(0.95,1)))$rate.output,
                  0)
  })


  # Remove plot pdf ---------------------------------------------------------

  ## this may cause problems with cmd-check....
  ## .... but apparently not!
  suppressWarnings(file.remove("Rplots.pdf"))

}) ## end capture.output


