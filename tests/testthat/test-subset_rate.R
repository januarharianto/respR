## library(testthat)
## testthat::test_file("tests/testthat/test-subset_rate.R")

capture.output({  ## stops printing outputs on assigning

  ## auto_rate object for testing
  ## contains both negative and positive rates
  suppressWarnings(ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
                     auto_rate(plot = FALSE))
  ## for testing with zero/non-zero change a couple to zero
  ar_obj_w_0 <- ar_obj
  ar_obj_w_0$rate[3] <- 0
  ar_obj_w_0$summary$rate[3] <- 0
  ar_obj_w_0$rate[9] <- 0
  ar_obj_w_0$summary$rate[9] <- 0

  ## make subset by method test objects
  suppressWarnings(ar_subset_pos <- subset_rate(ar_obj, method = "positive", plot = FALSE))
  suppressWarnings(ar_subset_neg <- subset_rate(ar_obj, method = "negative", plot = FALSE))
  suppressWarnings(ar_subset_nonzero <- subset_rate(ar_obj_w_0, method = "nonzero", plot = FALSE))
  suppressWarnings(ar_subset_zero <- subset_rate(ar_obj_w_0, method = "zero", plot = FALSE))


  ## objects to test subsetting of different auto_rate methods
  ar_obj_high <- auto_rate(urchins.rd[,1:2], method = "highest", plot = FALSE)
  ar_obj_low <- auto_rate(urchins.rd[,1:2], method = "lowest", plot = FALSE)
  ar_obj_maximum <- auto_rate(urchins.rd[,1:2], method = "maximum", plot = FALSE)
  ar_obj_minimum <- auto_rate(urchins.rd[,1:2], method = "minimum", plot = FALSE)
  ar_obj_max <- suppressWarnings(auto_rate(urchins.rd[,1:2], method = "max", plot = FALSE))
  ar_obj_min <- suppressWarnings(auto_rate(urchins.rd[,1:2], method = "min", plot = FALSE))
  ar_obj_int <- auto_rate(urchins.rd[,1:2], method = "interval", plot = FALSE)

  ar_obj_high_sub <- subset_rate(ar_obj_high, method = "rsq", n = c(0.96,1), plot = FALSE)


  # General checks ----------------------------------------------------------

  test_that("subset_rate works with auto_rate input", {
    expect_error(subset_rate(ar_obj, method = "positive", plot = FALSE),
                 regexp = NA)
  })

  test_that("subset_rate works with auto_rate_subset input", {
    expect_error(subset_rate(ar_subset_pos, method = "positive", plot = FALSE),
                 regexp = NA)
  })

  test_that("subset_rate stops if not auto_rate", {
    expect_error(subset_rate(calc_rate(intermittent.rd, from = 0, to = 1000,
                                       by = "row", plot = FALSE)),
                 regexp = "Input is not an 'auto_rate' object")
    expect_error(subset_rate(545),
                 regexp = "Input is not an 'auto_rate' object")
  })

  test_that("subset_rate stops if wrong method", {
    expect_error(subset_rate(ar_subset_pos, method = "blah", plot = FALSE),
                 regexp = "'method' input not recognised")
  })

  test_that("subset_rate output inherits auto_rate class", {
    expect_is(ar_subset_pos,
              "auto_rate")
  })

  test_that("subset_rate output has custom ADDITIONAL class", {
    expect_is(ar_subset_pos,
              "auto_rate_subset")
  })

  test_that("subset_rate output can be plotted", {
    ##  (checking all here)
    for(i in 1:length(ar_subset_pos$rate)) expect_output(plot(ar_subset_pos, pos = i))
  })

  test_that("subset_rate summary and print generics work", {
    expect_output(summary(ar_subset_pos))
    expect_output(print(ar_subset_pos))
  })

  test_that("subset_rate works with pipes", {
    suppressMessages(piped_ar_subset_obj <- inspect(intermittent.rd, plot = FALSE) %>%
                       auto_rate(plot = FALSE) %>%
                       subset_rate(method = "positive", plot = FALSE))

    expect_equal(piped_ar_subset_obj$rate, ar_subset_pos$rate)
  })

  test_that("subset_rate works chained by multiple pipes", {
    chain <- subset_rate(ar_obj_w_0, method = "nonzero", plot = FALSE) %>%
      subset_rate(method = "positive", plot = FALSE) %>%
      subset_rate(method = "row", n = c(2000, 4100), plot = FALSE) %>%
      subset_rate(method = "highest", n = 2, plot = FALSE)

    expect_length(chain$rate, 2)
    expect_equal(chain$rate[1], 0.001017504)
  })

  test_that("subset_rate output has saved original auto_rate input", {
    expect_equal(ar_subset_neg$original,
                 ar_obj)
  })

  test_that("subset_rate output has saved subset calls", {
    expect_equal(as.character(ar_subset_neg$subset_calls[[1]]),
                 c("subset_rate", "ar_obj", "negative", "FALSE"))
  })

  test_that("subset_rate works with auto_rate method = highest object", {
    expect_error(subset_rate(ar_obj_high, method = "rsq", n = c(0.96,1), plot = FALSE),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(subset_rate(ar_obj_high, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_high, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$subset_regs,
                 38)
  })

  test_that("subset_rate works with auto_rate method = lowest object", {
    expect_error(subset_rate(ar_obj_low, method = "rsq", n = c(0.96,1), plot = FALSE),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(subset_rate(ar_obj_low, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_low, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$subset_regs,
                 38)
  })

  test_that("subset_rate works with auto_rate method = maximum object", {
    expect_error(subset_rate(ar_obj_maximum, method = "rsq", n = c(0.96,1), plot = FALSE),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(subset_rate(ar_obj_maximum, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_maximum, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$subset_regs,
                 38)
  })

  test_that("subset_rate works with auto_rate method = minimum object", {
    expect_error(subset_rate(ar_obj_minimum, method = "rsq", n = c(0.96,1), plot = FALSE),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(subset_rate(ar_obj_minimum, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_minimum, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$subset_regs,
                 38)
  })

  test_that("subset_rate works with auto_rate method = max object (OLD METHOD", {
    expect_error(subset_rate(ar_obj_max, method = "rsq", n = c(0.96,1), plot = FALSE),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(subset_rate(ar_obj_max, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_max, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$subset_regs,
                 38)
  })

  test_that("subset_rate works with auto_rate method = min object (OLD METHOD", {
    expect_error(subset_rate(ar_obj_min, method = "rsq", n = c(0.96,1), plot = FALSE),
                 regexp = NA)
    ## should be 38 remaining
    expect_equal(subset_rate(ar_obj_min, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_min, method = "rsq", n = c(0.96,1), plot = FALSE)$metadata$subset_regs,
                 38)
  })

  test_that("subset_rate works with auto_rate method = interval object (OLD METHOD", {
    expect_error(subset_rate(ar_obj_int, method = "rsq", n = c(0.90,0.95), plot = FALSE),
                 regexp = NA)
    ## should be 3 remaining - IN INTERVAL CASE ONLY
    expect_equal(subset_rate(ar_obj_int, method = "rsq", n = c(0.90,0.95), plot = FALSE)$metadata$total_regs,
                 218)
    expect_equal(subset_rate(ar_obj_int, method = "rsq", n = c(0.90,0.95), plot = FALSE)$metadata$subset_regs,
                 3)
  })

  test_that("subset_rate works with empty objects", {

    empty_obj <- subset_rate(ar_obj, method = "rate", n = c(0.1, 1), plot = FALSE)

    # works with S3
    expect_output(print(empty_obj))
    expect_message(print(empty_obj),
                   "No rates found in auto_rate object.")
    expect_output(summary(empty_obj))
    expect_message(summary(empty_obj),
                   "No rates found in auto_rate object.")
    expect_output(mean(empty_obj))
    expect_message(mean(empty_obj),
                   "No rates found in auto_rate object.")

    # subsetting
    expect_error(subset_rate(empty_obj, method = "rsq", n = c(0.90,0.95), plot = FALSE),
                 regexp = NA)
    expect_message(subset_rate(empty_obj, method = "rsq", n = c(0.90,0.95), plot = FALSE),
                   regexp = "----- Subsetting complete. 0 rate\\(s) removed, 0 rate\\(s) remaining -----")
    # reordering
    expect_error(subset_rate(empty_obj, method = "rsq", n = NULL, plot = FALSE),
                 regexp = NA)
    expect_message(subset_rate(empty_obj, method = "rsq", n = NULL, plot = FALSE),
                   regexp = "----- Reordering complete. 0 rate\\(s) reordered by 'rsq' method -----")

  })

  # Check NULL stops fn --------------------------------------------
  test_that("subset_rate: method = NULL   - correct message", {
    expect_error(ar_obj_uniq <- subset_rate(ar_obj_high, plot = FALSE),
                 regexp = "subset_rate: Please specify a 'method'")
  })

  # Check "positive" method -------------------------------------------------

  test_that("subset_rate: method = positive  - all output rates are positive", {
    expect_true(all(ar_subset_pos$rate > 0))
  })

  test_that("subset_rate: method = positive  - check n rates are subset", {
    expect_length(ar_subset_pos$rate, 11)
  })

  test_that("subset_rate: method = positive  - check some exact values", {
    expect_equal(ar_subset_pos$rate[4], 0.0009992067)
  })

  test_that("subset_rate: method = positive  - check message", {
    expect_message(subset_rate(ar_obj, method = "positive", plot = FALSE),
                   "subset_rate: Subsetting all positive rate values. 'n' input ignored...")
  })

  # Check "negative" method -------------------------------------------------

  test_that("subset_rate: method = negative  - all output rates are negative", {
    expect_true(all(ar_subset_neg$rate < 0))
  })

  test_that("subset_rate: method = negative  - check n rates are subset", {
    expect_length(ar_subset_neg$rate, 5)
  })

  test_that("subset_rate: method = negative  - check some exact values", {
    expect_equal(ar_subset_neg$rate[4], -0.0005949222)
  })

  test_that("subset_rate: method = negative  - check message", {
    expect_message(subset_rate(ar_obj, method = "negative", plot = FALSE),
                   "Subsetting all negative rate values. 'n' input ignored.")
  })


  # Check "nonzero" method --------------------------------------------------

  test_that("subset_rate: method = nonzero  - NO rates should be zero", {
    expect_true(all(ar_subset_nonzero$rate != 0))
  })

  test_that("subset_rate: method = nonzero  - check message", {
    expect_message(subset_rate(ar_obj, method = "nonzero", plot = FALSE),
                   "Subsetting all non-zero rate values. 'n' input ignored.")
  })

  # Check "zero" method -----------------------------------------------------

  test_that("subset_rate: method = zero  - ALL rates should be zero", {
    expect_true(all(ar_subset_zero$rate == 0))
  })

  test_that("subset_rate: method = zero  - check message", {
    expect_message(subset_rate(ar_obj, method = "zero", plot = FALSE),
                   "Subsetting all zero rate values. 'n' input ignored.")
  })


  # Check "lowest" method ---------------------------------------------------

  test_that("subset_rate: method = lowest   - error if both neg and pos rates present", {
    expect_error(subset_rate(ar_obj, method = "lowest", n = 7, plot = FALSE),
                 regexp = "Object contains both negative and positive rates.")
  })

  test_that("subset_rate: method = lowest   - check n rates extracted for positive rates", {
    ar_subset_low_pos <- subset_rate(ar_subset_pos, method = "lowest", n = 3, plot = FALSE)
    expect_equal(length(ar_subset_low_pos$rate),
                 3)
  })

  test_that("subset_rate: method = lowest   - check they are LOWEST numerically for positive rates", {
    ar_subset_low_pos <- subset_rate(ar_subset_pos, method = "lowest", n = 3, plot = FALSE)
    expect_equal(head(sort(ar_subset_pos$rate), 3), ## head to get lowest
                 sort(ar_subset_low_pos$rate))
  })

  test_that("subset_rate: method = lowest   - check n rates extracted for negative rates", {
    ar_subset_low_neg <- subset_rate(ar_subset_neg, method = "lowest", n = 3, plot = FALSE)
    expect_equal(length(ar_subset_low_neg$rate),
                 3)
  })

  test_that("subset_rate: method = lowest   - check they are HIGHEST numerically for negative rates", {
    ar_subset_low_neg <- subset_rate(ar_subset_neg, method = "lowest", n = 3, plot = FALSE)
    expect_equal(tail(sort(ar_subset_neg$rate), 3), ## tail to get lowest
                 sort(ar_subset_low_neg$rate))
  })

  test_that("subset_rate: method = lowest   - check message", {
    expect_message(subset_rate(ar_subset_pos, method = "lowest", plot = FALSE, n = 3),
                   "Subsetting lowest")
  })


  # Check "highest" method --------------------------------------------------

  test_that("subset_rate: method = highest   - error if both neg and pos rates present", {
    expect_error(subset_rate(ar_obj, method = "highest", n = 7, plot = FALSE),
                 regexp = "Object contains both negative and positive rates.")
  })

  test_that("subset_rate: method = highest   - check n rates extracted for positive rates", {
    ar_subset_high_pos <- subset_rate(ar_subset_pos, method = "highest", n = 3, plot = FALSE)
    expect_equal(length(ar_subset_high_pos$rate),
                 3)
  })

  test_that("subset_rate: method = highest   - check they are HIGHEST numerically for positive rates", {
    ar_subset_high_pos <- subset_rate(ar_subset_pos, method = "highest", n = 3, plot = FALSE)
    expect_equal(tail(sort(ar_subset_pos$rate), 3), ## tail to get highest
                 sort(ar_subset_high_pos$rate))
  })

  test_that("subset_rate: method = highest   - check n rates extracted for negative rates", {
    ar_subset_high_neg <- subset_rate(ar_subset_neg, method = "highest", n = 3, plot = FALSE)
    expect_equal(length(ar_subset_high_neg$rate),
                 3)
  })

  test_that("subset_rate: method = highest   - check they are LOWEST numerically for negative rates", {
    ar_subset_high_neg <- subset_rate(ar_subset_neg, method = "highest", n = 3, plot = FALSE)
    expect_equal(head(sort(ar_subset_neg$rate), 3), ## head to get lowest
                 sort(ar_subset_high_neg$rate))
  })

  test_that("subset_rate: method = highest   - check message", {
    expect_message(subset_rate(ar_subset_pos, method = "highest", plot = FALSE, n = 3),
                   "Subsetting highest")
  })


  # Check "lowest_percentile" method ----------------------------------------

  test_that("subset_rate: method = lowest_percentile   - these should match", {
    ar_subset_pos_flip <- ar_subset_pos
    ar_subset_pos_flip$rate <- ar_subset_pos_flip$rate * -1
    ar_subset_pos_flip$summary$rate <- ar_subset_pos_flip$summary$rate * -1

    expect_equal(subset_rate(ar_subset_pos, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate,
                 subset_rate(ar_subset_pos_flip, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate *-1)
  })

  # Check "highest_percentile" method ---------------------------------------

  test_that("subset_rate: method = highest_percentile   - these should match", {
    ar_subset_pos_flip <- ar_subset_pos
    ar_subset_pos_flip$rate <- ar_subset_pos_flip$rate * -1
    ar_subset_pos_flip$summary$rate <- ar_subset_pos_flip$summary$rate * -1

    expect_equal(subset_rate(ar_subset_pos, method = "highest_percentile", n = 0.1, plot = FALSE)$rate,
                 subset_rate(ar_subset_pos_flip, method = "highest_percentile", n = 0.1, plot = FALSE)$rate *-1)
  })


  # Check "minimum" method --------------------------------------------------

  ar_subset_min_n <- subset_rate(ar_obj, method = "minimum", n = 4, plot = FALSE)

  test_that("subset_rate: method = minimum   - check n rates extracted", {
    expect_length(ar_subset_min_n$rate,
                  4)
  })

  test_that("subset_rate: method = minimum   - check they are LOWEST n rates", {
    expect_equal(head(sort(ar_obj$rate), 4), ## head to get lowest
                 sort(ar_subset_min_n$rate))
  })

  test_that("subset_rate: method = minimum   - check some exact values", {
    expect_equal(ar_subset_min_n$rate[2], -0.0005968452)
  })

  test_that("subset_rate: method = minimum   - check message", {
    expect_message(subset_rate(ar_obj, method = "minimum", n = 4, plot = FALSE),
                   "Subsetting minimum")
  })

  # Check "maximim" method --------------------------------------------------
  ar_subset_max_n <- subset_rate(ar_obj, method = "maximum", n = 4, plot = FALSE)

  test_that("subset_rate: method = maximum   - check n rates extracted", {
    expect_length(ar_subset_max_n$rate,
                  4)
  })

  test_that("subset_rate: method = maximum   - check they are HIGHEST n rates", {
    expect_equal(tail(sort(ar_obj$rate), 4),
                 sort(ar_subset_max_n$rate))
  })

  test_that("subset_rate: method = maximum   - check some exact values", {
    expect_equal(ar_subset_max_n$rate[2], 0.001605693)
  })

  test_that("subset_rate: method = maximum   - check message", {
    expect_message(subset_rate(ar_obj, method = "maximum", n = 4, plot = FALSE),
                   "Subsetting maximum")
  })

  # Check "minimum_percentile" method -------------------------------------------

  ar_subset_min_perc <- subset_rate(ar_obj, method = "minimum_percentile", n = 0.1, plot = FALSE)

  test_that("subset_rate: method = minimum_percentile   - these should match (in this case)", {
    expect_equal(ar_subset_min_perc$rate,
                 subset_rate(ar_obj, method = "minimum", n = 2, plot = FALSE)$rate)
  })

  test_that("subset_rate: method = minimum_percentile   - check stops with n not between 0-1", {
    expect_error(subset_rate(ar_obj, method = "minimum_percentile", n = 4, plot = FALSE),
                 regexp = "For 'percentile' methods 'n' must be between 0 and 1.")
  })

  # Check "maximum_percentile" method -------------------------------------------

  ar_subset_max_perc <- subset_rate(ar_obj, method = "maximum_percentile", n = 0.1, plot = FALSE)

  test_that("subset_rate: method = maximum_percentile   - these should match (in this case)", {
    expect_equal(ar_subset_max_perc$rate,
                 subset_rate(ar_obj, method = "maximum", n = 2, plot = FALSE)$rate)
  })

  test_that("subset_rate: method = maximum_percentile   - check stops with n not between 0-1", {
    expect_error(subset_rate(ar_obj, method = "maximum_percentile", n = 4, plot = FALSE),
                 regexp = "For 'percentile' methods 'n' must be between 0 and 1.")
  })

  # Check "rate" method -----------------------------------------------------

  ar_subset_rate <- subset_rate(ar_obj, method = "rate",
                                n = c(-0.0005, 0.0015), plot = FALSE)

  test_that("subset_rate: method = rate   - check all within n rate values", {
    sapply(ar_subset_rate$rate, function(x) expect_gte(x, -0.005))
    sapply(ar_subset_rate$rate, function(x) expect_lte(x, 0.0015))
  })

  test_that("subset_rate: method = rate   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "rate", n = 4, plot = FALSE),
                 regexp = "For 'rate' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "rate", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'rate' method 'n' must be a vector of two values.")
  })

  test_that("subset_rate: method = rate   - works if n entered either way round", {
    expect_equal(subset_rate(ar_obj, method = "rate",
                             n = c(0.0010, 0.0015), plot = FALSE)$rate,
                 subset_rate(ar_obj, method = "rate",
                             n = c(0.0015, 0.0010), plot = FALSE)$rate)
  })

  # Check "rsq" method ------------------------------------------------------

  ar_subset_rsq <- subset_rate(ar_obj, method = "rsq",
                               n = c(0.7, 1), plot = FALSE)

  test_that("subset_rate: method = rsq   - check all within rsq n range", {
    sapply(ar_subset_rsq$summary$rsq, function(x) expect_gte(x, 0.7))
    sapply(ar_subset_rsq$summary$rsq, function(x) expect_lte(x, 1))
  })

  test_that("subset_rate: method = rsq   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "rsq", n = 4, plot = FALSE),
                 regexp = "For 'rsq' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "rsq", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'rsq' method 'n' must be a vector of two values.")
  })

  test_that("subset_rate: method = rsq   - works if n entered either way round", {
    expect_equal(subset_rate(ar_obj, method = "rsq",
                             n = c(0.6, 0.7), plot = FALSE)$rate,
                 subset_rate(ar_obj, method = "rsq",
                             n = c(0.7, 0.6), plot = FALSE)$rate)
  })

  # Check "row" method ------------------------------------------------------

  ar_subset_row <- subset_rate(ar_obj, method = "row",
                               n = c(2000, 4100), plot = FALSE)

  test_that("subset_rate: method = row   - check all within row n range", {
    sapply(ar_subset_row$summary$row, function(x) expect_gte(x, 2000))
    sapply(ar_subset_row$summary$endrow, function(x) expect_lte(x, 4100))
  })

  test_that("subset_rate: method = row   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "row", n = 4, plot = FALSE),
                 regexp = "For 'row' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "row", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'row' method 'n' must be a vector of two values.")
  })

  # Check "row_omit" method -------------------------------------------------

  test_that("subset_rate: works with method = row_omit and a single n input", {
    expect_error(ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                                   n = 2201, plot = FALSE),
                 regexp = NA)
    expect_equal(nrow(subset_rate(ar_obj, method = "row_omit",
                                  n = 2201, plot = FALSE)$summary),
                 9)
  })

  test_that("subset_rate: with method = row_omit correctly omits rates with single n input", {
    ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                      n = 2201, plot = FALSE)

    ## check omitted row not within rows for each regression
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(2201 %in% x[1]:x[2]))
  })

  test_that("subset_rate: works with method = row_omit and n input of multiple values", {
    expect_error(ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                                   n = c(1000,2000,3000), plot = FALSE),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(1000 %in% x[1]:x[2]))
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(2000 %in% x[1]:x[2]))
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(3000 %in% x[1]:x[2]))
  })

  test_that("subset_rate: works with method = row_omit and n input of range of values", {
    expect_error(ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                                   n = c(1000:3000), plot = FALSE),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(1000 %in% x[1]:x[2]))
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(2000 %in% x[1]:x[2]))
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(3000 %in% x[1]:x[2]))
  })

  test_that("subset_rate: works with method = row_omit and n input of two values", {
    expect_error(ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                                   n = c(2000,3000), plot = FALSE),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(2000 %in% x[1]:x[2]))
    apply(ar_subset_row_omit$summary, 1, function(x)
      expect_false(3000 %in% x[1]:x[2]))
  })

  test_that("subset_rate: stops with method = row_omit and n input malformed", {
    ## check stops with n not numeric or integer
    expect_error(subset_rate(ar_obj, method = "row_omit",
                             n = "string", plot = FALSE),
                 regexp = "subset_rate: For 'row_omit' method 'n' must only contain integer values of row.")
    expect_error(subset_rate(ar_obj, method = "row_omit",
                             n = 1.2, plot = FALSE),
                 regexp = "subset_rate: For 'row_omit' method 'n' must only contain integer values of row.")
    expect_error(subset_rate(ar_obj, method = "row_omit",
                             n = c(1.2, 2.5), plot = FALSE),
                 regexp = "subset_rate: For 'row_omit' method 'n' must only contain integer values of row.")
    ## check stops when out of range
    expect_error(subset_rate(ar_obj, method = "row_omit",
                             n = 5000, plot = FALSE),
                 regexp = "subset_rate: Input for 'n': row inputs out of data frame range.")
  })



  # Check "time" method -----------------------------------------------------

  test_that("subset_rate: method = time   - check all rates are within time range", {
    ar_subset_time <- subset_rate(ar_obj, method = "time", n = c(2000,3500), plot = FALSE)
    sapply(ar_subset_time$summary$time, function(x) expect_gte(x, 2000))
    sapply(ar_subset_time$summary$endtime, function(x) expect_lte(x, 3500))
  })

  test_that("subset_rate: method = time   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "time", n = 4, plot = FALSE),
                 regexp = "For 'time' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "time", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'time' method 'n' must be a vector of two values.")
  })

  # Check "time_omit" method -------------------------------------------------

  test_that("subset_rate: works with method = time_omit and a single n input", {
    expect_error(ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                                    n = 2201, plot = FALSE),
                 regexp = NA)
    expect_equal(nrow(subset_rate(ar_obj, method = "time_omit",
                                  n = 2201, plot = FALSE)$summary),
                 9)
  })

  test_that("subset_rate: with method = time_omit correctly omits rates with single n input", {
    ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                       n = 2201, plot = FALSE)

    ## check omitted row not within rows for each regression
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(2201 %in% x[1]:x[2]))
  })

  test_that("subset_rate: works with method = time_omit and n input of multiple values", {
    expect_error(ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                                    n = c(1000,2000,3000), plot = FALSE),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(1000 %in% x[1]:x[2]))
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(2000 %in% x[1]:x[2]))
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(3000 %in% x[1]:x[2]))
  })

  test_that("subset_rate: works with method = time_omit and n input of range of values", {
    expect_error(ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                                    n = c(1000:3000), plot = FALSE),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(1000 %in% x[1]:x[2]))
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(2000 %in% x[1]:x[2]))
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(3000 %in% x[1]:x[2]))
  })

  test_that("subset_rate: works with method = time_omit and n input of two values", {
    expect_error(ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                                    n = c(2000,3000), plot = FALSE),
                 regexp = NA)
    ## check omitted rows not within rows for each regression
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(2000 %in% x[1]:x[2]))
    apply(ar_subset_time_omit$summary, 1, function(x)
      expect_false(3000 %in% x[1]:x[2]))
  })

  test_that("subset_rate: stops with method = time_omit and n input malformed", {
    ## check stops with n not numeric
    expect_error(subset_rate(ar_obj, method = "time_omit",
                             n = "string", plot = FALSE),
                 regexp = "subset_rate: For 'time_omit' method 'n' must contain only numeric values of time.")
    ## check stops when out of range
    expect_error(subset_rate(ar_obj, method = "time_omit",
                             n = 5000, plot = FALSE),
                 regexp = "subset_rate: Input for 'n': time inputs out of time data range.")
  })


  # Check "rank" method ------------------------------------------------------
  ar_subset_rank <- subset_rate(ar_obj, method = "rank",
                                n = c(4, 10), plot = FALSE)

  test_that("subset_rate: method = rank   - check all within rank n range", {
    sapply(ar_subset_rank$summary$rank, function(x) expect_gte(x, 4))
    sapply(ar_subset_rank$summary$rank, function(x) expect_lte(x, 10))
  })

  test_that("subset_rate: method = rank   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "rank", n = 4, plot = FALSE),
                 regexp = "For 'rank' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "rank", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'rank' method 'n' must be a vector of two values.")
  })



  # Check "oxygen" method ----------------------------------------------------
  ar_subset_oxy <- subset_rate(ar_obj_high, method = "oxygen",
                               c(7.35, 7), plot = FALSE)

  # this doesn't strictly check proper functonality, in that the method checks
  # *ALL* oxygen values in the original regression
  test_that("subset_rate: method = oxygen   - check all within oxygen n range", {
    sapply(ar_subset_oxy$summary$endoxy, function(x) expect_gte(x, 7))
    sapply(ar_subset_oxy$summary$oxy, function(x) expect_lte(x, 7.35))
  })

  test_that("subset_rate: method = oxygen   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "oxygen", n = 4, plot = FALSE),
                 regexp = "For 'oxygen' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "oxygen", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'oxygen' method 'n' must be a vector of two values.")
  })



  # Check "oxygen_omit" method ----------------------------------------------------
  ar_subset_oxy_om <- subset_rate(ar_obj, method = "oxygen_omit",
                                  c(7.00), plot = FALSE)

  # this doesn't strictly check proper functonality, in that the method checks
  # *ALL* oxygen_omit values in the original regression
  test_that("subset_rate: method = oxygen_omit - no remaining results have ommitted value", {
    for(z in 1:nrow(ar_subset_oxy_om$summary)) {
      mapply(function(p,q) expect_false(any((ar_subset_oxy_om$dataframe$y[p:q] == 7.35))),
             p = ar_subset_oxy_om$summary$row,
             q = ar_subset_oxy_om$summary$endrow)
    }
  })

  test_that("subset_rate: works with method = oxygen_omit and n input of multiple values", {
    expect_error(ar_subset_oxygen_omit <- subset_rate(ar_obj, method = "oxygen_omit",
                                                      n = c(7, 6.9, 6.8), plot = FALSE),
                 regexp = NA)
  })

  test_that("subset_rate: works with method = oxygen_omit and n input of range of values", {
    expect_error(ar_subset_oxygen_omit <- subset_rate(ar_obj, method = "oxygen_omit",
                                                      n = c(7:5), plot = FALSE),
                 regexp = NA)
  })

  test_that("subset_rate: works with method = oxygen_omit and n input of two values", {
    expect_error(ar_subset_oxygen_omit <- subset_rate(ar_obj, method = "oxygen_omit",
                                                      n = c(7,6), plot = FALSE),
                 regexp = NA)
  })

  test_that("subset_rate: stops with method = oxygen_omit and n input malformed", {
    ## check stops with n not numeric
    expect_error(subset_rate(ar_obj, method = "oxygen_omit",
                             n = "string", plot = FALSE),
                 regexp = "subset_rate: For 'oxygen_omit' method 'n' must contain only numeric values of oxygen.")
  })



  # Check "manual" method -----------------------------------------------------

  test_that("subset_rate: method = manual   - subsets a single rate", {
    ar_subset_man <- subset_rate(ar_obj, method = "manual",
                                 n = 1, plot = FALSE)

    expect_equal(length(ar_subset_man$rate),
                 1)
    expect_equal(ar_obj$rate[1],
                 ar_subset_man$rate[1])
  })

  test_that("subset_rate: method = manual   - subsets multiple consecutive rates", {
    ar_subset_man <- subset_rate(ar_obj, method = "manual",
                                 n = c(3,2), plot = FALSE)

    expect_equal(length(ar_subset_man$rate),
                 2)
    expect_equal(ar_obj$rate[2:3],
                 ar_subset_man$rate)
  })

  test_that("subset_rate: method = manual   - subsets multiple non-consecutive rates", {
    ar_subset_man <- subset_rate(ar_obj, method = "manual",
                                 n = c(1,5,9), plot = FALSE)

    expect_equal(length(ar_subset_man$rate),
                 3)
    expect_equal(ar_obj$rate[c(1,5,9)],
                 ar_subset_man$rate)
  })

  test_that("subset_rate: method = manual   - check stops with n out of range", {
    expect_error(subset_rate(ar_obj, method = "manual", n = 17, plot = FALSE),
                 regexp = "For 'manual' method: 'n' values are out of range of ")
  })


  # Check "density" method --------------------------------------------------

  ar_subset_density <- subset_rate(ar_obj, method = "density",
                                   n = c(300, 7500), plot = FALSE)

  test_that("subset_rate: method = density   - check all within density n range", {
    sapply(ar_subset_density$summary$density, function(x) expect_gte(x, 300))
    sapply(ar_subset_density$summary$density, function(x) expect_lte(x, 7500))
  })

  test_that("subset_rate: method = density   - check stops with n not length 2 vector", {
    expect_error(subset_rate(ar_obj, method = "density", n = 4, plot = FALSE),
                 regexp = "For 'density' method 'n' must be a vector of two values.")
    expect_error(subset_rate(ar_obj, method = "density", n = c(1,2,3), plot = FALSE),
                 regexp = "For 'density' method 'n' must be a vector of two values.")
  })

  test_that("subset_rate: method = density   - check stops with auto_rate objects not of 'linear' KDE method", {
    obj <- inspect(intermittent.rd, plot = FALSE) %>%
      auto_rate(method = "maximum", plot = FALSE)
    expect_error(subset_rate(obj, method = "density", n = c(300,7500), plot = FALSE),
                 regexp = "subset_rate: The 'density' method can only be used with results determined via the auto_rate 'linear' method.")

    obj <- inspect(intermittent.rd, plot = FALSE) %>%
      auto_rate(method = "interval", plot = FALSE)
    expect_error(subset_rate(obj, method = "density", n = c(300,7500), plot = FALSE),
                 regexp = "subset_rate: The 'density' method can only be used with results determined via the auto_rate 'linear' method.")
  })



  # Check "duration" method -------------------------------------------------

  test_that("subset_rate: method = duration   - subsets max duration (zero to something) correctly", {
    ar_subset_dur1 <- subset_rate(ar_obj, method = "duration",
                                  n = c(0,1000), plot = FALSE)
    ## should be 11 matches
    expect_equal(length(ar_subset_dur1$rate),
                 11)
  })

  test_that("subset_rate: method = duration   - min duration (something to something large) correctly", {
    ar_subset_dur2 <- subset_rate(ar_obj, method = "duration",
                                  n = c(1000, 5000), plot = FALSE)
    ## should be 5 matches
    expect_equal(length(ar_subset_dur2$rate),
                 5)
  })

  test_that("subset_rate: method = duration   - Test all possible durations match to original", {
    ar_subset_dur1 <- subset_rate(ar_obj, method = "duration",
                                  n = c(0,999), plot = FALSE)
    ar_subset_dur2 <- subset_rate(ar_obj, method = "duration",
                                  n = c(1000, Inf), plot = FALSE)
    expect_equal(length(ar_subset_dur1$rate) + length(ar_subset_dur2$rate),
                 length(ar_obj$rate))
  })

  # Check "overlap" method --------------------------------------------------

  test_that("subset_rate: method = overlap   - stops when n is outside 0 to 1", {
    expect_error(subset_rate(ar_obj, method = "overlap",
                             n = 2, plot = FALSE),
                 "For 'overlap' method 'n' must be between 0 and 1 inclusive.")
  })

  test_that("subset_rate: method = overlap   - outputs 4 rates with these inputs", {
    expect_equal(length(subset_rate(ar_obj,
                                    method = "overlap", n = 0.5, plot = FALSE)$rate),
                 4)
  })

  test_that("subset_rate: method = overlap   - outputs 4 rates with these inputs", {
    expect_equal(length(subset_rate(ar_obj,
                                    method = "overlap", n = 0.5, plot = FALSE)$rate),
                 4)
  })


  test_that("subset_rate - plot defaults are correctly restored", {

    # reset plotting first
    dev.off()
    # save par before
    parb4 <- par(no.readonly = TRUE)
    # now use a fn with plot
    subset_rate(ar_obj_w_0, method = "nonzero", plot = TRUE)
    # save after
    paraft <- par(no.readonly = TRUE)
    # mai is something changed from the default,
    # so if par settings not restored properly this should fail
    expect_identical(parb4$mai,
                     paraft$mai)

  })



  # Reordering checks -------------------------------------------------------

  ## put ar_obj results in a random order
  ar_obj_mixed_lin <- ar_obj
  new_order_lin <- c(10, 9, 14, 13, 5, 2, 1 , 3, 8, 16, 6, 15, 7, 4, 12, 11)
  ar_obj_mixed_lin$summary <- ar_obj_mixed_lin$summary[new_order_lin,]
  ar_obj_mixed_lin$rate <- ar_obj_mixed_lin$summary$rate

  # same with other auto_rate method that's not linear
  ar_obj_mixed_high <- ar_obj_high
  new_order_high <- sample(1:nrow(ar_obj_mixed_high$summary))
  ar_obj_mixed_high$summary <- ar_obj_mixed_high$summary[new_order_high,]
  ar_obj_mixed_high$rate <- ar_obj_mixed_high$summary$rate

  # empty object
  empty_obj <- subset_rate(ar_obj, method = "rate", n = c(0.1, 1), plot = FALSE)


  # method = "rolling" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = rolling - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_lin, method = "rolling", n = 1, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_lin, method = "rolling", n = 1:10, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_high, method = "rolling", n = 1, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rolling' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_high, method = "rolling", n = 1:10, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rolling' method. 'n' input ignored...")
  })

  test_that("subset_rate: method = rolling - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "rolling", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "rolling", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'rolling' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE)$summary$row,
                     sort(ar_obj$summary$row))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE)$summary$row,
                     sort(ar_obj_high$summary$row))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE)$rate,
                     ar_obj$rate[arrange(ar_obj$summary, row)$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, row)$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rolling", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rolling", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })


  # method = "row" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = row - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'row' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'row' method.")
  })

  test_that("subset_rate: method = row - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "row", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "row", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'row' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE)$summary$row,
                     sort(ar_obj$summary$row))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE)$summary$row,
                     sort(ar_obj_high$summary$row))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE)$rate,
                     ar_obj$rate[arrange(ar_obj$summary, row)$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, row)$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "row", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "row", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })


  # method = "time" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = time - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'time' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'time' method.")
  })

  test_that("subset_rate: method = time - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "time", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "time", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'time' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE)$summary$time,
                     sort(ar_obj$summary$time))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE)$summary$time,
                     sort(ar_obj_high$summary$time))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE)$rate,
                     ar_obj$rate[arrange(ar_obj$summary, time)$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, time)$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "time", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "time", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })


  # method = "linear" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = linear - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'linear' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_lin, method = "linear", n = 1, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'linear' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_lin, method = "linear", n = 1:10, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'linear' method. 'n' input ignored...")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'linear' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_high, method = "linear", n = 1, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'linear' method. 'n' input ignored...")
    expect_message(subset_rate(ar_obj_mixed_high, method = "linear", n = 1:10, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'linear' method. 'n' input ignored...")
  })

  test_that("subset_rate: method = linear - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "linear", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "linear", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'linear' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE)$summary$density,
                     (ar_obj$summary$density))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE)$summary$density,
                     (ar_obj_high$summary$density))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE)$rate,
                     ar_obj$rate[arrange(ar_obj$summary, desc(density))$rank])
    # will NOT be rearranged as no values in density column!
    expect_identical(subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE)$rate,
                     ar_obj_mixed_high$rate)

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "linear", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "linear", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "density" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = density - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'density' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'density' method.")
  })

  test_that("subset_rate: method = density - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "density", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "density", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'density' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE)$summary$density,
                     (ar_obj$summary$density))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE)$summary$density,
                     (ar_obj_high$summary$density))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE)$rate,
                     ar_obj$rate[arrange(ar_obj$summary, desc(density))$rank])
    # will NOT be rearranged as no values in density column!
    expect_identical(subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE)$rate,
                     ar_obj_mixed_high$rate)

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "density", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "density", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "rank" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = rank - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rank' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rank' method.")
  })

  test_that("subset_rate: method = rank - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "rank", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "rank", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'rank' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE)$summary$rank,
                     (ar_obj$summary$rank))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE)$summary$rank,
                     (ar_obj_high$summary$rank))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE)$rate,
                     ar_obj$rate[arrange(ar_obj$summary, rank)$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, rank)$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rank", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rank", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "rsq" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = rsq - correct message", {
    # linear object
    expect_message(subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rsq' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'rsq' method.")
  })

  test_that("subset_rate: method = rsq - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "rsq", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "rsq", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'rsq' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE)$summary$rsq,
                     rev(sort(ar_obj$summary$rsq)))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE)$summary$rsq,
                     rev(sort(ar_obj_high$summary$rsq)))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE)$summary$rate)
    # expect_identical(subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE)$rate,
    #                  ar_obj$rate[arrange(ar_obj$summary, desc(rsq))$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, desc(rsq))$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "rsq", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "rsq", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "lowest" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = lowest - correct message", {
    # linear object
    expect_message(subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'lowest' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'lowest' method.")
  })

  test_that("subset_rate: method = lowest - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "lowest", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "lowest", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'lowest' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE)$summary$lowest,
                     rev(sort(ar_obj$summary$lowest)))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE)$summary$lowest,
                     rev(sort(ar_obj_high$summary$lowest)))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE)$summary$rate)
    # expect_identical(subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE)$rate,
    #                  ar_subset_pos$rate[arrange(ar_subset_pos$summary, desc(rate))$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, desc(rate))$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_subset_pos, method = "lowest", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_subset_pos$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "lowest", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "highest" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = highest - correct message", {
    # linear object
    expect_message(subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'highest' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'highest' method.")
  })

  test_that("subset_rate: method = highest - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "highest", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "highest", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'highest' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE)$summary$highest,
                     rev(sort(ar_obj$summary$highest)))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE)$summary$highest,
                     rev(sort(ar_obj_high$summary$highest)))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE)$summary$rate)
    # expect_identical(subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE)$rate,
    #                  ar_subset_pos$rate[arrange(ar_subset_pos$summary, desc(rate))$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, (rate))$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_subset_pos, method = "highest", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_subset_pos$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "highest", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "minimum" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = minimum - correct message", {
    # linear object
    expect_message(subset_rate(ar_subset_pos, method = "minimum", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'minimum' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'minimum' method.")
  })

  test_that("subset_rate: method = minimum - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "minimum", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "minimum", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'minimum' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE)$summary$minimum,
                     rev(sort(ar_obj$summary$minimum)))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE)$summary$minimum,
                     rev(sort(ar_obj_high$summary$minimum)))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE)$rate,
                      ar_obj$rate[arrange(ar_obj$summary, (rate))$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, (rate))$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "minimum", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_mixed_lin$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "minimum", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # method = "maximum" -----------------------------------------------------

  # "rolling" and "linear" methods ignore 'n', all others it must be NULL to reorder
  # So for others only need top test here
  test_that("subset_rate: method = maximum - correct message", {
    # linear object
    expect_message(subset_rate(ar_subset_pos, method = "maximum", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'maximum' method.")
    # highest object
    expect_message(subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE),
                   regexp = "subset_rate: Reordering results by 'maximum' method.")
  })

  test_that("subset_rate: method = maximum - works and correctly reorders results", {

    # works
    expect_error(subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE),
                 NA)
    expect_error(subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE),
                 NA)

    # can be plotted
    reorder_obj_lin <- subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_lin),
                 regex = NA)
    reorder_obj_high <- subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE)
    expect_error(plot(reorder_obj_high),
                 regex = NA)

    # no error with empty object
    expect_error(subset_rate(empty_obj, method = "maximum", n = NULL, plot = FALSE),
                 NA)
    expect_message(subset_rate(empty_obj, method = "maximum", n = NULL, plot = FALSE),
                   "----- Reordering complete. 0 rate\\(s) reordered by 'maximum' method -----")

    # works with S3
    expect_output(print(reorder_obj_lin))
    expect_output(summary(reorder_obj_lin))
    expect_output(mean(reorder_obj_lin))
    expect_output(print(reorder_obj_high))
    expect_output(summary(reorder_obj_high))
    expect_output(mean(reorder_obj_high))

    # summary table correctly ordered for this method
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE)$summary$maximum,
                     rev(sort(ar_obj$summary$maximum)))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE)$summary$maximum,
                     rev(sort(ar_obj_high$summary$maximum)))

    # $rate element correctly ordered
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE)$rate,
                      ar_obj$rate[arrange(ar_obj$summary, desc(rate))$rank])
    expect_identical(subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE)$rate,
                     subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE)$summary$rate)
    expect_identical(subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE)$rate,
                     ar_obj_high$rate[arrange(ar_obj_high$summary, desc(rate))$rank])

    # $subset_regs element correct length
    expect_identical(subset_rate(ar_obj_mixed_lin, method = "maximum", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_mixed_lin$rate))
    expect_identical(subset_rate(ar_obj_mixed_high, method = "maximum", n = NULL, plot = FALSE)$metadata$subset_regs,
                     length(ar_obj_high$rate))

  })




  # Remove plot pdf ---------------------------------------------------------

  ## this may cause problems with cmd-check....
  ## .... but apparently not!
  suppressWarnings(file.remove("Rplots.pdf"))

}) ## turns printing back on


