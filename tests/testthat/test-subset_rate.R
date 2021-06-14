## testthat::test_file("tests/testthat/test-subset_rate.R")

capture.output({  ## stops printing outputs on assigning

## auto_rate object for testing
## contains both negative and positive rates
suppressWarnings(ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
                   auto_rate(plot = FALSE))
## for testing with zero/non-zero change a couple to zero
ar_obj_w_0 <- ar_obj
ar_obj_w_0$rate[3] <- 0
ar_obj_w_0$rate[9] <- 0

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



# Check duplicates are removed --------------------------------------------

## use sardine.rd here as it produces multiple duplicates
ar_obj_sard <- auto_rate(sardine.rd, plot = FALSE)

test_that("subset_rate: method = NULL   - correct message", {
  expect_message(ar_obj_uniq <- subset_rate(ar_obj_sard, plot = FALSE),
                 regexp = "subset_rate: Subsetting only unique regressions. `n` input ignored")
})

test_that("subset_rate: method = NULL   - works with NULL input", {
  expect_error(ar_obj_uniq <- subset_rate(ar_obj_sard, plot = FALSE),
               regexp = NA)
})

test_that("subset_rate: method = NULL   - correctly removes duplicates", {
  ar_obj_uniq <- subset_rate(ar_obj_sard, method = NULL,
                             plot = FALSE)
  expect_equal(nrow(ar_obj_uniq$summary),
               39)
  expect_equal(ar_obj_uniq$summary[,c(2:4,6:8)], unique(ar_obj_sard$summary[,c(2:4,6:8)]))
})

test_that("subset_rate: method = unique   - correct message", {
  expect_message(ar_obj_uniq <- subset_rate(ar_obj_sard, plot = FALSE),
                 regexp = "subset_rate: Subsetting only unique regressions. `n` input ignored")
})

test_that("subset_rate: method = unique   - works", {
  expect_error(ar_obj_uniq <- subset_rate(ar_obj_sard, method = "unique",
                                          plot = FALSE),
               regexp = NA)
})

test_that("subset_rate: method = unique   - correctly removes duplicates", {
  ar_obj_uniq <- subset_rate(ar_obj_sard, method = "unique",
                             plot = FALSE)
  expect_equal(nrow(ar_obj_uniq$summary),
               39)
  expect_equal(ar_obj_uniq$summary[,c(2:4,6:8)], unique(ar_obj_sard$summary[,c(2:4,6:8)]))
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
                 "Subsetting all positive rate values. `n` input ignored.")
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
                 "Subsetting all negative rate values. `n` input ignored.")
})


# Check "nonzero" method --------------------------------------------------

test_that("subset_rate: method = nonzero  - NO rates should be zero", {
  expect_true(all(ar_subset_nonzero$rate != 0))
})

test_that("subset_rate: method = nonzero  - check message", {
  expect_message(subset_rate(ar_obj, method = "nonzero", plot = FALSE),
                 "Subsetting all non-zero rate values. `n` input ignored.")
})

# Check "zero" method -----------------------------------------------------

test_that("subset_rate: method = zero  - ALL rates should be zero", {
  expect_true(all(ar_subset_zero$rate == 0))
})

test_that("subset_rate: method = zero  - check message", {
  expect_message(subset_rate(ar_obj, method = "zero", plot = FALSE),
                 "Subsetting all zero rate values. `n` input ignored.")
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

  expect_equal(subset_rate(ar_subset_pos, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate,
               subset_rate(ar_subset_pos_flip, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate *-1)
})

# Check "highest_percentile" method ---------------------------------------

test_that("subset_rate: method = highest_percentile   - these should match", {
  ar_subset_pos_flip <- ar_subset_pos
  ar_subset_pos_flip$rate <- ar_subset_pos_flip$rate * -1

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
                                                 n = 3000, plot = FALSE),
               regexp = NA)
})

test_that("subset_rate: with method = row_omit correctly omits rates with single n input", {
  ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                    n = 3000, plot = FALSE)

  ## check omitted row not within rows for each regression
  apply(ar_subset_row_omit$summary, 1, function(x)
    expect_false(3000 %in% x[1]:x[2]))
})

test_that("subset_rate: works with method = row_omit and n input of two values", {
  expect_error(ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                                 n = c(2000,3000), plot = FALSE),
               regexp = NA)

})

test_that("subset_rate: with method = row_omit correctly omits rates n input of two values", {
  ar_subset_row_omit <- subset_rate(ar_obj, method = "row_omit",
                                    n = c(2000, 3000), plot = FALSE)

  ## check omitted region not within rows for each regression
  apply(ar_subset_row_omit$summary, 1, function(x)
    expect_false(2000 %in% x[1]:x[2]))
  apply(ar_subset_row_omit$summary, 1, function(x)
    expect_false(3000 %in% x[1]:x[2]))
})


test_that("subset_rate: stops with method = row_omit and n input greater than 2 values", {
  ## check stops with n not length 1 or 2
  expect_error(subset_rate(ar_obj, method = "row_omit",
                           n = c(3,4,5), plot = FALSE),
               regexp = "subset_rate: For 'row_omit' method 'n' must be a single value or vector of two values.")
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

# Check "time_omit" method ------------------------------------------------

test_that("subset_rate works with method = time_omit and 1 n input", {
  ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                     n = 3000, plot = FALSE)

  ## check omitted row not within rows for each regression
  apply(ar_subset_time_omit$summary, 1, function(x)
    expect_false(3000 %in% x[3]:x[4]))
})

test_that("subset_rate works with method = time_omit and 2 n inputs", {
  ar_subset_time_omit <- subset_rate(ar_obj, method = "time_omit",
                                     n = c(2000, 3000), plot = FALSE)

  ## check omitted region not within rows for each regression
  apply(ar_subset_time_omit$summary, 1, function(x)
    expect_false(2000 %in% x[3]:x[4]))
  apply(ar_subset_time_omit$summary, 1, function(x)
    expect_false(3000 %in% x[3]:x[4]))
})


test_that("subset_rate stops with method = time_omit and n input greater than 2 values", {
  ## check stops with n not length 1 or 2
  expect_error(subset_rate(ar_obj, method = "time_omit",
                           n = c(3,4,5), plot = FALSE),
               regexp = "subset_rate: For 'time_omit' method 'n' must be a single value or vector of two values.")
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

## this may cause problems with cmd-check....
suppressWarnings(file.remove("Rplots.pdf"))

}) ## turns printing back on
