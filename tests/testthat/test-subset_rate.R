
## auto_rate object for testing
## contains both negative and positive rates
ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
  auto_rate(plot = FALSE)
## for testing with zero/non-zero change a couple to zero
ar_obj_w_0 <- ar_obj
ar_obj_w_0$rate[3] <- 0
ar_obj_w_0$rate[9] <- 0

## make subset by method test objects
ar_subset_pos <- subset_rate(ar_obj, method = "positive", plot = FALSE)
ar_subset_neg <- subset_rate(ar_obj, method = "negative", plot = FALSE)
ar_subset_nonzero <- subset_rate(ar_obj_w_0, method = "nonzero", plot = FALSE)
ar_subset_zero <- subset_rate(ar_obj_w_0, method = "zero", plot = FALSE)

# General checks ----------------------------------------------------------

## works with auto_rate input
test_that("subset_rate works with auto_rate input", {
  expect_error(subset_rate(ar_obj, method = "positive", plot = FALSE),
               regexp = NA)})

## works with subset_rate input
## i.e. auto_rate_subset class
## May need to change this depending on how we handle classes
test_that("subset_rate works with auto_rate_subset input", {
  expect_error(subset_rate(ar_subset_pos, method = "positive", plot = FALSE),
               regexp = NA)})

## stops if not auto_rate
expect_error(subset_rate(calc_rate(intermittent.rd, from = 0, to = 1000,
                                   by = "row", plot = FALSE)),
             regexp = "Input is not an 'auto_rate' object")
expect_error(subset_rate(545),
             regexp = "Input is not an 'auto_rate' object")

## stops if no method
expect_error(subset_rate(ar_obj, method = NULL, plot = FALSE),
             regexp = "Please specify a 'method'")

## stops if wrong method
expect_error(subset_rate(ar_subset_pos, method = "blah", plot = FALSE),
             regexp = "'method' input not recognised")

## output inherits auto_rate class
expect_is(ar_subset_pos,
          "auto_rate")

## check custom ADDITIONAL class
expect_is(ar_subset_pos,
          "auto_rate_subset")

## output can be plotted (checking all here)
for(i in 1:length(ar_subset_pos$rate)) expect_output(plot(ar_subset_pos, pos = i))

## expect summary and print
expect_output(summary(ar_subset_pos))
expect_output(print(ar_subset_pos))

## works with pipes
suppressMessages(piped_ar_subset_obj <- inspect(intermittent.rd, plot = FALSE) %>%
                   auto_rate(plot = FALSE) %>%
                   subset_rate(method = "positive", plot = FALSE))

expect_equal(piped_ar_subset_obj$rate, ar_subset_pos$rate)

## works chained
chain <- subset_rate(ar_obj_w_0, method = "nonzero", plot = FALSE) %>%
  subset_rate(method = "positive", plot = FALSE) %>%
  subset_rate(method = "row", n = c(2000, 4100), plot = FALSE) %>%
  subset_rate(method = "highest", n = 2, plot = FALSE)

expect_length(chain$rate, 2)
expect_equal(chain$rate[1], 0.001017504)

## Output
test_that("subset_rate output has saved original auto_rate input", {
  expect_equal(ar_subset_neg$original,
               ar_obj)
})

test_that("subset_rate output has saved subset calls", {
  expect_equal(as.character(ar_subset_neg$subset_calls[[1]]),
               c("subset_rate", "ar_obj", "negative", "FALSE"))
})


# Check "positive" method -------------------------------------------------

## all rates should be positive
expect_true(all(ar_subset_pos$rate > 0))

## check n rates subset
expect_length(ar_subset_pos$rate, 11)

## check some exact values
expect_equal(ar_subset_pos$rate[4], 0.0009992067)

## check message
expect_message(subset_rate(ar_obj, method = "positive", plot = FALSE),
               "Subsetting all positive rate values. `n` input ignored.")


# Check "negative" method -------------------------------------------------

## all rates should be negative
expect_true(all(ar_subset_neg$rate < 0))

## check n rates subset
expect_length(ar_subset_neg$rate, 5)

## check some exact values
expect_equal(ar_subset_neg$rate[4], -0.0005949222)

## check message
expect_message(subset_rate(ar_obj, method = "negative", plot = FALSE),
               "Subsetting all negative rate values. `n` input ignored.")



# Check "nonzero" method --------------------------------------------------

## NO rates should be zero
expect_true(all(ar_subset_nonzero$rate != 0))

## check message
expect_message(subset_rate(ar_obj, method = "nonzero", plot = FALSE),
               "Subsetting all non-zero rate values. `n` input ignored.")

# Check "zero" method -----------------------------------------------------

## ALL rates should be zero
expect_true(all(ar_subset_zero$rate == 0))

## check message
expect_message(subset_rate(ar_obj, method = "zero", plot = FALSE),
               "Subsetting all zero rate values. `n` input ignored.")



# Check "lowest" method ---------------------------------------------------

## error if both neg and pos rates present
expect_error(subset_rate(ar_obj, method = "lowest", n = 7, plot = FALSE),
             regexp = "Object contains both negative and positive rates.")

## if all positive, takes LOWEST 3
ar_subset_low_pos <- subset_rate(ar_subset_pos, method = "lowest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_subset_low_pos$rate),
             3)
## check they are LOWEST three
expect_equal(head(sort(ar_subset_pos$rate), 3), ## head to get lowest
             sort(ar_subset_low_pos$rate))

## if all negative, takes HIGHEST 3
ar_subset_low_neg <- subset_rate(ar_subset_neg, method = "lowest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_subset_low_neg$rate),
             3)
## check they are HIGHEST three
expect_equal(tail(sort(ar_subset_neg$rate), 3), ## tail to get lowest
             sort(ar_subset_low_neg$rate))

## check message
expect_message(subset_rate(ar_subset_pos, method = "lowest", plot = FALSE, n = 3),
               "Subsetting lowest")



# Check "highest" method --------------------------------------------------

## error if both neg and pos rates present
expect_error(subset_rate(ar_obj, method = "highest", n = 7, plot = FALSE),
             regexp = "Object contains both negative and positive rates.")

## if all positive, takes HIGHEST 3
ar_subset_high_pos <- subset_rate(ar_subset_pos, method = "highest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_subset_high_pos$rate),
             3)
## check they are HIGHEST three
expect_equal(tail(sort(ar_subset_pos$rate), 3), ## tail to get highest
             sort(ar_subset_high_pos$rate))

## if all negative, takes LOWEST 3
ar_subset_high_neg <- subset_rate(ar_subset_neg, method = "highest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_subset_high_neg$rate),
             3)
## check they are LOWEST three
expect_equal(head(sort(ar_subset_neg$rate), 3), ## head to get lowest
             sort(ar_subset_high_neg$rate))

## check message
expect_message(subset_rate(ar_subset_pos, method = "highest", plot = FALSE, n = 3),
               "Subsetting highest")



# Check "lowest_percentile" method ----------------------------------------

## change sign of rates for testing
ar_subset_pos_flip <- ar_subset_pos
ar_subset_pos_flip$rate <- ar_subset_pos_flip$rate * -1

## these should match
expect_equal(subset_rate(ar_subset_pos, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate,
             subset_rate(ar_subset_pos_flip, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate *-1
)

# Check "highest_percentile" method ---------------------------------------

## these should match
expect_equal(subset_rate(ar_subset_pos, method = "highest_percentile", n = 0.1, plot = FALSE)$rate,
             subset_rate(ar_subset_pos_flip, method = "highest_percentile", n = 0.1, plot = FALSE)$rate *-1
)


# Check "min" method ------------------------------------------------------
ar_subset_min_n <- subset_rate(ar_obj, method = "min", n = 4, plot = FALSE)

## check n rates extracted
expect_length(ar_subset_min_n$rate,
              4)
## check they are LOWEST n rates
expect_equal(head(sort(ar_obj$rate), 4), ## head to get lowest
             sort(ar_subset_min_n$rate))
## check some exact values
expect_equal(ar_subset_min_n$rate[2], -0.0005968452)
## check message
expect_message(subset_rate(ar_obj, method = "min", n = 4, plot = FALSE),
               "Subsetting minimum")


# Check "max" method ------------------------------------------------------
ar_subset_max_n <- subset_rate(ar_obj, method = "max", n = 4, plot = FALSE)

## check n rates extracted
expect_length(ar_subset_max_n$rate,
              4)
## check they are HIGHEST n rates
expect_equal(tail(sort(ar_obj$rate), 4),
             sort(ar_subset_max_n$rate))
## check some exact values
expect_equal(ar_subset_max_n$rate[2], 0.001605693)
## check message
expect_message(subset_rate(ar_obj, method = "max", n = 4, plot = FALSE),
               "Subsetting maximum")


# Check "min_percentile" method -------------------------------------------

ar_subset_min_perc <- subset_rate(ar_obj, method = "min_percentile", n = 0.1, plot = FALSE)
## these should match (in this case)
expect_equal(ar_subset_min_perc$rate,
             subset_rate(ar_obj, method = "min", n = 2, plot = FALSE)$rate)

## check stops with n not between 0-1
expect_error(subset_rate(ar_obj, method = "min_percentile", n = 4, plot = FALSE),
             regexp = "For 'percentile' methods 'n' must be between 0 and 1.")


# Check "max_percentile" method -------------------------------------------

ar_subset_max_perc <- subset_rate(ar_obj, method = "max_percentile", n = 0.1, plot = FALSE)

## these should match (in this case)
expect_equal(ar_subset_max_perc$rate,
             subset_rate(ar_obj, method = "max", n = 2, plot = FALSE)$rate)

## check stops with n not between 0-1
expect_error(subset_rate(ar_obj, method = "max_percentile", n = 4, plot = FALSE),
             regexp = "For 'percentile' methods 'n' must be between 0 and 1.")


# Check "rate" method -----------------------------------------------------

ar_subset_rate <- subset_rate(ar_obj, method = "rate",
                            n = c(-0.0005, 0.0015), plot = FALSE)

## check all within rate
sapply(ar_subset_rate$rate, function(x) expect_gte(x, -0.005))
sapply(ar_subset_rate$rate, function(x) expect_lte(x, 0.0015))

## check stops with n not length 2 vector
expect_error(subset_rate(ar_obj, method = "rate", n = 4, plot = FALSE),
             regexp = "For 'rate' method 'n' must be a vector of two values.")
expect_error(subset_rate(ar_obj, method = "rate", n = c(1,2,3), plot = FALSE),
             regexp = "For 'rate' method 'n' must be a vector of two values.")


# Check "rsq" method ------------------------------------------------------

ar_subset_rsq <- subset_rate(ar_obj, method = "rsq",
                           n = c(0.7, 1), plot = FALSE)

## check all within rsq range
sapply(ar_subset_rsq$summary$rsq, function(x) expect_gte(x, 0.7))
sapply(ar_subset_rsq$summary$rsq, function(x) expect_lte(x, 1))

## check stops with n not length 2 vector
expect_error(subset_rate(ar_obj, method = "rsq", n = 4, plot = FALSE),
             regexp = "For 'rsq' method 'n' must be a vector of two values.")
expect_error(subset_rate(ar_obj, method = "rsq", n = c(1,2,3), plot = FALSE),
             regexp = "For 'rsq' method 'n' must be a vector of two values.")


# Check "row" method ------------------------------------------------------

ar_subset_row <- subset_rate(ar_obj, method = "row",
                           n = c(2000, 4100), plot = FALSE)

## check all within row range
sapply(ar_subset_row$summary$row, function(x) expect_gte(x, 2000))
sapply(ar_subset_row$summary$endrow, function(x) expect_lte(x, 4100))

## check stops with n not length 2 vector
expect_error(subset_rate(ar_obj, method = "row", n = 4, plot = FALSE),
             regexp = "For 'row' method 'n' must be a vector of two values.")
expect_error(subset_rate(ar_obj, method = "row", n = c(1,2,3), plot = FALSE),
             regexp = "For 'row' method 'n' must be a vector of two values.")


# Check "time" method -----------------------------------------------------

ar_subset_time <- subset_rate(ar_obj, method = "time",
                            n = c(2000,3500), plot = FALSE)

## check all within time range
sapply(ar_subset_time$summary$time, function(x) expect_gte(x, 2000))
sapply(ar_subset_time$summary$endtime, function(x) expect_lte(x, 3500))

## check stops with n not length 2 vector
expect_error(subset_rate(ar_obj, method = "time", n = 4, plot = FALSE),
             regexp = "For 'time' method 'n' must be a vector of two values.")
expect_error(subset_rate(ar_obj, method = "time", n = c(1,2,3), plot = FALSE),
             regexp = "For 'time' method 'n' must be a vector of two values.")



# Check "manual" method -----------------------------------------------------

## keep single rate
ar_subset_man <- subset_rate(ar_obj, method = "manual",
                           n = 1, plot = FALSE)

expect_equal(length(ar_subset_man$rate),
             1)
expect_equal(ar_obj$rate[1],
             ar_subset_man$rate[1])

## keep multiple rates
ar_subset_man <- subset_rate(ar_obj, method = "manual",
                           n = c(3,2), plot = FALSE)

expect_equal(length(ar_subset_man$rate),
             2)
expect_equal(ar_obj$rate[2:3],
             ar_subset_man$rate)

## keep multiple rates non-consecutive
ar_subset_man <- subset_rate(ar_obj, method = "manual",
                           n = c(1,5,9), plot = FALSE)

expect_equal(length(ar_subset_man$rate),
             3)
expect_equal(ar_obj$rate[c(1,5,9)],
             ar_subset_man$rate)


## check stops with n out of range
expect_error(subset_rate(ar_obj, method = "manual", n = 17, plot = FALSE),
             regexp = "For 'manual' method: 'n' values are out of range of ")



# Check "duration" method -------------------------------------------------

## max duration (zero to something)
ar_subset_dur1 <- subset_rate(ar_obj, method = "duration",
                            n = c(0,1000), plot = FALSE)
## should be 11 matches
expect_equal(length(ar_subset_dur1$rate),
             11)

## min duration (something to something large )
ar_subset_dur2 <- subset_rate(ar_obj, method = "duration",
                            n = c(1000, 5000), plot = FALSE)
## should be 5 matches
expect_equal(length(ar_subset_dur2$rate),
             5)

## Test both
expect_equal(length(ar_subset_dur1$rate) + length(ar_subset_dur2$rate),
             length(ar_obj$rate))


# Check "overlap" method --------------------------------------------------

## test errors
test_that("subset_rate stops when n is outside 0 to 1", {
  expect_error(subset_rate(ar_obj, method = "overlap",
                           n = 2, plot = FALSE),
               "For 'overlap' method 'n' must be between 0 and 1 inclusive.")})

test_that("subset_rate stops when auto_rate method is not 'linear'", {
  expect_error(subset_rate(auto_rate(intermittent.rd, method = "max", plot = FALSE),
                           method = "overlap", n = 0.5, plot = FALSE),
               "The 'overlap' method should only be used with results determined via the auto_rate 'linear' method.")})

test_that("subset_rate outputs 4 rates with these inputs", {
  expect_equal(length(subset_rate(ar_obj,
                                  method = "overlap", n = 0.5, plot = FALSE)$rate),
               4)})

test_that("subset_rate outputs 4 rates with these inputs", {
  expect_equal(length(subset_rate(ar_obj,
                                  method = "overlap", n = 0.5, plot = FALSE)$rate),
               4)})

