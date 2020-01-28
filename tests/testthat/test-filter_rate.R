
## auto_rate object for testing
## contains both negative and positive rates
ar_obj <- inspect(intermittent.rd, plot = FALSE) %>%
  auto_rate(plot = FALSE)
## for testing with zero/non-zero change a couple to zero
ar_obj_w_0 <- ar_obj
ar_obj_w_0$rate[3] <- 0
ar_obj_w_0$rate[9] <- 0

## make filter by method test objects
ar_filt_pos <- filter_rate(ar_obj, method = "positive", plot = FALSE)
ar_filt_neg <- filter_rate(ar_obj, method = "negative", plot = FALSE)
ar_filt_nonzero <- filter_rate(ar_obj_w_0, method = "nonzero", plot = FALSE)
ar_filt_zero <- filter_rate(ar_obj_w_0, method = "zero", plot = FALSE)


# General checks ----------------------------------------------------------

## works with auto_rate input
expect_error(filter_rate(ar_obj, method = "positive", plot = FALSE),
             regexp = NA)

## works with filter_rate input
## i.e. auto_rate_filt class
## May need to change this depending on how we handle classes
expect_error(filter_rate(ar_filt_pos, method = "positive", plot = FALSE),
             regexp = NA)

## stops if not auto_rate
expect_error(filter_rate(calc_rate(intermittent.rd, from = 0, to = 1000,
                                   by = "row", plot = FALSE)),
             regexp = "Input is not an 'auto_rate' object")
expect_error(filter_rate(545),
             regexp = "Input is not an 'auto_rate' object")

## stops if wrong method
expect_error(filter_rate(ar_filt_pos, method = "blah", plot = FALSE),
             regexp = "'method' input not recognised")

## output inherits auto_rate class
expect_is(ar_filt_pos,
          "auto_rate")

## check custom ADDITIONAL class
expect_is(ar_filt_pos,
          "auto_rate_filt")

## output can be plotted (checking all here)
for(i in 1:length(ar_filt_pos$rate)) expect_output(plot(ar_filt_pos, pos = i))

## expect summary and print
expect_output(summary(ar_filt_pos))
expect_output(print(ar_filt_pos))

## works with pipes
piped_ar_filt_obj <- inspect(intermittent.rd, plot = FALSE) %>%
  auto_rate(plot = FALSE) %>%
  filter_rate(method = "positive", plot = FALSE) %>%
  expect_equal(ar_filt_pos)

## works chained
chain <- filter_rate(ar_obj_w_0, method = "nonzero", plot = FALSE) %>%
  filter_rate(method = "positive", plot = FALSE) %>%
  filter_rate(method = "row", n = c(2000, 4100), plot = FALSE) %>%
  filter_rate(method = "highest", n = 2, plot = FALSE)

expect_length(chain$rate, 2)
expect_equal(chain$rate[1], 0.001017504)



# Check "positive" method -------------------------------------------------

## all rates should be positive
expect_true(all(ar_filt_pos$rate > 0))

## check n rates filtered
expect_length(ar_filt_pos$rate, 11)

## check some exact values
expect_equal(ar_filt_pos$rate[4], 0.0009992067)

## check message
expect_message(filter_rate(ar_obj, method = "positive", plot = FALSE),
               "Filtering all positive rate values. `n` input ignored.")


# Check "negative" method -------------------------------------------------

## all rates should be negative
expect_true(all(ar_filt_neg$rate < 0))

## check n rates filtered
expect_length(ar_filt_neg$rate, 5)

## check some exact values
expect_equal(ar_filt_neg$rate[4], -0.0005949222)

## check message
expect_message(filter_rate(ar_obj, method = "negative", plot = FALSE),
               "Filtering all negative rate values. `n` input ignored.")



# Check "nonzero" method --------------------------------------------------

## NO rates should be zero
expect_true(all(ar_filt_nonzero$rate != 0))

## check message
expect_message(filter_rate(ar_obj, method = "nonzero", plot = FALSE),
               "Filtering all non-zero rate values. `n` input ignored.")

# Check "zero" method -----------------------------------------------------

## ALL rates should be zero
expect_true(all(ar_filt_zero$rate == 0))

## check message
expect_message(filter_rate(ar_obj, method = "zero", plot = FALSE),
               "Filtering all zero rate values. `n` input ignored.")



# Check "lowest" method ---------------------------------------------------

## error if both neg and pos rates present
expect_error(filter_rate(ar_obj, method = "lowest", n = 7, plot = FALSE),
             regexp = "Object contains both negative and positive rates.")

## if all positive, takes LOWEST 3
ar_filt_low_pos <- filter_rate(ar_filt_pos, method = "lowest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_filt_low_pos$rate),
             3)
## check they are LOWEST three
expect_equal(head(sort(ar_filt_pos$rate), 3), ## head to get lowest
             sort(ar_filt_low_pos$rate))

## if all negative, takes HIGHEST 3
ar_filt_low_neg <- filter_rate(ar_filt_neg, method = "lowest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_filt_low_neg$rate),
             3)
## check they are HIGHEST three
expect_equal(tail(sort(ar_filt_neg$rate), 3), ## tail to get lowest
             sort(ar_filt_low_neg$rate))

## check message
expect_message(filter_rate(ar_filt_pos, method = "lowest", plot = FALSE),
               "Filtering lowest")



# Check "highest" method --------------------------------------------------

## error if both neg and pos rates present
expect_error(filter_rate(ar_obj, method = "highest", n = 7, plot = FALSE),
             regexp = "Object contains both negative and positive rates.")

## if all positive, takes HIGHEST 3
ar_filt_high_pos <- filter_rate(ar_filt_pos, method = "highest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_filt_high_pos$rate),
             3)
## check they are HIGHEST three
expect_equal(tail(sort(ar_filt_pos$rate), 3), ## tail to get highest
             sort(ar_filt_high_pos$rate))

## if all negative, takes LOWEST 3
ar_filt_high_neg <- filter_rate(ar_filt_neg, method = "highest", n = 3, plot = FALSE)
## check n rates extracted
expect_equal(length(ar_filt_high_neg$rate),
             3)
## check they are LOWEST three
expect_equal(head(sort(ar_filt_neg$rate), 3), ## head to get lowest
             sort(ar_filt_high_neg$rate))

## check message
expect_message(filter_rate(ar_filt_pos, method = "highest", plot = FALSE),
               "Filtering highest")



# Check "lowest_percentile" method ----------------------------------------

## change sign of rates for testing
ar_filt_pos_flip <- ar_filt_pos
ar_filt_pos_flip$rate <- ar_filt_pos_flip$rate * -1

## these should match
expect_equal(filter_rate(ar_filt_pos, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate,
             filter_rate(ar_filt_pos_flip, method = "lowest_percentile", n = 0.1, plot = FALSE)$rate *-1
             )

# Check "highest_percentile" method ---------------------------------------

## these should match
expect_equal(filter_rate(ar_filt_pos, method = "highest_percentile", n = 0.1, plot = FALSE)$rate,
             filter_rate(ar_filt_pos_flip, method = "highest_percentile", n = 0.1, plot = FALSE)$rate *-1
             )


# Check "min" method ------------------------------------------------------
ar_filt_min_n <- filter_rate(ar_obj, method = "min", n = 4, plot = FALSE)

## check n rates extracted
expect_length(ar_filt_min_n$rate,
             4)
## check they are LOWEST n rates
expect_equal(head(sort(ar_obj$rate), 4), ## head to get lowest
             sort(ar_filt_min_n$rate))
## check some exact values
expect_equal(ar_filt_min_n$rate[2], -0.0005968452)
## check message
expect_message(filter_rate(ar_obj, method = "min", n = 4, plot = FALSE),
               "Filtering minimum")


# Check "max" method ------------------------------------------------------
ar_filt_max_n <- filter_rate(ar_obj, method = "max", n = 4, plot = FALSE)

## check n rates extracted
expect_length(ar_filt_max_n$rate,
              4)
## check they are HIGHEST n rates
expect_equal(tail(sort(ar_obj$rate), 4),
             sort(ar_filt_max_n$rate))
## check some exact values
expect_equal(ar_filt_max_n$rate[2], 0.001605693)
## check message
expect_message(filter_rate(ar_obj, method = "max", n = 4, plot = FALSE),
               "Filtering maximum")


# Check "min_percentile" method -------------------------------------------

ar_filt_min_perc <- filter_rate(ar_obj, method = "min_percentile", n = 0.1, plot = FALSE)
## these should match (in this case)
expect_equal(ar_filt_min_perc$rate,
             filter_rate(ar_obj, method = "min", n = 2, plot = FALSE)$rate)

## check stops with n not between 0-1
expect_error(filter_rate(ar_obj, method = "min_percentile", n = 4, plot = FALSE),
             regexp = "For 'percentile' methods 'n' must be between 0 and 1.")


# Check "max_percentile" method -------------------------------------------

ar_filt_max_perc <- filter_rate(ar_obj, method = "max_percentile", n = 0.1, plot = FALSE)

## these should match (in this case)
expect_equal(ar_filt_max_perc$rate,
             filter_rate(ar_obj, method = "max", n = 2, plot = FALSE)$rate)

## check stops with n not between 0-1
expect_error(filter_rate(ar_obj, method = "max_percentile", n = 4, plot = FALSE),
             regexp = "For 'percentile' methods 'n' must be between 0 and 1.")


# Check "rate" method -----------------------------------------------------

ar_filt_rate <- filter_rate(ar_obj, method = "rate",
                             n = c(-0.0005, 0.0015), plot = FALSE)

## check all within rate
sapply(ar_filt_rate$rate, function(x) expect_gte(x, -0.005))
sapply(ar_filt_rate$rate, function(x) expect_lte(x, 0.0015))

## check stops with n not length 2 vector
expect_error(filter_rate(ar_obj, method = "rate", n = 4, plot = FALSE),
             regexp = "For 'rate' method 'n' must be a vector of two values.")
expect_error(filter_rate(ar_obj, method = "rate", n = c(1,2,3), plot = FALSE),
             regexp = "For 'rate' method 'n' must be a vector of two values.")


# Check "rsq" method ------------------------------------------------------

ar_filt_rsq <- filter_rate(ar_obj, method = "rsq",
                           n = c(0.7, 1), plot = FALSE)

## check all within rsq range
sapply(ar_filt_rsq$summary$rsq, function(x) expect_gte(x, 0.7))
sapply(ar_filt_rsq$summary$rsq, function(x) expect_lte(x, 1))

## check stops with n not length 2 vector
expect_error(filter_rate(ar_obj, method = "rsq", n = 4, plot = FALSE),
             regexp = "For 'rsq' method 'n' must be a vector of two values.")
expect_error(filter_rate(ar_obj, method = "rsq", n = c(1,2,3), plot = FALSE),
             regexp = "For 'rsq' method 'n' must be a vector of two values.")


# Check "row" method ------------------------------------------------------

ar_filt_row <- filter_rate(ar_obj, method = "row",
                           n = c(2000, 4100), plot = FALSE)

## check all within row range
sapply(ar_filt_row$summary$row, function(x) expect_gte(x, 2000))
sapply(ar_filt_row$summary$endrow, function(x) expect_lte(x, 4100))

## check stops with n not length 2 vector
expect_error(filter_rate(ar_obj, method = "row", n = 4, plot = FALSE),
             regexp = "For 'row' method 'n' must be a vector of two values.")
expect_error(filter_rate(ar_obj, method = "row", n = c(1,2,3), plot = FALSE),
             regexp = "For 'row' method 'n' must be a vector of two values.")


# Check "time" method -----------------------------------------------------

ar_filt_time <- filter_rate(ar_obj, method = "time",
                            n = c(2000,3500), plot = FALSE)

## check all within time range
sapply(ar_filt_time$summary$time, function(x) expect_gte(x, 2000))
sapply(ar_filt_time$summary$endtime, function(x) expect_lte(x, 3500))

## check stops with n not length 2 vector
expect_error(filter_rate(ar_obj, method = "time", n = 4, plot = FALSE),
             regexp = "For 'time' method 'n' must be a vector of two values.")
expect_error(filter_rate(ar_obj, method = "time", n = c(1,2,3), plot = FALSE),
             regexp = "For 'time' method 'n' must be a vector of two values.")



# Check "manual" method -----------------------------------------------------

## keep single rate
ar_filt_man <- filter_rate(ar_obj, method = "manual",
                            n = 1, plot = FALSE)

expect_equal(length(ar_filt_man$rate),
             1)
expect_equal(ar_obj$rate[1],
             ar_filt_man$rate[1])

## keep multiple rates
ar_filt_man <- filter_rate(ar_obj, method = "manual",
                           n = c(3,2), plot = FALSE)

expect_equal(length(ar_filt_man$rate),
             2)
expect_equal(ar_obj$rate[2:3],
             ar_filt_man$rate)

## keep multiple rates non-consecutive
ar_filt_man <- filter_rate(ar_obj, method = "manual",
                           n = c(1,5,9), plot = FALSE)

expect_equal(length(ar_filt_man$rate),
             3)
expect_equal(ar_obj$rate[c(1,5,9)],
             ar_filt_man$rate)


## check stops with n out of range
expect_error(filter_rate(ar_obj, method = "manual", n = 17, plot = FALSE),
             regexp = "For 'manual' method: 'n' values are out of range of ")


