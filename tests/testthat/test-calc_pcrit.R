sink("/dev/null") ## stops printing outputs on assigning

# Thin the data so it won't take too long
squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]

## pcrit works with default values
expect_error(
    suppressMessages(
      calc_pcrit(squid, plot = F, parallel = F)), regexp = NA)


## accepts inspect objects
sq_i <- inspect(squid, plot = F)
expect_error(calc_pcrit(sq_i, plot = F, parallel = F), regexp = NA)

## accepts inspect_data objects
sq_id <- suppressWarnings(inspect_data(squid, plot = F))
expect_error(calc_pcrit(sq_id, plot = F, parallel = F), regexp = NA)

## accepts data frame objects
expect_error(calc_pcrit(squid, plot = F, parallel = F), regexp = NA)

# stops if not a dataframe
expect_error(calc_pcrit(as.matrix(squid), parallel = F, plot=F),
             regexp = "Input must be data.frame object.")

# stops if width too high
expect_error(calc_pcrit(squid, width = 1000, parallel = F, plot=F),
             regexp = "'width' input is bigger than length of data.")

# stops if both oxygen and rate column IDs entered
expect_error(calc_pcrit(squid, oxygen = 2, rate = 3, parallel = F, plot=F),
             regexp = "Choose either an 'oxygen' or 'rate' column, cannot enter both.")

# stops if oxygen/rate in column 1 and time not specified
expect_error(calc_pcrit(squid, oxygen = 1, parallel = F, plot=F),
             regexp = "Please specify a 'time' argument.")
expect_error(calc_pcrit(squid, rate = 1, parallel = F, plot=F),
             regexp = "Please specify a 'time' argument.")

## works without column specifiers - i.e. assumes time=1, oxygen=2
expect_is(calc_pcrit(squid, parallel = F, plot=F),
          "calc_pcrit")
expect_equal(calc_pcrit(squid, parallel = F, plot=F)$result.intercept,
             2.60027)

## works with only rate specified - i.e. assumes time = 1
## (nonsense analysis and result)
expect_is(calc_pcrit(squid, rate = 2, parallel = F, plot=F),
          "calc_pcrit")
expect_equal(calc_pcrit(squid, rate = 2, parallel = F, plot=F)$result.midpoint,
             19850)


## correctly defaults to cols 1 and 2 for time and o2 and test exact value output
expect_equal(round(calc_pcrit(squid, parallel = F, plot=F)$result.midpoint, 4),
             2.5944)

# This analysis does not make sense, but we are just testing that the function
expect_error(calc_pcrit(squid, time = 1, oxygen = 2, parallel = F, plot = F), regexp = NA)
expect_error(calc_pcrit(squid, time = 1, rate = 2, parallel = F, plot = F), regexp = NA)

# works with multi column data frames - again analysis makes no sense
expect_error(calc_pcrit(urchins.rd, time = 1, oxygen = 12, parallel = F, plot = F), regexp = NA)
expect_error(calc_pcrit(urchins.rd, time = 11, oxygen = 16, parallel = F, plot = F), regexp = NA)
expect_error(calc_pcrit(urchins.rd, time = 11, rate = 16, parallel = F, plot = F), regexp = NA)

## pcrit S3 generics work with both oxygen and rate
# oxygen
pcr <- calc_pcrit(squid, parallel = F, plot = F)
expect_output(print(pcr))
expect_output(summary(pcr))
expect_error(plot(pcr), regexp = NA)
# rate
pcr <- calc_pcrit(squid, rate = 2, parallel = F, plot = F)
expect_output(print(pcr))
expect_output(summary(pcr))
expect_error(plot(pcr), regexp = NA)

## parallel code works
expect_error(pcrit(squid, parallel = T, plot=F),
             regexp = NA)


sink() ## turns printing back on

