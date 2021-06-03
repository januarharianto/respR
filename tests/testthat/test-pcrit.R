sink("/dev/null") ## stops printing outputs on assigning

skip("skip - until we revise this fn")

## pcrit works with default values
# Thin the data so it won't take too long
squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
expect_error(
  suppressMessages(
    pcrit(squid, plot = F, parallel = F)), regexp = NA)

## pcrit parses input rate properly

## correctly defaults to cols 1 and 2 for time and o2 nd test exact value output
expect_equal(round(pcrit(squid, parallel = F, plot=F)$result.midpoint, 4),
             2.5944)

# This analysis does not make sense, but we are just testing that the function
expect_error(pcrit(squid, parallel = F, plot = F), regexp = NA)
expect_error(pcrit(squid, parallel = F, plot = F), regexp = NA)

# works with multi column data frames - again analysis makes no sense
expect_error(pcrit(urchins.rd, parallel = F, plot = F), regexp = NA)
expect_error(pcrit(urchins.rd, parallel = F, plot = F), regexp = NA)
expect_error(pcrit(urchins.rd, parallel = F, plot = F), regexp = NA)

## pcrit S3 generics work
pcr <- pcrit(squid, parallel = F)
expect_output(print(pcr))
expect_output(summary(pcr))
expect_error(plot(pcr), regexp = NA)


## accepts inspec and inspect_data objects
sq_i <- inspect(squid.rd, plot = F)
expect_error(pcrit(sq_i, plot = F, parallel = F), regexp = NA)

sq_id <- suppressWarnings(inspect_data(squid.rd, plot = F))
expect_error(pcrit(sq_id, plot = F, parallel = F), regexp = NA)



sink() ## turns printing back on

