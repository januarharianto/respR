
## pcrit works with default values
# Thin the data so it won't take too long
squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
expect_error(
    suppressMessages(
      pcrit(squid, plot = F, parallel = F)), regexp = NA)

## pcrit parses input with has.rate properly
# This analysis does not make sense, but we are just testing that the function
# works with has.rate = TRUE.
expect_error(pcrit(squid, has.rate = T, parallel = F), regexp = NA)

## pcrit S3 generics work
pcr <- pcrit(squid, parallel = F)
expect_output(print(pcr))
expect_output(summary(pcr))
expect_error(plot(pcr), regexp = NA)
