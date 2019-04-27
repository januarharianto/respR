
## subsample outputs is data.frame object
ssm <- subsample(sardine.rd, n = 10)
expect_is(ssm, "data.frame")

## subsample random.start argument works as expected
expect_error(subsample(sardine.rd, random_start = T), regexp = NA)

## subsample will not break when plot argument is changed
expect_error(subsample(sardine.rd, plot = F), regexp = NA)
