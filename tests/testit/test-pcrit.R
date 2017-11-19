library(testit)

# Thin the data so it won't take too long
squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
assert(
  "pcrit works with default values",
  !has_error(
    suppressMessages(
      pcrit(squid, plot = F, parallel = F)))
)


# This analysis does not make sense, but we are just testing that the function
# works with has.rate = TRUE.
assert(
  "pcrit parses input with has.rate properly",
    !has_error(pcrit(squid, has.rate = T, parallel = F))
)

pcr <- pcrit(squid, parallel = F)
assert(
  "pcrit S3 generics work",
  !has_error(print(pcr)),
  !has_error(summary(pcr)),
  !has_error(plot(pcr))
)
