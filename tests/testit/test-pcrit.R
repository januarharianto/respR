library(testit)

# Thin the data so it won't take too long
squid <- squid.rd[seq.int(1, nrow(squid.rd), 100), ]
assert(
  "pcrit works with default values",
  !has_error(
    suppressMessages(
      pcrit(squid, plot = F, parallel = F)
    )
  )
)

