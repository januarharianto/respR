library(testit)

ssm <- subsample(sardine.rd, n = 10)
assert(
  "subsample outputs is data.frame object",
  is.data.frame(ssm)
)

assert(
  "subsample random.start argument works as expected",
  !has_error(subsample(sardine.rd, random_start = T))
)

assert(
  "subsample will not break when plot argument is changed",
  !has_error(subsample(sardine.rd, plot = F))
)
