library(testit)

assert(
  "calc_rate produces object of class `class_rate`",
  class(calc_rate(sardine.rd, plot = F)) == "calc_rate"
)

assert(
  "calc_rate default subsetting methods work",
  !has_error(calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)),
  !has_warning(calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)),
  !has_warning(calc_rate(sardine.rd, 2000, 4000, by = "row", plot = F)),
  !has_warning(calc_rate(sardine.rd, 94, 93, by = "o2", plot = F)),
  !has_warning(calc_rate(sardine.rd, 0.5, 0, by = "proportion", plot = F))
)

rate <- calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)
assert(
  "calc_rate S3 generics work",
  !has_error(print(rate)),
  !has_error(summary(rate)),
  !has_error(plot(rate))
)

assert(
  "calling linear_fit (calc_rate) produces coefficients",
  names(linear_fit(sardine.rd)) == c("intercept_b0", "rate_b1", "rsq")
)
