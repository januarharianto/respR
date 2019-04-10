
## calc_rate produces object of class `class_rate`
cr <- calc_rate(sardine.rd, plot = F)
expect_is(cr,
          "calc_rate")

## calc_rate default subsetting methods work
cr <- calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)
expect_is(cr,
          "calc_rate")
cr <- calc_rate(sardine.rd, 2000, 4000, by = "row", plot = F)
expect_is(cr,
          "calc_rate")
cr <- calc_rate(sardine.rd, 94, 93, by = "o2", plot = F)
expect_is(cr,
          "calc_rate")
cr <- calc_rate(sardine.rd, 0.5, 0, by = "proportion", plot = F)
expect_is(cr,
          "calc_rate")

## calc_rate S3 generics work
cr <- calc_rate(sardine.rd, from = 2000, to = 4000, plot = F)

expect_output(print(cr))
expect_output(summary(cr))
expect_output(plot(cr))

## calling linear_fit (calc_rate) produces coefficients

expect_equal(c("intercept_b0", "rate_b1", "rsq"),
             names(linear_fit(sardine.rd)))
