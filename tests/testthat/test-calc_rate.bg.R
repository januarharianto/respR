
## calc_rate.bg works using default arguments",

crbg <- calc_rate.bg(
  urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "time",
  plot = F)

expect_is(crbg,
          "calc_rate.bg")


## Works with variations of `by` input
expect_error(calc_rate.bg(
  urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "Time",
  plot = F), regexp = NA)
expect_error(calc_rate.bg(
  urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "TIME",
  plot = F), regexp = NA)

expect_error(calc_rate.bg(
  urchins.rd, xcol = 1, ycol = 18, from = 20, to = 100, by = "Row", 
  plot = F), regexp = NA)
expect_error(calc_rate.bg(
  urchins.rd, xcol = 1, ycol = 19, from = 20, to = 100, by = "r",
  plot = F), regexp = NA)

## Error with wrong by
expect_error(calc_rate(sardine.rd, plot = F, by = "tttimmmeee"), 
             "`by` input not recognised")

