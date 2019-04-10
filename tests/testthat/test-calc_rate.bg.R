
## calc_rate.bg works using default arguments",

crbg <- calc_rate.bg(
  urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "time",
  plot = F)

expect_is(crbg,
          "calc_rate.bg")


