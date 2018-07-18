library(testit)

assert(
  "calc_rate.bg works using default arguments",
  !has_error(calc_rate.bg(
    urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "time",
    plot = F))
)
