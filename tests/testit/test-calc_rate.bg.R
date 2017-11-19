library(testit)

assert(
  "calc_rate.bg works using default arguments",
  !has_error(calc_rate.bg(
    urchins.rd, xcol = 1, ycol = 18:19, from = 5, to = 45, by = "time",
    plot = F))
)

assert(
  "subset_data returns a data.frame object",
  is.data.frame(subset_data(sardine.rd, 1000, 2000, by = "time")),
  is.data.frame(subset_data(sardine.rd, 1000, 2000, by = "row")),
  is.data.frame(subset_data(sardine.rd, 93, 91, by = "o2")),
  is.data.frame(subset_data(sardine.rd, 1000, 2000, by = "row"))
)
