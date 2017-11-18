library(testit)

rate <- suppressMessages(auto_rate(sardine.rd, parallel = F))
assert(
  "auto_rate works using default arguments",
  class(rate) == "auto_rate",
  is.numeric(rate$rate),
  is.data.frame(rate$df)
  )

assert(
  "auto_rate works by default with `max` and `min` arguments",
  !has_error(suppressMessages(
    auto_rate(sardine.rd, method = "max", plot = F))),
  !has_error(suppressMessages(
    auto_rate(sardine.rd, width = 7500, by = "time", method = "max", plot = F)
    )),
  !has_error(suppressMessages(
    auto_rate(sardine.rd, method = "min", plot = F))),
  !has_error(suppressMessages(
    auto_rate(sardine.rd, width = 7500, by = "time", method = "min", plot = F)
  ))
)

assert(
  "auto_rate will perform interval method using default values",
  !has_error(suppressMessages(
    auto_rate(sardine.rd, method = "interval", plot = F)
  ))
)

assert(
  "auto_rate can be plotted",
  !has_error(plot(rate))
)

assert(
  "auto_rate can be printed",
  !has_error(capture.output(print(rate)))
)

assert(
  "auto_rate can print summary",
  !has_error((capture.output(summary(rate))))
)

sroll <- static_roll(sardine.rd, 1500)
assert(
  "static_roll (auto_rate) outputs a data frame object",
  is.data.frame(sroll)
)


troll <- time_roll(sardine.rd, width = 7500, parallel = FALSE)
assert(
  "time_roll (auto_rate) produces data.frame object",
  is.data.frame(troll)
)

# TODO time_lm
tlm <- time_lm(sardine.rd, 10, 50)
assert(
  "time_lm (auto_rate) produces data.frame object",
  is.data.frame(tlm)
)

# TODO kde_fit
