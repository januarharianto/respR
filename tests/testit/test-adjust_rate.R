library(testit)

adj <- adjust_rate(100, 20)
assert(
  "adjust_rate works with numeric inputs",
  is.numeric(adj$input)
)

assert(
  "adjust_rate works with input of class `auto_rate` or `calc_rate`",
  !has_error(adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)),
  !has_error(suppressMessages(
    adjust_rate(auto_rate(sardine.rd, plot = F), 0.001))
  )
)

assert(
  "adjust_rate can be printed",
  !has_error(capture.output(print(adj)))
)
