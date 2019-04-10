
## adjust_rate works with numeric inputs
adj <- adjust_rate(100, 20)
expect_is(adj$input,
          "numeric")

## adjust_rate works with input of class `auto_rate` or `calc_rate`
cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
expect_is(cr,
          "adjust_rate")
ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
expect_is(ar,
          "adjust_rate")


## adjust_rate can be printed
expect_output(print(ar))


