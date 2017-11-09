context("calc.rate")

test_that("function works using default arguments", {
    expect_equal(calc.rate(sardine)$rate, -0.0007280136)
  })
test_that("non-df input for x will give an error message", {
  expect_error(calc.rate("sardine"),
    "Input must be a data.frame object.")
})
test_that("non-numeric inputs of `from` and `to` will give error messages", {
  expect_error(calc.rate(sardine, from = NULL, to = 4000))
})
test_that("intermittent respirometry works", {
  expect_equal(nrow(calc.rate(intermittent, c(200,2300,4100), c(1800,3200,4600),
    by = 'time')$results), 3)
})
