context("calc_rate")

test_that("calc_rate object is of class `calc_rate`", {
  expect_is(calc_rate(sardine.rd), "calc_rate")
  expect_is(calc_rate(squid.rd), "calc_rate")
})

test_that("function works using default arguments", {
  expect_equal(calc_rate(sardine.rd)$rate, -0.0007280136)
})

test_that("non-df input for x will give an error message", {
  expect_error(calc_rate("sardine.rd"), "Input must be a data.frame object.")
})

test_that("non-numeric inputs of `from` and `to` will give error messages",
  {
    expect_error(calc_rate(sardine.rd, from = NULL, to = 4000))
  })

test_that("analysis of intermittent respirometry works", {
  expect_equal(length(calc_rate(intermittent.rd, c(200, 2300,
    4100), c(1800, 3200, 4600), by = "time")$rate), 3)
})


