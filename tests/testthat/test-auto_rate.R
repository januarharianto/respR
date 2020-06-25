## test_file("tests/testthat/test-auto_rate.R")

sink("/dev/null") ## stops printing outputs on assigning

## 2 col data from urchins.rd so it's fast, but does not output constant
## multi column warnings
urch_data <- urchins.rd[,1:2]


test_that("auto_rate accepts inspect and inspect_data objects", {
  insp_d <- suppressWarnings(inspect_data(urch_data))
  expect_error(auto_rate(insp_d), regexp = NA)
  insp <- suppressWarnings(inspect(urch_data))
  expect_error(auto_rate(insp), regexp = NA)
})

test_that("auto_rate stops if input is not a df (or inspect type object)", {
  expect_error(auto_rate(as.matrix(urch_data[1,1:2])),
               regexp = "Input data must be of class data.frame")
})

test_that("auto_rate accepts multi column dataset", {
  expect_warning(auto_rate(urchins.rd),
                 regexp = "Multi-column dataset detected")
})

test_that("auto_rate subsets out 2 columns from larger dataset", {
  ar <- suppressWarnings(auto_rate(urchins.rd))
  expect_equal(ncol(ar$df), 2)
})

test_that("auto_rate stops with malformed method", {
  expect_error(auto_rate(urch_data, method = "wrong"),
               regexp = "The method argument cannot be recognised")
})

test_that("auto_rate works using default arguments", {
  ar <- auto_rate(urch_data, plot = F)
  expect_is(ar,
            "auto_rate")
  expect_is(ar$rate,
            "numeric")
  expect_is(ar$df,
            "data.frame")
})

test_that("auto_rate works by default with `max` argument", {
  ar <- auto_rate(urch_data, plot = F, method = "max")
  expect_is(ar,
            "auto_rate")
  ar <- auto_rate(urch_data, plot = F, method = "max", width = 10, by = "time")
  expect_is(ar,
            "auto_rate")
})

test_that("auto_rate  works by default with `min` argument", {
  ar <- auto_rate(urch_data, plot = F, method = "min")
  expect_is(ar,
            "auto_rate")
  ar <- auto_rate(urch_data, plot = F, method = "min", width = 10, by = "time")
  expect_is(ar,
            "auto_rate")
})

test_that("auto_rate  will perform interval method using default values", {
  ar <- auto_rate(urch_data, method = "interval", plot = F)
  expect_is(ar,
            "auto_rate")
})

test_that("auto_rate can be plotted with default method", {
  ## NB - This saves a Rplots.pdf to testthat directory
  ## Don't know why
  ## Can't see another way of doing this
  ar <- auto_rate(urch_data)
  expect_error(plot(ar), regex = NA)
  suppressWarnings(file.remove("Rplots.pdf"))
})

test_that("auto_rate can be plotted with max/min/interval methods", {
  ar <- auto_rate(urch_data, method = "max", plot = F)
  expect_error(plot(ar), regex = NA)

  ar <- auto_rate(urch_data, method = "min", plot = F)
  expect_error(plot(ar), regex = NA)

  ar <- auto_rate(urch_data, method = "interval", plot = F)
  expect_error(plot(ar), regex = NA)
  suppressWarnings(file.remove("Rplots.pdf"))
})

test_that("auto_rate: panels 1 to 6 can be plot with choose", {
  ## plots individual panels
  ar <- auto_rate(urch_data, plot = F)
  expect_output(plot(ar, choose =1))
  expect_output(plot(ar, choose =2))
  expect_output(plot(ar, choose =3))
  expect_output(plot(ar, choose =4))
  expect_output(plot(ar, choose =5))
  expect_output(plot(ar, choose =6))
})

test_that("auto_rate: different results can be plot with pos", {
  ## plots individual panels
  ar <- auto_rate(urch_data, plot = F)
  expect_output(plot(ar, pos =1))
  expect_output(plot(ar, pos =2))
  expect_output(plot(ar, pos =3))
  expect_output(plot(ar, pos =4))
})


test_that("auto_rate: S3 generics work for different methods", {
  ar <- auto_rate(urch_data, method = "linear", plot = F)
  expect_output(print(ar))
  expect_output(summary(ar))
  expect_output(mean(ar))
  ar <- auto_rate(urch_data, method = "max", plot = F)
  expect_output(print(ar))
  expect_output(summary(ar))
  expect_output(mean(ar))
  ar <- auto_rate(urch_data, method = "max", plot = F)
  expect_output(print(ar))
  expect_output(summary(ar))
  expect_output(mean(ar))
  ar <- auto_rate(urch_data, method = "interval", plot = F)
  expect_output(print(ar))
  expect_output(summary(ar))
  expect_output(mean(ar))
})

test_that("auto_rate summary works with different pos", {
  ar <- auto_rate(urch_data)
  expect_output(summary(ar, pos = 2))
})

test_that("auto_rate print and summary works when there are more than 5 results", {
  ar <- auto_rate(urch_data, method = "interval", plot = F, width = 0.1)
  expect_output(print(ar))
  expect_output(summary(ar))
})

test_that("auto_rate summary export = TRUE works", {
  ar <- auto_rate(urch_data, plot = F)
  expect_output(summary(ar, export = TRUE))
})

test_that("static_roll (auto_rate) outputs a data frame object", {
  sroll <- static_roll(urch_data, 1500)
  expect_is(sroll,
            "data.frame")
})

test_that("time_roll (auto_rate) produces data.frame object", {
  troll <- time_roll(urch_data, width = 10)
  expect_is(troll,
            "data.frame")
})

test_that("time_lm (auto_rate) produces data.frame object", {
  tlm <- time_lm(urch_data, 10, 50)
  expect_is(tlm,
            "data.frame")
})

test_that("auto_rate works with variations of `by` input", {
  expect_error(auto_rate(urch_data, plot = F, by = "Time"), regexp = NA)
  expect_error(auto_rate(urch_data, plot = F, by = "T"), regexp = NA)
  expect_error(auto_rate(urch_data, plot = F, by = "Row"), regexp = NA)
  expect_error(auto_rate(urch_data, plot = F, by = "r"), regexp = NA)
})

test_that("auto_rate error produced with wrong by", {
  expect_error(auto_rate(urch_data, plot = F, by = "o2"),
               "The by argument must be 'time' or 'row'")
})

test_that("auto_rate works with method = interval and by = time", {
  int_tm <- auto_rate(urch_data, plot = F, method = "interval", by = "Time")
  expect_equal(int_tm$rate[1],
               -0.0335646)
})

test_that("auto_rate works even if width is set to width of entire data", {
  expect_error(auto_rate(urch_data, width = 271, by = "row", method = "min", plot = F),
               regexp = NA)
})

test_that("auto_rate works if width is set to less than 1", {
  expect_error(auto_rate(urch_data, width = 0.5, by = "row", method = "min", plot = F),
               regexp = NA)
  expect_error(auto_rate(urch_data, width = 0.5, by = "time", method = "min", plot = F),
               regexp = NA)
})


sink() ## turns printing back on
