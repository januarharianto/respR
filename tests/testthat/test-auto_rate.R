## testthat::test_file("tests/testthat/test-auto_rate.R")

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
               regexp = "auto_rate: Input data must be of class 'data.frame' or 'inspect'")
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
               regexp = "auto_rate: The 'method' argument is not recognised: ")
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

test_that("auto_rate works by default with `highest` argument", {
  ar <- auto_rate(urch_data, plot = F, method = "highest")
  expect_is(ar,
            "auto_rate")
  ar <- auto_rate(urch_data, plot = F, method = "highest", width = 10, by = "time")
  expect_is(ar,
            "auto_rate")
})

test_that("auto_rate  works by default with `lowest` argument", {
  ar <- auto_rate(urch_data, plot = F, method = "lowest")
  expect_is(ar,
            "auto_rate")
  ar <- auto_rate(urch_data, plot = F, method = "lowest", width = 10, by = "time")
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

test_that("auto_rate can be plotted with highest/lowest/interval methods", {
  ar <- auto_rate(urch_data, method = "highest", plot = F)
  expect_error(plot(ar), regex = NA)

  ar <- auto_rate(urch_data, method = "lowest", plot = F)
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
  ar <- auto_rate(urch_data, method = "highest", plot = F)
  expect_output(print(ar))
  expect_output(summary(ar))
  expect_output(mean(ar))
  ar <- auto_rate(urch_data, method = "highest", plot = F)
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
               "auto_rate: The 'by' argument must be 'time' or 'row'")
})

test_that("auto_rate works with method = interval and by = time", {
  int_tm <- auto_rate(urch_data, plot = F, method = "interval", by = "Time")
  expect_equal(int_tm$rate[1],
               -0.0335646)
})

test_that("auto_rate works even if width is set to width of entire data", {
  expect_error(auto_rate(urch_data, width = 271, by = "row", method = "lowest", plot = F),
               regexp = NA)
})

test_that("auto_rate works if width is set to less than 1", {
  expect_error(auto_rate(urch_data, width = 0.5, by = "row", method = "lowest", plot = F),
               regexp = NA)
  expect_error(auto_rate(urch_data, width = 0.5, by = "time", method = "lowest", plot = F),
               regexp = NA)
})

test_that("auto_rate stops if width is too high", {
  expect_error(auto_rate(urch_data, width = 272, by = "row", method = "lowest", plot = F),
               regexp = "auto_rate: 'width' exceeds length of dataset")
})


# methods -----------------------------------------------------------------

test_that("auto_rate outputs expected results when method = 'linear'", {

  ar_obj_lin <- auto_rate(urch_data, method = "linear", plot = FALSE)
  ## exact value with these data
  expect_equal(ar_obj_lin$rate[1],
               -0.02926567)
  ## should be 4 results
  expect_equal(length(ar_obj_lin$rate),
               4)
  ## output has elements not in other methods
  expect_false(is.null(ar_obj_lin$metadata$total_peaks))
  expect_false(is.null(ar_obj_lin$metadata$kde_bw))
  expect_false(is.null(ar_obj_lin$density))
  expect_false(is.null(ar_obj_lin$peaks))
  expect_false(is.null(ar_obj_lin$density$bw))
})

test_that("auto_rate outputs expected results when method = 'interval'", {

  ar_obj_int <- auto_rate(urch_data, method = "interval", plot = FALSE)
  ## exact value with these data
  expect_equal(ar_obj_int$rate[1],
               -0.03399183)
  ## should be 5 results
  expect_equal(length(ar_obj_int$rate),
               5)
})

## test objects
ar_obj_high <- auto_rate(urch_data, method = "highest", plot = FALSE)
ar_obj_low <- auto_rate(urch_data, method = "lowest", plot = FALSE)
ar_obj_maximum <- auto_rate(urch_data, method = "maximum", plot = FALSE)
ar_obj_minimum <- auto_rate(urch_data, method = "minimum", plot = FALSE)
ar_obj_max <- suppressWarnings(auto_rate(urch_data, method = "max", plot = FALSE)) # OLD METHOD
ar_obj_min <- suppressWarnings(auto_rate(urch_data, method = "min", plot = FALSE)) # OLD METHOD

## reverse data for testing absolute rate ordering
urch_data_rev <- urch_data
urch_data_rev[[2]] <- rev(urch_data_rev[[2]])

test_that("auto_rate outputs expected results when method = 'highest'", {
  ## these should all be the same with negative rates
  expect_equal(ar_obj_high$rate,
               rev(ar_obj_low$rate),
               ar_obj_min$rate)
  ## should be 218 results
  expect_equal(length(ar_obj_high$rate),
               218)
  ## stops when mix of positive and negative rates
  expect_error(auto_rate(intermittent.rd, method = "highest", plot = FALSE),
               regexp = "auto_rate: Analysis produces both negative and positive rates.")
  ## correctly orders by absolute rate
  expect_equal(ar_obj_high$rate,
               -1 * auto_rate(urch_data_rev, method = "highest", plot = FALSE)$rate)
})

test_that("auto_rate outputs expected results when method = 'lowest'", {
  ## these should all be the same with negative rates
  expect_equal(ar_obj_low$rate,
               rev(ar_obj_high$rate),
               ar_obj_max$rate)
  ## should be 218 results
  expect_equal(length(ar_obj_low$rate),
               218)
  ## stops when mix of positive and negative rates
  expect_error(auto_rate(intermittent.rd, method = "lowest", plot = FALSE),
               regexp = "auto_rate: Analysis produces both negative and positive rates.")
  ## correctly orders by absolute rate
  expect_equal(ar_obj_low$rate,
               -1 * auto_rate(urch_data_rev, method = "lowest", plot = FALSE)$rate)
})

test_that("auto_rate outputs expected results when method = 'maximum'", {
  ## these should all be the same with negative rates
  expect_equal(ar_obj_maximum$rate,
               rev(ar_obj_minimum$rate),
               ar_obj_min$rate)
  ## should be 218 results
  expect_equal(length(ar_obj_maximum$rate),
               218)
  ## message when mix of positive and negative rates
  expect_message(auto_rate(intermittent.rd, method = "maximum", plot = FALSE),
               regexp = "auto_rate: Note dataset contains both negative and positive rates.")
  ## correctly orders by numerical maximum rate
  expect_equal(auto_rate(intermittent.rd, method = "maximum", plot = FALSE)$rate,
               rev(auto_rate(intermittent.rd, method = "minimum", plot = FALSE)$rate))
})

test_that("auto_rate outputs expected results when method = 'minimum'", {
  ## these should all be the same with negative rates
  expect_equal(ar_obj_minimum$rate,
               rev(ar_obj_maximum$rate),
               ar_obj_max$rate)
  ## should be 218 results
  expect_equal(length(ar_obj_minimum$rate),
               218)
  ## message when mix of positive and negative rates
  expect_message(auto_rate(intermittent.rd, method = "minimum", plot = FALSE),
               regexp = "auto_rate: Note dataset contains both negative and positive rates.")
  ## correctly orders by numerical minimum rate
  expect_equal(auto_rate(intermittent.rd, method = "minimum", plot = FALSE)$rate,
               rev(auto_rate(intermittent.rd, method = "maximum", plot = FALSE)$rate))
})


### OLD METHODS OF MAX/MIN
test_that("auto_rate outputs expected results when method = 'max'", {
  ## these should all be the same with negative rates
  expect_equal(ar_obj_max$rate,
               rev(ar_obj_min$rate))
  expect_equal(ar_obj_max$rate,
               ar_obj_minimum$rate)
  ## should be 218 results
  expect_equal(length(ar_obj_max$rate),
               218)
  ## message when mix of positive and negative rates
  expect_message(suppressWarnings(auto_rate(intermittent.rd, method = "max", plot = FALSE)),
               regexp = "auto_rate: Note dataset contains both negative and positive rates.")
  expect_warning(auto_rate(intermittent.rd, method = "max", plot = FALSE),
               regexp = "auto_rate: the 'min' and 'max' methods have been deprecated")
  ## INCORRECTLY orders by numerical max rate
  ## this was the original behaviour
  expect_equal(suppressWarnings(auto_rate(intermittent.rd, method = "max", plot = FALSE)$rate),
               auto_rate(intermittent.rd, method = "minimum", plot = FALSE)$rate)
})

test_that("auto_rate outputs expected results when method = 'min'", {
  ## these should all be the same with negative rates
  expect_equal(ar_obj_min$rate,
               rev(ar_obj_max$rate))
  expect_equal(ar_obj_min$rate,
               ar_obj_maximum$rate)
  ## should be 218 results
  expect_equal(length(ar_obj_min$rate),
               218)
  ## message when mix of positive and negative rates
  expect_message(suppressWarnings(auto_rate(intermittent.rd, method = "min", plot = FALSE)),
               regexp = "auto_rate: Note dataset contains both negative and positive rates.")
  expect_warning(auto_rate(intermittent.rd, method = "min", plot = FALSE),
               regexp = "auto_rate: the 'min' and 'max' methods have been deprecated")
  ## INCORRECTLY orders by numerical min rate
  ## this was the original behaviour
  expect_equal(suppressWarnings(auto_rate(intermittent.rd, method = "min", plot = FALSE)$rate),
               auto_rate(intermittent.rd, method = "maximum", plot = FALSE)$rate)
})

sink() ## turns printing back on
