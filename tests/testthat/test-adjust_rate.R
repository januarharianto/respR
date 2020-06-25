## library(testthat)
## test_file("tests/testthat/test-adjust_rate.R")

sink("/dev/null") ## stops printing outputs on assigning

test_that("adjust_rate stops if primary input is not numeric vector or calc_rate/auto_rate", {
  bg <- calc_rate.bg(urchins.rd, time = 1, oxygen = 18, plot = F)

  expect_error(adjust_rate("text", bg),
               regexp = "adjust_rate: 'x' must be numeric or an object of class 'calc_rate' or 'auto_rate'")
  expect_error(adjust_rate(urchins.rd, bg),
               regexp = "adjust_rate: 'x' must be numeric or an object of class 'calc_rate' or 'auto_rate'")
})

test_that("adjust_rate accepts calc_rate.bg inputs", {
  bg <- calc_rate.bg(urchins.rd, time = 1, oxygen = 18:19, plot = F)
  expect_error(adjust_rate(100, bg),
               regexp = NA)
})

test_that("adjust_rate accepts numeric inputs", {
  adj <- adjust_rate(100, 20)
  expect_is(adj$input,
            "numeric")

  adj <- adjust_rate(c(100,200,300), 20, 40, "mean")
  expect_is(adj$input,
            "numeric")

  adj <- adjust_rate(100, 20, 40, "mean")
  expect_is(adj$input,
            "numeric")
})

test_that("adjust_rate stops with non-numeric or non calc_rate.bg objects", {
  bg <- calc_rate.bg(urchins.rd, time = 1, oxygen = 18:19, plot = F)
  not_bg <- suppressWarnings(inspect(urchins.rd, time = 1, oxygen = 18:19, plot=FALSE))
  expect_error(adjust_rate(100, not_bg),
               regexp = "adjust_rate: 'by' must be numeric or object of class 'calc_rate.bg'")
  expect_error(adjust_rate(100, bg, not_bg),
               regexp = "adjust_rate: 'by2' must be a single numeric value or 'calc_rate.bg' object containing one column of oxygen data.")
})


test_that("adjust_rate accepts valid methods", {
  cr_obj <- suppressWarnings(calc_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))
  bg_obj2 <- suppressMessages(calc_rate.bg(urchins.rd, 1, 19))

  expect_error(adjust_rate(100, c(10,9), method = "mean"),
               regexp = NA)
  expect_error(adjust_rate(cr_obj, by = bg_obj, by2 = NULL, method = "paired"),
               regexp = NA)
  # expect_error(adjust_rate(cr_obj, bg_obj, bg_obj2, method = "linear"),
  #              regexp = NA)
  # expect_error(adjust_rate(cr_obj, bg_obj,bg_obj2, method = "exponential"),
  #              regexp = NA)
})

test_that("adjust_rate stops with wrong method", {
  expect_error(adjust_rate(100, 10, method = "text"),
               regexp = "adjust_rate: 'method' input not recognised")
})

test_that("adjust_rate works with input of class `auto_rate` or `calc_rate`", {
  cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
  expect_is(cr,
            "adjust_rate")
  cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
  ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
  expect_is(ar,
            "adjust_rate")
})

## method = "mean"
test_that("adjust_rate outputs correct results when method = mean", {

  ## single adjustment/single rate
  expect_equal(adjust_rate(100, by = 10, method = "mean")$adjusted.rate,
               90)
  expect_equal(adjust_rate(100, by = 10, method = "mean")$adjustment,
               10)
  ## multiple adjustment/single rate
  expect_equal(adjust_rate(100, by = c(10,9,8,7,6), method = "mean")$adjusted.rate,
               92)
  expect_equal(adjust_rate(100, by = c(10,9,8,7,6), method = "mean")$adjustment,
               mean(c(10,9,8,7,6)))
  ## single adjustment/multiple rates
  expect_equal(adjust_rate(c(100,90,80,70,60), by = 8, method = "mean")$adjusted.rate,
               c(92,82,72,62,52))
  expect_equal(adjust_rate(c(100,90,80,70,60), by = 8, method = "mean")$adjustment,
               8)
  ## multiple adjustment/multiple rates
  expect_equal(adjust_rate(c(100,90,80,70,60), by = c(10,9,8,7,6), method = "mean")$adjusted.rate,
               c(92,82,72,62,52))
  expect_equal(adjust_rate(c(100,90,80,70,60), by = c(10,9,8,7,6), method = "mean")$adjustment,
               mean(c(10,9,8,7,6)))

  ## single  columns of calc_rate.bg data
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))
  expect_equal(adjust_rate(c(100,90,80,70,60), by = bg_obj, method = "mean")$adjusted.rate,
               c(100,90,80,70,60) - mean(bg_obj$bgrate))

  ## multiple columns of calc_rate.bg data
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18:19))
  expect_equal(adjust_rate(c(100,90,80,70,60), by = bg_obj, method = "mean")$adjusted.rate,
               c(100,90,80,70,60) - mean(bg_obj$bgrate))

  ## single  columns of calc_rate.bg data - with calc_rate
  cr_obj <- suppressWarnings(calc_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))
  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "mean")$adjusted.rate,
               cr_obj$rate - mean(bg_obj$bgrate))

  ## single  columns of calc_rate.bg data - with calc_rate multiple
  cr_obj <- suppressWarnings(calc_rate(urchins.rd, from = c(10,30), to = c(20,40)))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))
  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "mean")$adjusted.rate,
               cr_obj$rate - mean(bg_obj$bgrate))

  ## multiple columns of calc_rate.bg data - with calc_rate
  cr_obj <- suppressWarnings(calc_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18:19))
  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "mean")$adjusted.rate,
               cr_obj$rate - mean(bg_obj$bgrate))

  ## multiple columns of calc_rate.bg data - with calc_rate multiple
  cr_obj <- suppressWarnings(calc_rate(urchins.rd, from = c(10,30), to = c(20,40)))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18:19))
  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "mean")$adjusted.rate,
               cr_obj$rate - mean(bg_obj$bgrate))

})



# method = "paired" -------------------------------------------------------

test_that("adjust_rate stops when method = paired and wrong inputs", {

  cr_obj <- suppressWarnings(calc_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))

  expect_error(adjust_rate(100, bg_obj, method = "paired"),
               regexp = "adjust_rate: For method = \"paired\" the 'x' input must be a calc_rate or auto_rate object.")
  expect_error(adjust_rate(cr_obj, 10, method = "paired"),
               regexp = "adjust_rate: For method = \"paired\" the 'by' input must be a calc_rate.bg object.")
})

test_that("adjust_rate warns when 'x' and 'by' inputs differ in length", {

  ## bg obj shorter
  cr_obj <- suppressWarnings(calc_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd[1:200,], 1, 18))

  expect_warning(adjust_rate(cr_obj, bg_obj, method = "paired"),
                 regexp = "adjust_rate: 'x' and 'by' inputs differ in length by more than 5%.")

  ## rate obj shorter
  cr_obj <- suppressWarnings(calc_rate(urchins.rd[1:200,]))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))

  expect_warning(adjust_rate(cr_obj, bg_obj, method = "paired"),
                 regexp = "adjust_rate: 'x' and 'by' inputs differ in length by more than 5%.")
})

test_that("adjust_rate warns when some time values missing in by or x", {

  ## bg obj shorter
  ar_obj <- suppressWarnings(auto_rate(urchins.rd, plot = F))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd[1:200,], 1, 18))

  expect_warning(adjust_rate(ar_obj, bg_obj, method = "paired"),
                 regexp = "adjust_rate: Some time values used in 'x' rate calculations not found in 'by' background data.")

  ## rate obj shorter - this should not matter, since this is where the times come from

})


test_that("adjust_rate outputs correct results when method = paired", {

  ## calc_rate - one rate - should be same as mean method if using whole dataset
  cr_obj <- suppressWarnings(calc_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))

  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "paired")$adjusted.rate,
               adjust_rate(cr_obj, by = bg_obj, method = "mean")$adjusted.rate)

  ## adjusted rate should be rate minus rate from that same region of bg data
  cr_obj <- suppressWarnings(calc_rate(urchins.rd, from = 10, to = 20))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))

  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "paired")$adjusted.rate,
               cr_obj$rate - calc_rate(urchins.rd[,c(1,18)], from = 10, to = 20)$rate)

  ## calc_rate - multiple rates
  cr_obj <- suppressWarnings(calc_rate(urchins.rd, from = c(10,30), to = c(20,40)))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))

  expect_equal(adjust_rate(cr_obj, by = bg_obj, method = "paired")$adjusted.rate,
               cr_obj$rate - calc_rate(urchins.rd[,c(1,18)], from = c(10,30), to = c(20,40))$rate)

  ## auto_rate - multiple rates
  ar_obj <- suppressWarnings(auto_rate(urchins.rd))
  bg_obj <- suppressMessages(calc_rate.bg(urchins.rd, 1, 18))

  expect_equal(adjust_rate(ar_obj, by = bg_obj, method = "paired")$adjusted.rate,
               ar_obj$rate - calc_rate(urchins.rd[,c(1,18)],
                                       from = ar_obj$summary$time,
                                       to = ar_obj$summary$endtime)$rate)


})



# method = "linear" -------------------------------------------------------

test_that("adjust_rate stops if method = linear and x and t_x are numeric and different lengths", {
  expect_error(adjust_rate(x = -c(100,110,120), by = 0, by2 = -50, t_x = 50,
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = "'t_x' input should be the same length as the 'x' input.")
  expect_error(adjust_rate(x = -100, by = 0, by2 = -50, t_x = c(50,60),
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = "'t_x' input should be the same length as the 'x' input.")
})




test_that("adjust_rate outputs correct results when method = linear", {

  ## numeric single inputs
  expect_error(adjust_rate(x = -100, by = 0, by2 = -50, t_x = 50,
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = NA)
  expect_equal(adjust_rate(x = -100, by = 0, by2 = -50, t_x = 50,
                           t_by = 0, t_by2 = 100, method = "linear")$adjusted.rate,
               -75)
  ## with calc_rate.bg for by, by2 and both
  bg_obj1 <- suppressMessages(calc_rate.bg(urchins.rd[1:10,], 1, 18))
  bg_obj2 <- suppressMessages(calc_rate.bg(urchins.rd[250:271,], 1, 18))
  ##  by
  expect_error(adjust_rate(x = -0.033, by = bg_obj1, by2 = -0.007030772, t_x = 15,
                           t_by = NULL, t_by2 = 43.25, method = "linear"),
               regexp = NA)
  expect_equal(adjust_rate(x = -0.033, by = bg_obj1, by2 = -0.007030772, t_x = 15,
                           t_by = NULL, t_by2 = 43.25, method = "linear")$adjustment,
               -0.005878997)
  ##  by2
  expect_error(adjust_rate(x = -0.033, by = -0.005298013, by2 = bg_obj2, t_x = 15,
                           t_by = 0.75, t_by2 = NULL, method = "linear"),
               regexp = NA)
  expect_equal(adjust_rate(x = -0.033, by = -0.005298013, by2 = bg_obj2, t_x = 15,
                           t_by = 0.75, t_by2 = NULL, method = "linear")$adjustment,
               -0.005878997)
  ##  both
  expect_error(adjust_rate(x = -0.033, by = bg_obj1, by2 = bg_obj2, t_x = 15,
                           t_by = NULL, t_by2 = NULL, method = "linear"),
               regexp = NA)
  expect_equal(adjust_rate(x = -0.033, by = bg_obj1, by2 = bg_obj2, t_x = 15,
                           t_by = NULL, t_by2 = NULL, method = "linear")$adjustment,
               -0.005878997)


  ## numeric vector inputs
  expect_error(adjust_rate(x = -c(100,110,120), by = 0, by2 = -50, t_x = c(50,60,70),
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = NA)
  expect_equal(adjust_rate(x = -c(100,110,120), by = 0, by2 = -50, t_x = c(50,60,70),
                           t_by = 0, t_by2 = 100, method = "linear")$adjusted.rate,
               c(-75, -80, -85))

  ## calc_rate input - one rate
  cr_obj <- suppressWarnings(calc_rate(urchins.rd)) ## whole dataset
  ## accepts calc_rate object
  expect_error(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = NA)
  ## should apply exactly half of the by2 correction if over same time period
  expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 45, method = "linear")$adjustment,
               -0.01/2)
  expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 45, method = "linear")$adjusted.rate,
               -0.02282768)
  ## successfully ignores t_x input
  expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01, t_x = 1000,
                           t_by = 0, t_by2 = 45, method = "linear")$adjusted.rate,
               -0.02282768)

  ## calc_rate input - multiples rates
  cr_obj <- suppressWarnings(calc_rate(urchins.rd, from = c(0,10,20,30), to = c(10,20,30,40)))
  ## accepts calc_rate object
  expect_error(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = NA)
  ## should apply a ninth to seven ninths of the by2 correction if over same time period
  expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 45, method = "linear")$adjustment,
               c(-0.01*1/9, -0.01*3/9, -0.01*5/9, -0.01*7/9))
  expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 45, method = "linear")$adjusted.rate,
               c(-0.03178548, -0.02555831, -0.02166402, -0.01793542))

  ## auto_rate input
  ar_obj <- suppressWarnings(auto_rate(urchins.rd))
  ## accepts auto_rate object
  expect_error(adjust_rate(x = ar_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 100, method = "linear"),
               regexp = NA)
  ## values
  expect_equal(adjust_rate(x = ar_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 45, method = "linear")$adjustment,
               c(-0.003477778, -0.007422222, -0.001066667, -0.001222222))
  expect_equal(adjust_rate(x = ar_obj, by = 0, by2 = -0.01,
                           t_by = 0, t_by2 = 45, method = "linear")$adjusted.rate,
               c(-0.02578789, -0.01791415, -0.03132537, -0.03072295))

})




# test_that("adjust_rate can be printed - if adjustment done to calc_rate or auto_rate object", {
#   cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
#   ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
#   expect_output(print(cr))
#   expect_output(print(ar))
# })
#
# test_that("adjust_rate can be printed - if adjustment done to numeric value", {
#   nr <- adjust_rate(0.1, 0.001)
#   expect_output(print(nr))
# })
#
# test_that("adjust_rate S3 generics work", {
#   cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
#   ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
#   nr <- adjust_rate(0.1, 0.001)
#   expect_output(summary(cr))
#   expect_output(summary(ar))
#   expect_output(summary(nr))
#   expect_output(print(cr))
#   expect_output(print(ar))
#   expect_output(print(nr))
#   expect_output(suppressWarnings(mean(cr)))
#   expect_output(suppressWarnings(mean(ar)))
#   expect_output(suppressWarnings(mean(nr)))
# })
#
# test_that("adjust_rate outputs correct exact values", {
#   ar <- adjust_rate(auto_rate(sardine.rd, plot = F), 0.001)
#   cr <- adjust_rate(calc_rate(sardine.rd, plot = F), 0.001)
#   cr2 <- adjust_rate(calc_rate(sardine.rd,
#                                from = 100:110,
#                                to = 1000:1010,
#                                plot = F), 0.001)
#
#   expect_equal(ar$adjusted.rate[1:5],
#                c(-0.001660665, -0.001687558, -0.001661814, -0.001708236, -0.001705913))
#   expect_equal(cr$adjusted.rate,
#                -0.001728014)
#   expect_equal(cr2$adjusted.rate,
#                c(-0.001787651, -0.001786868, -0.001786072, -0.001786743, -0.001786663,
#                  -0.001787309, -0.001786466, -0.001786350, -0.001787697, -0.001789769,
#                  -0.001788133))
# })
#
# test_that("adjust_rate uses mean of multiple bg rates by default", {
#   ## should adjuast all by 4
#   ar <- adjust_rate(c(10,20,30,40,50), c(2,3,4,5,6))
#   expect_equal(ar$adjusted.rate,
#                c(6,16,26,36,46))
# })
#
#
#

sink() ## turns printing back on
