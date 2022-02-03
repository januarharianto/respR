# library(testthat)
# rm(list=ls())
# testthat::test_file("tests/testthat/test-adjust_rate.ft.R")
# covr::file_coverage("R/calc_rate.ft.R", "tests/testthat/test-adjust_rate.ft.R")
# x <- covr::package_coverage()
# covr::report(x)

capture.output({  ## stops printing outputs on assigning

# Create testing objects --------------------------------------------------

insp.ft_obj <- inspect.ft(flowthrough.rd, delta.oxy = 4, plot = FALSE)
## oxygen production data
insp.ft_obj_prod <- inspect.ft(cbind(flowthrough.rd[,1], -1*(flowthrough.rd[,4])),
                               time = 1, delta.oxy = 2, plot = FALSE)

# x objects
x1 <- calc_rate.ft(insp.ft_obj,
                   flowrate = 2, from = 200, to = 500,
                   by = NULL, width = NULL, plot = FALSE
)
xmany <- calc_rate.ft(insp.ft_obj,
                      flowrate = 2, from = 200:300, to = 500:600,
                      by = NULL, width = NULL, plot = FALSE
)
xwidth <- calc_rate.ft(insp.ft_obj,
                       flowrate = 2,
                       by = "row", width = 300, plot = FALSE
)
xprod <- calc_rate.ft(insp.ft_obj_prod,
                      flowrate = 2, from = 200, to = 500,
                      by = NULL, width = NULL, plot = FALSE
)

# by objects

by_val <- -0.7
by_vec <- seq(-0.75, -0.85, -0.01)
by_crft <- calc_rate.ft(suppressWarnings(inspect.ft(flowthrough_mult.rd, out.oxy = 4, in.oxy = 8, plot = FALSE)),
                        flowrate = 2, plot = FALSE)
  by_crft_mult <- calc_rate.ft(suppressWarnings(inspect.ft(flowthrough_mult.rd, out.oxy = 4, in.oxy = 8, plot = FALSE)),
                             from = c(2000, 2500), to = c(2400, 2900), by = "row",
                             flowrate = 2, plot = FALSE)
# by_crft_nl <- calc_rate.ft(inspect.ft(flowthrough_sim.rd, out.oxy = 2, in.oxy = 3, plot = FALSE),
#                         flowrate = 2, plot = FALSE)

adj.ft_obj.insp.1 <- adjust_rate.ft(x1, by = by_val)
adj.ft_obj.insp.many <- adjust_rate.ft(xmany, by = by_val)
adj.ft_obj.insp.width <- adjust_rate.ft(xwidth, by = by_val)
adj.ft_obj.insp.prod <- adjust_rate.ft(xprod, by = 0.7)
adj.ft_obj.vec <- adjust_rate.ft(xmany$rate, by = by_val)
adj.ft_obj.val <- adjust_rate.ft(-1.5, by = by_val)


# validate 'x' inputs -----------------------------------------------------

test_that("adjust_rate.ft - stops if 'x' input not numeric or 'calc_rate.ft'", {
  expect_error(adjust_rate.ft("string",
                              by = -0.7),
               regexp = "adjust_rate.ft: 'x' must be numeric or 'calc_rate.ft' object.")
  expect_error(adjust_rate.ft(as.data.frame(flowthrough.rd),
                              by = -0.7),
               regexp = "adjust_rate.ft: 'x' must be numeric or 'calc_rate.ft' object.")
  expect_error(adjust_rate.ft(inspect(flowthrough.rd, plot = FALSE),
                              by = -0.7),
               regexp = "adjust_rate.ft: 'x' must be numeric or 'calc_rate.ft' object.")
})



# validate 'by' inputs ----------------------------------------------------

test_that("adjust_rate.ft - stops if 'by' input not numeric or 'calc_rate.ft'", {
  expect_error(adjust_rate.ft(x1,
                              by = "string"),
               regexp = "adjust_rate.ft: 'by' must be numeric or 'calc_rate.ft' object.")
  expect_error(adjust_rate.ft(x1,
                              by = as.data.frame(flowthrough.rd)),
               regexp = "adjust_rate.ft: 'by' must be numeric or 'calc_rate.ft' object.")
  expect_error(adjust_rate.ft(x1,
                              by = inspect(flowthrough.rd, plot = FALSE)),
               regexp = "adjust_rate.ft: 'by' must be numeric or 'calc_rate.ft' object.")
})

test_that("adjust_rate.ft - warns if 'by' contains multiple rates", {
  expect_warning(adjust_rate.ft(x1,
                                by = c(23,34)),
                 regexp = "adjust_rate.ft: the 'by' input contains multiple background rates. The mean value will be used to perform adjustments.")
  expect_warning(adjust_rate.ft(x1,
                                by = xmany),
                 regexp = "adjust_rate.ft: the 'by' input contains multiple background rates. The mean value will be used to perform adjustments.")
})

test_that("adjust_rate.ft - stops if 'x' and 'by' are calc_rate.ft and have been determined using different 'flowrates'", {
  expect_error(adjust_rate.ft(calc_rate.ft(insp.ft_obj,
                                           flowrate = 2,
                                           from = 200, to = 500, plot = FALSE),
                              by = calc_rate.ft(suppressWarnings(inspect.ft(flowthrough_mult.rd, out.oxy = 12, in.oxy = 13, plot = FALSE)),
                                                flowrate = 1.5, plot = FALSE)),
               regexp = "adjust_rate.ft: 'x' and by' input rates have been calculated using different 'flowrates'!")
})


# Verify adjustment -------------------------------------------------------

test_that("adjust_rate.ft - adjustment value correctly extracted from 'by'", {
  expect_equal(adjust_rate.ft(x1,
                              by = by_val)$adjustment,
               by_val)
  expect_equal(suppressWarnings(adjust_rate.ft(x1,
                                               by = by_vec))$adjustment,
               mean(by_vec))
  expect_equal(adjust_rate.ft(x1,
                              by = by_crft)$adjustment,
               by_crft$rate)
  expect_equal(suppressWarnings(adjust_rate.ft(x1,
                                               by = by_crft_mult))$adjustment,
               mean(by_crft_mult$rate))
})

test_that("adjust_rate.ft - adjustment value correctly applied", {
  expect_equal(adjust_rate.ft(x1,
                              by = by_val)$rate.adjusted,
               x1$rate - by_val)
  expect_equal(suppressWarnings(adjust_rate.ft(x1,
                                               by = by_vec))$rate.adjusted,
               x1$rate - mean(by_vec))
  expect_equal(adjust_rate.ft(x1,
                              by = by_crft)$rate.adjusted,
               x1$rate - mean(by_crft$rate))
  expect_equal(suppressWarnings(adjust_rate.ft(x1,
                                               by = by_crft_mult))$rate.adjusted,
               x1$rate - mean(by_crft_mult$rate))

  expect_equal(adjust_rate.ft(xmany,
                              by = by_val)$rate.adjusted,
               xmany$rate - by_val)
  expect_equal(suppressWarnings(adjust_rate.ft(xmany,
                                               by = by_vec))$rate.adjusted,
               xmany$rate - mean(by_vec))
  expect_equal(adjust_rate.ft(xmany,
                              by = by_crft)$rate.adjusted,
               xmany$rate - mean(by_crft$rate))
  expect_equal(suppressWarnings(adjust_rate.ft(xmany,
                                               by = by_crft_mult))$rate.adjusted,
               xmany$rate - mean(by_crft_mult$rate))

  expect_equal(adjust_rate.ft(xwidth,
                              by = by_val)$rate.adjusted,
               xwidth$rate - by_val)
  expect_equal(suppressWarnings(adjust_rate.ft(xwidth,
                                               by = by_vec))$rate.adjusted,
               xwidth$rate - mean(by_vec))
  expect_equal(adjust_rate.ft(xwidth,
                              by = by_crft)$rate.adjusted,
               xwidth$rate - mean(by_crft$rate))
  expect_equal(suppressWarnings(adjust_rate.ft(xwidth,
                                               by = by_crft_mult))$rate.adjusted,
               xwidth$rate - mean(by_crft_mult$rate))
})

test_that("adjust_rate.ft - adjustment value correctly applied with oxygen production rates", {
  expect_equal(adjust_rate.ft(xprod,
                              by = -1*by_val)$rate.adjusted,
               xprod$rate - (-1*by_val))
  expect_equal(suppressWarnings(adjust_rate.ft(xprod,
                                               by = -1*by_vec))$rate.adjusted,
               xprod$rate -(-1*mean(by_vec)))
})

test_that("adjust_rate.ft - if multiple rates in 'by' the mean value is correctly applied", {
  expect_equal(suppressWarnings(adjust_rate.ft(x1,
                                               by = by_vec))$rate.adjusted,
               x1$rate - mean(by_vec))
  expect_equal(suppressWarnings(adjust_rate.ft(x1,
                                               by = by_crft_mult))$rate.adjusted,
               x1$rate - mean(by_crft_mult$rate))

  expect_equal(suppressWarnings(adjust_rate.ft(xmany,
                                               by = by_vec))$rate.adjusted,
               xmany$rate - mean(by_vec))
  expect_equal(suppressWarnings(adjust_rate.ft(xmany,
                                               by = by_crft_mult))$rate.adjusted,
               xmany$rate - mean(by_crft_mult$rate))

  expect_equal(suppressWarnings(adjust_rate.ft(xwidth,
                                               by = by_vec))$rate.adjusted,
               xwidth$rate - mean(by_vec))
  expect_equal(suppressWarnings(adjust_rate.ft(xwidth,
                                               by = by_crft_mult))$rate.adjusted,
               xwidth$rate - mean(by_crft_mult$rate))

})



# S3 Generics tests -------------------------------------------------------

test_that("adjust_rate.ft - objects can be printed.", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(print(adj.ft_obj.insp.1),
               regexp = NA)
  expect_output(print(adj.ft_obj.insp.1),
                regexp = "Adjusted Rate :")

  expect_error(print(adj.ft_obj.insp.many),
               regexp = NA)
  expect_output(print(adj.ft_obj.insp.many),
                regexp = "Adjusted Rate :")

  expect_error(print(adj.ft_obj.insp.width),
               regexp = NA)
  expect_output(print(adj.ft_obj.insp.width),
                regexp = "Adjusted Rate :")

  expect_error(print(adj.ft_obj.insp.prod),
               regexp = NA)
  expect_output(print(adj.ft_obj.insp.prod),
                regexp = "Adjusted Rate :")

  expect_error(print(adj.ft_obj.vec),
               regexp = NA)
  expect_output(print(adj.ft_obj.vec),
                regexp = "Adjusted Rate :")

  expect_error(print(adj.ft_obj.val),
               regexp = NA)
  expect_output(print(adj.ft_obj.val),
                regexp = "Adjusted Rate :")

})

test_that("adjust_rate.ft - objects can be printed with 'pos' input.", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(print(adj.ft_obj.insp.many, pos = 2),
               regexp = NA)
  expect_output(print(adj.ft_obj.insp.many, pos = 2),
                regexp = "Rank 2 of 101 adjusted")

  expect_error(print(adj.ft_obj.insp.width, pos = 100),
               regexp = NA)
  expect_output(print(adj.ft_obj.insp.width, pos = 100),
                regexp = "Rank 100 of 636 adjusted")

  expect_error(print(adj.ft_obj.vec, pos = 20),
               regexp = NA)
  expect_output(print(adj.ft_obj.vec, pos = 20),
                regexp = "Rank 20 of 101 adjusted")

  ## to check Adjustment value is properly included in output
  ## previously caught some failures to do this when pos was > 1
  expect_failure(expect_output(print(adj.ft_obj.vec, pos = 20),
                               regexp = "Adjustment    : NA"))

})

test_that("adjust_rate.ft - print() stops with invalid 'pos' input.", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(print(adj.ft_obj.insp.1, pos = 2),
               regexp = "print.adjust_rate.ft: Invalid 'pos' rank: only 1 adjusted rates found.")
  expect_error(print(adj.ft_obj.insp.many, pos = 2000),
               regexp = "print.adjust_rate.ft: Invalid 'pos' rank: only 101 adjusted rates found.")
  expect_error(print(adj.ft_obj.insp.many, pos = 20:30),
               regexp = "print.adjust_rate.ft: 'pos' must be a single value. To examine multiple results use summary().")

})

test_that("adjust_rate.ft - objects work with summary()", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(summary(adj.ft_obj.insp.1),
               regexp = NA)
  expect_output(summary(adj.ft_obj.insp.1),
                regexp = "rate.adjusted")

  expect_error(summary(adj.ft_obj.insp.many),
               regexp = NA)
  expect_output(summary(adj.ft_obj.insp.many),
                regexp = "rate.adjusted")

  expect_error(summary(adj.ft_obj.insp.width),
               regexp = NA)
  expect_output(summary(adj.ft_obj.insp.width),
                regexp = "rate.adjusted")

  expect_error(summary(adj.ft_obj.insp.prod),
               regexp = NA)
  expect_output(summary(adj.ft_obj.insp.prod),
                regexp = "rate.adjusted")

  expect_error(summary(adj.ft_obj.vec),
               regexp = NA)
  expect_output(summary(adj.ft_obj.vec),
                regexp = "rate.adjusted")

  expect_error(summary(adj.ft_obj.val),
               regexp = NA)
  expect_output(summary(adj.ft_obj.val),
                regexp = "rate.adjusted")
})

test_that("adjust_rate.ft - objects work with summary() and 'pos' input", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(summary(adj.ft_obj.insp.many, pos = 2),
               regexp = NA)
  expect_output(summary(adj.ft_obj.insp.many, pos = 2),
                regexp = "rate.adjusted")
  expect_equal(nrow(summary(adj.ft_obj.insp.many, pos = 2, export = TRUE)),
                1)
  expect_error(summary(adj.ft_obj.insp.many, pos = 2:10),
               regexp = NA)
  expect_output(summary(adj.ft_obj.insp.many, pos = 2:10),
                regexp = "rate.adjusted")
  expect_equal(nrow(summary(adj.ft_obj.insp.many, pos = 2:10, export = TRUE)),
                9)
  expect_error(summary(adj.ft_obj.vec, pos = c(2,4,6,8)),
               regexp = NA)
  expect_output(summary(adj.ft_obj.vec, pos = c(2,4,6,8)),
                regexp = "rate.adjusted")
  expect_equal(nrow(summary(adj.ft_obj.vec, pos = c(2,4,6,8), export = TRUE)),
                4)
  expect_equal(summary(adj.ft_obj.vec, pos = c(2,4,6,8), export = TRUE)$rank,
               c(2,4,6,8))
})

test_that("adjust_rate.ft - summary() stops with invalid 'pos' input", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(summary(adj.ft_obj.insp.many, pos = 102),
               regexp = "summary.adjust_rate.ft: Invalid 'pos' rank: only 101 rates found.")
})

test_that("adjust_rate.ft - objects work with summary() and 'export' input", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(summary(adj.ft_obj.insp.many, export = TRUE),
               regexp = NA)
  expect_equal(nrow(summary(adj.ft_obj.insp.many, export = TRUE)),
               nrow(adj.ft_obj.insp.many$summary))
})

test_that("adjust_rate.ft - objects work with mean()", {

  # # objects
  # adj.ft_obj.insp.1
  # adj.ft_obj.insp.many
  # adj.ft_obj.insp.width
  # adj.ft_obj.insp.prod
  # adj.ft_obj.vec
  # adj.ft_obj.val

  expect_error(mean(adj.ft_obj.insp.1),
               regexp = NA)
  expect_output(mean(adj.ft_obj.insp.1),
                regexp = "Mean of 1 adjusted rates:")
  expect_equal(mean(adj.ft_obj.insp.1, export = TRUE),
               mean(adj.ft_obj.insp.1$rate.adjusted))
  expect_message(mean(adj.ft_obj.insp.1),
               regexp = "Only 1 rate found. Returning mean rate anyway")

  expect_error(mean(adj.ft_obj.insp.many),
               regexp = NA)
  expect_output(mean(adj.ft_obj.insp.many),
                regexp = "Mean of 101 adjusted rates:")
  expect_equal(mean(adj.ft_obj.insp.many, export = TRUE),
               mean(adj.ft_obj.insp.many$rate.adjusted))

  expect_error(mean(adj.ft_obj.insp.width),
               regexp = NA)
  expect_output(mean(adj.ft_obj.insp.width),
                regexp = "Mean of 636 adjusted rates:")
  expect_equal(mean(adj.ft_obj.insp.width, export = TRUE),
               mean(adj.ft_obj.insp.width$rate.adjusted))

  expect_error(mean(adj.ft_obj.insp.prod),
               regexp = NA)
  expect_output(mean(adj.ft_obj.insp.prod),
                regexp = "Mean of 1 adjusted rates:")
  expect_equal(mean(adj.ft_obj.insp.prod, export = TRUE),
               mean(adj.ft_obj.insp.prod$rate.adjusted))

  expect_error(mean(adj.ft_obj.vec),
               regexp = NA)
  expect_output(mean(adj.ft_obj.vec),
                regexp = "Mean of 101 adjusted rates:")
  expect_equal(mean(adj.ft_obj.vec, export = TRUE),
               mean(adj.ft_obj.vec$rate.adjusted))

  expect_error(mean(adj.ft_obj.val),
               regexp = NA)
  expect_output(mean(adj.ft_obj.val),
                regexp = "Mean of 1 adjusted rates:")
  expect_equal(mean(adj.ft_obj.val, export = TRUE),
               mean(adj.ft_obj.val$rate.adjusted))
})

test_that("adjust_rate.ft - objects work with mean() and 'pos' input", {

  expect_error(mean(adj.ft_obj.insp.many, pos = 1:10),
               regexp = NA)
  expect_output(mean(adj.ft_obj.insp.many, pos = 1:10),
                regexp = "Mean of adjusted rate results from entered 'pos' ranks:")
  expect_equal(mean(adj.ft_obj.insp.many, pos = 1:10, export = TRUE),
               mean(adj.ft_obj.insp.many$rate.adjusted[1:10]))
})

test_that("adjust_rate.ft - stops with mean() if 'pos' too high", {
  expect_error(mean(adj.ft_obj.insp.many, pos = 200),
               regexp = "mean.adjust_rate.ft: Invalid 'pos' rank: only 101 adjusted rates found.")
})

}) ## turns printing back on

