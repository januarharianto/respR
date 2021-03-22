## rm(list=ls())
## library(testthat)
## test_file("tests/testthat/test-inspect.ft.R")
## covr::file_coverage("R/inspect.ft.R", "tests/testthat/test-inspect.ft.R")

## multicolumn df
{
flowthrough_mult.rd <- flowthrough.rd
flowthrough_mult.rd[,5:7] <- flowthrough.rd[,2:4]
flowthrough_mult.rd[,8:10] <- flowthrough.rd[,2:4]
flowthrough_mult.rd <- cbind(flowthrough_mult.rd[,1], flowthrough_mult.rd[,c(2,5,8)],
                          flowthrough_mult.rd[,c(3,6,9)], flowthrough_mult.rd[,c(4,7,10)],
                          flowthrough_mult.rd[,9])
flowthrough_mult.rd <- data.table::as.data.table(flowthrough_mult.rd)
names(flowthrough_mult.rd) <- c("num_time",
                             "oxy.out.1", "oxy.out.2", "oxy.out.3",
                             "oxy.in.1", "oxy.in.2", "oxy.in.3",
                             "oxy.delta.1", "oxy.delta.2", "oxy.delta.3",
                             "oxy.header")
flowthrough_mult.rd[, 2] <- flowthrough_mult.rd[,2] - runif(nrow(flowthrough_mult.rd), min = -0.03, max = 0.03)
flowthrough_mult.rd[, 3] <- flowthrough_mult.rd[,3] - runif(nrow(flowthrough_mult.rd), min = 0.03, max = 0.07)
flowthrough_mult.rd[, 4] <- flowthrough_mult.rd[,4] - runif(nrow(flowthrough_mult.rd), min = 0.08, max = 0.13)
flowthrough_mult.rd[, 5] <- flowthrough_mult.rd[,5] - runif(nrow(flowthrough_mult.rd), min = -0.01, max = 0.01)
flowthrough_mult.rd[, 6] <- flowthrough_mult.rd[,6] - runif(nrow(flowthrough_mult.rd), min = -0.01, max = 0.01)
flowthrough_mult.rd[, 7] <- flowthrough_mult.rd[,7] - runif(nrow(flowthrough_mult.rd), min = -0.01, max = 0.01)
flowthrough_mult.rd[, 8] <- flowthrough_mult.rd[,5] - flowthrough_mult.rd[,2]
flowthrough_mult.rd[, 9] <- flowthrough_mult.rd[,6] - flowthrough_mult.rd[,3]
flowthrough_mult.rd[,10] <- flowthrough_mult.rd[,7] - flowthrough_mult.rd[,4]
flowthrough_mult.rd[,11] <- flowthrough_mult.rd[,7] + runif(nrow(flowthrough_mult.rd), min = 0.03, max = 0.07)

#par(mfrow = c(3,4))
#for (i in 2:11) plot(flowthrough_mult.rd[[i]] ~ flowthrough_mult.rd[[1]])

# inspect(flowthrough_mult.rd, time = 1, oxygen = 2:10)
# inspect.ft(flowthrough_mult.rd)

## versions with various errors
## NaN in time
ft_mult_time_nan_1 <- flowthrough_mult.rd
ft_mult_time_nan_1[431:433,1]
ft_mult_time_nan_1[432,1] <- NA

ft_mult_time_nan_3 <- flowthrough_mult.rd
ft_mult_time_nan_3[431:433,1]
ft_mult_time_nan_3[432,1] <- NA
ft_mult_time_nan_3[623:625,1]
ft_mult_time_nan_3[624,1] <- NA
ft_mult_time_nan_3[723:725,1]
ft_mult_time_nan_3[724,1] <- NA

## loads - more than 20 to check printing
ft_mult_time_nan_mult <- flowthrough_mult.rd
ft_mult_time_nan_mult[431:462,1]
ft_mult_time_nan_mult[432:461,1] <- NA
ft_mult_time_nan_mult[621:672,1]
ft_mult_time_nan_mult[622:671,1] <- NA

## non-sequential in time
ft_mult_time_nonseq_1 <- flowthrough_mult.rd
ft_mult_time_nonseq_1[431:433,1]
ft_mult_time_nonseq_1[432:433,1] <- c(432,431)

ft_mult_time_nonseq_3 <- flowthrough_mult.rd
ft_mult_time_nonseq_3[431:433,1]
ft_mult_time_nonseq_3[432:433,1] <- c(432,431)
ft_mult_time_nonseq_3[751:753,1]
ft_mult_time_nonseq_3[752:753,1] <- c(752,751)
ft_mult_time_nonseq_3[781:783,1]
ft_mult_time_nonseq_3[782:783,1] <- c(782,781)

ft_mult_time_nonseq_mult <- flowthrough_mult.rd
ft_mult_time_nonseq_mult[431:462,1]
ft_mult_time_nonseq_mult[432:461,1] <- ft_mult_time_nonseq_mult[461:432,1]
ft_mult_time_nonseq_mult[621:672,1]
ft_mult_time_nonseq_mult[622:671,1] <- ft_mult_time_nonseq_mult[671:622,1]

## duplicated in time
ft_mult_time_dupe_1 <- flowthrough_mult.rd
ft_mult_time_dupe_1[431:433,1]
ft_mult_time_dupe_1[432:433,1] <- c(431,431)

ft_mult_time_dupe_3 <- flowthrough_mult.rd
ft_mult_time_dupe_3[431:434,1]
ft_mult_time_dupe_3[432:434,1] <- c(431,431,431)
ft_mult_time_dupe_3[751:753,1]
ft_mult_time_dupe_3[752:753,1] <- c(752,752)
ft_mult_time_dupe_3[781:787,1]
ft_mult_time_dupe_3[782:787,1] <- c(781,781,781,781,781,781)

ft_mult_time_dupe_mult <- flowthrough_mult.rd
ft_mult_time_dupe_mult[431:462,1]
ft_mult_time_dupe_mult[432:461,1] <- 431
ft_mult_time_dupe_mult[621:672,1]
ft_mult_time_dupe_mult[622:671,1] <- 622

## unevenly spaced in time
ft_mult_time_uneven_1 <- flowthrough_mult.rd
ft_mult_time_uneven_1[431:433,1]
ft_mult_time_uneven_1 <- ft_mult_time_uneven_1[-432,]

ft_mult_time_uneven_3 <- flowthrough_mult.rd
ft_mult_time_uneven_3[431:434,1]
ft_mult_time_uneven_3 <- ft_mult_time_uneven_3[-432,]
ft_mult_time_uneven_3[751:753,1]
ft_mult_time_uneven_3 <- ft_mult_time_uneven_3[-752,]
ft_mult_time_uneven_3[781:787,1]
ft_mult_time_uneven_3 <- ft_mult_time_uneven_3[-782,]

ft_mult_time_uneven_mult <- flowthrough_mult.rd
ft_mult_time_uneven_mult[431:482,1]
ft_mult_time_uneven_mult <- ft_mult_time_uneven_mult[-seq(432, 472, 2),]
ft_mult_time_uneven_mult[831:882,1]
ft_mult_time_uneven_mult <- ft_mult_time_uneven_mult[-seq(832, 872, 2),]

}

test_that("inspect.ft - stops if input column numbers found to conflict",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:6,
                          in.o2 = 5:8, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: Input columns conflict.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, delta.o2 = 1:10, plot = F),
               regexp = "inspect.ft: Input columns conflict.")
})

test_that("inspect.ft - stops if out.o2 entered and neither or in.o2 or in.o2.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:3,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: With 'out.o2' data, paired 'in.o2' columns or an 'in.o2.value' value is required.")
})

test_that("inspect.ft - stops if both or in.o2 and in.o2.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:3,
                          in.o2 = 4, in.o2.value = 5, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: Only one of 'in.o2' or 'in.o2.value' can be entered.")
})

test_that("inspect.ft - stops if out.o2 and in.o2 entered, but in.o2 does not have same number of columns or a single column.",{
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4,
                          in.o2 = 5:6, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: With 'out.o2' data, 'in.o2' must be a single column or an equal number of paired columns.")
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4,
                          in.o2 = 5:9, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: With 'out.o2' data, 'in.o2' must be a single column or an equal number of paired columns.")
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:3,
                          in.o2 = 4, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = NA)
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:3,
                          in.o2 = 4:5, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = NA)
})

test_that("inspect.ft - stops if delta.o2 entered and out.o2, in.o2 or in.o2.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
                          in.o2 = NULL, in.o2.value = 4, delta.o2 = 3, plot = F),
               regexp = "inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
                          in.o2 = 4, in.o2.value = NULL, delta.o2 = 3, plot = F),
               regexp = "inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = 4, in.o2.value = NULL, delta.o2 = 3, plot = F),
               regexp = "inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = 4, delta.o2 = 3, plot = F),
               regexp = "inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")
})

test_that("inspect.ft - stops if df not a dataframe", {
  expect_error(inspect.ft(as.matrix(flowthrough.rd), time = 1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'df' must be data.frame object.")
  expect_error(inspect.ft(flowthrough.rd[[1]], time = 1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'df' must be data.frame object.")
  expect_error(inspect.ft("blah", time = 1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'df' must be data.frame object.")
})

test_that("inspect.ft - stops if column inputs malformed", {
  ## time
  expect_error(inspect.ft(flowthrough.rd, time = 1.1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'time' input - Some column inputs are not integers.")
  expect_error(inspect.ft(flowthrough.rd, time = 1:2, out.o2 = 3,
                          in.o2 = 4, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'time' input - Input is greater than the maximum allowed number of columns.")
  expect_error(inspect.ft(flowthrough.rd, time = 8, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'time' input - Some column inputs are out of range of allowed data columns.")
  ## out.o2
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2.1,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' input - Some column inputs are not integers")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 4:9,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' input - Input is greater than the maximum allowed number of columns.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 8:9,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' input - Some column inputs are out of range of allowed data columns.")
  ## in.o2
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
                          in.o2 = 3.1, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'in.o2' input - Some column inputs are not integers.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
                          in.o2 = 4:9, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'in.o2' input - Input is greater than the maximum allowed number of columns.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
                          in.o2 = 8:9, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'in.o2' input - Some column inputs are out of range of allowed data columns.")
  ## delta.o2
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = 4.1, plot = F),
               regexp = "inspect.ft: 'delta.o2' input - Some column inputs are not integers")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = 2:5, plot = F),
               regexp = "inspect.ft: 'delta.o2' input - Input is greater than the maximum allowed number of columns.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = 5, plot = F),
               regexp = "inspect.ft: 'delta.o2' input - Some column inputs are out of range of allowed data columns.")
})

# sink("/dev/null") ## stops printing console outputs on assigning
#
# test_that("inspect.ft works on 2-column data",
#           expect_error(inspect.ft(sardine.rd, plot = F),
#                        regexp = NA))
#
# test_that("inspect.ft works on multi-column data", {
#   ## default first 2 columns
#   expect_equal(ncol(suppressWarnings(inspect.ft(urchins.rd, plot = F)$dataframe)),
#                2)
#   ## differnt 2 columns
#   expect_equal(ncol(suppressWarnings(inspect.ft(urchins.rd, time = 1, oxygen = 3, plot = F)$dataframe)),
#                2)
#   ## multiple columns
#   expect_equal(ncol(suppressWarnings(inspect.ft(urchins.rd, time = 1, oxygen = 3:6, plot = F)$dataframe)),
#                5)
# })
#
# ur2c <- suppressWarnings(inspect.ft(urchins.rd, plot = F))
# test_that("inspect.ft produces plot with 2-column data",
#           expect_error(plot(ur2c),
#                        regexp = NA))
#
# ur3c <- suppressWarnings(inspect.ft(urchins.rd, oxygen = 2:3, plot = F))
# test_that("inspect.ft produces plot with multi-column data",
#           expect_error(plot(ur3c),
#                        regexp = NA))
#
#
# # suppressWarnings(file.remove("Rplots.pdf"))
#
#
# test_that("inspect.ft objects can be printed", {
#   expect_output(print(ur2c))
#   expect_output(print(ur3c))
# })
#
# test_that("inspect.ft works with NULL inputs", {
#   expect_error(inspect.ft(intermittent.rd, time = NULL, oxygen = NULL, plot = F),
#                regexp = NA)
#   expect_error(inspect.ft(intermittent.rd, time = NULL, plot = F),
#                regexp = NA)
#   expect_error(inspect.ft(intermittent.rd, oxygen = NULL, plot = F),
#                regexp = NA)
# })
#
#
# test_that("inspect.ft stops if input not df", {
#   expect_error(inspect.ft(as.matrix(urchins.rd), plot = F),
#                "inspect.ft: 'df' must be data.frame object.")
#   expect_error(inspect.ft(urchins.rd[[1]], plot = F),
#                "inspect.ft: 'df' must be data.frame object.")
#   expect_error(inspect.ft(3435, plot = F),
#                "inspect.ft: 'df' must be data.frame object.")
# })
#
# test_that("inspect.ft stops if time/oxygen/width inputs out of range", {
#   expect_error(inspect.ft(urchins.rd, time = 0.2, plot = F),
#                "inspect.ft: 'time' column: must be numeric integer.")
#   expect_error(inspect.ft(urchins.rd, time = 1, oxygen = 0.5, plot = F),
#                "inspect.ft: 'oxygen' column\\(s): must be numeric integers.")
#   expect_error(inspect.ft(urchins.rd, width = 1.5, plot = F),
#                "inspect.ft: 'width' must be between 0 and 1.")
# })
#
#
# # -------------------------------------------------------------------------
#
# test_that("inspect.ft: unevenly spaced time detected message",
#           expect_warning(inspect.ft(urchins.rd, plot = F),
#                         "Time values are not evenly-spaced \\(numerically)."))
#
# base <- select(intermittent.rd, 1,2)
# base[[3]] <- intermittent.rd[[2]]
#
# input <- base
# input[100,1] <- NA
# input[200:205,1] <- NA
#
# test_that("inspect.ft: NA in time detected",
#           expect_warning(inspect.ft(input, plot = F),
#                         "NA/NaN values detected in Time column."))
#
# input <- base
# input[100,2] <- NA
# input[200:205,2] <- NA
# test_that("inspect.ft: NA in oxygen detected",
#           expect_warning(inspect.ft(input, plot = F),
#                         "NA/NaN values detected in Oxygen column\\(s)."))
#
#
# input <- base
# input[100,2] <- NA
# input[200:205,3] <- NA
# test_that("inspect.ft: NA in oxygen detected in multiple columns",
#           expect_warning(inspect.ft(input, plot = F),
#                         "NA/NaN values detected in Oxygen column\\(s)."))
#
# input <- base
# input[9,1] <- 9
# input[10,1] <- 8
# input[325,1] <- 325
# input[326,1] <- 324
# test_that("inspect.ft: non-sequential time detected",
#           expect_warning(inspect.ft(input, plot = F),
#                         "Non-sequential Time values found."))
#
# input <- base
# input[12,1] <- 10
# input[100,1] <- 98
# test_that("inspect.ft: non-sequential time detected",
#           expect_warning(inspect.ft(input, plot = F),
#                         "Duplicate Time values found."))
#
# input <- base
# test_that("inspect.ft: all good message if no errors",
#           expect_message(suppressWarnings(inspect.ft(input, plot = F)),
#                         "No issues detected while inspect.fting data frame."))
#
#
# sink() ## turns console printing back on
