# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-inspect.ft.R")
# covr::file_coverage("R/inspect.ft.R", "tests/testthat/test-inspect.ft.R")
# x <- covr::package_coverage()
# covr::report(x)

sink("/dev/null") ## stops printing console outputs on assigning

# Create multicolumn dfs for testing --------------------------------------

{
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


  ## NaN in out.o2
  ft_mult_out.o2_nan_1 <- flowthrough_mult.rd
  ft_mult_out.o2_nan_1[432,2] <- NA

  ft_mult_out.o2_nan_3 <- flowthrough_mult.rd
  ft_mult_out.o2_nan_3[432,2] <- NA
  ft_mult_out.o2_nan_3[624,2] <- NA
  ft_mult_out.o2_nan_3[724,2] <- NA
  ft_mult_out.o2_nan_3[624,3] <- NA
  ft_mult_out.o2_nan_3[724,3] <- NA

  ft_mult_out.o2_nan_mult <- flowthrough_mult.rd
  ft_mult_out.o2_nan_mult[432:461,2] <- NA
  ft_mult_out.o2_nan_mult[622:671,2] <- NA

  ## NaN in in.o2
  ft_mult_in.o2_nan_1 <- flowthrough_mult.rd
  ft_mult_in.o2_nan_1[432,5] <- NA

  ft_mult_in.o2_nan_3 <- flowthrough_mult.rd
  ft_mult_in.o2_nan_3[432,5] <- NA
  ft_mult_in.o2_nan_3[624,5] <- NA
  ft_mult_in.o2_nan_3[724,5] <- NA
  ft_mult_in.o2_nan_3[624,6] <- NA
  ft_mult_in.o2_nan_3[724,6] <- NA

  ft_mult_in.o2_nan_mult <- flowthrough_mult.rd
  ft_mult_in.o2_nan_mult[432:461,5] <- NA
  ft_mult_in.o2_nan_mult[622:671,5] <- NA

  ## NaN in delta.o2
  ft_mult_delta.o2_nan_1 <- flowthrough_mult.rd
  ft_mult_delta.o2_nan_1[432,8] <- NA

  ft_mult_delta.o2_nan_3 <- flowthrough_mult.rd
  ft_mult_delta.o2_nan_3[432,8] <- NA
  ft_mult_delta.o2_nan_3[624,8] <- NA
  ft_mult_delta.o2_nan_3[724,8] <- NA
  ft_mult_delta.o2_nan_3[624,9] <- NA
  ft_mult_delta.o2_nan_3[724,9] <- NA

  ft_mult_delta.o2_nan_mult <- flowthrough_mult.rd
  ft_mult_delta.o2_nan_mult[432:461,8] <- NA
  ft_mult_delta.o2_nan_mult[622:671,8] <- NA


  ## 2 column input (i.e. time- and calculated delta)
  insp.ft.obj <- inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2,
                            in.o2 = 5, delta.o2 = NULL, plot = F)
  ## multi column input (i.e. time- and calculated delta)
  insp.ft.mult.obj <- inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4,
                                 in.o2 = 5:7, delta.o2 = NULL, plot = F)
}


# tests -------------------------------------------------------------------

test_that("inspect.ft - works with NULL inputs and applies defaults correctly",{
  ## time = NULL
  expect_message(inspect.ft(flowthrough_mult.rd, time = NULL, out.o2 = 2,
                            in.o2 = 5, delta.o2 = NULL, plot = F),
                 regexp = "inspect.ft: Applying column default of 'time = 1")
  expect_error(inspect.ft(flowthrough_mult.rd, time = NULL, out.o2 = 2,
                          in.o2 = 5, delta.o2 = NULL, plot = F),
               regexp = NA)
  expect_equal(inspect.ft(flowthrough_mult.rd, time = NULL, out.o2 = 2,
                          in.o2 = 5, delta.o2 = NULL, plot = F)$input_data$time[[1]],
               flowthrough_mult.rd[[1]])

  # in.o2 = NULL & out.o2 = NULL & delta.o2 = NULL
  expect_message(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = NULL,
                            in.o2 = NULL, delta.o2 = NULL, plot = F),
                 regexp = "inspect.ft: Applying column default of all non-time column\\(s\\) as 'delta.o2'")
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, delta.o2 = NULL, plot = F),
               regexp = NA)
  expect_equal(as.data.frame(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = NULL,
                                        in.o2 = NULL, delta.o2 = NULL, plot = F)$input_data$delta.o2),
               as.data.frame(flowthrough_mult.rd[,2:13]))
})

test_that("inspect.ft - stops if input column numbers found to conflict",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:6,
                          in.o2 = 5:9, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: Input columns conflict.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, delta.o2 = 1:10, plot = F),
               regexp = "inspect.ft: Input columns conflict.")
})

test_that("inspect.ft - stops if out.o2 entered and neither or in.o2 or in.o2.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:3,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: With 'out.o2' data, paired 'in.o2' columns or an 'in.o2.value' is required.")
})

test_that("inspect.ft - stops if in.o2 entered and out.o2 does not have argument",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: An 'in.o2' input requires paired 'out.o2' column\\(s\\).")
})

test_that("inspect.ft - stops if both or in.o2 and in.o2.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:3,
                          in.o2 = 4, in.o2.value = 5, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: Only one of 'in.o2' or 'in.o2.value' can be entered.")
})

test_that("inspect.ft - correctly accepts and handles in.o2.value input",{
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2,
                          in.o2 = NULL, in.o2.value = 9, delta.o2 = NULL, plot = F)$dataframe$in.o2.value,
               regexp = NA)
  expect_equal(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2,
                          in.o2 = NULL, in.o2.value = 9, delta.o2 = NULL, plot = F)$dataframe$in.o2.value,
               rep(9, nrow(flowthrough_mult.rd)))
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
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 4,
                          in.o2 = 5, in.o2.value = NULL, delta.o2 = 3, plot = F),
               regexp = "inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = 4, delta.o2 = 3, plot = F),
               regexp = "inspect.ft: With 'delta.o2' data, 'out.o2', 'in.o2' and 'in.o2.value' should be NULL.")
})

test_that("inspect.ft - stops if x not a dataframe", {
  expect_error(inspect.ft(as.matrix(flowthrough.rd), time = 1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'x' must be data.frame object.")
  expect_error(inspect.ft(flowthrough.rd[[1]], time = 1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'x' must be data.frame object.")
  expect_error(inspect.ft("blah", time = 1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'x' must be data.frame object.")
})

test_that("inspect.ft - stops if column inputs malformed", {
  ## time
  expect_error(inspect.ft(flowthrough.rd, time = 1.1, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'time' - some column inputs are not integers.")
  expect_error(inspect.ft(flowthrough.rd, time = 1:2, out.o2 = 3,
                          in.o2 = 4, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'time' - cannot enter more than 1 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 8, out.o2 = 2,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'time' - one or more column inputs are out of range of allowed data columns.")
  ## out.o2
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2.1,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' - some column inputs are not integers")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 4:9,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' - cannot enter more than 3 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 8:9,
                          in.o2 = 3, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' - one or more column inputs are out of range of allowed data columns.")
  ## in.o2
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2,
                          in.o2 = 3.1, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'in.o2' - some column inputs are not integers.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 4:9,
                          in.o2 = 2, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'out.o2' - cannot enter more than 3 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = 2:3,
                          in.o2 = 8:9, in.o2.value = NULL, delta.o2 = NULL, plot = F),
               regexp = "inspect.ft: 'in.o2' - one or more column inputs are out of range of allowed data columns.")
  ## delta.o2
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = 4.1, plot = F),
               regexp = "inspect.ft: 'delta.o2' - some column inputs are not integers")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = 2:5, plot = F),
               regexp = "inspect.ft: 'delta.o2' - cannot enter more than 3 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.o2 = NULL,
                          in.o2 = NULL, in.o2.value = NULL, delta.o2 = 5, plot = F),
               regexp = "inspect.ft: 'delta.o2' - one or more column inputs are out of range of allowed data columns.")
})


test_that("inspect.ft - outputs correct warnings", {

  ## time NA
  expect_warning(inspect.ft(ft_mult_time_nan_1, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Time column.")
  expect_warning(inspect.ft(ft_mult_time_nan_3, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Time column.")
  expect_warning(inspect.ft(ft_mult_time_nan_mult, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Time column.")
  ## time non-seq
  expect_warning(inspect.ft(ft_mult_time_nonseq_1, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Non-sequential Time values found.")
  expect_warning(inspect.ft(ft_mult_time_nonseq_3, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Non-sequential Time values found.")
  expect_warning(inspect.ft(ft_mult_time_nonseq_mult, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Non-sequential Time values found.")
  ## time duplicates
  expect_warning(inspect.ft(ft_mult_time_dupe_1, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Duplicate Time values found.")
  expect_warning(inspect.ft(ft_mult_time_dupe_3, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Duplicate Time values found.")
  expect_warning(inspect.ft(ft_mult_time_dupe_mult, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Duplicate Time values found.")
  ## time uneven
  expect_warning(inspect.ft(ft_mult_time_uneven_1, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Time values are not evenly-spaced")
  expect_warning(inspect.ft(ft_mult_time_uneven_3, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Time values are not evenly-spaced")
  expect_warning(inspect.ft(ft_mult_time_uneven_mult, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "Time values are not evenly-spaced.")
  ## out.o2 NA
  expect_warning(inspect.ft(ft_mult_out.o2_nan_1, time = 1, out.o2 = 2:4,
                            in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_out.o2_nan_3, time = 1, out.o2 = 2:4,
                            in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_out.o2_nan_mult, time = 1, out.o2 = 2:4,
                            in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  ## in.o2 NA
  expect_warning(inspect.ft(ft_mult_in.o2_nan_1, time = 1, out.o2 = 2:4,
                            in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_in.o2_nan_3, time = 1, out.o2 = 2:4,
                            in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_in.o2_nan_mult, time = 1, out.o2 = 2:4,
                            in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  ## delta.o2 NA
  expect_warning(inspect.ft(ft_mult_delta.o2_nan_1, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = 8:10, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_delta.o2_nan_3, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = 8:10, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_delta.o2_nan_mult, time = 1, out.o2 = NULL,
                            in.o2 = NULL, in.o2.value = NULL, delta.o2 = 8:10, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
})


test_that("inspect.ft works with 2-column data", {
  expect_error(inspect.ft(sardine.rd, plot = F),
               regexp = NA)
  expect_equal(ncol(inspect.ft(sardine.rd, plot = F)$dataframe),
               2)
})

test_that("inspect.ft works with multi-column data", {
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4,
                          in.o2 = 5:7, plot = F),
               regexp = NA)
  expect_equal(ncol(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2:4,
                               in.o2 = 5:7, plot = F)$dataframe),
               7 + 3) # 7 inputs, 3 delta calcs
})


test_that("inspect.ft outputs plots", {
  expect_error(inspect.ft(flowthrough_mult.rd, time = 1, out.o2 = 2,
                          in.o2 = 5, delta.o2 = NULL, plot = T),
               regexp = NA)
  expect_output(plot(insp.ft.obj))
  expect_error(plot(insp.ft.obj),
               regexp = NA)
  expect_output(plot(insp.ft.mult.obj))
  expect_error(plot(insp.ft.mult.obj),
               regexp = NA)
})

test_that("inspect.ft outputs plots when 'pos =' option is used", {
  expect_output(plot(insp.ft.mult.obj, pos = 2))
  expect_error(plot(insp.ft.mult.obj, pos = 2),
               regexp = NA)
  expect_output(plot(insp.ft.mult.obj, pos = 2))
  expect_error(plot(insp.ft.mult.obj, pos = 2),
               regexp = NA)
})

test_that("inspect.ft does not plot when 'pos =' input too large", {
  expect_error(plot(insp.ft.obj, pos = 9),
               regexp = "plot.inspect.ft: Invalid 'pos' input: only 1 data inputs found.")
  expect_error(plot(insp.ft.mult.obj, pos = 9),
               regexp = "inspect.ft: Invalid 'pos' input: only 3 data inputs found.")
})


test_that("inspect.ft objects can be printed, including those which shows warnings", {
  ## regular good data
  expect_output(print(insp.ft.obj))
  expect_error(print(insp.ft.obj),
               regexp = NA)
  expect_output(print(insp.ft.mult.obj))
  expect_error(print(insp.ft.mult.obj),
               regexp = NA)

  ## various errors/warnings
  ob1 <- suppressWarnings(inspect.ft(ft_mult_out.o2_nan_mult, time = 1, out.o2 = 2:4,
                                     in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F))
  ob2 <- suppressWarnings(inspect.ft(ft_mult_time_nonseq_mult, time = 1, out.o2 = NULL,
                                     in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F))
  ob3 <- suppressWarnings(inspect.ft(ft_mult_time_nan_mult, time = 1, out.o2 = NULL,
                                     in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F))
  ob4 <- suppressWarnings(inspect.ft(ft_mult_out.o2_nan_mult, time = 1, out.o2 = NULL,
                                     in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F))
  ob5 <- suppressWarnings(inspect.ft(ft_mult_out.o2_nan_mult, time = 1, out.o2 = 2:4,
                                     in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F))
  ob6 <- suppressWarnings(inspect.ft(ft_mult_in.o2_nan_3, time = 1, out.o2 = 2:4,
                                     in.o2 = 5:7, in.o2.value = NULL, delta.o2 = NULL, plot = F))
  ob7 <- suppressWarnings(inspect.ft(ft_mult_delta.o2_nan_1, time = 1, out.o2 = NULL,
                                     in.o2 = NULL, in.o2.value = NULL, delta.o2 = 8:10, plot = F))
  ob8 <- suppressWarnings(inspect.ft(ft_mult_time_dupe_mult, time = 1, out.o2 = NULL,
                                     in.o2 = NULL, in.o2.value = NULL, delta.o2 = NULL, plot = F))

  expect_output(print(ob1))
  expect_output(print(ob2))
  expect_output(print(ob3))
  expect_output(print(ob4))
  expect_output(print(ob5))
  expect_output(print(ob6))
  expect_output(print(ob7))
  expect_output(print(ob8))

  expect_error(print(ob1),
               regexp = NA)
  expect_error(print(ob2),
               regexp = NA)
  expect_error(print(ob3),
               regexp = NA)
  expect_error(print(ob4),
               regexp = NA)
  expect_error(print(ob5),
               regexp = NA)
  expect_error(print(ob6),
               regexp = NA)
  expect_error(print(ob7),
               regexp = NA)
  expect_error(print(ob8),
               regexp = NA)

})

sink() ## turns console printing back on
