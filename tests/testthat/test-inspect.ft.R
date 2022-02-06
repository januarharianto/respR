# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-inspect.ft.R")
# covr::file_coverage("R/inspect.ft.R", "tests/testthat/test-inspect.ft.R")
# x <- covr::package_coverage()
# covr::report(x)
# covr::report(covr::package_coverage())

capture.output({  ## stops printing console outputs on assigning

# Create multicolumn dfs for testing --------------------------------------

{
  ## versions with various errors
  ## non-numeric in time
  ft_mult_time_nonnum_1 <- as.data.frame(flowthrough_mult.rd)
  ft_mult_time_nonnum_1[432,1]
  ft_mult_time_nonnum_1[432,1] <- "7.2"

  ## inf in time
  ft_mult_time_inf_1 <- flowthrough_mult.rd
  ft_mult_time_inf_1[431:433,1]
  ft_mult_time_inf_1[432,1] <- Inf

  ft_mult_time_inf_3 <- flowthrough_mult.rd
  ft_mult_time_inf_3[431:433,1]
  ft_mult_time_inf_3[432,1] <- Inf
  ft_mult_time_inf_3[623:625,1]
  ft_mult_time_inf_3[624,1] <- Inf
  ft_mult_time_inf_3[723:725,1]
  ft_mult_time_inf_3[724,1] <- Inf

  ## loads - more than 20 to check printing
  ft_mult_time_inf_mult <- flowthrough_mult.rd
  ft_mult_time_inf_mult[431:462,1]
  ft_mult_time_inf_mult[432:461,1] <- Inf
  ft_mult_time_inf_mult[621:672,1]
  ft_mult_time_inf_mult[622:671,1] <- Inf


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

  ## non-numeric in out.oxy
  ft_mult_out.oxy_nonnum_1 <- as.data.frame(flowthrough_mult.rd)
  ft_mult_out.oxy_nonnum_1[432,2]
  ft_mult_out.oxy_nonnum_1[432,2] <- "92.95885"

  ## non-numeric in in.oxy
  ft_mult_in.oxy_nonnum_1 <- as.data.frame(flowthrough_mult.rd)
  ft_mult_in.oxy_nonnum_1[432,6]
  ft_mult_in.oxy_nonnum_1[432,6] <- "97.19841"

  ## non-numeric in delta.oxy
  ft_mult_delta.oxy_nonnum_1 <- as.data.frame(flowthrough_mult.rd)
  ft_mult_delta.oxy_nonnum_1[432,10]
  ft_mult_delta.oxy_nonnum_1[432,10] <- "-4.239564"

  ## NaN in out.oxy
  ft_mult_out.oxy_nan_1 <- flowthrough_mult.rd
  ft_mult_out.oxy_nan_1[432,2] <- NA

  ft_mult_out.oxy_nan_3 <- flowthrough_mult.rd
  ft_mult_out.oxy_nan_3[432,2] <- NA
  ft_mult_out.oxy_nan_3[624,2] <- NA
  ft_mult_out.oxy_nan_3[724,2] <- NA
  ft_mult_out.oxy_nan_3[624,3] <- NA
  ft_mult_out.oxy_nan_3[724,3] <- NA

  ft_mult_out.oxy_nan_mult <- flowthrough_mult.rd
  ft_mult_out.oxy_nan_mult[432:461,2] <- NA
  ft_mult_out.oxy_nan_mult[622:671,2] <- NA

  ## NaN in in.oxy
  ft_mult_in.oxy_nan_1 <- flowthrough_mult.rd
  ft_mult_in.oxy_nan_1[432,5] <- NA

  ft_mult_in.oxy_nan_3 <- flowthrough_mult.rd
  ft_mult_in.oxy_nan_3[432,5] <- NA
  ft_mult_in.oxy_nan_3[624,5] <- NA
  ft_mult_in.oxy_nan_3[724,5] <- NA
  ft_mult_in.oxy_nan_3[624,6] <- NA
  ft_mult_in.oxy_nan_3[724,6] <- NA

  ft_mult_in.oxy_nan_mult <- flowthrough_mult.rd
  ft_mult_in.oxy_nan_mult[432:461,5] <- NA
  ft_mult_in.oxy_nan_mult[622:671,5] <- NA

  ## NaN in delta.oxy
  ft_mult_delta.oxy_nan_1 <- flowthrough_mult.rd
  ft_mult_delta.oxy_nan_1[432,8] <- NA

  ft_mult_delta.oxy_nan_3 <- flowthrough_mult.rd
  ft_mult_delta.oxy_nan_3[432,8] <- NA
  ft_mult_delta.oxy_nan_3[624,8] <- NA
  ft_mult_delta.oxy_nan_3[724,8] <- NA
  ft_mult_delta.oxy_nan_3[624,9] <- NA
  ft_mult_delta.oxy_nan_3[724,9] <- NA

  ft_mult_delta.oxy_nan_mult <- flowthrough_mult.rd
  ft_mult_delta.oxy_nan_mult[432:461,8] <- NA
  ft_mult_delta.oxy_nan_mult[622:671,8] <- NA

  ## inf in out.oxy
  ft_mult_out.oxy_inf_1 <- flowthrough_mult.rd
  ft_mult_out.oxy_inf_1[432,2] <- Inf

  ft_mult_out.oxy_inf_3 <- flowthrough_mult.rd
  ft_mult_out.oxy_inf_3[432,2] <- Inf
  ft_mult_out.oxy_inf_3[624,2] <- Inf
  ft_mult_out.oxy_inf_3[724,2] <- Inf
  ft_mult_out.oxy_inf_3[624,3] <- Inf
  ft_mult_out.oxy_inf_3[724,3] <- Inf

  ft_mult_out.oxy_inf_mult <- flowthrough_mult.rd
  ft_mult_out.oxy_inf_mult[432:461,2] <- Inf
  ft_mult_out.oxy_inf_mult[622:671,2] <- Inf

  ## inf in in.oxy
  ft_mult_in.oxy_inf_1 <- flowthrough_mult.rd
  ft_mult_in.oxy_inf_1[432,5] <- Inf

  ft_mult_in.oxy_inf_3 <- flowthrough_mult.rd
  ft_mult_in.oxy_inf_3[432,5] <- Inf
  ft_mult_in.oxy_inf_3[624,5] <- Inf
  ft_mult_in.oxy_inf_3[724,5] <- Inf
  ft_mult_in.oxy_inf_3[624,6] <- Inf
  ft_mult_in.oxy_inf_3[724,6] <- Inf

  ft_mult_in.oxy_inf_mult <- flowthrough_mult.rd
  ft_mult_in.oxy_inf_mult[432:461,5] <- Inf
  ft_mult_in.oxy_inf_mult[622:671,5] <- Inf

  ## inf in delta.oxy
  ft_mult_delta.oxy_inf_1 <- flowthrough_mult.rd
  ft_mult_delta.oxy_inf_1[432,8] <- Inf

  ft_mult_delta.oxy_inf_3 <- flowthrough_mult.rd
  ft_mult_delta.oxy_inf_3[432,8] <- Inf
  ft_mult_delta.oxy_inf_3[624,8] <- Inf
  ft_mult_delta.oxy_inf_3[724,8] <- Inf
  ft_mult_delta.oxy_inf_3[624,9] <- Inf
  ft_mult_delta.oxy_inf_3[724,9] <- Inf

  ft_mult_delta.oxy_inf_mult <- flowthrough_mult.rd
  ft_mult_delta.oxy_inf_mult[432:461,8] <- Inf
  ft_mult_delta.oxy_inf_mult[622:671,8] <- Inf


  ## 2 column input (i.e. time- and calculated delta)
  insp.ft.obj <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2,
                            in.oxy = 5, delta.oxy = NULL, plot = F))
  ## multi column input (i.e. time- and calculated delta)
  insp.ft.mult.obj <- suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4,
                                 in.oxy = 5:7, delta.oxy = NULL, plot = F))
}


# tests -------------------------------------------------------------------

test_that("inspect.ft - works with NULL inputs and applies defaults correctly",{
  ## time = NULL
  expect_message(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = NULL, out.oxy = 2,
                            in.oxy = 5, delta.oxy = NULL, plot = F)),
                 regexp = "inspect.ft: Applying column default of 'time = 1")
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = NULL, out.oxy = 2,
                          in.oxy = 5, delta.oxy = NULL, plot = F)),
               regexp = NA)
  expect_equal(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = NULL, out.oxy = 2,
                          in.oxy = 5, delta.oxy = NULL, plot = F))$data$time[[1]],
               flowthrough_mult.rd[[1]])

  # in.oxy = NULL & out.oxy = NULL & delta.oxy = NULL
  expect_message(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = NULL,
                            in.oxy = NULL, delta.oxy = NULL, plot = F)),
                 regexp = "inspect.ft: Applying column default of all non-time column\\(s\\) as 'delta.oxy'")
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = NULL,
                          in.oxy = NULL, delta.oxy = NULL, plot = F)),
               regexp = NA)
  expect_equal(as.data.frame(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = NULL,
                                        in.oxy = NULL, delta.oxy = NULL, plot = F)$data$delta.oxy)),
               as.data.frame(flowthrough_mult.rd[,2:15]))
})

test_that("inspect.ft - stops if input column numbers found to conflict",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2:6,
                          in.oxy = 5:9, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: Input columns conflict.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = NULL,
                          in.oxy = NULL, delta.oxy = 1:10, plot = F),
               regexp = "inspect.ft: Input columns conflict.")
})

test_that("inspect.ft - stops if out.oxy entered and neither or in.oxy or in.oxy.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2:3,
                          in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: With 'out.oxy' data, paired 'in.oxy' columns or an 'in.oxy.value' is required.")
})

test_that("inspect.ft - stops if in.oxy entered and out.oxy does not have argument",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = NULL,
                          in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: An 'in.oxy' input requires paired 'out.oxy' column\\(s\\).")
})

test_that("inspect.ft - stops if both or in.oxy and in.oxy.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2:3,
                          in.oxy = 4, in.oxy.value = 5, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: Only one of 'in.oxy' or 'in.oxy.value' can be entered.")
})

test_that("inspect.ft - correctly accepts and handles in.oxy.value input",{
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2,
                          in.oxy = NULL, in.oxy.value = 9, delta.oxy = NULL, plot = F))$dataframe$in.oxy.value,
               regexp = NA)
  expect_equal(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2,
                          in.oxy = NULL, in.oxy.value = 9, delta.oxy = NULL, plot = F))$dataframe$in.oxy.value,
               rep(9, nrow(flowthrough_mult.rd)))
})

test_that("inspect.ft - stops if out.oxy and in.oxy entered, but in.oxy does not have same number of columns or a single column.",{
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4,
                          in.oxy = 5:6, in.oxy.value = NULL, delta.oxy = NULL, plot = F)),
               regexp = "inspect.ft: With 'out.oxy' data, 'in.oxy' must be a single column or an equal number of paired columns.")
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4,
                          in.oxy = 5:9, in.oxy.value = NULL, delta.oxy = NULL, plot = F)),
               regexp = "inspect.ft: With 'out.oxy' data, 'in.oxy' must be a single column or an equal number of paired columns.")
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:3,
                          in.oxy = 4, in.oxy.value = NULL, delta.oxy = NULL, plot = F)),
               regexp = NA)
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:3,
                          in.oxy = 4:5, in.oxy.value = NULL, delta.oxy = NULL, plot = F)),
               regexp = NA)
})

test_that("inspect.ft - stops if delta.oxy entered and out.oxy, in.oxy or in.oxy.value have arguments",{
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2,
                          in.oxy = NULL, in.oxy.value = 4, delta.oxy = 3, plot = F),
               regexp = "inspect.ft: With 'delta.oxy' data, 'out.oxy', 'in.oxy' and 'in.oxy.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2,
                          in.oxy = 4, in.oxy.value = NULL, delta.oxy = 3, plot = F),
               regexp = "inspect.ft: With 'delta.oxy' data, 'out.oxy', 'in.oxy' and 'in.oxy.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 4,
                          in.oxy = 5, in.oxy.value = NULL, delta.oxy = 3, plot = F),
               regexp = "inspect.ft: With 'delta.oxy' data, 'out.oxy', 'in.oxy' and 'in.oxy.value' should be NULL.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = NULL,
                          in.oxy = NULL, in.oxy.value = 4, delta.oxy = 3, plot = F),
               regexp = "inspect.ft: With 'delta.oxy' data, 'out.oxy', 'in.oxy' and 'in.oxy.value' should be NULL.")
})

test_that("inspect.ft - stops if x not a dataframe", {
  expect_error(inspect.ft(as.matrix(flowthrough.rd), time = 1, out.oxy = 2,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'x' must be data.frame object.")
  expect_error(inspect.ft(flowthrough.rd[[1]], time = 1, out.oxy = 2,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'x' must be data.frame object.")
  expect_error(inspect.ft("blah", time = 1, out.oxy = 2,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'x' must be data.frame object.")
})

test_that("inspect.ft - stops if column inputs malformed", {
  ## time
  expect_error(inspect.ft(flowthrough.rd, time = 1.1, out.oxy = 2,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'time' - some column inputs are not integers.")
  expect_error(inspect.ft(flowthrough.rd, time = 1:2, out.oxy = 3,
                          in.oxy = 4, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'time' - cannot enter more than 1 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 8, out.oxy = 2,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'time' - one or more column inputs are out of range of allowed data columns.")
  ## out.oxy
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2.1,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'out.oxy' - some column inputs are not integers")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 4:9,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'out.oxy' - cannot enter more than 3 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 8:9,
                          in.oxy = 3, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'out.oxy' - one or more column inputs are out of range of allowed data columns.")
  ## in.oxy
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2,
                          in.oxy = 3.1, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'in.oxy' - some column inputs are not integers.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 4:9,
                          in.oxy = 2, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'out.oxy' - cannot enter more than 3 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = 2:3,
                          in.oxy = 8:9, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
               regexp = "inspect.ft: 'in.oxy' - one or more column inputs are out of range of allowed data columns.")
  ## delta.oxy
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = NULL,
                          in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 4.1, plot = F),
               regexp = "inspect.ft: 'delta.oxy' - some column inputs are not integers")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = NULL,
                          in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 2:5, plot = F),
               regexp = "inspect.ft: 'delta.oxy' - cannot enter more than 3 column\\(s\\) with this input or this dataset.")
  expect_error(inspect.ft(flowthrough.rd, time = 1, out.oxy = NULL,
                          in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 5, plot = F),
               regexp = "inspect.ft: 'delta.oxy' - one or more column inputs are out of range of allowed data columns.")
})


  test_that("inspect.ft - outputs correct warnings", {

  ## time nonnum
  expect_warning(inspect.ft(ft_mult_time_nonnum_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Time column not numeric. Other column checks skipped.")
  ## time Inf
  expect_warning(inspect.ft(ft_mult_time_inf_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Time column. Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_time_inf_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Time column. Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_time_inf_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Time column. Remove or replace before proceeding.")
  ## time NA
  expect_warning(inspect.ft(ft_mult_time_nan_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Time column.")
  expect_warning(inspect.ft(ft_mult_time_nan_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Time column.")
  expect_warning(inspect.ft(ft_mult_time_nan_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Time column.")
  ## time non-seq
  expect_warning(inspect.ft(ft_mult_time_nonseq_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Non-sequential Time values found.")
  expect_warning(inspect.ft(ft_mult_time_nonseq_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Non-sequential Time values found.")
  expect_warning(inspect.ft(ft_mult_time_nonseq_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Non-sequential Time values found.")
  ## time duplicates
  expect_warning(inspect.ft(ft_mult_time_dupe_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Duplicate Time values found.")
  expect_warning(inspect.ft(ft_mult_time_dupe_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Duplicate Time values found.")
  expect_warning(inspect.ft(ft_mult_time_dupe_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Duplicate Time values found.")
  ## time uneven
  expect_warning(inspect.ft(ft_mult_time_uneven_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Time values are not evenly-spaced")
  expect_warning(inspect.ft(ft_mult_time_uneven_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Time values are not evenly-spaced")
  expect_warning(inspect.ft(ft_mult_time_uneven_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "Time values are not evenly-spaced.")
  ## out.oxy nonnum
  expect_warning(inspect.ft(ft_mult_out.oxy_nonnum_1, time = 1, out.oxy = 2:4,
                            in.oxy = 6:8, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Oxygen column\\(s) not numeric. Other column checks skipped.")
  ## out.oxy Inf
  expect_warning(inspect.ft(ft_mult_out.oxy_inf_1, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_out.oxy_inf_3, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_out.oxy_inf_mult, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  ## out.oxy NA
  expect_warning(inspect.ft(ft_mult_out.oxy_nan_1, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_out.oxy_nan_3, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_out.oxy_nan_mult, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  ## out.oxy nonnum
  expect_warning(inspect.ft(ft_mult_in.oxy_nonnum_1, time = 1, out.oxy = 2:4,
                            in.oxy = 6:8, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Oxygen column\\(s) not numeric. Other column checks skipped.")
  ## in.oxy Inf
  expect_warning(inspect.ft(ft_mult_in.oxy_inf_1, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_in.oxy_inf_3, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_in.oxy_inf_mult, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  ## in.oxy NA
  expect_warning(inspect.ft(ft_mult_in.oxy_nan_1, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_in.oxy_nan_3, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_in.oxy_nan_mult, time = 1, out.oxy = 2:4,
                            in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")

  ## delta.oxy non-numeric
  expect_warning(inspect.ft(ft_mult_delta.oxy_nonnum_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "inspect.ft: Oxygen column\\(s) not numeric. Other column checks skipped.")
  ## delta.oxy Inf
  expect_warning(inspect.ft(ft_mult_delta.oxy_inf_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_delta.oxy_inf_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  expect_warning(inspect.ft(ft_mult_delta.oxy_inf_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "inspect.ft: Inf/-Inf values detected in Oxygen column\\(s). Remove or replace before proceeding.")
  ## delta.oxy NA
  expect_warning(inspect.ft(ft_mult_delta.oxy_nan_1, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_delta.oxy_nan_3, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
  expect_warning(inspect.ft(ft_mult_delta.oxy_nan_mult, time = 1, out.oxy = NULL,
                            in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F),
                 regexp = "NA/NaN values detected in Oxygen column\\(s\\).")
})


test_that("inspect.ft works with 2-column data", {
  expect_error(inspect.ft(sardine.rd[,1:2], plot = F),
               regexp = NA)
  expect_equal(ncol(inspect.ft(sardine.rd[,1:2], plot = F)$dataframe),
               2)
})

test_that("inspect.ft works with multi-column data", {
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4,
                          in.oxy = 5:7, plot = F)),
               regexp = NA)
  expect_equal(ncol(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2:4,
                               in.oxy = 5:7, plot = F))$dataframe),
               7 + 3) # 7 inputs, 3 delta calcs
})

test_that("inspect.ft outputs plots", {
  expect_error(suppressWarnings(inspect.ft(flowthrough_mult.rd, time = 1, out.oxy = 2,
                          in.oxy = 5, delta.oxy = NULL, plot = T)),
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

 test_that("inspect.ft objects can be printed, including those which show warnings", {
  ## regular good data
  expect_output(print(insp.ft.obj))
  expect_error(print(insp.ft.obj),
               regexp = NA)
  expect_output(print(insp.ft.mult.obj))
  expect_error(print(insp.ft.mult.obj),
               regexp = NA)

  ## various errors/warnings
  ob1 <- suppressWarnings(inspect.ft(ft_mult_out.oxy_nan_mult, time = 1, out.oxy = 2:4,
                                     in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob2 <- suppressWarnings(inspect.ft(ft_mult_time_nonseq_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob3 <- suppressWarnings(inspect.ft(ft_mult_time_nan_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob4 <- suppressWarnings(inspect.ft(ft_mult_out.oxy_nan_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob5 <- suppressWarnings(inspect.ft(ft_mult_out.oxy_nan_mult, time = 1, out.oxy = 2:4,
                                     in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob6 <- suppressWarnings(inspect.ft(ft_mult_in.oxy_nan_3, time = 1, out.oxy = 2:4,
                                     in.oxy = 5:7, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob7 <- suppressWarnings(inspect.ft(ft_mult_delta.oxy_nan_1, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F))
  ob8 <- suppressWarnings(inspect.ft(ft_mult_time_dupe_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob8 <- suppressWarnings(inspect.ft(ft_mult_time_dupe_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob9 <- suppressWarnings(inspect.ft(ft_mult_time_inf_1, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob10 <- suppressWarnings(inspect.ft(ft_mult_time_inf_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob11 <- suppressWarnings(inspect.ft(ft_mult_out.oxy_inf_1, time = 1, out.oxy = 2,
                                     in.oxy = 6, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob12 <- suppressWarnings(inspect.ft(ft_mult_out.oxy_inf_mult, time = 1, out.oxy = 2:4,
                                     in.oxy = 6:8, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob13 <- suppressWarnings(inspect.ft(ft_mult_in.oxy_inf_1, time = 1, out.oxy = 2,
                                     in.oxy = 6, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob14 <- suppressWarnings(inspect.ft(ft_mult_in.oxy_inf_mult, time = 1, out.oxy = 2:4,
                                     in.oxy = 6:8, in.oxy.value = NULL, delta.oxy = NULL, plot = F))
  ob15 <- suppressWarnings(inspect.ft(ft_mult_delta.oxy_inf_1, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8, plot = F))
  ob16 <- suppressWarnings(inspect.ft(ft_mult_delta.oxy_inf_mult, time = 1, out.oxy = NULL,
                                     in.oxy = NULL, in.oxy.value = NULL, delta.oxy = 8:10, plot = F))


  expect_output(print(ob1))
  expect_output(print(ob2))
  expect_output(print(ob3))
  expect_output(print(ob4))
  expect_output(print(ob5))
  expect_output(print(ob6))
  expect_output(print(ob7))
  expect_output(print(ob8))
  expect_output(print(ob9))
  expect_output(print(ob10))
  expect_output(print(ob11))
  expect_output(print(ob12))
  expect_output(print(ob13))
  expect_output(print(ob14))
  expect_output(print(ob15))
  expect_output(print(ob16))

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
  expect_error(print(ob9),
               regexp = NA)
  expect_error(print(ob10),
               regexp = NA)
  expect_error(print(ob11),
               regexp = NA)
  expect_error(print(ob12),
               regexp = NA)
  expect_error(print(ob13),
               regexp = NA)
  expect_error(print(ob14),
               regexp = NA)
  expect_error(print(ob15),
               regexp = NA)
  expect_error(print(ob16),
               regexp = NA)

  expect_output(summary(ob1))
  expect_output(summary(ob2))
  expect_output(summary(ob3))
  expect_output(summary(ob4))
  expect_output(summary(ob5))
  expect_output(summary(ob6))
  expect_output(summary(ob7))
  expect_output(summary(ob8))
  expect_output(summary(ob9))
  expect_output(summary(ob10))
  expect_output(summary(ob11))
  expect_output(summary(ob12))
  expect_output(summary(ob13))
  expect_output(summary(ob14))
  expect_output(summary(ob15))
  expect_output(summary(ob16))

  expect_error(summary(ob1),
               regexp = NA)
  expect_error(summary(ob2),
               regexp = NA)
  expect_error(summary(ob3),
               regexp = NA)
  expect_error(summary(ob4),
               regexp = NA)
  expect_error(summary(ob5),
               regexp = NA)
  expect_error(summary(ob6),
               regexp = NA)
  expect_error(summary(ob7),
               regexp = NA)
  expect_error(summary(ob8),
               regexp = NA)
  expect_error(summary(ob9),
               regexp = NA)
  expect_error(summary(ob10),
               regexp = NA)
  expect_error(summary(ob11),
               regexp = NA)
  expect_error(summary(ob12),
               regexp = NA)
  expect_error(summary(ob13),
               regexp = NA)
  expect_error(summary(ob14),
               regexp = NA)
  expect_error(summary(ob15),
               regexp = NA)
  expect_error(summary(ob16),
               regexp = NA)

})

test_that("inspect.ft - plot defaults are correctly restored", {

  # reset plotting first
  dev.off()
  # save par before
  parb4 <- par(no.readonly = TRUE)
  # now use a fn with plot
  inspect.ft(flowthrough.rd, 1, 2, 3)
  # save after
  paraft <- par(no.readonly = TRUE)
  # mai is something changed from the default,
  # so if par settings not restored properly this should fail
  expect_identical(parb4$mai,
                   paraft$mai)

})


}) ## turns console printing back on
