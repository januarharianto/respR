## library(testthat)
## rm(list=ls())
## testthat::test_file("tests/testthat/test-adjust_rate.R")

capture.output({  ## stops printing outputs on assigning

  # test_that("adjust_rate: All tests pass",
  #   {

  test_that("adjust_rate stops with wrong method", {
    expect_error(adjust_rate(100, 10, method = "text"),
                 regexp = "adjust_rate: 'method' input not recognised")
  })


  # make objects for running tests ------------------------------------------

  {
    suppressWarnings(suppressMessages(insp_obj_single <- inspect(urchins.rd, time = 1, oxygen = 2, plot = F)))
    suppressWarnings(suppressMessages(insp_obj_multiple <- inspect(urchins.rd, time = 1, oxygen = 2:5, plot = F)))

    cr_obj_single <- calc_rate(insp_obj_single, from = 3, to = 40, by = "time", plot = F)
    cr_obj_three <- calc_rate(insp_obj_single, from = c(3,13,23), to = c(12,22,32), by = "time", plot = F)
    cr_obj_eight <- calc_rate(insp_obj_single, from = c(3,6,9,12,15,18,21,24), to = c(6,9,12,15,18,21,24,27), by = "time", plot = F)

    ar_obj <- auto_rate(insp_obj_single, plot = F)
    ## auto_rate object with single rate
    ar_obj_single <- ar_obj
    ar_obj_single$summary <- ar_obj_single$summary[2,]
    ar_obj_single$rate <- ar_obj_single$rate[2]

    ## bg objects
    bg_single <- calc_rate.bg(urchins.rd, time = 1, oxygen = 18, plot = F)
    bg_three <- calc_rate.bg(urchins.rd, time = 1, oxygen = 12:14, plot = F)
    bg_four <- calc_rate.bg(urchins.rd, time = 1, oxygen = 12:15, plot = F)
    bg_eight <- calc_rate.bg(urchins.rd, time = 1, oxygen = 12:19, plot = F)

    ## calc_rate objects
    cr_single <- suppressWarnings(calc_rate(inspect(urchins.rd, time = 1, oxygen = 18, plot = F), plot = F))
    cr_three <-  suppressWarnings(calc_rate(inspect(urchins.rd, time = 1, oxygen = 12:14, plot = F), plot = F))
    cr_four <-   suppressWarnings(calc_rate(inspect(urchins.rd, time = 1, oxygen = 12:15, plot = F), plot = F))
    cr_eight <-  suppressWarnings(calc_rate(inspect(urchins.rd, time = 1, oxygen = 12:19, plot = F), plot = F))

    # dfs with diff column numbers
    bg_df2col <- urchins.rd[,c(1,18)]
    bg_df3col <- urchins.rd[,c(1,18,19)]
    bg_df7col <- urchins.rd[,c(1,18,19,18,19,18,19,18)]

    # inspect of above
    insp_bg_df2col <- suppressWarnings(suppressMessages(inspect(bg_df2col, time = 1, oxygen = 2, plot = F)))
    insp_bg_df3col <- suppressWarnings(suppressMessages(inspect(bg_df3col, time = 1, oxygen = 2:3, plot = F)))
    insp_bg_df7col <- suppressWarnings(suppressMessages(inspect(bg_df7col, time = 1, oxygen = 2:8, plot = F)))

    # calc_rate.bg of above
    crbg_df2col <- calc_rate.bg(bg_df2col, plot = F)
    crbg_df3col <- calc_rate.bg(bg_df3col, plot = F)
    crbg_df7col <- calc_rate.bg(bg_df7col, plot = F)

    # objects for "linear" and "exponential" testing
    # "pre" experiment background rate
    # gives "low" bg rate of -0.0004567706
    crbg_pre_2col <- urchins.rd[1:70, c(1,18)] %>%
      calc_rate.bg(plot = FALSE)
    crbg_pre_3col <- urchins.rd[1:70, c(1,18:19)] %>%
      calc_rate.bg(plot = FALSE)
    ## as df
    bgdf_pre_2col <- urchins.rd[1:70, c(1,18)]
    bgdf_pre_3col <- urchins.rd[1:70, c(1,18:19)]
    ## as inspect
    insp_pre_2col <- suppressWarnings(suppressMessages(inspect(urchins.rd[1:70, c(1,18)], plot = FALSE)))
    insp_pre_3col <- suppressWarnings(suppressMessages(inspect(urchins.rd[1:70, c(1,18:19)], time = 1, oxygen = 2:3, plot = F)))
    ## as calc_rate - this for testing warning of rate timestamp outside time range of by/by2
    cr_pre <- calc_rate(urchins.rd[1:70, c(1,18)], plot = FALSE)

    # "post" experiment background rate
    # gives "high" bg rate of -0.001268691
    crbg_post_2col <- urchins.rd[230:271, c(1,19)] %>%
      calc_rate.bg(plot = FALSE)
    crbg_post_3col <- urchins.rd[230:271, c(1,18:19)] %>%
      calc_rate.bg(plot = FALSE)
    ## as df
    bgdf_post_2col <- urchins.rd[230:271, c(1,19)]
    bgdf_post_3col <- urchins.rd[230:271, c(1,18:19)]
    ## as inspect
    insp_post_2col <- suppressWarnings(suppressMessages(inspect(urchins.rd[230:271, c(1,19)], plot = FALSE)))
    insp_post_3col <- suppressWarnings(suppressMessages(inspect(urchins.rd[230:271, c(1,18:19)], plot = FALSE)))

    ## versions of above with POSITIVE background rates
    rev_pre <- urchins.rd[1:70, c(1,18:19)]
    rev_pre[[2]] <- rev(rev_pre[[2]])
    rev_pre[[3]] <- rev(rev_pre[[3]])

    crbg_pre_2col_pos <- rev_pre[,1:2] %>%
      calc_rate.bg(plot = FALSE)
    crbg_pre_3col_pos <- rev_pre %>%
      calc_rate.bg(plot = FALSE)
    ## as df
    bgdf_pre_2col_pos <- rev_pre[,1:2]
    bgdf_pre_3col_pos <- rev_pre
    ## as inspect
    insp_pre_2col_pos <- suppressWarnings(suppressMessages(inspect(rev_pre[,1:2], plot = FALSE)))
    insp_pre_3col_pos <- suppressWarnings(suppressMessages(inspect(rev_pre, time = 1, oxygen = 2:3, plot = F)))

    ## versions of above with POSITIVE background rates
    rev_post <- urchins.rd[230:271, c(1,18:19)]
    rev_post[[2]] <- rev(rev_post[[2]])
    rev_post[[3]] <- rev(rev_post[[3]])

    crbg_post_2col_pos <- rev_post[,1:2] %>%
      calc_rate.bg(plot = FALSE)
    crbg_post_3col_pos <- rev_post %>%
      calc_rate.bg(plot = FALSE)
    ## as df
    bgdf_post_2col_pos <- rev_post[,1:2]
    bgdf_post_3col_pos <- rev_post
    ## as inspect
    insp_post_2col_pos <- suppressWarnings(suppressMessages(inspect(rev_post[,1:2], plot = FALSE)))
    insp_post_3col_pos <- suppressWarnings(suppressMessages(inspect(rev_post, time = 1, oxygen = 2:3, plot = F)))



    ## intermediately timed data of a specimen
    # gives specimen rate of -0.0280796
    ## as df
    spec_df <- urchins.rd[71:199, c(1,2)]
    ## as inspect
    spec_insp <- suppressWarnings(suppressMessages(inspect(urchins.rd[71:199, c(1,2)], plot = FALSE)))
    ## as calc_rate object
    spec_cr <- urchins.rd[71:199, c(1,2)] %>%
      calc_rate(plot = FALSE)
    ## as auto_rate object - gives three rates
    spec_ar <- urchins.rd[71:199, c(1,2)] %>%
      auto_rate(plot = FALSE)
    ## as auto_rate object - with single rate
    spec_ar_single <- spec_ar
    spec_ar_single$summary <- spec_ar_single$summary[2,]
    spec_ar_single$rate <- spec_ar_single$rate[2]


    ## objs with rates of different sign
    cr_obj_mixed_sign <- calc_rate(intermittent.rd, from = c(30, 1000, 1900, 2000),
                                   to = c(130, 1100, 2000, 2100), by = "time", plot = FALSE)
    cr_obj_pos <- calc_rate(intermittent.rd, from = c(1900, 2000, 3550, 3600),
                            to = c(2000, 2100, 3650, 3700), by = "time", plot = FALSE)

    ar_obj_pos <- auto_rate(
      data.frame(urchins.rd[[1]], rev(urchins.rd[[2]])), plot = FALSE)
    ar_obj_mixed_sign <- auto_rate(intermittent.rd, plot = FALSE)


    ## auto_rate objects of different methods with lots of rates
    ar_obj_highest <- auto_rate(urchins.rd[,1:2], method = "highest", plot = F)
    ar_obj_lowest <- auto_rate(urchins.rd[,1:2], method = "lowest", plot = F)
    ar_obj_interval <- auto_rate(urchins.rd[,1:2], method = "interval", width = 0.05, plot = F)


  } # end make objects

  # Tests -------------------------------------------------------------------


  # method = "value" -------------------------------------------------------

  test_that("adjust_rate: method = 'value' - correct message", {
    # if NULL
    expect_message(adjust_rate(x = -0.1, by = -0.005, method = "value"),
                   "adjust_rate: Rate adjustments applied using \"value\" method.",
                   fixed = TRUE)
  })

  test_that("adjust_rate: method = 'value' - stops if 'x' is not numeric, calc_rate, or auto_rate", {
    expect_error(adjust_rate(NULL, by = -0.005, method = "value"),
                 "adjust_rate: for method = 'value' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate("text", by = -0.005, method = "value"),
                 "adjust_rate: for method = 'value' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(bg_single, by = -0.005, method = "value"),
                 "adjust_rate: for method = 'value' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
  })

  test_that("adjust_rate: method = 'value' - stops if 'by' is not a single numeric or calc_rate.bg", {
    expect_error(adjust_rate(ar_obj, by = c(1,2), method = "value"),
                 "adjust_rate: for method = 'value' the 'by' input must be a single numeric value, 'calc_rate.bg' object with one value in '\\$rate.bg', or `calc_rate` object with one value in '\\$rate'")
    expect_error(adjust_rate(ar_obj, by = "text", method = "value"),
                 "adjust_rate: for method = 'value' the 'by' input must be a single numeric value, 'calc_rate.bg' object with one value in '\\$rate.bg', or `calc_rate` object with one value in '\\$rate'")
    expect_error(adjust_rate(ar_obj, by = ar_obj, method = "value"),
                 "adjust_rate: for method = 'value' the 'by' input must be a single numeric value, 'calc_rate.bg' object with one value in '\\$rate.bg', or `calc_rate` object with one value in '\\$rate'")
    expect_error(adjust_rate(ar_obj, by = crbg_df3col, method = "value"),
                 "adjust_rate: for method = 'value' the 'by' input must be a single numeric value, 'calc_rate.bg' object with one value in '\\$rate.bg', or `calc_rate` object with one value in '\\$rate'")
    # Causes obscure R CMD CHECK error
    #expect_error(adjust_rate(ar_obj, by = bg_df2col, method = "value"),
    #            "adjust_rate: for method = 'value' the 'by' input must be a single numeric value, 'calc_rate.bg' object with one value in '\\$rate.bg', or `calc_rate` object with one value in '\\$rate'")
  })

  test_that("adjust_rate: method = 'value' - stops if 'by2', 'time_x', 'time_by', or 'time_by2' have inputs.", {
    expect_error(adjust_rate(cr_obj_three, by = -0.005, by2 = 0.003, method = "value"),
                 "adjust_rate: for method = 'value' the 'by2' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, by = -0.005, time_x = 0.003, method = "value"),
                 "adjust_rate: for method = 'value' the 'time_x' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, by = -0.005, time_by = 0.003, method = "value"),
                 "adjust_rate: for method = 'value' the 'time_by' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, by = -0.005, time_by2 = 0.003, method = "value"),
                 "adjust_rate: for method = 'value' the 'time_by2' input should be NULL.")
  })

  test_that("adjust_rate: method = 'value' - outputs correct results with numeric 'x', numeric 'by' inputs.", {
    ## single values for both
    rate = -0.01
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate - by)
    ## multiple values for rate
    rate = c(-0.01, -0.02, -0.03)
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate - by)
    ## should always be equal to 'mean' method
    rate = c(-0.01, -0.02, -0.03)
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 adjust_rate(rate, by = by, method = "mean")$rate.adjusted)
  })

  test_that("adjust_rate: method = 'value' - outputs correct results with calc_rate 'x', numeric 'by' inputs.", {
    ## calc_rate object with single rate, adjusted by single by
    rate = cr_obj_single
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by)
    ## calc_rate object with multiple rates, adjusted by single by
    rate = cr_obj_three
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by)
  })

  test_that("adjust_rate: method = 'value' - outputs correct results with auto_rate 'x', numeric 'by' inputs.", {
    ## auto_rate object adjusted by single by
    rate = ar_obj
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by)

    rate = ar_obj_single
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by)

    rate = ar_obj_highest
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by)

    rate = ar_obj_interval
    by = -0.001
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by)
  })

  test_that("adjust_rate: method = 'value' - outputs correct results with numeric 'x', calc_rate.bg 'by' inputs.", {
    ## single x, calc_rate.bg with single bg rate
    rate = -0.01
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate - by$rate.bg)
    ## multiple x, calc_rate.bg with single bg rate
    rate = c(-0.01, -0.02, -0.03)
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate - by$rate.bg)
  })

  test_that("adjust_rate: method = 'value' - outputs correct results with calc_rate 'x', calc_rate.bg 'by' inputs.", {
    ## single calc_rate x, calc_rate.bg with single bg rate
    rate = cr_obj_single
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate.bg)
    ## multiple calc_rate x, calc_rate.bg with single bg rate
    rate = cr_obj_three
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate.bg)
  })

  test_that("adjust_rate: method = 'value' - outputs correct results with auto_rate 'x', calc_rate.bg 'by' inputs.", {
    rate = ar_obj
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate.bg)

    rate = ar_obj_single
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate.bg)

    rate = ar_obj_highest
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate.bg)

    rate = ar_obj_interval
    by = bg_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate.bg)
  })

  ## added ability to use calc_rate objects as `by` in March 2022
  ## This is the only test that it works. Not gonna do it for every x input
  test_that("adjust_rate: method = 'value' - outputs correct results with auto_rate 'x', calc_rate 'by' inputs.", {
    rate = ar_obj
    by = cr_obj_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate)

    rate = ar_obj_single
    by = cr_obj_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate)

    rate = ar_obj_highest
    by = cr_obj_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate)

    rate = ar_obj_interval
    by = cr_obj_single
    expect_equal(adjust_rate(rate, by = by, method = "value")$rate.adjusted,
                 rate$rate - by$rate)
  })


  # method = "mean" ---------------------------------------------------------

  test_that("adjust_rate: method = 'mean' - correct message", {
    # if NULL
    expect_message(adjust_rate(cr_obj_three, by = -0.005),
                   "adjust_rate: Rate adjustments applied using \"mean\" method.",
                   fixed = TRUE)
    # if method = "mean" specified
    expect_message(adjust_rate(cr_obj_single, method = "mean", by = -0.005),
                   "adjust_rate: Rate adjustments applied using \"mean\" method.",
                   fixed = TRUE)
  })

  test_that("adjust_rate: method = 'mean' - stops if 'x' is not numeric, calc_rate, or auto_rate", {
    expect_error(adjust_rate(NULL, by = -0.005),
                 "adjust_rate: for method = 'mean' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate("text", by = -0.005),
                 "adjust_rate: for method = 'mean' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(bg_single, by = -0.005),
                 "adjust_rate: for method = 'mean' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
  })

  test_that("adjust_rate: method = 'mean' - stops if 'by' is not numeric or calc_rate.bg", {
    expect_error(adjust_rate(ar_obj, by = "text"),
                 "adjust_rate: for method = 'mean' the 'by' input must be numeric, object of class 'calc_rate.bg', or object of class 'calc_rate'.")
    expect_error(adjust_rate(ar_obj, by = ar_obj),
                 "adjust_rate: for method = 'mean' the 'by' input must be numeric, object of class 'calc_rate.bg', or object of class 'calc_rate'.")
  })

  test_that("adjust_rate: method = 'mean' - stops if 'by2', 'time_x', 'time_by', or 'time_by2' have inputs.", {
    expect_error(adjust_rate(cr_obj_three, by = -0.005, by2 = 0.003),
                 "adjust_rate: for method = 'mean' the 'by2' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, by = -0.005, time_x = 0.003),
                 "adjust_rate: for method = 'mean' the 'time_x' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, by = -0.005, time_by = 0.003),
                 "adjust_rate: for method = 'mean' the 'time_by' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, by = -0.005, time_by2 = 0.003),
                 "adjust_rate: for method = 'mean' the 'time_by2' input should be NULL.")
  })

  test_that("adjust_rate: method = 'mean' - outputs correct results with numeric 'x', numeric 'by' inputs.", {
    ## single values for both
    rate = -0.01
    by = -0.001
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by))
    ## multiple values for rate
    rate = c(-0.01, -0.02, -0.03)
    by = -0.001
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by))
    ## multiple values for by
    rate = -0.01
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by))
    ## multiple values for both - equal lengths
    rate = c(-0.01, -0.02, -0.03)
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by))
    ## multiple values for both - different lengths
    rate = c(-0.01, -0.02, -0.03, -0.04, -0.05)
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by))
    rate = c(-0.01, -0.02, -0.03)
    by = c(-0.001, -0.002, -0.003, -0.004, -0.005)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by))
  })

  test_that("adjust_rate: method = 'mean' - outputs correct results with calc_rate 'x', numeric 'by' inputs.", {
    ## calc_rate object with single rate, adjusted by single by
    rate = cr_obj_single
    by = -0.001
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## calc_rate object with multiple rates, adjusted by single by
    rate = cr_obj_three
    by = -0.001
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## calc_rate object with single rate, adjusted by multiple by
    rate = cr_obj_single
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## calc_rate object with multiple rates, adjusted by multiple by - equal lengths
    rate = cr_obj_three
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## calc_rate object with multiple rates, adjusted by multiple by - different lengths
    rate = cr_obj_three
    by = c(-0.001, -0.002)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    rate = cr_obj_three
    by = c(-0.001, -0.002, -0.003, -0.004, -0.005)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
  })

  test_that("adjust_rate: method = 'mean' - outputs correct results with auto_rate 'x', numeric 'by' inputs.", {
    ## auto_rate object adjusted by single by
    rate = ar_obj
    by = -0.001
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## auto_rate object adjusted by multiple by
    rate = ar_obj
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    rate = ar_obj
    by = c(-0.001, -0.002, -0.003, -0.004, -0.005)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## auto_rate object with multiple rates, adjusted by multiple by - equal lengths
    rate = ar_obj
    by = c(-0.001, -0.002, -0.003, -0.004)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    ## auto_rate object with multiple rates, adjusted by multiple by - equal lengths
    rate = ar_obj_highest
    by = c(-0.001, -0.002, -0.003, -0.004)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    rate = ar_obj_interval
    by = c(-0.001, -0.002, -0.003, -0.004)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
    rate = ar_obj_single
    by = c(-0.001, -0.002, -0.003, -0.004)
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by))
  })

  test_that("adjust_rate: method = 'mean' - outputs correct results with numeric 'x', calc_rate.bg 'by' inputs.", {
    ## single x, calc_rate.bg with single bg rate
    rate = -0.01
    by = bg_single
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by$rate.bg))
    ## multiple x, calc_rate.bg with single bg rate
    rate = c(-0.01, -0.02, -0.03)
    by = bg_single
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by$rate.bg))
    ## single x, calc_rate.bg with multiple bg rate
    rate = -0.01
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by$rate.bg))
    ## multiple x, calc_rate.bg with multiple bg rate
    rate = c(-0.01, -0.02, -0.03)
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate - mean(by$rate.bg))
  })

  test_that("adjust_rate: method = 'mean' - outputs correct results with calc_rate 'x', calc_rate.bg 'by' inputs.", {
    ## single calc_rate x, calc_rate.bg with single bg rate
    rate = cr_obj_single
    by = bg_single
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
    ## multiple calc_rate x, calc_rate.bg with single bg rate
    rate = cr_obj_three
    by = bg_single
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
    ## single calc_rate x, calc_rate.bg with multiple bg rate
    rate = cr_obj_single
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
    ## multiple calc_rate x, calc_rate.bg with multiple bg rate
    rate = cr_obj_three
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
  })

  test_that("adjust_rate: method = 'mean' - outputs correct results with auto_rate 'x', calc_rate.bg 'by' inputs.", {
    ## single auto_rate x, calc_rate.bg with single bg rate
    rate = ar_obj
    by = bg_single
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
    ## single auto_rate x, calc_rate.bg with multiple bg rate
    rate = ar_obj
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))

    rate = ar_obj_highest
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
    rate = ar_obj_interval
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
    rate = ar_obj_single
    by = bg_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate.bg))
  })


  test_that("adjust_rate: method = 'mean' - outputs correct results with auto_rate 'x', calc_rate 'by' inputs.", {
    ## single auto_rate x, calc_rate.bg with single bg rate
    rate = ar_obj
    by = cr_obj_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate))
    ## single auto_rate x, calc_rate with multiple bg rate
    rate = ar_obj
    by = cr_obj_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate))

    rate = ar_obj_highest
    by = cr_obj_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate))
    rate = ar_obj_interval
    by = cr_obj_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate))
    rate = ar_obj_single
    by = cr_obj_three
    expect_equal(adjust_rate(rate, by = by)$rate.adjusted,
                 rate$rate - mean(by$rate))
  })


  # method = "paired" -------------------------------------------------------

  test_that("adjust_rate: method = 'paired' - correct message", {
    # if method = "mean" specified
    expect_message(adjust_rate(cr_obj_single, method = "paired", by = -0.005),
                   "adjust_rate: Rate adjustments applied using \"paired\" method.",
                   fixed = TRUE)
  })

  test_that("adjust_rate: method = 'paired' - stops if 'x' is not numeric, calc_rate, or auto_rate", {
    expect_error(adjust_rate("text", method = "paired", by = -0.005),
                 "adjust_rate: for method = 'paired' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(bg_single, method = "paired", by = -0.005),
                 "adjust_rate: for method = 'paired' the 'x' input must be numeric or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
  })

  test_that("adjust_rate: method = 'paired' - stops if 'by' is not numeric or calc_rate.bg", {
    expect_error(adjust_rate(ar_obj, method = "paired", by = "text"),
                 "adjust_rate: for method = 'paired' the 'by' input must be numeric, object of class 'calc_rate.bg', or object of class 'calc_rate'.")
    expect_error(adjust_rate(ar_obj, method = "paired", by = ar_obj),
                 "adjust_rate: for method = 'paired' the 'by' input must be numeric, object of class 'calc_rate.bg', or object of class 'calc_rate'.")
  })

  test_that("adjust_rate: method = 'paired' - stops if 'by2', 'time_x', 'time_by', or 'time_by2' have inputs.", {
    expect_error(adjust_rate(cr_obj_three, method = "paired", by = -0.005, by2 = 0.003),
                 "adjust_rate: for method = 'paired' the 'by2' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, method = "paired", by = -0.005, time_x = 0.003),
                 "adjust_rate: for method = 'paired' the 'time_x' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, method = "paired", by = -0.005, time_by = 0.003),
                 "adjust_rate: for method = 'paired' the 'time_by' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, method = "paired", by = -0.005, time_by2 = 0.003),
                 "adjust_rate: for method = 'paired' the 'time_by2' input should be NULL.")
  })

  test_that("adjust_rate: method = 'paired' - stops if 'x' and 'by' are not same length", {
    expect_error(adjust_rate(-0.05, method = "paired", by = c(-0.004, -0.005)),
                 "adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")
    expect_error(adjust_rate(cr_obj_single, method = "paired", by = c(-0.004, -0.005)),
                 "adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")
    expect_error(adjust_rate(cr_obj_three, method = "paired", by = -0.005),
                 "adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")
    expect_error(adjust_rate(ar_obj, method = "paired", by = -0.005),
                 "adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")
    expect_error(adjust_rate(ar_obj, method = "paired", by = bg_single),
                 "adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")
    expect_error(adjust_rate(ar_obj, method = "paired", by = bg_three),
                 "adjust_rate: for method = 'paired' the 'x' and 'by' inputs should have the same number of rates.")
  })

  test_that("adjust_rate: method = 'paired' - outputs correct results with numeric 'x', numeric 'by' inputs.", {
    ## single values
    rate = -0.01
    by = -0.001
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate - (by))
    ## multiple values
    rate = c(-0.01, -0.02, -0.03)
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate - (by))
  })

  test_that("adjust_rate: method = 'paired' - outputs correct results with calc_rate 'x', numeric 'by' inputs.", {
    ## single values
    rate = cr_obj_single
    by = -0.001
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by))
    ## multiple values
    rate = cr_obj_three
    by = c(-0.001, -0.002, -0.003)
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by))
  })

  test_that("adjust_rate: method = 'paired' - outputs correct results with auto_rate 'x', numeric 'by' inputs.", {
    rate = ar_obj
    by = c(-0.001, -0.002, -0.003, -0.004)
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by))
  })

  test_that("adjust_rate: method = 'paired' - outputs correct results with calc_rate 'x', calc_rate.bg 'by' inputs.", {
    ## single values
    rate = cr_obj_single
    by = bg_single
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate.bg))
    ## multiple values
    rate = cr_obj_three
    by = bg_three
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate.bg))
    rate = cr_obj_eight
    by = bg_eight
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate.bg))
  })

  test_that("adjust_rate: method = 'paired' - outputs correct results with auto_rate 'x', calc_rate.bg 'by' inputs.", {
    rate = ar_obj
    by = bg_four
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate.bg))
  })


  test_that("adjust_rate: method = 'paired' - outputs correct results with calc_rate 'x', calc_rate 'by' inputs.", {
    # all of these adjustments should equal zero
    ## single values
    rate = cr_obj_single
    by = cr_obj_single
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate))
    ## multiple values
    rate = cr_obj_three
    by = cr_obj_three
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate))
    rate = cr_obj_eight
    by = cr_obj_eight
    expect_equal(adjust_rate(rate, method = "paired", by = by)$rate.adjusted,
                 rate$rate - (by$rate))
  })

  # method = "concurrent" ---------------------------------------------------

  test_that("adjust_rate: method = 'concurrent' - correct message", {
    # if method = "mean" specified
    expect_message(adjust_rate(cr_obj_single, method = "concurrent", by = bg_df2col),
                   "adjust_rate: Rate adjustments applied using \"concurrent\" method.",
                   fixed = TRUE)
  })

  test_that("adjust_rate: method = 'concurrent' - stops if 'x' is not calc_rate or auto_rate", {
    expect_error(adjust_rate("text", method = "concurrent", by = bg_df2col),
                 "adjust_rate: For method = \"concurrent\" the 'x' input must be a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' object.")
    expect_error(adjust_rate(bg_df2col, method = "concurrent", by = bg_df2col),
                 "adjust_rate: For method = \"concurrent\" the 'x' input must be a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' object.")
    expect_error(adjust_rate(bg_df2col, method = "concurrent", by = bg_df2col),
                 "adjust_rate: For method = \"concurrent\" the 'x' input must be a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' object.")
    expect_error(adjust_rate(bg_df7col, method = "concurrent", by = bg_df2col),
                 "adjust_rate: For method = \"concurrent\" the 'x' input must be a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' object.")
    expect_error(adjust_rate(insp_bg_df2col, method = "concurrent", by = bg_df2col),
                 "adjust_rate: For method = \"concurrent\" the 'x' input must be a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' object.")
    expect_error(adjust_rate(insp_bg_df7col, method = "concurrent", by = bg_df2col),
                 "adjust_rate: For method = \"concurrent\" the 'x' input must be a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' object.")
  })

  test_that("adjust_rate: method = 'concurrent' - stops if 'by' is not data.frame, inspect or calc_rate.bg", {
    expect_error(adjust_rate(ar_obj, method = "concurrent", by = "text"),
                 "adjust_rate: For method = \"concurrent\" the 'by' input must be a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object.")
    expect_error(adjust_rate(ar_obj, method = "concurrent", by = -0.0005),
                 "adjust_rate: For method = \"concurrent\" the 'by' input must be a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object.")
    expect_error(adjust_rate(ar_obj, method = "concurrent", by = c(-0.0005,-0.0004,-0.0003)),
                 "adjust_rate: For method = \"concurrent\" the 'by' input must be a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object.")
    # expect_error(adjust_rate(ar_obj, method = "concurrent", by = cr_obj_single),
    #              "adjust_rate: For method = \"concurrent\" the 'by' input must be a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object.")
    expect_error(adjust_rate(ar_obj, method = "concurrent", by = ar_obj),
                 "adjust_rate: For method = \"concurrent\" the 'by' input must be a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object.")
  })

  test_that("adjust_rate: method = 'concurrent' - stops if 'by2', 'time_x', 'time_by', or 'time_by2' have inputs.", {
    expect_error(adjust_rate(cr_obj_three, method = "concurrent", by = bg_df2col, by2 = 0.003),
                 "adjust_rate: for method = 'concurrent' the 'by2' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, method = "concurrent", by = bg_df2col, time_x = 0.003),
                 "adjust_rate: for method = 'concurrent' the 'time_x' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, method = "concurrent", by = bg_df2col, time_by = 0.003),
                 "adjust_rate: for method = 'concurrent' the 'time_by' input should be NULL.")
    expect_error(adjust_rate(cr_obj_three, method = "concurrent", by = bg_df2col, time_by2 = 0.003),
                 "adjust_rate: for method = 'concurrent' the 'time_by2' input should be NULL.")
  })

  test_that("adjust_rate: method = 'concurrent' - warns if 'x' and 'by' are differ in length by more than 5%", {
    expect_warning(adjust_rate(cr_obj_single, method = "concurrent", by = urchins.rd[1:250,]),
                   "adjust_rate: 'x' and 'by' inputs differ in length by more than 5%.")
    expect_warning(adjust_rate(cr_obj_single, method = "concurrent", by = sardine.rd),
                   "adjust_rate: 'x' and 'by' inputs differ in length by more than 5%.")
  })

  test_that("adjust_rate: method = 'concurrent' - warns if 'by' is missing some time values used for rates in 'x'.", {
    expect_warning(adjust_rate(cr_obj_single, method = "concurrent", by = urchins.rd[1:240,]),
                   "adjust_rate: Some time values used in 'x' rate calculations not present in 'by' background data.")
  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with calc_rate 'x', data.frame 'by' inputs.", {
    ## single rate value, single concurrent blank
    rate <- cr_obj_single
    by <- bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, single concurrent blank
    rate <- cr_obj_three
    by <- bg_df2col
    o_rate <- rate$rate
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, single concurrent blank
    rate <- cr_obj_eight
    by <- bg_df2col
    o_rate <- rate$rate
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## single rate value, two concurrent blanks
    rate <- cr_obj_single
    by <- bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, two concurrent blanks
    rate <- cr_obj_three
    by <- bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, two concurrent blanks
    rate <- cr_obj_eight
    by <- bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))


    ## single rate value, seven concurrent blanks
    rate <- cr_obj_single
    by <- bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, seven concurrent blanks
    rate <- cr_obj_three
    by <- bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, seven concurrent blanks
    rate <- cr_obj_eight
    by <- bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))
  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with calc_rate 'x', inspect 'by' inputs.", {
    ## single rate value, single concurrent blank
    rate <- cr_obj_single
    by <- insp_bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, single concurrent blank
    rate <- cr_obj_three
    by <- insp_bg_df2col
    o_rate <- rate$rate
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, single concurrent blank
    rate <- cr_obj_eight
    by <- insp_bg_df2col
    o_rate <- rate$rate
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## single rate value, two concurrent blanks
    rate <- cr_obj_single
    by <- insp_bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, two concurrent blanks
    rate <- cr_obj_three
    by <- insp_bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, two concurrent blanks
    rate <- cr_obj_eight
    by <- insp_bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))


    ## single rate value, seven concurrent blanks
    rate <- cr_obj_single
    by <- insp_bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, seven concurrent blanks
    rate <- cr_obj_three
    by <- insp_bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, seven concurrent blanks
    rate <- cr_obj_eight
    by <- insp_bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))
  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with calc_rate 'x', calc_rate.bg 'by' inputs.", {
    ## single rate value, single concurrent blank
    rate <- cr_obj_single
    by <- crbg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by$dataframe, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, single concurrent blank
    rate <- cr_obj_three
    by <- crbg_df2col
    o_rate <- rate$rate
    adj <- calc_rate(by$dataframe, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, single concurrent blank
    rate <- cr_obj_eight
    by <- crbg_df2col
    o_rate <- rate$rate
    adj <- calc_rate(by$dataframe, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## single rate value, two concurrent blanks
    rate <- cr_obj_single
    by <- crbg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, two concurrent blanks
    rate <- cr_obj_three
    by <- crbg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, two concurrent blanks
    rate <- cr_obj_eight
    by <- crbg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))


    ## single rate value, seven concurrent blanks
    rate <- cr_obj_single
    by <- crbg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, seven concurrent blanks
    rate <- cr_obj_three
    by <- crbg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, seven concurrent blanks
    rate <- cr_obj_eight
    by <- crbg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))
  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with auto_rate 'x', data.frame 'by' inputs.", {
    ## auto_rate obj, single concurrent blank
    rate <- ar_obj
    by <- bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, single concurrent blank
    rate <- ar_obj_single
    by <- bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with MANY rates, single concurrent blank
    rate <- ar_obj_highest
    by <- bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj, two concurrent blanks
    rate <- ar_obj
    by <- bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, two concurrent blanks
    rate <- ar_obj_single
    by <- bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))


    ## auto_rate obj, seven concurrent blanks
    rate <- ar_obj
    by <- bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, seven concurrent blanks
    rate <- ar_obj_single
    by <- bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))
  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with auto_rate 'x', inspect 'by' inputs.", {
    ## auto_rate obj, single concurrent blank
    rate <- ar_obj
    by <- insp_bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, single concurrent blank
    rate <- ar_obj_single
    by <- insp_bg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))


    ## auto_rate obj, two concurrent blanks
    rate <- ar_obj
    by <- insp_bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## ## auto_rate obj with single rate, two concurrent blanks
    rate <- ar_obj_single
    by <- insp_bg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))


    ## auto_rate obj, seven concurrent blanks
    rate <- ar_obj
    by <- insp_bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, seven concurrent blanks
    rate <- ar_obj_single
    by <- insp_bg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with auto_rate 'x', calc_rate.bg 'by' inputs.", {
    ## auto_rate obj, single concurrent blank
    rate <- ar_obj
    by <- crbg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by$dataframe, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, single concurrent blank
    rate <- ar_obj_single
    by <- crbg_df2col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(by$dataframe, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj, two concurrent blanks
    rate <- ar_obj
    by <- crbg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## ## auto_rate obj with single rate, two concurrent blanks
    rate <- ar_obj_single
    by <- crbg_df3col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj, seven concurrent blanks
    rate <- ar_obj
    by <- crbg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## auto_rate obj with single rate, seven concurrent blanks
    rate <- ar_obj_single
    by <- crbg_df7col
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,3)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj3 <- calc_rate(by$dataframe[,c(1,4)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj4 <- calc_rate(by$dataframe[,c(1,5)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj5 <- calc_rate(by$dataframe[,c(1,6)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj6 <- calc_rate(by$dataframe[,c(1,7)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj7 <- calc_rate(by$dataframe[,c(1,8)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2+adj3+adj4+adj5+adj6+adj7)/7

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))
  })

  test_that("adjust_rate: method = 'concurrent' - outputs correct results with calc_rate 'x', calc_rate 'by' inputs.", {
    ## single rate value, single concurrent blank
    rate <- cr_obj_single
    by <- calc_rate(insp_bg_df2col, plot = F)
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj <- calc_rate(insp_bg_df2col, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, single concurrent blank
    rate <- cr_obj_three
    by <- calc_rate(insp_bg_df2col, plot = F)
    o_rate <- rate$rate
    adj <- calc_rate(insp_bg_df2col, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, single concurrent blank
    rate <- cr_obj_eight
    by <- calc_rate(insp_bg_df2col)
    o_rate <- rate$rate
    adj <- calc_rate(insp_bg_df2col, from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## single rate value, two concurrent blanks
    rate <- cr_obj_single
    by <- suppressWarnings(calc_rate(insp_bg_df3col, plot = F))
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(insp_bg_df3col$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(insp_bg_df3col$dataframe[,c(1,2)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## three rate values, two concurrent blanks
    rate <- cr_obj_three
    by <- suppressWarnings(calc_rate(insp_bg_df3col, plot = F))
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(insp_bg_df3col$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(insp_bg_df3col$dataframe[,c(1,2)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))

    ## eight rate values, two concurrent blanks
    rate <- cr_obj_eight
    by <- suppressWarnings(calc_rate(insp_bg_df3col, plot = F))
    # rate should be this
    o_rate <- rate$rate
    # adjustment should be this
    adj1 <- calc_rate(by$dataframe[,1:2], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj2 <- calc_rate(by$dataframe[,c(1,2)], from = rate$summary$time, to = rate$summary$endtime, by = "time", plot = F)$rate
    adj <- (adj1+adj2)/2

    expect_equal(adjust_rate(rate, method = "concurrent", by = by)$rate.adjusted,
                 o_rate - (adj))
  })


  # method = "linear" -------------------------------------------------------

  test_that("adjust_rate: method = 'linear' - accepts calc_rate objects as 'by'", {
    expect_error(suppressWarnings(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = cr_obj_single, time_by = NULL,
                by2 = -0.003, time_by2 = 40)),
                NA)
  })

  test_that("adjust_rate: method = 'linear' - correct message", {
    # if method = "mean" specified
    expect_message(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: Rate adjustments applied using \"linear\" method.",
                   fixed = TRUE)
  })

  test_that("adjust_rate: method = 'linear' - stops if 'x' is not numeric, calc_rate, or auto_rate", {
    expect_error(adjust_rate(x = NULL, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(x = "text", time_x = 20, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(x = crbg_df2col, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(x = urchins.rd, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'x' is numeric, and 'time_x' not also numeric, not entered or is unequal length", {
    expect_error(adjust_rate(x = -0.03, time_x = NULL, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = c(20,21), method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = "text", method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = bg_df2col, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = crbg_df2col, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'x' is calc_rate or auto_rate, and 'time_x' is not NULL", {
    expect_error(adjust_rate(x = cr_obj_single, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
    expect_error(adjust_rate(x = cr_obj_single, time_x = bg_df2col, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
    expect_error(adjust_rate(x = ar_obj, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
    expect_error(adjust_rate(x = ar_obj, time_x = bg_df2col, method = "linear", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'by' is not a single numeric value, or data.frame, inspect or calc_rate.bg", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = NULL, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = c(-0.001,-0.002), time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = "text", time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = ar_obj, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'by' is a single numeric value, and 'time_by' is not entered or not also a single numeric", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = NULL,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = crbg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'by' is dataframe, calc_rate.bg, or inspect, and 'time_by' is not NULL", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = bg_df2col, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = bg_df2col, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = bg_df2col, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = bg_df2col, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = bg_df2col, time_by = crbg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = crbg_df2col, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = crbg_df2col, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = crbg_df2col, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = crbg_df2col, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = crbg_df2col, time_by = crbg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = insp_bg_df2col, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = insp_bg_df2col, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = insp_bg_df2col, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = insp_bg_df2col, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = insp_bg_df2col, time_by = insp_bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'by2' is not a single numeric value, or data.frame, inspect or calc_rate.bg", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = NULL, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = c(-0.001,-0.002), time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = "text", time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = ar_obj, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'by2' is a single numeric value, and 'time_by2' is not entered or not also a single numeric", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = -0.002, time_by2 = NULL,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = -0.002, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = -0.002, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = -0.002, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = -0.002, time_by2 = crbg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'by2' is dataframe, calc_rate.bg, or inspect, and 'time_by2' is not NULL", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = bg_df2col, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = bg_df2col, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = bg_df2col, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = bg_df2col, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = bg_df2col, time_by2 = crbg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = crbg_df2col, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = crbg_df2col, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = crbg_df2col, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = crbg_df2col, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = crbg_df2col, time_by2 = crbg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = insp_bg_df2col, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = insp_bg_df2col, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = insp_bg_df2col, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = insp_bg_df2col, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by2 = insp_bg_df2col, time_by2 = insp_bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"linear\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
  })

  test_that("adjust_rate: method = 'linear' - stops if 'time_by' and 'time_by2' are wrong way round (either entered or extracted from objects)", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = 40,
                             by2 = -0.003, time_by2 = 0),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = crbg_post_2col, time_by = NULL,
                             by2 = crbg_pre_2col, time_by2 = NULL),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = insp_post_2col, time_by = NULL,
                             by2 = insp_pre_2col, time_by2 = NULL),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = bgdf_post_2col, time_by = NULL,
                             by2 = bgdf_pre_2col, time_by2 = NULL),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
  })

  test_that("adjust_rate: method = 'linear' - warns if any timestamps of 'x' rates are outside (i.e. before/after) the time_by' and 'time_by2' time range", {
    # one rate outside
    expect_warning(adjust_rate(x = -0.03, time_x = 10, method = "linear", by = -0.002, time_by = 20,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
    # two rates, both outside
    expect_warning(adjust_rate(x = c(-0.03, -0.02), time_x = c(10,15), method = "linear", by = -0.002, time_by = 20,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
    # two rates, one outside
    expect_warning(adjust_rate(x = c(-0.03, -0.02), time_x = c(10,25), method = "linear", by = -0.002, time_by = 20,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
    # using input objects
    expect_warning(adjust_rate(x = cr_pre, time_x = NULL, method = "linear", by = spec_insp, time_by = NULL,
                               by2 = insp_post_2col, time_by2 = NULL),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
  })

  test_that("adjust_rate: method = 'linear' - warns if 'by' and 'by2' rate values have opposite sign", {
    expect_warning(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.002, time_by = 0,
                               by2 = 0.003, time_by2 = 40),
                   "adjust_rate: background rates in 'by' and 'by2' differ in sign \\(i.e. one is \\+ve, one is \\-ve\\).")
    expect_warning(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = 0.002, time_by = 0,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: background rates in 'by' and 'by2' differ in sign \\(i.e. one is \\+ve, one is \\-ve\\).")
    expect_warning(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = 0.002, time_by = 0,
                               by2 = crbg_post_2col, time_by2 = NULL),
                   "adjust_rate: background rates in 'by' and 'by2' differ in sign \\(i.e. one is \\+ve, one is \\-ve\\).")
    ## test the warning ISN'T SHOWN when either (or both) are zero
    expect_warning(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = 0, time_by = 0,
                               by2 = -0.003, time_by2 = 40),
                   NA)
    expect_warning(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = -0.003, time_by = 0,
                               by2 = 0, time_by2 = 40),
                   NA)
    expect_warning(adjust_rate(x = -0.03, time_x = 20, method = "linear", by = 0, time_by = 0,
                               by2 = 0, time_by2 = 40),
                   NA)
  })

  ## These tests basically run EVERY combination of EVERY acceptable input for 'x', 'by' and 'by2'
  ## and check they output the correct adjusted rate

  ## names of inputs - for creating assertion and therefore reporting which combinations fail
  x_in <- c("c(-0.030)", "c(0.030)", "c(0)",
            "c(-0.030, -0.029, -0.028, -0.027, -0.026)", "c(0.030, 0.029, 0.028, 0.027, 0.026)",
            "c(-0.030, -0.029, 0.028, 0.027, 0.026)",
            "cr_obj_single", "cr_obj_three", "cr_obj_eight", "cr_obj_pos", "cr_obj_mixed_sign",
            "ar_obj", "ar_obj_single", "ar_obj_pos", "ar_obj_mixed_sign",
            "ar_obj_highest", "ar_obj_lowest", "ar_obj_interval")

  by_in <- c("c(-0.001)", "c(0.001)", "c(-0.003)", "c(0.003)",
             "bgdf_pre_2col", "bgdf_pre_3col",
             "insp_pre_2col", "insp_pre_3col",
             "crbg_pre_2col", "crbg_pre_3col")

  by2_in <- c("c(-0.001)", "c(0.001)", "c(-0.003)", "c(0.003)",
              "bgdf_post_2col", "bgdf_post_3col",
              "insp_post_2col", "insp_post_3col",
              "crbg_post_2col", "crbg_post_3col")

  ## matrix of all combinations of above
  name_mat <- expand.grid(x_in,
                          by_in,
                          by2_in, stringsAsFactors = FALSE)

  ## list of lists of ALL POSSIBLE inputs
  all_objs <- list(x_in = list(c(-0.030), c(0.030), c(0),
                               c(-0.030, -0.029, -0.028, -0.027, -0.026), c(0.030, 0.029, 0.028, 0.027, 0.026),
                               c(-0.030, -0.029, 0.028, 0.027, 0.026),
                               cr_obj_single, cr_obj_three, cr_obj_eight, cr_obj_pos, cr_obj_mixed_sign,
                               ar_obj, ar_obj_single, ar_obj_pos, ar_obj_mixed_sign,
                               ar_obj_highest, ar_obj_lowest, ar_obj_interval),
                   time_x_in = list(c(20), c(20), c(20), # time inputs need to be paired with above appropriately
                                    c(20,25,30,35,40), c(20,25,30,35,40),
                                    c(20,25,30,35,40),
                                    NULL, NULL, NULL, NULL, NULL,
                                    NULL, NULL, NULL, NULL,
                                    NULL, NULL, NULL),
                   by_in = list(c(-0.001), c(0.001), c(-0.003), c(0.003),
                                bgdf_pre_2col, bgdf_pre_3col,
                                insp_pre_2col, insp_pre_3col,
                                crbg_pre_2col, crbg_pre_3col),
                   time_by_in = list(c(5), c(5), c(0), c(1),
                                     NULL, NULL,
                                     NULL, NULL,
                                     NULL, NULL),
                   by2_in = list(c(-0.001), c(0.001), c(-0.003), c(0.003),
                                 bgdf_post_2col, bgdf_post_3col,
                                 insp_post_2col, insp_post_3col,
                                 crbg_post_2col, crbg_post_3col),
                   time_by2_in = list(c(40), c(40), c(45), c(40),
                                      NULL, NULL,
                                      NULL, NULL,
                                      NULL, NULL))

  ## numeric matrix of inputs for choosing inputs on each loop
  num_mat <- expand.grid(1:length(x_in),
                         1:length(by_in),
                         1:length(by2_in))
  ## column of row/iteration numbers - used to build assertion
  num_mat[[4]] <- 1:nrow(num_mat)


  ## test every combination
  apply(num_mat, 1, function(z) {

    method <- "linear"

    ## select x, by, by2 inputs
    x <- all_objs$x_in[[z[[1]]]]
    by <- all_objs$by_in[[z[[2]]]]
    by2 <- all_objs$by2_in[[z[[3]]]]

    ## associated timestamps
    time_x <- all_objs$time_x_in[[z[[1]]]]
    time_by <- all_objs$time_by_in[[z[[2]]]]
    time_by2 <- all_objs$time_by2_in[[z[[3]]]]

    ## Calculate what the adjusted rates SHOULD be.
    ## We calculate these the same way, but outside the adjust_rate function
    ## as much as possible.

    # x rate should be this
    # extract based on input type
    if(is.numeric(x)) o_x <- x else
      o_x <- x$rate
    if(is.numeric(time_x)) o_time_x <- time_x else
      o_time_x <- (x$summary$endtime + x$summary$time)/2 ## CHANGE

    # adjustment should be this
    # extract rate and timestamp based on input type
    if(is.numeric(by)) {
      o_by <- by
      o_time_by <- time_by
    } else if(is.data.frame(by)) {
      o_by <- mean(suppressMessages(calc_rate.bg(as.data.frame(by), plot = F))$rate.bg)
      o_time_by <- sum(range(by[[1]]))/2
    } else {
      o_by <- mean(suppressMessages(calc_rate.bg(as.data.frame(by$dataframe), plot = F))$rate.bg)
      o_time_by <- sum(range(by$dataframe[[1]]))/2
    }
    if(is.numeric(by2)) {
      o_by2 <- by2
      o_time_by2 <- time_by2
    } else if(is.data.frame(by2)) {
      o_by2 <- mean(suppressMessages(calc_rate.bg(as.data.frame(by2), plot = F))$rate.bg)
      o_time_by2 <- sum(range(by2[[1]]))/2
    } else {
      o_by2 <- mean(suppressMessages(calc_rate.bg(as.data.frame(by2$dataframe), plot = F))$rate.bg)
      o_time_by2 <- sum(range(by2$dataframe[[1]]))/2
    }

    ## calc adjustment
    lm <- lm(c(o_by, o_by2) ~ c(o_time_by, o_time_by2)) # adjustment model
    o_adj <- as.numeric(o_time_x * lm$coef[2] + lm$coef[1]) # actual adjustment value for each x rate timestamp

    ## build assertion so we know which test fails
    assertion <- glue::glue("adjust_rate: method = 'linear' outputs correct results - combination x = {name_mat[z[[4]],][[1]]}, by = {name_mat[z[[4]],][[2]]}, by2 = {name_mat[z[[4]],][[3]]}}")

    skip("skip - until I figure out how to skip these on R CMD CHECK")
    test_that(assertion,{
      skip_on_cran()
      expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                                by = by, time_by = time_by,
                                                by2 = by2, time_by2 = time_by2))$rate.adjusted,
                   o_x - o_adj)
    })

  })

  ## Just to absolutely double check - these are tests of exact values we KNOW are correct

  test_that("adjust_rate: method = 'linear' - outputs KNOWN correct results.", {
    ## all negative
    method <- "linear"
    x <- -100
    time_x <- 50
    by <- 0
    time_by <- 0
    by2 <- -1
    time_by2 <- 100

    expect_equal(adjust_rate(x = x, time_x = time_x, method = method,
                             by = by, time_by = time_by,
                             by2 = by2, time_by2 = time_by2)$rate.adjusted,
                 -99.5)
    ## all positive
    method <- "linear"
    x <- 100
    time_x <- 50
    by <- 0
    time_by <- 0
    by2 <- 1
    time_by2 <- 100

    expect_equal(adjust_rate(x = x, time_x = time_x, method = method,
                             by = by, time_by = time_by,
                             by2 = by2, time_by2 = time_by2)$rate.adjusted,
                 99.5)

    method <- "linear"
    x <- 100
    time_x <- 20
    by <- 0
    time_by <- 0
    by2 <- 1
    time_by2 <- 100

    expect_equal(adjust_rate(x = x, time_x = time_x, method = method,
                             by = by, time_by = time_by,
                             by2 = by2, time_by2 = time_by2)$rate.adjusted,
                 99.8)

    method <- "linear"
    x <- 100
    time_x <- 50
    by <- -1
    time_by <- 0
    by2 <- 1
    time_by2 <- 100

    expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                              by = by, time_by = time_by,
                                              by2 = by2, time_by2 = time_by2))$rate.adjusted,
                 100)

    method <- "linear"
    x <- -100
    time_x <- 50
    by <- -1
    time_by <- 0
    by2 <- 1
    time_by2 <- 100

    expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                              by = by, time_by = time_by,
                                              by2 = by2, time_by2 = time_by2))$rate.adjusted,
                 -100)
    ## numeric single inputs
    expect_error(adjust_rate(x = -100, by = 0, by2 = -50, time_x = 50,
                             time_by = 0, time_by2 = 100, method = "linear"),
                 regexp = NA)
    expect_equal(adjust_rate(x = -100, by = 0, by2 = -50, time_x = 50,
                             time_by = 0, time_by2 = 100, method = "linear")$rate.adjusted,
                 -75)
    ## with calc_rate.bg for by, by2 and both
    bg_obj1 <- suppressMessages(calc_rate.bg(urchins.rd[1:10,], 1, 18, plot = F))
    bg_obj2 <- suppressMessages(calc_rate.bg(urchins.rd[250:271,], 1, 18, plot = F))
    ##  by
    expect_error(adjust_rate(x = -0.033, by = bg_obj1, by2 = -0.007030772, time_x = 15,
                             time_by = NULL, time_by2 = 43.25, method = "linear"),
                 regexp = NA)
    expect_equal(adjust_rate(x = -0.033, by = bg_obj1, by2 = -0.007030772, time_x = 15,
                             time_by = NULL, time_by2 = 43.25, method = "linear")$adjustment,
                 -0.005878997)
    ##  by2
    expect_error(adjust_rate(x = -0.033, by = -0.005298013, by2 = bg_obj2, time_x = 15,
                             time_by = 0.75, time_by2 = NULL, method = "linear"),
                 regexp = NA)
    expect_equal(adjust_rate(x = -0.033, by = -0.005298013, by2 = bg_obj2, time_x = 15,
                             time_by = 0.75, time_by2 = NULL, method = "linear")$adjustment,
                 -0.005878997)
    ##  both
    expect_error(adjust_rate(x = -0.033, by = bg_obj1, by2 = bg_obj2, time_x = 15,
                             time_by = NULL, time_by2 = NULL, method = "linear"),
                 regexp = NA)
    expect_equal(adjust_rate(x = -0.033, by = bg_obj1, by2 = bg_obj2, time_x = 15,
                             time_by = NULL, time_by2 = NULL, method = "linear")$adjustment,
                 -0.005878997)


    ## numeric vector inputs
    expect_error(adjust_rate(x = -c(100,110,120), by = 0, by2 = -50, time_x = c(50,60,70),
                             time_by = 0, time_by2 = 100, method = "linear"),
                 regexp = NA)
    expect_equal(adjust_rate(x = -c(100,110,120), by = 0, by2 = -50, time_x = c(50,60,70),
                             time_by = 0, time_by2 = 100, method = "linear")$rate.adjusted,
                 c(-75, -80, -85))

    ## calc_rate input - one rate
    cr_obj <- suppressWarnings(calc_rate(urchins.rd, plot = F)) ## whole dataset
    ## accepts calc_rate object
    expect_error(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 100, method = "linear"),
                 regexp = NA)
    ## should apply exactly half of the by2 correction if over same time period
    expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 45, method = "linear")$adjustment,
                 -0.01/2)
    expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 45, method = "linear")$rate.adjusted,
                 -0.02282768)

    ## calc_rate input - multiples rates
    cr_obj <- suppressWarnings(calc_rate(urchins.rd, from = c(0,10,20,30), to = c(10,20,30,40), plot = F))
    ## accepts calc_rate object
    expect_error(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 100, method = "linear"),
                 regexp = NA)
    ## should apply a ninth to seven ninths of the by2 correction if over same time period
    expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 45, method = "linear")$adjustment,
                 c(-0.01*1/9, -0.01*3/9, -0.01*5/9, -0.01*7/9))
    expect_equal(adjust_rate(x = cr_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 45, method = "linear")$rate.adjusted,
                 c(-0.03178548, -0.02555831, -0.02166402, -0.01793542))

    ## auto_rate input
    ar_obj <- suppressWarnings(auto_rate(urchins.rd, plot = F))
    ## accepts auto_rate object
    expect_error(adjust_rate(x = ar_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 100, method = "linear"),
                 regexp = NA)
    ## values
    expect_equal(adjust_rate(x = ar_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 45, method = "linear")$adjustment,
                 c(-0.003477778, -0.007422222, -0.001066667, -0.001222222))
    expect_equal(adjust_rate(x = ar_obj, by = 0, by2 = -0.01,
                             time_by = 0, time_by2 = 45, method = "linear")$rate.adjusted,
                 c(-0.02578789, -0.01791415, -0.03132537, -0.03072295))

  })


  # method = "exponential" --------------------------------------------------

  test_that("adjust_rate: method = 'exponential' - correct message", {
    # if method = "mean" specified
    expect_message(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: Rate adjustments applied using \"exponential\" method.",
                   fixed = TRUE)
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'x' is not numeric, calc_rate, or auto_rate", {
    expect_error(adjust_rate(x = NULL, time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(x = "text", time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(x = crbg_df2col, time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
    expect_error(adjust_rate(x = urchins.rd, time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'x' input must be a numeric value or vector, or an object of class 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int'.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'x' is numeric, and 'time_x' not also numeric, not entered or is unequal length", {
    expect_error(adjust_rate(x = -0.03, time_x = NULL, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = c(20,21), method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = "text", method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = bg_df2col, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
    expect_error(adjust_rate(x = -0.03, time_x = crbg_df2col, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'x' input, the 'time_x' must be a numeric input of the same length \\(i.e. timestamp\\(s\\) for all rates in 'x'\\).")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'x' is calc_rate or auto_rate, and 'time_x' is not NULL", {
    expect_error(adjust_rate(x = cr_obj_single, time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
    expect_error(adjust_rate(x = cr_obj_single, time_x = bg_df2col, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
    expect_error(adjust_rate(x = ar_obj, time_x = 20, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
    expect_error(adjust_rate(x = ar_obj, time_x = bg_df2col, method = "exponential", by = -0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a 'calc_rate', 'calc_rate.int', 'auto_rate', or 'auto_rate.int' 'x' input, the 'time_x' input must be NULL.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by' is not a single numeric value, or data.frame, inspect or calc_rate.bg", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = NULL, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = c(-0.001,-0.002), time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = "text", time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = ar_obj, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", the 'by' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' object containing background time~oxygen data.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by' is a single numeric value, and 'time_by' is not entered or not also a single numeric", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = NULL,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = crbg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by' input, the 'time_by' input also requires a single numeric value \\(i.e. a timestamp for 'by' background rate\\).")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by' is dataframe, calc_rate.bg, or inspect, and 'time_by' is not NULL", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = bg_df2col, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = bg_df2col, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = bg_df2col, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = bg_df2col, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = bg_df2col, time_by = crbg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = crbg_df2col, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = crbg_df2col, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = crbg_df2col, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = crbg_df2col, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = crbg_df2col, time_by = crbg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = insp_bg_df2col, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = insp_bg_df2col, time_by = "text",
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = insp_bg_df2col, time_by = c(1,2),
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = insp_bg_df2col, time_by = bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = insp_bg_df2col, time_by = insp_bg_df2col,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by' input, the 'time_by' must be NULL.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by2' is not a single numeric value, or data.frame, inspect or calc_rate.bg", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = NULL, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = c(-0.001,-0.002), time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = "text", time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = ar_obj, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", the 'by2' input must be a single numeric value, or a 'data.frame', 'inspect', 'calc_rate.bg', or `calc_rate` object containing background time~oxygen data.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by2' is a single numeric value, and 'time_by2' is not entered or not also a single numeric", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = -0.002, time_by2 = NULL,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = -0.002, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = -0.002, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = -0.002, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = -0.002, time_by2 = crbg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\" and a numeric 'by2' input, the 'time_by2' input also requires a single numeric value \\(i.e. a timestamp for 'by2' background rate\\).")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by2' is dataframe, calc_rate.bg, or inspect, and 'time_by2' is not NULL", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = bg_df2col, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = bg_df2col, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = bg_df2col, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = bg_df2col, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = bg_df2col, time_by2 = crbg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = crbg_df2col, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = crbg_df2col, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = crbg_df2col, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = crbg_df2col, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = crbg_df2col, time_by2 = crbg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = insp_bg_df2col, time_by2 = 0,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = insp_bg_df2col, time_by2 = "text",
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = insp_bg_df2col, time_by2 = c(1,2),
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = insp_bg_df2col, time_by2 = bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by2 = insp_bg_df2col, time_by2 = insp_bg_df2col,
                             by = -0.003, time_by = 40),
                 "adjust_rate: For method = \"exponential\", and a 'data.frame', 'inspect', 'calc_rate.bg', or 'calc_rate' 'by2' input, the 'time_by2' must be NULL.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'time_by' and 'time_by2' are wrong way round (either entered or extracted from objects)", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = -0.002, time_by = 40,
                             by2 = -0.003, time_by2 = 0),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = crbg_post_2col, time_by = NULL,
                             by2 = crbg_pre_2col, time_by2 = NULL),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = insp_post_2col, time_by = NULL,
                             by2 = insp_pre_2col, time_by2 = NULL),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = bgdf_post_2col, time_by = NULL,
                             by2 = bgdf_pre_2col, time_by2 = NULL),
                 "adjust_rate: Error in inputs. Timestamp for 'by2' is before timestamp for 'by' suggesting they are in the wrong order or come from different datasets.")
  })

  test_that("adjust_rate: method = 'exponential' - warns if any timestamps of 'x' rates are outside (i.e. before/after) the time_by' and 'time_by2' time range", {
    # one rate outside
    expect_warning(adjust_rate(x = -0.03, time_x = 10, method = "exponential", by = -0.002, time_by = 20,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
    # two rates, both outside
    expect_warning(adjust_rate(x = c(-0.03, -0.02), time_x = c(10,15), method = "exponential", by = -0.002, time_by = 20,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
    # two rates, one outside
    expect_warning(adjust_rate(x = c(-0.03, -0.02), time_x = c(10,25), method = "exponential", by = -0.002, time_by = 20,
                               by2 = -0.003, time_by2 = 40),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
    # using input objects
    expect_warning(adjust_rate(x = cr_pre, time_x = NULL, method = "exponential", by = spec_insp, time_by = NULL,
                               by2 = insp_post_2col, time_by2 = NULL),
                   "adjust_rate: One or more of the timestamps for the rate\\(s\\) in 'x' do not lie between the timestamps for the 'by' and 'by2' background rates.")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by' and 'by2' rate values have opposite sign", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential",
                             by = -0.002, time_by = 0,
                             by2 = 0.003, time_by2 = 40),
                 "adjust_rate: method = \"exponential\" cannot be used when 'by' and 'by2' background rates are not the same sign \\(i.e. one is \\+ve, one is \\-ve\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = 0.002, time_by = 0,
                             by2 = -0.003, time_by2 = 40),
                 "adjust_rate: method = \"exponential\" cannot be used when 'by' and 'by2' background rates are not the same sign \\(i.e. one is \\+ve, one is \\-ve\\).")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = 0.002, time_by = 0,
                             by2 = crbg_post_2col, time_by2 = NULL),
                 "adjust_rate: method = \"exponential\" cannot be used when 'by' and 'by2' background rates are not the same sign \\(i.e. one is \\+ve, one is \\-ve\\).")
  })

  test_that("adjust_rate: method = 'exponential' - stops if 'by' or 'by2' rate values are zero", {
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = 0, time_by = 0,
                             by2 = 0.003, time_by2 = 40),
                 "adjust_rate: method = \"exponential\" cannot be used when a 'by' or 'by2' background rate is zero.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = 0.002, time_by = 0,
                             by2 = 0, time_by2 = 40),
                 "adjust_rate: method = \"exponential\" cannot be used when a 'by' or 'by2' background rate is zero.")
    expect_error(adjust_rate(x = -0.03, time_x = 20, method = "exponential", by = 0, time_by = 0,
                             by2 = 0, time_by2 = 40),
                 "adjust_rate: method = \"exponential\" cannot be used when a 'by' or 'by2' background rate is zero.")
  })

  ## These tests basically run EVERY combination of EVERY acceptable input for 'x', 'by' and 'by2'
  ## and check they output the correct adjusted rate

  ## for exponential - can't mix signs of by and by2 so we run it twice = all neg, all pos

  #  Negative bg rates ------------------------------------------------------

  ## names of inputs - for creating assertion and therefore reporting which combinations fail
  x_in <- c("c(-0.030)", "c(0.030)", "c(0)",
            "c(-0.030, -0.029, -0.028, -0.027, -0.026)", "c(0.030, 0.029, 0.028, 0.027, 0.026)",
            "c(-0.030, -0.029, 0.028, 0.027, 0.026)",
            "cr_obj_single", "cr_obj_three", "cr_obj_eight", "cr_obj_pos", "cr_obj_mixed_sign",
            "ar_obj", "ar_obj_single", "ar_obj_pos", "ar_obj_mixed_sign",
            "ar_obj_highest", "ar_obj_lowest", "ar_obj_interval")

  by_in <- c("c(-0.001)", "c(-0.001)", "c(-0.003)", "c(-0.003)",
             "bgdf_pre_2col", "bgdf_pre_3col",
             "insp_pre_2col", "insp_pre_3col",
             "crbg_pre_2col", "crbg_pre_3col")

  by2_in <- c("c(-0.001)", "c(-0.001)", "c(-0.003)", "c(-0.003)",
              "bgdf_post_2col", "bgdf_post_3col",
              "insp_post_2col", "insp_post_3col",
              "crbg_post_2col", "crbg_post_3col")

  ## matrix of all combinations of above
  name_mat <- expand.grid(x_in,
                          by_in,
                          by2_in, stringsAsFactors = FALSE)

  ## list of lists of ALL POSSIBLE inputs
  all_objs <- list(x_in = list(c(-0.030), c(0.030), c(0),
                               c(-0.030, -0.029, -0.028, -0.027, -0.026), c(0.030, 0.029, 0.028, 0.027, 0.026),
                               c(-0.030, -0.029, 0.028, 0.027, 0.026),
                               cr_obj_single, cr_obj_three, cr_obj_eight, cr_obj_pos, cr_obj_mixed_sign,
                               ar_obj, ar_obj_single, ar_obj_pos, ar_obj_mixed_sign,
                               ar_obj_highest, ar_obj_lowest, ar_obj_interval),
                   time_x_in = list(c(20), c(20), c(20), # time inputs need to be paired with above appropriately
                                    c(20,25,30,35,40), c(20,25,30,35,40),
                                    c(20,25,30,35,40),
                                    NULL, NULL, NULL, NULL, NULL,
                                    NULL, NULL, NULL, NULL,
                                    NULL, NULL, NULL),
                   by_in = list(c(-0.001), c(-0.001), c(-0.003), c(-0.003),
                                bgdf_pre_2col, bgdf_pre_3col,
                                insp_pre_2col, insp_pre_3col,
                                crbg_pre_2col, crbg_pre_3col),
                   time_by_in = list(c(5), c(5), c(0), c(1),
                                     NULL, NULL,
                                     NULL, NULL,
                                     NULL, NULL),
                   by2_in = list(c(-0.001), c(-0.001), c(-0.003), c(-0.003),
                                 bgdf_post_2col, bgdf_post_3col,
                                 insp_post_2col, insp_post_3col,
                                 crbg_post_2col, crbg_post_3col),
                   time_by2_in = list(c(40), c(40), c(45), c(40),
                                      NULL, NULL,
                                      NULL, NULL,
                                      NULL, NULL))

  ## numeric matrix of inputs for choosing inputs on each loop
  num_mat <- expand.grid(1:length(x_in),
                         1:length(by_in),
                         1:length(by2_in))
  ## column of row/iteration numbers - used to build assertion
  num_mat[[4]] <- 1:nrow(num_mat)

  # z<-num_mat[1,]
  ## test every combination
  apply(num_mat, 1, function(z) {

    method <- "exponential"

    ## select x, by, by2 inputs
    x <- all_objs$x_in[[z[[1]]]]
    by <- all_objs$by_in[[z[[2]]]]
    by2 <- all_objs$by2_in[[z[[3]]]]

    ## associated timestamps
    time_x <- all_objs$time_x_in[[z[[1]]]]
    time_by <- all_objs$time_by_in[[z[[2]]]]
    time_by2 <- all_objs$time_by2_in[[z[[3]]]]

    ## Calculate what the adjusted rates SHOULD be.
    ## We calculate these the same way, but outside the adjust_rate function
    ## as much as possible.

    # x rate should be this
    # extract based on input type
    if(is.numeric(x)) o_x <- x else
      o_x <- x$rate
    if(is.numeric(time_x)) o_time_x <- time_x else
      o_time_x <- (x$summary$endtime + x$summary$time)/2 ## CHANGE

    # adjustment should be this
    # extract rate and timestamp based on input type
    if(is.numeric(by)) {
      o_by <- by
      o_time_by <- time_by
    } else if(is.data.frame(by)) {
      o_by <- mean(suppressMessages(calc_rate.bg(as.data.frame(by), plot = F))$rate.bg)
      o_time_by <- sum(range(by[[1]]))/2
    } else {
      o_by <- mean(suppressMessages(calc_rate.bg(as.data.frame(by$dataframe), plot = F))$rate.bg)
      o_time_by <- sum(range(by$dataframe[[1]]))/2
    }
    if(is.numeric(by2)) {
      o_by2 <- by2
      o_time_by2 <- time_by2
    } else if(is.data.frame(by2)) {
      o_by2 <- mean(suppressMessages(calc_rate.bg(as.data.frame(by2), plot = F))$rate.bg)
      o_time_by2 <- sum(range(by2[[1]]))/2
    } else {
      o_by2 <- mean(suppressMessages(calc_rate.bg(as.data.frame(by2$dataframe), plot = F))$rate.bg)
      o_time_by2 <- sum(range(by2$dataframe[[1]]))/2
    }

    ## convert to positive (ONLY FOR NEGATIVE BG RATES)
    ## can't fit exponential to negatives
    o_by <- o_by * -1
    o_by2 <- o_by2 * -1

    ## calc adjustment - EXPONENTIAL
    expm <- lm(log(c(o_by, o_by2)) ~ c(o_time_by, o_time_by2)) # adjustment model

    ## extract slope and intercept
    ## needs to convert back from log
    expm_int <- exp(coef(expm)[1])
    expm_slp <- exp(coef(expm)[2])

    o_adj <- as.numeric(unname(expm_int * expm_slp ^ o_time_x))

    ## convert back to negative (ONLY FOR NEGATIVE BG RATES)
    o_adj <- o_adj * -1

    ## build assertion so we know which test fails
    assertion <- glue::glue("adjust_rate: method = 'exponential' outputs correct results - combination x = {name_mat[z[[4]],][[1]]}, by = {name_mat[z[[4]],][[2]]}, by2 = {name_mat[z[[4]],][[3]]}}")

    skip("skip - until I figure out how to skip these on R CMD CHECK")
    test_that(assertion, {
      skip_on_cran()
      expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                                by = by, time_by = time_by,
                                                by2 = by2, time_by2 = time_by2))$rate.adjusted,
                   o_x - o_adj)
    })

  })




  #  Positive bg rates ------------------------------------------------------

  ## names of inputs - for creating assertion and therefore reporting which combinations fail
  x_in <- c("c(-0.030)", "c(0.030)", "c(0)",
            "c(-0.030, -0.029, -0.028, -0.027, -0.026)", "c(0.030, 0.029, 0.028, 0.027, 0.026)",
            "c(-0.030, -0.029, 0.028, 0.027, 0.026)",
            "cr_obj_single", "cr_obj_three", "cr_obj_eight", "cr_obj_pos", "cr_obj_mixed_sign",
            "ar_obj", "ar_obj_single", "ar_obj_pos", "ar_obj_mixed_sign")

  by_in <- c("c(0.001)", "c(0.001)", "c(0.003)", "c(0.003)",
             "bgdf_pre_2col_pos", "bgdf_pre_3col_pos",
             "insp_pre_2col_pos", "insp_pre_3col_pos",
             "crbg_pre_2col_pos", "crbg_pre_3col_pos")

  by2_in <- c("c(0.001)", "c(0.001)", "c(0.003)", "c(0.003)",
              "bgdf_post_2col_pos", "bgdf_post_3col_pos",
              "insp_post_2col_pos", "insp_post_3col_pos",
              "crbg_post_2col_pos", "crbg_post_3col_pos")

  ## matrix of all combinations of above
  name_mat <- expand.grid(x_in,
                          by_in,
                          by2_in, stringsAsFactors = FALSE)

  ## list of lists of ALL POSSIBLE inputs
  all_objs <- list(x_in = list(c(-0.030), c(0.030), c(0),
                               c(-0.030, -0.029, -0.028, -0.027, -0.026), c(0.030, 0.029, 0.028, 0.027, 0.026),
                               c(-0.030, -0.029, 0.028, 0.027, 0.026),
                               cr_obj_single, cr_obj_three, cr_obj_eight, cr_obj_pos, cr_obj_mixed_sign,
                               ar_obj, ar_obj_single, ar_obj_pos, ar_obj_mixed_sign),
                   time_x_in = list(c(20), c(20), c(20), # time inputs need to be paired with above appropriately
                                    c(20,25,30,35,40), c(20,25,30,35,40),
                                    c(20,25,30,35,40),
                                    NULL, NULL, NULL, NULL, NULL,
                                    NULL, NULL, NULL, NULL),
                   by_in = list(c(0.001), c(0.001), c(0.003), c(0.003),
                                bgdf_pre_2col_pos, bgdf_pre_3col_pos,
                                insp_pre_2col_pos, insp_pre_3col_pos,
                                crbg_pre_2col_pos, crbg_pre_3col_pos),
                   time_by_in = list(c(5), c(5), c(0), c(1),
                                     NULL, NULL,
                                     NULL, NULL,
                                     NULL, NULL),
                   by2_in = list(c(0.001), c(0.001), c(0.003), c(0.003),
                                 bgdf_post_2col_pos, bgdf_post_3col_pos,
                                 insp_post_2col_pos, insp_post_3col_pos,
                                 crbg_post_2col_pos, crbg_post_3col_pos),
                   time_by2_in = list(c(40), c(40), c(45), c(40),
                                      NULL, NULL,
                                      NULL, NULL,
                                      NULL, NULL))

  ## numeric matrix of inputs for choosing inputs on each loop
  num_mat <- expand.grid(1:length(x_in),
                         1:length(by_in),
                         1:length(by2_in))
  ## column of row/iteration numbers - used to build assertion
  num_mat[[4]] <- 1:nrow(num_mat)

  # z<-num_mat[1,]
  ## test every combination
  apply(num_mat, 1, function(z) {

    method <- "exponential"

    ## select x, by, by2 inputs
    x <- all_objs$x_in[[z[[1]]]]
    by <- all_objs$by_in[[z[[2]]]]
    by2 <- all_objs$by2_in[[z[[3]]]]

    ## associated timestamps
    time_x <- all_objs$time_x_in[[z[[1]]]]
    time_by <- all_objs$time_by_in[[z[[2]]]]
    time_by2 <- all_objs$time_by2_in[[z[[3]]]]

    ## Calculate what the adjusted rates SHOULD be.
    ## We calculate these the same way, but outside the adjust_rate function
    ## as much as possible.

    # x rate should be this
    # extract based on input type
    if(is.numeric(x)) o_x <- x else
      o_x <- x$rate
    if(is.numeric(time_x)) o_time_x <- time_x else
      o_time_x <- (x$summary$endtime + x$summary$time)/2 ## CHANGE

    # adjustment should be this
    # extract rate and timestamp based on input type
    if(is.numeric(by)) {
      o_by <- by
      o_time_by <- time_by
    } else if(is.data.frame(by)) {
      o_by <- mean(suppressMessages(calc_rate.bg(as.data.frame(by), plot = F))$rate.bg)
      o_time_by <- sum(range(by[[1]]))/2
    } else {
      o_by <- mean(suppressMessages(calc_rate.bg(as.data.frame(by$dataframe), plot = F))$rate.bg)
      o_time_by <- sum(range(by$dataframe[[1]]))/2
    }
    if(is.numeric(by2)) {
      o_by2 <- by2
      o_time_by2 <- time_by2
    } else if(is.data.frame(by2)) {
      o_by2 <- mean(suppressMessages(calc_rate.bg(as.data.frame(by2), plot = F))$rate.bg)
      o_time_by2 <- sum(range(by2[[1]]))/2
    } else {
      o_by2 <- mean(suppressMessages(calc_rate.bg(as.data.frame(by2$dataframe), plot = F))$rate.bg)
      o_time_by2 <- sum(range(by2$dataframe[[1]]))/2
    }


    ## calc adjustment - EXPONENTIAL
    expm <- lm(log(c(o_by, o_by2)) ~ c(o_time_by, o_time_by2)) # adjustment model

    ## extract slope and intercept
    ## needs to convert back from log
    expm_int <- exp(coef(expm)[1])
    expm_slp <- exp(coef(expm)[2])

    o_adj <- as.numeric(unname(expm_int * expm_slp ^ o_time_x))


    ## build assertion so we know which test fails
    assertion <- glue::glue("adjust_rate: method = 'exponential' outputs correct results - combination x = {name_mat[z[[4]],][[1]]}, by = {name_mat[z[[4]],][[2]]}, by2 = {name_mat[z[[4]],][[3]]}}")

    skip("skip - until I figure out how to skip these on R CMD CHECK")
    test_that(assertion, {
      skip_on_cran()
      expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                                by = by, time_by = time_by,
                                                by2 = by2, time_by2 = time_by2))$rate.adjusted,
                   o_x - o_adj)
    })

  })

  test_that("adjust_rate: method = 'exponential' - outputs KNOWN correct results.", {
    ## These values checked in Excel ¯\_(ツ)_/¯
    ## all negative
    method <- "exponential"
    x <- -100
    time_x <- 50
    by <- -1
    time_by <- 0
    by2 <- -2
    time_by2 <- 100

    expect_equal(adjust_rate(x = x, time_x = time_x, method = method,
                             by = by, time_by = time_by,
                             by2 = by2, time_by2 = time_by2)$rate.adjusted,
                 -98.58579,
                 tolerance = 0.00001)

    ## all positive
    method <- "exponential"
    x <- 89.67
    time_x <- 21.3
    by <- 7.1
    time_by <- 3
    by2 <- 17.8
    time_by2 <- 67

    expect_equal(adjust_rate(x = x, time_x = time_x, method = method,
                             by = by, time_by = time_by,
                             by2 = by2, time_by2 = time_by2)$rate.adjusted,
                 80.43592,
                 tolerance = 0.00001)
  })






  # S3 tests ----------------------------------------------------------------

  test_that("adjust_rate can be printed - if adjustment done to calc_rate or auto_rate object", {
    cr <- suppressWarnings(adjust_rate(calc_rate(sardine.rd, plot = F), 0.001))
    ar <- suppressWarnings(adjust_rate(auto_rate(sardine.rd, plot = F), 0.001))
    expect_output(print(cr))
    expect_output(print(ar))
  })

  test_that("adjust_rate can be printed - if adjustment done to numeric value", {
    nr <- adjust_rate(0.1, 0.001)
    expect_output(print(nr))
  })

  test_that("adjust_rate S3 generics work", {
    cr <- suppressWarnings(adjust_rate(calc_rate(sardine.rd, plot = F), 0.001))
    ar <- suppressWarnings(adjust_rate(auto_rate(sardine.rd, plot = F), 0.001))
    nr <- suppressWarnings(adjust_rate(0.1, 0.001))
    expect_output(summary(cr))
    expect_output(summary(ar))
    expect_output(summary(nr))
    expect_output(print(cr))
    expect_output(print(ar))
    expect_output(print(nr))
    expect_output(suppressWarnings(mean(cr)))
    expect_output(suppressWarnings(mean(ar)))
    expect_output(suppressWarnings(mean(nr)))
  })

  test_that("adjust_rate applies mean method by default", {
    ## should adjust all by 4
    ar <- adjust_rate(x = c(10,20,30,40,50), by = c(2,3,4,5,6))
    expect_equal(ar$rate.adjusted,
                 c(6,16,26,36,46))
  })

  test_that("adjust_rate - stops with plot()", {
    ar <- adjust_rate(x = c(10,20,30,40,50), by = c(2,3,4,5,6))
    expect_message(plot(ar),
                   regexp = "adjust_rate: plot\\(\\) is not available for 'adjust_rate' objects.")
  })

  # })

}) ## turns printing back on
