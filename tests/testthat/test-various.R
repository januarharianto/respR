# rm(list=ls())
# library(testthat)

# This holds various tests that we want to keep but take too long so are skipped.
# Periodically we can comment out the top skip to check everything works ok.

capture.output({  ## stops printing outputs on assigning

  skip("Extended function tests")

  # adjust_rate -------------------------------------------------------------

  # These don't actually take that long, but caused issues during R CMD CHK when they just crawled
  # to a halt. No idea why. (Haven't actually tested if that still happens).

  ## These tests basically run EVERY combination of EVERY acceptable input for 'x', 'by' and 'by2'
  ## and check they output the correct adjusted rate

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

  # ----------- "linear" method ------------

  { # Create testing objects
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
  }

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

    test_that(assertion,{
      expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                                by = by, time_by = time_by,
                                                by2 = by2, time_by2 = time_by2))$rate.adjusted,
                   o_x - o_adj)
    })

  })


  # "exponential" method ----------------------------------------------------

  ## for exponential - can't mix signs of by and by2 so we run it twice = all neg, all pos

  # Negative bg rates

  { # Create testing objects
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
  }

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

    test_that(assertion, {
      expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                                by = by, time_by = time_by,
                                                by2 = by2, time_by2 = time_by2))$rate.adjusted,
                   o_x - o_adj)
    })

  })

  # Positive bg rates

  { # Create testing objects
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
  }

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

    test_that(assertion, {
      expect_equal(suppressWarnings(adjust_rate(x = x, time_x = time_x, method = method,
                                                by = by, time_by = time_by,
                                                by2 = by2, time_by2 = time_by2))$rate.adjusted,
                   o_x - o_adj)
    })

  })



  # convert_MR --------------------------------------------------------------

  # Rationale of these tests - convert_rate with relevant output units should produce same result
  # as converting various output units

  # create testing objects
  {
    S <- 35
    t <- 12
    P <- 1.01

    # single unconverted rate to convert
    rate <- -1.82

    # multiple unconverted rates to convert
    rates <- c(-1.82, -2.3, -0.56, 5.677, 3.88)

    # Absolute
    in.units <- c("mg/h", "ug.min", "mol s-1", "mmol per hour", "UMOLE/s", "pmol/s-1",
                  "ml/day", "ul.hour", "cm3_h", "mm3 per s", "mgO2 day-1")
    out.units <- c("mg/h", "ug.min", "mol s-1", "mmol per hour", "UMOLE/s", "pmol/s-1",
                   "ml/day", "ul.hour", "cm3_h", "mm3 per s", "mgO2 day-1")

    all_combs_abs <- expand.grid(in.units, out.units, stringsAsFactors = FALSE)

    # Mass-specific
    in.units <- c("mg/h/ug", "ug.min.mg", "mol s-1 g-1", "mmol per hour per kilogram", "UMOLE/s/ug", "pmol/s-1/mg-1",
                  "ml/day/g", "ul/hour/kg", "cm3/h/ug", "mm3 per s per mg", "mgO2/day/KG")
    out.units <- c("mg/h/ug", "ug.min.mg", "mol s-1 g-1", "mmol per hour per kilogram", "UMOLE/s/ug", "pmol/s-1/mg-1",
                   "ml/day/g", "ul/hour/kg", "cm3/h/ug", "mm3 per s per mg", "mgO2/day/KG")
    masses <- round(runif(121, 0.001, 0.5), 2)

    all_combs_ms <- expand.grid(in.units, out.units, stringsAsFactors = FALSE)
    all_combs_ms[[3]] <- masses


    # Area-specific
    in.units <- c("mg/h/mm2", "ug.min.cm^2", "mol s-1 m-2", "mmol per hour per kilometresq", "UMOLE/s/mmsq", "pmol/s-1/cm-2",
                  "ml/day/m2", "ul/hour/km2", "cm3/h/mm2", "mm3 per s per cmsq", "mgO2/day/KMsq")
    out.units <- c("mg/h/mm2", "ug.min.cm^2", "mol s-1 m-2", "mmol per hour per kilometresq", "UMOLE/s/mmsq", "pmol/s-1/cm-2",
                   "ml/day/m2", "ul/hour/km2", "cm3/h/mm2", "mm3 per s per cmsq", "mgO2/day/KMsq")
    areas <- round(runif(121, 0.001, 0.5), 2)

    all_combs_as <- expand.grid(in.units, out.units, stringsAsFactors = FALSE)
    all_combs_as[[3]] <- areas
  }

  # Absolute rate checks ----------------------------------------------------

  test_that("convert_MR - absolute rates from convert_rate objects are converted correctly", {
    apply(all_combs_abs, 1, function(z) {

      res1 <- convert_rate(rate,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           S = S, t = t, P = P)
      res2 <- convert_rate(rate,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - absolute rates from convert_rate objects with multiple rates are converted correctly", {
    apply(all_combs_abs, 1, function(z) {

      res1 <- convert_rate(rates,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           S = S, t = t, P = P)
      res2 <- convert_rate(rates,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - absolute rates from convert_rate.ft objects are converted correctly", {
    apply(all_combs_abs, 1, function(z) {

      res1 <- convert_rate.ft(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[1],
                              S = S, t = t, P = P)

      res2 <- convert_rate.ft(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[2],
                              S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - absolute rates from convert_rate.ft objects with multiple rates are converted correctly", {
    apply(all_combs_abs, 1, function(z) {

      res1 <- convert_rate.ft(rates,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[1],
                              S = S, t = t, P = P)

      res2 <- convert_rate.ft(rates,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[2],
                              S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - absolute rates from single numerics are converted correctly", {
    # 100 random rates
    rates <- round(runif(121, -5, 5), 2)
    all_combs_abs[[3]] <- rates

    apply(all_combs_abs, 1, function(z) {

      res1 <- convert_rate(as.numeric(z[3]),
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           S = S, t = t, P = P)
      res2 <- convert_rate(as.numeric(z[3]),
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1$rate.output,
                         from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3)
    })
  })

  test_that("convert_MR - absolute rates from vector numerics are converted correctly", {
    # 100 random rates
    rates <- round(runif(121, -5, 5), 2)
    all_combs_abs[[3]] <- rates

    # This just tests one conversion from ul/hr to ug/min
    # Just to test it actually outputs a numeric vector
    res1 <- convert_rate(as.numeric(all_combs_abs[[3]]),
                         oxy.unit = "mg/l",
                         time.unit = "min",
                         output.unit = all_combs_abs[18,1],
                         volume = 1.09,
                         S = S, t = t, P = P)
    res2 <- convert_rate(as.numeric(all_combs_abs[[3]]),
                         oxy.unit = "mg/l",
                         time.unit = "min",
                         output.unit = all_combs_abs[18,2],
                         volume = 1.09,
                         S = S, t = t, P = P)
    res3 <- convert_MR(res1$rate.output,
                       from = all_combs_abs[18,1],
                       to = all_combs_abs[18,2],
                       S = S, t = t, P = P)

    #print(z)
    expect_equal(res2$rate.output, res3)
    expect_equal(nrow(all_combs_abs), length(res3))
  })

  # Mass-specific rate checks -----------------------------------------------

  test_that("convert_MR - mass-specific rates from convert_rate objects are converted correctly", {
    apply(all_combs_ms, 1, function(z) {

      res1 <- convert_rate(rate,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           mass = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res2 <- convert_rate(rate,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           mass = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - mass-specific rates from convert_rate objects with multiple rates are converted correctly", {
    apply(all_combs_ms, 1, function(z) {

      res1 <- convert_rate(rates,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           mass = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res2 <- convert_rate(rates,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           mass = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - mass-specific rates from convert_rate.ft objects are converted correctly", {
    apply(all_combs_ms, 1, function(z) {

      res1 <- convert_rate.ft(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[1],
                              mass = as.numeric(z[3]),
                              S = S, t = t, P = P)

      res2 <- convert_rate.ft(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[2],
                              mass = as.numeric(z[3]),
                              S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - mass-specific rates from convert_rate.ft objects with multiple rates are converted correctly", {
    apply(all_combs_ms, 1, function(z) {

      res1 <- convert_rate.ft(rates,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[1],
                              mass = as.numeric(z[3]),
                              S = S, t = t, P = P)

      res2 <- convert_rate.ft(rates,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[2],
                              mass = as.numeric(z[3]),
                              S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - mass-specific rates from single numerics are converted correctly", {
    # 100 random rates
    rates <- round(runif(121, -5, 5), 2)
    all_combs_ms[[4]] <- rates

    apply(all_combs_ms, 1, function(z) {

      res1 <- convert_rate(as.numeric(z[4]),
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           mass = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res2 <- convert_rate(as.numeric(z[4]),
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           mass = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1$rate.output,
                         from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3)
    })
  })

  test_that("convert_MR - mass-specific rates from vector numerics are converted correctly", {
    # 100 random rates
    rates <- round(runif(121, -5, 5), 2)
    all_combs_ms[[4]] <- rates

    # This just tests one conversion from ul/hr to ug/min
    # Just to test it actually outputs a numeric vector
    res1 <- convert_rate(as.numeric(all_combs_ms[[4]]),
                         oxy.unit = "mg/l",
                         time.unit = "min",
                         output.unit = all_combs_ms[18,1],
                         volume = 1.09,
                         mass = as.numeric(all_combs_ms[18,3]),
                         S = S, t = t, P = P)
    res2 <- convert_rate(as.numeric(all_combs_ms[[4]]),
                         oxy.unit = "mg/l",
                         time.unit = "min",
                         output.unit = all_combs_ms[18,2],
                         volume = 1.09,
                         mass = as.numeric(all_combs_ms[18,3]),
                         S = S, t = t, P = P)
    res3 <- convert_MR(res1$rate.output,
                       from = all_combs_ms[18,1],
                       to = all_combs_ms[18,2],
                       S = S, t = t, P = P)

    expect_equal(res2$rate.output, res3)
    expect_equal(nrow(all_combs_ms), length(res3))
  })

  # Area-specific rate checks -----------------------------------------------

  test_that("convert_MR - area-specific rates from convert_rate objects are converted correctly", {
    apply(all_combs_as, 1, function(z) {

      res1 <- convert_rate(rate,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           area = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res2 <- convert_rate(rate,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           area = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - area-specific rates from convert_rate objects with multiple rates are converted correctly", {
    apply(all_combs_as, 1, function(z) {

      res1 <- convert_rate(rates,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           area = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res2 <- convert_rate(rates,
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           area = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - area-specific rates from convert_rate.ft objects are converted correctly", {
    apply(all_combs_as, 1, function(z) {

      res1 <- convert_rate.ft(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[1],
                              area = as.numeric(z[3]),
                              S = S, t = t, P = P)

      res2 <- convert_rate.ft(rate,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[2],
                              area = as.numeric(z[3]),
                              S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - area-specific rates from convert_rate.ft objects with multiple rates are converted correctly", {
    apply(all_combs_as, 1, function(z) {

      res1 <- convert_rate.ft(rates,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[1],
                              area = as.numeric(z[3]),
                              S = S, t = t, P = P)

      res2 <- convert_rate.ft(rates,
                              oxy.unit = "mg/l",
                              time.unit = "min",
                              flowrate.unit = "l/m",
                              output.unit = z[2],
                              area = as.numeric(z[3]),
                              S = S, t = t, P = P)
      res3 <- convert_MR(res1,
                         #from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3$rate.output)
      expect_identical(res2$to, res3$to)
      expect_true(all.equal(res2$summary, res3$summary))
    })
  })

  test_that("convert_MR - area-specific rates from single numerics are converted correctly", {
    # 100 random rates
    rates <- round(runif(121, -5, 5), 2)
    all_combs_as[[4]] <- rates

    apply(all_combs_as, 1, function(z) {

      res1 <- convert_rate(as.numeric(z[4]),
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[1],
                           volume = 1.09,
                           area = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res2 <- convert_rate(as.numeric(z[4]),
                           oxy.unit = "mg/l",
                           time.unit = "min",
                           output.unit = z[2],
                           volume = 1.09,
                           area = as.numeric(z[3]),
                           S = S, t = t, P = P)
      res3 <- convert_MR(res1$rate.output,
                         from = z[1],
                         to = z[2],
                         S = S, t = t, P = P)

      #print(z)
      expect_equal(res2$rate.output, res3)
    })
  })

  test_that("convert_MR - area-specific rates from vector numerics are converted correctly", {
    # 100 random rates
    rates <- round(runif(121, -5, 5), 2)
    all_combs_as[[4]] <- rates

    # This just tests one conversion from ul/hr to ug/min
    # Just to test it actually outputs a numeric vector
    res1 <- convert_rate(as.numeric(all_combs_as[[4]]),
                         oxy.unit = "mg/l",
                         time.unit = "min",
                         output.unit = all_combs_as[18,1],
                         volume = 1.09,
                         area = as.numeric(all_combs_as[18,3]),
                         S = S, t = t, P = P)
    res2 <- convert_rate(as.numeric(all_combs_as[[4]]),
                         oxy.unit = "mg/l",
                         time.unit = "min",
                         output.unit = all_combs_as[18,2],
                         volume = 1.09,
                         area = as.numeric(all_combs_as[18,3]),
                         S = S, t = t, P = P)
    res3 <- convert_MR(res1$rate.output,
                       from = all_combs_as[18,1],
                       to = all_combs_as[18,2],
                       S = S, t = t, P = P)

    expect_equal(res2$rate.output, res3)
    expect_equal(nrow(all_combs_as), length(res3))
  })

# convert.rate.ft ---------------------------------------------------------

  # Extensive output tests
  #
  # These take absolutely FOREVER!

  # This creates a matrix of every combination of input and output values and
  # units, adds the appropriate divisor for the volume unit, and an iteration
  # number. Then expect_equal compares the outputs of cr and crft, prints the
  # inputs so you can see where it stops if it meets an error.

  test_that("convert_rate and convert_rate.ft output same results - huge block of tests", {

    #job::job({
    # Absolute rates ----------------------------------------------------------

    inputs_abs <- list(
      # random rates
      oxy.rates = c(-0.002755, -0.035, -0.88, -5.42, 0.00132, 0.0484, 0.5902, 6.4747),
      # input oxygen units
      oxy.units = c("mg/l", "hPa", "ug/l", "%Air", "mmol/L", "umol/kg", "inHg", "mL/L"),
      # flow units separated
      flow.vol.units = c("ul", "ml", "L"),
      flow.time.units = c("s", "m", "h", "d"),
      # output units
      out.units = c("ug/s", "mg/min", "umol/h", "mmol/day", "mL/min")
    )

    # all combinations
    grid_abs <- expand.grid(inputs_abs, stringsAsFactors = FALSE)
    # create flow units
    grid_abs$flow.units <- paste(grid_abs$flow.vol.units, grid_abs$flow.time.units, sep = "/")
    # add appropriate volume divisor
    grid_abs$vol.div <- apply(grid_abs, 1, function(z) {
      if(z[[3]] == "ul") return(1000000) else
        if(z[[3]] == "ml") return(1000) else
          if(z[[3]] == "L") return(1)
    })
    # add iteration
    grid_abs$iter <- 1:nrow(grid_abs)

    # S t P for units which require them
    S = 30
    t = 15
    P = 1

    test_that("convert_rate and convert_rate.ft output same results - ABSOLUTE RATES", {
      apply(grid_abs, 1, function(z) {
        expect_equal(suppressMessages(convert_rate.ft(as.numeric(z[[1]]),
                                                      oxy.unit = z[[2]],
                                                      flowrate.unit = z[[6]],
                                                      output.unit = z[[5]],
                                                      area = NULL, mass = NULL,
                                                      S = S, t = t, P = P))$rate.output,

                     suppressMessages(convert_rate(as.numeric(z[[1]]),
                                                   oxy.unit = z[[2]],
                                                   time.unit = z[[4]],
                                                   volume = 1/as.numeric(z[[7]]),
                                                   output.unit = z[[5]],
                                                   area = NULL, mass = NULL,
                                                   S = S, t = t, P = P))$rate.output,
                     label = glue::glue("FAILED on row {z[[8]]}"))
        #print(paste(z))
      })
    })

    # Mass-specific rates -----------------------------------------------------
    # not row indexes change because of extra mass/area columns
    inputs_ms <- list(
      # random rates
      oxy.rates = c(-0.002755, -0.035, -0.88, -5.42, 0.00132, 0.0484, 0.5902, 6.4747),
      # input oxygen units
      oxy.units = c("mg/l", "hPa", "ug/l", "%Air", "mmol/L", "umol/kg", "inHg", "mL/L"),
      # flow units separated
      flow.vol.units = c("ul", "ml", "L"),
      flow.time.units = c("s", "m", "h", "d"),
      # output units - mass spec
      out.units = c("ug/s/ug", "mg/min/mg", "umol/h/g", "mmol/day/kg", "mL/min/g"),
      mass = c(0.0034, 0.065, 0.122, 2.78, 87.6) # all in kg
    )

    # all combinations
    grid_ms <- expand.grid(inputs_ms, stringsAsFactors = FALSE)
    # create flow units
    grid_ms$flow.units <- paste(grid_ms$flow.vol.units, grid_ms$flow.time.units, sep = "/")
    # add appropriate volume divisor
    grid_ms$vol.div <- apply(grid_ms, 1, function(z) {
      if(z[[3]] == "ul") return(1000000) else
        if(z[[3]] == "ml") return(1000) else
          if(z[[3]] == "L") return(1)
    })
    # add iteration
    grid_ms$iter <- 1:nrow(grid_ms)

    # S t P for units which require them
    S = 30
    t = 15
    P = 1

    test_that("convert_rate and convert_rate.ft output same results - MASS SPECIFIC", {
      apply(grid_ms, 1, function(z) {
        expect_equal(suppressMessages(convert_rate.ft(as.numeric(z[[1]]),
                                                      oxy.unit = z[[2]],
                                                      flowrate.unit = z[[7]],
                                                      output.unit = z[[5]],
                                                      area = NULL,
                                                      mass = as.numeric(z[[6]]),
                                                      S = S, t = t, P = P))$rate.output,

                     suppressMessages(convert_rate(as.numeric(z[[1]]),
                                                   oxy.unit = z[[2]],
                                                   time.unit = z[[4]],
                                                   volume = 1/as.numeric(z[[8]]),
                                                   output.unit = z[[5]],
                                                   area = NULL,
                                                   mass = as.numeric(z[[6]]),
                                                   S = S, t = t, P = P))$rate.output,
                     label = glue::glue("FAILED on row {z[[9]]}"))
        #print(paste(z))
      })
    })

    # Area-specific rates -----------------------------------------------------

    inputs_as <- list(
      # random rates
      oxy.rates = c(-0.002755, -0.035, -0.88, -5.42, 0.00132, 0.0484, 0.5902, 6.4747),
      # input oxygen units
      oxy.units = c("mg/l", "hPa", "ug/l", "%Air", "mmol/L", "umol/kg", "inHg", "mL/L"),
      # flow units separated
      flow.vol.units = c("ul", "ml", "L"),
      flow.time.units = c("s", "m", "h", "d"),
      # output units - area spec
      out.units = c("ug/s/mm2", "mg/min/cm2", "umol/h/m2", "mmol/day/km2", "mL/min/mm2"),
      area = c(0.0034, 0.065, 0.122, 2.78, 87.6) # all in m2
    )

    # all combinations
    grid_as <- expand.grid(inputs_as, stringsAsFactors = FALSE)
    # create flow units
    grid_as$flow.units <- paste(grid_as$flow.vol.units, grid_as$flow.time.units, sep = "/")
    # add appropriate volume divisor
    grid_as$vol.div <- apply(grid_as, 1, function(z) {
      if(z[[3]] == "ul") return(1000000) else
        if(z[[3]] == "ml") return(1000) else
          if(z[[3]] == "L") return(1)
    })
    # add iteration
    grid_as$iter <- 1:nrow(grid_as)

    # S t P for units which require them
    S = 30
    t = 15
    P = 1

    test_that("convert_rate and convert_rate.ft output same results - AREA SPECIFIC", {
      apply(grid_as, 1, function(z) {
        expect_equal(suppressMessages(convert_rate.ft(as.numeric(z[[1]]),
                                                      oxy.unit = z[[2]],
                                                      flowrate.unit = z[[7]],
                                                      output.unit = z[[5]],
                                                      mass = NULL,
                                                      area = as.numeric(z[[6]]),
                                                      S = S, t = t, P = P))$rate.output,

                     suppressMessages(convert_rate(as.numeric(z[[1]]),
                                                   oxy.unit = z[[2]],
                                                   time.unit = z[[4]],
                                                   volume = 1/as.numeric(z[[8]]),
                                                   output.unit = z[[5]],
                                                   mass = NULL,
                                                   area = as.numeric(z[[6]]),
                                                   S = S, t = t, P = P))$rate.output,
                     label = glue::glue("FAILED on row {z[[9]]}"))
        #print(paste(z))
      })
    })
    #}) #job::job end
  })



# select_rate -------------------------------------------------------------

# These are tests which take a wee bit too long
#
# Create test objects
  {
    # large object
    conv_rt_ar_low_obj <- inspect(sardine.rd) %>%
      auto_rate(method = "lowest", plot = FALSE) %>%
      convert_rate(oxy.unit = "mg/l",
                   time.unit = "min",
                   output.unit = "mg/h/g",
                   volume = 2.379,
                   mass = 0.006955)
  }

  test_that("select_rate: works with method = row_omit and n input of multiple random", {
    skip_on_cran()
    # 10 random rows
    ran <- round(runif(5, 500, 4500))

    # runs ok
    expect_error(conv_rt_ar_low_obj_subset_row_omit <- select_rate(conv_rt_ar_low_obj, method = "row_omit",
                                                                   n = ran),
                 regexp = NA)
    ## check omitted times not within times for each regression
    for(i in ran) apply(conv_rt_ar_low_obj_subset_row_omit$summary, 1, function(x)
      expect_false(i %in% x[7]:x[8]))

  })

  test_that("select_rate: works with method = row_omit and n input of range as both range and vector gives same result", {
    skip_on_cran()
    expect_identical(conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_low_obj, method = "row_omit",
                                                               n = c(2000,2200))$summary,
                     conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_low_obj, method = "row_omit",
                                                               n = 2000:2200)$summary)
  })

  test_that("select_rate: works with method = time_omit and n input of multiple random", {
    skip_on_cran()

    # 10 random times
    ran <- runif(5, 500, 4500)

    # runs ok
    expect_error(conv_rt_ar_low_obj_subset_time_omit <- select_rate(conv_rt_ar_low_obj, method = "time_omit",
                                                                    n = ran),
                 regexp = NA)
    ## check omitted times not within times for each regression
    for(i in ran) apply(conv_rt_ar_low_obj_subset_time_omit$summary, 1, function(x)
      expect_false(i %in% x[9]:x[10]))

  })

  test_that("select_rate: works with method = time_omit and n input of range as both range and vector gives same result", {
    skip_on_cran()
    expect_identical(conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_low_obj, method = "time_omit",
                                                               n = c(1000,1500))$summary,
                     conv_rt_ar_subset_row_omit <- select_rate(conv_rt_ar_low_obj, method = "time_omit",
                                                               n = 1000:1500)$summary)
  })

}) ## end capture.output
