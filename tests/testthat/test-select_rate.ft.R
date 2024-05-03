## library(testthat)
## testthat::test_file("tests/testthat/test-select_rate.R")

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()

  # Create testing objects --------------------------------------------------
  insp.ft_obj <- inspect.ft(flowthrough.rd, delta.oxy = 4, plot = FALSE)
  # object with rolling rate
  crftwidth <- calc_rate.ft(insp.ft_obj,
                            flowrate = 2,
                            by = "row", width = 300, plot = FALSE)
  by_val <- -0.7
  S=30
  t=15
  P=1.01
  adjft.width <- adjust_rate.ft(crftwidth, by = by_val)
  conv.adjft.width <- convert_rate.ft(adjft.width, oxy.unit = "mg/l", flowrate.unit = "L/m",
                                      output.unit = "mg/h",
                                      mass = NULL, area = NULL,
                                      S=S, t=t, P=P)


  test_that("select_rate.ft - runs ok", {
    expect_error(select_rate.ft(conv.adjft.width, method = "rate", n = c(-43.1, -43.2))$summary,
                 NA)
  })

  test_that("select_rate.ft - identical results to select_rate", {
    expect_identical(select_rate.ft(conv.adjft.width, method = "rate", n = c(-43.1, -43.2))$summary,
                     select_rate(conv.adjft.width, method = "rate", n = c(-43.1, -43.2))$summary)
    expect_identical(select_rate.ft(conv.adjft.width, method = "rsq", n = NULL)$summary,
                     select_rate(conv.adjft.width, method = "rsq", n = NULL)$summary)
    expect_identical(select_rate.ft(conv.adjft.width, method = "rank", n = 1:5)$summary,
                     select_rate(conv.adjft.width, method = "rank", n = 1:5)$summary)
  })

  test_that("select_rate.ft - classes correctly applied", {
    srft <- select_rate.ft(conv.adjft.width, method = "rate", n = c(-43.1, -43.2))
    sr <- select_rate(conv.adjft.width, method = "rate", n = c(-43.1, -43.2))

    expect_is(srft,
              "convert_rate.ft")
    expect_is(srft,
              "convert_rate.ft_select")
    expect_is(sr,
              "convert_rate.ft")
    expect_is(sr,
              "convert_rate.ft_select")
  })


}) ## end capture.output


