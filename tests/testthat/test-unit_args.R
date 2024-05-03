## test_file("tests/testthat/test-unit_args.R")

if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
skip_on_cran()

test_that("unit_args just works", {
  expect_output(unit_args())
})
