## test_file("tests/testthat/test-unit_args.R")

if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
skip_on_cran()
skip_on_ci()
test_that("unit_args just works", {
  expect_output(unit_args())
})
