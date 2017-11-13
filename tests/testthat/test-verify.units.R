context("verify.units")

test_that("function can identify one-dimensional O2 unit", {
  expect_equal(verify.units("ml", "vol"), "mL.vol")
})

test_that("function can identify two-dimensional O2 unit", {
  expect_equal(verify.units("mg/l", "o2"), "mg/L.o2")
  expect_equal(verify.units("mg/kg", "o2"), "mg/kg.o2")
})

test_that("function can identify volume unit", {
  expect_equal(verify.units("ml", "vol"), "mL.vol")
})

test_that("function can identify mass unit", {
  expect_equal(verify.units("mg", "mass"), "mg.mass")
})

