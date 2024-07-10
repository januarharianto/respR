# library(testthat)
# rm(list=ls())
# testthat::test_file("tests/testthat/test-convert_val.R")
# covr::file_coverage("R/convert_val.R", "tests/testthat/test-convert_val.R")
# cvr <- covr::package_coverage()
# covr::report(cvr)

capture.output({  ## stops printing outputs on assigning

  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
  skip_on_cran()
  skip_on_ci()
test_that("convert_val - stops if from and to units not same type", {
  expect_error(convert_val(-273.15, "K", "kg"),
               "convert_val: 'from' and 'to' appear to be different unit types.")
})

test_that("convert_val - stops if units not recognised, or not of accepted type", {
  expect_error(convert_val(-273.15, "sec", "kg"),
               "convert_val: 'from' unit is not one of the accepted unit types.")
  expect_error(convert_val(-273.15, "kg", "hr"),
               "convert_val: 'to' unit is not one of the accepted unit types.")
  expect_error(convert_val(-273.15, "F", "blah"),
               "convert_val: 'blah' unit not recognised")
})

test_that("convert_val - stops if 'from' input malformed", {
  expect_error(convert_val(-273.15, NULL, "kg"),
               "convert_val: 'from' input is required.")
  expect_error(convert_val(-273.15, 22, "blah"),
               "convert_val: 'from' input is numeric and should not be.")
})

test_that("convert_val - stops if 'to input malformed", {
  expect_error(convert_val(273.15, "g", NULL),
               NA)
  expect_error(convert_val(273.15, "g", 22),
               "convert_val: 'to' input is numeric and should not be.")
})

test_that("convert_val - default 'to' units correctly applied", {
  expect_equal(convert_val(23, "g", NULL),
               convert_val(23, "g", "kg"))
  expect_equal(convert_val(23, "K", NULL),
               convert_val(23, "K", "C"))
  expect_equal(convert_val(23, "ml", NULL),
               convert_val(23, "ml", "l"))
  expect_equal(convert_val(23, "mm2", NULL),
               convert_val(23, "mm2", "msq"))
  expect_equal(convert_val(23, "torr", NULL),
               convert_val(23, "torr", "Bar"))
})

test_that("convert_val - Volume conversions have expected results", {
  uns_in <- c("ul", "mL", "L")
  uns_out <- c("ul", "mL", "L")
  vals <- c(2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  mult <- c(1/1000000, 1/1000, 1)

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = vals))
  grid[[4]] <- apply(grid, 1, function(z) mult[match(z[1], uns_in)])
  grid[[5]] <- apply(grid, 1, function(z) mult[match(z[2], uns_in)])
  grid[[6]] <- apply(grid[3:5], 1, function(z) z[1] * (z[2]/z[3]))

  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[1], z[2]),
                 as.numeric(z[6]))
  )
})

test_that("convert_val - Mass conversions have expected results", {
  uns_in <- c("ug", "mg", "g", "kg")
  uns_out <- c("ug", "mg", "g", "kg")
  vals <- c(2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  mult <- c(1/1000000000, 1/1000000, 1/1000, 1)

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = vals))
  grid[[4]] <- apply(grid, 1, function(z) mult[match(z[1], uns_in)])
  grid[[5]] <- apply(grid, 1, function(z) mult[match(z[2], uns_in)])
  grid[[6]] <- apply(grid[3:5], 1, function(z) z[1] * (z[2]/z[3]))

  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[1], z[2]),
                 as.numeric(z[6]))
  )
})


test_that("convert_val - Area conversions have expected results", {
  uns_in <- c("mm2", "cm2", "msq", "kmsq")
  uns_out <- c("mm2", "cm2", "msq", "kmsq")
  vals <- c(2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  mult <- c(1/1000000, 1/10000, 1, 1000000)

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = vals))
  grid[[4]] <- apply(grid, 1, function(z) mult[match(z[1], uns_in)])
  grid[[5]] <- apply(grid, 1, function(z) mult[match(z[2], uns_in)])
  grid[[6]] <- apply(grid[3:5], 1, function(z) z[1] * (z[2]/z[3]))
  grid[[7]] <- 1:nrow(grid)

  apply(grid, 1, function(z){
    #print(z[7])
    expect_equal(convert_val(as.numeric(z[3]), z[1], z[2]),
                 as.numeric(z[6]))
  }
  )
})

test_that("convert_val - Pressure conversions have expected results", {
  uns_in <- c("kpa", "hPa", "pa", "Ubar", "MBAR",
              "bar", "atm", "Torr")
  uns_out <- c("kpa", "hPa", "pa", "Ubar", "MBAR",
               "bar", "atm", "Torr")
  vals <- c(2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  mult <- c(100, 1000, 100000, 1000000, 1000,
            1, 0.98692, 750.06)

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = vals,
                                 stringsAsFactors = FALSE))
  grid[[4]] <- apply(grid, 1, function(z) mult[match(z[1], uns_in)])
  grid[[5]] <- apply(grid, 1, function(z) mult[match(z[2], uns_in)])
  grid[[6]] <- apply(grid[3:5], 1, function(z) z[1] * (z[3]/z[2]))
  grid[[7]] <- 1:nrow(grid)
  # marelac in unit input
  grid[[8]] <- apply(grid, 1, function(z) {
    if(z[1] %in% c("kpa", "hPa", "pa")) return("Pa") else
      if(z[1] %in% c("Ubar", "MBAR", "bar")) return("bar") else
        if(z[1] %in% c("atm")) return("atm") else
          if(z[1] %in% c("Torr")) return("torr")
  })
  # marelac multiplier for in unit
  grid[[9]] <- apply(grid, 1, function(z) {
    if(z[1] %in% c("kpa")) return(1000) else
      if(z[1] %in% c("hPa")) return(100) else
        if(z[1] %in% c("pa")) return(1) else
          if(z[1] %in% c("Ubar")) return(1/1000000) else
            if(z[1] %in% c("MBAR")) return(1/1000) else
              if(z[1] %in% c("bar")) return(1) else
                if(z[1] %in% c("atm")) return(1) else
                  if(z[1] %in% c("Torr")) return(1)
  })
  # marelac out unit input
  grid[[10]] <- apply(grid, 1, function(z) {
    if(z[2] %in% c("kpa", "hPa", "pa")) return("Pa") else
      if(z[2] %in% c("Ubar", "MBAR", "bar")) return("bar") else
        if(z[2] %in% c("atm")) return("atm") else
          if(z[2] %in% c("Torr")) return("torr")
  })
  # marelac multiplier for out unit
  grid[[11]] <- apply(grid, 1, function(z) {
    if(z[2] %in% c("kpa")) return(1/1000) else
      if(z[2] %in% c("hPa")) return(1/100) else
        if(z[2] %in% c("pa")) return(1) else
          if(z[2] %in% c("Ubar")) return(1000000) else
            if(z[2] %in% c("MBAR")) return(1000) else
              if(z[2] %in% c("bar")) return(1) else
                if(z[2] %in% c("atm")) return(1) else
                  if(z[2] %in% c("Torr")) return(1)
  })
  # marelac out unit reference
  grid[[12]] <- apply(grid, 1, function(z) {
    if(z[10] %in% c("Pa")) return(1) else
      if(z[10] %in% c("bar")) return(2) else
        if(z[10] %in% c("atm")) return(4) else
          if(z[10] %in% c("torr")) return(5)

  })

  # internal calcs
  apply(grid, 1, function(z){
    #print(z[7])
    expect_equal(convert_val(as.numeric(z[3]), z[1], z[2]),
                 as.numeric(z[6]),
                 tolerance = 1e-4)
  })

  # compare to marelac
  apply(grid, 1, function(z){
    #print(z[[7]])
    z <-as.vector(z)
    expect_equal(convert_val(as.numeric(z[3]), z[1], z[2]),
                 marelac::convert_p(as.numeric(z[3])*as.numeric(z[9]),
                                    z[8])[[as.numeric(z[12])]]*as.numeric(z[11])
    )
  })

  # just test some exact known value too (from online converter) to make sure
  # multipliers are done correctly
  expect_equal(convert_val(1, "atm", "bar"),
               1.013253,
               tolerance = 1e-4)
  expect_equal(convert_val(1, "atm", "ubar"),
               1013250,
               tolerance = 1e-4)
  expect_equal(convert_val(1, "atm", "mbar"),
               1013.253,
               tolerance = 1e-4)
  expect_equal(convert_val(1, "atm", "hpa"),
               1013.25,
               tolerance = 1e-4)
  expect_equal(convert_val(1, "atm", "kpa"),
               101.325,
               tolerance = 1e-4)
  expect_equal(convert_val(1, "atm", "pa"),
               101325,
               tolerance = 1e-4)
})

test_that("convert_val - Temperature conversions have expected results", {
  uns_in <- c("C")
  uns_out <- c("K")
  vals <- c(40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  mult <- c(273.15)

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 mult = mult,
                                 stringsAsFactors = FALSE))
  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[[1]], z[[2]]),
                 as.numeric(z[3])+as.numeric(z[4]))
  )

  uns_in <- c("K")
  uns_out <- c("C")
  vals <- c(0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  mult <- c(273.15)

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 mult = mult,
                                 stringsAsFactors = FALSE))
  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[[1]], z[[2]]),
                 as.numeric(z[3])-as.numeric(z[4]))
  )

  uns_in <- c("C")
  uns_out <- c("F")
  vals <- c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 #mult = mult,
                                 stringsAsFactors = FALSE))
  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[[1]], z[[2]]),
                 as.numeric(z[3]) * 9/5 + 32)
  )

  uns_in <- c("F")
  uns_out <- c("C")
  vals <- c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 #mult = mult,
                                 stringsAsFactors = FALSE))
  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[[1]], z[[2]]),
                 (as.numeric(z[3]) - 32) * 5/9)
  )

  uns_in <- c("K")
  uns_out <- c("F")
  vals <- c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 #mult = mult,
                                 stringsAsFactors = FALSE))
  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[[1]], z[[2]]),
                 (as.numeric(z[3]) * 9/5 - 459.67))
  )
  uns_in <- c("F")
  uns_out <- c("K")
  vals <- c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(-40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 #mult = mult,
                                 stringsAsFactors = FALSE))
  apply(grid, 1, function(z)
    expect_equal(convert_val(as.numeric(z[3]), z[[1]], z[[2]]),
                 (as.numeric(z[3]) + 459.67) * 5/9)
  )

  # converts same units - or rather doesn't
  expect_equal(convert_val(10, "c", "c"),
               10)
  expect_equal(convert_val(10, "k", "k"),
               10)
  expect_equal(convert_val(10, "f", "f"),
               10)

  # has same outputs as marelac
  uns_in <- c("C", "K", "F")
  uns_out <- c("C", "K", "F")
  vals <- c(40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values
  vals_min <- -c(40, 0, 2,5, 0.1, 0.0045, 747, 26, 0.00003829, 90000) # random values

  grid <- data.frame(expand.grid(uns_in = uns_in,
                                 uns_out = uns_out,
                                 vals = c(vals, vals_min),
                                 stringsAsFactors = FALSE))

  grid[[4]] <- apply(grid, 1, function(z) {
    if(z[2] %in% c("K")) return(1) else
      if(z[2] %in% c("C")) return(2) else
        if(z[2] %in% c("F")) return(3)
  })
  grid[[5]] <- 1:nrow(grid)

  # compare to marelac
  apply(grid, 1, function(z){
    z <-as.vector(z)
    #print(z[[5]])
    expect_equal(convert_val(as.numeric(z[3]), z[1], z[2]),
                 marelac::convert_T(as.numeric(z[3]), z[[1]])[[as.numeric(z[4])]])
  })
})


test_that("convert_val - works with vectors", {

  expect_error(convert_val(10:20, "g", "kg"),
               NA)
  expect_error(convert_val(10:20, "l", "ml"),
               NA)
  expect_error(convert_val(10:20, "atm", "Torr"),
               NA)
  expect_error(convert_val(10:20, "cm2", "mm2"),
               NA)
  expect_error(convert_val(10:20, "C", "K"),
               NA)

  expect_equal(convert_val(10:20, "g", "kg"),
               c(0.010, 0.011, 0.012, 0.013, 0.014, 0.015, 0.016, 0.017, 0.018, 0.019, 0.020))
  expect_equal(convert_val(10:20, "l", "ml"),
               c(10000, 11000, 12000, 13000, 14000, 15000, 16000, 17000, 18000, 19000, 20000))
  expect_equal(convert_val(10:20, "atm", "Torr"),
               c(7600.008, 8360.009, 9120.010, 9880.011, 10640.011, 11400.012, 12160.013, 12920.014, 13680.015, 14440.015, 15200.016),
               tolerance = 1e-4)
  expect_equal(convert_val(10:20, "cm2", "mm2"),
               c(1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000))
  expect_equal(convert_val(10:20, "C", "K"),
               10:20 + 273.15)
})
}) ## end capture.output
