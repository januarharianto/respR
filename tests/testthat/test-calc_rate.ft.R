
## calc_rate.ft works with single values
expect_error(calc_rate.ft(inflow.o2 = 8.88, outflow.o2 = 8.17, flowrate =  0.00234),
             regexp = NA)

## calc_rate.ft works with single inflow and vector outflow
expect_error(calc_rate.ft(inflow.o2 = 8.88, outflow.o2 = flowthrough.rd$o2.out,
                          flowrate =  0.00234),
             regexp = NA)

## calc_rate.ft works with multiple vectors
expect_error(calc_rate.ft(inflow.o2 = flowthrough.rd$o2.in,
                          outflow.o2 = flowthrough.rd$o2.out, flowrate =  0.00234),
             regexp = NA)

## calc_rate.ft stops with x = NULL and one of inflow.o2 or outflow.o2 missing
expect_error(calc_rate.ft(inflow.o2 = 3.4,
                          outflow.o2 = NULL,
                          flowrate =  0.00234),
             regexp = "Both 'inflow.o2' and 'outflow.o2' inputs should have a value.")
expect_error(calc_rate.ft(inflow.o2 = NULL,
                          outflow.o2 = 3.4,
                          flowrate =  0.00234),
             regexp = "Both 'inflow.o2' and 'outflow.o2' inputs should have a value.")



## calc_rate.ft works with data.frame
expect_error(calc_rate.ft(flowthrough.rd, outflow.o2 = 2,
                          inflow.o2 = 3, flowrate =  0.00234),
             regexp = NA)

## works with `inspect` object
ftdat <- inspect(flowthrough.rd, oxygen = NULL)
expect_error(calc_rate.ft(ftdat, outflow.o2 = 2,
                          inflow.o2 = 3, flowrate =  0.00234),
             regexp = NA)

## works with `inspect_data` object (OLD INSPECT FUNCTION)
sink("/dev/null") ## stops printing outputs on assigning
ftdat_oldinsp <- suppressWarnings(inspect_data(flowthrough.rd, time = 1, outflow.o2 = 2, inflow.o2 = 3))
expect_error(calc_rate.ft(ftdat_oldinsp, outflow.o2 = 2, inflow.o2 = 3, flowrate =  0.00234),
             regexp = NA)
sink() ## turns printing back on


## stops if flowrate missing
expect_error(calc_rate.ft(flowthrough.rd, outflow.o2 = 2,
                          inflow.o2 = 3, flowrate = NULL),
             regexp = "numeric 'flowrate' value must be provided.")

## stops if df/inspect object and inflow.o2 OR outflow.o2 OR BOTH missing
expect_error(calc_rate.ft(flowthrough.rd, outflow.o2 = NULL,
                          inflow.o2 = 3, flowrate =  0.00234),
             regexp = "Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")

expect_error(calc_rate.ft(ftdat, outflow.o2 = 2, # with inspect obj
                          inflow.o2 = NULL, flowrate =  0.00234),
             regexp = "Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")

expect_error(calc_rate.ft(ftdat, outflow.o2 = NULL,
                          inflow.o2 = NULL, flowrate =  0.00234),
             regexp = "Column indices must be provided for 'outflow.o2' and 'inflow.o2'.")


## resulting object converts ok
ftrt <- calc_rate.ft(ftdat, outflow.o2 = 2,
                     inflow.o2 = 3, flowrate =  0.00234, plot = F)

ftrt_conv <- suppressWarnings(convert_rate(ftrt, o2.unit = "mg/l", time.unit = "m", output.unit = "mg/h",
                                           volume = 1))
expect_equal(round(ftrt_conv$output.rate, 8),
             -0.09909348)

ftrt_conv_ms <- suppressWarnings(convert_rate(ftrt, o2.unit = "mg/l", time.unit = "m", output.unit = "mg/h/g",
                                              volume = 1, mass = 0.000070))
expect_equal(round(ftrt_conv_ms$output.rate, 6),
             -1.415621)


## calc_rate S3 generics work
expect_output(print(ftrt))
expect_output(summary(ftrt))
# expect_output(plot(ftrt)) ## this fails - don't know why - it plots
expect_error(plot(ftrt), regexp = NA) ## alternative to above?


