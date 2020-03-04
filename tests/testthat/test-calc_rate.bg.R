
sink("/dev/null") ## stops printing outputs on assigning in log

## Accepts data.frame
## Works using default arguments
## Analyses all columns by default
urbg <- calc_rate.bg(urchins.rd,  plot = F)
expect_is(urbg,
          "calc_rate.bg")
expect_equal(ncol(urbg$data), 19)
expect_equal(length(urbg$bgrate), 18)

## Accepts 2 columns
urbg <- calc_rate.bg(urchins.rd,  time = 1, oxygen = 18, plot = F)
expect_equal(ncol(urbg$data), 2)

## Accepts multiple columns
urbg <- calc_rate.bg(urchins.rd,  time = 1, oxygen = c(18,19), plot = F)
expect_equal(ncol(urbg$data), 3)

## Accepts `inspect_data` objects
ur <- inspect_data(urchins.rd, plot = F)
urbg <- calc_rate.bg(ur,  plot = F)
expect_is(urbg,
          "calc_rate.bg")

## Accepts `inspect` objects
ur <- inspect(urchins.rd, plot = F)
urbg <- calc_rate.bg(ur,  plot = F)
expect_is(urbg,
          "calc_rate.bg")

## Correctly uses specified columns in `inspect` objects
ur <- inspect(urchins.rd, time = 1, oxygen = NULL, plot = F)
urbg <- calc_rate.bg(ur,  time = 1, oxygen = c(18,19), plot = F)
expect_equal(ncol(urbg$data), 3)
expect_equal(ur$dataframe$b1, urbg$data$b1)
expect_equal(ur$dataframe$b2, urbg$data$b2)

## calc_rate S3 generics work
expect_output(print(urbg))
# expect_output(plot(urbg)) ## this fails - don't know why - it plots
expect_error(plot(urbg), regexp = NA) ## alternative to above?


sink() ## turns printing back on
