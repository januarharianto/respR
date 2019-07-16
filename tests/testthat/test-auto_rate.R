## accepts inspect and inspect_data objects
insp_d <- suppressWarnings(inspect_data(sardine.rd))
expect_error(auto_rate(insp_d), regexp = NA)

insp <- inspect(sardine.rd)
expect_error(auto_rate(insp), regexp = NA)

## stops if input is not a df (or inspect type object)
expect_error(auto_rate(as.matrix(sardine.rd)),
             regexp = "`df` must be a data frame object.")

## uses first two columns of larger df
## message
df3col <- cbind(sardine.rd, sardine.rd)
expect_message(auto_rate(df3col),
               regexp = "auto_rate: Large dataframe detected. Selecting first 2 columns by default.")
## subsets only 2 columns
ar <- auto_rate(df3col)
expect_equal(ncol(ar$df), 2)

## stops with malformed method
## For some reason can't get the actual message to match here
expect_error(auto_rate(sardine.rd, method = "wrong"),
             regexp = NULL)

## auto_rate works using default arguments
ar <- auto_rate(sardine.rd, parallel = F, plot = F)
expect_is(ar,
          "auto_rate")
expect_is(ar$rate,
          "numeric")
expect_is(ar$df,
          "data.frame")

## auto_rate works by default with `max` argument
ar <- auto_rate(sardine.rd, parallel = F, plot = F, method = "max")
expect_is(ar,
          "auto_rate")
ar <- auto_rate(sardine.rd, parallel = F, plot = F, method = "max", width = 7500, by = "time")
expect_is(ar,
          "auto_rate")

## auto_rate works by default with `min` argument
ar <- auto_rate(sardine.rd, parallel = F, plot = F, method = "min")
expect_is(ar,
          "auto_rate")
ar <- auto_rate(sardine.rd, parallel = F, plot = F, method = "min", width = 7500, by = "time")
expect_is(ar,
          "auto_rate")


## auto_rate will perform interval method using default values
ar <- auto_rate(sardine.rd, method = "interval", plot = F, parallel = F)
expect_is(ar,
          "auto_rate")

## auto_rate can be plotted
      ## NB - This saves a Rplots.pdf to testthat directory
      ## Don't know why - something to do with new plots
      ## Can't see another way of doing this
expect_error(plot(ar), regex = NA)
suppressWarnings(file.remove("Rplots.pdf"))

## auto_rate can be printed
expect_output(print(ar))

## auto_rate can print summary
expect_output(summary(ar))

## static_roll (auto_rate) outputs a data frame object
sroll <- static_roll(sardine.rd, 1500)
expect_is(sroll,
          "data.frame")

## time_roll (auto_rate) produces data.frame object",
troll <- time_roll(sardine.rd, width = 7500, parallel = FALSE)
expect_is(troll,
          "data.frame")

## time_lm (auto_rate) produces data.frame object",
tlm <- time_lm(sardine.rd, 10, 50)
expect_is(tlm,
          "data.frame")


## Works with variations of `by` input
expect_error(auto_rate(sardine.rd, parallel = F, plot = F, by = "Time"), regexp = NA)
expect_error(auto_rate(sardine.rd, parallel = F, plot = F, by = "T"), regexp = NA)
expect_error(auto_rate(sardine.rd, parallel = F, plot = F, by = "Row"), regexp = NA)
expect_error(auto_rate(sardine.rd, parallel = F, plot = F, by = "r"), regexp = NA)
## Error with wrong by
expect_error(auto_rate(sardine.rd, parallel = F, plot = F, by = "o2"),
             "Invalid `by`` input value, must be 'time' or 'row'.")



## works with method == "interval" and by == "time"
int_tm <- auto_rate(sardine.rd, parallel = F, plot = F, method = "interval", by = "Time")
expect_equal(int_tm$rate[1],
             -0.0009244829)



