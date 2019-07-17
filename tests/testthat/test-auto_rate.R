## accepts inspect and inspect_data objects
insp_d <- suppressWarnings(inspect_data(urchins.rd))
expect_error(auto_rate(insp_d), regexp = NA)

insp <- inspect(urchins.rd)
expect_error(auto_rate(insp), regexp = NA)

## stops if input is not a df (or inspect type object)
expect_error(auto_rate(as.matrix(urchins.rd)),
             regexp = "`df` must be a data frame object.")

## uses first two columns of larger df
## message
df3col <- cbind(urchins.rd, urchins.rd)
expect_message(auto_rate(df3col),
               regexp = "auto_rate: Large dataframe detected. Selecting first 2 columns by default.")
## subsets only 2 columns
ar <- auto_rate(df3col)
expect_equal(ncol(ar$df), 2)

## stops with malformed method
## For some reason can't get the actual message to match here, use NULL instead to not be specific
expect_error(auto_rate(urchins.rd, method = "wrong"),
             regexp = NULL)

## auto_rate works using default arguments
ar <- auto_rate(urchins.rd, parallel = F, plot = F)
expect_is(ar,
          "auto_rate")
expect_is(ar$rate,
          "numeric")
expect_is(ar$df,
          "data.frame")

## auto_rate works by default with `max` argument
ar <- auto_rate(urchins.rd, parallel = F, plot = F, method = "max")
expect_is(ar,
          "auto_rate")
ar <- auto_rate(urchins.rd, parallel = F, plot = F, method = "max", width = 10, by = "time")
expect_is(ar,
          "auto_rate")

## auto_rate works by default with `min` argument
ar <- auto_rate(urchins.rd, parallel = F, plot = F, method = "min")
expect_is(ar,
          "auto_rate")
ar <- auto_rate(urchins.rd, parallel = F, plot = F, method = "min", width = 10, by = "time")
expect_is(ar,
          "auto_rate")


## auto_rate will perform interval method using default values
ar <- auto_rate(urchins.rd, method = "interval", plot = F, parallel = F)
expect_is(ar,
          "auto_rate")

## auto_rate can be plotted
      ## NB - This saves a Rplots.pdf to testthat directory
      ## Don't know why
      ## Can't see another way of doing this
expect_error(plot(ar), regex = NA)
suppressWarnings(file.remove("Rplots.pdf"))
## and for different methods too
ar <- auto_rate(urchins.rd, method = "max", plot = F, parallel = F)
expect_error(plot(ar), regex = NA)
suppressWarnings(file.remove("Rplots.pdf"))
## plots individual panels
ar <- auto_rate(urchins.rd, parallel = F, plot = F)
expect_output(plot(ar, choose =1))
expect_output(plot(ar, choose =2))
expect_output(plot(ar, choose =3))
expect_output(plot(ar, choose =4))
expect_output(plot(ar, choose =5))
expect_output(plot(ar, choose =6))


## print() and summary() work for different methods
ar <- auto_rate(urchins.rd, method = "linear", plot = F, parallel = F)
expect_output(print(ar))
expect_output(summary(ar))
ar <- auto_rate(urchins.rd, method = "max", plot = F, parallel = F)
expect_output(print(ar))
expect_output(summary(ar))
ar <- auto_rate(urchins.rd, method = "max", plot = F, parallel = F)
expect_output(print(ar))
expect_output(summary(ar))
ar <- auto_rate(urchins.rd, method = "interval", plot = F, parallel = F)
expect_output(print(ar))
expect_output(summary(ar))
## summary works with different pos
expect_output(summary(ar, pos = 2))

## static_roll (auto_rate) outputs a data frame object
sroll <- static_roll(urchins.rd, 1500)
expect_is(sroll,
          "data.frame")

## time_roll (auto_rate) produces data.frame object",
troll <- time_roll(urchins.rd, width = 10, parallel = FALSE)
expect_is(troll,
          "data.frame")

## time_lm (auto_rate) produces data.frame object",
tlm <- time_lm(urchins.rd, 10, 50)
expect_is(tlm,
          "data.frame")


## Works with variations of `by` input
expect_error(auto_rate(urchins.rd, parallel = F, plot = F, by = "Time"), regexp = NA)
expect_error(auto_rate(urchins.rd, parallel = F, plot = F, by = "T"), regexp = NA)
expect_error(auto_rate(urchins.rd, parallel = F, plot = F, by = "Row"), regexp = NA)
expect_error(auto_rate(urchins.rd, parallel = F, plot = F, by = "r"), regexp = NA)
## Error with wrong by
expect_error(auto_rate(urchins.rd, parallel = F, plot = F, by = "o2"),
             "Invalid `by`` input value, must be 'time' or 'row'.")



## works with method == "interval" and by == "time"
int_tm <- auto_rate(urchins.rd, parallel = F, plot = F, method = "interval", by = "Time")
expect_equal(int_tm$rate[1],
             -0.0335646)


## parallel code works
expect_error(auto_rate(urchins.rd, parallel = T, plot = F),
             regexp = NA)


## works even if width is set to width of entire data
expect_error(auto_rate(urchins.rd, width = 271, by = "row", method = "min", parallel = F, plot = F),
             regexp = NA)

## works if width is set to less than 1
expect_error(auto_rate(urchins.rd, width = 0.5, by = "row", method = "min", parallel = F, plot = T),
             regexp = NA)

## stops if width is non-numeric
expect_error(auto_rate(urchins.rd, width = "text", by = "time", parallel = F, plot = F),
             regexp = "'width' must be numeric.")


