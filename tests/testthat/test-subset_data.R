sink("/dev/null") ## stops printing outputs on assigning 

## works with and outputs df input
sub <- (subset_data(sardine.rd, from = 2000, to = 3000, by = "time"))
expect_is(sub,
          "data.frame")
## exact values
expect_equal(sub$Oxygen[1],
             93.8)
expect_equal(tail(sub$Oxygen, 1),
             93.1)

## works by row
sub <- subset_data(sardine.rd, from = 2000, to = 3000, by = "row")
expect_is(sub,
          "data.frame")
## works by O2
sub <- subset_data(sardine.rd, from = 93, to = 92, by = "o2")
expect_is(sub,
          "data.frame")

## Works with variations of `by` input
invisible({
expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "Time"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "T"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "Row"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 2000, to = 3000, by = "r"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 95, to = 94, by = "Oxygen"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 95, to = 94, by = "O2"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 0.8, to = 0.6, by = "Prop"), regexp = NA)
expect_error(subset_data(sardine.rd, from = 0.8, to = 0.6, by = "p"), regexp = NA)
})

## Error with wrong by
expect_error(subset_data(sardine.rd, by = "tttimmmeee"), 
             "`by` input not recognised")


## adjust_rate can be printed
expect_output(print(sub))


sink() ## turns printing back on

