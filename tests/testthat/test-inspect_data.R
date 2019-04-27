
## inspect_data works on 2-column data
expect_error(suppressMessages(
    suppressWarnings(
      capture.output(inspect_data(sardine.rd)))), regexp = NA)

## inspect_data works on multi-column data
expect_error(suppressMessages(
    suppressWarnings(
      capture.output(inspect_data(urchins.rd, plot = F)))), regexp = NA)
expect_error(suppressMessages(
    suppressWarnings(
      capture.output(inspect_data(urchins.rd, 1, 15, plot = F)))), regexp = NA)
