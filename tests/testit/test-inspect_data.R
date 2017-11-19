library(testit)

assert(
  "inspect_data works on 2-column data",
  !has_error(suppressMessages(
    suppressWarnings(
      capture.output(inspect_data(sardine.rd)))))
)


assert(
  "inspect_data works on multi-column data",
  !has_error(suppressMessages(
    suppressWarnings(
      capture.output(inspect_data(urchins.rd, plot = F))))),
  !has_error(suppressMessages(
    suppressWarnings(
      capture.output(inspect_data(urchins.rd, 1, 15, plot = F)))))
)
