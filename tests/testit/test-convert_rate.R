library(testit)

assert(
  "convert_rate works with default values",
  !has_error(suppressWarnings(
    convert_rate(10, volume = 1, mass = 1)))
)

# use known warnings to do these checks
# maybe make this more specific later
assert(
  "convert_rate falls back to default arguments properly",
  has_warning(convert_rate(10, volume = 1, mass = 1)),
  has_warning(convert_rate(10, o2.unit = "mg/l", volume = 1, mass = 1)),
  has_warning(convert_rate(10, time.unit = "s", volume = 1, mass = 1)),
  has_warning(convert_rate(10, output.unit = "mg/h/g", volume = 1, mass = 1)),
  has_warning(convert_rate(10, time.unit = "s", volume = 1, mass = 1))
)

assert(
  "adjust_scale (convert_rate) produces valid output",
  is.numeric(adjust_scale(10, "mg", "kg")),
  is.numeric(adjust_scale(10, "ml", "l"))
  )
