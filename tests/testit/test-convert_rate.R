library(testit)

assert(
  "convert_rate works with default values",
  !has_error(suppressWarnings(
    convert_rate(10, volume = 1, mass = 1)))
)

assert("convert rate falls back to default arguments properly",
  has_warning(convert_rate(10, volume = 1, mass = 1)),
  has_warning(convert_rate(10, o2.unit = "mg/l", volume = 1, mass = 1)),
  has_warning(convert_rate(10, time.unit = "s", volume = 1, mass = 1)),
  has_warning(convert_rate(10, output.unit = "mg/h/g", volume = 1, mass = 1)),
  has_warning(convert_rate(10, time.unit = "s", volume = 1, mass = 1))
)

assert("adjust_scale produces valid output",
  is.numeric(adjust_scale(10, "mg", "kg"))
  )
