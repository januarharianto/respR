library(testit)

assert(
  "output conversions, using %, have expected results",
  all.equal(round(convert_DO(10, "%", "mg/l")$output, 3), 0.675),
  all.equal(round(convert_DO(10, "%", "ug/l")$output, 3), 675.11),
  all.equal(round(convert_DO(10, "%", "mmol/l")$output, 3), 0.021),
  all.equal(round(convert_DO(10, "%", "umol/l")$output, 3), 21.098),
  all.equal(round(convert_DO(10, "%", "ml/l")$output, 3), 0.516),
  all.equal(round(convert_DO(10, "%", "mg/kg")$output, 3), 0.66),
  all.equal(round(convert_DO(10, "%", "ug/kg")$output, 3), 659.79),
  all.equal(round(convert_DO(10, "%", "mmol/kg")$output, 3), 0.021),
  all.equal(round(convert_DO(10, "%", "umol/kg")$output, 3), 20.619),
  all.equal(round(convert_DO(10, "%", "ml/kg")$output, 3), 0.504),
  all.equal(round(convert_DO(10, "%", "%")$output, 3), 10),
  all.equal(round(convert_DO(10, "%", "hPa")$output, 3), 20.854),
  all.equal(round(convert_DO(10, "%", "kPa")$output, 3), 2.085),
  all.equal(round(convert_DO(10, "%", "mmHg")$output, 3), 15.642),
  all.equal(round(convert_DO(10, "%", "inHg")$output, 3), 0.616),
  all.equal(round(convert_DO(10, "%", "Torr")$output, 3), 15.642)
)

assert(
  "check that `from` arguments do not produce error",
  !has_error(convert_DO(10, "mg/l", "mg/l")),
  !has_error(convert_DO(10, "ug/l", "mg/l")),
  !has_error(convert_DO(10, "mmol/l", "mg/l")),
  !has_error(convert_DO(10, "umol/l", "mg/l")),
  !has_error(convert_DO(10, "mg/kg", "mg/l")),
  !has_error(convert_DO(10, "ug/kg", "mg/l")),
  !has_error(convert_DO(10, "mmol/kg", "mg/l")),
  !has_error(convert_DO(10, "umol/kg", "mg/l")),
  !has_error(convert_DO(10, "ml/kg", "mg/l")),
  !has_error(convert_DO(10, "%", "mg/l")),
  !has_error(convert_DO(10, "Torr", "mg/l")),
  !has_error(convert_DO(10, "hPa", "mg/l")),
  !has_error(convert_DO(10, "kPa", "mg/l")),
  !has_error(convert_DO(10, "mmHg", "mg/l")),
  !has_error(convert_DO(10, "inHg", "mg/l"))
)

assert(
  "conversion works with changing salinity value",
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 35)$output, 3), 0.506),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 25)$output, 3), 0.536),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 15)$output, 3), 0.567),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 5)$output, 3), 0.601),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 0)$output, 3), 0.618)
)

assert(
  "conversion works with changing pressure value",
  all.equal(round(convert_DO(7.5, "%", "mg/l", P = 0.337)$output, 3), 0.168)
)

assert(
  "conversion works with changing temperature",
  all.equal(round(convert_DO(100, "%", "mg/l", t = 25)$output, 3), 6.751),
  all.equal(round(convert_DO(100, "%", "mg/l", t = 20)$output, 3), 7.377)
)
