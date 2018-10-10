library(testit)

assert(
  "output conversions, using %, have expected results",
  all.equal(round(convert_DO(10, "%", "mg/l", S = 35, t = 25)$output, 3), 0.675),
  all.equal(round(convert_DO(10, "%", "ug/l", S = 35, t = 25)$output, 3), 675.11),
  all.equal(round(convert_DO(10, "%", "mmol/l", S = 35, t = 25)$output, 3), 0.021),
  all.equal(round(convert_DO(10, "%", "umol/l", S = 35, t = 25)$output, 3), 21.098),
  all.equal(round(convert_DO(10, "%", "ml/l", S = 35, t = 25)$output, 3), 0.516),
  all.equal(round(convert_DO(10, "%", "mg/kg", S = 35, t = 25)$output, 3), 0.66),
  all.equal(round(convert_DO(10, "%", "ug/kg", S = 35, t = 25)$output, 3), 659.79),
  all.equal(round(convert_DO(10, "%", "mmol/kg", S = 35, t = 25)$output, 3), 0.021),
  all.equal(round(convert_DO(10, "%", "umol/kg", S = 35, t = 25)$output, 3), 20.619),
  all.equal(round(convert_DO(10, "%", "ml/kg", S = 35, t = 25)$output, 3), 0.504),
  all.equal(round(convert_DO(10, "%", "%", S = 35, t = 25)$output, 3), 10),
  all.equal(round(convert_DO(10, "%", "hPa", S = 35, t = 25)$output, 3), 20.854),
  all.equal(round(convert_DO(10, "%", "kPa", S = 35, t = 25)$output, 3), 2.085),
  all.equal(round(convert_DO(10, "%", "mmHg", S = 35, t = 25)$output, 3), 15.642),
  all.equal(round(convert_DO(10, "%", "inHg", S = 35, t = 25)$output, 3), 0.616),
  all.equal(round(convert_DO(10, "%", "Torr", S = 35, t = 25)$output, 3), 15.642)
  )

assert(
  "check that `from` arguments do not produce error",
  !has_error(convert_DO(10, "mg/l", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "ug/l", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "mmol/l", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "umol/l", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "ml/l", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "mg/kg", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "ug/kg", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "mmol/kg", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "umol/kg", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "ml/kg", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "%", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "Torr", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "hPa", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "kPa", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "mmHg", "mg/l", S = 35, t = 25)),
  !has_error(convert_DO(10, "inHg", "mg/l", S = 35, t = 25))
  )

assert(
  "conversion works with changing salinity value",
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 35, t = 25)$output, 3), 0.506),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 25, t = 25)$output, 3), 0.536),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 15, t = 25)$output, 3), 0.567),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 5, t = 25)$output, 3), 0.601),
  all.equal(round(convert_DO(7.5, "%", "mg/l", S = 0, t = 25)$output, 3), 0.618)
  )

assert(
  "conversion works with changing pressure value",
  all.equal(round(convert_DO(7.5, "%", "mg/l", P = 0.337, S = 35, t = 25)$output, 3), 0.168)
  )

assert(
  "conversion works with changing temperature",
  all.equal(round(convert_DO(100, "%", "mg/l", t = 25, S = 35)$output, 3), 6.751),
  all.equal(round(convert_DO(100, "%", "mg/l", t = 20, S = 35)$output, 3), 7.377)
  )

assert(
  "verify_units should work...",
  is.character(verify_units("mg/l", "o2")),
  is.character(verify_units("ml", "vol")),
  is.character(verify_units("mg", "mass")),
  is.character(verify_units("mg", "o1"))
)

assert(
  "convert_DO will print",
  !has_error(print(convert_DO(10, "inHg", "mg/l", S = 35, t = 25)))
  )

