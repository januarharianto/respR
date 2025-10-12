# rm(list=ls())
# library(testthat)
# test_file("tests/testthat/test-utils.R")

if (!identical(Sys.getenv("NOT_CRAN"), "true")) return()
skip_on_cran()
skip_on_ci()
# units.val ------------------------------------------------------------

test_that("units.val - works", {
  expect_is(units.val("mg/l", "o2"), "character")
  expect_is(units.val("ml", "vol"), "character")
  expect_is(units.val("mg", "mass"), "character")
  expect_is(units.val("mg", "o1"), "character")

  expect_equal(units.val("mg/l", "o2"), "mg/L.o2")
  expect_equal(units.val("ml", "vol"), "mL.vol")
  expect_equal(units.val("mg", "mass"), "mg.mass")
  expect_equal(units.val("mg", "o1"), "mg.o2")
})

# time

test_that("units.val - time inputs are parsed correctly", {
  # variations
  # we want all these to be recognised
  sec.vars  = c('seconds', 'second', 'sec', 'secs', 's',
                'Seconds', 'Second', 'Sec', 'Secs', 'S')
  min.vars  = c('minutes', 'minute', 'min', 'mins', 'm',
                'Minutes', 'Minute', 'Min', 'Mins', 'M')
  hour.vars = c('hours', 'hour', 'hr', 'hrs', 'h',
                'Hours', 'Hour', 'Hr', 'Hrs', 'H')
  day.vars = c('days', 'day', 'dy', 'dys', 'd',
               'Days', 'Day', 'Dy', 'Dys', 'D')

  sapply(sec.vars, function(z) expect_equal(units.val(z, "time"),
                                            "sec.time"))
  sapply(min.vars, function(z) expect_equal(units.val(z, "time"),
                                            "min.time"))
  sapply(hour.vars, function(z) expect_equal(units.val(z, "time"),
                                             "hr.time"))
  sapply(day.vars, function(z) expect_equal(units.val(z, "time"),
                                            "day.time"))

  # partial matching should not work
  expect_error(units.val("secon", "time"),
               "units.val: unit 'secon' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("mi", "time"),
               "units.val: unit 'mi' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("ours", "time"),
               "units.val: unit 'ours' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("Da", "time"),
               "units.val: unit 'Da' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - volume inputs are parsed correctly", {

  # variations
  uL.vars = c("uL.vol","ul","uL","microlitre","microliter",
              "micro litre","micro liter")
  mL.vars = c("mL.vol","ml","mL","millilitre","milli litre","milliliter",
              "milli liter")
  L.vars  = c("L.vol","l","L","liter","litre","Litre","Liter")

  sapply(uL.vars, function(z) expect_equal(units.val(z, "vol"),
                                           "uL.vol")
  )
  sapply(mL.vars, function(z) expect_equal(units.val(z, "vol"),
                                           "mL.vol"))
  sapply(L.vars, function(z) expect_equal(units.val(z, "vol"),
                                          "L.vol"))
  # partial matching should not work
  expect_error(units.val("micro", "vol"),
               "units.val: unit 'micro' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("mil", "vol"),
               "units.val: unit 'mil' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("lit", "vol"),
               "units.val: unit 'lit' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - mass inputs are parsed correctly", {
  # variations
  ug.vars  <- c('ug.mass','ug','UG','ugram','microgram','microgramme','micro gram','micro gramme')
  mg.vars  <- c('mg.mass','mg','MG','mgram','milligram','milligramme','milli gram','milli gramme')
  g.vars   <- c('g.mass','g','G','gram','gramme')
  kg.vars  <- c('kg.mass','kg','KG','kilogram','kilogramme','kilo gram','kilo gramme','kgram')

  sapply(ug.vars, function(z) expect_equal(units.val(z, "mass"),
                                           "ug.mass"))
  sapply(mg.vars, function(z) expect_equal(units.val(z, "mass"),
                                           "mg.mass"))
  sapply(g.vars, function(z) expect_equal(units.val(z, "mass"),
                                          "g.mass"))
  sapply(kg.vars, function(z) expect_equal(units.val(z, "mass"),
                                           "kg.mass"))

  # partial matching should not work
  expect_error(units.val("microg", "time"),
               "units.val: unit 'microg' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("mi", "time"),
               "units.val: unit 'mi' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("amme", "time"),
               "units.val: unit 'amme' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("kilo", "time"),
               "units.val: unit 'kilo' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - o2 inputs are parsed correctly", {

  # error with old % input
  perc.vars <- c("%", "perc", "percent","percentage")
  sapply(perc.vars, function(z) expect_error(units.val(z, "o2"),
                                             "units.val: unit \"%\" has been deprecated. Please use \"%Air\" or \"%Oxy\" instead. See unit_args()."))

  # variations
  percair.vars  <- c("%Air.o2",
                     "%air","%Air","% air","%.air", "%_air", "%A","%a","percair","percentair",
                     "percentageair", "percent air","percentage air")
  percoxy.vars  <- c("%Oxy.o2",
                     "%oxy","%Oxy","%OX","%OXY","%o2","%Oxy","%o","%O",
                     "percoxygen","percentoxygen","percentageoxygen",
                     "perc oxygen","percent.oxygen","percentage_oxygen",
                     "percoxy","percentoxy","percentageoxy",
                     "perc.o2","percent o2","percentage_o2",
                     "percO2","percentO2","percentageO2")
  ugperloxy.vars <- c("ug/L.o2",
                      "ug/L","ug/l","ug / L","ug / l","ugL-1",
                      "ugl-1","ug L-1","ug l -1","ug per liter","ug per litre",
                      "ugO2.L","ugO2_l",
                      "ugO2/L","ugO2/l","ugO2 / L","ugO2 / l","ugO2L-1",
                      "ugO2l-1","ugO2 L-1","ugO2 l -1","ugO2 per liter","ugO2 per litre",
                      "ugO2 per l")
  mgperloxy.vars <- c("mg/L.o2",
                      "mg/L","mg/l","mg / L","mg / l","mgL-1",
                      "mgl-1","mg L-1","mg l -1","mg per liter","mg per litre",
                      "mgO2.L","mgO2_l",
                      "mgO2/L","mgO2/l","mgO2 / L","mgO2 / l","mgO2L-1",
                      "mgO2l-1","mgO2 L-1","mgO2 l -1","mgO2 per liter","mgO2 per litre",
                      "mgO2 per l")
  molperloxy.vars <- c("mol/L.o2",
                       "mol/L","mol/l","mol / L","mol / l","molL-1",
                       "moll-1","mol L-1","mol l -1","mol per liter","mol per litre",
                       "molO2.L","molO2_l",
                       "molel-1","mole L-1","mole l -1","mole per liter","mole per litre",
                       "moleO2.L","moleO2_l",
                       "molO2/L","molO2/l","molO2 / L","molO2 / l","molO2L-1",
                       "molO2l-1","molO2 L-1","molO2 l -1","molO2 per liter","molO2 per litre",
                       "molO2 per l")
  mmolperloxy.vars <- c("mmol/L.o2",
                        "mmol/L","mmol/l","mmol / L","mmol / l","mmolL-1",
                        "mmoll-1","mmol L-1","mmol l -1","mmol per liter","mmol per litre",
                        "mmolO2.L","mmolO2_l",
                        "mmolel-1","mmole L-1","mmole l -1","mmole per liter","mmole per litre",
                        "mmoleO2.L","mmoleO2_l",
                        "mmolO2/L","mmolO2/l","mmolO2 / L","mmolO2 / l","mmolO2L-1",
                        "mmolO2l-1","mmolO2 L-1","mmolO2 l -1","mmolO2 per liter","mmolO2 per litre",
                        "mmolO2 per l")
  umolperloxy.vars <- c("umol/L.o2",
                        "umol/L","umol/l","umol / L","umol / l","umolL-1",
                        "umoll-1","umol L-1","umol l -1","umol per liter","umol per litre",
                        "umolO2.L","umolO2_l",
                        "umolel-1","umole L-1","umole l -1","umole per liter","umole per litre",
                        "umoleO2.L","umoleO2_l",
                        "umolO2/L","umolO2/l","umolO2 / L","umolO2 / l","umolO2L-1",
                        "umolO2l-1","umolO2 L-1","umolO2 l -1","umolO2 per liter","umolO2 per litre",
                        "umolO2 per l")
  nmolperloxy.vars <- c("nmol/L.o2",
                        "nmol/L","nmol/l","nmol / L","nmol / l","nmolL-1",
                        "nmoll-1","nmol L-1","nmol l -1","nmol per liter","nmol per litre",
                        "nmolO2.L","nmolO2_l",
                        "nmolel-1","nmole L-1","nmole l -1","nmole per liter","nmole per litre",
                        "nmoleO2.L","nmoleO2_l",
                        "nmolO2/L","nmolO2/l","nmolO2 / L","nmolO2 / l","nmolO2L-1",
                        "nmolO2l-1","nmolO2 L-1","nmolO2 l -1","nmolO2 per liter","nmolO2 per litre",
                        "nmolO2 per l")
  pmolperloxy.vars <- c("pmol/L.o2",
                        "pmol/L","pmol/l","pmol / L","pmol / l","pmolL-1",
                        "pmoll-1","pmol L-1","pmol l -1","pmol per liter","pmol per litre",
                        "pmolO2.L","pmolO2_l",
                        "pmolel-1","pmole L-1","pmole l -1","pmole per liter","pmole per litre",
                        "pmoleO2.L","pmoleO2_l",
                        "pmolO2/L","pmolO2/l","pmolO2 / L","pmolO2 / l","pmolO2L-1",
                        "pmolO2l-1","pmolO2 L-1","pmolO2 l -1","pmolO2 per liter","pmolO2 per litre",
                        "pmolO2 per l")
  mlperloxy.vars <- c("mL/L.o2",
                      "ml/L","mL/L","mL/l","ml/l","mll-1","mLl-1",
                      "mLL-1","mlL-1","ml / L","mL / L","mL / l","ml / l",
                      "ml l-1","mL l-1","mL L-1","ml L-1","ml per l","mL per L",
                      "ml per L",
                      "mlO2/L","mLO2/L","mLO2/l","mlO2/l","mlO2l-1","mLO2l-1",
                      "mLO2L-1","mlO2L-1","mlO2 / L","mLO2 / L","mLO2 / l","mlO2 / l",
                      "mlO2 l-1","mLO2 l-1","mLO2 L-1","mlO2 L-1","mlO2 per l","mLO2 per L",
                      "mlO2 per L")
  cm3perloxy.vars <- c("cm3/L.o2",
                       "cm3/L","cm3/L","cm3/l","cm3/l","cm3l-1","cm3l-1",
                       "cm3L-1","cm3L-1","cm3 / L","cm3 / L","cm3 / l","cm3 / l",
                       "cm3 l-1","cm3 l-1","cm3 L-1","cm3 L-1","cm3 per l","cm3 per L",
                       "cm3 per L",
                       "cm3O2/L","cm3O2/L","cm3O2/l","cm3O2/l","cm3O2l-1","cm3O2l-1",
                       "cm3O2L-1","cm3O2L-1","cm3O2 / L","cm3O2 / L","cm3O2 / l","cm3O2 / l",
                       "cm3O2 l-1","cm3O2 l-1","cm3O2 L-1","cm3O2 L-1","cm3O2 per l","cm3O2 per L",
                       "cm3O2 per L",
                       "cm^3/L","cm^3O2 per L",
                       "cc/L","cc/L","cc/l","cc/l","ccl-1","ccl-1",
                       "ccL-1","ccL-1","cc / L","cc / L","cc / l","cc / l",
                       "cc l-1","cc l-1","cc L-1","cc L-1","cc per l","cc per L",
                       "cc per L",
                       "cubiccm/L","cubic cmO2/L","cubic cmO2/l","cubic cmO2/l","cubic cmO2l-1","cubic cmO2l-1",
                       "cubic cmO2L-1","cubic cmO2L-1","cubic cmO2 / L","cubic cmO2 / L","cubic cmO2 / l","cubic cmO2 / l",
                       "cubic cmO2 l-1","cubic cmO2 l-1","cubic cmO2 L-1","cubic cmO2 L-1","cubic cmO2 per l","cubic cmO2 per L",
                       "cubic cmO2 per L")
  mgperkgoxy.vars <- c("mg/kg.o2",
                       "mg/KG","Mg/KG","Mg/kg","mg/kg","mgKg-1","MgKg-1",
                       "Mgkg-1","mgkg-1","mg / kg","Mg / kg","Mg / kg","mg / kg",
                       "mg Kg-1","Mg Kg-1","Mg kg-1","mg kg-1","mg per kg","Mg per kg",
                       "mg per kg",
                       "mgO2/KG","MgO2/KG","MgO2/kg","mgO2/kg","mgO2Kg-1","MgO2Kg-1",
                       "MgO2kg-1","mgO2kg-1","mgO2 / kg","MgO2 / kg","MgO2 / kg","mgO2 / kg",
                       "mgO2 Kg-1","MgO2 Kg-1","MgO2 kg-1","mgO2 kg-1","mgO2 per kg","MgO2 per kg",
                       "mgO2 per kg")
  ugperkgoxy.vars <- c("ug/kg.o2",
                       "ug/KG","Ug/KG","Ug/kg","ug/kg","ugKg-1","UgKg-1",
                       "Ugkg-1","ugkg-1","ug / kg","Ug / kg","Ug / kg","ug / kg",
                       "ug Kg-1","Ug Kg-1","Ug kg-1","ug kg-1","ug per kg","Ug per kg",
                       "ug per kg",
                       "ugO2/KG","UgO2/KG","UgO2/kg","ugO2/kg","ugO2Kg-1","UgO2Kg-1",
                       "UgO2kg-1","ugO2kg-1","ugO2 / kg","UgO2 / kg","UgO2 / kg","ugO2 / kg",
                       "ugO2 Kg-1","UgO2 Kg-1","UgO2 kg-1","ugO2 kg-1","ugO2 per kg","UgO2 per kg",
                       "ugO2 per kg")
  mlperkgoxy.vars <- c("mL/kg.o2",
                       "ml/Kg","mL/Kg","mL/kg","ml/kg","mlkg-1","mLkg-1",
                       "mLKG-1","mlKG-1","ml / KG","mL / KG","mL / kg","ml / kg",
                       "ml kg-1","mL kg-1","mL KG-1","ml KG-1","ml per kg","mL per KG",
                       "ml per KG",
                       "mlO2/Kg","mLO2/Kg","mLO2/kg","mlO2/kg","mlO2kg-1","mLO2kg-1",
                       "mLO2KG-1","mlO2KG-1","mlO2 / KG","mLO2 / KG","mLO2 / kg","mlO2 / kg",
                       "mlO2 kg-1","mLO2 kg-1","mLO2 KG-1","mlO2 KG-1","mlO2 per kg","mLO2 per KG",
                       "mlO2 per KG")
  molperkgoxy.vars <- c("mol/kg.o2",
                        "mol/Kg","mole/Kg","mole/kg","mol/kg","molkg-1","molekg-1",
                        "moleKG-1","molKG-1","mol / KG","mole / KG","mole / kg","mol / kg",
                        "mol kg-1","mole kg-1","mole KG-1","mol KG-1","mol per kg","mole per KG",
                        "mol per KG",
                        "molO2/Kg","moleO2/Kg","moleO2/kg","molO2/kg","molO2kg-1","moleO2kg-1",
                        "moleO2KG-1","molO2KG-1","molO2 / KG","moleO2 / KG","moleO2 / kg","molO2 / kg",
                        "molO2 kg-1","moleO2 kg-1","moleO2 KG-1","molO2 KG-1","molO2 per kg","moleO2 per KG",
                        "molO2 per KG")
  mmolperkgoxy.vars <- c("mmol/kg.o2",
                         "mmol/Kg","mmole/Kg","mmole/kg","mmol/kg","mmolkg-1","mmolekg-1",
                         "mmoleKG-1","mmolKG-1","mmol / KG","mmole / KG","mmole / kg","mmol / kg",
                         "mmol kg-1","mmole kg-1","mmole KG-1","mmol KG-1","mmol per kg","mmole per KG",
                         "mmol per KG",
                         "mmolO2/Kg","mmoleO2/Kg","mmoleO2/kg","mmolO2/kg","mmolO2kg-1","mmoleO2kg-1",
                         "mmoleO2KG-1","mmolO2KG-1","mmolO2 / KG","mmoleO2 / KG","mmoleO2 / kg","mmolO2 / kg",
                         "mmolO2 kg-1","mmoleO2 kg-1","mmoleO2 KG-1","mmolO2 KG-1","mmolO2 per kg","mmoleO2 per KG",
                         "mmolO2 per KG")
  umolperkgoxy.vars <- c("umol/kg.o2",
                         "umol/Kg","umole/Kg","umole/kg","umol/kg","umolkg-1","umolekg-1",
                         "umoleKG-1","umolKG-1","umol / KG","umole / KG","umole / kg","umol / kg",
                         "umol kg-1","umole kg-1","umole KG-1","umol KG-1","umol per kg","umole per KG",
                         "umol per KG",
                         "umolO2/Kg","umoleO2/Kg","umoleO2/kg","umolO2/kg","umolO2kg-1","umoleO2kg-1",
                         "umoleO2KG-1","umolO2KG-1","umolO2 / KG","umoleO2 / KG","umoleO2 / kg","umolO2 / kg",
                         "umolO2 kg-1","umoleO2 kg-1","umoleO2 KG-1","umolO2 KG-1","umolO2 per kg","umoleO2 per KG",
                         "umolO2 per KG")
  nmolperkgoxy.vars <- c("nmol/kg.o2",
                         "nmol/Kg","nmole/Kg","nmole/kg","nmol/kg","nmolkg-1","nmolekg-1",
                         "nmoleKG-1","nmolKG-1","nmol / KG","nmole / KG","nmole / kg","nmol / kg",
                         "nmol kg-1","nmole kg-1","nmole KG-1","nmol KG-1","nmol per kg","nmole per KG",
                         "nmol per KG",
                         "nmolO2/Kg","nmoleO2/Kg","nmoleO2/kg","nmolO2/kg","nmolO2kg-1","nmoleO2kg-1",
                         "nmoleO2KG-1","nmolO2KG-1","nmolO2 / KG","nmoleO2 / KG","nmoleO2 / kg","nmolO2 / kg",
                         "nmolO2 kg-1","nmoleO2 kg-1","nmoleO2 KG-1","nmolO2 KG-1","nmolO2 per kg","nmoleO2 per KG",
                         "nmolO2 per KG")
  pmolperkgoxy.vars <- c("pmol/kg.o2",
                         "pmol/Kg","pmole/Kg","pmole/kg","pmol/kg","pmolkg-1","pmolekg-1",
                         "pmoleKG-1","pmolKG-1","pmol / KG","pmole / KG","pmole / kg","pmol / kg",
                         "pmol kg-1","pmole kg-1","pmole KG-1","pmol KG-1","pmol per kg","pmole per KG",
                         "pmol per KG",
                         "pmolO2/Kg","pmoleO2/Kg","pmoleO2/kg","pmolO2/kg","pmolO2kg-1","pmoleO2kg-1",
                         "pmoleO2KG-1","pmolO2KG-1","pmolO2 / KG","pmoleO2 / KG","pmoleO2 / kg","pmolO2 / kg",
                         "pmolO2 kg-1","pmoleO2 kg-1","pmoleO2 KG-1","pmolO2 KG-1","pmolO2 per kg","pmoleO2 per KG",
                         "pmolO2 per KG")
  torroxy.vars <- c("Torr.o2p", "torr","TORR","Torr","Tor","tor","torrO2","TORRO2","TorrO2","TorO2","torO2")
  hpaoxy.vars <- c("hPa.o2p",
                   "hPa","hpa","Hpa","HPA","HPa","hectopascal",
                   "hpascal",
                   "hPaO2","hpaO2","HpaO2","HPAO2","HPaO2","hectopascalO2",
                   "hpascalO2")
  kpaoxy.vars <- c("kPa.o2p",
                   "kPa","kpa","Kpa","KPA","KPa","kilopascal",
                   "kpascal",
                   "kPaO2","kpaO2","KpaO2","KPAO2","KPaO2","kilopascalO2",
                   "kpascalO2")
  mmHgoxy.vars <- c("mmHg.o2p",
                    "mmHg","mm Hg","mmhg","mm hg","MMHG","MM HG",
                    "millimeter of mercury","mm mercury",
                    "mmHgO2","mm HgO2","mmhgO2","mm hgO2","MMHGO2","MM HGO2",
                    "millimeter of mercuryO2","mm mercuryO2")
  inHgoxy.vars <- c("inHg.o2p",
                    "inHg","in Hg","inhg","in hg","inchHG","inches HG",
                    "inches of mercury","in mercury",
                    "inHgO2","in HgO2","inhgO2","in hgO2","INHGO2","IN HGO2",
                    "inches of mercuryO2","in mercuryO2")

  sapply(percair.vars, function(z) expect_equal(units.val(z, "o2"),
                                                "%Air.o2"))
  sapply(percoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                "%Oxy.o2"))
  sapply(ugperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                  "ug/L.o2"))
  sapply(mgperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                  "mg/L.o2"))
  sapply(molperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                   "mol/L.o2"))
  sapply(mmolperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                    "mmol/L.o2"))
  sapply(umolperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                    "umol/L.o2"))
  sapply(nmolperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                    "nmol/L.o2"))
  sapply(pmolperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                    "pmol/L.o2"))
  sapply(mlperloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                  "mL/L.o2"))
  sapply(cm3perloxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                   "cm3/L.o2"))
  sapply(mgperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                   "mg/kg.o2"))
  sapply(ugperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                   "ug/kg.o2"))
  sapply(mlperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                   "mL/kg.o2"))
  sapply(molperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                    "mol/kg.o2"))
  sapply(mmolperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                     "mmol/kg.o2"))
  sapply(umolperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                     "umol/kg.o2"))
  sapply(nmolperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                     "nmol/kg.o2"))
  sapply(pmolperkgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                     "pmol/kg.o2"))
  sapply(torroxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                "Torr.o2p"))
  sapply(hpaoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                               "hPa.o2p"))
  sapply(kpaoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                               "kPa.o2p"))
  sapply(mmHgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                "mmHg.o2p"))
  sapply(inHgoxy.vars, function(z) expect_equal(units.val(z, "o2"),
                                                "inHg.o2p"))

  # partial matching should not work
  expect_error(units.val("%ai", "o2"),
               "units.val: unit '%ai' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("_%air", "o2"),
               "units.val: unit '_%air' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("%oxyg", "o2"),
               "units.val: unit '%oxyg' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("ugO2 per lit", "o2"),
               "units.val: unit 'ugO2 per lit' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("mgO2 per lit", "o2"),
               "units.val: unit 'mgO2 per lit' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("molO2 per lit", "o2"),
               "units.val: unit 'molO2 per lit' not recognised. Check it is valid for the input or output type.")
  expect_error(units.val("mmolO2 per lit", "o2"),
               "units.val: unit 'mmolO2 per lit' not recognised. Check it is valid for the input or output type.")

})

test_that("units.val - area inputs are parsed correctly", {
  # variations
  mm.vars  <- c('mm2.area','mmsq','mm2','MM2','millimetre2')
  cm.vars  <- c('cm2.area','cmsq','cm2','CM2','centimetre2')
  m.vars   <- c('m2.area','msq','m2','M2','metre2')
  km.vars  <- c('km2.area','kmsq','km2','KM2','kilometre2')

  sapply(mm.vars, function(z) expect_equal(units.val(z, "area"),
                                           "mm2.area"))
  sapply(cm.vars, function(z) expect_equal(units.val(z, "area"),
                                           "cm2.area"))
  sapply(m.vars, function(z) expect_equal(units.val(z, "area"),
                                           "m2.area"))
  sapply(km.vars, function(z) expect_equal(units.val(z, "area"),
                                           "km2.area"))

  # partial matching should not work
  expect_error(units.val("millim", "area"),
               "units.val: unit 'millim' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - o1 inputs are parsed correctly", {
  # variations
  mg.vars  <- c('mg.o2','mgo2','mgO2','mg','Milligram','milligramme','milligrams','milligrammes')
  ug.vars  <- c('ug.o2','ugo2','ugO2','ug','microgram','microgramme','micrograms','microgrammes')


  mol.vars  <- c('mol.o2','molo2','molO2','mol','mole')
  mmol.vars <- c('mmol.o2','mmolo2','mmolO2','mmol','millimol','millimole')
  umol.vars <- c('umol.o2','umolo2','umolO2','umol','micromol','micromole')
  nmol.vars <- c('nmol.o2','nmolo2','nmolO2','nmol','nanomol','nanomole')
  pmol.vars <- c('pmol.o2','pmolo2','pmolO2','pmol','picomol','picomole')
  ml.vars   <- c('mL.o2','mlo2','mlO2','ml','mLo2','mLO2','mL',
                'millilitre','milliliter','millilitres','milliliters')
  cm3.vars  <- c('cm3.o2','cm3o2','cm3.O2','cm3O2','cm3',
                 'CM3.o2','CM3o2','CM3.O2','CM3O2','CM3',
                 'cm^3')

  sapply(mg.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "mg.o2"))
  sapply(ug.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "ug.o2"))
  sapply(mol.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "mol.o2"))
  sapply(mmol.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "mmol.o2"))
  sapply(umol.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "umol.o2"))
  sapply(nmol.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "nmol.o2"))
  sapply(pmol.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "pmol.o2"))
  sapply(ml.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "mL.o2"))
  sapply(cm3.vars, function(z) expect_equal(units.val(z, "o1"),
                                           "cm3.o2"))

  # partial matching should not work
  expect_error(units.val("micro", "o1"),
               "units.val: unit 'micro' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - flow inputs are parsed correctly", {
  # variations
  ulpersec.vars  <- c("ul/S.flow",
                      "ul/Sec","ul/secs","ul / second","ul / s","ulS-1",
                      "uls-1","ul sec-1","ul secs -1","ul per second","ul per Seconds",
                      "ul.S","ul_s",
                      "UL.S","UL/s","UL / S","UL / s","ULS-1",
                      "microlitre s-1","Microlitre SEC-1","microliters_secs -1","Microliter per second","UL per seconds",
                      "Ul per s")
  mlpersec.vars  <- c("ml/S.flow",
                      "ml/Sec","ml/secs","ml / second","ml / s","mlS-1",
                      "mls-1","ml sec-1","ml secs -1","ml per second","ml per Seconds",
                      "ml.S","ml_s",
                      "ML.S","ML/s","ML / S","ML / s","MLS-1",
                      "millilitre s-1","Millilitre SEC-1","milliliters_secs -1","Milliliter per second","ML per seconds",
                      "Ml per s")
  lpersec.vars  <- c("l/S.flow",
                      "L/Sec","l/secs","l / second","L / s","lS-1",
                      "ls-1","l sec-1","l secs -1","l per second","l per Seconds",
                      "l.S","l_s",
                      "L.S","L/s","L / S","L / s","LS-1",
                      "litre s-1","Litre SEC-1","liters_secs -1","Liter per second","L per seconds",
                      "l per s")
  ulpermin.vars  <- c("ul/M.flow",
                      "ul/Min","ul/mins","ul / minute","ul / m","ulM-1",
                      "ulm-1","ul min-1","ul mins -1","ul per minute","ul per Minutes",
                      "ul.M","ul_m",
                      "UL.M","UL/m","UL / M","UL / m","ULM-1",
                      "microlitre m-1","Microlitre MIN-1","microliters_mins -1","Microliter per minute","UL per minutes",
                      "Ul per m")
  mlpermin.vars  <- c("ml/M.flow",
                      "ml/Min","ml/mins","ml / minute","ml / m","mlM-1",
                      "mlm-1","ml min-1","ml mins -1","ml per minute","ml per Minutes",
                      "ml.M","ml_m",
                      "ML.M","ML/m","ML / M","ML / m","MLM-1",
                      "millilitre m-1","Millilitre MIN-1","milliliters_mins -1","Milliliter per minute","ML per minutes",
                      "Ml per m")
  lpermin.vars  <- c("l/M.flow",
                     "L/Min","l/mins","l / minute","L / m","lM-1",
                     "lm-1","l min-1","l mins -1","l per minute","l per Minutes",
                     "l.M","l_m",
                     "L.M","L/m","L / M","L / m","LM-1",
                     "litre m-1","Litre MIN-1","liters_mins -1","Liter per minute","L per minutes",
                     "l per m")
  ulperhr.vars  <- c("ul/H.flow",
                      "ul/Hr","ul/hour","ul / hour","ul / h","ulH-1",
                      "ulh-1","ul hr-1","ul hrs -1","ul per hour","ul per Hours",
                      "ul.H","ul_h",
                      "UL.H","UL/h","UL / H","UL / h","ULH-1",
                      "microlitre h-1","Microlitre HR-1","microliters_hrs -1","Microliter per hour","UL per hours",
                      "Ul per h")
  mlperhr.vars  <- c("ml/H.flow",
                      "ml/Hr","ml/hour","ml / hour","ml / h","mlH-1",
                      "mlh-1","ml hr-1","ml hrs -1","ml per hour","ml per Hours",
                      "ml.H","ml_h",
                      "ML.H","ML/h","ML / H","ML / h","MLH-1",
                      "millilitre h-1","Millilitre HR-1","milliliters_hrs -1","Milliliter per hour","ML per hours",
                      "Ml per h")
  lperhr.vars  <- c("l/H.flow",
                      "L/Hr","l/hour","l / hour","L / h","lH-1",
                      "lh-1","l hr-1","l hrs -1","l per hour","l per Hours",
                      "l.H","l_h",
                      "L.H","L/h","L / H","L / h","LH-1",
                      "litre h-1","Litre HR-1","liters_hrs -1","Liter per hour","L per hours",
                      "l per h")
  ulperday.vars  <- c("ul/D.flow",
                      "ul/D","ul/day","ul / day","ul / d","ulD-1",
                      "uld-1","ul day-1","ul days -1","ul per day","ul per Days",
                      "ul.D","ul_d",
                      "UL.D","UL/d","UL / D","UL / d","ULD-1",
                      "microlitre d-1","Microlitre D-1","microliters_days -1","Microliter per day","UL per days",
                      "Ul per d")
  mlperday.vars  <- c("ml/D.flow",
                      "ml/D","ml/day","ml / day","ml / d","mlD-1",
                      "mld-1","ml day-1","ml days -1","ml per day","ml per Days",
                      "ml.D","ml_d",
                      "ML.D","ML/d","ML / D","ML / d","MLD-1",
                      "millilitre d-1","Millilitre D-1","milliliters_days -1","Milliliter per day","ML per days",
                      "Ml per d")
  lperday.vars  <- c("l/D.flow",
                      "L/D","l/day","l / day","L / d","lD-1",
                      "ld-1","l day-1","l days -1","l per day","l per Days",
                      "l.D","l_d",
                      "L.D","L/d","L / D","L / d","LD-1",
                      "litre d-1","Litre D-1","liters_days -1","Liter per day","L per days",
                      "l per d")

  sapply(ulpersec.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "uL/sec.flow"))
  sapply(mlpersec.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "mL/sec.flow"))
  sapply(lpersec.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "L/sec.flow"))
  sapply(ulpermin.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "uL/min.flow"))
  sapply(mlpermin.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "mL/min.flow"))
  sapply(lpermin.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "L/min.flow"))
  sapply(ulperhr.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "uL/hr.flow"))
  sapply(mlperhr.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "mL/hr.flow"))
  sapply(lperhr.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "L/hr.flow"))
  sapply(ulperday.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "uL/day.flow"))
  sapply(mlperday.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "mL/day.flow"))
  sapply(lperday.vars, function(z) expect_equal(units.val(z, "flow"),
                                                 "L/day.flow"))

  # partial matching should not work
  expect_error(units.val("micro", "flow"),
               "units.val: unit 'micro' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - pressure inputs are parsed correctly", {
  # variations
  kpa.vars  <- c('kPa.p', 'kPa','kpa', 'KPA')
  hpa.vars  <- c('hPa.p', 'hPa','hpa', 'HPA')
  pa.vars   <- c('Pa.p', 'Pa','pa', 'PA')
  ubar.vars <- c('uBar.p', 'ub', 'ubar', 'Ubar', 'UBAR', 'uBar', 'ubr', 'UBR')
  mbar.vars <- c('mBar.p', 'mb', 'mbar', 'Mbar', 'MBAR', 'mBar', 'mbr', 'MBR')
  bar.vars  <- c('Bar.p', 'bar', 'bar', 'BAR', 'Bar', 'br', 'BR')
  atm.vars  <- c('atm.p', 'atm', 'Atm', 'ATM', 'Atmos', 'ATMOS')
  torr.vars <- c('Torr.p', 'torr','TORR','Torr','Tor','tor')
  mmhg.vars <- c('mmHg.p', 'mmHg','mm Hg','mmhg','mm hg','MMHG','MM HG')
  inhg.vars <- c('inHg.p', 'inHg','in Hg','inhg','in hg','INHG','IN HG')

  sapply(kpa.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "kPa.p"))
  sapply(hpa.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "hPa.p"))
  sapply(pa.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "Pa.p"))
  sapply(ubar.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "uBar.p"))
  sapply(mbar.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "mBar.p"))
  sapply(bar.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "Bar.p"))
  sapply(atm.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "atm.p"))
  sapply(torr.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "Torr.p"))
  sapply(mmhg.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "mmHg.p"))
  sapply(inhg.vars, function(z) expect_equal(units.val(z, "pressure"),
                                                 "inHg.p"))

  # partial matching should not work
  expect_error(units.val("micro", "flow"),
               "units.val: unit 'micro' not recognised. Check it is valid for the input or output type.")
})

test_that("units.val - temperature inputs are parsed correctly", {
  # variations
  c.vars  <- c('C','c', 'dgrc', 'DGRC', 'dgr c', 'DGR C',
              'degrees c', 'DEGREES C',
              'celsius', 'Celsius', 'CELSIUS',
              'centigrade', 'Centigrade')
  k.vars  <- c('K','k', 'dgrk', 'DGRK', 'dgr k', 'DGR K',
              'degrees k', 'DEGREES K',
              'kelvin', 'Kelvin', 'KELVIN')
  f.vars <- c('F','f', 'dgrf', 'DGRF', 'dgr f', 'DGR F',
              'degrees f', 'DEGREES F',
              'fahrenheit', 'Fahrenheit', 'FAHRENHEIT')

  sapply(c.vars, function(z) expect_equal(units.val(z, "temperature"),
                                                 "C.temp"))
  sapply(k.vars, function(z) expect_equal(units.val(z, "temperature"),
                                                 "K.temp"))
  sapply(f.vars, function(z) expect_equal(units.val(z, "temperature"),
                                                 "F.temp"))
  # partial matching should not work
  expect_error(units.val("celc", "temperature"),
               "units.val: unit 'celc' not recognised. Check it is valid for the input or output type.")
})



# StP units tests - o2 and mr
