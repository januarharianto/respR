# Convert respiration units
# =========================
#
# Convert from one unit to another. Example, % to mg/L.
# Tested and working with data tables i.e. dataframes.
# Note: converts raw data only. Does not convert calculated resp (not yet, anyway).

# Usage
# =========================
# Function name:        convUnits()
# Possible arguments:   x           data to be converted
#                       from        initial unit
#                       to          converted unit
#                       S           salinity in ppt
#                       t           temperature in degree C
#                       P           pressure in bar
#
# Usage:          
# Convert raw data units:
# convUnits(x, from = '%', to = 'mgo2/L')
# convUnits(x, "mgl-1", "ugl-1")
# convUnits(x, "mg/kg", "mg per litre")
#
# Note that there are multiple ways to insert unit arguments, so that the user
# may use the function intuitively without referring to documentation for exact
# syntax.

# Let's do this
# =========================
# Let's id all the common ways to write the units.
.pct   <- (c("%", "percent", "percentage", "%o2", "%O2"))
.mg.l  <- (c("mg/L", "mg/l", "mgL-1", "mgl-1", "mg per liter", "mg per litre"))
.ug.l  <- (c("ug/L", "ug/l", "ugL-1", "ugl-1", "ug per liter", "ug per litre",
           "µg/L", "µg/l", "µgL-1", "µgl-1", "µg per liter", "µg per litre"))
.mm.l  <- (c("mmol/L", "mmol/l", "mmolL-1,", "mmoll-1", "mmol per liter", "mmol per litre"))
.um.l  <- (c("umol/L", "umol/l", "umolL-1", "umoll-1", "µmol/L", "µmol/l", "µmolL-1", "µmoll-1", 
             "umol per litre","umol per liter", "µmol per litre", "µmol per liter"))
.ml.l  <- (c("ml/L", "mL/L", "ml/l", "mll-1", "mLl-1", "mLL-1", "mlL-1", 
           "ml per l", "mL per L", "ml per L"))
.mg.kg <- (c("mg/kg", "mgkg-1", "mg per kg"))
.ug.kg <- (c("ug/kg", "ugkg-1", "ug per kg", "µg/kg", "µgkg-1", "µg per kg"))
.ml.kg <- (c("ml/kg", "mL/kg", "mlkg-1", "mLkg-1", "ml per kg"))
.mm.kg <- (c("mmol/kg", "mmol/Kg", "mmolkg-1", "mmolKg-1", "mmol per kg", "mmol per Kg"))
.um.kg <- (c("umol/kg", "umol/Kg", "umolkg-1,", "umolKg-1", "umol per kg", "umol per Kg",
            "µmol/kg", "µmol/Kg", "µmolkg-1,", "µmolKg-1", "µmol per kg", "µmol per Kg"))
.torr  <- (c("torr", "TORR", "Torr", "Tor", "tor"))
.hPa   <- (c("hPa", "hpa", "Hpa", "HPA", "HPa", "hectopascal", "hpascal")) 
.kPa   <- (c("kPa", "kpa", "Kpa", "KPA", "KPa", "kilopascal", "kpascal")) 
.mmHg  <- (c("mmHg", "mmhg", "MMHG", "millimeter of mercury", "mm mercury"))
.inHg  <- (c("inHg", "inhg", "INHG", "inch of mercury", "inch mercury"))

# The main function below:
convUnits <- function(x, from, to, S = 35, t = 25, P = 1.013253) { # P unit is bar
      # Let's define a few constants and relationships using the `marelac` and gsw packages
      o2molVol  <- unname(marelac::molvol(t, P, species = "O2"))  # moles of O2 in 1 litre
      o2molWt   <- unname(marelac::molweight('O2'))  # molecular weight of O2
      o2gasSat  <- unname(marelac::gas_satconc(S, t, P, species = "O2")) # gas sat.con. in given StP
      swDensity <- gsw::gsw_rho_t_exact(S, t, (P * 10))  # [1] # sw density in given in-situ StP
      vapor     <- marelac::vapor(S = S, t = t)  # partial pressure of water in saturated air
      o2atmComp <- unname(marelac::atmComp('O2')) # atmospheric composition of O2
      # Let's convert any value to mg/L first.
      if (any(from == .pct)) {    # VERIFIED - diff by +0.0003% - P = 1.013253 vs 1.01325 in other
            a <- (x / 100         # convert % to proportion
                    * o2gasSat    # calculate proportion of know O2 sat. conc. in mmol/m^3
                    * o2molWt     # convert mmol/m^3 to mg/1000L
                    / 1e3)        # convert mg/1000L to mg/L
      }
      if (any(from == .mg.l)) {   # VERIFIED - match
            a <- x                # output same value
      }
      if (any(from == .ug.l)) {   # VERIFIED - match
            a <- x / 1e3          # convert ug/L to mg/L
      }
      if (any(from == .mm.l)) {   # VERIFIED - match
            a <- x * o2molWt      # convert mmol/L to mg/L
      } 
      if (any(from == .um.l)) {   # VERIFIED - match
            a <- (x / 1e3         # convert umol/L to mmol/L
                    * o2molWt)    # convert mmol/L to mg/L
      } 
      if (any(from == .ml.l)) {   # VERIFIED - diff by *0.2%
            a <- (x / 1e3         # convert mL/L to L/L
                    / o2molVol    # convert L/L to mol/L
                    * o2molWt     # convert mol/L to g/L
                    * 1e3)        # convert g/L to mg/L
      } 
      if (any(from == .mg.kg)) {  # VERIFIED - diff by -0.008%
            a <- (x * swDensity   # convert mg/kg to mg/1000L
                    / 1e3)        # convert mg/1000L to mg/L
      }
      if (any(from == .ug.kg)) {  # VERIFIED - diff by -0.008%
            a <- (x * swDensity   # convert ug/kg to ug/1000L
                    / 1e6)        # convert ug/1000L to mg/L
      } 
      if (any(from == .ml.kg)) {  # VERIFIED - diff by +0.2%
            a <- (x * o2molWt     # convert mL/kg to mmol/kg
                    / o2molVol    # convert mmol/kg to mg/kg
                    * swDensity   # convert mg/kg to mg/1000L
                    / 1e3)        # convert mg/1000L to mg/L
      }
      if (any(from == .mm.kg)) {  # VERIFIED - diff by -0.008%
            a <- (x * swDensity   # convert mmol/kg to mmol/1000L
                    / 1e3         # convert mmol/1000L to mmol/L
                    * o2molWt)    # convert mmol/L to mg/L
      }
      if (any(from == .um.kg)) {  # VERIFIED - diff by 0.01%
            a <- (x * swDensity   # convert umol/kg to mmol/1000L
                    / 1e3         # convert umol/1000L to umol/L
                    * o2molWt     # convert umol/L to ug/L
                    / 1e3)        # convert ug/L to mg/L
      } 
      if (any(from == .torr)) {
            a <- (x / 760.000066005 # convert Torr to Atm
                    / (P - o2vapor) # divide by vapor-free pressure
                    / o2atmComp     # convert to mmol/m^3
                    * o2gasSat      # multiply by know O2 sat.conc. for proportion (mmol/m^3)
                    * o2molWt       # convert mmol/m^3 to mg/m^3
                    / 1e3)          # convert mg/m^3 to mg/L
      } 
      if (any(from == .hPa)) {
            a <- (x / 1013.253      # convert hPa to Atm
                  / (P - o2vapor)   # divide by vapor-free pressure
                  / o2atmComp       # convert to mmol/m^3
                  * o2gasSat        # multiply by know O2 sat.conc. for proportion (mmol/m^3)
                  * o2molWt         # convert mmol/m^3 to mg/m^3
                  / 1e3)            # convert mg/m^3 to mg/L
      } 
      if (any(from == .kPa)) {
            a <- (x / 101.3253      # convert kPa to Atm
                  / (P - o2vapor)   # divide by vapor-free pressure
                  / o2atmComp       # convert to mmol/m^3
                  * o2gasSat        # multiply by know O2 sat.conc. for proportion (mmol/m^3)
                  * o2molWt         # convert mmol/m^3 to mg/m^3
                  / 1e3)            # convert mg/m^3 to mg/L
      } 
      if (any(from == .mmHg)) {
            a <- (x / 759.999951996 # convert mmHg to Atm
                  / (P - o2vapor)   # divide by vapor-free pressure
                  / o2atmComp       # convert to mmol/m^3
                  * o2gasSat        # multiply by know O2 sat.conc. for proportion (mmol/m^3)
                  * o2molWt         # convert mmol/m^3 to mg/m^3
                  / 1e3)            # convert mg/m^3 to mg/L
      } 
      if (any(from == .inHg)) {
            a <- (x / 29.9212583001 # convert inHg to Atm
                  / (P - o2vapor)   # divide by vapor-free pressure
                  / o2atmComp       # convert to mmol/m^3
                  * o2gasSat        # multiply by know O2 sat.conc. for proportion (mmol/m^3)
                  * o2molWt         # convert mmol/m^3 to mg/m^3(mg/1000L)
                  / 1e3)            # convert mg/1000L to mg/L
      } 
      # Now to conversions
      if (any(to == .pct)) {           # VERIFIED - diff by -0.0003%
            output <- (a * 1e3         # convert mg/L to mg/1000L
                         / o2molWt     # convert mg/1000L to mmol/m^3
                         / o2gasSat    # convert mmol/m^3 to proportion of known O2 sat.conc.
                         * 100)        # convert proportion to %
      }
      if (any(to == .mg.l)) {          # VERIFIED - match
            output <- a                # output same value
      }       
      if (any(to == .ug.l)) {          # VERIFIED - match
            output <- a * 1e3          # convert mg/L to ug/L
      }  
      if (any(to == .mm.l)) {          # VERIFIED - match
            output <- a / o2molWt      # convert mg/L to mmol/L
      }
      if (any(to == .um.l)) {          # VERIFIED - match
            output <- (a / o2molWt     # convert mg/L to mmol/L
                         * 1e3)        # convert mmol/L to umol/L
      } 
      if (any(to == .ml.l)) {          # VERIFIED - diff by -0.2%
            output <- (a / 1e3         # convert mg/L to g/L
                         / o2molWt     # convert g/L to mol/L
                         * o2molVol    # convert mol/L to L/L
                         * 1e3)        # convert L/L to mL/L
      }
      if (any(to == .mg.kg)) {
            output <- (a * 1e3         # convert mg/L to mg/1000L
                         / swDensity)  # convert mg/1000L to mg/kg
      } 
      if (any(to == .ug.kg)) {
            output <- (a * 1e3         # convert mg/L to mg/1000L
                         / swDensity   # convert mg/1000L to mg/kg
                         * 1e3)        # convert mg/kg to ug/kg
      }
      if (any(to == .ml.kg)) {
            output <- (a * 1e3         # convert mg/L to mg/1000L
                         / swDensity   # convert mg/1000L to mg/kg
                         * o2molVol    # convert mg/kg to mmol/kg
                         / o2molWt)    # convert mmol/kg to mL/kg 
      }
      if (any(to == .mm.kg)) {
            output <- (a / o2molWt     # convert mg/L to mmol/L
                         * 1e3         # convert mmol/L to mmol/1000L
                         / swDensity)  # convert mmol/1000L to mmol/kg
      }
      if (any(to == .um.kg)) {
            output <- (a / o2molWt     # convert mg/L to mmol/L
                         * 1e3         # convert mmol/L to mmol/1000L
                         / swDensity   # convert mmol/1000L to mmol/kg
                         * 1e3)        # convert mmol/kg to umol/kg
      }      
      if (any(to == .torr)) {
            output <- (a * 1e3           # convert mg/L to mg/1000L
                       / o2molWt         # convert mg/1000L to mmol/1000L (mmol/m^3)
                       / o2gasSat        # divide by known O2 sat.conc. (mmol/m^3)
                       * o2atmComp       #  
                       * (P - vapor)     # calculate partial pressure in Atm
                       * 760.000066005)  # convert Atm to Torr
      }
      if (any(to == .hPa)) {
            output <- (a * 1e3           # convert mg/L to mg/1000L
                       / o2molWt         # convert mg/1000L to mmol/1000L (mmol/m^3)
                       / o2gasSat        # divide by known O2 sat.conc. (mmol/m^3)
                       * o2atmComp       #  
                       * (P - vapor)     # calculate partial pressure in Atm
                       * 1013.253)       # convert Atm to hPa
      }
      if (any(to == .kPa)) {
            output <- (a * 1e3           # convert mg/L to mg/1000L
                       / o2molWt         # convert mg/1000L to mmol/1000L (mmol/m^3)
                       / o2gasSat        # divide by known O2 sat.conc. (mmol/m^3)
                       * o2atmComp       #  
                       * (P - vapor)     # calculate partial pressure in Atm
                       * 101.3253)       # convert Atm to kPa
      }
      if (any(to == .mmHg)) {
            output <- (a * 1e3           # convert mg/L to mg/1000L
                       / o2molWt         # convert mg/1000L to mmol/1000L (mmol/m^3)
                       / o2gasSat        # divide by known O2 sat.conc. (mmol/m^3)
                       * o2atmComp       #  
                       * (P - vapor)     # calculate partial pressure in Atm
                       * 759.999951996)  # convert Atm to mmHg
      }
      if (any(to == .inHg)) {
            output <- (a * 1e3           # convert mg/L to mg/1000L
                       / o2molWt         # convert mg/1000L to mmol/1000L (mmol/m^3)
                       / o2gasSat        # divide by known O2 sat.conc. (mmol/m^3)
                       * o2atmComp       #  
                       * (P - vapor)     # calculate partial pressure in Atm
                       * 29.9212583001)  # convert Atm to inHg
      }
      # Done! Let's print the result.
      output
}

# # Input Tests, all convert to mg/L
# convUnits(100, "%", "mgl-1") # test %
# respirometry::conv_o2(o2 = 100, from = "percent_a.s.", to = "mg_per_l")
# convUnits(7, "mg/l", "mgl-1") # test mg/l or ug/l
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "mg_per_l")
# convUnits(13.5, "mmoll-1", "mgl-1") # test mmol/l or umol/l
# respirometry::conv_o2(o2 = 13.5, from = "mmol_per_l", to = "mg_per_l")
# convUnits(5, "mll-1", "mgl-1") # test ml/l or ul/l
# respirometry::conv_o2(o2 = 5, from = "ml_per_l", to = "mg_per_l")
# convUnits(5, "mgkg-1", "mgl-1") # test mg/kg or ug/kg
# respirometry::conv_o2(o2 = 5, from = "mg_per_kg", to = "mg_per_l")
# convUnits(5, "mlkg-1", "mgl-1") # test ml/kg or ul/kg
# respirometry::conv_o2(o2 = 5, from = "ml_per_kg", to = "mg_per_l")
# convUnits(7, "mmol/kg", "mgl-1") # test mmol/kg or umol/kg
# respirometry::conv_o2(o2 = 7, from = "mmol_per_kg", to = "mg_per_l")
# convUnits(50, "torr", "mgl-1") # test Torr
# respirometry::conv_o2(o2 = 50, from = "torr", to = "mg_per_l")
# convUnits(50, "hpa", "mgl-1") # test hPa
# respirometry::conv_o2(o2 = 50, from = "hPa", to = "mg_per_l")
# convUnits(50, "KPA", "mgl-1") # test kPa
# respirometry::conv_o2(o2 = 50, from = "kPa", to = "mg_per_l")
# convUnits(50, "mmhg", "mgl-1") # test mmhg
# respirometry::conv_o2(o2 = 50, from = "mmHg", to = "mg_per_l")
# convUnits(50, "inhg", "mgl-1") # test inHg
# respirometry::conv_o2(o2 = 50, from = "inHg", to = "mg_per_l")
# 
# # Output Tests, all convert from mg/L
# convUnits(7, "mg/l", "%") # test %
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "percent_a.s.")
# convUnits(7, "mg/l", "umoll-1") # test umol/L
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "umol_per_l")
# convUnits(7, "mg/l", "mll-1") # test mL/L
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "ml_per_l")
# convUnits(7, "mg/l", "mgkg-1") # test mg/kg
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "mg_per_kg")
# convUnits(7, "mg/l", "mlkg-1") # test mL/kg
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "ml_per_kg")
# convUnits(7, "mg/l", "mmolkg-1") # test mmol/kg
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "mmol_per_kg")
# convUnits(7, "mg/l", "torr") # test Torr
# respirometry::conv_o2(o2 = 7, from = "mg_per_l", to = "torr")

# References
# [1] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation of seawater–2010: 
#     Calculation and use of thermodynamic properties. Technical Report 56, Intergovernmental 
#     Oceanographic Commission, Manuals and Guide. http://www.teos-10.org/pubs/TEOS-10_Manual.pdf.
#
# [2] Weiss R, 1970. The solubility of nitrogen, oxygen, and argon in water and seawater. Deep-Sea
#     Research 17, 721-35.