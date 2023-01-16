# convert_DO(100, "%Air", "cm3/l", S = 35, t = 25, P = 1.013253)
# convert_DO(100, "%Air", "ml/l", S = 35, t = 25, P = 1.013253)
# convert_DO(10, "cm^3/l", "%Air", S = 35, t = 25, P = 1.013253)
#
#
#
#
# # -------------------------------------------------------------------------
#
# # Oxygen
# # ⁠"ug" "mg" "pmol" "nmol" "umol" "mmol" "mol" "mL"⁠
# # add cm3 or cc = ml
# # mm3 = ul?
# # pressure?
#
# # Time
# # ⁠"sec" "min" "hour" "day"⁠
# # Mass
# # ⁠"ug" "mg" "g" "kg"⁠
# # Area
# # ⁠"mm2" "cm2" "m2" "km2"⁠
#
#
#
#
# rate <- 0.5
# in.unit <- "mg/s"
# out.unit <- "mg/day"
# S = 35
# t = 15
# P = 1.01
#
# rate <- 0.5
# in.unit <- "ml/h/g"
# out.unit <- "mg/day/kg"
# S = 35
# t = 15
# P = 1.01
#
# rate <- 0.5
# in.unit <- "ml/h/mm2"
# out.unit <- "mg/day/m2"
# S = 35
# t = 15
# P = 1.01
#
#
# in.unit <- "mg/h/g"
# in.unit <- "ml/h"
# in.unit <- "ml/h/g"
#
#
#
# # separate the input unit
# in.uns <- as.matrix(read.table(text = gsub("(?:-1|[/.[:space:]])+",
#                                            " ", in.unit), header = FALSE))
# # input unit types
# in.uns.type <- sapply(in.uns, function(x) respR:::unit_type(x))
#
# # unit type
# if(length(in.uns.type) == 2) un.type <- "abs" else
#   if(in.uns.type[3] == "mass") un.type <- "mass.spec" else
#     if(in.uns.type[3] == "area") un.type <- "area.spec" else
#       stop("convert_mr: unit error")
#
# if(un.type == "abs"){
#   # parse time unit
#   tm.unit <- respR:::verify_units(in.uns[2], "time")
#   # adjust rate to per hour
#   rate.adj <- adjust_scale(rate, tm.unit, "hour.time")
#
#   # parse oxygen unit - make it a per l conc - then we use 1 l as volume
#   ox.unit <- respR:::verify_units(paste0(in.uns[1], "/l"), "o2")
#   # adjust rate to mg/l oxygen
#   rate.adj <- convert_DO(rate.adj, ox.unit, "mg/l", S = S, t = t, P = P)
#
#   # now convert to output unit
#   rate.out <- convert_rate(rate.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = out.unit,
#                            volume = 1, mass = NULL, area = NULL,
#                            S = S, t = t, P = P,
#                            plot = FALSE)
# }
#
# if(un.type == "mass.spec"){
#   # parse time unit
#   tm.unit <- respR:::verify_units(in.uns[2], "time")
#   # adjust rate to per hour
#   rate.adj <- adjust_scale(rate, tm.unit, "hour.time")
#
#   # parse mass unit
#   as.unit <- respR:::verify_units(in.uns[3], "mass")
#   # adjust rate to per kg
#   rate.adj <- adjust_scale(rate.adj, as.unit, "kg.mass")
#
#   # parse oxygen unit - make it a per l conc - then we use 1 l as volume
#   ox.unit <- respR:::verify_units(paste0(in.uns[1], "/l"), "o2")
#   # adjust rate to mg/l oxygen
#   rate.adj <- convert_DO(rate.adj, ox.unit, "mg/l", S = S, t = t, P = P)
#
#   # now convert to output unit
#   rate.out <- convert_rate(rate.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = out.unit,
#                            volume = 1, mass = 1, area = NULL,
#                            S = S, t = t, P = P,
#                            plot = FALSE)
# }
#
# if(un.type == "area.spec"){
#   # parse time unit
#   tm.unit <- respR:::verify_units(in.uns[2], "time")
#   # adjust rate to per hour
#   rate.adj <- adjust_scale(rate, tm.unit, "hour.time")
#
#   # parse area unit
#   as.unit <- respR:::verify_units(in.uns[3], "area")
#   # adjust rate to per kg
#   rate.adj <- adjust_scale_area(rate.adj, as.unit, "m2.area")
#
#   # parse oxygen unit - make it a per l conc - then we use 1 l as volume
#   ox.unit <- respR:::verify_units(paste0(in.uns[1], "/l"), "o2")
#   # adjust rate to mg/l oxygen
#   rate.adj <- convert_DO(rate.adj, ox.unit, "mg/l", S = S, t = t, P = P)
#
#   # now convert to output unit
#   rate.out <- convert_rate(rate.adj, oxy.unit = "mg/l", time.unit = "hour", output.unit = out.unit,
#                            volume = 1, mass = NULL, area = 1,
#                            S = S, t = t, P = P,
#                            plot = FALSE)
# }
