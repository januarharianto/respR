#' Respirometry data of the sea urchin, *Heliocidaris Erythrogramma*
#'
#' Multiple measurements of oxygen consumption in a single sea urchin,
#' *Heliocidaris erythrogramma*, obtained using intermittent flow respirometry.
#' The experiment was conducted at the Sydney Institute of Marine Science in
#' Sydney, Australia. There are a total of 3 replicates where the respirometry
#' chamber was flushed. Data was collected using a Vernier Optical DO probe
#' (ODO-BTA).
#'
#' - Dissolved oxygen units: mg/L
#' - Time units: seconds
#' - Chamber volume (L): 2.379
#' - Specimen ash-free dry mass (kg): 0.006955
#'
#' @md
#' @author Nicholas Carey
#' @format A data frame object consisting of 2 columns (time and dissolved
#'   oxygen) and 4831 rows (approx 80 min of data).
"intermittent.rd"



#' Multi-column respirometry data of the sea urchin, *Heliocidaris
#' Erythrogramma*, including background respiration
#'
#' Oxygen consumption data of 16 individual *Heliocidaris erythrogramma*
#' specimens. In addition, there are 2 measurements of background respiration.
#' These were trial runs to determine the precision of the new Vernier Optical
#' DO probes (ODO-BTA), and thus chamber volumes and specimen masses were not
#' recorded.
#'
#' - Dissolved oxygen units: mg/L
#' - Time units: minutes
#' - Chamber volumes (L): N/A
#' - Specimen masses (kg): N/A
#'
#' @md
#' @author Januar Harianto
#' @format A data frame object consisting of one column of time, 16 columns of
#'   urchin oxygen consumption (a-p) and 2 columns of background oxygen
#'   consumption (b1 & b2). There are 271 rows of data spanning 45 minutes.
#'
"urchins.rd"



#' Respirometry data of the squid, *Doryteuthis opalescens*, with P_crit
#'
#' A single experiment on the squid species *Doryteuthis opalescens* in a
#' Loligo Systems swim tunnel and Witrox oxygen probe system. Experiment
#' conducted at Hopkins Marine Station, Stanford University, Pacific Grove,
#' California.
#'
#' - Dissolved oxygen units: mg/L
#' - Time units: seconds
#' - Chamber volume (L): 12.3
#' - Specimen wet mass (kg): 0.02141
#'
#' Data kindly supplied by Ben Burford, Hopkins Marine Station,
#' Stanford University. <bburford@stanford.edu>
#'
#' @md
#' @author Ben Burford
#' @format A data frame object consisting of 2 columns (time and dissolved
#'   oxygen) and 34120 rows (approx 9.5h of data).
"squid.rd"



#' Respirometry data of the sardine, *Sardinops sagax*
#'
#' A single experiment on the sardine species *Sardinops sagax* in a Loligo
#' Systems swim tunnel and Witrox oxygen probe system. Oxygen content recorded
#' in \% oxygen saturation. Temperature, salinity and atmospheric pressure are
#' supplied below to allow for conversion to concentration units.
#'
#' Experiment conducted at Hopkins Marine Station, Stanford University, Pacific
#' Grove, California.
#'
#' - Dissolved oxygen units: \% $O_2$ saturation
#' - Temperature (°C): 15
#' - Salinity: 35
#' - Atm. Pressure (kPa): 1.013253
#' - Time units: seconds
#' - Chamber volume (L): 12.3
#' - Specimen wet mass (kg): 0.0477
#'
#' @md
#' @author Nicholas Carey
#' @format A data frame object consisting of 2 columns (time and \% oxygen
#'   saturation) and 7513 rows (approx 2.1h of data).
"sardine.rd"


#' Flowthrough respirometry data on the chiton, *Mopalia lignosa*
#'
#' A single experiment on the chiton species *Mopalia lignosa* in a custom-built
#' flowthrough respirometry system. Conducted at University of British Columbia,
#' Vancouver, BC, Canada.
#'
#' - Dissolved oxygen units: mg/L
#' - Time units: seconds
#' - Flow rate (L/min): 0.00234
#' - Inflow oxygen concentration (mean, mg/L): 8.88
#' - Specimen ash-free dry mass (kg): 0.000070
#'
#' @md
#' @author Nicholas Carey
#' @format A data frame object consisting of and 935 rows (approx 16 mins of
#'   data),and 4 columns: time, oxygen inflow and outflow concentrations, and
#'   oxygen delta (the difference between inflow and outflow concentrations).
"flowthrough.rd"


#' Output object for the function `test_lin()` for "default" data
#'
#' This is the output data for the performance check on `auto_rate()`'s linear
#' detection algorithm. Data was randomly-generated, then tested, using the code
#' `test_lin(reps = 1000, len = 300, type = "default")`. Contain 1,000
#' iterations of the same test. For more information about the properties of
#' "default" data, please read our vignette on `auto_rate()`.
#'
#' @author Januar Harianto
#' @format A list object containing a dataframe object and an `lm` object.
"default1k"


#' Output object for the function `test_lin()` for "corrupted" data
#'
#' This is the output data for the performance check on `auto_rate()`'s linear
#' detection algorithm. Data was randomly-generated, then tested, using the code
#' `test_lin(reps = 1000, len = 300, type = "corrupted")`. Contain 1,000
#' iterations of the same test. For more information about the properties of
#' "corrupted" data, please read our vignette on `auto_rate()`.
#'
#' @author Januar Harianto
#' @format A list object containing a dataframe object and an `lm` object.
"corrupted1k"


#' Output object for the function `test_lin()` for "segmented" data
#'
#' This is the output data for the performance check on `auto_rate()`'s linear
#' detection algorithm. Data was randomly-generated, then tested, using the code
#' `test_lin(reps = 1000, len = 300, type = "segmented")`. Contain 1,000
#' iterations of the same test. For more information about the properties of
#' "segmented" data, please read our vignette on `auto_rate()`.
#'
#' @author Januar Harianto
#' @format A list object containing a dataframe object and an `lm` object.
"segmented1k"
