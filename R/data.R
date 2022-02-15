#' Respirometry data of the sea urchin, *Heliocidaris Erythrogramma*
#'
#' Multiple measurements of oxygen consumption in a single sea urchin,
#' *Heliocidaris erythrogramma*, obtained using intermittent flow respirometry.
#' The experiment was conducted at the Sydney Institute of Marine Science in
#' Sydney, Australia. There are a total of 3 replicates showing declining oxygen,
#' separated by flushes where new water was added showing increasing oxygen.
#' Data was collected using a Vernier Optical DO probe (ODO-BTA).
#'
#' - Dissolved oxygen units: `mg/L`
#'
#' - Time units: `seconds`
#'
#' - Chamber volume (L): `2.379`
#'
#' - Specimen ash-free dry mass (kg): `0.006955`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 2 columns (time and dissolved
#'   oxygen) and 4831 rows (approx 80 min of data).
"intermittent.rd"



#' Multi-column respirometry data of the sea urchin, *Heliocidaris
#' Erythrogramma*, including background respiration
#'
#' Oxygen consumption data of 16 individual *Heliocidaris erythrogramma*
#' specimens. In addition, there are two columns of background respiration.
#'
#' - Dissolved oxygen units: `mg/L`
#'
#' - Time units: `minutes`
#'
#' - Volume (L): `1.09`
#'
#' - Temperature (°C): `20`
#'
#' - Salinity: `30`
#'
#' - Atm. Pressure (bar): `1.01`
#'
#' @author Januar Harianto
#' @format A data frame object consisting of one column of time, 16 columns of
#'   urchin oxygen consumption (`a` to `p`) and 2 columns of background oxygen
#'   consumption (`b1` & `b2`). There are 271 rows of data spanning 45 minutes.
"urchins.rd"


#' Respirometry data of the squid, *Doryteuthis opalescens*
#'
#' A single experiment on the squid species *Doryteuthis opalescens* in a Loligo
#' Systems swim tunnel and Witrox oxygen probe system. Oxygen was recorded to
#' very low concentrations, making this dataset suitable for determining PCrit.
#' Experiment conducted at Hopkins Marine Station, Stanford University, Pacific
#' Grove, California. Mean temperature, salinity and atmospheric pressure are
#' supplied below to allow for conversion to oxygen concentration units.
#'
#' - Dissolved oxygen units: `mg/L`
#'
#' - Time units: `seconds`
#'
#' - Chamber volume (L): `12.3`
#'
#' - Specimen wet mass (kg): `0.02141`
#'
#' - Temperature (°C): `14`
#'
#' - Salinity: `35`
#'
#' - Atm. Pressure (bar): `1.013253`
#'
#' Data kindly supplied by Ben Burford, Hopkins Marine Station, Stanford
#' University.
#'
#' @author Ben Burford
#' @format A data frame object consisting of 2 columns (`$Time` and `$Oxygen`)
#'   and 34120 rows (approx 9.5h of data).
"squid.rd"



#' Respirometry data of the sardine, *Sardinops sagax*
#'
#' A single experiment on the sardine species *Sardinops sagax* in a Loligo
#' Systems swim tunnel and Witrox oxygen probe system. There are three columns:
#' `$Time` in seconds, `$Oxygen` content recorded in percent air saturation, and
#' `$Temperature` in °C. Mean temperature, salinity and atmospheric pressure are
#' supplied below to allow for conversion to oxygen concentration units.
#'
#' Experiment conducted at Hopkins Marine Station, Stanford University, Pacific
#' Grove, California.
#'
#' - Dissolved oxygen units: `% air saturation`
#'
#' - Time units: `seconds`
#'
#' - Chamber volume (L): `12.3`
#'
#' - Specimen wet mass (kg): `0.0477`
#'
#' - Temperature (°C): `14.8`
#'
#' - Salinity: `35`
#'
#' - Atm. Pressure (bar): `1.013253`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 3 columns (time, % air saturation
#'   and temperature) and 7513 rows (approx 2.1h of data).
"sardine.rd"


#' Flowthrough respirometry data on the chiton, *Mopalia lignosa*
#'
#' A single experiment on the chiton species *Mopalia lignosa* in a custom-built
#' flowthrough respirometry system. Conducted at University of British Columbia,
#' Vancouver, BC, Canada.
#'
#' - Dissolved oxygen units: `mg/L`
#'
#' - Time units: `seconds`
#'
#' - Flow rate (`mL/min`): `2.34`
#'
#' - Inflow oxygen concentration (calculated assuming 100% air saturated, `mg/L`): `8.919`
#'
#' - Specimen ash-free dry mass (`kg`): `0.000070`
#'
#' - Temperature (°C): `t = 12`
#'
#' - Salinity: `S = 30`
#'
#' - Atm. Pressure (bar): `P = 1.013`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 935 rows (approx 16 mins of
#'   data),and 4 columns: time, oxygen inflow and outflow concentrations, and
#'   oxygen delta (the outflow minus inflow concentrations).
"flowthrough.rd"


#' Multi-column flowthrough respirometry data
#'
#' A semi-simulated dataset for testing and demonstrating flowthrough
#' respirometry analyses. Contains one column of numeric time data (col 1 in
#' mins), four columns of outflow oxygen concentrations (cols 2:5), four columns
#' of inflow oxygen concentrations (cols 6:9), and four columns of delta oxygen
#' concentrations (cols 10:13, which is simply the numeric difference between
#' paired columns of outflow and inflow). There is also a column of inflow
#' oxygen concentrations as recorded from a shared header tank (col 14,
#' `$oxy.header`) supplying all chambers, to use as an alternative to the
#' individual inflow oxygen recordings. Lastly, there is a column of temperature
#' data (col 15, `$temperature` in °C).
#'
#' Outflow (2:5) and inflow (6:9) columns are paired, with the first three
#' containing specimens, and the fourth an empty control respirometer, or
#' "blank" experiment (oxy.out.blank, oxy.in.blank) to determine background
#' respiration.
#'
#' The third paired dataset (col 4 and col 8 pair) has a period of higher rates
#' at around the 40 minute timepoint, where the specimen increases its activity
#' then slowly recovers to routine respiration levels.
#'
#' - Dissolved oxygen units: `%Air`
#'
#' - Time units: `mins`
#'
#' - Flow rate (`L/min`): `0.1`
#'
#' - Specimen masses: (`kg`): `0.013, 0.015, 0.020`
#'
#' - Mean temperature (°C): `t = 18`
#'
#' - Salinity: `S = 0`, i.e. freshwater
#'
#' - Atmospheric pressure (bar): `P = 1.013`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 3740 rows (approx 62 mins of
#'   data),and 15 columns: time (col 1), oxygen outflow concentrations (cols
#'   2,3,4,5), inflow concentrations (cols 6,7,8,9 each paired with the
#'   respective outflow column, the fourth being a control), delta oxygen values
#'   (cols 10,11,12,13 or difference between outflow and inflow concentrations),
#'   inflow concentrations recorded in a shared header tank (col 14), and
#'   temperature (col 15).
"flowthrough_mult.rd"

#' Flowthrough respirometry data with increasing background rate
#'
#' A simulated dataset for testing and demonstrating flowthrough respirometry
#' analyses and background adjustment when the background respiration rate
#' increases over the course of the experiment. Contains one column of numeric
#' time data (`$num.time`), one column of specimen outflow oxygen concentrations
#' (`$oxy.out.spec`), one column of control or "blank" chamber outflow oxygen
#' concentrations (`$oxy.out.blank`), and one column of inflow oxygen
#' concentrations as recorded from a shared header tank (`$oxy.header`)
#' supplying both chambers.
#'
#' - Dissolved oxygen units: `mg/L`
#'
#' - Time units: `seconds`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 3740 rows (approx 62 mins of
#'   data),and 4 columns: time (col 1), specimen oxygen outflow concentrations
#'   (col 2), control/blank chamber oxygen outflow concentrations (col 3), and
#'   inflow concentrations recorded from a shared header tank (col 4).
"flowthrough_sim.rd"

#' Output objects for the function `test_lin`
#'
#' This data contains the results of 9 separate performance checks
#' on the [`auto_rate()`] linear detection algorithm (i.e. `method = "linear"`).
#' These test results are used to assess and discuss the performance of
#' `auto_rate` in the online vignette found here: .
#'
#' @author Januar Harianto
#' @format List of multiple output objects of class `test_lin`.
"test_lin_data"

#' Respirometry data of a zebrafish, *Danio rerio*
#'
#' Multiple measurements (106 replicates, plus initial and end background
#' measurements) of oxygen consumption in a zebrafish, *Danio rerio*, obtained
#' using intermittent flow respirometry. Data kindly provided by Davide
#' Thambithurai (University of Glasgow). Note, the data has been injected with
#' random noise, and volume and mass below are not the actual values from the
#' experiment.
#'
#' - Dissolved oxygen units: `mg/L`
#'
#' - Time units: `seconds`
#'
#' - Chamber volume (L): `0.12`
#'
#' - Specimen wet mass (kg): `0.0009
#'
#' - Temperature (°C): `25`
#'
#' - Salinity: `0`
#'
#' - Atm. Pressure (bar): `1.013253`
#'
#' Replicate structure (Rows - Experiment section):
#'
#' - `1:4999`      - Start background recording
#'
#' - `5000:5839`   - First replicate for MMR (14 mins duration)
#'
#' - `5840:75139`  - 105 further replicates of 11 minutes duration each (660 rows)
#'
#' - `75140:79251` - End background recording
#'
#' Each replicate comprises a measurement period (12 minutes for replicate 1, 9
#' minutes for all others) plus 2 minutes flush.
#'
#' @author Davide Thambithurai, University of Glasgow
#' @format A data frame object consisting of 2 columns (time and dissolved
#'   oxygen) and 79251 rows (approx 22h of data).
"zeb_intermittent.rd"


#' Background respirometry data (constant)
#'
#' Background oxygen consumption data. After the initial 30 minutes, data shows
#' a generally constant background rate. Taken from a Loligo swim tunnel
#' background recording. Oxygen recorded via a Witrox sensor in % air saturation
#' over nearly 6 hours at 1 second intervals. Data is from a real experiment.
#'
#' - Dissolved oxygen units: `% Air Saturation`
#'
#' - Time units: `seconds`
#'
#' - Swim tunnel volume (L): `12.3`
#'
#' - Temperature (°C): `14.5`
#'
#' - Salinity: `34`
#'
#' - Atm. Pressure (bar): `1.013253`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 20664 rows (approx 6 h of data),and
#'   2 columns: `$Time` in seconds, `$Oxygen` in % air saturation.
"background_con.rd"

#' Background respirometry data (linear)
#'
#' Background oxygen consumption data. After initial 30 minutes, data shows a
#' background rate which increases linearly with respect to time. Taken from a
#' Loligo swim tunnel background recording. Oxygen recorded via a Witrox sensor
#' in % air saturation over nearly 6 hours at 1 second intervals. Data is from a
#' real experiment, but has been manipulated to show a linear increase in
#' background rate for testing purposes.
#'
#' - Dissolved oxygen units: `% Air Saturation`
#'
#' - Time units: `seconds`
#'
#' - Swim tunnel volume (L): `12.3`
#'
#' - Temperature (°C): `14.5`
#'
#' - Salinity: `34`
#'
#' - Atm. Pressure (bar): `1.013253`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 20664 rows (approx 6 h of data),and
#'   2 columns: `$Time` in seconds, `$Oxygen` in % air saturation.
"background_lin.rd"


#' Background respirometry data (exponential)
#'
#' Background oxygen consumption data. Data shows a background rate which
#' increases exponentially with respect to time. Taken from a Loligo swim tunnel
#' background recording. Oxygen recorded via a Witrox sensor in % air saturation
#' over nearly 6 hours at 1 second intervals. Data is from a real experiment,
#' but oxygen decrease curve has been exaggerated to impose an exponential
#' increase in background consumption for testing purposes.
#'
#' - Dissolved oxygen units: `% Air Saturation`
#'
#' - Time units: `seconds`
#'
#' - Swim tunnel volume (L): `12.3`
#'
#' - Temperature (°C): `14.5`
#'
#' - Salinity: `34`
#'
#' - Atm. Pressure (bar): `1.013253`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 20664 rows (approx 6 h of data),and
#'   2 columns: `$Time` in seconds, `$Oxygen` in % air saturation.
"background_exp.rd"


#' Oxygen production respirometry data
#'
#' Data from a respirometry experiment on algae which shows oxygen production
#' over time.
#'
#' - Dissolved oxygen units: `% Air Saturation`
#'
#' - Time units: `hours`
#'
#' - Respirometer volume (L): `0.1`
#'
#' - Temperature (°C): `12`
#'
#' - Salinity: `30`
#'
#' @author Nicholas Carey
#' @format A data frame object consisting of 1200 rows (20 h of data),and
#'   2 columns: `$Time` in hours, `$Oxygen` in % air saturation.
"algae.rd"
