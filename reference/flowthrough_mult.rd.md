# Multi-column flowthrough respirometry data

A semi-simulated dataset for testing and demonstrating flowthrough
respirometry analyses. Contains one column of numeric time data (col 1
in mins), four columns of outflow oxygen concentrations (cols 2:5), four
columns of inflow oxygen concentrations (cols 6:9), and four columns of
delta oxygen concentrations (cols 10:13, which is simply the numeric
difference between paired columns of outflow and inflow). There is also
a column of inflow oxygen concentrations as recorded from a shared
header tank (col 14, `$oxy.header`) supplying all chambers, to use as an
alternative to the individual inflow oxygen recordings. Lastly, there is
a column of temperature data (col 15, `$temperature` in °C).

## Usage

``` r
flowthrough_mult.rd
```

## Format

A data frame object consisting of 3740 rows (approx 62 mins of data),and
15 columns: time (col 1), oxygen outflow concentrations (cols 2,3,4,5),
inflow concentrations (cols 6,7,8,9 each paired with the respective
outflow column, the fourth being a control), delta oxygen values (cols
10,11,12,13 or difference between outflow and inflow concentrations),
inflow concentrations recorded in a shared header tank (col 14), and
temperature (col 15).

## Details

Outflow (2:5) and inflow (6:9) columns are paired, with the first three
containing specimens, and the fourth an empty control respirometer, or
"blank" experiment (oxy.out.blank, oxy.in.blank) to determine background
respiration.

The third paired dataset (col 4 and col 8 pair) has a period of higher
rates at around the 40 minute timepoint, where the specimen increases
its activity then slowly recovers to routine respiration levels.

- Dissolved oxygen units: `%Air`

- Time units: `mins`

- Flow rate (`L/min`): `0.1`

- Specimen masses: (`kg`): `0.013, 0.015, 0.020`

- Mean temperature (°C): `t = 18`

- Salinity: `S = 0`, i.e. freshwater

- Atmospheric pressure (bar): `P = 1.013`

## Author

Nicholas Carey
