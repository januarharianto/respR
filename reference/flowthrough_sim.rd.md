# Flowthrough respirometry data with increasing background rate

A simulated dataset for testing and demonstrating flowthrough
respirometry analyses and background adjustment when the background
respiration rate increases over the course of the experiment. Contains
one column of numeric time data (`$num.time`), one column of specimen
outflow oxygen concentrations (`$oxy.out.spec`), one column of control
or "blank" chamber outflow oxygen concentrations (`$oxy.out.blank`), and
one column of inflow oxygen concentrations as recorded from a shared
header tank (`$oxy.header`) supplying both chambers.

## Usage

``` r
flowthrough_sim.rd
```

## Format

A data frame object consisting of 3740 rows (approx 62 mins of data),and
4 columns: time (col 1), specimen oxygen outflow concentrations (col 2),
control/blank chamber oxygen outflow concentrations (col 3), and inflow
concentrations recorded from a shared header tank (col 4).

## Details

- Dissolved oxygen units: `mg/L`

- Time units: `seconds`

## Author

Nicholas Carey
