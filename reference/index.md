# Package index

## Pre-processing

Functions for importing, processing, visualising and inspecting data:

- [`import_file()`](https://januarharianto.github.io/respR/reference/import_file.md)
  : Import respirometry system raw data files (DEPRECATED)

- [`format_time()`](https://januarharianto.github.io/respR/reference/format_time.md)
  : Parse date-time data to numeric time for use in respR functions

- [`inspect()`](https://januarharianto.github.io/respR/reference/inspect.md)
  : Explore and visualise respirometry data and check for common errors

- [`inspect.ft()`](https://januarharianto.github.io/respR/reference/inspect.ft.md)
  : Explore and visualise flowthrough respirometry data and check for
  errors

- [`subsample()`](https://januarharianto.github.io/respR/reference/subsample.md)
  : Subsample a data frame object

- [`subset_data()`](https://januarharianto.github.io/respR/reference/subset_data.md)
  :

  Subset a `data.frame`, `inspect`, or `inspect.ft` object

## Analytical

Functions for calculating rates of oxygen consumption or production:

- [`auto_rate()`](https://januarharianto.github.io/respR/reference/auto_rate.md)
  : Automatically determine most linear, highest, lowest and rolling
  oxygen uptake or production rates
- [`auto_rate.int()`](https://januarharianto.github.io/respR/reference/auto_rate.int.md)
  : Run auto_rate on multiple replicates in intermittent-flow
  respirometry data
- [`calc_rate()`](https://januarharianto.github.io/respR/reference/calc_rate.md)
  : Calculate rate of change in oxygen over time
- [`calc_rate.int()`](https://januarharianto.github.io/respR/reference/calc_rate.int.md)
  : Extract rates from multiple replicates in intermittent-flow
  respirometry data
- [`calc_rate.bg()`](https://januarharianto.github.io/respR/reference/calc_rate.bg.md)
  : Calculate background oxygen uptake or input rates
- [`calc_rate.ft()`](https://januarharianto.github.io/respR/reference/calc_rate.ft.md)
  : Calculate rate of change in oxygen from flowthrough respirometry
  data
- [`oxy_crit()`](https://januarharianto.github.io/respR/reference/oxy_crit.md)
  : Calculate critical oxygen values, such as PCrit

## Adjust

Functions for rate adjustments:

- [`adjust_rate()`](https://januarharianto.github.io/respR/reference/adjust_rate.md)
  : Adjust rates to account for background respiration or oxygen flux.
- [`adjust_rate.ft()`](https://januarharianto.github.io/respR/reference/adjust_rate.ft.md)
  : Adjust rates in flowthrough respirometry to account for background
  respiration or oxygen flux.

## Convert

Functions for rate and unit conversions:

- [`convert_rate()`](https://januarharianto.github.io/respR/reference/convert_rate.md)
  : Convert a unitless oxygen rate value to absolute, mass-specific or
  area-specific rate
- [`convert_rate.ft()`](https://januarharianto.github.io/respR/reference/convert_rate.ft.md)
  : Convert a unitless oxygen rate value from flowthrough respirometry
  to absolute, mass-specific or area-specific rates
- [`convert_DO()`](https://januarharianto.github.io/respR/reference/convert_DO.md)
  : Convert between units of dissolved oxygen
- [`convert_MR()`](https://januarharianto.github.io/respR/reference/convert_MR.md)
  : Convert between units of absolute, mass-specific, or area-specific
  metabolic rates
- [`convert_val()`](https://januarharianto.github.io/respR/reference/convert_val.md)
  : Convert values of temperature, volume, mass, area, and atmospheric
  pressure to different units
- [`unit_args()`](https://januarharianto.github.io/respR/reference/unit_args.md)
  : Print examples of unit inputs

## Summarise

Functions for rate filtering and summarising:

- [`select_rate()`](https://januarharianto.github.io/respR/reference/select_rate.md)
  [`select_rate.ft()`](https://januarharianto.github.io/respR/reference/select_rate.md)
  : Select rate results based on a range of criteria

## Data

Example data files:

- [`algae.rd`](https://januarharianto.github.io/respR/reference/algae.rd.md)
  : Oxygen production respirometry data

- [`background_con.rd`](https://januarharianto.github.io/respR/reference/background_con.rd.md)
  : Background respirometry data (constant)

- [`background_exp.rd`](https://januarharianto.github.io/respR/reference/background_exp.rd.md)
  : Background respirometry data (exponential)

- [`background_lin.rd`](https://januarharianto.github.io/respR/reference/background_lin.rd.md)
  : Background respirometry data (linear)

- [`flowthrough.rd`](https://januarharianto.github.io/respR/reference/flowthrough.rd.md)
  :

  Flowthrough respirometry data on the chiton, *Mopalia lignosa*

- [`flowthrough_mult.rd`](https://januarharianto.github.io/respR/reference/flowthrough_mult.rd.md)
  : Multi-column flowthrough respirometry data

- [`flowthrough_sim.rd`](https://januarharianto.github.io/respR/reference/flowthrough_sim.rd.md)
  : Flowthrough respirometry data with increasing background rate

- [`intermittent.rd`](https://januarharianto.github.io/respR/reference/intermittent.rd.md)
  :

  Respirometry data of the sea urchin, *Heliocidaris Erythrogramma*

- [`sardine.rd`](https://januarharianto.github.io/respR/reference/sardine.rd.md)
  :

  Respirometry data of the sardine, *Sardinops sagax*

- [`squid.rd`](https://januarharianto.github.io/respR/reference/squid.rd.md)
  :

  Respirometry data of the squid, *Doryteuthis opalescens*

- [`urchins.rd`](https://januarharianto.github.io/respR/reference/urchins.rd.md)
  :

  Multi-column respirometry data of the sea urchin, *Heliocidaris
  Erythrogramma*, including background respiration

- [`zeb_intermittent.rd`](https://januarharianto.github.io/respR/reference/zeb_intermittent.rd.md)
  :

  Respirometry data of a zebrafish, *Danio rerio*
