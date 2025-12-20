# Respirometry data of a zebrafish, *Danio rerio*

Multiple measurements (106 replicates, plus initial and end background
measurements) of oxygen consumption in a zebrafish, *Danio rerio*,
obtained using intermittent flow respirometry. Data kindly provided by
Davide Thambithurai (University of Glasgow). Note, the data has been
injected with random noise, and volume and mass below are not the actual
values from the experiment.

## Usage

``` r
zeb_intermittent.rd
```

## Format

A data frame object consisting of 2 columns (time and dissolved oxygen)
and 79251 rows (approx 22h of data).

## Details

- Dissolved oxygen units: `mg/L`

- Time units: `seconds`

- Chamber volume (L): `0.12`

- Specimen wet mass (kg): \`0.0009

- Temperature (°C): `25`

- Salinity: `0`

- Atm. Pressure (bar): `1.013253`

Replicate structure (Rows - Experiment section):

- `1:4999` - Start background recording

- `5000:5839` - First replicate for MMR (14 mins duration)

- `5840:75139` - 105 further replicates of 11 minutes duration each (660
  rows)

- `75140:79251` - End background recording

Each replicate comprises a measurement period (12 minutes for replicate
1, 9 minutes for all others) plus 2 minutes flush.

## Author

Davide Thambithurai, University of Glasgow
