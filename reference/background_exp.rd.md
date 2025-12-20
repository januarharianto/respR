# Background respirometry data (exponential)

Background oxygen consumption data. Data shows a background rate which
increases exponentially with respect to time. Taken from a Loligo swim
tunnel background recording. Oxygen recorded via a Witrox sensor in %
air saturation over nearly 6 hours at 1 second intervals. Data is from a
real experiment, but oxygen decrease curve has been exaggerated to
impose an exponential increase in background consumption for testing
purposes.

## Usage

``` r
background_exp.rd
```

## Format

A data frame object consisting of 20664 rows (approx 6 h of data),and 2
columns: `$Time` in seconds, `$Oxygen` in % air saturation.

## Details

- Dissolved oxygen units: `% Air Saturation`

- Time units: `seconds`

- Swim tunnel volume (L): `12.3`

- Temperature (°C): `14.5`

- Salinity: `34`

- Atm. Pressure (bar): `1.013253`

## Author

Nicholas Carey
