# Checks if an oxygen concentration/pressure unit or metabolic rate unit requires temperature, salinity, and pressure to convert to another unit.

`type` should be `"oxy"` or `"mr"`.

## Usage

``` r
StP.check(unit, type)
```

## Details

Returns `TRUE` if it requires t,S,P or `FALSE` if not.
