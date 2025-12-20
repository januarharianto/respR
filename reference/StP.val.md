# Returns an error message if an oxygen concentration/pressure unit or metabolic rate unit requires S or t and either is NULL, or a message if it requires P and it is NULL.

`type` should be `"oxy"` or `"mr"`. `P.chk` - perform P out of normal
range check. Only want to do this once per function.

## Usage

``` r
StP.val(unit, type, S, t, P, P.chk = TRUE, msg)
```

## Details

Also returns default value of P = 1.013253 if it is NULL.
