# Print examples of unit inputs

This is a basic function with no inputs. It prints to the console the
units that can be used in the functions
[`convert_DO()`](https://januarharianto.github.io/respR/reference/convert_DO.md),
[`convert_MR()`](https://januarharianto.github.io/respR/reference/convert_MR.md),
[`convert_rate()`](https://januarharianto.github.io/respR/reference/convert_rate.md),
and
[`convert_rate.ft()`](https://januarharianto.github.io/respR/reference/convert_rate.ft.md).

## Usage

``` r
unit_args()
```

## Value

A print out to the console of accepted units

## Details

Note that some oxygen unit conversions require temperature (`t`),
salinity (`S`), and atmospheric pressure (`P`) to be specified.

Note the difference between percent air saturation (`%Air`), where air
saturated water is ~100%, and percent oxygen saturation (`%Oxy`), where
air saturated water is ~20.946% *oxygen* saturated. In other words,
`%Oxy = %Air x 0.20946`.

For most units a fuzzy string matching algorithm is used to accept
different formatting styles. For example, `"mg/l"`, `"mg/L"`, `"mgL-1"`,
`"mg l-1"`, `"mg.l-1"` are all parsed the same.

### [`convert_DO()`](https://januarharianto.github.io/respR/reference/convert_DO.md)

#### Oxygen concentration or pressure units for `from` and `to`:

Oxygen concentration units. Should use SI units (`L` or `kg`) for the
denominator.

Do *NOT* require `t`, `S` and `P` for conversions:

- `"mg/L"`, `"ug/L"`, `"mol/L"`, `"mmol/L"`, `"umol/L"`, `"nmol/L"`,
  `"pmol/L"`

Require `t`, `S` and `P` for conversions:

- `"uL/L"`, `"mL/L"`, `"mm3/L"`, `"cm3/L"`, `"mg/kg"`, `"ug/kg"`,
  `"mol/kg"`, `"mmol/kg"`, `"umol/kg"`, `"nmol/kg"`, `"pmol/kg"`,
  `"uL/kg"`, `"mL/kg"`, `"ppm"` (i.e. parts per million, equivalent to
  `mg/kg`).

Percentage saturations (require `t`, `S` and `P`):

`"%Air"` (i.e. % Air Saturation), `"%Oxy"` (i.e. % Oxygen Saturation)

Pressure units (require `t`, `S` and `P`):

`"Torr"`, `"hPa"`, `"kPa"`, `"mmHg"`, `"inHg"`

### [`convert_rate()`](https://januarharianto.github.io/respR/reference/convert_rate.md) and [`convert_rate.ft()`](https://januarharianto.github.io/respR/reference/convert_rate.ft.md)

#### Oxygen concentration or pressure units for `oxy.unit`:

See above.

#### Time units for `time.unit` or as part of `flowrate.unit`:

- `"sec", "min", "hour", "day"`

#### Volume units for use as part of `flowrate.unit` (`convert_rate.ft` only):

For example, in `'ml/min'`, `'L/s'`, etc.

- `"uL"`, `"mL"`, `"L"`

### Metabolic rate units

Combining units for `output.unit` in
[`convert_rate()`](https://januarharianto.github.io/respR/reference/convert_rate.md)
and
[`convert_rate.ft()`](https://januarharianto.github.io/respR/reference/convert_rate.ft.md),
or for use in
[`convert_MR()`](https://januarharianto.github.io/respR/reference/convert_MR.md),
must follow these orders:

- Absolute rates: `Oxygen/Time` e.g. `"mg/s"`, `"umol/min"`, `"mL/h"`

- Mass-specific rates: `Oxygen/Time/Mass` e.g. `"mg/s/ug"`,
  `"umol/min/g"`, `"mL/h/kg"`

- Area-specific rates: `Oxygen/Time/Area` e.g. `"mg/s/mm2"`,
  `"umol/min/cm2"`, `"mL/h/m2"`

**Oxygen amount units:**

- `"ug"`, `"mg"`, `"pmol"`, `"nmol"`, `"umol"`, `"mmol"`, `"mol"`,
  `"uL"`, `"mL"`, `"mm3"`, `"cm3"`

Note `"mm3"` and `"cm3"` (i.e. `cc`) are used in some older
publications. These are equivalent to `"uL"` and `"mL"` respectively.

**Time units:**

- `"sec"`, `"min"`, `"hour"`, `"day"`

**Mass units for mass-specific rates:**

- `"ug"`, `"mg"`, `"g"`, `"kg"`

**Area units for area-specific rates:**

- `"mm2"`, `"cm2"`, `"m2"`, `"km2"`

## Examples

``` r

# Run the function:
unit_args()
#> Note: A string-matching algorithm is used to identify units. 
#> Example 1: These are recognised as the same: 'mg/L', 'mg/l', 'mg L-1', 'mg per litre', 'mg.L-1'
#> Example 2: These are recognised as the same: 'Hour', 'hr', 'h'
#> 
#> # Input Units # --------------------------------------
#> Oxygen concentration units should use SI units (`L` or `kg`) for the denominator.
#> 
#> Oxygen Concentration or Pressure Units - Do not require t, S and P
#> [1] "mg/L"   "ug/L"   "mol/L"  "mmol/L" "umol/L" "nmol/L" "pmol/L"
#> Oxygen Concentration or Pressure Units - Require t, S and P
#>  [1] "uL/L"    "mL/L"    "mm3/L"   "cm3/L"   "cc/L"    "mg/kg"   "ug/kg"   "ppm"     "mol/kg"  "mmol/kg" "umol/kg" "nmol/kg" "pmol/kg" "uL/kg"   "mL/kg"   "%Air"    "%Oxy"    "Torr"    "hPa"     "kPa"     "mmHg"    "inHg"   
#> 
#> Volume units for use in flow rates in calc_rate.ft and convert_rate.ft
#> (e.g. as in 'ml/min', 'L/s', etc.)
#> [1] "uL" "mL" "L" 
#> 
#> Time units (for 'time.unit' or as part of 'flowrate.unit')
#> [1] "sec"  "min"  "hour" "day" 
#> 
#> Mass units
#> [1] "ug" "mg" "g"  "kg"
#> 
#> Area units
#> [1] "mm2" "cm2" "m2"  "km2"
#> 
#> # Metabolic Rate Units # -----------------------------
#> For use in 'convert_rate', 'convert_rate.ft', 'convert_MR'
#> 
#> Must be in correct order:
#> Absolute rates:        Oxygen/Time       e.g. 'mg/sec',     'umol/min',     'mL/h'
#> Mass-specific rates:   Oxygen/Time/Mass  e.g. 'mg/sec/ug',  'umol/min/g',   'mL/h/kg'
#> Area-specific rates:   Oxygen/Time/Area  e.g. 'mg/sec/mm2', 'umol/min/cm2', 'mL/h/m2'
#> 
#> Output Oxygen amount units
#>  [1] "ug"   "mg"   "pmol" "nmol" "umol" "mmol" "mol"  "uL"   "mL"   "mm3"  "cm3" 
#> 
#> Output Time units
#> [1] "sec"  "min"  "hour" "day" 
#> 
#> Output Mass units for mass-specific rates
#> [1] "ug" "mg" "g"  "kg"
#> 
#> Output Area units for surface area-specific rates
#> [1] "mm2" "cm2" "m2"  "km2"
```
