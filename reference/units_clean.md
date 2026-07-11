# Cleans units from units_val to remove the suffix (.o2, .flow, etc)

These are the 'clean' or parsed names we want to use in outputs. All
input units get parsed to these.

## Usage

``` r
units_clean(unit, is)
```

## Details

Could make code much simpler with a regex for everything after the "."
but i like the specificity of this.
