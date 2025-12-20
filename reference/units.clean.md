# Cleans units from units.val to remove the suffix (.o2, .flow, etc)

These are the 'clean' or parsed names we want to use in outputs. All
input units get parsed to these.

## Usage

``` r
# S3 method for class 'clean'
units(unit, is)
```

## Details

Could make code much simpler with a regex for everything after the "."
but i like the specificity of this.
