# Check unit string against allowed values. See util_fns.R file for regex patterns

These names (before the .o2, .vol, etc.) are the 'clean' or parsed names
we want to use in outputs. All inputs units get parsed to these in
units.clean

## Usage

``` r
# S3 method for class 'val'
units(unit, is, msg = "units.val")
```
