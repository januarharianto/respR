# Import respirometry system raw data files (DEPRECATED)

Automatically import data from different respirometry hardware and
software systems. *WARNING*: This function is now deprecated and will
not be updated. See below.

## Usage

``` r
import_file(path, export = FALSE)
```

## Arguments

- path:

  string. Path to file.

- export:

  logical. If TRUE, exports the data as a `csv` to the same directory,
  as determined by the `path` parameter.

## Value

A `data.frame` object of all columned data

## Details

This function has been deprecated, will not be updated, and will be
removed entirely in the next major version of `respR` (i.e. `v3.0`,
which is not planned for any time soon, so may still be years away).

Note that use of this function to import data is *OPTIONAL* and in fact
*NOT RECOMMENDED*. `respR` has an extremely simple input data structure
requirement: paired numeric values of time and oxygen amount in any
common units in a `data.frame`. To our knowledge, every oxygen sensor
system allows you to export data in a format (e.g. `.csv`, `.txt`,
`.xlsx`) which are easy to import into `R`, and this is a basic skill
anyone using `R` should be comfortable with. `import_file` was only ever
a convenience function intended for those completely new to `R`. It is
*ALWAYS* better to import files yourself using generic functions such as
[`read.csv()`](https://rdrr.io/r/utils/read.table.html),
[`read.table()`](https://rdrr.io/r/utils/read.table.html) or `fread()`
as it gives you much more control and the ability to troubleshoot
issues.

For files currently supported, the function extracts data columns,
removes redundant rows of metadata, and generally cleans up column names
(e.g. removes whitespace and characters which cause text encoding
issues) to make the data easier to work with. Files should be sensor
system raw output files where possible; files opened and re-saved in a
different format will likely fail to import.

Currently tested and working for these files:

- Pyro Firesting

- Pyro Workbench (very experimental - likely to fail and will not be
  updated further)

- PreSens OXY10

- PreSens OXY4

- PreSens (OxyView generic, including multiplate systems)

- PreSens/Loligo 24-Well Multiplate System (output Excel files)

- MiniDOT

- Loligo AutoResp ('\_raw' files output, *not* metadata files)

- Loligo Witrox (same as AutoResp, without metadata)

- Vernier (raw qmbl, csv, or txt)

- NeoFox

- Qbox Aqua

Files with European numeric formatting (i.e. commas instead of points to
denote decimals) are supported, and will be converted to point decimals
on import. This is experimental functionality, so please provide
feedback for any files for which this might fail.

While the devices listed above are supported, the import functionality
is experimental due to limited access to sample files. Users should be
aware we have not been able to test every variation of file formats, and
should carefully check the imported data, and be prepared to import data
using other functions such as
[`read.csv()`](https://rdrr.io/r/utils/read.table.html).

### More

For additional help, documentation, vignettes, and more visit the
`respR` website at <https://januarharianto.github.io/respR/>

## Examples

``` r
if (FALSE) { # \dontrun{
# Import a file
import_file("path/to/file")

# Import a file and export it to same directory as a csv
import_file("path/to/file", export = TRUE)
} # }
```
