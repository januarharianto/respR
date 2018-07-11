# Version 1.0.0
The big update! Maybe it's time to submit to CRAN?

- UPDATE: We edited some examples to reduce processing times during R CMD checks.
- UPDATE: The packages `dplyr` and `magrittr` are now imported into `respR`.
- UPDATE: Now requires R 3.5.0
- NEW: `import_file()`: automatically import and format raw files from common devices. This function is not complete but needs to be put out there for user feedback. It's stable, just doesn't support all available machines yet. We expect this function to be improved dramatically once users send in feedback and sample files.
- FIX: `dplyr` pipes now work after `print()` and `summary()` commands. Will add support for `plot()` depending on feedback.
- FIX: `adjust_rate()`: minor print issue (new line required) that was messing with the console output.
- UPDATE: `adjust_rate()` documentation has been updated.

# Version 0.1.0
- Updates to documentation and a couple of stability fixes to improve performance. We will push the CRAN submission at version 1.0.0 now, while we work on a few more things.
- FIX: `inspect()` should now work with pipes.
- FIX: `inspect()` had issues with `data.table` objects, so we switched to `data.frame`.

# Version 0.0.7
- HOTFIX: `calc_rate.bg()` did not subset data properly in certain usage scenarios (see #49)

# Version 0.0.6
We are getting close to a 0.1 release, which will be ready for CRAN.

- HOTFIX: More robust method of parsing data-time data to start at the defined start time in `format_time()`, which fixes some errors with zeroing in some numeric data.

# Version 0.0.5
- NEW: Added new function `format_time()`. This function is a wrapper for `lubridate` functions and is used specifically to convert date-time to numeric time.
- NEW: Added replacement function `inspect()` that contains much cleaner code. The old function `inspect_data()` was tough to fix. It will be deprecated in a future update, but will still be supported for at least 18 months after we announce the deprecation. For now, users need not worry about using either function.
- Updated: `calc_rate()`, `calc_rate.bg()`, `calc_rate.ft()`, and `auto_rate()` are updated to work with `inspect()`. (TODO: `calc_rate.ft()` still needs to be fixed.)
- NEW: Vignette for `auto_rate()` is now available. 
- NEW: Performance benchmark vignette for `auto_rate()`'s linear detection method is now available.
- NEW: `sim_data()`: simulate data for benchmark analyses.
- NEW: `test_lin()`: perform benchmark analyses specific for `auto_rate()`'s linear detection (i.e. `methods = "linear"`).
- UPDATE: All plot outputs in the package have been updated. They are now "cleaner". Users are encouraged to provide feedback on our plot outputs as we can update them easily without changing the underlying analyses.
- FIX: Rogue parenthesis code in `auto_rate()` fixed (it was messing with `print()` commands). 

# Version 0.0.4
- HOTFIX: `convert_rate()` fails when output contains "mmol". Fixed.

# Version 0.0.3
- FIX: More updates to documentation.
- FIX: Workaround to show `pcrit()` example on website.

# Version 0.0.2
- FIX: Major updates to documentation.
- NEW: `subset_data()` now contains examples.
- NEW: Flowthrough vignette is now integrated into website. 
- FIX: `inspect_data()` plot output was plotting only columns 1 and 2 by default, even when other columns are selected in arguments.
- FIX: `convert_rate()` was not accepting objects of class `calc_rate.ft`.

# Version 0.0.1
- Stable release.

# Version 0.0.0.9000
- Dev release on Github.
