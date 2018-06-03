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
- NEW: `sim_data()` function: simulate data for benchmark analyses.
- NEW: `test_lin()` function: perform benchmark analyses specific for `auto_rate()`'s linear detection (i.e. `methods = "linear"`).
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
