# respR 1.1.1

# Version 1.1.0

Lots of updates! First, we have a new function, `calc_pcrit()`, which we will use to include new methods in the future. We've also made some improvements to `import_file()` with increased support to more files from Vernier, PRESENS and Loligo systems.

**Vignettes have been migrated into a new repository** on GitHub, and are now updated separately. This gives us several advantages -- the documentation can be updated without needing to compile a new package, and the size of the package is significantly reduced. Users can also contribute to the documentation more easily now that it is not tied to the package. 

### New Features

- NEW: You can now grab the reference for `respR` by running `citation("respR")`.
- NEW: Loading `respR` will print a startup message containing links to our published manuscript and vignettes. 
- NEW: `inspect()` can now plot data with multiple columns when more than 2 columns are detected. For some people, this may provide a great overview of the data.
- NEW: `calc_pcrit()` is a **new function** that will be developed in parallel to the current `pcrit()` function. In the future we intend to use `calc_pcrit()` to incorporate new methods. 
- NEW: `import_file()` supports even more files! Added more parsers for Vernier, PRESENS and Loligo systems.
- NEW: `format_time()` can now calculate time elapsed even if date information is not provided (e.g. "HMS"-only data). 
- NEW: The `by` argument in `auto_rate()`, `calc_rate()`, `calc_rate.bg()` and `subset_data()` is now more forgiving with string input values (e.g. `"o2"`, `"O2"`, `"oxygen"` and `"Oxygen"` are recognised). To achieve this we created a string matching function, `verify_by()` which uses brute force matching to recognise different ways of writing the same text.

### Fixes

- **`convert_DO()`**:
    - FIX: Argument for `"P"` (pressure) was ignored even when a value was already specified, resulting in the default used. The function will now respect the `"P"` value provided. 
    - FIX: The error message for an unrecognised unit was... cryptic. It has been changed and will now instruct the user on the proper syntax required to recognise units properly.
- **`inspect()`**:
    - FIX: Positive rates were not reflected accurately in plots.
    - FIX: Plot of rolling regression was not sensitive for smaller changes in datasets. The rolling window has been reduced to 10% of the length of the dataset to address this.
- **`convert_rate()`**:
    - FIX: Now stops if a `"mass"` argument is provided but `"output.unit"` is not a mass-specific unit. 
    - FIX: Properly accepts objects of class `calc_rate.bg`.
    - FIX: Optimised the syntax for the recognition of `output.unit` in the code.
- **`calc_rate.ft()`**:
    - FIX: Critical error in returned rates sometimes outputting values with the wrong sign.
    - FIX: Optimised code - the `"time"` argument has been removed as it was not used.
    - FIX: Properly accepts objects of class `inspect`.
- **`pcrit()`**:
    - FIX: Properly accepts objects of class `inspect` and `inspect_data`. Please note that the `inspect_data()` function is deprecated and will be removed in the near future.


# Version 1.0.5.1
This is a quick fix for images not showing in the online vignettes.

- FIX: images not showing in two vignettes. If you didn't notice it, it never happened...


# Version 1.0.5
This version is aimed at improving the functionality and usability of our conversion functions. We have decided to remove the default values for temperature and salinity inputs in `convert_DO()` and `convert_rate()` (i.e. switched `t` and `S` numerics to `NULL`). This was no easy decision, but we noticed that some users were running the functions by default without considering (or even knowing the existence of) these two *important* input variables. **With this change, existing workflows using `calc_DO()` and `calc_rate()`are likely to break**. Do note that this is a very rare modification -- we know that changing core functionality that breaks prior code is not something to be taken lightly. Please update these two functions respectively. 

- NEW: Additional vignettes added.
- UPDATE: Refreshed current vignettes. Still a lot of work to do.
- UPDATE: `convert_DO()` and `convert_rate()` now require the user to explicitly provide `t` (temperature) and `S` (salinity) values.
- UPDATE: `convert_DO()` and `convert_rate()` will warn the user when the default `P` value is used for O2 units that are strongly influenced by atmospheric pressure.
- UPDATE: `unit_args()` has been updated to indicate which O2 units need inputs of `t`, `S` and `P`.
- FIX: `inspect()` has a better-looking plot for rolling regressions.

# Version 1.0.4 and 1.0.4a
This version updates the new functions from 1.0.0 and tries to improve piping workflows.

- HOTFIX: `import_data()` was not importing firesting files properly.
- FIXED: `subset_data()` - objects produced by this function can now be immediately passed on to other functions. `subset_data()` is a lesser known function but it's very useful, e.g. for truncating intermittent data before analyses. It's been updated work well with other functions, and can also be piped.
- FIXED: We lowered the R version requirement even further, now people with R versions >3.3.0 should be able to run respR.
- UPDATE: `import_data()` - works with a lot more files. Now supports: Firesting Logger | Pyro Oxygen Logger (also Firesting) | PRESENS OXY10 | PRESENS (generic) | MiniDOT | Loligo Witrox Logger | Loligo AutoResp (software output). Get more files to us, people!
- NEW: better feedback from console outputs during piping.
- NEW: better piping! You can now chain print() and plot() commands, which is useful in some cases e.g. checking the top 3 results and plots of `auto_rate()`.


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
