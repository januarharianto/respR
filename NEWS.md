
<!---
Each version should:
List its release date in the above format.
Group changes to describe their impact on the project, as follows:
Added:        for new features.
Changed:      for changes in existing functionality.
Deprecated:   for once-stable features removed in upcoming releases.
Removed:      for deprecated features removed in this release.
Fixed:        for any bug fixes.
Security:     to invite users to upgrade in case of vulnerabilities. 

All version changes go in here now when you make a commit! 
--->

## Version 2.x.x -- 2022-xx-xx

CHANGE: `subset_rate`. For the `"row_omit"` and `"time_omit"` methods, the `n` input can now be a numeric vector of any length. Regressions covering any time or row value in `n` will be omitted. A continuous range of rows or time can be entered using regular R syntax for creating vectors such as `n = 10:20` or `seq()`.

CHANGE: `subset_rate`. For the `rate`, `rsq`, `row`, `time`, and `density` methods the `n` input of two values can now be entered in either order. 

CHANGE: `auto_rate` and `subset_rate`. These functions now behave better with objects which contain zero results, and allow piping operations to continue even if an empty object is encountered somewhere in the pipe. This can occur if subsetting criteria excludes every rate. These `auto_rate_subset` objects with no results now work with `print`, `summary`, and `mean` giving a message that they contain no rates but still printing to the console. `subset_rate` will not now stop if an empty object is input as `x` or piped from a previous `subset_rate` operation. Trying to plot these empty objects in `plot` or `plot_ar` will result in a console message but no plot and will not stop any pipes. 

FIX: `plot_ar` - Fix for incorrect subset number appearing in plot titles.


## Version 2.0.0 -- 2022-02-17

It's been a long time - over two and a half years (!) since the last update. We are happy to see `respR` being used by the respirometry community in that time. Amazingly we have picked up over 30 citations, so we are very grateful and happy that so many scientists are finding the package of use. 

We are glad to announce that `respR` has reached version 2.0 and is finally [available on CRAN](https://cran.r-project.org/web/packages/respR/index.html). 

v2.0 has been a massive and thorough update where almost every corner of the package has been revised, rewritten, streamlined and tested. In addition, we have added a lot of new functionality which you can read about below. 

We have also updated and added even more vignettes, function guides and more on our brand new [website](https://januarharianto.github.io/respR) detailing all the old and new functionality. 

Unfortunately, a major version update inevitably comes with some code breaking changes, and this one is no exception. We thought long and hard about introducing changes that might break code written for v1.1, but made the decision to fix as many as possible of the inconsistencies, bugs, and poor design decisions we made while we had the chance with this major version update. The upside of this is that the package will be much easier to update in the future without breaking existing code. 

We have [created a page on the new website](https://januarharianto.github.io/respR/articles/v2_code_change.html) to detail how v2.0 code has changed and how you may go about revising your earlier code to update it. The good news is most of these changes will be very easy to make. If you have submitted code or are in the process of submitting it as part of a publication, we want this code to remain reproducible long into the future. See [here](https://januarharianto.github.io/respR/articles/v1_code.html) for how you can link to resources that will allow anyone to run v1.1 code and keep these analyses reproducible. 

We are happy to say the package has never been more stable and even easier to use, and it is now on a great base upon which to build future functionality. Here are the major changes in this version:


================================== HIGHLIGHTS ==================================

- NEW: `respR` is now [available on CRAN](https://cran.r-project.org/web/packages/respR/index.html). It can be installed just like any other package via the RStudio package manager tab or by running `install.packages("respR")`. If you want to be on the bleeding edge, for example test out the latest dev version, you can use `install.github()` and the `ref` input to select a different branch.
- NEW: `subset_rate` function. This allows `auto_rate` results to be explored and filtered according to various criteria.
- NEW: `plot_ar` function. This plots `auto_rate` objects in a way that visualises how results are distributed within the dataset. Pairs very nicely with `subset_rate` above. 
- NEW: `adjust_rate` contains several new methods of adjusting for background respiration, including the ability to perform adjustments from paired or concurrent blank chambers, and to perform dynamic adjustments for background rates which change over the course of an experiment.
- NEW: Completely new flowthrough respirometry workflow. There are four new flowthrough-specific functions and new functionality for exploring, calculating and adjusting rates from flowthrough respirometry data.
- NEW: `inspect` and `inspect.ft` performs additional data checks: that columns contain numeric data and a check for infinite values
- NEW: Rates can now be output as surface area-specific rates via the `area` input in `convert_rate` (accepts `"mm2"`, `"cm2"`, `"m2"`, `"km2"`). You can also use `day` as the time metric in output rates (e.g. `"mgO2/day/m2"`)
- NEW: `auto_rate` `"rolling"` method. Performs fixed-width rolling regressions across the entire dataset with no ordering of results. This pairs nicely with the new `subset_rate` function to allows users full control of selection criteria (albeit with fixed width regressions)
- NEW: `auto_rate` `"highest"` and `"lowest"` methods for absolute minimum and maximum of rates regardless of sign
- NEW: Several new dissolved oxygen units are now supported in `convert_DO` and `convert_rate` including percent oxygen saturation, moles of oxygen, and more.
- NEW: `convert_val` function. A simple conversion function to help with inputting experimental parameters in the correct units.
- `citation("respR")` returns the citation to the `respR` journal publication, including a BibTeX entry for LaTeX users or other applications which accept this.
- Help documentation and vignettes rewritten and greatly expanded
- Tested with [`R 4.1`](https://www.r-bloggers.com/2021/05/new-features-in-r-4-1-0/) and the new native pipes (`|>`)
- Tested with Apple Silicon builds of `R`


=============================== General Changes ================================

- CHANGE: S3 Generic functions (`print()`, `summary()`, `plot()`) revised to produce cleaner, consistent outputs.
- CHANGE: Added `$rank` column to all `$summary` tables as first column to helps with consistent S3 summary outputs and generally keeping track of summary rows
- CHANGE: Documentation terminology clarifed. Rates now referred to as "absolute" (for whole animal or chamber) and mass- or area-specific.
- CHANGE: Updated startup message for full MEE citation
- CHANGE: Changed the data frame in output objects to `$dataframe` for consistency (was previously `$df`, `$data`, `$dataframe`, etc.)
- CHANGE: For consistency, changed primary function inputs from `df` to `x` where appropriate (`auto_rate`, `inspect`, `inspect.ft`, `subsample`). This was inconsistent and often confusing, in that `df` often did not have to be a data frame, but could be a different class of object.
- CHANGE: The deprecated functions `inspect_data` and `pcrit` have been removed
- CHANGE: Outputs (where relevant) have function call saved to `$call` element
- CHANGE: Outputs (where relevant) have all inputs saved to `$inputs` element
- CHANGE: Messages, warnings and errors now state what function they originate from. This was not always clear, especially in pipes
- FIX: Proper `par()` restoration behaviour.


======================== Function specific changes =============================

================================== inspect() ===================================

- NEW: Checks that each column contains numeric data. This is the most important check, and if any column fails, the function skips the remaining checks for that column and returns a NULL result
- NEW: Checks for infinite values (`Inf/-Inf`) in all inspected columns
- NEW: If uneven time intervals are detected, it now prints the minimum and maximum intervals found in the time data which can help indicate if there is a large gap in the data
- NEW: Added a `width` input to adjust rolling rate plot (default is 0.1)
- NEW: An additional data type (for example temperature, salinity, etc.) can be plotted alongside the oxygen timeseries by using the `add.data` input. This should be a column number from the same input data frame and share the same `time` data. Data checks are *not* performed on this column (unless all columns are specifically inspected), it is simply a plotting aid to understand where the parameter may affect rates in different regions of the data. There is similar functionality in `inspect.ft`.
- NEW: Can pass `rate.rev = FALSE` to plot rates *not* numerically reversed (for oxygen production respirometry)
- NEW: Can pass `legend = FALSE` to suppress legends
- NEW: `summary()` now works on output (although it's simply a wrapper for `print()`)

- CHANGE: Multiple rate plot improved. Now all plot on same axis range, better colours and spacing.
- CHANGE: Prints `NA` locations for all oxygen columns they are found in (previously only printed for first column)
- CHANGE: If multiple columns are inspected, message that columns 1 and 2 are used by default by subsequent functions, although all are inspected.
- CHANGE: Stops if `oxygen` or `time` columns are not present in the input, or conflict with each other
- CHANGE: Prints on assigning
- CHANGE: Output has additional elements `$call`, `$inputs` and `$add.data`
- CHANGE: Output `$list` and `$list_raw` renamed `$locs` and `$locs_raw`

- FIX: Rolling rate plot failed with even a single `NA` in time or oxygen data.
- FIX: Fix for points sometimes being plotted twice slightly offset in large datasets.
- FIX: Any `time` error locations are now printed when multiple oxygen columns are inspected


================================ calc_rate() ===================================

- NEW: Plot updated with title showing the rank of the current plot. It also now plots oxygen and rate against both time and row index
- NEW: Now calculates from start of the data if `from = NULL`, and to the end of the data if `to = NULL`
- NEW: S3 Generics: `plot()` has the additional inputs `panel` to plot selected panels individually, and `legend` to suppress the legend and other labels.

- CHANGE: Detects and warns about multi-column inputs
- CHANGE: Output elements have been renamed
- CHANGE: Most `$summary` columns have been renamed and reordered to be consistent with other functions
- CHANGE: Plots full 4 panel plot by default

- FIX: `by = "proportion"` and `by = "oxygen"` now work correctly with oxygen *production* data. Previously these methods didn't work with rising oxygen as the code for finding the `from`-`to` values assumed it was decreasing. Now it identifies all data within the range of `from`-`to`. Essentially this makes `from`-`to` interchangeable. Note this change means with some data it may give slightly different results than v1.1 because of the way the `from`-`to` region is identified internally. With most data any difference in rates should be very minor.
- FIX: Fix for failure to plot with very small datasets (2-5 values)
- FIX: Fix for failing to work when there were `NA` in the time data


================================ auto_rate() ===================================

- NEW - `"rolling"` method. Performs simple rolling regression of specified width, with *no* ordering of results. This pairs nicely with the new `subset_rate` function to allows users full control of selection criteria (albeit with fixed width regressions)
- NEW `"highest"` and `"lowest"` methods for *absolute* minimum and maximum rate values regardless of sign. These can only be used when rates all have the same sign.
- NEW `"maximum"` and `"minimum"` methods. These work the *opposite* way to `"min"` and `"max"` from v1.1. These are strictly numerical, and order by value taking account of the sign.
- NEW: Added the Kernel Density metric `$density` to the `$summary` table for the `"linear"` method. This allows you to see how the results are ranked in regards to KDE (also present for other methods, but as an `NA` column).
- NEW: Added `$oxy` and `$endoxy` to the `$summary` table, so you can easily see roughly how much of an oxygen change each regression is using to calculate a rate. This matches the summary table in `calc_rate` and `calc_rate.ft`
- NEW: Plot updated with title showing the rank of the current plot. It also now plots oxygen and rate against both time and row index.
- NEW: Can pass `rate.rev = FALSE` to plot rates *not* numerically reversed (for oxygen production respirometry)
- NEW: S3 Generics: `plot()` has the additional inputs `panel` to plot selected panels individually, and `legend` to suppress the legend and other labels.

- CHANGE: The `"min"`and `"max"` methods have been deprecated. They still work and produce the same results as in v1.1, but have a prominent warning. These will be removed entirely in a later version of `respR`.
- CHANGE: Under the `"linear"` method, occasionally linear regions which were identical but had different density scores were identified. The lower ranked duplicate results are now removed.
- CHANGE: The `width` input behaviour has changed slightly. Previously if it was a value between 0 and 1 it represented a proportional width of the whole dataset regardless of the `by` input. This now applies only to `by = "row"`. If `by = "time"` any value between 0 and 1 now represents (as with values above 1) a time window in the units of the time data.
- CHANGE: Reordered inputs to put `method` second
- CHANGE: Summary table reordered to be consistent with other functions
- CHANGE: Tidied and reordered summary S3 printout. Summary table comes first, density result printed more compactly for `"linear"` method
- CHANGE: The rolling regression plot (panel 3) is now plotted on the same x-axes of time and row as the full timeseries plot, and has better spacing.
- CHANGE: Prints all results of `"interval"` method regardless of `pos` or `width` inputs

- FIX: `plot()` now works correctly on `auto_rate` objects
- FIX: Fix for stopping with an obscure message when input datasets are small
- FIX: Removed unused method `"default"`
- FIX: Fix for plots failing when the result contained an `NA`
- FIX: `method = "max"` was missing `$total_regs` element from metadata
- FIX: Fixed an issue whereby under `by = "time"` and `method = "linear"` a non-proportional `width` (i.e. greater than 1) would get incorrectly passed as a row width rather than a time width. This was only an issue in data where time and row spacing were mismatched, for instance where time was in minutes or hours, or in seconds but not at one second intervals. This would not have affected returned rate *values*, but instead led to poor ranking of the returned rates.
- FIX: Fixed an issue where the function would fail when assembling final output
- FIX: Fixed an issue where rates would not be returned over regions which contained NA values. The function is now generally much more robust to missing values (though input data should wherever possible not contain NA values).


=============================== adjust_rate() ==================================

- NEW: Completely rewritten to allow several new methods, including dynamic background correction. It has the same original inputs (`x`, `by`) plus five additional ones. The `method` defaults to `"mean"` so previous code will output the same results as it did in v1.1, but there is a great deal of new functionality.

- CHANGE: In the output, `$corrected` changed to `$rate.adjusted`, and `$input.rate` changed to `$rate.input`

- FIX: The `adjustment` column was missing from `$summary` table
- FIX: `summary()` now works correctly on objects
- FIX: `print()` prints adjustment correctly if `pos` > 1


=============================== convert_rate() =================================

- NEW: Can now convert to surface area-specific rates via the `area` input (accepts `"mm2"`, `"cm2"`, `"m2"`, `"km2"`)
- NEW: Can now import data and output rates using `"day"` as the time unit e.g. `"mgO2/day/m2"`
- NEW: Can now output oxygen used in moles e.g. `"molO2/day/kg"`
- NEW: Can now accept input oxygen concentration units in moles e.g. `"mol/L"`, `"mol/kg"` and percent oxygen saturation (`"%Oxy"`). Because of this % air saturation is now `"%Air"` and the previous `"%"` operator for this has been deprecated.

- CHANGE: The `o2.unit` input has been renamed to `oxy.unit`
- CHANGE: `oxy.unit = NULL` and `time.unit = NULL` now stop the function instead of applying a default unit
- CHANGE: Now applies a default `output.unit` for mass- and area specific rates as well as absolute rates
- CHANGE: The `output.unit` oxygen amount component now has "O2" appended to ensure it is not confused with the mass-specific component. For example, `"mgO2/h/mg"`, `"umolO2/day/kg"`. These variations are also accepted as inputs. 
- CHANGE: output elements have been renamed (e.g. `$output` to `$rate.output`, `$absolute` to `$rate.absolute` etc.).

- FIX: `summary()` prints summary table as columns correctly


================================== oxy_crit() ==================================

- NEW: This replaces the `calc_pcrit` function from v1.1
- NEW: `method` input. This selects the method to use to determine the critical oxygen value. At present either `"bsr"` for Broken-Stick or `"segmented"` for the Segmented method. Previously both were performed.
- NEW: `thin` input. Defaults to 5000. This applies only to the Broken-Stick regression analysis. If the dataset is longer than this, this determines the number of rows it is uniformly subsampled to before running the analysis. The BSR method is quite computationally intensive, so this speeds it up. In testing, values above this have little effect on the result, but this may vary with different data. To prevent any subsampling and use the entire dataset enter this as `NULL`.
- NEW: S3 Generics: `plot()` has the additional inputs `legend`, `quiet`, `rate.rev` and `panel`.
- NEW: S3 Generics: `summary()` allows results to be exported via `export = TRUE`.

- CHANGE: `parallel = FALSE` is now the default. Use `parallel = TRUE` to use parallel processing if your datasets are particularly large and the function is taking too long to process.
- CHANGE: Uptake rates are now plotted as negative in the rolling rate plot on a reversed y-axis. Plots look the same, but y-axis values are now negative. There is a `rate.rev` input to control this, for instance if you are using the function with existing positive rate values.
- CHANGE: An increase to the internal `thin` value (see above) from 1000 to 5000 may mean results will differ slightly from the previous version, but should be more accurate.
- CHANGE: Output has been completely restructured and elements renamed. Previous code used to extract results will likely now not work.

- FIX:  Fixed issue which caused a failure if the package wasn't loaded but function was called, i.e. using `respR::oxy_crit()`
- FIX:  Fix for plot failure when results were very close together.


================================ import_file() =================================

- NEW: Imports European style formatted numeric data which uses commas instead of points for decimals.
- NEW: Support for Presens Datamanager files

- CHANGE: Importing now generally does not remove any columns (e.g. empty or NA columns). Column naming is now much improved (e.g. unique names, data type linked to channel IDs, etc.).

- FIX: Fix for PreSens Oxyvew csv and txt files not being imported as numeric. Now separate parsing for each.
- FIX: Fix for MiniDot files not being imported as numeric.
- FIX: Fix for Pyro/Firesting numeric columns being imported as character when Firesting software replaces missing data with "---"


================================ format_time() =================================

- CHANGE: Numeric time column name changed from `elapsed` to `time.num`

- FIX: Fix for not appending new numeric time column onto a `data.table`
- FIX: Fix for data frame columns being renamed on return
- FIX: Fix for not working with multiple date-time columns if one of them wasn't column 1
- FIX: Fix for failing to format date-times which were already in POSIX/POSIX.ct format


================================ convert_val() =================================

- New function for doing simple conversions between volume, time, mass, area and pressure units
- Useful for functions that have some inputs which must be in specific units, such as `convert_DO` and `convert_rate`. See Examples.


=============================== calc_rate.bg() =================================

- NEW: The oxygen timeseries is now plotted against both time and row index
- NEW: Plots have `legend` input to suppress equation text boxes

- CHANGE: In the output `$bgrate` changed to `$rate.bg`, and `$mean` changed to `$rate.bg.mean`
- CHANGE: Stops if `oxygen` or `time` columns not present in the input, or conflict with each other
- CHANGE: Removed "input detected" message at start


================================ convert_DO() ==================================

- NEW: Added support for % oxygen saturation (`"%O2"`) units. % air saturation is now `"%Air"` and the previous `"%"` operator for this has been deprecated
- NEW: Added support for oxygen concentration units of `mol/L` and `mol/kg`
- NEW: Added support for oxygen concentration units of `cm3/L`. This is a scarce unit, only used in older publications and is equivalent to `ml/L`.
- NEW: Added `simplify` input. The default is `simplify = TRUE` and means the converted values are output as a numeric vector, rather than within a `list` object. If `FALSE` the output is a `list` object of class `convert_DO` which works with `print()`, `summary()`, and `mean()`

- CHANGE: Removed ability to accept objects of class `calc_rate`, `auto_rate` or `adjust_rate`. This didn't make any sense in this function as rates are not a DO measure!
- CHANGE: Added warning for `P` atmospheric pressure values being outside a realistic range.

- FIX: Fix for error with non-numeric `x` inputs


================================ unit_args() ===================================

- NEW: Updated for new units (time in days, oxygen in mol and %O2) and more detail of which functions accept which units
- NEW: Updated with units for flowrates used in the new flowthrough respirometry workflow


================================= subsample() ==================================

- NEW: Now works on vectors as well as data frames
- NEW: Added `length.out` input which uniformly subsamples to an exact number of rows (data frames) or length (vectors)
- FIX: Now works with multicolumn data frames and plots them correctly (first data column only)


=============================== subset_data() ==================================

- NEW: Support for `inspect.ft` objects
- NEW: Added `quiet` input to allow console output to be suppressed
- NEW: Now subsets from start of the data if `from = NULL`, and to the end of the data if `to = NULL`

- CHANGE: Updated help documentation, particularly to specify that `by = "oxygen"` or `by = "proportion"` applies to first column only in multi-column dataframes
- CHANGE: Warns if output subset is empty

- FIX: Fix for failure with `inspect()` objects
- FIX: `by = "proportion"` and `by = "oxygen"` now work correctly with oxygen production data. Note this now generally results in longer subsets than v1.1 code because of a change in identifying where `to` is within the data.


============================== flowthrough data ================================

- NEW: `inspect.ft`
- NEW: `calc_rate.ft`
- NEW: `adjust_rate.ft`
- NEW: `convert_rate.ft`

- This is a completely new workflow specific to flowthough respirometry data. See help documentation and vignettes for full details.


==================================== data ======================================

- CHANGE: Example data are now all `data.table`
- CHANGE: `flowthrough.rd` amended to make delta column (i.e. unitless rates) negative values to be consistent with how uptake rates are always negative in the package
- CHANGE: `flowthrough.rd` column names changed to `time`, `oxy.out`, `oxy.in` and `oxy.delta`
- CHANGE: `zeb_intermittent.rd` column names changed to `Time` and `Oxygen`
- CHANGE: `sardine.rd` has had a `Temperature` column added (to demonstrate the new `inspect()` functionality to plot an additional data type)

NEW: New example datasets

- `algae.rd`: oxygen production data
- `flowthrough_mult.rd`: multiple column flowthrough data
- `flowthrough_sim.rd`: flowthrough data with increasing background recording
- `background_con.rd`: constant level background, for testing new background correction functionality
- `background_lin.rd`: linear increasing background, for testing new background correction functionality
- `background_exp.rd`: exponential increasing background, for testing new background correction functionality


================================= S3 Methods ===================================

- `mean.adjust_rate`  
- `mean.adjust_rate.ft`  
- `mean.auto_rate`  
- `mean.calc_rate`  
- `mean.calc_rate.bg`  
- `mean.calc_rate.ft`  
- `mean.convert_rate`  
- `mean.convert_rate.ft`  

- NEW: These are new generic S3 functions for calculating mean rates, and work on the relevant function output rate. None alter the output object, but have an `export` input so the mean values can be exported as a numeric value if `export = TRUE`. They also have a `pos` input (where relevant) to select a range of rates to average. Most useful is `mean.convert_rate` which can be used to output a final mean rate at the end of an analysis.

- `mean.convert_DO`

- NEW: This works as the above but averages converted oxygen values from `convert_DO`. The output must have been saved as a `convert_DO` object for this to work by using `simplify = FALSE` (otherwise `convert_DO` by default outputs a numeric vector upon which `mean()` works anyway and gives the same result).

- NEW: All `plot()` S3 have a `quiet` input to suppress console output
- NEW: All `summary()` S3 (where appropriate) have `export = TRUE` for export of summary table or value
- NEW: All S3 (where appropriate) have `pos` input to select positions or ranks of results to plot, print or average, and stop with an error if `pos` is too high
- NEW: Plotting functions accept the generic `par()` inputs `oma`, `mai`, `tck`, `mgp`, `las`, and `pch` via `...` to allow default parameters to be changed. Particularly useful are `las = 1` to make axis labels horizontal and adjusting the second (left side) of the four `mai` (inner margins) input to make y-axis labels more readable.

- CHANGE: Consistently named inputs (for `summary()` the first input should be `object`, but in all others it is `x`)
- CHANGE: All console output prints more consistently with nicer spacing, especially in pipes

- FIX: In `auto_rate` S3 methods stop with a message if no rates found in object (this can happen with over-enthusiastic subsetting in the new `subset_rate` function)
- FIX: All revised to work correctly with `|>` or `%>%` pipes


## Version 1.1.1

This is the final release of v1 of `respR`.

There is only one change, the addition of function called `installation_help`. This function simply opens its own help file. It contains resources which may be helpful in running v1 code in the future, such as a list of dependencies and the latest versions known to work with v1.1.1, and how to install them. This is intended for users who have recently submitted v1.1 code as part of a publication, and investigators who may wish to reproduce their analyses in the future.

The reason this may be necessary is because version 2.0 of `respR` will be released shortly and contains several code-breaking changes which means code written using v1.x will fail to run or return the same results. Using the information in this file an investigator should be able to install `respR` v1.1.1 and its dependencies and reproduce these analyses.

## Version 1.1.0

Lots of updates! First, we have a new function, `calc_pcrit()`, which we will use to include new methods in the future. We've also made some improvements to `import_file()` with increased support to more files from Vernier, PRESENS and Loligo systems.

**Vignettes have been migrated into a new repository** on GitHub, and are now updated separately. This gives us several advantages -- the documentation can be updated without needing to compile a new package, and the size of the package is significantly reduced. Users can also contribute to the documentation more easily now that it is not tied to the package. 

### New Features

- NEW: You can now grab the reference for `respR` by running `citation("respR")`.
- NEW: Loading `respR` will print a startup message containing links to our published manuscript and vignettes. 
- NEW: `inspect()` can now plot data with multiple columns when more than 2 columns are detected. For some people, this may provide a great overview of the data.
- NEW: `calc_pcrit()` is a **new function** that will be developed in parallel to the current `pcrit()` function. In the future we intend to use `calc_pcrit()` to incorporate new methods. 
- NEW: `import_file()` supports even more files! Added more parsers for Vernier, PRESENS and Loligo systems.
- NEW: `format_time()` can now calculate time elapsed even if date information is not provided (e.g. "HMS"-only data). 
- NEW: The `by` input in `auto_rate()`, `calc_rate()`, `calc_rate.bg()` and `subset_data()` is now more forgiving with string input values (e.g. `"oxygen"`, `"Oxygen"`, `"o2"`, `"O2"`, etc. are recognised). To achieve this we created a string matching function, `verify_by()` which uses brute force matching to recognise different ways of writing the same text.

### Fixes

- **`convert_DO()`**:
    - FIX: Argument for `"P"` (pressure) was ignored even when a value was already specified, resulting in the default used. The function will now respect the `"P"` value provided. 
    - FIX: The error message for an unrecognised unit was... cryptic. It has been changed and will now instruct the user on the proper syntax required to recognise units properly.
- **`inspect()`**:
    - FIX: Positive rates were not reflected accurately in plots.
    - FIX: Plot of rolling regression was not sensitive for smaller changes in datasets. The rolling window has been reduced to 10% of the length of the dataset to address this.
- **`convert_rate()`**:
    - FIX: Now stops if a `"mass"` input is provided but `"output.unit"` is not a mass-specific unit. 
    - FIX: Properly accepts objects of class `calc_rate.bg`.
    - FIX: Optimised the syntax for the recognition of `output.unit` in the code.
- **`calc_rate.ft()`**:
    - FIX: Critical error in returned rates sometimes outputting values with the wrong sign.
    - FIX: Optimised code - the `"time"` input has been removed as it was not used.
    - FIX: Properly accepts objects of class `inspect`.
- **`pcrit()`**:
    - FIX: Properly accepts objects of class `inspect` and `inspect_data`. Please note that the `inspect_data()` function is deprecated and will be removed in the near future.


## Version 1.0.5.1

This is a quick fix for images not showing in the online vignettes.

- FIX: images not showing in two vignettes. If you didn't notice it, it never happened...


## Version 1.0.5

This version is aimed at improving the functionality and usability of our conversion functions. We have decided to remove the default values for temperature and salinity inputs in `convert_DO()` and `convert_rate()` (i.e. switched `t` and `S` numerics to `NULL`). This was no easy decision, but we noticed that some users were running the functions by default without considering (or even knowing the existence of) these two *important* input variables. **With this change, existing workflows using `calc_DO()` and `calc_rate()`are likely to break**. Do note that this is a very rare modification -- we know that changing core functionality that breaks prior code is not something to be taken lightly. Please update these two functions respectively. 

- NEW: Additional vignettes added.
- UPDATE: Refreshed current vignettes. Still a lot of work to do.
- UPDATE: `convert_DO()` and `convert_rate()` now require the user to explicitly provide `t` (temperature) and `S` (salinity) values.
- UPDATE: `convert_DO()` and `convert_rate()` will warn the user when the default `P` value is used for O2 units that are strongly influenced by atmospheric pressure.
- UPDATE: `unit_args()` has been updated to indicate which O2 units need inputs of `t`, `S` and `P`.
- FIX: `inspect()` has a better-looking plot for rolling regressions.

## Version 1.0.4

This version updates the new functions from 1.0.0 and tries to improve piping workflows.

- HOTFIX: `import_data()` was not importing firesting files properly.
- FIXED: `subset_data()` - objects produced by this function can now be immediately passed on to other functions. `subset_data()` is a lesser known function but it's very useful, e.g. for truncating intermittent data before analyses. It's been updated work well with other functions, and can also be piped.
- FIXED: We lowered the R version requirement even further, now people with R versions >3.3.0 should be able to run respR.
- UPDATE: `import_data()` - works with a lot more files. Now supports: Firesting Logger | Pyro Oxygen Logger (also Firesting) | PRESENS OXY10 | PRESENS (generic) | MiniDOT | Loligo Witrox Logger | Loligo AutoResp (software output). Get more files to us, people!
- NEW: better feedback from console outputs during piping.
- NEW: better piping! You can now chain print() and plot() commands, which is useful in some cases e.g. checking the top 3 results and plots of `auto_rate()`.


## Version 1.0.0
The big update! Maybe it's time to submit to CRAN?

- UPDATE: We edited some examples to reduce processing times during R CMD checks.
- UPDATE: The packages `dplyr` and `magrittr` are now imported into `respR`.
- UPDATE: Now requires R 3.5.0
- NEW: `import_file()`: automatically import and format raw files from common devices. This function is not complete but needs to be put out there for user feedback. It's stable, just doesn't support all available machines yet. We expect this function to be improved dramatically once users send in feedback and sample files.
- FIX: `dplyr` pipes now work after `print()` and `summary()` commands. Will add support for `plot()` depending on feedback.
- FIX: `adjust_rate()`: minor print issue (new line required) that was messing with the console output.
- UPDATE: `adjust_rate()` documentation has been updated.

## Version 0.1.0
- Updates to documentation and a couple of stability fixes to improve performance. We will push the CRAN submission at version 1.0.0 now, while we work on a few more things.
- FIX: `inspect()` should now work with pipes.
- FIX: `inspect()` had issues with `data.table` objects, so we switched to `data.frame`.

## Version 0.0.7
- HOTFIX: `calc_rate.bg()` did not subset data properly in certain usage scenarios (see #49)

## Version 0.0.6
We are getting close to a 0.1 release, which will be ready for CRAN.

- HOTFIX: More robust method of parsing data-time data to start at the defined start time in `format_time()`, which fixes some errors with zeroing in some numeric data.

## Version 0.0.5
- NEW: Added new function `format_time()`. This function is a wrapper for `lubridate` functions and is used specifically to convert date-time to numeric time.
- NEW: Added replacement function `inspect()` that contains much cleaner code. The old function `inspect_data()` was tough to fix. It will be deprecated in a future update, but will still be supported for at least 18 months after we announce the deprecation. For now, users need not worry about using either function.
- Updated: `calc_rate()`, `calc_rate.bg()`, `calc_rate.ft()`, and `auto_rate()` are updated to work with `inspect()`. (TODO: `calc_rate.ft()` still needs to be fixed.)
- NEW: Vignette for `auto_rate()` is now available. 
- NEW: Performance benchmark vignette for `auto_rate()`'s linear detection method is now available.
- NEW: `sim_data()`: simulate data for benchmark analyses.
- NEW: `test_lin()`: perform benchmark analyses specific for `auto_rate()`'s linear detection (i.e. `methods = "linear"`).
- UPDATE: All plot outputs in the package have been updated. They are now "cleaner". Users are encouraged to provide feedback on our plot outputs as we can update them easily without changing the underlying analyses.
- FIX: Rogue parenthesis code in `auto_rate()` fixed (it was messing with `print()` commands). 

## Version 0.0.4
- HOTFIX: `convert_rate()` fails when output contains "mmol". Fixed.

## Version 0.0.3
- FIX: More updates to documentation.
- FIX: Workaround to show `pcrit()` example on website.

## Version 0.0.2
- FIX: Major updates to documentation.
- NEW: `subset_data()` now contains examples.
- NEW: Flowthrough vignette is now integrated into website. 
- FIX: `inspect_data()` plot output was plotting only columns 1 and 2 by default, even when other columns are selected in inputs.
- FIX: `convert_rate()` was not accepting objects of class `calc_rate.ft`.

## Version 0.0.1
- Stable release.

## Version 0.0.0.9000
- Dev release on Github.
