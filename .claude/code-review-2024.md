# respR Code Review — Issue Tracker

This document records all issues identified during a comprehensive code review,
their status, and what was done (or why nothing was done). Read this before
starting any new review session to avoid duplicating work.

## Issues Ignored / Not Bugs

These should NOT be reported or re-investigated in future sessions.

| # | Description | Reason to skip |
|---|-------------|----------------|
| 2 | (unknown — user said not a bug) | User: "Don't report as a bug/issue in future" |
| 7 | `1:length(x)` pattern (~80 occurrences) | Style issue, not a bug. All contexts guarantee non-empty input. User: "ok, ignore 7 in the future" |
| 12 | "celcius" typo in unit matching | By design — fuzzy input matching intentionally accepts common typos. User: "this is by design. ignore" |
| 19 | Duplicated code across S3 methods (print/summary/mean) | Stable boilerplate. Refactoring would be high-risk for no user-facing benefit. User: "ok, ignore this issue going forward" |

## Issues Fixed by User (Verified)

| # | Description | Status |
|---|-------------|--------|
| 1 | hPa constant value | User fixed |
| 3 | Comment issue | User fixed |
| 8 | `parallel` not in DESCRIPTION | User fixed — verified it's in Imports with correct `@importFrom` tags |
| 11 | (unknown) | User fixed |
| 13 | HTTP bit.ly URL | User fixed |

## Issues Fixed by Claude

### #4/#5: `any(class(x) %in% ...)` → `inherits(x, ...)`

**72 total occurrences** converted across 17 files. The old pattern fails with
S4 classes and is flagged by R CMD check in R ≥ 4.0. `inherits()` is the
correct R idiom for S3 class checking.

Files changed: `adjust_rate.R` (15), `adjust_rate.ft.R` (2), `calc_rate.ft.R` (2),
`oxy_crit.R` (5), `convert_rate.ft.R` (2), `util_plots.R` (3), `select_rate.R` (2),
`util_val.R` (22 in `class.val()`), plus files from the earlier session.

### #6: `panel == FALSE` in `auto_rate.R`

**Lines 631, 652** changed to `isFALSE(panel)` — consistent with existing
line 669. The `==` comparison between logical FALSE and integer coerces
silently; `isFALSE()` is type-safe.

### #9: `adjust_scale` time multiplier mapping

Added a clarifying comment to `util_funs.R` explaining why mass/amount prefixes
use absolute multipliers while time prefixes use inverse convention, but the
a/b ratio produces correct results for both.

### #10: `truncate_data` misleading indentation

Fixed indentation in `util_funs.R` — separated the from/to clamping blocks
with a blank line and corrected indent levels. Logic was correct but indentation
was misleading.

### #14: `@docType package` deprecation

Replaced deprecated roxygen2 `@docType package` pattern in `respR.R` with
modern `"_PACKAGE"` sentinel:
```r
#' @keywords internal
"_PACKAGE"
```

### #15: `dplyr::select` re-export

Removed 7-line re-export block from `util_funs.R`. `select` was never used in
package code — only re-exported, which creates namespace conflicts with other
packages.

### #16: Global `NULL` assignments for R CMD check NOTEs

Replaced the `x = NULL; endtime = NULL; ...` hack (lines 523–529 of
`util_funs.R`) with `utils::globalVariables(c(...))`. The old pattern created
actual namespace bindings that could shadow parameter names.

### #17: `c` used as variable name in `convert_DO.R`

Renamed all 50 occurrences of `c` → `conc` (29 assignments + 21 usages) in
the `convert_DO()` function. `c` shadows the base R `c()` function.

### #18: `convert_rate.ft` P default inconsistency

Changed `convert_rate.ft` signature from `P = 1.013253` to `P = NULL` to match
`convert_rate`. Both functions document the same default; `StP.val()` handles
applying the default internally when P is NULL.

### #20: Missing `call. = FALSE` in `stop()`/`warning()` calls

Added `call. = FALSE` to **59 calls** in S3 methods and internal helpers.
Without it, error messages show confusing internal function names (e.g.,
`Error in class.val(x, "inspect") :` instead of `Error: adjust_rate: ...`).

**Category 3 — Internal helpers (21 fixes):**
- `util_val.R`: 12 fixes in `column.id()`, `by_val()`, `input.val()`, `units.val()`
  - **Bug found**: line 451 had `call. = F` inside `glue::glue()` parentheses
    instead of as a `stop()` argument — silently ignored. Now correctly placed.
- `util_funs.R`: 1 fix in `os()`
- `util_plots.R`: 3 fixes in `grid.p()`

**Category 2 — S3 methods (38 fixes):**
- `adjust_rate.ft.R`: 4 (print, summary, mean)
- `calc_rate.ft.R`: 7 (print, summary, mean, plot)
- `calc_rate.int.R`: 6 (print, summary, plot ×2, mean)
- `convert_rate.ft.R`: 6 (print, summary, mean, plot ×2)
- `inspect.R`: 1 (plot)
- `oxy_crit.R`: 1 (plot)
- `convert_rate.R`: already had `call. = FALSE` — no changes needed

**Category 1 — Direct in exported functions (~112 calls):** NOT fixed.
These show the correct user-facing function name. The user may have
intentionally left them to show call context.

### #21: `convert_val` pressure conversion missing `mmHg` and `inHg`

`unit_type()` recognised `mmHg.p` and `inHg.p` as valid pressure units but the
conversion lookup table in `convert_val()` (lines 171–175) didn't include them.
Passing `"mmHg"` or `"inHg"` silently returned `NA`. Added both units with
multipliers derived from standard definitions: `mmHg → 750.06158`, `inHg → 29.52998`.
Also added to NEWS.md.

### #22: `call. = FALSE` inside `glue()` in `convert_val.R`

Same bug pattern as #20's `units.val()` fix. Line 283 had `call. = FALSE` as a
named argument to `glue::glue()` instead of `stop()`. Moved the closing
parenthesis so `call. = FALSE` is passed to `stop()`.

### #23: `T` instead of `TRUE`

`convert_val.R:250,281` used `== T` and `auto_rate_funs.R:287` used `na.rm = T`.
`T` can be overwritten by users (`T <- 0`), `TRUE` cannot. Changed all three to
`TRUE`.

### #24: `atm` multiplier precision in `convert_val.R`

The `atm` multiplier was `0.98692` (5 dp). Standard value is `1/1.01325 = 0.986923...`.
Bumped to `0.986923` (6 dp) for consistency with the newly added mmHg/inHg values.

### #25: `T`/`F` instead of `TRUE`/`FALSE` — widespread

`T` and `F` are not reserved in R and can be overwritten by users (e.g.
`T <- 0`), while `TRUE`/`FALSE` cannot. Replaced all boolean `T`/`F` usage
across the R source:

- **`call. = F` → `call. = FALSE`**: 33 fixes across `inspect.R` (9),
  `inspect.ft.R` (15), `convert_rate.ft.R` (3), `convert_MR.R` (1),
  `util_funs.R` (2), `util_val.R` (3)
- **`drop = F` → `drop = FALSE`**: 16 fixes in `util_funs.R`
- **`= F` function args** → `= FALSE`: `calc_rate.bg.R` (2 `title`),
  `util_plots.R` (1 `legend`), `oxy_crit.R` (3 `horiz`, 2 `plot`),
  `import_file.R` (1 `header`, 1 `== F`)
- **`byrow = T` → `byrow = TRUE`**: 1 fix in `import_file.R`

**Total: ~60 replacements across 9 files.**

## Other Fixes

- **`.Rbuildignore`**: Added `^\.claude` to suppress R CMD check NOTE about
  hidden `.claude` directory.

## pkgdown Deployment Fixes

### P1: `_pkgdown.yml` fa-home missing aria-label

Added `aria-label: Home` to the `fa-home` icon-only navbar item. Bootstrap 5
requires aria-label for accessibility on icon-only links.

### P2: Duplicate HTML identifiers from YAML title + body headings

When a vignette's `title:` YAML field produces the same slug as a `##` heading,
Pandoc generates duplicate HTML `id` attributes. Fixed by adding explicit
`{#unique-id}` suffixes to the body headings:

- `contact.Rmd`: `## Contact` → `## Contact {#contact-info}`
- `future.Rmd`: `## Future features` → `## Future features {#planned-features}`
- `refs.Rmd`: `## References` → `## References {#refs-list}`

### P3: Missing alt text on static images

Added alt text / `fig.cap` across all static images that lacked it:

- `README.md`: 2 images — added `alt=` attributes to inline HTML `<img>` tags
- `contact.Rmd`: 2 profile images — added `alt=` attributes
- `refs.Rmd`: 1 image — added `fig.alt` (not `fig.cap` to avoid layout issues;
  also fixed duplicate `out.width` in same chunk)
- `archive/auto_rate_comp.Rmd`: 15 images — added `fig.cap` to all chunks
- `archive/auto_rate_performance.Rmd`: 9 images — added `fig.cap` to all chunks
- `archive/packages_comp.Rmd`: 15 images — added `fig.cap` to all chunks
- `archive/oxy_crit_comp.Rmd`: 4 images — added `fig.cap` to all chunks

**Total: 48 static images fixed across 7 files.**

### P4: Missing alt text on dynamically generated plots

pkgdown flags `fig.alt` warnings for R code chunks that generate plots without
alt text (e.g. `unnamed-chunk-2-1.png`). Added `fig.alt` (invisible alt text,
no visible caption) to all plot-producing chunks across all main vignettes:

- `oxycrit.Rmd`, `closed.Rmd`, `inspecting.Rmd`, `calc_rate.Rmd`,
  `auto_rate.Rmd`, `flowthrough.Rmd`, `select_rate.Rmd`, `adjust_rate.Rmd`,
  `auto_rate.int.Rmd`, `calc_rate.int.Rmd`, `oxy_production.Rmd`,
  `intermittent_short.Rmd`

## Remaining Notes

- ~112 `stop()`/`warning()` calls in exported functions still lack
  `call. = FALSE`. These are in category 1 (user-facing functions where the
  call display is correct). Left intentionally.
- User needs to run `devtools::document()` after these changes to regenerate
  NAMESPACE and man pages.
