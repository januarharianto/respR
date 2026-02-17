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

## Other Fixes

- **`.Rbuildignore`**: Added `^\.claude` to suppress R CMD check NOTE about
  hidden `.claude` directory.

## Remaining Notes

- ~112 `stop()`/`warning()` calls in exported functions still lack
  `call. = FALSE`. These are in category 1 (user-facing functions where the
  call display is correct). Left intentionally.
- User needs to run `devtools::document()` after these changes to regenerate
  NAMESPACE and man pages.
