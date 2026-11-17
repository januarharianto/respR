# Check the measurement section fits within each replicate

Used by `calc_rate.int` and `auto_rate.int`. A `measure` input can
extend past the end of a replicate, in which case the measurement
section is quietly clipped to the data that remains and the rate
calculated from fewer rows than were asked for. This warns when that
happens and names the replicates affected. Replicates where `wait`
itself overruns are left to the errors in the calling functions.

## Usage

``` r
check_measure(reps, from, to, by, msg, allow_single = TRUE)
```

## Arguments

- reps:

  list. Data frames, one per replicate.

- from:

  numeric. Start of the measurement section within each replicate.

- to:

  numeric. End of the measurement section within each replicate.

- by:

  string.

- msg:

  string. Attach function name for message.

- allow_single:

  logical. `FALSE` stops with an error where a replicate is left with
  fewer than 2 rows, for functions which cannot use one row.

## Value

Called for its warning
