# Stop with the mixed positive/negative rates error

Raises a classed condition so that `auto_rate.rep` can catch it and
re-raise it with the replicate it came from. Both forms of the message
are built here so the wording stays in one place.

## Usage

``` r
stop_mixed_rates(method, msg = "auto_rate", rep = NULL, total_reps = NULL)
```

## Arguments

- method:

  string. The `auto_rate` method in use ('highest' or 'lowest').

- msg:

  string. Attach function name for message (usually auto_rate).

- rep:

  optional replicate number. Passed by the intermittent-flow functions,
  and adds the replicate and a 'wait'/'measure' hint.

- total_reps:

  optional total number of replicates.

## Value

Called for its error
