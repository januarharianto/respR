# Future Features

## Future features

These features are in development, planned or being considered. We are
happy to take any input as to which we should prioritise. Please feel
free to chime in via any of the methods listed in
[`vignette("contact")`](https://januarharianto.github.io/respR/articles/contact.md).

### Intermittent-flow respirometry

- ~~A function or workflow to automatically identify replicates, run
  `calc_rate` or `auto_rate` on each one, then summarise the result. See
  [here](https://github.com/januarharianto/respR/issues/131) and feel
  free to make suggestions.~~

This is now implemented! As part of
[v2.1](https://januarharianto.github.io/respR/articles/release_notes.html)
the
[`calc_rate.int()`](https://januarharianto.github.io/respR/reference/calc_rate.int.md),
[`auto_rate.int()`](https://januarharianto.github.io/respR/reference/auto_rate.int.md)
and
[`select_rate()`](https://januarharianto.github.io/respR/reference/select_rate.md)
functions allow you to extract rates from every replicate in
intermittent-flow respirometry data and summarise them according to a
huge array of criteria. It’s not quite *automatic* identification of
replicates, but the process of specifying them is very straightforward,
and leaves open the option to have some sort of automatic identification
included in the future.

### `convert_MR`

- ~~A function for converting between units of oxygen uptake or
  production rate. Could be useful for comparing outputs to results from
  the literature.~~

This is now added! As part of
[v2.3](https://januarharianto.github.io/respR/articles/release_notes.html)
the
[`convert_MR()`](https://januarharianto.github.io/respR/reference/convert_MR.md)
function allows you to convert between metabolic rates, including those
from older publications. See `vignette("convert_MR")` for details.

### `oxy_crit`

- Addition of further methods of determining critical oxygen values.
- Priorities would be the non-linear methods of Marshall et al. (2013)
  and α-method of Seibel et al. (2021).
- See [here](https://github.com/januarharianto/respR/issues/9) and feel
  free to make suggestions.

### Shiny app

- We are in the early stages of thinking about creating a Shiny web-app
  version of `respR`. While we have made it easy to use, some people
  still cry out for a graphical user interface.

### `auto_rate.ft`

- Flowthrough analysis workflow currently has no equivalent of
  `auto_rate`, that is some way of automatically and objectively
  identifying stable rates.

### Air respirometry

We are still desperate to
[hear](https://januarharianto.github.io/respR/articles/contact.html)
from users who have used (or tried to use) `respR` with data from air
respirometry experiments so that we can get an idea of how we can update
the package to directly support it.
