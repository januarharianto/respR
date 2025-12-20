# respR and the Tidyverse

## Working in the Tidyverse

`respR` integrates nicely with the
[tidyverse](https://www.tidyverse.org/), specifically with `dplyr`
functions
e.g. [`select()`](https://januarharianto.github.io/respR/reference/select.md),
[`filter()`](https://rdrr.io/r/stats/filter.html) and `mutate()`, and
`magrittr` pipe operators (“`%>%`”) to clearly express workflows in an
organised sequence.

It also works with the new native pipe operator (`|>`) introduced in [R
v4.1](https://www.r-bloggers.com/2021/05/new-features-in-r-4-1-0/),
however the `dpylr` pipes have some additional functionality. For more
information, see the [**Pipes**](http://r4ds.had.co.nz/pipes.md) chapter
in the online *R for Data Science* book.

## Examples

Here we show how using `%>%` pipes can make data analysis worklows
simpler for the user.

Typical analysis using regular `R` syntax:

``` r
# 1. check data for errors, select cols 1 and 15:
urch <- inspect(urchins.rd, 1, 15) 
# 2. automatically determine linear segment:
rate <- auto_rate(urch)
# 3. convert units
out <- convert_rate(rate, "mg/l", "s", "mg/h/kg", 0.6, 0.4)
```

Alternatively, use `tidyverse` pipes:

``` r
urchins.rd %>%        # using the urchins dataset,
  select(1, 15) %>%   # select columns 1 and 15
  inspect()     %>%   # inspect the data, then
  auto_rate()   %>%   # automatically determine most linear segment
  convert_rate("mg/l", "s", "mg/h/kg", 0.6, 0.4) # convert units
```
