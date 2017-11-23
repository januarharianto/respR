

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/respR)](https://cran.r-project.org/package=respR) [![Travis-CI Build Status](https://travis-ci.org/januarharianto/respR.svg?branch=master)](https://travis-ci.org/januarharianto/respR) [![codecov](https://codecov.io/gh/januarharianto/respR/branch/master/graph/badge.svg)](https://codecov.io/gh/januarharianto/respR) 

# Welcome

`respR` is an R package that provides a structural, reproducible workflow for the processing and analysis of respirometry-related data. 
While the focus of our package is on aquatic respirometry, it is highly likely that the main analytical functions in `respR` will process linear relationships in any related data (e.g. oxygen flux or photosynthesis).


## Installation
`respR` is not yet published in CRAN. For now, use the `devtools` package to grab the **stable** version:

```r
install.packages("devtools")
devtools::install_github("januarharianto/respR")
```

## Usage

A good place to start is our [online vignette](https://januarharianto.github.io/respR/) (click on "[Get Started](https://januarharianto.github.io/respR/articles/respR.html)"). 
For a quick evaluation of the package, try out the following code:

```r
library(respR) # load the library

# As lazy loading is in place, we do not need to call example data explicitly.
# This example will use the `sardine.rd` example data.

# 1. Check data for errors
x <- inspect_data(sardine.rd)

# 2. Manual rate calculations
calc_rate(x, from = 2000, to = 4000)
y <- calc_rate(x, from = c(200, 3000), to = c(800, 5500), by = "time")
print(y)   # show results
plot(y, 2) # plot second subset

# 3. Automatic rate calculations
y <- auto_rate(x) # defaults to most-linear rate
auto_rate(x, width = 1200, by = "time", method = "max")
auto_rate(x, width = 1000, method = "interval")

# 4. Conversions
# Convert y (using default values for S, t and P):
convert_rate(y, o2.unit = "%", time.unit = "s", output.unit = "mg/h/kg", 
  volume = 1.2, mass = 0.8)
# Convert % oxygen saturation, taking into account S, t, and P:
convert_rate(0.02, o2.unit = '%', time.unit = 's', output.unit = 'mg/h/kg', 
  volume = 1.2, mass = 0.5, S = 32, t = 20, P = 1.013253)
```
## Feedback and contributions

We would love for you to help us improve this package! 
If you find a bug, have suggestions to improve our output, or want new features let us know by [opening an issue](https://github.com/januarharianto/respr/issues).

Alternatively, you can fork this project and create a pull request. 

## Collaborators

- Januar **Harianto**
- Nicholas **Carey**
