

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/respR)](https://cran.r-project.org/package=respR) [![Travis-CI Build Status](https://travis-ci.org/januarharianto/respR.svg?branch=master)](https://travis-ci.org/januarharianto/respR) [![codecov](https://codecov.io/gh/januarharianto/respR/branch/master/graph/badge.svg)](https://codecov.io/gh/januarharianto/respR) 

# Welcome

`respR` is an R package that provides a structural, reproducible workflow for the processing and analysis of respirometry-related data. While the focus of our package is on aquatic respirometry, it is highly likely that the main analytical functions in `respR` will process any data that is linear.


## Installation
`respR` is not yet published in CRAN, but it will be. Meanwhile, use the `devtools` package to grab the **stable** version:

```r
install.packages("devtools")
devtools::install_github("januarharianto/respR")
```

## Usage

A good place to start is our [online vignette](https://januarharianto.github.io/respR/index.html). For a quick evaluation of the package, try out the following code:

```r
library(respR) # load the library
    
# 1. Check data for errors
x <- inspect_dat(sardine.rd)

# 2. Manual rate calculations
calc_rate(x, from = 2000, to = 4000)
y <- calc_rate(x, from = c(200, 3000), to = c(800, 5500), by = "time")
print(y)   # show results
plot(y, 2) # plot second subset

# 3. Automatic rate calculations
y <- auto_rate(x) # defaults to most-linear rate
auto_rate(x, width = 900, by = "time", method = "max")
auto_rate(x, width = 1000, method = "interval")

# 4. Conversions
convert_rate(y, o2.unit = "%", time.unit = "s", output.unit = "mg/h/kg", 
             volume = 1.2, mass = 0.8)

```
 


## Collaborators

- Januar **Harianto**
- Nicholas **Carey**
