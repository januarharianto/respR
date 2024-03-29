# <a href='https://github.com/januarharianto/respR'> <img src='man/figures/logo.png' align="right" width="11%" /></a> <font style="font-family:'Courier New'">respR : Processing and analysis of respirometry data</font> 

<!-- badges: start -->
<!-- [![metacran downloads](https://cranlogs.r-pkg.org/badges/respR)](https://cran.r-project.org/package=respR) -->
<!-- [![GitHub R package version](https://img.shields.io/github/r-package/v/januarharianto/respR)](https://github.com/januarharianto/respR) -->
<!-- [![R-CMD-check](https://github.com/januarharianto/respR/workflows/R-CMD-check/badge.svg)](https://github.com/januarharianto/respR/actions) -->
<!-- [![DOI](https://zenodo.org/badge/66126363.svg)](https://zenodo.org/badge/latestdoi/66126363) -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/respR)](https://cran.r-project.org/package=respR)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/respR)](https://cran.r-project.org/package=respR) 
[![R-CMD-check](https://github.com/januarharianto/respR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/januarharianto/respR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/januarharianto/respR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/januarharianto/respR?branch=master)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![DOI](https://img.shields.io/badge/DOI-10.1111%2F2041--210X.13162-blue)](https://doi.org/10.1111/2041-210X.13162)
[![Github Star](https://img.shields.io/github/stars/januarharianto/respR?style=social)](https://GitHub.com/januarharianto/respR/stargazers/)
[![Github watchers](https://img.shields.io/github/watchers/januarharianto/respR?label=Watch&style=social)](https://img.shields.io/github/watchers/januarharianto/respR?style=social)
[![Github sponsor](https://img.shields.io/static/v1?label=Sponsor&message=%E2%9D%A4&logo=GitHub&style=social)](https://github.com/sponsors/nicholascarey)
[![Mastodon](https://img.shields.io/badge/dynamic/json?label=Mastodon&query=totalItems&url=https%3A%2F%2Fmas.to%2Fusers%2FrespR%2Ffollowers.json&logo=mastodon&style=social)](https://mas.to/@respR)

<!-- badges: end -->


`respR` is a package for `R` that provides a structural, reproducible workflow for the processing and analysis of respirometry data. 
While the focus of the package is on aquatic respirometry, `respR` is largely unitless and so can process, explore, and determine rates from any respirometry data, and indeed linear relationships in any time-series data.

Use `respR` to:

- **Inspect** respirometry data for common issues before analysis
- **Explore** and **visualise** oxygen timeseries 
- **Calculate** rates manually or automatically using **multiple regression analysis** 
- **Adjust** rates for background oxygen consumption or production
- **Convert** rates to any common unit of oxygen consumption or production
- **Select** rates according to various criteria
- **Export** results quickly for reporting
- Identify **critical oxygen values**, such as $P_{crit}$

A highlight of the package is the `auto_rate()` function. This uses machine learning (kernel density estimation) to *automatically* identify linear regions of data, that is regions where oxygen uptake or production rates are stable and consistent. This allows metabolic rates to be extracted in an objective manner. See `vignette("auto_rate")` for more details.

## Installation

`respR` is now available on [**CRAN**](https://CRAN.R-project.org/package=respR), and can be installed via the 'Packages' tab in RStudio or by running this command:

```r
install.packages("respR")
```

You can also install the latest version direct from [**Github**](https://github.com/januarharianto/respr/):

```r
devtools::install_github("januarharianto/respR")
```

## Getting started

Visit the `respR` [**website**](https://januarharianto.github.io/respR/) to get started. The site has a range of vignettes detailing the functionality, plus example workflows, documentation, and more. 

We are also happy to help directly. If you have problems using the package or getting started with your analysis, [**get in touch**](https://januarharianto.github.io/respR/articles/contact.html) with a sample of your data and we will help get you started. 

## Publication 

<a href='https://doi.org/10.1111/2041-210X.13162'><img src='man/figures/mee_cover.jpg' align="right" width="22%" hspace = "20" /></a>

The package has also been [**peer reviewed and published**](https://doi.org/10.1111/2041-210X.13162) in *Methods in Ecology and Evolution*. Please cite this publication if you use `respR` in your published work. 

`respR` has been used to examine metabolic rates and photosynthesis in corals, plankton, micro- and macro-algae, fish, crustaceans, echinoderms, cephalopods, bivalves and more, in both lab and field studies. Check the [**respR Citations**](https://januarharianto.github.io/respR/articles/citations.html) page to see a list of published studies which have used the package to analyse their data.

 \
 \
 

## Contact, feedback and help

See [**here**](https://januarharianto.github.io/respR/articles/contact.html) for more ways of providing feedback and getting in touch if you are having issues.

For the latest news and regular updates from the world of respirometry follow `respR` on **<a rel="me" href="https://mas.to/@respR">Mastodon</a>** or [**Twitter**](https://twitter.com/respR_pkg). 

## Support package development

If you would like to help support the package development or just buy us a beer to say thanks see [**here**](https://januarharianto.github.io/respR/articles/contact.html#support-future-development) 

## Developers

- [**Nicholas Carey**](https://github.com/nicholascarey), Marine Scotland Science
- [**Januar Harianto**](https://github.com/januarharianto), University of Sydney

## Usage

For a quick evaluation of the package, try out the following code:

```r
library(respR) # load the package

# 1. Check data for errors, selecting cols 1 and 15:
urch <- inspect(urchins.rd, time = 1, oxygen = 15) 
# 2. Automatically determine most linear regions:
rate <- auto_rate(urch)
# 3. Convert
out <- convert_rate(rate, 
                    oxy.unit = "mg/L", 
                    time.unit = "min", 
                    output.unit = "mg/h/kg", 
                    volume = 0.6, 
                    mass = 0.4)
print(out)

## Alternatively, use pipes:
urchins.rd %>%        # using the urchins dataset,
  select(1, 15) %>%   # select columns 1 and 15
  inspect()     %>%   # inspect the data, then
  auto_rate()   %>%   # automatically determine most linear segment
  print()       %>%   # a quick preview
  convert_rate("mg/L", "min", "mg/h/kg", 0.6, 0.4) # convert to units
```


