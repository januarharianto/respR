# <a href='https://github.com/januarharianto/respR'> <img src='man/figures/logo.png' align="right" width="11%" /></a> <font style="font-family:'Courier New'">respR : Processing and analysis of respirometry data</font> 

<!-- badges: start -->
<!-- [![metacran downloads](https://cranlogs.r-pkg.org/badges/respR)](https://cran.r-project.org/package=respR) -->
<!-- [![GitHub R package version](https://img.shields.io/github/r-package/v/januarharianto/respR)](https://github.com/januarharianto/respR) -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/respR)](https://cran.r-project.org/package=respR)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/respR)](https://cran.r-project.org/package=respR) 
[![R-CMD-check](https://github.com/januarharianto/respR/workflows/R-CMD-check/badge.svg)](https://github.com/januarharianto/respR/actions)
[![Codecov test coverage](https://codecov.io/gh/januarharianto/respR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/januarharianto/respR?branch=master)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![DOI](https://zenodo.org/badge/66126363.svg)](https://zenodo.org/badge/latestdoi/66126363)
[![Github Star](https://img.shields.io/github/stars/januarharianto/respR?style=social)](https://GitHub.com/januarharianto/respR/stargazers/)
[![Github watchers](https://img.shields.io/github/watchers/januarharianto/respR?label=Watch&style=social)](https://img.shields.io/github/watchers/januarharianto/respR?style=social)
[![Twitter](https://img.shields.io/twitter/follow/respR_pkg.svg?label=Follow&style=social)](https://twitter.com/respR_pkg?ref_src=twsrc%5Etfw)
<!-- badges: end -->


`respR` is a package for `R` that provides a structural, reproducible workflow for the processing and analysis of respirometry data. 
While the focus of the package is on aquatic respirometry, `respR` is largely unitless and so can process, explore, and determine rates from any respirometry data, and indeed linear relationships in any time-series data.

Use `respR` to:

- Automatically **import** raw data from various oxygen probe equipment
- Rapidly **inspect** data for common issues before analysis
- **Explore** and **visualise** timeseries 
- Perform **multiple regression analysis** on data to manually or automatically to calculate rates
- **Adjust** rates for background oxygen consumption or production
- **Convert** rates to any common unit of oxygen consumption or production
- **Export** results quickly for reporting

A highlight of the package is the `auto_rate()` function. This uses machine learning (kernel density estimation) to *automatically* identify linear regions of data, that is regions where oxygen uptake or production rates are stable and consistent. This allows metabolic rates to be extracted in an objective manner. See `vignette("auto_rate")` for more details.

## Installation

`respR` is now available on [**CRAN**](https://CRAN.R-project.org/package=respR), and can be installed via the 'Packages' tab in RStudio or by running this command:

```r
install.packages("respR")
```

## Getting started

Visit the `respR` [**website**](https://januarharianto.github.io/respR) to get started. The site has a range of vignettes detailing the functionality, plus example workflows, documentation, and more. 

We are also happy to help directly. If you have problems using the package or getting started with your analysis, [**get in touch**](mailto:nicholascarey@gmail.com) with a sample of your data and we will help get you started. 

## Publication 

<a href='https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/2041-210X.13162'><img src='man/figures/mee_cover.jpg' align="right" width="22%" hspace = "20" /></a>

The package has also been [**peer reviewed and published**](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13162) in *Methods in Ecology and Evolution*. Please cite this publication if you use `respR` in your published work. 

`respR` has been used to examine metabolic rates and photosynthesis in corals, plankton, micro- and macro-algae, fish, crustaceans, echinoderms, cephalopods, bivalves and more, in both lab and field studies. Check the [**respR in use**](https://januarharianto.github.io/respR/articles/citations.html) page to see a list of published studies which have used the package to analyse their data.

 \
 \
 \
 \
 \
 \

## Contact, feedback and help

`respR` has a [**Twitter account**](https://twitter.com/respR_pkg). Please follow for latest news and regular updates from the world of respirometry! 

See [**here**](https://januarharianto.github.io/respR/articles/contact.html) for even more ways of communicating with us, providing feedback and getting touch if you are having issues.

## Support package development

See [**Support Us**](https://januarharianto.github.io/respR/articles/support.html) if you would like to help support the package development.

## Developers

- [**Januar Harianto**](https://github.com/januarharianto), University of Sydney
- [**Nicholas Carey**](https://github.com/nicholascarey), Marine Scotland Science

## Usage

For a quick evaluation of the package, try out the following code:

```r
library(respR) # load the package

# 1. check data for errors, select cols 1 and 15:
urch <- inspect(urchins.rd, time = 1, oxygen = 15) 
# 2. automatically determine most linear segment:
rate <- auto_rate(urch)
# 3. convert
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


