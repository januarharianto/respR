

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/respR)](https://cran.r-project.org/package=respR) [![Travis-CI Build Status](https://travis-ci.org/januarharianto/respR.svg?branch=master)](https://travis-ci.org/januarharianto/respR) [![codecov](https://codecov.io/gh/januarharianto/respR/branch/master/graph/badge.svg)](https://codecov.io/gh/januarharianto/respR)

# Welcome

`respR` is an R package that provides a structural, reproducible workflow for the processing and analysis of respirometry-related data. While the focus of our package is on aquatic respirometry, it is highly likely that the main analytical functions in `respR` will process linear relationships in any related data, such as oxygen flux or photosynthesis.


## Installation
`respR` is not yet published in CRAN. For now, use the `devtools` package to grab the **stable** version:

```r
install.packages("devtools")
devtools::install_github("januarharianto/respR")
```

## Usage

A good place to start is our [online vignette](https://januarharianto.github.io/respR/articles/respR.html). For a quick evaluation of the package, try out the following code:

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
auto_rate(x, width = 900, by = "time", method = "max")
auto_rate(x, width = 1000, method = "interval")

# 4. Conversions
convert_rate(y, o2.unit = "%", time.unit = "s", output.unit = "mg/h/kg",
             volume = 1.2, mass = 0.8)

```
## Feedback and contributions

`respR` is under continuous development. If you have any bugs or feedback, you can contact us easily by [opening an issue](https://github.com/januarharianto/respr/issues). Alternatively, you can fork this project and create a pull request.

## Collaborators

- Januar **Harianto**, University of Sydney
- Nicholas **Carey**, Stanford University
- Maria **Byrne**, University of Sydney


# Acknowledgements

The design of this package would not have been possible without inspiration from the following authors and their packages:

- [respirometry](https://cran.r-project.org/package=respirometry) - Matthew A. Birk
- [rMR](https://cran.r-project.org/package=rMR) - Tyler L. Moulton
- [LoLinR](https://github.com/colin-olito/LoLinR) - Colin Olito and Diego Barneche
- [segmented](https://cran.r-project.org/package=segmented) - Vito M. R. Muggeo



# References

Clark, T. D., Sandblom, E., & Jutfelt, F. (2013). Aerobic scope measurements of fishes in an era of climate change: respirometry, relevance and recommendations. Journal of Experimental Biology, 216(15), 2771–2782. [doi: 10.1242/Jeb.084251](https://doi.org/10.1242/Jeb.084251)

Gamble, S., Carton, A. G., & Pirozzi, I. (2014). Open-top static respirometry is a reliable method to determine the routine metabolic rate of barramundi, Lates calcarifer. Marine and Freshwater Behaviour and Physiology, 47(1), 19–28. [doi: 10.1080/10236244.2013.874119](https://doi.org/10.1080/10236244.2013.874119)

Leclercq, N., Gattuso, J.-P. & Jaubert, J. (1999). Measurement of oxygen metabolism in open-top aquatic mesocosms: Application to a coral reef community. Marine Ecology Progress Series, 177, 299–304. [doi: 10.3354/meps177299](https://doi.org/10.3354/meps177299)

Lighton, J.R.B. (2008). Measuring Metabolic Rates: A Manual for Scientists. Oxford University Press, USA.

Muggeo, V.M.R. (2003). Estimating regression models with unknown break-points. Statistics in Medicine, 22, 3055–3071. [doi: 10.1002/sim.1545](https://doi.org/10.1002/sim.1545)

Muggeo, V. (2008). Segmented: An R package to fit regression models with broken-line relationships. R News, 8, 20–25.

Silverman, B.W. (1986). Density Estimation for Statistics and Data Analysis. Chapman; Hall/CRC Press.

Steffensen, J. F. (1989). Some errors in respirometry of aquatic breathers: How to avoid and correct for them. Fish Physiology and Biochemistry, 6(1), 49–59. [doi: 10.1007/BF02995809](https://doi.org/10.1007/BF02995809)

Svendsen, M.B.S., Bushnell, P.G. & Steffensen, J.F. (2016). Design and setup of intermittent-flow respirometry system for aquatic organisms. Journal of Fish Biology, 88, 26–50. [doi: 10.1111/jfb.12797](https://doi.org/10.1111/jfb.12797)

White, C.R. & Kearney, M.R. (2013). Determinants of inter-specific variation in basal metabolic rate. Journal of Comparative Physiology B: Biochemical, Systemic, and Environmental Physiology, 183, 1–26. [doi: 10.1007/s00360-012-0676-5](https://doi.org/10.1007/s00360-012-0676-5)

Yeager, D.P. & Ultsch, G.R. (1989). Physiological regulation and conformation: A BASIC program for the determination of critical points. Physiological Zoology, 62, 888–907. [doi: 10.1086/physzool.62.4.30157935](https://doi.org/10.1086/physzool.62.4.30157935)
