# Welcome

**RespR** is an R package that analyses respirometry-related data. You start with `calc.rate()` or `auto.rate()` to obtain the rate of change of oxygen concentration on a subset, or multiple subsets of the data. You then convert the rate to volume- and/or weight-specific rate using `calc.mo2()` using the very robust conversion function of `convert.do()`.

There are other useful functions. For example, `calc.bg.rate()` allows you to calculate background rate, which is easily called in `calc.rate()` and `auto.rate()`, while `pcrit()` is useful to determine critical oxygen tension.

More information about the functions are in our vignettes.

### Installation
You can either install the stable version from CRAN (recommended), or play around with the developmental version here on GitHub.

    # CRAN Installation:
    install.packages("RespR")

The `devtools` package will need to be installed if you wish to grab our developmental version.

    # GitHub Installation:
    install.packages("devtools")
    devtools::install_github("januarharianto/RespR")

### Usage
We have prepared a series of vignettes that can help you get up to speed with the package based on simple scenarios. A good place to start is the introductory vignette [RespR - An R package for processing respirometry data]() (link will appear soon). Other vignettes are linked to this one, and provided below for your convenience:

- [Working with intermittent data]() (link will appear soon). If you're working on intermittent respirometry, or wish to subset multiple sections of a dataset, read this.
- [Maximum, minimum and interval analysis]() (link will appear soon). Use `auto.rate()` to efficiently run rolling regressions on your data and rank them.
- [Running pcrit analyses]() (link will appear soon). How to load data for automatic pcrit determination.

### Collaborators

- Nicholas **Carey**
- Januar **Harianto**
