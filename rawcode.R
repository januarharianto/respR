# Raw script

# Install the tools needed to develop packages. Do this one.
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages("rstudioapi")
# rstudioapi::isAvailable("0.99.149")  # version check, should return TRUE.
# devtools::install_github("hadley/devtools")


# load devtools and check that everything is working. Should return TRUE.
library(devtools)
has_devel()

