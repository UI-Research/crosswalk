# Precompile computationally expensive vignettes
#
# standardizing-longitudinal-data.Rmd is generated from
# standardizing-longitudinal-data.Rmd.orig: knitting the .orig file downloads
# six years of HMDA data and an NHGIS crosswalk (which requires an
# IPUMS_API_KEY), and the resulting .Rmd -- with outputs baked in -- is what
# R CMD build and pkgdown render, so package builds need neither network
# access nor API keys for this vignette. Re-run this script from the package
# root whenever the .orig file changes or its outputs should be refreshed:
#
#   source("vignettes/precompile.R")

knitr::knit(
  input = "vignettes/standardizing-longitudinal-data.Rmd.orig",
  output = "vignettes/standardizing-longitudinal-data.Rmd")
