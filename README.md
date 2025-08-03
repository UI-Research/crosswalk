# crosswalk

An R interface to inter-geography and inter-temporal crosswalking.

## Overview

This project provides a consistent API and standardized versions of crosswalks to
allow for programmatic interpolation over time and between geographies. Say goodbye to
manual crosswalk downloads and say hello to reproducible workflows.

## Installation

```r
# Install dependencies
renv::install("UI-Research/crosswalks")
```

## Usage

To get started with the geocorr-api:

```r
# Load the package
library(crosswalk)

place_county_crosswalk = get_geocorr_crosswalk(
  source_geography = "place",
  target_geography = "county",
  weight = c("population"),
  cache = here::here("data"))
```
