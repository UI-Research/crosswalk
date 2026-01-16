# crosswalk

An R interface to inter-geography and inter-temporal crosswalks.

## Overview

This project provides a consistent API and standardized versions of
crosswalks to allow for programmatic interpolation over time and between
geographies. Say goodbye to manual crosswalk downloads and hello to
reproducible workflows.

## Installation

``` r
# Install dependencies
renv::install("UI-Research/crosswalks")
```

## Usage

To get started with `library(crosswalk)`:

``` r
# Load the package
library(crosswalk)

## obtain a crosswalk to translate data in 2020-vintage place geographies
## to 2020-vintage county geographies, weighted by population
place_county_crosswalk = get_crosswalk(
  source_geography = "place",
  target_geography = "county",
  weight = c("population"),
  cache = here::here("data"))

## obtain a crosswalk to translate data in 2000-vintage place geographies
## to 2010-vintage place geographies. all available weighting options are
## returned
get_crosswalk(
  source_year = 2000,
  target_year = 2010,
  source_geography = "place",
  target_geography = "place",
  cache = here::here("data"))
```

## Why Use `library(crosswalk)`?

Crosswalks are a critical component of conducting social sciences
research as they enable analysts to translate data from one geography
and/or temporal vintage to another. For example, if source data are only
available at the county level, a crosswalk can help to produce estimates
of those source data at the city level, enabling analysis at a geography
(the city) that may be either more relevant to target audience(s) and/or
may align with the geography of other data that form part of the
overarching analysis.

There are excellent existing resources for crosswalks, including the
University of Missouri - Missouri Census Data Center's Geocorr 2022
crosswalking application and the IPUMS National Historical Geographic
Information System (NHGIS). In fact, the crosswalks returned by using
`library(crosswalk)` are those from Geocorr and NHGIS.

So why use this package at all? It provides:

-   A consistent, programmatic approach to acquire crosswalks, rather
    than ad-hoc manual downloads;

-   Standardized and clear crosswalk variable names so that you can
    easily work with multiple crosswalks using the same workflow;

-   Crosswalk metadata stored within the returned crosswalk–no more
    commenting in your script with the 15 options you configured prior
    to clicking "Download";

-   The ability to easily "cache" crosswalks locally.

In brief: this package facilitates a well documented and reproducible
analysis workflow, building on top of the robust underlying resources
already available for crosswalking.

## Citations!

The intellectual work and credit for the underlying crosswalks returned
by this package belongs to the original developers of those crosswalks.
You should (in the case of Geocorr crosswalks) and in some cases must
(in the case of NHGIS crosswalks) appropriately cite the developers when
you use these resources.

**For NHGIS**, you should refer to the NHGIS website and terms of use,
including the recommended citations provided at:
<https://www.nhgis.org/citation-and-use-nhgis-data>.

**For Geocorr**, the author of `library(crosswalk)` is unaware of a
required or suggested citation format. An example citation might look
like:

> Missouri Census Data Center, University of Missouri. (2022). Geocorr
> 2022: Geographic Correspondence Engine. Retrieved [202X-XX-XX] from:
> <https://mcdc.missouri.edu/applications/geocorr2022.html>.
