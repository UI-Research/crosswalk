# Getting Started with crosswalk

## Overview

Census geographies change. Tracts are redrawn every decade, counties are
occasionally renamed, merged, or split, and many analyses need to move
data between geographies (say, from ZIP Code Tabulation Areas to PUMAs)
that don’t nest within one another. A *crosswalk* is a table that
relates source geographies to target geographies, with allocation
factors describing what share of each source unit belongs to each target
unit.

The `crosswalk` package provides a consistent interface to crosswalks
from four sources, plus tooling to apply them to your data:

| Source | What it covers | API key |
|----|----|----|
| **GeoCorr** (Missouri Census Data Center) | Same-year crosswalks between geographies (GeoCorr 2022 for 2020s geography; GeoCorr 2018 for 2010s geography) | None |
| **IPUMS NHGIS** | Cross-decade crosswalks (e.g., 2010 tracts to 2020 tracts) | `IPUMS_API_KEY` |
| **CT Data Collaborative** | Connecticut’s 2020-2022 change from counties to planning regions | `CENSUS_API_KEY` (county requests only) |
| **County events** (curated, ships with the package) | County redefinitions outside decennial censuses, supporting county crosswalks between any years from 2000 on | None |

Two functions do most of the work:

- [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  fetches crosswalk(s), automatically chaining multiple sources when
  both geography and year change
- [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  applies crosswalk(s) to your data

This vignette walks through the core workflow. The examples that query
the Census Bureau’s API require a `CENSUS_API_KEY`; if you’re missing
outputs below, that’s why.

``` r

library(crosswalk)
library(dplyr)
```

## Fetching your first crosswalk

Suppose we have county-level data and need it at the PUMA (Public Use
Microdata Area) level. Counties don’t nest within PUMAs, so we need a
crosswalk. With no years specified,
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
uses GeoCorr 2022 (2020-Census geography):

``` r

## save fetched crosswalks to a directory so repeat requests read from disk;
## see the Caching section at the end of this vignette
cache_dir <- file.path(tempdir(), "crosswalk-cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

county_to_puma <- get_crosswalk(
  source_geography = "county",
  target_geography = "puma",
  weight = "population",
  cache = cache_dir)
```

[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
always returns a list with three elements:

| Element | Description |
|----|----|
| `crosswalks` | A named list of crosswalk tibbles (`step_1`, `step_2`, …) |
| `plan` | The plan describing each step: its source, geographies, and years |
| `message` | A human-readable description of the crosswalk chain |

``` r

county_to_puma$message
#> [1] "Single-step crosswalk:\n  Step 1: county -> puma (inter-geography, Geocorr 2022)\n\nSingle crosswalk; use allocation_factor_source_to_target directly."

county_to_puma$crosswalks$step_1 |>
  head()
#> # A tibble: 6 × 12
#>   state_fips state_abbreviation source_geoid target_geoid source_geography_name
#>   <chr>      <chr>              <chr>        <chr>        <chr>                
#> 1 01         AL                 01001        0101700      Autauga AL           
#> 2 01         AL                 01003        0102701      Baldwin AL           
#> 3 01         AL                 01003        0102702      Baldwin AL           
#> 4 01         AL                 01005        0102500      Barbour AL           
#> 5 01         AL                 01007        0101100      Bibb AL              
#> 6 01         AL                 01009        0100900      Blount AL            
#> # ℹ 7 more variables: target_geography_name <chr>,
#> #   allocation_factor_source_to_target <dbl>,
#> #   allocation_factor_target_to_source <dbl>, population_2020 <dbl>,
#> #   source_geography <chr>, target_geography <chr>, weighting_factor <chr>
```

Every crosswalk uses the same standardized columns, whatever its source:

| Column | Description |
|----|----|
| `source_geoid`, `target_geoid` | Identifiers for the source and target geographies |
| `allocation_factor_source_to_target` | The share of the source unit’s `weight` allocated to the target unit |
| `weighting_factor` | The attribute used to compute allocation factors (e.g., `population`, `housing_all`, `land`, `identity`) |

A county entirely inside one PUMA has a single row with an allocation
factor of 1; a county split across PUMAs has one row per PUMA, with
allocation factors that sum to 1. Crosswalks always contain exactly one
row per source-target pair.

The `weight` argument (“population”, “housing”, or “land”) determines
*how* split units are apportioned – population-weighted,
housing-unit-weighted, or area-weighted. Match it to what you’re
crosswalking: population-based weights for people-denominated variables,
housing for housing-unit variables. See
[`vignette("how-interpolation-works")`](https://ui-research.github.io/crosswalk/articles/how-interpolation-works.md)
for details.

### Where did this crosswalk come from?

Each crosswalk tibble carries a `crosswalk_metadata` attribute
documenting its provenance – the source, the parameters used, download
URLs, citation guidance, and more (see
[`?get_crosswalk`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
for the full structure):

``` r

metadata <- attr(county_to_puma$crosswalks$step_1, "crosswalk_metadata")

names(metadata)
#>  [1] "call_parameters"               "data_source"                  
#>  [3] "data_source_full_name"         "download_url"                 
#>  [5] "api_endpoint"                  "documentation_url"            
#>  [7] "citation_url"                  "github_repository"            
#>  [9] "source_geography"              "source_geography_standardized"
#> [11] "target_geography"              "target_geography_standardized"
#> [13] "source_year"                   "target_year"                  
#> [15] "reference_year"                "weighting_variable"           
#> [17] "state_coverage"                "notes"                        
#> [19] "retrieved_at"                  "cached"                       
#> [21] "cache_path"                    "read_from_cache"              
#> [23] "is_multi_step"                 "crosswalk_package_version"

metadata$data_source_full_name
#> [1] "Geocorr 2022 (Missouri Census Data Center)"
```

## Applying a crosswalk to data

[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
applies a crosswalk to your data. Let’s fetch county-level poverty
counts from the American Community Survey and translate them to PUMAs:

``` r

county_poverty <- tidycensus::get_acs(
    year = 2023,
    geography = "county",
    output = "wide",
    variables = c(below_poverty = "B17001_002"),
    progress_bar = FALSE) |>
  select(
    source_geoid = GEOID,
    count_below_poverty = below_povertyE)

head(county_poverty)
#> # A tibble: 6 × 2
#>   source_geoid count_below_poverty
#>   <chr>                      <dbl>
#> 1 01001                       6275
#> 2 01003                      24819
#> 3 01005                       4746
#> 4 01007                       4258
#> 5 01009                       8269
#> 6 01011                       2344
```

Note the column naming:
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
detects what to do with each column from its prefix.

| Prefix | Treatment |
|----|----|
| `count_` | Multiplied by the allocation factor, then summed (counts: population, households, loans, …) |
| `mean_`, `median_`, `percent_`, `ratio_` | Weighted mean, using the allocation factor as the weight (rates, averages) |

If you’d rather not rename your columns, pass them explicitly via
`count_columns` and `non_count_columns`. The column holding your GEOIDs
is named by `geoid_column` (default `"source_geoid"`).

``` r

puma_poverty <- crosswalk_data(
  data = county_poverty,
  crosswalk = county_to_puma)
#> Applying crosswalk step 1 of 1...

head(puma_poverty)
#> # A tibble: 6 × 3
#>   geoid   geography_name                                     count_below_poverty
#>   <chr>   <chr>                                                            <dbl>
#> 1 0100100 Lauderdale, Colbert & Franklin Counties                         27686 
#> 2 0100200 Limestone County                                                10561 
#> 3 0100300 Morgan & Lawrence Counties--Decatur City                        20947 
#> 4 0100401 Madison County (North & East)--Huntsville City (E…              11813.
#> 5 0100402 Huntsville (North & Far West), Madison (East) & T…              15934.
#> 6 0100403 Huntsville City (Central & South)                               11240.
```

The result has one row per *target* geography: the target identifier is
named `geoid` (plus `geography_name` when the crosswalk provides it).

## Join quality: did everything match?

Data rows whose GEOIDs don’t appear in the crosswalk can’t be allocated
anywhere, so they’re dropped – silently losing data is the main hazard
of crosswalking, which is why
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
reports on it. The join-quality messages print as it runs (suppress them
with `show_join_quality = FALSE`), and the same statistics are always
attached to the result as the `join_quality` attribute:

``` r

join_quality <- attr(puma_poverty, "join_quality")

## what share of data GEOIDs failed to match the crosswalk?
join_quality$pct_data_unmatched
#> [1] 0

## which GEOIDs were they? (none, in this case)
head(join_quality$data_geoids_unmatched)
#> character(0)
```

See
[`?crosswalk_data`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
for the attribute’s full structure, and
[`vignette("how-interpolation-works")`](https://ui-research.github.io/crosswalk/articles/how-interpolation-works.md)
for how to diagnose imperfect joins.

## Multi-step crosswalks: changing geography *and* year

No single source provides, say, 2014 counties to 2022 PUMAs. When both
the geography and the year change,
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
plans a chain: first change the year (holding geography constant), then
change the geography at the target year.

``` r

county14_to_puma22 <- get_crosswalk(
  source_geography = "county",
  target_geography = "puma",
  source_year = 2014,
  target_year = 2022,
  weight = "population",
  cache = cache_dir)

county14_to_puma22$message
#> [1] "Multi-step crosswalk required:\n  Step 1: 2014 county -> 2022 county (inter-temporal via county_events)\n  Step 2: 2022 county -> 2022 puma (inter-geography via Geocorr)\n\nIntermediate: county (2022)\n\nCompose crosswalks by joining on intermediate geography (county 2022) and multiplying allocation factors: final_allocation = step1_allocation * step2_allocation"
```

You don’t need to do anything special to apply a chain –
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
applies each step in sequence:

``` r

county_poverty_2014 <- tidycensus::get_acs(
    year = 2014,
    geography = "county",
    output = "wide",
    variables = c(below_poverty = "B17001_002"),
    progress_bar = FALSE) |>
  select(
    source_geoid = GEOID,
    count_below_poverty = below_povertyE)

puma22_poverty <- crosswalk_data(
  data = county_poverty_2014,
  crosswalk = county14_to_puma22)

head(puma22_poverty)
#> # A tibble: 6 × 3
#>   geoid   geography_name                                     count_below_poverty
#>   <chr>   <chr>                                                            <dbl>
#> 1 0100100 Lauderdale, Colbert & Franklin Counties                         34405 
#> 2 0100200 Limestone County                                                11684 
#> 3 0100300 Morgan & Lawrence Counties--Decatur City                        23609 
#> 4 0100401 Madison County (North & East)--Huntsville City (E…              13127.
#> 5 0100402 Huntsville (North & Far West), Madison (East) & T…              17707.
#> 6 0100403 Huntsville City (Central & South)                               12490.
```

The attributes on the returned tibble describe the final step; pass
`return_intermediate = TRUE` to get each step’s result (with its own
attributes) alongside the final one.

### Cross-decade crosswalks (NHGIS)

Crosswalks that span a decennial census – 2010 tracts to 2020 tracts,
for example – come from IPUMS NHGIS and require a free API key. Request
one at <https://account.ipums.org/api_keys> and store it in your
`.Renviron` as `IPUMS_API_KEY` (e.g., via `usethis::edit_r_environ()`).
Then:

``` r

tract_2010_to_zcta_2020 <- get_crosswalk(
  source_geography = "tract",
  target_geography = "zcta",
  source_year = 2010,
  target_year = 2020,
  weight = "population")

# Step 1: 2010 tracts -> 2020 tracts (NHGIS)
# Step 2: 2020 tracts -> 2020 ZCTAs (GeoCorr)
```

For a complete worked example – standardizing six years of mortgage data
to 2020 tract definitions – see
[`vignette("standardizing-longitudinal-data")`](https://ui-research.github.io/crosswalk/articles/standardizing-longitudinal-data.md).

## County crosswalks between any pair of years

Counties are occasionally redefined *outside* decennial censuses:
renames (Shannon County, SD becoming Oglala Lakota County in 2015),
merges (Bedford, VA in 2013), splits (Valdez-Cordova, AK in the 2020
vintage), and Connecticut’s 2022 switch to planning regions. The package
curates these events from Census Bureau documentation, so county -\>
county crosswalks work between **any** pair of years from 2000 onward
(forward in time only), with no API key and no network access:

``` r

county_changes <- get_crosswalk(
  source_geography = "county",
  target_geography = "county",
  source_year = 2014,
  target_year = 2023,
  silent = TRUE)

## most counties are unchanged (identity rows); here are some that changed
county_changes$crosswalks$step_1 |>
  filter(source_geoid != target_geoid) |>
  head()
#> # A tibble: 6 × 9
#>   source_geoid target_geoid source_geography_name target_geography_name
#>   <chr>        <chr>        <chr>                 <chr>                
#> 1 02261        02063        county                county               
#> 2 02261        02066        county                county               
#> 3 02270        02158        county                county               
#> 4 09001        09120        county                county               
#> 5 09001        09140        county                county               
#> 6 09001        09190        county                county               
#> # ℹ 5 more variables: source_year <chr>, target_year <chr>,
#> #   allocation_factor_source_to_target <dbl>, weighting_factor <chr>,
#> #   state_fips <chr>
```

See
[`vignette("county-crosswalks")`](https://ui-research.github.io/crosswalk/articles/county-crosswalks.md)
for what’s covered and how these crosswalks are constructed.

## Nested geographies: when you don’t need a crosswalk

If your source geography nests exactly within your target (tracts within
counties, block groups within tracts), there’s nothing to crosswalk –
every source unit belongs to exactly one target.
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
warns and returns an empty crosswalk; just aggregate your data directly:

``` r

nested <- get_crosswalk(
  source_geography = "tract",
  target_geography = "county")
#> Warning: The source geography is nested within the target geography and an empty result
#> will be returned. No crosswalk is needed to translate data between nested geographies;
#> simply aggregate your data to the desired geography.
```

## What combinations are supported?

[`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
lists every supported combination and which source serves it:

``` r

available <- get_available_crosswalks()

available |>
  count(crosswalk_source)
#> # A tibble: 4 × 2
#>   crosswalk_source     n
#>   <chr>            <int>
#> 1 county_events      606
#> 2 ctdata_2020_2022     7
#> 3 geocorr            153
#> 4 nhgis              171

## e.g., everything that translates data out of 2010 tracts
available |>
  filter(source_geography == "tract", source_year == 2010) |>
  head()
#> # A tibble: 6 × 5
#>   source_geography target_geography source_year target_year crosswalk_source
#>   <chr>            <chr>                  <int>       <int> <chr>           
#> 1 tract            county                  2010        2020 nhgis           
#> 2 tract            county                  2010        2022 nhgis           
#> 3 tract            tract                   2010        2011 county_events   
#> 4 tract            tract                   2010        2012 county_events   
#> 5 tract            tract                   2010        2013 county_events   
#> 6 tract            tract                   2010        2014 county_events
```

One nuance: GeoCorr combinations are listed under their reference years
(2022 and 2018), but requests resolve by *decade* – asking for 2021
tracts -\> 2021 ZCTAs is served by the tract -\> zcta 2022 row. See
[`?get_available_crosswalks`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md).

## Caching

We passed `cache = cache_dir` to each
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
call above, so each component crosswalk was saved as a CSV the first
time it was fetched:

``` r

list.files(cache_dir)
#> [1] "crosswalk_county_events_2014_to_2022_county.csv"                        
#> [2] "crosswalk_geocorr_2022_to_2022_county_to_puma_weightedby_population.csv"

## a repeat call with the same parameters reads from disk instead of
## re-downloading
refetched <- get_crosswalk(
  source_geography = "county",
  target_geography = "puma",
  weight = "population",
  cache = cache_dir,
  silent = TRUE)

attr(refetched$crosswalks$step_1, "crosswalk_metadata")$read_from_cache
#> [1] TRUE
```

In a real project, use a persistent directory (e.g.,
`here::here("crosswalks-cache")`) rather than
[`tempdir()`](https://rdrr.io/r/base/tempfile.html). Cached files never
expire; delete a file to force a re-download.

## Quieting the messages

Every function accepts `silent = TRUE`, and
`options(crosswalk.silent = TRUE)` silences the whole package. Silencing
suppresses messages only – the `crosswalk_metadata` and `join_quality`
attributes are always attached.

## Learn more

- [`vignette("how-interpolation-works")`](https://ui-research.github.io/crosswalk/articles/how-interpolation-works.md)
  – how allocation factors are applied, choosing a `weight`, the
  approximation involved in non-count variables, and diagnosing join
  quality
- [`vignette("county-crosswalks")`](https://ui-research.github.io/crosswalk/articles/county-crosswalks.md)
  – county redefinitions and arbitrary-year county crosswalks
- [`vignette("standardizing-longitudinal-data")`](https://ui-research.github.io/crosswalk/articles/standardizing-longitudinal-data.md)
  – a worked example building a six-year tract panel from mixed-vintage
  data
