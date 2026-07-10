# List All Available Crosswalk Combinations

Returns a tibble of all source/target geography and year combinations
supported by
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md),
along with the crosswalk source that serves each combination.

## Usage

``` r
get_available_crosswalks()
```

## Value

A tibble with columns: `source_geography`, `target_geography`,
`source_year`, `target_year`, `crosswalk_source`.

## Details

**How to read the year columns.** Years identify the geography
*vintages* a crosswalk translates between, not the only request years
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
accepts:

- **GeoCorr** (same-year geography changes) rows are listed under their
  reference years: 2022 for 2020-Census geography and 2018 for
  2010-Census geography. Requests with other year contexts resolve to
  the matching version – years 2020 and later (or no years at all) use
  GeoCorr 2022, and years 2010-2019 use GeoCorr 2018. For example, a
  request for 2021 tracts -\> 2021 ZCTAs is served by the tract -\> zcta
  2022 row.

- **County-events** rows enumerate every supported year pair explicitly
  (county -\> county: any forward pair from 2000 on; tract, block group,
  and block: same-decade pairs from 2010 on, including exactly
  reversible relabels).

**Nested geographies.** Same-year pairs in which the source geography
nests within the target (e.g., tract -\> county) are listed because
GeoCorr serves them, but
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
returns an empty crosswalk with a warning for these: no crosswalk is
needed – aggregate your data directly instead (e.g., tracts nest within
counties, so tract values can be summed by the first five GEOID
characters).

**API keys by source.** The `crosswalk_source` column indicates which
source
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
uses for a direct request of that combination (multi-step chains may
combine several):

- `geocorr`: no API key required

- `nhgis`: requires `IPUMS_API_KEY` (see
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md))

- `ctdata_2020_2022`: county requests require a Census API key
  (`CENSUS_API_KEY`) and the `tidycensus` package; sub-county requests
  require neither

- `county_events`: no API key; the data ships with the package and works
  offline

## See also

[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
to fetch a crosswalk;
[`list_nhgis_crosswalks()`](https://ui-research.github.io/crosswalk/reference/list_nhgis_crosswalks.md)
for the underlying NHGIS listing.

## Examples

``` r
available <- get_available_crosswalks()

# All combinations that translate data into 2020 tracts
available |>
  dplyr::filter(target_geography == "tract", target_year == 2020)
#> # A tibble: 16 × 5
#>    source_geography target_geography source_year target_year crosswalk_source
#>    <chr>            <chr>                  <int>       <int> <chr>           
#>  1 block            tract                   2010        2020 nhgis           
#>  2 block_group      tract                   2010        2020 nhgis           
#>  3 block_group      tract                   2011        2020 nhgis           
#>  4 block_group      tract                   2012        2020 nhgis           
#>  5 block_group      tract                   2014        2020 nhgis           
#>  6 block_group      tract                   2015        2020 nhgis           
#>  7 tract            tract                   2010        2020 nhgis           
#>  8 tract            tract                   2011        2020 nhgis           
#>  9 tract            tract                   2012        2020 nhgis           
#> 10 tract            tract                   2014        2020 nhgis           
#> 11 tract            tract                   2015        2020 nhgis           
#> 12 tract            tract                   2021        2020 county_events   
#> 13 tract            tract                   2022        2020 ctdata_2020_2022
#> 14 tract            tract                   2023        2020 county_events   
#> 15 tract            tract                   2024        2020 county_events   
#> 16 tract            tract                   2025        2020 county_events   

# County -> county crosswalks support arbitrary year pairs from 2000 on
available |>
  dplyr::filter(source_geography == "county", target_geography == "county")
#> # A tibble: 325 × 5
#>    source_geography target_geography source_year target_year crosswalk_source
#>    <chr>            <chr>                  <int>       <int> <chr>           
#>  1 county           county                  2000        2001 county_events   
#>  2 county           county                  2000        2002 county_events   
#>  3 county           county                  2000        2003 county_events   
#>  4 county           county                  2000        2004 county_events   
#>  5 county           county                  2000        2005 county_events   
#>  6 county           county                  2000        2006 county_events   
#>  7 county           county                  2000        2007 county_events   
#>  8 county           county                  2000        2008 county_events   
#>  9 county           county                  2000        2009 county_events   
#> 10 county           county                  2000        2010 county_events   
#> # ℹ 315 more rows
```
