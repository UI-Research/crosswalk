# County Crosswalks Between Any Years

Most geographic change in the census system happens on decennial
schedules – but counties don’t wait for the decade. Since 2000, counties
and county equivalents have been created, merged, split, renamed, and
transferred between one another a dozen-plus times. A county panel from,
say, 2014 to 2023 that ignores these events will silently misalign
Alaska, South Dakota, Virginia, and all of Connecticut.

The `crosswalk` package curates these events from the Census Bureau’s
[Substantial Changes to
Counties](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html)
documentation and ships them as internal package data, so county -\>
county crosswalks work between **any** pair of years from 2000 onward –
offline, with no API key.

``` r

library(crosswalk)
library(dplyr)
```

## What’s covered

| Event | Type | First vintage with new codes |
|----|----|----|
| Clifton Forge, VA gives up independent-city status | Merge | 2002 |
| Broomfield County, CO created | Split (from parts of four counties) | 2002 |
| Southeast Alaska reorganizations (Skagway, Hoonah-Angoon, Wrangell, Petersburg, Prince of Wales) | Splits/merges | 2008-2009 |
| York County / Newport News, VA territory exchange | Part transfer | 2008 |
| Bedford City, VA rejoins Bedford County | Merge | 2014 |
| Petersburg Borough, AK created | Split/merge | 2014 |
| Shannon County, SD renamed Oglala Lakota County (46113 -\> 46102) | FIPS change | 2015 |
| Wade Hampton, AK renamed Kusilvak (02270 -\> 02158) | FIPS change | 2015 |
| Valdez-Cordova, AK split into Chugach and Copper River | Split | 2020 |
| Connecticut counties replaced by nine planning regions | Reorganization | 2022 |

A subtlety worth internalizing: the years above are Census Bureau
*product vintages*, not effective dates. The Valdez-Cordova split took
legal effect on January 2, 2019, but 2019-vintage ACS products still use
the old county; the new codes first appear in 2020-vintage products. The
package keys every event to the first product vintage that uses the
post-change codes (verified empirically against ACS county inventories),
because that’s what determines which codes appear in the data you’re
crosswalking.

## Basic usage

Request any forward year pair. The result composes all events in the
interval on top of an identity mapping over the full county universe –
unchanged counties appear with an allocation factor of 1, so the
crosswalk is never sparse:

``` r

county_14_23 <- get_crosswalk(
  source_geography = "county",
  target_geography = "county",
  source_year = 2014,
  target_year = 2023,
  silent = TRUE)

crosswalk_14_23 <- county_14_23$crosswalks$step_1

nrow(crosswalk_14_23)
#> [1] 3232

## the counties that changed between 2014 and 2023
crosswalk_14_23 |>
  filter(source_geoid != target_geoid)
#> # A tibble: 23 × 9
#>    source_geoid target_geoid source_geography_name target_geography_name
#>    <chr>        <chr>        <chr>                 <chr>                
#>  1 02261        02063        county                county               
#>  2 02261        02066        county                county               
#>  3 02270        02158        county                county               
#>  4 09001        09120        county                county               
#>  5 09001        09140        county                county               
#>  6 09001        09190        county                county               
#>  7 09003        09110        county                county               
#>  8 09003        09140        county                county               
#>  9 09003        09160        county                county               
#> 10 09005        09140        county                county               
#> # ℹ 13 more rows
#> # ℹ 5 more variables: source_year <chr>, target_year <chr>,
#> #   allocation_factor_source_to_target <dbl>, weighting_factor <chr>,
#> #   state_fips <chr>
```

Renames (like 46113 -\> 46102) carry an allocation factor of 1 with an
`identity` weighting factor. Splits allocate the source county across
its successors with population-based factors:

``` r

county_19_21 <- get_crosswalk(
  source_geography = "county",
  target_geography = "county",
  source_year = 2019,
  target_year = 2021,
  silent = TRUE)

## Valdez-Cordova (02261) split into Chugach (02063) and Copper River (02066)
county_19_21$crosswalks$step_1 |>
  filter(source_geoid == "02261")
#> # A tibble: 2 × 9
#>   source_geoid target_geoid source_geography_name target_geography_name
#>   <chr>        <chr>        <chr>                 <chr>                
#> 1 02261        02063        county                county               
#> 2 02261        02066        county                county               
#> # ℹ 5 more variables: source_year <chr>, target_year <chr>,
#> #   allocation_factor_source_to_target <dbl>, weighting_factor <chr>,
#> #   state_fips <chr>
```

Applying these crosswalks works exactly as described in
[`vignette("crosswalk")`](https://ui-research.github.io/crosswalk/articles/crosswalk.md):
pass the result to
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
with your county-level data.

## Forward-only, with one exception

County temporal crosswalks require `source_year < target_year`:

``` r

get_crosswalk(
  source_geography = "county",
  target_geography = "county",
  source_year = 2021,
  target_year = 2019)
#> Error in `get_crosswalk()`:
#> ! County crosswalks from 2021 to 2019 are not supported: county temporal crosswalks are forward-only (reversing county changes would require disaggregating merged counties). Swap source_year and target_year to crosswalk forward in time.
```

The asymmetry is inherent: composing a *merge* forward is just addition,
but reversing it would require disaggregating the merged county’s values
back into its components, which a crosswalk row cannot honestly express
without additional data. Rather than return allocation factors that look
precise but aren’t, the package asks you to standardize *forward* to the
latest year in your panel.

## Sub-county geographies

County changes ripple into the GEOIDs of everything nested inside a
county: when Shannon County became Oglala Lakota, every tract, block
group, and block in `46113...` was relabeled to `46102...`. The package
serves these relabels for same-decade year pairs from 2010 onward:

``` r

tract_14_19 <- get_crosswalk(
  source_geography = "tract",
  target_geography = "tract",
  source_year = 2014,
  target_year = 2019,
  silent = TRUE)

## identity rows for all unchanged tracts, relabels for the affected ones
tract_14_19$crosswalks$step_1 |>
  filter(source_geoid != target_geoid) |>
  head()
#> # A tibble: 4 × 9
#>   source_geoid target_geoid source_geography_name target_geography_name
#>   <chr>        <chr>        <chr>                 <chr>                
#> 1 02270000100  02158000100  tract                 tract                
#> 2 46113940500  46102940500  tract                 tract                
#> 3 46113940800  46102940800  tract                 tract                
#> 4 46113940900  46102940900  tract                 tract                
#> # ℹ 5 more variables: source_year <chr>, target_year <chr>,
#> #   allocation_factor_source_to_target <dbl>, weighting_factor <chr>,
#> #   state_fips <chr>
```

Because these sub-county changes are exact 1:1 relabels, they are also
the one case where *backward* temporal crosswalks are offered (e.g.,
tract 2019 -\> 2014) – reversing a relabel is lossless in a way
reversing a merge is not.

Year pairs that span a decennial census (e.g., tract 2014 -\> 2023)
can’t be pure relabels, because tract boundaries themselves changed in
2020. For those,
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
automatically plans a chain that routes through NHGIS cross-decade
crosswalks (and so requires an `IPUMS_API_KEY`):

``` r

## planned automatically as: 2014 tracts -[relabel]-> 2010-decade vintage
## -[NHGIS]-> 2020-decade vintage -[relabel]-> 2023 tracts
tract_14_23 <- get_crosswalk(
  source_geography = "tract",
  target_geography = "tract",
  source_year = 2014,
  target_year = 2023)
```

## Connecticut, briefly

Connecticut’s 2022 replacement of its eight counties with nine
planning-region county equivalents is the most sweeping recent change –
every Connecticut county GEOID changed, and the old and new units don’t
nest. Direct 2020 \<-\> 2022 requests are served by crosswalks from the
CT Data Collaborative (county-level requests fetch county GEOIDs via
`tidycensus` and so need a `CENSUS_API_KEY`); county requests spanning
2022 (like the 2014 -\> 2023 example above) incorporate the same change
through the curated event registry, with population-based allocation
factors where old counties split across planning regions.

## Where the data comes from

The event registry, GEOID mappings, and year-by-year county universes
are built by `data-raw/build_county_events_sysdata.R` from Census Bureau
documentation and ACS county inventories, and ship as internal package
data (`R/sysdata.rda`). See
[`data-raw/README.md`](https://github.com/UI-Research/crosswalk/blob/main/data-raw/README.md)
for provenance details.
