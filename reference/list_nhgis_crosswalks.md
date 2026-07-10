# List supported NHGIS crosswalks

Returns a tibble of all IPUMS NHGIS crosswalks available through
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md).
NHGIS provides the package's inter-temporal (cross-decade) crosswalks;
fetching them requires an `IPUMS_API_KEY` (see
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)).
This listing itself is static and works offline.

## Usage

``` r
list_nhgis_crosswalks()
```

## Value

A tibble with one row per NHGIS crosswalk and columns:

- source_geography:

  Source geography name (e.g., "block", "tract")

- source_year:

  Year of the source geography (character)

- target_geography:

  Target geography name

- target_year:

  Year of the target geography (character)

- crosswalk_path:

  URL of the underlying NHGIS crosswalk file

## See also

[`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
for supported combinations across all sources;
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
to fetch a crosswalk.

## Examples

``` r
# All NHGIS crosswalks from 2010 tracts
list_nhgis_crosswalks() |>
  dplyr::filter(source_geography == "tract", source_year == "2010")
#> # A tibble: 4 × 5
#>   source_geography source_year target_geography target_year crosswalk_path      
#>   <chr>            <chr>       <chr>            <chr>       <chr>               
#> 1 tract            2010        tract            2020        https://api.ipums.o…
#> 2 tract            2010        tract            2022        https://api.ipums.o…
#> 3 tract            2010        county           2020        https://api.ipums.o…
#> 4 tract            2010        county           2022        https://api.ipums.o…
```
