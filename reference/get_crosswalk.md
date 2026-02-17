# Get a crosswalk(s) to translate data across time and geographies

Retrieves a crosswalk with interpolation values from a source geography
to a target geography, optionally across different years. Always returns
a list with a consistent structure containing one or more crosswalk
tibbles.

## Usage

``` r
get_crosswalk(
  source_geography,
  target_geography,
  source_year = NULL,
  target_year = NULL,
  cache = NULL,
  weight = "population"
)
```

## Arguments

- source_geography:

  Character. Source geography name. One of c("block", "block group",
  "tract", "place", "county", "urban_area", "zcta", "puma", "puma12",
  "puma22", "cd115", "cd116", "cd118", "cd119", "urban_area",
  "core_based_statistical_area").

- target_geography:

  Character. Target geography name. One of c("block", "block group",
  "tract", "place", "county", "urban_area", "zcta", "puma", "puma12",
  "puma22", "cd115", "cd116", "cd118", "cd119", "urban_area",
  "core_based_statistical_area").

- source_year:

  Character or numeric. Year of the source geography, one of c(1990,
  2000, 2010, 2020).

- target_year:

  Character or numeric. Year of the target geography, one of c(1990,
  2000, 2010, 2020) for decennial crosswalks, or c(2011, 2012, 2014,
  2015, 2022) for non-census year crosswalks (limited to block groups,
  tracts, and counties).

- cache:

  Directory path. Where to download the crosswalk to. If NULL (default),
  crosswalk is returned but not saved to disk. Individual component
  crosswalks are cached separately when provided.

- weight:

  Character. Weighting variable for Geocorr crosswalks. One of
  c("population", "housing", "land").

## Value

A list with a consistent structure:

- crosswalks:

  A named list of crosswalk tibbles (step_1, step_2, etc.). Single-step
  transformations have one crosswalk; multi-step have two or more.

- plan:

  The crosswalk plan describing the transformation steps

- message:

  A formatted message describing the crosswalk chain

Each crosswalk tibble includes an attribute `crosswalk_metadata` (access
via `attr(result$crosswalks$step_1, "crosswalk_metadata")`) containing
comprehensive information about how the crosswalk was produced.

Columns in returned crosswalk dataframes (some may not be present
depending on source):

- source_geoid:

  A unique identifier for the source geography

- target_geoid:

  A unique identifier for the target geography

- source_geography_name:

  The name of the source geography

- target_geography_name:

  The name of the target geography

- source_year:

  The year of the source geography

- target_year:

  The year of the target geography

- allocation_factor_source_to_target:

  The weight to interpolate values from the source geography to the
  target geography

- allocation_factor_target_to_source:

  The weight to interpolate values from the target geography to the
  source geography

- population_2020:

  The estimated overlap in population, if applicable

- housing_2020:

  The estimated overlap in housing units, if applicable

- land_area_sqmi:

  The overlap in land area, if applicable

- weighting_factor:

  The attribute used to calculate allocation factors

- state_fips:

  Two-digit state FIPS code, if applicable

## Details

This function sources crosswalks from Geocorr 2022, Geocorr 2018, IPUMS
NHGIS, and CT Data Collaborative. Crosswalk weights are from the
original sources and have not been modified; this function merely
standardizes the format of the returned crosswalks and enables easy
programmatic access and caching.

**GeoCorr version selection**: For same-year geography crosswalks, the
appropriate GeoCorr version is selected automatically based on the year:

- Years 2020+ (or no year specified): GeoCorr 2022 (2020 Census
  geography)

- Years 2010-2019: GeoCorr 2018 (2010 Census geography)

**Geography name resolution**: User-facing geography names like "puma",
"zcta", "place", and "blockgroup" are automatically resolved to the
correct API codes for the selected GeoCorr version. Version-specific
names are also accepted (e.g., "puma12" for GeoCorr 2018, "puma22" for
GeoCorr 2022).

**Multi-step crosswalks**: When both geography AND year change (e.g.,
2010 tracts to 2020 ZCTAs), no single crosswalk source provides this
directly. This function returns multiple crosswalks that should be
applied sequentially:

1.  First crosswalk changes year (via NHGIS): source_geog(source_year)
    -\> source_geog(target_year)

2.  Second crosswalk changes geography (via Geocorr):
    source_geog(target_year) -\> target_geog(target_year)

**Non-census year support**: For target years 2011, 2012, 2014, 2015,
and 2022, crosswalks are available only for block groups, tracts, and
counties. These years correspond to American Community Survey geography
changes.

**2020 to 2022 crosswalks**: The 2022 geographic changes only affected
Connecticut (county-equivalent planning regions replaced historical
counties). For this case, the function combines CT Data Collaborative
crosswalks for Connecticut with identity mappings for other states.

Note that an IPUMS NHGIS API key is required to access crosswalks from
that source. Use `usethis::edit_r_environ(scope = "user")` to save your
API key to your .Renviron; the name of the key should be
"IPUMS_API_KEY". You can obtain a key from:
https://account.ipums.org/api_keys.

## Examples

``` r
if (FALSE) { # \dontrun{
# Same-year crosswalk between geographies (uses Geocorr)
# Returns list with one crosswalk in crosswalks$step_1
result <- get_crosswalk(
  source_geography = "zcta",
  target_geography = "puma22",
  weight = "population",
  cache = here::here("crosswalks-cache"))

# Apply to data using crosswalk_data()
output <- crosswalk_data(
  data = my_data,
  crosswalk = result,
  count_columns = "count_population")

# Multi-step crosswalk: both geography AND year change
# Returns list with two crosswalks in crosswalks$step_1 and crosswalks$step_2
result <- get_crosswalk(
  source_geography = "tract",
  target_geography = "zcta",
  source_year = 2010,
  target_year = 2020,
  weight = "population")

# crosswalk_data() automatically applies all steps
output <- crosswalk_data(
  data = my_data,
  crosswalk = result,
  count_columns = "count_population")

# To get intermediate results, set return_intermediate = TRUE
output <- crosswalk_data(
  data = my_data,
  crosswalk = result,
  count_columns = "count_population",
  return_intermediate = TRUE)
} # }
```
