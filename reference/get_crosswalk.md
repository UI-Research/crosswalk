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
  weight = "population",
  silent = getOption("crosswalk.silent", FALSE)
)
```

## Arguments

- source_geography:

  Character. Source geography name. For same-year (GeoCorr) crosswalks,
  one of c("block", "block group", "tract", "county", "place", "zcta",
  "puma", "puma12", "puma22", "cd115", "cd116", "cd118", "cd119"). For
  cross-decade (NHGIS) crosswalks, sources are limited to c("block",
  "block group", "tract"). Use
  [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  for the full listing of supported combinations.

- target_geography:

  Character. Target geography name. For same-year (GeoCorr) crosswalks,
  one of c("block", "block group", "tract", "county", "place", "zcta",
  "puma", "puma12", "puma22", "cd115", "cd116", "cd118", "cd119"). For
  cross-decade (NHGIS) crosswalks, targets additionally include
  c("urban_area", "core_based_statistical_area"). Use
  [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  for the full listing.

- source_year:

  Character or numeric. Year of the source geography, one of c(1990,
  2000, 2010, 2020) for decennial years, or c(2011, 2012, 2014,
  2015, 2022) for non-census years (limited to block groups, tracts, and
  counties). For county -\> county crosswalks, any year from 2000 onward
  is supported; for same-decade tract/block group/block crosswalks, any
  year from 2010 onward (see Details).

- target_year:

  Character or numeric. Year of the target geography, one of c(1990,
  2000, 2010, 2020) for decennial crosswalks, or c(2011, 2012, 2014,
  2015, 2022) for non-census year crosswalks (limited to block groups,
  tracts, and counties). For county -\> county crosswalks, any year from
  2000 onward is supported; for same-decade tract/block group/block
  crosswalks, any year from 2010 onward (see Details).

- cache:

  Directory path. Where to save the crosswalk. If NULL (default), the
  crosswalk is returned but not saved to disk. When provided, individual
  component crosswalks are cached separately, and subsequent calls with
  the same parameters read from the cached files instead of
  re-downloading (see Details).

- weight:

  Character. Weighting variable used to construct or select allocation
  factors. One of c("population", "housing", "land"). Applies to GeoCorr
  crosswalks and to the selection of the NHGIS interpolation weight
  (block-based NHGIS crosswalks provide a single combined weight, which
  is used regardless of this argument).

- silent:

  Logical. If `TRUE`, suppresses all informational messages and
  warnings. Defaults to `getOption("crosswalk.silent", FALSE)`. Set
  `options(crosswalk.silent = TRUE)` to silence all calls by default.

## Value

A list with a consistent structure:

- crosswalks:

  A named list of crosswalk tibbles (step_1, step_2, etc.). Single-step
  transformations have one crosswalk; multi-step have two or more.

- plan:

  The crosswalk plan describing the transformation steps

- message:

  A formatted message describing the crosswalk chain

For nested same-year requests (e.g., tract -\> county),
`crosswalks$step_1` is an empty tibble and a warning explains that no
crosswalk is needed (see Details).

Each crosswalk tibble includes an attribute `crosswalk_metadata` (access
via `attr(result$crosswalks$step_1, "crosswalk_metadata")`) documenting
how the crosswalk was produced; see the "Crosswalk metadata" section.

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
NHGIS, CT Data Collaborative, and a curated registry of county change
events. Crosswalk weights are from the original sources and have not
been modified; this function merely standardizes the format of the
returned crosswalks and enables easy programmatic access and caching.

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

**Arbitrary-year county crosswalks (county change events)**: Counties
and county equivalents are occasionally redefined outside decennial
censuses (e.g., the 2013 Bedford, VA merge; the 2015 Shannon -\> Oglala
Lakota, SD FIPS change; the Valdez-Cordova, AK split, first reflected in
2020-vintage products; the 2022 Connecticut planning regions). County
-\> county crosswalks are therefore available between ANY pair of years
from 2000 through the curated horizon (e.g., 2014 -\> 2019 or 2014 -\>
2023), built from an identity mapping plus the documented change events
in the interval, with population-based allocation factors for splits and
part transfers. County crosswalks are forward-only (source_year \<
target_year). For sub-county geographies (tract, block group, block)
affected by these county changes, same-decade crosswalks are available
from 2010 onward (e.g., tract 2014 -\> 2019); year pairs spanning a
decennial census are planned automatically as chains through NHGIS
crosswalks (e.g., tract 2014 -\> 2023).

**Nested geographies**: When the source geography nests exactly within
the target geography and the years match (e.g., tract -\> county, block
group -\> tract), no crosswalk is needed: every source unit belongs to
exactly one target unit. In this case the function warns and returns the
standard list structure with an *empty* crosswalk tibble – aggregate
your data directly instead (e.g., sum tract values by the first five
GEOID characters to obtain county values).

**Caching**: When `cache` is a directory path, each component crosswalk
is written there as a CSV, and subsequent calls with the same parameters
read the file from disk instead of re-downloading. Cached files never
expire; delete a file to force a fresh download. The metadata attribute
records `cache_path` and whether the crosswalk was `read_from_cache`.

Note that an IPUMS NHGIS API key is required to access crosswalks from
that source. Use `usethis::edit_r_environ(scope = "user")` to save your
API key to your .Renviron; the name of the key should be
"IPUMS_API_KEY". You can obtain a key from:
https://account.ipums.org/api_keys.

## Crosswalk metadata

The `crosswalk_metadata` attribute attached to each crosswalk tibble is
a list recording the crosswalk's provenance. Fields with no value for a
given source are NULL. Elements include:

- call_parameters:

  The arguments the crosswalk was requested with (`source_geography`,
  `target_geography`, `source_year`, `target_year`, `weight`, `cache`).

- data_source, data_source_full_name:

  Short code and full name of the source that produced the crosswalk
  (e.g., "geocorr", "Geocorr 2022 (Missouri Census Data Center)").

- download_url, api_endpoint, documentation_url, citation_url,
  github_repository:

  Links to the underlying data, its documentation, and citation
  guidance, where applicable.

- source_geography, target_geography, source_year, target_year:

  The requested geographies and years, plus
  `source_geography_standardized` / `target_geography_standardized` (the
  source-specific codes they resolved to) and `reference_year` (the
  source's reference vintage, e.g., the GeoCorr version).

- weighting_variable:

  The weighting variable used to construct or select allocation factors.

- state_coverage, notes:

  Geographic coverage notes and source-specific caveats.

- retrieved_at, cached, cache_path, read_from_cache:

  When the crosswalk was retrieved, and whether it was written to or
  read from a local cache.

- is_multi_step, crosswalk_package_version:

  Whether the crosswalk is part of a multi-step chain, and the package
  version that produced it.

## See also

[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
to apply the returned crosswalk(s) to data;
[`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
for all supported geography/year combinations;
[`list_nhgis_crosswalks()`](https://ui-research.github.io/crosswalk/reference/list_nhgis_crosswalks.md)
for the NHGIS-specific listing.

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

# Arbitrary-year county crosswalk handling non-decennial county changes
# (e.g., 2015 Shannon -> Oglala Lakota, SD; Valdez-Cordova, AK split)
result <- get_crosswalk(
  source_geography = "county",
  target_geography = "county",
  source_year = 2014,
  target_year = 2023)
} # }
```
