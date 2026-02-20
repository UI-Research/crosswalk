# Interpolate data using a crosswalk(s)

Applies geographic crosswalk weights to transform data from a source
geography to a target geography. Can either accept a pre-fetched
crosswalk from
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
or fetch the crosswalk automatically using the provided geography and
year parameters.

## Usage

``` r
crosswalk_data(
  data,
  crosswalk = NULL,
  source_geography = NULL,
  target_geography = NULL,
  source_year = NULL,
  target_year = NULL,
  weight = "population",
  cache = NULL,
  geoid_column = "source_geoid",
  count_columns = NULL,
  non_count_columns = NULL,
  return_intermediate = FALSE,
  show_join_quality = TRUE,
  silent = getOption("crosswalk.silent", FALSE)
)
```

## Arguments

- data:

  A data frame or tibble containing the data to crosswalk.

- crosswalk:

  The output from
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md) -
  a list containing:

  crosswalks

  :   A named list of crosswalk tibbles (step_1, step_2, etc.)

  plan

  :   The crosswalk plan

  message

  :   Description of the crosswalk chain

  Alternatively, a single crosswalk tibble can be provided for backwards
  compatibility. If NULL, the crosswalk will be fetched using
  `source_geography` and `target_geography` parameters.

- source_geography:

  Character or NULL. Source geography name. Required if `crosswalk` is
  NULL. One of c("block", "block group", "tract", "place", "county",
  "urban_area", "zcta", "puma", "cd118", "cd119",
  "core_based_statistical_area").

- target_geography:

  Character or NULL. Target geography name. Required if `crosswalk` is
  NULL. Same options as `source_geography`.

- source_year:

  Numeric or NULL. Year of the source geography. If NULL and crosswalk
  is being fetched, uses same-year crosswalk via Geocorr.

- target_year:

  Numeric or NULL. Year of the target geography. If NULL and crosswalk
  is being fetched, uses same-year crosswalk via Geocorr.

- weight:

  Character. Weighting variable for Geocorr crosswalks when fetching.
  One of c("population", "housing", "land"). Default is "population".

- cache:

  Directory path or NULL. Where to cache fetched crosswalks. If NULL
  (default), crosswalk is fetched but not saved to disk.

- geoid_column:

  Character. The name of the column in `data` containing the source
  geography identifiers (GEOIDs). Default is "source_geoid".

- count_columns:

  Character vector or NULL. Column names in `data` that represent count
  variables. These will be summed after multiplying by the allocation
  factor. If NULL (default), automatically detects columns with the
  prefix "count\_".

- non_count_columns:

  Character vector or NULL. Column names in `data` that represent mean,
  median, percentage, and ratio variables. These will be calculated as
  weighted means using the allocation factor as weights. If NULL
  (default), automatically detects columns with prefixes "mean\_",
  "median\_", "percent\_", or "ratio\_".

- return_intermediate:

  Logical. If TRUE and crosswalk has multiple steps, returns a list
  containing both the final result and intermediate results from each
  step. Default is FALSE, which returns only the final result.

- show_join_quality:

  Logical. If TRUE (default), prints diagnostic messages about join
  quality, including the number of data rows not matching the crosswalk
  and vice versa. For state-nested geographies (tract, county, block
  group, etc.), also reports state-level concentration of unmatched
  rows. Set to FALSE to suppress these messages. Automatically
  suppressed when `silent = TRUE`.

- silent:

  Logical. If `TRUE`, suppresses all informational messages and
  warnings, including join quality diagnostics regardless of
  `show_join_quality`. Defaults to
  `getOption("crosswalk.silent", FALSE)`. Set
  `options(crosswalk.silent = TRUE)` to silence all calls by default.

## Value

If `return_intermediate = FALSE` (default), a tibble with data
summarized to the final target geography.

If `return_intermediate = TRUE` and there are multiple crosswalk steps,
a list with:

- final:

  The final crosswalked data

- intermediate:

  A named list of intermediate results (step_1, step_2, etc.)

The returned tibble(s) include an attribute `crosswalk_metadata` from
the underlying crosswalk (access via
`attr(result, "crosswalk_metadata")`).

## Details

**Two usage patterns**:

1.  **Pre-fetched crosswalk**: Pass the output of
    [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
    to the `crosswalk` parameter. Useful when you want to inspect or
    reuse the crosswalk.

2.  **Direct crosswalking**: Pass `source_geography` and
    `target_geography` (and optionally `source_year`, `target_year`,
    `weight`, `cache`) and the crosswalk will be fetched automatically.
    Useful for one-off transformations.

**Count variables** (specified in `count_columns`) are interpolated by
summing the product of the value and the allocation factor across all
source geographies that overlap with each target geography.

**Non-count variables** (specified in `non_count_columns`) are
interpolated using a weighted mean, with the allocation factor serving
as the weight.

**Automatic column detection**: If `count_columns` and
`non_count_columns` are both NULL, the function will automatically
detect columns based on naming prefixes:

- Columns starting with "count\_" are treated as count variables

- Columns starting with "mean\_", "median\_", "percent\_", or "ratio\_"
  are treated as non-count variables

**Other columns**: Columns that are not the geoid column, count columns,
or non-count columns (e.g., metadata like `data_year`) are preserved by
taking the first non-missing value within each target geography group.
If all values are missing, NA is returned.

**Multi-step crosswalks**: When
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
returns multiple crosswalks (for transformations that change both
geography and year), this function automatically applies them in
sequence.

## Examples

``` r
if (FALSE) { # \dontrun{
# Option 1: Pre-fetched crosswalk
crosswalk <- get_crosswalk(
  source_geography = "tract",
  target_geography = "zcta",
  weight = "population")

result <- crosswalk_data(
  data = my_tract_data,
  crosswalk = crosswalk,
  geoid_column = "tract_geoid",
  count_columns = c("count_population", "count_housing_units"))

# Option 2: Direct crosswalking (crosswalk fetched automatically)
result <- crosswalk_data(
  data = my_tract_data,
  source_geography = "tract",
  target_geography = "zcta",
  weight = "population",
  geoid_column = "tract_geoid",
  count_columns = c("count_population", "count_housing_units"))

# Direct crosswalking with year change
result <- crosswalk_data(
  data = my_data,
  source_geography = "tract",
  target_geography = "zcta",
  source_year = 2010,
  target_year = 2020,
  weight = "population",
  geoid_column = "tract_geoid",
  count_columns = "count_population")

# Pre-fetched crosswalk with intermediate results
crosswalk <- get_crosswalk(
  source_geography = "tract",
  target_geography = "zcta",
  source_year = 2010,
  target_year = 2020,
  weight = "population")

result <- crosswalk_data(
  data = my_data,
  crosswalk = crosswalk,
  geoid_column = "tract_geoid",
  count_columns = "count_population",
  return_intermediate = TRUE)

# Access intermediate and final
result$intermediate$step_1  # After first crosswalk
result$final                # Final result
} # }
```
