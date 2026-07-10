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
  custom_interpolations = NULL,
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

  Character or NULL. Source geography name (e.g., "tract", "zcta").
  Required if `crosswalk` is NULL; passed to
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md).
  See
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  for accepted names and
  [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  for all supported combinations.

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

- custom_interpolations:

  A list of lists, each specifying a group of columns and a custom
  interpolation function. Each element must have:

  columns

  :   Character vector of column names

  fn

  :   A function or formula for interpolation. Receives two arguments:
      `.x` (column values) and `.w` (allocation factors). Formulas using
      `~` syntax (e.g., `~sum(.x * .w, na.rm = TRUE)`) are converted to
      functions via
      [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

  Columns in `custom_interpolations` must not overlap with
  `count_columns` or `non_count_columns`. Default is NULL (no custom
  interpolations).

- return_intermediate:

  Logical. If TRUE and crosswalk has multiple steps, returns a list
  containing both the final result and intermediate results from each
  step. Default is FALSE, which returns only the final result.

- show_join_quality:

  Logical. If TRUE (default), prints diagnostic messages about join
  quality, including the number of data rows not matching the crosswalk
  and vice versa. For state-nested geographies (tract, county, block
  group, etc.), also reports state-level concentration of unmatched
  rows. Set to FALSE to suppress these messages; the `join_quality`
  attribute (see the "Join quality diagnostics" section) is computed and
  attached to the result either way. Messages are automatically
  suppressed when `silent = TRUE`.

- silent:

  Logical. If `TRUE`, suppresses all informational messages and
  warnings, including join quality diagnostics regardless of
  `show_join_quality` (the `join_quality` attribute is still attached to
  the result). Defaults to `getOption("crosswalk.silent", FALSE)`. Set
  `options(crosswalk.silent = TRUE)` to silence all calls by default.

## Value

If `return_intermediate = FALSE` (default), a tibble with data
summarized to the final target geography. The target identifier column
is named `geoid` (and, when present in the crosswalk, the geography type
is in `geography_name`). Data rows whose GEOIDs do not match the
crosswalk cannot be allocated to a target geography and are dropped; use
the `join_quality` attribute to inspect them.

If `return_intermediate = TRUE` and there are multiple crosswalk steps,
a list with:

- final:

  The final crosswalked data

- intermediate:

  A named list of intermediate results (step_1, step_2, etc.)

The returned tibble(s) carry two attributes:

- crosswalk_metadata:

  Provenance of the crosswalk that produced the result (access via
  `attr(result, "crosswalk_metadata")`); see the "Crosswalk metadata"
  section of
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md).

- join_quality:

  Statistics describing how well the data joined to the crosswalk
  (access via `attr(result, "join_quality")`); see the "Join quality
  diagnostics" section below. For multi-step crosswalks, both attributes
  describe the *final* step; set `return_intermediate = TRUE` to obtain
  each step's result with its own attributes.

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
as the weight. Note this is an approximation: the allocation factor
reflects each source geography's share allocated to the target, not the
relative size of the source geographies, so weighted means are most
accurate when source units are of broadly similar size.

**One row per GEOID**: `data` must contain at most one row per GEOID.
For panel data with multiple time periods, split the data by period
(e.g., with
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)) and
crosswalk each subset separately.

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
sequence. The attributes on the final result describe only the final
step; to inspect join quality for earlier steps, set
`return_intermediate = TRUE` and examine the attributes of each
intermediate tibble.

## Join quality diagnostics

The `join_quality` attribute is a list with the following elements:

- n_data_total:

  Number of unique GEOIDs in the input data.

- n_data_unmatched:

  Number of data GEOIDs with no match in the crosswalk. These rows
  cannot be allocated to a target geography and are dropped from the
  result.

- pct_data_unmatched:

  `n_data_unmatched` as a percentage (0-100) of `n_data_total`.

- data_geoids_unmatched:

  Character vector of the unmatched data GEOIDs.

- state_analysis_data:

  For state-nested geographies with unmatched data rows, a list
  describing state-level concentration of the unmatched GEOIDs:
  `state_counts` (a tibble of unmatched counts and percentages by state
  FIPS), `top_states` (the three most-affected states), and
  `is_concentrated` (logical; TRUE when any single state accounts for
  more than 15% of unmatched GEOIDs). NULL when there are no unmatched
  rows or state analysis is not applicable.

- n_crosswalk_total:

  Number of unique source GEOIDs in the crosswalk.

- n_crosswalk_unmatched:

  Number of crosswalk source GEOIDs absent from the data (e.g.,
  geographies with no observations).

- pct_crosswalk_unmatched:

  `n_crosswalk_unmatched` as a percentage (0-100) of
  `n_crosswalk_total`.

- crosswalk_geoids_unmatched:

  Character vector of the crosswalk source GEOIDs absent from the data.

- state_analysis_crosswalk:

  As `state_analysis_data`, but for crosswalk GEOIDs absent from the
  data; otherwise NULL.

- source_geography:

  The source geography of the crosswalk step, when known from its
  metadata.

- state_analysis_applicable:

  Logical; whether state-level analysis is meaningful for this geography
  (FALSE for geographies that cross state lines, such as ZCTAs).

## See also

[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
to fetch and inspect crosswalks before applying them;
[`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
for all supported combinations.

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

# Custom interpolation functions
result <- crosswalk_data(
  data = my_data,
  crosswalk = crosswalk,
  custom_interpolations = list(
    list(
      columns = c("count_population", "count_housing"),
      fn = ~sum(.x * .w, na.rm = TRUE)
    ),
    list(
      columns = c("pct_poverty"),
      fn = ~weighted.mean(.x, .w, na.rm = TRUE)
    )
  ))
} # }
```
