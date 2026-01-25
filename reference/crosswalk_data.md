# Interpolate data using a crosswalk(s)

Applies geographic crosswalk weights to transform data from a source
geography to a target geography. Accepts the output from
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
and automatically applies all crosswalk steps sequentially for
multi-step transformations.

## Usage

``` r
crosswalk_data(
  data,
  crosswalk,
  geoid_column = "geoid",
  count_columns = NULL,
  non_count_columns = NULL,
  return_intermediate = FALSE,
  show_join_quality = TRUE
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
  compatibility.

- geoid_column:

  Character. The name of the column in `data` containing the source
  geography identifiers (GEOIDs). Default is "geoid".

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
  rows. Set to FALSE to suppress these messages.

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
# Single-step crosswalk
crosswalk <- get_crosswalk(
  source_geography = "tract",
  target_geography = "zcta",
  weight = "population")

result <- crosswalk_data(
  data = my_tract_data,
  crosswalk = crosswalk,
  geoid_column = "tract_geoid",
  count_columns = c("count_population", "count_housing_units"))

# Multi-step crosswalk (geography + year change)
crosswalk <- get_crosswalk(
  source_geography = "tract",
  target_geography = "zcta",
  source_year = 2010,
  target_year = 2020,
  weight = "population")

# Automatically applies both steps
result <- crosswalk_data(
  data = my_data,
  crosswalk = crosswalk,
  geoid_column = "tract_geoid",
  count_columns = "count_population")

# To get intermediate results
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
