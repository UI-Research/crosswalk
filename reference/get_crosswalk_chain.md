# Get a Chain of Crosswalks for Multi-Step Transformations

Retrieves a list of crosswalks needed to transform data from a source
geography/year to a target geography/year. For multi-step
transformations, users should apply each crosswalk sequentially using
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md).

## Usage

``` r
get_crosswalk_chain(
  source_geography,
  target_geography,
  source_year = NULL,
  target_year = NULL,
  weight = "population",
  cache = NULL
)
```

## Arguments

- source_geography:

  Character. Source geography name.

- target_geography:

  Character. Target geography name.

- source_year:

  Numeric or NULL. Year of the source geography.

- target_year:

  Numeric or NULL. Year of the target geography.

- weight:

  Character or NULL. Weighting variable for Geocorr crosswalks.

- cache:

  Directory path or NULL. Where to cache crosswalks.

## Value

A list with:

- crosswalks:

  A named list of crosswalk tibbles (step_1, step_2, etc.)

- plan:

  The crosswalk plan from plan_crosswalk_chain()

- message:

  A formatted message describing the crosswalk chain

## Examples

``` r
if (FALSE) { # \dontrun{
# Get crosswalks for 2010 tracts to 2020 ZCTAs (requires two steps)
chain <- get_crosswalk_chain(
  source_geography = "tract",
  target_geography = "zcta",
  source_year = 2010,
  target_year = 2020,
  weight = "population")

# Apply crosswalks sequentially
data_step1 <- crosswalk_data(
  data = my_data,
  crosswalk = chain$crosswalks$step_1,
  count_columns = "count_population")

data_final <- crosswalk_data(
  data = data_step1,
  crosswalk = chain$crosswalks$step_2,
  count_columns = "count_population")
} # }
```
