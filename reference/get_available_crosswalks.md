# List All Available Crosswalk Combinations

Returns a tibble of all source/target geography and year combinations
supported by
[`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md).

## Usage

``` r
get_available_crosswalks()
```

## Value

A tibble with columns: `source_geography`, `target_geography`,
`source_year`, `target_year`.
