# List supported NHGIS crosswalks

Returns a tibble of all available NHGIS geographic crosswalks with their
corresponding parameters that can be used with get_nhgis_crosswalk().

## Usage

``` r
list_nhgis_crosswalks()
```

## Value

A tibble with columns:

- source_year: Year of the source geography

- source_geography: Source geography name

- target_year: Year of the target geography

- target_geography: Target geography name
