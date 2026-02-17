# Standardizing Longitudinal Data

## Overview

A common challenge with longitudinal tract data is that census tract
boundaries change between decennial censuses. Data from before 2020
typically uses 2010 tract definitions, while more recent data uses 2020
tract definitions. To analyze trends over time, you need to standardize
all years to a consistent tract vintage.

This vignette demonstrates how the `crosswalk` package efficiently
handles this task using the Urban Institute’s [HMDA Neighborhood Summary
Files](https://datacatalog.urban.org/dataset/home-mortgage-disclosure-act-neighborhood-summary-files-census-tract-level),
which provide tract-level mortgage lending data from 2018-2023. The
2018-2021 files use 2010 tract definitions, while 2022-2023 use 2020
tract definitions.

## Setup

``` r
library(crosswalk)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(tidycensus)
library(ggplot2)
```

## Step 1: Download the Data

The Urban Institute publishes annual HMDA tract-level summary files.
Let’s download all six years (2018-2023):

``` r
## metadata object describing data year/vintage/url
metadata = tibble::tribble(
  ~ year, ~ vintage, ~ url,
  2018, 2010, "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/12/20/hmda_tract_2018.csv",
  2019, 2010, "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/12/20/hmda_tract_2019.csv",
  2020, 2010, "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/12/20/hmda_tract_2020.csv",
  2021, 2010, "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/12/20/hmda_tract_2021.csv",
  2022, 2020, "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2023/12/20/hmda_tract_2022.csv",
  2023, 2020, "https://urban-data-catalog.s3.amazonaws.com/drupal-root-live/2024/12/17/hmda_tract_2023.csv")

## iterate over the metadata object and read in data for each year
hmda_data <- pmap(metadata, function(url, year, vintage) {
  read_csv(url, show_col_types = FALSE) |>
    mutate(
      vintage = vintage,
      data_year = as.integer(year)) })

names(hmda_data) = metadata$year %>% as.character()
```

Let’s inspect the structure of the data:

``` r
## just view the first ten columns
glimpse(hmda_data[["2018"]] %>% select(1:10))
#> Rows: 74,652
#> Columns: 10
#> $ geo2010                            <chr> "00XXXXXXXXX", "01001020100", "0100…
#> $ owner_purchase_originations        <dbl> 2, 21, 11, 41, 64, 185, 44, 68, 41,…
#> $ occupancy_investment_origination   <dbl> 2, 4, 4, 3, 6, 19, 3, 5, 3, 5, 6, 0…
#> $ occupancy_investment_units_1_4     <dbl> 2, 4, 3, 3, 6, 17, 3, 5, 3, 5, 5, 0…
#> $ occupancy_investment_units_5ormore <dbl> 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 1, 0,…
#> $ income_available                   <dbl> 0, 21, 11, 41, 64, 184, 42, 68, 41,…
#> $ race_available                     <dbl> 2, 21, 10, 39, 61, 171, 41, 66, 38,…
#> $ race_income_available              <dbl> 0, 21, 10, 39, 61, 170, 39, 66, 38,…
#> $ age_available                      <dbl> 2, 21, 11, 41, 64, 185, 44, 68, 41,…
#> $ race_white_purchase                <dbl> 2, 19, 7, 33, 54, 128, 30, 47, 34, …
```

## Step 2: Prepare Data for Crosswalking

We’ll focus on a subset of variables for crosswalking (total
applications by race/ethnicity and median loan amounts). We could
explicitly pass the variables we want to crosswalk to the appropriate
parameter (`count_columns` or `non_count_columns`), but it’s easy (and
nice practice) to prefix these variables with their unit types (“count”
and “median”, respectively), and
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
will crosswalk each appropriately by default.

``` r
prepare_hmda <- function(data) {
  data |>
    rename_with(.cols = matches("^geo20"), .fn = ~ "source_geoid") |>
    select(
      source_geoid,
      vintage,
      data_year,
      # Count variables: rename with count_ prefix for automatic detection
      count_race_white_purchase = race_white_purchase,
      count_owner_purchase_originations = owner_purchase_originations,
      median_owner_loan_amount = owner_loan_amount_median) |>
    mutate(source_geoid = as.character(source_geoid)) }

hmda_prepared <- map(hmda_data, prepare_hmda)
```

## Step 3: Obtain the 2010→2020 Tract Crosswalk

Next we get our crosswalk (the same for each of our 2010-vintage years
of HMDA data), which contains allocation factors that specify how to
distribute values from 2010 tracts definitions to those for 2020 tracts.

``` r
tract_crosswalk <- get_crosswalk(
  source_geography = "tract",
  target_geography = "tract",
  source_year = 2010,
  target_year = 2020,
  weight = "population")

# View the crosswalk plan
tract_crosswalk$message
#> [1] "Single-step crosswalk:\n  Step 1: 2010 tract -> 2020 tract (inter-temporal)\n\nSingle crosswalk; use allocation_factor_source_to_target directly."
```

## Step 4: Apply the Crosswalk to 2018-2021 Data

Now we apply the crosswalk to the four years of data that use 2010 tract
definitions. We can see that relatively small, though not insignificant,
fractions of records in our source data do not join to our crosswalk.
When this occurs, source data is effectively lost because it has no
associated target geography nor allocation factor assigned to it.

``` r
# Years that need crosswalking (2010 vintage)
years_to_crosswalk <- c("2018", "2019", "2020", "2021")

# Apply crosswalk to each year
hmda_crosswalked <- map_if(
  .x = hmda_prepared, 
  .p = names(hmda_prepared) %in% years_to_crosswalk,
  .f = ~ crosswalk_data(
      data = .x,
      crosswalk = tract_crosswalk,
      geoid_column = "source_geoid",
      show_join_quality = TRUE)) 
```

## Step 5: Assess Crosswalking Quality

Is there anything we can learn about our source data that doesn’t join
to our crosswalk? Ideally, every record in our source data maps to a
record in our crosswalk.

``` r
## we see that some observations that don't match have "XXXXXX" in lieu of
## a real tract code--which, from reading the data documentation, we know is
## done to to retain valid observations that, in the source data, do not have 
## a valid tract identifier but do have valid county and/or state identifies
hmda_crosswalked |>
  map(~ 
    .x |> 
    attr("join_quality") |> 
    pluck("data_geoids_unmatched") |>
    head(5))
#> $`2018`
#> [1] "00XXXXXXXXX" "01001XXXXXX" "01003XXXXXX" "01005XXXXXX" "01007XXXXXX"
#> 
#> $`2019`
#> [1] "01001XXXXXX" "01003XXXXXX" "01005XXXXXX" "01007XXXXXX" "01009XXXXXX"
#> 
#> $`2020`
#> [1] "01001XXXXXX" "01003XXXXXX" "01005XXXXXX" "01007XXXXXX" "01009XXXXXX"
#> 
#> $`2021`
#> [1] "01001XXXXXX" "01003XXXXXX" "01005XXXXXX" "01007XXXXXX" "01009XXXXXX"
#> 
#> $`2022`
#> NULL
#> 
#> $`2023`
#> NULL
```

``` r
## how many source records are we unable to crosswalk each year, excluding
## those with "X" in their GEOIDs? under 30 each year.
hmda_crosswalked |>
  map(~ 
    .x |> 
    attr("join_quality") |> 
    pluck("data_geoids_unmatched") %>%
    .[!str_detect(., "X")] |>
    length())
#> $`2018`
#> [1] 25
#> 
#> $`2019`
#> [1] 27
#> 
#> $`2020`
#> [1] 29
#> 
#> $`2021`
#> [1] 24
#> 
#> $`2022`
#> [1] 0
#> 
#> $`2023`
#> [1] 0
```

## Result: A Panel Dataset in 2020 Tract Definitions

We now have a single dataframe with all six years of HMDA data
standardized to 2020 tract definitions. Due to changes in tract
geographies between decades, we were previously unable to accurately
compare neighborhood changes over time.

Now, we have apples-to-apples measurements for tracts from 2018 through
2023.

``` r
## there's a little bit of variation year-to-year in terms of which tracts have
## reported HMDA data, but for the majority, we have observations in each of the
## six years:
hmda_combined <- bind_rows(hmda_crosswalked) |>
  ## data for years that are crosswalked have slightly different/additional columns
  mutate(
    geoid = if_else(is.na(geoid), source_geoid, geoid)) |>
  count(geoid) |>
  count(n)

head(hmda_combined)
#> # A tibble: 6 × 2
#>       n    nn
#>   <int> <int>
#> 1     1   626
#> 2     2  2457
#> 3     3   107
#> 4     4   560
#> 5     5   414
#> 6     6 82672
```
