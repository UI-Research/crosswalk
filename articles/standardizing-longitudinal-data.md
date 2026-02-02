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
glimpse(hmda_data[["2018"]])
#> Rows: 74,652
#> Columns: 74
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
#> $ race_black_purchase                <dbl> 0, 1, 2, 3, 5, 19, 5, 14, 3, 37, 6,…
#> $ race_hispanic_purchase             <dbl> 0, 1, 0, 1, 2, 7, 1, 2, 0, 5, 3, 0,…
#> $ race_asian_purchase                <dbl> 0, 0, 0, 0, 0, 2, 2, 0, 0, 4, 0, 0,…
#> $ race_nhpi_purchase                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ race_asian_or_nhpi_purchase        <dbl> 0, 0, 0, 0, 0, 2, 2, 0, 0, 5, 0, 0,…
#> $ race_multiple_purchase             <dbl> 0, 0, 0, 2, 0, 11, 2, 2, 0, 6, 1, 0…
#> $ race_missing_purchase              <dbl> 0, 0, 1, 2, 3, 14, 3, 2, 3, 10, 3, …
#> $ race_mixed_purchase                <dbl> 0, 0, 0, 0, 0, 4, 1, 1, 1, 5, 3, 0,…
#> $ race_aian_purchase                 <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_indian_purchase         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_chinese_purchase        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ race_asian_filipino_purchase       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_japanese_purchase       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_korean_purchase         <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_vietnamese_purchase     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ income_verylow_purchase            <dbl> 0, 1, 2, 12, 11, 5, 3, 5, 0, 6, 3, …
#> $ income_low_purchase                <dbl> 0, 8, 4, 14, 23, 30, 10, 27, 1, 34,…
#> $ income_moderate_purchase           <dbl> 0, 3, 2, 7, 19, 52, 10, 23, 4, 68, …
#> $ income_high_purchase               <dbl> 0, 9, 3, 8, 11, 97, 19, 13, 36, 122…
#> $ race_white_income_verylow          <dbl> 0, 1, 2, 10, 11, 4, 2, 4, 0, 5, 2, …
#> $ race_white_income_low              <dbl> 0, 7, 4, 12, 19, 24, 7, 20, 1, 25, …
#> $ race_white_income_moderate         <dbl> 0, 3, 0, 5, 14, 39, 8, 15, 3, 48, 1…
#> $ race_white_income_high             <dbl> 0, 8, 1, 6, 10, 60, 11, 8, 30, 84, …
#> $ race_black_income_verylow          <dbl> 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1,…
#> $ race_black_income_low              <dbl> 0, 0, 0, 1, 4, 1, 1, 6, 0, 4, 3, 1,…
#> $ race_black_income_moderate         <dbl> 0, 0, 0, 1, 1, 6, 1, 5, 0, 14, 0, 1…
#> $ race_black_income_high             <dbl> 0, 1, 2, 1, 0, 12, 2, 2, 3, 18, 3, …
#> $ race_hispanic_income_verylow       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,…
#> $ race_hispanic_income_low           <dbl> 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,…
#> $ race_hispanic_income_moderate      <dbl> 0, 0, 0, 1, 2, 2, 0, 1, 0, 1, 1, 0,…
#> $ race_hispanic_income_high          <dbl> 0, 0, 0, 0, 0, 4, 0, 1, 0, 4, 1, 0,…
#> $ race_aian_income_verylow           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_aian_income_low               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_aian_income_moderate          <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_aian_income_high              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_or_nhpi_income_verylow  <dbl> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
#> $ race_asian_or_nhpi_income_low      <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0,…
#> $ race_asian_or_nhpi_income_moderate <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ race_asian_or_nhpi_income_high     <dbl> 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 0, 0,…
#> $ race_multiple_income_verylow       <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_multiple_income_low           <dbl> 0, 0, 0, 1, 0, 4, 0, 1, 0, 1, 0, 0,…
#> $ race_multiple_income_moderate      <dbl> 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 1, 0,…
#> $ race_multiple_income_high          <dbl> 0, 0, 0, 0, 0, 5, 2, 1, 0, 4, 0, 0,…
#> $ race_mixed_income_verylow          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_mixed_income_low              <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_mixed_income_moderate         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 0,…
#> $ race_mixed_income_high             <dbl> 0, 0, 0, 0, 0, 4, 1, 1, 1, 2, 1, 0,…
#> $ race_missing_income_verylow        <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ race_missing_income_low            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0,…
#> $ race_missing_income_moderate       <dbl> 0, 0, 1, 0, 2, 3, 1, 2, 1, 0, 2, 0,…
#> $ race_missing_income_high           <dbl> 0, 0, 0, 1, 1, 11, 2, 0, 2, 8, 1, 0…
#> $ age_older_purchase                 <dbl> 0, 1, 0, 1, 5, 7, 2, 2, 1, 10, 3, 0…
#> $ age_middleolder_purchase           <dbl> 0, 5, 5, 8, 13, 41, 16, 12, 17, 53,…
#> $ age_middleyounger_purchase         <dbl> 2, 10, 5, 23, 37, 117, 21, 37, 19, …
#> $ age_younger_purchase               <dbl> 0, 4, 1, 9, 8, 10, 4, 11, 0, 8, 7, …
#> $ age_mixed_purchase                 <dbl> 0, 1, 0, 0, 1, 10, 1, 6, 4, 12, 6, …
#> $ income_median                      <dbl> 152500, 61000, 40000, 41000, 47500,…
#> $ owner_loan_amount_median           <dbl> 360000, 145000, 135000, 105000, 135…
#> $ occupancy_unitsoccupied_count      <dbl> NA, 765, 719, 1296, 1639, 4174, 133…
#> $ occupancy_owneroccupied_count      <dbl> NA, 570, 464, 841, 1262, 2321, 1031…
#> $ occupancy_owneroccupied_percent    <dbl> NA, 0.75, 0.65, 0.65, 0.77, 0.56, 0…
#> $ invalid_geo                        <dbl> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ vintage                            <dbl> 2010, 2010, 2010, 2010, 2010, 2010,…
#> $ data_year                          <int> 2018, 2018, 2018, 2018, 2018, 2018,…
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
hmda_combined <- bind_rows(hmda_crosswalked) |>
  ## data for years that are crosswalked have slightly different/additional columsn
  mutate(
    geoid = if_else(is.na(geoid), source_geoid, geoid)) |>
  select(-c(geography_name, source_geoid, vintage)) |>
  arrange(geoid, data_year) |>
  mutate(
    state = str_sub(geoid, 1, 2),
    percent_race_white_purchase = count_race_white_purchase / count_owner_purchase_originations)

## there's a little bit of variation year-to-year in terms of which tracts have
## reported HMDA data, but for the majority, we have observations in each of the
## six years:
hmda_combined |>
  count(geoid) |>
  count(n)
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
