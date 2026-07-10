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

Note: this vignette is precomputed because it downloads six years of
HMDA data and an NHGIS crosswalk (which requires an `IPUMS_API_KEY`).
The outputs shown were generated when the vignette was last knit from
its source (`vignettes/standardizing-longitudinal-data.Rmd.orig`); see
`vignettes/precompile.R`.

## Setup

``` r

library(crosswalk)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
```

## Step 1: Download the Data

The Urban Institute publishes annual HMDA tract-level summary files.
Let’s download all six years (2018-2023):

``` r

## metadata object describing data year/vintage/url
metadata <- tribble(
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

names(hmda_data) <- as.character(metadata$year)
```

Let’s inspect the structure of the data:

``` r

## just view the first ten columns
glimpse(hmda_data[["2018"]] |> select(1:10))
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

Note that non-count variables like `median_owner_loan_amount` are
interpolated as weighted means, which is an approximation – see
[`vignette("how-interpolation-works")`](https://ui-research.github.io/crosswalk/articles/how-interpolation-works.md)
for how to interpret crosswalked non-count values.

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
#> [1] "Single-step crosswalk:\n  Step 1: 2010 tract -> 2020 tract (inter-temporal via nhgis)\n\nSingle crosswalk; use allocation_factor_source_to_target directly."
```

## Step 4: Apply the Crosswalk to 2018-2021 Data

Now we apply the crosswalk to the four years of data that use 2010 tract
definitions.
[`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
prints join-quality diagnostics as it works (shown below); relatively
small, though not insignificant, fractions of records in our source data
do not join to our crosswalk. When this occurs, source data is
effectively lost because it has no associated target geography nor
allocation factor assigned to it.

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
#> Applying crosswalk step 1 of 1...
#> Join quality: 2,643 of 74,652 unique data GEOIDs (3.5%) did not match the crosswalk.
#>   Top states with unmatched data rows: TX (9%, 233 rows), GA (6%, 154 rows), KY (4%, 114 rows)
#> Join quality: 1,993 of 74,002 crosswalk source GEOIDs (2.7%) were not in input data.
#>   Top states not in data: PR (47%, 943 rows), NY (7%, 147 rows), CA (5%,  93 rows)
#>   (This is expected if your data covers a geographic subset.)
#> Applying crosswalk step 1 of 1...
#> Join quality: 2,447 of 74,485 unique data GEOIDs (3.3%) did not match the crosswalk.
#>   Top states with unmatched data rows: TX (10%, 238 rows), GA (6%, 154 rows), KY (4%, 106 rows)
#> Join quality: 1,964 of 74,002 crosswalk source GEOIDs (2.7%) were not in input data.
#>   Top states not in data: PR (48%, 942 rows), NY (7%, 143 rows), CA (5%,  90 rows)
#>   (This is expected if your data covers a geographic subset.)
#> Applying crosswalk step 1 of 1...
#> Join quality: 2,622 of 74,689 unique data GEOIDs (3.5%) did not match the crosswalk.
#>   Top states with unmatched data rows: TX (9%, 239 rows), GA (6%, 154 rows), KY (4%, 115 rows)
#> Join quality: 1,935 of 74,002 crosswalk source GEOIDs (2.6%) were not in input data.
#>   Top states not in data: PR (49%, 944 rows), NY (7%, 135 rows), MI (5%,  92 rows)
#>   (This is expected if your data covers a geographic subset.)
#> Applying crosswalk step 1 of 1...
#> Join quality: 2,700 of 74,811 unique data GEOIDs (3.6%) did not match the crosswalk.
#>   Top states with unmatched data rows: TX (9%, 239 rows), GA (6%, 158 rows), KY (4%, 115 rows)
#> Join quality: 1,891 of 74,002 crosswalk source GEOIDs (2.6%) were not in input data.
#>   Top states not in data: PR (50%, 943 rows), NY (7%, 136 rows), CA (4%,  85 rows)
#>   (This is expected if your data covers a geographic subset.)
```

## Step 5: Assess Crosswalking Quality

The same diagnostics printed above are attached to each result as the
`join_quality` attribute (see
[`?crosswalk_data`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
for its full structure), so we can work with them programmatically. Is
there anything we can learn about our source data that doesn’t join to
our crosswalk? Ideally, every record in our source data maps to a record
in our crosswalk.

``` r

## we see that some observations that don't match have "XXXXXX" in lieu of
## a real tract code--which, from reading the data documentation, we know is
## done to to retain valid observations that, in the source data, do not have
## a valid tract identifier but do have valid county and/or state identifies
hmda_crosswalked[years_to_crosswalk] |>
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
```

``` r

## how many source records are we unable to crosswalk each year, excluding
## those with "X" in their GEOIDs? under 30 each year.
hmda_crosswalked[years_to_crosswalk] |>
  map(~
    .x |>
    attr("join_quality") |>
    pluck("data_geoids_unmatched") |>
    discard(~ str_detect(.x, "X")) |>
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
```

## Result: A Panel Dataset in 2020 Tract Definitions

We now have all six years of HMDA data standardized to 2020 tract
definitions. Due to changes in tract geographies between decades, we
were previously unable to accurately compare neighborhood changes over
time.

``` r

hmda_combined <- bind_rows(hmda_crosswalked) |>
  ## crosswalked years name their tract identifier `geoid`; the 2022-2023
  ## years (already in 2020 definitions) still use `source_geoid`
  mutate(geoid = if_else(is.na(geoid), source_geoid, geoid))

## there's a little bit of variation year-to-year in terms of which tracts have
## reported HMDA data, but the large majority of tracts have observations in
## each of the six years:
hmda_combined |>
  count(geoid, name = "n_years_observed") |>
  count(n_years_observed)
#> # A tibble: 6 × 2
#>   n_years_observed     n
#>              <int> <int>
#> 1                1   626
#> 2                2  2457
#> 3                3   107
#> 4                4   559
#> 5                5   414
#> 6                6 82672
```

To see the payoff, consider a 2010 tract that was split into multiple
2020 tracts. Before crosswalking, its 2018-2021 lending activity was
reported under a tract identifier that no longer exists in the 2022-2023
data; after crosswalking, each of its 2020 successor tracts has an
apples-to-apples series covering all six years:

``` r

## find the 2010 tract split across the most 2020 tracts
split_tract <- tract_crosswalk$crosswalks$step_1 |>
  summarize(.by = source_geoid, n_targets = n_distinct(target_geoid)) |>
  filter(n_targets > 1) |>
  arrange(desc(n_targets), source_geoid) |>
  slice(1) |>
  pull(source_geoid)

successor_tracts <- tract_crosswalk$crosswalks$step_1 |>
  filter(source_geoid == split_tract) |>
  pull(target_geoid)

split_tract
#> [1] "51810990100"

## owner-occupied purchase originations, by year, for the successor tracts
hmda_combined |>
  filter(geoid %in% successor_tracts) |>
  select(geoid, data_year, count_owner_purchase_originations) |>
  arrange(geoid, data_year) |>
  pivot_wider(
    names_from = data_year,
    values_from = count_owner_purchase_originations)
#> # A tibble: 17 × 7
#>    geoid       `2018` `2019` `2020` `2021` `2022` `2023`
#>    <chr>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 51810040000   0      0      0      2        NA     NA
#>  2 51810041801  51     71     52     61        46     44
#>  3 51810041803  61.8   63.3   69.0   64.2      47     37
#>  4 51810041804  68.2   69.7   76.0   70.8      41     37
#>  5 51810043004  57     51     56     70        49     43
#>  6 51810043005  40.6   47.3   53.1   56.2      36     24
#>  7 51810043006  56.5   65.8   74.0   78.1      63     50
#>  8 51810043200   1.42   2.39   1.42   1.88     NA      1
#>  9 51810043400  32.0   34.1   38.0   43.0      41     17
#> 10 51810043600  25.0   35.0   33.0   38.0      33     15
#> 11 51810043800  69.3   53.3   66.3   64.7      34     31
#> 12 51810044004  41.1   48.1   51.1   40.2      15     13
#> 13 51810044008  35.4   38.9   42.7   46.9      19     23
#> 14 51810045200  10.1   18.2   18.2   14.2      12     11
#> 15 51810045412  17     19     36     26        21     18
#> 16 51810046400  65.0   57.0   86.0   75.0      65     40
#> 17 51810990100   0      0      0      0         0     NA
```

Each row is a 2020 tract with a complete 2018-2023 series: the 2018-2021
values are allocated shares of the original (since-split) 2010 tract,
and the 2022-2023 values come directly from the source data.
