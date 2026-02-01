# Tests for get_ctdata_crosswalk() - CT Data Collaborative crosswalks

# ==============================================================================
# Basic structure tests
# ==============================================================================

test_that("get_ctdata_crosswalk returns correct structure for tracts", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract")

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "source_geoid", "target_geoid",
    "source_geography_name", "target_geography_name",
    "source_year", "target_year",
    "allocation_factor_source_to_target",
    "weighting_factor", "state_fips")

  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("get_ctdata_crosswalk tract data has correct values", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract")

  ## CT tracts have different source and target geoids (879 CT tracts)
  ## Other states have identical source and target geoids (identity mapping)
  ct_tracts <- result |>
    dplyr::filter(source_geoid != target_geoid)
  expect_equal(nrow(ct_tracts), 879)
  expect_true(all(ct_tracts$state_fips == "09"))

  non_ct_tracts <- result |>
    dplyr::filter(source_geoid == target_geoid)
  expect_true(all(non_ct_tracts$state_fips != "09"))

  expect_equal(unique(result$source_year), "2020")
  expect_equal(unique(result$target_year), "2022")
  expect_equal(length(unique(result$state_fips)), 52)
  expect_equal(unique(result$weighting_factor), "identity")
  expect_true(all(result$allocation_factor_source_to_target == 1))

  expect_equal(unique(result$source_geography_name), "tract")
  expect_equal(unique(result$target_geography_name), "tract")
})

test_that("get_ctdata_crosswalk returns nationally comprehensive data", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract")

  # Should have 52 states/territories (50 states + DC + PR)
  expect_equal(length(unique(result$state_fips)), 52)

  # Should have many more than 879 tracts (national coverage)
  expect_gt(nrow(result), 70000)

  # CT should be included
  expect_true("09" %in% result$state_fips)
})

# ==============================================================================
# Geography handling tests
# ==============================================================================

test_that("get_ctdata_crosswalk handles block geography", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "block")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography_name), "block")
  expect_equal(unique(result$target_geography_name), "block")

  expect_true(all(stringr::str_length(result$source_geoid) == 15))
  expect_true(all(stringr::str_length(result$target_geoid) == 15))
})

test_that("get_ctdata_crosswalk handles block_group geography", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "block_group")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography_name), "block_group")
  expect_equal(unique(result$target_geography_name), "block_group")

  expect_true(all(stringr::str_length(result$source_geoid) == 12))
  expect_true(all(stringr::str_length(result$target_geoid) == 12))
})

test_that("get_ctdata_crosswalk handles county geography", {
  skip_if_offline()
  skip_if_not_installed("tidycensus")
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "CENSUS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "county")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography_name), "county")

  # Filter to CT for county-specific checks
  ct_result <- result |> dplyr::filter(state_fips == "09")

  n_source_counties <- length(unique(ct_result$source_geoid))
  n_target_regions <- length(unique(ct_result$target_geoid))
  expect_equal(n_source_counties, 8)
  expect_equal(n_target_regions, 9)

  # CT uses population weighting, other states use identity
  expect_equal(unique(ct_result$weighting_factor), "population")

  # CT allocation factors should sum to 1 for each source county
  ct_allocation_sums <- ct_result |>
    dplyr::summarize(
      total = sum(allocation_factor_source_to_target),
      .by = "source_geoid")
  expect_true(all(abs(ct_allocation_sums$total - 1) < 0.001))

  # Non-CT counties should have identity mapping
  non_ct_result <- result |> dplyr::filter(state_fips != "09")
  expect_equal(unique(non_ct_result$weighting_factor), "identity")
})

# ==============================================================================
# Error handling tests
# ==============================================================================

test_that("get_ctdata_crosswalk errors on unsupported geography", {
  expect_error(
    get_ctdata_crosswalk(geography = "zcta"),
    regexp = "not supported")

  expect_error(
    get_ctdata_crosswalk(geography = "place"),
    regexp = "not supported")
})

# ==============================================================================
# Geography spelling variations tests
# ==============================================================================

test_that("get_ctdata_crosswalk accepts various geography spellings", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result1 <- get_ctdata_crosswalk(geography = "tract")
  result2 <- get_ctdata_crosswalk(geography = "tracts")
  result3 <- get_ctdata_crosswalk(geography = "tr")

  expect_equal(nrow(result1), nrow(result2))
  expect_equal(nrow(result1), nrow(result3))
})

test_that("get_ctdata_crosswalk accepts block group spelling variations", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result1 <- get_ctdata_crosswalk(geography = "block_group")
  result2 <- get_ctdata_crosswalk(geography = "block group")
  result3 <- get_ctdata_crosswalk(geography = "bg")

  expect_equal(nrow(result1), nrow(result2))
  expect_equal(nrow(result1), nrow(result3))
})

# ==============================================================================
# Caching tests
# ==============================================================================

test_that("get_ctdata_crosswalk caching works", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  cache_dir <- tempfile("crosswalk_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  result1 <- get_ctdata_crosswalk(geography = "tract", cache = cache_dir)

  # Correct filename for national crosswalk
  cached_file <- file.path(cache_dir, "crosswalk_national_2020_to_2022_tract.csv")
  expect_true(file.exists(cached_file))

  result2 <- get_ctdata_crosswalk(geography = "tract", cache = cache_dir)

  # Compare data only, excluding metadata attributes that differ between fresh and cached
  # (retrieved_at differs, read_from_cache differs)
  expect_equal(
    result1 |> tibble::as_tibble(),
    result2 |> tibble::as_tibble(),
    ignore_attr = TRUE)

  # Verify cached read has correct read_from_cache flag
  meta1 <- attr(result1, "crosswalk_metadata")
  meta2 <- attr(result2, "crosswalk_metadata")
  expect_false(meta1$read_from_cache)
  expect_true(meta2$read_from_cache)
})

# ==============================================================================
# Metadata tests
# ==============================================================================

test_that("get_ctdata_crosswalk attaches metadata", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract")

  metadata <- attr(result, "crosswalk_metadata")

  expect_type(metadata, "list")
  expect_equal(metadata$data_source, "ctdata_nhgis_combined")
  expect_true(stringr::str_detect(metadata$ctdata_download_url, "https://raw.githubusercontent.com/CT-Data-Collaborative"))
  expect_equal(metadata$source_year, "2020")
  expect_equal(metadata$target_year, "2022")
  expect_equal(metadata$state_coverage, "National (all 50 states, DC, and Puerto Rico)")
})

# ==============================================================================
# Data integrity tests
# ==============================================================================

test_that("CT tract GEOIDs have correct format changes", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract")

  # Filter to CT records only for this test

  ct_result <- result |> dplyr::filter(state_fips == "09")

  expect_true(all(stringr::str_starts(ct_result$source_geoid, "09")))
  expect_true(all(stringr::str_starts(ct_result$target_geoid, "09")))

  expect_true(all(stringr::str_length(ct_result$source_geoid) == 11))
  expect_true(all(stringr::str_length(ct_result$target_geoid) == 11))

  # CT county FIPS codes changed between 2020 and 2022
  source_counties <- stringr::str_sub(ct_result$source_geoid, 3, 5)
  target_counties <- stringr::str_sub(ct_result$target_geoid, 3, 5)
  expect_false(all(source_counties == target_counties))
})

test_that("CT county crosswalk maps 8 old counties to 9 planning regions", {
  skip_if_offline()
  skip_if_not_installed("tidycensus")
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "CENSUS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "county")

  # Filter to CT only for this test
  ct_result <- result |> dplyr::filter(state_fips == "09")

  n_source_counties <- length(unique(ct_result$source_geoid))
  n_target_regions <- length(unique(ct_result$target_geoid))

  expect_equal(n_source_counties, 8)
  expect_equal(n_target_regions, 9)

  # Should have more rows than source counties due to many-to-many mapping
  expect_gt(nrow(ct_result), 8)

  expect_true(all(ct_result$allocation_factor_source_to_target > 0))
  expect_true(all(ct_result$allocation_factor_source_to_target <= 1))

  # Non-CT counties should have identity mapping (allocation_factor = 1)
  non_ct_result <- result |> dplyr::filter(state_fips != "09")
  expect_true(all(non_ct_result$allocation_factor_source_to_target == 1))
  expect_true(all(non_ct_result$source_geoid == non_ct_result$target_geoid))
})

# ==============================================================================
# Reverse direction tests (2022 -> 2020)
# ==============================================================================

test_that("get_ctdata_crosswalk supports 2022 -> 2020 for tracts", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract", source_year = 2022, target_year = 2020)

  expect_s3_class(result, "tbl_df")

  # Check years are reversed
  expect_equal(unique(result$source_year), "2022")
  expect_equal(unique(result$target_year), "2020")

  # CT tracts should have reversed geoids (2022 FIPS -> 2020 FIPS)
  ct_tracts <- result |> dplyr::filter(state_fips == "09")
  expect_equal(nrow(ct_tracts), 879)
  expect_true(all(ct_tracts$source_geoid != ct_tracts$target_geoid))

  # Non-CT tracts should still have identical geoids (identity mapping both directions)
  non_ct_tracts <- result |> dplyr::filter(state_fips != "09")
  expect_true(all(non_ct_tracts$source_geoid == non_ct_tracts$target_geoid))

  # Allocation factors should all be 1 (identity)
  expect_true(all(result$allocation_factor_source_to_target == 1))
})

test_that("get_ctdata_crosswalk supports 2022 -> 2020 for blocks", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "block", source_year = 2022, target_year = 2020)

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_year), "2022")
  expect_equal(unique(result$target_year), "2020")

  # Block GEOIDs should be 15 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 15))
  expect_true(all(stringr::str_length(result$target_geoid) == 15))

  # All records should have allocation_factor = 1
  expect_true(all(result$allocation_factor_source_to_target == 1))
})

test_that("get_ctdata_crosswalk supports 2022 -> 2020 for block_groups", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "block_group", source_year = 2022, target_year = 2020)

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_year), "2022")
  expect_equal(unique(result$target_year), "2020")

  # Block group GEOIDs should be 12 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 12))
  expect_true(all(stringr::str_length(result$target_geoid) == 12))

  # All records should have allocation_factor = 1
  expect_true(all(result$allocation_factor_source_to_target == 1))
})

test_that("get_ctdata_crosswalk errors on 2022 -> 2020 for county", {
  expect_error(
    get_ctdata_crosswalk(geography = "county", source_year = 2022, target_year = 2020),
    regexp = "County crosswalks from 2022 to 2020 are not supported")
})

test_that("2022 -> 2020 crosswalk is inverse of 2020 -> 2022 for tracts", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  forward <- get_ctdata_crosswalk(geography = "tract", source_year = 2020, target_year = 2022)
  reverse <- get_ctdata_crosswalk(geography = "tract", source_year = 2022, target_year = 2020)

  # Same number of rows
  expect_equal(nrow(forward), nrow(reverse))

  # Forward source_geoid should match reverse target_geoid
  forward_ct <- forward |>
    dplyr::filter(state_fips == "09") |>
    dplyr::arrange(source_geoid)
  reverse_ct <- reverse |>
    dplyr::filter(state_fips == "09") |>
    dplyr::arrange(target_geoid)

  expect_equal(forward_ct$source_geoid, reverse_ct$target_geoid)
  expect_equal(forward_ct$target_geoid, reverse_ct$source_geoid)
})

test_that("get_ctdata_crosswalk metadata reflects direction", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "tract", source_year = 2022, target_year = 2020)

  metadata <- attr(result, "crosswalk_metadata")

  expect_equal(metadata$source_year, "2022")
  expect_equal(metadata$target_year, "2020")
})

test_that("get_ctdata_crosswalk errors on invalid year combinations", {
  expect_error(
    get_ctdata_crosswalk(geography = "tract", source_year = 2010, target_year = 2020),
    regexp = "CTData crosswalks are only available for 2020 <-> 2022")

  expect_error(
    get_ctdata_crosswalk(geography = "tract", source_year = 2020, target_year = 2010),
    regexp = "CTData crosswalks are only available for 2020 <-> 2022")
})
