# Tests for get_geocorr_crosswalk() - Geocorr 2022 crosswalks

# ==============================================================================
# Basic structure tests
# ==============================================================================

test_that("get_geocorr_crosswalk returns correct structure for tract to zcta", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "source_geoid", "target_geoid",
    "source_geography_name", "target_geography_name",
    "allocation_factor_source_to_target",
    "allocation_factor_target_to_source",
    "weighting_factor")

  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("get_geocorr_crosswalk returns data for all states", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "puma22",
    weight = "population")

  state_fips <- unique(result$state_fips)

  # Should have all 50 states + DC + PR = 52
  expect_gte(length(state_fips), 50)
  expect_true("01" %in% state_fips)  # Alabama
  expect_true("06" %in% state_fips)  # California
  expect_true("36" %in% state_fips)  # New York
  expect_true("48" %in% state_fips)  # Texas
})

# ==============================================================================
# Weighting variable tests
# ==============================================================================

test_that("get_geocorr_crosswalk accepts population weight", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "population")

  expect_equal(unique(result$weighting_factor), "population")
  expect_true("population_2020" %in% colnames(result))
})

test_that("get_geocorr_crosswalk accepts housing weight", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "housing")

  expect_equal(unique(result$weighting_factor), "housing")
  expect_true("housing_2020" %in% colnames(result))
})

test_that("get_geocorr_crosswalk accepts land weight", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "land")

  expect_equal(unique(result$weighting_factor), "land")
  expect_true("land_area_sqmi" %in% colnames(result))
})

# ==============================================================================
# Geography type tests
# ==============================================================================

test_that("get_geocorr_crosswalk handles tract geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "puma22",
    weight = "population")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "tract")
  expect_equal(unique(result$target_geography), "puma22")

  # Tract GEOIDs should be 11 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 11))
})

test_that("get_geocorr_crosswalk handles blockgroup geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "blockgroup",
    target_geography = "zcta",
    weight = "population")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "blockgroup")

  # Block group GEOIDs should be 12 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 12))
})

test_that("get_geocorr_crosswalk handles place geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "place",
    target_geography = "county",
    weight = "population")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "place")
  expect_equal(unique(result$target_geography), "county")
})

test_that("get_geocorr_crosswalk handles puma22 geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "puma22",
    target_geography = "county",
    weight = "population")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "puma22")
})

test_that("get_geocorr_crosswalk handles congressional district geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "cd119",
    target_geography = "county",
    weight = "population")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "cd119")
})

# ==============================================================================
# Allocation factor tests
# ==============================================================================

test_that("get_geocorr_crosswalk allocation factors are valid", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  # Allocation factors should be between 0 and 1
  expect_true(all(result$allocation_factor_source_to_target >= 0))
  expect_true(all(result$allocation_factor_source_to_target <= 1))

  # Reverse allocation factors should also be between 0 and 1
  expect_true(all(result$allocation_factor_target_to_source >= 0))
  expect_true(all(result$allocation_factor_target_to_source <= 1))
})

test_that("get_geocorr_crosswalk allocation factors sum to 1 per source", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  # Check that allocation factors sum to approximately 1 for each source
  allocation_sums <- result |>
    dplyr::summarize(
      total = sum(allocation_factor_source_to_target, na.rm = TRUE),
      .by = "source_geoid")

  # Allow small floating point tolerance
  expect_true(all(abs(allocation_sums$total - 1) < 0.01))
})

# ==============================================================================
# Caching tests
# ==============================================================================

test_that("get_geocorr_crosswalk caching works", {
  skip_if_offline()

  cache_dir <- tempfile("crosswalk_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # First call should fetch and cache
  result1 <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "population",
    cache = cache_dir)

  # Check cache file exists
  cached_file <- file.path(
    cache_dir,
    "crosswalk_geocorr_2022_to_2022_county_to_zcta_weightedby_population.csv")
  expect_true(file.exists(cached_file))

  # Second call should read from cache
  result2 <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "population",
    cache = cache_dir)

  expect_equal(nrow(result1), nrow(result2))
})

# ==============================================================================
# Metadata tests
# ==============================================================================

test_that("get_geocorr_crosswalk attaches metadata", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  metadata <- attr(result, "crosswalk_metadata")

  expect_type(metadata, "list")
  expect_equal(metadata$data_source, "geocorr")
  expect_equal(metadata$data_source_full_name, "Geocorr 2022 (Missouri Census Data Center)")
  expect_equal(metadata$source_geography, "tract")
  expect_equal(metadata$target_geography, "zcta")
  expect_equal(metadata$weighting_variable, "population")
  expect_equal(metadata$reference_year, "2022")
  expect_true("api_endpoint" %in% names(metadata))
  expect_true("documentation_url" %in% names(metadata))
})

test_that("get_geocorr_crosswalk cached metadata is marked as cached", {
  skip_if_offline()

  cache_dir <- tempfile("crosswalk_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # Fetch and cache
  result1 <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "puma22",
    weight = "population",
    cache = cache_dir)

  # Read from cache
  result2 <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "puma22",
    weight = "population",
    cache = cache_dir)

  metadata2 <- attr(result2, "crosswalk_metadata")
  expect_true(metadata2$read_from_cache)
})

# ==============================================================================
# Block geography tests (special chunking)
# ==============================================================================

test_that("get_geocorr_crosswalk handles block geography with chunking", {
  skip_if_offline()
  skip("Block-level crosswalks are slow; run manually if needed")

  # Block-level crosswalks require chunking (max 13 states per request)
  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "block",
    target_geography = "tract",
    weight = "population")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "block")

  # Block GEOIDs should be 15 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 15))
})

# ==============================================================================
# Integration with get_crosswalk() tests
# ==============================================================================

test_that("get_crosswalk routes same-year requests to Geocorr", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")
  
  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "geocorr")
})

test_that("get_crosswalk routes to Geocorr when no years specified", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "county",
    target_geography = "puma22",
    weight = "population")

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "geocorr")
})
