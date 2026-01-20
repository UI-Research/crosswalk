# Tests for get_crosswalk() - the main user-facing function

# ==============================================================================
# Basic functionality tests
# ==============================================================================

test_that("get_crosswalk returns list with expected structure for single-step", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  # Always returns list structure

  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_true("plan" %in% names(result))
  expect_true("message" %in% names(result))

  # Single-step has one crosswalk
  expect_equal(length(result$crosswalks), 1)
  expect_true("step_1" %in% names(result$crosswalks))

  # The crosswalk tibble has expected columns
  crosswalk_tibble <- result$crosswalks$step_1
  expect_s3_class(crosswalk_tibble, "tbl_df")
  expected_cols <- c("source_geoid", "target_geoid", "allocation_factor_source_to_target")
  expect_true(all(expected_cols %in% colnames(crosswalk_tibble)))
})

test_that("get_crosswalk attaches comprehensive metadata attribute to crosswalk tibble", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  # Metadata is attached to individual crosswalk tibbles
  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")

  expect_type(metadata, "list")
  expect_true("call_parameters" %in% names(metadata))
  expect_true("data_source" %in% names(metadata))
  expect_true("data_source_full_name" %in% names(metadata))
  expect_true("source_geography" %in% names(metadata))
  expect_true("target_geography" %in% names(metadata))
  expect_true("source_year" %in% names(metadata))
  expect_true("target_year" %in% names(metadata))
  expect_true("crosswalk_package_version" %in% names(metadata))

  expect_type(metadata$call_parameters, "list")
  expect_equal(metadata$call_parameters$source_geography, "tract")
  expect_equal(metadata$call_parameters$target_geography, "tract")
})

# ==============================================================================
# Routing tests
# ==============================================================================

test_that("get_crosswalk routes 2020-2022 to CTData", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "ctdata_nhgis_combined")
})

test_that("get_crosswalk routes to NHGIS for inter-temporal requests", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020)

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "nhgis")
})

test_that("get_crosswalk routes to Geocorr for same-year requests", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "geocorr")
})

# ==============================================================================
# Nested geography tests
# ==============================================================================

test_that("get_crosswalk warns for nested geographies", {
  expect_warning(
    result <- get_crosswalk(
      source_geography = "tract",
      target_geography = "county"),
    regexp = "nested within")

  # Returns list structure with empty crosswalk
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_equal(nrow(result$crosswalks$step_1), 0)
})

test_that("get_crosswalk warns for block to tract nested geography", {
  expect_warning(
    result <- get_crosswalk(
      source_geography = "block",
      target_geography = "tract"),
    regexp = "nested within")

  # Returns list structure with empty crosswalk
  expect_type(result, "list")
  expect_equal(nrow(result$crosswalks$step_1), 0)
})

# ==============================================================================
# Multi-step crosswalk tests
# ==============================================================================

test_that("get_crosswalk returns list for multi-step requests (geography + year change)", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  # Multi-step returns a list, not a tibble
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_true("plan" %in% names(result))
  expect_true("message" %in% names(result))

  # Check that individual crosswalks are tibbles
  expect_s3_class(result$crosswalks$step_1, "tbl_df")
  expect_s3_class(result$crosswalks$step_2, "tbl_df")
})

test_that("get_crosswalk multi-step crosswalks have valid allocation factors", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  step1 <- result$crosswalks$step_1
  step2 <- result$crosswalks$step_2

  expect_true(all(step1$allocation_factor_source_to_target >= 0))
  expect_true(all(step1$allocation_factor_source_to_target <= 1))
  expect_true(all(step2$allocation_factor_source_to_target >= 0))
  expect_true(all(step2$allocation_factor_source_to_target <= 1))
})

test_that("get_crosswalk multi-step plan is correct", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  expect_true(result$plan$is_multi_step)
  expect_s3_class(result$plan$steps, "tbl_df")
  expect_equal(nrow(result$plan$steps), 2)
})

# ==============================================================================
# 2020 to 2022 specific tests
# ==============================================================================

test_that("get_crosswalk 2020-2022 metadata contains correct info", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")

  expect_equal(metadata$source_year, "2020")
  expect_equal(metadata$target_year, "2022")
  expect_equal(metadata$data_source, "ctdata_nhgis_combined")
  expect_true(length(metadata$notes) > 0)
})

test_that("get_crosswalk 2020-2022 returns nationally comprehensive data", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  crosswalk_tibble <- result$crosswalks$step_1

  # Should have multiple states (nationally comprehensive)
  state_fips <- unique(crosswalk_tibble$state_fips)
  expect_gt(length(state_fips), 1)

  # Connecticut should be included
  expect_true("09" %in% state_fips)
})

test_that("get_crosswalk 2020-2022 errors on unsupported geography", {
  expect_error(
    get_crosswalk(
      source_geography = "zcta",
      target_geography = "zcta",
      source_year = 2020,
      target_year = 2022),
    regexp = "not supported")
})

# ==============================================================================
# get_crosswalk_2020_2022() internal function tests
# ==============================================================================

test_that("get_crosswalk_2020_2022 returns CT crosswalk with attributes", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- crosswalk:::get_crosswalk_2020_2022(geography = "tract")

  expect_s3_class(result, "tbl_df")

  # Should be nationally comprehensive
  state_fips <- unique(result$state_fips)
  expect_gt(length(state_fips), 1)
})

test_that("get_crosswalk_2020_2022 errors on invalid geography", {
  expect_error(
    crosswalk:::get_crosswalk_2020_2022(geography = "puma"),
    regexp = "not supported")
})

# ==============================================================================
# Consistent list return type tests
# ==============================================================================

test_that("get_crosswalk returns list for single-step spatial", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  # Always returns list structure
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_equal(length(result$crosswalks), 1)
  expect_s3_class(result$crosswalks$step_1, "tbl_df")
})

test_that("get_crosswalk returns list for single-step temporal", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020)

  # Always returns list structure
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_equal(length(result$crosswalks), 1)
  expect_s3_class(result$crosswalks$step_1, "tbl_df")
})

test_that("get_crosswalk returns list for multi-step", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  # Always returns list structure
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_equal(length(result$crosswalks), 2)
  expect_s3_class(result$crosswalks$step_1, "tbl_df")
  expect_s3_class(result$crosswalks$step_2, "tbl_df")
})
