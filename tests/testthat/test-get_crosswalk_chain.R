# Tests for get_crosswalk_chain() - exported function for multi-step crosswalks

# ==============================================================================
# get_crosswalk_chain() basic structure tests
# ==============================================================================

test_that("get_crosswalk_chain returns expected structure", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  expect_type(result, "list")
  expect_true("plan" %in% names(result))
  expect_true("crosswalks" %in% names(result))
  expect_true("message" %in% names(result))
})

test_that("get_crosswalk_chain returns plan object", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  plan <- result$plan

  expect_type(plan, "list")
  expect_true(plan$is_multi_step)
  expect_s3_class(plan$steps, "tbl_df")
  expect_equal(nrow(plan$steps), 2)
})

# ==============================================================================
# Multi-step crosswalk chain tests
# ==============================================================================

test_that("get_crosswalk_chain fetches both steps for multi-step request", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  expect_true("step_1" %in% names(result$crosswalks))
  expect_true("step_2" %in% names(result$crosswalks))

  # Step 1 should be NHGIS (temporal)
  step1_meta <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(step1_meta$data_source, "nhgis")

  # Step 2 should be Geocorr (spatial)
  step2_meta <- attr(result$crosswalks$step_2, "crosswalk_metadata")
  expect_equal(step2_meta$data_source, "geocorr")
})

test_that("get_crosswalk_chain crosswalks have correct structure", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  step1 <- result$crosswalks$step_1
  step2 <- result$crosswalks$step_2

  # Both should be tibbles with required columns
  expect_s3_class(step1, "tbl_df")
  expect_s3_class(step2, "tbl_df")

  expect_true("source_geoid" %in% colnames(step1))
  expect_true("target_geoid" %in% colnames(step1))
  expect_true("allocation_factor_source_to_target" %in% colnames(step1))

  expect_true("source_geoid" %in% colnames(step2))
  expect_true("target_geoid" %in% colnames(step2))
  expect_true("allocation_factor_source_to_target" %in% colnames(step2))
})

test_that("get_crosswalk_chain crosswalks have valid allocation factors", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  step1 <- result$crosswalks$step_1
  step2 <- result$crosswalks$step_2

  # Allocation factors should be between 0 and 1
  expect_true(all(step1$allocation_factor_source_to_target >= 0))
  expect_true(all(step1$allocation_factor_source_to_target <= 1))
  expect_true(all(step2$allocation_factor_source_to_target >= 0))
  expect_true(all(step2$allocation_factor_source_to_target <= 1))
})

# ==============================================================================
# Single-step chain tests
# ==============================================================================

test_that("get_crosswalk_chain handles single-step spatial request", {
  skip_if_offline()

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  expect_false(result$plan$is_multi_step)
  expect_equal(length(result$crosswalks), 1)
  expect_true("step_1" %in% names(result$crosswalks))
})

test_that("get_crosswalk_chain handles single-step temporal request", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020)

  expect_false(result$plan$is_multi_step)
  expect_equal(length(result$crosswalks), 1)
})

# ==============================================================================
# Error handling tests
# ==============================================================================

test_that("get_crosswalk_chain errors for unsupported multi-step", {
  # ZCTA as source for multi-step isn't supported by NHGIS
  expect_error(
    get_crosswalk_chain(
      source_geography = "zcta",
      target_geography = "puma",
      source_year = 2010,
      target_year = 2020),
    regexp = "NHGIS does not support")
})

# ==============================================================================
# Integration with get_crosswalk() tests
# ==============================================================================

test_that("get_crosswalk returns list for multi-step", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  # Multi-step should return a list
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_true("plan" %in% names(result))
  expect_true("message" %in% names(result))

  # Should have two steps
  expect_true("step_1" %in% names(result$crosswalks))
  expect_true("step_2" %in% names(result$crosswalks))
})

test_that("get_crosswalk returns list for single-step (consistent structure)", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  # Single-step now also returns list (consistent structure)
  expect_type(result, "list")
  expect_true("crosswalks" %in% names(result))
  expect_true("plan" %in% names(result))
  expect_true("message" %in% names(result))

  # Should have one step
  expect_equal(length(result$crosswalks), 1)
  expect_true("step_1" %in% names(result$crosswalks))

  # The crosswalk tibble has expected columns
  crosswalk_tibble <- result$crosswalks$step_1
  expect_s3_class(crosswalk_tibble, "tbl_df")
  expect_true("source_geoid" %in% colnames(crosswalk_tibble))
  expect_true("target_geoid" %in% colnames(crosswalk_tibble))
  expect_true("allocation_factor_source_to_target" %in% colnames(crosswalk_tibble))
})

# ==============================================================================
# Sequential application tests
# ==============================================================================

test_that("get_crosswalk_chain crosswalks can be applied sequentially", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  # Step 1 target_geoid should match Step 2 source_geoid
  step1_targets <- unique(result$crosswalks$step_1$target_geoid)
  step2_sources <- unique(result$crosswalks$step_2$source_geoid)

  # There should be overlap - step1 targets should be usable as step2 sources
  overlap <- intersect(step1_targets, step2_sources)
  expect_gt(length(overlap), 0)
})
