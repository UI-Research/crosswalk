# Tests for get_available_crosswalks()

# ==============================================================================
# Structure tests
# ==============================================================================

test_that("get_available_crosswalks returns a tibble with expected columns", {
  result <- get_available_crosswalks()

  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 5)
  expect_equal(
    colnames(result),
    c("source_geography", "target_geography", "source_year", "target_year",
      "crosswalk_source"))
})

test_that("get_available_crosswalks has correct column types", {
  result <- get_available_crosswalks()

  expect_type(result$source_geography, "character")
  expect_type(result$target_geography, "character")
  expect_type(result$source_year, "integer")
  expect_type(result$target_year, "integer")
  expect_type(result$crosswalk_source, "character")
  expect_true(all(result$crosswalk_source %in%
    c("geocorr", "nhgis", "ctdata_2020_2022", "county_events")))
})

test_that("get_available_crosswalks has no duplicate rows", {
  result <- get_available_crosswalks()

  expect_equal(nrow(result), nrow(dplyr::distinct(result)))
})

test_that("get_available_crosswalks lists each combination exactly once", {
  result <- get_available_crosswalks()

  combos <- result |>
    dplyr::count(source_geography, target_geography, source_year, target_year) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(combos), 0)
})

test_that("get_available_crosswalks routes 2020->2022 pairs to CTData", {
  result <- get_available_crosswalks()

  # These combinations are also servable by the county-events engine; the
  # listing should mirror the planner, which routes direct requests to CTData
  ct_pair <- result |>
    dplyr::filter(
      source_geography == "county",
      target_geography == "county",
      source_year == 2020L,
      target_year == 2022L)

  expect_equal(ct_pair$crosswalk_source, "ctdata_2020_2022")
})

test_that("get_available_crosswalks contains county-events rows", {
  result <- get_available_crosswalks()

  county_events_match <- result |>
    dplyr::filter(
      source_geography == "county",
      target_geography == "county",
      source_year == 2014L,
      target_year == 2023L)

  expect_equal(nrow(county_events_match), 1)
  expect_equal(county_events_match$crosswalk_source, "county_events")
})

test_that("get_available_crosswalks has no same-year self-pairs for Geocorr rows", {
  result <- get_available_crosswalks()

  geocorr_self_pairs <- result |>
    dplyr::filter(
      source_geography == target_geography,
      source_year == target_year,
      source_year %in% c(2018L, 2022L))

  expect_equal(nrow(geocorr_self_pairs), 0)
})

# ==============================================================================
# Content tests - NHGIS rows
# ==============================================================================

test_that("get_available_crosswalks contains NHGIS rows", {
  result <- get_available_crosswalks()

  nhgis_match <- result |>
    dplyr::filter(
      source_geography == "tract",
      target_geography == "tract",
      source_year == 2010L,
      target_year == 2020L)

  expect_equal(nrow(nhgis_match), 1)
})

# ==============================================================================
# Content tests - Geocorr 2022 rows
# ==============================================================================

test_that("get_available_crosswalks contains Geocorr 2022 rows", {
  result <- get_available_crosswalks()

  geocorr_2022_match <- result |>
    dplyr::filter(
      source_geography == "tract",
      target_geography == "zcta",
      source_year == 2022L,
      target_year == 2022L)

  expect_equal(nrow(geocorr_2022_match), 1)
})

# ==============================================================================
# Content tests - Geocorr 2018 rows
# ==============================================================================

test_that("get_available_crosswalks contains Geocorr 2018 rows", {
  result <- get_available_crosswalks()

  geocorr_2018_match <- result |>
    dplyr::filter(
      source_geography == "tract",
      target_geography == "puma12",
      source_year == 2018L,
      target_year == 2018L)

  expect_equal(nrow(geocorr_2018_match), 1)
})

# ==============================================================================
# Content tests - CTData rows
# ==============================================================================

test_that("get_available_crosswalks contains CTData rows", {
  result <- get_available_crosswalks()

  # county 2020 -> 2022
  ctdata_match <- result |>
    dplyr::filter(
      source_geography == "county",
      target_geography == "county",
      source_year == 2020L,
      target_year == 2022L)

  expect_equal(nrow(ctdata_match), 1)
})
