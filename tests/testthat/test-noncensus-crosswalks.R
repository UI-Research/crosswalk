# Tests for non-census year crosswalk functionality

# ==============================================================================
# list_nhgis_crosswalks() tests
# ==============================================================================

test_that("list_nhgis_crosswalks includes non-census target years", {
 crosswalks <- list_nhgis_crosswalks()

  expect_s3_class(crosswalks, "tbl_df")
  expect_true("target_year" %in% colnames(crosswalks))

  target_years <- unique(crosswalks$target_year)

  expect_true("2011" %in% target_years)
  expect_true("2012" %in% target_years)
  expect_true("2014" %in% target_years)
  expect_true("2015" %in% target_years)
  expect_true("2022" %in% target_years)
})

test_that("list_nhgis_crosswalks non-census years only have bg/tr/co targets", {
  crosswalks <- list_nhgis_crosswalks()

  noncensus_years <- c("2011", "2012", "2014", "2015", "2022")

  noncensus_crosswalks <- crosswalks |>
    dplyr::filter(target_year %in% noncensus_years)

  target_geogs <- unique(noncensus_crosswalks$target_geography)

  expect_true(all(target_geogs %in% c("block_group", "tract", "county")))
  expect_false("place" %in% target_geogs)
  expect_false("zcta" %in% target_geogs)
  expect_false("puma" %in% target_geogs)
})

test_that("list_nhgis_crosswalks includes 2010 to 2022 crosswalks", {
  crosswalks <- list_nhgis_crosswalks()

  crosswalks_2010_to_2022 <- crosswalks |>
    dplyr::filter(source_year == "2010", target_year == "2022")

  expect_gt(nrow(crosswalks_2010_to_2022), 0)

  expect_true("block_group" %in% crosswalks_2010_to_2022$target_geography)
  expect_true("tract" %in% crosswalks_2010_to_2022$target_geography)
  expect_true("county" %in% crosswalks_2010_to_2022$target_geography)
})

# ==============================================================================
# get_ctdata_crosswalk() tests
# ==============================================================================

test_that("get_ctdata_crosswalk returns correct structure for tracts", {
  skip_if_offline()

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

  result <- get_ctdata_crosswalk(geography = "tract")

  expect_equal(unique(result$source_year), "2020")
  expect_equal(unique(result$target_year), "2022")
  expect_equal(unique(result$state_fips), "09")
  expect_equal(unique(result$weighting_factor), "identity")
  expect_true(all(result$allocation_factor_source_to_target == 1))

  expect_equal(unique(result$source_geography_name), "tract")
  expect_equal(unique(result$target_geography_name), "tract")
})

test_that("get_ctdata_crosswalk returns 879 CT tracts",
{
  skip_if_offline()

  result <- get_ctdata_crosswalk(geography = "tract")

  expect_equal(nrow(result), 879)
})

test_that("get_ctdata_crosswalk handles block_group geography", {
  skip_if_offline()

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

  n_source_counties <- length(unique(result$source_geoid))
  n_target_regions <- length(unique(result$target_geoid))
  expect_equal(n_source_counties, 8)
  expect_equal(n_target_regions, 9)

  expect_equal(unique(result$weighting_factor), "population")

  allocation_sums <- result |>
    dplyr::summarize(
      total = sum(allocation_factor_source_to_target),
      .by = "source_geoid")
  expect_true(all(abs(allocation_sums$total - 1) < 0.001))
})

test_that("get_ctdata_crosswalk errors on unsupported geography", {
  expect_error(
    get_ctdata_crosswalk(geography = "zcta"),
    regexp = "not supported")

  expect_error(
    get_ctdata_crosswalk(geography = "place"),
    regexp = "not supported")
})


test_that("get_ctdata_crosswalk accepts various geography spellings", {
  skip_if_offline()

  result1 <- get_ctdata_crosswalk(geography = "tract")
  result2 <- get_ctdata_crosswalk(geography = "tracts")
  result3 <- get_ctdata_crosswalk(geography = "tr")

 expect_equal(nrow(result1), nrow(result2))
  expect_equal(nrow(result1), nrow(result3))
})

test_that("get_ctdata_crosswalk caching works", {
  skip_if_offline()

  cache_dir <- tempfile("crosswalk_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  result1 <- get_ctdata_crosswalk(geography = "tract", cache = cache_dir)

  cached_file <- file.path(cache_dir, "crosswalk_ctdata_2020_to_2022_tract.csv")
  expect_true(file.exists(cached_file))

  result2 <- get_ctdata_crosswalk(geography = "tract", cache = cache_dir)
  expect_equal(result1, result2)
})

# ==============================================================================
# get_crosswalk() routing tests
# ==============================================================================

test_that("get_crosswalk routes 2020-2022 to CTData", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  expect_s3_class(result, "tbl_df")

  metadata <- attr(result, "crosswalk_metadata")
  expect_true("ctdata" %in% metadata$source)
})

test_that("get_crosswalk attaches metadata attribute", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  metadata <- attr(result, "crosswalk_metadata")

  expect_type(metadata, "list")
  expect_true("source" %in% names(metadata))
  expect_true("source_year" %in% names(metadata))
  expect_true("target_year" %in% names(metadata))
  expect_true("source_geography" %in% names(metadata))
  expect_true("target_geography" %in% names(metadata))
  expect_true("notes" %in% names(metadata))
})

test_that("get_crosswalk 2020-2022 metadata contains correct info", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  metadata <- attr(result, "crosswalk_metadata")

  expect_equal(metadata$source_year, "2020")
  expect_equal(metadata$target_year, "2022")
  expect_true(length(metadata$notes) > 0)
})

test_that("get_crosswalk 2020-2022 only returns Connecticut data", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  state_fips <- unique(result$state_fips)
  expect_equal(state_fips, "09")
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
# get_crosswalk_2020_2022() tests
# ==============================================================================

test_that("get_crosswalk_2020_2022 returns CT crosswalk with attributes", {
  skip_if_offline()

  result <- crosswalk:::get_crosswalk_2020_2022(geography = "tract")

  expect_s3_class(result, "tbl_df")

  sources_attr <- attr(result, "crosswalk_sources")
  expect_type(sources_attr, "list")
  expect_equal(sources_attr$connecticut, "ctdata")
  expect_equal(sources_attr$other_states, "identity_mapping")

  note_attr <- attr(result, "identity_states_note")
  expect_type(note_attr, "character")
})

test_that("get_crosswalk_2020_2022 errors on invalid geography", {
  expect_error(
    crosswalk:::get_crosswalk_2020_2022(geography = "puma"),
    regexp = "not supported")
})

# ==============================================================================
# get_nhgis_crosswalk() validation tests
# ==============================================================================

test_that("get_nhgis_crosswalk validates non-census year geography restrictions", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  expect_error(
    get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = "block",
      target_year = 2022,
      target_geography = "zcta"),
    regexp = "Non-census year crosswalks.*only available")

  expect_error(
    get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = "block",
      target_year = 2011,
      target_geography = "place"),
    regexp = "Non-census year crosswalks.*only available")
})

test_that("get_nhgis_crosswalk accepts valid non-census year requests", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  expect_no_error({
    result <- get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = "block",
      target_year = 2022,
      target_geography = "tract")
  })
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("CT tract GEOIDs have correct format changes", {
  skip_if_offline()

  result <- get_ctdata_crosswalk(geography = "tract")

  expect_true(all(stringr::str_starts(result$source_geoid, "09")))
  expect_true(all(stringr::str_starts(result$target_geoid, "09")))

  expect_true(all(stringr::str_length(result$source_geoid) == 11))
  expect_true(all(stringr::str_length(result$target_geoid) == 11))

  source_counties <- stringr::str_sub(result$source_geoid, 3, 5)
  target_counties <- stringr::str_sub(result$target_geoid, 3, 5)
  expect_false(all(source_counties == target_counties))
})

test_that("CT county crosswalk maps 8 old counties to 9 planning regions", {
  skip_if_offline()
  skip_if_not_installed("tidycensus")
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "CENSUS_API_KEY not set")

  result <- get_ctdata_crosswalk(geography = "county")

  n_source_counties <- length(unique(result$source_geoid))
  n_target_regions <- length(unique(result$target_geoid))

  expect_equal(n_source_counties, 8)
  expect_equal(n_target_regions, 9)

  expect_gt(nrow(result), 8)

  expect_true(all(result$allocation_factor_source_to_target > 0))
  expect_true(all(result$allocation_factor_source_to_target <= 1))
})
