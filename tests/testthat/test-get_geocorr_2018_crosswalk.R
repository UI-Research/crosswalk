# Tests for get_geocorr_crosswalk() with GeoCorr 2018 (geocorr_version = "2018")

# ==============================================================================
# Basic structure tests
# ==============================================================================

test_that("get_geocorr_crosswalk 2018 returns correct structure for tract to zcta", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "source_geoid", "target_geoid",
    "source_geography_name", "target_geography_name",
    "allocation_factor_source_to_target",
    "weighting_factor")

  expect_true(all(expected_cols %in% colnames(result)))
})

# ==============================================================================
# Weighting variable tests
# ==============================================================================

test_that("get_geocorr_crosswalk 2018 accepts population weight", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "population",
    geocorr_version = "2018")

  expect_equal(unique(result$weighting_factor), "population")
  expect_true("population_2010" %in% colnames(result))
})

test_that("get_geocorr_crosswalk 2018 accepts housing weight", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "housing",
    geocorr_version = "2018")

  expect_equal(unique(result$weighting_factor), "housing")
  expect_true("housing_2010" %in% colnames(result))
})

test_that("get_geocorr_crosswalk 2018 accepts land weight", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "land",
    geocorr_version = "2018")

  expect_equal(unique(result$weighting_factor), "land")
  expect_true("land_area_sqmi" %in% colnames(result))
})

# ==============================================================================
# Geography type tests
# ==============================================================================

test_that("get_geocorr_crosswalk 2018 handles tract geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "puma",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "tract")
  expect_equal(unique(result$target_geography), "puma")

  # Tract GEOIDs should be 11 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 11))
})

test_that("get_geocorr_crosswalk 2018 handles bg geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "blockgroup",
    target_geography = "zcta",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "blockgroup")

  # Block group GEOIDs should be 12 characters
  expect_true(all(stringr::str_length(result$source_geoid) == 12))
})

test_that("get_geocorr_crosswalk 2018 handles place geography via placefp", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "place",
    target_geography = "county",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "place")
  expect_equal(unique(result$target_geography), "county")
})

test_that("get_geocorr_crosswalk 2018 handles puma12 geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "puma",
    target_geography = "county",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "puma")
})

test_that("get_geocorr_crosswalk 2018 handles congressional district geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "cd116",
    target_geography = "county",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "cd116")
})

test_that("get_geocorr_crosswalk 2018 handles zcta5 geography", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "zcta",
    target_geography = "county",
    weight = "population",
    geocorr_version = "2018")

  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$source_geography), "zcta")
})

# ==============================================================================
# Allocation factor tests
# ==============================================================================

test_that("get_geocorr_crosswalk 2018 allocation factors are valid", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population",
    geocorr_version = "2018")

  # Allocation factors should be between 0 and 1
  expect_true(all(result$allocation_factor_source_to_target >= 0))
  expect_true(all(result$allocation_factor_source_to_target <= 1))

  # GeoCorr 2018 does not return reverse allocation factor (afact2)
  expect_false("allocation_factor_target_to_source" %in% colnames(result))
})

test_that("get_geocorr_crosswalk 2018 allocation factors sum to 1 per source", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population",
    geocorr_version = "2018")

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

test_that("get_geocorr_crosswalk 2018 caching works", {
  skip_if_offline()

  cache_dir <- tempfile("crosswalk_cache_2018_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # First call should fetch and cache
  result1 <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "population",
    cache = cache_dir,
    geocorr_version = "2018")

  # Check cache file exists with 2018 naming
  cached_file <- file.path(
    cache_dir,
    "crosswalk_geocorr_2018_to_2018_county_to_zcta_weightedby_population.csv")
  expect_true(file.exists(cached_file))

  # Second call should read from cache
  result2 <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "county",
    target_geography = "zcta",
    weight = "population",
    cache = cache_dir,
    geocorr_version = "2018")

  expect_equal(nrow(result1), nrow(result2))
})

# ==============================================================================
# Metadata tests
# ==============================================================================

test_that("get_geocorr_crosswalk 2018 attaches correct metadata", {
  skip_if_offline()

  result <- crosswalk:::get_geocorr_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population",
    geocorr_version = "2018")

  metadata <- attr(result, "crosswalk_metadata")

  expect_type(metadata, "list")
  expect_equal(metadata$data_source, "geocorr")
  expect_equal(metadata$data_source_full_name, "Geocorr 2018 (Missouri Census Data Center)")
  expect_equal(metadata$source_geography, "tract")
  expect_equal(metadata$target_geography, "zcta")
  expect_equal(metadata$weighting_variable, "population")
  expect_equal(metadata$reference_year, "2018")
  expect_true("api_endpoint" %in% names(metadata))
  expect_equal(metadata$documentation_url, "https://mcdc.missouri.edu/applications/geocorr2018.html")
})

# ==============================================================================
# Integration: get_crosswalk() routes to GeoCorr 2018 for same-year 2010
# ==============================================================================

test_that("get_crosswalk routes same-year 2010 requests to GeoCorr 2018", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2010,
    weight = "population")

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "geocorr")
  expect_equal(metadata$reference_year, "2018")
})

test_that("get_crosswalk routes no-year requests to GeoCorr 2022 (backward compat)", {
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "geocorr")
  expect_equal(metadata$reference_year, "2022")
})

# ==============================================================================
# Config and version helper tests
# ==============================================================================

test_that("get_geocorr_config returns valid config for 2022", {
  config <- crosswalk:::get_geocorr_config("2022")

  expect_equal(config$program, "apps.geocorr2022.sas")
  expect_equal(config$reference_year, "2022")
  expect_equal(config$census_year, "2020")
  expect_equal(config$weight_map[["population"]], "pop20")
  expect_equal(config$puma_col, "puma22")
  expect_true(config$has_name_columns)
})

test_that("get_geocorr_config returns valid config for 2018", {
  config <- crosswalk:::get_geocorr_config("2018")

  expect_equal(config$program, "apps.geocorr2018.sas")
  expect_equal(config$reference_year, "2018")
  expect_equal(config$census_year, "2010")
  expect_equal(config$weight_map[["population"]], "pop10")
  expect_equal(config$puma_col, "puma12")
  expect_false(config$has_name_columns)
})

test_that("get_geocorr_config errors on invalid version", {
  expect_error(
    crosswalk:::get_geocorr_config("2014"),
    regexp = "Invalid GeoCorr version")
})

test_that("resolve_geocorr_geography resolves user-facing names for 2022", {
  expect_equal(crosswalk:::resolve_geocorr_geography("puma", "2022"), "puma22")
  expect_equal(crosswalk:::resolve_geocorr_geography("zcta", "2022"), "zcta")
  expect_equal(crosswalk:::resolve_geocorr_geography("blockgroup", "2022"), "blockgroup")
  expect_equal(crosswalk:::resolve_geocorr_geography("place", "2022"), "place")
})

test_that("resolve_geocorr_geography resolves user-facing names for 2018", {
  expect_equal(crosswalk:::resolve_geocorr_geography("puma", "2018"), "puma12")
  expect_equal(crosswalk:::resolve_geocorr_geography("zcta", "2018"), "zcta5")
  expect_equal(crosswalk:::resolve_geocorr_geography("blockgroup", "2018"), "bg")
  expect_equal(crosswalk:::resolve_geocorr_geography("place", "2018"), "placefp")
})

test_that("resolve_geocorr_geography errors on unsupported geography", {
  expect_error(
    crosswalk:::resolve_geocorr_geography("puma22", "2018"),
    regexp = "not supported by GeoCorr 2018")
})

test_that("determine_geocorr_version returns correct versions", {
  expect_equal(crosswalk:::determine_geocorr_version(NULL), "2022")
  expect_equal(crosswalk:::determine_geocorr_version(2020), "2022")
  expect_equal(crosswalk:::determine_geocorr_version(2022), "2022")
  expect_equal(crosswalk:::determine_geocorr_version(2010), "2018")
  expect_equal(crosswalk:::determine_geocorr_version(2015), "2018")
  expect_equal(crosswalk:::determine_geocorr_version(2019), "2018")
})

test_that("determine_geocorr_version errors for pre-2010 years", {
  expect_error(
    crosswalk:::determine_geocorr_version(2000),
    regexp = "not available for years before 2010")
})
