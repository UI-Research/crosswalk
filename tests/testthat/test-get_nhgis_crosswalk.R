# Tests for get_nhgis_crosswalk() and list_nhgis_crosswalks() - NHGIS crosswalks

# ==============================================================================
# list_nhgis_crosswalks() tests
# ==============================================================================

test_that("list_nhgis_crosswalks returns expected structure", {
  crosswalks <- list_nhgis_crosswalks()

  expect_s3_class(crosswalks, "tbl_df")
  expect_true("source_year" %in% colnames(crosswalks))
  expect_true("source_geography" %in% colnames(crosswalks))
  expect_true("target_year" %in% colnames(crosswalks))
  expect_true("target_geography" %in% colnames(crosswalks))
  expect_true("crosswalk_path" %in% colnames(crosswalks))
})

test_that("list_nhgis_crosswalks includes non-census target years", {
  crosswalks <- list_nhgis_crosswalks()

  target_years <- unique(crosswalks$target_year)

  expect_true("2011" %in% target_years)
  expect_true("2012" %in% target_years)
  expect_true("2014" %in% target_years)
  expect_true("2015" %in% target_years)
  expect_true("2022" %in% target_years)
})

test_that("list_nhgis_crosswalks includes non-census SOURCE years", {
  crosswalks <- list_nhgis_crosswalks()

  source_years <- unique(crosswalks$source_year)

  # Non-census years should be valid as source years
  expect_true("2011" %in% source_years)
  expect_true("2012" %in% source_years)
  expect_true("2014" %in% source_years)
  expect_true("2015" %in% source_years)
  expect_true("2022" %in% source_years)
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

test_that("list_nhgis_crosswalks non-census source years only have bg/tr sources", {
  crosswalks <- list_nhgis_crosswalks()

  noncensus_years <- c("2011", "2012", "2014", "2015", "2022")

  noncensus_source_crosswalks <- crosswalks |>
    dplyr::filter(source_year %in% noncensus_years)

  source_geogs <- unique(noncensus_source_crosswalks$source_geography)

  # Non-census source years only support bg and tr (not block)
  expect_true(all(source_geogs %in% c("block_group", "tract")))
  expect_false("block" %in% source_geogs)
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

test_that("list_nhgis_crosswalks includes bidirectional crosswalks", {
  crosswalks <- list_nhgis_crosswalks()

  # 2014 to 2020 should exist
  expect_gt(nrow(crosswalks |>
    dplyr::filter(source_year == "2014", target_year == "2020")), 0)

  # 2022 to 2010 should exist
  expect_gt(nrow(crosswalks |>
    dplyr::filter(source_year == "2022", target_year == "2010")), 0)

  # 2011 to 2022 should exist (both non-census, different decades)
  expect_gt(nrow(crosswalks |>
    dplyr::filter(source_year == "2011", target_year == "2022")), 0)
})

test_that("list_nhgis_crosswalks does not include block group parts", {
  crosswalks <- list_nhgis_crosswalks()

  expect_false(any(stringr::str_detect(crosswalks$source_geography, "part")))
  expect_false(any(stringr::str_detect(crosswalks$target_geography, "part")))
})

# ==============================================================================
# standardize_geography() tests
# ==============================================================================

test_that("standardize_geography handles various block spellings", {
  expect_equal(crosswalk:::standardize_geography("block", "source"), "blk")
  expect_equal(crosswalk:::standardize_geography("blocks", "source"), "blk")
  expect_equal(crosswalk:::standardize_geography("blk", "source"), "blk")
  expect_equal(crosswalk:::standardize_geography("census block", "source"), "blk")
})

test_that("standardize_geography handles various block group spellings", {
  expect_equal(crosswalk:::standardize_geography("block_group", "source"), "bg")
  expect_equal(crosswalk:::standardize_geography("block group", "source"), "bg")
  expect_equal(crosswalk:::standardize_geography("blockgroup", "source"), "bg")
  expect_equal(crosswalk:::standardize_geography("bg", "source"), "bg")
})

test_that("standardize_geography handles various tract spellings", {
  expect_equal(crosswalk:::standardize_geography("tract", "source"), "tr")
  expect_equal(crosswalk:::standardize_geography("tracts", "source"), "tr")
  expect_equal(crosswalk:::standardize_geography("tr", "source"), "tr")
  expect_equal(crosswalk:::standardize_geography("census tract", "source"), "tr")
})

test_that("standardize_geography validates source geographies", {
  # Valid source geographies
  expect_no_error(crosswalk:::standardize_geography("block", "source"))
  expect_no_error(crosswalk:::standardize_geography("block_group", "source"))
  expect_no_error(crosswalk:::standardize_geography("tract", "source"))

  # Invalid source geography (county is target-only for NHGIS)
  expect_error(crosswalk:::standardize_geography("county", "source"))
  expect_error(crosswalk:::standardize_geography("zcta", "source"))
})

test_that("standardize_geography validates target geographies", {
  # Valid target geographies
  expect_no_error(crosswalk:::standardize_geography("block", "target"))
  expect_no_error(crosswalk:::standardize_geography("block_group", "target"))
  expect_no_error(crosswalk:::standardize_geography("tract", "target"))
  expect_no_error(crosswalk:::standardize_geography("county", "target"))
  expect_no_error(crosswalk:::standardize_geography("zcta", "target"))
  expect_no_error(crosswalk:::standardize_geography("puma", "target"))
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
    regexp = "NHGIS only provides cross-decade")
})

test_that("get_nhgis_crosswalk validates non-census SOURCE year geography restrictions", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  # Non-census source years only support bg, tr, co - not block
  expect_error(
    get_nhgis_crosswalk(
      source_year = 2014,
      source_geography = "block",
      target_year = 2020,
      target_geography = "tract"),
    regexp = "Non-census year crosswalks.*only available")
})

test_that("get_nhgis_crosswalk rejects within-decade crosswalks", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  # 2010 to 2014 is within-decade (both 2010s)
  expect_error(
    get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = "tract",
      target_year = 2014,
      target_geography = "tract"),
    regexp = "cross-decade")

  # 2020 to 2022 is within-decade (both 2020s)
  expect_error(
    get_nhgis_crosswalk(
      source_year = 2020,
      source_geography = "tract",
      target_year = 2022,
      target_geography = "tract"),
    regexp = "cross-decade")

  # 2011 to 2015 is within-decade (both 2010s)
  expect_error(
    get_nhgis_crosswalk(
      source_year = 2011,
      source_geography = "tract",
      target_year = 2015,
      target_geography = "tract"),
    regexp = "cross-decade")
})

test_that("get_nhgis_crosswalk validates county target restrictions for 2011/2012 sources", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  # 2011 source year cannot target county
  expect_error(
    get_nhgis_crosswalk(
      source_year = 2011,
      source_geography = "block_group",
      target_year = 2020,
      target_geography = "county"),
    regexp = "County crosswalks are not available from source years 2011 or 2012")

  # 2012 source year cannot target county
  expect_error(
    get_nhgis_crosswalk(
      source_year = 2012,
      source_geography = "tract",
      target_year = 2020,
      target_geography = "county"),
    regexp = "County crosswalks are not available from source years 2011 or 2012")
})

test_that("get_nhgis_crosswalk validates 1990/2000 to county restrictions", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  # 1990 to county only supports 2010, 2014, 2015 targets (not 2011 or 2012)
  expect_error(
    get_nhgis_crosswalk(
      source_year = 1990,
      source_geography = "block",
      target_year = 2011,
      target_geography = "county"),
    regexp = "years 2010, 2014, and 2015")

  expect_error(
    get_nhgis_crosswalk(
      source_year = 2000,
      source_geography = "block",
      target_year = 2012,
      target_geography = "county"),
    regexp = "years 2010, 2014, and 2015")
})

test_that("get_nhgis_crosswalk requires API key", {
  # Temporarily unset API key
  original_key <- Sys.getenv("IPUMS_API_KEY")
  Sys.setenv(IPUMS_API_KEY = "")
  on.exit(Sys.setenv(IPUMS_API_KEY = original_key))

  expect_error(
    get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = "tract",
      target_year = 2020,
      target_geography = "tract"),
    regexp = "API key required")
})

# ==============================================================================
# get_nhgis_crosswalk() successful requests
# ==============================================================================

test_that("get_nhgis_crosswalk accepts valid non-census year requests", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  expect_no_error({
    result <- get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = "tract",
      target_year = 2022,
      target_geography = "tract")
  })
})

test_that("get_nhgis_crosswalk accepts non-census SOURCE years", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  # 2014 (non-census) to 2020 (decennial) - cross-decade
  expect_no_error({
    result <- get_nhgis_crosswalk(
      source_year = 2014,
      source_geography = "tract",
      target_year = 2020,
      target_geography = "tract")
  })

  # 2022 (non-census) to 2010 (decennial) - cross-decade
  expect_no_error({
    result <- get_nhgis_crosswalk(
      source_year = 2022,
      source_geography = "block_group",
      target_year = 2010,
      target_geography = "block_group")
  })
})

test_that("get_nhgis_crosswalk returns correct structure", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  result <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "tract",
    target_year = 2020,
    target_geography = "tract")

  expect_s3_class(result, "tbl_df")

  expected_cols <- c(
    "source_geoid", "target_geoid",
    "source_geography_name", "target_geography_name",
    "source_year", "target_year",
    "weighting_factor", "allocation_factor_source_to_target")

  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("get_nhgis_crosswalk returns valid allocation factors", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  result <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "tract",
    target_year = 2020,
    target_geography = "tract")

  # Allocation factors should be between 0 and 1
  expect_true(all(result$allocation_factor_source_to_target >= 0))
  expect_true(all(result$allocation_factor_source_to_target <= 1))
})

# ==============================================================================
# Metadata tests
# ==============================================================================

test_that("get_nhgis_crosswalk attaches metadata", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  result <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "tract",
    target_year = 2020,
    target_geography = "tract")

  metadata <- attr(result, "crosswalk_metadata")

  expect_type(metadata, "list")
  expect_equal(metadata$data_source, "nhgis")
  expect_equal(metadata$data_source_full_name, "IPUMS NHGIS (National Historical Geographic Information System)")
  expect_equal(metadata$source_year, "2010")
  expect_equal(metadata$target_year, "2020")
  expect_true("download_url" %in% names(metadata))
  expect_true("citation_url" %in% names(metadata))
  expect_true("documentation_url" %in% names(metadata))
})

# ==============================================================================
# Caching tests
# ==============================================================================

test_that("get_nhgis_crosswalk caching works", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  cache_dir <- tempfile("crosswalk_cache_")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE))

  # First call should fetch and cache
  result1 <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "tract",
    target_year = 2020,
    target_geography = "tract",
    cache = cache_dir)

  # Check cache file exists
  cached_files <- list.files(cache_dir, pattern = "nhgis")
  expect_gt(length(cached_files), 0)

  # Second call should read from cache
  result2 <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "tract",
    target_year = 2020,
    target_geography = "tract",
    cache = cache_dir)

  expect_equal(nrow(result1), nrow(result2))

  metadata2 <- attr(result2, "crosswalk_metadata")
  expect_true(metadata2$read_from_cache)
})

# ==============================================================================
# Integration with get_crosswalk() tests
# ==============================================================================

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

test_that("get_crosswalk routes to NHGIS for non-census year requests", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2022)

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$data_source, "nhgis")
})

# ==============================================================================
# Slow integration tests (optional)
# ==============================================================================

test_that("get_nhgis_crosswalk works for sample of crosswalks", {
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true", "Slow tests not enabled")
  skip_if_offline()

  crosswalks <- list_nhgis_crosswalks()

  # Sample 5 random crosswalks to test
  set.seed(42)
  sample_crosswalks <- crosswalks |>
    dplyr::slice_sample(n = 5)

  for (i in seq_len(nrow(sample_crosswalks))) {
    cw <- sample_crosswalks[i, ]

    result <- get_nhgis_crosswalk(
      source_year = cw$source_year,
      source_geography = cw$source_geography,
      target_year = cw$target_year,
      target_geography = cw$target_geography)

    expect_s3_class(result, "tbl_df")
    expect_gt(nrow(result), 0)
  }
})
