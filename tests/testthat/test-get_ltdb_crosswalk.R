# Tests for Longitudinal Tract Database (LTDB) crosswalks: the LTDB listing,
# routing in the chain planner, and (when slow tests are enabled) the real
# download from the LTDB server.

# ==============================================================================
# list_ltdb_crosswalks()
# ==============================================================================

test_that("list_ltdb_crosswalks lists only tract 2010 -> 2000", {
  ltdb_crosswalks <- crosswalk:::list_ltdb_crosswalks()

  expect_s3_class(ltdb_crosswalks, "tbl_df")
  expect_equal(nrow(ltdb_crosswalks), 1)
  expect_equal(ltdb_crosswalks$source_geography, "tract")
  expect_equal(ltdb_crosswalks$target_geography, "tract")
  expect_equal(ltdb_crosswalks$source_year, "2010")
  expect_equal(ltdb_crosswalks$target_year, "2000")
  expect_match(ltdb_crosswalks$download_url, "^https://s4\\.ad\\.brown\\.edu/")
})

test_that("LTDB is listed only for pairs that NHGIS does not publish", {
  overlapping_pairs <- crosswalk:::list_ltdb_crosswalks() |>
    dplyr::semi_join(
      list_nhgis_crosswalks(),
      by = c("source_geography", "target_geography", "source_year", "target_year"))

  expect_equal(nrow(overlapping_pairs), 0)
})

# ==============================================================================
# get_ltdb_crosswalk() input checks (no download needed)
# ==============================================================================

test_that("get_ltdb_crosswalk errors for combinations LTDB does not cover", {
  expect_error(
    crosswalk:::get_ltdb_crosswalk("tract", "tract", 2020, 2010),
    "not supported")
  expect_error(
    crosswalk:::get_ltdb_crosswalk("block_group", "block_group", 2010, 2000),
    "not supported")
})

test_that("get_crosswalk_single rejects non-population weights for LTDB before downloading", {
  expect_error(
    crosswalk:::get_crosswalk_single(
      source_geography = "tract",
      target_geography = "tract",
      source_year = 2010,
      target_year = 2000,
      weight = "housing",
      crosswalk_source = "ltdb"),
    "only one")
})

# ==============================================================================
# Chain planning
# ==============================================================================

test_that("find_temporal_path uses LTDB for tract 2010 -> 2000", {
  path <- crosswalk:::find_temporal_path("tract", "2010", "2000")

  expect_equal(length(path), 1)
  expect_equal(path[[1]]$crosswalk_source, "ltdb")
})

test_that("plan_crosswalk_chain routes tract 2010 -> 2000 to LTDB", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2000,
    weight = "population")

  expect_null(plan$error)
  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source, "ltdb")
})

test_that("plan_crosswalk_chain chains tract 2020 -> 2000 through NHGIS then LTDB", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2000,
    weight = "population")

  expect_null(plan$error)
  expect_true(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source, c("nhgis", "ltdb"))
  expect_equal(plan$steps$source_year, c("2020", "2010"))
  expect_equal(plan$steps$target_year, c("2010", "2000"))
})

test_that("plan_crosswalk_chain errors when LTDB is needed but weight is not population", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2000,
    weight = "housing")

  expect_false(is.null(plan$error))
  expect_match(plan$error, "LTDB provides only one weight")
})

test_that("plan_crosswalk_chain chains LTDB with GeoCorr 2000 for a geography change in 2000", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2000,
    weight = "population")

  expect_null(plan$error)
  expect_true(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source, c("ltdb", "geocorr"))
  expect_equal(plan$steps$source_year, c("2010", "2000"))
  expect_equal(plan$steps$target_year, c("2000", "2000"))
})

test_that("existing NHGIS tract routes are unchanged", {
  forward_plan <- plan_crosswalk_chain("tract", "tract", 2000, 2010, "population")
  backward_plan <- plan_crosswalk_chain("tract", "tract", 2020, 2010, "population")

  expect_equal(forward_plan$steps$crosswalk_source, "nhgis")
  expect_equal(backward_plan$steps$crosswalk_source, "nhgis")
})

test_that("get_crosswalk errors before downloading when weight is not population", {
  expect_error(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "tract",
      source_year = 2010,
      target_year = 2000,
      weight = "land",
      silent = TRUE),
    "LTDB provides only one weight")
})

test_that("get_available_crosswalks lists tract 2010 -> 2000 as LTDB", {
  ltdb_rows <- get_available_crosswalks() |>
    dplyr::filter(
      source_geography == "tract",
      target_geography == "tract",
      source_year == 2010L,
      target_year == 2000L)

  expect_equal(nrow(ltdb_rows), 1)
  expect_equal(ltdb_rows$crosswalk_source, "ltdb")
})

# ==============================================================================
# Slow integration tests (download from the LTDB server)
# ==============================================================================

test_that("get_crosswalk returns a standardized LTDB tract 2010 -> 2000 crosswalk", {
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if_offline()

  cache_directory <- tempfile("ltdb_cache_")
  dir.create(cache_directory)
  on.exit(unlink(cache_directory, recursive = TRUE), add = TRUE)

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2000,
    cache = cache_directory,
    silent = TRUE)
  crosswalk_table <- result$crosswalks$step_1

  expect_true(all(c(
    "source_geoid", "target_geoid", "source_geography_name",
    "target_geography_name", "source_year", "target_year",
    "allocation_factor_source_to_target", "weighting_factor",
    "ltdb_change_type") %in% names(crosswalk_table)))
  expect_gt(nrow(crosswalk_table), 100000)
  expect_true(all(nchar(crosswalk_table$source_geoid) == 11))
  expect_true(all(nchar(crosswalk_table$target_geoid) == 11))
  expect_true(all(crosswalk_table$source_year == "2010"))
  expect_true(all(crosswalk_table$target_year == "2000"))
  expect_true(all(crosswalk_table$weighting_factor == "ltdb_area_and_population"))
  expect_true(all(crosswalk_table$ltdb_change_type %in%
    c("1 to 1", "many to 1", "1 to many", "many to many")))

  # LTDB weights for each 2010 tract sum to 1
  allocation_totals <- crosswalk_table |>
    dplyr::summarize(
      total = sum(allocation_factor_source_to_target),
      .by = source_geoid)
  expect_true(all(abs(allocation_totals$total - 1) < 0.001))

  metadata <- attr(crosswalk_table, "crosswalk_metadata")
  expect_equal(metadata$data_source, "ltdb")
  expect_false(metadata$read_from_cache)
  expect_true(any(stringr::str_detect(metadata$notes, "differs from NHGIS")))
  expect_equal(
    metadata$citation_url,
    "https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx")
  expect_true(any(stringr::str_detect(metadata$notes, "US2010 Project webpage")))
  expect_true(any(stringr::str_detect(metadata$notes, "Logan, John R\\.")))

  # A second call reads the cached CSV, still prints the citation guidance
  # (as NHGIS does on cache reads), and returns the same rows
  expect_message(
    cached_result <- get_crosswalk(
      source_geography = "tract",
      target_geography = "tract",
      source_year = 2010,
      target_year = 2000,
      cache = cache_directory,
      silent = FALSE),
    "Use of LTDB crosswalks requires citing the US2010 Project webpage")
  cached_table <- cached_result$crosswalks$step_1

  expect_true(attr(cached_table, "crosswalk_metadata")$read_from_cache)
  # Compare rows only; the metadata attributes differ by design
  # (`read_from_cache`, `retrieved_at`)
  expect_equal(
    structure(cached_table, crosswalk_metadata = NULL),
    structure(crosswalk_table, crosswalk_metadata = NULL))
})

test_that("get_crosswalk fetches tract 2020 -> 2000 as an NHGIS + LTDB chain", {
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2000,
    silent = TRUE)

  expect_equal(names(result$crosswalks), c("step_1", "step_2"))
  expect_equal(
    attr(result$crosswalks$step_1, "crosswalk_metadata")$data_source, "nhgis")
  expect_equal(
    attr(result$crosswalks$step_2, "crosswalk_metadata")$data_source, "ltdb")
})
