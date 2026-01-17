# Test comprehensive NHGIS crosswalk coverage
# These tests verify that the package can successfully retrieve a random sample
# of crosswalks from the NHGIS API.

test_that("list_nhgis_crosswalks returns expected structure", {
  crosswalks <- list_nhgis_crosswalks()

  expect_s3_class(crosswalks, "tbl_df")
  expect_true(all(c("source_geography", "source_year", "target_geography",
                    "target_year", "crosswalk_path") %in% names(crosswalks)))
  expect_true(nrow(crosswalks) > 100) # Should have 140+ crosswalks
})

test_that("list_nhgis_crosswalks includes all expected crosswalk categories", {
  crosswalks <- list_nhgis_crosswalks()

  # Block-to-block crosswalks exist
  blk_to_blk <- crosswalks |>
    dplyr::filter(source_geography == "block", target_geography == "block")
  expect_equal(nrow(blk_to_blk), 4)

  # BG-to-BG bidirectional crosswalks exist
  bg_to_bg <- crosswalks |>
    dplyr::filter(source_geography == "block_group", target_geography == "block_group")
  expect_equal(nrow(bg_to_bg), 20)

  # Tract-to-tract bidirectional crosswalks exist
  tr_to_tr <- crosswalks |>
    dplyr::filter(source_geography == "tract", target_geography == "tract")
  expect_equal(nrow(tr_to_tr), 30)

  # Block to other geographies exist (cbsa, pl, puma, ua, zcta)
  blk_to_other <- crosswalks |>
    dplyr::filter(source_geography == "block",
                  target_geography %in% c("core_based_statistical_area", "place",
                                          "puma", "urban_area", "zcta"))
  expect_equal(nrow(blk_to_other), 20)
})

test_that("list_nhgis_crosswalks includes non-decadal source years", {
  crosswalks <- list_nhgis_crosswalks()
  source_years <- unique(crosswalks$source_year)

  expect_true("2011" %in% source_years)
  expect_true("2012" %in% source_years)
  expect_true("2014" %in% source_years)
  expect_true("2015" %in% source_years)
  expect_true("2022" %in% source_years)
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

test_that("county crosswalks correctly exclude 2011/2012 source years", {
  crosswalks <- list_nhgis_crosswalks()

  # 2011 and 2012 should NOT have county targets
  co_from_2011 <- crosswalks |>
    dplyr::filter(source_year == "2011", target_geography == "county")
  expect_equal(nrow(co_from_2011), 0)

  co_from_2012 <- crosswalks |>
    dplyr::filter(source_year == "2012", target_geography == "county")
  expect_equal(nrow(co_from_2012), 0)

  # But 2014, 2015, 2022 SHOULD have county targets
  co_from_2014 <- crosswalks |>
    dplyr::filter(source_year == "2014", target_geography == "county")
  expect_true(nrow(co_from_2014) > 0)

  co_from_2022 <- crosswalks |>
    dplyr::filter(source_year == "2022", target_geography == "county")
  expect_true(nrow(co_from_2022) > 0)
})

test_that("1990/2000 to county crosswalks only include 2010, 2014, 2015 targets", {
  crosswalks <- list_nhgis_crosswalks()

  # From 1990 to county
  co_from_1990 <- crosswalks |>
    dplyr::filter(source_year == "1990", target_geography == "county")
  expect_true(all(co_from_1990$target_year %in% c("2010", "2014", "2015")))
  expect_false(any(co_from_1990$target_year %in% c("2011", "2012")))

  # From 2000 to county
  co_from_2000 <- crosswalks |>
    dplyr::filter(source_year == "2000", target_geography == "county")
  expect_true(all(co_from_2000$target_year %in% c("2010", "2014", "2015")))
  expect_false(any(co_from_2000$target_year %in% c("2011", "2012")))
})

test_that("get_nhgis_crosswalk rejects 2011/2012 source years to county", {
  skip_if_not(Sys.getenv("IPUMS_API_KEY") != "", "IPUMS_API_KEY not set")

  expect_error(
    get_nhgis_crosswalk(
      source_year = 2011,
      source_geography = "block group",
      target_year = 2020,
      target_geography = "county"),
    regexp = "County crosswalks are not available from source years 2011 or 2012")

  expect_error(
    get_nhgis_crosswalk(
      source_year = 2012,
      source_geography = "tract",
      target_year = 2020,
      target_geography = "county"),
    regexp = "County crosswalks are not available from source years 2011 or 2012")
})

test_that("get_nhgis_crosswalk rejects 1990/2000 to county 2011/2012", {
  skip_if_not(Sys.getenv("IPUMS_API_KEY") != "", "IPUMS_API_KEY not set")

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


# Integration tests that actually query the NHGIS API
# These are slow tests that should only run when IPUMS_API_KEY is available
# and when explicitly requested via an environment variable

test_that("random sample of 20 NHGIS crosswalks can be retrieved", {
  skip_if_not(Sys.getenv("IPUMS_API_KEY") != "", "IPUMS_API_KEY not set")
  skip_if_not(
    Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") == "true",
    "Set CROSSWALK_RUN_SLOW_TESTS=true to run slow integration tests")

  # Set seed for reproducibility
  set.seed(12345)
  
  # Get all available crosswalks and select a sample of 20
  sample_crosswalks <- list_nhgis_crosswalks() |>
    dplyr::slice_sample(n = 20)

  test_results = purrr::pmap(
    sample_crosswalks |> dplyr::select(-crosswalk_path),
    get_nhgis_crosswalk)
  
  valid_queries = purrr::map(
    test_results,
    function(crosswalk) {
      metadata = attr(crosswalk, "crosswalk_metadata")
      
      ## if this is null, the query failed
      if (is.null(metadata$retrieved_at)) 0 else 1
    }) |>
  purrr::reduce(sum)

  expect_equal(valid_queries, 20)
  
})

test_that("specific crosswalk types work correctly", {
  skip_if_not(Sys.getenv("IPUMS_API_KEY") != "", "IPUMS_API_KEY not set")
  skip_if_not(
    Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") == "true",
    "Set CROSSWALK_RUN_SLOW_TESTS=true to run slow integration tests")

  # Test block-to-block (decennial)
  blk_blk <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "block",
    target_year = 2020,
    target_geography = "block")
  expect_s3_class(blk_blk, "tbl_df")
  expect_true(nrow(blk_blk) > 0)

  # Test bg-to-bg with non-census source year
  bg_bg_noncensus <- get_nhgis_crosswalk(
    source_year = 2014,
    source_geography = "block group",
    target_year = 2020,
    target_geography = "block group")
  expect_s3_class(bg_bg_noncensus, "tbl_df")
  expect_true(nrow(bg_bg_noncensus) > 0)

  # Test tract-to-tract backwards (2020s -> 2010s)
  tr_tr_backwards <- get_nhgis_crosswalk(
    source_year = 2022,
    source_geography = "tract",
    target_year = 2014,
    target_geography = "tract")
  expect_s3_class(tr_tr_backwards, "tbl_df")
  expect_true(nrow(tr_tr_backwards) > 0)

  # Test block to ZCTA (decennial only)
  blk_zcta <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = "block",
    target_year = 2020,
    target_geography = "zcta")
  expect_s3_class(blk_zcta, "tbl_df")
  expect_true(nrow(blk_zcta) > 0)
})
