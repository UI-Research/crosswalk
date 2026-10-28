# Tests for GeoCorr 2000 support (same-year crosswalks between 2000 Census
# geographies). Tests that query the GeoCorr 2000 web tool are slow and run
# only when CROSSWALK_RUN_SLOW_TESTS is "true".

# ==============================================================================
# Version routing and configuration (no network access)
# ==============================================================================

test_that("determine_geocorr_version routes 2000-2009 to GeoCorr 2000", {
  expect_equal(crosswalk:::determine_geocorr_version(2000), "2000")
  expect_equal(crosswalk:::determine_geocorr_version(2005), "2000")
  expect_equal(crosswalk:::determine_geocorr_version("2009"), "2000")
  expect_equal(crosswalk:::determine_geocorr_version(2010), "2018")
})

test_that("get_geocorr_config returns the GeoCorr 2000 configuration", {
  config <- crosswalk:::get_geocorr_config("2000")

  expect_equal(config$program, "apps.geocorr2000.sas")
  expect_equal(config$reference_year, "2000")
  expect_equal(config$census_year, "2000")
  expect_equal(
    config$weight_map,
    list("population" = "pop2k", "housing" = "hus2k", "land" = "LandSQMI"))
  expect_false(config$include_pr)
  expect_equal(config$block_states_per_request, 10)
  ## the GeoCorr 2000 tool fails unless the urban area filter field is sent
  expect_true("uaucs" %in% names(config$extra_params))
})

test_that("get_geocorr_config rejects unknown versions", {
  expect_error(
    crosswalk:::get_geocorr_config("1990"),
    regexp = "Must be '2022', '2018', or '2000'")
})

test_that("resolve_geocorr_geography maps user-facing names for GeoCorr 2000", {
  expect_equal(crosswalk:::resolve_geocorr_geography("puma", "2000"), "puma5")
  expect_equal(crosswalk:::resolve_geocorr_geography("puma5", "2000"), "puma5")
  expect_equal(crosswalk:::resolve_geocorr_geography("zcta", "2000"), "zcta5")
  expect_equal(crosswalk:::resolve_geocorr_geography("place", "2000"), "placefp")
  expect_equal(crosswalk:::resolve_geocorr_geography("block_group", "2000"), "bg")
  expect_equal(crosswalk:::resolve_geocorr_geography("tract", "2000"), "tract")
  expect_equal(crosswalk:::resolve_geocorr_geography("county", "2000"), "county")
  expect_equal(crosswalk:::resolve_geocorr_geography("msacmsa", "2000"), "msacmsa")
})

test_that("resolve_geocorr_geography explains that puma1 is unavailable", {
  expect_error(
    crosswalk:::resolve_geocorr_geography("puma1", "2000"),
    regexp = "Use 'puma5'")
})

test_that("resolve_geocorr_geography rejects 2020-era geographies for GeoCorr 2000", {
  expect_error(
    crosswalk:::resolve_geocorr_geography("puma22", "2000"),
    regexp = "not supported by GeoCorr 2000")
  expect_error(
    crosswalk:::resolve_geocorr_geography("aiannh", "2000"),
    regexp = "not supported by GeoCorr 2000")
})

# ==============================================================================
# Tract GEOID construction (no network access)
# ==============================================================================

test_that("build_tract_geoid builds 11-character GEOIDs from GeoCorr 2000 codes", {
  ## state, county, and tract codes as written in GeoCorr 2000 output, where
  ## county is the 5-digit state-and-county code and tract has a decimal point
  expect_equal(
    crosswalk:::build_tract_geoid(
      state = c("44", "44", "10"),
      county = c("44001", "44001", "10001"),
      tract = c("0301.00", "0306.01", "0401.00")),
    c("44001030100", "44001030601", "10001040100"))
})

test_that("build_tract_geoid accepts 3-digit county codes", {
  expect_equal(
    crosswalk:::build_tract_geoid(state = "44", county = "001", tract = "0301.00"),
    "44001030100")
})

test_that("build_tract_geoid handles tract codes written in other forms", {
  ## "1234.56"-style codes, codes already in 6-digit form, and whole tract
  ## numbers without a suffix
  expect_equal(
    crosswalk:::build_tract_geoid(
      state = c("06", "06", "06"),
      county = c("037", "037", "037"),
      tract = c("1234.56", "123456", "1234")),
    c("06037123456", "06037123456", "06037123400"))
})

test_that("build_tract_geoid returns NA when an input is missing", {
  expect_true(is.na(
    crosswalk:::build_tract_geoid(state = "44", county = "44001", tract = NA_character_)))
})

# ==============================================================================
# Planning and listing (no network access)
# ==============================================================================

test_that("plan_crosswalk_chain describes GeoCorr 2000 for same-year 2000 geography change", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "puma",
    source_year = 2000,
    target_year = 2000)

  expect_false(plan$is_multi_step)
  expect_null(plan$error)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "geocorr")
  expect_true(stringr::str_detect(plan$steps$description[1], "Geocorr 2000"))
})

test_that("get_available_crosswalks lists GeoCorr 2000 combinations", {
  available <- get_available_crosswalks()

  geocorr_2000 <- available |>
    dplyr::filter(source_year == 2000L, target_year == 2000L)

  expect_true(all(geocorr_2000$crosswalk_source == "geocorr"))
  expect_setequal(
    unique(c(geocorr_2000$source_geography, geocorr_2000$target_geography)),
    c("block", "block_group", "tract", "county", "place", "zcta", "puma5", "msacmsa"))
  ## 8 geographies, every ordered pair of different geographies
  expect_equal(nrow(geocorr_2000), 8L * 7L)
  expect_equal(
    nrow(dplyr::filter(geocorr_2000, source_geography == "tract", target_geography == "puma5")),
    1L)
})

# ==============================================================================
# Live GeoCorr 2000 queries (slow; network access required)
# ==============================================================================

test_that("get_crosswalk returns a 2000 tract to 2000 PUMA crosswalk from GeoCorr 2000", {
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "puma",
    source_year = 2000,
    target_year = 2000,
    weight = "population",
    silent = TRUE)

  crosswalk_tibble <- result$crosswalks$step_1
  metadata <- attr(crosswalk_tibble, "crosswalk_metadata")

  expect_equal(metadata$data_source, "geocorr")
  expect_equal(metadata$reference_year, "2000")
  expect_true(all(nchar(crosswalk_tibble$source_geoid) == 11))
  expect_true(all(nchar(crosswalk_tibble$target_geoid) == 7))
  ## 50 states and the District of Columbia; GeoCorr 2000 has no Puerto Rico
  expect_equal(dplyr::n_distinct(crosswalk_tibble$state_fips), 51)
  expect_true("population_2000" %in% colnames(crosswalk_tibble))
  expect_true(all(crosswalk_tibble$weighting_factor == "population"))

  allocation_sums <- crosswalk_tibble |>
    dplyr::summarise(
      total = sum(allocation_factor_source_to_target),
      .by = source_geoid)
  expect_true(all(abs(allocation_sums$total - 1) < 0.001))
})

test_that("get_crosswalk returns 2000 tract to ZCTA crosswalks weighted by housing", {
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2000,
    target_year = 2000,
    weight = "housing",
    silent = TRUE)

  crosswalk_tibble <- result$crosswalks$step_1

  expect_true(all(nchar(crosswalk_tibble$source_geoid) == 11))
  expect_true(all(nchar(crosswalk_tibble$target_geoid) == 5))
  expect_true("housing_2000" %in% colnames(crosswalk_tibble))
  expect_true(all(crosswalk_tibble$weighting_factor == "housing"))
})

test_that("get_crosswalk returns 2000 county to PUMA crosswalks for a 2005 year context", {
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if_offline()

  result <- get_crosswalk(
    source_geography = "county",
    target_geography = "puma5",
    source_year = 2005,
    target_year = 2005,
    weight = "land",
    silent = TRUE)

  crosswalk_tibble <- result$crosswalks$step_1
  metadata <- attr(crosswalk_tibble, "crosswalk_metadata")

  expect_equal(metadata$reference_year, "2000")
  expect_true(all(nchar(crosswalk_tibble$source_geoid) == 5))
  expect_true(all(nchar(crosswalk_tibble$target_geoid) == 7))
  expect_true("land_area_sqmi" %in% colnames(crosswalk_tibble))
})

test_that("get_crosswalk warns about and drops New England rows without MSA/CMSA codes", {
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if_offline()

  expect_warning(
    result <- get_crosswalk(
      source_geography = "tract",
      target_geography = "msacmsa",
      source_year = 2000,
      target_year = 2000,
      weight = "population"),
    regexp = "no MSA/CMSA code")

  crosswalk_tibble <- result$crosswalks$step_1

  expect_false(any(is.na(crosswalk_tibble$target_geoid)))
  expect_true(all(nchar(crosswalk_tibble$target_geoid) == 4))
  ## "9999" marks areas outside any 1999 metro area
  expect_true("9999" %in% crosswalk_tibble$target_geoid)
})
