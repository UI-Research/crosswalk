# Tests for the `weight` argument: which weights each source accepts, the
# household and housing tenure weights for NHGIS crosswalks (issue #20), and
# the weight recorded in the crosswalk metadata.

# ==============================================================================
# Weight lists
# ==============================================================================

test_that("GeoCorr weights are population, housing, and land", {
  expect_equal(get_geocorr_weights(), c("population", "housing", "land"))
})

test_that("NHGIS weight map accepts household and housing tenure weights", {
  nhgis_weight_map <- get_nhgis_weight_map()

  expect_setequal(
    names(nhgis_weight_map),
    c("population", "housing", "land", "household", "housing_owned", "housing_rented"))

  # The household and tenure weights have no fallback to another weight
  expect_equal(nhgis_weight_map$household, "household")
  expect_equal(nhgis_weight_map$housing_owned, "housing_owned")
  expect_equal(nhgis_weight_map$housing_rented, "housing_rented")

  # Every GeoCorr weight is also accepted for NHGIS
  expect_true(all(get_geocorr_weights() %in% names(nhgis_weight_map)))
})

# ==============================================================================
# Up-front validation (no downloads)
# ==============================================================================

test_that("an unrecognized weight raises an error listing accepted weights", {
  expect_error(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "zcta",
      weight = "families",
      silent = TRUE),
    "Invalid weight: 'families'.*household, housing_owned, housing_rented")
})

test_that("weight must be a single character value", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta")

  expect_error(validate_weight_for_plan(weight = NULL, plan = plan), "Invalid weight")
  expect_error(validate_weight_for_plan(weight = NA_character_, plan = plan), "Invalid weight")
  expect_error(
    validate_weight_for_plan(weight = c("population", "household"), plan = plan),
    "Invalid weight")
})

test_that("household weight for a GeoCorr crosswalk errors and lists GeoCorr weights", {
  expect_error(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "zcta",
      weight = "household",
      silent = TRUE),
    "not available from GeoCorr.*step 1 of 1.*population, housing, land")
})

test_that("housing tenure weights for a GeoCorr crosswalk error", {
  expect_error(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "puma",
      source_year = 2020,
      target_year = 2020,
      weight = "housing_owned",
      silent = TRUE),
    "not available from GeoCorr")

  expect_error(
    get_crosswalk(
      source_geography = "county",
      target_geography = "zcta",
      weight = "housing_rented",
      silent = TRUE),
    "not available from GeoCorr")
})

test_that("household weight in a NHGIS then GeoCorr chain errors naming step 2", {
  # 2010 tracts -> 2020 ZCTAs: step 1 is NHGIS (can use households), step 2
  # is GeoCorr (cannot). The error must arrive before anything is downloaded,
  # so this test needs neither a network connection nor an API key.
  expect_error(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "zcta",
      source_year = 2010,
      target_year = 2020,
      weight = "household",
      silent = TRUE),
    "step 2 of 2")
})

test_that("crosswalk_data() passes the household weight through to validation", {
  data_example <- tibble::tibble(
    source_geoid = character(),
    count_households = numeric())

  expect_error(
    crosswalk_data(
      data = data_example,
      source_geography = "tract",
      target_geography = "zcta",
      weight = "household",
      geoid_column = "source_geoid",
      silent = TRUE),
    "not available from GeoCorr")
})

test_that("household weight passes validation for NHGIS-only plans", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020)

  expect_equal(plan$steps$crosswalk_source, "nhgis")
  expect_invisible(validate_weight_for_plan(weight = "household", plan = plan))
  expect_null(validate_weight_for_plan(weight = "housing_owned", plan = plan))
  expect_null(validate_weight_for_plan(weight = "housing_rented", plan = plan))
})

# ==============================================================================
# Sources without a choice of weight (county events, CT Data Collaborative)
# ==============================================================================

test_that("county-events crosswalks accept household weight and record identity", {
  # 2014 -> 2019 contains only renames, so every row is an identity mapping
  result <- get_crosswalk(
    source_geography = "county",
    target_geography = "county",
    source_year = 2014,
    target_year = 2019,
    weight = "household",
    silent = TRUE)

  crosswalk <- result$crosswalks$step_1
  metadata <- attr(crosswalk, "crosswalk_metadata")

  expect_equal(unique(crosswalk$weighting_factor), "identity")
  expect_equal(metadata$weighting_variable, "identity")
  expect_equal(metadata$call_parameters$weight, "household")
})

test_that("county-events crosswalks note when population factors replace the requested weight", {
  # 2014 -> 2023 includes county splits, which use population-based factors
  expect_message(
    result <- get_crosswalk(
      source_geography = "county",
      target_geography = "county",
      source_year = 2014,
      target_year = 2023,
      weight = "household"),
    "population-based allocation factors.*'household' weight is not offered")

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$weighting_variable, "identity, population")
})

test_that("county-events crosswalks do not send the weight note for population", {
  messages <- testthat::capture_messages(
    get_crosswalk(
      source_geography = "county",
      target_geography = "county",
      source_year = 2014,
      target_year = 2023,
      weight = "population"))

  expect_false(any(stringr::str_detect(messages, "is not offered by this source")))
})

# ==============================================================================
# NHGIS weight selection (requires network access and an IPUMS API key)
# ==============================================================================

test_that("household weight selects the NHGIS household interpolation weight", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020,
    weight = "household",
    silent = TRUE)

  crosswalk <- result$crosswalks$step_1
  metadata <- attr(crosswalk, "crosswalk_metadata")

  expect_equal(unique(crosswalk$weighting_factor), "household")
  expect_equal(metadata$weighting_variable, "household")
  expect_equal(anyDuplicated(crosswalk[c("source_geoid", "target_geoid")]), 0L)
})

test_that("housing tenure weights select the NHGIS owner and renter weights", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  owned_result <- get_crosswalk(
    source_geography = "block_group",
    target_geography = "block_group",
    source_year = 2010,
    target_year = 2020,
    weight = "housing_owned",
    silent = TRUE)

  rented_result <- get_crosswalk(
    source_geography = "block_group",
    target_geography = "block_group",
    source_year = 2010,
    target_year = 2020,
    weight = "housing_rented",
    silent = TRUE)

  expect_equal(unique(owned_result$crosswalks$step_1$weighting_factor), "housing_owned")
  expect_equal(
    attr(owned_result$crosswalks$step_1, "crosswalk_metadata")$weighting_variable,
    "housing_owned")
  expect_equal(unique(rented_result$crosswalks$step_1$weighting_factor), "housing_rented")
  expect_equal(
    attr(rented_result$crosswalks$step_1, "crosswalk_metadata")$weighting_variable,
    "housing_rented")
})

test_that("housing weight records the NHGIS weight actually used in metadata", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  result <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020,
    weight = "housing",
    silent = TRUE)

  metadata <- attr(result$crosswalks$step_1, "crosswalk_metadata")
  expect_equal(metadata$weighting_variable, "housing_all")
})

test_that("household weight from a block-based NHGIS file errors, without substitution", {
  # National block-to-block files are large downloads
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  expect_error(
    get_crosswalk(
      source_geography = "block",
      target_geography = "block",
      source_year = 2010,
      target_year = 2020,
      weight = "household",
      silent = TRUE),
    "'household' is not available for the NHGIS.*Use one of these values for `weight`")
})
