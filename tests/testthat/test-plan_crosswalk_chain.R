# Tests for plan_crosswalk_chain() - crosswalk chain planning

# ==============================================================================
# Basic structure tests
# ==============================================================================
test_that("plan_crosswalk_chain returns expected structure", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  expect_type(plan, "list")
  expect_true("is_multi_step" %in% names(plan))
  expect_true("steps" %in% names(plan))
  expect_true("intermediate_geography" %in% names(plan))
  expect_true("intermediate_year" %in% names(plan))
  expect_true("composition_note" %in% names(plan))
  expect_true("error" %in% names(plan))
})

# ==============================================================================
# Single-step detection tests
# ==============================================================================

test_that("plan_crosswalk_chain detects same geography/year as no crosswalk", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract")

  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source[1], "none")
})

test_that("plan_crosswalk_chain detects same-year geography change as single-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta")

  expect_false(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "geocorr")
})

test_that("plan_crosswalk_chain detects same-geography year change as single-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
})

test_that("plan_crosswalk_chain detects 2020-2022 as CTData", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source[1], "ctdata_2020_2022")
})

# ==============================================================================
# Multi-step detection tests
# ==============================================================================

test_that("plan_crosswalk_chain detects geography + year change as multi-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 2)
  expect_equal(plan$intermediate_geography, "tract")
  expect_equal(plan$intermediate_year, "2020")
})

test_that("plan_crosswalk_chain multi-step has correct step order", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  # Step 1: year change (NHGIS)
  expect_equal(plan$steps$source_geography[1], "tract")
  expect_equal(plan$steps$target_geography[1], "tract")
  expect_equal(plan$steps$source_year[1], "2010")
  expect_equal(plan$steps$target_year[1], "2020")
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")

  # Step 2: geography change (Geocorr)
  expect_equal(plan$steps$source_geography[2], "tract")
  expect_equal(plan$steps$target_geography[2], "zcta")
  expect_equal(plan$steps$source_year[2], "2020")
  expect_equal(plan$steps$target_year[2], "2020")
  expect_equal(plan$steps$crosswalk_source[2], "geocorr")
})

test_that("plan_crosswalk_chain multi-step works for block_group", {
  plan <- plan_crosswalk_chain(
    source_geography = "block_group",
    target_geography = "puma",
    source_year = 2010,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(plan$intermediate_geography, "block_group")
})

# ==============================================================================
# Direct NHGIS crosswalk tests (geography + year change with direct NHGIS crosswalk)
# ==============================================================================

test_that("plan_crosswalk_chain detects direct NHGIS crosswalk for block to zcta", {
  # block 2010 -> zcta 2020 is available directly from NHGIS
  plan <- plan_crosswalk_chain(
    source_geography = "block",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
  expect_true(stringr::str_detect(plan$steps$description[1], "direct NHGIS"))
})

test_that("plan_crosswalk_chain detects direct NHGIS crosswalk for block to puma", {
  # block 2010 -> puma 2020 is available directly from NHGIS
  plan <- plan_crosswalk_chain(
    source_geography = "block",
    target_geography = "puma",
    source_year = 2010,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
})

test_that("plan_crosswalk_chain detects direct NHGIS crosswalk for block to place", {
  # block 2010 -> place 2020 is available directly from NHGIS
  plan <- plan_crosswalk_chain(
    source_geography = "block",
    target_geography = "place",
    source_year = 2010,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
})

test_that("plan_crosswalk_chain detects direct NHGIS crosswalk for block to urban_area", {
  # block 2010 -> urban_area 2020 is available directly from NHGIS
  plan <- plan_crosswalk_chain(
    source_geography = "block",
    target_geography = "urban_area",
    source_year = 2010,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
})

test_that("plan_crosswalk_chain detects direct NHGIS crosswalk for block to cbsa", {
  # block 2010 -> cbsa 2020 is available directly from NHGIS
  plan <- plan_crosswalk_chain(
    source_geography = "block",
    target_geography = "cbsa",
    source_year = 2010,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
})

test_that("plan_crosswalk_chain still uses multi-step for tract to zcta (no direct NHGIS)", {
  # tract 2010 -> zcta 2020 is NOT available directly from NHGIS
  # So it should still require multi-step
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 2)
})

test_that("plan_crosswalk_chain detects direct NHGIS crosswalk for reverse direction", {
  # block 2020 -> zcta 2010 is available directly from NHGIS
  plan <- plan_crosswalk_chain(
    source_geography = "block",
    target_geography = "zcta",
    source_year = 2020,
    target_year = 2010)

  expect_false(plan$is_multi_step)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
})

# ==============================================================================
# Error handling tests
# ==============================================================================

test_that("plan_crosswalk_chain errors for unsupported source geography in multi-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "zcta",
    target_geography = "puma",
    source_year = 2010,
    target_year = 2020)

  expect_false(is.null(plan$error))
  expect_true(stringr::str_detect(plan$error, "NHGIS does not support"))
})

test_that("plan_crosswalk_chain errors for puma as source in multi-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "puma",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  expect_false(is.null(plan$error))
})

# ==============================================================================
# Helper function tests
# ==============================================================================

test_that("standardize_geography_for_chain handles various spellings", {
  expect_equal(crosswalk:::standardize_geography_for_chain("tract"), "tract")
  expect_equal(crosswalk:::standardize_geography_for_chain("tracts"), "tract")
  expect_equal(crosswalk:::standardize_geography_for_chain("tr"), "tract")
  expect_equal(crosswalk:::standardize_geography_for_chain("block_group"), "block_group")
  expect_equal(crosswalk:::standardize_geography_for_chain("block group"), "block_group")
  expect_equal(crosswalk:::standardize_geography_for_chain("bg"), "block_group")
  expect_equal(crosswalk:::standardize_geography_for_chain("zcta"), "zcta")
  expect_equal(crosswalk:::standardize_geography_for_chain("puma"), "puma")
})

test_that("standardize_geography_for_chain handles 2010-era geography aliases", {
  expect_equal(crosswalk:::standardize_geography_for_chain("puma12"), "puma")
  expect_equal(crosswalk:::standardize_geography_for_chain("zcta5"), "zcta")
  expect_equal(crosswalk:::standardize_geography_for_chain("placefp"), "place")
  expect_equal(crosswalk:::standardize_geography_for_chain("cd115"), "cd115")
  expect_equal(crosswalk:::standardize_geography_for_chain("cd116"), "cd116")
})

test_that("determine_temporal_source returns correct source", {
  expect_equal(crosswalk:::determine_temporal_source("2010", "2020"), "nhgis")
  expect_equal(crosswalk:::determine_temporal_source("2020", "2022"), "ctdata_2020_2022")
  expect_equal(crosswalk:::determine_temporal_source("2022", "2020"), "ctdata_2020_2022")
  expect_equal(crosswalk:::determine_temporal_source("1990", "2010"), "nhgis")
})

test_that("format_chain_plan_message produces readable output", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  message <- crosswalk:::format_chain_plan_message(plan)

  expect_type(message, "character")
  expect_true(stringr::str_detect(message, "Multi-step"))
  expect_true(stringr::str_detect(message, "Step 1"))
  expect_true(stringr::str_detect(message, "Step 2"))
})

test_that("format_chain_plan_message handles single-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta")

  message <- crosswalk:::format_chain_plan_message(plan)

  expect_type(message, "character")
  expect_true(stringr::str_detect(message, "Single-step"))
})

test_that("format_chain_plan_message handles errors", {
  plan <- plan_crosswalk_chain(
    source_geography = "zcta",
    target_geography = "puma",
    source_year = 2010,
    target_year = 2020)

  message <- crosswalk:::format_chain_plan_message(plan)

  expect_true(stringr::str_detect(message, "Error"))
})

# ==============================================================================
# Composition note tests
# ==============================================================================

test_that("plan_crosswalk_chain includes composition note for multi-step", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020)

  expect_true(stringr::str_detect(plan$composition_note, "step1_allocation"))
  expect_true(stringr::str_detect(plan$composition_note, "step2_allocation"))
})

# ==============================================================================
# GeoCorr 2018 / 2010-era tests
# ==============================================================================

test_that("plan_crosswalk_chain describes GeoCorr 2018 for same-year 2010 geography change", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2010)

  expect_false(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "geocorr")
  expect_true(stringr::str_detect(plan$steps$description[1], "Geocorr 2018"))
})

test_that("plan_crosswalk_chain describes GeoCorr 2022 for same-year 2020 geography change", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2020,
    target_year = 2020)

  expect_false(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 1)
  expect_equal(plan$steps$crosswalk_source[1], "geocorr")
  expect_true(stringr::str_detect(plan$steps$description[1], "Geocorr 2022"))
})

test_that("plan_crosswalk_chain describes GeoCorr 2022 when no years specified", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta")

  expect_false(plan$is_multi_step)
  expect_true(stringr::str_detect(plan$steps$description[1], "Geocorr 2022"))
})

# ==============================================================================
# find_temporal_path() unit tests
# ==============================================================================

test_that("find_temporal_path finds direct single-hop path (2010->2020 tract)", {
  path <- crosswalk:::find_temporal_path("tract", "2010", "2020")

  expect_equal(length(path), 1)
  expect_equal(path[[1]]$source_year, "2010")
  expect_equal(path[[1]]$target_year, "2020")
})

test_that("find_temporal_path finds multi-hop path (1990->2020 tract)", {
  path <- crosswalk:::find_temporal_path("tract", "1990", "2020")

  expect_equal(length(path), 2)
  expect_equal(path[[1]]$source_year, "1990")
  expect_equal(path[[1]]$target_year, "2010")
  expect_equal(path[[2]]$source_year, "2010")
  expect_equal(path[[2]]$target_year, "2020")
})

test_that("find_temporal_path finds multi-hop path (2000->2020 tract)", {
  path <- crosswalk:::find_temporal_path("tract", "2000", "2020")

  expect_equal(length(path), 2)
  expect_equal(path[[1]]$source_year, "2000")
  expect_equal(path[[1]]$target_year, "2010")
  expect_equal(path[[2]]$source_year, "2010")
  expect_equal(path[[2]]$target_year, "2020")
})

test_that("find_temporal_path finds multi-hop path (1990->2020 block)", {
  path <- crosswalk:::find_temporal_path("block", "1990", "2020")

  expect_equal(length(path), 2)
  expect_equal(path[[1]]$source_year, "1990")
  expect_equal(path[[1]]$target_year, "2010")
  expect_equal(path[[2]]$source_year, "2010")
  expect_equal(path[[2]]$target_year, "2020")
})

test_that("find_temporal_path returns NULL for impossible path (2020->1990 tract)", {
  path <- crosswalk:::find_temporal_path("tract", "2020", "1990")

  expect_null(path)
})

test_that("find_temporal_path returns NULL for unsupported geography (zcta)", {
  path <- crosswalk:::find_temporal_path("zcta", "2010", "2020")

  expect_null(path)
})

test_that("find_temporal_path accepts pre-fetched available_crosswalks", {
  available <- crosswalk:::list_nhgis_crosswalks()
  path <- crosswalk:::find_temporal_path("tract", "2010", "2020", available)

  expect_equal(length(path), 1)
  expect_equal(path[[1]]$source_year, "2010")
  expect_equal(path[[1]]$target_year, "2020")
})

# ==============================================================================
# Multi-hop year-only plan tests
# ==============================================================================

test_that("plan_crosswalk_chain produces 2-step plan for 1990->2020 tract", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 1990,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 2)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
  expect_equal(plan$steps$crosswalk_source[2], "nhgis")

  # Step 1: 1990 tract -> 2010 tract
  expect_equal(plan$steps$source_year[1], "1990")
  expect_equal(plan$steps$target_year[1], "2010")

  # Step 2: 2010 tract -> 2020 tract
  expect_equal(plan$steps$source_year[2], "2010")
  expect_equal(plan$steps$target_year[2], "2020")

  expect_equal(plan$intermediate_geography, "tract")
  expect_equal(plan$intermediate_year, "2010")
})

test_that("plan_crosswalk_chain produces 2-step plan for 2000->2020 tract", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2000,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 2)
  expect_equal(plan$steps$source_year[1], "2000")
  expect_equal(plan$steps$target_year[1], "2010")
  expect_equal(plan$steps$source_year[2], "2010")
  expect_equal(plan$steps$target_year[2], "2020")
})

# ==============================================================================
# 3-step combined plan tests (multi-hop temporal + geography change)
# ==============================================================================

test_that("plan_crosswalk_chain produces 3-step plan for 2000 tract -> 2020 ZCTA", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2000,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 3)

  # Step 1: 2000 tract -> 2010 tract (NHGIS)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
  expect_equal(plan$steps$source_year[1], "2000")
  expect_equal(plan$steps$target_year[1], "2010")
  expect_equal(plan$steps$source_geography[1], "tract")
  expect_equal(plan$steps$target_geography[1], "tract")

  # Step 2: 2010 tract -> 2020 tract (NHGIS)
  expect_equal(plan$steps$crosswalk_source[2], "nhgis")
  expect_equal(plan$steps$source_year[2], "2010")
  expect_equal(plan$steps$target_year[2], "2020")
  expect_equal(plan$steps$source_geography[2], "tract")
  expect_equal(plan$steps$target_geography[2], "tract")

  # Step 3: 2020 tract -> 2020 ZCTA (Geocorr)
  expect_equal(plan$steps$crosswalk_source[3], "geocorr")
  expect_equal(plan$steps$source_year[3], "2020")
  expect_equal(plan$steps$target_year[3], "2020")
  expect_equal(plan$steps$source_geography[3], "tract")
  expect_equal(plan$steps$target_geography[3], "zcta")

  expect_equal(plan$intermediate_geography, "tract")
  expect_equal(plan$intermediate_year, c("2010", "2020"))
})

test_that("plan_crosswalk_chain produces 3-step plan for 1990 tract -> 2020 PUMA", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "puma",
    source_year = 1990,
    target_year = 2020)

  expect_true(plan$is_multi_step)
  expect_equal(nrow(plan$steps), 3)
  expect_equal(plan$steps$crosswalk_source[1], "nhgis")
  expect_equal(plan$steps$crosswalk_source[2], "nhgis")
  expect_equal(plan$steps$crosswalk_source[3], "geocorr")
})

# ==============================================================================
# Error cases for impossible temporal paths
# ==============================================================================

test_that("plan_crosswalk_chain errors for impossible temporal path (2020->1990 tract)", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 1990)

  expect_false(is.null(plan$error))
  expect_true(stringr::str_detect(plan$error, "No temporal crosswalk path"))
})

test_that("plan_crosswalk_chain errors for impossible temporal path in combined plan", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2020,
    target_year = 1990)

  expect_false(is.null(plan$error))
  expect_true(stringr::str_detect(plan$error, "no temporal crosswalk path"))
})

# ==============================================================================
# Format message tests for multi-hop plans
# ==============================================================================

test_that("format_chain_plan_message formats 3-step plan correctly", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2000,
    target_year = 2020)

  message <- crosswalk:::format_chain_plan_message(plan)

  expect_type(message, "character")
  expect_true(stringr::str_detect(message, "Multi-step"))
  expect_true(stringr::str_detect(message, "Step 1"))
  expect_true(stringr::str_detect(message, "Step 2"))
  expect_true(stringr::str_detect(message, "Step 3"))
  expect_true(stringr::str_detect(message, "2010, 2020"))
})

test_that("format_chain_plan_message formats 2-step year-only plan correctly", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 1990,
    target_year = 2020)

  message <- crosswalk:::format_chain_plan_message(plan)

  expect_type(message, "character")
  expect_true(stringr::str_detect(message, "Multi-step"))
  expect_true(stringr::str_detect(message, "Step 1"))
  expect_true(stringr::str_detect(message, "Step 2"))
})

# ==============================================================================
# Composition note tests for multi-hop plans
# ==============================================================================

test_that("plan_crosswalk_chain composition note covers all steps in 3-step plan", {
  plan <- plan_crosswalk_chain(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2000,
    target_year = 2020)

  expect_true(stringr::str_detect(plan$composition_note, "step1_allocation"))
  expect_true(stringr::str_detect(plan$composition_note, "step2_allocation"))
  expect_true(stringr::str_detect(plan$composition_note, "step3_allocation"))
})
