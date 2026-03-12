# Tests for GEOID length correctness across all crosswalk sources
#
# Standard Census GEOID structures:
#   block:                        15 chars (state=2 + county=3 + tract=6 + block=4)
#   block_group:                  12 chars (state=2 + county=3 + tract=6 + bg=1)
#   tract:                        11 chars (state=2 + county=3 + tract=6)
#   county:                        5 chars (state=2 + county=3)
#   place:                         7 chars (state=2 + place=5)
#   zcta:                          5 chars (5-digit ZCTA, no state prefix)
#   puma:                          7 chars (state=2 + puma=5)
#   core_based_statistical_area:   5 chars (5-digit CBSA code)
#   urban_area:                    5 chars (5-digit UA code)
#   congressional district:        4 chars (state=2 + district=2)

expected_geoid_lengths <- c(
  "block" = 15L,
  "block_group" = 12L,
  "block group" = 12L,
  "tract" = 11L,
  "county" = 5L,
  "place" = 7L,
  "zcta" = 5L,
  "puma" = 7L,
  "puma12" = 7L,
  "puma22" = 7L,
  "core_based_statistical_area" = 5L,
  "urban_area" = 5L,
  "cd115" = 4L,
  "cd116" = 4L,
  "cd118" = 4L,
  "cd119" = 4L
)

# Helper: assert GEOID lengths for all steps in a crosswalk result
assert_geoid_lengths <- function(result, label = "") {
  for (step_name in names(result$crosswalks)) {
    step_df <- result$crosswalks[[step_name]]
    metadata <- attr(step_df, "crosswalk_metadata")

    if (is.null(step_df) || nrow(step_df) == 0) next

    source_geog <- metadata$call_parameters$source_geography
    target_geog <- metadata$call_parameters$target_geography
    if (is.null(source_geog)) source_geog <- metadata$source_geography
    if (is.null(target_geog)) target_geog <- metadata$target_geography

    expected_source <- unname(expected_geoid_lengths[source_geog])
    expected_target <- unname(expected_geoid_lengths[target_geog])

    prefix <- paste0(label, " ", step_name)

    if (!is.na(expected_source)) {
      source_lengths <- nchar(step_df$source_geoid)
      bad_source <- which(source_lengths != expected_source)
      expect_equal(
        length(bad_source), 0,
        label = paste0(
          prefix, " source_geoid (", source_geog, "): expected ",
          expected_source, " chars, found ",
          paste(sort(unique(source_lengths[bad_source])), collapse = ","),
          " in ", length(bad_source), "/", nrow(step_df), " rows"))
    }

    if (!is.na(expected_target)) {
      target_lengths <- nchar(step_df$target_geoid)
      bad_target <- which(target_lengths != expected_target)
      expect_equal(
        length(bad_target), 0,
        label = paste0(
          prefix, " target_geoid (", target_geog, "): expected ",
          expected_target, " chars, found ",
          paste(sort(unique(target_lengths[bad_target])), collapse = ","),
          " in ", length(bad_target), "/", nrow(step_df), " rows"))
    }
  }
}

# ==============================================================================
# GeoCorr 2022 GEOID length tests
# ==============================================================================

test_that("GeoCorr 2022 crosswalks have correct-length GEOIDs", {
  skip_if_offline()

  # Note: tract->county is nested (returns empty). Use non-nested pairs to

  # test each geography type: zcta->county for county, place->zcta for place.
  tests <- list(
    list(source = "tract",       target = "zcta"),
    list(source = "tract",       target = "puma"),
    list(source = "tract",       target = "place"),
    list(source = "block_group", target = "zcta"),
    list(source = "zcta",        target = "county"),
    list(source = "county",      target = "zcta")
  )

  for (t in tests) {
    result <- get_crosswalk(
      source_geography = t$source,
      target_geography = t$target,
      weight = "population")
    assert_geoid_lengths(result, label = paste0("geocorr2022 ", t$source, "->", t$target))
  }
})

test_that("GeoCorr 2022 congressional district crosswalks have correct-length GEOIDs", {
  skip_if_offline()

  for (cd in c("cd118", "cd119")) {
    result <- get_crosswalk(
      source_geography = cd,
      target_geography = "zcta",
      weight = "population")
    assert_geoid_lengths(result, label = paste0("geocorr2022 ", cd, "->zcta"))
  }
})

# ==============================================================================
# GeoCorr 2018 GEOID length tests
# ==============================================================================

test_that("GeoCorr 2018 crosswalks have correct-length GEOIDs", {
  skip_if_offline()

  tests <- list(
    list(source = "tract",       target = "zcta"),
    list(source = "tract",       target = "puma"),
    list(source = "tract",       target = "place"),
    list(source = "block_group", target = "zcta"),
    list(source = "zcta",        target = "county"),
    list(source = "county",      target = "zcta")
  )

  for (t in tests) {
    result <- get_crosswalk(
      source_geography = t$source,
      target_geography = t$target,
      source_year = 2010,
      target_year = 2010,
      weight = "population")
    assert_geoid_lengths(result, label = paste0("geocorr2018 ", t$source, "->", t$target))
  }
})

test_that("GeoCorr 2018 congressional district crosswalks have correct-length GEOIDs", {
  skip_if_offline()

  for (cd in c("cd115", "cd116")) {
    result <- get_crosswalk(
      source_geography = cd,
      target_geography = "zcta",
      source_year = 2010,
      target_year = 2010,
      weight = "population")
    assert_geoid_lengths(result, label = paste0("geocorr2018 ", cd, "->zcta"))
  }
})

# ==============================================================================
# CTData 2020-2022 GEOID length tests
# ==============================================================================

test_that("CTData 2020-2022 crosswalks have correct-length GEOIDs", {
  skip_if_offline()

  for (geog in c("tract", "block_group", "county")) {
    result <- get_crosswalk(
      source_geography = geog,
      target_geography = geog,
      source_year = 2020,
      target_year = 2022)
    assert_geoid_lengths(result, label = paste0("ctdata ", geog))
  }
})

# ==============================================================================
# NHGIS GEOID length tests
# ==============================================================================

test_that("NHGIS same-geography cross-decade crosswalks have correct-length GEOIDs", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  tests <- list(
    list(sg = "tract",       sy = 2010, tg = "tract",       ty = 2020),
    list(sg = "block_group", sy = 2010, tg = "block_group", ty = 2020),
    list(sg = "tract",       sy = 2020, tg = "tract",       ty = 2010),
    list(sg = "tract",       sy = 1990, tg = "tract",       ty = 2010),
    list(sg = "tract",       sy = 2000, tg = "tract",       ty = 2010)
  )

  for (t in tests) {
    result <- get_crosswalk(
      source_geography = t$sg,
      target_geography = t$tg,
      source_year = t$sy,
      target_year = t$ty)
    assert_geoid_lengths(
      result,
      label = paste0("nhgis ", t$sg, " ", t$sy, "->", t$tg, " ", t$ty))
  }
})

test_that("NHGIS cross-geography cross-decade crosswalks have correct-length GEOIDs", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  tests <- list(
    list(sg = "tract",       sy = 2010, tg = "county", ty = 2020),
    list(sg = "block_group", sy = 2010, tg = "county", ty = 2020),
    list(sg = "block_group", sy = 2010, tg = "tract",  ty = 2020)
  )

  for (t in tests) {
    result <- get_crosswalk(
      source_geography = t$sg,
      target_geography = t$tg,
      source_year = t$sy,
      target_year = t$ty)
    assert_geoid_lengths(
      result,
      label = paste0("nhgis ", t$sg, " ", t$sy, "->", t$tg, " ", t$ty))
  }
})

test_that("NHGIS non-census year crosswalks have correct-length GEOIDs", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  for (ty in c(2011, 2012, 2014, 2015, 2022)) {
    result <- get_crosswalk(
      source_geography = "tract",
      target_geography = "tract",
      source_year = 2010,
      target_year = ty)
    assert_geoid_lengths(
      result,
      label = paste0("nhgis tract 2010->tract ", ty))
  }
})
