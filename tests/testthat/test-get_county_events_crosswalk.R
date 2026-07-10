# ==============================================================================
# Curated data validation (sysdata)
# ==============================================================================

test_that("curated county events data is internally consistent", {
  events <- crosswalk:::county_events
  mappings <- crosswalk:::county_event_mappings
  universes <- crosswalk:::county_universes
  meta <- crosswalk:::county_events_meta

  # Allocation factors sum to 1 per source county within each event
  factor_sums <- mappings |>
    dplyr::summarize(total = sum(allocation_factor), .by = c(event_id, source_geoid))
  expect_true(all(abs(factor_sums$total - 1) < 1e-9))

  # GEOIDs are 5-character county codes
  expect_true(all(nchar(mappings$source_geoid) == 5))
  expect_true(all(nchar(mappings$target_geoid) == 5))
  expect_true(all(nchar(universes$geoid) == 5))

  # Every event has county-level mappings, and vice versa
  expect_setequal(events$event_id, mappings$event_id)

  # Valid categorical values and vintages
  expect_true(all(events$event_type %in%
                    c("rename", "merge", "split", "part_transfer")))
  expect_true(all(events$subcounty_handling %in%
                    c("prefix_swap", "explicit_rows", "ctdata_runtime", "none", "unsupported")))
  expect_true(all(events$effective_vintage_year > 2000))
  expect_true(all(events$effective_vintage_year <= meta$known_through_year))

  # Universes at all three decennial bases
  expect_setequal(unique(universes$decade_base), c(2000L, 2010L, 2020L))
})


# ==============================================================================
# county_universe_at()
# ==============================================================================

test_that("county_universe_at rolls universes through events", {
  u2014 <- crosswalk:::county_universe_at(2014)
  u2016 <- crosswalk:::county_universe_at(2016)
  u2003 <- crosswalk:::county_universe_at(2003)
  u2021 <- crosswalk:::county_universe_at(2021)
  u2023 <- crosswalk:::county_universe_at(2023)

  # 2014: Bedford city merged (vintage 2014), Shannon County not yet renamed
  expect_false("51515" %in% u2014)
  expect_true("46113" %in% u2014)
  expect_false("46102" %in% u2014)

  # 2016: renames applied
  expect_true("46102" %in% u2016)
  expect_false("46113" %in% u2016)
  expect_true("02158" %in% u2016)
  expect_false("02270" %in% u2016)

  # 2003: Broomfield exists, Clifton Forge gone, AK reorganizations not yet
  expect_true("08014" %in% u2003)
  expect_false("51560" %in% u2003)
  expect_true("02232" %in% u2003)
  expect_false("02105" %in% u2003)

  # 2021: Valdez-Cordova split (vintage 2020) applied; CT counties still present
  expect_true(all(c("02063", "02066") %in% u2021))
  expect_false("02261" %in% u2021)
  expect_true("09001" %in% u2021)

  # 2023: CT planning regions replace counties
  expect_true("09110" %in% u2023)
  expect_false("09001" %in% u2023)
})


# ==============================================================================
# County-level crosswalks (offline)
# ==============================================================================

test_that("identity interval returns a full identity crosswalk with a message", {
  expect_message(
    cw <- crosswalk:::get_county_events_crosswalk("county", 2003, 2005),
    "No county boundary or code changes")

  expect_equal(nrow(cw), length(crosswalk:::county_universe_at(2003)))
  expect_true(all(cw$source_geoid == cw$target_geoid))
  expect_true(all(cw$allocation_factor_source_to_target == 1))
  expect_true(all(cw$weighting_factor == "identity"))
})

test_that("rename interval maps old to new FIPS codes", {
  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("county", 2014, 2016))

  expect_equal(cw$target_geoid[cw$source_geoid == "46113"], "46102")
  expect_equal(cw$target_geoid[cw$source_geoid == "02270"], "02158")
  expect_false("46102" %in% cw$source_geoid)
  expect_false("46113" %in% cw$target_geoid)

  # Everything else is identity
  others <- cw |> dplyr::filter(!source_geoid %in% c("46113", "02270"))
  expect_true(all(others$source_geoid == others$target_geoid))

  # One row per source-target pair
  expect_equal(anyDuplicated(cw[c("source_geoid", "target_geoid")]), 0L)
})

test_that("split interval fans out with population factors summing to 1", {
  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("county", 2018, 2021))

  valdez <- cw |> dplyr::filter(source_geoid == "02261")
  expect_setequal(valdez$target_geoid, c("02063", "02066"))
  expect_equal(sum(valdez$allocation_factor_source_to_target), 1)
  expect_true(all(valdez$weighting_factor == "population"))
  expect_false("02261" %in% cw$target_geoid)
})

test_that("multi-event intervals compose factors correctly", {
  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("county", 2006, 2020))

  # Factors still sum to 1 per source across composed events
  sums <- cw |>
    dplyr::summarize(
      total = sum(allocation_factor_source_to_target), .by = source_geoid)
  expect_true(all(abs(sums$total - 1) < 1e-9))

  # Skagway-Hoonah-Angoon (02232) fans through the 2007 split and the 2013
  # Petersburg transfer: 02230, 02105, and a sliver reaching 02195
  skagway <- cw |> dplyr::filter(source_geoid == "02232")
  expect_setequal(skagway$target_geoid, c("02105", "02195", "02230"))

  # Bedford city merged
  expect_equal(cw$target_geoid[cw$source_geoid == "51515"], "51019")
})

test_that("county-events crosswalk has the standard column contract", {
  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("county", 2014, 2016))

  expect_named(cw, c(
    "source_geoid", "target_geoid",
    "source_geography_name", "target_geography_name",
    "source_year", "target_year",
    "allocation_factor_source_to_target", "weighting_factor", "state_fips"))
  expect_type(cw$source_geoid, "character")
  expect_type(cw$source_year, "character")
  expect_type(cw$allocation_factor_source_to_target, "double")
  expect_equal(unique(cw$source_year), "2014")
  expect_equal(unique(cw$target_year), "2016")

  metadata <- attr(cw, "crosswalk_metadata")
  expect_equal(metadata$data_source, "county_events")
  expect_true(any(stringr::str_detect(metadata$notes, "Event applied")))
})

test_that("cache round-trip preserves types and leading zeros", {
  cache_dir <- file.path(tempdir(), "county-events-test-cache")
  dir.create(cache_dir, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE))

  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("county", 2014, 2016, cache = cache_dir))
  cw_cached <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("county", 2014, 2016, cache = cache_dir))

  expect_true(attr(cw_cached, "crosswalk_metadata")$read_from_cache)
  expect_type(cw_cached$source_geoid, "character")
  expect_true(all(stringr::str_detect(cw_cached$source_geoid[
    stringr::str_starts(cw_cached$source_geoid, "0")], "^0")))
  expect_equal(
    cw |> dplyr::arrange(source_geoid, target_geoid),
    cw_cached |> dplyr::arrange(source_geoid, target_geoid),
    ignore_attr = TRUE)
})


# ==============================================================================
# Validation errors
# ==============================================================================

test_that("reverse county requests error with a targeted message", {
  expect_error(
    crosswalk:::get_county_events_crosswalk("county", 2019, 2014),
    "forward")
})

test_that("out-of-range years error", {
  expect_error(
    crosswalk:::get_county_events_crosswalk("county", 1999, 2010),
    "2000 through")
  expect_error(
    crosswalk:::get_county_events_crosswalk("county", 2010, 2099),
    "2000 through")
})

test_that("identical years error", {
  expect_error(
    crosswalk:::get_county_events_crosswalk("county", 2014, 2014),
    "identical")
})

test_that("sub-county requests before 2010 error", {
  expect_error(
    crosswalk:::get_county_events_crosswalk("tract", 2005, 2008),
    "2010 and later")
})

test_that("sub-county requests spanning a decennial census error", {
  expect_error(
    crosswalk:::get_county_events_crosswalk("tract", 2014, 2023),
    "same tract-definition era")
})

test_that("unsupported geographies error", {
  expect_error(
    crosswalk:::get_county_events_crosswalk("zcta", 2014, 2016),
    "not supported")
})


# ==============================================================================
# list_county_events_edges()
# ==============================================================================

test_that("county edges cover all forward pairs in range", {
  edges <- crosswalk:::list_county_events_edges("county")
  known_through <- crosswalk:::county_events_meta$known_through_year
  n_years <- known_through - 2000 + 1

  expect_equal(nrow(edges), n_years * (n_years - 1) / 2)
  expect_true(all(as.numeric(edges$source_year) < as.numeric(edges$target_year)))
  expect_true(nrow(dplyr::filter(edges, source_year == "2014", target_year == "2019")) == 1)
  expect_true(nrow(dplyr::filter(edges, source_year == "2014", target_year == "2023")) == 1)
})

test_that("sub-county edges respect eras and reversal-exactness", {
  edges <- crosswalk:::list_county_events_edges("tract")

  # Forward same-era pairs exist
  expect_true(nrow(dplyr::filter(edges, source_year == "2014", target_year == "2019")) == 1)
  expect_true(nrow(dplyr::filter(edges, source_year == "2021", target_year == "2023")) == 1)

  # No cross-era pairs
  expect_equal(
    nrow(dplyr::filter(edges, source_year == "2014", target_year == "2023")), 0)

  # Backward pair over exact relabels (2015 renames) exists
  expect_true(nrow(dplyr::filter(edges, source_year == "2017", target_year == "2014")) == 1)

  # Backward pair over non-exact events (2014 Bedford/Petersburg) excluded
  expect_equal(
    nrow(dplyr::filter(edges, source_year == "2017", target_year == "2013")), 0)

  # No edges before 2010 for sub-county geographies
  expect_true(all(as.numeric(edges$source_year) >= 2010))

  # Unsupported geographies return no edges
  expect_equal(nrow(crosswalk:::list_county_events_edges("zcta")), 0)
})


# ==============================================================================
# Sub-county crosswalks (network + API keys required)
# ==============================================================================

test_that("tract rename interval applies county prefix swaps", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")

  cache_dir <- file.path(tempdir(), "county-events-subcounty-cache")
  dir.create(cache_dir, showWarnings = FALSE)

  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("tract", 2014, 2016, cache = cache_dir))

  shannon <- cw |> dplyr::filter(stringr::str_starts(source_geoid, "46113"))
  expect_true(nrow(shannon) > 0)
  expect_true(all(stringr::str_starts(shannon$target_geoid, "46102")))
  expect_equal(
    stringr::str_sub(shannon$source_geoid, 6),
    stringr::str_sub(shannon$target_geoid, 6))
  expect_true(all(shannon$allocation_factor_source_to_target == 1))

  # Non-affected tracts are identity
  other <- cw |> dplyr::filter(!stringr::str_starts(source_geoid, "46113"),
                               !stringr::str_starts(source_geoid, "02270"))
  expect_true(all(other$source_geoid == other$target_geoid))
})

test_that("tract 2021 -> 2023 composes the CT planning region relabels", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")

  cache_dir <- file.path(tempdir(), "county-events-subcounty-cache")
  dir.create(cache_dir, showWarnings = FALSE)

  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("tract", 2021, 2023, cache = cache_dir))

  ct <- cw |> dplyr::filter(state_fips == "09")
  expect_true(nrow(ct) > 0)
  # CT tracts move from county prefixes (090xx) to planning region prefixes (091xx)
  expect_true(all(stringr::str_sub(ct$target_geoid, 3, 3) == "1"))
  expect_true(all(ct$allocation_factor_source_to_target == 1))

  # Non-CT tracts are identity
  other <- cw |> dplyr::filter(state_fips != "09")
  expect_true(all(other$source_geoid == other$target_geoid))
})


# ==============================================================================
# Consistency with the CTData county crosswalk (network + API keys required)
# ==============================================================================

test_that("sysdata CT county factors match get_ctdata_crosswalk", {
  skip_if_offline()
  skip_if_not_installed("tidycensus")
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "CENSUS_API_KEY not set")

  ctdata <- suppressMessages(
    crosswalk:::get_ctdata_crosswalk("county", 2020, 2022)) |>
    dplyr::filter(stringr::str_starts(source_geoid, "09")) |>
    dplyr::arrange(source_geoid, target_geoid)

  sysdata_ct <- crosswalk:::county_event_mappings |>
    dplyr::filter(event_id == "2022_ct_planning_regions") |>
    dplyr::arrange(source_geoid, target_geoid)

  expect_equal(ctdata$source_geoid, sysdata_ct$source_geoid)
  expect_equal(ctdata$target_geoid, sysdata_ct$target_geoid)
  expect_equal(
    ctdata$allocation_factor_source_to_target,
    sysdata_ct$allocation_factor,
    tolerance = 1e-6)
})

test_that("tract 2023 -> 2021 reverses the CT relabels exactly", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")
  skip_if(Sys.getenv("CROSSWALK_RUN_SLOW_TESTS") != "true",
          "Set CROSSWALK_RUN_SLOW_TESTS=true to run")

  cache_dir <- file.path(tempdir(), "county-events-subcounty-cache")
  dir.create(cache_dir, showWarnings = FALSE)

  cw <- suppressMessages(
    crosswalk:::get_county_events_crosswalk("tract", 2023, 2021, cache = cache_dir))

  ct <- cw |> dplyr::filter(state_fips == "09")
  expect_true(nrow(ct) > 0)
  # Sources are planning-region prefixed, targets county prefixed
  expect_true(all(stringr::str_sub(ct$source_geoid, 3, 3) == "1"))
  expect_true(all(stringr::str_sub(ct$target_geoid, 3, 3) == "0"))
  expect_true(all(cw$allocation_factor_source_to_target == 1))
})
