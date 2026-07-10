# Extracted from test-get_county_events_crosswalk.R:309

# test -------------------------------------------------------------------------
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
expect_true(all(stringr::str_sub(ct$target_geoid, 3, 3) == "1"))
