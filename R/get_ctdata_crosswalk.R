#' Get Nationally Comprehensive 2020-2022 Crosswalk
#'
#' Retrieves a nationally comprehensive crosswalk between 2020 and 2022 geographies.
#' Connecticut records come from the CT Data Collaborative (handling the 2022 change
#' from historical counties to planning regions). All other states use identity
#' mapping since no geographic changes occurred outside Connecticut.
#'
#' @details This function combines:
#'   - **Connecticut**: Crosswalk from CT Data Collaborative GitHub repository.
#'     In 2022, Connecticut replaced its 8 historical counties with 9 planning regions,
#'     which changed county boundaries. For sub-county geographies (tract, block group,
#'     block), physical boundaries did not change, only the FIPS codes changed to
#'     align with the new county-level identifiers.
#'   - **Other states**: Identity mapping derived from NHGIS 2010 -> 2020 crosswalks.
#'     Since no geographic changes occurred between 2020 and 2022 outside Connecticut,
#'     source_geoid equals target_geoid with allocation_factor = 1.
#'
#'   **Reverse direction (2022 -> 2020)**: For block, block_group, and tract geographies,
#'   the crosswalk can be reversed by swapping source/target columns since these are
#'   identity mappings (same boundaries, different FIPS codes). County 2022 -> 2020 is
#'   NOT supported because Connecticut's county boundaries changed (9 planning regions
#'   to 8 counties requires different allocation factors).
#'
#' @param geography Character. Geography type: one of "block", "block_group", "tract",
#'    or "county" (county only for 2020 -> 2022 direction).
#' @param source_year Numeric. Year of the source geography, either 2020 or 2022.
#'    Default is 2020.
#' @param target_year Numeric. Year of the target geography, either 2020 or 2022.
#'    Default is 2022.
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#'
#' @return A tibble containing the national crosswalk with columns:
#'   \describe{
#'     \item{source_geoid}{The source year FIPS code}
#'     \item{target_geoid}{The target year FIPS code}
#'     \item{source_geography_name}{The geography type}
#'     \item{target_geography_name}{The geography type}
#'     \item{source_year}{The source year (2020 or 2022)}
#'     \item{target_year}{The target year (2020 or 2022)}
#'     \item{allocation_factor_source_to_target}{1 for all records (identity or CT FIPS change)}
#'     \item{weighting_factor}{"identity" for non-CT, varies for CT county}
#'     \item{state_fips}{Two-digit state FIPS code}
#'   }
#' @keywords internal
#' @noRd
get_ctdata_crosswalk <- function(geography, source_year = 2020, target_year = 2022, cache = NULL) {

  # Validate year parameters
  source_year <- as.numeric(source_year)
  target_year <- as.numeric(target_year)

  valid_combinations <- list(
    c(2020, 2022),
    c(2022, 2020))

  year_combo <- c(source_year, target_year)
  is_valid_combo <- any(sapply(valid_combinations, function(x) identical(x, year_combo)))

  if (!is_valid_combo) {
    stop(
"CTData crosswalks are only available for 2020 <-> 2022.
The provided years (", source_year, " -> ", target_year, ") are not supported.")}

  # Determine direction
  is_reverse <- source_year == 2022 && target_year == 2020

  geography_standardized <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("_", " ")

  geography_standardized <- dplyr::case_when(
    geography_standardized %in% c("block", "blocks", "blk") ~ "block",
    geography_standardized %in% c("block group", "blockgroup", "bg") ~ "block_group",
    geography_standardized %in% c("tract", "tracts", "tr") ~ "tract",
    geography_standardized %in% c("county", "counties", "co") ~ "county",
    TRUE ~ NA_character_)

  if (is.na(geography_standardized)) {
    stop(
"2020-2022 crosswalks are only available for blocks, block groups, tracts, and counties.
The provided geography '", geography, "' is not supported.")}

  # County 2022 -> 2020 is not supported (requires different allocation factors)
  if (is_reverse && geography_standardized == "county") {
    stop(
"County crosswalks from 2022 to 2020 are not supported.
Connecticut's county boundaries changed (9 planning regions -> 8 counties),
requiring population-weighted disaggregation which is not implemented.
Only block, block_group, and tract geographies support the 2022 -> 2020 direction.")}

  if (is.null(cache)) {
    cache_path <- tempdir()
  } else {
    cache_path <- cache
  }

  csv_path <- file.path(
    cache_path,
    stringr::str_c("crosswalk_national_", source_year, "_to_", target_year, "_", geography_standardized, ".csv"))

  ctdata_urls <- list(
    block = "https://raw.githubusercontent.com/CT-Data-Collaborative/2022-block-crosswalk/main/2022blockcrosswalk.csv",
    tract = "https://raw.githubusercontent.com/CT-Data-Collaborative/2022-tract-crosswalk/main/2022tractcrosswalk.csv")

  # Determine which URL will be used based on geography for CT data
 ctdata_download_url <- if (geography_standardized %in% c("block", "block_group")) {
    ctdata_urls$block
  } else {
    ctdata_urls$tract
  }

  # Check cache for full national crosswalk
  if (file.exists(csv_path) & !is.null(cache)) {
    message(stringr::str_c("Reading national ", source_year, "-", target_year, " crosswalk from cache."))
    result <- readr::read_csv(
      csv_path,
      col_types = readr::cols(.default = readr::col_character(),
                              allocation_factor_source_to_target = readr::col_double()),
      show_col_types = FALSE)

    # Weighting note for metadata
    weighting_note <- "All records have allocation_factor = 1 (identity mapping or CT FIPS code change)."

    attr(result, "crosswalk_metadata") <- list(
      data_source = "ctdata_nhgis_combined",
      data_source_full_name = "CT Data Collaborative (CT) + NHGIS-derived identity mapping (other states)",
      ctdata_download_url = ctdata_download_url,
      ctdata_github_repository = "https://github.com/CT-Data-Collaborative",
      ctdata_documentation_url = "https://github.com/CT-Data-Collaborative/2022-tract-crosswalk",
      nhgis_crosswalk_used = stringr::str_c(geography_standardized, "2010_", geography_standardized, "2020"),
      nhgis_citation_url = "https://www.nhgis.org/citation-and-use-nhgis-data",
      source_year = as.character(source_year),
      target_year = as.character(target_year),
      source_geography = geography,
      source_geography_standardized = geography_standardized,
      target_geography = geography,
      target_geography_standardized = geography_standardized,
      state_coverage = "National (all 50 states, DC, and Puerto Rico)",
      notes = c(
        "Connecticut: 8 historical counties replaced by 9 planning regions in 2022 (county boundaries changed; sub-county geographies had FIPS code changes only).",
        "Other states: No geographic changes between 2020 and 2022 (identity mapping).",
        weighting_note),
      retrieved_at = NA,
      cached = TRUE,
      cache_path = csv_path,
      read_from_cache = TRUE)

    return(result)
  }

  message("Constructing nationally comprehensive 2020-2022 crosswalk...")

  # ===========================================================================
  # STEP 1: Get all 2020 GEOIDs from NHGIS crosswalk (non-CT) or tidycensus (county)
  # ===========================================================================

  # Map geography names for NHGIS
  nhgis_geog_map <- c(
    "block" = "block",
    "block_group" = "block_group",
    "tract" = "tract",
    "county" = "county")

  nhgis_source_geog <- nhgis_geog_map[[geography_standardized]]

  if (geography_standardized == "county") {
    # For county, use tidycensus since NHGIS doesn't have county -> county crosswalks
    message("Fetching all 2020 county GEOIDs via tidycensus...")

    all_2020_geoids <- suppressMessages({
      tidycensus::get_acs(
        year = 2021,
        geography = "county",
        variables = "B01003_001",
        output = "wide") |>
        dplyr::select(geoid_2020 = GEOID) |>
        dplyr::filter(!stringr::str_starts(geoid_2020, "09")) |>
        dplyr::pull(geoid_2020)
    })

  } else {
    # For block, block_group, tract: use NHGIS 2010 -> 2020 crosswalk
    message(stringr::str_c(
      "Fetching NHGIS ", nhgis_source_geog, " 2010 -> 2020 crosswalk to obtain all 2020 GEOIDs..."))

    nhgis_crosswalk <- get_nhgis_crosswalk(
      source_year = 2010,
      source_geography = nhgis_source_geog,
      target_year = 2020,
      target_geography = nhgis_source_geog,
      cache = cache)

    # Extract unique 2020 GEOIDs, excluding Connecticut (FIPS 09)
    all_2020_geoids <- nhgis_crosswalk |>
      dplyr::select(target_geoid) |>
      dplyr::distinct() |>
      dplyr::filter(!stringr::str_starts(target_geoid, "09")) |>
      dplyr::pull(target_geoid)
  }

  message(stringr::str_c(
    "Found ", format(length(all_2020_geoids), big.mark = ","),
    " non-CT 2020 ", geography_standardized, " GEOIDs."))

  # ===========================================================================
  # STEP 2: Create identity mapping for non-CT states
  # ===========================================================================

  non_ct_crosswalk <- tibble::tibble(
    source_geoid = all_2020_geoids,
    target_geoid = all_2020_geoids,
    source_geography_name = geography_standardized,
    target_geography_name = geography_standardized,
    source_year = "2020",
    target_year = "2022",
    allocation_factor_source_to_target = 1,
    weighting_factor = "identity",
    state_fips = stringr::str_sub(all_2020_geoids, 1, 2))

  # ===========================================================================
  # STEP 3: Get CT-specific crosswalk from CT Data Collaborative
  # ===========================================================================

  message("Fetching Connecticut crosswalk from CT Data Collaborative...")

  if (geography_standardized == "block") {
    raw_df <- readr::read_csv(ctdata_urls$block, show_col_types = FALSE)

    ct_crosswalk <- raw_df |>
      dplyr::transmute(
        source_geoid = block_fips_2020,
        target_geoid = block_fips_2022,
        source_geography_name = "block",
        target_geography_name = "block",
        source_year = "2020",
        target_year = "2022",
        allocation_factor_source_to_target = 1,
        weighting_factor = "identity",
        state_fips = "09")

  } else if (geography_standardized == "block_group") {
    raw_df <- readr::read_csv(ctdata_urls$block, show_col_types = FALSE)

    ct_crosswalk <- raw_df |>
      dplyr::transmute(
        source_geoid = stringr::str_sub(block_fips_2020, 1, 12),
        target_geoid = stringr::str_sub(block_fips_2022, 1, 12)) |>
      dplyr::distinct() |>
      dplyr::mutate(
        source_geography_name = "block_group",
        target_geography_name = "block_group",
        source_year = "2020",
        target_year = "2022",
        allocation_factor_source_to_target = 1,
        weighting_factor = "identity",
        state_fips = "09")

  } else if (geography_standardized == "tract") {
    raw_df <- readr::read_csv(ctdata_urls$tract, show_col_types = FALSE)

    ct_crosswalk <- raw_df |>
      dplyr::transmute(
        source_geoid = tract_fips_2020,
        target_geoid = Tract_fips_2022,
        source_geography_name = "tract",
        target_geography_name = "tract",
        source_year = "2020",
        target_year = "2022",
        allocation_factor_source_to_target = 1,
        weighting_factor = "identity",
        state_fips = "09")

  } else if (geography_standardized == "county") {
    raw_df <- readr::read_csv(ctdata_urls$tract, show_col_types = FALSE) |>
      janitor::clean_names() |>
      dplyr::select(
        tract_fips_2020,
        tract_fips_2022,
        county_fips_2020,
        county_fips_2022 = ce_fips_2022)

    ct_tract_populations <- suppressMessages({
      tidycensus::get_acs(
        year = 2021,
        geography = "tract",
        state = "CT",
        variables = "B01003_001",
        output = "wide") |>
        dplyr::select(
          tract_fips_2020 = GEOID,
          population_2020 = B01003_001E)
    })

    ct_crosswalk <- raw_df |>
      dplyr::left_join(ct_tract_populations, by = "tract_fips_2020") |>
      dplyr::summarize(
        population_2020 = sum(population_2020, na.rm = TRUE),
        .by = c("county_fips_2020", "county_fips_2022")) |>
      dplyr::mutate(
        population_2020_total = sum(population_2020, na.rm = TRUE),
        .by = "county_fips_2020") |>
      dplyr::mutate(
        source_geoid = county_fips_2020,
        target_geoid = county_fips_2022,
        source_geography_name = "county",
        target_geography_name = "county",
        source_year = "2020",
        target_year = "2022",
        allocation_factor_source_to_target = population_2020 / population_2020_total,
        weighting_factor = "population",
        state_fips = "09") |>
      dplyr::select(
        source_geoid, target_geoid,
        source_geography_name, target_geography_name,
        source_year, target_year,
        allocation_factor_source_to_target,
        weighting_factor, state_fips)
  }

  # ===========================================================================
  # STEP 4: Combine CT and non-CT crosswalks
  # ===========================================================================

  result <- dplyr::bind_rows(ct_crosswalk, non_ct_crosswalk) |>
    dplyr::arrange(source_geoid)

  message(stringr::str_c(
    "National 2020-2022 crosswalk constructed: ",
    format(nrow(ct_crosswalk), big.mark = ","), " CT records + ",
    format(nrow(non_ct_crosswalk), big.mark = ","), " non-CT records = ",
    format(nrow(result), big.mark = ","), " total records."))

  # ===========================================================================
  # STEP 4b: Reverse direction if needed (2022 -> 2020)
  # ===========================================================================

  if (is_reverse) {
    message("Reversing crosswalk direction to 2022 -> 2020...")

    # For identity crosswalks (block, block_group, tract), simply swap columns
    # Note: County is not supported for reverse direction (checked earlier)
    result <- result |>
      dplyr::mutate(
        # Swap source and target geoids
        temp_geoid = source_geoid,
        source_geoid = target_geoid,
        target_geoid = temp_geoid,
        # Update years
        source_year = "2022",
        target_year = "2020") |>
      dplyr::select(-temp_geoid) |>
      dplyr::arrange(source_geoid)
  }

  # ===========================================================================
  # STEP 5: Cache and return
  # ===========================================================================

  if (!is.null(cache)) {
    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE)
    }
    readr::write_csv(result, csv_path)
    message(stringr::str_c("Cached to: ", csv_path))
  }

  message(stringr::str_c(
"National ", source_year, "-", target_year, " crosswalk constructed:
- Connecticut: CT Data Collaborative (https://github.com/CT-Data-Collaborative)
- Other states: Identity mapping derived from NHGIS 2010-2020 crosswalk"))

  # Attach metadata to result
  weighting_note <- "All records have allocation_factor = 1 (identity mapping or CT FIPS code change)."

  attr(result, "crosswalk_metadata") <- list(
    data_source = "ctdata_nhgis_combined",
    data_source_full_name = "CT Data Collaborative (CT) + NHGIS-derived identity mapping (other states)",
    ctdata_download_url = ctdata_download_url,
    ctdata_github_repository = "https://github.com/CT-Data-Collaborative",
    ctdata_documentation_url = "https://github.com/CT-Data-Collaborative/2022-tract-crosswalk",
    nhgis_crosswalk_used = stringr::str_c(geography_standardized, "2010_", geography_standardized, "2020"),
    nhgis_citation_url = "https://www.nhgis.org/citation-and-use-nhgis-data",
    source_year = as.character(source_year),
    target_year = as.character(target_year),
    source_geography = geography,
    source_geography_standardized = geography_standardized,
    target_geography = geography,
    target_geography_standardized = geography_standardized,
    state_coverage = "National (all 50 states, DC, and Puerto Rico)",
    notes = c(
      "Connecticut: 8 historical counties replaced by 9 planning regions in 2022 (county boundaries changed; sub-county geographies had FIPS code changes only).",
      "Other states: No geographic changes between 2020 and 2022 (identity mapping).",
      weighting_note),
    retrieved_at = Sys.time(),
    cached = !is.null(cache),
    cache_path = if (!is.null(cache)) csv_path else NULL,
    read_from_cache = FALSE)

  return(result)
}

utils::globalVariables(c(
  "B01003_001E", "GEOID", "Tract_fips_2022", "allocation_factor_source_to_target",
  "block_fips_2020", "block_fips_2022", "ce_fips_2022", "county_fips_2020",
  "county_fips_2022", "geoid_2020", "population_2020", "population_2020_total",
  "source_geography_name", "source_geoid", "source_year", "state_fips",
  "target_geography_name", "target_geoid", "target_year", "temp_geoid",
  "tract_fips_2020", "tract_fips_2022", "weighting_factor"))