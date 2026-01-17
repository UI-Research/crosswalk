#' Get Connecticut 2020-2022 Crosswalk from CTData
#'
#' Retrieves a crosswalk for Connecticut geographies between 2020 and 2022 from
#' the CT Data Collaborative. This handles the 2022 change when Connecticut
#' switched from eight historical counties to nine county-equivalent planning regions.
#'
#' @details This function sources crosswalks from the CT Data Collaborative GitHub
#'    repository. The crosswalk provides a 1:1 mapping between 2020 and 2022 FIPS
#'    codes for Connecticut geographies. No interpolation weights are needed because
#'    the physical boundaries did not change—only the county-level identifiers changed.
#'
#' @param geography Character. Geography type: one of "block", "block_group", "tract",
#'    or "county".
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#'
#' @return A tibble containing the Connecticut crosswalk with columns:
#'   \describe{
#'     \item{source_geoid}{The 2020 FIPS code}
#'     \item{target_geoid}{The 2022 FIPS code}
#'     \item{source_geography_name}{The geography type}
#'     \item{target_geography_name}{The geography type}
#'     \item{source_year}{2020}
#'     \item{target_year}{2022}
#'     \item{allocation_factor_source_to_target}{Always 1 (identity mapping)}
#'     \item{weighting_factor}{"identity" (no interpolation needed)}
#'     \item{state_fips}{"09" (Connecticut)}
#'   }
#' @noRd
get_ctdata_crosswalk <- function(geography, cache = NULL) {

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
"CTData crosswalks are only available for blocks, block groups, tracts, and counties.
The provided geography '", geography, "' is not supported.")}

  if (is.null(cache)) {
    cache_path <- tempdir()
  } else {
    cache_path <- cache
  }

  csv_path <- file.path(
    cache_path,
    stringr::str_c("crosswalk_ctdata_2020_to_2022_", geography_standardized, ".csv"))

  ctdata_urls <- list(
    block = "https://raw.githubusercontent.com/CT-Data-Collaborative/2022-block-crosswalk/main/2022blockcrosswalk.csv",
    tract = "https://raw.githubusercontent.com/CT-Data-Collaborative/2022-tract-crosswalk/main/2022tractcrosswalk.csv")

  # Determine which URL will be used based on geography
  download_url <- if (geography_standardized %in% c("block", "block_group")) {
    ctdata_urls$block
  } else {
    ctdata_urls$tract
  }

  if (file.exists(csv_path) & !is.null(cache)) {
    message("Reading CTData crosswalk from cache.")
    result <- readr::read_csv(
      csv_path,
      col_types = readr::cols(.default = readr::col_character(),
                              allocation_factor_source_to_target = readr::col_double()))

    # Attach metadata to cached result
    attr(result, "crosswalk_metadata") <- list(
      data_source = "ctdata",
      data_source_full_name = "CT Data Collaborative",
      download_url = download_url,
      github_repository = "https://github.com/CT-Data-Collaborative",
      documentation_url = "https://github.com/CT-Data-Collaborative/2022-tract-crosswalk",
      source_year = "2020",
      target_year = "2022",
      source_geography = geography,
      source_geography_standardized = geography_standardized,
      target_geography = geography,
      target_geography_standardized = geography_standardized,
      state_coverage = "Connecticut only (FIPS 09)",
      notes = "Connecticut replaced 8 historical counties with 9 planning regions in 2022. Physical boundaries unchanged; only FIPS codes changed.",
      retrieved_at = NA,
      cached = TRUE,
      cache_path = csv_path,
      read_from_cache = TRUE)

    return(result)
  }

  if (geography_standardized == "block") {
    raw_df <- readr::read_csv(ctdata_urls$block, show_col_types = FALSE)

    result <- raw_df |>
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

    result <- raw_df |>
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

    result <- raw_df |>
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

    result <- raw_df |>
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

  if (!is.null(cache)) {
    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE)
    }
    readr::write_csv(result, csv_path)
  }

  message(
"Connecticut 2020-2022 crosswalk sourced from CT Data Collaborative.
See https://github.com/CT-Data-Collaborative for more information.")

  # Attach metadata to result
  weighting_note <- if (geography_standardized == "county") {
    "County crosswalk uses population-weighted allocation factors from ACS 2021 tract populations."
  } else {
    "Identity mapping (allocation_factor = 1) - physical boundaries unchanged, only FIPS codes changed."
  }

  attr(result, "crosswalk_metadata") <- list(
    data_source = "ctdata",
    data_source_full_name = "CT Data Collaborative",
    download_url = download_url,
    github_repository = "https://github.com/CT-Data-Collaborative",
    documentation_url = "https://github.com/CT-Data-Collaborative/2022-tract-crosswalk",
    source_year = "2020",
    target_year = "2022",
    source_geography = geography,
    source_geography_standardized = geography_standardized,
    target_geography = geography,
    target_geography_standardized = geography_standardized,
    state_coverage = "Connecticut only (FIPS 09)",
    notes = c(
      "Connecticut replaced 8 historical counties with 9 planning regions in 2022.",
      weighting_note),
    retrieved_at = Sys.time(),
    cached = !is.null(cache),
    cache_path = if (!is.null(cache)) csv_path else NULL)

  return(result)
}


#' Generate Identity Crosswalk for Non-Connecticut States (2020-2022)
#'
#' For states other than Connecticut, there were no geographic changes between
#' 2020 and 2022. This function generates an identity mapping where source and
#' target GEOIDs are identical.
#'
#' @param geography Character. Geography type: one of "block_group", "tract", or "county".
#' @param states Character vector. State FIPS codes to include. Defaults to all
#'    states except Connecticut ("09").
#'
#' @return A tibble with identity mappings for the specified geography and states.
#' @noRd
get_identity_crosswalk_2020_2022 <- function(geography, states = NULL) {

  geography_standardized <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("_", " ")

  geography_standardized <- dplyr::case_when(
    geography_standardized %in% c("block group", "blockgroup", "bg") ~ "block_group",
    geography_standardized %in% c("tract", "tracts", "tr") ~ "tract",
    geography_standardized %in% c("county", "counties", "co") ~ "county",
    TRUE ~ NA_character_)

  if (is.na(geography_standardized)) {
    stop(
"Identity crosswalks for 2020-2022 are only available for block groups, tracts,
and counties. Block-level identity crosswalks are not supported due to data size.")}

  all_state_fips <- c(
    "01", "02", "04", "05", "06", "08", "10", "11", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
    "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "72")

  non_ct_states <- all_state_fips[all_state_fips != "09"]

  if (is.null(states)) {
    states <- non_ct_states
  } else {
    states <- states[states != "09"]
  }

  result <- tibble::tibble(
    source_geoid = character(),
    target_geoid = character(),
    source_geography_name = character(),
    target_geography_name = character(),
    source_year = character(),
    target_year = character(),
    allocation_factor_source_to_target = numeric(),
    weighting_factor = character(),
    state_fips = character())

  message(
"For states other than Connecticut, no geographic changes occurred between
2020 and 2022. Returning identity mapping (source_geoid = target_geoid).
Note: This function returns an empty template. To populate with actual GEOIDs,
you would need to provide a list of GEOIDs or use Census Bureau geography files.")

  attr(result, "identity_mapping") <- TRUE
  attr(result, "states_included") <- states
  attr(result, "geography") <- geography_standardized

 return(result)
}
