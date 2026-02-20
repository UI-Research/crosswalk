#' Get a crosswalk(s) to translate data across time and geographies
#'
#' Retrieves a crosswalk with interpolation values from a source geography to a target
#' geography, optionally across different years. Always returns a list with a consistent
#' structure containing one or more crosswalk tibbles.
#'
#' @details This function sources crosswalks from Geocorr 2022, Geocorr 2018,
#'    IPUMS NHGIS, and CT Data Collaborative. Crosswalk weights are from the
#'    original sources and have not been modified; this function merely standardizes
#'    the format of the returned crosswalks and enables easy programmatic access
#'    and caching.
#'
#'    **GeoCorr version selection**: For same-year geography crosswalks, the
#'    appropriate GeoCorr version is selected automatically based on the year:
#'    - Years 2020+ (or no year specified): GeoCorr 2022 (2020 Census geography)
#'    - Years 2010-2019: GeoCorr 2018 (2010 Census geography)
#'
#'    **Geography name resolution**: User-facing geography names like "puma",
#'    "zcta", "place", and "blockgroup" are automatically resolved to the correct
#'    API codes for the selected GeoCorr version. Version-specific names are also
#'    accepted (e.g., "puma12" for GeoCorr 2018, "puma22" for GeoCorr 2022).
#'
#'    **Multi-step crosswalks**: When both geography AND year change (e.g.,
#'    2010 tracts to 2020 ZCTAs), no single crosswalk source provides this directly.
#'    This function returns multiple crosswalks that should be applied sequentially:
#'    1. First crosswalk changes year (via NHGIS): source_geog(source_year) -> source_geog(target_year)
#'    2. Second crosswalk changes geography (via Geocorr): source_geog(target_year) -> target_geog(target_year)
#'
#'    **Non-census year support**: For target years 2011, 2012, 2014, 2015, and 2022,
#'    crosswalks are available only for block groups, tracts, and counties. These
#'    years correspond to American Community Survey geography changes.
#'
#'    **2020 to 2022 crosswalks**: The 2022 geographic changes only affected
#'    Connecticut (county-equivalent planning regions replaced historical counties).
#'    For this case, the function combines CT Data Collaborative crosswalks for
#'    Connecticut with identity mappings for other states.
#'
#'    Note that an IPUMS NHGIS API key is required to access crosswalks from that
#'    source. Use `usethis::edit_r_environ(scope = "user")` to save your API key
#'    to your .Renviron; the name of the key should be "IPUMS_API_KEY". You can
#'    obtain a key from: https://account.ipums.org/api_keys.
#'
#' @param source_year Character or numeric. Year of the source geography, one of
#'    c(1990, 2000, 2010, 2020).
#' @param source_geography Character. Source geography name. One of c("block",
#'    "block group", "tract", "place", "county", "urban_area", "zcta", "puma",
#'    "puma12", "puma22", "cd115", "cd116", "cd118", "cd119", "urban_area",
#'    "core_based_statistical_area").
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020) for decennial crosswalks, or c(2011, 2012, 2014,
#'    2015, 2022) for non-census year crosswalks (limited to block groups, tracts,
#'    and counties).
#' @param target_geography Character. Target geography name. One of c("block",
#'    "block group", "tract", "place", "county", "urban_area", "zcta", "puma",
#'    "puma12", "puma22", "cd115", "cd116", "cd118", "cd119", "urban_area",
#'    "core_based_statistical_area").
#' @param weight Character. Weighting variable for Geocorr crosswalks. One of
#'    c("population", "housing", "land").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk. Individual component crosswalks
#'    are cached separately when provided.
#' @param silent Logical. If `TRUE`, suppresses all informational messages and
#'    warnings. Defaults to `getOption("crosswalk.silent", FALSE)`. Set
#'    `options(crosswalk.silent = TRUE)` to silence all calls by default.
#'
#' @return A list with a consistent structure:
#'    \describe{
#'      \item{crosswalks}{A named list of crosswalk tibbles (step_1, step_2, etc.).
#'         Single-step transformations have one crosswalk; multi-step have two or more.}
#'      \item{plan}{The crosswalk plan describing the transformation steps}
#'      \item{message}{A formatted message describing the crosswalk chain}
#'    }
#'
#'    Each crosswalk tibble includes an attribute `crosswalk_metadata` (access via
#'    `attr(result$crosswalks$step_1, "crosswalk_metadata")`) containing comprehensive
#'    information about how the crosswalk was produced.
#'
#'    Columns in returned crosswalk dataframes (some may not be present depending on source):
#'   \describe{
#'     \item{source_geoid}{A unique identifier for the source geography}
#'     \item{target_geoid}{A unique identifier for the target geography}
#'     \item{source_geography_name}{The name of the source geography}
#'     \item{target_geography_name}{The name of the target geography}
#'     \item{source_year}{The year of the source geography}
#'     \item{target_year}{The year of the target geography}
#'     \item{allocation_factor_source_to_target}{The weight to interpolate values
#'        from the source geography to the target geography}
#'     \item{allocation_factor_target_to_source}{The weight to interpolate values
#'        from the target geography to the source geography}
#'     \item{population_2020}{The estimated overlap in population, if applicable}
#'     \item{housing_2020}{The estimated overlap in housing units, if applicable}
#'     \item{land_area_sqmi}{The overlap in land area, if applicable}
#'     \item{weighting_factor}{The attribute used to calculate allocation factors}
#'     \item{state_fips}{Two-digit state FIPS code, if applicable}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Same-year crosswalk between geographies (uses Geocorr)
#' # Returns list with one crosswalk in crosswalks$step_1
#' result <- get_crosswalk(
#'   source_geography = "zcta",
#'   target_geography = "puma22",
#'   weight = "population",
#'   cache = here::here("crosswalks-cache"))
#'
#' # Apply to data using crosswalk_data()
#' output <- crosswalk_data(
#'   data = my_data,
#'   crosswalk = result,
#'   count_columns = "count_population")
#'
#' # Multi-step crosswalk: both geography AND year change
#' # Returns list with two crosswalks in crosswalks$step_1 and crosswalks$step_2
#' result <- get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "zcta",
#'   source_year = 2010,
#'   target_year = 2020,
#'   weight = "population")
#'
#' # crosswalk_data() automatically applies all steps
#' output <- crosswalk_data(
#'   data = my_data,
#'   crosswalk = result,
#'   count_columns = "count_population")
#'
#' # To get intermediate results, set return_intermediate = TRUE
#' output <- crosswalk_data(
#'   data = my_data,
#'   crosswalk = result,
#'   count_columns = "count_population",
#'   return_intermediate = TRUE)
#' }

get_crosswalk <- function(
  source_geography,
  target_geography,
  source_year = NULL,
  target_year = NULL,
  cache = NULL,
  weight = "population",
  silent = getOption("crosswalk.silent", FALSE)) {

  old_opts <- options(crosswalk.silent = silent)
  on.exit(options(old_opts), add = TRUE)

  # Check for nested geographies (no crosswalk needed)
  # Determine if years match (both NULL, or both non-NULL and equal)
  years_match <- (is.null(source_year) && is.null(target_year)) ||
    (!is.null(source_year) && !is.null(target_year) && isTRUE(source_year == target_year))

  is_nested <- (source_geography == "block" && target_geography %in% c("block group", "tract", "county", "core_based_statistical_area")) ||
    (source_geography == "block group" && target_geography %in% c("tract", "county", "core_based_statistical_area")) ||
    (source_geography == "tract" && target_geography %in% c("county", "core_based_statistical_area")) ||
    (source_geography == "county" && target_geography == "core_based_statistical_area")

  if (is_nested && years_match) {
    cw_warning(
"The source geography is nested within the target geography and an empty result
will be returned. No crosswalk is needed to translate data between nested geographies;
simply aggregate your data to the desired geography.", call. = FALSE)

    # Return empty list structure for consistency
    return(list(
      crosswalks = list(step_1 = tibble::tibble()),
      plan = NULL,
      message = "No crosswalk needed for nested geographies"))
  }

  # Plan the crosswalk chain to determine if multi-step is needed
  plan <- plan_crosswalk_chain(
    source_geography = source_geography,
    target_geography = target_geography,
    source_year = source_year,
    target_year = target_year,
    weight = weight)

  # Check for planning errors
  if (!is.null(plan$error)) {
    stop(plan$error)
  }

  # Use get_crosswalk_chain for both single and multi-step
  # (it handles both cases and returns consistent structure)
  result <- get_crosswalk_chain(
    source_geography = source_geography,
    target_geography = target_geography,
    source_year = source_year,
    target_year = target_year,
    weight = weight,
    cache = cache)

  return(result)
}


#' Get a Single-Step Crosswalk (Internal)
#'
#' Internal function that retrieves a single crosswalk from the appropriate source.
#' This handles routing to Geocorr, NHGIS, or CTData based on the parameters.
#'
#' @inheritParams get_crosswalk
#' @return A tibble containing the crosswalk.
#' @keywords internal
#' @noRd
get_crosswalk_single <- function(
    source_geography,
    target_geography,
    source_year = NULL,
    target_year = NULL,
    weight = "population",
    cache = NULL) {

  # Convert years to character for consistent processing
  source_year_chr <- if (!is.null(source_year)) as.character(source_year) else NULL
  target_year_chr <- if (!is.null(target_year)) as.character(target_year) else NULL

  # Determine which source to use

  # Use Geocorr for: no years specified, or same year
  use_geocorr <- is.null(source_year) || is.null(target_year) ||
    (!is.null(source_year) && !is.null(target_year) && isTRUE(source_year == target_year))

  # Use CTData for 2020 <-> 2022 (Connecticut planning region changes)
  use_ctdata <- !is.null(source_year_chr) && !is.null(target_year_chr) &&
    ((source_year_chr == "2020" && target_year_chr == "2022") ||
     (source_year_chr == "2022" && target_year_chr == "2020"))

  if (use_geocorr) {
    crosswalk_source <- "geocorr"
  } else if (use_ctdata) {
    crosswalk_source <- "ctdata_2020_2022"
  } else {
    crosswalk_source <- "nhgis"
  }

  # Fetch the crosswalk from the appropriate source
  if (crosswalk_source == "ctdata_2020_2022") {
    result <- get_crosswalk_2020_2022(
      geography = source_geography,
      source_year = source_year,
      target_year = target_year,
      cache = cache)

  } else if (crosswalk_source == "nhgis") {
    result <- get_nhgis_crosswalk(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography,
      cache = cache)

  } else {
    geocorr_version <- determine_geocorr_version(target_year)
    result <- get_geocorr_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      weight = weight,
      cache = cache,
      geocorr_version = geocorr_version)
  }

  # Retrieve metadata from internal function (if present)
  internal_metadata <- attr(result, "crosswalk_metadata")

  # Build comprehensive metadata object
  metadata <- list(
    call_parameters = list(
      source_geography = source_geography,
      target_geography = target_geography,
      source_year = source_year_chr,
      target_year = target_year_chr,
      weight = weight,
      cache = cache),

    data_source = if (!is.null(internal_metadata$data_source)) {
      internal_metadata$data_source
    } else {
      crosswalk_source
    },
    data_source_full_name = if (!is.null(internal_metadata$data_source_full_name)) {
      internal_metadata$data_source_full_name
    } else {
      switch(crosswalk_source,
        "nhgis" = "IPUMS NHGIS (National Historical Geographic Information System)",
        "geocorr" = {
          geocorr_ver <- determine_geocorr_version(target_year)
          stringr::str_c("Geocorr ", get_geocorr_config(geocorr_ver)$reference_year,
                        " (Missouri Census Data Center)")
        },
        "ctdata_2020_2022" = "CT Data Collaborative",
        crosswalk_source)
    },

    download_url = internal_metadata$download_url,
    api_endpoint = internal_metadata$api_endpoint,
    documentation_url = internal_metadata$documentation_url,
    citation_url = internal_metadata$citation_url,
    github_repository = internal_metadata$github_repository,

    source_geography = source_geography,
    source_geography_standardized = internal_metadata$source_geography_standardized,
    target_geography = target_geography,
    target_geography_standardized = internal_metadata$target_geography_standardized,
    source_year = source_year_chr,
    target_year = target_year_chr,
    reference_year = internal_metadata$reference_year,

    weighting_variable = if (!is.null(internal_metadata$weighting_variable)) {
      internal_metadata$weighting_variable
    } else {
      weight
    },

    state_coverage = internal_metadata$state_coverage,
    notes = if (crosswalk_source == "ctdata_2020_2022") {
      c("Connecticut: CTData Collaborative 2020-2022 crosswalk",
        "Other states: No geographic changes between 2020 and 2022; use identity mapping",
        internal_metadata$notes)
    } else {
      internal_metadata$notes
    },

    retrieved_at = internal_metadata$retrieved_at,
    cached = internal_metadata$cached,
    cache_path = internal_metadata$cache_path,
    read_from_cache = internal_metadata$read_from_cache,

    is_multi_step = FALSE,
    crosswalk_package_version = as.character(utils::packageVersion("crosswalk")))

  attr(result, "crosswalk_metadata") <- metadata

  result <- result |>
    dplyr::mutate(
      dplyr::across(
        .cols = -allocation_factor_source_to_target,
        .fns = as.character),
      dplyr::across(
        .cols = allocation_factor_source_to_target,
        as.numeric))

  return(result)
}


#' Determine GeoCorr Version Based on Year
#'
#' Internal function that selects the appropriate GeoCorr version based on the
#' target year context. GeoCorr 2022 uses 2020 Census geography (for years >= 2020),
#' while GeoCorr 2018 uses 2010 Census geography (for years 2010-2019).
#'
#' @param year Numeric or NULL. The year to determine version for. If NULL,
#'    defaults to "2022".
#' @return Character. Either "2022" or "2018".
#' @keywords internal
#' @noRd
determine_geocorr_version <- function(year) {
  if (is.null(year)) {
    return("2022")
  }

  year_num <- as.numeric(year)

  if (year_num >= 2020) {
    return("2022")
  } else if (year_num >= 2010) {
    return("2018")
  } else {
    stop(
      "GeoCorr crosswalks are not available for years before 2010. ",
      "The requested year ", year, " is not supported. ",
      "GeoCorr 2018 covers 2010-2019 (2010 Census geography) and ",
      "GeoCorr 2022 covers 2020+ (2020 Census geography).")
  }
}


#' Get 2020 <-> 2022 Crosswalk (National)
#'
#' Internal function that handles the special case of 2020 to 2022 crosswalks
#' (and the reverse direction for identity crosswalks).
#' Returns a nationally comprehensive crosswalk with Connecticut data from
#' CT Data Collaborative (handling the planning region changes) and identity
#' mappings for all other states (where no changes occurred).
#'
#' @param geography Character. Geography type: one of "block", "block_group",
#'    "tract", or "county" (county only for 2020 -> 2022 direction).
#' @param source_year Numeric. Year of the source geography, either 2020 or 2022.
#' @param target_year Numeric. Year of the target geography, either 2020 or 2022.
#' @param cache Directory path for caching component crosswalks.
#'
#' @return A tibble containing the national crosswalk with Connecticut
#'    from CTData and identity mappings for other states.
#' @keywords internal
#' @noRd
get_crosswalk_2020_2022 <- function(geography, source_year = 2020, target_year = 2022, cache = NULL) {

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
"2020 <-> 2022 crosswalks are only available for blocks, block groups, tracts,
and counties. The provided geography '", geography, "' is not supported.")}

  # get_ctdata_crosswalk() now returns nationally comprehensive data
  result <- get_ctdata_crosswalk(
    geography = geography_standardized,
    source_year = source_year,
    target_year = target_year,
    cache = cache)

  return(result)
}

#' List All Available Crosswalk Combinations
#'
#' Returns a tibble of all source/target geography and year combinations
#' supported by `get_crosswalk()`.
#'
#' @return A tibble with columns: `source_geography`, `target_geography`,
#'   `source_year`, `target_year`.
#' @export
get_available_crosswalks <- function() {

  # 1. NHGIS: reuse list_nhgis_crosswalks(), select and coerce years to integer
  nhgis <- list_nhgis_crosswalks() |>
    dplyr::select(source_geography, target_geography, source_year, target_year) |>
    dplyr::mutate(
      source_year = as.integer(source_year),
      target_year = as.integer(target_year))

  # 2. Geocorr 2022: all pairwise combinations of 9 canonical geographies
  geocorr_2022_geographies <- c(
    "block", "block_group", "tract", "county", "place",
    "zcta", "puma22", "cd118", "cd119")

  geocorr_2022 <- tidyr::crossing(
    source_geography = geocorr_2022_geographies,
    target_geography = geocorr_2022_geographies) |>
    dplyr::filter(source_geography != target_geography) |>
    dplyr::mutate(
      source_year = 2022L,
      target_year = 2022L)

  # 3. Geocorr 2018: all pairwise combinations of 9 canonical geographies
  geocorr_2018_geographies <- c(
    "block", "block_group", "tract", "county", "place",
    "zcta", "puma12", "cd115", "cd116")

  geocorr_2018 <- tidyr::crossing(
    source_geography = geocorr_2018_geographies,
    target_geography = geocorr_2018_geographies) |>
    dplyr::filter(source_geography != target_geography) |>
    dplyr::mutate(
      source_year = 2018L,
      target_year = 2018L)

  # 4. CTData: 7 manually specified combinations (2020<->2022)
  ctdata <- tibble::tibble(
    source_geography = c("block", "block_group", "tract", "county",
                         "block", "block_group", "tract"),
    target_geography = c("block", "block_group", "tract", "county",
                         "block", "block_group", "tract"),
    source_year = c(rep(2020L, 4), rep(2022L, 3)),
    target_year = c(rep(2022L, 4), rep(2020L, 3)))

  # 5. Combine, deduplicate, and sort
  dplyr::bind_rows(nhgis, geocorr_2022, geocorr_2018, ctdata) |>
    dplyr::distinct() |>
    dplyr::arrange(source_geography, target_geography, source_year, target_year)
}

utils::globalVariables(c(
  "allocation_factor_source_to_target", "geoid", "label",
  "n_unmatched", "pct_of_unmatched", "state_abbr"))