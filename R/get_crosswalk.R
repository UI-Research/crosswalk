#' Get a crosswalk(s) to translate data across time and geographies
#'
#' Retrieves a crosswalk with interpolation values from a source geography to a target
#' geography, optionally across different years. Always returns a list with a consistent
#' structure containing one or more crosswalk tibbles.
#'
#' @details This function sources crosswalks from Geocorr 2022, Geocorr 2018,
#'    IPUMS NHGIS, CT Data Collaborative, and a curated registry of county
#'    change events. Crosswalk weights are from the original sources and have
#'    not been modified; this function merely standardizes the format of the
#'    returned crosswalks and enables easy programmatic access and caching.
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
#'    **Arbitrary-year county crosswalks (county change events)**: Counties and
#'    county equivalents are occasionally redefined outside decennial censuses
#'    (e.g., the 2013 Bedford, VA merge; the 2015 Shannon -> Oglala Lakota, SD
#'    FIPS change; the Valdez-Cordova, AK split, first reflected in 2020-vintage
#'    products; the 2022 Connecticut planning regions). County -> county
#'    crosswalks are therefore available between ANY pair of years from 2000
#'    through the curated horizon (e.g., 2014 -> 2019 or 2014 -> 2023), built
#'    from an identity mapping plus the documented change events in the
#'    interval, with population-based allocation factors for splits and
#'    part transfers. County crosswalks are forward-only (source_year <
#'    target_year). For sub-county geographies (tract, block group, block)
#'    affected by these county changes, same-decade crosswalks are available
#'    from 2010 onward (e.g., tract 2014 -> 2019); year pairs spanning a
#'    decennial census are planned automatically as chains through NHGIS
#'    crosswalks (e.g., tract 2014 -> 2023).
#'
#'    **Nested geographies**: When the source geography nests exactly within
#'    the target geography and the years match (e.g., tract -> county, block
#'    group -> tract), no crosswalk is needed: every source unit belongs to
#'    exactly one target unit. In this case the function warns and returns the
#'    standard list structure with an *empty* crosswalk tibble -- aggregate
#'    your data directly instead (e.g., sum tract values by the first five
#'    GEOID characters to obtain county values).
#'
#'    **Caching**: When `cache` is a directory path, each component crosswalk
#'    is written there as a CSV, and subsequent calls with the same parameters
#'    read the file from disk instead of re-downloading. Cached files never
#'    expire; delete a file to force a fresh download. The metadata attribute
#'    records `cache_path` and whether the crosswalk was `read_from_cache`.
#'
#'    Note that an IPUMS NHGIS API key is required to access crosswalks from that
#'    source. Use `usethis::edit_r_environ(scope = "user")` to save your API key
#'    to your .Renviron; the name of the key should be "IPUMS_API_KEY". You can
#'    obtain a key from: https://account.ipums.org/api_keys.
#'
#' @param source_year Character or numeric. Year of the source geography, one of
#'    c(1990, 2000, 2010, 2020) for decennial years, or c(2011, 2012, 2014, 2015,
#'    2022) for non-census years (limited to block groups, tracts, and counties).
#'    For county -> county crosswalks, any year from 2000 onward is supported;
#'    for same-decade tract/block group/block crosswalks, any year from 2010
#'    onward (see Details).
#' @param source_geography Character. Source geography name. For same-year
#'    (GeoCorr) crosswalks, one of c("block", "block group", "tract", "county",
#'    "place", "zcta", "puma", "puma12", "puma22", "cd115", "cd116", "cd118",
#'    "cd119"). For cross-decade (NHGIS) crosswalks, sources are limited to
#'    c("block", "block group", "tract"). Use `get_available_crosswalks()` for
#'    the full listing of supported combinations.
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020) for decennial crosswalks, or c(2011, 2012, 2014,
#'    2015, 2022) for non-census year crosswalks (limited to block groups, tracts,
#'    and counties). For county -> county crosswalks, any year from 2000 onward
#'    is supported; for same-decade tract/block group/block crosswalks, any year
#'    from 2010 onward (see Details).
#' @param target_geography Character. Target geography name. For same-year
#'    (GeoCorr) crosswalks, one of c("block", "block group", "tract", "county",
#'    "place", "zcta", "puma", "puma12", "puma22", "cd115", "cd116", "cd118",
#'    "cd119"). For cross-decade (NHGIS) crosswalks, targets additionally
#'    include c("urban_area", "core_based_statistical_area"). Use
#'    `get_available_crosswalks()` for the full listing.
#' @param weight Character. Weighting variable used to construct or select
#'    allocation factors. One of c("population", "housing", "land"). Applies to
#'    GeoCorr crosswalks and to the selection of the NHGIS interpolation weight
#'    (block-based NHGIS crosswalks provide a single combined weight, which is
#'    used regardless of this argument).
#' @param cache Directory path. Where to save the crosswalk. If NULL (default),
#'    the crosswalk is returned but not saved to disk. When provided, individual
#'    component crosswalks are cached separately, and subsequent calls with the
#'    same parameters read from the cached files instead of re-downloading
#'    (see Details).
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
#'    For nested same-year requests (e.g., tract -> county), `crosswalks$step_1`
#'    is an empty tibble and a warning explains that no crosswalk is needed
#'    (see Details).
#'
#'    Each crosswalk tibble includes an attribute `crosswalk_metadata` (access via
#'    `attr(result$crosswalks$step_1, "crosswalk_metadata")`) documenting how the
#'    crosswalk was produced; see the "Crosswalk metadata" section.
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
#' @section Crosswalk metadata:
#' The `crosswalk_metadata` attribute attached to each crosswalk tibble is a
#' list recording the crosswalk's provenance. Fields with no value for a given
#' source are NULL. Elements include:
#' \describe{
#'   \item{call_parameters}{The arguments the crosswalk was requested with
#'      (`source_geography`, `target_geography`, `source_year`, `target_year`,
#'      `weight`, `cache`).}
#'   \item{data_source, data_source_full_name}{Short code and full name of the
#'      source that produced the crosswalk (e.g., "geocorr", "Geocorr 2022
#'      (Missouri Census Data Center)").}
#'   \item{download_url, api_endpoint, documentation_url, citation_url,
#'      github_repository}{Links to the underlying data, its documentation, and
#'      citation guidance, where applicable.}
#'   \item{source_geography, target_geography, source_year, target_year}{The
#'      requested geographies and years, plus `source_geography_standardized` /
#'      `target_geography_standardized` (the source-specific codes they resolved
#'      to) and `reference_year` (the source's reference vintage, e.g., the
#'      GeoCorr version).}
#'   \item{weighting_variable}{The weighting variable used to construct or
#'      select allocation factors.}
#'   \item{state_coverage, notes}{Geographic coverage notes and source-specific
#'      caveats.}
#'   \item{retrieved_at, cached, cache_path, read_from_cache}{When the
#'      crosswalk was retrieved, and whether it was written to or read from a
#'      local cache.}
#'   \item{is_multi_step, crosswalk_package_version}{Whether the crosswalk is
#'      part of a multi-step chain, and the package version that produced it.}
#' }
#'
#' @seealso [crosswalk_data()] to apply the returned crosswalk(s) to data;
#'   [get_available_crosswalks()] for all supported geography/year
#'   combinations; [list_nhgis_crosswalks()] for the NHGIS-specific listing.
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
#'
#' # Arbitrary-year county crosswalk handling non-decennial county changes
#' # (e.g., 2015 Shannon -> Oglala Lakota, SD; Valdez-Cordova, AK split)
#' result <- get_crosswalk(
#'   source_geography = "county",
#'   target_geography = "county",
#'   source_year = 2014,
#'   target_year = 2023)
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

  validate_cache_dir(cache)

  # Check for nested geographies (no crosswalk needed)
  # Determine if years match (both NULL, or both non-NULL and equal)
  years_match <- (is.null(source_year) && is.null(target_year)) ||
    (!is.null(source_year) && !is.null(target_year) && isTRUE(source_year == target_year))

  # Standardize names so aliases (e.g. "blockgroup", "bg") are detected too
  source_geog_nested <- standardize_geography_for_chain(source_geography)
  target_geog_nested <- standardize_geography_for_chain(target_geography)

  is_nested <- (source_geog_nested == "block" && target_geog_nested %in% c("block_group", "tract", "county", "cbsa")) ||
    (source_geog_nested == "block_group" && target_geog_nested %in% c("tract", "county", "cbsa")) ||
    (source_geog_nested == "tract" && target_geog_nested %in% c("county", "cbsa")) ||
    (source_geog_nested == "county" && target_geog_nested == "cbsa")

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
#' This handles routing to Geocorr, NHGIS, CTData, or the county-events engine
#' based on the parameters.
#'
#' @inheritParams get_crosswalk
#' @param crosswalk_source Character or NULL. The crosswalk source determined
#'   by the chain planner ("geocorr", "nhgis", "ctdata_2020_2022", or
#'   "county_events"). When NULL, the source is derived from the other
#'   parameters.
#' @return A tibble containing the crosswalk.
#' @keywords internal
#' @noRd
get_crosswalk_single <- function(
    source_geography,
    target_geography,
    source_year = NULL,
    target_year = NULL,
    weight = "population",
    cache = NULL,
    crosswalk_source = NULL) {

  # Convert years to character for consistent processing
  source_year_chr <- if (!is.null(source_year)) as.character(source_year) else NULL
  target_year_chr <- if (!is.null(target_year)) as.character(target_year) else NULL

  # Year context for GeoCorr version selection: prefer the target year, but
  # fall back to the source year when only the source year is provided
  geocorr_year_context <- if (!is.null(target_year)) target_year else source_year

  # Determine which source to use when the planner did not specify one
  if (is.null(crosswalk_source)) {

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
      # Same-geography temporal requests that NHGIS cannot serve fall through
      # to the county-events engine when it covers the geography and years
      events_geog <- standardize_geography_for_events(source_geography)
      use_county_events <- !is.na(events_geog) &&
        isTRUE(events_geog == standardize_geography_for_events(target_geography)) &&
        !is_nhgis_crosswalk_available(
          source_geography, target_geography, source_year, target_year) &&
        min(as.numeric(source_year), as.numeric(target_year)) >= 2000 &&
        max(as.numeric(source_year), as.numeric(target_year)) <=
          county_events_meta$known_through_year

      crosswalk_source <- if (use_county_events) "county_events" else "nhgis"
    }
  }

  # Fetch the crosswalk from the appropriate source
  if (crosswalk_source == "ctdata_2020_2022") {
    result <- get_crosswalk_2020_2022(
      geography = source_geography,
      source_year = source_year,
      target_year = target_year,
      cache = cache)

  } else if (crosswalk_source == "county_events") {
    result <- get_county_events_crosswalk(
      geography = source_geography,
      source_year = source_year,
      target_year = target_year,
      cache = cache)

    if (weight != "population") {
      cw_message(stringr::str_c(
        "County-events crosswalks use population-derived allocation factors ",
        "for changed counties; the requested '", weight,
        "' weight does not apply to this step."))
    }

  } else if (crosswalk_source == "nhgis") {
    result <- get_nhgis_crosswalk(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography,
      cache = cache)

    # NHGIS crosswalks are long format, with one row per source-target pair
    # per interpolation weight. Keep only the weight matching the `weight`
    # argument so each pair appears exactly once; otherwise downstream
    # interpolation would multiply-count values. Candidates are ordered by
    # preference: block-based crosswalks provide a single combined
    # housing/population weight ("housing_population") rather than separate
    # population and housing weights.
    if (nrow(result) > 0 && "weighting_factor" %in% names(result)) {
      nhgis_weight_map <- list(
        "population" = c("population", "housing_population"),
        "housing" = c("housing_all", "housing_population"),
        "land" = c("landarea"))

      if (!weight %in% names(nhgis_weight_map)) {
        stop(
          "Invalid weight: '", weight, "'. Must be one of: ",
          paste(names(nhgis_weight_map), collapse = ", "), ".")
      }

      available_weights <- unique(result$weighting_factor)
      candidate_weights <- intersect(nhgis_weight_map[[weight]], available_weights)

      if (length(candidate_weights) > 0) {
        selected_weight <- candidate_weights[1]
        if (selected_weight != nhgis_weight_map[[weight]][1]) {
          cw_message(stringr::str_c(
            "This NHGIS crosswalk does not provide a separate '", weight,
            "' weight; using the combined '", selected_weight,
            "' interpolation weight instead."))
        }
        result <- result |>
          dplyr::filter(weighting_factor == selected_weight)
      } else if (length(available_weights) == 1) {
        cw_message(stringr::str_c(
          "This NHGIS crosswalk provides a single interpolation weight ('",
          available_weights,
          "'), which is used regardless of the requested weight."))
      } else {
        stop(
          "The requested weight '", weight, "' is not available for this ",
          "NHGIS crosswalk. Available weighting factors: ",
          paste(available_weights, collapse = ", "), ".")
      }
    }

  } else {
    geocorr_version <- determine_geocorr_version(geocorr_year_context)
    result <- get_geocorr_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      weight = weight,
      cache = cache,
      geocorr_version = geocorr_version)
  }

  # If the internal function returned an empty tibble (e.g., failed download),
  # return early with a warning

  if (ncol(result) == 0 || nrow(result) == 0) {
    warning(
      "No crosswalk data was returned for ",
      source_geography, " ", source_year, " -> ",
      target_geography, " ", target_year,
      ". The download may have failed. Check your IPUMS_API_KEY and network connection.")
    return(list(
      crosswalks = list(step_1 = tibble::tibble()),
      plan = NULL,
      message = "Crosswalk retrieval failed. No data returned."))
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
          geocorr_ver <- determine_geocorr_version(geocorr_year_context)
          stringr::str_c("Geocorr ", get_geocorr_config(geocorr_ver)$reference_year,
                        " (Missouri Census Data Center)")
        },
        "ctdata_2020_2022" = "CT Data Collaborative",
        "county_events" = "U.S. Census Bureau county boundary change records (curated in the crosswalk package)",
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

  # Standardize types: GEOIDs, names, and years as character; allocation
  # factors and overlap measures as numeric
  crosswalk_numeric_columns <- c(
    "allocation_factor_source_to_target", "allocation_factor_target_to_source",
    "population_2020", "housing_2020", "population_2010", "housing_2010",
    "land_area_sqmi")

  result <- result |>
    dplyr::mutate(
      dplyr::across(
        .cols = -dplyr::any_of(crosswalk_numeric_columns),
        .fns = as.character),
      dplyr::across(
        .cols = dplyr::any_of(crosswalk_numeric_columns),
        .fns = as.numeric))

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
#' supported by `get_crosswalk()`, along with the crosswalk source that serves
#' each combination.
#'
#' @details
#' **How to read the year columns.** Years identify the geography *vintages* a
#' crosswalk translates between, not the only request years `get_crosswalk()`
#' accepts:
#' - **GeoCorr** (same-year geography changes) rows are listed under their
#'   reference years: 2022 for 2020-Census geography and 2018 for 2010-Census
#'   geography. Requests with other year contexts resolve to the matching
#'   version -- years 2020 and later (or no years at all) use GeoCorr 2022,
#'   and years 2010-2019 use GeoCorr 2018. For example, a request for 2021
#'   tracts -> 2021 ZCTAs is served by the tract -> zcta 2022 row.
#' - **County-events** rows enumerate every supported year pair explicitly
#'   (county -> county: any forward pair from 2000 on; tract, block group, and
#'   block: same-decade pairs from 2010 on, including exactly reversible
#'   relabels).
#'
#' **Nested geographies.** Same-year pairs in which the source geography nests
#' within the target (e.g., tract -> county) are listed because GeoCorr serves
#' them, but `get_crosswalk()` returns an empty crosswalk with a warning for
#' these: no crosswalk is needed -- aggregate your data directly instead (e.g.,
#' tracts nest within counties, so tract values can be summed by the first five
#' GEOID characters).
#'
#' **API keys by source.** The `crosswalk_source` column indicates which source
#' `get_crosswalk()` uses for a direct request of that combination (multi-step
#' chains may combine several):
#' - `geocorr`: no API key required
#' - `nhgis`: requires `IPUMS_API_KEY` (see [get_crosswalk()])
#' - `ctdata_2020_2022`: county requests require a Census API key
#'   (`CENSUS_API_KEY`) and the `tidycensus` package; sub-county requests
#'   require neither
#' - `county_events`: no API key; the data ships with the package and works
#'   offline
#'
#' @return A tibble with columns: `source_geography`, `target_geography`,
#'   `source_year`, `target_year`, `crosswalk_source`.
#' @seealso [get_crosswalk()] to fetch a crosswalk; [list_nhgis_crosswalks()]
#'   for the underlying NHGIS listing.
#' @export
#' @examples
#' available <- get_available_crosswalks()
#'
#' # All combinations that translate data into 2020 tracts
#' available |>
#'   dplyr::filter(target_geography == "tract", target_year == 2020)
#'
#' # County -> county crosswalks support arbitrary year pairs from 2000 on
#' available |>
#'   dplyr::filter(source_geography == "county", target_geography == "county")
get_available_crosswalks <- function() {

  # 1. NHGIS: reuse list_nhgis_crosswalks(), select and coerce years to integer
  nhgis <- list_nhgis_crosswalks() |>
    dplyr::select(source_geography, target_geography, source_year, target_year) |>
    dplyr::mutate(
      source_year = as.integer(source_year),
      target_year = as.integer(target_year),
      crosswalk_source = "nhgis")

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
      target_year = 2022L,
      crosswalk_source = "geocorr")

  # aiannh (tribal areas) is supported as a target geography only, via GeoCorr 2022
  geocorr_2022_aiannh <- tibble::tibble(
    source_geography = geocorr_2022_geographies,
    target_geography = "aiannh",
    source_year = 2022L,
    target_year = 2022L)

  geocorr_2022 <- dplyr::bind_rows(geocorr_2022, geocorr_2022_aiannh)

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
      target_year = 2018L,
      crosswalk_source = "geocorr")

  # 4. CTData: 7 manually specified combinations (2020<->2022)
  ctdata <- tibble::tibble(
    source_geography = c("block", "block_group", "tract", "county",
                         "block", "block_group", "tract"),
    target_geography = c("block", "block_group", "tract", "county",
                         "block", "block_group", "tract"),
    source_year = c(rep(2020L, 4), rep(2022L, 3)),
    target_year = c(rep(2022L, 4), rep(2020L, 3)),
    crosswalk_source = "ctdata_2020_2022")

  # 5. County events: same-geography temporal combinations served by the
  # curated county-change engine (shares list_county_events_edges() with the
  # chain planner so the registry cannot drift from routing)
  county_events_combos <- purrr::map_dfr(
    c("county", "tract", "block_group", "block"),
    \(geog) list_county_events_edges(geog) |>
      dplyr::mutate(
        source_geography = geog,
        target_geography = geog,
        source_year = as.integer(source_year),
        target_year = as.integer(target_year),
        crosswalk_source = "county_events") |>
      dplyr::select(source_geography, target_geography, source_year,
                    target_year, crosswalk_source))

  # 6. Combine, deduplicate, and sort. The 2020 <-> 2022 pairs can be served by
  # both CTData and the county-events engine; keep the source the planner
  # routes direct requests to (CTData), mirroring get_crosswalk()'s behavior
  source_preference <- c(
    "geocorr" = 1L, "nhgis" = 2L, "ctdata_2020_2022" = 3L, "county_events" = 4L)

  dplyr::bind_rows(nhgis, geocorr_2022, geocorr_2018, ctdata, county_events_combos) |>
    dplyr::distinct() |>
    dplyr::slice_min(
      source_preference[crosswalk_source],
      by = c(source_geography, target_geography, source_year, target_year),
      with_ties = FALSE) |>
    dplyr::arrange(source_geography, target_geography, source_year, target_year)
}

utils::globalVariables(c(
  "allocation_factor_source_to_target", "geoid", "label",
  "n_unmatched", "pct_of_unmatched", "state_abbr",
  "source_geography", "target_geography", "crosswalk_source"))