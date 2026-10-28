#' Get a crosswalk(s) to translate data across time and geographies
#'
#' Retrieves a crosswalk with interpolation values from a source geography to a target
#' geography, optionally across different years. Always returns a list with a consistent
#' structure containing one or more crosswalk tibbles.
#'
#' @details This function sources crosswalks from Geocorr 2022, Geocorr 2018,
#'    Geocorr 2000, IPUMS NHGIS, CT Data Collaborative, the Longitudinal Tract
#'    Database (LTDB, Brown University), and a curated registry of county
#'    change events. Crosswalk weights are from the original sources and have
#'    not been modified; this function merely standardizes the format of the
#'    returned crosswalks and enables easy programmatic access and caching.
#'
#'    **GeoCorr version selection**: For same-year geography crosswalks, the
#'    appropriate GeoCorr version is selected automatically based on the year:
#'    - Years 2020+ (or no year specified): GeoCorr 2022 (2020 Census geography)
#'    - Years 2010-2019: GeoCorr 2018 (2010 Census geography)
#'    - Years 2000-2009: GeoCorr 2000 (2000 Census geography)
#'
#'    Years before 2000 are not supported for same-year crosswalks.
#'
#'    **Geography name resolution**: User-facing geography names like "puma",
#'    "zcta", "place", and "blockgroup" are automatically resolved to the correct
#'    API codes for the selected GeoCorr version. Version-specific names are also
#'    accepted (e.g., "puma5" for GeoCorr 2000, "puma12" for GeoCorr 2018,
#'    "puma22" for GeoCorr 2022).
#'
#'    **2000 geography (GeoCorr 2000)**: Request 2000-vintage crosswalks with
#'    `source_year = 2000, target_year = 2000` (e.g., 2000 tracts to 2000
#'    PUMAs, which NHGIS does not provide directly). Supported geographies are
#'    "block", "block group", "tract", "county", "place", "zcta", "puma" (or
#'    "puma5": the PUMA for the 2000 5% sample, which 2005-2011 ACS microdata
#'    also use), and "msacmsa" (1999 Metropolitan Statistical Areas and
#'    Consolidated Metropolitan Statistical Areas, identified by four-digit
#'    codes, with "9999" for areas outside any metro area). PUMAs for the 2000
#'    1% sample ("puma1") are not available. GeoCorr 2000 has no Puerto Rico
#'    data. GeoCorr 2000 leaves the MSA/CMSA code blank for New England areas,
#'    where 1999 metro areas were built from cities and towns; those rows are
#'    dropped with a warning. Years 2001-2009 also use GeoCorr 2000, so they
#'    reflect 2000 boundaries, not later changes such as the creation of
#'    Broomfield County, Colorado.
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
#'    **Tracts back to 2000 boundaries (LTDB)**: NHGIS does not publish any
#'    crosswalk that targets 2000 geography. For tract 2010 -> tract 2000,
#'    the function uses the "Backwards LTDB" crosswalk from the Longitudinal
#'    Tract Database at Brown University, downloaded without registration from
#'    <https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx>.
#'    Later tract years are planned as chains that end with this step (e.g.,
#'    tract 2020 -> 2000 plans NHGIS 2020 -> 2010, then LTDB 2010 -> 2000).
#'    LTDB weights differ from NHGIS weights: LTDB provides one weight per
#'    tract pair, built from a combination of land area and census block
#'    population, while NHGIS builds its weights from census blocks with a
#'    choice of population, household, or housing unit counts. Because LTDB
#'    has only that one weight, requests that use LTDB must use
#'    `weight = "population"` (the default); other weights produce an error.
#'    Changing geography in 2000 (e.g., tract 2010 -> ZCTA 2000) is not
#'    supported, because GeoCorr covers only 2010 and later geography. Use of
#'    LTDB crosswalks requires citing the US2010 Project webpage as the source
#'    of data and Logan, Xu, and Stults (2014) as the technical reference; see
#'    <https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx>.
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
#'    "cd119"), plus c("puma5", "msacmsa") for 2000 geography. For cross-decade
#'    (NHGIS) crosswalks, sources are limited to
#'    c("block", "block group", "tract"). Use `get_available_crosswalks()` for
#'    the full listing of supported combinations.
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020) for decennial crosswalks, or c(2011, 2012, 2014,
#'    2015, 2022) for non-census year crosswalks (limited to block groups, tracts,
#'    and counties). For county -> county crosswalks, any year from 2000 onward
#'    is supported; for same-decade tract/block group/block crosswalks, any year
#'    from 2010 onward (see Details). Tracts can also target 2000 from 2010 or
#'    later tract years, using the LTDB crosswalk (see Details).
#' @param target_geography Character. Target geography name. For same-year
#'    (GeoCorr) crosswalks, one of c("block", "block group", "tract", "county",
#'    "place", "zcta", "puma", "puma12", "puma22", "cd115", "cd116", "cd118",
#'    "cd119"), plus c("puma5", "msacmsa") for 2000 geography. For cross-decade
#'    (NHGIS) crosswalks, targets additionally
#'    include c("urban_area", "core_based_statistical_area"). Use
#'    `get_available_crosswalks()` for the full listing.
#' @param weight Character. Weighting variable used to construct or select
#'    allocation factors. Defaults to "population". The accepted values depend
#'    on the source that serves each step (see "Available weights" below):
#'    GeoCorr accepts c("population", "housing", "land"); NHGIS additionally
#'    accepts c("household", "housing_owned", "housing_rented") for block group
#'    and tract based crosswalks; LTDB (tracts to 2000) accepts only
#'    "population". A weight that a step cannot provide raises an error,
#'    except as described under "Available weights".
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
#'     \item{population_2020}{The estimated overlap in population, if applicable
#'        (`population_2010` or `population_2000` for GeoCorr 2018 or GeoCorr
#'        2000)}
#'     \item{housing_2020}{The estimated overlap in housing units, if applicable
#'        (`housing_2010` or `housing_2000` for GeoCorr 2018 or GeoCorr 2000)}
#'     \item{land_area_sqmi}{The overlap in land area, if applicable}
#'     \item{weighting_factor}{The attribute used to calculate allocation factors}
#'     \item{state_fips}{Two-digit state FIPS code, if applicable}
#'   }
#'
#' @section Available weights:
#' The `weight` argument chooses the attribute used to split a source unit
#' across the target units it overlaps. What each source offers:
#' \describe{
#'   \item{GeoCorr (same-year crosswalks)}{"population", "housing" (all
#'      housing units), and "land" (land area). Requesting any other weight
#'      raises an error that lists these three.}
#'   \item{NHGIS, block group and tract based files}{"population",
#'      "housing" (all housing units, NHGIS `wt_hu`), "household"
#'      (households, NHGIS `wt_hh`; use for household-level measures such as
#'      HUD CHAS data), "housing_owned" (owner-occupied housing units, NHGIS
#'      `wt_ownhu`), and "housing_rented" (renter-occupied housing units,
#'      NHGIS `wt_renthu`).}
#'   \item{NHGIS, block-based files}{A single combined weight, recorded as
#'      `weighting_factor = "housing_population"`, which is used (with a
#'      message) when "population" or "housing" is requested. Some files also
#'      carry a land-area weight ("land"). Requesting "household",
#'      "housing_owned", or "housing_rented" raises an error that lists the
#'      weights the file provides; another weight is never substituted.}
#'   \item{CT Data Collaborative (2020 <-> 2022) and county-events
#'      crosswalks}{No choice of weight. Units whose boundaries did not
#'      change have an allocation factor of 1 (`weighting_factor =
#'      "identity"`); units whose boundaries changed use population-based
#'      allocation factors. Any accepted `weight` is allowed for these steps;
#'      a message notes when population-based factors are used instead of
#'      the requested weight.}
#'   \item{LTDB (tract 2010 -> 2000)}{A single weight that combines land area
#'      and census block population. Only "population" is accepted; any other
#'      weight raises an error before anything is downloaded.}
#' }
#' In a multi-step chain, every step must be able to use the requested
#' weight. For example, `weight = "household"` cannot be used for 2010 tracts
#' to 2020 ZCTAs, because the second step (2020 tracts to 2020 ZCTAs) comes from
#' GeoCorr; the error names that step. The check runs before any crosswalk is
#' downloaded. The `weighting_factor` column and the `weighting_variable`
#' metadata field record the weight actually used.
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
#'   \item{weighting_variable}{The weighting variable actually used to
#'      construct or select allocation factors. For GeoCorr this is the
#'      requested `weight`. For NHGIS it is the `weighting_factor` value kept
#'      from the file (e.g., "household", "housing_all", or
#'      "housing_population"). For CT Data Collaborative and county-events
#'      crosswalks it lists the weighting factors present (e.g., "identity,
#'      population").}
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
#' # Same-year crosswalk for 2000 geography (uses Geocorr 2000), e.g., to
#' # aggregate 2000 Census tract data to 2000 PUMAs
#' result <- get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "puma",
#'   source_year = 2000,
#'   target_year = 2000,
#'   weight = "population")
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
#'   by the chain planner ("geocorr", "nhgis", "ctdata_2020_2022",
#'   "county_events", or "ltdb"). When NULL, the source is derived from the other
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

      # Tract year pairs that NHGIS does not publish but the Longitudinal
      # Tract Database (LTDB) does (2010 -> 2000)
      ltdb_source_geography <- standardize_geography_for_chain(source_geography)
      ltdb_target_geography <- standardize_geography_for_chain(target_geography)
      use_ltdb <- !use_county_events &&
        !is_nhgis_crosswalk_available(
          source_geography, target_geography, source_year, target_year) &&
        nrow(dplyr::filter(
          list_ltdb_crosswalks(),
          source_geography == ltdb_source_geography,
          target_geography == ltdb_target_geography,
          source_year == source_year_chr,
          target_year == target_year_chr)) > 0

      crosswalk_source <- if (use_county_events) {
        "county_events"
      } else if (use_ltdb) {
        "ltdb"
      } else {
        "nhgis"
      }
    }
  }

  # The weighting factor actually used to build this crosswalk's allocation
  # factors, recorded in the metadata. NULL means the requested `weight` was
  # used as given (GeoCorr).
  weighting_variable_used <- NULL

  # Fetch the crosswalk from the appropriate source
  if (crosswalk_source %in% c("ctdata_2020_2022", "county_events")) {
    result <- if (crosswalk_source == "ctdata_2020_2022") {
      get_crosswalk_2020_2022(
        geography = source_geography,
        source_year = source_year,
        target_year = target_year,
        cache = cache)
    } else {
      get_county_events_crosswalk(
        geography = source_geography,
        source_year = source_year,
        target_year = target_year,
        cache = cache)
    }

    # These sources do not offer a choice of weight: areas whose boundaries
    # did not change get an allocation factor of 1 ("identity"), and areas
    # whose boundaries changed use population-based allocation factors.
    # Record what was used, and tell the user when it differs from the
    # requested weight.
    if (nrow(result) > 0 && "weighting_factor" %in% names(result)) {
      weighting_factors_used <- sort(unique(result$weighting_factor))
      weighting_variable_used <- stringr::str_c(weighting_factors_used, collapse = ", ")
      non_identity_factors_used <- setdiff(weighting_factors_used, "identity")

      if (length(non_identity_factors_used) > 0 &&
          !all(non_identity_factors_used == weight)) {
        cw_message(stringr::str_c(
          "The ", source_year, " -> ", target_year, " ", source_geography,
          " crosswalk uses ",
          stringr::str_c(non_identity_factors_used, collapse = ", "),
          "-based allocation factors for areas whose boundaries changed. The ",
          "requested '", weight, "' weight is not offered by this source and ",
          "does not apply to this step."))
      }
    }

  } else if (crosswalk_source == "ltdb") {
    # LTDB has a single combined weight. The planner already rejects other
    # weights; check again here for direct internal calls.
    if (!identical(weight, "population")) {
      stop(
        "The Longitudinal Tract Database (LTDB) crosswalk provides only one ",
        "weight, which combines land area and census block population. The ",
        "requested weight '", weight, "' is not available. Use ",
        "weight = \"population\" to accept the LTDB weight.",
        call. = FALSE)
    }

    result <- get_ltdb_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      source_year = source_year,
      target_year = target_year,
      cache = cache)

  } else if (crosswalk_source == "nhgis") {
    nhgis_crosswalk <- get_nhgis_crosswalk(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography,
      cache = cache)

    # NHGIS crosswalks are long format, with one row per source-target pair
    # per interpolation weight. Keep only the weight matching the `weight`
    # argument so each pair appears exactly once; otherwise downstream
    # interpolation would count values more than once. See
    # get_nhgis_weight_map() for the order of preference among candidates.
    if (nrow(nhgis_crosswalk) > 0 && "weighting_factor" %in% names(nhgis_crosswalk)) {
      nhgis_weight_map <- get_nhgis_weight_map()

      if (!weight %in% names(nhgis_weight_map)) {
        stop(
          "Invalid weight: '", weight, "'. Must be one of: ",
          paste(names(nhgis_weight_map), collapse = ", "), ".")
      }

      available_weights <- unique(nhgis_crosswalk$weighting_factor)
      candidate_weights <- intersect(nhgis_weight_map[[weight]], available_weights)

      # The `weight` values that this particular NHGIS file can provide
      usable_weight_arguments <- names(nhgis_weight_map)[
        purrr::map_lgl(nhgis_weight_map, \(factors) any(factors %in% available_weights))]

      if (length(candidate_weights) > 0) {
        weighting_variable_used <- candidate_weights[1]
        if (weighting_variable_used != nhgis_weight_map[[weight]][1]) {
          cw_message(stringr::str_c(
            "This NHGIS crosswalk does not provide a separate '", weight,
            "' weight; using the combined '", weighting_variable_used,
            "' interpolation weight instead."))
        }
        result <- nhgis_crosswalk |>
          dplyr::filter(weighting_factor == weighting_variable_used)
      } else if (length(available_weights) == 1 && weight %in% get_geocorr_weights()) {
        weighting_variable_used <- available_weights
        cw_message(stringr::str_c(
          "This NHGIS crosswalk provides a single interpolation weight ('",
          available_weights,
          "'), which is used regardless of the requested weight."))
        result <- nhgis_crosswalk
      } else {
        # Household and tenure weights are never replaced by another weight:
        # the caller asked for them specifically, so substituting a different
        # weight would give misleading results.
        stop(
          "The requested weight '", weight, "' is not available for the NHGIS ",
          source_geography, " ", source_year, " -> ", target_geography, " ",
          target_year, " crosswalk. This crosswalk provides these weighting ",
          "factors: ", paste(sort(available_weights), collapse = ", "),
          ". Use one of these values for `weight`: ",
          paste(usable_weight_arguments, collapse = ", "), ".",
          call. = FALSE)
      }
    } else {
      result <- nhgis_crosswalk
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
        "ltdb" = "Longitudinal Tract Database (LTDB), Brown University",
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
    } else if (!is.null(weighting_variable_used)) {
      weighting_variable_used
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
    "population_2000", "housing_2000", "land_area_sqmi")

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
#' GeoCorr 2018 uses 2010 Census geography (for years 2010-2019), and GeoCorr
#' 2000 uses 2000 Census geography (for years 2000-2009).
#'
#' @param year Numeric or NULL. The year to determine version for. If NULL,
#'    defaults to "2022".
#' @return Character. One of "2022", "2018", or "2000".
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
  } else if (year_num >= 2000) {
    return("2000")
  } else {
    stop(
      "GeoCorr crosswalks are not available for years before 2000. ",
      "The requested year ", year, " is not supported. ",
      "GeoCorr 2000 covers 2000-2009 (2000 Census geography), ",
      "GeoCorr 2018 covers 2010-2019 (2010 Census geography), and ",
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
#'   reference years: 2022 for 2020-Census geography, 2018 for 2010-Census
#'   geography, and 2000 for 2000-Census geography. Requests with other year
#'   contexts resolve to the matching version -- years 2020 and later (or no
#'   years at all) use GeoCorr 2022, years 2010-2019 use GeoCorr 2018, and
#'   years 2000-2009 use GeoCorr 2000. For example, a request for 2021
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
#' - `ltdb`: no API key; downloaded from the Longitudinal Tract Database
#'   (Brown University). Used only for tract 2010 -> 2000, which NHGIS does
#'   not publish. LTDB weights combine land area and block population and
#'   differ from NHGIS weights (see [get_crosswalk()])
#'
#' **Weights by source.** The `weight` argument of [get_crosswalk()] accepts
#' different values depending on `crosswalk_source` (see the "Available
#' weights" section of [get_crosswalk()] for details):
#' - `geocorr`: "population", "housing", "land"
#' - `nhgis`: block group and tract based crosswalks offer "population",
#'   "housing", "household", "housing_owned", and "housing_rented";
#'   block-based crosswalks offer a single combined population and housing
#'   weight (used for "population" or "housing") and, for some files, "land"
#' - `ctdata_2020_2022` and `county_events`: no choice of weight; unchanged
#'   units have an allocation factor of 1 and changed units use
#'   population-based factors
#' - `ltdb`: a single weight that combines land area and block population;
#'   only "population" is accepted
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
    target_year = 2022L,
    crosswalk_source = "geocorr")

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

  # 3b. Geocorr 2000: all pairwise combinations of 8 canonical geographies
  # (2000 Census geography; "puma5" is the PUMA for the 2000 5% sample and
  # "msacmsa" is the 1999 MSA/CMSA)
  geocorr_2000_geographies <- c(
    "block", "block_group", "tract", "county", "place",
    "zcta", "puma5", "msacmsa")

  geocorr_2000 <- tidyr::crossing(
    source_geography = geocorr_2000_geographies,
    target_geography = geocorr_2000_geographies) |>
    dplyr::filter(source_geography != target_geography) |>
    dplyr::mutate(
      source_year = 2000L,
      target_year = 2000L,
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

  # 6. Longitudinal Tract Database (LTDB): tract year pairs NHGIS does not
  # publish (2010 -> 2000). Shares list_ltdb_crosswalks() with the planner.
  ltdb <- list_ltdb_crosswalks() |>
    dplyr::transmute(
      source_geography,
      target_geography,
      source_year = as.integer(source_year),
      target_year = as.integer(target_year),
      crosswalk_source = "ltdb")

  # 7. Combine, deduplicate, and sort. The 2020 <-> 2022 pairs can be served by
  # both CTData and the county-events engine; keep the source the planner
  # routes direct requests to (CTData), mirroring get_crosswalk()'s behavior
  source_preference <- c(
    "geocorr" = 1L, "nhgis" = 2L, "ctdata_2020_2022" = 3L, "county_events" = 4L,
    "ltdb" = 5L)

  dplyr::bind_rows(
    nhgis, geocorr_2022, geocorr_2018, geocorr_2000, ctdata,
    county_events_combos, ltdb) |>
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