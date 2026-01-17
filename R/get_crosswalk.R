#' Get an inter-temporal or inter-geography crosswalk
#'
#' Retrieves a crosswalk with interpolation values from a source geography to a target
#' geography or from a source year to a target year.
#'
#' @details This function sources crosswalks from Geocorr 2022, IPUMS NHGIS, and
#'    CT Data Collaborative. Crosswalk weights are from the original sources and
#'    have not been modified; this function merely standardizes the format of the
#'    returned crosswalks and enables easy programmatic access and caching.
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
#'    "block group", "tract", "place", "county", "urban_area", "zcta", "puma", "cd118",
#'    "cd119", "urban_area", "core_based_statistical_area").
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020) for decennial crosswalks, or c(2011, 2012, 2014,
#'    2015, 2022) for non-census year crosswalks (limited to block groups, tracts,
#'    and counties).
#' @param target_geography Character. Target geography name. One of c("block",
#'    "block group", "tract", "place", "county", "urban_area", "zcta", "puma", "cd118",
#'    "cd119", "urban_area", "core_based_statistical_area").
#' @param weight Character. Weighting variable for Geocorr crosswalks. One of
#'    c("population", "housing", "land").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk. Individual component crosswalks
#'    are cached separately when provided.
#'
#' @return A tibble containing the crosswalk between the specified geographies.
#'    Data are tidy-formatted, with each observation reflecting a unique
#'    source-target-weighting factor combination.
#'
#'    The returned tibble includes an attribute `crosswalk_metadata` (access via
#'    `attr(result, "crosswalk_metadata")`) containing comprehensive information
#'    about how the crosswalk was produced:
#'    \describe{
#'      \item{call_parameters}{List of the parameters passed to get_crosswalk()}
#'      \item{data_source}{Short identifier for the data source (e.g., "nhgis", "geocorr", "ctdata")}
#'      \item{data_source_full_name}{Full name of the data source}
#'      \item{download_url}{URL from which the crosswalk was downloaded (NHGIS, CTData)}
#'      \item{api_endpoint}{API endpoint used (Geocorr)}
#'      \item{documentation_url}{URL to documentation for the crosswalk source}
#'      \item{citation_url}{URL to citation requirements (NHGIS)}
#'      \item{github_repository}{GitHub repository URL (CTData)}
#'      \item{source_geography}{Source geography as specified by user}
#'      \item{source_geography_standardized}{Standardized source geography code}
#'      \item{target_geography}{Target geography as specified by user}
#'      \item{target_geography_standardized}{Standardized target geography code}
#'      \item{source_year}{Source year (if applicable)}
#'      \item{target_year}{Target year (if applicable)}
#'      \item{reference_year}{Reference year for same-year crosswalks (Geocorr)}
#'      \item{weighting_variable}{Variable used to calculate allocation factors}
#'      \item{state_coverage}{Geographic coverage notes (e.g., "Connecticut only")}
#'      \item{notes}{Additional notes about the crosswalk}
#'      \item{retrieved_at}{Timestamp when crosswalk was retrieved}
#'      \item{cached}{Logical indicating if result was cached}
#'      \item{cache_path}{Path to cached file (if applicable)}
#'      \item{read_from_cache}{Logical indicating if result was read from cache}
#'      \item{crosswalk_package_version}{Version of the crosswalk package used}
#'    }
#'
#'    Columns in the returned dataframe (some may not be present depending on source):
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
#' get_crosswalk(
#'   source_geography = "zcta",
#'   target_geography = "puma22",
#'   weight = "population",
#'   cache = here::here("crosswalks-cache"))
#'
#' # Inter-temporal crosswalk (uses NHGIS)
#' get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "tract",
#'   source_year = 2010,
#'   target_year = 2020,
#'   cache = here::here("crosswalks-cache"))
#'
#' # Non-census year crosswalk (2020 to 2022, CT changes)
#' get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "tract",
#'   source_year = 2020,
#'   target_year = 2022,
#'   cache = here::here("crosswalks-cache"))
#' }

get_crosswalk <- function(
  source_geography,
  target_geography,
  source_year = NULL,
  target_year = NULL,
  cache = NULL,
  weight = NULL) {

  if (
    (source_geography == "block" & target_geography %in% c("block group", "tract", "county", "core_based_statistical_area") |
    source_geography == "block group" & target_geography %in% c("tract", "county", "core_based_statistical_area") |
    source_geography == "tract" & target_geography %in% c("county", "core_based_statistical_area") |
    source_geography == "county" & target_geography == "core_based_statistical_area") & 
    ((is.null(source_year) & is.null(target_year)) | (source_year == target_year))
  ) {
    warning(
"The source geography is nested within the target geography and an empty result
will be returned. No crosswalk is needed to translate data between nested geographies;
simply aggregate your data to the desired geography.")

    return(tibble::tibble())
  }

  source_year_chr <- if (!is.null(source_year)) as.character(source_year) else NULL
  target_year_chr <- if (!is.null(target_year)) as.character(target_year) else NULL

  if (is.null(source_year) | is.null(target_year)) {
    crosswalk_source <- "geocorr"
  } else if (source_year_chr == "2020" & target_year_chr == "2022") {
    crosswalk_source <- "ctdata_2020_2022"
  } else {
    crosswalk_source <- "nhgis"
  }

  if (crosswalk_source == "ctdata_2020_2022") {
    result <- get_crosswalk_2020_2022(
      geography = source_geography,
      cache = cache)

  } else if (crosswalk_source == "nhgis") {
    result <- get_nhgis_crosswalk(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography,
      cache = cache)

  } else {
    result <- get_geocorr_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      weight = weight,
      cache = cache)
  }

  # Retrieve metadata from internal function (if present)
  internal_metadata <- attr(result, "crosswalk_metadata")

  # Build comprehensive metadata object
  metadata <- list(
    # Call parameters
    call_parameters = list(
      source_geography = source_geography,
      target_geography = target_geography,
      source_year = source_year_chr,
      target_year = target_year_chr,
      weight = weight,
      cache = cache),

    # Data source information
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
        "geocorr" = "Geocorr 2022 (Missouri Census Data Center)",
        "ctdata_2020_2022" = "CT Data Collaborative",
        crosswalk_source)
    },

    # URLs and documentation
    download_url = internal_metadata$download_url,
    api_endpoint = internal_metadata$api_endpoint,
    documentation_url = internal_metadata$documentation_url,
    citation_url = internal_metadata$citation_url,
    github_repository = internal_metadata$github_repository,

    # Geography and year details
    source_geography = source_geography,
    source_geography_standardized = internal_metadata$source_geography_standardized,
    target_geography = target_geography,
    target_geography_standardized = internal_metadata$target_geography_standardized,
    source_year = source_year_chr,
    target_year = target_year_chr,
    reference_year = internal_metadata$reference_year,

    # Weighting
    weighting_variable = if (!is.null(internal_metadata$weighting_variable)) {
      internal_metadata$weighting_variable
    } else {
      weight
    },

    # Coverage and notes
    state_coverage = internal_metadata$state_coverage,
    notes = if (crosswalk_source == "ctdata_2020_2022") {
      c("Connecticut: CTData Collaborative 2020-2022 crosswalk",
        "Other states: No geographic changes between 2020 and 2022; use identity mapping",
        internal_metadata$notes)
    } else {
      internal_metadata$notes
    },

    # Retrieval information
    retrieved_at = internal_metadata$retrieved_at,
    cached = internal_metadata$cached,
    cache_path = internal_metadata$cache_path,
    read_from_cache = internal_metadata$read_from_cache,

    # Package information
    crosswalk_package_version = as.character(utils::packageVersion("crosswalk")))

  attr(result, "crosswalk_metadata") <- metadata

  result = result |>
    dplyr::mutate(
      dplyr::across(
        .cols = -allocation_factor_source_to_target,
        .fns = as.character),
      dplyr::across(
        .cols = allocation_factor_source_to_target,
        as.numeric))

  return(result)
}


#' Get 2020 to 2022 Crosswalk (Connecticut + Identity Mapping)
#'
#' Internal function that handles the special case of 2020 to 2022 crosswalks.
#' Connecticut changed from historical counties to planning regions in 2022,
#' while all other states had no geographic changes.
#'
#' @param geography Character. Geography type: one of "block", "block_group",
#'    "tract", or "county".
#' @param cache Directory path for caching component crosswalks.
#'
#' @return A tibble containing the national crosswalk with Connecticut from CTData
#'    and identity mappings for other states.
#' @noRd
get_crosswalk_2020_2022 <- function(geography, cache = NULL) {

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
"2020 to 2022 crosswalks are only available for blocks, block groups, tracts,
and counties. The provided geography '", geography, "' is not supported.")}

  message(
"Constructing 2020 to 2022 crosswalk:
- Connecticut: Using CT Data Collaborative crosswalk (FIPS code changes only,
  boundaries unchanged). Historical counties were replaced by planning regions.
- Other states: No geographic changes occurred between 2020 and 2022.
  Returning identity mapping (source_geoid = target_geoid) for non-CT states.")

 ct_crosswalk <- get_ctdata_crosswalk(
    geography = geography_standardized,
    cache = cache)

  message(
    "Connecticut crosswalk loaded: ", nrow(ct_crosswalk), " ",
    geography_standardized, " records.")

  attr(ct_crosswalk, "crosswalk_sources") <- list(
    connecticut = "ctdata",
    other_states = "identity_mapping")
  attr(ct_crosswalk, "identity_states_note") <-
"For states other than Connecticut, no geographic changes occurred between 2020
and 2022. When joining your data, non-CT records will match on identical GEOIDs.
This crosswalk only contains Connecticut records where FIPS codes changed."

  return(ct_crosswalk)
}

# ## write out geocorr crosswalks
# core_sources_geocorr = c(
#   #"place", "county",
#   "tract",
#   #"blockgroup",
#   "zcta",
#   "puma22"#,
#   #"cd119", "cd118"
#   )

# library(climateapi)
## create an intersection of all geography combinations
# expand.grid(core_sources_geocorr, core_sources_geocorr) |>
#   dplyr::rename(source_geography = 1, target_geography = 2) |>
#   ## drop where the source and target geographies are the same
#   dplyr::filter(source_geography != target_geography) |>
#   dplyr::mutate(
#     weight = "housing",
#     cache = file.path("C:", "Users", climateapi::get_system_username(), "Box", "Arnold LIHTC study", "Data", "Shapefiles and crosswalks", "crosswalk_acs_decennial_chas"),
#     dplyr::across(dplyr::where(is.factor), as.character)) |>
#   purrr::pwalk(get_crosswalk)

# tibble::tibble(
#   source_geography = "tract",
#   target_geography = "tract",
#   source_year = c(1990, 2000, 2010),
#   target_year = c(2010, 2010, 2020)) |>
#   dplyr::mutate(
#     weight = "housing",
#     cache = file.path("C:", "Users", climateapi::get_system_username(), "Box", "Arnold LIHTC study", "Data", "Shapefiles and crosswalks", "crosswalk_acs_decennial_chas"),
#     dplyr::across(dplyr::where(is.factor), as.character)) |>
#   purrr::pwalk(get_crosswalk)
