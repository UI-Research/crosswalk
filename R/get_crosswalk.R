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
#'    The returned tibble includes an attribute `crosswalk_metadata` containing:
#'    \describe{
#'      \item{source}{Character vector of data sources used (e.g., "nhgis", "ctdata")}
#'      \item{source_year}{The source year}
#'      \item{target_year}{The target year}
#'      \item{source_geography}{The source geography}
#'      \item{target_geography}{The target geography}
#'      \item{notes}{Any relevant notes about the crosswalk construction}
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
    source_geography == "block" & target_geography %in% c("block group", "tract", "county", "core_based_statistical_area") |
    source_geography == "block group" & target_geography %in% c("tract", "county", "core_based_statistical_area") |
    source_geography == "tract" & target_geography %in% c("county", "core_based_statistical_area") |
    source_geography == "county" & target_geography == "core_based_statistical_area"
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

  metadata <- list(
    source = character(),
    source_year = source_year_chr,
    target_year = target_year_chr,
    source_geography = source_geography,
    target_geography = target_geography,
    notes = character())

  if (crosswalk_source == "ctdata_2020_2022") {
    result <- get_crosswalk_2020_2022(
      geography = source_geography,
      cache = cache)
    metadata$source <- c("ctdata", "identity")
    metadata$notes <- c(
      "Connecticut: CTData Collaborative 2020-2022 crosswalk (identity mapping, FIPS code change only)",
      "Other states: Identity mapping (no geographic changes between 2020 and 2022)")

  } else if (crosswalk_source == "nhgis") {
    result <- get_nhgis_crosswalk(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography,
      cache = cache)
    metadata$source <- "nhgis"

  } else {
    result <- get_geocorr_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      weight = weight,
      cache = cache)
    metadata$source <- "geocorr"
  }

  attr(result, "crosswalk_metadata") <- metadata

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
