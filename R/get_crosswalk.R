#' Get an inter-temporal or inter-geography crosswalk
#'
#' Retrieves a crosswalk with interpolation values from a source geography to a target
#' geography or from a source year to a target year.
#'
#' @details This function sources crosswalks from Geocorr 2022 and IPUMS NHGIS.
#'    Crosswalk weights are from the original sources and have not been modified;
#'    this function merely standardizes the format of the returned crosswalks and
#'    enables easy programmatic access and cacheing.
#'
#'    Note that an IPUMS NHGIS API key is required to access crosswalks from that
#'    source. Use `usethis::edit_r_environ(scope = "user")` to save your API key
#'    to your .Renviron; the name of the key should be "IPUMS_API_KEY". You can
#'    obtain a key from: https://account.ipums.org/api_keys.
#'
#' @param source_year Character or numeric. Year of the source geography one of
#'    c(1990, 2000, 2010, 2020).
#' @param source_geography Character. Source geography name. One of c("block",
#'    "block group", "tract", "place", county", "urban_area", "zcta", "puma", "cd118",
#'    "cd119", "urban_area", "core_based_statistical_area").
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020).
#' @param target_geography Character. Target geography name. One of c("block",
#'    "block group", "tract", "place", county", "urban_area", "zcta", "puma", "cd118",
#'    "cd119", "urban_area", "core_based_statistical_area").
#' @param weight Character. Weighting variable. One of c("population", "housing", "land").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#'
#' @return A data frame containing the crosswalk between the specified geographies.
#'    Data are tidy-formatted, with each observation reflecting a unique
#'    source-target-weighting factor combination. Note that all (typically two
#'    or three) available weighting factors are returned.
#'
#' @return A dataframe representing the requested crosswalk for all 51 states
#'    and Puerto Rico. Depending on the desired geographies and the source of the
#'    crosswalk (Geocorr vs. NHGIS), some fields may not be included.
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
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' get_crosswalk(
#   source_geography = "zcta",
#   target_geography = "puma22",
#   weight = c("population"),
#   cache = here::here("crosswalks-cache"))
#' }

get_crosswalk = function(
  source_geography,
  target_geography,
  source_year = NULL,
  target_year = NULL,
  cache = NULL,
  weight = NULL) {

  if (
    (source_year == target_year | (is.null(source_year) & is.null(target_year))) &

    source_geography == "block" & target_geography %in% c("block group", "tract", "county", "core_based_statistical_area") |
    source_geography == "block group" & target_geography %in% c("tract", "county", "core_based_statistical_area") |
    source_geography == "tract" & target_geography %in% c("county", "core_based_statistical_area") |
    source_geography == "county" & target_geography == "core_based_statistical_area") {

    warning(
"The source geography is nested within the target geography and an empty result
will be returned. No crosswalk is needed to translate data between nested geographies;
simply aggregate your data to the desired geography.")

    return(tibble::tibble())
  }

  if (is.null(source_year) | is.null(target_year)) {
    crosswalk_source = "geocorr"
  } else { crosswalk_source = "nhgis" }

  if (crosswalk_source == "nhgis") {
    result = get_nhgis_crosswalk(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography,
      cache = cache)
  } else {
    result = get_geocorr_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      weight = weight,
      cache = cache)
  }
}

# ## write out geocorr crosswalks
# core_sources_geocorr = c(
#   "place", "county", "tract", "blockgroup", "zcta", "puma22", "cd119", "cd118")
#
# ## create an intersection of all geography combinations
# expand.grid(core_sources_geocorr, core_sources_geocorr) |>
#   dplyr::rename(source_geography = 1, target_geography = 2) |>
#   ## drop where the source and target geographies are the same
#   dplyr::filter(source_geography != target_geography) |>
#   dplyr::mutate(
#     weight = "housing",
#     cache = file.path("C:", "Users", climateapi::get_system_username(), "Box", "Arnold LIHTC study", "Data", "Crosswalks"),
#     dplyr::across(dplyr::where(is.factor), as.character)) |>
#   dplyr::slice(9) |>
#   purrr::pwalk(get_crosswalk)
