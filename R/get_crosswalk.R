#' Get an Inter-temporal or Inter-geography Crosswalk
#'
#' Retrieves a crosswalk with interpolation values from a source geography to a target
#' geography, and if desired, from a source year to a target year.
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
#'    "block group", "tract").
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020).
#' @param target_geography Character. Target geography name. One of c("block",
#'    "block group", "tract", "place", county", "urban_area", "zcta", "puma",
#'    "core_based_statistical_area").
#' @param weight Character. Weighting variable. One of c("population", "housing", "land").
#' @param cache File path. Where to download the crosswalk to. If NULL (default),
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
