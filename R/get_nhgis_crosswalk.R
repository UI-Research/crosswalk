#' Standardize Geography Names
#'
#' Internal helper function to convert various geography name spellings to standard codes.
#'
#' @param geography Character. Geography name in various formats.
#' @param context Character. Either "source" or "target" to determine valid options.
#' @return Character. Standardized geography code.
#' @keywords internal
standardize_geography <- function(geography, context = "source") {
  # Convert to lowercase and remove extra whitespace
  geography <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_trim() |>
    stringr::str_replace_all("_", " ")

  # Define mapping for different spellings
  geography_mapping <- list(
    # Blocks
    "blk" = "blk",
    "block" = "blk",
    "blocks" = "blk",
    "census block" = "blk",
    "census blocks" = "blk",

    # Block groups
    "bg" = "bg",
    "blockgroup" = "bg",
    "block group" = "bg",
    "blockgroups" = "bg",
    "block groups" = "bg",
    "census block group" = "bg",
    "census block groups" = "bg",

    # Block group parts (source only)
    "bgp" = "bgp",
    "block group part" = "bgp",
    "block group parts" = "bgp",
    "blockgroup part" = "bgp",
    "blockgroup parts" = "bgp",
    "census block group part" = "bgp",
    "census block group parts" = "bgp",

    # Tracts
    "tr" = "tr",
    "tract" = "tr",
    "tracts" = "tr",
    "census tract" = "tr",
    "census tracts" = "tr",

    # Counties
    "co" = "co",
    "county" = "co",
    "counties" = "co",
    "cnty" = "co"
  )

  # Check if the geography is in our mapping
  if (geography %in% names(geography_mapping)) {
    standardized <- geography_mapping[[geography]]

    # Validate based on context (source vs target geographies have different options)
    if (context == "source") {
      valid_geogs <- c("blk", "bg", "tr")
      if (standardized %in% valid_geogs) {
        return(standardized)
      }
    } else if (context == "target") {
      valid_geogs <- c("blk", "bg", "tr", "co")
      if (standardized %in% valid_geogs) {
        return(standardized)
      }
    }
  }

  stop("The provided geography is invalid. Use `list_nhgis_crosswalks()` to check available crosswalks.")
}

#' List Available NHGIS Crosswalks
#'
#' Returns a tibble of all available NHGIS geographic crosswalks with their
#' corresponding parameters that can be used with get_nhgis_crosswalk().
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item source_year: Year of the source geography
#'     \item source_geography: Source geography name
#'     \item target_year: Year of the target geography
#'     \item target_geography: Target geography name
#'   }
#'
#' @export
list_nhgis_crosswalks <- function() {
  nhgis_crosswalks_vector = c(
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_blk2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp1990_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp1990_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp1990_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_blk2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2000_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2000_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2000_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_blk2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_blk2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_co2010.zip")

  ## for the time being, not supporting block group parts
  nhgis_crosswalks_vector = nhgis_crosswalks_vector[!stringr::str_detect(nhgis_crosswalks_vector, "bgp")]

  nhgis_crosswalks = purrr::map_dfr(
    nhgis_crosswalks_vector |> stringr::str_remove_all(".*nhgis_|\\.zip"),
    function(path) {
      path_parts = stringr::str_split(path, "_") |> _[[1]]

      tibble::tibble(
        source_geography = path_parts[1] |> stringr::str_extract("[a-zA-Z]{1,10}"),
        source_year = path_parts[1] |> stringr::str_extract("[0-9]{1,10}"),
        target_geography = path_parts[2] |> stringr::str_extract("[a-zA-Z]{1,10}"),
        target_year = path_parts[2] |> stringr::str_extract("[0-9]{1,10}")) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::matches("geography"),
          .fns = ~ .x |> stringr::str_replace_all(c(
            "blk" = "block",
            "bgp" = "block_group_part",
            "bg" = "block_group",
            "tr" = "tract",
            "co" = "county")))) }) |>
    dplyr::bind_cols(tibble::tibble(crosswalk_path = nhgis_crosswalks_vector))

  return(nhgis_crosswalks)
}

#' Get NHGIS Geographic Crosswalk
#'
#' Retrieves a geographic crosswalk from the IPUMS NHGIS API based on user-specified
#' source and target geographies and years. Use `list_nhgis_crosswalks()` to view valid
#' parameter combinations.
#'
#' Note: for the moment, this function does not support block group part crosswalks.
#'
#' @param source_year Character or numeric. Year of the source geography one of c(1990, 2000, 2010, 2020).
#' @param source_geography Character. Source geography name. One of c("block", "block group", "tract").
#' @param target_year Character or numeric. Year of the target geography, one of c(1990, 2000, 2010, 2020).
#' @param target_geography Character. Target geography name. One of c("block", "block group", "tract", "county").
#' @param download_directory File path. Where to download the crosswalk to.
#' @param use_cache FALSE by default. If TRUE, read in an already-downloaded crosswalk stored in the `download_directory`, if such a file exists.
#' @param api_key Character. NULL by default, in which case the function looks for an `IPUMS_API_KEY` environment variable.
#'
#' @return A data frame containing the crosswalk between the specified geographies.
#'
#'#' @return A dataframe representing the requested Geocorr22 crosswalk for all 51 states and Puerto Rico. Depending on the desired geographies, some fields may not be included.
#'   \describe{
#'     \item{source_geoid}{A unique identifier for the source geography}
#'     \item{target_geoid}{A unique identifier for the target geography}
#'     \item{source_geography_name}{The name of the source geography}
#'     \item{target_geography_name}{The name of the target geography}
#'     \item{source_year}{The year of the source geography}
#'     \item{target_year}{The year of the target geography}
#'     \item{allocation_factor_source_to_target}{The weight to interpolate values from the source geography to the target geography}
#'     \item{weighting_factor}{The attribute used to calculate allocation factors}
#'   }
#'
#' @export

get_nhgis_crosswalk <- function(
    source_year,
    source_geography,
    target_year,
    target_geography,
    download_directory,
    use_cache = FALSE,
    api_key = NULL) {

  # Convert years to character for consistent processing
  source_year = as.character(source_year)
  target_year = as.character(target_year)

  # Standardize geography names
  source_geography_standardized = standardize_geography(source_geography, "source")
  target_geography_standardized = standardize_geography(target_geography, "target")

  crosswalk_sub_path = stringr::str_c(source_geography_standardized, source_year, "_", target_geography_standardized, target_year)
  crosswalk_path <- paste0("https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_", crosswalk_sub_path, ".zip")

  ## identify the relevant filepaths for potentially-cached crosswalks
  csv_path = file.path(download_directory, stringr::str_c("nhgis_crosswalk_", crosswalk_sub_path, ".csv"))

  ## if the file exists and the user does not wish to overwrite it
  if (file.exists(csv_path) & isTRUE(use_cache)) {
    result = readr::read_csv(csv_path)

    message("Use of NHGIS crosswalks is subject to the same conditions as for all NHGIS data. See https://www.nhgis.org/citation-and-use-nhgis-data.")
    message("Reading file from cache.")

    return(result) }

  # Validate inputs
  valid_years = c("1990", "2000", "2010", "2020")
  valid_source_geogs = c("blk", "bg", "tr")
  valid_target_geogs = c("blk", "bg", "tr", "co")

  if (source_year == "1990" & target_year == "2000") {
    stop("There are no crosswalks from 1990 to 2000; 1990 source geography crosswalks are available only to 2010 geographies.")}

  if (!source_year %in% valid_years) {
    stop("source_year must be one of: ", paste(valid_years, collapse = ", "))}

  if (!target_year %in% valid_years) {
    stop("target_year must be one of: ", paste(valid_years, collapse = ", "))}

  if (is.null(source_geography_standardized)) {
    stop("source_geography '", source_geography, "' is not valid. Must be one of: blocks, block group parts, or tracts (various spellings accepted)")}

  if (is.null(target_geography_standardized)) {
    stop("target_geography '", target_geography, "' is not valid. Must be one of: blocks, block groups, tracts, or counties (various spellings accepted)")}

  if (!(crosswalk_path %in% list_nhgis_crosswalks()$crosswalk_path)) {
    stop(stringr::str_c("There is no available crosswalk between the specified geographies and years.")) }

  if (!is.null(api_key)) {
message(
"For future use, it may be easiest to store your API key in an environment variable.
We recommend storing your IPUMS API key by using usethis::edit_r_environ(scope = 'user'),
creating a new name-key pair akin to 'IPUMS_API_KEY=[your key here]'.") }

  # Get API key
  if (is.null(api_key)) {
    api_key = Sys.getenv("IPUMS_API_KEY")
    if (api_key == "") {
      stop("API key required. Provide via api_key parameter or set IPUMS_API_KEY environment variable. Get your key at https://account.ipums.org/api_keys") }}

  crosswalk_df1 = tryCatch({

    zip_path = file.path(download_directory, stringr::str_c(crosswalk_sub_path, ".zip"))
    csv_path_temporary = file.path(download_directory, stringr::str_c("nhgis_", crosswalk_sub_path, ".csv"))

    ## if the specified directory doesn't yet exist, create it
    if (!dir.exists(download_directory)) { dir.create(download_directory) }

    # Download the crosswalk file
    response = httr::GET(
      crosswalk_path,
      httr::add_headers(Authorization = api_key),
      httr::write_disk(zip_path, overwrite = TRUE), overwrite = TRUE)

    # Unzip the .zip
    utils::unzip(
      zipfile = zip_path,
      exdir = file.path(download_directory))

    crosswalk_df = readr::read_csv(csv_path_temporary) |>
      janitor::clean_names()

    # Remove the zipped folder and the raw CSV file
    file.remove(zip_path)
    file.remove(csv_path_temporary)

    crosswalk_df
    },
    error = function(e) {
      stop("Failed to retrieve crosswalk: ", e$message) })

  crosswalk_df = crosswalk_df1 |>
    dplyr::select(-dplyr::matches("gj")) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ .x |> stringr::str_replace_all(c(
        ## for block-based crosswalks, there's only a single, combined weight
        "^weight$" = "weight_housing_population",
        "parea" = "weight_landarea",
        "wt" = "weight",
        "pop$" = "population",
        "fam" = "family",
        "hh" = "household",
        "_hu" = "_housing_all",
        "ownhu" = "housing_owned",
        "renthu" = "housing_rented"))) |>
    dplyr::rename(
      source_geoid = !!(stringr::str_c(source_geography_standardized, source_year, "ge")),
      target_geoid = !!(stringr::str_c(target_geography_standardized, target_year, "ge"))) |>
    dplyr::mutate(
      source_geography_name = source_geography_standardized,
      target_geography_name = target_geography_standardized,
      dplyr::across(
        .cols = dplyr::matches("geography_name"),
        .fns = ~ .x |> stringr::str_replace_all(c(
          "blk" = "block",
          "bgp" = "block_group_part",
          "bg" = "block_group",
          "tr" = "tract",
          "co" = "county"))),
      source_year = source_year,
      target_year = target_year) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("weight_"),
      names_to = "weighting_factor",
      values_to = "allocation_factor_source_to_target")

    ## if the file does not already exit or if overwrite_cache is TRUE, write to cache
    readr::write_csv(crosswalk_df, csv_path)

  message("Use of NHGIS crosswalks is subject to the same conditions as for all NHGIS data. See https://www.nhgis.org/citation-and-use-nhgis-data.")

  return(crosswalk_df)
}

# list_nhgis_crosswalks() |>
#   dplyr::select(-crosswalk_path) |>
#   dplyr::mutate(download_directory = here::here("crosswalks-cache"), use_cache = TRUE) |>
#   dplyr::slice(28) |>
#   purrr::pmap(get_nhgis_crosswalk)
