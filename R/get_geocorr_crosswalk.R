#' Obtain a Geocorr22 Crosswalk
#'
#' Query Geocorr22 (https://mcdc.missouri.edu/applications/geocorr2022.html) for
#' a crosswalk between two geographies for all 51 states and Puerto Rico.
#'
#' @details Note: this function is under development but does not yet support all
#'    of the geographies supported by Geocorr. Currently this includes:
#'    c("place", "county", "tract", "blockgroup", "zcta", "puma22", "cd119", "cd118").
#'
#' @param source_geography Character. Source geography name. One of:
#'    c("place", "county", "tract", "blockgroup", "zcta", "puma22", "cd119", "cd118").
#'    Note "cd" stands for "congressional district".
#' @param target_geography Character. Target geography name. See `source_geography`
#'    for options.
#' @param weight Character. Weighting variable. One of c("population", "housing", "land").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#'
#' @return A dataframe representing the requested Geocorr22 crosswalk for all 51
#'    states and Puerto Rico. Depending on the desired geographies, some fields
#'    may not be included.
#'   \describe{
#'     \item{state_fips}{A two-digit identified for the state (or DC/PR), if applicable}
#'     \item{state_abbreviation}{A two-character abbreviation for the state (or DC/PR),
#'        if applicable}
#'     \item{source_geoid}{A unique identifier for the source geography}
#'     \item{target_geoid}{A unique identifier for the target geography}
#'     \item{source_geography_name}{The name of the source geography}
#'     \item{target_geography_name}{The name of the target geography}
#'     \item{allocation_factor_source_to_target}{The weight to interpolate values
#'        from the source geography to the target geography}
#'     \item{allocation_factor_target_to_source}{The weight to interpolate values
#'        from the source geography to the target geography}
#'     \item{population_2020}{The estimated overlap in population, if applicable}
#'     \item{housing_2020}{The estimated overlap in housing units, if applicable}
#'     \item{land_area_sqmi}{The overlap in land area, if applicable}
#'     \item{weighting_factor}{The attribute used to calculate allocation factors
#'        (one of population, housing, land)}
#'   }
#' @noRd
get_geocorr_crosswalk <- function(
    source_geography,
    target_geography,
    weight = c("population", "housing", "land"),
    cache = NULL) {

  ## identify the relevant file paths for potentially-cached crosswalks
  if (!is.null(cache)) {
    outpath = file.path(
      cache,
      stringr::str_c("crosswalk_geocorr_2022_to_2022_", source_geography, "_to_",
                     target_geography, "_weightedby_", weight, ".csv")) }

  ## if the file exists and the user does not wish to overwrite it
  if (file.exists(outpath) & !is.null(cache)) {
    result = readr::read_csv(outpath)

    message("Reading file from cache.")

    return(result) }

  # Base API URL for geocorr2022
  base_url <- "https://mcdc.missouri.edu/cgi-bin/broker"

  # Map weight parameter to API format
  weight_value <- switch(weight,
    "population" = "pop20",
    "land" = "landsqmi",
    "housing" = "hus20")

  # Define all states with their abbreviations and FIPS codes
  # Based on the sample URL pattern: state=Mo29, state=Al01, etc.
  states_data <- list(
    list(abbr = "Al", fips = "01"),
    list(abbr = "Ak", fips = "02"),
    list(abbr = "Az", fips = "04"),
    list(abbr = "Ar", fips = "05"),
    list(abbr = "Ca", fips = "06"),
    list(abbr = "Co", fips = "08"),
    list(abbr = "Ct", fips = "09"),
    list(abbr = "De", fips = "10"),
    list(abbr = "Dc", fips = "11"),
    list(abbr = "Fl", fips = "12"),
    list(abbr = "Ga", fips = "13"),
    list(abbr = "Hi", fips = "15"),
    list(abbr = "Id", fips = "16"),
    list(abbr = "Il", fips = "17"),
    list(abbr = "In", fips = "18"),
    list(abbr = "Ia", fips = "19"),
    list(abbr = "Ks", fips = "20"),
    list(abbr = "Ky", fips = "21"),
    list(abbr = "La", fips = "22"),
    list(abbr = "Me", fips = "23"),
    list(abbr = "Md", fips = "24"),
    list(abbr = "Ma", fips = "25"),
    list(abbr = "Mi", fips = "26"),
    list(abbr = "Mn", fips = "27"),
    list(abbr = "Ms", fips = "28"),
    list(abbr = "Mo", fips = "29"),
    list(abbr = "Mt", fips = "30"),
    list(abbr = "Ne", fips = "31"),
    list(abbr = "Nv", fips = "32"),
    list(abbr = "Nh", fips = "33"),
    list(abbr = "Nj", fips = "34"),
    list(abbr = "Nm", fips = "35"),
    list(abbr = "Ny", fips = "36"),
    list(abbr = "Nc", fips = "37"),
    list(abbr = "Nd", fips = "38"),
    list(abbr = "Oh", fips = "39"),
    list(abbr = "Ok", fips = "40"),
    list(abbr = "Or", fips = "41"),
    list(abbr = "Pa", fips = "42"),
    list(abbr = "Ri", fips = "44"),
    list(abbr = "Sc", fips = "45"),
    list(abbr = "Sd", fips = "46"),
    list(abbr = "Tn", fips = "47"),
    list(abbr = "Tx", fips = "48"),
    list(abbr = "Ut", fips = "49"),
    list(abbr = "Vt", fips = "50"),
    list(abbr = "Va", fips = "51"),
    list(abbr = "Wa", fips = "53"),
    list(abbr = "Wv", fips = "54"),
    list(abbr = "Wi", fips = "55"),
    list(abbr = "Wy", fips = "56"),
    list(abbr = "Pr", fips = "72")) |>
    purrr::map_chr(~ paste0(.x$abbr, .x$fips))

  ## for block-level crosswalks, the maximum number of states per query is 13
  if ("block" %in% c(source_geography, target_geography)) {

    n = length(states_data) / 13
    groups = cut(seq_along(states_data), n, labels = FALSE)
    states_chunked = split(states_data, groups)

    df1 = purrr::map_dfr(
      states_chunked,
      function(states) {
        # Build query parameters
        params <- list(
          `_PROGRAM` = "apps.geocorr2022.sas",
          `_SERVICE` = "MCDC_long",
          `_debug` = "0",
          `afacts2` = "on",
          `g1_` = source_geography,
          `g2_` = target_geography,
          `wtvar` = weight_value,
          `nozerob` = "1",
          `fileout` = "1",
          `filefmt` = "csv",
          `lstfmt` = "html",
          `title` = "",
          `sort2` = "on",
          `counties` = "",
          `metros` = "",
          `places` = "",
          `oropt` = "",
          `latitude` = "",
          `longitude` = "",
          `distance` = "",
          `kiloms` = "0",
          `locname` = "",
          `state` = states)

        # Make the HTTP GET request using httr2
        request <- httr2::request(base_url) |>
          httr2::req_url_query(!!!params, .multi = "explode")

        csv_path <- httr2::req_perform(request)  |>
          httr2::resp_body_html() |>
          rvest::html_element("body") |>
          rvest::html_text2() |>
          stringr::str_extract("geocorr.*.csv")

        if (is.na(csv_path)) { stop("Unable to acquire the specified crosswalk; please file an issue.") }

        df1 = readr::read_csv(file.path("https://mcdc.missouri.edu", "temp", csv_path)) |>
          janitor::clean_names() })} else {

    # Build query parameters
    params <- list(
      `_PROGRAM` = "apps.geocorr2022.sas",
      `_SERVICE` = "MCDC_long",
      `_debug` = "0",
      `afacts2` = "on",
      `g1_` = source_geography,
      `g2_` = target_geography,
      `wtvar` = weight_value,
      `nozerob` = "1",
      `fileout` = "1",
      `filefmt` = "csv",
      `lstfmt` = "html",
      `title` = "",
      `sort2` = "on",
      `counties` = "",
      `metros` = "",
      `places` = "",
      `oropt` = "",
      `latitude` = "",
      `longitude` = "",
      `distance` = "",
      `kiloms` = "0",
      `locname` = "",
      `state` = states_data)

    # Make the HTTP GET request using httr2
    request <- httr2::request(base_url) |>
      httr2::req_url_query(!!!params, .multi = "explode")

    csv_path <- httr2::req_perform(request)  |>
      httr2::resp_body_html() |>
      rvest::html_element("body") |>
      rvest::html_text2() |>
      stringr::str_extract("geocorr.*.csv")

    if (is.na(csv_path)) { stop("Unable to acquire the specified crosswalk; please file an issue.") }

    df1 = readr::read_csv(file.path("https://mcdc.missouri.edu", "temp", csv_path)) |>
      janitor::clean_names() }

  df2 = df1 |>
    dplyr::slice(2:nrow(df1)) |>
    ## naming conventions for some geographies are inconsistent; we standardize
    dplyr::rename_with(
      .cols = dplyr::matches("zip_name"),
      .fn = ~ .x |> stringr::str_replace("zip", "zcta")) |>
    dplyr::rename_with(
      .cols = dplyr::matches("puma22name"),
      .fn = ~ .x |> stringr::str_replace("puma22name", "puma22_name")) |>
    dplyr::rename_with(
      .cols = dplyr::matches("hus20|pop20|landsqmi"),
      .fn = ~ stringr::str_replace_all(.x, c(
        "hus20" = "housing_2020", "pop20" = "population_2020", "landsqmi" = "land_area_sqmi")))

  if (!"state" %in% colnames(df2)) {
    df2 = df2 |>
      dplyr::mutate(
       state = stringr::str_sub(county, 1, 2),
       county = stringr::str_sub(county, 3, 5)) }

  df2 = df2 |>
    dplyr::mutate(
      ## data with blocks/block groups/tracts have differently structured/named columns
      ## we standardize here so that subsequent workflows are uniform
      dplyr::across(
        .cols = dplyr::matches("^block$"),
        .fns = ~ stringr::str_c(state, county, tract, block) |>
          stringr::str_remove_all("\\.")),
      dplyr::across(
        .cols = dplyr::matches("^block$"),
        .fns = ~ stringr::str_c(county_name, " ", tract, block),
        .names = "block_name"),
      dplyr::across(
        .cols = dplyr::matches("^blockgroup$"),
        .fns = ~ stringr::str_c(state, county, tract, blockgroup) |>
          stringr::str_remove_all("\\.")),
      dplyr::across(
        .cols = dplyr::matches("^blockgroup$"),
        .fns = ~ stringr::str_c(county_name, " ", blockgroup),
        .names = "blockgroup_name"),
      dplyr::across(
        .cols = dplyr::matches("^tract$"),
        .fns = ~ stringr::str_c(state, county, tract) |>
          stringr::str_remove_all("\\.")),
      dplyr::across(
        .cols = dplyr::matches("^tract$"),
        .fns = ~ stringr::str_c(county_name, " ", tract),
        .names = "tract_name"),
      dplyr::across(
        .cols = dplyr::matches("^cd11"),
        .fns = ~ stringr::str_c(stab, "-", .x),
        .names = "{.col}_name")) |>
    dplyr::rename_with(
      .cols = dplyr::matches("state|stab"),
      .fn = ~ stringr::str_replace_all(.x, c("state" = "state_fips", "stab" = "state_abbreviation"))) |>
    dplyr::select(
      dplyr::matches("state"),
      source_geoid = source_geography,
      target_geoid = target_geography,
      source_geography_name = !!stringr::str_c(source_geography, "_name"),
      target_geography_name = !!stringr::str_c(target_geography, "_name"),
      allocation_factor_source_to_target = afact,
      allocation_factor_target_to_source = afact2,
      dplyr::any_of(c("housing_2020", "population_2020", "land_area_sqmi"))) |>
    dplyr::mutate(
      source_geography = source_geography,
      target_geography = target_geography,
      weighting_factor = weight,
      dplyr::across(.cols = dplyr::matches("allocation"), .fns = as.numeric))

  if (!is.null(cache)) {
    ## if the file does not already exist and cache is TRUE
    if (!file.exists(outpath) & !is.null(cache)) {
      ## if the specified cache directory doesn't yet exist, create it
      if (!dir.exists(cache)) { dir.create(cache) }
      readr::write_csv(df2, outpath)
    }
  }
  return(df2)
}

utils::globalVariables(c("afact", "afact2", "county"))

# get_geocorr_crosswalk(
#   source_geography = "zcta",
#   target_geography = "puma22",
#   weight = c("population"),
#   cache = here::here("crosswalks-cache"),
#   overwrite_cache = FALSE)

# ## omitting the provided MO-specific geographies
# sources = c(
#   "place", "county", "tract", "blockgroup", "block", "zcta", "puma22", "cousub",
#   "cbsa20", "cbsatype20", "metdiv20", "csa20", "necta", "nectadiv", "cnect", "aiannh",
#   ## these may be formatted differently -- including a state parameter?
#   "sduni20", "sdelem20", "sdsec20", "sdbest20", "sdbesttype20", "placesc", "puma12",
#   "countysc", "inplace", "ur", "ua", "cbsa23", "cbsatype23", "metdiv23", "csa23",
#   "cbsacentral23", "sldu24", "sldl24", "sldu22", "sld22", "sldu18", "sldl28",
#   "cd119", "cd118", "cd117", "cd116",
#   ## ctregion only works for CT; "vtd20" may be nested at the country level?
#   "ctregion", "vtd20", "hsa19", "hrr19", "rucc23")
#
# ## "block" -- this level requires submitting 13 or fewer states at a time
#
# core_sources = c("place", "county", "tract", "blockgroup",
#                   "zcta", "puma22", "cd119", "cd118")
#
# expand.grid(core_sources, core_sources) |>
#   dplyr::rename(source_geography = 1, target_geography = 2) |>
#   dplyr::filter(source_geography != target_geography) |>
#   dplyr::mutate(weight = "population", cache = here::here("crosswalks-cache"), overwrite_cache = FALSE, dplyr::across(dplyr::where(is.factor), as.character)) |>
#   purrr::pwalk(get_geocorr_crosswalk)


