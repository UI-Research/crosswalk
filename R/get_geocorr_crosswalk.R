#' Get GeoCorr Version Configuration
#'
#' Returns configuration for a specific GeoCorr version (2022 or 2018).
#'
#' @param version Character. Either "2022" or "2018".
#' @return A named list with version-specific configuration.
#' @keywords internal
#' @noRd
get_geocorr_config <- function(version = "2022") {
  if (version == "2022") {
    list(
      program = "apps.geocorr2022.sas",
      documentation_url = "https://mcdc.missouri.edu/applications/geocorr2022.html",
      reference_year = "2022",
      census_year = "2020",
      weight_map = list(
        "population" = "pop20",
        "housing" = "hus20",
        "land" = "landsqmi"),
      weight_rename = c(
        "hus20" = "housing_2020",
        "pop20" = "population_2020",
        "landsqmi" = "land_area_sqmi"),
      geography_api_codes = list(
        "place" = "place",
        "county" = "county",
        "tract" = "tract",
        "block" = "block",
        "blockgroup" = "blockgroup",
        "block_group" = "blockgroup",
        "zcta" = "zcta",
        "puma" = "puma22",
        "puma22" = "puma22",
        "cd118" = "cd118",
        "cd119" = "cd119"),
      puma_col = "puma22",
      cd_pattern = "^cd11[89]",
      has_name_columns = TRUE,
      include_pr = TRUE,
      # GeoCorr 2022 uses fileout=1 + filefmt=csv for CSV output
      csv_params = list(`fileout` = "1", `filefmt` = "csv"),
      extra_params = list(`afacts2` = "on", `sort2` = "on"))
  } else if (version == "2018") {
    list(
      program = "apps.geocorr2018.sas",
      documentation_url = "https://mcdc.missouri.edu/applications/geocorr2018.html",
      reference_year = "2018",
      census_year = "2010",
      weight_map = list(
        "population" = "pop10",
        "housing" = "hus10",
        "land" = "landsqmi"),
      weight_rename = c(
        "hus10" = "housing_2010",
        "pop10" = "population_2010",
        "landsqmi" = "land_area_sqmi"),
      geography_api_codes = list(
        "place" = "placefp",
        "placefp" = "placefp",
        "county" = "county",
        "tract" = "tract",
        "block" = "block",
        "blockgroup" = "bg",
        "block_group" = "bg",
        "bg" = "bg",
        "zcta" = "zcta5",
        "zcta5" = "zcta5",
        "puma" = "puma12",
        "puma12" = "puma12",
        "cd115" = "cd115",
        "cd116" = "cd116"),
      puma_col = "puma12",
      cd_pattern = "^cd11[56]",
      has_name_columns = FALSE,
      include_pr = FALSE,
      # GeoCorr 2018 uses csvout=1 for CSV output (different from 2022)
      csv_params = list(`csvout` = "1"),
      extra_params = list(`namoptf` = "b", `namoptr` = "b"))
  } else {
    stop("Invalid GeoCorr version: '", version, "'. Must be '2022' or '2018'.")
  }
}


#' Resolve Geography Name to GeoCorr API Code
#'
#' Translates a user-facing geography name to the appropriate API parameter
#' for the specified GeoCorr version.
#'
#' @param geography Character. User-facing geography name.
#' @param geocorr_version Character. "2022" or "2018".
#' @return Character. The API code for the geography.
#' @keywords internal
#' @noRd
resolve_geocorr_geography <- function(geography, geocorr_version = "2022") {
  config <- get_geocorr_config(geocorr_version)
  api_code <- config$geography_api_codes[[geography]]

  if (is.null(api_code)) {
    valid_names <- paste(names(config$geography_api_codes), collapse = ", ")
    stop(
      "Geography '", geography, "' is not supported by GeoCorr ", geocorr_version,
      ". Supported geographies: ", valid_names)
  }

  api_code
}


#' Obtain a GeoCorr Crosswalk
#'
#' Query GeoCorr (Missouri Census Data Center) for a crosswalk between two
#' geographies for all 51 states and Puerto Rico. Supports both GeoCorr 2022
#' (2020 Census geography) and GeoCorr 2018 (2010 Census geography).
#'
#' @details Note: this function is under development but does not yet support all
#'    of the geographies supported by GeoCorr.
#'
#'    For GeoCorr 2022: c("place", "county", "tract", "blockgroup", "zcta",
#'    "puma22", "cd119", "cd118").
#'
#'    For GeoCorr 2018: c("placefp", "county", "tract", "bg", "zcta5",
#'    "puma12", "cd115", "cd116").
#'
#'    User-facing names like "puma", "zcta", "place", "blockgroup" are
#'    automatically resolved to the correct API code for the version.
#'
#' @param source_geography Character. Source geography name.
#' @param target_geography Character. Target geography name.
#' @param weight Character. Weighting variable. One of c("population", "housing", "land").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#' @param geocorr_version Character. GeoCorr version to use. One of "2022" (default)
#'    or "2018". GeoCorr 2022 uses 2020 Census geography; GeoCorr 2018 uses 2010
#'    Census geography.
#'
#' @return A dataframe representing the requested GeoCorr crosswalk for all 51
#'    states and Puerto Rico.
#' @keywords internal
#' @noRd
get_geocorr_crosswalk <- function(
    source_geography,
    target_geography,
    weight = "population",
    cache = NULL,
    geocorr_version = "2022") {

  config <- get_geocorr_config(geocorr_version)

  # Resolve user-facing geography names to API codes
  source_api_code <- resolve_geocorr_geography(source_geography, geocorr_version)
  target_api_code <- resolve_geocorr_geography(target_geography, geocorr_version)

  outpath = "no file exists here"
  ## identify the relevant file paths for potentially-cached crosswalks
  if (!is.null(cache)) {
    outpath = file.path(
      cache,
      stringr::str_c("crosswalk_geocorr_", config$reference_year, "_to_",
                     config$reference_year, "_", source_geography, "_to_",
                     target_geography, "_weightedby_", weight, ".csv")) }

  ## if the file exists and the user does not wish to overwrite it
  if (file.exists(outpath) & !is.null(cache)) {
    result = readr::read_csv(outpath, show_col_types = FALSE)

    cw_message("Reading file from cache.")

    # Attach metadata to cached result
    attr(result, "crosswalk_metadata") <- list(
      data_source = "geocorr",
      data_source_full_name = stringr::str_c(
        "Geocorr ", config$reference_year, " (Missouri Census Data Center)"),
      api_endpoint = "https://mcdc.missouri.edu/cgi-bin/broker",
      documentation_url = config$documentation_url,
      source_geography = source_geography,
      target_geography = target_geography,
      weighting_variable = weight,
      reference_year = config$reference_year,
      retrieved_at = NA,
      cached = TRUE,
      cache_path = outpath,
      read_from_cache = TRUE)

    return(result) }

  # Base API URL
  base_url <- "https://mcdc.missouri.edu/cgi-bin/broker"

  if (is.null(weight)) {
    cw_message("Setting the default crosswalk weighting variable to: population.")
    weight = "population"
  }

  # Map weight parameter to API format
  weight_value <- config$weight_map[[weight]]
  if (is.null(weight_value)) {
    stop("Invalid weight: '", weight, "'. Must be one of: population, housing, land.")
  }

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

  # GeoCorr 2018 does not support Puerto Rico
  if (!config$include_pr) {
    states_data <- states_data[!stringr::str_detect(states_data, "^Pr")]
  }

  # Helper: build query params for a chunk of states
  build_geocorr_params <- function(states, config, source_api_code, target_api_code, weight_value) {
    params <- list(
      `_PROGRAM` = config$program,
      `_SERVICE` = "MCDC_long",
      `_debug` = "0",
      `g1_` = source_api_code,
      `g2_` = target_api_code,
      `wtvar` = weight_value,
      `nozerob` = "1",
      `lstfmt` = "html",
      `title` = "",
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
    # Add version-specific CSV output params and extra params
    params <- c(params, config$csv_params, config$extra_params)
    params
  }

  # Helper: perform a single geocorr query for a chunk of states
  fetch_geocorr_chunk <- function(states, config, source_api_code, target_api_code, weight_value, base_url) {
    params <- build_geocorr_params(states, config, source_api_code, target_api_code, weight_value)

    request <- httr2::request(base_url) |>
      httr2::req_url_query(!!!params, .multi = "explode")

    csv_path <- httr2::req_perform(request) |>
      httr2::resp_body_html() |>
      rvest::html_element("body") |>
      rvest::html_text2() |>
      stringr::str_extract("geocorr.*.csv") |>
      trimws()

    if (is.na(csv_path)) { stop("Unable to acquire the specified crosswalk; please file an issue.") }

    readr::read_csv(file.path("https://mcdc.missouri.edu", "temp", csv_path), show_col_types = FALSE) |>
      janitor::clean_names()
  }

  ## for block-level crosswalks, the maximum number of states per query is 13
  if ("block" %in% c(source_api_code, target_api_code)) {

    n = length(states_data) / 13
    groups = cut(seq_along(states_data), n, labels = FALSE)
    states_chunked = split(states_data, groups)

    df1 = purrr::map_dfr(
      states_chunked,
      ~ fetch_geocorr_chunk(.x, config, source_api_code, target_api_code, weight_value, base_url))} else {

    df1 = fetch_geocorr_chunk(states_data, config, source_api_code, target_api_code, weight_value, base_url) }

  # Build dynamic rename patterns based on config
  weight_rename_pattern <- paste(names(config$weight_rename), collapse = "|")

  df2 = df1 |>
    dplyr::slice(2:nrow(df1)) |>
    ## naming conventions for some geographies are inconsistent; we standardize
    dplyr::rename_with(
      .cols = dplyr::matches("zip_name"),
      .fn = ~ .x |> stringr::str_replace("zip", "zcta")) |>
    dplyr::rename_with(
      .cols = dplyr::matches(stringr::str_c(config$puma_col, "name")),
      .fn = ~ .x |> stringr::str_replace(
        stringr::str_c(config$puma_col, "name"),
        stringr::str_c(config$puma_col, "_name"))) |>
    dplyr::rename_with(
      .cols = dplyr::matches(weight_rename_pattern),
      .fn = ~ stringr::str_replace_all(.x, config$weight_rename))

  if (!"state" %in% colnames(df2)) {
    df2 = df2 |>
      dplyr::mutate(
       state = stringr::str_sub(county, 1, 2),
       county = stringr::str_sub(county, 3, 5)) }

  # Build the blockgroup column name used by the API for this version
  bg_api_col <- config$geography_api_codes[["blockgroup"]]

  df2 = df2 |>
    dplyr::mutate(
      ## data with blocks/block groups/tracts have differently structured/named columns
      ## we standardize here so that subsequent workflows are uniform
      dplyr::across(
        .cols = dplyr::matches("^block$"),
        .fns = ~ stringr::str_c(state, county, tract, block) |>
          stringr::str_remove_all("\\.")),
      dplyr::across(
        .cols = dplyr::matches(stringr::str_c("^", bg_api_col, "$")),
        .fns = ~ stringr::str_c(state, county, tract, !!rlang::sym(bg_api_col)) |>
          stringr::str_remove_all("\\.")),
      dplyr::across(
        .cols = dplyr::matches("^tract$"),
        .fns = ~ stringr::str_c(state, county, tract) |>
          stringr::str_remove_all("\\.")),
      dplyr::across(
        .cols = dplyr::matches(config$cd_pattern),
        .fns = ~ if ("stab" %in% colnames(df2)) {
          stringr::str_c(stab, "-", .x)
        } else {
          stringr::str_c(state, "-", .x)
        },
        .names = "{.col}_name"),
      dplyr::across(
        .cols = dplyr::matches(stringr::str_c("^", config$puma_col, "$")),
        .fns = ~ stringr::str_c(state, .x) ))

  # Create name columns: conditional on whether GeoCorr version provides them
  if (config$has_name_columns) {
    df2 = df2 |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::matches("^block$"),
          .fns = ~ stringr::str_c(county_name, " ", tract, block),
          .names = "block_name"),
        dplyr::across(
          .cols = dplyr::matches(stringr::str_c("^", bg_api_col, "$")),
          .fns = ~ stringr::str_c(county_name, " ", !!rlang::sym(bg_api_col)),
          .names = "{bg_api_col}_name"),
        dplyr::across(
          .cols = dplyr::matches("^tract$"),
          .fns = ~ stringr::str_c(county_name, " ", tract),
          .names = "tract_name"))
  } else {
    # GeoCorr 2018 does not return name columns; construct from IDs where possible
    if ("block" %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(block_name = block)
    }
    if (bg_api_col %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(bg_api_col, "_name") := !!rlang::sym(bg_api_col))
    }
    if ("tract" %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(tract_name = tract)
    }
    # For place, zcta, puma, county: use the GEOID as the name
    place_col <- config$geography_api_codes[["place"]]
    if (place_col %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(place_col, "_name") := !!rlang::sym(place_col))
    }
    zcta_col <- config$geography_api_codes[["zcta"]]
    if (zcta_col %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(zcta_col, "_name") := !!rlang::sym(zcta_col))
    }
    puma_col <- config$puma_col
    if (puma_col %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(puma_col, "_name") := !!rlang::sym(puma_col))
    }
    if ("county" %in% colnames(df2) && !"county_name" %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(
        county_name = dplyr::if_else(
          nchar(county) == 3,
          stringr::str_c(state, county),
          county))
    }
  }

  # Standardize GEOIDs: prepend state FIPS to county, place, and CD columns
  # when the state prefix is missing. GeoCorr responses inconsistently include
  # the state prefix depending on the query; this ensures standard Census GEOID
  # lengths (county=5, place=7, CD=4).
  place_api_col <- config$geography_api_codes[["place"]]
  cd_pattern_anchored <- stringr::str_c(config$cd_pattern, "$")

  df2 <- df2 |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^county$"),
        .fns = ~ dplyr::if_else(nchar(.x) == 3, stringr::str_c(state, .x), .x)),
      dplyr::across(
        .cols = dplyr::matches(stringr::str_c("^", place_api_col, "$")),
        .fns = ~ dplyr::if_else(nchar(.x) == 5, stringr::str_c(state, .x), .x)),
      dplyr::across(
        .cols = dplyr::matches(cd_pattern_anchored),
        .fns = ~ dplyr::if_else(nchar(.x) == 2, stringr::str_c(state, .x), .x)))

  # Build population/housing column names based on census year
  pop_col <- stringr::str_c("population_", config$census_year)
  housing_col <- stringr::str_c("housing_", config$census_year)

  df2 = df2 |>
    dplyr::rename_with(
      .cols = dplyr::matches("state|stab"),
      .fn = ~ stringr::str_replace_all(.x, c("state" = "state_fips", "stab" = "state_abbreviation"))) |>
    dplyr::select(
      dplyr::matches("state"),
      source_geoid = !!rlang::sym(source_api_code),
      target_geoid = !!rlang::sym(target_api_code),
      source_geography_name = !!stringr::str_c(source_api_code, "_name"),
      target_geography_name = !!stringr::str_c(target_api_code, "_name"),
      allocation_factor_source_to_target = afact,
      dplyr::any_of("afact2"),
      dplyr::any_of(c(housing_col, pop_col, "land_area_sqmi")))

  # Rename afact2 to allocation_factor_target_to_source if present (GeoCorr 2022 only)
  if ("afact2" %in% colnames(df2)) {
    df2 <- df2 |> dplyr::rename(allocation_factor_target_to_source = afact2)
  }

  df2 = df2 |>
    dplyr::mutate(
      ## tract-level geoids (or the component columns we use to create them) aren't consistently
      ## structured across tract-level crosswalks. in the case that we've accidentally created
      ## 13-character geoids (by duplicating the state fips), we drop that here
      source_geography = source_geography,
      source_geoid = dplyr::case_when(
        source_api_code == "tract" & nchar(source_geoid) == 13 ~ stringr::str_sub(source_geoid, 3, 13),
        TRUE ~ source_geoid),
      target_geography = target_geography,
      target_geoid = dplyr::case_when(
        target_api_code == "tract" & nchar(target_geoid) == 13 ~ stringr::str_sub(target_geoid, 3, 13),
        TRUE ~ target_geoid),
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

  # Attach metadata to result
  attr(df2, "crosswalk_metadata") <- list(
    data_source = "geocorr",
    data_source_full_name = stringr::str_c(
      "Geocorr ", config$reference_year, " (Missouri Census Data Center)"),
    api_endpoint = base_url,
    documentation_url = config$documentation_url,
    source_geography = source_geography,
    target_geography = target_geography,
    weighting_variable = weight,
    reference_year = config$reference_year,
    retrieved_at = Sys.time(),
    cached = !is.null(cache),
    cache_path = if (!is.null(cache)) outpath else NULL)

  return(df2)
}

utils::globalVariables(c("afact", "afact2", "county", "stab", "state"))
