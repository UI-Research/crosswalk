#' Get GeoCorr Version Configuration
#'
#' Returns configuration for a specific GeoCorr version (2022, 2018, or 2000).
#'
#' @param version Character. One of "2022", "2018", or "2000".
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
        "cd119" = "cd119",
        "aiannh" = "aiannh",
        "tribal" = "aiannh",
        "tribal_area" = "aiannh",
        "american_indian_area" = "aiannh"),
      puma_col = "puma22",
      cd_pattern = "^cd11[89]",
      has_name_columns = TRUE,
      ## columns holding real geography names, as new name = name in the
      ## GeoCorr CSV (after janitor::clean_names); none needed for 2022
      name_columns = character(0),
      include_pr = TRUE,
      ## GeoCorr limits how many states a block-level request may include
      block_states_per_request = 13,
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
      name_columns = character(0),
      include_pr = FALSE,
      block_states_per_request = 13,
      # GeoCorr 2018 uses csvout=1 for CSV output (different from 2022)
      csv_params = list(`csvout` = "1"),
      extra_params = list(`namoptf` = "b", `namoptr` = "b"))
  } else if (version == "2000") {
    ## GeoCorr 2000 uses Census 2000 geography. Its web form offers PUMAs for
    ## the 5% sample ("puma5") only; requests for 1% sample PUMAs ("puma1")
    ## return no rows. Metro areas are the 1999 Metropolitan Statistical Areas
    ## and Consolidated Metropolitan Statistical Areas ("msacmsa").
    list(
      program = "apps.geocorr2000.sas",
      documentation_url = "https://mcdc.missouri.edu/applications/geocorr2000.html",
      reference_year = "2000",
      census_year = "2000",
      weight_map = list(
        "population" = "pop2k",
        "housing" = "hus2k",
        "land" = "LandSQMI"),
      ## janitor::clean_names() turns "LandSQMI" into "land_sqmi"
      weight_rename = c(
        "hus2k" = "housing_2000",
        "pop2k" = "population_2000",
        "land_sqmi" = "land_area_sqmi"),
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
        "puma" = "puma5",
        "puma5" = "puma5",
        "msacmsa" = "msacmsa"),
      puma_col = "puma5",
      ## congressional districts are not offered for GeoCorr 2000 in this
      ## package; this pattern matches the codes GeoCorr 2000 would use
      cd_pattern = "^cd1(06|08|09|10)",
      has_name_columns = FALSE,
      name_columns = c(
        "county_name" = "cntyname",
        "zcta5_name" = "zipname",
        "placefp_name" = "placenm",
        "msacmsa_name" = "msaname"),
      ## GeoCorr 2000 has no Puerto Rico data
      include_pr = FALSE,
      block_states_per_request = 10,
      csv_params = list(`csvout` = "1"),
      ## GeoCorr 2000 fails unless the urban area filter field (uaucs) is sent,
      ## even when empty; afacts2 adds target-to-source allocation factors
      extra_params = list(
        `namoptf` = "b", `namoptr` = "b", `uaucs` = "", `afacts2` = "on"))
  } else {
    stop(
      "Invalid GeoCorr version: '", version,
      "'. Must be '2022', '2018', or '2000'.")
  }
}


#' Resolve Geography Name to GeoCorr API Code
#'
#' Translates a user-facing geography name to the appropriate API parameter
#' for the specified GeoCorr version.
#'
#' @param geography Character. User-facing geography name.
#' @param geocorr_version Character. "2022", "2018", or "2000".
#' @return Character. The API code for the geography.
#' @keywords internal
#' @noRd
resolve_geocorr_geography <- function(geography, geocorr_version = "2022") {
  config <- get_geocorr_config(geocorr_version)
  api_code <- config$geography_api_codes[[geography]]

  if (is.null(api_code)) {
    valid_names <- paste(names(config$geography_api_codes), collapse = ", ")
    ## PUMAs for the 2000 1% sample are listed in older GeoCorr 2000
    ## documentation, but the current web tool returns no rows for them
    if (identical(geography, "puma1")) {
      stop(
        "PUMAs for the 2000 1% sample ('puma1') are not available: the ",
        "GeoCorr 2000 web tool returns no rows for them. Use 'puma5' (PUMAs ",
        "for the 2000 5% sample) instead.")
    }
    stop(
      "Geography '", geography, "' is not supported by GeoCorr ", geocorr_version,
      ". Supported geographies: ", valid_names)
  }

  api_code
}


#' Obtain a GeoCorr Crosswalk
#'
#' Query GeoCorr (Missouri Census Data Center) for a crosswalk between two
#' geographies for all 51 states and Puerto Rico. Supports GeoCorr 2022
#' (2020 Census geography), GeoCorr 2018 (2010 Census geography), and GeoCorr
#' 2000 (2000 Census geography).
#'
#' @details Note: this function is under development but does not yet support all
#'    of the geographies supported by GeoCorr.
#'
#'    For GeoCorr 2022: c("place", "county", "tract", "blockgroup", "zcta",
#'    "puma22", "cd119", "cd118", "aiannh"). Note that "aiannh" (tribal areas)
#'    is supported as a **target** geography only, not as a source.
#'
#'    For GeoCorr 2018: c("placefp", "county", "tract", "bg", "zcta5",
#'    "puma12", "cd115", "cd116"). Tribal areas (aiannh) are not available in
#'    GeoCorr 2018.
#'
#'    For GeoCorr 2000: c("placefp", "county", "tract", "bg", "block",
#'    "zcta5", "puma5", "msacmsa"). "puma5" is the PUMA for the 2000 5% sample
#'    (the PUMA used by 2000 Census and 2005-2011 ACS microdata). "msacmsa" is
#'    the 1999 Metropolitan Statistical Area or Consolidated Metropolitan
#'    Statistical Area, identified by its four-digit code; GeoCorr 2000 assigns
#'    the code "9999" to areas outside any metro area. GeoCorr 2000 has no
#'    Puerto Rico data.
#'
#'    User-facing names like "puma", "zcta", "place", "blockgroup" are
#'    automatically resolved to the correct API code for the version.
#'
#' @param source_geography Character. Source geography name.
#' @param target_geography Character. Target geography name.
#' @param weight Character. Weighting variable. One of c("population", "housing", "land").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#' @param geocorr_version Character. GeoCorr version to use. One of "2022"
#'    (default), "2018", or "2000". GeoCorr 2022 uses 2020 Census geography;
#'    GeoCorr 2018 uses 2010 Census geography; GeoCorr 2000 uses 2000 Census
#'    geography.
#'
#' @return A dataframe representing the requested GeoCorr crosswalk for all 51
#'    states and Puerto Rico (GeoCorr 2018 and GeoCorr 2000 exclude Puerto
#'    Rico).
#' @importFrom rlang :=
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

  # Tribal areas (aiannh) are currently supported as a target geography only.
  # Sourcing from aiannh would require re-normalizing allocation factors across
  # per-state query chunks for tribal areas that span state boundaries.
  if (source_api_code == "aiannh") {
    stop(
      "Tribal areas ('aiannh') are currently supported as a target geography ",
      "only, not as a source. Please specify 'aiannh' via `target_geography`.")
  }

  outpath = "no file exists here"
  ## identify the relevant file paths for potentially-cached crosswalks
  if (!is.null(cache)) {
    outpath = file.path(
      cache,
      stringr::str_c("crosswalk_geocorr_", config$reference_year, "_to_",
                     config$reference_year, "_", source_geography, "_to_",
                     target_geography, "_weightedby_", weight, ".csv")) }

  geocorr_numeric_columns <- c(
    "allocation_factor_source_to_target", "allocation_factor_target_to_source",
    "population_2020", "housing_2020", "population_2010", "housing_2010",
    "population_2000", "housing_2000", "land_area_sqmi")

  ## if the file exists and the user does not wish to overwrite it
  if (file.exists(outpath) & !is.null(cache)) {
    ## read GEOIDs as character (type-guessing can strip leading zeros)
    result = readr::read_csv(
      outpath,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of(geocorr_numeric_columns), as.numeric))

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
    list(abbr = "Pr", fips = "72"))

  ## stab (uppercase 2-letter abbreviation) → 2-char state FIPS lookup; used as
  ## a fallback when GeoCorr returns `stab` but neither `state` nor `county`
  state_fips_lookup <- tibble::tibble(
    stab = purrr::map_chr(states_data, ~ toupper(.x$abbr)),
    state = purrr::map_chr(states_data, ~ .x$fips))

  states_data <- purrr::map_chr(states_data, ~ paste0(.x$abbr, .x$fips))

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

    ## GeoCorr's server (mcdc.missouri.edu) intermittently times out or drops
    ## connections, which otherwise fails the whole fetch -- and any vignette or
    ## package build that relies on it -- on a single flaky request. Retry
    ## transient failures (including low-level connection/SSL timeouts) with
    ## backoff. Used for both the query request and the CSV download below.
    perform_with_retry <- function(request, path = NULL) {
      request |>
        httr2::req_retry(max_tries = 4, retry_on_failure = TRUE) |>
        httr2::req_perform(path = path)
    }

    response_html <- httr2::request(base_url) |>
      httr2::req_url_query(!!!params, .multi = "explode") |>
      perform_with_retry() |>
      httr2::resp_body_html()

    response_text <- response_html |>
      rvest::html_element("body") |>
      rvest::html_text2()

    ## GeoCorr reports "0 observations" (and links to a file that does not
    ## exist) when a request is valid but matches nothing
    if (stringr::str_detect(response_text, "\\b0 observations on output")) {
      stop(
        "GeoCorr ", config$reference_year, " returned no rows for this ",
        "request (", source_api_code, " to ", target_api_code, ").")
    }

    ## GeoCorr 2022 and 2018 print the CSV file name in the page text;
    ## GeoCorr 2000 shows it only in the link address
    csv_path_from_text <- response_text |>
      stringr::str_extract("geocorr.*.csv") |>
      trimws()

    csv_path <- if (!is.na(csv_path_from_text)) {
      csv_path_from_text
    } else {
      response_html |>
        rvest::html_elements("a") |>
        rvest::html_attr("href") |>
        stringr::str_extract("geocorr[^/]*\\.csv$") |>
        stats::na.omit() |>
        dplyr::first(default = NA_character_)
    }

    if (is.na(csv_path)) { stop("Unable to acquire the specified crosswalk; please file an issue.") }

    ## stream the CSV to a temp file (byte-identical to reading the URL directly)
    ## so the download is retried on the same transient failures as the query
    csv_file <- tempfile(fileext = ".csv")
    httr2::request(file.path("https://mcdc.missouri.edu", "temp", csv_path)) |>
      perform_with_retry(path = csv_file)

    readr::read_csv(csv_file, show_col_types = FALSE) |>
      janitor::clean_names() |>
      ## the first row of every GeoCorr CSV is a human-readable label row;
      ## drop it here so chunked (multi-request) queries don't retain the
      ## label rows of chunks after the first
      dplyr::slice(-1)
  }

  ## for block-level crosswalks, GeoCorr limits the number of states per
  ## query (13 for GeoCorr 2022 and 2018; 10 for GeoCorr 2000)
  if ("block" %in% c(source_api_code, target_api_code)) {

    states_chunked = split(
      states_data,
      ceiling(seq_along(states_data) / config$block_states_per_request))

    df1 = purrr::map_dfr(
      states_chunked,
      ~ fetch_geocorr_chunk(.x, config, source_api_code, target_api_code, weight_value, base_url))} else {

    df1 = fetch_geocorr_chunk(states_data, config, source_api_code, target_api_code, weight_value, base_url) }

  # Build dynamic rename patterns based on config
  weight_rename_pattern <- paste(names(config$weight_rename), collapse = "|")

  df2 = df1 |>
    ## GeoCorr 2000 returns real geography names under its own column names;
    ## rename them to the "{geography code}_name" pattern used below
    dplyr::rename(dplyr::any_of(config$name_columns)) |>
    ## naming conventions for some geographies are inconsistent; we standardize
    dplyr::rename_with(
      .cols = dplyr::matches("zip_name"),
      .fn = ~ .x |> stringr::str_replace("zip", "zcta")) |>
    ## defensive: ensure aiannh name column is snake_cased regardless of how
    ## janitor::clean_names handled the original aiannhName field
    dplyr::rename_with(
      .cols = dplyr::matches("^aiannhname$"),
      .fn = ~ stringr::str_replace(.x, "aiannhname", "aiannh_name")) |>
    dplyr::rename_with(
      .cols = dplyr::matches(stringr::str_c(config$puma_col, "name")),
      .fn = ~ .x |> stringr::str_replace(
        stringr::str_c(config$puma_col, "name"),
        stringr::str_c(config$puma_col, "_name"))) |>
    dplyr::rename_with(
      .cols = dplyr::matches(weight_rename_pattern),
      .fn = ~ stringr::str_replace_all(.x, config$weight_rename))

  ## Ensure a 2-char `state` FIPS column exists. GeoCorr's response varies by
  ## query: sometimes `state` is returned directly; sometimes it's only
  ## available as the leading 2 chars of a 5-char `county` (SSCCC) field;
  ## sometimes (e.g., place/zcta/puma sources targeting aiannh) neither is
  ## present and we must derive it from `stab` (state abbreviation).
  if (!"state" %in% colnames(df2)) {
    if ("county" %in% colnames(df2)) {
      df2 = df2 |>
        dplyr::mutate(
          state = stringr::str_sub(county, 1, 2),
          county = stringr::str_sub(county, 3, 5))
    } else if ("stab" %in% colnames(df2)) {
      df2 = df2 |>
        dplyr::left_join(state_fips_lookup, by = "stab")
    }
  }

  # Build the blockgroup column name used by the API for this version
  bg_api_col <- config$geography_api_codes[["blockgroup"]]

  df2 = df2 |>
    dplyr::mutate(
      ## data with blocks/block groups/tracts have differently structured/named columns
      ## we standardize here so that subsequent workflows are uniform
      dplyr::across(
        .cols = dplyr::matches("^block$"),
        .fns = ~ stringr::str_c(build_tract_geoid(state, county, tract), .x)),
      dplyr::across(
        .cols = dplyr::matches(stringr::str_c("^", bg_api_col, "$")),
        .fns = ~ stringr::str_c(build_tract_geoid(state, county, tract), .x)),
      dplyr::across(
        .cols = dplyr::matches("^tract$"),
        .fns = ~ build_tract_geoid(state, county, tract)),
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
    ## (GeoCorr 2000 already provides real names for some of these; keep them)
    place_col <- config$geography_api_codes[["place"]]
    if (place_col %in% colnames(df2) &&
        !stringr::str_c(place_col, "_name") %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(place_col, "_name") := !!rlang::sym(place_col))
    }
    zcta_col <- config$geography_api_codes[["zcta"]]
    if (zcta_col %in% colnames(df2) &&
        !stringr::str_c(zcta_col, "_name") %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(zcta_col, "_name") := !!rlang::sym(zcta_col))
    }
    puma_col <- config$puma_col
    if (puma_col %in% colnames(df2) &&
        !stringr::str_c(puma_col, "_name") %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(!!stringr::str_c(puma_col, "_name") := !!rlang::sym(puma_col))
    }
    if ("county" %in% colnames(df2) && !"county_name" %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(
        county_name = dplyr::if_else(
          nchar(county) == 3,
          stringr::str_c(state, county),
          county))
    }
    if ("msacmsa" %in% colnames(df2) && !"msacmsa_name" %in% colnames(df2)) {
      df2 = df2 |> dplyr::mutate(msacmsa_name = msacmsa)
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
      dplyr::across(.cols = dplyr::any_of(geocorr_numeric_columns), .fns = as.numeric))

  ## When target is aiannh, GeoCorr returns rows with NA target_geoid for the
  ## portion of each source geography that falls outside any tribal area.
  ## These rows carry no crosswalk information (there is no tribal area to
  ## allocate to) so we drop them. After this filter, allocation factors per
  ## source sum to <= 1 rather than exactly 1, which correctly reflects that
  ## tribal areas do not cover all land.
  if (target_api_code == "aiannh") {
    df2 <- df2 |> dplyr::filter(!is.na(target_geoid))
  }

  ## GeoCorr 2000 leaves the MSA/CMSA code blank in some states. In New
  ## England, 1999 metro areas were built from cities and towns rather than
  ## whole counties, and GeoCorr 2000 does not assign them to these rows.
  ## Rows without a code cannot be allocated, so we drop them and say so.
  if ("msacmsa" %in% c(source_api_code, target_api_code)) {
    missing_metro_rows <- df2 |>
      dplyr::filter(is.na(source_geoid) | is.na(target_geoid))

    if (nrow(missing_metro_rows) > 0) {
      missing_metro_states <- if ("state_fips" %in% colnames(missing_metro_rows)) {
        missing_metro_rows$state_fips |> unique() |> sort() |> paste(collapse = ", ")
      } else {
        "unknown"
      }
      cw_warning(stringr::str_c(
        "GeoCorr ", config$reference_year, " has no MSA/CMSA code for ",
        nrow(missing_metro_rows), " rows (state FIPS codes: ",
        missing_metro_states, "). These rows were dropped, so the affected ",
        "areas are missing from the crosswalk or have allocation factors that ",
        "sum to less than 1. In New England, 1999 ",
        "metro areas were defined with cities and towns, which GeoCorr 2000 ",
        "does not assign to an MSA/CMSA code."), call. = FALSE)

      df2 <- df2 |>
        dplyr::filter(!is.na(source_geoid), !is.na(target_geoid))
    }
  }

  if (!is.null(cache) && !file.exists(outpath)) {
    readr::write_csv(df2, outpath)
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

#' Build an 11-Character Census Tract GEOID
#'
#' Combines a state FIPS code, a county code, and a tract code into the
#' standard 11-character tract GEOID (2-digit state, 3-digit county, 6-digit
#' tract). GeoCorr writes tract codes with a decimal point (for example,
#' "0301.00" or "1234.56"); the decimal point is removed. Tract codes written
#' without a decimal point and with four or fewer digits (for example, "0301")
#' are treated as whole tract numbers and receive the "00" suffix. County codes
#' may be either the 3-digit county code or the 5-digit state-and-county code;
#' only the last three digits are used.
#'
#' @param state Character vector of 2-digit state FIPS codes.
#' @param county Character vector of 3-digit county codes or 5-digit
#'    state-and-county codes.
#' @param tract Character vector of tract codes, such as "0301.00".
#' @return Character vector of 11-character tract GEOIDs (NA where any input
#'    is NA).
#' @keywords internal
#' @noRd
build_tract_geoid <- function(state, county, tract) {
  county_code <- stringr::str_sub(county, -3, -1)

  tract_whole_number <- stringr::str_extract(tract, "^[0-9]+")
  tract_suffix <- stringr::str_match(tract, "\\.([0-9]+)$")[, 2]

  tract_code <- dplyr::case_when(
    stringr::str_detect(tract, "\\.") ~ stringr::str_c(
      stringr::str_pad(tract_whole_number, width = 4, side = "left", pad = "0"),
      stringr::str_pad(tract_suffix, width = 2, side = "right", pad = "0")),
    nchar(tract) == 6 ~ tract,
    TRUE ~ stringr::str_c(
      stringr::str_pad(tract, width = 4, side = "left", pad = "0"), "00"))

  stringr::str_c(state, county_code, tract_code)
}

utils::globalVariables(c(
  "afact", "afact2", "block", "county", "msacmsa", "stab", "state", "tract"))
