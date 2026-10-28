#' Get a Longitudinal Tract Database (LTDB) Crosswalk
#'
#' Downloads and standardizes a tract crosswalk published by the Longitudinal
#' Tract Database (LTDB) at Brown University. The package uses LTDB only for
#' tract year pairs that IPUMS NHGIS does not publish. At present that is one
#' pair: 2010 tracts to 2000 tracts (the "Backwards LTDB" file).
#'
#' @details
#' **Where the file comes from.** The 2010 to 2000 crosswalk is published as a
#' zip archive (`interpolate_to_2000.zip`) on the LTDB downloads page,
#' <https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx>.
#' The archive holds a Stata file (`crosswalk_2010_2000.dta`) and a Stata
#' program. No registration is needed to download it. The Stata file has one
#' row per pair of a 2010 tract and a 2000 tract that share population, with
#' the columns `trtid10` (2010 tract GEOID), `trtid00` (2000 tract GEOID),
#' `weight` (share of the 2010 tract assigned to the 2000 tract; the shares
#' for each 2010 tract sum to 1), `changetype` (the kind of boundary change:
#' "1 to 1", "many to 1", "1 to many", or "many to many"), and three
#' attributes of the 2000 tract (`placefp00`, `ccflag00`, `msapmsa00`) that
#' this function drops.
#'
#' **How LTDB weights differ from NHGIS weights.** LTDB provides a single
#' weight per tract pair. Its method combines land area with census block
#' population (Logan, Xu, and Stults 2014). NHGIS builds its tract crosswalks
#' from census blocks and offers separate weights based on population,
#' households, housing units, and other counts. The two sources therefore
#' produce different allocation factors for the same pair of tracts, and the
#' user cannot choose the weighting variable for an LTDB crosswalk.
#'
#' **Citation.** The LTDB downloads page asks users to cite the US2010
#' Project webpage as the source of data and Logan, Xu, and Stults (2014) as
#' the technical reference. Like the NHGIS source, this function prints that
#' guidance each time it returns a crosswalk and records the page in the
#' `citation_url` metadata field.
#'
#' **Caching.** When `cache` is a directory, the standardized crosswalk is
#' written there as a CSV and read from that file on later calls. A CSV
#' previously written by this function can be placed in the cache directory to
#' avoid downloading from the LTDB server.
#'
#' @param source_geography Character. Source geography name. Must resolve to
#'   "tract".
#' @param target_geography Character. Target geography name. Must resolve to
#'   "tract".
#' @param source_year Character or numeric. Year of the source tracts (2010).
#' @param target_year Character or numeric. Year of the target tracts (2000).
#' @param cache Directory path or NULL. Where to save the standardized
#'   crosswalk. If NULL (default), the crosswalk is returned but not saved.
#'
#' @return A tibble with one row per source-target tract pair and columns:
#'   \describe{
#'     \item{source_geoid}{11-character 2010 tract GEOID}
#'     \item{target_geoid}{11-character 2000 tract GEOID}
#'     \item{source_geography_name, target_geography_name}{"tract"}
#'     \item{source_year, target_year}{"2010" and "2000"}
#'     \item{allocation_factor_source_to_target}{LTDB's `weight`: the share of
#'        the 2010 tract assigned to the 2000 tract}
#'     \item{weighting_factor}{"ltdb_area_and_population", a label for LTDB's
#'        single combined weight}
#'     \item{ltdb_change_type}{LTDB's description of the boundary change}
#'   }
#'   The tibble carries a `crosswalk_metadata` attribute describing its source.
#'
#' @references Logan, John R., Zengwang Xu, and Brian J. Stults. 2014.
#'   "Interpolating US Decennial Census Tract Data from as Early as 1970 to
#'   2010: A Longitudinal Tract Database." The Professional Geographer
#'   66(3): 412-420. \doi{10.1080/00330124.2014.905156}
#'
#' @keywords internal
#' @noRd
get_ltdb_crosswalk <- function(
    source_geography,
    target_geography,
    source_year,
    target_year,
    cache = NULL) {

  source_year_character <- as.character(source_year)
  target_year_character <- as.character(target_year)
  source_geography_standardized <- standardize_geography_for_chain(source_geography)
  target_geography_standardized <- standardize_geography_for_chain(target_geography)

  requested_crosswalk <- list_ltdb_crosswalks() |>
    dplyr::filter(
      source_geography == source_geography_standardized,
      target_geography == target_geography_standardized,
      source_year == source_year_character,
      target_year == target_year_character)

  if (nrow(requested_crosswalk) == 0) {
    stop(
      "The Longitudinal Tract Database (LTDB) crosswalks used by this package ",
      "cover only these combinations: ",
      list_ltdb_crosswalks() |>
        dplyr::mutate(label = stringr::str_c(
          source_year, " ", source_geography, " -> ", target_year, " ", target_geography)) |>
        dplyr::pull(label) |>
        paste(collapse = "; "),
      ". The requested combination (", source_year, " ", source_geography, " -> ",
      target_year, " ", target_geography, ") is not supported.",
      call. = FALSE)
  }

  cache_directory <- if (is.null(cache)) tempdir() else cache
  csv_path <- file.path(
    cache_directory,
    stringr::str_c(
      "crosswalk_ltdb_", source_year_character, "_to_", target_year_character, "_",
      source_geography_standardized, "_to_", target_geography_standardized, ".csv"))

  read_from_cache <- !is.null(cache) && file.exists(csv_path)

  if (read_from_cache) {
    crosswalk <- readr::read_csv(
      csv_path,
      col_types = readr::cols(
        .default = readr::col_character(),
        allocation_factor_source_to_target = readr::col_double()),
      show_col_types = FALSE)
  } else {
    download_directory <- tempfile("ltdb_")
    dir.create(download_directory)
    on.exit(unlink(download_directory, recursive = TRUE), add = TRUE)
    zip_path <- file.path(download_directory, basename(requested_crosswalk$download_url))

    # The LTDB server is a single university web server; retry transient
    # failures (time-outs, dropped connections) before giving up.
    tryCatch(
      httr2::request(requested_crosswalk$download_url) |>
        httr2::req_retry(max_tries = 4, retry_on_failure = TRUE) |>
        httr2::req_timeout(600) |>
        httr2::req_perform(path = zip_path),
      error = function(error) {
        stop(
          "Failed to download the LTDB crosswalk from ",
          requested_crosswalk$download_url, ": ", conditionMessage(error),
          " The LTDB downloads page is ",
          "https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx.",
          call. = FALSE)
      })

    extracted_files <- tryCatch(
      utils::unzip(
        zip_path,
        files = requested_crosswalk$file_in_archive,
        exdir = download_directory),
      error = function(error) character(0),
      warning = function(warning) character(0))

    if (length(extracted_files) == 0 || !file.exists(extracted_files[1])) {
      stop(
        "The downloaded LTDB archive does not contain the expected file '",
        requested_crosswalk$file_in_archive, "'. The LTDB may have changed how ",
        "it publishes this crosswalk; please file an issue at ",
        "https://github.com/UI-Research/crosswalk/issues.",
        call. = FALSE)
    }

    raw_crosswalk <- haven::read_dta(extracted_files[1])

    expected_columns <- c("trtid10", "trtid00", "weight", "changetype")
    missing_columns <- setdiff(expected_columns, names(raw_crosswalk))
    if (length(missing_columns) > 0) {
      stop(
        "The LTDB crosswalk file is missing expected columns: ",
        paste(missing_columns, collapse = ", "), ". The LTDB may have changed ",
        "the file format; please file an issue at ",
        "https://github.com/UI-Research/crosswalk/issues.",
        call. = FALSE)
    }

    crosswalk <- raw_crosswalk |>
      dplyr::transmute(
        source_geoid = as.character(trtid10),
        target_geoid = as.character(trtid00),
        source_geography_name = source_geography_standardized,
        target_geography_name = target_geography_standardized,
        source_year = source_year_character,
        target_year = target_year_character,
        allocation_factor_source_to_target = as.numeric(weight),
        weighting_factor = "ltdb_area_and_population",
        ltdb_change_type = as.character(haven::as_factor(changetype)))

    if (!is.null(cache)) {
      readr::write_csv(crosswalk, csv_path)
    }
  }

  # LTDB's weights for each source tract should sum to 1. Warn if they do not,
  # because interpolated totals would then not match the source totals.
  allocation_totals <- crosswalk |>
    dplyr::summarize(
      total_allocation = sum(allocation_factor_source_to_target, na.rm = TRUE),
      .by = source_geoid)
  source_tracts_not_summing_to_one <- allocation_totals |>
    dplyr::filter(abs(total_allocation - 1) > 0.001) |>
    nrow()
  if (source_tracts_not_summing_to_one > 0) {
    cw_warning(
      source_tracts_not_summing_to_one, " source tracts in the LTDB crosswalk have ",
      "allocation factors that do not sum to 1. Interpolated totals for these ",
      "tracts will not equal their original totals.",
      call. = FALSE)
  }

  weight_note <- stringr::str_c(
    "LTDB provides a single weight per tract pair, built from a combination of ",
    "land area and census block population (Logan, Xu, and Stults 2014). It ",
    "differs from NHGIS weights, which are built from census blocks with a ",
    "choice of population, household, or housing unit counts.")

  # Citation guidance, shown every time (including cache reads), in the same
  # form as the NHGIS citation message
  cw_message(
"Use of LTDB crosswalks requires citing the US2010 Project webpage as the source
of data, and Logan, Xu, and Stults (2014) as the technical reference.
See https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx.")

  cw_message(stringr::str_c(
    "This step uses the Longitudinal Tract Database (LTDB, Brown University) ",
    "crosswalk from ", source_year_character, " to ", target_year_character,
    " tracts. ", weight_note))

  attr(crosswalk, "crosswalk_metadata") <- list(
    data_source = "ltdb",
    data_source_full_name = "Longitudinal Tract Database (LTDB), Brown University",
    download_url = requested_crosswalk$download_url,
    # The downloads page is where LTDB states how to cite its data
    citation_url = "https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/DataList.aspx",
    documentation_url = "https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm",
    source_year = source_year_character,
    target_year = target_year_character,
    source_geography = source_geography,
    source_geography_standardized = source_geography_standardized,
    target_geography = target_geography,
    target_geography_standardized = target_geography_standardized,
    weighting_variable = "ltdb_area_and_population",
    state_coverage = "National (tracts in the LTDB file)",
    notes = c(
      weight_note,
      "The requested `weight` argument does not apply: LTDB has one weight.",
      stringr::str_c(
        "Citation: LTDB asks users to cite the US2010 Project webpage as the ",
        "source of data (the address LTDB gives, ",
        "http://www.s4.brown.edu/us2010/Researcher/Bridging.htm, no longer loads; ",
        "the current page is ",
        "https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm), ",
        "and the technical reference below."),
      stringr::str_c(
        "Technical reference: Logan, John R., Zengwang Xu, and Brian J. Stults. 2014. ",
        "Interpolating US Decennial Census Tract Data from as Early as 1970 to ",
        "2010: A Longitudinal Tract Database. The Professional Geographer ",
        "66(3): 412-420. https://doi.org/10.1080/00330124.2014.905156")),
    retrieved_at = if (read_from_cache) NA else Sys.time(),
    cached = !is.null(cache),
    cache_path = if (!is.null(cache)) csv_path else NULL,
    read_from_cache = read_from_cache)

  return(crosswalk)
}


#' List LTDB Crosswalks Used by This Package
#'
#' Returns the Longitudinal Tract Database crosswalks that `get_crosswalk()`
#' can route to. LTDB is used only for tract year pairs that NHGIS does not
#' publish, so this list excludes LTDB files that duplicate NHGIS coverage
#' (for example, LTDB's 2000 to 2010 and 2020 to 2010 files).
#'
#' This table is the single place that defines LTDB coverage; the chain
#' planner, `get_available_crosswalks()`, and `get_ltdb_crosswalk()` all read
#' it.
#'
#' @return A tibble with columns `source_geography`, `source_year`,
#'   `target_geography`, `target_year` (all character), `download_url`, and
#'   `file_in_archive` (the file inside the downloaded zip archive).
#' @keywords internal
#' @noRd
list_ltdb_crosswalks <- function() {
  tibble::tibble(
    source_geography = "tract",
    source_year = "2010",
    target_geography = "tract",
    target_year = "2000",
    download_url = "https://s4.ad.brown.edu/Projects/Diversity/Researcher/LTBDDload/Dfiles/interpolate_to_2000.zip",
    file_in_archive = "crosswalk_2010_2000.dta")
}

utils::globalVariables(c(
  "trtid10", "trtid00", "weight", "changetype", "total_allocation",
  "download_url", "file_in_archive"))
