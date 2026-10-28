#' Conditionally Emit a Message
#'
#' Wrapper around `message()` that respects the `crosswalk.silent` option.
#' When `getOption("crosswalk.silent")` is `TRUE`, the message is suppressed.
#'
#' @param ... Arguments passed to `message()`.
#' @keywords internal
#' @noRd
cw_message <- function(...) {
  if (!getOption("crosswalk.silent", FALSE)) message(...)
}

#' Conditionally Emit a Warning
#'
#' Wrapper around `warning()` that respects the `crosswalk.silent` option.
#' When `getOption("crosswalk.silent")` is `TRUE`, the warning is suppressed.
#'
#' @param ... Arguments passed to `warning()`.
#' @param call. Logical. Whether to include the call in the warning message.
#' @param immediate. Logical. Whether to print the warning immediately.
#' @keywords internal
#' @noRd
cw_warning <- function(..., call. = TRUE, immediate. = FALSE) {
  if (!getOption("crosswalk.silent", FALSE)) warning(..., call. = call., immediate. = immediate.)
}

#' Validate a User-Provided Cache Directory
#'
#' Errors if `cache` is non-NULL and does not refer to an existing directory.
#' Users must create the directory themselves; the package will not create it
#' silently, so a typo in the path is surfaced rather than producing an
#' unexpected new directory.
#'
#' @param cache The user-supplied cache path (or NULL).
#' @keywords internal
#' @noRd
validate_cache_dir <- function(cache) {
  if (is.null(cache)) return(invisible(NULL))
  if (!is.character(cache) || length(cache) != 1L || is.na(cache) || !nzchar(cache)) {
    stop("`cache` must be NULL or a single non-empty character path.", call. = FALSE)
  }
  if (!dir.exists(cache)) {
    stop(
      "The cache directory does not exist: '", cache, "'. ",
      "Create it (e.g., `dir.create(\"", cache, "\", recursive = TRUE)`) ",
      "or pass `cache = NULL` to skip caching.",
      call. = FALSE)
  }
  invisible(NULL)
}

#' Weights Offered by GeoCorr Crosswalks
#'
#' GeoCorr builds its allocation factors from one of three attributes.
#'
#' @return Character vector of the `weight` values GeoCorr accepts.
#' @keywords internal
#' @noRd
get_geocorr_weights <- function() {
  c("population", "housing", "land")
}

#' Map `weight` Values to NHGIS Weighting Factors
#'
#' NHGIS crosswalk files carry several interpolation weights. After
#' `get_nhgis_crosswalk()` renames them, each weight appears as a value of the
#' `weighting_factor` column. This function maps each accepted `weight`
#' argument to the `weighting_factor` values that can serve it, in order of
#' preference.
#'
#' Block group and tract based NHGIS files carry separate weights for
#' population (`wt_pop`), households (`wt_hh`), all housing units (`wt_hu`),
#' owner-occupied housing units (`wt_ownhu`), and renter-occupied housing units
#' (`wt_renthu`). Block-based files carry a single combined weight, which the
#' package labels `housing_population`; it is accepted as a fallback for
#' `"population"` and `"housing"` only. The household and tenure weights have
#' no fallback, so requesting them from a file that lacks them is an error.
#'
#' @return A named list. Names are accepted `weight` values; elements are
#'   character vectors of `weighting_factor` values, most preferred first.
#' @keywords internal
#' @noRd
get_nhgis_weight_map <- function() {
  list(
    "population" = c("population", "housing_population"),
    "housing" = c("housing_all", "housing_population"),
    "land" = c("landarea"),
    "household" = c("household"),
    "housing_owned" = c("housing_owned"),
    "housing_rented" = c("housing_rented"))
}

#' Check That Every Step of a Crosswalk Plan Can Use the Requested Weight
#'
#' Runs before any crosswalk is downloaded, so that a request that cannot be
#' served fails immediately rather than after a partial download. GeoCorr
#' steps accept only `get_geocorr_weights()`. NHGIS steps accept every name in
#' `get_nhgis_weight_map()`; whether a given NHGIS file actually carries the
#' requested weight is checked after download, in `get_crosswalk_single()`.
#' CT Data Collaborative and county-events steps do not offer a choice of
#' weight, so any accepted value is allowed and the weight actually used is
#' recorded in the metadata.
#'
#' @param weight The `weight` argument passed to `get_crosswalk()`.
#' @param plan A plan returned by `plan_crosswalk_chain()`.
#' @return `invisible(NULL)`; raises an error when the weight cannot be used.
#' @keywords internal
#' @noRd
validate_weight_for_plan <- function(weight, plan) {
  all_weights <- names(get_nhgis_weight_map())

  if (!is.character(weight) || length(weight) != 1L || is.na(weight) ||
      !weight %in% all_weights) {
    stop(
      "Invalid weight: '", paste(weight, collapse = ", "), "'. Must be one of: ",
      paste(all_weights, collapse = ", "), ". GeoCorr crosswalks accept ",
      paste(get_geocorr_weights(), collapse = ", "), " only.",
      call. = FALSE)
  }

  geocorr_steps <- plan$steps |>
    dplyr::filter(
      crosswalk_source == "geocorr",
      !weight %in% get_geocorr_weights())

  if (nrow(geocorr_steps) > 0) {
    total_steps <- nrow(plan$steps)
    step_labels <- purrr::map_chr(
      seq_len(nrow(geocorr_steps)),
      \(row) stringr::str_c(
        "step ", geocorr_steps$step_number[row], " of ", total_steps,
        " (", geocorr_steps$description[row], ")"))

    stop(
      "The requested weight '", weight, "' is not available from GeoCorr, ",
      "which serves ", paste(step_labels, collapse = " and "), ". GeoCorr ",
      "crosswalks offer these weights: ",
      paste(get_geocorr_weights(), collapse = ", "), ". The household and ",
      "housing tenure weights ('household', 'housing_owned', ",
      "'housing_rented') are only available from NHGIS block group and ",
      "tract crosswalks that change the census year in a single step.",
      call. = FALSE)
  }

  invisible(NULL)
}
