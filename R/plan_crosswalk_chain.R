#' Plan a Crosswalk Chain
#'
#' Internal function that analyzes source and target geography/year combinations
#' to determine the sequence of crosswalks needed. Returns a plan object describing
#' the chain without fetching any data.
#'
#' @param source_geography Character. Source geography name.
#' @param target_geography Character. Target geography name.
#' @param source_year Numeric or NULL. Year of the source geography.
#' @param target_year Numeric or NULL. Year of the target geography.
#' @param weight Character or NULL. Weighting variable for Geocorr crosswalks.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{is_multi_step}{Logical. TRUE if multiple crosswalks are needed.}
#'     \item{steps}{A tibble describing each step with columns: step_number,
#'       source_geography, source_year, target_geography, target_year,
#'       crosswalk_source, description.}
#'     \item{intermediate_geography}{Character or NULL. The pivot geography
#'       between steps (if multi-step).}
#'     \item{intermediate_year}{Numeric or NULL. The pivot year between steps
#'       (if multi-step).}
#'     \item{composition_note}{Character. Explanation of how to compose
#'       allocation factors.}
#'     \item{error}{Character or NULL. Error message if the chain is not possible.}
#'   }
#'
#' @details
#' Multi-step crosswalks use a year-first approach:
#' 1. Step 1 (NHGIS): Change year while keeping geography constant
#' 2. Step 2 (Geocorr): Change geography at the target year
#'
#' This approach works because NHGIS supports inter-temporal crosswalks for
#' base Census geographies (block, block group, tract, county), while Geocorr
#' has broader geography coverage (ZCTA, PUMA, place, etc.).
#'
#' @keywords internal
#' @noRd
plan_crosswalk_chain <- function(
    source_geography,
    target_geography,
    source_year = NULL,
    target_year = NULL,
    weight = NULL) {

  # Initialize result structure
  result <- list(
    is_multi_step = FALSE,
    steps = tibble::tibble(),
    intermediate_geography = NULL,
    intermediate_year = NULL,
    composition_note = NULL,
    error = NULL)

  # Standardize geography names for comparison
  source_geog_std <- standardize_geography_for_chain(source_geography)
  target_geog_std <- standardize_geography_for_chain(target_geography)


  # Convert years to character for consistent handling
  # Use NA_character_ instead of NULL for tibble compatibility
  source_year_chr <- if (!is.null(source_year)) as.character(source_year) else NA_character_
  target_year_chr <- if (!is.null(target_year)) as.character(target_year) else NA_character_

  # Determine what kind of crosswalk is needed
  geography_changes <- !isTRUE(source_geog_std == target_geog_std)
  year_changes <- !is.na(source_year_chr) && !is.na(target_year_chr) &&
    !isTRUE(source_year_chr == target_year_chr)

  # Case 1: Same geography, same year (or no years) - no crosswalk needed
  if (!geography_changes && !year_changes) {
    result$steps <- tibble::tibble(
      step_number = 0L,
      source_geography = source_geography,
      source_year = source_year_chr,
      target_geography = target_geography,
      target_year = target_year_chr,
      crosswalk_source = "none",
      description = "No crosswalk needed: source and target are identical")
    result$composition_note <- "No composition needed."
    return(result)
  }

  # Case 2: Same geography, different years - single NHGIS crosswalk
  if (!geography_changes && year_changes) {
    result$steps <- tibble::tibble(
      step_number = 1L,
      source_geography = source_geography,
      source_year = source_year_chr,
      target_geography = target_geography,
      target_year = target_year_chr,
      crosswalk_source = determine_temporal_source(source_year_chr, target_year_chr),
      description = stringr::str_c(
        source_year_chr, " ", source_geog_std, " -> ",
        target_year_chr, " ", target_geog_std, " (inter-temporal)"))
    result$composition_note <- "Single crosswalk; use allocation_factor_source_to_target directly."
    return(result)
  }

  # Case 3: Different geography, same year (or no years) - single Geocorr crosswalk
  if (geography_changes && !year_changes) {
    ref_year <- if (!is.na(target_year_chr)) target_year_chr else "2022"
    result$steps <- tibble::tibble(
      step_number = 1L,
      source_geography = source_geography,
      source_year = source_year_chr,
      target_geography = target_geography,
      target_year = target_year_chr,
      crosswalk_source = "geocorr",
      description = stringr::str_c(
        source_geog_std, " -> ", target_geog_std, " (inter-geography, ", ref_year, ")"))
    result$composition_note <- "Single crosswalk; use allocation_factor_source_to_target directly."
    return(result)
  }

  # Case 4: Different geography AND different year - multi-step required
  result$is_multi_step <- TRUE

  # Check if NHGIS supports the source geography for inter-temporal crosswalk
  nhgis_temporal_geographies <- c("block", "block_group", "tract", "county")
  source_supports_temporal <- source_geog_std %in% nhgis_temporal_geographies

  if (!source_supports_temporal) {
    # Cannot do year-first approach; source geography not supported by NHGIS
    result$error <- stringr::str_c(
      "Multi-step crosswalk not possible: NHGIS does not support inter-temporal ",
      "crosswalks for '", source_geography, "'. NHGIS only supports: ",
      paste(nhgis_temporal_geographies, collapse = ", "), ". ",
      "Consider using a different source geography or performing the crosswalk ",
      "in a different order manually.")
    return(result)
  }

  # Determine the intermediate point (year-first approach)
  # Step 1: source_geog(source_year) -> source_geog(target_year) via NHGIS
  # Step 2: source_geog(target_year) -> target_geog(target_year) via Geocorr

  result$intermediate_geography <- source_geography[1]
  result$intermediate_year <- target_year_chr[1]

  # Determine temporal crosswalk source for step 1
  step1_source <- determine_temporal_source(source_year_chr[1], target_year_chr[1])

  # Use first element to ensure scalar values
  src_geog <- source_geography[1]
  tgt_geog <- target_geography[1]
  src_year <- source_year_chr[1]
  tgt_year <- target_year_chr[1]
  src_std <- source_geog_std[1]
  tgt_std <- target_geog_std[1]

  result$steps <- tibble::tibble(
    step_number = c(1L, 2L),
    source_geography = c(src_geog, src_geog),
    source_year = c(src_year, tgt_year),
    target_geography = c(src_geog, tgt_geog),
    target_year = c(tgt_year, tgt_year),
    crosswalk_source = c(step1_source, "geocorr"),
    description = c(
      stringr::str_c(
        src_year, " ", src_std, " -> ",
        tgt_year, " ", src_std, " (inter-temporal via ", step1_source, ")"),
      stringr::str_c(
        tgt_year, " ", src_std, " -> ",
        tgt_year, " ", tgt_std, " (inter-geography via Geocorr)")))

  result$composition_note <- stringr::str_c(
    "Compose crosswalks by joining on intermediate geography (",
    src_std, " ", tgt_year, ") and multiplying allocation factors: ",
    "final_allocation = step1_allocation * step2_allocation")

  return(result)
}


#' Standardize Geography Name for Chain Planning
#'
#' Simplified geography standardization for internal chain planning.
#'
#' @param geography Character. Geography name to standardize.
#' @return Character. Standardized geography name.
#' @keywords internal
#' @noRd
standardize_geography_for_chain <- function(geography) {
  geography <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("_", " ")

  dplyr::case_when(
    geography %in% c("blk", "block", "blocks", "census block") ~ "block",
    geography %in% c("bg", "blockgroup", "block group", "census block group") ~ "block_group",
    geography %in% c("tr", "tract", "tracts", "census tract") ~ "tract",
    geography %in% c("co", "county", "counties", "cnty") ~ "county",
    geography %in% c("pl", "place", "places") ~ "place",
    geography %in% c("zcta", "zctas", "zip code", "zip code tabulation area") ~ "zcta",
    geography %in% c("puma", "pumas", "puma22", "public use microdata area") ~ "puma",
    geography %in% c("cbsa", "core based statistical area") ~ "cbsa",
    geography %in% c("ua", "urban area", "urban areas") ~ "urban_area",
    geography %in% c("cd118", "cd119", "congressional district") ~ geography,
    TRUE ~ geography)
}


#' Determine Temporal Crosswalk Source
#'
#' Determines which data source to use for inter-temporal crosswalks.
#'
#' @param source_year Character. Source year.
#' @param target_year Character. Target year.
#' @return Character. One of "nhgis", "ctdata", or "identity".
#' @keywords internal
#' @noRd
determine_temporal_source <- function(source_year, target_year) {
  # 2020 to 2022 special case: CTData for Connecticut
  if (isTRUE(source_year == "2020") && isTRUE(target_year == "2022")) {
    return("ctdata_2020_2022")
  }

  # 2022 to 2020 would be reverse of CTData case
  if (isTRUE(source_year == "2022") && isTRUE(target_year == "2020")) {
    return("ctdata_2020_2022")
  }

  # All other inter-temporal crosswalks use NHGIS
  return("nhgis")
}


#' Format Crosswalk Chain Plan as Message
#'
#' Formats a crosswalk chain plan as a human-readable message.
#'
#' @param plan A plan object from plan_crosswalk_chain().
#' @return Character. Formatted message describing the plan.
#' @keywords internal
#' @noRd
format_chain_plan_message <- function(plan) {
  if (!is.null(plan$error)) {
    return(stringr::str_c("Error: ", plan$error))
  }

  if (nrow(plan$steps) == 0) {
    return("No crosswalk steps defined.")
  }

  if (plan$steps$crosswalk_source[1] == "none") {
    return("No crosswalk needed: source and target are identical.")
  }

  if (!plan$is_multi_step) {
    return(stringr::str_c(
      "Single-step crosswalk:\n",
      "  Step 1: ", plan$steps$description[1], "\n",
      "\n", plan$composition_note))
  }

  step_lines <- purrr::map_chr(
    seq_len(nrow(plan$steps)),
    ~ stringr::str_c("  Step ", .x, ": ", plan$steps$description[.x]))

  stringr::str_c(
    "Multi-step crosswalk required:\n",
    paste(step_lines, collapse = "\n"), "\n",
    "\nIntermediate: ", plan$intermediate_geography, " (", plan$intermediate_year, ")\n",
    "\n", plan$composition_note)
}
