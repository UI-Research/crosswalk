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

  # Cache available crosswalks for reuse across checks
  available_crosswalks <- list_nhgis_crosswalks()

  # Case 2: Same geography, different years - temporal crosswalk(s)
  if (!geography_changes && year_changes) {
    # Check for special sources first (e.g. CTData 2020<->2022)
    temporal_source <- determine_temporal_source(source_year_chr, target_year_chr)
    if (temporal_source != "nhgis") {
      result$steps <- tibble::tibble(
        step_number = 1L,
        source_geography = source_geography,
        source_year = source_year_chr,
        target_geography = target_geography,
        target_year = target_year_chr,
        crosswalk_source = temporal_source,
        description = stringr::str_c(
          source_year_chr, " ", source_geog_std, " -> ",
          target_year_chr, " ", target_geog_std, " (inter-temporal)"))
      result$composition_note <- "Single crosswalk; use allocation_factor_source_to_target directly."
      return(result)
    }

    # Targeted validation for county temporal requests (served by the
    # county-events engine, which is forward-only and bounded)
    county_events_error <- validate_county_events_years(
      source_geog_std, source_year_chr, target_year_chr)
    if (!is.null(county_events_error)) {
      result$error <- county_events_error
      return(result)
    }

    # Find temporal path via NHGIS and county-events same-geography crosswalks
    temporal_path <- find_temporal_path(
      source_geog_std, source_year_chr, target_year_chr, available_crosswalks)

    if (is.null(temporal_path)) {
      result$error <- stringr::str_c(
        "No temporal crosswalk path found from ", source_year_chr, " to ",
        target_year_chr, " for geography '", source_geography,
        "'. NHGIS may not provide same-geography crosswalks for this ",
        "geography or year combination.",
        temporal_path_hint(source_geog_std))
      return(result)
    }

    n_hops <- length(temporal_path)

    if (n_hops == 1L) {
      # Single temporal hop
      result$steps <- tibble::tibble(
        step_number = 1L,
        source_geography = source_geography,
        source_year = source_year_chr,
        target_geography = target_geography,
        target_year = target_year_chr,
        crosswalk_source = temporal_path[[1]]$crosswalk_source,
        description = stringr::str_c(
          source_year_chr, " ", source_geog_std, " -> ",
          target_year_chr, " ", target_geog_std,
          " (inter-temporal via ", temporal_path[[1]]$crosswalk_source, ")"))
      result$composition_note <- "Single crosswalk; use allocation_factor_source_to_target directly."
    } else {
      # Multi-hop temporal chain
      result$is_multi_step <- TRUE
      result$intermediate_geography <- source_geography[1]
      result$intermediate_year <- purrr::map_chr(
        temporal_path[-n_hops], ~ .x$target_year)

      result$steps <- tibble::tibble(
        step_number = seq_len(n_hops),
        source_geography = rep(source_geography[1], n_hops),
        source_year = purrr::map_chr(temporal_path, ~ .x$source_year),
        target_geography = rep(target_geography[1], n_hops),
        target_year = purrr::map_chr(temporal_path, ~ .x$target_year),
        crosswalk_source = purrr::map_chr(temporal_path, ~ .x$crosswalk_source),
        description = purrr::map_chr(seq_len(n_hops), ~ stringr::str_c(
          temporal_path[[.x]]$source_year, " ", source_geog_std, " -> ",
          temporal_path[[.x]]$target_year, " ", source_geog_std,
          " (inter-temporal via ", temporal_path[[.x]]$crosswalk_source, ")")))

      result$composition_note <- stringr::str_c(
        "Compose crosswalks by joining on intermediate geography (",
        source_geog_std, " ",
        paste(result$intermediate_year, collapse = ", "),
        ") and multiplying allocation factors: final_allocation = ",
        paste(
          purrr::map_chr(seq_len(n_hops), ~ stringr::str_c("step", .x, "_allocation")),
          collapse = " * "))
    }
    return(result)
  }

  # Case 3: Different geography, same year (or no years) - single Geocorr crosswalk
  if (geography_changes && !year_changes) {
    # Determine which GeoCorr version will be used based on year context:
    # prefer the target year, fall back to the source year if only it is given
    year_context_chr <- if (!is.na(target_year_chr)) target_year_chr else source_year_chr
    geocorr_ref <- if (!is.na(year_context_chr)) {
      year_context_num <- as.numeric(year_context_chr)
      if (year_context_num >= 2020) "Geocorr 2022" else if (year_context_num >= 2010) "Geocorr 2018" else "Geocorr"
    } else {
      "Geocorr 2022"
    }
    result$steps <- tibble::tibble(
      step_number = 1L,
      source_geography = source_geography,
      source_year = source_year_chr,
      target_geography = target_geography,
      target_year = target_year_chr,
      crosswalk_source = "geocorr",
      description = stringr::str_c(
        source_geog_std, " -> ", target_geog_std, " (inter-geography, ", geocorr_ref, ")"))
    result$composition_note <- "Single crosswalk; use allocation_factor_source_to_target directly."
    return(result)
  }

  # Case 4: Different geography AND different year
  # First, check if NHGIS has a direct crosswalk for this combination
  # (e.g., block 2010 -> zcta 2020 is available directly from NHGIS)
  nhgis_direct_available <- is_nhgis_crosswalk_available(
    source_geography = source_geography,
    target_geography = target_geography,
    source_year = source_year,
    target_year = target_year,
    available_crosswalks = available_crosswalks)

  if (nhgis_direct_available) {
    # Single-step NHGIS crosswalk available
    result$steps <- tibble::tibble(
      step_number = 1L,
      source_geography = source_geography,
      source_year = source_year_chr,
      target_geography = target_geography,
      target_year = target_year_chr,
      crosswalk_source = "nhgis",
      description = stringr::str_c(
        source_year_chr, " ", source_geog_std, " -> ",
        target_year_chr, " ", target_geog_std, " (direct NHGIS crosswalk)"))
    result$composition_note <- "Single crosswalk; use allocation_factor_source_to_target directly."
    return(result)
  }

  # No direct NHGIS crosswalk available - multi-step required
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

  # Use first element to ensure scalar values
  src_geog <- source_geography[1]
  tgt_geog <- target_geography[1]
  src_year <- source_year_chr[1]
  tgt_year <- target_year_chr[1]
  src_std <- source_geog_std[1]
  tgt_std <- target_geog_std[1]

  # Targeted validation for county temporal requests
  county_events_error <- validate_county_events_years(src_std, src_year, tgt_year)
  if (!is.null(county_events_error)) {
    result$error <- county_events_error
    return(result)
  }

  # Find temporal path at source geography from source_year to target_year
  temporal_path <- find_temporal_path(
    src_std, src_year, tgt_year, available_crosswalks)

  if (is.null(temporal_path)) {
    result$error <- stringr::str_c(
      "Multi-step crosswalk not possible: no temporal crosswalk path found from ",
      src_year, " to ", tgt_year, " for geography '", source_geography,
      "'. NHGIS may not provide same-geography crosswalks for this ",
      "geography or year combination.",
      temporal_path_hint(src_std))
    return(result)
  }

  n_temporal <- length(temporal_path)
  n_total <- n_temporal + 1L  # temporal hops + 1 Geocorr geography step

  # Build intermediate year vector (all bridge points between steps)
  # For temporal hops: the target year of each hop except the last becomes a bridge

  # The target year of the last temporal hop is also a bridge to the Geocorr step
  intermediate_years <- purrr::map_chr(temporal_path, ~ .x$target_year)
  # The last intermediate year (= tgt_year) is the bridge to the Geocorr step
  result$intermediate_geography <- src_geog
  result$intermediate_year <- intermediate_years

  # Build temporal step rows
  temporal_steps <- tibble::tibble(
    step_number = seq_len(n_temporal),
    source_geography = rep(src_geog, n_temporal),
    source_year = purrr::map_chr(temporal_path, ~ .x$source_year),
    target_geography = rep(src_geog, n_temporal),
    target_year = purrr::map_chr(temporal_path, ~ .x$target_year),
    crosswalk_source = purrr::map_chr(temporal_path, ~ .x$crosswalk_source),
    description = purrr::map_chr(seq_len(n_temporal), ~ stringr::str_c(
      temporal_path[[.x]]$source_year, " ", src_std, " -> ",
      temporal_path[[.x]]$target_year, " ", src_std,
      " (inter-temporal via ", temporal_path[[.x]]$crosswalk_source, ")")))

  # Geocorr geography step (final step)
  geocorr_step <- tibble::tibble(
    step_number = n_total,
    source_geography = src_geog,
    source_year = tgt_year,
    target_geography = tgt_geog,
    target_year = tgt_year,
    crosswalk_source = "geocorr",
    description = stringr::str_c(
      tgt_year, " ", src_std, " -> ",
      tgt_year, " ", tgt_std, " (inter-geography via Geocorr)"))

  # GeoCorr snapshots geography at its reference year (2018 or 2022); warn when
  # county change events make the requested year's county codes differ from the
  # GeoCorr vintage's codes (county codes prefix block/bg/tract GEOIDs, so this
  # affects the join between the temporal steps and the GeoCorr step)
  tgt_year_num <- as.numeric(tgt_year)
  geocorr_ref_year <- if (tgt_year_num >= 2020) 2022 else 2018
  if (tgt_year_num >= 2000 &&
      tgt_year_num <= county_events_meta$known_through_year &&
      !tgt_year_num %in% c(2010, 2020, geocorr_ref_year) &&
      !identical(county_universe_at(tgt_year_num), county_universe_at(geocorr_ref_year))) {
    cw_warning(stringr::str_c(
      "The final geography step uses GeoCorr ", geocorr_ref_year,
      " for year ", tgt_year, ". County codes changed between ",
      geocorr_ref_year, " and ", tgt_year,
      " (county change events), so some GEOIDs may not align with the GeoCorr vintage."))
  }

  result$steps <- dplyr::bind_rows(temporal_steps, geocorr_step)

  result$composition_note <- stringr::str_c(
    "Compose crosswalks by joining on intermediate geography (",
    src_std, " ",
    paste(intermediate_years, collapse = ", "),
    ") and multiplying allocation factors: final_allocation = ",
    paste(
      purrr::map_chr(seq_len(n_total), ~ stringr::str_c("step", .x, "_allocation")),
      collapse = " * "))

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
    geography %in% c("pl", "place", "places", "placefp") ~ "place",
    geography %in% c("zcta", "zctas", "zcta5", "zip code", "zip code tabulation area") ~ "zcta",
    geography %in% c("puma", "pumas", "puma22", "puma12", "public use microdata area") ~ "puma",
    geography %in% c("cbsa", "core based statistical area") ~ "cbsa",
    geography %in% c("ua", "urban area", "urban areas") ~ "urban_area",
    geography %in% c("cd115", "cd116", "cd118", "cd119", "congressional district") ~ geography,
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


#' Validate County Temporal Requests Against County-Events Constraints
#'
#' County temporal crosswalks are served by the county-events engine, which is
#' forward-only and covers 2000 through the curated horizon. Returns an error
#' string for requests that violate those constraints, or NULL.
#'
#' @param geography_std Character. Standardized geography name.
#' @param source_year Character. Source year.
#' @param target_year Character. Target year.
#' @return Character error message, or NULL if the request is valid.
#' @keywords internal
#' @noRd
validate_county_events_years <- function(geography_std, source_year, target_year) {
  if (!isTRUE(geography_std == "county")) return(NULL)

  source_year_num <- as.numeric(source_year)
  target_year_num <- as.numeric(target_year)
  known_through <- county_events_meta$known_through_year

  if (source_year_num > target_year_num) {
    return(stringr::str_c(
      "County crosswalks from ", source_year, " to ", target_year,
      " are not supported: county temporal crosswalks are forward-only ",
      "(reversing county changes would require disaggregating merged counties). ",
      "Swap source_year and target_year to crosswalk forward in time."))
  }

  if (source_year_num < 2000 || target_year_num > known_through) {
    return(stringr::str_c(
      "County temporal crosswalks cover 2000 through ", known_through,
      ". The provided years (", source_year, " -> ", target_year,
      ") are outside this range."))
  }

  NULL
}


#' Hint Appended to No-Temporal-Path Errors
#'
#' For geographies covered by the county-events engine, explains which year
#' combinations it can serve.
#'
#' @param geography_std Character. Standardized geography name.
#' @return Character. Hint text (possibly empty).
#' @keywords internal
#' @noRd
temporal_path_hint <- function(geography_std) {
  if (!geography_std %in% c("tract", "block_group", "block")) return("")

  stringr::str_c(
    " County-change-aware ", geography_std, " crosswalks are available for ",
    "same-decade year pairs from 2010 onward (e.g., 2014 -> 2019); year pairs ",
    "spanning a decennial census chain through NHGIS crosswalks where those exist.")
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
    "\nIntermediate: ", plan$intermediate_geography,
    " (", paste(plan$intermediate_year, collapse = ", "), ")\n",
    "\n", plan$composition_note)
}


#' Find Temporal Path Between Years via Same-Geography Crosswalks
#'
#' Uses BFS to find the shortest sequence of same-geography temporal crosswalks
#' bridging a temporal span. The graph combines NHGIS same-geography edges
#' (from `list_nhgis_crosswalks()`) with county-events edges (from
#' `list_county_events_edges()`); NHGIS edges are preferred when both provide
#' the same hop. Each hop is tagged with the `crosswalk_source` that serves it.
#'
#' @param geography_std Character. Standardized geography name (e.g. "tract",
#'   "block", "block_group", "county") matching `list_nhgis_crosswalks()`
#'   output.
#' @param source_year Character or numeric. Source year.
#' @param target_year Character or numeric. Target year.
#' @param available_crosswalks Tibble or NULL. Pre-fetched output of
#'   `list_nhgis_crosswalks()` to avoid redundant calls.
#'
#' @return A list of `list(source_year, target_year, crosswalk_source)` hops
#'   representing the shortest path, or `NULL` if no path exists.
#' @keywords internal
#' @noRd
find_temporal_path <- function(geography_std,
                               source_year,
                               target_year,
                               available_crosswalks = NULL) {
  if (is.null(available_crosswalks)) {
    available_crosswalks <- list_nhgis_crosswalks()
  }

  source_year_chr <- as.character(source_year)
  target_year_chr <- as.character(target_year)

  # NHGIS same-geography temporal edges, then county-events edges. Insertion
  # order matters: BFS prefers earlier edges on equal path length, so NHGIS
  # remains the source for hops both can serve.
  edges <- dplyr::bind_rows(
    available_crosswalks |>
      dplyr::filter(
        source_geography == geography_std,
        target_geography == geography_std) |>
      dplyr::transmute(
        source_year, target_year, crosswalk_source = "nhgis"),
    list_county_events_edges(geography_std) |>
      dplyr::mutate(crosswalk_source = "county_events"))

  if (nrow(edges) == 0) return(NULL)

  # Build adjacency list: year -> tibble of (target_year, crosswalk_source),
  # keeping the first (preferred) source per target year
  adj <- list()
  for (i in seq_len(nrow(edges))) {
    from <- edges$source_year[i]
    to <- edges$target_year[i]
    if (to %in% adj[[from]]$target_year) next
    adj[[from]] <- dplyr::bind_rows(
      adj[[from]],
      tibble::tibble(target_year = to, crosswalk_source = edges$crosswalk_source[i]))
  }

  # BFS from source_year to target_year
  queue <- list(list(year = source_year_chr, path = list()))
  visited <- character()

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    if (current$year == target_year_chr) {
      return(current$path)
    }

    if (current$year %in% visited) next
    visited <- c(visited, current$year)

    neighbors <- adj[[current$year]]
    if (is.null(neighbors)) next

    for (j in seq_len(nrow(neighbors))) {
      neighbor <- neighbors$target_year[j]
      if (!neighbor %in% visited) {
        new_path <- c(
          current$path,
          list(list(
            source_year = current$year,
            target_year = neighbor,
            crosswalk_source = neighbors$crosswalk_source[j])))
        queue <- c(queue, list(list(year = neighbor, path = new_path)))
      }
    }
  }

  return(NULL)
}
