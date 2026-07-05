#' Get a County-Events Crosswalk Between Arbitrary Years
#'
#' Builds a crosswalk that accounts for substantial changes to counties and
#' county-equivalent entities occurring outside decennial censuses (renames,
#' merges, splits, and part transfers such as the 2013 Bedford, VA merge, the
#' 2015 Shannon -> Oglala Lakota, SD FIPS change, or the 2019 Valdez-Cordova,
#' AK split). Backed by a curated event registry shipped as internal package
#' data (see `data-raw/build_county_events_sysdata.R`).
#'
#' For a request `(geography, source_year, target_year)` the crosswalk is an
#' identity mapping over the full GEOID universe at `source_year`, composed
#' with the mappings of every event whose effective vintage falls in
#' `(source_year, target_year]`. Identity rows are included for all unchanged
#' units because `crosswalk_data()` drops unmatched rows.
#'
#' County crosswalks span any pair of years in range (counties are not
#' redefined at decennial censuses, so no NHGIS chaining is needed).
#' Sub-county crosswalks (tract, block group, block) are limited to year pairs
#' within the same tract-definition era (2010-2019 or 2020+); cross-decade
#' requests are planned as chains with NHGIS crosswalks by
#' `plan_crosswalk_chain()`.
#'
#' @param geography Character. One of "county", "tract", "block_group", or
#'   "block" (aliases accepted).
#' @param source_year Numeric. Vintage year of the source GEOIDs.
#' @param target_year Numeric. Vintage year of the target GEOIDs. For county,
#'   must be greater than `source_year` (forward-only). For sub-county
#'   geographies, backward requests are allowed only when every event in the
#'   interval is an exact 1:1 relabel at that geography level.
#' @param cache Directory path or NULL. Where to cache the crosswalk.
#'
#' @return A tibble with the standard crosswalk columns: `source_geoid`,
#'   `target_geoid`, `source_geography_name`, `target_geography_name`,
#'   `source_year`, `target_year`, `allocation_factor_source_to_target`,
#'   `weighting_factor`, `state_fips`, with a `crosswalk_metadata` attribute.
#' @keywords internal
#' @noRd
get_county_events_crosswalk <- function(geography,
                                        source_year,
                                        target_year,
                                        cache = NULL) {

  source_year <- as.numeric(source_year)
  target_year <- as.numeric(target_year)

  geography_standardized <- standardize_geography_for_events(geography)

  if (is.na(geography_standardized)) {
    stop(
"County-events crosswalks are only available for counties, tracts, block groups, and blocks.
The provided geography '", geography, "' is not supported.")}

  known_through <- county_events_meta$known_through_year

  if (min(source_year, target_year) < 2000 || max(source_year, target_year) > known_through) {
    stop(
"County-events crosswalks cover ", 2000, " through ", known_through, ".
The provided years (", source_year, " -> ", target_year, ") are outside this range.")}

  if (source_year == target_year) {
    stop(
"source_year and target_year are identical (", source_year, "); no county-events crosswalk is needed.")}

  is_backward <- source_year > target_year

  if (geography_standardized == "county" && is_backward) {
    stop(
"County crosswalks from ", source_year, " to ", target_year, " are not supported.
Reversing county changes would require disaggregating merged counties, which is
not implemented. Swap source_year and target_year to crosswalk forward in time.")}

  if (geography_standardized != "county" && min(source_year, target_year) < 2010) {
    stop(
"Sub-county county-events crosswalks are only available for 2010 and later.
The provided years (", source_year, " -> ", target_year, ") reach into the 2000s,
for which sub-county mappings are not curated. County-level crosswalks cover
2000 onward.")}

  if (geography_standardized != "county" &&
      tract_era(source_year) != tract_era(target_year)) {
    stop(
"Sub-county county-events crosswalks require source_year and target_year in the
same tract-definition era (2010-2019 or 2020 onward). The provided years (",
source_year, " -> ", target_year, ") span a decennial census; use get_crosswalk(),
which chains county-events relabels with NHGIS inter-decade crosswalks.")}

  # Events in the (chronologically ordered) interval
  interval_events <- county_events_in_interval(source_year, target_year)

  if (is_backward) {
    reversal_col <- stringr::str_c("reversal_exact_", geography_standardized)
    inexact <- interval_events[
      !interval_events[[reversal_col]] &
        interval_events$subcounty_handling != "none", ]
    if (nrow(inexact) > 0) {
      stop(
"Backward crosswalks from ", source_year, " to ", target_year, " for ",
geography_standardized, " are not supported because the interval contains
county changes that are not exact 1:1 relabels:\n",
paste0("- ", inexact$description, collapse = "\n"),
"\nSwap source_year and target_year to crosswalk forward in time.")}
  }

  # ---- Cache read ------------------------------------------------------------
  cache_path <- if (is.null(cache)) tempdir() else cache
  csv_path <- file.path(
    cache_path,
    stringr::str_c(
      "crosswalk_county_events_", source_year, "_to_", target_year, "_",
      geography_standardized, ".csv"))

  if (file.exists(csv_path) && !is.null(cache)) {
    cw_message(stringr::str_c(
      "Reading county-events ", source_year, " -> ", target_year, " ",
      geography_standardized, " crosswalk from cache."))
    result <- readr::read_csv(
      csv_path,
      col_types = readr::cols(
        .default = readr::col_character(),
        allocation_factor_source_to_target = readr::col_double()),
      show_col_types = FALSE)
    attr(result, "crosswalk_metadata") <- county_events_metadata(
      geography, geography_standardized, source_year, target_year,
      interval_events, csv_path, read_from_cache = TRUE)
    return(result)
  }

  # ---- Build -----------------------------------------------------------------
  if (nrow(interval_events) == 0) {
    cw_message(stringr::str_c(
      "No county boundary or code changes between ", source_year, " and ",
      target_year, "; returning an identity crosswalk."))
  } else {
    cw_message(stringr::str_c(
      "Applying ", nrow(interval_events), " county change event(s) between ",
      source_year, " and ", target_year, ":\n",
      paste0("- ", interval_events$description, collapse = "\n")))
  }

  if (geography_standardized == "county") {
    crosswalk <- build_county_events_county_crosswalk(
      source_year, target_year, interval_events)
  } else {
    crosswalk <- build_county_events_subcounty_crosswalk(
      geography_standardized, source_year, target_year, interval_events, cache)
  }

  result <- crosswalk |>
    dplyr::mutate(
      source_geography_name = geography_standardized,
      target_geography_name = geography_standardized,
      source_year = as.character(source_year),
      target_year = as.character(target_year),
      state_fips = stringr::str_sub(source_geoid, 1, 2)) |>
    dplyr::select(
      source_geoid, target_geoid,
      source_geography_name, target_geography_name,
      source_year, target_year,
      allocation_factor_source_to_target,
      weighting_factor, state_fips) |>
    dplyr::arrange(source_geoid, target_geoid)

  # ---- Cache write -----------------------------------------------------------
  if (!is.null(cache)) {
    if (!dir.exists(cache_path)) {
      dir.create(cache_path, recursive = TRUE)
    }
    readr::write_csv(result, csv_path)
    cw_message(stringr::str_c("Cached to: ", csv_path))
  }

  attr(result, "crosswalk_metadata") <- county_events_metadata(
    geography, geography_standardized, source_year, target_year,
    interval_events, if (!is.null(cache)) csv_path else NULL,
    read_from_cache = FALSE)

  return(result)
}


#' Standardize Geography Name for County-Events Crosswalks
#' @noRd
standardize_geography_for_events <- function(geography) {
  geography_standardized <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("_", " ")

  dplyr::case_when(
    geography_standardized %in% c("county", "counties", "co", "cnty") ~ "county",
    geography_standardized %in% c("tract", "tracts", "tr", "census tract") ~ "tract",
    geography_standardized %in% c("block group", "blockgroup", "bg", "census block group") ~ "block_group",
    geography_standardized %in% c("block", "blocks", "blk", "census block") ~ "block",
    TRUE ~ NA_character_)
}


#' Tract-Definition Era for a Vintage Year
#' @noRd
tract_era <- function(year) {
  dplyr::case_when(
    year >= 2020 ~ 2020L,
    year >= 2010 ~ 2010L,
    TRUE ~ 2000L)
}


#' County Change Events Within a Year Interval
#'
#' Returns events with effective vintage in `(min(y1, y2), max(y1, y2)]`,
#' ordered chronologically.
#' @noRd
county_events_in_interval <- function(source_year, target_year) {
  lower <- min(source_year, target_year)
  upper <- max(source_year, target_year)

  county_events |>
    dplyr::filter(
      effective_vintage_year > lower,
      effective_vintage_year <= upper) |>
    dplyr::arrange(effective_vintage_year, event_id)
}


#' County GEOID Universe at a Vintage Year
#'
#' Rolls the decennial-base county universe forward through every event with
#' effective vintage in `(decade_base, year]`.
#' @noRd
county_universe_at <- function(year) {
  base <- tract_era(year)

  universe <- county_universes |>
    dplyr::filter(decade_base == base) |>
    dplyr::pull(geoid)

  base_events <- county_events_in_interval(base, year)

  for (eid in base_events$event_id) {
    mapping <- county_event_mappings |> dplyr::filter(event_id == eid)
    universe <- union(setdiff(universe, mapping$source_geoid), mapping$target_geoid)
  }

  sort(universe)
}


#' Compose an Event Mapping Onto a Working Crosswalk
#'
#' Rows of `crosswalk` whose `target_geoid` is a source of the event mapping
#' fan out to the event targets with multiplied factors; all other rows pass
#' through. The result has one row per source-target pair.
#' @noRd
compose_event_mapping <- function(crosswalk, mapping) {
  step <- mapping |>
    dplyr::select(
      step_source = source_geoid,
      step_target = target_geoid,
      step_factor = allocation_factor,
      step_weighting = weighting_factor)

  affected <- crosswalk |>
    dplyr::inner_join(
      step, by = c("target_geoid" = "step_source"),
      relationship = "many-to-many") |>
    dplyr::mutate(
      target_geoid = step_target,
      allocation_factor_source_to_target =
        allocation_factor_source_to_target * step_factor,
      weighting_factor = dplyr::if_else(
        step_weighting == "identity", weighting_factor, step_weighting)) |>
    dplyr::select(-step_target, -step_factor, -step_weighting)

  unaffected <- crosswalk |>
    dplyr::anti_join(step, by = c("target_geoid" = "step_source"))

  dplyr::bind_rows(unaffected, affected) |>
    dplyr::summarize(
      allocation_factor_source_to_target = sum(allocation_factor_source_to_target),
      weighting_factor = dplyr::if_else(
        all(weighting_factor == "identity"), "identity", "population"),
      .by = c(source_geoid, target_geoid))
}


#' Build a County-Level County-Events Crosswalk
#' @noRd
build_county_events_county_crosswalk <- function(source_year,
                                                 target_year,
                                                 interval_events) {
  universe <- county_universe_at(source_year)

  crosswalk <- tibble::tibble(
    source_geoid = universe,
    target_geoid = universe,
    allocation_factor_source_to_target = 1,
    weighting_factor = "identity")

  for (eid in interval_events$event_id) {
    mapping <- county_event_mappings |> dplyr::filter(event_id == eid)
    crosswalk <- compose_event_mapping(crosswalk, mapping)
  }

  crosswalk
}


#' Sub-County Mapping Rows for One Event
#'
#' Builds the sub-county (tract/block-group/block) source -> target mapping for
#' a single event, according to its `subcounty_handling`. `current_geoids` is
#' the working GEOID universe the mapping will be applied to. Returns NULL for
#' events with no within-era sub-county effect.
#' @noRd
subcounty_event_mapping <- function(event_row,
                                    geography_standardized,
                                    current_geoids,
                                    reverse = FALSE,
                                    cache = NULL) {
  handling <- event_row$subcounty_handling

  if (handling == "none") {
    return(NULL)
  }

  if (handling == "unsupported") {
    stop(
"Sub-county crosswalks are not supported across this county change:\n- ",
event_row$description, "\nCounty-level crosswalks are available for this period.")}

  if (handling == "prefix_swap") {
    county_mapping <- county_event_mappings |>
      dplyr::filter(event_id == event_row$event_id)
    stopifnot(all(county_mapping$allocation_factor == 1))

    prefix_from <- county_mapping$source_geoid
    prefix_to <- county_mapping$target_geoid
    if (reverse) {
      # Reversing a prefix swap is only valid for renames: for merges, only
      # some of the target county's GEOIDs originated in the source county
      stopifnot(event_row$event_type == "rename")
      tmp <- prefix_from
      prefix_from <- prefix_to
      prefix_to <- tmp
    }

    affected <- current_geoids[stringr::str_sub(current_geoids, 1, 5) %in% prefix_from]
    if (length(affected) == 0) {
      return(NULL)
    }
    new_prefix <- prefix_to[match(stringr::str_sub(affected, 1, 5), prefix_from)]

    return(tibble::tibble(
      source_geoid = affected,
      target_geoid = stringr::str_c(new_prefix, stringr::str_sub(affected, 6)),
      allocation_factor = 1,
      weighting_factor = "identity"))
  }

  if (handling == "explicit_rows") {
    mapping <- county_event_subcounty_mappings |>
      dplyr::filter(
        event_id == event_row$event_id,
        geography == geography_standardized) |>
      dplyr::select(source_geoid, target_geoid, allocation_factor) |>
      dplyr::mutate(weighting_factor = dplyr::if_else(
        allocation_factor == 1, "identity", "population"))

    if (nrow(mapping) == 0) {
      stop(
"Sub-county (", geography_standardized, ") mappings for this county change have
not been curated:\n- ", event_row$description)}

    if (reverse) {
      stopifnot(all(mapping$allocation_factor == 1))
      mapping <- mapping |>
        dplyr::rename(source_geoid = target_geoid, target_geoid = source_geoid)
    }

    return(mapping)
  }

  if (handling == "ctdata_runtime") {
    ct <- get_ctdata_crosswalk(
      geography = geography_standardized,
      source_year = if (reverse) 2022 else 2020,
      target_year = if (reverse) 2020 else 2022,
      cache = cache)

    return(ct |>
      dplyr::filter(state_fips == "09") |>
      dplyr::select(
        source_geoid, target_geoid,
        allocation_factor = allocation_factor_source_to_target,
        weighting_factor))
  }

  stop("Unknown subcounty_handling: ", handling)
}


#' Drop Sub-County GEOIDs That Retain an Abolished County Prefix
#'
#' After an event is applied, no GEOID may keep a county prefix the event
#' abolished. GEOIDs missing from the event's sub-county mapping (e.g., the
#' four Connecticut water tracts absent from the CT Data crosswalk) would
#' otherwise survive as identity rows with stale prefixes; drop them instead,
#' mirroring `get_ctdata_crosswalk()`, which also omits them.
#' @noRd
drop_abolished_prefixes <- function(geoids, event_row, reverse = FALSE) {
  county_mapping <- county_event_mappings |>
    dplyr::filter(event_id == event_row$event_id)

  from_counties <- county_mapping$source_geoid
  to_counties <- county_mapping$target_geoid
  if (reverse) {
    tmp <- from_counties
    from_counties <- to_counties
    to_counties <- tmp
  }
  abolished <- setdiff(from_counties, to_counties)

  stale <- stringr::str_sub(geoids, 1, 5) %in% abolished
  if (any(stale)) {
    cw_message(stringr::str_c(
      "Dropping ", sum(stale), " GEOID(s) not covered by the sub-county ",
      "mapping for: ", event_row$description))
  }
  geoids[!stale]
}


#' Build a Sub-County County-Events Crosswalk
#'
#' Enumerates the era-anchor GEOID universe from the NHGIS decennial crosswalk
#' for the geography, relabels it to `source_year` by applying prior events in
#' the era, then composes the events in `(source_year, target_year]`.
#' @noRd
build_county_events_subcounty_crosswalk <- function(geography_standardized,
                                                    source_year,
                                                    target_year,
                                                    interval_events,
                                                    cache = NULL) {
  era <- tract_era(source_year)
  is_backward <- source_year > target_year

  cw_message(stringr::str_c(
    "Enumerating the ", era, "-era ", geography_standardized,
    " universe from the NHGIS 2010 -> 2020 crosswalk..."))

  nhgis_crosswalk <- get_nhgis_crosswalk(
    source_year = 2010,
    source_geography = geography_standardized,
    target_year = 2020,
    target_geography = geography_standardized,
    cache = cache)

  universe_col <- if (era == 2010L) "source_geoid" else "target_geoid"
  universe <- nhgis_crosswalk[[universe_col]] |> unique() |> sort()

  # Relabel the era-anchor universe to source_year through prior events
  prior_events <- county_events_in_interval(era, source_year)

  for (i in seq_len(nrow(prior_events))) {
    mapping <- subcounty_event_mapping(
      prior_events[i, ], geography_standardized, universe, reverse = FALSE,
      cache = cache)
    if (!is.null(mapping)) {
      stopifnot(all(mapping$allocation_factor == 1))
      relabeled <- dplyr::coalesce(
        mapping$target_geoid[match(universe, mapping$source_geoid)], universe)
      universe <- sort(unique(
        drop_abolished_prefixes(relabeled, prior_events[i, ], reverse = FALSE)))
    }
  }

  crosswalk <- tibble::tibble(
    source_geoid = universe,
    target_geoid = universe,
    allocation_factor_source_to_target = 1,
    weighting_factor = "identity")

  event_order <- if (is_backward) rev(seq_len(nrow(interval_events))) else seq_len(nrow(interval_events))

  for (i in event_order) {
    mapping <- subcounty_event_mapping(
      interval_events[i, ], geography_standardized,
      unique(crosswalk$target_geoid), reverse = is_backward, cache = cache)
    if (!is.null(mapping)) {
      crosswalk <- compose_event_mapping(crosswalk, mapping) |>
        dplyr::filter(target_geoid %in% drop_abolished_prefixes(
          target_geoid, interval_events[i, ], reverse = is_backward))
    }
  }

  crosswalk
}


#' List Temporal Edges Provided by the County-Events Engine
#'
#' Enumerates the (source_year, target_year) pairs the county-events engine can
#' serve for a geography, for use by the chain planner and
#' `get_available_crosswalks()`.
#'
#' County: all forward pairs in `[2000, known_through_year]`. Sub-county:
#' same-era forward pairs in `[2010, known_through_year]`, plus backward pairs
#' whose interval contains only exact 1:1 relabels (or no events), excluding
#' any interval containing an event whose sub-county mappings are unsupported
#' or not yet curated.
#'
#' @param geography_std Character. Standardized geography name ("county",
#'   "tract", "block_group", or "block").
#' @return A tibble with columns `source_year`, `target_year` (character).
#' @keywords internal
#' @noRd
list_county_events_edges <- function(geography_std) {
  known_through <- county_events_meta$known_through_year

  if (geography_std == "county") {
    return(
      tidyr::crossing(
        source_year = 2000:known_through,
        target_year = 2000:known_through) |>
        dplyr::filter(source_year < target_year) |>
        dplyr::mutate(
          source_year = as.character(source_year),
          target_year = as.character(target_year)))
  }

  if (!geography_std %in% c("tract", "block_group", "block")) {
    return(tibble::tibble(source_year = character(), target_year = character()))
  }

  eras <- list(2010:2019, 2020:known_through)

  event_supported <- function(event_row) {
    handling <- event_row$subcounty_handling
    if (handling %in% c("none", "prefix_swap", "ctdata_runtime")) return(TRUE)
    if (handling == "unsupported") return(FALSE)
    # explicit_rows: supported only once curated
    nrow(county_event_subcounty_mappings |>
           dplyr::filter(
             event_id == event_row$event_id,
             geography == geography_std)) > 0
  }

  event_reversal_exact <- function(event_row) {
    if (event_row$subcounty_handling == "none") return(TRUE)
    event_row[[stringr::str_c("reversal_exact_", geography_std)]]
  }

  purrr::map_dfr(eras, function(era_years) {
    tidyr::crossing(y1 = era_years, y2 = era_years) |>
      dplyr::filter(y1 != y2) |>
      dplyr::rowwise() |>
      dplyr::filter({
        evs <- county_events_in_interval(y1, y2)
        supported <- nrow(evs) == 0 ||
          all(purrr::map_lgl(seq_len(nrow(evs)), ~ event_supported(evs[.x, ])))
        exact_ok <- y1 < y2 || nrow(evs) == 0 ||
          all(purrr::map_lgl(seq_len(nrow(evs)), ~ event_reversal_exact(evs[.x, ])))
        supported && exact_ok
      }) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        source_year = as.character(y1),
        target_year = as.character(y2))
  })
}


#' Build the Metadata Attribute for a County-Events Crosswalk
#' @noRd
county_events_metadata <- function(geography,
                                   geography_standardized,
                                   source_year,
                                   target_year,
                                   interval_events,
                                   cache_file,
                                   read_from_cache) {
  event_notes <- if (nrow(interval_events) > 0) {
    stringr::str_c("Event applied: ", interval_events$description)
  } else {
    "No county boundary or code changes in this interval; identity crosswalk."
  }

  list(
    data_source = "county_events",
    data_source_full_name = "U.S. Census Bureau county boundary change records (curated in the crosswalk package)",
    documentation_url = county_events_meta$source_documentation_url,
    source_year = as.character(source_year),
    target_year = as.character(target_year),
    source_geography = geography,
    source_geography_standardized = geography_standardized,
    target_geography = geography,
    target_geography_standardized = geography_standardized,
    state_coverage = "National (all 50 states, DC, and Puerto Rico)",
    notes = c(
      event_notes,
      "Allocation factors for split/transferred counties are population-based (see factor derivations in data-raw/build_county_events_sysdata.R).",
      "Minor county boundary corrections and non-county-related ACS tract revisions are not modeled."),
    retrieved_at = Sys.time(),
    cached = !is.null(cache_file),
    cache_path = cache_file,
    read_from_cache = read_from_cache)
}


utils::globalVariables(c(
  "effective_vintage_year", "event_id", "decade_base", "geoid", "geography",
  "subcounty_handling", "allocation_factor", "step_source", "step_target",
  "step_factor", "step_weighting", "y1", "y2"))
