## explicitly enable/acknowledge data.table (used by tidytable)
.datatable.aware = TRUE

#' Interpolate data using a crosswalk(s)
#'
#' Applies geographic crosswalk weights to transform data from a source geography
#' to a target geography. Can either accept a pre-fetched crosswalk from
#' `get_crosswalk()` or fetch the crosswalk automatically using the provided
#' geography and year parameters.
#'
#' @param data A data frame or tibble containing the data to crosswalk.
#' @param crosswalk The output from `get_crosswalk()` - a list containing:
#'    \describe{
#'      \item{crosswalks}{A named list of crosswalk tibbles (step_1, step_2, etc.)}
#'      \item{plan}{The crosswalk plan}
#'      \item{message}{Description of the crosswalk chain}
#'    }
#'    Alternatively, a single crosswalk tibble can be provided for backwards
#'    compatibility. If NULL, the crosswalk will be fetched using `source_geography`
#'    and `target_geography` parameters.
#' @param source_geography Character or NULL. Source geography name. Required if
#'    `crosswalk` is NULL. One of c("block", "block group", "tract", "place",
#'    "county", "urban_area", "zcta", "puma", "cd118", "cd119",
#'    "core_based_statistical_area").
#' @param target_geography Character or NULL. Target geography name. Required if
#'    `crosswalk` is NULL. Same options as `source_geography`.
#' @param source_year Numeric or NULL. Year of the source geography. If NULL and
#'    crosswalk is being fetched, uses same-year crosswalk via Geocorr.
#' @param target_year Numeric or NULL. Year of the target geography. If NULL and
#'    crosswalk is being fetched, uses same-year crosswalk via Geocorr.
#' @param weight Character. Weighting variable for Geocorr crosswalks when fetching.
#'    One of c("population", "housing", "land"). Default is "population".
#' @param cache Directory path or NULL. Where to cache fetched crosswalks. If NULL
#'    (default), crosswalk is fetched but not saved to disk.
#' @param geoid_column Character. The name of the column in `data` containing
#'    the source geography identifiers (GEOIDs). Default is "source_geoid".
#' @param count_columns Character vector or NULL. Column names in `data` that represent
#'    count variables. These will be summed after multiplying by the allocation factor.
#'    If NULL (default), automatically detects columns with the prefix "count_".
#' @param non_count_columns Character vector or NULL. Column names in `data` that represent
#'    mean, median, percentage, and ratio variables. These will be calculated as weighted
#'    means using the allocation factor as weights. If NULL (default), automatically
#'    detects columns with prefixes "mean_", "median_", "percent_", or "ratio_".
#' @param return_intermediate Logical. If TRUE and crosswalk has multiple steps,
#'    returns a list containing both the final result and intermediate results
#'    from each step. Default is FALSE, which returns only the final result.
#' @param show_join_quality Logical. If TRUE (default), prints diagnostic messages
#'    about join quality, including the number of data rows not matching the crosswalk
#'    and vice versa. For state-nested geographies (tract, county, block group, etc.),
#'    also reports state-level concentration of unmatched rows. Set to FALSE to
#'    suppress these messages.
#'
#' @return If `return_intermediate = FALSE` (default), a tibble with data summarized
#'    to the final target geography.
#'
#'    If `return_intermediate = TRUE` and there are multiple crosswalk steps, a list with:
#'    \describe{
#'      \item{final}{The final crosswalked data}
#'      \item{intermediate}{A named list of intermediate results (step_1, step_2, etc.)}
#'    }
#'
#'    The returned tibble(s) include an attribute `crosswalk_metadata` from the
#'    underlying crosswalk (access via `attr(result, "crosswalk_metadata")`).
#'
#' @details
#' **Two usage patterns**:
#'
#' 1. **Pre-fetched crosswalk**: Pass the output of `get_crosswalk()` to the
#'    `crosswalk` parameter. Useful when you want to inspect or reuse the crosswalk.
#'
#' 2. **Direct crosswalking**: Pass `source_geography` and `target_geography`
#'    (and optionally `source_year`, `target_year`, `weight`, `cache`) and the
#'    crosswalk will be fetched automatically. Useful for one-off transformations.
#'
#' **Count variables** (specified in `count_columns`) are interpolated by summing
#' the product of the value and the allocation factor across all source geographies
#' that overlap with each target geography.
#'
#' **Non-count variables** (specified in `non_count_columns`) are interpolated using
#' a weighted mean, with the allocation factor serving as the weight.
#'
#' **Automatic column detection**: If `count_columns` and `non_count_columns` are
#' both NULL, the function will automatically detect columns based on naming prefixes:
#' - Columns starting with "count_" are treated as count variables
#' - Columns starting with "mean_", "median_", "percent_", or "ratio_" are treated
#'   as non-count variables
#'
#' **Other columns**: Columns that are not the geoid column, count columns, or
#' non-count columns (e.g., metadata like `data_year`) are preserved by taking
#' the first non-missing value within each target geography group. If all values
#' are missing, NA is returned.
#'
#' **Multi-step crosswalks**: When `get_crosswalk()` returns multiple crosswalks
#' (for transformations that change both geography and year), this function
#' automatically applies them in sequence.
#'
#' @export
#' @examples
#' \dontrun{
#' # Option 1: Pre-fetched crosswalk
#' crosswalk <- get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "zcta",
#'   weight = "population")
#'
#' result <- crosswalk_data(
#'   data = my_tract_data,
#'   crosswalk = crosswalk,
#'   geoid_column = "tract_geoid",
#'   count_columns = c("count_population", "count_housing_units"))
#'
#' # Option 2: Direct crosswalking (crosswalk fetched automatically)
#' result <- crosswalk_data(
#'   data = my_tract_data,
#'   source_geography = "tract",
#'   target_geography = "zcta",
#'   weight = "population",
#'   geoid_column = "tract_geoid",
#'   count_columns = c("count_population", "count_housing_units"))
#'
#' # Direct crosswalking with year change
#' result <- crosswalk_data(
#'   data = my_data,
#'   source_geography = "tract",
#'   target_geography = "zcta",
#'   source_year = 2010,
#'   target_year = 2020,
#'   weight = "population",
#'   geoid_column = "tract_geoid",
#'   count_columns = "count_population")
#'
#' # Pre-fetched crosswalk with intermediate results
#' crosswalk <- get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "zcta",
#'   source_year = 2010,
#'   target_year = 2020,
#'   weight = "population")
#'
#' result <- crosswalk_data(
#'   data = my_data,
#'   crosswalk = crosswalk,
#'   geoid_column = "tract_geoid",
#'   count_columns = "count_population",
#'   return_intermediate = TRUE)
#'
#' # Access intermediate and final
#' result$intermediate$step_1  # After first crosswalk
#' result$final                # Final result
#' }

crosswalk_data <- function(
    data,
    crosswalk = NULL,
    source_geography = NULL,
    target_geography = NULL,
    source_year = NULL,
    target_year = NULL,
    weight = "population",
    cache = NULL,
    geoid_column = "source_geoid",
    count_columns = NULL,
    non_count_columns = NULL,
    return_intermediate = FALSE,
    show_join_quality = TRUE) {

  # Determine if we need to fetch the crosswalk
  crosswalk_provided <- !is.null(crosswalk)
  geography_provided <- !is.null(source_geography) && !is.null(target_geography)

  if (!crosswalk_provided && !geography_provided) {
    stop(
      "Either provide a crosswalk via the 'crosswalk' parameter, or provide ",
      "'source_geography' and 'target_geography' to fetch a crosswalk automatically.")
  }

  if (crosswalk_provided && geography_provided) {
    warning(
      "Both 'crosswalk' and geography parameters provided. ",
      "Using the provided 'crosswalk' and ignoring geography parameters.")
  }

  # Fetch crosswalk if not provided
  if (!crosswalk_provided) {
    message("Fetching crosswalk from ", source_geography, " to ", target_geography, "...")
    crosswalk <- get_crosswalk(
      source_geography = source_geography,
      target_geography = target_geography,
      source_year = source_year,
      target_year = target_year,
      weight = weight,
      cache = cache)
  }

  # Determine if crosswalk is a list (from get_crosswalk) or a single tibble
  crosswalk_list <- extract_crosswalk_list(crosswalk)

  # Auto-detect columns if not specified
  data_columns <- names(data)

  if (is.null(count_columns)) {
    count_columns <- data_columns[stringr::str_starts(data_columns, "count_")]
  }

  if (is.null(non_count_columns)) {
    non_count_columns <- data_columns[
      stringr::str_starts(data_columns, "mean_") |
      stringr::str_starts(data_columns, "median_") |
      stringr::str_starts(data_columns, "percent_") |
      stringr::str_starts(data_columns, "ratio_")]
  }

  if (length(count_columns) == 0 & length(non_count_columns) == 0) {
    stop(
      "No columns to crosswalk. Either specify `count_columns` or `non_count_columns`, ",
      "or ensure your data has columns with prefixes: count_, mean_, median_, percent_, or ratio_.")
  }

  # Check that specified columns exist in original data
  all_value_columns <- c(count_columns, non_count_columns)
  missing_columns <- setdiff(all_value_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "The following columns were not found in data: ",
      paste(missing_columns, collapse = ", "))
  }

  # Validate geoid_column exists in original data
  if (!geoid_column %in% names(data)) {
    stop("Column '", geoid_column, "' not found in data.")
  }

  # Apply crosswalks sequentially
  n_steps <- length(crosswalk_list)
  intermediate_results <- list()
  current_data <- data
  current_geoid_column <- geoid_column

  for (i in seq_len(n_steps)) {
    step_name <- names(crosswalk_list)[i]
    step_crosswalk <- crosswalk_list[[i]]

    message(stringr::str_c("Applying crosswalk step ", i, " of ", n_steps, "..."))

    # Apply single crosswalk step
    current_data <- apply_single_crosswalk(
      data = current_data,
      crosswalk = step_crosswalk,
      geoid_column = current_geoid_column,
      count_columns = count_columns,
      non_count_columns = non_count_columns,
      step_number = i,
      total_steps = n_steps,
      show_join_quality = show_join_quality)

    # Store intermediate result if requested
    if (return_intermediate) {
      intermediate_results[[step_name]] <- current_data
    }

    # After first step, geoid column is renamed to "geoid"
    current_geoid_column <- "geoid"
  }

  # Return based on return_intermediate flag
  if (return_intermediate && n_steps > 1) {
    return(list(
      final = current_data,
      intermediate = intermediate_results))
  }

  return(current_data)
}


#' Extract Crosswalk List from Various Input Formats
#'
#' Internal function that normalizes crosswalk input to a list of crosswalk tibbles.
#'
#' @param crosswalk Either a list from get_crosswalk() or a single tibble
#' @return A named list of crosswalk tibbles (step_1, step_2, etc.)
#' @keywords internal
#' @noRd
extract_crosswalk_list <- function(crosswalk) {

  # If it's a list with a "crosswalks" element (from get_crosswalk)
  if (is.list(crosswalk) && "crosswalks" %in% names(crosswalk)) {
    crosswalk_list <- crosswalk$crosswalks

    # Validate each crosswalk in the list
    for (name in names(crosswalk_list)) {
      xwalk <- crosswalk_list[[name]]
      validate_crosswalk_tibble(xwalk, name)
    }

    return(crosswalk_list)
  }

  # If it's a data frame directly (backwards compatibility or manual input)
  if (is.data.frame(crosswalk)) {
    validate_crosswalk_tibble(crosswalk, "crosswalk")
    return(list(step_1 = crosswalk))
  }

  # Otherwise, invalid input

  stop(
    "Invalid crosswalk input. Expected either:\n",
    "  1. Output from get_crosswalk() (a list with $crosswalks element), or\n",
    "  2. A single crosswalk tibble with columns: source_geoid, target_geoid, allocation_factor_source_to_target\n",
    "Alternatively, leave 'crosswalk' NULL and provide 'source_geography' and 'target_geography' to fetch automatically.")
}


#' Validate a Crosswalk Tibble
#'
#' Internal function that checks a crosswalk tibble has required columns.
#'
#' @param crosswalk A tibble to validate
#' @param name Name to use in error messages
#' @keywords internal
#' @noRd
validate_crosswalk_tibble <- function(crosswalk, name) {
  required_cols <- c("source_geoid", "target_geoid", "allocation_factor_source_to_target")
  missing_cols <- setdiff(required_cols, names(crosswalk))

  if (length(missing_cols) > 0) {
    stop(
      "Crosswalk '", name, "' is missing required columns: ",
      paste(missing_cols, collapse = ", "))
  }
}


#' Check if Geography is Nested Within States
#'
#' Internal function that determines whether a geography type has GEOIDs
#' where the first two characters represent the state FIPS code.
#'
#' Geographies nested within states (state FIPS derivable from GEOID):
#' - block, block_group, tract, county, place, puma, congressional districts
#'
#' Geographies NOT nested within states (cross state boundaries):
#' - zcta, cbsa/core_based_statistical_area, urban_area
#'
#' @param geography Character. The geography type to check.
#' @return Logical. TRUE if geography is nested within states.
#' @keywords internal
#' @noRd
is_state_nested_geography <- function(geography) {
  if (is.null(geography) || is.na(geography)) {
    return(FALSE)
  }

  geography_lower <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("_", " ")

  # Geographies where GEOID starts with state FIPS

  state_nested <- c(
    "block", "blocks", "blk",
    "block group", "blockgroup", "bg",
    "tract", "tracts", "tr",
    "county", "counties", "co",
    "place", "places", "pl",
    "puma", "pumas", "puma22",
    "cd118", "cd119", "congressional district"
  )

  geography_lower %in% state_nested
}


#' State FIPS to Abbreviation Lookup
#'
#' Internal lookup table for converting state FIPS codes to abbreviations.
#' @keywords internal
#' @noRd
state_fips_to_abbr <- tibble::tibble(
  state_fips = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
    "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
    "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
    "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
    "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
    "56", "72", "78", "66", "60", "69"
  ),
  state_abbr = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
    "WY", "PR", "VI", "GU", "AS", "MP"
  )
)


#' Analyze State Concentration of Unmatched GEOIDs
#'
#' Internal function that analyzes the state-level distribution of unmatched GEOIDs.
#'
#' @param unmatched_geoids Character vector of GEOIDs that did not match.
#' @return A list with state_counts tibble, top_states tibble, and is_concentrated flag.
#' @keywords internal
#' @noRd
analyze_state_concentration <- function(unmatched_geoids) {
  if (length(unmatched_geoids) == 0) {
    return(NULL)
  }

  state_counts <- tibble::tibble(
    geoid = unmatched_geoids,
    state_fips = stringr::str_sub(geoid, 1, 2)
  ) |>
    dplyr::count(state_fips, name = "n_unmatched") |>
    dplyr::mutate(
      pct_of_unmatched = n_unmatched / sum(n_unmatched) * 100
    ) |>
    dplyr::arrange(dplyr::desc(n_unmatched))

  top_states <- state_counts |>
    dplyr::slice_head(n = 3)

  is_concentrated <- any(state_counts$pct_of_unmatched > 15)

  list(
    state_counts = state_counts,
    top_states = top_states,
    is_concentrated = is_concentrated
  )
}


#' Format Join Quality Message
#'
#' Internal function that formats join quality diagnostics as messages.
#'
#' @param join_quality List of join quality statistics.
#' @param step_number Integer. Current step number.
#' @param total_steps Integer. Total number of steps.
#' @return Character vector of messages to print.
#' @keywords internal
#' @noRd
format_join_quality_message <- function(join_quality, step_number, total_steps) {
  messages <- character(0)

  step_prefix <- if (total_steps > 1) {
    stringr::str_c("Step ", step_number, " join quality: ")
  } else {
    "Join quality: "
  }

  # Report unmatched data rows
  if (join_quality$n_data_unmatched > 0) {
    msg1 <- stringr::str_c(
      step_prefix,
      format(join_quality$n_data_unmatched, big.mark = ","),
      " of ",
      format(join_quality$n_data_total, big.mark = ","),
      " data rows (",
      sprintf("%.1f%%", join_quality$pct_data_unmatched),
      ") did not match the crosswalk."
    )
    messages <- c(messages, msg1)

    # Add state concentration info if available
    if (!is.null(join_quality$state_analysis_data) &&
        nrow(join_quality$state_analysis_data$top_states) > 0) {

      top_states_str <- join_quality$state_analysis_data$top_states |>
        dplyr::left_join(state_fips_to_abbr, by = "state_fips") |>
        dplyr::mutate(
          state_abbr = dplyr::if_else(is.na(state_abbr), state_fips, state_abbr),
          label = stringr::str_c(
            state_abbr, " (",
            sprintf("%.0f%%", pct_of_unmatched), ", ",
            format(n_unmatched, big.mark = ","), " rows)"
          )
        ) |>
        dplyr::pull(label) |>
        stringr::str_c(collapse = ", ")

      msg2 <- stringr::str_c("  Top states with unmatched data rows: ", top_states_str)
      messages <- c(messages, msg2)
    }
  }

  # Report crosswalk source GEOIDs not in data
  if (join_quality$n_crosswalk_unmatched > 0) {
    msg3 <- stringr::str_c(
      step_prefix,
      format(join_quality$n_crosswalk_unmatched, big.mark = ","),
      " of ",
      format(join_quality$n_crosswalk_total, big.mark = ","),
      " crosswalk source GEOIDs (",
      sprintf("%.1f%%", join_quality$pct_crosswalk_unmatched),
      ") were not in input data."
    )
    messages <- c(messages, msg3)

    # Add state concentration info if available
    if (!is.null(join_quality$state_analysis_crosswalk) &&
        nrow(join_quality$state_analysis_crosswalk$top_states) > 0) {

      top_states_str <- join_quality$state_analysis_crosswalk$top_states |>
        dplyr::left_join(state_fips_to_abbr, by = "state_fips") |>
        dplyr::mutate(
          state_abbr = dplyr::if_else(is.na(state_abbr), state_fips, state_abbr),
          label = stringr::str_c(
            state_abbr, " (",
            sprintf("%.0f%%", pct_of_unmatched), ", ",
            format(n_unmatched, big.mark = ","), " rows)"
          )
        ) |>
        dplyr::pull(label) |>
        stringr::str_c(collapse = ", ")

      msg4 <- stringr::str_c("  Top states not in data: ", top_states_str)
      messages <- c(messages, msg4)
    }

    msg5 <- "  (This is expected if your data covers a geographic subset.)"
    messages <- c(messages, msg5)
  }

  return(messages)
}


#' Report Join Quality Between Data and Crosswalk
#'
#' Internal function that computes and reports join quality diagnostics.
#'
#' @param data The input data tibble.
#' @param crosswalk The crosswalk tibble.
#' @param geoid_column Column name for source geoid in data.
#' @param step_number Integer. Which step this is (for multi-step reporting).
#' @param total_steps Integer. Total number of steps.
#' @param source_geography Character or NULL. The source geography type, used to
#'    determine if state-level analysis is applicable.
#' @return A list with join quality statistics (also prints messages if issues found).
#' @keywords internal
#' @noRd
report_join_quality <- function(data, crosswalk, geoid_column, step_number = 1,
                                total_steps = 1, source_geography = NULL) {

  # Ensure geoid columns are character for consistent comparison
  data_geoids <- data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(geoid_column), as.character)) |>
    dplyr::pull(!!rlang::sym(geoid_column)) |>
    unique()

  crosswalk_source_geoids <- crosswalk |>
    dplyr::pull(source_geoid) |>
    unique()

  # Data rows not in crosswalk
  data_not_in_crosswalk <- setdiff(data_geoids, crosswalk_source_geoids)
  n_data_unmatched <- length(data_not_in_crosswalk)
  n_data_total <- length(data_geoids)
  pct_data_unmatched <- if (n_data_total > 0) {
    n_data_unmatched / n_data_total * 100
  } else {
    0
  }

  # Crosswalk source GEOIDs not in data
  crosswalk_not_in_data <- setdiff(crosswalk_source_geoids, data_geoids)
  n_crosswalk_unmatched <- length(crosswalk_not_in_data)
  n_crosswalk_total <- length(crosswalk_source_geoids)
  pct_crosswalk_unmatched <- if (n_crosswalk_total > 0) {
    n_crosswalk_unmatched / n_crosswalk_total * 100
  } else {
    0
  }

  # State concentration analysis only for state-nested geographies
  # (ZCTAs, CBSAs, urban areas cross state boundaries so state analysis not meaningful)
  do_state_analysis <- is_state_nested_geography(source_geography)

  # State concentration analysis for unmatched data rows
  state_analysis_data <- if (n_data_unmatched > 0 && do_state_analysis) {
    analyze_state_concentration(data_not_in_crosswalk)
  } else {
    NULL
  }

  # State concentration analysis for crosswalk rows not in data
  state_analysis_crosswalk <- if (n_crosswalk_unmatched > 0 && do_state_analysis) {
    analyze_state_concentration(crosswalk_not_in_data)
  } else {
    NULL
  }

  # Build quality statistics
  join_quality <- list(
    n_data_total = n_data_total,
    n_data_unmatched = n_data_unmatched,
    pct_data_unmatched = pct_data_unmatched,
    data_geoids_unmatched = data_not_in_crosswalk,
    state_analysis_data = state_analysis_data,
    n_crosswalk_total = n_crosswalk_total,
    n_crosswalk_unmatched = n_crosswalk_unmatched,
    pct_crosswalk_unmatched = pct_crosswalk_unmatched,
    crosswalk_geoids_unmatched = crosswalk_not_in_data,
    state_analysis_crosswalk = state_analysis_crosswalk,
    source_geography = source_geography,
    state_analysis_applicable = do_state_analysis
  )

  # Print messages if there are issues
  messages <- format_join_quality_message(join_quality, step_number, total_steps)
  if (length(messages) > 0) {
    purrr::walk(messages, message)
  }

  return(join_quality)
}


#' Apply a Single Crosswalk Step
#'
#' Internal function that applies one crosswalk tibble to data.
#'
#' @param data Data to crosswalk
#' @param crosswalk A single crosswalk tibble
#' @param geoid_column Column name for source geoid
#' @param count_columns Count variable columns
#' @param non_count_columns Non-count variable columns
#' @param step_number Integer. Current step number for multi-step reporting.
#' @param total_steps Integer. Total number of steps for multi-step reporting.
#' @param show_join_quality Logical. Whether to report join quality diagnostics.
#' @return Crosswalked data
#' @keywords internal
#' @noRd
apply_single_crosswalk <- function(
    data,
    crosswalk,
    geoid_column,
    count_columns,
    non_count_columns,
    step_number = 1,
    total_steps = 1,
    show_join_quality = TRUE) {

  # Check if crosswalk is empty
  if (nrow(crosswalk) == 0) {
    warning(
      "Crosswalk is empty. If source geography is nested within target geography, ",
      "consider aggregating your data directly instead.")
    return(tibble::tibble())
  }

  # Store metadata for later attachment
  crosswalk_metadata <- attr(crosswalk, "crosswalk_metadata")

  # Extract source geography from metadata for state analysis determination
  source_geography <- if (!is.null(crosswalk_metadata)) {
    crosswalk_metadata$source_geography
  } else {
    NULL
  }

  # Report join quality (if enabled)
  join_quality <- if (show_join_quality) {
    report_join_quality(
      data = data,
      crosswalk = crosswalk,
      geoid_column = geoid_column,
      step_number = step_number,
      total_steps = total_steps,
      source_geography = source_geography)
  } else {
    NULL
  }

  # Determine grouping columns (target_geography_name may not always be present)
  group_cols <- "target_geoid"
  if ("target_geography_name" %in% names(crosswalk)) {
    group_cols <- c("target_geoid", "target_geography_name")
  }

  # Filter to columns that exist in current data (intermediate steps may have fewer)
  current_count_cols <- intersect(count_columns, names(data))
  current_non_count_cols <- intersect(non_count_columns, names(data))

  # Identify "other" columns (not geoid, count, or non-count columns)
  # These will be aggregated by taking the first non-missing value
  # Include both original crosswalk column names AND their renamed versions
  # (e.g., "geography_name" which comes from "target_geography_name" after renaming)
  # to prevent duplicates in multi-step crosswalks
  crosswalk_cols <- c("source_geoid", "target_geoid", "allocation_factor_source_to_target",
                      "source_geography_name", "target_geography_name",
                      "geography_name", "geoid",
                      "weighting_factor", "source_year", "target_year",
                      "population_2020", "housing_2020", "land_area_sqmi")
  other_cols <- setdiff(
    names(data),
    c(geoid_column, current_count_cols, current_non_count_cols, crosswalk_cols)
  )


  # Join crosswalk to data
  result <- data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(geoid_column), as.character)) |>
    dplyr::left_join(
      crosswalk,
      by = stats::setNames("source_geoid", geoid_column),
      relationship = "one-to-many") |>
    tidytable::summarize(
      .by = dplyr::all_of(group_cols),
      ## count variables we take the sum of the weighted count variable
      dplyr::across(
        .cols = dplyr::all_of(current_count_cols),
        .fns = ~ sum(.x * allocation_factor_source_to_target, na.rm = TRUE)),
      ## non-count variables--means, medians, percentages, ratios, etc.--
      ## we take the weighted mean of the variable, weighted by the allocation factor
      tidytable::across(
        .cols = tidytable::all_of(current_non_count_cols),
        .fns = ~ stats::weighted.mean(.x, allocation_factor_source_to_target, na.rm = TRUE)),
      ## other columns: take first non-missing value (or NA if all missing)
      tidytable::across(
        .cols = tidytable::all_of(other_cols),
        .fns = ~ dplyr::first(.x, na_rm = TRUE)),
      tidytable::across(
        .cols = tidytable::all_of(c(current_count_cols, current_non_count_cols)),
        .fns = ~ sum(!is.na(.x)),
        .names = "{.col}_validx")) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::all_of(c(current_count_cols, current_non_count_cols)),
        .fns = ~ tidytable::if_else(get(stringr::str_c(tidytable::cur_column(), "_validx")) > 0, .x, NA))) |>
    dplyr::select(-dplyr::matches("_validx$")) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ stringr::str_remove_all(.x, "target_")) |>
    tibble::as_tibble()

  # Attach metadata
  attr(result, "crosswalk_metadata") <- crosswalk_metadata

  # Attach join quality statistics
  attr(result, "join_quality") <- join_quality

  return(result)
}
