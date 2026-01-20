## explicitly enable/acknowledge data.table (used by tidytable)
.datatable.aware = TRUE

#' Interpolate data using a crosswalk(s)
#'
#' Applies geographic crosswalk weights to transform data from a source geography
#' to a target geography. Accepts the output from `get_crosswalk()` and automatically
#' applies all crosswalk steps sequentially for multi-step transformations.
#'
#' @param data A data frame or tibble containing the data to crosswalk.
#' @param crosswalk The output from `get_crosswalk()` - a list containing:
#'    \describe{
#'      \item{crosswalks}{A named list of crosswalk tibbles (step_1, step_2, etc.)}
#'      \item{plan}{The crosswalk plan}
#'      \item{message}{Description of the crosswalk chain}
#'    }
#'    Alternatively, a single crosswalk tibble can be provided for backwards
#'    compatibility.
#' @param geoid_column Character. The name of the column in `data` containing
#'    the source geography identifiers (GEOIDs). Default is "geoid".
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
#' **Multi-step crosswalks**: When `get_crosswalk()` returns multiple crosswalks
#' (for transformations that change both geography and year), this function
#' automatically applies them in sequence.
#'
#' @export
#' @examples
#' \dontrun{
#' # Single-step crosswalk
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
#' # Multi-step crosswalk (geography + year change)
#' crosswalk <- get_crosswalk(
#'   source_geography = "tract",
#'   target_geography = "zcta",
#'   source_year = 2010,
#'   target_year = 2020,
#'   weight = "population")
#'
#' # Automatically applies both steps
#' result <- crosswalk_data(
#'   data = my_data,
#'   crosswalk = crosswalk,
#'   geoid_column = "tract_geoid",
#'   count_columns = "count_population")
#'
#' # To get intermediate results
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
    crosswalk,
    geoid_column = "geoid",
    count_columns = NULL,
    non_count_columns = NULL,
    return_intermediate = FALSE) {

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
      non_count_columns = non_count_columns)

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
    "  2. A single crosswalk tibble with columns: source_geoid, target_geoid, allocation_factor_source_to_target")
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


#' Apply a Single Crosswalk Step
#'
#' Internal function that applies one crosswalk tibble to data.
#'
#' @param data Data to crosswalk
#' @param crosswalk A single crosswalk tibble
#' @param geoid_column Column name for source geoid
#' @param count_columns Count variable columns
#' @param non_count_columns Non-count variable columns
#' @return Crosswalked data
#' @keywords internal
#' @noRd
apply_single_crosswalk <- function(
    data,
    crosswalk,
    geoid_column,
    count_columns,
    non_count_columns) {

  # Check if crosswalk is empty
  if (nrow(crosswalk) == 0) {
    warning(
      "Crosswalk is empty. If source geography is nested within target geography, ",
      "consider aggregating your data directly instead.")
    return(tibble::tibble())
  }

  # Store metadata for later attachment
  crosswalk_metadata <- attr(crosswalk, "crosswalk_metadata")

  # Determine grouping columns (target_geography_name may not always be present)
  group_cols <- "target_geoid"
  if ("target_geography_name" %in% names(crosswalk)) {
    group_cols <- c("target_geoid", "target_geography_name")
  }

  # Filter to columns that exist in current data (intermediate steps may have fewer)
  current_count_cols <- intersect(count_columns, names(data))
  current_non_count_cols <- intersect(non_count_columns, names(data))

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
      tidytable::across(
        .cols = tidytable::all_of(c(current_count_cols, current_non_count_cols)),
        .fns = ~ sum(!is.na(.x)),
        .names = "{.col}_validx")) |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidytable::all_of(c(current_count_cols, current_non_count_cols)),
        .fns = ~ tidytable::if_else(get(stringr::str_c(tidytable::cur_column(), "_validx$")) > 0, .x, NA))) |>
    dplyr::select(-dplyr::matches("_validx$")) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ stringr::str_remove_all(.x, "target_")) |>
    tibble::as_tibble()

  # Attach metadata
  attr(result, "crosswalk_metadata") <- crosswalk_metadata

  return(result)
}
