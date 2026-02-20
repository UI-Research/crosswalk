#' Get a Chain of Crosswalks for Multi-Step Transformations
#'
#' Retrieves a list of crosswalks needed to transform data from a source
#' geography/year to a target geography/year. For multi-step transformations,
#' users should apply each crosswalk sequentially using `crosswalk_data()`.
#'
#' @param source_geography Character. Source geography name.
#' @param target_geography Character. Target geography name.
#' @param source_year Numeric or NULL. Year of the source geography.
#' @param target_year Numeric or NULL. Year of the target geography.
#' @param weight Character or NULL. Weighting variable for Geocorr crosswalks.
#' @param cache Directory path or NULL. Where to cache crosswalks.
#'
#' @return A list with:
#'   \describe{
#'     \item{crosswalks}{A named list of crosswalk tibbles (step_1, step_2, etc.)}
#'     \item{plan}{The crosswalk plan from plan_crosswalk_chain()}
#'     \item{message}{A formatted message describing the crosswalk chain}
#'   }
#'
#' @keywords internal
#' @noRd
get_crosswalk_chain <- function(
    source_geography,
    target_geography,
    source_year = NULL,
    target_year = NULL,
    weight = "population",
    cache = NULL) {

  # Get the plan
  plan <- plan_crosswalk_chain(
    source_geography = source_geography,
    target_geography = target_geography,
    source_year = source_year,
    target_year = target_year,
    weight = weight)

  # Check for planning errors
  if (!is.null(plan$error)) {
    stop(plan$error)
  }

  # Initialize result
  result <- list(
    crosswalks = list(),
    plan = plan,
    message = format_chain_plan_message(plan))

  # Print the plan message
  cw_message(result$message)

  # Handle case where no crosswalk is needed
  if (nrow(plan$steps) > 0 && plan$steps$crosswalk_source[1] == "none") {
    cw_message("Returning empty crosswalk list since no transformation is needed.")
    return(result)
  }

  # Fetch each crosswalk step
  for (i in seq_len(nrow(plan$steps))) {
    step <- plan$steps[i, ]
    step_name <- stringr::str_c("step_", i)

    cw_message(stringr::str_c("\nFetching ", step_name, ": ", step$description))

    crosswalk_i <- get_crosswalk_single(
      source_geography = step$source_geography,
      target_geography = step$target_geography,
      source_year = if (!is.na(step$source_year)) as.numeric(step$source_year) else NULL,
      target_year = if (!is.na(step$target_year)) as.numeric(step$target_year) else NULL,
      weight = weight,
      cache = cache)

    result$crosswalks[[step_name]] <- crosswalk_i
  }

  return(result)
}


