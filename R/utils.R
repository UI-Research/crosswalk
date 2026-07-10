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
