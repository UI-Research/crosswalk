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
