# Tests for silent mode functionality

# ==============================================================================
# Helper: mock crosswalk for testing crosswalk_data() without network calls
# ==============================================================================

make_mock_crosswalk <- function() {
  xwalk <- tibble::tibble(
    source_geoid = c("A", "A", "B"),
    target_geoid = c("X", "Y", "Y"),
    allocation_factor_source_to_target = c(0.6, 0.4, 1.0),
    source_geography_name = "mock",
    target_geography_name = "mock")

  attr(xwalk, "crosswalk_metadata") <- list(
    source_geography = "tract",
    target_geography = "zcta")

  list(
    crosswalks = list(step_1 = xwalk),
    plan = NULL,
    message = "Mock crosswalk")
}

make_mock_data <- function() {
  tibble::tibble(
    source_geoid = c("A", "B"),
    count_population = c(100, 200))
}

# ==============================================================================
# get_crosswalk() silent parameter
# ==============================================================================

test_that("get_crosswalk with silent = TRUE suppresses warning for nested geographies", {
  expect_silent(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "county",
      silent = TRUE))
})

test_that("get_crosswalk with silent = FALSE emits warning for nested geographies", {
  expect_warning(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "county",
      silent = FALSE),
    "nested within the target geography")
})

# ==============================================================================
# crosswalk_data() silent parameter
# ==============================================================================

test_that("crosswalk_data with silent = TRUE suppresses messages", {
  mock_cw <- make_mock_crosswalk()
  mock_data <- make_mock_data()

  expect_silent(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_cw,
      count_columns = "count_population",
      silent = TRUE))
})

test_that("crosswalk_data with silent = FALSE emits messages", {
  mock_cw <- make_mock_crosswalk()
  mock_data <- make_mock_data()

  expect_message(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_cw,
      count_columns = "count_population",
      silent = FALSE),
    "Applying crosswalk step")
})

test_that("crosswalk_data silent = TRUE suppresses join quality even when show_join_quality = TRUE", {
  mock_cw <- make_mock_crosswalk()
  # Data with an unmatched GEOID to trigger join quality message
  mock_data <- tibble::tibble(
    source_geoid = c("A", "B", "C"),
    count_population = c(100, 200, 300))

  expect_silent(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_cw,
      count_columns = "count_population",
      show_join_quality = TRUE,
      silent = TRUE))
})

test_that("crosswalk_data silent = TRUE suppresses warning for both crosswalk and geography params", {
  mock_cw <- make_mock_crosswalk()
  mock_data <- make_mock_data()

  expect_silent(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_cw,
      source_geography = "tract",
      target_geography = "zcta",
      count_columns = "count_population",
      silent = TRUE))
})

# ==============================================================================
# Global option: crosswalk.silent
# ==============================================================================

test_that("global option crosswalk.silent = TRUE works as default", {
  old_opts <- options(crosswalk.silent = TRUE)
  on.exit(options(old_opts), add = TRUE)

  expect_silent(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "county"))
})

test_that("explicit silent = FALSE overrides global crosswalk.silent = TRUE", {
  old_opts <- options(crosswalk.silent = TRUE)
  on.exit(options(old_opts), add = TRUE)

  expect_warning(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "county",
      silent = FALSE),
    "nested within the target geography")
})

# ==============================================================================
# Errors still surface in silent mode
# ==============================================================================

test_that("stop() errors still surface when silent = TRUE", {
  expect_error(
    get_crosswalk(
      source_geography = "tract",
      target_geography = "zcta",
      source_year = 2005,
      target_year = 2020,
      silent = TRUE))
})

test_that("crosswalk_data stop() errors still surface when silent = TRUE", {
  expect_error(
    crosswalk_data(
      data = tibble::tibble(x = 1),
      count_columns = "count_foo",
      silent = TRUE),
    "Either provide a crosswalk")
})

# ==============================================================================
# Internal helpers respect the option
# ==============================================================================

test_that("cw_message is silent when option is TRUE", {
  old_opts <- options(crosswalk.silent = TRUE)
  on.exit(options(old_opts), add = TRUE)

  expect_silent(crosswalk:::cw_message("test message"))
})

test_that("cw_message emits message when option is FALSE", {
  old_opts <- options(crosswalk.silent = FALSE)
  on.exit(options(old_opts), add = TRUE)

  expect_message(crosswalk:::cw_message("test message"), "test message")
})

test_that("cw_warning is silent when option is TRUE", {
  old_opts <- options(crosswalk.silent = TRUE)
  on.exit(options(old_opts), add = TRUE)

  expect_silent(crosswalk:::cw_warning("test warning"))
})

test_that("cw_warning emits warning when option is FALSE", {
  old_opts <- options(crosswalk.silent = FALSE)
  on.exit(options(old_opts), add = TRUE)

  expect_warning(crosswalk:::cw_warning("test warning"), "test warning")
})
