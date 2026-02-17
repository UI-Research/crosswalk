# ============================================================================
# GEOID Length Audit Script
# ============================================================================
# Audits all crosswalks supported by the crosswalk package to verify that
# source_geoid and target_geoid columns have the correct number of characters
# for their geography type.
#
# Usage:
#   Rscript scripts/audit_geoid_lengths.R
#   # or from R console (from package root):
#   source("scripts/audit_geoid_lengths.R")
#
# Output:
#   Prints a summary table of all GEOID length checks. Crosswalks with
#   incorrect-length GEOIDs are flagged and detailed at the end.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
})

devtools::load_all(quiet = TRUE)

# ============================================================================
# Expected GEOID character lengths by geography type
# ============================================================================
# Standard Census GEOID structures:
#   block:                        15 chars (state=2 + county=3 + tract=6 + block=4)
#   block_group:                  12 chars (state=2 + county=3 + tract=6 + bg=1)
#   tract:                        11 chars (state=2 + county=3 + tract=6)
#   county:                        5 chars (state=2 + county=3)
#   place:                         7 chars (state=2 + place=5)
#   zcta:                          5 chars (5-digit ZCTA, no state prefix)
#   puma:                          7 chars (state=2 + puma=5)
#   core_based_statistical_area:   5 chars (5-digit CBSA code)
#   urban_area:                    5 chars (5-digit UA code)
#   congressional district:        4 chars (state=2 + district=2)

expected_geoid_lengths <- c(
  "block" = 15L,
  "block_group" = 12L,
  "block group" = 12L,
  "tract" = 11L,
  "county" = 5L,
  "place" = 7L,
  "zcta" = 5L,
  "puma" = 7L,
  "puma12" = 7L,
  "puma22" = 7L,
  "core_based_statistical_area" = 5L,
  "urban_area" = 5L,
  "cd115" = 4L,
  "cd116" = 4L,
  "cd118" = 4L,
  "cd119" = 4L
)

# ============================================================================
# Helper: Check GEOID lengths for a single crosswalk result
# ============================================================================
check_geoid_lengths <- function(result, label) {
  checks <- list()

  for (step_name in names(result$crosswalks)) {
    step_df <- result$crosswalks[[step_name]]
    metadata <- attr(step_df, "crosswalk_metadata")

    if (is.null(step_df) || nrow(step_df) == 0) {
      checks[[length(checks) + 1]] <- tibble(
        label = label,
        step = step_name,
        column = "source_geoid",
        geography = NA_character_,
        expected_length = NA_integer_,
        actual_lengths = "EMPTY",
        n_rows = 0L,
        n_wrong = NA_integer_,
        pct_wrong = NA_real_,
        pass = NA
      )
      next
    }

    # Get geography names from metadata
    source_geog <- metadata$call_parameters$source_geography
    target_geog <- metadata$call_parameters$target_geography
    if (is.null(source_geog)) source_geog <- metadata$source_geography
    if (is.null(target_geog)) target_geog <- metadata$target_geography

    expected_source <- unname(expected_geoid_lengths[source_geog])
    expected_target <- unname(expected_geoid_lengths[target_geog])

    source_lengths <- nchar(step_df$source_geoid)
    target_lengths <- nchar(step_df$target_geoid)

    n <- nrow(step_df)

    # Check source_geoid
    n_wrong_source <- if (!is.na(expected_source)) {
      sum(source_lengths != expected_source, na.rm = TRUE)
    } else {
      NA_integer_
    }

    checks[[length(checks) + 1]] <- tibble(
      label = label,
      step = step_name,
      column = "source_geoid",
      geography = source_geog,
      expected_length = as.integer(expected_source),
      actual_lengths = paste(sort(unique(source_lengths)), collapse = ", "),
      n_rows = n,
      n_wrong = as.integer(n_wrong_source),
      pct_wrong = if (!is.na(n_wrong_source) && n > 0) round(100 * n_wrong_source / n, 2) else NA_real_,
      pass = if (!is.na(expected_source)) all(source_lengths == expected_source, na.rm = TRUE) else NA
    )

    # Check target_geoid
    n_wrong_target <- if (!is.na(expected_target)) {
      sum(target_lengths != expected_target, na.rm = TRUE)
    } else {
      NA_integer_
    }

    checks[[length(checks) + 1]] <- tibble(
      label = label,
      step = step_name,
      column = "target_geoid",
      geography = target_geog,
      expected_length = as.integer(expected_target),
      actual_lengths = paste(sort(unique(target_lengths)), collapse = ", "),
      n_rows = n,
      n_wrong = as.integer(n_wrong_target),
      pct_wrong = if (!is.na(n_wrong_target) && n > 0) round(100 * n_wrong_target / n, 2) else NA_real_,
      pass = if (!is.na(expected_target)) all(target_lengths == expected_target, na.rm = TRUE) else NA
    )
  }

  bind_rows(checks)
}

# ============================================================================
# Define crosswalks to test
# ============================================================================

# --- GeoCorr 2022 (no year, or year >= 2020) ---
# One crosswalk per geography type to verify GEOID construction.
# Note: tract->county is nested (returns empty); use non-nested pairs instead.
geocorr_2022_tests <- tribble(
  ~source_geography, ~target_geography,
  "tract",       "zcta",
  "tract",       "puma",
  "tract",       "place",
  "block_group", "zcta",
  "zcta",        "county",
  "county",      "zcta",
  "cd118",       "zcta",
  "cd119",       "zcta"
)

# --- GeoCorr 2018 (year = 2010) ---
geocorr_2018_tests <- tribble(
  ~source_geography, ~target_geography,
  "tract",       "zcta",
  "tract",       "puma",
  "tract",       "place",
  "block_group", "zcta",
  "zcta",        "county",
  "county",      "zcta",
  "cd115",       "zcta",
  "cd116",       "zcta"
)

# --- CTData 2020-2022 ---
ctdata_tests <- c("tract", "block_group", "county")

# --- NHGIS (representative set covering each geography type combo) ---
nhgis_tests <- tribble(
  ~source_geography, ~source_year, ~target_geography, ~target_year,
  # Same-geography cross-decade
  "tract",       2010, "tract",       2020,
  "block_group", 2010, "block_group", 2020,
  "tract",       1990, "tract",       2010,
  "tract",       2000, "tract",       2010,
  "tract",       2020, "tract",       2010,
  # Cross-geography cross-decade
  "tract",       2010, "county",      2020,
  "block_group", 2010, "county",      2020,
  "block_group", 2010, "tract",       2020,
  # Non-census target years
  "tract",       2010, "tract",       2022,
  "block_group", 2010, "block_group", 2022,
  "tract",       2010, "tract",       2011,
  "tract",       2010, "tract",       2012,
  "tract",       2010, "tract",       2014,
  "tract",       2010, "tract",       2015
)

# ============================================================================
# Run the audit
# ============================================================================
all_results <- list()
all_errors <- list()

safe_test <- function(label, fetch_fn) {
  cat(str_c("  ", label, " ... "))
  tryCatch({
    result <- fetch_fn()
    checks <- check_geoid_lengths(result, label)
    status <- if (all(checks$pass, na.rm = TRUE)) "PASS" else "FAIL"
    cat(status, "\n")
    all_results[[length(all_results) + 1]] <<- checks
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    all_errors[[length(all_errors) + 1]] <<- tibble(
      label = label, error = conditionMessage(e))
  })
}

# --- GeoCorr 2022 ---
cat("\n============================\n")
cat("GeoCorr 2022 crosswalks\n")
cat("============================\n")
for (i in seq_len(nrow(geocorr_2022_tests))) {
  row <- geocorr_2022_tests[i, ]
  label <- str_c("geocorr2022: ", row$source_geography, " -> ", row$target_geography)
  safe_test(label, function() {
    get_crosswalk(
      source_geography = row$source_geography,
      target_geography = row$target_geography,
      weight = "population")
  })
}

# --- GeoCorr 2018 ---
cat("\n============================\n")
cat("GeoCorr 2018 crosswalks\n")
cat("============================\n")
for (i in seq_len(nrow(geocorr_2018_tests))) {
  row <- geocorr_2018_tests[i, ]
  label <- str_c("geocorr2018: ", row$source_geography, " -> ", row$target_geography)
  safe_test(label, function() {
    get_crosswalk(
      source_geography = row$source_geography,
      target_geography = row$target_geography,
      source_year = 2010,
      target_year = 2010,
      weight = "population")
  })
}

# --- CTData 2020-2022 ---
cat("\n============================\n")
cat("CTData 2020-2022 crosswalks\n")
cat("============================\n")
for (geog in ctdata_tests) {
  label <- str_c("ctdata: ", geog, " 2020 -> 2022")
  safe_test(label, function() {
    get_crosswalk(
      source_geography = geog,
      target_geography = geog,
      source_year = 2020,
      target_year = 2022)
  })
}

# --- NHGIS ---
cat("\n============================\n")
cat("NHGIS crosswalks\n")
cat("============================\n")

has_ipums_key <- Sys.getenv("IPUMS_API_KEY") != ""
if (!has_ipums_key) {
  cat("  SKIPPING: IPUMS_API_KEY not set\n")
} else {
  for (i in seq_len(nrow(nhgis_tests))) {
    row <- nhgis_tests[i, ]
    label <- str_c(
      "nhgis: ", row$source_geography, " ", row$source_year,
      " -> ", row$target_geography, " ", row$target_year)
    safe_test(label, function() {
      get_crosswalk(
        source_geography = row$source_geography,
        target_geography = row$target_geography,
        source_year = row$source_year,
        target_year = row$target_year,
        weight = "population")
    })
  }
}

# ============================================================================
# Summary report
# ============================================================================
cat("\n\n")
cat("============================================================\n")
cat("GEOID LENGTH AUDIT REPORT\n")
cat("============================================================\n\n")

results_df <- bind_rows(all_results)
errors_df <- bind_rows(all_errors)

# Overall summary
n_checks <- nrow(results_df)
n_pass <- sum(results_df$pass, na.rm = TRUE)
n_fail <- sum(!results_df$pass, na.rm = TRUE)
n_na <- sum(is.na(results_df$pass))
n_errors <- nrow(errors_df)

cat(str_c("Total checks:  ", n_checks, "\n"))
cat(str_c("Passed:        ", n_pass, "\n"))
cat(str_c("Failed:        ", n_fail, "\n"))
cat(str_c("Skipped/NA:    ", n_na, "\n"))
cat(str_c("Fetch errors:  ", n_errors, "\n\n"))

# Show failures
if (n_fail > 0) {
  cat("------------------------------------------------------------\n")
  cat("FAILURES (wrong-length GEOIDs)\n")
  cat("------------------------------------------------------------\n\n")

  failures <- results_df |> filter(!pass)
  for (i in seq_len(nrow(failures))) {
    f <- failures[i, ]
    cat(str_c(
      "  Crosswalk:  ", f$label, "\n",
      "  Step:       ", f$step, "\n",
      "  Column:     ", f$column, "\n",
      "  Geography:  ", f$geography, "\n",
      "  Expected:   ", f$expected_length, " chars\n",
      "  Actual:     ", f$actual_lengths, " chars\n",
      "  Rows:       ", format(f$n_rows, big.mark = ","), "\n",
      "  Wrong:      ", format(f$n_wrong, big.mark = ","),
      " (", f$pct_wrong, "%)\n\n"))
  }
} else {
  cat("All GEOID length checks PASSED.\n\n")
}

# Show NA/unknown expected lengths
if (n_na > 0) {
  cat("------------------------------------------------------------\n")
  cat("UNKNOWN expected lengths (no entry in expected_geoid_lengths)\n")
  cat("------------------------------------------------------------\n\n")
  na_checks <- results_df |> filter(is.na(pass))
  for (i in seq_len(nrow(na_checks))) {
    r <- na_checks[i, ]
    cat(str_c(
      "  ", r$label, " | ", r$column, " | geography: ", r$geography,
      " | actual: ", r$actual_lengths, "\n"))
  }
  cat("\n")
}

# Show fetch errors
if (n_errors > 0) {
  cat("------------------------------------------------------------\n")
  cat("FETCH ERRORS\n")
  cat("------------------------------------------------------------\n\n")
  for (i in seq_len(nrow(errors_df))) {
    e <- errors_df[i, ]
    cat(str_c("  ", e$label, ": ", e$error, "\n"))
  }
  cat("\n")
}

# Full results table
cat("------------------------------------------------------------\n")
cat("FULL RESULTS TABLE\n")
cat("------------------------------------------------------------\n\n")
print(results_df |> select(label, step, column, geography, expected_length, actual_lengths, pass), n = 200)

cat("\n\nAudit complete.\n")
