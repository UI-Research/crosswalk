# Tests for crosswalk_data() - data transformation function

# ==============================================================================
# Basic functionality tests
# ==============================================================================

test_that("crosswalk_data returns expected structure", {
  skip_if_offline()

  # Create mock data
  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200", "01001020300"),
    count_population = c(1000, 2000, 1500),
    mean_income = c(50000, 60000, 45000))

  # Get a crosswalk first
  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"),
    non_count_columns = c("mean_income"))

  expect_s3_class(result, "tbl_df")
  expect_true("geoid" %in% colnames(result))
  expect_true("count_population" %in% colnames(result))
  expect_true("mean_income" %in% colnames(result))
})

test_that("crosswalk_data attaches metadata", {
  skip_if_offline()

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100"),
    count_population = c(1000))

  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"))

  metadata <- attr(result, "crosswalk_metadata")
  expect_type(metadata, "list")
})

# ==============================================================================
# Count variable interpolation tests
# ==============================================================================

test_that("crosswalk_data correctly interpolates count variables", {
  # Create a controlled test case
  mock_data <- tibble::tibble(
    source_geoid = c("A", "B"),
    count_value = c(100, 200))

  # Create a mock crosswalk
  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A", "A", "B"),
    target_geoid = c("X", "Y", "X"),
    target_geography_name = c("test", "test", "test"),
    allocation_factor_source_to_target = c(0.6, 0.4, 1.0))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "source_geoid",
    count_columns = "count_value")

  # A -> X: 100 * 0.6 = 60
  # B -> X: 200 * 1.0 = 200
  # X total: 260
  x_result <- result |> dplyr::filter(geoid == "X") |> dplyr::pull(count_value)
  expect_equal(x_result, 260)

  # A -> Y: 100 * 0.4 = 40
  y_result <- result |> dplyr::filter(geoid == "Y") |> dplyr::pull(count_value)
  expect_equal(y_result, 40)
})

# ==============================================================================
# Non-count variable interpolation tests
# ==============================================================================

test_that("crosswalk_data correctly interpolates non-count variables", {
  # Create a controlled test case with weighted means
  mock_data <- tibble::tibble(
    source_geoid = c("A", "B"),
    mean_income = c(50000, 80000))

  # Create a mock crosswalk
  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A", "B"),
    target_geoid = c("X", "X"),
    target_geography_name = c("test", "test"),
    allocation_factor_source_to_target = c(0.3, 0.7))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "source_geoid",
    non_count_columns = "mean_income")

  # For non-count variables, we use weighted mean
  # Weighted mean = (50000 * 0.3 + 80000 * 0.7) / (0.3 + 0.7) = 71000
  expected_x <- stats::weighted.mean(c(50000, 80000), c(0.3, 0.7))

  x_result <- result |> dplyr::filter(geoid == "X") |> dplyr::pull(mean_income)
  expect_equal(x_result, expected_x)
})

# ==============================================================================
# Auto-detection of column types tests
# ==============================================================================

test_that("crosswalk_data auto-detects count_ prefixed columns", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000),
    count_housing = c(500),
    other_column = c(100))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    target_geography_name = c("test"),
    allocation_factor_source_to_target = c(1.0))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "geoid")

  # Auto-detected count columns should be in result
  expect_true("count_population" %in% colnames(result))
  expect_true("count_housing" %in% colnames(result))
})

test_that("crosswalk_data auto-detects mean_ prefixed columns", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000),
    mean_income = c(50000))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    target_geography_name = c("test"),
    allocation_factor_source_to_target = c(1.0))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "geoid")

  expect_true("mean_income" %in% colnames(result))
})

test_that("crosswalk_data auto-detects median_ prefixed columns", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000),
    median_age = c(35))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    target_geography_name = c("test"),
    allocation_factor_source_to_target = c(1.0))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "geoid")

  expect_true("median_age" %in% colnames(result))
})

test_that("crosswalk_data auto-detects percent_ prefixed columns", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000),
    percent_employed = c(0.65))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    target_geography_name = c("test"),
    allocation_factor_source_to_target = c(1.0))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "geoid")

  expect_true("percent_employed" %in% colnames(result))
})

test_that("crosswalk_data auto-detects ratio_ prefixed columns", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000),
    ratio_income_to_poverty = c(2.5))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    target_geography_name = c("test"),
    allocation_factor_source_to_target = c(1.0))

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "geoid")

  expect_true("ratio_income_to_poverty" %in% colnames(result))
})

# ==============================================================================
# Error handling tests
# ==============================================================================

test_that("crosswalk_data errors when geoid_column not found", {
  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100"),
    count_population = c(1000))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("01001020100"),
    target_geoid = c("X"),
    allocation_factor_source_to_target = c(1.0))

  expect_error(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_crosswalk,
      geoid_column = "wrong_column",
      count_columns = c("count_population")),
    regexp = "not found")
})

test_that("crosswalk_data errors when no columns to crosswalk", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    some_column = c(100))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    allocation_factor_source_to_target = c(1.0))

  expect_error(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_crosswalk,
      geoid_column = "geoid"),
    regexp = "No columns to crosswalk")
})

test_that("crosswalk_data errors when specified columns not found", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"),
    allocation_factor_source_to_target = c(1.0))

  expect_error(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_crosswalk,
      geoid_column = "geoid",
      count_columns = c("nonexistent_column")),
    regexp = "not found")
})

test_that("crosswalk_data errors on invalid crosswalk input", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000))

  # Completely invalid input should error
  expect_error(
    crosswalk_data(
      data = mock_data,
      crosswalk = list(other_field = "something"),
      geoid_column = "geoid",
      count_columns = c("count_population")),
    regexp = "Invalid crosswalk input")

  # String input should error
  expect_error(
    crosswalk_data(
      data = mock_data,
      crosswalk = "not_a_crosswalk",
      geoid_column = "geoid",
      count_columns = c("count_population")),
    regexp = "Invalid crosswalk input")
})

test_that("crosswalk_data errors when crosswalk missing required columns", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("A"),
    target_geoid = c("X"))

  expect_error(
    crosswalk_data(
      data = mock_data,
      crosswalk = mock_crosswalk,
      geoid_column = "geoid",
      count_columns = c("count_population")),
    regexp = "missing required columns")
})

# ==============================================================================
# Empty crosswalk handling tests
# ==============================================================================

test_that("crosswalk_data warns for empty crosswalk", {
  mock_data <- tibble::tibble(
    geoid = c("A"),
    count_population = c(1000))

  empty_crosswalk <- tibble::tibble(
    source_geoid = character(),
    target_geoid = character(),
    allocation_factor_source_to_target = numeric())

  expect_warning(
    result <- crosswalk_data(
      data = mock_data,
      crosswalk = empty_crosswalk,
      geoid_column = "geoid",
      count_columns = c("count_population")),
    regexp = "empty")

  # Should return empty tibble
  expect_equal(nrow(result), 0)
})

# ==============================================================================
# Inter-temporal crosswalk tests
# ==============================================================================

test_that("crosswalk_data works with NHGIS inter-temporal crosswalk", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200"),
    count_population = c(1000, 2000))

  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2010,
    target_year = 2020)

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"))

  expect_s3_class(result, "tbl_df")
  expect_true("geoid" %in% colnames(result))
  expect_true("count_population" %in% colnames(result))
})

# ==============================================================================
# 2020-2022 crosswalk tests
# ==============================================================================

test_that("crosswalk_data works with 2020-2022 CT crosswalk", {
  skip_if_offline()

  # Use CT tract GEOIDs
  mock_data <- tibble::tibble(
    tract_geoid = c("09001010101", "09001010102"),
    count_population = c(1000, 2000))

  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "tract",
    source_year = 2020,
    target_year = 2022)

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"))

  expect_s3_class(result, "tbl_df")
  expect_true("geoid" %in% colnames(result))

  # Since CT 2020-2022 is identity mapping for tracts, values should be preserved
  expect_equal(sum(result$count_population), 3000)
})

# ==============================================================================
# Multi-step crosswalk tests
# ==============================================================================

test_that("crosswalk_data automatically applies multi-step crosswalk from get_crosswalk", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200"),
    count_population = c(1000, 2000))

  # Get multi-step crosswalk (returns list with multiple crosswalks)
  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  expect_type(crosswalk, "list")
  expect_true("crosswalks" %in% names(crosswalk))
  expect_equal(length(crosswalk$crosswalks), 2)

  # crosswalk_data automatically applies all steps
  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"))

  expect_s3_class(result, "tbl_df")
  expect_true("geoid" %in% colnames(result))
  expect_true("count_population" %in% colnames(result))

  # Total population should approximately be preserved
  expect_gt(sum(result$count_population, na.rm = TRUE), 0)
})

test_that("crosswalk_data can apply individual crosswalk steps manually", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200"),
    count_population = c(1000, 2000))

  # Get multi-step crosswalk (returns list)
  chain <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  # Apply step 1 manually (passing individual tibble)
  step1_result <- crosswalk_data(
    data = mock_data,
    crosswalk = chain$crosswalks$step_1,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"))

  expect_s3_class(step1_result, "tbl_df")
  expect_true("geoid" %in% colnames(step1_result))

  # Apply step 2 manually
  step2_result <- crosswalk_data(
    data = step1_result,
    crosswalk = chain$crosswalks$step_2,
    geoid_column = "geoid",
    count_columns = c("count_population"))

  expect_s3_class(step2_result, "tbl_df")
  expect_true("geoid" %in% colnames(step2_result))
  expect_true("count_population" %in% colnames(step2_result))

  # Total population should approximately be preserved
  expect_gt(sum(step2_result$count_population, na.rm = TRUE), 0)
})

# ==============================================================================
# return_intermediate parameter tests
# ==============================================================================

test_that("crosswalk_data return_intermediate=TRUE returns intermediate results for multi-step", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200"),
    count_population = c(1000, 2000))

  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"),
    return_intermediate = TRUE)

  # Should return a list with final and intermediate
  expect_type(result, "list")
  expect_true("final" %in% names(result))
  expect_true("intermediate" %in% names(result))

  # Final should be a tibble
  expect_s3_class(result$final, "tbl_df")
  expect_true("geoid" %in% colnames(result$final))

  # Intermediate should have results from each step
  expect_type(result$intermediate, "list")
  expect_true("step_1" %in% names(result$intermediate))
  expect_true("step_2" %in% names(result$intermediate))
  expect_s3_class(result$intermediate$step_1, "tbl_df")
  expect_s3_class(result$intermediate$step_2, "tbl_df")
})

test_that("crosswalk_data return_intermediate=FALSE returns tibble for multi-step", {
  skip_if_offline()
  skip_if(Sys.getenv("IPUMS_API_KEY") == "", "IPUMS_API_KEY not set")

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200"),
    count_population = c(1000, 2000))

  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    source_year = 2010,
    target_year = 2020,
    weight = "population")

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"),
    return_intermediate = FALSE)

  # Should return just a tibble, not a list
  expect_s3_class(result, "tbl_df")
  expect_true("geoid" %in% colnames(result))
})

test_that("crosswalk_data return_intermediate=TRUE returns tibble for single-step", {
  skip_if_offline()

  mock_data <- tibble::tibble(
    tract_geoid = c("01001020100", "01001020200"),
    count_population = c(1000, 2000))

  # Single-step crosswalk
  crosswalk <- get_crosswalk(
    source_geography = "tract",
    target_geography = "zcta",
    weight = "population")

  result <- crosswalk_data(
    data = mock_data,
    crosswalk = crosswalk,
    geoid_column = "tract_geoid",
    count_columns = c("count_population"),
    return_intermediate = TRUE)

  # For single-step, return_intermediate=TRUE returns tibble (no intermediate to return)
  expect_s3_class(result, "tbl_df")
})

# ==============================================================================
# Character GEOID handling tests
# ==============================================================================

test_that("crosswalk_data handles numeric GEOIDs", {
  mock_data <- tibble::tibble(
    geoid = c(1, 2),  # Numeric
    count_population = c(1000, 2000))

  mock_crosswalk <- tibble::tibble(
    source_geoid = c("1", "2"),
    target_geoid = c("X", "X"),
    target_geography_name = c("test", "test"),
    allocation_factor_source_to_target = c(0.5, 0.5))

  # Should work - function converts geoid to character
  result <- crosswalk_data(
    data = mock_data,
    crosswalk = mock_crosswalk,
    geoid_column = "geoid",
    count_columns = c("count_population"))

  expect_s3_class(result, "tbl_df")
})
