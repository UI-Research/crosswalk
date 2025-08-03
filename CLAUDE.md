# geocorr-api R Package

## Project Overview
This R package provides simplified programmatic access to geographic crosswalks from the geocorr2022 website (https://mcdc.missouri.edu/applications/geocorr2022.html). It allows users to obtain geographic correspondence tables between different geographic units (e.g., census tracts to ZIP codes, counties to congressional districts) without manually using the web interface.

## Key Features
- Simplified programmatic access to geocorr2022 geographic crosswalks
- Support for various geographic unit combinations
- Automated data retrieval and processing
- Always returns crosswalks for all 50 states (no state subsetting)
- Consistent data frame output format
- No complex filtering options - focuses on core crosswalk functionality

## Project Design
- Use tools from the `rvest` package to dynamically interact with the geocorr22 website and submit a request for a crosswalk
- Use tools from the `rvest` package to download the requested website once it is available from geocorr22
- Standardize the returned crosswalk so that users are always returned a crosswalk with the same column names, regardless of the geographies that are being crosswalked
- Simplify the geocorr2022 interface by removing complex options and always requesting nationwide data

## Simplified Interface Design
- **No state selection**: Always returns crosswalks for all 50 states
- **Single output format**: Always returns an R data frame (no CSV, HTML, or PDF options)
- **No geographic filters**: Removes optional county codes, metro area codes, and other filtering options
- **Core functionality only**: Focuses on essential crosswalk generation without advanced customization

## Development Guidelines
- Follow R package development best practices
- Use roxygen2 for documentation
- Include comprehensive examples in function documentation
- Ensure proper error handling for API interactions
- Write unit tests for all public functions
- Consider rate limiting for web requests

## Testing
- Run `devtools::test()` to execute unit tests
- Use `devtools::check()` before submitting changes
- Test with various geographic unit combinations

## Dependencies
- Tidyverse packages: dplyr, stringr, purrr, tibble
- API packages: httr2, jsonlite
- Web scraping: rvest
- Development tools: devtools, testthat, roxygen2

## Coding Style Guidelines
- **Always prefer tidyverse packages over base R equivalents**
- Use stringr functions instead of base R string functions (never use grepl, gsub, regexpr, etc.)
- Use dplyr for data manipulation instead of base R
- Use purrr for functional programming instead of apply family functions
- Use tibble instead of data.frame when possible
- Follow tidyverse style conventions and pipe operators