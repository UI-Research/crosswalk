# nhgis_crosswalks = c(
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_blk2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp1990_bg2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp1990_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp1990_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_blk2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2000_bg2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2000_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2000_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_blk2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_bg2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_tr2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_co2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2010_bg2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2010_tr2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2010_co2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_tr2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_co2020.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_blk2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2020_bg2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2020_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bgp2020_co2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2010.zip",
#     "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_co2010.zip"
# )

#' Standardize Geography Names
#'
#' Internal helper function to convert various geography name spellings to standard codes.
#'
#' @param geography Character. Geography name in various formats.
#' @param context Character. Either "source" or "target" to determine valid options.
#' @return Character. Standardized geography code.
#' @keywords internal
standardize_geography <- function(geography, context = "source") {
  # Convert to lowercase and remove extra whitespace
  geography <- geography |> stringr::str_to_lower() |> stringr::str_squish() |> stringr::str_trim()

  # Define mapping for different spellings
  geography_mapping <- list(
    # Blocks
    "blk" = "blk",
    "block" = "blk",
    "blocks" = "blk",
    "census block" = "blk",
    "census blocks" = "blk",

    # Block groups
    "bg" = "bg",
    "blockgroup" = "bg",
    "block group" = "bg",
    "blockgroups" = "bg",
    "block groups" = "bg",
    "census block group" = "bg",
    "census block groups" = "bg",

    # Block group parts (source only)
    "bgp" = "bgp",
    "block group part" = "bgp",
    "block group parts" = "bgp",
    "blockgroup part" = "bgp",
    "blockgroup parts" = "bgp",
    "census block group part" = "bgp",
    "census block group parts" = "bgp",

    # Tracts
    "tr" = "tr",
    "tract" = "tr",
    "tracts" = "tr",
    "census tract" = "tr",
    "census tracts" = "tr",

    # Counties
    "co" = "co",
    "county" = "co",
    "counties" = "co",
    "cnty" = "co"
  )

  # Check if the geography is in our mapping
  if (geography %in% names(geography_mapping)) {
    standardized <- geography_mapping[[geography]]

    # Validate based on context (source vs target geographies have different options)
    if (context == "source") {
      valid_geogs <- c("blk", "bgp", "tr")
      if (standardized %in% valid_geogs) {
        return(standardized)
      }
    } else if (context == "target") {
      valid_geogs <- c("blk", "bg", "tr", "co")
      if (standardized %in% valid_geogs) {
        return(standardized)
      }
    }
  }

  stop("The provided geography is invalid. Use `list_nhgis_crosswalks()` to check available crosswalks.")
}

#' List Available NHGIS Crosswalks
#'
#' Returns a data frame of all available NHGIS geographic crosswalks with their
#' corresponding parameters that can be used with get_nhgis_crosswalk().
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item source_year: Year of the source geography
#'     \item source_geography: Source geography name (standardized)
#'     \item target_year: Year of the target geography
#'     \item target_geography: Target geography name (standardized)
#'   }
#'
#' @keywords internal
list_nhgis_crosswalks <- function() {

  # Function to convert abbreviated geography codes to full names
  expand_geography <- function(geog_code) {
    mapping <- c(
      "blk" = "block",
      "bg" = "block group",
      "bgp" = "block group part",
      "tr" = "tract",
      "co" = "county"
    )
    return(mapping[geog_code])
  }

  # Extract crosswalk information from URLs
  crosswalk_info <- lapply(nhgis_crosswalks, function(url) {
    # Extract the filename from the URL
    filename <- basename(url)

    # Remove the "nhgis_" prefix and ".zip" suffix
    core_pattern <- filename |>
      stringr::str_remove("^nhgis_") |>
      stringr::str_remove("\\.zip$")

    # Parse the pattern: source_geog + source_year + "_" + target_geog + target_year
    # Use stringr to extract components
    if (!stringr::str_detect(core_pattern, "^([a-z]+)(\\d{4})_([a-z]+)(\\d{4})$")) {
      return(NULL)  # Skip if pattern doesn't match
    }

    # Extract individual components using capture groups
    pattern_parts <- stringr::str_match(core_pattern, "^([a-z]+)(\\d{4})_([a-z]+)(\\d{4})$")[1, ]

    if (length(pattern_parts) != 5) {  # Full match + 4 groups
      return(NULL)
    }

    source_geog_code <- pattern_parts[2]
    source_year <- as.numeric(pattern_parts[3])
    target_geog_code <- pattern_parts[4]
    target_year <- as.numeric(pattern_parts[5])

    # Convert codes to full names
    source_geography <- expand_geography(source_geog_code)
    target_geography <- expand_geography(target_geog_code)

    # Return as a named list
    return(list(
      source_year = source_year,
      source_geography = source_geography,
      target_year = target_year,
      target_geography = target_geography
    ))
  })

  # Remove NULL entries and convert to data frame
  crosswalk_info <- crosswalk_info[!sapply(crosswalk_info, is.null)]

  if (length(crosswalk_info) == 0) {
    return(data.frame(
      source_year = integer(0),
      source_geography = character(0),
      target_year = integer(0),
      target_geography = character(0)
    ))
  }

  # Convert list of lists to data frame
  crosswalk_df <- do.call(rbind, lapply(crosswalk_info, data.frame, stringsAsFactors = FALSE))

  # Sort by source year, then source geography, then target year, then target geography
  crosswalk_df <- crosswalk_df[order(crosswalk_df$source_year,
                                   crosswalk_df$source_geography,
                                   crosswalk_df$target_year,
                                   crosswalk_df$target_geography), ]

  # Reset row names
  rownames(crosswalk_df) <- NULL

  return(crosswalk_df)
}

#' Get NHGIS Geographic Crosswalk
#'
#' Retrieves a geographic crosswalk from the IPUMS NHGIS API based on user-specified
#' source and target geographies and years. Accepts various spellings and formats
#' for geography names.
#'
#' @param source_year Character or numeric. Year of the source geography one of c(1990, 2000, 2010, 2020).
#' @param source_geography Character. Source geography name. One of c("block", "block group part", "tract").
#' @param target_year Character or numeric. Year of the target geography, one of c(1990, 2000, 2010, 2020).
#' @param target_geography Character. Target geography name. One of c("block", "block group part", "tract", "county").
#' @param api_key Character. NULL by default, in which case the function looks for an `IPUMS_API_KEY` environment variable.
#'
#' @return A data frame containing the crosswalk between the specified geographies.
#' @keywords internal
get_nhgis_crosswalk <- function(source_year, source_geography, target_year, target_geography, api_key = NULL) {

  # Get API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("IPUMS_API_KEY")
    if (api_key == "") {
      stop("API key required. Provide via api_key parameter or set IPUMS_API_KEY environment variable. Get your key at https://account.ipums.org/api_keys")
    }
  }

  # Convert years to character for consistent processing
  source_year <- as.character(source_year)
  target_year <- as.character(target_year)

  # Standardize geography names
  source_geography_std <- standardize_geography(source_geography, "source")
  target_geography_std <- standardize_geography(target_geography, "target")

  # Validate inputs
  valid_years <- c("1990", "2000", "2010", "2020")
  valid_source_geogs <- c("blk", "bgp", "tr")
  valid_target_geogs <- c("blk", "bg", "tr", "co")

  if (!source_year %in% valid_years) {
    stop("source_year must be one of: ", paste(valid_years, collapse = ", "))
  }

  if (!target_year %in% valid_years) {
    stop("target_year must be one of: ", paste(valid_years, collapse = ", "))
  }

  if (is.null(source_geography_std)) {
    stop("source_geography '", source_geography, "' is not valid. Must be one of: blocks, block group parts, or tracts (various spellings accepted)")
  }

  if (is.null(target_geography_std)) {
    stop("target_geography '", target_geography, "' is not valid. Must be one of: blocks, block groups, tracts, or counties (various spellings accepted)")
  }

  # Construct the expected crosswalk filename pattern using standardized codes
  crosswalk_pattern <- paste0("nhgis_", source_geography_std, source_year, "_", target_geography_std, target_year, ".zip")

  # Find matching crosswalk URL
  matching_url <- nhgis_crosswalks[stringr::str_detect(nhgis_crosswalks, stringr::fixed(crosswalk_pattern))]

  if (length(matching_url) == 0) {
    available_crosswalks <- stringr::str_extract(nhgis_crosswalks, "(?<=nhgis_).*(?=\\.zip)")
    stop("No crosswalk available for ", source_geography_std, source_year, " to ", target_geography_std, target_year,
         ". Available crosswalks: \n", paste(available_crosswalks, collapse = "\n"))
  }

  # Download and process the crosswalk
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()

  tryCatch({
    # Download the crosswalk file
    response <- httr::GET(
      matching_url[1],
      httr::add_headers(Authorization = api_key),
      httr::write_disk(temp_zip, overwrite = TRUE)
    )

    httr::stop_for_status(response)

    # Extract the zip file
    utils::unzip(temp_zip, exdir = temp_dir)

    # Find the CSV file in the extracted contents
    csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

    if (length(csv_files) == 0) {
      stop("No CSV file found in the downloaded crosswalk data")
    }

    # Read the CSV file
    crosswalk_data <- utils::read.csv(csv_files[1], stringsAsFactors = FALSE)

    return(crosswalk_data)

  }, error = function(e) {
    stop("Failed to retrieve crosswalk: ", e$message)
  }, finally = {
    # Clean up temporary files
    if (file.exists(temp_zip)) {
      file.remove(temp_zip)
    }
    csv_files_to_remove <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
    if (length(csv_files_to_remove) > 0) {
      file.remove(csv_files_to_remove)
    }
  })
}
