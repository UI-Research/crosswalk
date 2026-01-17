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
  geography <- geography |>
    stringr::str_to_lower() |>
    stringr::str_squish() |>
    stringr::str_trim() |>
    stringr::str_replace_all("_", " ")

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
    "cnty" = "co",

    # Places
    "pl" = "pl",
    "place" = "pl",
    "places" = "pl",

    # CBSAs
    "cbsa" = "cbsa",
    "cbsas" = "cbsa",
    "core based statistical area" = "cbsa",
    "core based statistical areas" = "cbsa",

    # Urban Areas
    "ua" = "ua",
    "uas" = "ua",
    "urban area" = "ua",
    "urban areas" = "ua",

    # PUMAs
    "puma" = "puma",
    "pumas" = "puma",
    "public use microdata area" = "puma",
    "public use microdata areas" = "puma",

    # ZCTAs
    "zcta" = "zcta",
    "zctas" = "zcta",
    "zip code" = "zcta",
    "zip codes" = "zcta",
    "zip code tabulation area" = "zcta",
    "zip code tabulation areas" = "zcta")

  # Check if the geography is in our mapping
  if (geography %in% names(geography_mapping)) {
    standardized <- geography_mapping[[geography]]

    # Validate based on context (source vs target geographies have different options)
    if (context == "source") {
      valid_geogs = c("blk", "bg", "tr")
      if (standardized %in% valid_geogs) {
        return(standardized)
      }
    } else if (context == "target") {
      valid_geogs = c("blk", "bg", "tr", "co", "pl", "ua", "zcta", "puma", "cbsa")
      if (standardized %in% valid_geogs) {
        return(standardized)
      }
    }
  }

  stop(
"The provided geography is invalid. Use `list_nhgis_crosswalks()` to check
available crosswalks.")
}

#' List Available NHGIS Crosswalks
#'
#' Returns a tibble of all available NHGIS geographic crosswalks with their
#' corresponding parameters that can be used with get_nhgis_crosswalk().
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item source_year: Year of the source geography
#'     \item source_geography: Source geography name
#'     \item target_year: Year of the target geography
#'     \item target_geography: Target geography name
#'   }
#'
#' @export
list_nhgis_crosswalks <- function() {
  nhgis_crosswalks_vector = c(
    ## =========================================================================
    ## BLOCK-TO-BLOCK CROSSWALKS (decennial years only)
    ## =========================================================================
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_blk2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_blk2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_blk2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_blk2010.zip",

    ## =========================================================================
    ## BLOCK → BLOCK GROUP
    ## =========================================================================
    ## from 1990
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_bg2015.zip",
    ## from 2000
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_bg2015.zip",
    ## from 2010
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_bg2022.zip",
    ## from 2020
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_bg2015.zip",

    ## =========================================================================
    ## BLOCK GROUP ↔ BLOCK GROUP (bidirectional)
    ## =========================================================================
    ## from 2010s
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_bg2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2011_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2011_bg2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2012_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2012_bg2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2014_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2014_bg2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2015_bg2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2015_bg2022.zip",
    ## from 2020s
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_bg2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_bg2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_bg2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_bg2015.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_bg2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_bg2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_bg2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_bg2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_bg2015.zip",

    ## =========================================================================
    ## BLOCK → TRACT
    ## =========================================================================
    ## from 1990
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_tr2015.zip",
    ## from 2000
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_tr2015.zip",
    ## from 2010
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_tr2022.zip",
    ## from 2020
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_tr2015.zip",

    ## =========================================================================
    ## BLOCK GROUP → TRACT
    ## =========================================================================
    ## from 2010s
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2011_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2011_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2012_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2012_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2014_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2014_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2015_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2015_tr2022.zip",
    ## from 2020s
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_tr2015.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_tr2015.zip",

    ## =========================================================================
    ## TRACT ↔ TRACT (bidirectional)
    ## =========================================================================
    ## from 1990
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_tr2015.zip",
    ## from 2000
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_tr2015.zip",
    ## from 2010s
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2011_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2011_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2012_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2012_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2014_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2014_tr2022.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2015_tr2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2015_tr2022.zip",
    ## from 2020s
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_tr2015.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_tr2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_tr2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_tr2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_tr2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_tr2015.zip",

    ## =========================================================================
    ## BLOCK → COUNTY
    ## Note: 2011/2012 targets only available from 2020 source (not 1990/2000)
    ## =========================================================================
    ## from 1990 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_co2015.zip",
    ## from 2000 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_co2015.zip",
    ## from 2010
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_co2022.zip",
    ## from 2020 (all 2010s targets available)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2011.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2012.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_co2015.zip",

    ## =========================================================================
    ## BLOCK GROUP → COUNTY
    ## Note: bg source to co only available for 2010, 2014, 2015 sources
    ## (NOT 2011 or 2012 sources)
    ## =========================================================================
    ## from 2010
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2010_co2022.zip",
    ## from 2014
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2014_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2014_co2022.zip",
    ## from 2015
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2015_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2015_co2022.zip",
    ## from 2020 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2020_co2015.zip",
    ## from 2022 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_bg2022_co2015.zip",

    ## =========================================================================
    ## TRACT → COUNTY
    ## Note: tr source to co only available for 1990, 2000, 2010, 2014, 2015,
    ## 2020, 2022 sources (NOT 2011 or 2012 sources)
    ## =========================================================================
    ## from 1990 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr1990_co2015.zip",
    ## from 2000 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2000_co2015.zip",
    ## from 2010
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2010_co2022.zip",
    ## from 2014
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2014_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2014_co2022.zip",
    ## from 2015
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2015_co2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2015_co2022.zip",
    ## from 2020 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2020_co2015.zip",
    ## from 2022 (to 2010, 2014, 2015 only)
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_co2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_co2014.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_tr2022_co2015.zip",

    ## =========================================================================
    ## BLOCK → OTHER GEOGRAPHIES (decennial years only)
    ## =========================================================================
    ## CBSA
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_cbsa2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_cbsa2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_cbsa2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_cbsa2010.zip",
    ## Place
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_pl2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_pl2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_pl2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_pl2010.zip",
    ## PUMA
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_puma2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_puma2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_puma2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_puma2010.zip",
    ## Urban Area
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_ua2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_ua2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_ua2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_ua2010.zip",
    ## ZCTA
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk1990_zcta2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2000_zcta2010.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2010_zcta2020.zip",
    "https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_blk2020_zcta2010.zip")

  ## for the time being, not supporting block group parts
  nhgis_crosswalks_vector = nhgis_crosswalks_vector[!stringr::str_detect(nhgis_crosswalks_vector, "bgp")]

  nhgis_crosswalks = purrr::map_dfr(
    nhgis_crosswalks_vector |> stringr::str_remove_all(".*nhgis_|\\.zip"),
    function(path) {
      path_parts = stringr::str_split(path, "_") |> _[[1]]

      tibble::tibble(
        source_geography = path_parts[1] |> stringr::str_extract("[a-zA-Z]{1,10}"),
        source_year = path_parts[1] |> stringr::str_extract("[0-9]{1,10}"),
        target_geography = path_parts[2] |> stringr::str_extract("[a-zA-Z]{1,10}"),
        target_year = path_parts[2] |> stringr::str_extract("[0-9]{1,10}")) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::matches("geography"),
          .fns = ~ .x |> stringr::str_replace_all(c(
            "pl" = "place",
            "blk" = "block",
            "bgp" = "block_group_part",
            "bg" = "block_group",
            "tr" = "tract",
            "co" = "county",
            "ua" = "urban_area",
            "cbsa" = "core_based_statistical_area")))) }) |>
    dplyr::bind_cols(tibble::tibble(crosswalk_path = nhgis_crosswalks_vector))

  return(nhgis_crosswalks)
}

#' Get NHGIS Geographic Crosswalk
#'
#' Retrieves a geographic crosswalk from the IPUMS NHGIS API based on user-specified
#' source and target geographies and years. Use `list_nhgis_crosswalks()` to view valid
#' parameter combinations.
#'
#' @details Note: this function does not support block group part crosswalks at this time.
#'
#' @param source_year Character or numeric. Year of the source geography one of
#'    c(1990, 2000, 2010, 2020).
#' @param source_geography Character. Source geography name. One of c("block",
#'    "blockgroup", "tract").
#' @param target_year Character or numeric. Year of the target geography, one of
#'    c(1990, 2000, 2010, 2020).
#' @param target_geography Character. Target geography name. One of c("block",
#'    "block group", "tract", "place", county", "urban_area", "zcta", "puma",
#'    "core_based_statistical_area").
#' @param cache Directory path. Where to download the crosswalk to. If NULL (default),
#'    crosswalk is returned but not saved to disk.
#'
#' @return A data frame containing the crosswalk between the specified geographies.
#'    Data are tidy-formatted, with each observation reflecting a unique
#'    source-target-weighting factor combination. Note that all (typically two
#'    or three) available weighting factors are returned.
#'
#'#' @return A dataframe representing the requested Geocorr22 crosswalk for all
#'      51 states and Puerto Rico. Depending on the desired geographies, some
#'      fields may not be included.
#'   \describe{
#'     \item{source_geoid}{A unique identifier for the source geography}
#'     \item{target_geoid}{A unique identifier for the target geography}
#'     \item{source_geography_name}{The name of the source geography}
#'     \item{target_geography_name}{The name of the target geography}
#'     \item{source_year}{The year of the source geography}
#'     \item{target_year}{The year of the target geography}
#'     \item{allocation_factor_source_to_target}{The weight to interpolate values
#'        from the source geography to the target geography}
#'     \item{weighting_factor}{The attribute used to calculate allocation factors}
#'   }
#' @noRd
get_nhgis_crosswalk <- function(
    source_year,
    source_geography,
    target_year,
    target_geography,
    cache = NULL,
    api_key = NULL) {

  if (is.null(cache)) { cache_path = tempdir() } else {cache_path = cache}

  # Convert years to character for consistent processing
  source_year = as.character(source_year)
  target_year = as.character(target_year)

  # Standardize geography names
  source_geography_standardized = standardize_geography(source_geography, "source")
  target_geography_standardized = standardize_geography(target_geography, "target")

  crosswalk_sub_path = stringr::str_c(source_geography_standardized, source_year, "_", target_geography_standardized, target_year)
  crosswalk_path <- paste0("https://api.ipums.org/supplemental-data/nhgis/crosswalks/nhgis_", crosswalk_sub_path, ".zip")

  ## identify the relevant file paths for potentially-cached crosswalks
  csv_path = file.path(
    cache_path,
    stringr::str_c(
      "crosswalk_nhgis_", source_year, "_to_", target_year, "_",
      source_geography, "_to_", target_geography, ".csv"))

  ## if the file exists and cache == TRUE
  if (file.exists(csv_path) & !is.null(cache)) {
    result = readr::read_csv(csv_path)

    message(
"Use of NHGIS crosswalks is subject to the same conditions as for all NHGIS data.
See https://www.nhgis.org/citation-and-use-nhgis-data.")
    message("Reading file from cache.")

    # Attach metadata to cached result
    attr(result, "crosswalk_metadata") <- list(
      data_source = "nhgis",
      data_source_full_name = "IPUMS NHGIS (National Historical Geographic Information System)",
      download_url = crosswalk_path,
      citation_url = "https://www.nhgis.org/citation-and-use-nhgis-data",
      documentation_url = "https://www.nhgis.org/geographic-crosswalks",
      source_year = source_year,
      target_year = target_year,
      source_geography = source_geography,
      source_geography_standardized = source_geography_standardized,
      target_geography = target_geography,
      target_geography_standardized = target_geography_standardized,
      retrieved_at = NA,
      cached = TRUE,
      cache_path = csv_path,
      read_from_cache = TRUE)

    return(result) }

  # Validate inputs

  # Define valid years

  valid_decennial_years <- c("1990", "2000", "2010", "2020")
  valid_noncensus_years <- c("2011", "2012", "2014", "2015", "2022")
  valid_years <- c(valid_decennial_years, valid_noncensus_years)
  valid_source_geogs <- c("blk", "bg", "tr")
  valid_target_geogs <- c("blk", "bg", "tr", "co", "ua", "zcta", "puma", "cbsa")
  noncensus_geogs <- c("bg", "tr", "co")

  # Helper to determine decade for a year
  get_decade <- function(year) {
    dplyr::case_when(
      year == "1990" ~ "1990s",
      year == "2000" ~ "2000s",
      year %in% c("2010", "2011", "2012", "2014", "2015") ~ "2010s",
      year %in% c("2020", "2022") ~ "2020s",
      TRUE ~ NA_character_)
  }

  # Validate source and target years are recognized
  if (!source_year %in% valid_years) {
    stop("source_year must be one of: ", paste(valid_years, collapse = ", "))}

  if (!target_year %in% valid_years) {
    stop("target_year must be one of: ", paste(valid_years, collapse = ", "))}

  # NHGIS only provides cross-decade crosswalks
  source_decade <- get_decade(source_year)
  target_decade <- get_decade(target_year)

  if (source_decade == target_decade) {
    stop(
"NHGIS only provides cross-decade crosswalks. The requested combination (",
source_year, " to ", target_year, ") is within the same decade (",
source_decade, "). For within-decade crosswalks like 2020 to 2022, use
get_crosswalk() which handles special cases like Connecticut.")}

  # 1990 can only go to 2010s (not 2000s or 2020s)
  if (source_year == "1990" & target_decade != "2010s") {
    stop(
"Crosswalks from 1990 are only available to 2010s geographies (2010, 2011,
2012, 2014, 2015). Target year ", target_year, " is not supported.")}

  # 2000 can only go to 2010s
  if (source_year == "2000" & target_decade != "2010s") {
    stop(
"Crosswalks from 2000 are only available to 2010s geographies (2010, 2011,
2012, 2014, 2015). Target year ", target_year, " is not supported.")}

  # Non-census years have geography restrictions (applies to both source and target)
  if (source_year %in% valid_noncensus_years) {
    if (!source_geography_standardized %in% noncensus_geogs) {
      stop(
"Non-census year crosswalks (2011, 2012, 2014, 2015, 2022) are only available
for block groups, tracts, and counties. The requested source geography '",
source_geography, "' is not supported for source year ", source_year, ".")
    }
  }

  if (target_year %in% valid_noncensus_years) {
    if (!target_geography_standardized %in% noncensus_geogs) {
      stop(
"Non-census year crosswalks (2011, 2012, 2014, 2015, 2022) are only available
for block groups, tracts, and counties. The requested target geography '",
target_geography, "' is not supported for target year ", target_year, ".")
    }
  }

  # County target restrictions: 2011 and 2012 source years don't have county targets
  if (source_year %in% c("2011", "2012") & target_geography_standardized == "co") {
    stop(
"County crosswalks are not available from source years 2011 or 2012.
County targets are only available from source years: 1990, 2000, 2010, 2014,
2015, 2020, 2022.")
  }

  # County target restrictions: 1990/2000 to county only has 2010, 2014, 2015 targets
  if (source_year %in% c("1990", "2000") &
      target_geography_standardized == "co" &
      target_year %in% c("2011", "2012")) {
    stop(
"Crosswalks from ", source_year, " to county are only available for target
years 2010, 2014, and 2015 (not ", target_year, ").")
  }

  if (is.null(source_geography_standardized)) {
    stop(
"source_geography '", source_geography, "' is not valid. Must be one of: blocks,
block group parts, or tracts (various spellings accepted).")}

  if (is.null(target_geography_standardized)) {
    stop(
"target_geography '", target_geography, "' is not valid. Must be one of: blocks,
block groups, tracts, or counties (various spellings accepted)")}

  if (!(crosswalk_path %in% list_nhgis_crosswalks()$crosswalk_path)) {
    stop(stringr::str_c(
"There is no available crosswalk between the specified geographies and years.")) }

  api_key = Sys.getenv("IPUMS_API_KEY")
  if (api_key == "") {
    stop(
"API key required. Save your API key to the IPUMS_API_KEY environment
variable. Get your key at https://account.ipums.org/api_keys") }

  # Helper function to safely check zip contents

  safe_unzip_list = function(zip_file) {
    tryCatch(
      utils::unzip(zip_file, list = TRUE),
      error = function(e) NULL
    )
  }

  # Helper function to safely extract zip
  safe_unzip_extract = function(zip_file, exdir) {
    tryCatch({
      utils::unzip(zipfile = zip_file, exdir = exdir)
      TRUE
    },
    error = function(e) FALSE
    )
  }

  crosswalk_df1 = tryCatch({

    # Use a unique temporary directory for downloading and extracting
    temp_dir = file.path(tempdir(), stringr::str_c("nhgis_", crosswalk_sub_path, "_", format(Sys.time(), "%Y%m%d%H%M%S")))
    dir.create(temp_dir, recursive = TRUE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    zip_path = file.path(temp_dir, stringr::str_c(crosswalk_sub_path, ".zip"))

    # Download the crosswalk file
    response = httr::GET(
      crosswalk_path,
      httr::add_headers(Authorization = api_key),
      httr::write_disk(zip_path, overwrite = TRUE), overwrite = TRUE)

    # Check what's in the zip before extracting
    zip_contents = safe_unzip_list(zip_path)

    if (is.null(zip_contents) || nrow(zip_contents) == 0) {
      warning(
        "The downloaded zip file for crosswalk ", crosswalk_sub_path,
        " is empty or cannot be opened. This crosswalk may not be available from NHGIS. ",
        "Returning an empty tibble.")
      return(tibble::tibble())
    }

    # Extract the outer zip to temp directory
    extract_success = safe_unzip_extract(zip_path, temp_dir)
    if (!extract_success) {
      warning(
        "Failed to extract the downloaded zip file for crosswalk ", crosswalk_sub_path,
        ". The file may be corrupted. Returning an empty tibble.")
      return(tibble::tibble())
    }

    # List extracted files (excluding the original zip)
    all_files = list.files(temp_dir, full.names = TRUE)
    all_files = all_files[all_files != zip_path]

    # Look for CSV files first (some crosswalks may not be nested)
    csv_files = all_files[stringr::str_detect(all_files, "\\.csv$")]

    # If no CSV found directly, look for nested zip and extract it
    if (length(csv_files) == 0) {
      nested_zips = all_files[stringr::str_detect(all_files, "\\.zip$")]

      if (length(nested_zips) == 0) {
        warning(
          "No CSV or nested zip file found in the downloaded archive for ",
          crosswalk_sub_path, ". Returning an empty tibble.")
        return(tibble::tibble())
      }

      # Check if nested zip can be opened
      nested_zip = nested_zips[1]
      nested_contents = safe_unzip_list(nested_zip)

      if (is.null(nested_contents) || nrow(nested_contents) == 0) {
        warning(
          "The nested zip file for crosswalk ", crosswalk_sub_path,
          " is empty or cannot be opened. This crosswalk may not be available from NHGIS. ",
          "Returning an empty tibble.")
        return(tibble::tibble())
      }

      # Extract the nested zip
      nested_extract_success = safe_unzip_extract(nested_zip, temp_dir)
      if (!nested_extract_success) {
        warning(
          "Failed to extract the nested zip file for crosswalk ", crosswalk_sub_path,
          ". The file may be corrupted. Returning an empty tibble.")
        return(tibble::tibble())
      }

      # Now look for CSV files again
      all_files = list.files(temp_dir, full.names = TRUE)
      csv_files = all_files[stringr::str_detect(all_files, "\\.csv$")]
    }

    if (length(csv_files) == 0) {
      warning(
        "No CSV file found after extracting zip archive(s) for ", crosswalk_sub_path,
        ". Returning an empty tibble.")
      return(tibble::tibble())
    }

    crosswalk_df = readr::read_csv(
      csv_files[1],
      col_types = readr::cols(.default = readr::col_character())) |>
      janitor::clean_names()

    crosswalk_df
  },
    error = function(e) {
      warning("Failed to retrieve crosswalk ", crosswalk_sub_path, ": ", e$message,
              ". Returning an empty tibble.")
      return(tibble::tibble())
    })

  # Handle case where empty tibble was returned due to empty zip
  if (nrow(crosswalk_df1) == 0) {
    return(tibble::tibble())
  }

  crosswalk_df = crosswalk_df1 |>
    dplyr::select(-dplyr::matches("gj")) |>
    dplyr::rename_with(
      .cols = dplyr::everything(),
      .fn = ~ .x |> stringr::str_replace_all(c(
        ## for block-based crosswalks, there's only a single, combined weight
        "^weight$" = "weight_housing_population",
        "parea" = "weight_landarea",
        "wt" = "weight",
        "pop$" = "population",
        "fam" = "family",
        "hh" = "household",
        "_hu" = "_housing_all",
        "ownhu" = "housing_owned",
        "renthu" = "housing_rented"))) |>
    # Convert weight columns to numeric before pivoting
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches("^weight_"),
        .fns = as.numeric)) |>
    dplyr::rename(
      source_geoid = !!(stringr::str_c(source_geography_standardized, source_year, "ge")),
      target_geoid = !!(stringr::str_c(target_geography_standardized, target_year, "ge"))) |>
    dplyr::mutate(
      source_geography_name = source_geography_standardized,
      target_geography_name = target_geography_standardized,
      dplyr::across(
        .cols = dplyr::matches("geography_name"),
        .fns = ~ .x |> stringr::str_replace_all(c(
          "blk" = "block",
          "bgp" = "block_group_part",
          "bg" = "block_group",
          "tr" = "tract",
          "co" = "county"))),
      source_year = source_year,
      target_year = target_year) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("weight_"),
      names_to = "weighting_factor",
      values_to = "allocation_factor_source_to_target")

    ## if the file does not already exist and cache is not NULL
    if (!file.exists(csv_path) & !is.null(cache)) {
      if (!dir.exists(cache)) {
        dir.create(cache, recursive = TRUE)
      }
      readr::write_csv(crosswalk_df, csv_path)
    }

message(
"Use of NHGIS crosswalks is subject to the same conditions as for all NHGIS data.
See https://www.nhgis.org/citation-and-use-nhgis-data.")

  # Attach metadata to result
  attr(crosswalk_df, "crosswalk_metadata") <- list(
    data_source = "nhgis",
    data_source_full_name = "IPUMS NHGIS (National Historical Geographic Information System)",
    download_url = crosswalk_path,
    citation_url = "https://www.nhgis.org/citation-and-use-nhgis-data",
    documentation_url = "https://www.nhgis.org/geographic-crosswalks",
    source_year = source_year,
    target_year = target_year,
    source_geography = source_geography,
    source_geography_standardized = source_geography_standardized,
    target_geography = target_geography,
    target_geography_standardized = target_geography_standardized,
    retrieved_at = Sys.time(),
    cached = !is.null(cache),
    cache_path = if (!is.null(cache)) csv_path else NULL)

  return(crosswalk_df)
}


