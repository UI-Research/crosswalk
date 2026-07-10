#' @details
#' The package standardizes access to geographic crosswalks from four sources:
#' GeoCorr (Missouri Census Data Center) for same-year geography changes, IPUMS
#' NHGIS for cross-decade changes, the CT Data Collaborative for Connecticut's
#' 2020-2022 county changes, and a curated registry of county boundary change
#' events for county crosswalks between any pair of years from 2000 onward.
#'
#' Typical usage involves two functions:
#' - [get_crosswalk()] fetches one or more crosswalks (chaining them
#'   automatically when both geography and year change)
#' - [crosswalk_data()] applies crosswalk(s) to a dataset, interpolating count
#'   and non-count variables and reporting join quality
#'
#' Two helpers support discovery:
#' - [get_available_crosswalks()] lists every supported combination of source
#'   and target geography and year
#' - [list_nhgis_crosswalks()] lists the NHGIS subset with source URLs
#'
#' To get oriented, see `vignette("crosswalk")`. For the interpolation
#' methodology and guidance on choosing weights, see
#' `vignette("how-interpolation-works")`.
#'
#' @keywords internal
"_PACKAGE"
