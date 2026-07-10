# crosswalk: Streamlined Inter-Temporal and Inter-Geography Crosswalking of Census Data

Provides a standardized interface for accessing and applying geographic
crosswalks between United States Census geographies, both across
geographies (e.g., tracts to ZIP Code Tabulation Areas) and across time
(e.g., 2010 tracts to 2020 tracts). Crosswalks are sourced from the
Missouri Census Data Center's Geocorr applications, IPUMS NHGIS, the CT
Data Collaborative, and a curated registry of county boundary changes
drawn from Census Bureau documentation, and are returned with consistent
column names and rich metadata. Also interpolates count and non-count
variables from source to target geographies, with diagnostics describing
join quality.

## Details

The package standardizes access to geographic crosswalks from four
sources: GeoCorr (Missouri Census Data Center) for same-year geography
changes, IPUMS NHGIS for cross-decade changes, the CT Data Collaborative
for Connecticut's 2020-2022 county changes, and a curated registry of
county boundary change events for county crosswalks between any pair of
years from 2000 onward.

Typical usage involves two functions:

- [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  fetches one or more crosswalks (chaining them automatically when both
  geography and year change)

- [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  applies crosswalk(s) to a dataset, interpolating count and non-count
  variables and reporting join quality

Two helpers support discovery:

- [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  lists every supported combination of source and target geography and
  year

- [`list_nhgis_crosswalks()`](https://ui-research.github.io/crosswalk/reference/list_nhgis_crosswalks.md)
  lists the NHGIS subset with source URLs

To get oriented, see
[`vignette("crosswalk")`](https://ui-research.github.io/crosswalk/articles/crosswalk.md).
For the interpolation methodology and guidance on choosing weights, see
[`vignette("how-interpolation-works")`](https://ui-research.github.io/crosswalk/articles/how-interpolation-works.md).

## See also

Useful links:

- <https://ui-research.github.io/crosswalk/>

- <https://github.com/UI-Research/crosswalk>

- Report bugs at <https://github.com/UI-Research/crosswalk/issues>

## Author

**Maintainer**: Will Curran-Groome <wcurrangroome@urban.org>
