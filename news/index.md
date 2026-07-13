# Changelog

## crosswalk 0.0.0.9002

### New features

- **Arbitrary-year county crosswalks.**
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  now supports county -\> county crosswalks between any pair of years
  from 2000 through 2025 (e.g., `2014 -> 2019` or `2014 -> 2023`),
  accounting for counties redefined outside decennial censuses:
  Broomfield County, CO created (2002 vintage); Clifton Forge, VA merge
  (2002); the 2007-2008 southeast Alaska reorganizations (2008-2009);
  the York/Newport News, VA territory exchange (2008); the Bedford, VA
  merge and Petersburg Borough, AK reorganization (2014); the Shannon
  -\> Oglala Lakota, SD and Wade Hampton -\> Kusilvak, AK FIPS changes
  (2015); the Valdez-Cordova, AK split (2020 vintage); and the
  Connecticut planning regions (2022). Crosswalks compose all events in
  the requested interval on top of an identity mapping over the full
  county universe, with population-based allocation factors for splits
  and part transfers. County crosswalks are forward-only. The event
  registry, mappings, and county universes are curated from the Census
  Bureau’s “Substantial Changes to Counties and County Equivalent
  Entities” documentation by `data-raw/build_county_events_sysdata.R`
  and shipped as internal package data, so county requests work offline.
- **Sub-county geographies affected by county changes.** Tract, block
  group, and block crosswalks are available for same-decade year pairs
  from 2010 onward (e.g., tract `2014 -> 2019`), applying county-prefix
  relabels for FIPS renames/merges and the CT Data Collaborative
  crosswalk for Connecticut’s 2022 change. Year pairs spanning a
  decennial census (e.g., tract `2014 -> 2023`) are planned
  automatically as chains through NHGIS crosswalks, including exact
  backward relabels to reach an NHGIS-served vintage (e.g., tract
  `2017 -> 2023` plans `2017 -> 2014 -> 2020/22 -> 2023`).
- The chain planner’s temporal-path search now spans multiple sources:
  hops are tagged with the crosswalk source that serves them, and NHGIS
  is preferred when both NHGIS and the county-events engine can serve a
  hop.
  [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  lists the new county-events combinations.
- Multi-step plans whose final GeoCorr step targets a year with county
  codes differing from the GeoCorr vintage now emit a warning.
- [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  now always computes and attaches the `join_quality` attribute;
  `show_join_quality = FALSE` and `silent = TRUE` suppress only the
  printed diagnostics. Previously, disabling the messages also silently
  omitted the attribute.
- [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  gains a `crosswalk_source` column identifying which source serves each
  combination, and now lists each combination exactly once: the 2020
  \<-\> 2022 pairs, which both CTData and the county-events engine can
  serve, are listed under `ctdata_2020_2022`, mirroring how
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  routes direct requests.

### Documentation

- Three new vignettes:
  [`vignette("crosswalk")`](https://ui-research.github.io/crosswalk/articles/crosswalk.md)
  (getting started; the core workflow, multi-step chains, metadata, join
  quality, caching),
  [`vignette("how-interpolation-works")`](https://ui-research.github.io/crosswalk/articles/how-interpolation-works.md)
  (what the interpolation math assumes, choosing a `weight`, accuracy
  against published values, diagnosing joins), and
  [`vignette("county-crosswalks")`](https://ui-research.github.io/crosswalk/articles/county-crosswalks.md)
  (county change events and arbitrary-year county crosswalks).
- The “Standardizing Longitudinal Data” vignette is now precomputed from
  `vignettes/standardizing-longitudinal-data.Rmd.orig` (see
  `vignettes/precompile.R`), so building the package requires neither
  network access nor an IPUMS API key. It also now displays the
  join-quality diagnostics it narrates and closes with a worked example
  of a split tract’s six-year series.
- The structures of the `join_quality` and `crosswalk_metadata`
  attributes are now fully documented (in
  [`?crosswalk_data`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  and
  [`?get_crosswalk`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  respectively), along with caching read-back behavior, the
  empty-crosswalk return for nested geographies, and multi-step
  attribute semantics.
- Added a package-level help topic
  ([`?crosswalk`](https://ui-research.github.io/crosswalk/reference/crosswalk-package.md))
  and a curated pkgdown reference index; the README was slimmed to a
  quick start that links to the vignettes.

### Breaking changes

- [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  now filters NHGIS crosswalks to a single interpolation weight matching
  the `weight` argument (`"population"`, `"housing"`, or `"land"`).
  Previously, NHGIS crosswalks were returned in long format with one row
  per source-target pair *per weighting factor* (typically eight rows
  per pair), and applying them with
  [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  silently multiply-counted values by roughly the number of weighting
  factors. NHGIS `weighting_factor` values no longer carry a `weight_`
  prefix (e.g., `"population"` rather than `"weight_population"`).
- [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  now errors when a crosswalk contains multiple rows per source-target
  GEOID pair (which would multiply-count values), and when the input
  data contains duplicated GEOIDs (panel data must be split by period
  and crosswalked separately).
- [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  no longer emits a `geoid = NA` row aggregating data rows that failed
  to match the crosswalk; unmatched rows are dropped and reported via
  the `join_quality` attribute.
- [`crosswalk_data()`](https://ui-research.github.io/crosswalk/reference/crosswalk_data.md)
  now renames only `target_geoid` -\> `geoid` and
  `target_geography_name` -\> `geography_name` in its output; user
  columns containing the string “target\_” are no longer mangled.

### Bug fixes

- GeoCorr requests now retry transient failures (connection/SSL timeouts
  and dropped connections from `mcdc.missouri.edu`) with backoff, and
  the CSV download is retried alongside the query. A single flaky
  request no longer fails the whole crosswalk fetch, or a vignette or
  package build that relies on it.
- NHGIS 1990 tract GEOIDs are now right-padded with the implied `"00"`
  tract suffix, so all 1990 tract GEOIDs have the standard 11 characters
  (previously ~66% of rows had 9-character GEOIDs).
- Cached NHGIS and GeoCorr crosswalks are now read back with explicit
  column types; previously, type-guessing could silently strip leading
  zeros from GEOIDs (e.g., ZCTA `"00601"` became `601`) on cache reads.
- GeoCorr 2018 block-level queries are now chunked into groups of at
  most 13 states; previously, chunks of 17 states exceeded the GeoCorr
  API limit.
- Block-level GeoCorr crosswalks assembled from multiple chunked
  requests no longer retain the human-readable label row of each chunk
  after the first.
- Numeric crosswalk columns (`allocation_factor_target_to_source`,
  `population_2020`, `housing_2020`, `land_area_sqmi`, etc.) are no
  longer coerced to character by
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md).
- When only `source_year` is provided, the GeoCorr version is now
  selected from the source year; previously
  `get_crosswalk(..., source_year = 2015)` silently used GeoCorr 2022
  (2020 Census geography).
- The nested-geography check in
  [`get_crosswalk()`](https://ui-research.github.io/crosswalk/reference/get_crosswalk.md)
  now recognizes alternate spellings (e.g., `"blockgroup"`, `"bg"`).
- NHGIS download failures (e.g., an invalid API key) now raise a clear
  error with the HTTP status instead of a warning about an empty zip
  file.
- The `api_key` argument to the internal NHGIS fetcher is no longer
  ignored.
- County 2020 -\> 2022 crosswalk metadata no longer claims all
  allocation factors equal 1 (Connecticut county records are
  population-weighted).
- `get_ctdata_crosswalk()` now fails informatively when `tidycensus` is
  not installed (county crosswalks only).

## crosswalk 0.0.0.9001

- Support for chains of three or more crosswalks.
- Silent mode via `silent` argument and the `crosswalk.silent` option.
- [`get_available_crosswalks()`](https://ui-research.github.io/crosswalk/reference/get_available_crosswalks.md)
  lists all supported combinations.
- GeoCorr 2018 support for 2010s-era geographies.
