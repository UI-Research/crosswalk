# data-raw

Scripts that build the package's internal data (`R/sysdata.rda`). This
directory is excluded from the built package via `.Rbuildignore`.

## build_county_events_sysdata.R

Builds the curated county-change ("county events") data backing
`get_county_events_crosswalk()`:

- `county_events` -- one row per substantial change to counties or county
  equivalents since 2000 (renames, merges, splits, part transfers)
- `county_event_mappings` -- county-level source-target mappings with
  allocation factors (factors sum to 1 per source county within an event)
- `county_event_subcounty_mappings` -- explicit tract/block-group/block
  mappings for events flagged `explicit_rows` (currently empty: every curated
  event is a county-prefix relabel, has no sub-county code changes, delegates
  to CT Data at runtime, or predates sub-county support; the table is the
  escape hatch for future events that genuinely renumber sub-county GEOIDs)
- `county_universes` -- full county GEOID inventories at the 2000/2010/2020
  decennial bases (50 states + DC + Puerto Rico)
- `county_events_meta` -- `known_through_year`, build timestamp, source URL

### Rebuild

```bash
Rscript data-raw/build_county_events_sysdata.R
```

Requirements:

- Network access to census.gov and api.census.gov
- `CENSUS_API_KEY` in the environment (used by tidycensus for decennial
  populations, ACS county inventories, and the Connecticut section)
- Dev-only packages: `tidycensus`, `usethis`, `devtools` (not package
  Imports; if `renv::snapshot()` picks them up, that is expected for dev
  environments)

Intermediate downloads are cached in `data-raw/cache/` (gitignored).

### Provenance

- Event list, effective dates, and part populations: U.S. Census Bureau,
  "Changes to Counties and County Equivalent Entities: 1970-Present"
  (<https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html>),
  decade pages for the 2000s and 2010s. Published part populations reconcile
  exactly with decennial census counts of the source areas; remainders
  derived by subtraction are noted per mapping in `factor_derivation`.
- Allocation-factor denominators: Census 2000 / 2010 SF1 county populations
  (via tidycensus).
- Valdez-Cordova split factors: Census 2010 block populations apportioned to
  2020 counties via the Census 2010-2020 tabulation block relationship file
  (<https://www2.census.gov/geo/docs/maps-data/data/rel2020/t10t20/>).
- Connecticut planning-region county factors: computed by the package's own
  `get_ctdata_crosswalk()` (CT Data Collaborative crosswalk + tidycensus
  tract populations), so sysdata and the runtime CT path cannot diverge.
- `effective_vintage_year` is the first Census product vintage (TIGER/ACS
  geography year) using the post-change codes. Verified empirically against
  ACS 5-year county inventories where those exist (2010s events); for the
  2000s, the January-1 boundary-snapshot convention is applied to the
  documented effective dates. Notably, the Valdez-Cordova split (effective
  January 2, 2019) first appears in 2020-vintage products.
- The 2007 York County / Newport News, VA territory exchange is modeled as a
  one-way transfer of the published NET population (gross flows were not
  published).

### Validation built into the script

- Allocation factors sum to 1 per source county within each event (1e-9)
- Rolling the 2000 universe forward through the 2000s events reproduces the
  Census 2010 FIPS list exactly; likewise 2010 through the 2010s events to
  the Census 2020 FIPS list (completeness check on the event set)
- ACS vintage assertions per 2010s event (first vintage using new codes)
- GEOID length and categorical-value checks
