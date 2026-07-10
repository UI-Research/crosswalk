# Build R/sysdata.rda: curated county-change ("county events") data
#
# Rebuild with: Rscript data-raw/build_county_events_sysdata.R
# Requires: CENSUS_API_KEY (tidycensus) and network access to census.gov.
# See data-raw/README.md for provenance details.
#
# Primary source: U.S. Census Bureau, "Changes to Counties and County
# Equivalent Entities: 1970-Present",
# https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html
# (decade pages .../county-changes/2000.html and .../county-changes/2010.html).
# All part populations quoted below are the figures published on those pages.

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

devtools::load_all(".")

data_raw_cache <- file.path("data-raw", "cache")
dir.create(data_raw_cache, showWarnings = FALSE, recursive = TRUE)

# ==== County universes per decennial base ====================================
# 2010 base: Census 2010 FIPS list (reflects January 1, 2010 boundaries)
# 2020 base: Census 2020 FIPS list (reflects January 1, 2020 boundaries)
# 2000 base: derived by reversing the 2000s events from the 2010 base
# Scope: 50 states + DC + Puerto Rico (matches get_ctdata_crosswalk()'s
# tidycensus universe); island-area territories excluded.

in_scope_state <- function(state_fips) {
  state_fips <= "56" | state_fips == "72"
}

universe_2010 <- readr::read_csv(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
  col_names = c("state_abbr", "state_fips", "county_fips", "county_name", "class_fips"),
  col_types = readr::cols(.default = readr::col_character())) |>
  dplyr::filter(in_scope_state(state_fips)) |>
  dplyr::transmute(
    decade_base = 2010L,
    geoid = stringr::str_c(state_fips, county_fips),
    state_fips = state_fips)

universe_2020 <- readr::read_delim(
  "https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt",
  delim = "|",
  col_types = readr::cols(.default = readr::col_character())) |>
  dplyr::filter(in_scope_state(STATEFP)) |>
  dplyr::transmute(
    decade_base = 2020L,
    geoid = stringr::str_c(STATEFP, COUNTYFP),
    state_fips = STATEFP)

# Counties created during the 2000s (absent from the 2000 base) and counties
# that existed in 2000 but were dissolved/renamed before 2010
created_in_2000s <- c("02105", "02195", "02198", "02230", "02275", "08014")
existed_in_2000_only <- tibble::tibble(
  decade_base = 2000L,
  geoid = c("02201", "02232", "02280", "51560"),
  state_fips = c("02", "02", "02", "51"))

universe_2000 <- universe_2010 |>
  dplyr::filter(!geoid %in% created_in_2000s) |>
  dplyr::mutate(decade_base = 2000L) |>
  dplyr::bind_rows(existed_in_2000_only) |>
  dplyr::arrange(geoid)

county_universes <- dplyr::bind_rows(universe_2000, universe_2010, universe_2020)

# ==== Decennial base populations for allocation-factor denominators ==========

pops_2000 <- tidycensus::get_decennial(
  geography = "county", variables = "P001001", year = 2000, sumfile = "sf1",
  state = c("02", "08", "51")) |>
  dplyr::select(geoid = GEOID, pop = value) |>
  tibble::deframe()

pops_2010 <- tidycensus::get_decennial(
  geography = "county", variables = "P001001", year = 2010, sumfile = "sf1",
  state = c("02", "46", "51")) |>
  dplyr::select(geoid = GEOID, pop = value) |>
  tibble::deframe()

# ==== Event registry ==========================================================
# effective_vintage_year is the first Census product vintage (TIGER/ACS
# geography year) whose products use the post-change codes/boundaries.
# Convention: boundary changes appear in the vintage of the first January 1
# on/after the effective date; the 2015 FIPS code corrections and the 2019
# Valdez-Cordova split were applied by the Census Bureau to that same year's
# products (verified empirically against ACS 5-year county lists below).

county_events <- tibble::tribble(
  ~event_id, ~effective_vintage_year, ~decade_base, ~event_type, ~state_fips,
  ~subcounty_handling, ~reversal_exact_tract, ~reversal_exact_block_group, ~reversal_exact_block,
  ~description, ~citation_url,

  "2002_broomfield_co_created", 2002L, 2000L, "part_transfer", "08",
  "unsupported", FALSE, FALSE, FALSE,
  "Broomfield County, CO (08014) created from parts of Adams (08001), Boulder (08013), Jefferson (08059), and Weld (08123) counties, effective November 15, 2001",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2000.html",

  "2002_clifton_forge_va_merge", 2002L, 2000L, "merge", "51",
  "unsupported", FALSE, FALSE, FALSE,
  "Clifton Forge independent city, VA (51560) changed to town status and added to Alleghany County (51005), effective July 1, 2001",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2000.html",

  "2008_skagway_hoonah_angoon_split", 2008L, 2000L, "split", "02",
  "unsupported", FALSE, FALSE, FALSE,
  "Skagway-Hoonah-Angoon Census Area, AK (02232) split into Skagway Municipality (02230) and Hoonah-Angoon Census Area (02105), effective June 20, 2007",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2000.html",

  "2008_york_newport_news_exchange", 2008L, 2000L, "part_transfer", "51",
  "unsupported", FALSE, FALSE, FALSE,
  "York County, VA (51199) exchanged territory with Newport News city (51700), effective July 1, 2007; modeled as a one-way transfer of the published net population",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2000.html",

  "2009_ak_southeast_reorg", 2009L, 2000L, "split", "02",
  "unsupported", FALSE, FALSE, FALSE,
  "Wrangell-Petersburg Census Area, AK (02280) split into Wrangell City and Borough (02275) and Petersburg Census Area (02195); Prince of Wales-Outer Ketchikan Census Area (02201) reorganized into Prince of Wales-Hyder Census Area (02198) with parts to Ketchikan Gateway Borough (02130) and Wrangell (02275), effective May 19 / June 1, 2008",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2000.html",

  "2014_bedford_va_merge", 2014L, 2010L, "merge", "51",
  "prefix_swap", FALSE, FALSE, FALSE,
  "Bedford independent city, VA (51515) changed to town status and added to Bedford County (51019), effective July 1, 2013; census tract 050100 retained its code under the new county (verified against ACS tract inventories), so sub-county GEOIDs are county-prefix relabels. Forward-only at sub-county levels (reversing would require the affected tract list)",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html",

  "2014_petersburg_borough_reorg", 2014L, 2010L, "part_transfer", "02",
  "none", FALSE, FALSE, FALSE,
  "Petersburg Borough, AK (02195) created from most of Petersburg Census Area (02195) plus a small part of Hoonah-Angoon Census Area (02105); the remainder of the former census area transferred to Prince of Wales-Hyder Census Area (02198), effective January 3, 2013. Census tract sets and codes are unchanged across this event (verified against ACS tract inventories); tract boundaries were revised to follow the new county line, so sub-county crosswalks treat it as identity with a population-comparability caveat",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html",

  "2015_shannon_oglala_lakota_rename", 2015L, 2010L, "rename", "46",
  "prefix_swap", TRUE, TRUE, TRUE,
  "Shannon County, SD (46113) renamed Oglala Lakota County (46102), effective May 1, 2015; boundaries unchanged",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html",

  "2015_wade_hampton_kusilvak_rename", 2015L, 2010L, "rename", "02",
  "prefix_swap", TRUE, TRUE, TRUE,
  "Wade Hampton Census Area, AK (02270) renamed Kusilvak Census Area (02158), effective July 1, 2015; boundaries unchanged",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html",

  "2020_valdez_cordova_split", 2020L, 2010L, "split", "02",
  "none", FALSE, FALSE, FALSE,
  "Valdez-Cordova Census Area, AK (02261) split into Chugach Census Area (02063) and Copper River Census Area (02066), effective January 2, 2019; first reflected in 2020-vintage Census products (verified against ACS county inventories), so sub-county transitions are handled by NHGIS 2010-2020 crosswalks",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes/2010.html",

  "2022_ct_planning_regions", 2022L, 2020L, "split", "09",
  "ctdata_runtime", TRUE, TRUE, TRUE,
  "Connecticut's eight counties (09001-09015) replaced by nine planning regions (09110-09190) as county equivalents, effective in 2022 Census products; sub-county geographies are exact 1:1 FIPS relabels (reversible), sourced from CT Data Collaborative at runtime",
  "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html")

# ==== County-level event mappings ============================================
# One row per source-target county pair per event; allocation factors sum to 1
# per source county within an event. Part populations quoted from the Census
# county-changes pages; denominators are the nearest preceding decennial
# census counts (the published parts reconcile exactly with those counts).

frac <- function(part, geoid, pops) part / unname(pops[geoid])

county_event_mappings <- dplyr::bind_rows(

  # Broomfield: published detached populations by donor county
  # Adams 15,870; Boulder 21,512; Jefferson 1,726; Weld 69 (sum = 39,177)
  purrr::map_dfr(
    list(c("08001", 15870), c("08013", 21512), c("08059", 1726), c("08123", 69)),
    \(x) {
      donor <- x[1]
      part <- as.numeric(x[2])
      tibble::tibble(
        event_id = "2002_broomfield_co_created",
        source_geoid = donor,
        target_geoid = c("08014", donor),
        allocation_factor = c(frac(part, donor, pops_2000), 1 - frac(part, donor, pops_2000)),
        weighting_factor = "population",
        factor_derivation = "Census-published detached population / Census 2000 donor county population")
    }),

  tibble::tibble(
    event_id = "2002_clifton_forge_va_merge",
    source_geoid = "51560",
    target_geoid = "51005",
    allocation_factor = 1,
    weighting_factor = "identity",
    factor_derivation = "Entire independent city merged into Alleghany County"),

  # Skagway-Hoonah-Angoon: Skagway Municipality 862; Hoonah-Angoon 2,574
  # (sum = 3,436 = Census 2000 population of 02232)
  tibble::tibble(
    event_id = "2008_skagway_hoonah_angoon_split",
    source_geoid = "02232",
    target_geoid = c("02230", "02105"),
    allocation_factor = c(frac(862, "02232", pops_2000), frac(2574, "02232", pops_2000)),
    weighting_factor = "population",
    factor_derivation = "Census-published part populations / Census 2000 population of 02232"),

  # York/Newport News: only the NET exchanged population (293) is published;
  # modeled as a one-way York -> Newport News transfer
  tibble::tibble(
    event_id = "2008_york_newport_news_exchange",
    source_geoid = "51199",
    target_geoid = c("51700", "51199"),
    allocation_factor = c(frac(293, "51199", pops_2000), 1 - frac(293, "51199", pops_2000)),
    weighting_factor = "population",
    factor_derivation = "Census-published NET exchanged population / Census 2000 population of York County (approximation: gross flows not published)"),

  # Southeast Alaska 2008 reorganization:
  # 02280 (Census 2000 pop 6,684): Petersburg CA 4,260 (published); remainder
  #   2,424 to Wrangell (derived: 6,684 - 4,260)
  # 02201 (Census 2000 pop 6,146): Prince of Wales-Hyder 6,115 (published);
  #   Ketchikan Gateway 7 (published); remainder 24 (Meyers Chuck area,
  #   derived: 6,146 - 6,115 - 7) to Wrangell
  tibble::tibble(
    event_id = "2009_ak_southeast_reorg",
    source_geoid = c("02280", "02280", "02201", "02201", "02201"),
    target_geoid = c("02195", "02275", "02198", "02130", "02275"),
    allocation_factor = c(
      frac(4260, "02280", pops_2000), frac(unname(pops_2000["02280"]) - 4260, "02280", pops_2000),
      frac(6115, "02201", pops_2000), frac(7, "02201", pops_2000),
      frac(unname(pops_2000["02201"]) - 6115 - 7, "02201", pops_2000)),
    weighting_factor = "population",
    factor_derivation = "Census-published part populations (remainders derived by subtraction) / Census 2000 source populations"),

  tibble::tibble(
    event_id = "2014_bedford_va_merge",
    source_geoid = "51515",
    target_geoid = "51019",
    allocation_factor = 1,
    weighting_factor = "identity",
    factor_derivation = "Entire independent city merged into Bedford County"),

  # Petersburg Borough 2013: 613 (published) from Petersburg CA to Prince of
  # Wales-Hyder; 1 (published) from Hoonah-Angoon to Petersburg Borough;
  # denominators are Census 2010 counts (02195 = 3,815; 02105 = 2,150)
  tibble::tibble(
    event_id = "2014_petersburg_borough_reorg",
    source_geoid = c("02195", "02195", "02105", "02105"),
    target_geoid = c("02195", "02198", "02195", "02105"),
    allocation_factor = c(
      1 - frac(613, "02195", pops_2010), frac(613, "02195", pops_2010),
      frac(1, "02105", pops_2010), 1 - frac(1, "02105", pops_2010)),
    weighting_factor = "population",
    factor_derivation = "Census-published part populations / Census 2010 source populations"),

  tibble::tibble(
    event_id = "2015_shannon_oglala_lakota_rename",
    source_geoid = "46113",
    target_geoid = "46102",
    allocation_factor = 1,
    weighting_factor = "identity",
    factor_derivation = "FIPS code change only; boundaries unchanged"),

  tibble::tibble(
    event_id = "2015_wade_hampton_kusilvak_rename",
    source_geoid = "02270",
    target_geoid = "02158",
    allocation_factor = 1,
    weighting_factor = "identity",
    factor_derivation = "FIPS code change only; boundaries unchanged"))

# ---- Valdez-Cordova split: factors from Census 2010 block populations -------
# 2010 blocks of 02261 are assigned to their 2020 counties via the official
# 2010-2020 tabulation block relationship file (land-area apportionment for
# the rare split blocks), then weighted by Census 2010 block populations.

vc_blocks_2010 <- tidycensus::get_decennial(
  geography = "block", variables = "P001001", year = 2010, sumfile = "sf1",
  state = "02", county = "261") |>
  dplyr::transmute(block_2010 = GEOID, pop = value)

block_rel_zip <- file.path(data_raw_cache, "tab2010_tab2020_st02.zip")
if (!file.exists(block_rel_zip)) {
  download.file(
    "https://www2.census.gov/geo/docs/maps-data/data/rel2020/t10t20/TAB2010_TAB2020_ST02.zip",
    block_rel_zip, mode = "wb", quiet = TRUE)
}

vc_block_assignment <- readr::read_delim(
  block_rel_zip, delim = "|",
  col_types = readr::cols(.default = readr::col_character())) |>
  dplyr::filter(STATE_2010 == "02", COUNTY_2010 == "261") |>
  dplyr::mutate(
    block_2010 = stringr::str_c(STATE_2010, COUNTY_2010, TRACT_2010, BLK_2010),
    county_2020 = stringr::str_c(STATE_2020, COUNTY_2020),
    arealand_int = as.numeric(AREALAND_INT)) |>
  dplyr::mutate(
    block_land_total = sum(arealand_int),
    area_share = dplyr::if_else(
      block_land_total > 0, arealand_int / block_land_total, 1 / dplyr::n()),
    .by = block_2010) |>
  dplyr::summarize(share = sum(area_share), .by = c(block_2010, county_2020))

valdez_parts <- vc_block_assignment |>
  dplyr::inner_join(vc_blocks_2010, by = "block_2010") |>
  dplyr::summarize(part_pop = sum(pop * share), .by = county_2020)

# Population leaking to counties other than the two successors (from minor
# boundary corrections in the relationship file) must be negligible
leaked_pop <- valdez_parts |>
  dplyr::filter(!county_2020 %in% c("02063", "02066")) |>
  dplyr::pull(part_pop) |>
  sum()
stopifnot(leaked_pop / sum(valdez_parts$part_pop) < 0.001)

valdez_cordova_mappings <- valdez_parts |>
  dplyr::filter(county_2020 %in% c("02063", "02066")) |>
  dplyr::transmute(
    event_id = "2020_valdez_cordova_split",
    source_geoid = "02261",
    target_geoid = county_2020,
    allocation_factor = part_pop / sum(part_pop),
    weighting_factor = "population",
    factor_derivation = "Census 2010 block populations apportioned to 2020 counties via the Census 2010-2020 tabulation block relationship file")

# Sanity check against Census 2020 counts (Chugach 7,102 / Copper River 2,617)
stopifnot(abs(
  valdez_cordova_mappings$allocation_factor[valdez_cordova_mappings$target_geoid == "02063"] -
    7102 / (7102 + 2617)) < 0.05)

# ---- Connecticut planning regions: reuse the package's CTData computation ---
# Guarantees the sysdata factors match get_ctdata_crosswalk("county", 2020, 2022).

ct_county_crosswalk <- get_ctdata_crosswalk(
  geography = "county", source_year = 2020, target_year = 2022,
  cache = data_raw_cache)

ct_mappings <- ct_county_crosswalk |>
  dplyr::filter(stringr::str_starts(source_geoid, "09")) |>
  dplyr::transmute(
    event_id = "2022_ct_planning_regions",
    source_geoid = source_geoid,
    target_geoid = target_geoid,
    allocation_factor = allocation_factor_source_to_target,
    weighting_factor = weighting_factor,
    factor_derivation = "Population-weighted tract aggregation from CT Data Collaborative crosswalk (see get_ctdata_crosswalk())")

county_event_mappings <- dplyr::bind_rows(
  county_event_mappings, valdez_cordova_mappings, ct_mappings) |>
  dplyr::arrange(event_id, source_geoid, target_geoid)

# ==== Sub-county mappings (explicit_rows events) ==============================
# Populated in a follow-up curation pass; empty schema for now.

county_event_subcounty_mappings <- tibble::tibble(
  event_id = character(),
  geography = character(),
  source_geoid = character(),
  target_geoid = character(),
  allocation_factor = double())

# ==== Empirical vintage verification (ACS 5-year county inventories) =========
# Asserts that effective_vintage_year is the first ACS vintage using the
# post-change codes for every 2010s event.

acs_counties <- function(year, states) {
  tidycensus::get_acs(
    geography = "county", variables = "B01001_001", year = year,
    survey = "acs5", state = states) |>
    dplyr::select(GEOID, NAME)
}

acs13 <- acs_counties(2013, c("VA", "AK"))
acs14 <- acs_counties(2014, c("VA", "AK", "SD"))
acs15 <- acs_counties(2015, c("AK", "SD"))
acs19 <- acs_counties(2019, "AK")
acs20 <- acs_counties(2020, "AK")

stopifnot(
  # Bedford city present through vintage 2013, gone in 2014
  "51515" %in% acs13$GEOID, !"51515" %in% acs14$GEOID,
  # Petersburg: Census Area through 2013, Borough from 2014
  any(stringr::str_detect(acs13$NAME, "Petersburg Census Area")),
  any(stringr::str_detect(acs14$NAME, "Petersburg Borough")),
  # Shannon/Wade Hampton codes through vintage 2014, new codes from 2015
  "46113" %in% acs14$GEOID, !"46113" %in% acs15$GEOID, "46102" %in% acs15$GEOID,
  "02270" %in% acs14$GEOID, !"02270" %in% acs15$GEOID, "02158" %in% acs15$GEOID,
  # Valdez-Cordova through vintage 2019, Chugach/Copper River from 2020
  "02261" %in% acs19$GEOID, !any(c("02063", "02066") %in% acs19$GEOID),
  !"02261" %in% acs20$GEOID, all(c("02063", "02066") %in% acs20$GEOID))

# ==== Metadata ================================================================

county_events_meta <- list(
  known_through_year = 2025L,
  built_at = as.character(Sys.time()),
  source_documentation_url = "https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html")

# ==== Validation ==============================================================

# Factors sum to 1 per source county within each event
factor_sums <- county_event_mappings |>
  dplyr::summarize(total = sum(allocation_factor), .by = c(event_id, source_geoid))
stopifnot(all(abs(factor_sums$total - 1) < 1e-9))

# GEOIDs are 5-character county codes
stopifnot(
  all(nchar(county_event_mappings$source_geoid) == 5),
  all(nchar(county_event_mappings$target_geoid) == 5),
  all(nchar(county_universes$geoid) == 5))

# Every event has at least one mapping row (CT included via ctdata)
stopifnot(setequal(county_events$event_id, county_event_mappings$event_id))

# Rolling each decade's universe forward through its events reproduces the
# next decennial base exactly
roll_universe <- function(universe, mappings) {
  sources <- unique(mappings$source_geoid)
  targets <- mappings |>
    dplyr::distinct(target_geoid) |>
    dplyr::transmute(
      geoid = target_geoid,
      state_fips = stringr::str_sub(target_geoid, 1, 2))
  universe |>
    dplyr::select(geoid, state_fips) |>
    dplyr::filter(!geoid %in% sources) |>
    dplyr::bind_rows(targets) |>
    dplyr::distinct(geoid, state_fips) |>
    dplyr::arrange(geoid)
}

events_2000s <- county_events |> dplyr::filter(decade_base == 2000L)
events_2010s <- county_events |> dplyr::filter(decade_base == 2010L)

rolled_2010 <- roll_universe(
  universe_2000,
  county_event_mappings |> dplyr::filter(event_id %in% events_2000s$event_id))
stopifnot(identical(
  rolled_2010$geoid,
  universe_2010 |> dplyr::arrange(geoid) |> dplyr::pull(geoid)))

rolled_2020 <- roll_universe(
  universe_2010,
  county_event_mappings |> dplyr::filter(event_id %in% events_2010s$event_id))
stopifnot(identical(
  rolled_2020$geoid,
  universe_2020 |> dplyr::arrange(geoid) |> dplyr::pull(geoid)))

# Universes contain/exclude the right pre/post-event codes
stopifnot(
  all(c("02201", "02232", "02280", "51560") %in% universe_2000$geoid),
  !any(c("08014", "02105") %in% universe_2000$geoid),
  all(c("08014", "51515", "46113", "02270", "02261") %in% universe_2010$geoid),
  !any(c("46102", "02158", "02063") %in% universe_2010$geoid),
  all(c("46102", "02158", "02063", "02066") %in% universe_2020$geoid),
  !any(c("51515", "46113", "02270", "02261") %in% universe_2020$geoid))

# Valid categorical values
stopifnot(
  all(county_events$event_type %in% c("rename", "merge", "split", "part_transfer")),
  all(county_events$subcounty_handling %in%
        c("prefix_swap", "explicit_rows", "ctdata_runtime", "none", "unsupported")),
  all(county_events$effective_vintage_year > 2000L),
  all(county_events$effective_vintage_year <= county_events_meta$known_through_year))

# ==== Write sysdata ===========================================================

usethis::use_data(
  county_events,
  county_event_mappings,
  county_event_subcounty_mappings,
  county_universes,
  county_events_meta,
  internal = TRUE,
  overwrite = TRUE)

message("sysdata.rda written: ",
        nrow(county_events), " events, ",
        nrow(county_event_mappings), " county mappings, ",
        nrow(county_universes), " universe rows")
