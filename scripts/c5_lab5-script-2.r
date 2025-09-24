#c5_lab5-script-2 

# Libraries: tidyverse gives dplyr/tibble/purrr/stringr/ggplot2; sf for geometry; tidycensus for data+shapes
library(tidyverse)
library(tidycensus)
library(sf)

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# --- INPUT: CSV with tract GEOIDs; produce character vector of 11-digit IDs ---

CD_CT <- read.csv("~/Desktop/lab_5/data/2020_Census_Tracts_to_2020_NTAs_and_CDTAs_Equivalency_20250903.csv")
tracts_id <- as.character(CD_CT$GEOID[CD_CT$CDTACode == "MN03"]) |> stringr::str_pad(11, pad = "0")

# --- Build state/county lookup from tract IDs (works across counties/states) ---
tract_meta <- tibble(GEOID = tracts_id) %>%
  mutate(
    state  = stringr::str_sub(GEOID, 1, 2),
    county = stringr::str_sub(GEOID, 3, 5)
  ) %>%
  distinct(state, county)

# --- Pull ACS 2023 5yr total population for BLOCK GROUPS with geometry ----
acs_bg_sf <- purrr::pmap_dfr(tract_meta, function(state, county) {
  get_acs(
    geography   = "block group",
    variables   = "B01001_001",  # total population (ACS detailed tables)
    survey      = "acs5",
    year        = 2023,
    state       = state,
    county      = county,
    geometry    = TRUE,
    cache_table = TRUE
  )
})

# --- Restrict to BGs in target tracts; capture NA flags BEFORE any replacement ---
acs_bg_sel <- acs_bg_sf %>%
  filter(stringr::str_sub(GEOID, 1, 11) %in% tracts_id) %>%
  distinct(GEOID, .keep_all = TRUE)

# Vector of BGs where total population estimate is NA (zeros are OK and NOT flagged)
na_bg_ids <- acs_bg_sel %>%
  filter(is.na(estimate)) %>%
  distinct(GEOID) %>%
  pull(GEOID)

# --- Prep plotting data (let zeros pass; only NAs were flagged above) ---
bg_plot_sf <- acs_bg_sel %>%
  transmute(
    GEOID,
    NAME,
    geometry,
    pop_total = tidyr::replace_na(estimate, 0),  # zeros kept; NAs replaced with 0 for sums
    flagged_na_pop = GEOID %in% na_bg_ids,       # logical flag column for reference
    BG_label  = stringr::str_sub(GEOID, -5)
  ) %>%
  mutate(
    pop_share = pop_total / sum(pop_total, na.rm = TRUE),
    pct_label = scales::percent(pop_share, accuracy = 0.1)
  ) %>%
  st_transform(2263)  # EPSG:2263 (NYC-friendly)

# --- Map 1: label each CBG with last 5 digits of GEOID ---
ggplot(bg_plot_sf) +
  geom_sf(size = 0.2, fill = "white", color = "black") +
  geom_sf_text(aes(label = BG_label), size = 2.5, color = "darkred", check_overlap = TRUE) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Census Block Groups for Manhattan CD03 Tracts",
    subtitle = "Labels are last 5 digits of GEOID",
    caption = "Source: US Census Bureau ACS 2023 Boundaries"
  ) +
  theme_void(base_size = 11)

# --- Map 2: label each CBG with % of total population (within selection) ---
ggplot(bg_plot_sf) +
  geom_sf(size = 0.2, fill = "white", color = "black") +
  geom_sf_text(aes(label = pct_label), size = 2.5, color = "darkred", check_overlap = TRUE) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Census Block Groups for Manhattan CD03 Tracts",
    subtitle = "% of total population across selected BGs (ACS 2023 5-year)",
    caption = "Source: US Census Bureau; tidycensus"
  ) +
  theme_void(base_size = 11)

# --- Print vector of BG GEOIDs with NA total population (if any) ---
if (length(na_bg_ids) > 0) {
  message("Block groups with NA total population (B01001_001, ACS 2023):")
  print(na_bg_ids)
} else {
  message("No block groups with NA total population in the selected tracts.")
}

