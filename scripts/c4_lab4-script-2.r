#c4_lab4-script-2
# Libraries: tidyverse gives dplyr/tibble/purrr/stringr/ggplot2; sf for geometry; tidycensus for data+shapes
library(tidyverse)
library(tidycensus)
library(sf)

options(scipen = 999)
# (Optional but helpful: tidycensus uses tigris under the hood and respects these options)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# --- INPUT: CSV with tract GEOIDs; produce character vector of 11-digit IDs ---
CD_CT <- read.csv("~/Desktop/lab_4/data/2020_Census_Tracts_to_2020_NTAs_and_CDTAs_Equivalency_20250903.csv")
tracts_id <- as.character(CD_CT$GEOID[CD_CT$CDTACode == "MN03"])
tracts_id <- stringr::str_pad(tracts_id, 11, pad = "0")

# --- Build state/county lookup from tract IDs (works across counties/states) ---
tract_meta <- tibble(GEOID = tracts_id) %>%
  mutate(state = stringr::str_sub(GEOID, 1, 2),
         county = stringr::str_sub(GEOID, 3, 5)) %>%
  distinct(state, county)

# --- Pull ACS 2023 5yr total population for BLOCK GROUPS with geometry ----
acs_bg_sf <- purrr::pmap_dfr(tract_meta, function(state, county) {
  get_acs(
    geography   = "block group",
    variables   = "B01001_001",  # total population (use "DP05_0001E" if you prefer Data Profile)
    survey      = "acs5",
    year        = 2023,
    state       = state,
    county      = county,
    geometry    = TRUE,
    cache_table = TRUE
  )
})

# --- Keep only BGs whose first 11 digits (tract) are in the target set; compute labels & shares ---
bg_plot_sf <- acs_bg_sf %>%
  filter(stringr::str_sub(GEOID, 1, 11) %in% tracts_id) %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  rename(pop_total = estimate) %>%
  mutate(
    pop_total = tidyr::replace_na(pop_total, 0),
    pop_share = pop_total / sum(pop_total, na.rm = TRUE),
    BG_label  = stringr::str_sub(GEOID, -5),
    pct_label = scales::percent(pop_share, accuracy = 0.1)
  ) %>%
  # Project to planar CRS to avoid label warnings on lon/lat
  st_transform(2263)  # EPSG:2263 (NYC-friendly)

# Save if needed for future use
#save(bg_plot_sf, file = "bg_plot_sf.RData")


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
