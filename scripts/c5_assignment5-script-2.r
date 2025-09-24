#c5_assignment5-script-2

# --- Tracts (Community District TBD) — % Owner-Occupied Housing Units (ACS 2019–2023, B25032)
#     0 values are VALID (only NA is "No data available").

library(tidyverse)
library(tidycensus)
library(sf)
library(classInt)
library(RColorBrewer)

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

csv_path <- "~/Desktop/assignment_5/data/2020_Census_Tracts_to_2020_NTAs_and_CDTAs_Equivalency_20250903.csv"
CD_CT <- read.csv(csv_path)

tracts_id <- as.character(CD_CT$GEOID[CD_CT$CDTACode == "MN03"]) |>
  stringr::str_pad(11, pad = "0")

tract_meta <- tibble(GEOID = tracts_id) |>
  mutate(
    state  = stringr::str_sub(GEOID, 1, 2),
    county = stringr::str_sub(GEOID, 3, 5)
  ) |>
  distinct(state, county)

acs_year    <- 2023
total_var   <- "B25032_001"  # total occupied units
owner_total <- "B25032_002"  # owner-occupied TOTAL

acs_long <- purrr::pmap_dfr(tract_meta, function(state, county) {
  get_acs(
    geography   = "tract",
    variables   = c(total_var, owner_total),
    survey      = "acs5",
    year        = acs_year,
    state       = state,
    county      = county,
    geometry    = TRUE,
    cache_table = TRUE
  )
}) |>
  filter(GEOID %in% tracts_id) |>
  distinct(GEOID, variable, .keep_all = TRUE)

# Summarize to % owner-occupied (numerator=002, denom=001)
tract_owner <- acs_long |>
  group_by(GEOID, NAME, geometry) |>
  summarise(
    total_occ  = sum(estimate[variable == total_var],  na.rm = TRUE),
    owner_occ  = sum(estimate[variable == owner_total], na.rm = TRUE),
    total_moe  = sum(moe[variable == total_var],  na.rm = TRUE),
    owner_moe  = sum(moe[variable == owner_total], na.rm = TRUE),
    .groups    = "drop"
  ) |>
  mutate(
    pct_owner = dplyr::if_else(total_occ > 0, 100 * owner_occ / total_occ, NA_real_)
    # Optional MOE for percent:
    # pct_owner_moe = tidycensus::moe_ratio(owner_occ, owner_moe, total_occ, total_moe) * 100
  ) |>
  st_transform(2263)

# --- NA handling (0% is VALID data)
no_data_label <- "No data available"
no_data_fill  <- "grey85"
n_classes     <- 5
palette_name  <- "YlOrBr"     # orange–beige
outer_stroke_col <- "grey70"
outer_stroke_lwd <- 0.7

tract_owner <- tract_owner |>
  mutate(no_data = is.na(pct_owner))   # <-- only NA flagged; 0% is kept as data

na_zero_tracts <- tract_owner |>
  st_drop_geometry() |>
  filter(no_data) |>
  select(GEOID, NAME, total_occ, owner_occ, pct_owner) |>
  arrange(GEOID)

# Dissolved outer boundary for single outline
outer_boundary <- tract_owner |>
  st_make_valid() |>
  st_union() |>
  st_boundary()

# Binning helper (include zeros now)
make_bins <- function(values, style = c("equal", "quantile", "jenks"), k = n_classes) {
  style  <- match.arg(style)
  vals_ok <- values[is.finite(values)]           # include 0; drop NA/Inf
  uniq_n <- length(unique(vals_ok))
  if (uniq_n < k) k <- max(2, uniq_n)
  if (uniq_n <= 1) {
    brks <- c(min(vals_ok, na.rm = TRUE) - 1e-6, max(vals_ok, na.rm = TRUE))
  } else {
    brks <- classIntervals(vals_ok, n = k, style = style)$brks
  }
  fmt <- function(x) scales::percent(x / 100, accuracy = 0.1)
  lab <- paste0(fmt(head(brks, -1)), " – ", fmt(tail(brks, -1)))
  cut(values, breaks = brks, include.lowest = TRUE, labels = lab)
}

# Build bins + explicit NA category
tract_binned <- tract_owner |>
  mutate(
    bin_equal = make_bins(pct_owner, "equal",    n_classes),
    bin_quant = make_bins(pct_owner, "quantile", n_classes),
    bin_jenks = make_bins(pct_owner, "jenks",    n_classes)
  ) |>
  mutate(
    bin_equal = forcats::fct_explicit_na(bin_equal, na_level = no_data_label),
    bin_quant = forcats::fct_explicit_na(bin_quant, na_level = no_data_label),
    bin_jenks = forcats::fct_explicit_na(bin_jenks, na_level = no_data_label)
  )

# Map factory (light grey NA category + single outer boundary stroke)
make_map <- function(df, fill_col, title_suffix) {
  lvls <- levels(df[[fill_col]])
  class_lvls <- setdiff(lvls, no_data_label)
  nbins <- length(class_lvls)
  pal <- RColorBrewer::brewer.pal(max(3, min(9, nbins)), palette_name)
  if (nbins != length(pal)) pal <- colorRampPalette(pal)(nbins)
  values_vec <- setNames(pal, class_lvls)
  if (no_data_label %in% lvls) values_vec[no_data_label] <- no_data_fill
  
  ggplot() +
    geom_sf(data = df, aes(fill = .data[[fill_col]]), color = "white", linewidth = 0.12) +
    geom_sf(data = outer_boundary, fill = NA, color = outer_stroke_col, linewidth = outer_stroke_lwd) +
    coord_sf(datum = NA) +
    scale_fill_manual(
      name   = "Owner-occupied housing units (%)",
      values = values_vec,
      drop   = FALSE,
      guide  = guide_legend(title.position = "top",
                            keyheight = grid::unit(4, "mm"),
                            keywidth  = grid::unit(10, "mm"))
    ) +
    labs(
      title   = paste0("Census Tracts (MN03) — Owner-Occupied Share, ACS 2019–2023\n", title_suffix),
      caption = "Source: U.S. Census Bureau, ACS 5-year (B25032). NA shown as light gray."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title   = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid   = element_blank()
    )
}

map_equal_OO <- make_map(tract_binned, "bin_equal", "Classification: Equal Interval (5 bins)")
map_quant_OO <- make_map(tract_binned, "bin_quant", "Classification: Quantile (5 bins)")
map_jenks_OO <- make_map(tract_binned, "bin_jenks", "Classification: Jenks (Natural Breaks; 5 bins)")

print(map_equal_OO)
print(map_quant_OO)
print(map_jenks_OO)

# Print only-NA tracts (0% *not* included here)
na_zero_tracts

# Save your choice of map classification options; example uses equal intervals
saveRDS(map_equal_OO, file = "~/Desktop/assignment_5/results/map_equal_OO.rds")


