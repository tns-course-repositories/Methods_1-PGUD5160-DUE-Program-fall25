#c5_assignment5-script-1.r

# --- Tracts (Community District TBD) — Median Household Income, ACS 2019–2023
#     0 values are VALID (only NA is "No data available").

library(tidyverse)
library(tidycensus)
library(sf)
library(classInt)
library(RColorBrewer)

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# ---- Input CSV of tracts & filter to MN03 ----
csv_path <- "~/Desktop/lab_5/data/2020_Census_Tracts_to_2020_NTAs_and_CDTAs_Equivalency_20250903.csv"  # adjust as needed
CD_CT <- read.csv(csv_path)

tracts_id <- as.character(CD_CT$GEOID[CD_CT$CDTACode == "MN03"]) |>
  stringr::str_pad(11, pad = "0")

# Unique (state, county) list from the tract IDs
tract_meta <- tibble(GEOID = tracts_id) |>
  mutate(state = stringr::str_sub(GEOID, 1, 2),
         county = stringr::str_sub(GEOID, 3, 5)) |>
  distinct(state, county)

# ---- Pull ACS 2023 5-year *tract* median household income ----
acs_year   <- 2023
var_income <- "B19013_001"  # Median household income (inflation-adjusted $)

acs_tracts_sf <- purrr::pmap_dfr(tract_meta, function(state, county) {
  get_acs(
    geography   = "tract",
    variables   = var_income,
    survey      = "acs5",
    year        = acs_year,
    state       = state,
    county      = county,
    geometry    = TRUE,
    cache_table = TRUE
  )
}) |>
  filter(GEOID %in% tracts_id) |>
  distinct(GEOID, .keep_all = TRUE)

tract_income <- acs_tracts_sf |>
  transmute(
    GEOID, NAME, geometry,
    median_income = estimate,
    moe_income    = moe
  ) |>
  st_transform(2263)  # NYC-friendly planar CRS

# ---- NA handling (0 is VALID) & outputs ----
no_data_label <- "No data available"
no_data_fill  <- "grey85"
n_classes     <- 5
palette_name  <- "Greens"
outer_stroke_col <- "grey70"
outer_stroke_lwd <- 0.7

# Only NA flagged as no data (zeros are kept as data)
tract_income <- tract_income |>
  mutate(no_data = is.na(median_income))

# Data frame of NA tracts (printed at end)
na_only_tracts <- tract_income |>
  st_drop_geometry() |>
  filter(no_data) |>
  select(GEOID, NAME, median_income, moe_income) |>
  arrange(GEOID)

# ---- Dissolved outer boundary (single stroke) ----
outer_boundary <- tract_income |>
  st_make_valid() |>
  st_union() |>
  st_boundary()

# ---- Binning helper (includes zeros now; drops only NA/Inf) ----
make_bins <- function(values, style = c("equal", "quantile", "jenks"), k = n_classes) {
  style  <- match.arg(style)
  vals_ok <- values[is.finite(values)]                     # include 0
  uniq_n <- length(unique(vals_ok))
  if (uniq_n < k) k <- max(2, uniq_n)
  if (uniq_n <= 1) {
    brks <- c(min(vals_ok, na.rm = TRUE) - 1e-6, max(vals_ok, na.rm = TRUE))
  } else {
    brks <- classIntervals(vals_ok, n = k, style = style)$brks
  }
  fmt <- function(x) scales::dollar(x, accuracy = 1, big.mark = ",")
  lab <- paste0(fmt(head(brks, -1)), " – ", fmt(tail(brks, -1)))
  cut(values, breaks = brks, include.lowest = TRUE, labels = lab)
}

# ---- Create bins + explicit 'No data available' ----
tract_binned <- tract_income |>
  mutate(
    bin_equal = make_bins(median_income, "equal",    n_classes),
    bin_quant = make_bins(median_income, "quantile", n_classes),
    bin_jenks = make_bins(median_income, "jenks",    n_classes)
  ) |>
  mutate(
    bin_equal = forcats::fct_explicit_na(bin_equal, na_level = no_data_label),
    bin_quant = forcats::fct_explicit_na(bin_quant, na_level = no_data_label),
    bin_jenks = forcats::fct_explicit_na(bin_jenks, na_level = no_data_label)
  )

# ---- Map factory (light grey NA category + single outer boundary stroke) ----
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
      name   = "Median household income (USD)",
      values = values_vec,
      drop   = FALSE,
      guide  = guide_legend(title.position = "top",
                            keyheight = grid::unit(4, "mm"),
                            keywidth  = grid::unit(10, "mm"))
    ) +
    labs(
      title   = paste0("Census Tracts (MN03) — Median Household Income, ACS 2019–2023\n", title_suffix),
      caption = "Source: U.S. Census Bureau, ACS 5-year (B19013_001). NA shown as light gray."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title   = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid   = element_blank()
    )
}

# ---- Build and print the three maps in sequence ----
map_equal_MI <- make_map(tract_binned, "bin_equal", "Classification: Equal Interval (5 bins)")
map_quant_MI <- make_map(tract_binned, "bin_quant", "Classification: Quantile (5 bins)")
map_jenks_MI <- make_map(tract_binned, "bin_jenks", "Classification: Jenks (Natural Breaks; 5 bins)")

print(map_equal_MI)
print(map_quant_MI)
print(map_jenks_MI)

# ---- Print NA tracts (zeros not included) ----
na_only_tracts


# Save your choice of map classification options; example uses equal intervals
saveRDS(map_equal_MI, file = "~/Desktop/assignment_5/results/map_equal_MI.rds")
