# c5_lab5-script-1

# Mapping NYS Counties - Median Income, with pre-map histogram
# - Only NA are flagged as "No data"; zeros are valid

# ---- Packages (install if needed) ----
# install.packages(c("tidyverse","tidycensus","sf","classInt","RColorBrewer","ggplot2","tigris"))

library(tidyverse)
library(tidycensus)
library(sf)
library(classInt)      # equal / quantile / jenks breaks
library(RColorBrewer)
library(ggplot2)

options(tigris_use_cache = TRUE, tigris_class = "sf")
options(scipen = 999)

# ---- Census API key (uncomment + set once) ----
# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)


# ---- Settings ----
acs_year      <- 2023
var_income    <- "B19013_001"      # Median household income (2023 inflation-adjusted $)
n_classes     <- 6
palette_name  <- "Greens"
no_data_label <- "No data (NA)"
no_data_fill  <- "grey80"

# ---- Pull ACS: NY State Counties w/ geometry ----
ny_counties <- get_acs(
  geography   = "county",
  variables   = var_income,
  state       = "NY",
  survey      = "acs5",
  year        = acs_year,
  geometry    = TRUE,
  cache_table = TRUE
) |>
  st_as_sf()

# ---- Clean + flag *only* NA as no-data (zeros pass through) ----
ny_income <- ny_counties |>
  transmute(
    GEOID, NAME, geometry,
    median_income = estimate
  ) |>
  mutate(
    no_data = is.na(median_income)   # <-- only NA flagged
  )

# --- Report NA counties (vector + original rows) ---
na_geoids <- ny_income$GEOID[ny_income$no_data]

na_original <- ny_counties |>
  st_drop_geometry() |>
  filter(GEOID %in% na_geoids) |>
  select(GEOID, NAME, estimate, moe) |>
  arrange(GEOID)

message("NA GEOIDs: ", paste(na_geoids, collapse = ", "))
if (nrow(na_original) > 0) {
  message("NA original rows (showing first few):")
  print(utils::head(na_original, 10))
} else {
  message("NA original rows: none")
}

# ---- Quick histogram BEFORE mapping (zeros included as valid) ----
hist_data <- ny_income |>
  filter(!is.na(median_income))

p_hist <- ggplot(hist_data, aes(x = median_income)) +
  geom_histogram(bins = 20, linewidth = 0.2, color = "white") +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = paste0("Distribution of Median Household Income — NY Counties (ACS ", acs_year, " 5-year)"),
    x = "Median household income (USD)",
    y = "Number of counties",
    caption = "Source: U.S. Census Bureau, ACS 2019–2023 5-year (B19013_001)."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
print(p_hist)

# ---- Helper: make classed factor bins ----
make_bins <- function(values, style = c("equal", "quantile", "jenks"), k = n_classes) {
  style  <- match.arg(style)
  vals_ok <- values[is.finite(values)]        # <-- keep zeros; only drop NA/Inf
  uniq_n <- length(unique(vals_ok))
  
  if (uniq_n < k) k <- max(2, uniq_n)
  
  if (uniq_n <= 1) {
    # degenerate case: one distinct value
    brks <- c(min(vals_ok, na.rm = TRUE) - 1e-6, max(vals_ok, na.rm = TRUE))
  } else {
    brks <- classIntervals(vals_ok, n = k, style = style)$brks
  }
  
  fmt <- function(x) scales::dollar(x, accuracy = 1, big.mark = ",")
  lab <- paste0(fmt(head(brks, -1)), " – ", fmt(tail(brks, -1)))
  cut(values, breaks = brks, include.lowest = TRUE, labels = lab)
}

# ---- Bin three ways; explicit 'No data (NA)' label only for NA ----
ny_binned <- ny_income |>
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

# ---- Map factory ----
make_map <- function(df, fill_col, title_suffix) {
  lvls <- levels(df[[fill_col]])
  has_nodata <- no_data_label %in% lvls
  class_lvls <- setdiff(lvls, no_data_label)
  
  nbins <- length(class_lvls)
  pal <- brewer.pal(max(3, min(9, nbins)), palette_name)
  if (nbins != length(pal)) pal <- colorRampPalette(pal)(nbins)
  
  values_vec <- setNames(pal, class_lvls)
  if (has_nodata) values_vec[no_data_label] <- no_data_fill
  
  ggplot(df) +
    geom_sf(aes(fill = .data[[fill_col]]), linewidth = 0.2, color = "white") +
    coord_sf(datum = NA) +
    scale_fill_manual(
      name = "Median household income (USD)",
      values = values_vec,
      drop = FALSE,
      guide = guide_legend(title.position = "top", keyheight = unit(4, "mm"), keywidth = unit(10, "mm"))
    ) +
    labs(
      title = paste0("New York State Counties — Median Household Income (ACS ", acs_year, " 5-year)\n", title_suffix),
      caption = "Source: U.S. Census Bureau, ACS 2019–2023 5-year (B19013_001).\nOnly NA shown as “No data (NA)”."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )
}

# ---- Build the three maps ----
map_equal <- make_map(ny_binned, "bin_equal", "Classification: Equal Interval")
map_quant <- make_map(ny_binned, "bin_quant", "Classification: Quantile")
map_jenks <- make_map(ny_binned, "bin_jenks", "Classification: Jenks (Natural Breaks)")

# ---- Print maps ----
print(map_equal)
print(map_quant)
print(map_jenks)

# ---- Output NA values it vector/table format ----
na_geoids <- na_geoids
na_original <- na_original
