#c7_assignment7-script-1.r
#Map NYC Building Permits to NYC Buildings Footprints

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/assignment7", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

# ---- DOB Permits by Issued Date (>= 2025-01-01) ----

library(tidyverse)
library(sf)
library(tidycensus)
library(tmap)
library(classInt)      # for Jenks
library(RColorBrewer)  # for the "Greens" palette
library(scales)

# if census api key is needed
# census_api_key("YOUR CENSUS API KEY HERE")



# Filter out Scientific Notation for Plots
options(scipen = 999)  # globally discourage scientific notation

# ---- Read in DOB Permits by Issued Date (>= 2025-01-01 + NYC Building Centroids ----
permits_path <- "~/Desktop/assignment_7/data/DOB_permits_2025-2-date.csv"
building_centroids <- st_read("~/Desktop/assignment_7/data/bes_20230301/bes_20230301.shp")

# --- Read CSV (base utils to avoid vroom/readr path issues) ---
permits_tbl <- utils::read.csv(
  permits_path,
  check.names = FALSE,     # keep original column names like "Bin"
  stringsAsFactors = FALSE # keep text as character
)

# If there are any empty column names, repair them to avoid dplyr rename issues later
if (any(!nzchar(names(permits_tbl)))) {
  empty_idx <- which(!nzchar(names(permits_tbl)))
  names(permits_tbl)[empty_idx] <- paste0("unnamed_col_", empty_idx)
}

# ---  Standardize BIN columns/types on both datasets ---
# building_centroids is an sf POINT with numeric column 'bin'
cent_sf <- building_centroids %>%
  mutate(bin = as.integer(bin)) %>%      # coerce numeric → integer
  filter(!is.na(bin)) %>%
  group_by(bin) %>% slice(1) %>% ungroup()   # ensure 1 centroid per BIN

# --- Standardize BIN in permits (create a clean 'bin' column without rename) ---
nm  <- names(permits_tbl); nml <- tolower(nm)
bin_idx <- which(nm == "Bin" | nml == "bin")[1]
stopifnot(!is.na(bin_idx))
permits_tbl$bin <- as.integer(permits_tbl[[nm[bin_idx]]])
permits_tbl <- permits_tbl %>% filter(!is.na(bin))

# --- ONE-TO-MANY join ---
permits_centroids_sf <- dplyr::inner_join(cent_sf, permits_tbl, by = "bin")

# --- QA ---
message("Centroids (unique BINs): ", nrow(cent_sf))
message("Permits (rows, no de-dup): ", nrow(permits_tbl))
message("Joined rows (one per permit with centroid): ", nrow(permits_centroids_sf))

# bin_counts: one row per BIN with number of permits
bin_counts <- permits_centroids_sf |>
  sf::st_drop_geometry() |>
  dplyr::count(bin, name = "n_permits")

centroids_with_counts <- cent_sf |>
  dplyr::left_join(bin_counts, by = "bin") |>
  dplyr::mutate(n_permits = tidyr::replace_na(n_permits, 0L))

# Keep buildings with >= 1 permit (includes singles)
multi_centroids_sf <- centroids_with_counts |>
  dplyr::filter(n_permits >= 1)

# Quick map with ggplot
gg_permit_plot <- ggplot() +
  geom_sf(data = multi_centroids_sf, aes(size = n_permits), alpha = 0.1) +
  scale_size_continuous(name = "Permits per building") +
  coord_sf(expand = FALSE) +
  labs(title = "Buildings with Multiple Permits") +
  theme_minimal()# Tmap

# Print the plot
gg_permit_plot


# --- tmap proportional symbols (size ~ n_permits) ---

tmap_mode("view")  # interactive

tm_multi <-
  tm_shape(multi_centroids_sf) +
  tm_symbols(
    size = "n_permits",        # proportional symbolization (like ggplot size aesthetic)
    col  = "#2b6cb0",          # constant fill color (blue)
    alpha = 0.7,
    border.col = "grey35"
    # You can tweak scaling if you want:
    # size.lim = c(min(multi_centroids_sf$n_permits), max(multi_centroids_sf$n_permits)),
    # size.max = 0.25  # increase/decrease overall symbol sizes
  ) +
  tm_layout(
    title = "Buildings with Multiple Permits (size ∝ count)",
    legend.outside = TRUE
  )

tm_multi

# ----  Pull ACS 2023 5-yr median income for NY PUMAs (geometry) ----
# B19013_001 = Median household income (inflation-adjusted $)
pumas_ny <- get_acs(
  geography = "public use microdata area",
  state     = "NY",
  variables = "B19013_001",
  year      = 2023,
  survey    = "acs5",
  geometry  = TRUE,
  output    = "wide"   # gives B19013_001E (estimate) and B19013_001M (MOE)
)

# ----  Keep only NYC PUMAs (spatial clip to 5 counties) ----
nyc_counties <- c("005","047","061","081","085")  # Bronx, Kings, New York, Queens, Richmond
nyc_sf <- tigris::counties(state = "NY", cb = TRUE, year = 2023, class = "sf") |>
  filter(COUNTYFP %in% nyc_counties) |>
  st_union()

# Check and reinstate PUMAs whose representative point is within NYC
pumas_nyc <- pumas_ny[ lengths(sf::st_within(sf::st_point_on_surface(pumas_ny), nyc_sf)) > 0, ]

# ----  Reproject to EPSG:2263 and compute area (US ft²) and mi² ----
pumas_2263 <- st_transform(pumas_nyc, 2263)

# st_area() in EPSG:2263 returns US survey feet²; keep numeric to avoid unit wrangling
pumas_2263 <- pumas_2263 |>
  mutate(
    area_usft2 = as.numeric(st_area(geometry)),           # US ft² (since CRS 2263 is US-foot based)
    area_mi2   = area_usft2 / (5280^2)                    # square miles
  )

# ----  Points-in-polygon: count multi_centroids_sf total permits within each PUMA ----
stopifnot(exists("multi_centroids_sf"), inherits(multi_centroids_sf, "sf"))

# Reproject from EPSG: 8767 to EPSG: 2263
multi_centroids_sf <- sf::st_transform(multi_centroids_sf, 2263)

# Fast count: number of permits intersecting each polygon
ix <- st_intersects(pumas_2263, multi_centroids_sf, sparse = TRUE)
permit_counts <- vapply(ix, function(i) sum(multi_centroids_sf$n_permits[i], na.rm = TRUE), numeric(1))

# Write to Console Permits and Points Statistics
total_permits <- sum(permit_counts, na.rm = TRUE)
message("Total permits counted across PUMAs: ",
        formatC(total_permits, format = "d", big.mark = ","))
message("Total points in multi_centroids_sf: ",
        formatC(nrow(multi_centroids_sf), format = "d", big.mark = ","))


# ----  Normalize to rate per square mile & tidy income cols ----
pumas_enriched <- pumas_2263 |>
  mutate(
    permits_total        = permit_counts,
    permits_per_sqmi     = ifelse(area_mi2 > 0, permits_total / area_mi2, NA_real_),
    median_income_2023   = B19013_001E,
    median_income_moe    = B19013_001M
  ) |>
  select(GEOID, NAME, median_income_2023, median_income_moe,
         area_usft2, area_mi2, permits_total, permits_per_sqmi, geometry)


# ---- Console summary: totals + overall rate + top 5 by rate ----
total_permits <- sum(pumas_enriched$permits_total, na.rm = TRUE)
total_area_mi2 <- sum(pumas_enriched$area_mi2, na.rm = TRUE)
overall_rate <- if (total_area_mi2 > 0) total_permits / total_area_mi2 else NA_real_

message(sprintf("Total permits counted across NYC PUMAs: %s",
                formatC(total_permits, format = "d", big.mark = ",")))
message(sprintf("Overall permits per square mile (citywide): %.2f", overall_rate))

# Top 5 PUMAs by permits per square mile
top5 <- pumas_enriched |>
  sf::st_drop_geometry() |>
  dplyr::select(GEOID, NAME, permits_total, permits_per_sqmi) |>
  dplyr::arrange(dplyr::desc(permits_per_sqmi)) |>
  head(5)

print(top5)


# --- Jenks (5) on median income ---
vals <- pumas_enriched |> st_drop_geometry() |> pull(median_income_2023)
vals <- vals[is.finite(vals)]
ci   <- classInt::classIntervals(vals, n = 5, style = "jenks")
brks <- unique(ci$brks)

# Lower/upper vectors
lowers <- head(brks, -1)
uppers <- tail(brks, -1) 


# Build pretty labels like "$35,000–$55,400"
bin_labels <- map2_chr(lowers, uppers,
                       ~ paste0(dollar(.x, accuracy = 1000), "–", dollar(.y, accuracy = 1000)))
bin_labels[length(bin_labels)] <- paste0("≥ ", dollar(lowers[length(lowers)], accuracy = 1000))

pumas_plot <- pumas_enriched %>%
  mutate(mi_jenks = cut(median_income_2023,
                        breaks = brks,
                        include.lowest = TRUE,
                        labels = bin_labels))

# one representative point per PUMA for proportional symbols
puma_pts <- sf::st_point_on_surface(pumas_plot)


# --- Static ggplot: Greens ramp + proportional circles ---
gg_pumas_plot <- ggplot() +
  geom_sf(data = pumas_plot,
          aes(fill = mi_jenks),
          color = "white", linewidth = 0.25) +
  scale_fill_brewer(palette = "Greens",
                    name = "Median HH Income (ACS 2023)") +  # labels already pretty
  geom_sf(data = puma_pts,
          aes(size = permits_total),
          shape = 21, fill = "black", color = "white", alpha = 0.7) +
  scale_size_area(max_size = 10,
                  name = "Total permits",
                  labels = scales::label_comma()) +          # also avoid sci notation for sizes
  coord_sf(expand = FALSE) +
  labs(title = "NYC PUMAs: Median Income (Jenks 5) + Proportional Permit Totals") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#Create Plot
gg_pumas_plot


## Correlative plot

# Build a clean data.frame
df <- pumas_enriched %>%
  st_drop_geometry() %>%
  transmute(
    median_income = as.numeric(median_income_2023),
    permits_total = as.numeric(permits_total)
  ) %>%
  filter(is.finite(median_income), is.finite(permits_total))

# Plot: points + linear smooth, nicely formatted axes
gg_corr_plot <- ggplot(df, aes(x = median_income, y = permits_total)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,
              color = "#2ca25f", fill = "#a1d99b") +  # green line + light-green ribbon
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Permits vs. Median Household Income by PUMA",
    x = "Median household income (ACS 2023 5-year)",
    y = "Total permits"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

gg_corr_plot



