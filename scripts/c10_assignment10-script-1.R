#c10_assignment10-script-1.R

##########################################
#  NYC Restaurant Inspections — SCORE vs # Critical Violations
#  Default handling: "Critical"=1, "Not Critical"=0, "Not Applicable"=0
##########################################

# ------------------------------------------------------------
# - Reads DOH CSV data dated to October 29th, 2025
# - Builds per-inspection table (CAMIS × day)
# - Histograms, correlation tests, scatter
# - Maps points by SCORE over NYC boroughs (nybb, EPSG:2263)
# ------------------------------------------------------------

# install.packages(c(
#   "tidyverse","lubridate","janitor","stringr",
#   "ggcorrplot","patchwork","sf","viridis"
# ))

library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(ggcorrplot)
library(patchwork)
library(sf)
library(viridis)

# ---------------- 1) Read local CSV (all text) ----------
# revise upstream path as needed
csv_path <- "~/Desktop/assignment_10/data/DOHMH_New_York_City_Restaurant_Inspection_Results_20251029.csv"

raw <- readr::read_csv(
  file = csv_path,
  col_types = cols(.default = col_character()),
  na = c("", "NA", "N/A", "NULL")
) |>
  clean_names()

stopifnot(all(c("inspection_date","camis","score","critical_flag") %in% names(raw)))

# ----------- 2) Canonical inspection day (m/d/y) & core fields --------------
dat <- raw |>
  mutate(
    # use first 10 chars to drop any time strings (e.g., "06/27/2023 12:00:00 AM")
    inspection_day = mdy(str_sub(inspection_date, 1, 10)),
    # sentinel: 1900-01-01 => not yet inspected
    inspection_day = replace(inspection_day, inspection_day == ymd("1900-01-01"), as.Date(NA)),
    score          = readr::parse_number(score),
    
    # Critical mapping (default):
    # "Critical" -> 1; "Not Critical" -> 0; "Not Applicable" -> 0
    critical_flag_std = critical_flag |>
      str_replace_all("•", "") |>
      str_to_lower() |>
      str_squish(),
    critical_ind = case_when(
      critical_flag_std == "critical" ~ 1L,
      critical_flag_std == "not critical" ~ 0L,
      critical_flag_std == "not applicable" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # coordinates if present (if not, will be NA)
    latitude  = if ("latitude"  %in% names(raw)) readr::parse_number(latitude)  else NA_real_,
    longitude = if ("longitude" %in% names(raw)) readr::parse_number(longitude) else NA_real_
  )

# Keep real inspections only
dat_use <- dat |> filter(!is.na(inspection_day))

# ---- 3) Collapse repeated violation rows → one row per (CAMIS, inspection day)
per_insp <- dat_use |>
  group_by(camis, inspection_day) |>
  summarise(
    score      = suppressWarnings(as.numeric(first(na.omit(score)))),
    total_viol = dplyr::n(),                               # all items cited that day
    criticals  = sum(coalesce(critical_ind, 0L), na.rm = TRUE),
    lat        = suppressWarnings(as.numeric(first(na.omit(latitude)))),
    lon        = suppressWarnings(as.numeric(first(na.omit(longitude)))),
    .groups = "drop"
  ) |>
  filter(!is.na(score))

message("Rows in per_insp: ", nrow(per_insp))

# ---------------- 4) Histograms: SCORE & # criticals (side-by-side) ----------
p_hist_score <- ggplot(per_insp, aes(score)) +
  geom_histogram(bins = 30, color = "white", fill = "gray30") +
  labs(title = "Inspection SCORE", x = "Score (lower is better)", y = "Count") +
  theme_minimal(base_size = 12)

p_hist_crit <- ggplot(per_insp, aes(criticals)) +
  geom_histogram(bins = 30, color = "white", fill = "gray30") +
  labs(title = "# Critical Violations", x = "Count of critical violations", y = "Count") +
  theme_minimal(base_size = 12)

(p_hist_score | p_hist_crit)

# ---------------- 5) Correlations (Pearson & Spearman) -----------------------
vars <- per_insp |> select(score, criticals)

M_pearson  <- cor(vars, use = "pairwise.complete.obs", method = "pearson")
M_spearman <- cor(vars, use = "pairwise.complete.obs", method = "spearman")

cat("\nPearson correlation matrix:\n");  print(round(M_pearson, 3))
cat("\nSpearman correlation matrix:\n"); print(round(M_spearman, 3))

pearson_test  <- cor.test(per_insp$score, per_insp$criticals, method = "pearson")
spearman_test <- cor.test(per_insp$score, per_insp$criticals, method = "spearman", exact = FALSE)

cat("\n--- Pearson test ---\n")
cat("r =", round(pearson_test$estimate, 3),
    "| 95% CI [", round(pearson_test$conf.int[1], 3), ",", round(pearson_test$conf.int[2], 3), "]",
    "| p =", signif(pearson_test$p.value, 3), "\n")

cat("\n--- Spearman test ---\n")
cat("rho =", round(spearman_test$estimate, 3),
    "| p =", signif(spearman_test$p.value, 3), "\n")

ggcorrplot(M_pearson,  lab = TRUE, type = "lower", title = "Correlation (Pearson)")
ggcorrplot(M_spearman, lab = TRUE, type = "lower", title = "Correlation (Spearman)")

# ---------------- 6) Scatter: SCORE vs # critical violations -----------------
ggplot(per_insp, aes(criticals, score)) +
  geom_point(alpha = 0.35, color = "black") +
  labs(
    title = "Inspection SCORE vs # of Critical Violations",
    subtitle = paste0(
      "Pearson r = ", round(pearson_test$estimate, 2),
      " | Spearman ρ = ", round(spearman_test$estimate, 2)
    ),
    x = "# Critical Violations",
    y = "Inspection Score (lower is better)"
  ) +
  theme_minimal(base_size = 12)


# ---------------- 7) Map with score bins (more sensitivity at low end) -------
have_coords <- sum(!is.na(per_insp$lat) & !is.na(per_insp$lon)) > 0
# revise upstream path as needed
nybb_path   <- "~/Desktop/assignment_10/data/nybb_25c/nybb.shp"

if (have_coords && file.exists(nybb_path)) {
  nybb <- sf::st_read(nybb_path, quiet = TRUE)
  
  # Ensure EPSG:2263
  if (is.na(sf::st_crs(nybb))) {
    sf::st_crs(nybb) <- 2263
  } else if (sf::st_crs(nybb)$epsg != 2263) {
    nybb <- sf::st_transform(nybb, 2263)
  }
  
  # Create discrete bins:
  #   A-range split for sensitivity: 0–5, 6–10, 11–13
  #   B-range: 14–27
  #   C-range: 28–49
  #   Outliers: 50+
  bin_breaks <- c(-Inf, 5, 10, 13, 27, 49, Inf)
  bin_labels <- c("0–5 (A)", "6–10 (A)", "11–13 (A limit)",
                  "14–27 (B)", "28–49 (C)", "50+ (outlier)")
  
  pts_sf <- per_insp %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    mutate(
      score_bin = cut(score,
                      breaks = bin_breaks,
                      labels = bin_labels,
                      include.lowest = TRUE, right = TRUE),
      score_bin = forcats::fct_relevel(score_bin, bin_labels)  # enforce order
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(nybb))
  
  bb <- st_bbox(nybb)
  
  ggplot() +
    # Borough outline: 1 pt black stroke
    geom_sf(data = nybb, fill = NA, color = "black", linewidth = 1) +
    # Points colored by discrete score bins
    geom_sf(data = pts_sf, aes(color = score_bin), size = 0.8, alpha = 0.65) +
    # Discrete viridis palette
    scale_color_viridis_d(option = "C", direction = -1, name = "Score bins\n(lower is better)") +
    coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      expand = 0,
      crs = st_crs(nybb)
    ) +
    labs(
      title = "NYC Restaurant Inspections — Score Bins (A-sensitive, outliers split)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
} else if (!have_coords) {
  message("No latitude/longitude available in per_insp; map with bins skipped.")
} else {
  message("nybb shapefile not found at: ", nybb_path)
}

# ---------------- 8) Save PNGs) -------

## Creates ~/Desktop/assignment_10/results (if missing)
## and exports key plots with clear filenames.

# 1) output dir
# revise upstream path as needed
out_dir <- "~/Desktop/assignment_10/results"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

# 2) rebuild correlation plots if needed
p_corr_pearson  <- ggcorrplot(M_pearson,  lab = TRUE, type = "lower",
                              title = "Correlation (Pearson)")
p_corr_spearman <- ggcorrplot(M_spearman, lab = TRUE, type = "lower",
                              title = "Correlation (Spearman)")

# 3) rebuild the scatter
p_scatter <- ggplot(per_insp, aes(criticals, score)) +
  geom_point(alpha = 0.35, color = "black") +
  labs(
    title = "Inspection SCORE vs # of Critical Violations",
    subtitle = paste0(
      "Pearson r = ", round(pearson_test$estimate, 2),
      " | Spearman ρ = ", round(spearman_test$estimate, 2)
    ),
    x = "# Critical Violations",
    y = "Inspection Score (lower is better)"
  ) +
  theme_minimal(base_size = 12)

# 4) histogram pair (from earlier)
p_hists <- (p_hist_score | p_hist_crit)

ggsave(filename = file.path(out_dir, "score_histograms.png"),
       plot     = p_hists,
       width    = 10, height = 5, dpi = 300)

ggsave(filename = file.path(out_dir, "corr_pearson.png"),
       plot     = p_corr_pearson,
       width    = 6, height = 5, dpi = 300)

ggsave(filename = file.path(out_dir, "corr_spearman.png"),
       plot     = p_corr_spearman,
       width    = 6, height = 5, dpi = 300)

ggsave(filename = file.path(out_dir, "score_vs_criticals_scatter.png"),
       plot     = p_scatter,
       width    = 6.5, height = 5.5, dpi = 300)

# 5) save map only if it exists
if (exists("pts_sf") && exists("nybb")) {
  p_map <- ggplot() +
    geom_sf(data = nybb, fill = NA, color = "black", linewidth = 1) +
    geom_sf(data = pts_sf, aes(color = score_bin), size = 0.8, alpha = 0.65) +
    scale_color_viridis_d(option = "C", direction = -1,
                          name = "Score bins\n(lower is better)") +
    coord_sf(
      xlim = c(st_bbox(nybb)["xmin"], st_bbox(nybb)["xmax"]),
      ylim = c(st_bbox(nybb)["ymin"], st_bbox(nybb)["ymax"]),
      expand = 0,
      crs = st_crs(nybb)
    ) +
    labs(title = "NYC Restaurant Inspections — Score Bins") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  
  ggsave(filename = file.path(out_dir, "nyc_inspections_map.png"),
         plot     = p_map,
         width    = 7, height = 7, dpi = 300)
}

message("PNG export complete. Files saved to: ", out_dir)







