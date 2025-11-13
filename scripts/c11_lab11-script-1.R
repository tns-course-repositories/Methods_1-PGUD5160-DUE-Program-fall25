# c11_lab11-script-1.R
# =====================================================================
# NYC Street Tree Census 2015 — Streamlined EDA (tidyverse)
# Focus: tree_dbh, status, spc_common, cb_num
# =====================================================================

# ---- 0) Setup --------------------------------------------------------
# install.packages(c("tidyverse","janitor","forcats","scales","ggrepel"), quiet = TRUE)
library(tidyverse)
library(janitor)
library(forcats)
library(scales)
library(ggrepel)

options(dplyr.summarise.inform = FALSE)
options(scipen = 999)

# Point to your local CSV (download from: https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh)
# Tip: this is a big file; reading only the needed columns speeds things up.
data_path <- "~Desktop/c11_lab/data/nyc_tree_census_2015.csv"

# ---- 1) Read & Clean -------------------------------------------------
trees <- readr::read_csv(
  file = data_path,
  col_select = c(tree_dbh, status, spc_common, cncldist, borocode),
  progress = TRUE,
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    # standardize fields
    status     = as.factor(status),
    spc_common = na_if(spc_common, "") |> str_to_title() |> as.factor(),
    tree_dbh   = suppressWarnings(as.numeric(tree_dbh)),
    cncldist   = suppressWarnings(as.integer(cncldist)),
    borocode   = suppressWarnings(as.integer(borocode)),
    # derive readable borough name from borocode (1–5)
    boroname = case_when(
      borocode == 1 ~ "Manhattan",
      borocode == 2 ~ "Bronx",
      borocode == 3 ~ "Brooklyn",
      borocode == 4 ~ "Queens",
      borocode == 5 ~ "Staten Island",
      TRUE ~ NA_character_
    )
  ) %>%
  # defensive trim on DBH
  filter(!is.na(tree_dbh), tree_dbh >= 0, tree_dbh <= 200)

glimpse(trees)

# ---- 2) Quick summaries ----------------------------------------------
summary(select(trees, tree_dbh, status, spc_common, cncldist, borocode))

trees %>%
  count(boroname, status, sort = TRUE) %>%
  print(n = 10)

# ---- 3) Identify Top Species -----------------------------------------
top_n_species <- 15
top_species <- trees %>%
  count(spc_common, sort = TRUE, name = "n") %>%
  slice_head(n = top_n_species) %>%
  pull(spc_common) %>%
  as.character()   # <— make it character

trees <- trees %>%
  mutate(spc_common_top = fct_other(spc_common, keep = top_species))

# ---- 4) EDA Visualizations -------------------------------------------

# (a) DBH Distribution (raw)
p_dbh_hist <- ggplot(trees, aes(x = tree_dbh)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Street Tree Diameter (DBH) — NYC 2015",
    x = "DBH (inches)",
    y = "Count",
    caption = "Source: NYC 2015 Street Tree Census"
  ) +
  theme_minimal(base_size = 12)
p_dbh_hist

# (b) DBH Distribution (log1p)
p_dbh_hist_log <- ggplot(trees, aes(x = log1p(tree_dbh))) +
  geom_histogram(bins = 60) +
  labs(
    title = "Street Tree Diameter (log1p scale)",
    x = "log(1 + DBH)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)
p_dbh_hist_log

# (c) Status Composition
p_status <- trees %>%
  filter(!is.na(status)) %>%
  ggplot(aes(x = fct_infreq(status))) +
  geom_bar() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Tree Status Counts",
    x = "Status",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)
p_status

# (d) Top Species (overall)
p_species_top <- trees %>%
  filter(spc_common %in% top_species) %>%
  count(spc_common, sort = TRUE) %>%
  mutate(spc_common = fct_reorder(spc_common, n)) %>%
  ggplot(aes(n, spc_common)) +
  geom_col() +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = glue::glue("Top {top_n_species} Common Street Tree Species (2015)"),
    x = "Count",
    y = NULL
  ) +
  theme_minimal(base_size = 12)
p_species_top

# (e) DBH by Status (boxplot, trimmed to 1–99% to tame outliers)
p_dbh_status <- trees %>%
  filter(!is.na(status)) %>%
  ggplot(aes(x = status, y = tree_dbh)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_cartesian(ylim = quantile(trees$tree_dbh, c(0.01, 0.99), na.rm = TRUE)) +
  labs(
    title = "DBH by Status (trimmed 1st–99th percentile)",
    x = "Status",
    y = "DBH (inches)"
  ) +
  theme_minimal(base_size = 12)
p_dbh_status

# (f) Median DBH by Species (Top N)
p_dbh_species <- trees %>%
  filter(spc_common %in% top_species) %>%
  group_by(spc_common) %>%
  summarise(median_dbh = median(tree_dbh, na.rm = TRUE), n = n(), .groups = "drop") %>%
  mutate(spc_common = fct_reorder(spc_common, median_dbh)) %>%
  ggplot(aes(median_dbh, spc_common)) +
  geom_point(aes(size = n)) +
  scale_x_continuous(labels = label_number(accuracy = 1)) +
  scale_size_continuous(labels = label_comma()) +
  labs(
    title = "Median DBH by Species (Top N)",
    x = "Median DBH (inches)",
    y = NULL,
    size = "Trees"
  ) +
  theme_minimal(base_size = 12)
p_dbh_species

# (g) Top Districts by Tree Count (using cncldist)
dist_summary <- trees %>%
  filter(!is.na(cncldist), cncldist > 0) %>%
  group_by(cncldist) %>%
  summarise(
    n_trees = n(),
    med_dbh = median(tree_dbh, na.rm = TRUE),
    .groups = "drop"
  )

p_dist_counts <- dist_summary %>%
  slice_max(order_by = n_trees, n = 25) %>%
  mutate(cncldist = fct_reorder(as.factor(cncldist), n_trees)) %>%
  ggplot(aes(n_trees, cncldist)) +
  geom_col() +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Top 25 Districts by Tree Count (cncldist)",
    x = "Tree Count",
    y = "District (cncldist)"
  ) +
  theme_minimal(base_size = 12)
p_dist_counts

# (h) Borough overview (derived from borocode)
boro_summary <- trees %>%
  filter(!is.na(boroname)) %>%
  group_by(boroname) %>%
  summarise(
    n_trees = n(),
    med_dbh = median(tree_dbh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_trees))
print(boro_summary, n = Inf)

# ---- 5) Light Text Summaries -----------------------------------------
overall <- trees %>%
  summarise(
    n          = n(),
    median_dbh = median(tree_dbh, na.rm = TRUE),
    p95_dbh    = quantile(tree_dbh, 0.95, na.rm = TRUE)
  )
overall

status_summ <- trees %>%
  filter(status %in% c("Alive","Dead","Stump")) %>%
  group_by(status) %>%
  summarise(
    n = n(),
    median_dbh = median(tree_dbh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct = n / sum(n))
status_summ

species_summ <- trees %>%
  filter(spc_common %in% top_species) %>%
  group_by(spc_common) %>%
  summarise(n = n(), median_dbh = median(tree_dbh, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(n))
species_summ %>% slice_head(n = 10)

# ---- 6) Save PNG Outputs ---------------------------------------------
out_dir <- "./tree_eda_results"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

save_plot <- function(p, filename, w = 9, h = 6, dpi = 150) {
  ggsave(file.path(out_dir, filename), plot = p, width = w, height = h, dpi = dpi)
}

save_plot(p_dbh_hist,      "dbh_hist.png")
save_plot(p_dbh_hist_log,  "dbh_hist_log.png")
save_plot(p_status,        "status_counts.png")
save_plot(p_species_top,   "top_species.png")
save_plot(p_dbh_status,    "dbh_by_status.png")
save_plot(p_dbh_species,   "median_dbh_by_species.png")
save_plot(p_dist_counts,   "district_top_counts.png")

# Write summaries as small CSVs
readr::write_csv(dist_summary,  file.path(out_dir, "district_summary.csv"))
readr::write_csv(boro_summary,  file.path(out_dir, "borough_summary.csv"))
readr::write_csv(species_summ,  file.path(out_dir, "species_top_summary.csv"))