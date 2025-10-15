############################################################
# NYC Evictions — Top 25 Buildings by Executed Evictions
# Assignment 8 - c8_assignment8-script-2.R (annotated + GOALs)
###########################################################

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr (mutate/filter/select/arrange/left_join), tidyr, readr, ggplot2
  library(sf)         # spatial (st_read/st_transform/st_intersects/…)
  library(forcats)    # factor helpers (fct_reorder)
  library(scales)     # comma() labeling
  library(patchwork)  # plot composition
})

# =============================================================================
# INPUTS
# =============================================================================
centroids_shp  <- "~/Desktop/c8_assignment/data/bes_20251010/bes_20230301.shp"   # building centroids (EPSG:2263), has BIN
rs_parcels_shp <- "~/Desktop/c8_assignment/data/joined_rs/joined_rs.shp"         # RS parcels (EPSG:2263)
ev             <- readRDS("~/Desktop/c8_assignment/data/ev.rds")                 # must contain a BIN-like field

# =============================================================================
# HELPERS
# =============================================================================
# Normalize BIN to a clean character string of digits (no scientific notation, no punctuation)
normalize_bin <- function(x) {
  s <- if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
  s <- stringr::str_replace_all(s, "[^0-9]", "")
  ifelse(s == "", NA_character_, s)
}
# Remove known bogus/non-real BINs up front
invalid_bins <- c("1000000","2000000","3000000","4000000")

# =============================================================================
# 1) Evictions → count per BIN
#    GOAL: get a tidy table with one row per BIN and a column `evictions_total`
# =============================================================================
bin_col <- intersect(names(ev), c("BIN","bin","building_identification_number"))[1]
stopifnot(!is.na(bin_col))

ev_bin <- ev %>%
  # mutate(): create/standardize `bin` from the detected BIN-like column (data cleaning)
  mutate(bin = normalize_bin(.data[[bin_col]])) %>%
  # filter(): keep rows with a valid BIN and drop bogus BINs (row subset)
  filter(!is.na(bin), bin != "", !bin %in% invalid_bins) %>%
  # count(): group by BIN and count rows → produces `n`; name it `evictions_total`
  count(bin, name = "evictions_total")

# =============================================================================
# 2) Centroids + join counts
#    GOAL: keep (bin, geometry), enforce EPSG:2263, then join eviction counts by BIN
# =============================================================================
centroids_sf <- st_read(centroids_shp, quiet = TRUE)
bin_centroid_col <- intersect(names(centroids_sf), c("BIN","bin"))[1]
stopifnot(!is.na(bin_centroid_col))

centroids_2263 <- centroids_sf %>%
  # mutate(): standardize BIN to character digits
  mutate(bin = normalize_bin(.data[[bin_centroid_col]])) %>%
  # filter(): keep only centroids with a valid BIN
  filter(!is.na(bin), bin != "") %>%
  # select(): keep only the columns needed for analysis/join (narrower table)
  select(bin, geometry) %>%
  # st_transform(): ensure planar CRS (NY State Plane feet) for spatial ops later
  st_transform(2263) %>%
  # left_join(): add `evictions_total` from ev_bin by BIN (preserves all centroids)
  left_join(ev_bin, by = "bin") %>%
  # mutate(): replace NA counts with 0 (buildings with no recorded evictions)
  mutate(evictions_total = replace_na(evictions_total, 0L))

# =============================================================================
# 3) RS parcels + flag centroids inside RS
#    GOAL: clean RS polygons and mark each centroid as inside or outside RS
# =============================================================================
rs_parcels <- st_read(rs_parcels_shp, quiet = TRUE) %>%
  # st_make_valid(): fix invalid polygon rings if any
  st_make_valid() %>%
  # st_transform(): match centroids CRS for spatial relations
  st_transform(2263)

centroids_2263 <- centroids_2263 %>%
  # mutate(): new logical column TRUE if centroid intersects any RS parcel
  mutate(in_rs_parcel = lengths(st_intersects(., rs_parcels)) > 0)

# =============================================================================
# 4) Top-25 data frames
#    GOAL: two tables of BINs with the highest eviction totals (overall & inside RS)
# =============================================================================
top_buildings_all <- centroids_2263 %>%
  st_drop_geometry() %>%                # drop geometry for a plain tibble
  filter(evictions_total > 0) %>%       # filter(): only buildings with ≥1 eviction
  arrange(desc(evictions_total)) %>%    # arrange(): sort descending by count
  slice_head(n = 25)                    # slice_head(): keep top 25 rows

top_buildings_in_rs <- centroids_2263 %>%
  filter(in_rs_parcel, evictions_total > 0) %>%  # filter(): only inside RS & ≥1
  st_drop_geometry() %>%                          # drop geometry
  arrange(desc(evictions_total)) %>%              # sort descending
  slice_head(n = 25)                              # top 25

# =============================================================================
# 5) Plot helper: true horizontal bars (x = evictions, y = BIN)
#    HIGHLIGHT OPTION: color by whether a BIN also appears in the ALL top-25
# =============================================================================
plot_top25_bin <- function(df, title_text, highlight_bins = NULL, show_legend = FALSE) {
  df %>%
    # mutate(): prep aesthetics
    mutate(
      bin = as.character(bin),                      # ensure BIN is character
      evictions_total = as.integer(evictions_total),
      # fct_reorder(): order BIN factor by count so largest appears at top after flip
      bin = fct_reorder(bin, evictions_total)
    ) %>%
    ggplot(aes(
      x = evictions_total,                           # X: counts
      y = bin,                                       # Y: BIN labels
      # fill: TRUE if highlighting and BIN is in the provided list
      fill = if (!is.null(highlight_bins)) bin %in% highlight_bins else FALSE
    )) +
    # geom_col(): bar height = evictions_total
    geom_col() +
    # geom_text(): add count labels at the end of bars
    geom_text(aes(label = comma(evictions_total)), vjust = 0.5, hjust = -0.1, size = 3) +
    # scale_x_continuous(): comma format & right-side space so labels don't clip
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    # scale_fill_manual(): black for highlighted bars, grey otherwise
    scale_fill_manual(
      values = c(`TRUE` = "black", `FALSE` = "grey70"),
      breaks = c(TRUE, FALSE),
      labels = c("Also in ALL top 25", "Not in ALL top 25"),
      name = NULL,
      guide = if (show_legend) "legend" else "none"
    ) +
    labs(title = title_text, x = "Evictions (total)", y = "BIN") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      legend.position = if (show_legend) "bottom" else "none"
    )
}

# =============================================================================
# 6) Build plots (left: ALL; right: RS-only with overlap highlighted)
# =============================================================================
bins_in_all <- top_buildings_all$bin %>% as.character()

p_all <- plot_top25_bin(
  top_buildings_all,
  "Top 25 buildings — ALL",
  highlight_bins = NULL,   # no highlight in ALL panel
  show_legend = FALSE
)

p_rs  <- plot_top25_bin(
  top_buildings_in_rs,
  "Top 25 buildings — INSIDE RS",
  highlight_bins = bins_in_all,  # black if also in ALL
  show_legend = TRUE             # show legend here only
)

# =============================================================================
# 7) Sync X-limits so the two panels share the same scale (fair comparison)
# =============================================================================
max_x <- max(top_buildings_all$evictions_total, top_buildings_in_rs$evictions_total, na.rm = TRUE)
p_all_sync <- p_all + coord_cartesian(xlim = c(0, max_x))
p_rs_sync  <- p_rs  + coord_cartesian(xlim = c(0, max_x))

# =============================================================================
# 8) Side-by-side layout with explanatory subtitle (patchwork)
# =============================================================================
combined <- (p_all_sync | p_rs_sync) +
  plot_annotation(
    title = "Top 25 Buildings by Executed Evictions",
    subtitle = "Right panel: BLACK bars indicate BINs that also appear in the left panel (ALL top 25)."
  )

print(combined)

