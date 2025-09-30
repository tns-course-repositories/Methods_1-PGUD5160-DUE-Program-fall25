#c6_assignment6-script-2.r
# Creating a frequency table + map for land use in NYC PUMA

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab_6", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

#Install stringr package as needed
#install.packages("stringr")

# ---- Packages ----
library(sf)
library(tidyverse)
library(stringr)

# ---- Paths ----
pluto_path <- "~/Desktop/assignment_6/data/nyc_mappluto_25v2_1_shp/MapPLUTO.shp"
puma_path  <- "~/Desktop/assignment_6/data/nypuma2020_25c/nypuma2020.shp"

# ---- Read ----
pluto <- st_read(pluto_path, quiet = TRUE) %>% st_make_valid()
pumas <- st_read(puma_path,  quiet = TRUE) %>% st_make_valid()

# ---- Print CRSs BEFORE alignment ----
st_crs(pluto)
st_crs(pumas)

# ---- Align CRS: transform PUMAs to PLUTO's CRS ----
pumas <- st_transform(pumas, st_crs(pluto))

# ---- Select target PUMA (4103 is the example PUMA here) ----
puma_4103 <- pumas %>% filter(PUMA == 4103)

# ---- (Speed) Crop PLUTO to PUMA bbox BEFORE spatial filter ----
pluto_crop <- st_crop(pluto, st_bbox(puma_4103))

# ---- Keep only parcels that actually intersect PUMA 4103 ----
pluto_4103 <- st_filter(pluto_crop, puma_4103, .predicate = st_intersects) %>%
  filter(!st_is_empty(geometry))

# ---- Quick counts ----
message("Parcels in cropped bbox: ", nrow(pluto_crop))
message("Parcels intersecting PUMA 4103: ", nrow(pluto_4103))

# ---- Compute bbox for coord_sf ----
bb <- st_bbox(puma_4103)
print(bb)

# ---- Plot: ONLY intersecting parcels + PUMA outline; extent snapped to PUMA bbox ----
ggplot() +
  geom_sf(data = pluto_4103, fill = NA, color = "steelblue", linewidth = 0.15) +
  geom_sf(data = st_boundary(puma_4103), color = "black", linewidth = 0.4) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title    = "MapPLUTO Parcels Intersecting PUMA 4103 (MN03)",
    subtitle = "Intersecting parcels (blue) with PUMA 4103 outline (black)",
    caption  = "Sources: NYC DCP MapPLUTO; NYC PUMAs (2020)"
  )

# --- NYC DCP Area Map Standard: colors + labels ---
lu_cols <- c(
  `01` = "#FFFFB3",  # One & Two Family Buildings
  `02` = "#FFEB8A",  # Multi-Family Walkup
  `03` = "#FFC400",  # Multi-Family Elevator
  `04` = "#FF9966",  # Mixed Commercial/Residential
  `05` = "#E83333",  # Commercial/Office
  `06` = "#AB59C9",  # Industrial/Manufacturing
  `07` = "#FFCCFF",  # Transportation/Utility
  `08` = "#2E6DFF",  # Public Facilities & Institutions
  `09` = "#99FF66",  # Open Space
  `10` = "#AAAAAA",  # Parking
  `11` = "#2E2E2E",  # Vacant Land
  `Other / No Data` = "#EBEBEB"
)

lu_labels <- c(
  `01` = "One & Two Family Buildings",
  `02` = "Multi-Family Walkup",
  `03` = "Multi-Family Elevator",
  `04` = "Mixed Commercial/Residential",
  `05` = "Commercial/Office",
  `06` = "Industrial/Manufacturing",
  `07` = "Transportation/Utility",
  `08` = "Public Facilities & Institutions",
  `09` = "Open Space",
  `10` = "Parking",
  `11` = "Vacant Land",
  `Other / No Data` = "Other / No Data"
)

# --- Normalize PLUTO LandUse to two-digit codes; bucket others ---
pluto_4103 <- pluto_4103 |>
  mutate(
    LandUse_raw  = as.character(LandUse),
    LandUse_num  = suppressWarnings(as.integer(str_trim(LandUse_raw))),
    LandUse_code = ifelse(!is.na(LandUse_num) & dplyr::between(LandUse_num, 1, 11),
                          str_pad(LandUse_num, width = 2, pad = "0"),
                          "Other / No Data"),
    LandUse_code = factor(LandUse_code, levels = names(lu_labels))
  )

# --- Frequency table (counts + percents) using the same order as legend ---
freq_tbl <- pluto_4103 |>
  st_drop_geometry() |>
  count(LandUse_code, .drop = FALSE, name = "n") |>
  mutate(
    label   = lu_labels[as.character(LandUse_code)],
    percent = round(100 * n / sum(n), 2)
  ) |>
  select(LandUse_code, label, n, percent)

# Print to console (tibble)
print(freq_tbl)

# nicer table with gt
library(gt)
freq_tbl |>
   gt(rowname_col = "LandUse_code") |>
   tab_header(title = md("**PUMA 4103 — Parcels by Land Use (PLUTO)**")) |>
   cols_label(
     label = "Category",
     n = "Count",
    percent = "Percent"
   )


# save your resulting freq_tbl as an RDS object
saveRDS(freq_tbl, file = "~/Desktop/assignment_6/results/freq_tbl.rds")

# --- Tight extent to PUMA bbox ---
bb <- st_bbox(puma_4103)

# --- Plot parcels by LandUse (opaque fills, ordered legend) ---
landuse_plot <- ggplot() +
  geom_sf(data = pluto_4103, aes(fill = LandUse_code), color = NA) +
  geom_sf(data = st_boundary(puma_4103), color = "black", linewidth = 0.5) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    name   = "Land Use (PLUTO)",
    values = lu_cols,
    breaks = names(lu_labels),   # 01–11 then Other / No Data
    labels = lu_labels,
    drop   = FALSE
  ) +
  theme_minimal() +
  labs(
    title    = "PUMA 4103 (MN03): MapPLUTO Parcels by Land Use",
    subtitle = "Legend ordered 01–11 + Other / No Data",
    caption  = "Sources: NYC DCP MapPLUTO; NYC PUMAs (2020)"
  )

print(landuse_plot)

# save your resulting landuse_plot as an RDS object
saveRDS(landuse_plot, file = "~/Desktop/assignment_6/results/landuse_plot.rds")
