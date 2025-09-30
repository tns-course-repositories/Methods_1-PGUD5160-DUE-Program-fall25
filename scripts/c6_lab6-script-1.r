#c6_lab6-script-1.r
#lat/lon reading>plotting with sf package

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab_6", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

# ---- Packages ----
library(sf)
library(tidyverse)

# ---- 1) Read points CSV ----
# Replace with your path to ~/.csv
pts_raw <- readr::read_csv("~/Desktop/lab_6/data/311_Service_Requests_from_2010_to_Present_20250929.csv", show_col_types = FALSE) |>
  filter(!is.na(Longitude), !is.na(Latitude))

# ---- 2) Convert to sf in WGS84 (EPSG:4326) ----
pts_wgs84 <- st_as_sf(
  pts_raw,
  coords = c("Longitude", "Latitude"),
  crs    = 4326,
  remove = FALSE
)

# ---- 3) Reproject to NYC planar feet (EPSG:2263) ----
pts_2263 <- st_transform(pts_wgs84, 2263)

# ---- 4) Read borough boundaries ----
# Replace with your path to nybb.shp
boro_path <- "~/Desktop/lab_6/data/nybb_25c/nybb.shp"
boroughs <- st_read(boro_path, quiet = TRUE)

# ---- 5) Align CRS of boroughs to be consistent with points
boroughs_2263 <- st_transform(boroughs, 2263)

# ---- 6) Plot: EPSG 2263 with borough labels ----
p_2263 <- ggplot() +
  geom_sf(data = boroughs_2263, fill = "grey95", color = "black") +
  geom_sf_text(data = boroughs_2263, aes(label = BoroName),
               color = "black", size = 3, fontface = "bold") +
  geom_sf(data = pts_2263, color = "blue", size = 1.0) +
  theme_minimal() +
  labs(
    title = "Points in EPSG:2263 (NAD83 / NY Long Island ftUS)",
    subtitle = "Projected map with Boroughs"
  )

# ---- 7) Print Plot ----
print(p_2263)

