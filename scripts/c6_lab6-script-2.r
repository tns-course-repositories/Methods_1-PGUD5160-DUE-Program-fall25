#c6_lab6-script-2.r
#CRS Comparison Mapping for NYS

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab_6", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

#Install the patchwork package if not currently installed
#install.packages("patchwork")

# ---- Packages ----
library(sf)
library(tidyverse)
library(tigris)
library(patchwork)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)  # optional: simplifies centroid/buffer behavior for planar ops

# ---- 1) Get New York State polygon from Census ----
ny_4326 <- states(year = 2023, cb = TRUE, progress_bar = FALSE) |>
  filter(STUSPS == "NY") |>
  st_transform(4326)

# ---- 2) Make UTM Zone 18N version (EPSG:32618, WGS84 UTM) ----
ny_utm <- st_transform(ny_4326, 32618)

# ---- 3) Centroid-of-surface + constant-distance buffer in UTM (meters) ----
ny_center_utm <- st_point_on_surface(ny_utm)                  # guaranteed inside the polygon
buf_dist_m     <- 150000                                      # 150 km buffer, adjust to taste
ny_buffer_utm  <- st_buffer(ny_center_utm, dist = buf_dist_m)

# Reproject the same buffer to 4326 for the left map
ny_center_4326 <- st_transform(ny_center_utm, 4326)
ny_buffer_4326 <- st_transform(ny_buffer_utm, 4326)

# ---- 4) Plot: EPSG:4326 (degrees) ----
p_deg <- ggplot() +
  geom_sf(data = ny_4326, fill = "grey95", color = "black") +
  geom_sf(data = ny_buffer_4326, fill = NA, color = "purple", linewidth = 1) +
  geom_sf(data = ny_center_4326, color = "purple", size = 2) +
  coord_sf(expand = FALSE) +  # shows lon/lat ticks & graticules
  theme_minimal() +
  labs(
    title = "New York State — EPSG:4326 (WGS84)",
    subtitle = paste0("Axes in degrees; buffer is the reprojected 150 km circle"),
    x = "Longitude (°)", y = "Latitude (°)"
  )

# ---- 5) Plot: UTM Zone 18N (meters) ----
p_utm <- ggplot() +
  geom_sf(data = ny_utm, fill = "grey95", color = "black") +
  geom_sf(data = ny_buffer_utm, fill = NA, color = "purple", linewidth = 1) +
  geom_sf(data = ny_center_utm, color = "purple", size = 2) +
  coord_sf(expand = FALSE) +  # shows projected meters
  theme_minimal() +
  labs(
    title = "New York State — EPSG:32618 (UTM Zone 18N, meters)",
    subtitle = paste0("Axes in meters; buffer is a true circle of ", format(buf_dist_m, big.mark=","), " m"),
    x = "Easting (m)", y = "Northing (m)"
  )

# ---- 6) Side-by-side comparison ----
p_deg + p_utm
