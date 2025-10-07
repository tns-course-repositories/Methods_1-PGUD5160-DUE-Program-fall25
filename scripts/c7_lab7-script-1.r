#c7_lab7-script-1.r
# Opportunity Zones + ATM Locations

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab7", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

# ---- Packages ----
# install.packages(c("sf", "tmap", "tidyverse")) #as needed

library(tidyverse)
library(sf)
library(tmap)

# ---- Path to your GeoJSON (edit this) ----
geojson_path <- "~Desktop/lab_7/data/export.geojson"

# ---- Read GeoJSON with sf ----
# sf::read_sf() auto-detects driver; stringsAsFactors = FALSE by default
points_sf <- read_sf(geojson_path)

# print to Console - #, Type and CRS
message("Features: ", nrow(points_sf))
message("Geometry type(s): ", paste(unique(st_geometry_type(points_sf)), collapse = ", "))
print(st_crs(points_sf))


# ---- Static map (quick ggplot look) ----
ggplot(points_sf) +
  geom_sf(color = "#1f78b4", size = 2) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Point Features from GeoJSON",
    subtitle = paste0("CRS: ", st_crs(points_sf)$input %||% st_crs(points_sf)$wkt)
  ) +
  theme_minimal(base_size = 12)

# ---- Interactive map (tmap) ----
tmap_mode("view")

tm_shape(points_sf) +
  tm_basemap("OpenStreetMap") +
  tm_dots(
    col = "#3182bd",
    size = 1.5,
    border.lwd = 0,
    popup.vars = names(points_sf)[!names(points_sf) %in% attr(points_sf, "sf_column")]
  ) +
  tm_view(set.zoom.limits = c(3, 19))

# ---- Read in NYC Borough Boundaries + Plot ----

shp_path <- "~Desktop/lab_7/data/nybb_25c/nybb.shp"  # <-- edit to your .shp path

boroughs <- st_read(shp_path, quiet = TRUE)

# Inspect CRS & geometry
message("CRS: ", st_crs(boroughs)$input %||% st_crs(boroughs)$wkt)
message("Geometry types: ", paste(unique(st_geometry_type(boroughs)), collapse = ", "))

# force topologically valid geometries
boroughs_valid <- st_make_valid(boroughs)

# ---- Dissolve all borough features into one polygon ----

nyc_union <- boroughs_valid |>
  summarise(geometry = st_union(geometry), .groups = "drop")

# ---- Plot the dissolved polygon ----
ggplot(nyc_union) +
  geom_sf(fill = "#3182bd", color = "#08519c", linewidth = 0.4) +
  coord_sf(expand = FALSE) +
  labs(title = "NYC Boroughs — Dissolved Boundary",
       subtitle = paste0("CRS: ", st_crs(nyc_union)$input %||% st_crs(nyc_union)$wkt)) +
  theme_minimal(base_size = 12)

# ---- Read in Opportunity Zones  + Plot ----

shp_path <- "~Desktop/lab_7/data/opportunity-zones=8764.-9-10-2019/8764oz.shp"  # <-- edit to your .shp path

oz <- st_read(shp_path, quiet = TRUE)

# Inspect CRS & geometry
message("Features: ", nrow(oz))
message("Geometry type(s): ", paste(unique(st_geometry_type(oz)), collapse = ", "))
print(st_crs(oz))

# Large File; first filter for just New York State 
oz_ny <- oz %>%
  filter(STATENAME == "New York")

# Quick check
message("Number of New York Opportunity Zones: ", nrow(oz_ny))


# ---- Dissolve all NY features into one polygon ----
oz_ny_union <- oz_ny %>%
  summarise(geometry = st_union(geometry), .groups = "drop")


# ---- Plot Opportunity Zones ----
ggplot(oz_ny_union) +
  geom_sf(fill = "#3182bd", color = "#08519c", linewidth = 0.2, alpha = 0.6) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Opportunity Zones | New York State",
    subtitle = paste0("CRS: ", st_crs(oz_ny_union)$input %||% st_crs(oz_ny_union)$wkt)
  ) +
  theme_minimal(base_size = 12)

# ---- Create Internal NYC Opportunity Zone from NYS Zones ----

# ---- Harmonize CRS between NYC Borough Dissolve & Opportunity Zone Dissolve----
if (st_crs(nyc_union) != st_crs(oz_ny_union)) {
  oz_ny_union <- st_transform(oz_ny_union, st_crs(nyc_union))
}

# ---- Clean & set precision (helps avoid slivers) ----
nyc_union  <- nyc_union |> st_make_valid() |> st_set_precision(1e5) |> st_make_valid()
oz_ny_union <- oz_ny_union |> st_make_valid() |> st_set_precision(1e5) |> st_make_valid()

# ---- Clip OZ Dissolve to the Borough Boundary Dissolve ----
oz_in_borough <- st_intersection(oz_ny_union, nyc_union) |> st_make_valid()

# If there is no overlap, guard against empties- safety check
if (nrow(oz_in_borough) == 0 || all(st_is_empty(oz_in_borough))) {
  stop("No Opportunity Zone area overlaps the dissolved borough boundary.")
}

# ---- Partition the borough: inside OZ vs outside OZ ----
borough_outside_oz <- st_difference(nyc_union, oz_in_borough) |> st_make_valid()

# Ensure polygonal outputs (strip any stray non-polygons)
oz_in_borough      <- st_collection_extract(oz_in_borough, "POLYGON")
borough_outside_oz <- st_collection_extract(borough_outside_oz, "POLYGON")

# ---- Assemble two-class sf ----
inside_sf  <- oz_in_borough      |> mutate(zone_partition = "Inside_OZ")
outside_sf <- borough_outside_oz |> mutate(zone_partition = "Outside_OZ")

borough_partition <- bind_rows(inside_sf, outside_sf) |>
  mutate(zone_partition = factor(zone_partition, levels = c("Outside_OZ", "Inside_OZ")))

# ---- Check CRS ----
print(st_crs(borough_partition))


# ---- Plot: borough partition (inside vs outside OZ) ----
ggplot() +
  geom_sf(data = borough_partition, aes(fill = zone_partition), color = "grey25", linewidth = 0.3, alpha = 0.8) +
  geom_sf(data = boroughs_valid, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(expand = FALSE) +
  labs(
    title = "NYC Borough Boundary Partitioned by Opportunity Zones",
    subtitle = paste0("CRS: ", st_crs(borough_partition)$input %||% st_crs(borough_partition)$wkt),
    fill = "Partition"
  ) +
  theme_minimal(base_size = 12)

# ---- Compute areas (ft^2 -> acres) ----
FEET_PER_SQ_MILE <-  27878400

borough_partition_area <- borough_partition %>%
  mutate(
    area_ft2 = as.numeric(st_area(geometry)),     # st_area returns units; coerce to numeric ft^2
    sq_mile  = area_ft2 / FEET_PER_SQ_MILE          # total square miles per partition
  )

# ---- View Result in Console ---
borough_partition_area %>%
  st_drop_geometry() %>%
  select(zone_partition, area_ft2, sq_mile) %>%
  arrange(zone_partition) %>%
  glimpse()

# ---- Reproject ATM points to 2263 ---
points_sf_2263 <- points_sf %>%
  st_make_valid() %>%
  st_transform(2263)

# Inspect CRS
message("CRS: ", st_crs(points_sf_2263)$input %||% st_crs(points_sf_2263)$wkt)

# ---- Count points within each partition ----
# Use st_join with st_within (strictly inside). For boundary-inclusive, use st_intersects.
pt_counts <- points_sf_2263 %>%
  st_join(borough_partition_area %>% select(zone_partition), join = st_within, left = FALSE) %>%
  st_drop_geometry() %>%
  count(zone_partition, name = "NUMPOINTS")


# ---- Attach counts to polygons; compute RATE (= points per sq mile) ----
borough_partition_area <- borough_partition_area %>%
  left_join(pt_counts, by = "zone_partition") %>%
  mutate(
    NUMPOINTS = replace_na(NUMPOINTS, 0L),
    RATE      = NUMPOINTS / sq_mile
  )

borough_partition_area %>%
  st_drop_geometry() %>%
  select(zone_partition, NUMPOINTS, sq_mile, RATE) %>%
  arrange(zone_partition) %>%
  glimpse()


# ---- Clip points to borough boundary ----
# st_intersection() keeps only points that fall within the polygon
clipped_points <- st_intersection(points_sf_2263, borough_partition)


# ---- Plot: borough partition (inside vs outside OZ) ----
ggplot() +
  # filled borough partition (two zones)
  geom_sf(
    data = borough_partition,
    aes(fill = zone_partition),
    color = "grey25",
    linewidth = 0.3,
    alpha = 0.8
  ) +
  # borough boundary outline
  geom_sf(
    data = boroughs_valid,
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  # point layer on top (small black dots)
  geom_sf(
    data = clipped_points,
    color = "black",
    size = 0.6,
    alpha = 0.9
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "NYC Borough Boundary Partitioned by Opportunity Zones",
    subtitle = paste0("CRS: ", st_crs(borough_partition)$input %||% st_crs(borough_partition)$wkt),
    fill = "Partition"
  ) +
  theme_minimal(base_size = 12)

# ---- 3) tmap v4 interactive view ----

# Ensure zone_partition is a factor for consistent coloring
bp_viz <- borough_partition
if (!is.factor(bp_viz$zone_partition)) {
  bp_viz$zone_partition <- factor(bp_viz$zone_partition,
                                  levels = c("Inside_OZ", "Outside_OZ"))
}

tmap_mode("view")

tm_basemap("OpenStreetMap") +
  tm_shape(bp_viz) +
  tm_polygons(
    fill        = "zone_partition",
    col         = "grey25",         # outline color
    lwd         = 0.3,
    fill_alpha  = 0.8,
    fill.scale  = tm_scale(values = c(
      "Inside_OZ"  = "#3182bd",     # blue
      "Outside_OZ" = "#f16913"      # orange
    )),
    fill.legend = tm_legend(title = "Partition")
  ) +
  tm_shape(clipped_points) +
  tm_dots(size = 0.6, fill = "black", col = NA, fill_alpha = 0.9) +
  tm_view(set_zoom_limits = c(8, 19)) +
  tm_layout(
    main.title = "NYC Borough Boundary Partitioned by Opportunity Zones",
    main.title.size = 1.1
  )


