############################################################
# NYC Evictions — Full-Span EDA + Leaflet Hex Map
# Assignment 8 - c8_assignment8-script-1.R (annotated + WHY IMPORTANT)
############################################################

# ---- Packages (quiet) ------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)  # dplyr (mutate/filter/count/arrange/slice_*), tidyr, readr, ggplot2
  library(lubridate)  # date parsing & floor_date()
  library(sf)         # spatial data frames & operations
  library(ggplot2)    # plotting
  library(scales)     # axis/label helpers like comma()
  library(leaflet)    # interactive maps
  library(classInt)   # data-driven class breaks (Fisher–Jenks/quantile)
  library(forcats)    # factor helpers (fct_reorder)
})

# =============================================================================
# HARD PATHS (edit to your machine)
# WHY IMPORTANT: Centralizes file locations so students change paths in one place.
# =============================================================================
ev_rds_path    <- "~/Desktop/c8_assignment/data/ev.rds"                    # REQUIRED: evictions .rds
rs_parcels_shp <- "~/Desktop/c8_assignment/data/joined_rs/joined_rs.shp"   # OPTIONAL: RS parcels outline

# =============================================================================
# HELPERS — legend bins + RS dissolve
# WHY IMPORTANT: Reusable, tested utilities prevent copy/paste bugs and keep code DRY.
# =============================================================================

# make_bins(): compute ordered class breaks for choropleths
# WHY IMPORTANT: Consistent, monotone bins → legible legend; Fisher–Jenks emphasizes natural groupings
make_bins <- function(x, k = 6) {
  x <- x[is.finite(x) & x >= 0]
  if (length(unique(x)) <= 2) return(c(0, 1, Inf))
  ci <- try(classInt::classIntervals(x, n = k, style = "fisher"), silent = TRUE)  # Fisher–Jenks
  if (inherits(ci, "try-error") || anyDuplicated(ci$brks))
    ci <- try(classInt::classIntervals(x, n = k, style = "quantile"), silent = TRUE)  # fallback
  brks <- if (!inherits(ci, "try-error")) ci$brks else pretty(range(x, finite = TRUE), n = k)
  brks <- sort(unique(c(0, brks)))
  if (!is.infinite(tail(brks, 1))) brks[length(brks)] <- Inf
  if (length(brks) < 3) brks <- c(0, 1, Inf)
  brks
}

# labels_for_bins(): readable labels (e.g., "1–5", "6–10", "11+")
# WHY IMPORTANT: Human-friendly bins avoid cognitive load when interpreting legends.
labels_for_bins <- function(b) {
  labs <- character(length(b) - 1)
  for (i in seq_len(length(labs))) {
    a <- b[i]; z <- b[i + 1]
    lower <- if (a <= 0) "0" else format(a, big.mark = ",", trim = TRUE)
    labs[i] <- if (is.infinite(z)) paste0(lower, "+")
    else paste0(lower, "–", format(z, big.mark = ",", trim = TRUE))
  }
  labs
}

# build_rs_outline_safe(): dissolve RS polygons safely (planar, s2-off), then to 4326
# WHY IMPORTANT: Many civic polygon datasets contain ring defects; planar dissolve + simplify is robust & fast.
build_rs_outline_safe <- function(rs_sf, simplify_ft = 75) {
  old_s2 <- sf_use_s2(); on.exit(sf_use_s2(old_s2), add = TRUE); sf_use_s2(FALSE)  # planar ops
  
  rs <- rs_sf %>%
    st_transform(2263) %>%   # mutate CRS to NY State Plane (feet)
    st_make_valid() %>%      # fix invalid rings
    st_set_precision(1) %>%  # snap precision → removes micro-slivers
    st_buffer(0)             # heal overlaps/gaps
  
  u  <- try(st_union(rs), silent = TRUE)                     # dissolve to one geometry
  if (inherits(u, "try-error")) return(NULL)
  g  <- try(suppressWarnings(st_collection_extract(u, "POLYGON", warn = FALSE)), silent = TRUE)
  if (inherits(g, "try-error") || length(g) == 0) return(NULL)
  
  g  <- st_sfc(g, crs = 2263) %>%
    st_buffer(0) %>%                                      # heal again post-dissolve
    st_simplify(dTolerance = simplify_ft) %>%             # simplify in FEET (planar)
    st_transform(4326)                                    # to WGS84 for web maps
  
  list(line = st_as_sf(data.frame(layer="RS dissolved (line)"), geometry=st_boundary(g)))
}

# =============================================================================
# 1) LOAD EVICTIONS (RDS) + PARSE DATES
# WHY IMPORTANT: Clean dates are the backbone of time-series EDA (aggregation/filters).
# =============================================================================
stopifnot(file.exists(ev_rds_path))           # fail fast with clear message
ev <- readRDS(ev_rds_path)

stopifnot("Executed Date" %in% names(ev))     # require the exact column (schema check)

ev <- ev %>%
  # mutate(): convert to Date; tolerate character formats safely
  mutate(
    executed_date = if (inherits(`Executed Date`, "Date")) as.Date(`Executed Date`)
    else suppressWarnings(lubridate::mdy(`Executed Date`))  # parse "MM/DD/YYYY"
  ) %>%
  # filter(): drop rows with impossible/NA dates to avoid phantom spikes
  filter(!is.na(executed_date))

# =============================================================================
# 2) FULL-SPAN EDA — MONTHLY TREND
# WHY IMPORTANT: Monthly time-step smooths noise and reveals policy/seasonal shifts.
# =============================================================================
ev_monthly <- ev %>%
  mutate(month = floor_date(executed_date, "month")) %>%  # mutate(): month bucket
  count(month, name = "evictions") %>%                    # count(): tally per bucket
  arrange(month)                                          # arrange(): chronological

ggplot(ev_monthly, aes(month, evictions)) +
  geom_line() +
  labs(title = "Executed Evictions per Month — Full Span", x = NULL, y = "Evictions") +
  theme_minimal() %>%
  print()

# =============================================================================
# EDA — EVICIONS BY NTA (horizontal bars)
# WHY IMPORTANT: Area ranking shows geographic concentration for targeting/briefing.
# =============================================================================
ev_nta <- ev %>%
  filter(!is.na(NTA), NTA != "") %>%          # filter(): remove missing labels
  count(NTA, name = "evictions") %>%          # count(): events per NTA
  arrange(desc(evictions))                    # arrange(): rank high→low

top_n <- 25                                   # show the head for readability
ev_nta_top <- ev_nta %>%
  slice_max(evictions, n = top_n, with_ties = FALSE)  # fastest way to take top N

ggplot(ev_nta_top, aes(x = fct_reorder(NTA, evictions), y = evictions)) +
  geom_col() +
  geom_text(aes(label = comma(evictions)), hjust = -0.15, size = 3) +
  coord_flip(clip = "off") +  # WHY IMPORTANT: horizontal bars improve label legibility
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = paste0("Executed Evictions by NTA — Top ", top_n),
    x = "NTA",
    y = "Evictions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

# =============================================================================
# 3) LEAFLET HEX MAP — ALL EVICITIONS WITH LAT/LON
# WHY IMPORTANT: Hex aggregation is fast, suppresses overplotting, and preserves spatial patterns.
# =============================================================================

suppressPackageStartupMessages({ library(dplyr); library(sf); library(leaflet); library(classInt) })

# ---- 3.1 Fence to NYC-ish bbox and drop (0,0) -----------------------------
# WHY IMPORTANT: Removes obvious geocoding errors so they don't distort the map extent.
lat_col <- intersect(names(ev), c("Latitude","latitude","LATITUDE"))[1]
lon_col <- intersect(names(ev), c("Longitude","longitude","LONGITUDE"))[1]
stopifnot(!is.na(lat_col), !is.na(lon_col))

nyc_lon_range <- c(-74.5, -73.5)   # wide NYC bounds
nyc_lat_range <- c( 40.35, 41.10)

ev_clean <- ev %>%
  filter(
    is.finite(.data[[lat_col]]), is.finite(.data[[lon_col]]),  # filter(): valid numerics
    .data[[lat_col]] != 0, .data[[lon_col]] != 0,              # filter(): drop (0,0)
    dplyr::between(.data[[lon_col]], nyc_lon_range[1], nyc_lon_range[2]),
    dplyr::between(.data[[lat_col]], nyc_lat_range[1], nyc_lat_range[2])
  )
stopifnot(nrow(ev_clean) > 0)  # WHY IMPORTANT: fail fast if bbox removed all points

# ---- 3.2 Points → SF; project to Web Mercator meters (EPSG:3857) ----------
# WHY IMPORTANT: Meters are required to make meaningful hex sizes (e.g., 600 m).
ev_pts_4326 <- st_as_sf(ev_clean, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
ev_pts_3857 <- st_transform(ev_pts_4326, 3857)

# ---- 3.3 Build a hex grid on a buffered convex hull -----------------------
# WHY IMPORTANT: Grid only where points exist → fewer hexes → faster render.
hull_3857 <- st_buffer(st_convex_hull(st_union(ev_pts_3857)), 300)  # small pad to avoid clipping edges
hex_cellsize_m <- 600                                               # knob: ↑ faster, ↓ more detail

hex_3857 <- st_make_grid(hull_3857, cellsize = hex_cellsize_m, square = FALSE) %>%
  st_as_sf() %>%
  rename(geometry = x) %>%              # dplyr::rename(): unify geometry column name
  st_set_crs(3857) %>%
  mutate(hex_id = dplyr::row_number())  # mutate(): stable ID for joins

# ---- 3.4 Assign points to hexes, then count per hex -----------------------
# WHY IMPORTANT: st_within keeps strict containment (no edge ambiguity).
pt_in_hex <- st_join(
  ev_pts_3857,
  hex_3857 %>% select(hex_id),          # select(): light join (only carry ID)
  join = st_within,
  left = FALSE
)

# WHY IMPORTANT: drop geometry for aggregation speed; coalesce NA counts;
#                remove zero-eviction hexes to reduce clutter.
hex_counts <- pt_in_hex %>%
  st_drop_geometry() %>%
  count(hex_id, name = "evictions_sum")

hex_counts_3857 <- hex_3857 %>%
  left_join(hex_counts, by = "hex_id") %>%
  mutate(evictions_sum = dplyr::coalesce(evictions_sum, 0L)) %>%  # NA → 0
  filter(evictions_sum > 0)                                       # keep signal only

# ---- 3.5 Legend bins (robust to duplicates/flat distributions) ------------
# WHY IMPORTANT: Guarantees bins even if distribution is flat or single-valued.
hex_ll <- st_transform(hex_counts_3857, 4326)
vals <- hex_ll$evictions_sum
vals <- vals[is.finite(vals) & vals > 0]

safe_bins <- function(x, k = 6) {
  x <- sort(x); u <- sort(unique(x))
  if (length(x) == 0) stop("No positive counts to bin.")
  if (length(u) == 1) { v <- u[1]; return(c(v - 0.5, v + 0.5, Inf)) }  # single-valued case
  ci <- try(classInt::classIntervals(x, n = k, style = "fisher"), silent = TRUE)
  if (inherits(ci, "try-error") || anyDuplicated(ci$brks)) {
    ci <- try(classInt::classIntervals(x, n = k, style = "quantile"), silent = TRUE)
  }
  brks <- if (!inherits(ci, "try-error")) ci$brks else pretty(range(x), n = k)
  brks <- sort(unique(brks)); brks <- brks[brks > 0]
  if (!is.infinite(tail(brks, 1))) brks[length(brks)] <- Inf
  if (length(brks) < 3) { a <- min(x); b <- max(x); if (a == b) return(c(a - 0.5, a + 0.5, Inf)); brks <- sort(unique(c(a, b, Inf))) }
  brks
}
bins       <- safe_bins(vals, k = 6)
bin_labels <- labels_for_bins(bins)
pal_hex    <- colorBin("YlOrRd", domain = hex_ll$evictions_sum, bins = bins,
                       right = TRUE, pretty = FALSE, na.color = "#cccccc")

# ---- 3.6 OPTIONAL: RS outline (dissolved) if shapefile is present ---------
# WHY IMPORTANT: Adds policy context without heavy polygon rendering.
rs_outline <- NULL
if (exists("rs_parcels_shp") && is.character(rs_parcels_shp) && file.exists(rs_parcels_shp)) {
  rs_parcels <- st_read(rs_parcels_shp, quiet = TRUE) %>% st_make_valid()
  rs_outline <- try(build_rs_outline_safe(rs_parcels, simplify_ft = 75), silent = TRUE)
  if (inherits(rs_outline, "try-error")) rs_outline <- NULL
}

# ---- 3.7 Leaflet map (hexes over RS outline) ------------------------------
# WHY IMPORTANT: Hexes (polygons) are far fewer than points → smooth pan/zoom.
m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron)

if (!is.null(rs_outline) && !inherits(rs_outline, "try-error") &&
    !is.null(rs_outline$line) && inherits(rs_outline$line, "sf") && nrow(rs_outline$line) > 0) {
  m <- m %>%
    addPolylines(data = rs_outline$line, color = "#2b8cbe", weight = 1.1,
                 opacity = 0.9, group = "RS (outline)")
}

m <- m %>%
  addPolygons(
    data = hex_ll,
    fillColor = ~pal_hex(evictions_sum),
    fillOpacity = 0.6,                     # semi-transparent → base/outline visible
    color = "#666666", weight = 0.3, opacity = 0.8,
    label = ~paste0("Evictions (sum): ", evictions_sum),
    highlightOptions = highlightOptions(weight = 1.2, opacity = 0.9, bringToFront = TRUE),
    group = "Evictions (hex)"
  ) %>%
  addLegend(position = "bottomright",
            colors = pal_hex(bins[-length(bins)] + 1e-9),  # epsilon to land inside bin
            labels = bin_labels,
            title = "Evictions (sum)", opacity = 1) %>%
  addLayersControl(
    overlayGroups = c("RS (outline)", "Evictions (hex)"),
    options = layersControlOptions(collapsed = TRUE)
  )

# Render in RStudio Viewer
m
