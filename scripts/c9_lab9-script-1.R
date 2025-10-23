#c9_lab9-script-1.R

############################################################
# Working with the Time Dimension in R - Lubridate + Forcats
# Files needed in working dir:
#   - "311_Service_Requests_from_2010_to_Present_20251019.csv"
#   - "BUILDING_view_-5690770882456580009.geojson"
############################################################


# --- Setup: packages & data ---------------------------------------------
library(tidyverse)
library(lubridate)
library(forcats)
library(tidyr)
library(gt)
library(scales)
library(stringr)

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab_6", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

DOB_path <- "~/Desktop/lab_9/data/311_Service_Requests_from_2010_to_Present_20251019.csv" # Illegal Conversion of Residential Building/Space
FOOTPRINTS_path <- "~/Desktop/lab_9/data/BUILDING_view_-5690770882456580009.geojson" # NYC Building Footprints

# ========================================================================
# 311 Buildings Complaints for Illegal Conversions
# ========================================================================

# 1) Read in the data set using tidyverse readr
dob <- readr::read_csv(DOB_path, show_col_types = FALSE)

# 2) Quick structure
glimpse(dob)

# 3) Columns that look like time/date fields (by name) - option 1
time_cols <- names(dob) %>%
  str_subset(regex("(date|time)", ignore_case = TRUE))

print(time_cols)

# 4) Columns that look like time/date fields (by name) - option 2
message("Likely time/date columns: ", paste(time_cols, collapse = ", "))
dob %>%
  select(all_of(time_cols)) %>%
  summarise(across(everything(), ~ paste(head(unique(.x), 3), collapse = " | "))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "examples") %>%
  arrange(column) %>%
  print(n = Inf)



# 5) Parser Development:tight detector → checker → explicit-parser flow for dob$Created Date
# --- Helper (detect + quick check) ---
x <- dob$`Created Date`
trial <- suppressWarnings(mdy_hms(head(x[!is.na(x)], 100), tz = "America/New_York"))
message(sprintf("Detected mdy_hms (AM/PM) → parsed %.1f%% of sample",
                mean(!is.na(trial)) * 100))



# 6) Explicit Lubridate parse using mdy_hms
dob <- dob |>
  mutate(
    created_date_raw = `Created Date`,
    created_date     = mdy_hms(`Created Date`, tz = "America/New_York")
  )

# 7) Quick type check
stopifnot("created_date" %in% names(dob), inherits(dob$created_date, "POSIXt"))

# 8) Core summary
dob %>%
  summarise(
    rows        = n(),
    parsed_NA   = sum(is.na(created_date)),
    tz          = attr(created_date, "tzone") %||% Sys.timezone(),
    min_created = suppressWarnings(min(created_date, na.rm = TRUE)),
    max_created = suppressWarnings(max(created_date, na.rm = TRUE))
  ) %>%
  print()


# 9) Year distribution (helps spot parse glitches)
dob %>%
  filter(!is.na(created_date)) %>%
  count(year = year(created_date), name = "n") %>%
  arrange(year) %>%
  print(n = Inf)


# 10) Bounds check (311 data exist since 2010; adjust if needed)
lower <- as.POSIXct("2010-01-01 00:00:00", tz = "America/New_York")
upper <- as.POSIXct(Sys.time(),            tz = "America/New_York")
out_of_bounds <- dob %>%
  filter(!is.na(created_date) & (created_date < lower | created_date > upper))
if (nrow(out_of_bounds) > 0) {
  warning("Found created_date values outside [2010-01-01, now] — showing a few:")
  print(dplyr::slice_head(out_of_bounds, n = 5))
}

# 11) Order results as factors using forcats for months
#Assume dob$created_date is already parsed (POSIXct)
dob_time <- dob %>%
  filter(!is.na(created_date)) %>%
  mutate(
    year  = year(created_date),
    month = month(created_date, label = TRUE, abbr = TRUE),     # ordered factor Jan–Dec
    ym    = floor_date(created_date, "month")                   # Year-month date
  )

# (a) Monthly time series (one value per calendar month)
by_ym <- dob_time %>%
  count(ym, name = "n") %>%
  arrange(ym)

# (b) Month × Year grid (good for faceting or colored lines by year)
by_year_month <- dob_time %>%
  count(year, month, name = "n") %>%
  complete(year, month, fill = list(n = 0)) %>%
  arrange(year, month)


# (c) Seasonality across all years (total by month, Jan–Dec order)
by_month_all <- dob_time %>%
  count(month, name = "n") %>%
  mutate(month = fct_expand(month, levels(month))) %>%
  arrange(month)


# 12) Visualization via ggplot

# Helper for subtitle date range
range_txt <- function(x) {
  rng <- range(x, na.rm = TRUE)
  paste0(format(rng[1], "%b %Y"), "–", format(rng[2], "%b %Y"))
}


# 1) Monthly time series (one point per month)
p_monthly <- ggplot(by_ym, aes(ym, n)) +
  geom_line(linewidth = 0.8) +
  labs(
    title    = "311 ‘Building Construction’ Complaints — Monthly",
    subtitle = range_txt(by_ym$ym),
    x        = NULL,
    y        = "Complaints per month",
    caption  = "Source: NYC Open Data (311 Service Requests, filtered; time = Created Date)"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_monthly


# 2) Month profile per year (lines colored by year)
p_by_year <- ggplot(by_year_month, aes(month, n, group = year, color = factor(year))) +
  geom_line(linewidth = 0.8) +
  labs(
    title    = "Seasonality by Year — 311 ‘Building Construction’ Complaints",
    subtitle = "Calendar months (Jan–Dec) compared across years",
    x        = NULL,
    y        = "Complaints",
    color    = "Year",
    caption  = "Source: NYC Open Data (311 Service Requests)"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal(base_size = 12)

p_by_year


# 2a) Facet (small multiples by year)
p_by_year_facet <- ggplot(by_year_month, aes(month, n, group = 1)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(year), ncol = 4) +
  labs(
    title    = "Seasonality (Small Multiples) — 311 ‘Building Construction’ Complaints",
    subtitle = "Each panel shows Jan–Dec pattern for a given year",
    x        = NULL, y = "Complaints",
    caption  = "Source: NYC Open Data (311 Service Requests)"
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  theme_minimal(base_size = 12)


p_by_year_facet


# 3) Total seasonality across all years (bar chart)
p_month_profile <- ggplot(by_month_all, aes(month, n)) +
  geom_col() +
  labs(
    title    = "Seasonality (All Years Combined) — 311 ‘Building Construction’ Complaints",
    subtitle = "Total complaints by calendar month",
    x        = NULL,
    y        = "Total complaints",
    caption  = "Source: NYC Open Data (311 Service Requests)"
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  theme_minimal(base_size = 12)

p_month_profile


# ========================================================================
# NYC Buildings Footprints (Construction Year Plotting and Mapping)
# ========================================================================

#install.packages("sf")
#add `sf` for spatial
library(sf)
# Verify
packageVersion("sf")
sf:::sf_extSoftVersion()   # shows GEOS/GDAL/PROJ versions sf is using(need 1.0.21)

#1) Read GeoJSON as sf ------------------------------------------------
fp_path <- "~/Desktop/lab_9/data/BUILDING_view_-5690770882456580009.geojson"
stopifnot(file.exists(fp_path))

footprints <- st_read(fp_path, quiet = TRUE, options = "FLATTEN_NESTED_ATTRIBUTES=YES")

#2) Quick structure ------------------------------------------------
glimpse(footprints)

#3) Columns that look like time/date/year fields (by name)
time_cols <- names(footprints) %>%
  str_subset(regex("(date|time|year)", ignore_case = TRUE))

print(time_cols)


#4) Subset to Manhattan by BBL starting with "1" ---------------------

# Find the BBL column — prefer MAPPLUTO_BBL, otherwise fall back to any *bbl* field
bbl_col <- names(footprints) %>%
  # priority 1: exact MAPPLUTO_BBL (case-insensitive)
  { .[str_detect(., regex("^MAPPLUTO[_]?BBL$", ignore_case = TRUE))] } %>%
  { if (length(.) == 0) names(footprints)[str_detect(names(footprints),
                                                     regex("\\bMAPPLUTO[_]?BBL\\b|^BBL$|\\bBBL\\b", ignore_case = TRUE))] else . } %>%
  first()

if (is.na(bbl_col)) {
  stop(
    "Couldn't find a BBL-like column (looked for MAPPLUTO_BBL / BBL). Columns are:\n",
    paste(names(footprints), collapse = ", ")
  )
}

# Make a clean character BBL (handles numeric/scientific) and filter Manhattan (starts with "1")
footprints <- footprints %>%
  mutate(
    BBL_chr = case_when(
      is.numeric(.data[[bbl_col]]) ~ format(.data[[bbl_col]], scientific = FALSE, trim = TRUE),
      TRUE                         ~ as.character(.data[[bbl_col]])
    ) |> str_trim()
  )

# Keep only BBLs that *start with* "1" (Manhattan)
manhattan_fp <- footprints %>% filter(str_starts(BBL_chr, "1"))


# 5) set to 2263 and clean up geometries -----------------------------------------------

# a) Keep everything on GEOS (turn off s2 for the session)
sf_use_s2(FALSE)
fp <- manhattan_fp
fp <- st_transform(fp, 2263)
# b) Quick GEOS repair pass
fp <- fp |>
  st_make_valid() |>      # fix self-intersections / ring direction
  st_buffer(0)            # clean dangling bits / duplicate vertices

# c) Drop any stubborn invalid features (last-resort; keeps code simple)
bad <- !st_is_valid(fp)
if (any(bad)) {
  message("Dropping ", sum(bad), " invalid feature(s) that resisted repair.")
  fp <- fp[!bad, ]
}
# d) (Optional) simplify gently for speed (tolerance in feet)
tol_ft <- 3
fp <- st_simplify(fp, dTolerance = tol_ft, preserveTopology = TRUE)


# 6) Set Time Bounds ------------------------------------------------
# Bounds you consider "reasonable" for construction years
lower_ok <- 1500L
upper_ok <- year(Sys.Date()) + 1L

# 7) Drop out NA and zeros based on metadata; pars the YEAR with lubridate
fp <- fp %>%
  mutate(
    # keep original
    construction_year_raw = `CONSTRUCTION_YEAR`,
    # parse to integer; blanks/NA -> NA; zeros -> NA
    construction_year = readr::parse_integer(as.character(`CONSTRUCTION_YEAR`), na = c("", "NA")),
    construction_year = if_else(construction_year == 0L, NA_integer_, construction_year),
    # drop out-of-bounds years
    construction_year = if_else(
      construction_year < lower_ok | construction_year > upper_ok,
      NA_integer_, construction_year
    ),
    # make a Date at Jan 1 of that year (lubridate)
    construction_date = make_date(year = construction_year, month = 1, day = 1)
  )


# 8) Yearly summary via forcats with a levels argument
by_year <- fp %>%
  filter(!is.na(construction_year)) %>%
  count(construction_year, name = "n") %>%
  arrange(construction_year) %>%
  mutate(
    year_f = factor(
      construction_year,
      levels = construction_year,           # chronological order
      ordered = TRUE
    )
  )

# quick check
print(head(by_year, 10))
print(tail(by_year, 10))

# 9) Yearly summary via forcats with a levels argument
by_decade <- fp %>%
  filter(!is.na(construction_year)) %>%
  mutate(
    decade        = (construction_year %/% 10) * 10,       # 1994 -> 1990
    decade_label  = paste0(decade, "s")
  ) %>%
  count(decade, decade_label, name = "n") %>%
  arrange(decade) %>%
  mutate(
    decade_f = factor(
      decade_label,
      levels = decade_label,               # chronological order
      ordered = TRUE
    )
  )

# quick check
print(by_decade)


#10) Visualization via ggplot + Mapping ------------------------------------------------

options(scipen = 999)        #fixed/decimal notation

# A) Bars by YEAR (ordered)
ggplot(by_year, aes(year_f, n)) +
  geom_col() +
  labs(
    title = "Buildings by Construction Year",
    x = NULL, y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# B) Bars by DECADE (ordered)
ggplot(by_decade, aes(decade_f, n)) +
  geom_col() +
  labs(
    title = "Buildings by Decade of Construction",
    x = NULL, y = "Count"
  ) +
  theme_minimal(base_size = 12)

# Pre-war vs Post-war classification -------------------------------
fp <- fp %>%
  mutate(
    era = case_when(
      !is.na(construction_year) & construction_year < 1940L ~ "Pre-war",
      !is.na(construction_year) & construction_year >= 1940L ~ "Post-war",
      TRUE ~ NA_character_
    ),
    era = factor(era, levels = c("Pre-war", "Post-war"), ordered = TRUE)
  )

# C) Plot (orange = pre-war, blue = post-war)
ggplot(filter(fp, !is.na(era))) +
  geom_sf(aes(fill = era), color = NA, linewidth = 0) +
  scale_fill_manual(values = c("Pre-war" = "orange", "Post-war" = "steelblue"), drop = FALSE) +
  labs(
    title    = "NYC Building Footprints — Manhattan (EPSG:2263)",
    subtitle = paste0("Pre-war (< 1940) vs Post-war (≥ 1940); simplified at ~", tol_ft, " ft"),
    fill     = "Era",
    caption  = "Source: NYC Building Footprints (GeoJSON)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.title = element_blank())



############################################################
# END - Class 9 Lab Experiments for the Time Dimension
###########################################################