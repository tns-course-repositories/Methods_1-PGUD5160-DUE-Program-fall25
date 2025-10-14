#c8_lab8-script-1.R
############################################################
# NYC Evictions + Rent-Stabilized (RS) Experiments (11)
# Files needed in working dir:
#   - "Evictions_20251012.csv"
#   - "ny_likely_rs_2020.csv"
############################################################

# --- Setup: packages & data ---------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(tidyr)
  library(gt)
  library(scales)
  library(stringr)
})

#if any packages missing
#install.packages("tidyverse","lubridate","tidyr","gt","scales","stringr"")

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab_6", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

ev_path <- "~/Desktop/lab_8/data/Evictions_20251012.csv" # Evictions Data Path
rs_path <- "~/Desktop/lab_8/data/ny_likely_rs_2020.csv" # Rent Stabilized Data Path

# Quiet dplyr summarize notes + default ggplot theme
options(dplyr.summarise.inform = FALSE); try({ggplot2::theme_set(ggplot2::theme_minimal())}, silent = TRUE)

# (Optional) one-liner to install+load packages
# pkgs <- c("tidyverse","lubridate","tidyr","gt","scales","stringr"); miss <- setdiff(pkgs, rownames(installed.packages())); if(length(miss)) install.packages(miss); suppressPackageStartupMessages(invisible(lapply(pkgs, function(p) library(p, character.only = TRUE))))

# -------------------------------------------------------------------------
# Read Evictions — MINIMAL: use exact column names (`Executed Date`, `BBL`, `BOROUGH`)
# -------------------------------------------------------------------------
ev <- readr::read_csv(ev_path, show_col_types = FALSE) %>%
  mutate(
    executed_date = suppressWarnings(lubridate::mdy(`Executed Date`)),  # parse date
    bbl           = as.character(`BBL`),                                 # keep leading zeros
    BOROUGH       = toupper(`BOROUGH`)                                    # standardize to UPPER
  ) %>%
  filter(!is.na(executed_date))

# -------------------------------------------------------------------------
# Read RS — normalize to lower_snake; keep bbl as character
# -------------------------------------------------------------------------
rs <- readr::read_csv(rs_path, show_col_types = FALSE) %>%
  rename_with(~ stringr::str_to_lower(gsub("\\s+", "_", .x))) %>%
  mutate(
    bbl      = as.character(bbl),
    borough  = stringr::str_to_title(borough),
    unitsres = suppressWarnings(as.numeric(unitsres))
  )

# Window for “last 52 weeks of evictions data (ev)"
anchor <- max(ev$executed_date, na.rm = TRUE)
start  <- anchor - weeks(52) + days(1)

# ---------- Helpers (define once, used by Experiments 5–11) ----------
# Map BBL -> uppercase BOROUGH (NYC code in first digit)
borough_from_bbl <- function(bbl_chr) {
  d <- substr(trimws(as.character(bbl_chr)), 1, 1)
  dplyr::case_when(
    d == "1" ~ "MANHATTAN",
    d == "2" ~ "BRONX",
    d == "3" ~ "BROOKLYN",
    d == "4" ~ "QUEENS",
    d == "5" ~ "STATEN ISLAND",
    TRUE     ~ NA_character_
  )
}

# Deduplicate RS to one row per BBL to avoid join fan-out
make_rs_key <- function(rs_tbl) {
  rs_tbl %>%
    arrange(bbl) %>%
    distinct(bbl, .keep_all = TRUE)
}

# RS units by BOROUGH derived from BBL (robust keying)
rs_units_by_borough <- function(rs_key_tbl) {
  rs_key_tbl %>%
    mutate(BOROUGH = borough_from_bbl(bbl)) %>%
    group_by(BOROUGH) %>%
    summarize(units = sum(unitsres, na.rm = TRUE), .groups = "drop")
}

# Event-level join (keep every eviction row; add RS columns without duplication)
join_rs_events <- function(ev_tbl, rs_key_tbl) {
  ev_tbl %>%
    filter(!is.na(bbl)) %>%
    inner_join(
      rs_key_tbl %>% select(bbl, address, zipcode, unitsres, ownername),
      by = "bbl"
    )
}

# Pretty GT printer
show_gt <- function(x, title = NULL) {
  x %>%
    gt::gt() %>%
    gt::tab_header(title = if (is.null(title)) NULL else gt::md(paste0("**", title, "**"))) %>%
    gt::tab_options(table.width = gt::pct(100), data_row.padding = gt::px(6), table.font.size = gt::px(14)) %>%
    print()
}

# ========================================================================
# EXPERIMENT 1 — Weekly evictions (last 52 weeks)
# ========================================================================
ev_weekly_last_year <- ev %>%
  filter(executed_date >= start, executed_date <= anchor) %>%
  mutate(week = floor_date(executed_date, "week", week_start = 1)) %>%
  count(week, name = "evictions") %>%
  complete(week = seq(min(week), max(week), by = "1 week"),
           fill  = list(evictions = 0)) %>%
  arrange(week)

print(head(ev_weekly_last_year, 8))
if (interactive()) {
  ev_weekly_last_year %>%
    ggplot(aes(week, evictions)) +
    geom_line() +
    labs(title = "Evictions per Week — Last 52 Weeks", x = NULL, y = "Evictions") +
    theme_minimal() %>%
    print()
}

# ========================================================================
# EXPERIMENT 2 — Weekly evictions by BOROUGH (last 52 weeks)
# ========================================================================
ev_weekly_boro <- ev %>%
  filter(executed_date >= start, executed_date <= anchor) %>%
  mutate(week = floor_date(executed_date, "week", week_start = 1)) %>%
  count(BOROUGH, week, name = "evictions") %>%
  group_by(BOROUGH) %>%
  complete(week = seq(min(week), max(week), by = "1 week"),
           fill  = list(evictions = 0)) %>%
  ungroup() %>%
  arrange(BOROUGH, week)

ev_weekly_boro %>%
  group_by(BOROUGH) %>%
  slice_head(n = 5) %>%
  ungroup() %>%
  print(n = 20)

if (interactive()) {
  ev_weekly_boro %>%
    ggplot(aes(week, evictions, color = BOROUGH)) +
    geom_line() +
    labs(title = "Evictions per Week by Borough — Last 52 Weeks", x = NULL, y = "Evictions") +
    theme_minimal() + theme(legend.position = "bottom") %>%
    print()
}

# ========================================================================
# EXPERIMENT 3 — Latest 12 weeks (sorted table)
# ========================================================================
ev_weekly_last_12 <- ev_weekly_last_year %>%
  slice_tail(n = 12) %>%
  arrange(desc(evictions))

show_gt(ev_weekly_last_12 %>%
          mutate(week = as.Date(week),
                 evictions = number(evictions, accuracy = 1)),
        title = "Evictions — Latest 12 Weeks (sorted)")

# ========================================================================
# EXPERIMENT 4 — RS stock by borough (shares of buildings & units)
# ========================================================================
rs_by_boro <- rs %>%
  group_by(borough) %>%
  summarize(
    buildings = n(),
    units     = sum(unitsres, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    share_buildings = buildings / sum(buildings),
    share_units     = units     / sum(units)
  ) %>%
  arrange(desc(share_units))

show_gt(
  rs_by_boro %>%
    mutate(
      buildings       = number(buildings, accuracy = 1, big.mark = ","),
      units           = number(units,     accuracy = 1, big.mark = ","),
      share_buildings = percent(share_buildings, accuracy = 0.1),
      share_units     = percent(share_units,     accuracy = 0.1)
    ),
  title = "Rent-Stabilized Stock by Borough — Shares of Buildings & Units"
)

# ========================================================================
# EXPERIMENT 5
# EDA — RS Parcels: Buildings by Year Built (YEARBLT), yearly bars
# PURPOSE: See concentration of RS buildings by construction year.
# ========================================================================

# 1) Clean YEARBLT -> integer; keep plausible years; count per year; fill gaps with 0
year_limit_min <- 1850
year_limit_max <- as.integer(format(Sys.Date(), "%Y"))

rs_yearly <- rs %>%
  mutate(year_built = suppressWarnings(as.integer(yearbuilt))) %>%
  filter(!is.na(year_built),
         dplyr::between(year_built, year_limit_min, year_limit_max)) %>%
  count(year_built, name = "buildings") %>%
  complete(
    year_built = seq(min(year_built), max(year_built), by = 1),
    fill = list(buildings = 0)
  )

# 2) Plot: yearly bars (geom_col on pre-aggregated data)
ggplot(rs_yearly, aes(x = year_built, y = buildings)) +
  geom_col() +
  scale_x_continuous(
    breaks = pretty_breaks(n = 12),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Rent-Stabilized Parcels — Buildings by Year Built",
    subtitle = paste0("YEARBLT (", year_limit_min, "–", year_limit_max, "); zero-filled for missing years"),
    x = "Year Built",
    y = "Number of Buildings"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# ========================================================================
# EXPERIMENT 6 — Event-level join: evictions to RS by BBL
# ========================================================================

rs_key <- make_rs_key(rs)          # de-duped RS (one row per BBL)
rs_ev  <- join_rs_events(ev, rs_key)

head(rs_ev, 10)

# ========================================================================
# EXPERIMENT 7 — Borough rates: events per 1,000 RS units + % of RS units
# (robust via BBL→BOROUGH on both sides)
# ========================================================================
rs_units_upper <- rs_units_by_borough(rs_key)   # BOROUGH + units (from BBL)

rs_ev_boro <- rs_ev %>%
  mutate(BOROUGH = dplyr::coalesce(BOROUGH, borough_from_bbl(bbl))) %>%
  count(BOROUGH, name = "evictions_at_rs")

rs_rates <- rs_ev_boro %>%
  left_join(rs_units_upper, by = "BOROUGH") %>%
  mutate(
    units          = tidyr::replace_na(units, 0),
    rate_per_1000  = ifelse(units > 0, 1000 * evictions_at_rs / units, NA_real_),
    rate_pct       = ifelse(units > 0, evictions_at_rs / units,          NA_real_)
  ) %>%
  arrange(desc(rate_per_1000))

show_gt(
  rs_rates %>%
    mutate(
      evictions_at_rs = number(evictions_at_rs, accuracy = 1, big.mark = ","),
      units           = number(units,           accuracy = 1, big.mark = ","),
      rate_per_1000   = number(rate_per_1000,   accuracy = 0.1),
      rate_pct        = percent(rate_pct,       accuracy = 0.1)
    ),
  title = "Eviction Events at RS Buildings — Per 1,000 Units and % of RS Units"
)

# ========================================================================
# EXPERIMENT 8 — Top RS owners by eviction event count
# ========================================================================
clean_owner <- function(x) {
  x %>% str_to_lower() %>% str_squish() %>%
    str_replace_all("[\\.,&']", "") %>%
    str_replace_all("\\b(llc|inc|corp|ltd|co|company|holding|holdings)\\b", "") %>%
    str_squish()
}

top_owners <- rs_ev %>%
  mutate(owner_clean = if ("ownername" %in% names(.)) clean_owner(ownername) else NA_character_) %>%
  filter(!is.na(owner_clean), owner_clean != "") %>%
  count(owner_clean, sort = TRUE) %>%
  slice_head(n = 15)

show_gt(top_owners, title = "Top RS Owner Names by Eviction Event Count (Top 15)")

# ========================================================================
# EXPERIMENT 9 — RS buildings with most eviction events
# ========================================================================
top_bbls <- rs_ev %>%
  mutate(bbl = as.character(BBL)) %>%          # ensure name/type match
  filter(!is.na(bbl)) %>%
  count(bbl, sort = TRUE, name = "evictions") %>%
  left_join(
    rs_ev %>%
      mutate(bbl = as.character(BBL)) %>%
      distinct(bbl, address, BOROUGH, zipcode, unitsres),
    by = "bbl"
  ) %>%
  slice_head(n = 15)


show_gt(
  top_bbls %>%
    mutate(
      evictions = number(evictions, accuracy = 1, big.mark = ","),
      unitsres  = number(unitsres,  accuracy = 1, big.mark = ",")
    ),
  title = "RS Buildings with Most Eviction Events (Top 15)"
)

# ========================================================================
# EXPERIMENT 10 — Weekly eviction events at RS buildings (last 52 weeks)
# ========================================================================
ev_weekly_rs <- ev %>%
  filter(executed_date >= start, executed_date <= anchor, !is.na(bbl)) %>%
  inner_join(rs_key %>% select(bbl), by = "bbl") %>%
  mutate(week = floor_date(executed_date, "week", week_start = 1)) %>%
  count(week, name = "evictions_at_rs") %>%
  complete(week = seq(min(week), max(week), by = "1 week"),
           fill = list(evictions_at_rs = 0)) %>%
  arrange(week)

head(ev_weekly_rs, 8)

ev_weekly_rs %>%
  ggplot(aes(x = week, y = evictions_at_rs)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Eviction Events at RS Buildings — Weekly (Last 52 Weeks)",
    x = NULL,
    y = "Evictions at RS (weekly)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# ========================================================================
# EXPERIMENT 11 — Borough shares: eviction events vs RS unit shares
# ========================================================================

# Eviction EVENTS by borough (last 52 weeks), plus share of citywide events
ev_boro_total <- ev %>%
  filter(executed_date >= start, executed_date <= anchor) %>%
  mutate(BOROUGH = dplyr::coalesce(BOROUGH, borough_from_bbl(bbl))) %>%
  count(BOROUGH, name = "evictions") %>%
  mutate(share_ev = evictions / sum(evictions))

# RS unit totals by borough (derived from BBL), plus share of citywide RS units
rs_boro_share <- rs_units_by_borough(rs_key) %>%
  mutate(share_units = units / sum(units)) %>%
  select(BOROUGH, share_units)

# Join and compute over/under-representation (share_diff)
share_compare <- ev_boro_total %>%
  left_join(rs_boro_share, by = "BOROUGH") %>%
  mutate(share_diff = share_ev - share_units) %>%
  arrange(desc(share_ev))

# --------- Pretty GT table with column explainers ---------
share_tbl <- share_compare %>%
  select(BOROUGH, evictions, share_ev, share_units, share_diff) %>%
  mutate(
    evictions   = number(evictions, big.mark = ","),
    share_ev    = percent(share_ev,    accuracy = 0.1),
    share_units = percent(share_units,  accuracy = 0.1),
    share_diff  = percent(share_diff,   accuracy = 0.1)
  )

share_tbl %>%
  gt() %>%
  cols_label(
    BOROUGH     = "Borough",
    evictions   = "Eviction events",
    share_ev    = "Share of evictions",
    share_units = "Share of RS units",
    share_diff  = "Gap (ev − units)"
  ) %>%
  tab_header(
    title = md("**Borough Shares: Eviction Events vs RS Unit Shares (Last 52 Weeks)**")
  ) %>%
  # Header footnotes (explainers)
  tab_footnote(
    footnote  = "Uppercase borough from evictions (or derived from BBL when needed).",
    locations = cells_column_labels(columns = BOROUGH)
  ) %>%
  tab_footnote(
    footnote  = "Count of eviction events in the 52-week window.",
    locations = cells_column_labels(columns = evictions)
  ) %>%
  tab_footnote(
    footnote  = "Borough’s eviction events ÷ citywide eviction events.",
    locations = cells_column_labels(columns = share_ev)
  ) %>%
  tab_footnote(
    footnote  = "Borough’s RS units ÷ citywide RS units (from RS file).",
    locations = cells_column_labels(columns = share_units)
  ) %>%
  tab_footnote(
    footnote  = "share_ev − share_units; positive = over-represented, negative = under-represented.",
    locations = cells_column_labels(columns = share_diff)
  ) %>%
  # Brief reading guide
  tab_source_note(
    md("**Reading guide:** Positive *Gap* ⇒ more evictions than expected given RS stock; negative ⇒ fewer.")
  ) %>%
  tab_options(
    table.width = pct(100),
    data_row.padding = px(6),
    table.font.size  = px(14)
  )

############################################################
# END - Class 8 Lab Experiments 1-11 for rs + ev data
############################################################
