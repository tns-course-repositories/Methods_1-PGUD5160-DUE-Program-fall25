#c4-lab4-script-3

# --- Packages ---
library(tidyverse)
library(tidycensus)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# census key only necessary if you have not install = TRUE in past sessions
# if you need to install a new key, you can use overwrite = TRUE

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)




# --- Your 2020 tract list (MN03 filter as before) ---
CD_CT <- read.csv("~/Desktop/lab_4/data/2020_Census_Tracts_to_2020_NTAs_and_CDTAs_Equivalency_20250903.csv")
tracts_2020 <- as.character(CD_CT$GEOID[CD_CT$CDTACode == "MN03"]) |>
  stringr::str_pad(11, pad = "0")

# --- NHGIS 2010→2020 tract crosswalk CSV; adjust path if needed) ---
xwalk_path <- "~/Desktop/lab_4/data/nhgis_tr2010_tr2020_36.csv"

# Helper to pick a column by exact name or regex (guided by README naming)
pick_col <- function(df, exact = NULL, regex = NULL) {
  nms <- names(df)
  if (!is.null(exact) && exact %in% nms) return(exact)
  if (!is.null(regex)) {
    hits <- nms[stringr::str_detect(nms, regex)]
    if (length(hits) > 0) return(hits[[1]])
  }
  stop("Could not find a column by the provided spec: ",
       paste(na.omit(c(exact, regex)), collapse = " / "))
}

# --- Load crosswalk & standardize columns using README conventions ---
# Expecting columns like: tr2010gj, tr2010ge, tr2020gj, tr2020ge, parea, wt_pop, ...
xwalk_raw <- readr::read_csv(xwalk_path, show_col_types = FALSE)

src_ge_col <- pick_col(xwalk_raw, exact = "tr2010ge", regex = "^tr2010ge$")
tgt_ge_col <- pick_col(xwalk_raw, exact = "tr2020ge", regex = "^tr2020ge$")
# Prefer population weight for people-based rates; fallback to 'weight' (block crosswalk) or 'parea'
wt_col <- if ("wt_pop" %in% names(xwalk_raw)) {
  "wt_pop"
} else if ("weight" %in% names(xwalk_raw)) {
  "weight"
} else if ("parea" %in% names(xwalk_raw)) {
  "parea"
} else {
  stop("No suitable weight column found (expected wt_pop/weight/parea).")
}

xwalk <- xwalk_raw |>
  transmute(
    tracts_2010 = stringr::str_pad(stringr::str_sub(.data[[src_ge_col]], 1, 11), 11, pad = "0"),
    tracts_2020 = stringr::str_pad(stringr::str_sub(.data[[tgt_ge_col]], 1, 11), 11, pad = "0"),
    pop_wt     = as.numeric(.data[[wt_col]])
  ) |>
  filter(is.finite(pop_wt), pop_wt > 0)

# --- County/state lookup for API calls (from your 2020 tract list) ---
tract_meta_2020 <- tibble(GEOID = tracts_2020) |>
  mutate(
    state  = stringr::str_sub(GEOID, 1, 2),
    county = stringr::str_sub(GEOID, 3, 5)
  ) |>
  distinct(state, county)

# --- MOE combiner for already-weighted pieces (quadrature) ---
combine_moe <- function(moe_scaled) {
  ok <- is.finite(moe_scaled)
  if (!any(ok)) return(NA_real_)
  sqrt(sum((moe_scaled[ok])^2))
}

# --- Fetch + reallocate for one ACS 5-year 'year' ---
# Uses tract-level ACS for Numerator=B17001_002, Denominator=B17001_001
# 2015–2019: reallocate (2010→2020) with NHGIS population weights
# 2020–2023: already 2020 tracts; filter & sum

#Poverty = vars <- c(pov_num = "B17001_002", pov_den = "B17001_001")
fetch_year <- function(year) {
  vars <- c(pov_num = "B17001_002", pov_den = "B17001_001") 
  
acs_tr <- purrr::pmap_dfr(
    tract_meta_2020,
    function(state, county) {
      get_acs(
        geography   = "tract",
        variables   = vars,
        survey      = "acs5",
        year        = year,
        state       = state,
        county      = county,
        geometry    = FALSE,
        cache_table = TRUE,
        output      = "wide"   # pov_numE, pov_numM, pov_denE, pov_denM
      )
    }
  ) |>
    transmute(
      tract = stringr::str_pad(stringr::str_sub(GEOID, 1, 11), 11, pad = "0"),
      pov_numE, pov_numM, pov_denE, pov_denM
    )

  if (year <= 2019) {
    # Reallocate 2010 tracts → 2020 tracts
    reall <- xwalk |>
      left_join(acs_tr, by = c("tracts_2010" = "tract")) |>
      mutate(
        num_est_w = pop_wt * pov_numE,
        den_est_w = pop_wt * pov_denE,
        num_moe_w = pop_wt * pov_numM,
        den_moe_w = pop_wt * pov_denM
      ) |>
      group_by(tracts_2020) |>
      summarise(
        pov_numE = sum(num_est_w, na.rm = TRUE),
        pov_denE = sum(den_est_w, na.rm = TRUE),
        pov_numM = combine_moe(num_moe_w),
        pov_denM = combine_moe(den_moe_w),
        .groups  = "drop"
      ) |>
      rename(tract = tracts_2020)
  } else {
    # 2020+ is already in 2020 tracts
    reall <- acs_tr
  }

  # Aggregate to your 2020 tract set
  out <- reall |>
    filter(tract %in% tracts_2020) |>
    summarise(
      pov_num     = sum(pov_numE, na.rm = TRUE),
      pov_den     = sum(pov_denE, na.rm = TRUE),
      pov_num_moe = sqrt(sum(replace_na(pov_numM, 0)^2, na.rm = TRUE)),
      pov_den_moe = sqrt(sum(replace_na(pov_denM, 0)^2, na.rm = TRUE)),
      .groups     = "drop"
    ) |>
    mutate(
      year         = year,
      pov_rate     = ifelse(pov_den > 0, pov_num / pov_den, NA_real_),
      pov_rate_moe = moe_prop(pov_num, pov_den, pov_num_moe, pov_den_moe)  # 90% MOE
    )

  out
}

# --- Run for 2015–2023 & inspect ---
years <- 2015:2023
ts_poverty_xwalk <- purrr::map_dfr(years, fetch_year) |>
  arrange(year)

ts_poverty_xwalk |>
  mutate(
    pov_rate_pct     = 100 * pov_rate,
    pov_rate_moe_pct = 100 * pov_rate_moe
  ) |>
  print(n = Inf)

# Save if needed for future use
#save(ts_poverty_xwalk, file = "ts_poverty_xwalk.RData")



# --- Time Series Plot ---
ggplot(ts_poverty_xwalk, aes(x = year, y = pov_rate)) +
  geom_ribbon(
    aes(ymin = pmax(0, pov_rate - pov_rate_moe),
        ymax = pmin(1, pov_rate + pov_rate_moe)),
    alpha = 0.2
  ) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(0, 1)) +
  labs(
    title = "Poverty Rate (ACS 5-year) for MN03 (Aligned to 2020 Tracts via NHGIS Crosswalk)",
    subtitle = "Numerator=B17001_002; Denominator=B17001_001 • Ribbon=90% MOE\n2015–2019 reallocated from 2010 tracts; 2020–2023 native 2020 tracts",
    x = "ACS 5-year (end year)",
    y = "Poverty rate",
    caption = "Source: U.S. Census Bureau (ACS 5-year, tidycensus); NHGIS 2010→2020 tract crosswalk"
  ) +
  theme_minimal(base_size = 12)

