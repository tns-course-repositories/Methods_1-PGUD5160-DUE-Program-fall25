#c4-lab4-script-4


#as needed
#install.packages("gt")


# ---- Packages ----
library(tidyverse)
library(tidycensus)
library(gt)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# census key only necessary if you have not install = TRUE in past sessions
# if you need to install a new key, you can use overwrite = TRUE

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)


# PUMA code 3604103 access codes here: https://www2.census.gov/geo/maps/DC2020/PUMA/st36_ny/Catalog_PUMAmaps_st36.pdf

# ---- Settings ----
acs_year   <- 2023
var_povpct <- "S1701_C03_001"   # Percent below poverty level (population for whom poverty status is determined)
puma_code5  <- "04103"          # PUMA code within NY
puma_geoid7 <- paste0("36", puma_code5)  # "3604103"

safe_div <- function(num, den) ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)

# ---- PUMA: NY PUMAs then filter to 3604103 (suffix 04103) ----
puma_df <- get_acs(
  geography   = "public use microdata area",
  variables   = var_povpct,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  output      = "wide",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  filter(GEOID == puma_geoid7 | GEOID == puma_code5 | stringr::str_sub(GEOID, -5, -1) == puma_code5) %>%
  transmute(
    geography = "PUMA 3604103 (Manhattan)",
    estimate  = S1701_C03_001E,   # percent
    moe       = S1701_C03_001M    # percentage points (90% MOE)
  )

# ---- County: New York County (Manhattan) ----
county_df <- get_acs(
  geography   = "county",
  variables   = var_povpct,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  county      = "New York",
  output      = "wide",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  transmute(
    geography = "New York County (Manhattan), NY",
    estimate  = S1701_C03_001E,
    moe       = S1701_C03_001M
  )

# ---- State: New York ----
state_df <- get_acs(
  geography   = "state",
  variables   = var_povpct,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  output      = "wide",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  transmute(
    geography = "New York State",
    estimate  = S1701_C03_001E,
    moe       = S1701_C03_001M
  )

# ---- Nation: United States ----
us_df <- get_acs(
  geography   = "us",
  variables   = var_povpct,
  survey      = "acs5",
  year        = acs_year,
  output      = "wide",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  transmute(
    geography = "United States",
    estimate  = S1701_C03_001E,
    moe       = S1701_C03_001M
  )

# ---- Combine + quality metrics ----
# Note: estimate is a percent; moe is percentage points (pp).
# For CV(%), compute on proportion scale to be dimensionless, then express as a percent.
compare_tbl <- bind_rows(puma_df, county_df, state_df, us_df) %>%
  mutate(
    # MOE as % of estimate (just a diagnostic)
    moe_pct_of_est = 100 * safe_div(moe, estimate),

    # Convert to proportions for CV calc
    est_prop = estimate / 100,
    se_prop  = (moe / 100) / 1.645,     # SE from 90% MOE
    cv_pct   = 100 * safe_div(se_prop, est_prop)
  ) %>%
  select(geography, estimate, moe, moe_pct_of_est, cv_pct)


# Save if needed for future use
#save(compare_tbl, file = "compare_tbl.RData")


# ---- Build the gt table (with version-safe source note) ----
poverty_table <- compare_tbl %>%
  gt() %>%
  tab_header(
    title = gt::md(paste0("Poverty Rate (Last 12 Months) — ACS ", acs_year, " 5-year")),
    subtitle = "PUMA 3604103 vs Manhattan (county), New York State, and U.S."
  ) %>%
  cols_label(
    geography      = "Geography",
    estimate       = "Estimate (%)",
    moe            = "MOE (± pct. pts)",
    moe_pct_of_est = "MOE (% of est.)",
    cv_pct         = "CV (%)"
  ) %>%
  fmt_number(columns = c(estimate, moe), decimals = 1) %>%
  fmt_number(columns = c(moe_pct_of_est, cv_pct), decimals = 1)

# Add a source note if available; else add a caption as fallback
src_txt <- paste0(
  "Source: U.S. Census Bureau, ACS 5-year via **tidycensus** (S1701_C03_001: Percent below poverty level). ",
  "MOE is 90% CI; CV computed on proportion scale as (MOE/1.645)/Estimate."
)

if ("tab_source_note" %in% getNamespaceExports("gt")) {
  poverty_table <- gt::tab_source_note(
    poverty_table,
    source_note = gt::md(src_txt)
  )
} else {
  poverty_table <- gt::tab_caption(
    poverty_table,
    caption = src_txt
  )
}

# ---- Print the table object ----
poverty_table
