#c4-assignment-script-2

# ---- Packages ----
library(tidyverse)
library(tidycensus)
library(gt)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)

# ---- Settings ----
acs_year  <- 2023
threshold <- 30   # <<< TOGGLE: set to 30 for ≥30% or 50 for ≥50% (severely burdened)

# B25070: Gross rent as % of household income (renter households)
vars_needed <- c(
  "B25070_001", # total renter households
  "B25070_007", # 30-34%
  "B25070_008", # 35-39%
  "B25070_009", # 40-49%
  "B25070_010", # 50%+
  "B25070_011"  # not computed
)

# Three PUMAs to compare (5-digit codes within NY)
puma_codes <- c("04103", "04401", "04414")
puma_labels <- c(
  "PUMA 3604103 (Manhattan)",
  "PUMA 3604401 (Queens CD 1—Astoria & Queensbridge)",
  "PUMA 3604414 (Queens CD 14—The Rockaways)"
)
names(puma_labels) <- puma_codes

# Desired row/plot order (top -> bottom)
geo_order <- c(
  "PUMA 3604103 (Manhattan)",
  "PUMA 3604401 (Queens CD 1—Astoria & Queensbridge)",
  "PUMA 3604414 (Queens CD 14—The Rockaways)",
  "New York City (5 Boroughs)",
  "New York State",
  "United States"
)

# Helpers
safe_div <- function(num, den) ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)
get_col  <- function(df, nm) if (nm %in% names(df)) df[[nm]] else 0
th_label <- paste0("≥", threshold, "%")

# ---- core: compute burdened share from a tidy get_acs() table ----
# Uses threshold toggle: if 30 -> 30–34 + 35–39 + 40–49 + 50%+; if 50 -> 50%+ only
compute_share <- function(df, threshold = 30) {
  wide <- df %>%
    select(GEOID, NAME, variable, estimate, moe) %>%
    pivot_wider(
      names_from  = variable,
      values_from = c(estimate, moe)
    )
  
  # components
  e7  <- get_col(wide, "estimate_B25070_007"); m7  <- get_col(wide, "moe_B25070_007")
  e8  <- get_col(wide, "estimate_B25070_008"); m8  <- get_col(wide, "moe_B25070_008")
  e9  <- get_col(wide, "estimate_B25070_009"); m9  <- get_col(wide, "moe_B25070_009")
  e10 <- get_col(wide, "estimate_B25070_010"); m10 <- get_col(wide, "moe_B25070_010")
  etot <- get_col(wide, "estimate_B25070_001"); mtot <- get_col(wide, "moe_B25070_001")
  enc  <- get_col(wide, "estimate_B25070_011"); mnc  <- get_col(wide, "moe_B25070_011")
  
  # numerator by threshold
  if (threshold == 50) {
    burden_est <- e10
    burden_moe <- m10
  } else {
    m78        <- tidycensus::moe_sum(m7, m8)
    m910       <- tidycensus::moe_sum(m9, m10)
    burden_est <- e7 + e8 + e9 + e10
    burden_moe <- tidycensus::moe_sum(m78, m910)
  }
  
  # denominator: total - "not computed" (if present)
  denom_est <- etot - enc
  denom_moe <- if ("moe_B25070_011" %in% names(wide)) tidycensus::moe_sum(mtot, mnc) else mtot
  
  share_prop     <- safe_div(burden_est, denom_est)
  share_prop_moe <- tidycensus::moe_ratio(burden_est, denom_est, burden_moe, denom_moe)
  
  tibble(
    GEOID    = wide$GEOID,
    NAME     = wide$NAME,
    estimate = 100 * share_prop,      # percent
    moe      = 100 * share_prop_moe   # ± percentage points
  )
}

# ---- PUMAs: pull all NY PUMAs then filter to the 3 of interest ----
pumas_df <- get_acs(
  geography   = "public use microdata area",
  variables   = vars_needed,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  output      = "tidy",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  mutate(puma5 = stringr::str_sub(GEOID, -5, -1)) %>%
  filter(puma5 %in% puma_codes) %>%
  compute_share(threshold = threshold) %>%
  mutate(geography = puma_labels[stringr::str_sub(GEOID, -5, -1)]) %>%
  select(geography, estimate, moe)

# ---- NYC (5 Boroughs) aggregate from counties using B25070 counts ----
# Sum county estimates; combine MOEs via root-sum-of-squares; then compute ratio at aggregate level.
nyc_counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")

nyc_cnty_wide <- get_acs(
  geography   = "county",
  variables   = vars_needed,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  county      = nyc_counties,
  output      = "tidy",
  geometry    = FALSE
) %>%
  select(GEOID, NAME, variable, estimate, moe) %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe))

nyc_agg <- nyc_cnty_wide %>%
  summarise(
    # estimates (sums across counties)
    est_001 = sum(estimate_B25070_001, na.rm = TRUE),
    est_007 = sum(estimate_B25070_007, na.rm = TRUE),
    est_008 = sum(estimate_B25070_008, na.rm = TRUE),
    est_009 = sum(estimate_B25070_009, na.rm = TRUE),
    est_010 = sum(estimate_B25070_010, na.rm = TRUE),
    est_011 = sum(estimate_B25070_011, na.rm = TRUE),
    # MOEs (root-sum-of-squares across counties)
    moe_001 = sqrt(sum((moe_B25070_001)^2, na.rm = TRUE)),
    moe_007 = sqrt(sum((moe_B25070_007)^2, na.rm = TRUE)),
    moe_008 = sqrt(sum((moe_B25070_008)^2, na.rm = TRUE)),
    moe_009 = sqrt(sum((moe_B25070_009)^2, na.rm = TRUE)),
    moe_010 = sqrt(sum((moe_B25070_010)^2, na.rm = TRUE)),
    moe_011 = sqrt(sum((moe_B25070_011)^2, na.rm = TRUE))
  ) %>%
  mutate(
    num_est = if (threshold == 50) est_010 else (est_007 + est_008 + est_009 + est_010),
    num_moe = if (threshold == 50) moe_010 else tidycensus::moe_sum(
      tidycensus::moe_sum(moe_007, moe_008),
      tidycensus::moe_sum(moe_009, moe_010)
    ),
    den_est = est_001 - est_011,
    den_moe = tidycensus::moe_sum(moe_001, moe_011),
    share   = safe_div(num_est, den_est),
    share_m = tidycensus::moe_ratio(num_est, den_est, num_moe, den_moe),
    estimate = 100 * share,
    moe      = 100 * share_m
  ) %>%
  transmute(
    geography = "New York City (5 Boroughs)",
    estimate, moe
  )

# ---- State: New York ----
state_df <- get_acs(
  geography   = "state",
  variables   = vars_needed,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  output      = "tidy",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  compute_share(threshold = threshold) %>%
  transmute(
    geography = "New York State",
    estimate, moe
  )

# ---- Nation: United States ----
us_df <- get_acs(
  geography   = "us",
  variables   = vars_needed,
  survey      = "acs5",
  year        = acs_year,
  output      = "tidy",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  compute_share(threshold = threshold) %>%
  transmute(
    geography = "United States",
    estimate, moe
  )

# ---- Combine + quality metrics + shared ordering & groups ----
compare_tbl <- bind_rows(pumas_df, nyc_agg, state_df, us_df) %>%
  mutate(
    moe_pct_of_est = 100 * safe_div(moe, estimate),
    est_prop = estimate / 100,
    se_prop  = (moe / 100) / 1.645,     # SE from 90% MOE
    cv_pct   = 100 * safe_div(se_prop, est_prop),
    geography = factor(geography, levels = geo_order),
    group = if_else(as.character(geography) %in% puma_labels, "PUMAs", "Comparators")
  ) %>%
  arrange(geography) %>%
  select(group, geography, estimate, moe, moe_pct_of_est, cv_pct)

# ---- Plot: bar chart with MOE error bars (order matches table top→bottom) ----
# coord_flip() reverses order visually; reverse levels so U.S. is at the bottom.
plot_df <- compare_tbl %>%
  mutate(geography = factor(geography, levels = rev(geo_order)))

rb_plot <- ggplot(plot_df, aes(x = geography, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.2) +
  coord_flip() +
  labs(
    title = paste0("Renter Households Cost-Burdened (", th_label, " of Income) — ACS ", acs_year, " 5-year"),
    subtitle = "Three PUMAs vs New York City (5 Boroughs), New York State, and U.S.",
    x = NULL,
    y = "Estimate (%)",
    caption = "Bars show point estimates; error bars are ±90% MOE (percentage points)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9)
  )

# Print plot
rb_plot

# ---- GT table with row groups ('PUMAs' and 'Comparators') ----
rentburden_table <- compare_tbl %>%
  gt(groupname_col = "group", rowname_col = "geography") %>%
  tab_header(
    title = gt::md(paste0("Renter Households Cost-Burdened (", th_label, " of Income) — ACS ", acs_year, " 5-year")),
    subtitle = "Three PUMAs vs New York City (5 Boroughs), New York State, and United States"
  ) %>%
  cols_label(
    geography      = "Geography",
    estimate       = "Estimate (%)",
    moe            = "MOE (± pct. pts)",
    moe_pct_of_est = "MOE (% of est.)",
    cv_pct         = "CV (%)"
  ) %>%
  fmt_number(columns = c(estimate, moe), decimals = 1) %>%
  fmt_number(columns = c(moe_pct_of_est, cv_pct), decimals = 1) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups = c("PUMAs", "Comparators"))
  )

src_txt <- paste0(
  "Source: U.S. Census Bureau, ACS 5-year via tidycensus (B25070). ",
  "Burdened defined as ", th_label, " of income (≥30% combines 30–34, 35–39, 40–49, and 50%+; ≥50% uses 50%+ only). ",
  "Denominator excludes 'Not computed'. NYC (5 Boroughs) aggregates Bronx, Kings, New York, Queens, and Richmond. ",
  "MOEs combined via root-sum-of-squares across counties; ratio MOE via moe_ratio(). ",
  "CV computed as ((MOE/1.645)/Estimate) on the proportion scale."
)

if ("tab_source_note" %in% getNamespaceExports("gt")) {
  rentburden_table <- gt::tab_source_note(rentburden_table, source_note = gt::md(src_txt))
} else {
  rentburden_table <- gt::tab_caption(rentburden_table, caption = src_txt)
}

# Print table
rentburden_table

# As needed for Assignment 4 .Rmd

save(compare_tbl, file = "~/Desktop/assignment_4/results/compare_tbl.RData")
