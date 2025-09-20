#c4-lab4-script-4

# ---- Packages ----
library(tidyverse)
library(tidycensus)
library(gt)

options(scipen = 999)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# census key only necessary if you have not install = TRUE in past sessions
# if you need to install a new key, you can use overwrite = TRUE

# census_api_key("YOUR_KEY_HERE", install = TRUE, overwrite = TRUE)


# ---- Settings ----
acs_year   <- 2023
var_povpct <- "S1701_C03_001"   # Percent below poverty level (population for whom poverty status is determined)

# Three PUMAs to compare (5-digit codes within NY)
puma_codes <- c("04103", "04401", "04414")
puma_labels <- c(
  "PUMA 3604103 (Manhattan)",
  "PUMA 3604401 (Queens CD 1—Astoria & Queensbridge)",
  "PUMA 3604414 (Queens CD 14—The Rockaways)"
)
names(puma_labels) <- puma_codes

# Desired row/plot order
geo_order <- c(
  "PUMA 3604103 (Manhattan)",
  "PUMA 3604401 (Queens CD 1—Astoria & Queensbridge)",
  "PUMA 3604414 (Queens CD 14—The Rockaways)",
  "New York City (5 Boroughs)",
  "New York State",
  "United States"
)

# Helper
safe_div <- function(num, den) ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)

# ---- PUMAs: pull all NY PUMAs then filter to the 3 of interest ----
pumas_df <- get_acs(
  geography   = "public use microdata area",
  variables   = var_povpct,
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  output      = "wide",
  geometry    = FALSE,
  cache_table = TRUE
) %>%
  mutate(puma5 = stringr::str_sub(GEOID, -5, -1)) %>%
  filter(puma5 %in% puma_codes) %>%
  transmute(
    geography = puma_labels[puma5],
    estimate  = S1701_C03_001E,   # percent
    moe       = S1701_C03_001M    # percentage points (90% MOE)
  )

# ---- NYC (5 Boroughs) aggregate: B17001 num/den + MOE via RSS ----
# B17001_001 = denominator: pop for whom poverty status is determined
# B17001_002 = numerator: below poverty level
nyc_counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")

nyc_cnty <- get_acs(
  geography   = "county",
  variables   = c(den = "B17001_001", num = "B17001_002"),
  survey      = "acs5",
  year        = acs_year,
  state       = "NY",
  county      = nyc_counties,
  output      = "wide",
  geometry    = FALSE
)

nyc_bd <- nyc_cnty %>%
  summarise(
    num  = sum(numE, na.rm = TRUE),
    den  = sum(denE, na.rm = TRUE),
    # root-sum-of-squares MOE aggregation
    numM = sqrt(sum((numM)^2, na.rm = TRUE)),
    denM = sqrt(sum((denM)^2, na.rm = TRUE))
  ) %>%
  mutate(
    estimate = 100 * safe_div(num, den),                      # percent
    moe      = 100 * tidycensus::moe_ratio(num, den, numM, denM)  # pp
  ) %>%
  transmute(
    geography = "New York City (5 Boroughs)",
    estimate, moe
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

# ---- Combine + quality metrics + shared ordering ----
compare_tbl <- bind_rows(pumas_df, nyc_bd, state_df, us_df) %>%
  mutate(
    moe_pct_of_est = 100 * safe_div(moe, estimate),
    est_prop = estimate / 100,
    se_prop  = (moe / 100) / 1.645,     # SE from 90% MOE
    cv_pct   = 100 * safe_div(se_prop, est_prop),
    geography = factor(geography, levels = geo_order),
    group = if_else(
      as.character(geography) %in% puma_labels,
      "PUMAs",
      "Comparators"
    )
  ) %>%
  arrange(geography) %>%
  select(group, geography, estimate, moe, moe_pct_of_est, cv_pct)

# ---- Plot: bar chart with MOE error bars (matches table order top-to-bottom) ----
plot_df <- compare_tbl %>%
  mutate(
    geography = factor(
      geography,
      levels = rev(geo_order)  # reverse the order for plotting with coord_flip()
    )
  )

pov_plot <- ggplot(plot_df, aes(x = geography, y = estimate)) +
  geom_col() +
  geom_errorbar(aes(ymin = estimate - moe, ymax = estimate + moe), width = 0.2) +
  coord_flip() +
  labs(
    title = paste0("Poverty Rate (Last 12 Months) — ACS ", acs_year, " 5-year"),
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

pov_plot

# ---- GT table with row groups ('PUMAs' and 'Comparators') ----
poverty_table <- compare_tbl %>%
  gt(groupname_col = "group", rowname_col = "geography") %>%
  tab_header(
    title = gt::md(paste0("Poverty Rate (Last 12 Months) — ACS ", acs_year, " 5-year")),
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
  "Source: U.S. Census Bureau, ACS 5-year via tidycensus. ",
  "PUMAs/State/US use S1701_C03_001 (Percent below poverty). ",
  "NYC (5 Boroughs) computed from B17001 (num=B17001_002, den=B17001_001); ",
  "MOEs aggregated by root-sum-of-squares; ratio MOE via moe_ratio()."
)

if ("tab_source_note" %in% getNamespaceExports("gt")) {
  poverty_table <- gt::tab_source_note(poverty_table, source_note = gt::md(src_txt))
} else {
  poverty_table <- gt::tab_caption(poverty_table, caption = src_txt)
}

# Print table
poverty_table

# As needed for Assignment 4 .Rmd

save(compare_tbl, file = "~/Desktop/assignment_4/results/compare_tbl.RData")
