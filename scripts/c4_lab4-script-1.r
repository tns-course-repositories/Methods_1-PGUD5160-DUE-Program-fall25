#c4_lab4-script-1
#this script features several EDA prompts for ACS 2023 5 yr survey ----
#Public Use Microdata Areas (PUMAs) for New York County (Manhattan) ----


# Install if needed
# install.packages(c("tidycensus", "tidyverse", "sf"))

library(tidycensus)
library(tidyverse)
library(sf)

# Set your Census API key (get one at https://api.census.gov/data/key_signup.html)
#census_api_key("YOUR KEY HERE", install = TRUE) #install for future sessions
#census_api_key("YOUR KEY HERE", overwrite = TRUE) #overwrite if needed

# variable loading ----

vars <- load_variables(2023, "acs5", cache = TRUE)
head(vars, 10)

# Get Median Household Income by PUMA (New York County Manhattan)----

options(tigris_use_cache = TRUE, tigris_class = "sf")

# Download ACS data for NY PUMAs
income_puma <- get_acs(
  geography = "puma",
  variables = "B19013_001", # Median household income
  state = "NY",
  year = 2023,
  survey = "acs5",
  geometry = TRUE
)

# Clean and Organize

income_clean <- income_puma %>%
  rename(median_income = estimate, moe_income = moe) %>%
  select(GEOID, NAME, median_income, moe_income, geometry)

# Filter just the ones where NAME contains "Manhattan"

income_manhattan <- income_clean %>%
  filter(str_detect(NAME, "Manhattan"))

# Save & Load if needed for future use
#save(income_manhattan, file = "income_manhattan.RData")
# Load if needed for future use
#load("income_manhattan.RData")

# Map outlines only ----

ggplot(income_manhattan) +
  geom_sf(fill = NA, color = "black", size = 0.8) +
  labs(
    title = "PUMA Boundaries in Manhattan",
    caption = "Source: ACS 2023 Boundaries via tidycensus"
  ) +
  theme_minimal()

# EDA - Comparison Plot ----

# Bar chart with error bars + MOE labels
ggplot(income_manhattan, aes(x = reorder(NAME, median_income), y = median_income)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(
    aes(ymin = median_income - moe_income, ymax = median_income + moe_income),
    width = 0.2, color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Median Household Income by PUMA (Manhattan, ACS 2023)",
    x = "Public Use Microdata Area (PUMA)",
    y = "Median Household Income (USD)",
    caption = "Source: U.S. Census Bureau, ACS 2023 5-year estimates\nBlack lines show the ACS margin of error (MOE) at 90% confidence."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# Mapping of the Income Distribution across PUMAs ----

# Plot map
ggplot(income_manhattan, aes(fill = median_income)) +
  geom_sf(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Median Household Income by PUMA (Manhattan, ACS 2023)",
    fill = "Income (USD)"
  ) +
  theme_minimal()


#Multi-Variable Example: Education vs Income ----

# Pull ACS variables for education & income at PUMA level
edu_income_puma <- get_acs(
  geography = "puma",
  variables = c(
    median_income = "B19013_001",   # Median household income
    bachelors     = "B15003_022",   # Count with Bachelor's degree
    total_edu     = "B15003_001"    # Total population 25+ with educational attainment
  ),
  state = "NY",
  year = 2023,
  survey = "acs5"
) %>%
  filter(str_detect(NAME, "Manhattan"))   # Keep only Manhattan PUMAs

# Reshape & calculate percentage
edu_income_wide <- edu_income_puma %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    bachelors_rate = 100 * bachelors / total_edu
  )


# Save & Load if needed for future use
#save(edu_income_wide, file = "edu_income_wide.RData")
# Load if needed for future use
#load("edu_income_wide.RData")


# Scatterplot: % with Bachelor's degree vs Median Income ----

ggplot(edu_income_wide, aes(x = bachelors_rate, y = median_income)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "magenta", se = FALSE) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Education vs Income (Manhattan PUMAs, ACS 2023)",
    x = "Adults (25+) with Bachelor's Degree (%)",
    y = "Median Household Income (USD)",
    caption = "Source: U.S. Census Bureau, ACS 2023 5-year estimates"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

