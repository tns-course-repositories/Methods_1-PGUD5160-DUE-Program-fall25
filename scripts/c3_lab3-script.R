#class 3 lab 3 script: working with US Census Decennial 2020 Data

#install necessary lab 3 packages in singular format
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("ggthemes")
#or install necessary lab 3 packages as a character vector
install.packages(c("tidyverse", "tidycensus", "ggthemes"))

#call the libraries into the working session
library(tidycensus)
library(tidyverse)
library(ggthemes)

#install your census API key, and set to TRUE
census_api_key("YOUR KEY GOES HERE", install = TRUE)

# Load variables for the 2020 PL file
pl_variables <- load_variables(2020, "pl")
# Load variables for the 2020 DHC file
dhc_variables <- load_variables(2020, "dhc")
#view the variable tables
view(pl_variables)
view(dhc_variables)

#exception: if backup data is utilized load w/ file path determined by setwd()
load("ny_housing.RData"); load("ny_population.RData"); load("pop_data.RData"); load("race.RData"); load("urban_rural.RData")

#exploration 1: state population
pop_data <- get_decennial(geography = "state",
                          variables = "P1_001N",
                          year = 2020,
                          sumfile = "pl"
                          )
#view head of result
head(pop_data)

#plot the result via ggplot
pop_data %>%
  ggplot(aes(x = reorder(NAME, value), y = value)) +
  geom_col() +
  coord_flip() +
  labs(title="Population by State, 2020")

#exploration 2: urban vs rural 
urban_rural <- get_decennial(
  geography = "us",
  variables = c("P2_002N","P2_003N"),
  year = 2020,
  sumfile = "dhc")

#view head of result
head(urban_rural)

#plot the result via ggplot
urban_rural %>%
  mutate(name = recode(variable, P2_002N = "Urban", P2_003N = "Rural")) |>
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  labs(title = "Rural vs Urban Population (2020)", x = NULL, y = "Population") +
  theme_minimal() + scale_y_continuous(labels = scales::comma)

#exploration 3: base R population plot + refined ggplot
#get decennial Census data for total population (variable P1_001N) for NY counties
#specify the year (e.g., 2020) and state (NY)
ny_population <- get_decennial(
  geography = "county",
  variables = "P1_001N", # Total Population
  state = "NY",
  year = 2020, #desired decennial year
  sumfile = "pl",
  geometry = FALSE #no need for spatial data for a bar chart
)

#view head of result
head(ny_population)

# Clean up county names (remove "County, New York")
ny_population$NAME <- gsub(" County, New York", "", ny_population$NAME)

# Order the data by population for a more organized bar chart (optional)
ny_population <- ny_population[order(ny_population$value, decreasing = TRUE),]


# Create the bar chart using base R
barplot(
  height = ny_population$value,
  names.arg = ny_population$NAME,
  main = "Total Population of New York State Counties (2020 Census)",
  xlab = "County",
  ylab = "Total Population",
  col = "skyblue", # Optional: Set bar color
  las = 2, # Rotate x-axis labels vertically for readability
  cex.names = 0.7 # Adjust label size if needed
)

# A more polished version of the bar char, this time with ggplot
ggplot(ny_population, aes(x = reorder(NAME, value), y = value)) +
  geom_col(fill = "#56B4E9") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) + # Format y-axis with commas
  labs(
    title = "New York County Populations (2020 Decennial Census)",
    subtitle = "Source: U.S. Census Bureau",
    x = "County",
    y = "Total Population"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 8)
  )


#exploration 4: race composition for NYC counties
# GEOIDs for NYC counties
#"36005", # Bronx
#"36047", # Kings (Brooklyn)
#"36061", # New York (Manhattan)
#"36081", # Queens
#"36085"  # Richmond (Staten Island)

#declare race total population + categories
race_vars <- c(
  Total = "P1_001N",
  White = "P1_003N",
  Black = "P1_004N",
  American_Indian = "P1_005N",
  Asian = "P1_006N",
  Pacific_Islander = "P1_007N",
  Other_Race = "P1_008N",
  Two_or_More_Races = "P1_009N"
)

#get decennial data using race variable declaration
race <- get_decennial(
  geography = "county",
  variables = race_vars,
  state = "NY",
  year = 2020,
  sumfile = "pl", # Specify the Redistricting Summary File
  output = "wide" # Get data in a wide format
)

#subset the data to just 5 NYC boroughs
nyc_race <- race %>%
  filter(GEOID %in% c("36005","36047","36061","36081","36085"))


# Reshape the data to longer format + calculate percentages
nyc_race_pct <- nyc_race %>%
  mutate(
    White_pct = White / Total,
    Black_pct = Black / Total,
    American_Indian_pct = American_Indian / Total,
    Asian_pct = Asian / Total,
    Pacific_Islander_pct = Pacific_Islander / Total,
    Other_Race_pct = Other_Race / Total,
    Two_or_More_Races_pct = Two_or_More_Races / Total
  ) %>%
  # Select only the borough name and percentage columns
  select(NAME, ends_with("_pct")) %>%
  # Reshape from wide to long format for plotting
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Race_Category",
    values_to = "Percentage"
  ) %>%
  # Rename NAME column to Borough and clean up the Race_Category names
  mutate(
    Borough = case_when(
      NAME == "Bronx County, New York" ~ "The Bronx",
      NAME == "Kings County, New York" ~ "Brooklyn",
      NAME == "New York County, New York" ~ "Manhattan",
      NAME == "Queens County, New York" ~ "Queens",
      NAME == "Richmond County, New York" ~ "Staten Island",
      TRUE ~ NAME
    ),
    Race_Category = str_replace(Race_Category, "_pct", "") %>%
      str_replace_all("_", " ") %>%
      # Reorder the levels of race categories
      factor(levels = c(
        "White", "Black", "Asian", "American Indian", "Pacific Islander",
        "Other Race", "Two or More Races"
      ))
  )

#review head result
head(nyc_race_pct)

# Create the stacked bar chart with ggplot
ggplot(nyc_race_pct, aes(x = Borough, y = Percentage, fill = Race_Category)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Racial Composition of NYC Boroughs (2020 Census)",
    x = "Borough",
    y = "Percentage",
    fill = "Race Category",
    caption = "Source: 2020 US Decennial Census, PL 94-171"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_fivethirtyeight() + # Use a clean, readable theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )

#extra credit - time permitting

#call the housing data
ny_housing <- get_decennial(
  geography = "county",
  variables = c(
    occupied = "H1_002N",
    vacant   = "H1_003N"
  ),
  state = "NY",
  year = 2020,
  output = "wide")

#review head result
head(ny_housing)

#reshape the results to long format
ny_housing_long <- ny_housing %>%
  pivot_longer(
    cols = c(occupied, vacant),
    names_to = "status",
    values_to = "count"
  ) %>%
  group_by(NAME) %>%
  mutate(percentage = count / sum(count))

#plot the reshaped dataframe
ggplot(ny_housing_long, aes(x = reorder(NAME, -count, sum), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Vacant vs. Occupied Housing Units in New York State Counties (2020)",
    x = "County",
    y = "Number of Units",
    fill = "Housing Status"
  ) +
  scale_fill_manual(values = c("occupied" = "steelblue", "vacant" = "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  scale_y_continuous(labels = scales::comma)

#save dataframes for later usage
save(ny_housing, file = "ny_housing.RData"); save(ny_population, file = "ny_population.RData"); save(pop_data, file = "pop_data.RData"); save(race, file = "race.RData"); save(urban_rural, file = "urban_rural.RData")

