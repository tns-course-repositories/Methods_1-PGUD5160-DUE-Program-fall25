#install packages as needed
install.packages(c("tidycensus", "tidyverse", "ggthemes"))

#load libraries
library(tidycensus)
library(tidyverse)
library(ggthemes)

# ---- Census API Key ----
#replace with your actual key. Set install = TRUE to save it for all future sessions.
census_api_key("YOUR_API_KEY_HERE", install = TRUE)

#load the NTA to Census Tract Equivalency file 
NTA_CT <- read.csv("~/Desktop/assignment_3/data/2020_Census_Tracts_to_2020_NTAs_and_CDTAs_Equivalency_20250903.csv")

#view the table
View(NTA_CT)

#suppress scientific notation
options(scipen = 999)

#extract the Lower East Side GEOIDS as numbers
les_GEOID_num <- NTA_CT$GEOID[NTA_CT$NTAName == "Lower East Side"]

#convert to character format to match with tidycensus GEOID character format
les_GEOID_chr <- as.character(les_GEOID_num)

#print the results to the Console for viewing
print(les_GEOID_chr)

#declare the race variables needed for the tidycensus call
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


#make the tidycensus call using the declared race variables
race_data <- get_decennial(
  geography = "tract",
  variables = race_vars, #Specify race variables
  state = "NY",
  year = 2020,
  sumfile = "pl", # Specify the Redistricting Summary File
  output = "wide" # Get data in a wide format
) 

#filter the tidycensus data for just those GEOIDs for census tracts that make up LES
#for the comparison neighborhood, this is done a second time replacing the abbreviation for LES
#for the comparison neighborhood, make sure to create a new GEOID character vector, not reuse les_GEOID_chr
LES_race_data <- race_data %>% filter(GEOID %in% les_GEOID_chr)

#(remove "County, New York") from the NAME for better legibility when plotting
LES_race_data <- LES_race_data %>%
  mutate(
    NAME = str_extract(NAME, "Census Tract [0-9.]+")
  )

#create the final dataframe in long format that will pass to the plot
LES_race_data_evaluated <- LES_race_data %>%
  mutate(
    White_pct              = White / Total,
    Black_pct              = Black / Total,
    American_Indian_pct    = American_Indian / Total,
    Asian_pct              = Asian / Total,
    Pacific_Islander_pct   = Pacific_Islander / Total,
    Other_Race_pct         = Other_Race / Total,
    Two_or_More_Races_pct  = Two_or_More_Races / Total
  ) %>%
  # Keep original NAME and percentage columns
  select(NAME, ends_with("_pct")) %>%
  # Reshape from wide to long format for plotting
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Race_Category",
    values_to = "Percentage"
  ) %>%
  # Clean up Race_Category only
  mutate(
    Race_Category = str_replace(Race_Category, "_pct", "") %>%
      str_replace_all("_", " ") %>%
      factor(levels = c(
        "White", "Black", "Asian", "American Indian", "Pacific Islander",
        "Other Race", "Two or More Races"
      ))
  )

#make sure to save your cleaned, fully evaluated dataframe:
save(LES_race_data_evaluated, file = "~/Desktop/assignment_3/results/LES_race_data_evaluated.RData")



# Create the stacked bar chart for LES
#replace reference to LES for the comparison plot
ggplot(LES_race_data_evaluated, aes(x = NAME, y = Percentage, fill = Race_Category)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Racial Composition of Lower East Side Neighborhood (NTA) Census Tracts",
    x = "Census Tract",
    y = "Percentage",
    fill = "Race Category",
    caption = "Source: 2020 US Decennial Census, PL 94-171"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_fivethirtyeight() + # Use a clean, readable theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right",
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )





