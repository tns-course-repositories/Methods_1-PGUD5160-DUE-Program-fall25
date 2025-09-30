#c6_lab6-script-3.r
#Load PUMA Preview Map

#print file path helper - step 1 > 2
#getwd() #return upstream path before ~/
#list.files("~/Desktop/lab_6", recursive = TRUE, include.dirs = TRUE, full.names = TRUE)

# ---- Packages ----
library(sf)
library(tidyverse)

# ---- 2. Read shapefile ----
pumas <- st_read("~/Desktop/lab_6/data/nypuma2020_25c/nypuma2020.shp")

# ---- 3. Inspect ----
print(st_crs(pumas))
print(head(pumas))

# ---- 4. Plot with labels ----
ggplot(pumas) +
  geom_sf(fill = "grey90", color = "black", linewidth = 0.3) +
  geom_sf_text(aes(label = PUMA), size = 3, color = "#850101") +
  theme_minimal() +
  labs(
    title = "NYC PUMA Boundaries",
    subtitle = "Labeled by PUMA identifier",
    caption = "Source: NYC DCP Public Use Microdata Areas"
  )
