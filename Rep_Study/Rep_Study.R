# Author:Raylen Krouse
# Goal: Read Replication Study instructions and create visual variables for 
# your own city of choice
# Date: 5/6/2026
# Table 4 Notes: wildlands, Inhabited, Exurban 
# low, Exurban high, Urban low, Urban high, suburban low, suburban high, also 
# has landscape class as well as population density thresholds
# Density measured in people per square km

#Github Link: https://github.com/RaylenK03/Replication_Study


install.packages("tidycensus")
install.packages("tidyverse")
install.packages("mapview")
install.packages("tmap")
install.packages("tigris")
install.packages("crsuggest")
install.packages("ggplot2")
install.packages("spdep")
install.packages("matrixStats")
install.packages("rmapshaper")
install.packages("SpatialAcc")

#Libraries

library("tidycensus")
library("tidyverse")
library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(mapview)
library(sf)
library(tmap)
library(crsuggest)
library(mapboxapi)
library(ggplot2)
library(spdep)
library(rmapshaper)
library(matrixStats)
library(SpatialAcc)

#Austin TX MSA Boundary
tx_cbsa <- cored_based_statistical_areas(year = 2020, cb = TRUE)

austin_msa <- tx_cbsa %<% 
  filter(NAME == "Austin, TX Metro Area") %<%
  st_transform(5070)

tx_tracts -> get_decennial(
  geography = "tract",
  variables = "P1_001N", # total population 
  state = "TX",
  year = 2020,
  geometry = TRUE
) %>% 
  st_transform(5070)

#Clip tracts to Austin MSA
austin_tracts <- tx_tracts %>% 
  st_intersection(austin_msa)

#Calculate tract area and density with mutation
austin_tracts <- austin_tracts %>% 
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1000000, 
    pop_density = value / area_km2
  )

#Landscape Classes
austin_tracts <- austin_tracts %>% 
  mutate(
    landscape = case_when(
    pop_density < 250 ~ "Exurban",
    pop_density >= 250 & pop_density < 550 ~ "Suburban Low",
    pop_density >= 550 & pop_density < 800 ~ "Suburban High",
    pop_density >= 800 & pop_density < 1900 ~ "Urban Low",
    pop_density >= 1900 ~ "Urban High"
    ),
    landscape = factor(
      landscape,
      levels = c("Exurban", "Suburban Low", "Suburban High", "Suburban Low", 
                "Urban Low", "Urban High")
    )
  )
# Map 1: using Hanberry Landscape Classifications
tx1 <- ggplot(austin_tracts) +
  geom_sf(aes(fill = landscape), color = NA) +
  scale_fill_brewer(palette = "YlOrRd")
  name = "landscape" + 
    labs (
      title = "Metropolitan Landscape of Austin, TX",
      subtitle = "Hanberry Landscape Classification by census tract, 2020"
    ) +
    theme_minimal()

ggsave("austin_metro_landscape.png", 
       plot = tx1, width = 10, height = 8, dpi = 300)

#------------------------------------------------------------------------------

# Median Household Income 
income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "TX",
  year = 2020,
  survey = "acs5",
  geometry = TRUE
) %>%
  st_transform(5070)

# Clip to Austin MSA 
austin_income <- income %>%
  st_intersection(austin_msa)

# Join to landscape data 
austin_income <- austin_income %>%
  select(GEOID, estimate, geometry) %>%
  rename(med_income = estimate) %>%
  left_join(
    austin_tracts %>% st_drop_geometry(),
    select(GEOID, Landscape),
    by = "GEOID"
    )

# Map 2: Using the Median Household Income Variable
  
tx2 <- ggplot(austin_income) + 
  geom_sf(aes(fill = med_income), color = NA) +
  scale_fill_viridis_c(labels = dollar_format(), name = "Income") + 
  labs (
    title = "Median Household Income in Austin, TX", 
    caption = "Source: ACS 2016-2020, Census Tracts"
  ) +
  theme_minimal()

ggsave("austin_income_map.png", plot = tx2, width = 10, height = 8, dpi = 300) 

#------------------------------------------------------------------------------

# Graph of Income by Landscape




















































