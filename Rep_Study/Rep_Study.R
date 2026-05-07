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
library(scales)


#Austin TX MSA Boundary
tx_counties <- counties(
  state = "TX",
  year = 2020,
  cb = TRUE
) %>%
  st_transform(5070)

austin_msa <- tx_counties %>%
  filter(NAME %in% c("Bastrop", "Caldwell", "Hays", "Travis", "Williamson")) %>%
  summarise()

tx_tracts <- get_decennial(
  geography = "tract",
  variables = "P1_001N", # total population 
  state = "TX",
  year = 2020,
  geometry = TRUE
) %>%
  st_transform(5070) 

#Clip tracts to Austin MSA
austin_tracts <- tx_tracts %>% 
  st_filter(austin_msa)

#Count Austin tracts
nrow(austin_tracts)

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
      levels = c("Exurban", "Suburban Low", "Suburban High", "Urban Low", 
                 "Urban High")
    )
  )

# Map 1: using Hanberry Landscape Classifications
tx1 <- ggplot(austin_tracts) +
  geom_sf(aes(fill = landscape), color = "grey10") +
  scale_fill_brewer(palette = "YlOrRd", name = "Landscape") +
  labs(
    title = "Metropolitan Areas of Austin, TX",
    subtitle = "Hanberry landscape classification by census tract, 2020", 
  ) +
  theme_void()

print(tx1)

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
  st_filter(austin_msa)

# Join to landscape data 
austin_income <- austin_income %>%
  select(GEOID, estimate, geometry) %>%
  rename(med_income = estimate) %>%
  left_join(
    austin_tracts %>% st_drop_geometry() %>% select(GEOID, landscape),
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
  theme_void()

print(tx2)

ggsave("austin_income_map.png", plot = tx2, width = 10, height = 8, dpi = 300) 

#------------------------------------------------------------------------------

# Boxchart of Population by Landscape for Austin, TX

austin_pop <- austin_tracts %>%
  st_drop_geometry() %>%
  dplyr::select(
    GEOID,
    landscape,
    population = value
  )

txbox1 <- ggplot(austin_pop, aes(x = landscape, y = population, fill = landscape)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title = "Population Distribution by Landscape, Austin TX",
    x = NULL,
    y = "Total Population per Census Tract",
    caption = "Source: 2020 Decennial Census"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(txbox1)

ggsave("austin_population_boxplot.png",
       plot = txbox1, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------------------------

austin_counties <- c(
  "Travis",
  "Williamson",
  "Hays",
  "Bastrop",
  "Caldwell"
)

# Get ACS age-sex table
tx_age <- get_acs(
  geography = "tract",
  table = "B01001",
  state = "TX",
  county = austin_counties,
  year = 2020,
  survey = "acs5",
  geometry = FALSE
)

austin_landscape <- austin_tracts %>%
  st_drop_geometry() %>%
  select(GEOID, landscape) %>%
  mutate(GEOID = as.character(GEOID))

# Match GEOID type
tx_age <- tx_age %>%
  mutate(GEOID = as.character(GEOID))

# Join landscape classifications
tx_age <- tx_age %>%
  left_join(austin_landscape, by = "GEOID")

vars <- load_variables(2020, "acs5", cache = TRUE)

b01001_labels <- vars %>%
  filter(str_detect(name, "^B01001")) %>%
  select(name, label)

tx_age <- tx_age %>%
  left_join(
    b01001_labels,
    by = c("variable" = "name")
  )

tx_age <- tx_age %>%
  filter(str_detect(label, "Male|Female")) %>%
  filter(!str_detect(label, "Total:$")) %>%
  mutate(
    sex = ifelse(
      str_detect(label, "Male"),
      "Male",
      "Female"
    ),
    
    age_group = label %>%
      str_remove("Estimate!!Total:!!") %>%
      str_remove("Male:!!") %>%
      str_remove("Female:!!")
  ) %>%
  filter(
    age_group != "",
    age_group != "Male:",
    age_group != "Female:"
  )

tx_age %>%
  filter(landscape %in% c("Urban Low", "Urban High")) %>%
  select(age_group, sex, estimate, landscape) %>%
  head()

# Manually setting the order of age groups
age_levels <- c(
  "Under 5 years",
  "5 to 9 years",
  "10 to 14 years",
  "15 to 17 years",
  "18 and 19 years",
  "20 years",
  "21 years",
  "22 to 24 years",
  "25 to 29 years",
  "30 to 34 years",
  "35 to 39 years",
  "40 to 44 years",
  "45 to 49 years",
  "50 to 54 years",
  "55 to 59 years",
  "60 and 61 years",
  "62 to 64 years",
  "65 and 66 years",
  "67 to 69 years",
  "70 to 74 years",
  "75 to 79 years",
  "80 to 84 years",
  "85 years and over"
)

tx_age <- tx_age %>%
  mutate(
    age_group = factor(age_group,
                       levels = age_levels)
  )

# Urban pyramid data
austin_urban_pyramid <- tx_age %>%
  filter(landscape %in% c("Urban Low", "Urban High")) %>%
  group_by(age_group, sex) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    population = ifelse(
      sex == "Male",
      -population,
      population
    )
  )

# Urban Pyramid plot
austin_urban_plot <- ggplot(
  austin_urban_pyramid,
  aes(
    x = population,
    y = age_group,
    fill = sex
  )
) +
  geom_col(width = 0.9) +
  scale_x_continuous(
    labels = \(x) abs(x)
  ) +
  scale_fill_manual(values = c(
    "Male" = "lightblue",
    "Female" = "pink"
  )) +
  labs(
    title = "Urban Population Pyramid of Austin, Texas",
    subtitle = "Austin MSA Urban Tracts, 2020",
    x = "Population",
    y = NULL
  ) +
  theme_minimal()

print(austin_urban_plot)

ggsave(
  "austin_urban_pyramid.png",
  plot = austin_urban_plot,
  width = 10,
  height = 8,
  dpi = 300
)

# Suburban pyramid data
austin_suburban_pyramid <- tx_age %>%
  filter(landscape %in% c("Suburban Low", "Suburban High")) %>%
  group_by(age_group, sex) %>%
  summarize(
    population = sum(estimate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    population = ifelse(
      sex == "Male",
      -population,
      population
    )
  )

# Suburban Pyramid Plot
austin_suburban_plot <- ggplot(
  austin_suburban_pyramid,
  aes(
    x = population,
    y = age_group,
    fill = sex
  )
) +
  geom_col(width = 0.9) +
  scale_x_continuous(
    labels = \(x) abs(x)
  ) +
  scale_fill_manual(values = c(
    "Male" = "lightblue",
    "Female" = "pink"
  )) +
  labs(
    title = "Suburban Population Pyramid of Austin, Texas",
    subtitle = "Austin MSA Suburban Tracts, 2020",
    x = "Population",
    y = NULL
  ) +
  theme_minimal()

print(austin_suburban_plot)

ggsave(
  "austin_suburban_pyramid.png",
  plot = austin_suburban_plot,
  width = 10,
  height = 8,
  dpi = 300
)





























