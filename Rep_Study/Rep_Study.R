# Author:Raylen Krouse
# Goal: Read Replication Study instructions and create visual variables for 
# your own city of choice
# Date: 5/6/2026
# Table 4 Notes: Social class: Wildlands, Inhabted, Exurban 
# low, Exurban high, Urban low, Urban high, suburban low, suburban high, also 
# has class threshold as well as population density thresholds
# Density measured in people per square km



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

austin_tracts <- austin_tracts %>% 
  mutate(
    landscape = case_when(
    pop_density < 250 ~  
  )
)






















































