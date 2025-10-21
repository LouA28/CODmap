#Load libraries     


library(readxl)
library(ggplot2)
library(plotly)
library(data.table)
library(DT)
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(scales)
library(lubridate)
library(sf)
library(countrycode)


# pref_data <- readRDS("CODdata/PUR_type of preference2025-09-24.RDS")

pref_data <- readRDS("CODdata/Cod_data2025-10-21.RDS")

## Map

map_sf <- st_read("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

# Check if geometries are valid
map_sf$valid <- st_is_valid(map_sf)

# Fix invalid geometries
map_sf_fixed <- map_sf %>%
  mutate(geometry = ifelse(valid, geometry, st_make_valid(geometry))) %>%
  select(-valid)

map_sf_centroids <- map_sf_fixed %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  select(NAME, lon, lat, ISO_A2) %>%
  mutate(ISO_A2 = case_when(
    NAME == "France" ~ "FR",
    NAME == "Norway" ~ "NO",
    NAME == "Kosovo" ~ "XK",  # Standard ISO code for Kosovo
    TRUE ~ ISO_A2  # Keep other values unchanged
  )) %>%
  filter(ISO_A2 != "-99")

map_sf_centroids <- map_sf_centroids %>%
  mutate(
    lon = ifelse(NAME == "France", 2.0, lon),
    lat = ifelse(NAME == "France", 46.5, lat)
  )

# Prepare coords lookup
coo_coords <- map_sf_centroids %>%
  select(cooalpha = ISO_A2, lat1 = lat, lon1 = lon)

cod_coords <- map_sf_centroids %>%
  select(codalpha = ISO_A2, lat2 = lat, lon2 = lon)

# Get UK coords once
uk_coords <- map_sf_centroids %>%
  filter(ISO_A2 == "GB") %>%
  select(lat3 = lat, lon3 = lon) %>%
  slice(1)

# Merge everything
final_importPURTEST <- pref_data %>%
  left_join(coo_coords, by = "cooalpha") %>%
  left_join(cod_coords, by = "codalpha") %>%
  mutate(lat3 = uk_coords$lat3, lon3 = uk_coords$lon3,
         ukalpha = "GB")


final_importPURTEST$country_destination <- countrycode(final_importPURTEST$ukalpha, origin = "genc2c", destination = "country.name")

cod_data <- final_importPURTEST %>%
  select(Year, CN8, CN8_desc, country_origin, country_dispatch,country_destination,
         cooalpha, codalpha, ukalpha, combocode, Pref_Trade, Eligible_Trade, Total_imp, PUR, lat1, lon1, lat2, lon2, lat3, lon3)


saveRDS(cod_data, "CODdata/cod_data.RDS")


# Precompute all dropdown values
country_choices <- sort(unique(cod_data$country_origin))

# Precompute mapping: country → years
years_by_country <- cod_data %>%
  group_by(country_origin) %>%
  summarise(years = list(sort(unique(Year))), .groups = "drop")

# Precompute mapping: country & year → chapters
chapters_by_country_year <- cod_data %>%
  mutate(HS2 = substr(CN8, 1, 2)) %>%
  group_by(country_origin, Year) %>%
  summarise(chapters = list(sort(unique(HS2))), .groups = "drop")

# Precompute mapping: country, year, chapter → CN8
cn8_by_country_year_chapter <- cod_data %>%
  mutate(HS2 = substr(CN8, 1, 2)) %>%
  group_by(country_origin, Year, HS2) %>%
  summarise(cn8 = list(sort(unique(CN8))), .groups = "drop")


saveRDS(country_choices, "CODdata/country_choices.RDS")
saveRDS(years_by_country, "CODdata/years_by_country.RDS")
saveRDS(chapters_by_country_year, "CODdata/chapters_by_country_year.RDS")
saveRDS(cn8_by_country_year_chapter, "CODdata/cn8_by_country_year_chapter.RDS")