
# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

#Geocoded data using Google Maps ---------------------------------------

Locations <- c("Vietlead Philadelphia","Asian Americans United Philadelphia")

locations_LatLon <- geocode(location = Locations)
locations_Shp <- locations_LatLon %>% 
  rename(Longitude = lon,
         Latitude = lat) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(Universities)) %>%
  cbind(Locations)
