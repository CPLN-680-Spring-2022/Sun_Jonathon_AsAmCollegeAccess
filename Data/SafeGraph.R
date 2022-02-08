
#Cleaning Data

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# SafeGraph Point data -------------------
Philly_Education <- read.csv("Data\\geometry.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(crs = "EPSG:4326")%>%
  select(!c(1,2,4,6,7,9,10,11,13,14)) %>%
  mutate(Higher_Education = ifelse(location_name %in% Universities$NAME | 
                                     street_address %in% Universities$STREET |
                                     grepl("University of Penn", location_name) == TRUE, TRUE, FALSE),
         Free_Library = ifelse(grepl("Free Library", location_name) == TRUE, TRUE, FALSE),
         Kumon = ifelse(grepl("Kumon", location_name) == TRUE, TRUE, FALSE),
         High_School = ifelse(grepl("Hs", location_name) == TRUE, TRUE, FALSE),
         Middle_School = ifelse(grepl("\\<Ms\\>", location_name) == TRUE, TRUE, FALSE),
         Elementary_School = ifelse(grepl("\\<Sch\\>", location_name) == TRUE, TRUE, FALSE),
         Charter_School = ifelse(grepl("\\<Cs\\>", location_name) == TRUE, TRUE, FALSE))

Philly_Education_Long <- pivot_longer(Philly_Education, 7:13, names_to = "Type_of_Ed", values_to = "Type_of_Ed_Yes") %>%
  filter(Type_of_Ed_Yes == TRUE) %>%
  select(!Type_of_Ed_Yes)

Philly_Education_Long_FALSE <- pivot_longer(Philly_Education, 7:13, names_to = "Type_of_Ed", values_to = "Type_of_Ed_Yes") %>%
  filter(Type_of_Ed_Yes == FALSE) %>%
  mutate(Type_of_Ed = "Not_Categorized") %>%
  select(!Type_of_Ed_Yes) %>%
  unique()

Philly_Education_Long <- rbind(Philly_Education_Long,Philly_Education_Long_FALSE)