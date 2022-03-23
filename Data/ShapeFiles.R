if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# Universities
Remove <- c("OBJECTID","CBSA","NMCBSA","CBSATYPE","CSA","SLDU","SCHOOLYEAR","NECTA","NMNECTA","CD","SLDL","NMCSA","LOCALE")

Universities <- st_read("https://opendata.arcgis.com/datasets/a15e8731a17a46aabc452ea607f172c0_0.geojson") %>%
  dplyr::select(!Remove) %>%
  filter(STATE == "PA") %>%
  filter(NMCNTY == "Philadelphia County") %>%
  st_as_sf()

ALL_Universities <- st_read("https://opendata.arcgis.com/datasets/a15e8731a17a46aabc452ea607f172c0_0.geojson") %>%
  dplyr::select(!Remove) %>%
  st_as_sf()
# TIGRIS Philadelphia Census Tracts -------------------------------
Philadelphia_tracts <- tracts(state = "PA", county = "Philadelphia County") %>%
  st_transform(st_crs(Universities))

#ggplot() +
#  geom_sf(data = Philadelphia_tracts)

# Census Tract centroids as points -------------------------------------
Philadelphia_Centroids <- Philadelphia_tracts %>%
  st_centroid() %>%
  st_transform(st_crs(Universities))

#ggplot() +
#  geom_sf(data = Philadelphia_Centroids)

# TIGRIS Philadelphia School Districts ---------------------------------
#Philadelphia_School_District <- school_districts(state = "PA", type = "unified") %>%
 # st_transform(st_crs(Universities)) %>%
 # st_intersection(st_union(Philadelphia_tracts))

Philadelphia_School_District <- county_subdivisions(state = "PA",
                                           county = "Philadelphia") %>%
                                st_transform(st_crs(Universities))


#ggplot() +
#  geom_sf(data = Philadelphia_School_District)

Philadelphia_HOLC <- st_read("https://dsl.richmond.edu/panorama/redlining/static/downloads/geojson/PAPhiladelphia1937.geojson") %>%
                        st_intersection(Philadelphia_School_District)

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = Philadelphia_HOLC, 
          aes(fill = holc_grade)) +
  mapTheme()
