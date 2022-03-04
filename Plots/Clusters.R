#Clusters

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")



df <- ACS.Long %>%
  filter(Year == "2018-01-01") %>%
  filter(Race_Ethnicity == unique(ACS.Long$Race_Ethnicity)[4])

clusters <- df %>%
              dplyr::select(Edu_Inc_Cluster) %>%
              mutate(Edu_Inc_Cluster = as.factor(Edu_Inc_Cluster)) %>%
              st_centroid()

ggplot() +
  geom_sf(data = df,
          aes(fill = Frequency)) +
  geom_sf(data = clusters, aes(shape = Edu_Inc_Cluster,
                               color = Edu_Inc_Cluster)) +
  mapTheme()


 
