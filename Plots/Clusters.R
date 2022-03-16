#Clusters

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

df <- left_join(ACS_Cluster_Group,
          Philadelphia_tracts %>%
            select(GEOID,NAME),
          by = c("GEOID")) %>%
        st_as_sf()

df_centroid <- left_join(ACS_Cluster_Group,
                         Philadelphia_Centroids %>%
                           select(GEOID,NAME),
                         by = c("GEOID")) %>%
                st_as_sf()

      

ggplot() +
  geom_sf(data = df, aes(fill = East_Asian_Asian)) +
  scale_fill_viridis() +
  geom_sf(data = Philadelphia_HOLC,
          aes(color = holc_grade),
          fill = "transparent") +
  ggnewscale::new_scale_color() +
  geom_sf(data = Philly_Schools %>%
                    filter(year == 2018), 
                 aes(shape = Overall.Tier,
                     color = Overall.Tier)) +
  labs(title = "East Asian Population Clusters") +
  mapTheme()

ggsave("Plots/East_Asian.jpg")

ggplot() +
  geom_sf(data = df, aes(fill = Southeast_Asian_Asian)) +
  scale_fill_viridis() +
  geom_sf(data = Philadelphia_HOLC,
          aes(color = holc_grade),
          fill = "transparent") +
  ggnewscale::new_scale_color() +
  geom_sf(data = Philly_Schools %>%
            filter(year == 2018), 
          aes(shape = Overall.Tier,
              color = Overall.Tier)) +
  labs(title = "South East Asian Population Clusters") +
  mapTheme()

ggsave("Plots/SouthEast_Asian.jpg")

df <- ACS.Long %>%
  filter(Year == "2018-01-01") %>%
  filter(Race_Ethnicity == unique(ACS.Long$Race_Ethnicity)[4])

clusters <- df %>%
              dplyr::select(Edu_Inc_Cluster) %>%
              mutate(Edu_Inc_Cluster = as.factor(Edu_Inc_Cluster)) %>%
              st_centroid()




 
