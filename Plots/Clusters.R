#Clusters

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

df <- left_join(ACS_Cluster_Group,
          Philadelphia_tracts %>%
            select(GEOID,NAME.x),
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
  scale_fill_gradient(low = "black",
                      high = "white") +
  labs(title = "East Asian Population Clusters") +
  mapTheme()

ggsave("Plots/East_Asian_HOLC.jpg", dpi = 600)

ggplot() +
  geom_sf(data = df, aes(fill = Southeast_Asian_Asian)) +
  scale_fill_viridis() +
  ggnewscale::new_scale_color() +
  geom_sf(data = Philadelphia_HOLC,
          aes(color = holc_grade),
          fill = "transparent") +
  scale_fill_gradient(low = "black",
                      high = "white") +
  labs(title = "South East Asian Population Clusters") +
  mapTheme()

ggsave("Plots/SouthEast_Asian_HOLC.jpg")

df <- ACS.Long %>%
  filter(Year == "2018-01-01") %>%
  filter(Race_Ethnicity == unique(ACS.Long$Race_Ethnicity)[4])

df %>%
  mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)) %>%
  ggplot() +
  geom_sf( aes(fill = Southeast_Asian_Asian,
               color = IncomeEduc_Cluster)) +
  scale_fill_gradient(low = "black",
                      high = "white") +
  geom_sf(data = df_centroid %>%
            mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)),
          aes(color = IncomeEduc_Cluster,
              shape = IncomeEduc_Cluster)) +
  labs(title = "Income Education Clusters on Southeast Asian Data") +
  mapTheme()
ggsave("Plots/Income_Educ_Cluster.jpg", dpi = 600)

df %>%
  mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)) %>%
  dplyr::select(GEOID,ends_with("income"),ends_with("attainment"),IncomeEduc_Cluster) %>%
  pivot_longer(cols = 2:7, names_to = "Income_Educ", values_to = "Frequency") %>%
  ggplot() +
  geom_sf( aes(fill = Frequency ,
               color = IncomeEduc_Cluster)) +
  scale_fill_gradient(low = "black",
                      high = "white") +
  geom_sf(data = df_centroid %>%
            mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)),
          aes(color = IncomeEduc_Cluster,
              shape = IncomeEduc_Cluster)) +
    facet_wrap(~Income_Educ) +
  labs(title = "Income and Education variables in K-cluster") +
  mapTheme()

ggsave("Plots/Income_Educ_facet.jpg", width = 25, height = 20, units = c("in"), dpi = 600)



 
