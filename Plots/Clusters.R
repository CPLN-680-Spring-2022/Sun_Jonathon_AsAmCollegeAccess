#Clusters

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

df <- left_join(ACS_Cluster_Group,
                Philadelphia_tracts %>%
                  dplyr::select(GEOID,NAME.x),
                by = c("GEOID")) %>%
      st_as_sf() %>%
      dplyr::select(-NAME.x)%>%
      pivot_longer(3:(length(colnames(df))-2), 
                names_to = "variables",
                values_to = "frequency")

philly_roads <- roads("PA", "Philadelphia")

for(i in 1:length(unique(df$variables))){
  df %>%
    filter(variables == unique(df$variables)[i]) %>%
    ggplot() +
    geom_sf(aes(fill = frequency)) +
    scale_fill_viridis() +
    geom_sf(data = Philadelphia_HOLC,
            aes(color = holc_grade),
            fill = "transparent") +
    ggnewscale::new_scale_color() +
    scale_fill_gradient(low = "black",
                        high = "white") +
    labs(title = paste0(unique(df$variables)[i])) +
    mapTheme()
  
  ggsave(paste0("Plots/HOLC_Maps/",unique(df$variables)[i],".jpg"), width = 10, height = 10, units = c("in"), dpi = 600)
}

for(i in 1:length(unique(df$variables))){
  df %>%
    filter(variables == unique(df$variables)[i]) %>%
    ggplot() +
    geom_sf(aes(fill = frequency)) +
    scale_fill_viridis() +
    scale_fill_gradient(low = "black",
                        high = "white") +
    geom_sf(data = GTFS_Rail_route,
            color = "green") +
    geom_sf(data = GTFS_Rail_points,
            color = "green",
            fill = "green",
            size = 3,
            shape = 25) +
    geom_sf(data = philly_roads %>%
              filter(RTTYP %in% c("I","S","U")) %>%
              rename(Road_Type = RTTYP),
            aes(color = Road_Type)) +
    ggnewscale::new_scale_color() +
    geom_sf(data = Universities %>%
              filter(YEARS == "2020-01-01")%>%
              dplyr::select(NAME, SHORT_CARNEGIE)%>%
              unique(),
            aes(color = SHORT_CARNEGIE),
            size = 3) +
    labs(title = paste0(unique(df$variables)[i])) +
    mapTheme()
  
  ggsave(paste0("Plots/University_Transit/",unique(df$variables)[i],".jpg"), width = 10, height = 10, units = c("in"), dpi = 600)
}

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = Philadelphia_HOLC,
          aes(fill = holc_grade)) +
  scale_fill_manual(values = c("A" = "Green",
                               "B" = "Yellow",
                               "C" = "Orange",
                               "D" = "Red")) +
  geom_sf(data = GTFS_Rail_route,
          color = "Black") +
  geom_sf(data = GTFS_Rail_points,
          color = "Black",
          fill = "black",
          size = 3,
          shape = 25) +
  geom_sf(data = philly_roads %>%
            filter(RTTYP %in% c("I","S","U")) %>%
            rename(Road_Type = RTTYP),
          aes(color = Road_Type)) +
  labs(title = "Rail Lines over HOLC corporation") +
  mapTheme()

ggsave(paste0("Plots/University_Transit/HOLC_Rail.jpg"), width = 10, height = 10, units = c("in"), dpi = 600)


ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = Philadelphia_HOLC,
          aes(fill = holc_grade)) +
  scale_fill_manual(values = c("A" = "Green",
                               "B" = "Yellow",
                               "C" = "Orange",
                               "D" = "Red")) +
  geom_sf(data = GTFS_Bus_route %>%
                    filter(Transit_form == "Subway"),
          color = "Black") +
  geom_sf(data = GTFS_Subway_stops_route,
          color = "Black",
          fill = "black",
          size = 3,
          shape = 25) +
  labs(title = "Subway over HOLC corporation") +
  mapTheme()

ggsave(paste0("Plots/University_Transit/HOLC_Subway.jpg"), width = 10, height = 10, units = c("in"), dpi = 600)

ggplot() +
  geom_sf(data = Philadelphia_School_District) +
  geom_sf(data = Philadelphia_HOLC,
          aes(fill = holc_grade)) +
  scale_fill_manual(values = c("A" = "Green",
                               "B" = "Yellow",
                               "C" = "Orange",
                               "D" = "Red")) +
  ggnewscale::new_scale_color() +  
  geom_sf(data = philly_roads %>%
            filter(RTTYP %in% c("I","S","U")) %>%
            rename(Road_Type = RTTYP),
          aes(color = Road_Type),
          size = 1.5) +
  ggnewscale::new_scale_color() +
  geom_sf(data = GTFS_Rail_route %>%
                  mutate(Rail = "Rail"),
          aes(color = Rail)) +
  scale_color_manual(values = "purple") +
  geom_sf(data = GTFS_Rail_points,
          color = "Purple",
          fill = "Purple",
          size = 3,
          shape = 25) +
  ggnewscale::new_scale_color() +
  geom_sf(data = GTFS_Bus_route %>%
            filter(Transit_form == "Subway") %>%
            rename(Subway = Transit_form),
          aes(color = Subway)) +
  scale_color_manual(values = "black") +
  geom_sf(data = GTFS_Subway_stops_route,
          color = "Black",
          fill = "black",
          size = 3,
          shape = 25) +
  labs(title = "Subway and Regional Rails over HOLC loan Designation") +
  mapTheme()

ggsave(paste0("Plots/University_Transit/HOLC_Subway_Regional_Rail.jpg"), width = 10, height = 10, units = c("in"), dpi = 600)


# Clustering Data 

df <- left_join(ACS_Cluster_Group,
                Philadelphia_tracts %>%
                  dplyr::select(GEOID,NAME.x),
                by = c("GEOID")) %>%
  st_as_sf()

df_centroid <- left_join(ACS_Cluster_Group,
                         Philadelphia_Centroids %>%
                           dplyr::select(GEOID,NAME),
                         by = c("GEOID")) %>%
  st_as_sf()

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

ACS_Cluster_Group %>%
  mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)) %>%
  dplyr::select(GEOID,ends_with("income"),ends_with("attainment"),IncomeEduc_Cluster) %>%
  pivot_longer(cols = 3:8, names_to = "Income_Educ", values_to = "Frequency") %>%
  group_by(IncomeEduc_Cluster, Income_Educ) %>%
  summarize(Average = mean(Frequency)) %>%
  pivot_wider(names_from = Income_Educ,
              values_from = Average) %>%
  View()

ACS_Cluster_Group %>%
  mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)) %>%
  dplyr::select(GEOID,ends_with("income"),ends_with("attainment"),IncomeEduc_Cluster) %>%
  pivot_longer(cols = 3:8, names_to = "Income_Educ", values_to = "Frequency") %>%
  group_by(IncomeEduc_Cluster, Income_Educ) %>%
  summarize(Average = mean(Frequency)) %>%
  ggplot() +
  geom_bar(aes(x = Income_Educ, 
               y = Average,
               fill = IncomeEduc_Cluster),
           stat = "identity",
           position = "dodge") +
  labs(title = "Average values in each cluster") +
  coord_flip() +
  plotTheme()


