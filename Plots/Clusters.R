#Clusters

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

df <- left_join(ACS_Cluster_Group,
                Philadelphia_tracts %>%
                  dplyr::select(GEOID,NAME),
                by = c("GEOID")) %>%
      st_as_sf() %>%
      dplyr::select(-NAME.y,-NAME.x) 
df <- df %>%
      pivot_longer(2:(length(colnames(df))-6), 
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


## Scatter plots -----------------
df <- left_join(ACS_Cluster_Group,
                Philadelphia_tracts %>%
                  dplyr::select(GEOID,NAME),
                by = c("GEOID")) %>%
  st_as_sf() %>%
  dplyr::select(-NAME.y,-NAME.x) 
df %>%
  select(-Black_Race,-okinwan_Asian)  %>%
  pivot_longer(c(2,3,10),names_to = "Income", values_to = "Income_values") %>%
  pivot_longer(c(3,6,11),names_to = "Education", values_to = "Education_values")%>%
  glimpse()
# Clustering Data -------------------

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
 
# Scaled IncomeEduc_Cluster -----------------------

scaled <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,NAME,ends_with("income"),ends_with("attainment"),IncomeEduc_Cluster)

Cbind <- data.frame(scale(scaled[-c(1:2,9)]))

scaled <- cbind(scaled[c(1:2,9)],
                Cbind) 

scaled %>%
  pivot_longer(cols = 4:9, names_to = "Income_Educ", values_to = "Frequency") %>%
  group_by(IncomeEduc_Cluster, Income_Educ) %>%
  summarize(Average = mean(Frequency)) %>%
  mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster)) %>%
  ggplot() +
  geom_bar(aes(x = Income_Educ, 
               y = Average,
               fill = IncomeEduc_Cluster),
           stat = "identity",
           position = "dodge") +
  labs(title = "Average values in each cluster") +
  coord_flip() +
  plotTheme()

# Faceted by group ---------------------------
df <- scaled %>%
  pivot_longer(cols = 7:9, names_to = "Education", values_to = "Educ_Scaled") %>%
  pivot_longer(cols = 4:6, names_to = "Income", values_to = "Income_scaled") %>%
  mutate(IncomeEduc_Cluster = as.factor(IncomeEduc_Cluster),
         IncomeEduc_Cluster = fct_relevel(IncomeEduc_Cluster,"1","2","3"),
         Variable_Pair = paste0(Education,"&",Income)) 


##faceted Clusters by pairing -------------------------------

for(i in 1:length(unique(df$Variable_Pair))) {
  plot <- df %>%
    filter(Variable_Pair == unique(df$Variable_Pair)[i])
  
  title <- plot$Variable_Pair[1]
  x <- plot$Education[1]
  y <- plot$Income[1]
  
  plot %>%
    ggplot(aes(x = Educ_Scaled, y = Income_scaled, 
               color = IncomeEduc_Cluster,
               shape = IncomeEduc_Cluster)) +
    geom_point(size = 2) +
    facet_wrap(~IncomeEduc_Cluster) +
    labs(title = paste0(title),
         x = paste0(x),
         y = paste0(y)) +
    plotTheme()
  
  ggsave(paste0("Plots/Cluster_plots/Income_Educ/",title,".jpg"), dpi = 600)
  
}


# Scaled Race_cluster -----------------------

scaled <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,NAME,ends_with("income"),ends_with("attainment"),Race_Cluster)

Cbind <- data.frame(scale(scaled[-c(1:2,9)]))

scaled <- cbind(scaled[c(1:2,9)],
                Cbind) 

scaled %>%
  pivot_longer(cols = 4:9, names_to = "Income_Educ", values_to = "Frequency") %>%
  group_by(Race_Cluster, Income_Educ) %>%
  summarize(Average = mean(Frequency)) %>%
  mutate(Race_Cluster = as.factor(Race_Cluster),
         Race_Cluster = fct_relevel(Race_Cluster,"1","2","3","4","5","6")) %>%
  ggplot() +
  geom_bar(aes(x = Income_Educ, 
               y = Average,
               fill = Race_Cluster),
           stat = "identity",
           position = "dodge") +
  labs(title = "Average values in each cluster") +
  coord_flip() +
  plotTheme()


df <- scaled %>%
  pivot_longer(cols = 7:9, names_to = "Education", values_to = "Educ_Scaled") %>%
  pivot_longer(cols = 4:6, names_to = "Income", values_to = "Income_scaled") %>%
  mutate(Race_Cluster = as.factor(Race_Cluster),
         Race_Cluster = fct_relevel(Race_Cluster,"1","2","3","4","5","6"),
         Variable_Pair = paste0(Education,"&",Income)) 

for(i in 1:length(unique(df$Variable_Pair))) {
  plot <- df %>%
    filter(Variable_Pair == unique(df$Variable_Pair)[i])
  
  title <- plot$Variable_Pair[1]
  x <- plot$Education[1]
  y <- plot$Income[1]
  
  plot %>%
    ggplot(aes(x = Educ_Scaled, y = Income_scaled, 
               color = Race_Cluster,
               shape = Race_Cluster)) +
    geom_point(size = 2) +
    facet_wrap(~Race_Cluster) +
    labs(title = paste0(title),
         x = paste0(x),
         y = paste0(y)) +
    plotTheme()
  
  ggsave(paste0("Plots/Cluster_plots/Race/",title,".jpg"), dpi = 600)
  
}

left_join(scaled,
          Philadelphia_tracts %>%
            dplyr::select(GEOID),
          by = c("GEOID")) %>%
  mutate(Race_Cluster = as.factor(Race_Cluster),
         Race_Cluster = fct_relevel(Race_Cluster,"1","2","3","4","5","6")) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf( aes(fill = Race_Cluster)) +
  mapTheme()


# Moran's I Clusters -------------------------

## Bar Charts ------------------------
for(i in 1:length(unique(df$variables))) {
  
  title <- unique(df$variables)[i]
  
  df %>%
    pivot_longer(2:6, names_to = "Cluster_Group", values_to = "Type_of_Cluster") %>%
    mutate(Type_of_Cluster = as.character(Type_of_Cluster),
           Type_of_Cluster = str_replace_all(Type_of_Cluster,"AAPI_Race", "variable"),
           Type_of_Cluster = str_replace_all(Type_of_Cluster,"East_Asian_Asian", "variable"),
           Type_of_Cluster = str_replace_all(Type_of_Cluster,"Filipino_Asian", "variable"),
           Type_of_Cluster = str_replace_all(Type_of_Cluster,"South_Asian_Asian","variable"),
           Type_of_Cluster = str_replace_all(Type_of_Cluster,"Southeast_Asian_Asian","variable"),
           Type_of_Cluster = as.factor(Type_of_Cluster)) %>%
    filter(variables == unique(df$variables)[i]) %>%
    ggplot( aes(fill = Type_of_Cluster, y = frequency, x = variables)) +
    geom_bar(position = "dodge",
             stat = "identity") +
    facet_wrap(~Cluster_Group) +
    labs(title = paste0("Asian Cluster by ",title)) +
    plotTheme()
  
  ggsave(paste0("Plots/AAPI_Clusters/",title,".jpg"), width = 10, dpi = 600)
}

## Income_Educ charts ------------------
scaled <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,NAME,ends_with("income"),ends_with("attainment"),ends_with("Cluster"))

Cbind <- data.frame(scale(scaled[c(3:8)]))

scaled <- cbind(scaled[-c(3:8)],
                Cbind) 

df <- scaled %>%
  pivot_longer(cols = 11:13, names_to = "Education", values_to = "Educ_Scaled") %>%
  pivot_longer(cols = 8:10, names_to = "Income", values_to = "Income_scaled") %>%
  pivot_longer(cols = 3:7, names_to = "Cluster_Group", values_to = "Type_of_Cluster") %>%
  mutate(Type_of_Cluster = as.character(Type_of_Cluster),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"AAPI_Race", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"East_Asian_Asian", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"Filipino_Asian", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"South_Asian_Asian","variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"Southeast_Asian_Asian","variable"),
         Type_of_Cluster = replace_na(Type_of_Cluster,"No_Relationship"),
         Type_of_Cluster = as.factor(Type_of_Cluster),
         Variable_Pair = paste0(Education," & ",Income))

## Income_Educ Loop ------------------------
for(i in 1:length(unique(df$Variable_Pair))) {
  
  for(j in 1:length(unique(as.character(df$Type_of_Cluster)))){
    
    cluster_type <- unique(as.character(df$Type_of_Cluster))[j]
    
    plot <- df %>%
      filter(Variable_Pair == unique(df$Variable_Pair)[i]) %>%
      filter(Type_of_Cluster == cluster_type)
    
    title <- plot$Variable_Pair[1]
    x <- plot$Education[1]
    y <- plot$Income[1]
    
    
    plot %>%
      ggplot(aes(x = Educ_Scaled, y = Income_scaled, 
                 color = Cluster_Group,
                 shape = Cluster_Group)) +
      geom_point(size = 2) +
      facet_wrap(~Cluster_Group) +
      labs(title = paste0(title),
           subtitle = paste0(cluster_type),
           x = paste0(x),
           y = paste0(y)) +
      theme(legend.position = "none") +
      plotTheme()
    
    ggsave(paste0("Plots/Cluster_plots/Moran_I/",title,"_",cluster_type,".jpg"), width = 10, dpi = 600)
  }
}

## Income and Median values --------------------------------------
scaled <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,NAME,ends_with("income"),ends_with("attainment"),ends_with("Cluster")) %>%
  left_join(ACS_Cluster_Group_Medians,
                    by = "GEOID")

Cbind <- data.frame(scale(scaled[c(3:8,14:20)]))

scaled <- cbind(scaled[-c(3:8,14:20)],
                Cbind) 
df <- scaled %>%
  pivot_longer(cols = 11:13, names_to = "Education", values_to = "Educ_Scaled") %>%
  pivot_longer(cols = 8:10, names_to = "Income", values_to = "Income_scaled") %>%
  pivot_longer(cols = 3:7, names_to = "Cluster_Group", values_to = "Type_of_Cluster") %>%
  pivot_longer(cols = 3:9, names_to = "Medians", values_to = "Medians_Scaled") %>%
  mutate(Type_of_Cluster = as.character(Type_of_Cluster),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"AAPI_Race", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"East_Asian_Asian", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"Filipino_Asian", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"South_Asian_Asian","variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"Southeast_Asian_Asian","variable"),
         Type_of_Cluster = replace_na(Type_of_Cluster,"No_Relationship"),
         Type_of_Cluster = as.factor(Type_of_Cluster),
         Medians = str_replace_all(Medians,"[.]"," "),
         Variable_Pair = paste0(Education," & ",Income),
         Variable_Pair_Median_Ed = paste0(Medians, " & ", Education))

for(i in 1:length(unique(df$Variable_Pair_Median_Ed))) {
  
  for(j in 1:length(unique(as.character(df$Type_of_Cluster)))){
    
    cluster_type <- unique(as.character(df$Type_of_Cluster))[j]
    
    plot <- df %>%
      filter(Variable_Pair_Median_Ed == unique(df$Variable_Pair_Median_Ed)[i]) %>%
      filter(Type_of_Cluster == cluster_type)
    
    title <- plot$Variable_Pair_Median_Ed[1]
    x <- plot$Education[1]
    y <- plot$Medians[1]
    
    
    plot %>%
      ggplot(aes(x = Educ_Scaled, y = Income_scaled, 
                 color = Cluster_Group,
                 shape = Cluster_Group)) +
      geom_point(size = 2) +
      facet_wrap(~Cluster_Group) +
      labs(title = paste0(title),
           subtitle = paste0(cluster_type),
           x = paste0(x),
           y = paste0(y)) +
      theme(legend.position = "none") +
      plotTheme()
    
    ggsave(paste0("Plots/Cluster_plots/Moran_I_Medians/",title,"_",cluster_type,".jpg"), width = 10, dpi = 600)
  }
}

Similar <- ACS_Cluster_Group %>%
              dplyr::select(GEOID,ends_with("Cluster")) %>%
            mutate(AAPI_Cluster = as.character(AAPI_Cluster),
                   AAPI_Cluster = str_replace_all(AAPI_Cluster,"AAPI_Race", "variable"),
                   AAPI_Cluster = replace_na(AAPI_Cluster,"No_Relation"),
                   East_Asian_Cluster = as.character(East_Asian_Cluster),
                   East_Asian_Cluster = str_replace_all(East_Asian_Cluster,"East_Asian_Asian", "variable"),
                   East_Asian_Cluster = replace_na(East_Asian_Cluster,"No_Relation"),
                   Filipino_Asian_Cluster = as.character(Filipino_Asian_Cluster),
                   Filipino_Asian_Cluster = str_replace_all(Filipino_Asian_Cluster,"Filipino_Asian", "variable"),
                   Filipino_Asian_Cluster = replace_na(Filipino_Asian_Cluster,"No_Relation"),
                   SouthAsian_Cluster = as.character(SouthAsian_Cluster),
                   SouthAsian_Cluster = str_replace_all(SouthAsian_Cluster,"South_Asian_Asian","variable"),
                   SouthAsian_Cluster = replace_na(SouthAsian_Cluster,"No_Relation"),
                   Southeast_Asian_Cluster = as.character(Southeast_Asian_Cluster),
                   Southeast_Asian_Cluster = str_replace_all(Southeast_Asian_Cluster,"Southeast_Asian_Asian","variable"),
                   Southeast_Asian_Cluster = replace_na(Southeast_Asian_Cluster,"No_Relation"),
                   AAPI_East_Same = paste0(AAPI_Cluster,East_Asian_Cluster),
                   AAPI_Filipino_Same = paste0(AAPI_Cluster,Filipino_Asian_Cluster),
                   AAPI_Southeast_Same = paste0(AAPI_Cluster,Southeast_Asian_Cluster),
                   AAPI_South_Same = paste0(AAPI_Cluster,SouthAsian_Cluster),
                   East_Filipino_Same = paste0(East_Asian_Cluster,Filipino_Asian_Cluster),
                   East_South_Same = paste0(East_Asian_Cluster,SouthAsian_Cluster),
                   East_Southeast_Same = paste0(East_Asian_Cluster, Southeast_Asian_Cluster),
                   Filipino_South_Same = paste0(Filipino_Asian_Cluster, SouthAsian_Cluster),
                   Filipino_Southeast_Same = paste0(Filipino_Asian_Cluster, Southeast_Asian_Cluster),
                   South_Southeast_Same = paste0(SouthAsian_Cluster, Southeast_Asian_Cluster)
                   )

# Counts of similar areas -------------------------

Counts <- Similar %>%
          dplyr::select(NAME,GEOID,ends_with("Cluster")) %>%
          pivot_longer(3:7,names_to = "Clusters",values_to = "Cluster_cat") %>%
          filter(NAME == unique(Similar$NAME)[1]) %>%
          group_by(GEOID, Cluster_cat) %>%
          summarize(Total = n())

for(i in 2:length(unique(Similar$NAME))) {
  merge <- Similar %>%
    dplyr::select(NAME,GEOID,ends_with("Cluster")) %>%
    pivot_longer(3:7,names_to = "Clusters",values_to = "Cluster_cat") %>%
    filter(NAME == unique(Similar$NAME)[i]) %>%
    group_by(GEOID, Cluster_cat) %>%
    summarize(Total = n())
  
  Counts <- rbind(Counts,merge)
}

Counts <- Counts %>%
            mutate(Total = factor(Total, levels = c("0","1","2","3","4","5")))

### Plots ---------------------------

Counts %>%
  group_by(Cluster_cat,Total) %>%
  summarize(Frequency = n()) %>%
  mutate(Cluster_cat = str_replace(Cluster_cat,"variable","frequency,"),
         Cluster_cat = str_to_sentence(Cluster_cat)) %>%
  ggplot( aes(x = Total, y = Frequency, fill = Cluster_cat)) +
  geom_bar(stat = "identity") +
  labs(title = "Overlap of Clustering by Racial Groups") +
  plotTheme()

ggsave("Plots/Cluster_plots/Overlaping_Clustering.jpg",width = 10, dpi = 600)

A <- Counts %>%
      mutate(Total = as.numeric(Total)-1) %>%
      group_by(GEOID,Cluster_cat) %>%
      slice(which.max(Total)) 

### Trying to pull only the max values for each GEOID --------------------

#A <- Counts %>%  
#      mutate(Total = as.numeric(Total)-1) %>%
#      filter(GEOID == unique(Philadelphia_tracts$GEOID)[1]) %>%
#      filter(Total > 1) %>%
#      slice(1)

#for(i in 2:length(unique(Philadelphia_tracts$GEOID))){
  
#  Merge <- Counts %>%  
#    mutate(Total = as.numeric(Total)-1) %>%
#    filter(GEOID == unique(Philadelphia_tracts$GEOID)[i]) %>%
#    filter(Total > 1)
  
#  if(nrow(Merge) >= 2) {
#    Merge_Wide <- Merge %>%
#      pivot_wider(names_from = Cluster_cat, values_from = Total)
    
#        if(Merge_Wide$No_Relation[1] > Merge_Wide[1,2] == TRUE) {
#          Merge <- Merge_Wide %>%
#            dplyr::select(1,2) %>%
#            pivot_longer(2,names_to = "Cluster_cat", values_to = "Total")
#          A <- rbind(A,Merge)
          
#        } else {
#          Merge <- Merge_Wide %>%
#            dplyr::select(1,3) %>%
#            pivot_longer(2,names_to = "Cluster_cat", values_to = "Total")
#          A <- rbind(A,Merge)
#        }
    
#  } else {
#    A <- rbind(A,Merge)
#  }
#}



  

# Overlapping by pairs --------------------------
AAPI_Same <- Similar %>%
              dplyr::select(GEOID,NAME,ends_with("Same")) %>%
              pivot_longer(3:12) 


### Overlap by specific cluster --------------

AAPI_Same <- AAPI_Same %>%
                mutate(value = case_when(
                                  value %in% unique(AAPI_Same$value)[1] == TRUE ~ "NA",
                                  value %in% unique(AAPI_Same$value)[2] == TRUE ~ "Both High Frequency, but different clustering",
                                  value %in% unique(AAPI_Same$value)[3] == TRUE ~ "High Frequency, low clustering, but no overlap",
                                  value %in% unique(AAPI_Same$value)[4] == TRUE ~ "High Frequency, high clustering, but no overlap",
                                  value %in% unique(AAPI_Same$value)[5] == TRUE ~ "No overlap, but High Frequency and High clustering",
                                  value %in% unique(AAPI_Same$value)[6] == TRUE ~ "Both High Frequency, High clustering",
                                  value %in% unique(AAPI_Same$value)[7] == TRUE ~ "No relation, but low frequency, and low clustering",
                                  value %in% unique(AAPI_Same$value)[8] == TRUE ~ "Low frequency, and low clustering, but no overlap",
                                  value %in% unique(AAPI_Same$value)[9] == TRUE ~ "Both Low Frequency, Low Clustering",
                                  value %in% unique(AAPI_Same$value)[10] == TRUE ~ "No relation, but high frequency, and low clustering",
                                  value %in% unique(AAPI_Same$value)[11] == TRUE ~ "Low Frequency, High Clustering Clustering, but no overlap",
                                  value %in% unique(AAPI_Same$value)[12] == TRUE ~ "Both High Frequency, Low Clustering",
                                  value %in% unique(AAPI_Same$value)[-c(6,9,12)] == TRUE ~ "Not Similar"
                ))

AAPI_Same[AAPI_Same == "NA"] <- NA

left_join(AAPI_Same,
          Philadelphia_tracts %>%
            dplyr::select(GEOID),
          by = "GEOID") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = value)) +
  facet_wrap(~name) +
  mapTheme()
            

### Easier to look at Clustering overlaps ------------------

AAPI_Same <- AAPI_Same %>%
  mutate(value = case_when(
    value %in% unique(AAPI_Same$value)[1] == TRUE ~ "NA",
    value %in% unique(AAPI_Same$value)[c(6,9,12)] == TRUE ~ "Similar",
    value %in% unique(AAPI_Same$value)[-c(6,9,12)] == TRUE ~ "Not Similar"
  ))

AAPI_Same[AAPI_Same == "NA"] <- NA

left_join(AAPI_Same,
          Philadelphia_tracts %>%
            dplyr::select(GEOID),
          by = "GEOID") %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = value)) +
  facet_wrap(~name) +
  mapTheme()

### Same scatter plot analysis as before, but with unique geometric locations -------------------------
scaled <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,NAME,ends_with("income"),ends_with("attainment"),ends_with("Cluster"))

Cbind <- data.frame(scale(scaled[c(3:8)]))

scaled <- cbind(scaled[-c(3:8)],
                Cbind) 

df <- scaled %>%
  pivot_longer(cols = 11:13, names_to = "Education", values_to = "Educ_Scaled") %>%
  pivot_longer(cols = 8:10, names_to = "Income", values_to = "Income_scaled") %>%
  pivot_longer(cols = 3:7, names_to = "Cluster_Group", values_to = "Type_of_Cluster") %>%
  mutate(Type_of_Cluster = as.character(Type_of_Cluster),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"AAPI_Race", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"East_Asian_Asian", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"Filipino_Asian", "variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"South_Asian_Asian","variable"),
         Type_of_Cluster = str_replace_all(Type_of_Cluster,"Southeast_Asian_Asian","variable"),
         Type_of_Cluster = replace_na(Type_of_Cluster,"No_Relationship"),
         Type_of_Cluster = as.factor(Type_of_Cluster),
         Variable_Pair = paste0(Education," & ",Income))


df <- left_join(AAPI_Same %>%
                  na.omit(),
                  df,
                  by = c("GEOID","NAME")) %>%
        dplyr::select(-c("Cluster_Group","Type_of_Cluster")) %>%
        rename(Cluster_Pair = name,
               Similarity = value)

for(i in 1:length(unique(df$Variable_Pair))) {
  plot <- df %>%
    filter(Variable_Pair == unique(df$Variable_Pair)[i]) %>%
    filter(Similarity == "Not Similar")
  
  title <- plot$Variable_Pair[1]
  x <- plot$Education[1]
  y <- plot$Income[1]
  
  plot %>%
    ggplot(aes(x = Educ_Scaled, y = Income_scaled, 
               color = Similarity,
               shape = Similarity)) +
    geom_point(size = 2) +
    facet_wrap(~Cluster_Pair) +
    labs(title = paste0(title),
         x = paste0(x),
         y = paste0(y)) +
    plotTheme()
  
  ggsave(paste0("Plots/Cluster_plots/Moran_I_Similarity/",title,"_",cluster_type,".jpg"), width = 10, dpi = 600)
  }


