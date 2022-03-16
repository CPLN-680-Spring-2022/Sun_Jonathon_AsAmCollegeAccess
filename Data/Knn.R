#Cleaning Data

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(caret, ckanr, FNN, ggcorplot, grid, gridExtra, gtools, httr, kableExtra, modelsummary, tidyverse, tidycensus, sf,  spdep, stargazer, tmap, viridis, zipcodeR, DT)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

getwd()
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


## Unviersity Nearest Neighbor
st_c <- st_coordinates

nndf <- ALL_Universities 

nnpoints <- Philadelphia_Centroids
subject <- "University"              


## University NN
Knn <- nnpoints %>%
        mutate(University_nn1 = nn_function(st_c(nnpoints), st_c(nndf), 1),
               University_nn2 = nn_function(st_c(nnpoints), st_c(nndf), 2),
               University_nn3 = nn_function(st_c(nnpoints), st_c(nndf), 3),
               University_nn4 = nn_function(st_c(nnpoints), st_c(nndf), 4),
               University_nn5 = nn_function(st_c(nnpoints), st_c(nndf), 5)) %>%
        select(GEOID,starts_with("University")) %>%
        st_drop_geometry()

Knn$University_nnAve <- rowMeans(Knn[,c(2:6)], na.rm=TRUE)

join_knn <- function(df){left_join(df,
                                   Knn, 
                                   by = "GEOID")}
ACS_Cluster_Group <- join_knn(ACS_Cluster_Group)
ACS.Long <- join_knn(ACS.Long)
ACS_Cluster <- join_knn(ACS_Cluster)


nndf <- Philly_Education_Long %>%
            filter(Type_of_Ed == "Tutoring_Sup") %>%
            st_as_sf()

nnpoints <- Philadelphia_Centroids
subject <- "Tutoring"       

## Tutoring
Knn <- nnpoints %>%
  mutate(Tutoring_nn1 = nn_function(st_c(nnpoints), st_c(nndf), 1),
         Tutoring_nn2 = nn_function(st_c(nnpoints), st_c(nndf), 2),
         Tutoring_nn3 = nn_function(st_c(nnpoints), st_c(nndf), 3),
         Tutoring_nn4 = nn_function(st_c(nnpoints), st_c(nndf), 4),
         Tutoring_nn5 = nn_function(st_c(nnpoints), st_c(nndf), 5)) %>%
  select(GEOID,starts_with(subject)) %>%
  st_drop_geometry()

Knn$Tutoring_nnAve <- rowMeans(Knn[,c(2:6)], na.rm=TRUE)

ACS_Cluster_Group <- join_knn(ACS_Cluster_Group)
ACS.Long <- join_knn(ACS.Long)
ACS_Cluster <- join_knn(ACS_Cluster)

# Distance to intervene Schools
nnpoints <- Philadelphia_Centroids
subject <- "INTERVENE"  

nndf <- Philly_Schools %>%
          filter(year == 2018) %>%
          filter(Overall.Tier == "INTERVENE") %>%
          st_as_sf()

## intervene Schools NN
Knn <- nnpoints %>%
  mutate(INTERVENE_nn1 = nn_function(st_c(nnpoints), st_c(nndf), 1),
         INTERVENE_nn2 = nn_function(st_c(nnpoints), st_c(nndf), 2),
         INTERVENE_nn3 = nn_function(st_c(nnpoints), st_c(nndf), 3),
         INTERVENE_nn4 = nn_function(st_c(nnpoints), st_c(nndf), 4),
         INTERVENE_nn5 = nn_function(st_c(nnpoints), st_c(nndf), 5)) %>%
  select(GEOID,starts_with("INTERVENE")) %>%
  st_drop_geometry()

Knn$Intervene_nnAve <- rowMeans(Knn[,c(2:6)], na.rm=TRUE)

ACS_Cluster_Group <- join_knn(ACS_Cluster_Group)
ACS.Long <- join_knn(ACS.Long)
ACS_Cluster <- join_knn(ACS_Cluster)

nndf <- Philly_Schools %>%
  filter(year == 2018) %>%
  filter(Overall.Tier == unique(Philly_Schools$Overall.Tier)[2]) %>%
  st_as_sf()

## Watch Schools NN
Knn <- nnpoints %>%
  mutate(WATCH_nn1 = nn_function(st_c(nnpoints), st_c(nndf), 1),
         WATCH_nn2 = nn_function(st_c(nnpoints), st_c(nndf), 2),
         WATCH_nn3 = nn_function(st_c(nnpoints), st_c(nndf), 3),
         WATCH_nn4 = nn_function(st_c(nnpoints), st_c(nndf), 4),
         WATCH_nn5 = nn_function(st_c(nnpoints), st_c(nndf), 5)) %>%
  select(GEOID,starts_with("WATCH")) %>%
  st_drop_geometry()

Knn$Watch_nnAve <- rowMeans(Knn[,c(2:6)], na.rm=TRUE)


ACS_Cluster_Group <- join_knn(ACS_Cluster_Group)
ACS.Long <- join_knn(ACS.Long)
ACS_Cluster <- join_knn(ACS_Cluster)

nndf <- Philly_Schools %>%
  filter(year == 2018) %>%
  filter(Overall.Tier == unique(Philly_Schools$Overall.Tier)[4]) %>%
  st_as_sf()

## intervene Schools NN
Knn <- nnpoints %>%
  mutate(REINFORCE_nn1 = nn_function(st_c(nnpoints), st_c(nndf), 1),
         REINFORCE_nn2 = nn_function(st_c(nnpoints), st_c(nndf), 2),
         REINFORCE_nn3 = nn_function(st_c(nnpoints), st_c(nndf), 3),
         REINFORCE_nn4 = nn_function(st_c(nnpoints), st_c(nndf), 4),
         REINFORCE_nn5 = nn_function(st_c(nnpoints), st_c(nndf), 5)) %>%
  select(GEOID,starts_with("REINFORCE")) %>%
  st_drop_geometry()

Knn$Reinforce_nnAve <- rowMeans(Knn[,c(2:6)], na.rm=TRUE)


ACS_Cluster_Group <- join_knn(ACS_Cluster_Group)
ACS.Long <- join_knn(ACS.Long)
ACS_Cluster <- join_knn(ACS_Cluster)

ACS_Cluster_Group <- ACS_Cluster_Group %>%
                        select(-ends_with("nn1"),
                               -ends_with("nn2"),
                               -ends_with("nn3"),
                               -ends_with("nn4"),
                               -ends_with("nn5"))

