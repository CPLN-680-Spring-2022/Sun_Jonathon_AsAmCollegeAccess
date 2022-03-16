#Cleaning Data

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

getwd()
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

#Functions -----------------



# Data Import and Cleaning --------------------------------
source("Data/ShapeFiles.R")
source("Data/SafeGraph.R")
source("Data/Census_Data.R")
source("Data/ACS_Cluster.R")
source("Data/Gmaps_Locations.R")
source("Data/IPEDS_DATA.R")
source("Data/Carnegie_Classification.R")
source("Data/Philly_School_metrics.R")
source("Data/GTFS_Data.R")
source("Data/Knn.R")

#Analysis
source("Data_Analysis/Summary_Stats.R")
source("Data_Analysis/Morans_I.R")
source("Data_Analysis/K-clustering.R")

#Figures -----------------------------
source("Plots/Clusters.R") #Visualize clusters and other Characteristics
source("Plots/IPEDS_FIGS.R") #Ipeds Data
source("Plots/Philly_k12.R") #Philly K-12 schools
source("Plots/Census_Charts.R") #Census data
 