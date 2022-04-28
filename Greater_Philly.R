if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

getwd()
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


#
source("Data/ShapeFiles.R")
source("Data/ACS_Cluster.R")
source("Data/Census_Data.R")
source("Data_Analysis/K-clustering.R")
source("Data_Analysis/Morans_I.R")
source("Data_ANalysis/ANOVA.R")