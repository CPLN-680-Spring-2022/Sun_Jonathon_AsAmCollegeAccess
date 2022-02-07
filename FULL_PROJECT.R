#Cleaning Data

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# Data Import and Cleaning --------------------------------
source("ShapeFiles.R")
source("SafeGraph.R")
source("Census_Data.R")
source("Gmaps_Locations.R")
source("IPEDS_DATA.R")
source("Carnegie_Classification.R")

# Figures -----------------------------
source("Figures_Plots.R")
