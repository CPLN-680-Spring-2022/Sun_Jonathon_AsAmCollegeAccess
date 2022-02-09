if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

path_data <- file.path("OTP")
dir.create(path_data) 

path_otp <- otp_dl_jar()

log1 <- otp_build_graph(otp = path_otp , dir = path_data, memory = 10240) 

log2 <- otp_setup(otp = path_otp, dir = path_data)

otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)