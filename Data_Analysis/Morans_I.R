#Moran's I

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(sp,ISLR,MASS,spatstat,spatial,maptools,ppp,fossil,adehabitHR,gdata,raster,rgdal,geostatst, spdep, caret)

library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
library(scales)


source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
# Log transform the population to even out the skew

ACS.Long %>%
  filter(Race_Ethnicity == "Asian_alone") %>%
  mutate(Frequency_log = log(Frequency)) %>%
  ggplot() +
  geom_histogram( aes(x = Frequency_log))

df <- ACS.Long %>% 
        filter(Year == "2017-01-01") %>%
        filter(Race_Ethnicity == unique(ACS.Long$Race_Ethnicity[2])) %>%
        na.omit() %>%
        st_as_sf()

nb <- poly2nb(df,
               queen = TRUE)

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

#Moran's I the hard way ------------------------------
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
inc.lag <- lag.listw(lw, ACS.Long %>%
                            filter(Race_Ethnicity == "Asian_alone") %>%
                            select(Race_Ethnicity))
inc.lag <- lag.listw(lw, df$Frequency)

plot(inc.lag ~ df$Frequency, pch = 16, asp = 1)
M1 = lm(inc.lag ~df$Frequency)
abline(M1, col = "blue")
coef(M1)[2]

#Moran's I the easy way -----------------------------
moran.test(df$Frequency,lw)
MC <- moran.mc(df$Frequency,lw,nsim=599)
plot(MC,main ="", las=1)
