# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate)

FE <- list.files("Data/Philadelphia_school_metrics")
files <- paste0("Data/Philadelphia_school_metrics/",FE)

df <- read_excel(files[3]) %>%
          select(1:3,starts_with("overall")) %>%
          mutate(Year = 2014)

import_school_metric <- function(x,year){
                              df <- read_excel(files[x], sheet = 4) %>%
                                select(1:4,`Street Address`,starts_with("overall"),-`Rpt Type Long`)
                            for(i in 4:6){
                              bind <- read_excel(files[x], sheet = i) %>%
                                select(1:4,`Street Address`, starts_with("overall"),-`Rpt Type Long`)
                              df <- rbind(df,bind)
                            }
                              df <- df %>%
                                      mutate(year = year)
                              }
# I have to manually check to see what kind of codes were made becauase
# I have no idea what is stored in these sheets
#2015 and 2016 ----------------
df <- import_school_metric(4,2015)
colnames(df)
bind <- import_school_metric(5,2016)

df <- rbind(df,bind)

#2017 -----------------
bind <- import_school_metric(6, 2017) %>%
          select(-`ULCS Code`,-`Overall Pts Earn`,-`Overall Pts Poss`)

df <- rbind(df,bind)

#2018 ---------------
bind <- import_school_metric(7, 2018) %>%
  select(-`ULCS Code`,-`Overall Pts Earn`,-`Overall Pts Poss`)

df <- rbind(df,bind)

#Merging Safegraph lat_lon to K-12 Data
Philly_Schools <- st_read("Data/Philadelphia_schools_shp")

