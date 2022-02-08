
# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# IPEDS Race Enrollment Data ----------------------------------
FE <- list.files("Data/IPEDS Fall Enrollment Data")
files <- paste0("Data/IPEDS Fall Enrollment Data/",FE)

Columns <- c("unitid",
             "efalevel",
             "line",
             "section",
             "lstudy",
             "efrace17",
             "efrace18",
             "efrace19",
             "efrace20",
             "efrace21",
             "efrace22",
             "efrace23",
             "efrace24")

Years <- seq(from = 2002, to = 2020, by = 1)

Fall_Enrollment <- read_csv(files[1]) %>%
  select(Columns) %>%
  rename_with(tolower) %>%
  mutate(Years = Years[1])


for(i in 2:6){  
  A <- read_csv(files[i]) %>%
    rename_with(tolower) %>%
    select(Columns) %>%
    mutate(Years = Years[i])
  
  Fall_Enrollment <- rbind(Fall_Enrollment,A)                      
}  


## IPEDS data has two different naming conventions

Colnames2007 <- c("unitid",
                  "efalevel",
                  "line",
                  "section",
                  "lstudy",
                  "efnralt",
                  "efbkaat",
                  "efaiant",
                  "efasiat",
                  "efhispt",
                  "efwhitt",
                  "efunknt",
                  "eftotlt")

A <- read_csv(files[7]) %>%
  rename_with(tolower) %>%
  mutate(Years = Years[i]) %>%
  select(starts_with("ef"))
colnames(A)

A <- read_csv(files[7]) %>%
  rename_with(tolower) %>%
  select(Colnames2007) %>%
  mutate(Years = Years[i])

for(i in 7:length(files)){  
  B <- read_csv(files[i]) %>%
    rename_with(tolower) %>%
    select(Colnames2007) %>%
    mutate(Years = Years[i])
  
  A <- rbind(A,B)
  rm(B)
}  
col.from <- Colnames2007
cols.to <- Columns

A <- A %>%
  rename_at(vars(col.from), function(x) cols.to) 

df_clean <- rbind(
  Fall_Enrollment %>%
    filter(efalevel %in% c(23)) %>% 
    select(unitid, starts_with("ef"),Years) %>%
    pivot_longer(starts_with("ef")) %>%
    mutate(Level_Student = "Undergraduate"),
  Fall_Enrollment %>%
    filter(efalevel %in% c(32)) %>% 
    select(unitid, starts_with("ef"),Years) %>%
    pivot_longer(starts_with("ef")) %>%
    mutate(Level_Student = "Graduate"))

A <- rbind(
  A %>%
    filter(efalevel %in% c(23)) %>% 
    select(unitid, starts_with("ef"),Years) %>%
    pivot_longer(starts_with("ef")) %>%
    mutate(Level_Student = "Undergraduate"),
  A %>%
    filter(efalevel %in% c(32)) %>% 
    select(unitid, starts_with("ef"),Years) %>%
    pivot_longer(starts_with("ef")) %>%
    mutate(Level_Student = "Graduate"))

IPEDS_Race <- rbind(df_clean, A) %>%
  filter(!name %in% c("efalevel")) %>%
  mutate(name = str_replace(name,"efrace17","Nonresident alien"),
         name = str_replace(name,"efrace18","Black non-Hispanic"),
         name = str_replace(name,"efrace19","American Indian/Alaska Native"),
         name = str_replace(name,"efrace20","Asian or Pacific Islander "),
         name = str_replace(name,"efrace21","Hispanic"),
         name = str_replace(name,"efrace22","White non-Hispanic"),
         name = str_replace(name,"efrace23","Race/ethnicity unknown"),
         name = str_replace(name,"efrace24","Total_Enrollment"),
         Years = ymd(paste(Years,"0101"))) %>%
  rename(Race = name,
         Frequency = value) 
