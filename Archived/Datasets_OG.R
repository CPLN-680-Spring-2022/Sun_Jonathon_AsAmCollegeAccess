
# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


# SafeGraph Point data -------------------
Philly_Education <- read.csv("Data\\geometry.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  st_transform(crs = "EPSG:4326")%>%
  select(!c(1,2,4,6,7,9,10,11,13,14)) %>%
  mutate(Higher_Education = ifelse(location_name %in% Universities$NAME | 
                                     street_address %in% Universities$STREET |
                                     grepl("University of Penn", location_name) == TRUE, TRUE, FALSE),
         Free_Library = ifelse(grepl("Free Library", location_name) == TRUE, TRUE, FALSE),
         Kumon = ifelse(grepl("Kumon", location_name) == TRUE, TRUE, FALSE),
         High_School = ifelse(grepl("Hs", location_name) == TRUE, TRUE, FALSE),
         Middle_School = ifelse(grepl("\\<Ms\\>", location_name) == TRUE, TRUE, FALSE),
         Elementary_School = ifelse(grepl("\\<Sch\\>", location_name) == TRUE, TRUE, FALSE),
         Charter_School = ifelse(grepl("\\<Cs\\>", location_name) == TRUE, TRUE, FALSE))

Philly_Education_Long <- pivot_longer(Philly_Education, 7:13, names_to = "Type_of_Ed", values_to = "Type_of_Ed_Yes") %>%
  filter(Type_of_Ed_Yes == TRUE) %>%
  select(!Type_of_Ed_Yes)

Philly_Education_Long_FALSE <- pivot_longer(Philly_Education, 7:13, names_to = "Type_of_Ed", values_to = "Type_of_Ed_Yes") %>%
  filter(Type_of_Ed_Yes == FALSE) %>%
  mutate(Type_of_Ed = "Not_Categorized") %>%
  select(!Type_of_Ed_Yes) %>%
  unique()

Philly_Education_Long <- rbind(Philly_Education_Long,Philly_Education_Long_FALSE)


# Census API Data ----------------------------


A <- load_variables(2019,
                    "acs5",
                    cache = FALSE) %>%
  filter(concept == "RACE")  %>%
  mutate(Merge_name = paste(name,"E", sep =""))

B <- load_variables(2019,
                    "acs5",
                    cache = FALSE) %>%
  filter(grepl("ASIAN ALONE BY SELECTED GROUPS", concept)) %>%
  mutate(Merge_name = paste(name,"E", sep ="")) %>%
  slice(-1)

Variables <- bind_rows(A,B) %>%
  mutate(
    label = str_remove(label,"Estimate!!"),
    label = str_remove(label,"Total:!!"),
    label = str_remove_all(label,"!!"),
    label = str_replace_all(label,":"," "),
    label = str_trim(label),
    label = str_replace_all(label," ","_"),
    label = str_replace(label,"Total","Total_Population")
  )

ACS.wide <-  get_acs(geography = "tract", 
                     variables = Variables$name, 
                     state = "PA",
                     county = "Philadelphia",
                     output = "wide",
                     geometry = TRUE,
                     year = 2019) %>% 
  dplyr::select (GEOID, NAME, ends_with("E")) %>%
  st_transform(st_crs(Universities))

col.from <- colnames(ACS.wide)

colnames(ACS.wide)

cols <- Variables$label
namestoAdd <- c("GEOID","NAME")
cols <- append(namestoAdd, cols)
cols.to <- append(cols, "geometry")

## this function below renames all the columns based on the two lists that we made. that being col.from the variables original names to cols.to are the names that we made from the list of census data.
ACS.wide  <- ACS.wide  %>%
  rename_at(vars(col.from), function(x) cols.to)
ACS.Long <- pivot_longer(ACS.wide,4:(ncol(ACS.wide)-1), names_to = "Race_Ethnicity", values_to = "Frequency") %>%
  mutate(Percentage = (Frequency/Total_Population)*100,
         Year = 2019)

Years <- seq(from = 2011, to = 2018, by = 1)

for(i in 1:length(Years)) {
  A <- load_variables(Years[i],
                      "acs5",
                      cache = FALSE) %>%
    filter(concept == "RACE")  %>%
    mutate(Merge_name = paste(name,"E", sep =""))
  
  B <- load_variables(Years[i],
                      "acs5",
                      cache = FALSE) %>%
    filter(grepl("ASIAN ALONE BY SELECTED GROUPS", concept)) %>%
    mutate(Merge_name = paste(name,"E", sep ="")) %>%
    slice(-1)
  
  Variables <- bind_rows(A,B) %>%
    mutate(
      label = str_remove(label,"Estimate!!"),
      label = str_remove(label,"Total:!!"),
      label = str_remove_all(label,"!!"),
      label = str_replace_all(label,":"," "),
      label = str_trim(label),
      label = str_replace_all(label," ","_"),
      label = str_replace(label,"Total","Total_Population")
    )  
  
  
  ACS.wide <-  get_acs(geography = "tract", 
                       variables = Variables$name, 
                       state = "PA",
                       county = "Philadelphia",
                       output = "wide",
                       geometry = TRUE,
                       year = Years[i]) %>% 
    dplyr::select (GEOID, NAME, ends_with("E")) %>%
    st_transform(st_crs(Universities))
  
  col.from <- colnames(ACS.wide)
  
  colnames(ACS.wide)
  
  cols <- Variables$label
  namestoAdd <- c("GEOID","NAME")
  cols <- append(namestoAdd, cols)
  cols.to <- append(cols, "geometry")  
  
  ACS.wide  <- ACS.wide  %>%
    rename_at(vars(col.from), function(x) cols.to)
  f <- pivot_longer(ACS.wide,4:(ncol(ACS.wide)-1), names_to = "Race_Ethnicity", values_to = "Frequency") %>%
    mutate(Percentage = (Frequency/Total_Population)*100,
           Year = Years[i])  
  ACS.Long <- rbind(ACS.Long,f)  
}

exclude <- c("15007990300", "15009990000", "15009991200", "15001990000", "15001990100", "15001990400", "15001990500", "15001990600", "15001990700", "15001990800", "15001990900", "15001991000", "15001991100","15001991200", "15001991300", "15001991400", "15001991500", "15001991600", "15001991700", "15005990000", "15003980300","15009990200","15003981200","15003980800","15001990300","15007990100","15003981300", "15007990200", "15003990001", "15007041200", "15009980000", "15003980000", "15003981000", "15003980700", "15003980600")

ACS.Long <- ACS.Long %>%
  filter(!GEOID %in% exclude) %>%
  mutate(Race_Ethnicity = str_remove(Race_Ethnicity,"Total_Population"),
         Race_Ethnicity = str_replace(Race_Ethnicity,"Chinese_,_except_Taiwanese","Chinese,_except_Taiwanese"),
         Race_Ethnicity = str_replace(Race_Ethnicity,"Two_or_more_racesTwo_races_including_Some_other_race","Two_or_more_races_Two_races_including_Some_other_race"),
         Race_Ethnicity = str_replace(Race_Ethnicity,"Two_or_more_racesTwo_races_excluding_Some_other_race,_and_three_or_more_races","Two_or_more_races_Two_races_excluding_Some_other_race,_and_three_or_more_races"))


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
# Carnegie Classification ---------------------
FE <- list.files("Data/Carnegie_Classification")
files <- paste0("Data/Carnegie_Classification/",FE)

Carnegie_Variables <- read_xlsx(files[2], 3)

Carnegie_Variables <- Carnegie_Variables %>%
  fill(Variable, Label...2) %>%
  mutate(Variable = toupper(Variable),
         Value = as.numeric(Value)) %>%
  filter(grepl("BASIC",Variable)) 

Carnegie_Classification <- read_xlsx(files[2], 4) %>%
  select(1, starts_with("basic")) 

Recode <- Carnegie_Variables %>%
  filter(Variable == "BASIC2021") %>%
  mutate(Numeric_code = Value + 1)

Carnegie_Classification <- Carnegie_Classification%>%
  mutate(basic2021 = basic2021 + 1,
         Carnegie_Classifcation2021 = recode(basic2021, !!!Recode$Label...4),
         Short_Carnegie = ifelse(str_detect(Carnegie_Classifcation2021,":") == TRUE, 
                                 gsub(":.*","",Carnegie_Classifcation2021),
                                 Carnegie_Classifcation2021))

IPEDS_Race <- left_join(IPEDS_Race,
                        Carnegie_Classification %>%
                          select(unitid,Carnegie_Classifcation2021, Short_Carnegie),
                        join_by = "unitid")

Universities <- left_join(Universities %>%
                            mutate(UNITID = as.numeric(UNITID)),
                          IPEDS_Race %>%
                            dplyr::rename_with(toupper), 
                          by = "UNITID","NAME")

ALL_Universities <- left_join(ALL_Universities %>%
                                mutate(UNITID = as.numeric(UNITID)),
                              IPEDS_Race %>%
                                dplyr::rename_with(toupper), 
                              by = "UNITID","NAME")