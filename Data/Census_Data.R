
# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, survey, srvyr)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# ---------------------------

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
  rename_at(vars(col.from), function(x) cols.to) %>%
  mutate(Asian_pop = Asian_alone)
ACS.Long <- pivot_longer(ACS.wide,4:(ncol(ACS.wide)-2), names_to = "Race_Ethnicity", values_to = "Frequency") %>%
  mutate(Tot_Percentage = (Frequency/Total_Population)*100,
         Asian_Percentage = (Frequency/Asian_pop) * 100,
         Year = ymd(paste(2019,"0101")))

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
      label = str_remove(label,"Total!!"),
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
    rename_at(vars(col.from), function(x) cols.to) %>%
    mutate(Asian_pop = Asian_alone)
  
  f <- pivot_longer(ACS.wide,4:(ncol(ACS.wide)-2), names_to = "Race_Ethnicity", values_to = "Frequency") %>%
    mutate(Tot_Percentage = (Frequency/Total_Population)*100,
           Asian_Percentage = (Frequency/Asian_pop) * 100,
           Year = ymd(paste(Years[i],"0101")))  
  ACS.Long <- rbind(ACS.Long,f)  
}

exclude <- c("15007990300", "15009990000", "15009991200", "15001990000", "15001990100", "15001990400", "15001990500", "15001990600", "15001990700", "15001990800", "15001990900", "15001991000", "15001991100","15001991200", "15001991300", "15001991400", "15001991500", "15001991600", "15001991700", "15005990000", "15003980300","15009990200","15003981200","15003980800","15001990300","15007990100","15003981300", "15007990200", "15003990001", "15007041200", "15009980000", "15003980000", "15003981000", "15003980700", "15003980600")

ACS.Long <- ACS.Long %>%
  filter(!GEOID %in% exclude) %>%
  mutate(Race_Ethnicity = str_remove(Race_Ethnicity,"Total_Population"),
         Race_Ethnicity = str_replace(Race_Ethnicity,"Chinese_,_except_Taiwanese","Chinese,_except_Taiwanese"),
         Race_Ethnicity = str_replace(Race_Ethnicity,"Two_or_more_racesTwo_races_including_Some_other_race","Two_or_more_races_Two_races_including_Some_other_race"),
         Race_Ethnicity = str_replace(Race_Ethnicity,"Two_or_more_racesTwo_races_excluding_Some_other_race,_and_three_or_more_races","Two_or_more_races_Two_races_excluding_Some_other_race,_and_three_or_more_races"))


test <- ACS.Long %>%
          mutate(Year_perm = Year) %>%
          pivot_wider(names_from = Year, values_from = Frequency) %>%
          rename(Year = Year_perm)

change_over_time <- test %>%
                      filter(Year == unique(ACS.Long$Year)[1]) %>%
                      select(GEOID,Race_Ethnicity) %>%
                      na.omit()

for(i in 1:length(unique(ACS.Long$Year))) {
x <- 9+i

f <- test %>%
  filter(Year == unique(ACS.Long$Year)[i]) %>%
  select(GEOID,Race_Ethnicity,x) %>%
  na.omit()

change_over_time <- left_join(change_over_time, f, by = c("GEOID","Race_Ethnicity"))

}

change_over_time <- change_over_time %>%
                      mutate(CoT2012 = (`2012-01-01` - `2011-01-01`),
                             CoT2013 = (`2013-01-01` - `2012-01-01`),
                             CoT2014 = (`2014-01-01` - `2013-01-01`),
                             CoT2015 = (`2015-01-01` - `2014-01-01`),
                             CoT2016 = (`2016-01-01` - `2015-01-01`),
                             CoT2017 = (`2017-01-01` - `2016-01-01`),
                             CoT2018 = (`2018-01-01` - `2017-01-01`),
                             CoT2019 = (`2019-01-01` - `2018-01-01`),
                             CoT2011_2019 = (`2019-01-01` - `2011-01-01`),
                             CoTP2012 = (`2012-01-01` - `2011-01-01`)/`2011-01-01`*100,
                             CoTP2013 = (`2013-01-01` - `2012-01-01`)/`2012-01-01`*100,
                             CoTP2014 = (`2014-01-01` - `2013-01-01`)/`2013-01-01`*100,
                             CoTP2015 = (`2015-01-01` - `2014-01-01`)/`2014-01-01`*100,
                             CoTP2016 = (`2016-01-01` - `2015-01-01`)/`2015-01-01`*100,
                             CoTP2017 = (`2017-01-01` - `2016-01-01`)/`2016-01-01`*100,
                             CoTP2018 = (`2018-01-01` - `2017-01-01`)/`2017-01-01`*100,
                             CoTP2019 = (`2019-01-01` - `2018-01-01`)/`2018-01-01`*100,
                             CoTP2011_2019 = (`2019-01-01` - `2011-01-01`)/`2011-01-01`*100)

#Change_over_time_Freq <- change_over_time %>%
#                            select(1,2,12:19) %>%
#                            pivot_longer(cols = starts_with("COT"), names_to = "Year", values_to = "Change_by_year") %>%
#                            mutate(Year = str_remove(as.character(Year),"CoT"),
#                                   Year = ymd(paste0(Year,"0101")))

ACS.Long <- left_join(ACS.Long, 
                      change_over_time %>%
                        select(1,2,12:19) %>%
                        pivot_longer(cols = starts_with("COT"), names_to = "Year", values_to = "Change_by_year") %>%
                        mutate(Year = str_remove(as.character(Year),"CoT"),
                               Year = ymd(paste0(Year,"0101"))),
                      by = c("GEOID","Race_Ethnicity","Year"))


#change_over_time_Perc <- change_over_time %>%
#                            select(1,2,starts_with("COTP")) %>%
#                            select(-CoTP2011_2019) %>%
#                            pivot_longer(cols = starts_with("COTP"), names_to = "Year", values_to = "Change_by_year") %>%
#                            mutate(Year = str_remove(as.character(Year),"CoTP"),
#                                   Year = ymd(paste0(Year,"0101")))
                    
ACS.Long <- left_join(ACS.Long, 
                      change_over_time %>%
                        select(1,2,starts_with("COTP")) %>%
                        select(-CoTP2011_2019) %>%
                        pivot_longer(cols = starts_with("COTP"), names_to = "Year", values_to = "Change_by_year_Percent") %>%
                        mutate(Year = str_remove(as.character(Year),"CoTP"),
                               Year = ymd(paste0(Year,"0101"))),
                      by = c("GEOID","Race_Ethnicity","Year"))

# Chinese ratio
ACS.Long <- ACS.Long %>%
          st_drop_geometry() %>%
          group_by(Year, GEOID, Race_Ethnicity) %>%
          summarize(Total = sum(Frequency)) %>%
          filter(Race_Ethnicity == "Chinese,_except_Taiwanese") %>%
          select(-Race_Ethnicity) %>%
          rename(Chinese_population = Total) %>%
          left_join(ACS.Long, by = c("Year","GEOID")) %>%
          relocate(Chinese_population, .after = last_col()) %>%
          mutate(Ratio_Chinese = Frequency/Chinese_population) %>%
          st_as_sf()
          



