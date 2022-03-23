if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


# Making all the plots


#cc <- scales::seq_gradient_pal("red", "green")(seq(0,1,length.out=A))

# Frequency ---------------------------------------

#ACS.Long %>%
#  filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1]) %>%
#  filter(Year == 2019) %>%
#  ggplot() +
#  geom_sf( aes(fill = Frequency)) +
#  geom_sf(data = Universities %>%
#            select(UNITID, RACE, FREQUENCY, YEARS, SHORT_CARNEGIE) %>%
#            filter(!is.na(SHORT_CARNEGIE)) %>%
#            filter(RACE == "Asian or Pacific Islander ") %>%
#            filter(YEARS == "2020-01-01") %>%
#            rename(AAPI_students = FREQUENCY),
#          aes(color = AAPI_students,
#              shape = SHORT_CARNEGIE)) +
#  scale_colour_gradient(low = "red", high = "green") +
#  scale_shape_manual(values = c(1:length(unique(Universities$SHORT_CARNEGIE)))) +
#  labs(title = print(unique(ACS.Long$Race_Ethnicity)[1])) +
#  mapTheme()

#ggsave(path = "Plots", filename = paste("plot",unique(ACS.Long$Race_Ethnicity)[1],".jpg"),  dpi=600, width = 16, height = 9)

for(i in 1:length(unique(ACS.Long$Race_Ethnicity))) {
  ACS.Long %>%
    filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[i]) %>%
    filter(Year == "2019-01-01") %>%
    mutate(Race_Ethnicity = str_replace(Race_Ethnicity,"_"," "),
           Race_Ethnicity = str_to_title(Race_Ethnicity)) %>%
    ggplot() +
    geom_sf( aes(fill = Frequency)) +
    geom_sf(data = Universities %>%
              select(UNITID, RACE, FREQUENCY, YEARS, SHORT_CARNEGIE) %>%
              filter(!is.na(SHORT_CARNEGIE)) %>%
              filter(RACE == "Asian or Pacific Islander ") %>%
              filter(YEARS == "2020-01-01") %>%
              rename(AAPI_students = FREQUENCY),
            aes(color = AAPI_students,
                shape = SHORT_CARNEGIE),
            size = 5) +
    scale_colour_gradient(low = "red", high = "green") +
    scale_shape_manual(values = c(12:(12+length(unique(Universities$SHORT_CARNEGIE))))) +
    labs(title = print(str_to_title(str_replace_all(unique(ACS.Long$Race_Ethnicity),"_"," "))[i])) +
    mapTheme()
  
  ggsave(path = "Plots/Frequencies", filename = paste("plot",unique(ACS.Long$Race_Ethnicity)[i],".jpg"),  dpi=600, width = 16, height = 9)
}

# Chinese Ratio ---------------------------------------

for(i in 1:length(unique(ACS.Long$Race_Ethnicity))) {
  ACS.Long %>%
    filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[i]) %>%
    filter(Year == "2019-01-01") %>%
    mutate(Race_Ethnicity = str_replace(Race_Ethnicity,"_"," "),
           Race_Ethnicity = str_to_title(Race_Ethnicity)) %>%
    ggplot() +
    geom_sf( aes(fill = Ratio_Chinese)) +
    geom_sf(data = Universities %>%
              select(UNITID, RACE, FREQUENCY, YEARS, SHORT_CARNEGIE) %>%
              filter(!is.na(SHORT_CARNEGIE)) %>%
              filter(RACE == "Asian or Pacific Islander ") %>%
              filter(YEARS == "2020-01-01") %>%
              rename(AAPI_students = FREQUENCY),
            aes(color = AAPI_students,
                shape = SHORT_CARNEGIE),
            size = 5) +
    scale_colour_gradient(low = "red", high = "green") +
    scale_shape_manual(values = c(12:(12+length(unique(Universities$SHORT_CARNEGIE))))) +
    labs(title = print(str_to_title(str_replace_all(unique(ACS.Long$Race_Ethnicity),"_"," "))[i])) +
    mapTheme()
  
  ggsave(path = "Plots/Chinese_Ratio", filename = paste("plot Chinese Ratio ",unique(ACS.Long$Race_Ethnicity)[i],".jpg"),  dpi=600, width = 16, height = 9)
}
