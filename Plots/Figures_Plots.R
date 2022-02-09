if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


## Higher Education Enrollment Data
ggplotly(
ALL_Universities %>%
  st_drop_geometry() %>%
  filter(is.na(CARNEGIE_CLASSIFCATION2021) == FALSE) %>%
  filter(!RACE == "Total_Enrollment") %>%
  group_by(YEARS, RACE) %>%
  summarize(n = sum(FREQUENCY)) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot( aes(x = YEARS, y = percentage, fill = RACE)) +
  geom_area() +
  labs(title = "Breakdown of Enrollment",
       subtitle = "From 2010 - 2020") +
  plotTheme()
)

ggplotly(
Universities %>%
  st_drop_geometry() %>%
  filter(is.na(CARNEGIE_CLASSIFCATION2021) == FALSE) %>%
  filter(!RACE == "Total_Enrollment") %>%
  group_by(YEARS, RACE) %>%
  summarize(n = sum(FREQUENCY)) %>%
  mutate(percentage = n / sum(n)) %>%
  filter(!RACE == "White non-Hispanic") %>%
  ggplot( aes(x = YEARS, y = percentage, fill = RACE)) +
  geom_area() +
  labs(title = "Breakdown of Enrollment",
       subtitle = "From 2010 - 2020") +
  plotTheme() 
)


Universities %>%
  group_by(NAME, SHORT_CARNEGIE, RACE) %>%
  summarize(FREQUENCY = sum(FREQUENCY)) %>%
  filter(RACE == "Total_Enrollment") %>%
  ggplot() +
  geom_sf( aes(color = SHORT_CARNEGIE,
               size = FREQUENCY)) +
  geom_sf(data = Philadelphia_School_District,
          fill = "transparent") +
  mapTheme()

ggplotly(
Universities %>%
  st_drop_geometry() %>%
  filter(is.na(CARNEGIE_CLASSIFCATION2021) == FALSE) %>%
  filter(!RACE == "Total_Enrollment") %>%
  mutate(Only_Year = year(YEARS)) %>%
  group_by(CARNEGIE_CLASSIFCATION2021, Only_Year, RACE) %>%
  summarize(Total = sum(FREQUENCY)) %>%
  ggplot(aes(x = Only_Year, y = Total, fill = RACE)) +
  geom_bar(stat="identity" ,position = "stack") +
  facet_wrap(~CARNEGIE_CLASSIFCATION2021,
             scales = "free") +
  gganimate::transition_time(Only_Year) %>%
  plotTheme()
)

## Census Tract Information

#cc <- scales::seq_gradient_pal("red", "green")(seq(0,1,length.out=A))


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
  
  ggsave(path = "Plots", filename = paste("plot",unique(ACS.Long$Race_Ethnicity)[i],".jpg"),  dpi=600, width = 16, height = 9)
}




## Census Data

ggplotly(
  ACS.Long %>%
    st_drop_geometry() %>%
    filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    mutate(Only_Year = year(Year)) %>%
    group_by(Only_Year, Race_Ethnicity) %>%
    summarize(n = sum(Frequency)) %>%
    mutate(percentage = n / sum(n)) %>%
    ggplot( aes(x = Only_Year, y = percentage, fill = Race_Ethnicity)) +
    geom_area() +
    labs(title = "Racial Breakdown",
         subtitle = "Philadelphia") +
    plotTheme()
)


ggplotly(
ACS.Long %>%
  st_drop_geometry() %>%
  filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
  mutate(Only_Year = year(Year)) %>%
  group_by(Only_Year, Race_Ethnicity) %>%
  summarize(n = sum(Frequency)) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot( aes(x = Only_Year, y = percentage, fill = Race_Ethnicity)) +
  geom_area() +
  labs(title = "Asian Ethnic group by Percentage",
       subtitle = "Philadelphia") +
  plotTheme()
)

ggplotly(
ACS.Long %>%
  st_drop_geometry() %>%
  filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
  group_by(Year, Race_Ethnicity) %>%
  summarize(Frequency = sum(Frequency)) %>%
  ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  labs(title = "Asian Ethnic group by Frequency",
       subtitle = "Philadelphia") +
plotTheme()
)

Figure1_Slides <- ggplotly(
                    ACS.Long %>%
                      st_drop_geometry() %>%
                      filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
                      group_by(Year, Race_Ethnicity) %>%
                      summarize(Frequency = sum(Frequency)) %>%
                      ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
                      geom_point() +
                      geom_line() +
                      theme(legend.position = "none") +
                      labs(title = "Racial Groups by Frequency",
                           subtitle = "Philadelphia") +
                      plotTheme()
                  )

htmlwidgets::saveWidget(Figure1_Slides,"Plots/Figure1.html")


figure2_slides <- ggplotly(
  ACS.Long %>%
    st_drop_geometry() %>%
    filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(Frequency = sum(Change_by_year)) %>%
    ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
    geom_point() +
    geom_line() +
    theme(legend.position = "none") +
    labs(title = "Asian Ethnic group by Frequency",
         subtitle = "Philadelphia") +
    plotTheme()
)


ggplotly(
  ACS.Long %>%
    st_drop_geometry() %>%
    filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(Frequency = sum(Change_by_year)) %>%
    ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
    geom_point() +
    geom_line() +
    theme(legend.position = "none") +
    labs(title = "Asian Ethnic group by Frequency",
         subtitle = "Philadelphia") +
    plotTheme()
)

ggplotly(
  ACS.Long %>%
    st_drop_geometry() %>%
    filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    mutate(Change_by_year_Percent = replace_na(Change_by_year_Percent,0)) %>%
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(Frequency = sum(Change_by_year_Percent)) %>%
    ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
    geom_point() +
    geom_line() +
    theme(legend.position = "none") +
    labs(title = "Asian Ethnic group by Frequency",
         subtitle = "Philadelphia") +
    plotTheme()
)


## Geometry Data

ggplot() +
  geom_sf(data = ACS.Long %>%
            filter(Race_Ethnicity == "Asian_alone"),
          aes(fill = Frequency)) +
  labs(title = "Education Institutions in Philadelphia",
       subtitle = "By institutional category") +
  geom_sf(data = Philly_Education_Long %>%
            filter(!Type_of_Ed == "Not_Categorized") %>%
            filter(grepl("School",Type_of_Ed)),
          aes(color = Type_of_Ed,
              shape = Type_of_Ed)) +
  scale_shape_manual(values = c(1:length(unique(Philly_Education_Long$Type_of_Ed)))) +
  geom_sf(data = Philadelphia_tracts, 
          fill = "transparent") +
  mapTheme()

ggplot() +
  geom_sf(data = ACS.Long, aes(fill = Frequency)) +
  facet_wrap(~Race_Ethnicity) +
  mapTheme()
