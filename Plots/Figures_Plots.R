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
