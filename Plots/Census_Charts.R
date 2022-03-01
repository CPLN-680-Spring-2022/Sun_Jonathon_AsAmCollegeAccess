if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


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

ggplotly(
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


ggplotly(
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