if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")


## Higher Education Enrollment Data

Enrollment_time <- ALL_Universities %>%
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


PoC_Enrollment_Time <- ggplotly(
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


Enrollment_time_Facet_Race <- Universities %>%
                            st_drop_geometry() %>%
                            filter(is.na(CARNEGIE_CLASSIFCATION2021) == FALSE) %>%
                            filter(!RACE == "Total_Enrollment") %>%
                            mutate(Only_Year = year(YEARS),
                                   CARNEGIE_CLASSIFCATION2021 = str_wrap(CARNEGIE_CLASSIFCATION2021, width = 40),
                                   RACE = str_wrap(RACE, width = 80)) %>%
                            group_by(CARNEGIE_CLASSIFCATION2021, Only_Year, RACE) %>%
                            summarize(Total = sum(FREQUENCY)) %>%
                            ggplot(aes(x = Only_Year, y = Total, fill = CARNEGIE_CLASSIFCATION2021)) +
                            geom_bar(stat="identity" ,position = "stack") +
                            facet_wrap(~RACE,
                                       scales = "free") +
                            plotTheme()



Enrollment_time_Facet_University <- Universities %>%
                          st_drop_geometry() %>%
                          filter(is.na(CARNEGIE_CLASSIFCATION2021) == FALSE) %>%
                          filter(!RACE == "Total_Enrollment") %>%
                          mutate(Only_Year = year(YEARS),
                                 CARNEGIE_CLASSIFCATION2021 = str_wrap(CARNEGIE_CLASSIFCATION2021, width = 20)) %>%
                          group_by(CARNEGIE_CLASSIFCATION2021, Only_Year, RACE) %>%
                          summarize(Total = sum(FREQUENCY)) %>%
                          ggplot(aes(x = Only_Year, y = Total, fill = RACE)) +
                          geom_bar(stat="identity" ,position = "stack") +
                          facet_wrap(~CARNEGIE_CLASSIFCATION2021,
                                     scales = "free") +
                          plotTheme()


## Census Data

ggplotly(
  ACS.Long %>%
    st_drop_geometry() %>%
    filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(n = sum(Frequency)) %>%
    mutate(percentage = n / sum(n)) %>%
    ggplot( aes(x = Year, y = percentage, fill = Race_Ethnicity)) +
    geom_area() +
    labs(title = "Racial Breakdown",
         subtitle = "Philadelphia") +
    plotTheme()
)


ggplotly(
  ACS.Long %>%
    st_drop_geometry() %>%
    filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(n = sum(Frequency)) %>%
    mutate(percentage = n / sum(n)) %>%
    ggplot( aes(x = Year, y = percentage, fill = Race_Ethnicity)) +
    geom_area() +
    labs(title = "Asian Ethnic group by Percentage",
         subtitle = "Philadelphia") +
    plotTheme()
)


Asian_Pop_Philly <- ACS.Long %>%
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


Race_Frequency_Philly <- ggplotly(
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

figure2_slides <-ACS.Long %>%
                  st_drop_geometry() %>%
                  filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
                  mutate(Race_Ethnicity = ifelse(grepl("Two_or_more_races", Race_Ethnicity),"Two_or_more_races",Race_Ethnicity),
                         Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")) %>%
                  group_by(Year, Race_Ethnicity) %>%
                  summarize(Frequency = sum(Change_by_year)) %>%
                  ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
                  geom_point() +
                  geom_line() +
                  labs(title = "Race change by year",
                       subtitle = "Philadelphia") +
                  plotTheme()


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
    labs(title = "Asian Ethnic group change by year",
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
    labs(title = "Asian Ethnic change by year percent",
         subtitle = "Philadelphia") +
    plotTheme()
)


## Geometry Data

Education_Institutions <- ggplot() +
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
                            facet_wrap(~Type_of_Ed) +
                            mapTheme()

Higher_Education_Institutions <- ACS.Long %>%
                                  filter(Race_Ethnicity == "Asian_alone") %>%
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
                                  labs(title = "Asian American Alone and Asian Enrollment") +
                                  mapTheme()

ggplot() +
  geom_sf(data = ACS.Long, aes(fill = Frequency)) +
  facet_wrap(~Race_Ethnicity) +
  mapTheme()
