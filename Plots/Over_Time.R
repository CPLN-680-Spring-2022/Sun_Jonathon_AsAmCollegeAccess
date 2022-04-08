options(scipen = 10000)

## Higher Education Enrollment Data

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

ggsave("Plots/Time_Plots/University_Enroll.jpg", dpi = 600)


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

ggsave("Plots/Time_Plots/University_Enroll_POC.jpg", dpi = 600)
  
Universities %>%
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

ggsave("Plots/Time_Plots/University_Enroll_Bar_Classification.jpg", width = 15, dpi = 600)

Universities %>%
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

ggsave("Plots/Time_Plots/University_Enroll_Bar_Race.jpg", width = 15, dpi = 600)

## Census Data

plot <- ACS.Long %>%
          dplyr::select(Year, Race_Ethnicity,Frequency) %>%
          st_drop_geometry() %>%
          filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1:9]) %>%
          group_by(Year, Race_Ethnicity) %>%
          summarize(n = sum(Frequency)) %>%
          mutate(percentage = n / sum(n)) 

#plot %>%
#    ggplot( aes(x = Year, y = percentage, fill = Race_Ethnicity)) +
#    geom_area() +
#    ggrepel::geom_label_repel(data = plot %>%
#                              filter(Year == "2018-01-01") %>%
#                              mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
#                            aes(label = Race_Ethnicity),
#                            min.segment.length = 1) +
#    labs(title = "Racial Breakdown",
#         subtitle = "Philadelphia") +
#    plotTheme()

#  ggsave("Plots/Time_Plots/Philly_Population_race.jpg", dpi = 600)

#plot <- ACS.Long %>%
#          dplyr::select(Year, Race_Ethnicity,Frequency) %>%
#          st_drop_geometry() %>%
#          filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1:9]) %>%
#          group_by(Year, Race_Ethnicity) %>%
#          summarize(n = sum(Frequency)) %>%
#          mutate(percentage = n / sum(n))
  
#plot %>%
#    ggplot( aes(x = Year, y = percentage, fill = Race_Ethnicity)) +
#    geom_area() +
#    ggrepel::geom_label_repel(data = plot %>%
#                                filter(Year == "2018-01-01") %>%
#                                mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
#                              aes(label = Race_Ethnicity),
#                              min.segment.length = 1) +
#    labs(title = "Asian Ethnic group by Percentage",
#         subtitle = "Philadelphia") +
#    plotTheme()

#ggsave("Plots/Time_Plots/Asian_Ethnic_Group_Percent.jpg", dpi = 600)

plot <- ACS.Long %>%
          dplyr::select(Year, Race_Ethnicity, Frequency) %>%
          st_drop_geometry() %>%
          filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1:9]) %>%
          group_by(Year, Race_Ethnicity) %>%
          summarize(Frequency = sum(Frequency)) 

plot %>%
  ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
  geom_point( size = 2) +
  geom_line( size = 1) +
  ggrepel::geom_label_repel(data = plot %>%
               filter(Year == "2018-01-01") %>%
               mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
               aes(label = Race_Ethnicity),
               min.segment.length = 1) +
  theme(legend.position = "none") +
  labs(title = "Asian Ethnic group by Frequency",
       subtitle = "Philadelphia") +
  plotTheme()

ggsave("Plots/Time_Plots/Asian_Ethnic_Line.jpg", dpi = 600)

plot <- ACS.Long %>%
        dplyr::select(Year, Race_Ethnicity, Frequency) %>%
        st_drop_geometry() %>%
        filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1:9]) %>%
        group_by(Year, Race_Ethnicity) %>%
        summarize(Frequency = sum(Frequency)) 

plot %>%
    ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
    geom_point( size = 2) +
    geom_line( size = 1) +
    theme(legend.position = "none") +
    ggrepel::geom_label_repel(data = plot %>%
                                filter(Year == "2018-01-01") %>%
                                mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
                              aes(label = Race_Ethnicity),
                              min.segment.length = 0,
                              max.overlaps = 5) +
    labs(title = "Racial Groups by Frequency",
         subtitle = "Philadelphia") +
    plotTheme()

ggsave("Plots/Time_Plots/Asian_Ethnic_Race_Line.jpg", dpi = 600)

plot <-ACS.Long %>%
  dplyr::select(Year, Race_Ethnicity, Change_by_year) %>%
  st_drop_geometry() %>%
  filter(Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1:9]) %>%
  mutate(Race_Ethnicity = ifelse(grepl("Two_or_more_races", Race_Ethnicity),"Two_or_more_races",Race_Ethnicity),
         Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")) %>%
  group_by(Year, Race_Ethnicity) %>%
  summarize(Frequency = sum(Change_by_year)) 

plot %>%
  ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
  geom_point( size = 2) +
  geom_line( size = 1) +
  theme(legend.position = "none") +
  ggrepel::geom_label_repel(data = plot %>%
                              filter(Year == "2018-01-01") %>%
                              mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
                            aes(label = Race_Ethnicity),
                            min.segment.length = 0,
                            max.overlaps = 3) +
  labs(title = "Race change by year",
       subtitle = "Philadelphia") +
  plotTheme()

ggsave("Plots/Time_Plots/Race_Change_By_Year.jpg", dpi = 600)

 plot <- ACS.Long %>%
    dplyr::select(Year, Race_Ethnicity, Change_by_year) %>%
    st_drop_geometry() %>%
    filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity)[1:9]) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(Frequency = sum(Change_by_year)) 
 
 plot %>%
    ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
     geom_point( size = 2) +
     geom_line( size = 1) +
     theme(legend.position = "none") +
     ggrepel::geom_label_repel(data = plot %>%
                                 filter(Year == "2017-01-01") %>%
                                 mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
                               aes(label = Race_Ethnicity),
                               min.segment.length = 0,
                               max.overlaps = 10) +
      labs(title = "Asian Ethnic group change by year",
           subtitle = "Philadelphia") +
      plotTheme()

 ggsave("Plots/Time_Plots/Asian_Ethnic_Change_By_year.jpg", dpi = 600)

 plot <- ACS.Long %>%
    dplyr::select(Year, Race_Ethnicity, Change_by_year_Percent) %>%
    st_drop_geometry() %>%
    filter(!Race_Ethnicity %in% unique(ACS.Long$Race_Ethnicity[1:9])) %>%
    mutate(Change_by_year_Percent = replace_na(Change_by_year_Percent,0)) %>%
    mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>%
    group_by(Year, Race_Ethnicity) %>%
    summarize(Frequency = sum(Change_by_year_Percent)) 
 
 plot %>%
    ggplot( aes(x = Year, y = Frequency, color = Race_Ethnicity)) +
   geom_point( size = 2) +
   geom_line( size = 1) +
   theme(legend.position = "none") +
   ggrepel::geom_label_repel(data = plot %>%
                               filter(Year == "2019-01-01") %>%
                               mutate(Race_Ethnicity = str_replace_all(Race_Ethnicity,"_"," ")),
                             aes(label = Race_Ethnicity),
                             min.segment.length = 0,
                             max.overlaps = 10) +
    theme(legend.position = "none") +
    labs(title = "Asian Ethnic change by year percent",
         subtitle = "Philadelphia") +
    plotTheme()

 ggsave("Plots/Time_Plots/Asian_Ethnic_Change_By_year_Percent.jpg", dpi = 600)

 