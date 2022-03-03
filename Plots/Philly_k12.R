if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# Philly Schools K-12 --------------------- 

Tier_order <- c("INTERVENE","WATCH","REINFORCE","MODEL","Insufficient Data")

Philly_Schools %>%
  filter(year == 2018) %>%
  filter(Report == "HS") %>%
  mutate(Overall.Tier = fct_relevel(Overall.Tier,Tier_order)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = ACS.Long %>%
            filter(Year == "2018-01-01") %>%
            filter(Race_Ethnicity == "Asian_alone") %>%
            st_as_sf(),
          aes(fill = Frequency)) +
  geom_sf(aes(shape = Overall.Tier,
              color = Overall.Tier)) +
  facet_wrap(~Overall.Tier) +
  mapTheme()


ggplotly(
  Philly_Schools %>%
    st_drop_geometry() %>%
    mutate(Overall.Tier = fct_relevel(Overall.Tier,Tier_order)) %>%
    group_by(year, Overall.Tier) %>%
    summarize(Total = n()) %>%
    ggplot(aes(x = year, y = Total, color = Overall.Tier)) +
    geom_point() +
    geom_line() +
    labs(title = "Philly K-12 Overall Tier") +
    plotTheme()
)

Philly_School_All %>%
  filter(year == 2018) %>%
  filter(Race == "Asian(Not_Hisp)") %>%
  mutate(Overall.Tier = fct_relevel(Overall.Tier,Tier_order)) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(data = ACS.Long %>%
            filter(Year == "2018-01-01") %>%
            filter(Race_Ethnicity == "Asian_alone") %>%
            st_as_sf(),
          aes(fill = Frequency)) +
  scale_fill_gradient(low = "white",
                      high = "Black")+
  geom_sf(aes(shape = Overall.Tier,
              color = Count)) +
  scale_color_gradient(low = "red",
                       high = "green") +
  facet_wrap(~Report) +
  mapTheme()

