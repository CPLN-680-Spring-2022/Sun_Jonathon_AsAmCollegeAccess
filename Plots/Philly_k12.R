if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, gganimate, plotly, htmlwidgets)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# Philly Schools K-12 ---------------------  
Philly_Schools %>%
  filter(year == 2018) %>%
  ggplot() +
  geom_sf(data = ACS.Long %>%
            filter(Year == "2018-01-01") %>%
            filter(Race_Ethnicity == "Asian_alone"),
          aes(fill = Frequency)) +
  geom_sf(aes(shape = Overall.Tier,
              color = Overall.Tier)) +
  mapTheme()


ggplotly(
  Philly_Schools %>%
    st_drop_geometry() %>%
    group_by(year, Overall.Tier) %>%
    summarize(Total = n()) %>%
    ggplot(aes(x = year, y = Total, color = Overall.Tier)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    labs(title = "Philly K-12 Overall Tier") +
    plotTheme()
)

