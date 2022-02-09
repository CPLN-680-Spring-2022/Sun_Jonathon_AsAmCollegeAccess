if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, timevis, lubridate, vistime, stargazer, apaTables, DT, formattable, huxtable) 

data <- read.csv("Data/timeline.csv") %>%
  rename(start = Start,
         end = End) %>%
  mutate(start = mdy(start),
         end = mdy(end))

timeline <- vistime(data,
        col.event = "Event",
        col.group = "Group",
        title = "Research Timeline")

hux(data)
