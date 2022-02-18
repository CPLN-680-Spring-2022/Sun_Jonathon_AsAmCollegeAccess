# Datasets
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(opentripplanner, tidytransit, tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, survey, srvyr)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
# -------
pums_vars_2018 <- pums_variables %>% 
  filter(year == 2018, survey == "acs5")

pums_vars_2018 %>% 
  distinct(var_code, var_label, data_type, level)

pums_vars_2018 %>% 
  distinct(var_code, var_label, data_type, level) %>% 
  filter(level == "person")

vt_pums <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "VT",
  survey = "acs1",
  year = 2018
)

vt_pums_recoded <- get_pums(
  variables = c("PUMA", "SEX", "AGEP", "SCHL"),
  state = "VT",
  survey = "acs1",
  year = 2018,
  recode = TRUE
)

# ----------

pums_vars_2019 <- pums_variables %>%
  filter(year == 2019, survey == "acs5", level == "person") %>%
  distinct(var_code, var_label, data_type, level) %>%
  filter(grepl("SPORDER|HISP|RAC|ANC|MIGSP|POBP", var_code))

pums_vars_2019 <- pums_variables %>%
  filter(year == 2019, survey == "acs1", level == "person") %>%
  distinct(var_code, var_label, data_type, level) %>%
  filter(grepl("SPORDER|ANC", var_code))

PA_pums <- get_pums(
  variables = pums_vars_2019$var_code,
  state = "PA",
  survey = "acs5",
  year = 2019,
  recode = TRUE
)

PA_pums_weight <- get_pums(
  variables = pums_vars_2019$var_code,
  state = "PA",
  survey = "acs1",
  year = 2019,
  recode = TRUE,
  rep_weights = "person"
) 

PA_survey_design <- to_survey(PA_pums_weight %>%
                                na.omit())

PA_survey_design %>%
  survey_count(ANC, 	
               FANCP)

PA_pums_weight %>%
  slice(1:100) %>%
  View()

PA_puma <- map("PA", tigris::pumas, class = "sf", cb = TRUE) %>%
  reduce(rbind)

ggplot() +
  geom_sf(data = PA_puma) +
  mapTheme()
