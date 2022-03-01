library(tidycensus)
library(dplyr)
library(tidyr)
library(srvyr)

CACHE_FILE <- "pa_pums_rep_weights.Rds"

if(file.exists(CACHE_FILE)){
  pa_pums_rep_weights <- readRDS(CACHE_FILE)
} else {
  pa_pums_rep_weights <- get_pums(
    variables = c("PUMA", "SEX", "AGEP", "SCHL"),
    state = "PA",
    survey = "acs1",
    year = 2018,
    recode = TRUE,
    rep_weights = "person"
  )
  saveRDS(pa_pums_rep_weights, file=CACHE_FILE)
}


pa_survey_design <- to_survey(pa_pums_rep_weights)
