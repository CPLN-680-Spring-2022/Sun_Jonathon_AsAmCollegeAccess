# Summary Statistics

if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, sf, tigris, ggmap, tidycensus, stm, readxl, lubridate, DataExplorer)

register_google(key = "AIzaSyDIDS0OgrYmmrJO221DhqIaEnMq9tQrMr0") 
options(scipen = 999)
options(scipen =  "sf")

df <- ACS_Cluster_Group %>%
        select(-ends_with("nn1"),
               -ends_with("nn2"),
               -ends_with("nn3"),
               -ends_with("nn4"),
               -ends_with("nn5"))


plot_histogram(df)
plot_correlation(df)


cor <- cor(df[-c(1:2)],
           method = "pearson",
           use = "complete.obs")

res <- as.data.frame(cor(ACS_Cluster_Group[-c(1:2)])) %>%
  rownames_to_column()

#pivot longer and sort through list 
A <- res %>%
  slice(-9) %>%
  pivot_longer(2:(ncol(res))) %>%
  filter(value >= 0.3 & value <=0.6)

B <- res %>%
  slice(-9) %>%
  pivot_longer(2:(ncol(res))) %>%
  filter(value >= -0.6 & value <= -0.3)

corlist <- bind_rows(A,B)

corlist <- corlist %>%
  group_by(value) %>%
  arrange(desc(value))

#all odd numbers between 1 and 80
# Removes duplicates pairs that come from correlation matrix
keep <- c(1:nrow(corlist))
keep <- which(keep %% 2 == 1)

corlist <- corlist %>%
  slice(keep) 

# includes all variables correlated with price
Asian <- corlist %>%
  filter(grepl("_Asian",rowname))
