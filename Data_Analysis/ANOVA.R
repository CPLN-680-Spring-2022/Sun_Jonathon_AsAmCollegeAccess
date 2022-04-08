
library(rstatix)

# T-tests

Similar <- ACS_Cluster_Group %>%
  dplyr::select(GEOID,ends_with("Cluster")) %>%
  mutate(AAPI_Cluster = as.character(AAPI_Cluster),
         AAPI_Cluster = str_replace_all(AAPI_Cluster,"AAPI_Race", "variable"),
         AAPI_Cluster = replace_na(AAPI_Cluster,"No_Relation"),
         East_Asian_Cluster = as.character(East_Asian_Cluster),
         East_Asian_Cluster = str_replace_all(East_Asian_Cluster,"East_Asian_Asian", "variable"),
         East_Asian_Cluster = replace_na(East_Asian_Cluster,"No_Relation"),
         Filipino_Asian_Cluster = as.character(Filipino_Asian_Cluster),
         Filipino_Asian_Cluster = str_replace_all(Filipino_Asian_Cluster,"Filipino_Asian", "variable"),
         Filipino_Asian_Cluster = replace_na(Filipino_Asian_Cluster,"No_Relation"),
         SouthAsian_Cluster = as.character(SouthAsian_Cluster),
         SouthAsian_Cluster = str_replace_all(SouthAsian_Cluster,"South_Asian_Asian","variable"),
         SouthAsian_Cluster = replace_na(SouthAsian_Cluster,"No_Relation"),
         Southeast_Asian_Cluster = as.character(Southeast_Asian_Cluster),
         Southeast_Asian_Cluster = str_replace_all(Southeast_Asian_Cluster,"Southeast_Asian_Asian","variable"),
         Southeast_Asian_Cluster = replace_na(Southeast_Asian_Cluster,"No_Relation"),
         AAPI_East_Same = paste0(AAPI_Cluster,East_Asian_Cluster),
         AAPI_Filipino_Same = paste0(AAPI_Cluster,Filipino_Asian_Cluster),
         AAPI_Southeast_Same = paste0(AAPI_Cluster,Southeast_Asian_Cluster),
         AAPI_South_Same = paste0(AAPI_Cluster,SouthAsian_Cluster),
         East_Filipino_Same = paste0(East_Asian_Cluster,Filipino_Asian_Cluster),
         East_South_Same = paste0(East_Asian_Cluster,SouthAsian_Cluster),
         East_Southeast_Same = paste0(East_Asian_Cluster, Southeast_Asian_Cluster),
         Filipino_South_Same = paste0(Filipino_Asian_Cluster, SouthAsian_Cluster),
         Filipino_Southeast_Same = paste0(Filipino_Asian_Cluster, Southeast_Asian_Cluster),
         South_Southeast_Same = paste0(SouthAsian_Cluster, Southeast_Asian_Cluster)
  )

# ANOVA on Handmade groups

T_Test <- left_join(Similar,
          ACS_Cluster_Group_Medians,
          by = "NAME") %>%
          dplyr::select(-c("Race_Cluster","IncomeEduc_Cluster"), -ends_with("_Same")) %>%
          pivot_longer(3:7, names_to = "Clusters", values_to = "Type_of_Cluster") %>%
          pivot_longer(3:9, names_to = "variables", values_to = "Values")

### Median Income ----------------------------------

df <- T_Test %>%
          ungroup() %>%
          dplyr::select(-c("NAME")) %>%
          filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[3]) %>%
          filter(variables == unique(T_Test$variables)[1]) %>%
          na.omit()

Remove <- df %>%
            group_by(GEOID) %>%
            summarize(Frequency = n()) %>%
            filter(Frequency > 1)

df %>% 
  filter(GEOID %in% Remove$GEOID) %>%
  group_by(Clusters) %>%
  get_summary_stats(Values)
  

Pairwise_Tests <- df %>% 
                    filter(GEOID %in% Remove$GEOID) %>%
                    pairwise_t_test(Values ~ Clusters) %>%
                    mutate(Variable = unique(T_Test$variables)[1])


#### Looping through other variables ----------------------

for(i in 2:length(unique(T_Test$variables))){
  
  if( i == 2) {
    next
  }
  
  df <- T_Test %>%
    ungroup() %>%
    dplyr::select(-c("NAME")) %>%
    filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[3]) %>%
    filter(variables == unique(T_Test$variables)[i]) %>%
    na.omit()
  
  Remove <- df %>%
    group_by(GEOID) %>%
    summarize(Frequency = n()) %>%
    filter(Frequency > 1)
  
  df %>% 
    filter(GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats(Values)
  
  
  merge <- df %>% 
            filter(GEOID %in% Remove$GEOID) %>%
            pairwise_t_test(Values ~ Clusters) %>%
            mutate(Variable = unique(T_Test$variables)[i])
  
  Pairwise_Tests <- rbind(Pairwise_Tests, merge)
  
}

#### Census tract demographics --------------------------------

T_Test <- ACS_Cluster_Group %>%
  mutate(AAPI_Cluster = as.character(AAPI_Cluster),
         AAPI_Cluster = str_replace_all(AAPI_Cluster,"AAPI_Race", "variable"),
         AAPI_Cluster = replace_na(AAPI_Cluster,"No_Relation"),
         East_Asian_Cluster = as.character(East_Asian_Cluster),
         East_Asian_Cluster = str_replace_all(East_Asian_Cluster,"East_Asian_Asian", "variable"),
         East_Asian_Cluster = replace_na(East_Asian_Cluster,"No_Relation"),
         Filipino_Asian_Cluster = as.character(Filipino_Asian_Cluster),
         Filipino_Asian_Cluster = str_replace_all(Filipino_Asian_Cluster,"Filipino_Asian", "variable"),
         Filipino_Asian_Cluster = replace_na(Filipino_Asian_Cluster,"No_Relation"),
         SouthAsian_Cluster = as.character(SouthAsian_Cluster),
         SouthAsian_Cluster = str_replace_all(SouthAsian_Cluster,"South_Asian_Asian","variable"),
         SouthAsian_Cluster = replace_na(SouthAsian_Cluster,"No_Relation"),
         Southeast_Asian_Cluster = as.character(Southeast_Asian_Cluster),
         Southeast_Asian_Cluster = str_replace_all(Southeast_Asian_Cluster,"Southeast_Asian_Asian","variable"),
         Southeast_Asian_Cluster = replace_na(Southeast_Asian_Cluster,"No_Relation"),
         AAPI_East_Same = paste0(AAPI_Cluster,East_Asian_Cluster),
         AAPI_Filipino_Same = paste0(AAPI_Cluster,Filipino_Asian_Cluster),
         AAPI_Southeast_Same = paste0(AAPI_Cluster,Southeast_Asian_Cluster),
         AAPI_South_Same = paste0(AAPI_Cluster,SouthAsian_Cluster),
         East_Filipino_Same = paste0(East_Asian_Cluster,Filipino_Asian_Cluster),
         East_South_Same = paste0(East_Asian_Cluster,SouthAsian_Cluster),
         East_Southeast_Same = paste0(East_Asian_Cluster, Southeast_Asian_Cluster),
         Filipino_South_Same = paste0(Filipino_Asian_Cluster, SouthAsian_Cluster),
         Filipino_Southeast_Same = paste0(Filipino_Asian_Cluster, Southeast_Asian_Cluster),
         South_Southeast_Same = paste0(SouthAsian_Cluster, Southeast_Asian_Cluster)) %>%
         pivot_longer(3:25, names_to = "variables", values_to = "Values") %>%
         select(-c("Race_Cluster","IncomeEduc_Cluster"), -ends_with("_Same")) %>%
         pivot_longer(3:7, names_to = "Clusters", values_to = "Type_of_Cluster")


df <- T_Test %>%
        ungroup() %>%
        dplyr::select(-c("NAME")) %>%
        filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[3]) %>%
        filter(variables == unique(T_Test$variables)[1]) %>%
        na.omit()


#### Looping through other variables ----------------------

for(i in 1:length(unique(T_Test$variables))){

  
  df <- T_Test %>%
    ungroup() %>%
    dplyr::select(-c("NAME")) %>%
    filter(Type_of_Cluster == unique(T_Test$Type_of_Cluster)[3]) %>%
    filter(variables == unique(T_Test$variables)[i]) %>%
    na.omit()
  
  Remove <- df %>%
    group_by(GEOID) %>%
    summarize(Frequency = n()) %>%
    filter(Frequency > 1)
  
  df %>% 
    filter(GEOID %in% Remove$GEOID) %>%
    group_by(Clusters) %>%
    get_summary_stats(Values)
  
  
  merge <- df %>% 
    filter(GEOID %in% Remove$GEOID) %>%
    pairwise_t_test(Values ~ Clusters) %>%
    mutate(Variable = unique(T_Test$variables)[i])
  
  Pairwise_Tests <- rbind(Pairwise_Tests, merge)
  
}

write.csv(Pairwise_Tests %>%
            arrange(p.signif),"Data_Analysis/Pairwise_Ttest.csv")

