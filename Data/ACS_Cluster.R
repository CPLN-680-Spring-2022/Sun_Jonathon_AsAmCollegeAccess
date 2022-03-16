#Census Data Cluster 

# ---------------------------
ACS_Variables <- function(year,search){
  load_variables(year,
                 "acs5",
                 cache = FALSE) %>%
    filter(concept == search)  %>%
    mutate(Merge_name = paste(name,"E", sep =""))
}

Clean_ACS_Variables <- function(df){  
  df %>%
    mutate(
      label = str_remove(label,"Estimate!!"),
      label = str_remove(label,"Total:!!"),
      label = str_remove_all(label,"!!"),
      label = str_replace_all(label,":"," "),
      label = str_trim(label),
      label = str_replace_all(label," ","_"),
      label = str_replace(label,"Total","Total_Population"))
}

# ACS Wide for Cluster Analysis ------------------------
A <- ACS_Variables(2019,"RACE") %>%
  mutate(Name_Code = "Race",
         Category = case_when(
           grepl("White", label) ~ "white",
           grepl("Black", label) ~ "Black",
           grepl("Indian", label) ~ "Indigenous",
           grepl("Asian|Native", label) ~ "AAPI",
           grepl("other", label) ~ "Other",
           grepl("Two", label) ~ "multiracial"
         )) %>%
  slice(-1)
B <- load_variables(2019,
                    "acs5",
                    cache = FALSE) %>%
  filter(grepl("ASIAN ALONE BY SELECTED GROUPS", concept)) %>%
  mutate(Merge_name = paste(name,"E", sep =""),
         Name_Code = "Asian",
         Category = case_when(
           grepl("Pakistani|Bangaldeshi|Sri Lankan|Nepal|Indian|Bangladeshi|Bhutan",label) ~ "South_Asian",
           grepl("Vietnamese|Cambodian|Lao|Hmong|Burmese|Indonesian|Malaysian",label) ~ "Southeast_Asian",
           grepl("Filipino",label) ~ "Filipino",
           grepl("Chinese|Japanese|Taiwanese|Korean|Mongolian|Thai",label) ~ "East_Asian",
           grepl("Other|Two or more",label) ~ "Other",
           grepl("Okinawan", label) ~ "okinwan")
  ) %>%
  slice(-1)

C <- load_variables(2019,
                    "acs5",
                    cache = FALSE) %>%
  filter(concept == "HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)") %>%
  mutate(Merge_name = paste(name,"E", sep =""),
         Name_Code = "Income",
         Category = case_when(
           grepl("10,|15,|20,|25,|30,|35,|40,|45,",label) ~ "Less_$50k",
           grepl("50,|60,|75,",label)  ~ "$50-$99k",
           grepl("100,|125,|150,|200,",label) ~ "$100+K")
  ) %>%
  slice(-1)

C$Category[14:15] <- "$100+K"



D <- load_variables(2019,
                    "acs5",
                    cache = FALSE) %>%
  filter(concept == "EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER") %>%
  mutate(Merge_name = paste(name,"E", sep =""),
         Name_Code = "Edu_Attainment",
         Category = case_when(
           grepl("No schooling completed|grade|Kindergarten|Nursery",label) ~"Some_HS_or_less",
           grepl("Bachelor|Professional|Master|Doctorate",label) ~ "Bachelor_more",
           grepl("Regular high school diploma|high school|GED|college|Associate",label) ~ "High_School_No_Bachelor",
         )) %>%
  slice(-1)

Variables <- bind_rows(A,B,C,D)
Variables <- Clean_ACS_Variables(Variables) %>%
  filter(!label == "Total_Population") %>%
  mutate(Full_rename = paste0(Category,"_",Name_Code),
         Long_rename = paste0(label,"_",Name_Code))

ACS_Cluster <- get_acs(geography = "tract", 
                       variables = Variables$name, 
                       state = "PA",
                       county = "Philadelphia",
                       output = "wide",
                       geometry = FALSE,
                       year = 2019) %>% 
  dplyr::select(!ends_with("E"))

col.from <- colnames(ACS_Cluster)

colnames(ACS_Cluster)

cols <- Variables$Long_rename
namestoAdd <- c("GEOID")
cols <- append(namestoAdd, cols)
cols.to <- append(cols, "geometry")  

ACS_Cluster  <- ACS_Cluster  %>%
  rename_at(vars(col.from), function(x) cols) 

ACS_Cluster_Tall <- get_acs(geography = "tract", 
                            variables = Variables$name, 
                            state = "PA",
                            county = "Philadelphia",
                            output = "tidy",
                            geometry = FALSE,
                            year = 2019) %>%
  select(-moe)


for(i in 1:nrow(Variables)){
  
  ACS_Cluster_Tall <- ACS_Cluster_Tall %>%
    mutate(variable = str_replace(variable,Variables$name[i],Variables$Full_rename[i]))
}


ACS_Cluster_Group <- ACS_Cluster_Tall %>%
  group_by(GEOID,NAME,variable) %>%
  summarize(Total = sum(estimate)) %>%
  na.omit() %>%
  pivot_wider(names_from = variable,
              values_from = Total)


#Philadelphia_tracts %>%
#  select(GEOID) %>%
#  left_join(Race %>%
#              select(GEOID,variable,est), 
#           by = "GEOID")