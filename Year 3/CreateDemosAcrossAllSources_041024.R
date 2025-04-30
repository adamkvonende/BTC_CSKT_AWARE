





# Clear all objects in memory

rm(list = ls())

if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor,psych, gtsummary,officer,flextable,
       summarytools,descr, readxl,ggview,ggpubr)


source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")
source("C:/Users/vonbe/OneDrive/Adam/Stats Local/R code Collection/GTsummaryGlobalOpts.R")


######################################################################
# Organise descripties -- use all sources
######################################################################
#dput(names(d1))
demo_vars <- c("race_native", "race_white", 
               "race_black", "race_hispanic", "race_other", "race_selfdescribe", 
               "ethnicity_tribalmember", "ethnicity_tribaldescendant", "ethnicity_tribe_no", 
               "race_tribe", "school", "role_parent", "role_grandparent", "role_teacher", 
               "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
               "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
               "role_student", "role_other", "role_other_describe", "age_cont", 
               "age.cat4", "age.cat4.f", "adult", "gender.3cat", "gender.3cat.f", 
               "school.f", "race_asian", "race_middleeastern", "race_hawaiian")


### Health and wellness

setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
a<- rio::import("CSKT_HealthWellnessConstructs_Year3_180924.xlsx")
a2 <- a %>% select(aware_id, combined_name, time, all_of(demo_vars))
a2 <- a2 %>% arrange(aware_id, time)
#a2 %>% select(aware_id, time, contains("role")) %>% View()
# Note -- a lot of rle info is missing for spring version of H&w
#subset to one row per person
a3 <- a2 %>% group_by(aware_id) %>% slice(1)

### Culture survey

b <- rio::import("CSKT_CultureSurveyConstructs_Year3_180924.xlsx")
# add race vars that arent present
b<- b %>% mutate(race_asian=NA, race_middleeastern=NA, race_hawaiian=NA)
b2 <- b %>% select(aware_id,combined_name, time, all_of(demo_vars))
b2 <- b2 %>% arrange(aware_id, time)
#b2 %>% select(aware_id, time, contains("role")) %>% View()
#subset to one row per person
b3 <- b2 %>% group_by(aware_id) %>% slice(1)

### Encampment

c <- rio::import("CSKT_EncampmentConstructsPrePost_Year3_200424.xlsx")
# add vars that arent present
c<- c %>% mutate(adult=NA, ethnicity_tribaldescendant=NA,school=NA)
c2 <- c %>% select(aware_id,combined_name, time, all_of(demo_vars),school_encamp)
c2 <- c2 %>% arrange(aware_id, time)
#subset to one row per person
c3 <- c2 %>% group_by(aware_id) %>% slice(1)

### Combine all

d <- rbind(a3,b3,c3)
d <- d %>% arrange(aware_id)
# Fill in all information
d2 <- d %>% group_by(aware_id) %>% fill(all_of(demo_vars), .direction="updown")
d2 <- d2 %>% mutate(tag=1) %>% group_by(aware_id) %>% mutate(t_surveys = sum(tag))



# Create a 'no role info available' variable

rolevars <- c("role_parent", "role_grandparent", "role_teacher", 
              "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
              "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
              "role_student", "role_other")
d2 <- d2 %>% ungroup() %>% mutate(no_role_info = if_else(rowSums(is.na(select(., all_of(rolevars)))) == length(rolevars), 1, 0))

# fill in role = 0 when SOME role info is available

d2 <- d2 %>%mutate(across(all_of(rolevars), ~ if_else(is.na(.x) & no_role_info == 0, 0, .x)))

# Calculate 'adult' when it is missing

d2 <- d2 %>% mutate(adult2 = case_when(grepl("^a", aware_id)~1, 
                                       grepl("^y", aware_id)~0))

d2 %>% tabyl(adult2)

# Fill in school as "NA -- adult" when adult is present

d2 <- d2 %>% mutate(school.f = if_else(adult2==1, "NA - adult", school.f))

# fill in 'key' race vars with 0 (not hawaain, middleeasterm, etc, as these were added laster)


racevars<- c("race_native", "race_white", "race_black", "race_hispanic", "race_other")
d2 <- d2 %>% mutate_at(racevars, function(x) if_else(is.na(x),0,x))

# Subset to one row per person

d3 <- d2 %>% group_by(aware_id) %>% slice(1)

# Some manual school entries

d3 %>% filter(is.na(school.f)) %>% select(aware_id) %>% pull()
d3 <- d3 %>% mutate(school.f = if_else(aware_id=="y1147", "Ronan High School", school.f))


### Table to check


spec_demo_vars <- c("age_cont",  "age.cat4.f", "adult2", "gender.3cat.f", "school.f",
                    "race_native", "race_white", "race_black", "race_hispanic", "race_other","race_asian", "race_middleeastern", "race_hawaiian",
                    "ethnicity_tribalmember", "ethnicity_tribaldescendant", "ethnicity_tribe_no", 
                    "role_parent", "role_grandparent", "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
                    "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
                    "role_student", "role_other", "no_role_info")



tbl1 <-
  d3 %>% ungroup() %>% 
  select(all_of(spec_demo_vars), t_surveys, no_role_info) %>%
  tbl_summary(by="adult2",
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ 1),
              missing = "ifany") 

tbl1




# Save demographic data se

dput(names(d3))

to_keep <- c("aware_id","combined_name", "adult2", "school.f","age_cont", 
             "age.cat4", "age.cat4.f", "gender.3cat", "gender.3cat.f",
             "race_native", "race_white", "race_black", 
             "race_hispanic", "race_other","race_asian", "race_middleeastern", "race_hawaiian", "race_selfdescribe", "ethnicity_tribalmember", 
             "ethnicity_tribaldescendant", "ethnicity_tribe_no", "race_tribe", 
             "role_parent", "role_grandparent", "role_teacher", 
             "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
             "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
             "role_student", "role_other", "role_other_describe", "no_role_info")


d4 <- d3 %>% select(all_of(to_keep))
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d4, "CSKT_DemoConstructs_200924.xlsx",overwrite=T)

