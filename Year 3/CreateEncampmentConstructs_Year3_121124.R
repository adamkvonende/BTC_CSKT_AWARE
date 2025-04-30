
rm(list = ls())



if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor)


source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")
source("C:/Users/vonbe/OneDrive/Adam/Stats Local/R code Collection/GTsummaryGlobalOpts.R")


### Pre


setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/Encampment")
d1 <- rio::import("CSKT AWARE 2024 Encampment Pre-Survey_8.14.24.xlsx")
#names(d1)
# Make all lower cases
names(d1)<-tolower(names(d1))
#names(d1)
d1$time <-"pre"
d1 <- d1 %>% rename(comfortable_songs=confortable_songs,
                    ethnicity_tribalmember=ethnicity_tribal_member,
                    ethnicity_tribaldescendent=ethnicity_tribal_descendent,
                    ethnicity_tribe_no=ethnicity_not_ai)
# Post


d2 <- rio::import("CLEAN_CSKT AWARE 2024 Encampment Post-Survey_9.3.24.xlsx")
#names(d2)
# Make all lower cases
names(d2)<-tolower(names(d2))
#names(d2)
d2$time <-"post"
d2 <- d2 %>% rename(participate_landscapes=participate_landscpes)


#Combine

d <- bind_rows(d1,d2)
#names(d)

d <- d %>% arrange(aware_id, time)
######################################################################
# Create demographic variables
######################################################################

# adult

d <- d %>%
  mutate(adult = case_when(
    str_starts(aware_id, "y") ~ 0,
    str_starts(aware_id, "a") ~ 1,
    TRUE ~ NA_real_  # Assign NA if it starts with something else
  ))


d %>% tabyl(adult)

###  Age

# View age related variables
#d %>% dplyr::select(aware_id, contains("age")) %>% View()
# Fill in
d <- d %>% group_by(aware_id) %>% fill(age, .direction="updown")
# Create approximate continuous age (9=)
d <- d %>% mutate(age_cont = case_when(age<=9~age+10,
                                         age==10~25,
                                         age==11~35,
                                         age==12~45))

# Create categories
d <- d %>% mutate(age.cat4 = case_when(age_cont<=20~1,
                                         age_cont>20&age_cont<=30~2,
                                         age_cont>30&age_cont<=40~3,
                                         age_cont>40~4))
# Create a factor version of variable
d <-d %>% mutate(age.cat4.f= factor(age.cat4))
#levels(d$age.cat4.f) 
# expand levels as does not exist in data
age.cat4.f<- fct_expand(c("1","2","3","4"))

# relevel with missing level last
levels(d$age.cat4.f) <-    c("10-20","40+","30-40", "20-30")   


# Relevel the factor so youngest is first
d$age.cat4.f<-fct_relevel(d$age.cat4.f, c("10-20","20-30", "30-40", "40+"))




### Gender


# convert to numeric

d$gender <-as.numeric(d$gender)
# Fill in
d <- d %>% group_by(aware_id) %>% fill(gender, .direction="updown")

# Create categories
d<- d %>% mutate(gender.3cat = case_when(gender==0~0,
                                           gender==1~1,
                                           TRUE~3))

d$gender.3cat.f <-factor(d$gender.3cat,levels = c(0, 1, 3), labels=c("Male", "Female", "Other"))
#levels(d$gender.3cat.f)


### Role

# View all role vars
#d %>% select(aware_id, time, contains("role")) %>% View()
# Fill in
d <- d %>% group_by(aware_id) %>% fill(contains("role_"), .direction="updown")

# convert role vars to numeric
rolevars <- c("role_parent", "role_grandparent", "role_student", "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", "role_elder", "role_commember", "role_mhprovider", "role_apprentice", "role_other")
d <- d %>% mutate_at(rolevars, function(x) as.numeric(x))



### School

### School
# view all school related vars
#d %>% dplyr::select(contains("school")) %>% View()
# convert to numeric
d <- d %>% mutate_at("school", function(x) as.numeric(x))
# Fill in
d <- d %>% group_by(aware_id) %>% fill(school, .direction="updown")
# other = missing
d <- d %>% mutate(school = ifelse(school %in% c(0,8), 10,school))

# create factor
d <-d %>% mutate(school.f = factor(school,levels = c("1","2", "3", "4","5", "6", "7", "8","9", "10")))
levels(d$school.f)
levels(d$school.f)<-c(
  "Mission Middle School",
  "Mission High School",
  "Ronan Middle School",
  "Ronan High School",
  "Mission elementary",
  "Polson Middle School",
  "Polson High School",
  "Arlee Middle School",
  "Arlee High school",
  "Other"
)

d$school_encamp <- d$school

###  Ethnicity

# convert to numeric

#d %>% dplyr::select(contains("ethn")) %>% View()

d <- d %>% mutate_at(vars(contains("ethnicity")), function(x) as.numeric(x))

# Fill in
d <- d %>% group_by(aware_id) %>% fill(contains("ethn"), .direction="updown")


# Fill in missing with zeroes

#d <- d %>% mutate_at(vars(contains("ethnicity")), function(x) ifelse(is.na(x),0,x))

d %>% tabyl(adult, time)
d %>% filter(time=="pre") %>% tabyl(adult)
######################################################################
# Scale items
######################################################################

#dput(names(d))

# survey vars
scale.items <-c("familiar_languages", "familiar_landscapes", "familiar_storytelling", 
              "familiar_songs", "familiar_materials", "familiar_rounds", "familiar_dancing", 
              "familiar_practices", "familiar_games", "familiar_foods", "participate_languages", 
              "participate_landscapes", "participate_storytelling", "participate_songs", 
              "participate_materials", "partcipate_rounds", "participate_dancing", 
              "participate_practices", "participate_games", "participate_food", 
              "comfortable_languages", "comfortable_landscapes", "comfortable_storytelling", 
              "comfortable_songs", "comfortable_materials", "comfortable_rounds", 
              "comfortable_dancing", "comfortable_practices", "comfortable_games", 
              "comfortable_food", "interested", "important_culture", "important_learn_languages", 
              "important_learn_principles", "important_learn_traditions_practices", 
              "important_learn_stories", "important_learn_family_traditions", 
              "important_learn_community_traditions", "important_learn_family_history", 
              "important_learn_land", "important_learn_elders", "important_learn_music", 
              "important_learn_spiritual", "important_learn_issues", "important_learn_community_involvement", 
              "feel_connected", "speak_selis", "understand_selis", "speak_ksanka", 
              "understand_ksanka",
              "learn_languages", "learn_landscapes", 
              "learn_storytelling", "learn_songs", "learn_materials", "learn_rounds", 
              "learn_dancing", "learn_practices", "learn_games", "learn_foods", 
              "strengthen_languages", "strengthen_landscapes", "strengthen_storytelling", 
              "strengthen_songs", "strengthen_materials", "strengthen_rounds", 
              "strengthen_dancing", "strengthen_practices", "strengthen_games", 
              "strengthen_foods", "participate_landscapes", "comfortable_songs", 
              "overall_health", "agree_disagree_talk", "agree_disagree_ask", 
              "agree_disagree_belonging", "agree_disagree_felt_tribal", "agree_disagree_attachment", 
              "agree_disagree_role_models", "agree_disagree_be_around", "agree_disagree_listen_carefully", 
              "agree_disagree_take_class", "agree_disagree_connection", "agree_disagree_life", 
              "agree_disagree_tribal_name", "agree_disagree_native_teachings", 
              "agree_disagree_dreams", "everyday_handling", "everyday_family", 
              "everday_friends", "everday_school_work", "everday_cope", "everday_satisfied_family", 
              "everyday_hopeful", "everyday_enjoy", "people_listen_understand", 
              "people_talk_problems", "people_crisis_support", "people_spend_time", 
              "people_chance_interested")

d <- d %>% mutate_at(scale.items, function(x) as.numeric(x))
d <-d %>% mutate_at(scale.items,function(x) floor(x)) # use lower if intermediate value (e.g. 1.5)


######################################################################
# Create scales
######################################################################

dput(names(d))

scale.categories <- list(fam_encamp = c("familiar_languages", "familiar_landscapes", "familiar_storytelling", 
                         "familiar_songs", "familiar_materials", "familiar_rounds", "familiar_dancing", 
                         "familiar_practices", "familiar_games", "familiar_foods"),
                         comfort_encamp= c("comfortable_languages", "comfortable_landscapes", "comfortable_storytelling", 
                         "comfortable_songs", "comfortable_materials", "comfortable_rounds", 
                         "comfortable_dancing", "comfortable_practices", "comfortable_games", 
                         "comfortable_food"),
                         imp_learn_encamp = c("important_culture", "important_learn_languages", 
                         "important_learn_principles", "important_learn_traditions_practices", 
                         "important_learn_stories", "important_learn_family_traditions", 
                         "important_learn_community_traditions", "important_learn_family_history", 
                         "important_learn_land", "important_learn_elders", "important_learn_music", 
                         "important_learn_spiritual", "important_learn_issues", "important_learn_community_involvement"),
                         cultural_connect_encamp = c(
                           "agree_disagree_ask", "agree_disagree_belonging", "agree_disagree_felt_tribal", 
                           "agree_disagree_attachment", "agree_disagree_role_models", "agree_disagree_be_around"
                         ),           
                         interpersonal_connect_encamp = c(
                                                "everyday_family", "everday_friends", "people_listen_understand",
                                                "people_talk_problems", "people_crisis_support", "people_spend_time",
                                                "people_chance_interested"
                                              ),
                        overall_wellbeing_encamp = c(
                                                "everyday_handling", "everday_school_work", "everday_cope",
                                                "everday_satisfied_family", "everyday_hopeful", "everyday_enjoy"
                                              ),
                        tribal_identity_encamp = c(
                                                "agree_disagree_talk", "agree_disagree_role_models", "agree_disagree_listen_carefully",
                                                "agree_disagree_take_class", "agree_disagree_connection", "agree_disagree_life",
                                                "agree_disagree_tribal_name", "agree_disagree_native_teachings", "agree_disagree_dreams"
                                              ) ,                        
                        social_support_encamp =c("people_listen_understand", 
                                                  "people_talk_problems", "people_crisis_support", "people_spend_time", 
                                                  "people_chance_interested"))
              


# Now create a scale for each category

for (i in 1:length(scale.categories)) {
  curr.cat <- scale.categories[[i]]
  tmp<-d[, curr.cat]
  nvars<-ncol(tmp)
  tmp$miss_count <- rowSums(is.na(tmp)) 
  tmp$miss_pct <- tmp$miss_count/nvars
  scale_name <- names(scale.categories)[i]
  d[, paste0(scale_name,"_scale")] =   ifelse(tmp$miss_pct <=1,rowMeans(tmp[,1:nvars],na.rm=T),NA_real_)
  
}

#names(d)

### participanion separately: 0=never,5=everyday, sum up across all variables
#old: times per week: d <- d %>%  mutate(across(scale.categories$part_encamp, ~ case_when(. == 0 ~ 0, . == 1 ~ 0.05, . == 2 ~ 0.3, . == 3 ~ 1, . == 4 ~ 3, . == 5 ~ 7)))
# d<- d %>% ungroup() %>% mutate(part_encamp_sum = rowSums(select(.,all_of(part_encamp)), na.rm = TRUE))




# For participation, strenght, and learn -- only calculate if they participate 
# part  = 0 - 5 (0 = did not participate)
# strength = 1 - 6 (1 = did not participate)
# learn = 1 - 6 (1 = did not participate)


part.categories <- list(
part_encamp = c("participate_landscapes", "participate_storytelling", "participate_songs", 
                "participate_materials", "partcipate_rounds", "participate_dancing", 
                "participate_practices", "participate_games", "participate_food"),
learn_encamp = c("learn_languages", "learn_landscapes", 
                 "learn_storytelling", "learn_songs", "learn_materials", "learn_rounds", 
                 "learn_dancing", "learn_practices", "learn_games", "learn_foods"),
strengthen_encamp = c("strengthen_languages", "strengthen_landscapes", "strengthen_storytelling", 
                      "strengthen_songs", "strengthen_materials", "strengthen_rounds", 
                      "strengthen_dancing", "strengthen_practices", "strengthen_games"))


for (i in 1:length(part.categories)) {
  curr.cat <- part.categories[[i]]
  tmp<-d[, curr.cat]
  if (names(part.categories)[i] == "part_encamp") { 
    tmp <-tmp %>% mutate_all(function(x)ifelse(x==0,NA,x)) # remove if 0 (no participaton)
  } else{
    tmp <-tmp %>% mutate_all(function(x)ifelse(x==1,NA,x-1)) # remove if 1; substract 1 so that it will be 1-5
    }
  nvars<-ncol(tmp)
  tmp$miss_count <- rowSums(is.na(tmp)) 
  tmp$miss_pct <- tmp$miss_count/nvars
  scale_name <- names(part.categories)[i]
  d[, paste0(scale_name,"_scale")] =  rowMeans(tmp[,1:nvars],na.rm=T)
  
}

 
 

# Save key variables

dput(names(d))

scales <- grep("_scale", names(d), value = TRUE)

to_keep <- c("aware_id", "combined_name", "time", "adult", "age","age_cont", "age.cat4", "age.cat4.f", "age_type", 
             "gender", "gender_describe", "gender.3cat","gender.3cat.f", 
             "school_encamp", "school_other", "school.f",
             "race_native", "race_asian", "race_black", "race_hispanic", "race_middleeastern", 
             "race_hawaiian", "race_white", "race_other", "race_selfdescribe", 
             "ethnicity_tribalmember", "ethnicity_tribaldescendent", "ethnicity_tribe_no", 
             "race_tribe","role_parent", "role_grandparent", "role_student", "role_teacher", 
             "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
             "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
             "role_other", "role_other_describe",
             "role_other", "role_other_describe","feel_connected", "speak_selis", "understand_selis", "speak_ksanka", 
             "understand_ksanka",scales,
             all_of(scale.items)
             )

d2 <- d %>% select(all_of(to_keep))

### Save data set

names(d2)
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d2, "CSKT_EncampmentConstructsPrePost_Year3_121124.xlsx", overwrite=T)

