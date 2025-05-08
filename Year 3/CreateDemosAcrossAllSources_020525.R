





# Clear all objects in memory

rm(list = ls())

if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor,psych, gtsummary,officer,flextable,
       summarytools,descr, readxl,ggview,ggpubr)


source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")
source("C:/Users/vonbe/OneDrive/Adam/Stats Local/R code Collection/GTsummaryGlobalOpts.R")


### Defint variables to keep


keep <- c("combined_name", "first_name", "last_name", 
          "aware_id", "date",  "collection timepoint", 
          "age_2", "gender", "gender_describe", "race_native", "race_white", 
          "race_black", "race_hispanic", "race_other", "race_selfdescribe", 
          "ethnicity_tribalmember", "ethnicity_tribaldescendant", "ethnicity_tribe_no", 
          "race_tribe", "age_type", "school_id", "school", "role_parent", 
          "role_grandparent", "role_teacher", "role_schooladmin", "role_para", 
          "role_adultsuppyouth", "role_knowledgekeeper", "role_elder", 
          "role_commember", "role_mhprovider", "role_apprentice", "role_student", 
          "role_other", "role_other_describe","race_asian", "race_middleeastern", "race_hawaiian")



######################################################################
# Culture survey
######################################################################


#Pre


setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/CultureSurvey")
d1 <- rio::import("CSKT AWARE Culture Survey Year 3_Fall2023.xlsx")
# Make all lower cases
names(d1)<-tolower(names(d1))
d1$time <-"pre"
#school
d1$school <- as.numeric(d1$school)
d1 <- d1 %>% mutate(race_asian=NA, race_middleeastern=NA,race_hawaiian=NA)
d1 <- d1 %>% select(all_of(keep))


# Pos

d2 <- rio::import("CSKT AWARE Culture Survey Year 3_Spring2024.xlsx")
# Make all lower cases
names(d2)<-tolower(names(d2))
d2$time <-"post"
d2 <- d2 %>% select(all_of(keep))


#Combine

d3 <- bind_rows(d1,d2)
#names(s)

d3 <- d3 %>% arrange(aware_id, time)

# Subset to ONLY those with aware Ids

d3 <- d3 %>% filter(!is.na(aware_id))

# Make school id numeric

d3 <- d3 %>% mutate(school_id = as.numeric(school_id))



s1 <- d3  %>% mutate(survey="Culture")




######################################################################
# H&W
######################################################################

#Pre


setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/HealthAndWellnessSurvey")
d1 <- rio::import("CSKT AWARE Health and Wellness Survey Dataset_Year 3_Fall2023.xlsx")
names(d1)
# Make all lower cases
names(d1)<-tolower(names(d1))
names(d1)
d1$time <-"pre"
#school
d1$school_id <- as.numeric(d1$school_id)
d1$school <- as.numeric(d1$school)
d1 <- d1 %>% mutate(race_asian=NA, race_middleeastern=NA,race_hawaiian=NA)
d1 <- d1 %>% select(all_of(keep))



# Post


d2 <- rio::import("CSKT AWARE Health and Wellness Survey Dataset_Year 3_Spring2024.xlsx")
# Make all names lower cases
names(d2)<-tolower(names(d2))
d2$time <-"post"
d2 <- d2 %>% select(all_of(keep))


#Combine

d3 <- bind_rows(d1,d2)

# Subset to ONLY those with aware Ids

d3 <- d3 %>% filter(!is.na(aware_id))

# Make school id numeric

d3 <- d3 %>% mutate(school_id = as.numeric(school_id))

#To combine

s2 <- d3 %>%  mutate(survey="H&W")

######################################################################
# Encampment
######################################################################


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
                    ethnicity_tribaldescendant=ethnicity_tribal_descendent,
                    ethnicity_tribe_no=ethnicity_not_ai)

d1<-d1 %>% mutate(age_2= age, `collection timepoint`="")
d1 <- d1 %>% select(all_of(keep))



# Post



d2 <- rio::import("CLEAN_CSKT AWARE 2024 Encampment Post-Survey_9.3.24.xlsx")
#names(d2)
# Make all lower cases
names(d2)<-tolower(names(d2))
names(d2)
d2$time <-"post"
d2 <- d2 %>% rename(participate_landscapes=participate_landscpes)
d2 <- d2 %>% rename(ethnicity_tribaldescendant=ethnicity_tribaldescendent)

d2<-d2 %>% mutate(age_2= age, `collection timepoint`="")
d2 <- d2 %>% select(all_of(keep))

#Combine

d3 <- bind_rows(d1,d2)

# Make school id numeric

d3 <- d3 %>% mutate(school_id = as.numeric(school_id))

# Some amendments to match other datasets


#To combine

s3 <- d3 %>% mutate(survey="Encampment")




###############################################################################
# Combine all surveys
###############################################################################

s <- bind_rows(s1,s2,s3)


###############################################################################
# Age
###############################################################################

# Fill in age by ID
s <- s %>% group_by(aware_id) %>% fill(age_2, .direction="updown")
s %>% tabyl(age_2)
# Create approximate continuous age (9=)
s <- s %>% mutate(age_cont = case_when(age_2<=9~age_2+10,
                                         age_2==10~25,
                                         age_2==11~35,
                                         age_2==12~45))

# Create categories
s <- s %>% mutate(age.cat4 = case_when(age_cont<=20~1,
                                         age_cont>20&age_cont<=30~2,
                                         age_cont>30&age_cont<=40~3,
                                         age_cont>40~4))
# Create a factor version of variable
s <-s %>% mutate(age.cat4.f= factor(age.cat4,levels = c("1","2","3","4")))

# expand levels as does not exist in data
age.cat4.f<- fct_expand(c("1","2","3","4"))

levels(s$age.cat4.f)

# relevel with missing level last
levels(s$age.cat4.f) <-    c("10-20", "20-30","30-40","40+")   

# Relevel the factor so youngest is first
s$age.cat4.f<-fct_relevel(s$age.cat4.f, c("10-20","20-30", "30-40", "40+"))


### Adult

s <- s %>% mutate(adult = ifelse(age_type==0,1,0))


# Calculate 'adult' when it is missing

s <- s %>% mutate(adult2 = case_when(grepl("^a", aware_id)~1, 
                                       grepl("^y", aware_id)~0))

s %>% tabyl(adult2)

###############################################################################
### Gender
##############################################################################

s %>% tabyl(gender)


# Fill in
s <- s %>% group_by(aware_id) %>% fill(gender, .direction="updown")

# Create categories
s<- s %>% mutate(gender.3cat = case_when(gender==0~0,
                                           gender==1~1,
                                           TRUE~3))

s$gender.3cat.f <-factor(s$gender.3cat, labels=c("Male", "Female", "Other"))
levels(s$gender.3cat.f)
s %>% tabyl(gender.3cat.f ) %>% adorn_totals()


###############################################################################
### Role
###############################################################################


# Fill in
s <- s %>% group_by(aware_id) %>% fill(contains("role_"), .direction="updown")

rolevars <- c("role_parent", "role_grandparent", "role_student", "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", "role_elder", "role_commember", "role_mhprovider", "role_apprentice", "role_other")
s <- s %>% mutate_at(rolevars, function(x) as.numeric(x))


# Create a 'no role info available' variable

rolevars <- c("role_parent", "role_grandparent", "role_teacher", 
              "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
              "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
              "role_student", "role_other")
s <- s %>% ungroup() %>% mutate(no_role_info = if_else(rowSums(is.na(select(., all_of(rolevars)))) == length(rolevars), 1, 0))

# fill in role = 0 when SOME role info is available

s <- s %>%mutate(across(all_of(rolevars), ~ if_else(is.na(.x) & no_role_info == 0, 0, .x)))


###############################################################################
### School
###############################################################################

# convert to numeric
s <- s %>% mutate_at("school", function(x) as.numeric(x))
# Change 0 (other) to 8

s %>% tabyl(school)

s <- s %>% mutate(school = ifelse(school==0,8,school))
# Fill in
s <- s %>% group_by(aware_id) %>% fill(school, .direction="updown")


# create factor
s <-s %>% mutate(school.f = factor(school,levels = c("1","2", "3", "4","5", "6", "7", "8","9", "10")))
levels(s$school.f)
levels(s$school.f)<-c(
  "Mission Middle School",
  "Mission High School",
  "Polson Middle School",
  "Polson High School",
  "Ronan Middle School",
  "Ronan High School",
  "Arlee Middle School",
  "Arlee High school",
  "Two Eagle River School",
  "Other"
)
s2 %>% tabyl(school.f)


# Fill in school as "NA -- adult" when adult is present

s <- s %>% mutate(school.f = if_else(adult2==1, "NA - adult", school.f))


s <- s %>%
  mutate(school.f = case_when(
    adult2 == 1 ~ "NA - adult",
    TRUE ~ as.character(school.f)
  ),
  school.f = factor(school.f))

s %>% tabyl(school.f)
s$school.f <-factor(s$school.f)

###############################################################################
### Ethnicity
###############################################################################

###  Ethnicity

# convert to numeric

s <- s %>% mutate_at(vars(contains("ethnicity")), function(x) as.numeric(x))

# Fill in missing with zeroes

s <- s %>% mutate_at(vars(contains("ethnicity")), function(x) ifelse(is.na(x),0,x))
# Fill in
s <- s %>% group_by(aware_id) %>% fill(contains("ethn"), .direction="updown")


###############################################################################
### Race
###############################################################################

# fill in 'key' race vars with 0 (not hawaain, middleeasterm, etc, as these were added laster)


racevars<- c("race_native", "race_white", "race_black", "race_hispanic", "race_other")
s <- s %>% mutate_at(racevars, function(x) if_else(is.na(x),0,x))


######################################################################
# Save final data set
######################################################################

# Subset to one row per person

s2 <- s %>% group_by(aware_id) %>% slice(1)


### Table to check


spec_demo_vars <- c("age_cont",  "age.cat4.f", "adult2", "gender.3cat.f", "school.f",
                    "race_native", "race_white", "race_black", "race_hispanic", "race_other","race_asian", "race_middleeastern", "race_hawaiian",
                    "ethnicity_tribalmember", "ethnicity_tribaldescendant", "ethnicity_tribe_no", 
                    "role_parent", "role_grandparent", "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
                    "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
                    "role_student", "role_other", "no_role_info")



tbl1 <-
  s2 %>% ungroup() %>% 
  select(all_of(spec_demo_vars), no_role_info) %>%
  tbl_summary(by="adult2",
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ 1),
              missing = "ifany") 

tbl1




# Save demographic data set


to_keep <- c("aware_id","combined_name", "adult2", "school.f","age_cont", 
             "age.cat4", "age.cat4.f", "gender.3cat", "gender.3cat.f",
             "race_native", "race_white", "race_black", 
             "race_hispanic", "race_other","race_asian", "race_middleeastern", "race_hawaiian", "race_selfdescribe", "ethnicity_tribalmember", 
             "ethnicity_tribaldescendant", "ethnicity_tribe_no", "race_tribe", 
             "role_parent", "role_grandparent", "role_teacher", 
             "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
             "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
             "role_student", "role_other", "role_other_describe", "no_role_info")


s3<- s2 %>% select(all_of(to_keep))
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(s3, "CSKT_DemoConstructs_020525.xlsx",overwrite=T)

