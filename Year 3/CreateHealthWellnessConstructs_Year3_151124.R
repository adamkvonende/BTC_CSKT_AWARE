
# Clear all objects in memory

rm(list = ls())

# Load packages

if (!require(pacman)) install.packages("pacman")
p_load(rio, tidyverse,janitor, gtsummary,lubridate)

source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")


### Import data


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
scale.items <-grep("^q", names(d1), value = TRUE)
d1 <- d1 %>% mutate_at(scale.items, function(x) as.numeric(x))


# Post


d2 <- rio::import("CSKT AWARE Health and Wellness Survey Dataset_Year 3_Spring2024.xlsx")
# Make all names lower cases
names(d2)<-tolower(names(d2))
d2$time <-"post"
names(d2)[42:171]<-paste0("q",names(d2)[42:171])
# Make scale items numeric
scale.items <-grep("^q", names(d2), value = TRUE)
d2 <- d2 %>% mutate_at(scale.items, function(x) as.numeric(x))


#Combine

d3 <- bind_rows(d1,d2)
# Sort
d3 <- d3 %>% arrange(aware_id, time)

# Subset to ONLY those with aware Ids

d3 <- d3 %>% filter(!is.na(aware_id))



######################################################################
# Create demographic variables
######################################################################


###  Age

# Fill in age by person (some missing)
d3 <- d3 %>% group_by(aware_id) %>% fill(age_2, .direction="updown")

# remove all non-numeric characters
d3$age2 <- gsub("[^0-9.]+", "", d3$age_2) # remove all non-numeric characters
# convert to numeric
d3$age2 <-as.numeric(d3$age2) 
# Create approximate continuous age (9=)
d3 <- d3 %>% mutate(age_cont = case_when(age2<=9~age2+10,
                                     age2==10~25,
                                     age2==11~35,
                                     age2==12~45))

# Create categories
d3 <- d3 %>% mutate(age.cat4 = case_when(age_cont<=20~1,
                                         age_cont>20&age_cont<=30~2,
                                         age_cont>30&age_cont<=40~3,
                                         age_cont>40~4))
# Create a factor version of variable
d3 <-d3 %>% mutate(age.cat4.f= factor(age.cat4,levels = c("1","2","3","4")))
levels(d3$age.cat4.f)

# expand levels as does not exist in data
age.cat4.f<- fct_expand(c("1","2","3","4"))

# relevel with missing level last
levels(d3$age.cat4.f) <-    c("10-20", "20-30","30-40","40+")   

# Relevel the factor so youngest is first
d3$age.cat4.f<-fct_relevel(d3$age.cat4.f, c("10-20","20-30", "30-40", "40+"))




### Adult
d3 <- d3 %>% mutate(adult = ifelse(age_type==0,1,0))


### Gender


# convert to numeric

d3$gender <-as.numeric(d3$gender)
# Fill in
d3 <- d3 %>% group_by(aware_id) %>% fill(gender, .direction="updown")

# Create categories
d3<- d3 %>% mutate(gender.3cat = case_when(gender==0~0,
                                           gender==1~1,
                                           TRUE~3))

d3$gender.3cat.f <-factor(d3$gender.3cat, labels=c("Male", "Female", "Other"))
levels(d3$gender.3cat.f)
d3 %>% tabyl(gender.3cat.f)


### Role


# convert role vars to numeric
rolevars <- c("role_parent", "role_grandparent", "role_student", "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", "role_elder", "role_commember", "role_mhprovider", "role_apprentice", "role_other")
d3 <- d3 %>% mutate_at(rolevars, function(x) as.numeric(x))

# Fill in
d3 <- d3 %>% group_by(aware_id) %>% fill(contains("role_"), .direction="updown")

### School

# convert to numeric
d3 <- d3 %>% mutate_at("school", function(x) as.numeric(x))
# Fill in
d3 <- d3 %>% group_by(aware_id) %>% fill(school, .direction="updown")


# create factor
d3 <-d3 %>% mutate(school.f = factor(school,levels = c("1","2", "3", "4","5", "6", "7", "8","9", "10")))
levels(d3$school.f)
levels(d3$school.f)<-c(
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

###  Ethnicity

# convert to numeric

d3 <- d3 %>% mutate_at(vars(contains("ethnicity")), function(x) as.numeric(x))

# Fill in missing with zeroes

d3 <- d3 %>% mutate_at(vars(contains("ethnicity")), function(x) ifelse(is.na(x),0,x))
# Fill in
d3 <- d3 %>% group_by(aware_id) %>% fill(contains("ethn"), .direction="updown")

### Race


racevars <- c("race_native", "race_white", "race_black", "race_hispanic", "race_other")
d3 <- d3 %>% mutate_at(racevars , function(x) as.numeric(x))

# Fill in
d3 <- d3 %>% group_by(aware_id) %>% fill(contains("race_"), .direction="updown")

# Fill in missing with zeroes

d3 <- d3 %>% mutate_at(racevars, function(x) ifelse(is.na(x),0,x))


################################################################################
# Create scale scores
################################################################################


scale.categories <- list(
  hw_cultural_connect = c(
    "q8_2", "q8_3", "q8_4", "q8_5", "q8_7"
  ),
  hw_interpersonal_connect = c(
    "q9_2", "q9_3", "q10_1", "q10_2", "q10_3", 
    "q10_4", "q10_5"
  ),
  hw_overall_wellbeing = c(
    "q9_1", "q9_4", "q9_5", 
    "q9_6"
  ),
  hw_tribal_identity = c(
    "q8_1", "q8_6", "q8_8", "q8_9", 
    "q8_10", "q8_11", "q8_12", "q8_13", "q8_14"
  )
)



# Now create a scale for each category

for (i in 1:length(scale.categories)) {
  curr.cat <- scale.categories[[i]]
  tmp<-d3[, curr.cat]
  nvars<-ncol(tmp)
  tmp$miss_count <- rowSums(is.na(tmp)) 
  tmp$miss_pct <- tmp$miss_count/nvars
  scale_name <- names(scale.categories)[i]
  d3[, paste0(scale_name,"_scale")] =   ifelse(tmp$miss_pct <=1,rowMeans(tmp[,1:nvars],na.rm=T),NA_real_)
  
}


######################################################################
# Save the derived data set
######################################################################



to_keep <- c("aware_id","combined_name",  "race_native", "race_white", 
             "race_black", "race_hispanic", "race_other", "race_selfdescribe", 
             "ethnicity_tribalmember", "ethnicity_tribaldescendant", "ethnicity_tribe_no", 
             "race_tribe", "school", "role_parent", 
             "role_grandparent", "role_teacher", "role_schooladmin", "role_para", 
             "role_adultsuppyouth", "role_knowledgekeeper", "role_elder", 
             "role_commember", "role_mhprovider", "role_apprentice", "role_student", 
             "role_other", "role_other_describe","age_cont", "age.cat4", "age.cat4.f", "adult", "gender.3cat", 
             "gender.3cat.f","school", "school.f", "time", 
             "race_asian", "race_middleeastern", "race_hawaiian", 
             "age_cont", "age.cat4", "age.cat4.f", "adult", "gender.3cat", 
             "gender.3cat.f", "school.f", "overall_health")

d4<-d3 %>% select(aware_id, time,all_of(to_keep), contains("scale"))



setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d4, "CSKT_HealthWellnessConstructs_Year3_191124.xlsx",overwrite=TRUE)

