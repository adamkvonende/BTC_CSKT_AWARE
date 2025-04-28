
# Clear all objects in memory

rm(list = ls())

# Load packages

if (!require(pacman)) install.packages("pacman")
p_load(rio, tidyverse,janitor, gtsummary,lubridate)


### Import data

d <- rio::import("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/CSKT AWARE Health and Wellness Survey Dataset_Year 2_11.8.23.xlsx")
names(d)
# Convert all names to lowercase
names(d)<-tolower(names(d))

# Subset to ONLY those with aware Ids

d <- d %>% rename(aware_id=`aware id`)
d <- d %>% rename(collection.timepoint=`collection timepoint`)
d2 <- d %>% filter(aware_id !=".")


# Create number of assessments variable

d2 <- d2 %>% group_by(aware_id) %>% arrange(collection.timepoint) %>% mutate(name_index = row_number())
d2<-d2 %>% group_by(aware_id) %>% mutate(n_assess = max(name_index))


# fix dates

d2$date2 <- as.Date("1900-01-01")  + as.numeric(d2$date)
d2 <- d2 %>% dplyr::select(aware_id, date, date2, everything())


######################################################################
# Create demographic variables
######################################################################


###  Age

# View age related variables
d2 %>% dplyr::select(contains("age")) %>% View()
# remove all non-numeric characters
d2$age2 <- gsub("[^0-9.]+", "", d2$age_2) # remove all non-numeric characters
# convert to numeric
d2$age2 <-as.numeric(d2$age2) 
# Create approximate continuous age (9=)
d2 <- d2 %>% mutate(age_cont = case_when(age2<=9~age2+10,
                                     age2==10~25,
                                     age2==11~35,
                                     age2==12~45))

# Create categories
d2 <- d2 %>% mutate(age.cat4 = case_when(age_cont<=20~1,
                                         age_cont>20&age_cont<=30~2,
                                         age_cont>30&age_cont<=40~3,
                                         age_cont>40~4))
# Create a factor version of variable
d2 <-d2 %>% mutate(age.cat4.f= factor(age.cat4))
levels(d2$age.cat4.f) 
levels(d2$age.cat4.f) <-    c("40+","30-40","20-30","10-20")               
# Relevel the factor so youngest is first
d2$age.cat4.f<-fct_relevel(d2$age.cat4.f, c("10-20","20-30", "30-40", "40+"))


### Adult

# If aware ID starts with 'a' then individual is adult
# If aware ID starts with 'y' then individual is youth
# if aware ID is missing then use age (>18 = adult)

d2 <- d2 %>% mutate(adult = case_when(grepl("^a", aware_id)~1, #if ID starts with
                                      grepl("^y", aware_id)~0,
                                      is.na(aware_id)~ifelse(age_cont>=18,1,0),
                                      aware_id=="0"~ifelse(age_cont>=18,1,0)))
d2 %>% tabyl(adult)

### Gender


# convert to numeric

d2$gender <-as.numeric(d2$gender1)
# Create categories
d2<- d2 %>% mutate(gender.3cat = case_when(gender1==0~0,
                                      gender1==1~1,
                                      TRUE~3))

d2$gender.3cat.f <-factor(d2$gender.3cat, labels=c("Male", "Female", "Other"))
levels(d2$gender.3cat.f)


### Role



# View all role vars
d2 %>% select(contains("role")) %>% View()
# convert role vars to numeric
rolevars <- c("role_parent", "role_grandparent", "role_student", "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", "role_elder", "role_commember", "role_mhprovider", "role_apprentice", "role_other")
d2 <- d2 %>% mutate_at(rolevars, function(x) as.numeric(x))



### School

# view all school related vars
d2 %>% dplyr::select(contains("school")) %>% View()
# convert to numeric
d2 <- d2 %>% mutate_at("school", function(x) as.numeric(x))
# create factor
d2 <-d2 %>% mutate(school.f = factor(school))
levels(d2$school.f)
levels(d2$school.f)<-c("Ronan High","Polson High","Mission High","Mission Middle","Ronan Middle","Polson Middle")



###  Ethnicity

# convert to numeric

d2 <- d2 %>% mutate_at(vars(contains("ethnicity")), function(x) as.numeric(x))

# Fill in missing with zeroes

d2 <- d2 %>% mutate_at(vars(contains("ethnicity")), function(x) ifelse(is.na(x),0,x))


### Race

names(d2)

d2 %>% select(contains("race")) %>% View()
racevars <- c("race_indigenous", "race_white", "race_black", "race_hispanic", "race_other")
d2 <- d2 %>% mutate_at(racevars , function(x) as.numeric(x))

# Fill in missing with zeroes

d2 <- d2 %>% mutate_at(racevars, function(x) ifelse(is.na(x),0,x))

# Fill errorenous values for race_other

d2 <- d2 %>% mutate(race_other = ifelse(race_other>1,0,race_other))

################################################################################
# Create scale scores
################################################################################



# q8_1,q8_2,  q8_6,  q8_7 -- Tribal Community Connection
# q8_3,  q8_4 ,q8_5,  q8_8,  q8_9,  q8_10, q8_11, q8_12,q8_13, q8_14 -- Native history and identity
# q9_1,  q9_2,  q9_3,  q9_4, q9_5,  q9_6 -- Overall well-being and functioning
# q10_1, q10_2, q10_3, q10_4 -- social support
# q11_1, q11_2-- low mood



# Define scale sets
tribal_connect_vars <- c("q8_1", "q8_2", "q8_6", "q8_7")
native_identity_vars <- c("q8_3", "q8_4", "q8_5", "q8_8", "q8_9", "q8_10", "q8_11", "q8_12", "q8_13", "q8_14")#
overall_wellbeing_vars <- c("q9_1", "q9_2", "q9_3", "q9_4", "q9_5", "q9_6")
social_support_vars <- c("q10_1", "q10_2", "q10_3", "q10_4", "q10_5")
low_mood_vars <- c("q11_1", "q11_2")
all_vars <- c(tribal_connect_vars,native_identity_vars,overall_wellbeing_vars,social_support_vars,low_mood_vars )

# Make all vars numeric

d2 <- d2 %>% mutate_at(all_vars, function(x) as.numeric(x))



# Tribal connect

tmp <- d2 %>% select(tribal_connect_vars) # create a temp data set
tmp$miss_count <- rowSums(is.na(tmp[,tribal_connect_vars])) # get number of missing items
tmp$miss_pct <- tmp$miss_count/length(tribal_connect_vars)  # get % of missing items
tmp$scale_score <- ifelse(tmp$miss_pct <=0.3,rowMeans(tmp[,tribal_connect_vars],na.rm=T),NA_real_) # scale score is mean of items unless % missing > 30%
d2$tribal_connect_scale <- tmp$scale_score # add to main data set


# native identity

tmp <- d2 %>% select(native_identity_vars) # create a temp data set
tmp$miss_count <- rowSums(is.na(tmp[,native_identity_vars])) # get number of missing items
tmp$miss_pct <- tmp$miss_count/length(native_identity_vars)  # get % of missing items
tmp$scale_score <- ifelse(tmp$miss_pct <=0.3,rowMeans(tmp[,native_identity_vars],na.rm=T),NA_real_) # scale score is mean of items unless % missing > 30%
d2$native_identity_scale <- tmp$scale_score # add to main data set

# well being

tmp <- d2 %>% select(overall_wellbeing_vars) # create a temp data set
tmp$miss_count <- rowSums(is.na(tmp[,overall_wellbeing_vars])) # get number of missing items
tmp$miss_pct <- tmp$miss_count/length(overall_wellbeing_vars)  # get % of missing items
tmp$scale_score <- ifelse(tmp$miss_pct <=0.3,rowMeans(tmp[,overall_wellbeing_vars],na.rm=T),NA_real_) # scale score is mean of items unless % missing > 30%
d2$overall_wellbeing_scale <- tmp$scale_score # add to main data set


# social support

tmp <- d2 %>% select(social_support_vars) # create a temp data set
tmp$miss_count <- rowSums(is.na(tmp[,social_support_vars])) # get number of missing items
tmp$miss_pct <- tmp$miss_count/length(social_support_vars)  # get % of missing items
tmp$scale_score <- ifelse(tmp$miss_pct <=0.3,rowMeans(tmp[,social_support_vars],na.rm=T),NA_real_) # scale score is mean of items unless % missing > 30%
d2$social_support_scale <- tmp$scale_score # add to main data set

# Low mood -- this one is just the sum of the 2 variables


tmp <- d2 %>% select(low_mood_vars) # create a temp data set
tmp$miss_count <- rowSums(is.na(tmp[,low_mood_vars])) # get number of missing items
tmp$scale_score <- ifelse(tmp$miss_count==0,rowMeans(tmp[,low_mood_vars],na.rm=T),NA_real_) # scale score if and only if both are non missing
d2$low_mood_scale <- tmp$scale_score # add to main data set


### Or do the whole thing in a loop

# 
# all.sets <- list(tribal_connect_vars,native_identity_vars,overall_wellbeing_vars,social_support_vars )
# stubs <- c("tribal_connect","native_identity","overall_wellbeing","social_support")
# 
# for (i in 1:length(all.sets)) {
#       set <- all.sets[[i]]
#       tmp <-  d2 %>% select(all_of(set))
#       tmp$miss_count <- rowSums(is.na(tmp)) 
#       tmp$miss_pct <- tmp$miss_count/length(set)  
#       #tmp[, paste0(stubs[[i]],"_scale")] =   ifelse(tmp$miss_pct <=0.3,rowMeans(tmp,na.rm=T),NA_real_)
#       d2[, paste0(stubs[[i]],"_scale")] =   ifelse(tmp$miss_pct <=0.3,rowMeans(tmp,na.rm=T),NA_real_)
#       
# }




######################################################################
# Save the derived data set
######################################################################

names(d2)
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d2, "CSKT_HealthWellnessConstructs_022624.xlsx")



######################################################################
# Descriptive table
######################################################################



tbl1 <-
   d2 %>% ungroup() %>% 
   select(age_cont, age.cat4.f, adult,gender.3cat.f,school.f,ethnicity_tribalmember,ethnicity_tribaldescendant, 
          race_indigenous, race_white,race_black,race_hispanic,race_other,
          role_parent,
          role_grandparent,role_student,role_teacher,role_schooladmin,
          role_para,role_adultsuppyouth,role_knowledgekeeper,
          role_elder,role_commember,role_mhprovider,role_apprentice,
          role_other) %>%
   tbl_summary(by=adult,
               statistic = list(all_continuous() ~ "{mean} ({sd})", 
                                all_categorical() ~ "{n} ({p}%)"),
               digits = list(all_continuous() ~ 1),
               missing = "no")  %>% as_tibble()

tbl1

# Save as excel file



setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Analyses/HealthWellness")
rio::export(tbl1,"HealthWellnessDescrTable.xlsx")



