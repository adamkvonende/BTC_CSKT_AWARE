
rm(list = ls())



if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor)


source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")
source("C:/Users/vonbe/OneDrive/Adam/Stats Local/R code Collection/GTsummaryGlobalOpts.R")


### Import data

# new data set as of 2 Nov 2023

setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data")
d <- rio::import("CSKT AWARE Culture Survey Year 2 Dataset_11.13.23.xlsx")
names(d)
# Make all lower cases
names(d)<-tolower(names(d))
# Some names don't start with 'q'; add a q to the beginning
names(d)[33:177]<-paste0("q",names(d)[33:177])
names(d)


# Subset to ONLY those with aware Ids

d2 <- d %>% filter(aware_id !=".")


# Create number of assessments variable

d2 <- d2 %>% group_by(aware_id) %>% arrange(collection.timepoint) %>% mutate(name_index = row_number())
d2<-d2 %>% group_by(aware_id) %>% mutate(n_assess = max(name_index))



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

d2 <- d2 %>% mutate(adult = case_when(grepl("^a", aware_id)~1, 
                                      grepl("^y", aware_id)~0))

d2 %>% tabyl(adult)

### Gender


# convert to numeric

d2$gender <-as.numeric(d2$gender)
# Create categories
d2<- d2 %>% mutate(gender.3cat = case_when(gender==0~0,
                                           gender==1~1,
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
#d2 %>% dplyr::select(contains("school")) %>% View()
# convert to numeric
d2 <- d2 %>% mutate_at("school", function(x) as.numeric(x))
# create factor
d2 <-d2 %>% mutate(school.f = factor(school))
levels(d2$school.f)<-c("Mission Middle", "Other","Mission High","Polson Middle","Polson High","Ronan Middle",
                       "Ronan High","Arlee Middle","Arlee High","Two Eagle River School")

# Relevel
d2$school.f<-fct_relevel(d2$school.f, c("Mission Middle","Mission High","Polson Middle","Polson High","Ronan Middle",
                                        "Ronan High","Arlee Middle","Arlee High","Two Eagle River School", "Other"))


###  Ethnicity

# convert to numeric

d2 <- d2 %>% mutate_at(vars(contains("ethnicity")), function(x) as.numeric(x))

# Fill in missing with zeroes

d2 <- d2 %>% mutate_at(vars(contains("ethnicity")), function(x) ifelse(is.na(x),0,x))



######################################################################
# Replace missing and make numeric
######################################################################
# survey vars
scale.items <-grep("^q", names(d2), value = TRUE)

d2 <- d2 %>% mutate_at(scale.items, function(x) as.numeric(x))
d2 <-d2 %>% mutate_at(scale.items,function(x) floor(x)) # use lower if intermediate value (e.g. 1.5)


######################################################################
# Create scales
######################################################################


t <- d2 %>% select(contains("_fam"))
dput(names(t))

scale.categories <- list(fam_overall = c("q5a1_fam_lang_salish", "q5a2_fam_lang_kootenai", "q5b1_fam_med_tree", "q5b2_fam_vcamp", "q5b3_fam_jocko", "q5b4_fam_dance_boy",  "q5b5_fam_falls", "q5b6_fam_cliff", "q5b7_fam_loseau", "q5b8_fam_hog", "q5c1_fam_story_salish", "q5c2_fam_story_kootenai", "q5d1_fam_song_cont", 
                         "q6b2_fam_jump", "q5d3_fam_song_stick", "q5d4_fam_song_gend", "q6b3_fam_sweat", "q5d6_fam_song_seas", "q6b4_fam_morn", "q5d8_fam_song_med", "q5e1_fam_pouch", "q6c6_fam_dye", "q6c3_fam_bead", "q6c4_fam_weav", 
                         "q6c5_fam_yaya", "q5e6_fam_cedar", "q6c7_fam_art", "q6c8_fam_drum", "q5e9_fam_parent", "q5e10_fam_reg", "q5e11_fam_game", "q5e12_fam_bags", 
                         "q5e13_fam_parf", "q5e14_fam_fans", "q5e15_fam_bustle", "q6d1_fam_trap", "q6d2_fam_fish", "q6d3_fam_hunt", "q6d4_fam_plant", "q6d5_fam_harv", 
                         "q6d6_fam_story", "q6d7_fam_tipi", "q6d9_fam_tan", "q5f9_fam_fire", "q5f10_fam_snare", "q5f11_fam_snow", "q5f12_fam_food", "q5f13_fam_pro", "q5f14_fam_rope", "q6f1_fam_pjd", "q6f2_fam_jing", "q6f4_fam_fancy", 
                         "q6f5_fam_grass", "q6g10_fam_trad", "q5g7_fam_dance_chick", "q5g8_fam_dance_med", "q6g1_fam_smud", "q6g3_fam_med", "q6g4_fam_fast", "q6g5_fam_bur", 
                        "q6g6_fam_celeb", "q6g7_fam_pray", "q6g12_fam_cult", "q6g13_fam_fea", "q5h9_fam_name", "q6h1_fam_hand", "q5i2_fam_hoop", "q5i3_fam_atl", 
                         "q5i4_fam_rock", "q6h3_fam_ball", "q6h4_fam_shin", "q5i7_fam_run", "q5j1_fam_bread", "q5j2_fam_fish", "q5j3_fam_meat", "q5j4_fam_root", 
                         "q6i12_fam_tea", "q5j6_fam_berry", "q5j7_fam_bark", "q5j8_fam_ack", "q5j9_fam_sap", "q5j10_fam_lich"),
                          fam_speak =c("q5a1_fam_lang_salish", "q5a2_fam_lang_kootenai"),
                         fam_trad_landscapes= c("q5b1_fam_med_tree", "q5b2_fam_vcamp", "q5b3_fam_jocko", "q5b4_fam_dance_boy", "q5b5_fam_falls", "q5b6_fam_cliff", "q5b7_fam_loseau", "q5b8_fam_hog"),
                         fam_storytelling=c("q5c1_fam_story_salish", "q5c2_fam_story_kootenai"),
                         fam_songs=c("q5d1_fam_song_cont", "q6b2_fam_jump", "q5d3_fam_song_stick", "q5d4_fam_song_gend", "q6b3_fam_sweat", "q5d6_fam_song_seas", "q6b4_fam_morn", "q5d8_fam_song_med"),
                         fam_trad_materials=c("q5e1_fam_pouch", "q6c6_fam_dye", "q6c3_fam_bead", "q6c4_fam_weav", "q6c5_fam_yaya", "q5e6_fam_cedar", "q6c7_fam_art", "q6c8_fam_drum", "q5e9_fam_parent", "q5e10_fam_reg", "q5e11_fam_game", "q5e12_fam_bags", "q5e13_fam_parf", "q5e14_fam_fans", "q5e15_fam_bustle"),
                         fam_seasonal_rounds=c("q6d1_fam_trap", "q6d2_fam_fish", "q6d3_fam_hunt", "q6d4_fam_plant", "q6d5_fam_harv", "q6d6_fam_story", "q6d7_fam_tipi", "q6d9_fam_tan", "q5f9_fam_fire", "q5f10_fam_snare", "q5f11_fam_snow", "q5f12_fam_food", "q5f13_fam_pro", "q5f14_fam_rope"),
                         fam_dancing=c("q6f1_fam_pjd", "q6f2_fam_jing", "q6f4_fam_fancy", "q6f5_fam_grass", "q6g10_fam_trad", "q5g7_fam_dance_chick", "q5g8_fam_dance_med"),
                         fam_trad_practices=c("q6g1_fam_smud", "q6g3_fam_med", "q6g4_fam_fast", "q6g5_fam_bur", "q6g6_fam_celeb", "q6g7_fam_pray", "q6g12_fam_cult", "q6g13_fam_fea", "q5h9_fam_name"),
                         fam_trad_games=c("q6h1_fam_hand", "q5i2_fam_hoop", "q5i3_fam_atl", "q5i4_fam_rock", "q6h3_fam_ball", "q6h4_fam_shin", "q5i7_fam_run"),
                         fam_trad_foods=c("q5j1_fam_bread", "q5j2_fam_fish", "q5j3_fam_meat", "q5j4_fam_root", "q6i12_fam_tea", "q5j6_fam_berry", "q5j7_fam_bark", "q5j8_fam_ack", "q5j9_fam_sap", "q5j10_fam_lich"),
                         part=c("q6_part_lang", "q6_part_land", "q6_part_story", "q6_part_song", "q6_part_mat", "q6_part_round", "q6_part_dance", "q6_part_pract", "q6_part_game", "q6_part_food"),
                         comf=c("q7_comf_lang", "q7_comf_land", "q7_comf_story", "q7_comf_song", "q7_comf_mat", "q7_comf_round", "q7_comf_dance", "q7_comf_pract", "q7_comf_game", "q7_comf_food"),
                         connect=c("q8_connect"),
                         learn=c("q9_learn_lang", "q9_learn_land", "q9_learn_story", "q9_learn_song", "q9_learn_mat", "q9_learn_round", "q9_learn_dance", "q9_learn_pract", "q9_learn_game", "q9_learn_food"),
                         imp = c("q10_imp_lang", "q10_imp_val", "q10_imp_trad", "q10_imp_teach", "q10_imp_fam_trad", "q10_imp_com_trad", "q10_imp_fam_hist", "q10_imp_land", "q10_imp_eld", "q10_imp_music", "q10_imp_spirit", "q10_imp_issue", "q10_imp_com_inv"),
                         speak=c("q11_salish_speak", "q12_salish_und", "q13_koot_speak","q14_koot_und"),
                         camp_new=c("q15_camp_lang", "q15_camp_land", "q15_camp_story","q15_camp_song", "q15_camp_mat", "q15_camp_round", "q15_camp_dance", "q15_camp_pract", "q15_camp_game", "q15_camp_food"),
                         camp_perc=c("q16_camp_con_fam", "q16_camp_con_cult", "q16_camp_con_comm", "q16_camp_fam", "q16_camp_act"))
              


# For participation, convert to approximate 'times per week.

d2<- d2 %>% mutate_at(scale.categories$part, funs(case_when(.==0~0, .==1~0.05, .==2~.3,.==3~1,.==4~3,.==5~7)))

# Now create a scale for each category

for (i in 1:length(scale.categories)) {
  curr.cat <- scale.categories[[i]]
  tmp<-d2[, curr.cat]
  nvars<-ncol(tmp)
  tmp$miss_count <- rowSums(is.na(tmp)) 
  tmp$miss_pct <- tmp$miss_count/nvars
  scale_name <- names(scale.categories)[i]
  d2[, paste0(scale_name,"_scale")] =   ifelse(tmp$miss_pct <=0.3,rowMeans(tmp[,1:nvars],na.rm=T),NA_real_)
  
}

names(d2)

### Save data set

names(d2)
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d2, "CSKT_CultureSurveyConstructs_022624.xlsx")

