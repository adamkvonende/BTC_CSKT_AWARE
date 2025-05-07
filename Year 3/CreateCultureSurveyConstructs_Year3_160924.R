
rm(list = ls())



if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor)


source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")
source("C:/Users/vonbe/OneDrive/Adam/Stats Local/R code Collection/GTsummaryGlobalOpts.R")


### Import data


#Pre


setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/CultureSurvey")
d1 <- rio::import("CSKT AWARE Culture Survey Year 3_Fall2023.xlsx")
# Make all lower cases
names(d1)<-tolower(names(d1))
d1$time <-"pre"
# Some names don't start with 'q'; add a q to the beginning
names(d1)[40:184]<-paste0("q",names(d1)[40:184])
#school
d1$school <- as.numeric(d1$school)
scale.items <-grep("^q", names(d1), value = TRUE)
d1 <- d1 %>% mutate_at(scale.items, function(x) as.numeric(x))



# Pos


d2 <- rio::import("CSKT AWARE Culture Survey Year 3_Spring2024.xlsx")
# Make all lower cases
names(d2)<-tolower(names(d2))
d2$time <-"post"
names(d2)[42:171]<-paste0("q",names(d2)[42:171])
scale.items <-grep("^q", names(d2), value = TRUE)
d2 <- d2 %>% mutate_at(scale.items, function(x) as.numeric(x))


#Combine

d3 <- bind_rows(d1,d2)
#names(d3)

d3 <- d3 %>% arrange(aware_id, time)

# Subset to ONLY those with aware Ids

d3 <- d3 %>% filter(!is.na(aware_id))



######################################################################
# Replace missing and make numeric
######################################################################
# survey vars
scale.items <-grep("^q", names(d3), value = TRUE)
d3 <- d3 %>% mutate_at(scale.items, function(x) as.numeric(x))
d3 <-d3 %>% mutate_at(scale.items,function(x) floor(x)) # use lower if intermediate value (e.g. 1.5)

######################################################################
# Create scales
######################################################################

scale.categories <- list(fam_overall = c("q5a1_fam_lang_salish", "q5a2_fam_lang_kootenai", "q5b1_fam_med_tree", "q5b2_fam_vcamp", "q5b3_fam_jocko", "q5b4_fam_dance_boy",  "q5b5_fam_falls", "q5b6_fam_cliff", "q5b7_fam_loseau", "q5b8_fam_hog", "q5c1_fam_story_salish", "q5c2_fam_story_kootenai", "q5d1_fam_song_cont", 
                         "q6b2_fam_jump", "q5d3_fam_song_stick", "q5d4_fam_song_gend", "q6b3_fam_sweat", "q5d6_fam_song_seas", "q6b4_fam_morn", "q5d8_fam_song_med", "q5e1_fam_pouch", "q6c6_fam_dye", "q6c3_fam_bead", "q6c4_fam_weav", 
                         "q6c5_fam_yaya", "q5e6_fam_cedar", "q6c7_fam_art", "q6c8_fam_drum", "q5e9_fam_parent", "q5e10_fam_reg", "q5e11_fam_game", "q5e12_fam_bags", 
                         "q5e13_fam_parf", "q5e14_fam_fans", "q5e15_fam_bustle", "q6d1_fam_trap",  "q6d3_fam_hunt", "q6d4_fam_plant", "q6d5_fam_harv", 
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
                         fam_seasonal_rounds=c("q6d1_fam_trap",  "q6d3_fam_hunt", "q6d4_fam_plant", "q6d5_fam_harv", "q6d6_fam_story", "q6d7_fam_tipi", "q6d9_fam_tan", "q5f9_fam_fire", "q5f10_fam_snare", "q5f11_fam_snow", "q5f12_fam_food", "q5f13_fam_pro", "q5f14_fam_rope"),
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

d3 <- d3 %>% mutate_at(scale.categories$part, funs(case_when(.==0~0, .==1~0.05, .==2~.3,.==3~1,.==4~3,.==5~7)))

# Now create a scale for each category

curr.cat <- c("q8_connect")

# Note: This function returns the row-wise mean across all items if <=30% of the items are missing

for (i in 1:length(scale.categories)) {
  curr.cat <- scale.categories[[i]]
  tmp <- d3[, curr.cat, drop = FALSE]
  nvars <- ncol(tmp)
  
  tmp$miss_count <- rowSums(is.na(tmp))
  tmp$miss_pct <- tmp$miss_count / nvars
  scale_name <- names(scale.categories)[i]
  
  if (nvars == 1) {
    # Only one item - just use the value if not too much missing
    d3[, paste0("culture_", scale_name, "_scale")] <- ifelse(tmp$miss_pct <= 0.3, tmp[[1]], NA_real_)
  } else {
    # Multiple items - take row mean
    d3[, paste0("culture_", scale_name, "_scale")] <- ifelse(tmp$miss_pct <= 0.3,
                                                             rowMeans(tmp[, 1:nvars], na.rm = TRUE),
                                                             NA_real_)
  }
}

# Create sum for participant variables

part_vars <-c("q6_part_lang", "q6_part_land", "q6_part_story", 
                "q6_part_song", "q6_part_mat", "q6_part_round", "q6_part_dance", 
                "q6_part_pract", "q6_part_game", "q6_part_food")

d3<- d3 %>% ungroup() %>% mutate(culture_part_sum = rowSums(select(.,all_of(part_vars)), na.rm = TRUE))


# make culture date

d3$date_culture <- d3$date


### Save data set

to_keep <- c("aware_id", "culture_part_sum", "culture_fam_overall_scale", 
             "culture_fam_speak_scale", "culture_fam_trad_landscapes_scale", 
             "culture_fam_storytelling_scale", "culture_fam_songs_scale", 
             "culture_fam_trad_materials_scale", "culture_fam_seasonal_rounds_scale", 
             "culture_fam_dancing_scale", "culture_fam_trad_practices_scale", 
             "culture_fam_trad_games_scale", "culture_fam_trad_foods_scale", 
             "culture_comf_scale", "culture_connect_scale", "culture_learn_scale", 
             "culture_imp_scale", "culture_speak_scale", "culture_camp_new_scale", 
             "culture_camp_perc_scale")
             
             

d4<-d3 %>% select(aware_id, time,all_of(to_keep))


#names(d3)
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d4, "CSKT_CultureSurveyConstructs_Year3_180924.xlsx", overwrite = TRUE)

