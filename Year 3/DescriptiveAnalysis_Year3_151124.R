
# Clear all objects in memory

rm(list = ls())

if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor,psych, gtsummary,
       summarytools,descr, readxl,lubridate,openxlsx)


######################################################################
# Load data and combine all data sets
######################################################################

# Specify which demographic variables to EXCLUDE in each data set, as we have compiled them in 
# CreateDemosAcrossAllSources_041024.R

demo_vars <- c("race_native", "race_white", 
               "race_black", "race_hispanic", "race_other", "race_selfdescribe", 
               "ethnicity_tribalmember", "ethnicity_tribaldescendant", "ethnicity_tribe_no", 
               "race_tribe", "school", "role_parent", "role_grandparent", "role_teacher", 
               "role_schooladmin", "role_para", "role_adultsuppyouth", "role_knowledgekeeper", 
               "role_elder", "role_commember", "role_mhprovider", "role_apprentice", 
               "role_student", "role_other", "role_other_describe", "age_cont", 
               "age.cat4", "age.cat4.f", "adult", "gender.3cat", "gender.3cat.f", 
               "school.f", "race_asian", "race_middleeastern", "race_hawaiian")


# Health and wellness survey 
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
a<- rio::import("CSKT_HealthWellnessConstructs_Year3_191124.xlsx")
a2 <- a %>% select(aware_id,time, !all_of(demo_vars))


# Culture survey 
b <- rio::import("CSKT_CultureSurveyConstructs_Year3_180924.xlsx")
b <- b %>% mutate(race_asian=NA,race_middleeastern=NA, race_hawaiian=NA )
b2 <- b %>% select(aware_id, !all_of(demo_vars))



# Activity data
e <- rio::import("CSKT_ActivityConstructs_Yr3_080924.xlsx")
e2 <- e %>% select(-gender,-native,school_act=school)



# Demographic data (from CreateDemosAcrossAllSources_041024.R)

setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
f <- rio::import("CSKT_DemoConstructs_200924.csv")


# set up base data with pre and post for all individuals -- then we merge in

unique_ids <- unique(c(a2$aware_id, b2$aware_id))
df <- data.frame(aware_id=rep(unique_ids, each=2))
df$time <- rep(c("pre", "post"), times=length(unique_ids))

# Merge in other datasets

d <- df %>% left_join(a2, by=c("aware_id", "time"))
d <- d %>% left_join(b2, by=c("aware_id", "time"))
d <- d %>% left_join(e2, by=c("aware_id"))
d <- d %>% left_join(f, by=c("aware_id"))



###############################################################################
# Create new variables in the combined data set
###############################################################################

# Numeric time variable (1=pre, 2=post)
d2 <- d %>% mutate(time2 = ifelse(time=="pre",1,2)) %>% select(aware_id, time2, everything())
d2 <- d2 %>% arrange(aware_id, time2)
# Tag first instance of AWARE ID
d2 <- d2 %>% arrange(aware_id, time2) %>% group_by(aware_id) %>% mutate(tag_id = if_else(row_number()==1,1,0))
# Tag if individual has any encampent data
d2 <- d2 %>% mutate(any_encampment = ifelse(aware_id %in% c$aware_id,1,0))


######################################################################
# Determine who completed each survey at each time point
######################################################################

# Define culture vars -- people who have non missing values across any variables will 
# be considered to have done the survey
culture_vars<-c("culture_part_sum", "culture_fam_overall_scale", 
                "culture_fam_speak_scale", "culture_fam_trad_landscapes_scale", 
                "culture_fam_storytelling_scale", "culture_fam_songs_scale", 
                "culture_fam_trad_materials_scale", "culture_fam_seasonal_rounds_scale", 
                "culture_fam_dancing_scale", "culture_fam_trad_practices_scale", 
                "culture_fam_trad_games_scale", "culture_fam_trad_foods_scale", 
                "culture_comf_scale", "culture_connect_scale", "culture_learn_scale", 
                "culture_imp_scale", "culture_speak_scale", "culture_camp_new_scale", 
                "culture_camp_perc_scale")
# Define health and wellness vars 
hw_vars <- c("hw_cultural_connect_scale", "hw_interpersonal_connect_scale", "hw_overall_wellbeing_scale", "hw_tribal_identity_scale")

# Determine: those who did the culture survey; the who did the H&W survey; those with activitiy data
d2 <- d2 %>% ungroup() %>% mutate(has_culture = if_else(rowSums(!is.na(select(., all_of(culture_vars)))) > 0, 1, 0))
d2 <- d2 %>% ungroup() %>% mutate(has_hw = if_else(rowSums(!is.na(select(., all_of(hw_vars)))) > 0, 1, 0))
d2 <- d2 %>% ungroup() %>% mutate(has_activity = if_else(!is.na(t_act), 1, 0))



# Now determine who has this  data across fall and spring and overall
d2 <- d2 %>% ungroup() %>% group_by(aware_id) %>% mutate(has_hw_fall = max(has_hw[time2==1],na.rm=T),
                                                         has_hw_spring = max(has_hw[time2==2],na.rm=T),
                                                         has_culture_fall = max(has_culture[time2==1],na.rm=T),
                                                         has_culture_spring = max(has_culture[time2==2],na.rm=T),
                                                         has_any_hw = max(has_hw,na.rm=T),
                                                         has_any_culture = max(has_culture,na.rm=T))
d2 <- d2 %>% mutate(has_any_both = if_else(has_any_hw==1|has_any_culture==1,1,0))
d2 <- d2 %>% mutate(has_both_all = if_else(has_hw_fall==1&has_hw_spring==1&has_culture_fall==1&has_culture_spring==1,1,0))
d2 <- d2 %>% mutate(has_both_spring = if_else(has_hw_spring==1&has_culture_spring==1,1,0))

             


######################################################################
# Subset to only youth who have activities data
######################################################################

# Because we are focusing on kids with activity data, we filter only kids
# who have activity data

d3 <- d2 %>% filter(adult2==0 & has_activity==1)                      
length(unique(d3$aware_id))



######################################################################
# Activity dosage pror to assessment
######################################################################

# Now we need to determine which assessment we use for each child
# For kids who have spring data (fall and spring or spring only), we use their spring assessment
# For kids who have only fall data, we use their fall assessment


# For kids whose activities start in the fall and end in the spring we consider 'fall and spring'
# For kids whose activites start in the spring we consider 'spring only'
# For kids whose activities start and end in fall we consider 'fall only'
# spring survey

# Create category variable

d3 <-d3 %>% mutate(act_complete = case_when(dateact_start<mdy("10/01/2023")&dateact_end>mdy("03/01/24")~"Fall & Spring",
                                            dateact_start>mdy("10/01/2023")&dateact_end>mdy("03/01/24")~"Spring only",
                                            dateact_start<mdy("12/01/2023")&dateact_end<mdy("03/01/24")~"Fall only") )


######################################################################
# Create dataset with one observation per person
######################################################################

# Now that we know which assessment to use based on their participation,
# we tag the assessment we want to keep


d3 <- d3 %>% mutate(keep = case_when(act_complete=="Fall & Spring" & time2==2~1,
                                     act_complete=="Spring only" & time2==2~1,
                                     act_complete=="Fall only" & time2==1~1,
                                     TRUE~0))

# Subset the data to the assessment we want to keep


d4 <- d3 %>% filter(keep==1)


# Now reccaulcuate who has culure and H&w data
d4 <- d4 %>% ungroup() %>% mutate(has_culture2 = if_else(rowSums(!is.na(select(., all_of(culture_vars)))) > 0, 1, 0))
d4 <- d4 %>% ungroup() %>% mutate(has_hw2 = if_else(rowSums(!is.na(select(., all_of(hw_vars)))) > 0, 1, 0))


# Finally, we subset to only kids who have BOTH culture and H&W survey

d5 <- d4 %>% filter(has_hw2==1& has_culture2==1)


# This is out analysis data set (240 kids in the current run)

######################################################################
# Initiate the excel workbook
######################################################################

wb <- createWorkbook()

                         
######################################################################
# Basic demographics table
######################################################################

covars <- c("district",  "grade_actual", 
            "age_cont",  
            "gender.3cat.f", "race_native", "race_white", "race_black", "race_hispanic", 
            "race_other", "race_asian", "race_middleeastern", "race_hawaiian", 
            "ethnicity_tribalmember", "ethnicity_tribaldescendant", 
            "ethnicity_tribe_no",
            "any_tier1", "any_tier2", 
            "any_tier3","prop_tier1", "prop_tier2", 
            "prop_tier3", 
            "tier_majority","t_meaning_int" ,"any_meaning_int",
            "act_duration_days")


continuous_vars <- covars[grepl("t_act|t_meaning", covars)]
df1 <-
  d5 %>% ungroup() %>%
  select(all_of(covars)) %>%
  tbl_summary(by="district",
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ 2),
              type = list(continuous_vars ~ "continuous"),
              missing = "no")  %>% add_overall() %>% as_tibble()

df1
names(df1) <- gsub("\\*", "", names(df1))

df1 <- df1 %>% mutate(Characteristic = if_else(Characteristic=="Unknown",
                                               paste0("   ", Characteristic), 
                                               Characteristic))


addWorksheet(wb, "DescriptiveTable")
writeData(wb, sheet = "DescriptiveTable", x = df1)


######################################################################
# Mean(SD) for all outcomes, overall and by district
######################################################################

covars <- c(hw_vars, culture_vars)

key_vars<-c("hw_cultural_connect_scale", "hw_interpersonal_connect_scale", "hw_overall_wellbeing_scale", "hw_tribal_identity_scale",
            "overall_health", "culture_part_sum", "culture_fam_overall_scale", 
            "culture_fam_speak_scale", "culture_fam_trad_landscapes_scale", 
            "culture_fam_storytelling_scale", "culture_fam_songs_scale", 
            "culture_fam_trad_materials_scale", "culture_fam_seasonal_rounds_scale", 
            "culture_fam_dancing_scale", "culture_fam_trad_practices_scale", 
            "culture_fam_trad_games_scale", "culture_fam_trad_foods_scale",
            "culture_comf_scale", "culture_connect_scale", "culture_learn_scale", 
            "culture_imp_scale", "culture_speak_scale")


labels<- c("H&W: Cultural connectedness (1)",
           "H&W: Interpersonal connectedness (2)", "H&W: Overall wellbeing (3)", "H&W: Tribal identity (4)",
           "H&W: Overall health (5)",
           "Culture: Participation (6)",
           "Culture: Overall familiarity with cultural practices (7)",
           "Culture: Fam with Salish/Ksanka (8)", "Culture: Fam with Traditional landscapes (9)", "Culture: Fam with Storytelling (10)", 
           "Culture: Fam with Singing Songs (11)", "Culture: Fam with Traditional materials (12)", "Culture: Fam with Seasonal rounds (13)", 
           "Culture: Fam with Dancing (14)", "Culture: Fam with Traditional practices (15)", "Culture: Fam with Traditional games (16)", 
           "Culture: Fam with Traditional foods (17)",
           "Culture: Comfort in cultural practices (18)", "Culture: Connectedness (19)", 
           "Culture: Interest to learn cultural practices (20)", 
           "Culture: Importance of learning cultural practices (21)", 
           "Culture: Proficiency in Salish/Ksanka (22)")


continuous_vars <- key_vars[grepl("culture|hw_|t_act|t_meaning|overall", key_vars)]
df2 <-
  d5 %>% ungroup() %>%
  select(district, all_of(key_vars)) %>%
  tbl_summary(by="district",
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ 2),
              type = list(continuous_vars ~ "continuous"),
              missing = "no")  %>% add_overall() %>% as_tibble()

df2
names(df2) <- gsub("\\*", "", names(df2))
df2$label <- labels


df2<- df2 %>% mutate(Characteristic = if_else(Characteristic=="Unknown",
                                               paste0("   ", Characteristic), 
                                               Characteristic))


# add ranges

df2<-df2 %>% mutate(range= case_when(
  grepl("^culture_fam_", Characteristic)~'1=Not at all familiar; 5=Extremely familiar',
  grepl("^culture_part", Characteristic)~'0=never,5=everyday (sum of 10 items)',
  grepl("^culture_imp", Characteristic)~'1=not important,5=very important',
  grepl("^culture_comf", Characteristic)~'1=Not at all comfortable; 5=Extremely comfortable',
  grepl("^culture_learn_", Characteristic)~'1=Not interested; 5=Extremely interested',
  grepl("^important_learn", Characteristic)~'1=Not important; 5=Extremely important',
  grepl("^hw_tribal", Characteristic)~'1=strongly disagree;6=Strongly agree',
  grepl("^hw_cultural", Characteristic)~'1=strongly disagree;6=Strongly agree',
  grepl("^hw_interpersonal", Characteristic)~'1=strongly disagree;6=Strongly agree',
  grepl("^overall_health", Characteristic)~'1=poor;5=very good',
  grepl("^hw_overall", Characteristic)~'1=strongly disagree;6=Strongly agree',
  grepl("^culture_speak_",  Characteristic)~'1=Not at all; 5=Extremely well',
  grepl("culture_learn",  Characteristic)~'1=Not interested; 5=Extremely interested',
  grepl("culture_connect",  Characteristic)~'1=Not connected; 5=Very connecyed',
  grepl("^overall_well", Characteristic)~'1=strongly disagree;5=Strongly agree',
  
))


df3 <- df2 %>% select(label, range, everything()) %>% select(-Characteristic)

addWorksheet(wb, "OutcomeMeans")
writeData(wb, sheet = "OutcomeMeans", x = df3)


######################################################################
# Correlations of all outcomes
######################################################################

# Define a function to add asterisks based on p-values
add_asterisks <- function(cor.table, pval.table) {
  out.table <- matrix(NA, nrow = nrow(cor.table), ncol = ncol(cor.table))
  for (i in 1:nrow(cor.table)) {
    for (j in 1:ncol(cor.table)) {
      curr.estimate <- cor.table[i, j]
      current.p <- pval.table[i, j]
      current.asterisk <- ifelse(current.p  < 0.001, "***", ifelse(current.p  < 0.01, "**", ifelse(current.p  < 0.05, "*", "")))
      out.table[i, j] <-paste0(curr.estimate , current.asterisk) # paste togethe the estimate and asterisk
      
    }
  }
  rownames(out.table)<-rownames(cor.table)
  colnames(out.table)<-colnames(cor.table)
  # Clean up invalid entries
  out.table [out.table  %in% c("*", "**", "***", "NA")]<-""
  diag(out.table)<-"1"
  out.table <- as.data.frame(out.table)
  return(out.table)
}

c<- d5 %>% select(all_of(key_vars))

### Create table

cor.table <- cor(c, use="pairwise.complete.obs",method = "pearson") # get cor table
cor.table[upper.tri(cor.table)] <- NA # delete upper table values
cor.table2 <- cor.table %>%  as.data.frame() #convert to to data frame
cor.table3  <-  cor.table2 %>% mutate_all(function(x) ifelse(is.na(x),"",formatC(x,digits=2,format="f"))) # format values
cor.pval <-corr.test(c,use="pairwise.complete.obs",method = "pearson")$p # get p values
cor.table4 <- add_asterisks(cor.table3, cor.pval) # add asterisks to main table

#Add row names


rownames(cor.table4) <- labels
colnames(cor.table4)<- paste0("(", 1:22, ")")

#Add items

cor.table4$items <- c(4,7,4,9,1,10,82,
                      2,8,2,8,15,14,7,9,7,10,
                      10,1,10,13,4)

cor.table4 <- cor.table4 %>% add_rownames()
cor.table4 <- cor.table4 %>% select(rowname, items, everything())


addWorksheet(wb, "CorrelationTable")
writeData(wb, sheet = "CorrelationTable", x = cor.table4)




#################################################################################
# Save workbook
###################################################################################

# Set column widesh
for (sheet_name in names(wb)) {
  setColWidths(wb, sheet = sheet_name, cols = 1:50, widths = "auto") # replace 50 with correct number
}


setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Analyses/DescriptiveAnalyses")
saveWorkbook(wb, "DescriptiveAnalyses_Year3_191124.xlsx", overwrite = TRUE)
