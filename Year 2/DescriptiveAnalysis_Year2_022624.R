
###############################################################################
# 
# Note: These analyses combine information from the Year 2 Culture Survey and 
# the Health/Wellness Survey.
#
# Please see the ReadMe file at: 
# https://github.com/adamkvonende/BTC_CSKT_AWARE
# for information on how the data sets used in these analyses were constructed.
#
# Construct files were created using the following scripts:
# - CreateCultureSurveyConstructs_Year2_022624.R
# - CreateHealthWellnessConstructs_Year2_022624.R
#
# You will also need to read in the 2 data dictionaries:
#
# CSKT_CultureSurveyConstructs_DataDictionary_022624.xlsx")
# CSKT_HealthWellnessConstructs_DataDictionary_022624.xlsx")
#
# Note: The two surveys have different numbers of participants. 
# For the purposes of Year 2 reporting, we use the Culture Survey as the 'base', 
# as it includes more individuals. In addition, if participants had more than 
# one assessment, only the first assessment was used.
#
# This file also adds a section comparing demographic variables across the 
# two data sets; however, this comparison was not included in the Year 2 reporting.
#
# Other outputs for Year 2 reporting include:
# 1) Basic demographic table by adult/youth
# 2) Item-level frequencies by adult/youth
# 3) Scale scores from both surveys by adult/youth
# 4) Correlation tables for scale scores by adult/youth
#
# All outputs are combined into a single Excel file.
###############################################################################


# Clear all objects in memory

rm(list = ls())

###############################################################################
# Load packages
######################################################################


if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor,psych, gtsummary,
       summarytools,descr, readxl,openxlsx)



###############################################################################
# Load data and merge data
######################################################################

# Navigate to where data sets are stored
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")

### Culture survey

a <- rio::import("CSKT_CultureSurveyConstructs_Year2_022624.xlsx")
names(a)
a %>% tabyl(name_index)
# Keep only first assessment (for now)
a2<- a %>% filter(name_index==1)

### Health and wellness

b <- rio::import("CSKT_HealthWellnessConstructs_Year2_022624.xlsx")
# Keep only first assessment (for now)
b2<- b %>% filter(name_index==1)
#create 'has health data' indicator
b2 <- b2 %>% mutate(has_health_data=1)


### Merge
# Note: when merging, if variable is duplicate, '.x' is appended to the left most data set,
# and '.y' the the right most data set

m <- a2 %>% left_join(b2,by="aware_id")
#create ' has health data indicator'
m %>% tabyl(has_health_data)


######################################################################
# Compare demographics between data sets 
######################################################################

# Note: we will create a new variable (e.g. age2) that combined information
# from variables in the culture survey (e.g. age2.x) and the health survey (e.g. age2.y)
# If both values agree, the new value will be equal to the agreed value;
# if one is missing but the other not, the value equal to the non missing value;
# if the values are both missing, then will be missing;
# if the values do not agree, then will be missing
#
# we will also create flags for each variable, with values:
# 'consistent, culture missing, health missing, both missing, inconsistent'


# Define vars to check
vars<- c("age2", "age_cont", "age.cat4",  "adult", "gender.3cat", "school", "role_parent", "role_grandparent", "role_student", 
        "role_teacher", "role_schooladmin", "role_para", "role_adultsuppyouth", 
        "role_knowledgekeeper", "role_elder", "role_commember", 
        "role_mhprovider", "role_apprentice", "role_other")


for (i in 1:length(vars)) { 
  # Select the .x and .y versions
  tmp <- m %>% dplyr::select(!!paste0(vars[i],".x"), !!paste0(vars[i],".y"))
  names(tmp)<-c("v1", "v2") # rename
  tmp<-tmp %>% mutate(v3 = case_when(v1==v2~v1, # both same value
                                is.na(v1) & !is.na(v2)~v2, # missing in x, non missing in y
                                !is.na(v1) & is.na(v2)~v1, #non missing in x, missing in y
                                is.na(v1) & is.na(v2)~NA_real_, # missing in both
                                v1!=v2~NA_real_ ),
                      v4 = case_when(v1==v2~"consistent", # both same value
                                     is.na(v1) & !is.na(v2)~"culture missing",
                                     !is.na(v1) & is.na(v2)~"health missing",
                                     is.na(v1) & is.na(v2)~"both missing",
                                     v1!=v2~"inconsistent" ))
  
  names(tmp )<- c("v1", "v2", vars[[i]], paste0(vars[[i]], "_check"))
  # add back into main data
  tmp2 <- tmp %>% dplyr::select(3:4)
  m <-cbind(m, tmp2) #cbind means 'column bind' (we could also use left_join)
    
}


# Quick table to check it worked

m %>%
dplyr::select(contains("check")) %>%
tbl_summary(
          statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
          digits = list(all_continuous() ~ 1),
          missing = "no") 

######################################################################
# Initiate the excel workbook for saving outputs. We will add to it
# and save it to disk at the end
######################################################################

wb <- createWorkbook()

######################################################################
# Output 1: Basic demographics table
######################################################################

### Create factor variables (this helps R understand they are categorical and to treat them as such)

# Age
m$age.cat4.f<-factor(m$age.cat4, labels = c("10-20","20-30", "30-40", "40+"))
levels(m$age.cat4.f)

# Gender

m$gender.3cat.f <-factor(m$gender.3cat, labels=c("Male", "Female", "Other"))
levels(m$gender.3cat.f)


# School

m$school.f <- factor(m$school, labels =c("Mission Middle","Mission High","Polson Middle","Polson High","Ronan Middle",
                                        "Ronan High","Arlee Middle","Arlee High","Two Eagle River School", "Other") )



# Create table (will output as data frame or 'tibble')


tbl1 <-
  m %>%
  select(age_cont, age.cat4.f, adult,gender.3cat.f,school.f, 
         role_parent,
         role_grandparent,role_student,role_teacher,role_schooladmin,
         role_para,role_adultsuppyouth,role_knowledgekeeper,
         role_elder,role_commember,role_mhprovider,role_apprentice,
         role_other,has_health_data) %>%
  tbl_summary(by=adult,
              statistic = list(all_continuous() ~ "{mean} ({sd})", 
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ 1),
              missing = "no")  %>% as_tibble()

tbl1

# Add sample sizes in first row

tbl1 <-tbl1 %>% add_row(.before=1) # add empty row
tbl1[1,]<-as.list(colnames(tbl1)) # assign column names to first row
names(tbl1)<-c("char", "youth", "adult")

# Set names
tbl1$char2 <- c("", "Age", "Age category", "10-20", "20-30", "30-40", "40+","Gender", "Male", "Female", "Other", "School", "Mission Middle", 
                "Mission High", "Polson Middle", "Polson High", "Ronan Middle", 
                "Ronan High", "Arlee Middle", "Arlee High", "Two Eagle River School","Other",
                  "Parent", "Grandparent", 
                "Student", "Teacher", "School admin", "Para", 
                "Adultsuppyouth", "knowledge keeper", "Elder", 
                "Community member", "MH provider", "Apprentice", "Other", "Completed health survey"
)
#Add header rows
tbl1 <-tbl1 %>% add_row(char2="Role",.before=23)
#Save only key columns
tbl1 <-tbl1 %>% select(char2,youth, adult)



addWorksheet(wb, "Descriptive Table")
writeData(wb, sheet = "Descriptive Table", x = tbl1)


######################################################################
# Output 2: Frequency table for individual items
######################################################################


# Define sets

scale.items <-grep("^q", names(m), value = TRUE)
part.vars <- c("q6_part_lang", "q6_part_land", "q6_part_story", "q6_part_song", "q6_part_mat", "q6_part_round", "q6_part_dance", "q6_part_pract", "q6_part_game", "q6_part_food")
camp_new.vars<- c("q15_camp_lang", "q15_camp_land", "q15_camp_story", "q15_camp_song", "q15_camp_mat", "q15_camp_round", "q15_camp_dance", "q15_camp_pract", "q15_camp_game", "q15_camp_food")
fam_speak.vars <- c("q5a1_fam_lang_salish", "q5a2_fam_lang_kootenai")



### Some modifications

# Make all scale items numeric

m<- m %>%mutate_at(scale.items, function(x) as.numeric(x))


# Add 1 to items that start with zero

m<- m %>% mutate_at(camp_new.vars, function(x) x+1)

# Some entries are between values (e.g. 1.5); use upper value (but not participation vars)

non_part_vars <- scale.items[!grepl("part", scale.items)]
m<- m %>%mutate_at(non_part_vars , function(x) ceiling(x))

### Create a function to get frequencies by item


FreqByItem <- function(data, items) {
  df.out <-data.frame()
  for (v in items) {
    # get output frequencies
    f <- summarytools::freq(data[,v])
    f <- as.data.frame(f) %>% rownames_to_column() #convert to data frame and add row names
    f <- f %>% dplyr::select(1:3) # first 3 columns
    f <- f %>% filter(rowname %in% c("1","2","3","4","5","6"))
    if (nrow(f)==0) { # if returns no frequencies then create just an empty data frame
        df <- data.frame(item=v)
    } else {
      df <- data.frame(v1=NA_real_,v2=NA_real_,v3=NA_real_,v4=NA_real_,v5=NA_real_,v6=NA_real_)
      names(f)<-c("value", "freq", "perc")
      f$value <-as.numeric(f$value)
      f$comb <- paste0(f$freq," (", formatC(f$perc,1,format="f"), "%", " )")    
      #create dummy data set
      df <- data.frame(v1=NA_real_,v2=NA_real_,v3=NA_real_,v4=NA_real_,v5=NA_real_,v6=NA_real_)
      #populate dummy data set with values
      for (i in 1:6 ) {
        df[,i] <- f$comb[f$value==i] # place the value at which f$comb = i in the ith column
      }
      
      # get overall mean
      item.mean <- formatC(summarytools::descr(data[,v])[1],5, format="f")
      # create final data set
      df <- df %>% transmute(item=v,  v1,v2,v3,v4,v5,v6,
                             nonmissing=sum(f$freq), 
                             mean = item.mean)
    }
    
    # Append local data frame to data set
    df.out<- bind_rows(df.out, df)

  }
  
  
  return(df.out)
}


### Now generate tables for adults and youth

# Load data dictionary for labels


dd1 <-rio::import("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets/CSKT_CultureSurveyConstructs_DataDictionary_022624.xlsx")
dd2 <-rio::import("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets/CSKT_HealthWellnessConstructs_DataDictionary_022624.xlsx")
dd <-rbind(dd1,dd2)
dd <- dd %>% group_by(variable) %>% slice(1) # keep only unique items
dd <- dd %>% select(item=variable, descr=Description, coding=Response)


# Youth


df.youth <-m %>% filter(adult==0) %>% select(scale.items) # filter data to youth
out.youth <- FreqByItem(data=df.youth, items=scale.items) # run function
out.youth2 <-out.youth %>% left_join(dd, by="item") # merge labels
out.youth3 <-out.youth2 %>% select(item, descr, everything()) # reorder vars


addWorksheet(wb, "By item (Youth)")
writeData(wb, sheet = "By item (Youth)", x = out.youth3 )

# Adult

df.adult <-m %>% filter(adult==1) %>% select(scale.items) # filter data to adults
out.adult <- FreqByItem(data=df.adult, items=scale.items) # run function
out.adult2 <-out.adult %>% left_join(dd, by="item") # merge labels
out.adult3 <-out.adult2 %>% select(item, descr, everything()) # reorder vars


addWorksheet(wb, "By item (Adults)")
writeData(wb, sheet = "By item (Adults)", x = out.adult3 )


################################################################################
### Output 3: Scale table by adult
################################################################################

## adjust low mood scale

m <-m %>% mutate(low_mood_scale=low_mood_scale-2)

tbl2 <-
  m %>%
  select(adult,
         contains("scale")) %>%
  tbl_summary(by=adult,
              type = c(connect_scale,fam_speak_scale,	
                       fam_storytelling_scale,low_mood_scale) ~ "continuous" ,
              statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2,
              missing = "no")  %>% add_p(test = all_continuous() ~ "t.test") %>%  add_overall() %>%

  as_tibble()

# Add sample sizes in first row

tbl2<-tbl2 %>% add_row(.before=1) # add empty row
tbl2[1,]<-as.list(colnames(tbl2)) # assign column names to first row
names(tbl2)<-c("char", "overall", "youth", "adult", "pval")


## Add ranges


tbl2<-tbl2 %>% mutate(range= case_when(
  grepl("^fam_", char)~'1=Not at all familiar; 5=Extremely familiar',
  grepl("^part_", char)~'Rescaled to times per week',
  grepl("^comf_", char)~'1=Not at all comfortable; 5=Extremely comfortable',
  grepl("^connect_", char)~'1=Not at all connected; 5=Extremely connected',
  grepl("^learn_", char)~'1=Not interested; 5=Extremely interested',
  grepl("^imp_", char)~'1=Not important; 5=Extremely important',
  grepl("^speak_", char)~'1=Not at all; 5=Extremely well',
  grepl("^camp_new_", char)~'0=Did not participate; 5=A great deal',
  grepl("^camp_perc", char)~'1=strongly disagree;6=Strongly agree',
  grepl("^tribal", char)~'1=strongly disagree;6=Strongly agree',
  grepl("^native", char)~'1=strongly disagree;5=Strongly agree',
  grepl("^overall", char)~'1=strongly disagree;5=Strongly agree',
  grepl("^social", char)~'1=strongly disagree;5=Strongly agree',
  grepl("^low", char)~'0-2'

))
#labels
tbl2$scale2 <- c("n", "Overall familiarity", "Salish/Ksanka", "Traditional landscapes", "Storytelling", 
                 "Singing Songs", "Traditional materials", "Seasonal rounds", 
                 "Dancing", "Traditional practices", "Traditional games", 
                 "Traditional foods", "Participation in cultural practices", 
                 "Comfort in cultural practices", "Connectedness", 
                 "Interest to learn cultural practices", "Importance of learning cultural practices", 
                 "Proficiency in Salish/Ksanka", "Learning at encampment", 
                 "Perceptions of encampment","Tribal connectedness",
                 "Native identity", "Overall wellbeing", "Social support",
                 "Low mood")

names(tbl2)
tbl2b <- tbl2 %>% select(scale2,char, scale2, overall, youth, adult, pval, range)
tbl2b<-tbl2b %>% add_row(.before=21, scale2 = "Health and Wellness:")

## Add to workbook

addWorksheet(wb, "Scales By Adult and Youth")
writeData(wb, sheet = "Scales By Adult and Youth", x = tbl2b )

######################################################################
# Output 4: Correlation table
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

#Select variables

c<-m %>% select(adult,contains("scale"))

### Youth correlation table

c1 <- c %>% filter(adult==0) %>% select(-adult)
cor.table <- cor(c1, use="pairwise.complete.obs",method = "pearson") # get cor table
cor.table[upper.tri(cor.table)] <- NA # delete upper table values
cor.table2 <- cor.table %>%  as.data.frame() #convert to to data frame
cor.table3  <-  cor.table2 %>% mutate_all(function(x) ifelse(is.na(x),"",formatC(x,digits=2,format="f"))) # format values
cor.pval <-corr.test(c1,use="pairwise.complete.obs",method = "pearson")$p # get p values
cor.table4 <- add_asterisks(cor.table3, cor.pval) # add asterisks to main table

#Add row names

rownames(cor.table4) <- c("Overall familiarity (1)","Salish/Ksanka (2)", "Traditional landscapes (3)", "Storytelling (4)", 
                    "Singing Songs (5)", "Traditional materials (6)", "Seasonal rounds (7)", 
                    "Dancing (8)", "Traditional practices (9)", "Traditional games (10)", 
                    "Traditional foods (11)", "Participation in cultural practices (12)", 
                    "Comfort in cultural practices (13)", "Connectedness (14)", 
                    "Interest to learn cultural practices (15)", 
                    "Importance of learning cultural practices (16)", 
                    "Proficiency in Salish/Ksanka (17)", "Learning at encampment (18)", 
                    "Perceptions of encampment (19)","Tribal connectedness (20)",
                    "Native identity (21)", "Overall wellbeing (22)", "Social support (23)",
                    "Low mood (24)")


               
               
colnames(cor.table4)<- paste0("(", 1:24, ")")
cor.table4 <-cor.table4 %>% rownames_to_column()

## Add to workbook

addWorksheet(wb, "Correlation Table (Youth)")
writeData(wb, sheet = "Correlation Table (Youth)", x = cor.table4 )



### Adult correlation table

c1 <- c %>% filter(adult==1) %>% select(-adult)
cor.table <- cor(c1, use="pairwise.complete.obs",method = "pearson") # get cor table
cor.table[upper.tri(cor.table)] <- NA # delete upper table values
cor.table2 <- cor.table %>%  as.data.frame() #convert to to data frame
cor.table3  <-  cor.table2 %>% mutate_all(function(x) ifelse(is.na(x),"",formatC(x,digits=2,format="f"))) # format values
cor.pval <-corr.test(c1,use="pairwise.complete.obs",method = "pearson")$p # get p values
cor.table4 <- add_asterisks(cor.table3, cor.pval) # add asterisks to main table

#Add row names

rownames(cor.table4) <- c("Overall familiarity (1)","Salish/Ksanka (2)", "Traditional landscapes (3)", "Storytelling (4)", 
                          "Singing Songs (5)", "Traditional materials (6)", "Seasonal rounds (7)", 
                          "Dancing (8)", "Traditional practices (9)", "Traditional games (10)", 
                          "Traditional foods (11)", "Participation in cultural practices (12)", 
                          "Comfort in cultural practices (13)", "Connectedness (14)", 
                          "Interest to learn cultural practices (15)", 
                          "Importance of learning cultural practices (16)", 
                          "Proficiency in Salish/Ksanka (17)", "Learning at encampment (18)", 
                          "Perceptions of encampment (19)","Tribal connectedness (20)",
                          "Native identity (21)", "Overall wellbeing (22)", "Social support (23)",
                          "Low mood (24)")




colnames(cor.table4)<- paste0("(", 1:24, ")")
cor.table4 <-cor.table4 %>% rownames_to_column()
## Add to workbook

addWorksheet(wb, "Correlation Table (Adult)")
writeData(wb, sheet = "Correlation Table (Adult)", x = cor.table4 )



#################################################################################
# Save excel  workbook to disk
#################################################################################

# Set column widesh
for (sheet_name in names(wb)) {
  setColWidths(wb, sheet = sheet_name, cols = 1:50, widths = "auto") # replace 50 with correct number
}


setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Analyses/DescriptiveAnalyses")
saveWorkbook(wb, "DescriptiveTablesYear2_022624.xlsx", overwrite = TRUE)


