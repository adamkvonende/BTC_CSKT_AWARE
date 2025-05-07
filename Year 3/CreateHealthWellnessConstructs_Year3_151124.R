
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
names(d2)
# Make scale items numeric
scale.items <-grep("^q", names(d2), value = TRUE)
d2 <- d2 %>% mutate_at(scale.items, function(x) as.numeric(x))


#Combine

d3 <- bind_rows(d1,d2)
# Sort
d3 <- d3 %>% arrange(aware_id, time)

# Subset to ONLY those with aware Ids

d3 <- d3 %>% filter(!is.na(aware_id))
names(d3)

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
d3$overall_health <- d3$q7

######################################################################
# Save the derived data set
######################################################################


d4<-d3 %>% select(aware_id, time,overall_health,contains("scale"))

setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d4, "CSKT_HealthWellnessConstructs_Year3_191124.xlsx",overwrite=TRUE)

