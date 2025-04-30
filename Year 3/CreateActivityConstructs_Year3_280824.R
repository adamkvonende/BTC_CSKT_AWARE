rm(list = ls())

# Load required packages
if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse, janitor, lubridate, gtsummary)

#===============================================================================
# Import and Prepare Data
#===============================================================================

# Set working directory
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ActivityData")

# Import data from each district (note: variable names differ slightly between districts)
# Mission district data
a <- rio::import("CSKT AWARE Y3 Mission Activity Dataset_CLEAN 8.8.24.xlsx")
# Rename variables to match other datasets
names(a)[45:53] <- c("meaning_int_1", "meaning_int_2", "meaning_int_3", "meaning_int_4", 
                     "meaning_int_5", "meaning_int_6", "meaning_int_7", "meaning_int_8", "meaning_int_9")
names(a)[54:57] <- c("behave_conc_1", "behave_conc_2", "behave_conc_3", "behave_conc_4")
a$district <- "Mission"

# Ronan district data
b <- rio::import("CSKT AWARE Y3 Ronan Activity Dataset_CLEAN 8.8.24.xlsx")
# Rename variables to match
names(b)[4] <- "native"
b$district <- "Ronan"

# Polson district data
c <- rio::import("CSKT AWARE Y3 Polson Activity Dataset_CLEAN 8.8.24.xlsx")
names(c)[21] <- "leader7"
names(c)[47:58] <- gsub("(int|conc)([0-9]+)", "\\1_\\2", names(c)[47:58])
c$district <- "Polson"

# Combine all datasets
d <- bind_rows(a, b, c)

# Reorder variables for better organization
d <- d[, c("district", "student_name", "aware_id", "gender", "native", "grade", "dateact", "schoolname", 
           "grade6_served", "grade7_served", "grade8_served", "grade9_served", "grade10_served", "grade11_served", "grade12_served", 
           "not_aware", "tier1", "tier2", "tier3", "activity_type", 
           "leader1", "leader2", "leader3", "leader4", "leader5", "leader6", "leader7", 
           "activity", "activity_descr", "act_cat_lang", "act_cat_land", "act_cat_story", "act_cat_sing", 
           "act_cat_create", "act_cat_season", "act_cat_dance", "act_cat_trad", "act_cat_game", "act_cat_food", "act_cat_hist", "act_cat_aware", 
           "act_cat_nas", "act_cat_na", "act_theme_cang", "act_theme_tc", "act_theme_sp", "act_theme_law", "act_theme_pi", "act_theme_haw", 
           "act_theme_wai", "act_theme_cam", 
           "coleader1", "coleader2", "coleader3", "coleader4", "coleader5", 
           "meaning_int_1", "meaning_int_2", "meaning_int_3", "meaning_int_4", "meaning_int_5", "meaning_int_6", "meaning_int_7", "meaning_int_8", "meaning_int_9", 
           "behave_conc_1", "behave_conc_2", "behave_conc_3", "behave_conc_4", "behave_conc_5", "behave_conc_6", 
           "int_descr", "act_index", "schoolstaff", "activity_type_spec", 
           "interact_yes", "behave_yes", "interact_bc_other")]

# Filter to only include records with valid AWARE IDs
d <- d %>% filter(!is.na(aware_id))

# Fill in missing school names by child
d <- d %>% group_by(aware_id) %>% tidyr::fill(schoolname, .direction = "updown")

#===============================================================================
# Calculate Activity Metrics
#===============================================================================

# Count total activities per student and create index
d <- d %>% 
  group_by(aware_id) %>% 
  arrange(aware_id, dateact) %>% 
  mutate(
    tag_aware_id = if_else(row_number() == 1, 1, 0),  # Tag first row for each person
    act_index = row_number(),                         # Index for activities (1:n)
    t_act = max(act_index)                            # Total activities per person
  )

#===============================================================================
# Analyze Activity Types
#===============================================================================

# Activity type codes:
# 1 = Teacher
# 2 = Knowledge Keeper
# 3 = Paraprofessional
# 4 = Apprentice
# 5 = Co-led
# 6 = Meaningful interaction
# 7 = Other

# Create variables for teacher-led activities
d <- d %>% 
  group_by(aware_id) %>% 
  mutate(
    t_act_type_tch = sum(activity_type[activity_type == 1], na.rm = TRUE),
    prop_act_type_tch = t_act_type_tch / t_act
  )

# Create variables for all activity types (total count and proportion)
act_type_vars <- c(
  "act_type_tch", "act_type_kk", "act_type_para", "act_type_app", 
  "act_type_coled", "act_type_other", "act_type_meaningint"
)

for (i in 1:7) {
  tmp <- d %>%
    group_by(aware_id) %>%
    mutate(
      t_act_type = sum(activity_type == i, na.rm = TRUE),
      prop_act_type = t_act_type / t_act
    )
  
  d[, paste0("t_", act_type_vars[i])] <- tmp$t_act_type
  d[, paste0("prop_", act_type_vars[i])] <- tmp$prop_act_type
}

# Summary table for activity types
d %>% 
  filter(tag_aware_id == 1) %>% 
  ungroup() %>% 
  dplyr::select(contains("act_type")) %>%
  tbl_summary(
    type = everything() ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ 1),
    missing = "no"
  ) 

#===============================================================================
# Analyze Activity Categories
#===============================================================================

# Create variables for activity categories (total count and proportion)
act_vars <- c(
  "act_cat_land", "act_cat_story", "act_cat_sing", "act_cat_create", "act_cat_season", 
  "act_cat_dance", "act_cat_trad", "act_cat_game", "act_cat_food", 
  "act_cat_hist", "act_cat_aware", "act_cat_nas", "act_cat_na", 
  "act_theme_cang", "act_theme_tc", "act_theme_sp", "act_theme_law", 
  "act_theme_pi", "act_theme_haw", "act_theme_wai", "act_theme_cam"
)

for (v in act_vars) {
  tmp <- d %>% 
    transmute(aware_id, act_cat = get(v), t_act) %>% 
    group_by(aware_id) %>%
    mutate(
      t_act_cat = sum(act_cat, na.rm = TRUE),
      prop_act_cat = t_act_cat / t_act
    )
  
  d[, paste0("t_", v)] <- tmp$t_act_cat
  d[, paste0("prop_", v)] <- tmp$prop_act_cat
}

# Summary table for activity categories
d %>% 
  filter(tag_aware_id == 1) %>% 
  ungroup() %>% 
  dplyr::select(contains("t_act_cat")) %>%
  tbl_summary(
    type = everything() ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_continuous() ~ 1),
    missing = "no"
  ) 

#===============================================================================
# Calculate Activity Date Information
#===============================================================================

# Convert date strings to Date objects and calculate program duration
d <- d %>% mutate(dateact2 = mdy(dateact))  # Convert to date
d <- d %>% 
  group_by(aware_id) %>%
  mutate(
    dateact_start = min(dateact2, na.rm = TRUE),
    dateact_end = max(dateact2, na.rm = TRUE),
    act_duration_days = as.numeric(dateact_end - dateact_start)
  )

#===============================================================================
# Handle Grade Information
#===============================================================================

# Display grade distribution
d %>% filter(tag_aware_id == 1) %>% tabyl(grade)

# Convert to actual grade (grade + 5)
d <- d %>% mutate(grade_actual = grade + 5)
d %>% filter(tag_aware_id == 1) %>% tabyl(grade_actual)

# For missing grades, infer based on most frequent grade served
d <- d %>% 
  group_by(aware_id) %>% 
  mutate(
    t_grade6 = sum(grade6_served, na.rm = TRUE),
    t_grade7 = sum(grade7_served, na.rm = TRUE),
    t_grade8 = sum(grade8_served, na.rm = TRUE),
    t_grade9 = sum(grade9_served, na.rm = TRUE),
    t_grade10 = sum(grade10_served, na.rm = TRUE),
    t_grade11 = sum(grade11_served, na.rm = TRUE),
    t_grade12 = sum(grade12_served, na.rm = TRUE),
    max_grade_value = pmax(t_grade6, t_grade7, t_grade8, t_grade9, t_grade10, t_grade11, t_grade12)
  )

# Assign imputed grade based on most frequent grade served
d <- d %>% 
  mutate(
    max_grade_imputed = case_when(
      max_grade_value == t_grade6 ~ 6,
      max_grade_value == t_grade7 ~ 7,
      max_grade_value == t_grade8 ~ 8,
      max_grade_value == t_grade9 ~ 9,
      max_grade_value == t_grade10 ~ 10,
      max_grade_value == t_grade11 ~ 11,
      max_grade_value == t_grade12 ~ 12
    )
  )

# Replace missing grade_actual with imputed grade
d <- d %>% mutate(grade_actual = if_else(is.na(grade_actual), max_grade_imputed, grade_actual))
d %>% filter(tag_aware_id == 1) %>% tabyl(grade_actual)

# Create school name based on district and grade level
d <- d %>% 
  mutate(
    school = case_when(
      district == "Mission" & grade_actual <= 8 ~ "Mission middle",
      district == "Mission" & grade_actual >= 9 ~ "Mission high",
      district == "Ronan" & grade_actual <= 8 ~ "Ronan middle",
      district == "Ronan" & grade_actual >= 9 ~ "Ronan high",
      district == "Polson" & grade_actual <= 8 ~ "Polson middle",
      district == "Polson" & grade_actual >= 9 ~ "Polson high"
    )
  )

#===============================================================================
# Analyze Meaningful Interactions
#===============================================================================

# Identify any meaningful interaction recorded
d <- d %>% 
  ungroup() %>% 
  mutate(meaning_int = if_else(rowSums(!is.na(select(., meaning_int_1:meaning_int_9))) > 0, 1, 0))

# Calculate metrics for meaningful interactions
d <- d %>% 
  group_by(aware_id) %>% 
  mutate(
    tag_aware_id = if_else(row_number() == 1, 1, 0),
    any_meaning_int = max(meaning_int, na.rm = TRUE),
    t_meaning_int = sum(meaning_int, na.rm = TRUE)
  )

# Map meaningful interactions to categories
# (Categorization done by Angelee)
map <- rio::import("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Analyses/SchoolActivity/Meaningful Interaction Categories.xlsx")
map2 <- map %>% filter(!is.na(Category))  # Exclude categories with no mapping

# Map raw inputs to categories
d2 <- d
for (i in 1:9) {
  tmp <- d %>% select(aware_id, act_index, paste0("meaning_int_", i))
  names(tmp) <- c("aware_id", "act_index", "meaning_int")
  
  tmp <- tmp %>% 
    left_join(map2, by = "meaning_int") %>% 
    dplyr::select(aware_id, act_index, Category)
  
  names(tmp) <- c("aware_id", "act_index", paste0("meaning_cat_", i))
  d2 <- d2 %>% left_join(tmp, by = c("aware_id", "act_index"))
}

# Tag interactions by category type
d2 <- d2 %>% 
  ungroup() %>%
  mutate(
    tag_meaning_cat_academic = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Academic", na.rm = TRUE) > 1, 1, 0),
    tag_meaning_cat_cultural = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Cultural", na.rm = TRUE) > 1, 1, 0),
    tag_meaning_cat_health = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Health and Wellness", na.rm = TRUE) > 1, 1, 0),
    tag_meaning_cat_behave = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Behavior concern", na.rm = TRUE) > 1, 1, 0),
    tag_meaning_cat_home = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Home and Family", na.rm = TRUE) > 1, 1, 0),
    tag_meaning_cat_interpersonal = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Interpersonal Relationships", na.rm = TRUE) > 1, 1, 0),
    tag_meaning_cat_family = if_else(rowSums(select(., meaning_cat_1:meaning_cat_9) == "Home and Family/Interpersonal Relationships", na.rm = TRUE) > 1, 1, 0)
  )

# Aggregate tags across all activities for each student
d2 <- d2 %>%
  group_by(aware_id) %>%
  mutate(
    # Sum of tagged categories
    t_meaning_cat_academic = sum(tag_meaning_cat_academic, na.rm = TRUE),
    t_meaning_cat_cultural = sum(tag_meaning_cat_cultural, na.rm = TRUE),
    t_meaning_cat_health = sum(tag_meaning_cat_health, na.rm = TRUE),
    t_meaning_cat_behave = sum(tag_meaning_cat_behave, na.rm = TRUE),
    t_meaning_cat_home = sum(tag_meaning_cat_home, na.rm = TRUE),
    t_meaning_cat_interpersonal = sum(tag_meaning_cat_interpersonal, na.rm = TRUE),
    t_meaning_cat_family = sum(tag_meaning_cat_family, na.rm = TRUE),
    
    # Flag for any occurrence of category
    any_meaning_cat_academic = if_else(t_meaning_cat_academic > 0, 1, 0),
    any_meaning_cat_cultural = if_else(t_meaning_cat_cultural > 0, 1, 0),
    any_meaning_cat_health = if_else(t_meaning_cat_health > 0, 1, 0),
    any_meaning_cat_behave = if_else(t_meaning_cat_behave > 0, 1, 0),
    any_meaning_cat_home = if_else(t_meaning_cat_home > 0, 1, 0),
    any_meaning_cat_interpersonal = if_else(t_meaning_cat_interpersonal > 0, 1, 0),
    any_meaning_cat_family = if_else(t_meaning_cat_family > 0, 1, 0)
  ) %>%
  ungroup()

# Summary table for meaningful interaction categories
d2 %>% 
  filter(tag_aware_id == 1) %>% 
  ungroup() %>% 
  dplyr::select(contains("any_meaning_cat")) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ 1),
    missing = "no"
  ) 

#===============================================================================
# Analyze Behavioral Concerns
#===============================================================================

# Identify any behavioral concern recorded
d2 <- d2 %>% 
  ungroup() %>% 
  mutate(behave_conc = if_else(rowSums(!is.na(select(., behave_conc_1:behave_conc_6))) > 0, 1, 0))

# Calculate metrics for behavioral concerns
d2 <- d2 %>% 
  group_by(aware_id) %>% 
  mutate(
    any_behave_conc = max(behave_conc, na.rm = TRUE),
    t_behave_conc = sum(behave_conc, na.rm = TRUE)
  )

#===============================================================================
# Analyze Tier Support
#===============================================================================

# Calculate total and any instance of each tier support
d2 <- d2 %>% 
  group_by(aware_id) %>% 
  mutate(
    t_tier1 = sum(tier1, na.rm = TRUE),
    t_tier2 = sum(tier2, na.rm = TRUE),
    t_tier3 = sum(tier3, na.rm = TRUE),
    any_tier1 = ifelse(t_tier1 > 0, 1, 0),
    any_tier2 = ifelse(t_tier2 > 0, 1, 0),
    any_tier3 = ifelse(t_tier3 > 0, 1, 0)
  )

# Calculate proportion of total activities for each tier
d2 <- d2 %>% 
  mutate(
    prop_tier1 = t_tier1 / t_act,
    prop_tier2 = t_tier2 / t_act,
    prop_tier3 = t_tier3 / t_act
  )

# Determine which tier makes up the majority of activities for each student
d2 <- d2 %>%
  mutate(
    tier_majority = case_when(
      t_tier1 > t_tier2 & t_tier1 > t_tier3 ~ "tier1",
      t_tier2 > t_tier1 & t_tier2 > t_tier3 ~ "tier2",
      t_tier3 > t_tier1 & t_tier3 > t_tier2 ~ "tier3",
      t_tier1 == t_tier2 & t_tier1 > t_tier3 ~ "tier2",
      t_tier1 == t_tier3 & t_tier1 > t_tier2 ~ "tier3",
      t_tier2 == t_tier3 & t_tier2 > t_tier1 ~ "tier3",
      t_tier1 == t_tier2 & t_tier1 == t_tier3 ~ "tier3",
      TRUE ~ NA_character_
    )
  )

# Summary table for tier support
d2 %>% 
  filter(tag_aware_id == 1) %>% 
  ungroup() %>% 
  dplyr::select(contains("any_tier"), tier_majority) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ 1),
    missing = "no"
  ) 

#===============================================================================
# Save Results
#===============================================================================

# Define variables to keep in final dataset
vars_to_keep <- c(
  "district", "aware_id", "gender", "native", "grade", "grade_actual", "school", 
  "t_act", "dateact_start", "dateact_end", "act_duration_days",
  "not_aware", "t_tier1", "t_tier2", "t_tier3", "any_tier1", "any_tier2", "any_tier3", 
  "tier_majority", "prop_tier1", "prop_tier2", "prop_tier3",
  "tag_aware_id", 
  "any_meaning_int", "t_meaning_int", "any_behave_conc", "t_behave_conc",
  "t_act_type_tch", "t_act_type_kk", "t_act_type_para", "t_act_type_app", 
  "t_act_type_coled", "t_act_type_other", "t_act_type_meaningint",
  "prop_act_type_tch", "prop_act_type_kk", "prop_act_type_para", 
  "prop_act_type_app", "prop_act_type_coled", "prop_act_type_other", "prop_act_type_meaningint",
  "t_act_cat_land", "t_act_cat_story", "t_act_cat_sing", "t_act_cat_create", 
  "t_act_cat_season", "t_act_cat_dance", "t_act_cat_trad", "t_act_cat_game", 
  "t_act_cat_food", "t_act_cat_hist", "t_act_cat_aware", "t_act_cat_nas", 
  "t_act_cat_na", "t_act_theme_cang", "t_act_theme_tc", "t_act_theme_sp", "t_act_theme_law", 
  "t_act_theme_pi", "t_act_theme_haw", "t_act_theme_wai", "t_act_theme_cam",
  "t_meaning_cat_academic", "t_meaning_cat_cultural", "t_meaning_cat_health", "t_meaning_cat_behave", 
  "t_meaning_cat_home", "t_meaning_cat_interpersonal", "t_meaning_cat_family", 
  "any_meaning_cat_academic", "any_meaning_cat_cultural", "any_meaning_cat_health", 
  "any_meaning_cat_behave", "any_meaning_cat_home", "any_meaning_cat_interpersonal", 
  "any_meaning_cat_family", "prop_act_cat_land", "prop_act_cat_story", "prop_act_cat_sing", 
  "prop_act_cat_create", "prop_act_cat_season", "prop_act_cat_dance", 
  "prop_act_cat_trad", "prop_act_cat_game", "prop_act_cat_food", 
  "prop_act_cat_hist", "prop_act_cat_aware", "prop_act_cat_nas", "prop_act_cat_na",
  "prop_act_theme_cang", "prop_act_theme_tc", "prop_act_theme_sp", 
  "prop_act_theme_law", "prop_act_theme_pi", "prop_act_theme_haw", 
  "prop_act_theme_wai", "prop_act_theme_cam"
)

# Save the long version with all rows (for counting leaders, etc.)
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d2, "CSKT_AllActivitiesLong_Yr3_041024.xlsx", overwrite = TRUE)

# Save one row per child with constructed variables
d3 <- d2 %>% 
  filter(tag_aware_id == 1) %>% 
  select(all_of(vars_to_keep))

setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets")
rio::export(d3, "CSKT_ActivityConstructs_Yr3_080924.xlsx", overwrite = TRUE)