
# Clear all objects in memory

rm(list = ls())

if (!require(pacman)) install.packages("pacman")
p_load(rio, dplyr, tidyverse,janitor,psych, gtsummary,officer,flextable,
       summarytools,descr, readxl,ggview,ggpubr)


source("C:\\Users\\vonbe\\OneDrive\\Adam\\NDPH Local\\Stats Local\\R code Collection\\repath.R")
source("C:/Users/vonbe/OneDrive/Adam/Stats Local/R code Collection/GTsummaryGlobalOpts.R")



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

scale.items <- unique(scale.items)

d<- rio::import("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Data/ConstructedDataSets/CSKT_EncampmentConstructsPrePost_Year3_311024.xlsx")
names(d)

######################################################################
# Frequency table for individual items
######################################################################



# Function to get frequencies by item
FreqByItem <- function(data, items) {
  df.out <- data.frame()
  
  for (v in items) {
    # Check if the column exists in data
    if (!v %in% colnames(data)) {
      warning(paste("Variable", v, "is not in the data"))
      next
    }
    
    # Get frequency table
    f <- summarytools::freq(data[[v]], report.nas = FALSE)
    f <- as.data.frame(f) %>% rownames_to_column() # Convert to data frame and add row names
    f <- f %>% dplyr::select(1:3) # Select first 3 columns (row name, frequency, percentage)
    f <- f %>% filter(rowname %in% c("0", "1","2","3","4","5","6", "7", "8", "9", "10"))
    
    # If no frequencies, create an empty row for the item
    if (nrow(f) == 0) { 
      df <- data.frame(item = v)
    } else {
      # Create a data frame with columns v0 to v10 for frequency representation
      df <- data.frame(matrix(NA_character_, nrow = 1, ncol = 11))
      colnames(df) <- paste0("v", 0:10)
      
      # Rename columns in f and format values
      names(f) <- c("value", "freq", "perc")
      f$value <- as.numeric(f$value)
      f$comb <- paste0(f$freq, " (", formatC(f$perc, 1, format = "f"), "% )")
      
      # Populate dummy data frame with values
      for (i in 0:10) {
        if (i %in% f$value) {
          df[[paste0("v", i)]] <- f$comb[f$value == i] # Assign value if exists
        } else {
          df[[paste0("v", i)]] <- NA  # Assign NA if no matching value
        }
      }
      
      # Calculate the mean, handle cases without data
      #item.mean <- ifelse(nrow(f) > 0, formatC(summarytools::descr(data[[v]])$Mean, 5, format = "f"), NA)
      
      # Create final data frame row with item name, frequencies, nonmissing count, and mean
      df <- df %>% mutate(
        item = v,
        nonmissing = sum(f$freq, na.rm = TRUE),
      )
    }
    
    # Append to output data frame
    df.out <- bind_rows(df.out, df)
  }
  
  return(df.out)
}




wb <- createWorkbook()

## Loop through youth and adults

d <- d %>% mutate(adult2 = ifelse(adult==1,"Adult","Youth"))


# Assuming `wb` is the workbook you're working with
wb <- createWorkbook()

for (i in c("Adult", "Youth")) {
  
  # Filter for Pre values
  tmp1 <- d %>% filter(time == "pre" & adult2 == i)
  df1 <- FreqByItem(data = tmp1, items = scale.items)
  df1$time <- "pre"
  
  # Filter for Post values
  tmp2 <- d %>% filter(time == "post" & adult2 == i)
  df2 <- FreqByItem(data = tmp2, items = scale.items)
  df2$time <- "post"
  
  # Combine pre and post data frames
  df <- rbind(df1, df2) 
  df$item.f <- factor(df$item, levels = scale.items)
  df$time.f <- factor(df$time, levels = c("pre", "post"))
  df <- df %>% arrange(item.f, time.f)
  df_selected <- df %>% select(item.f, time.f, nonmissing, contains("v"))
  
  # Add a new worksheet and write data to it
  sheetname <- paste0("Freqs_",i) 
  addWorksheet(wb, sheetname)
  writeData(wb, sheet = sheetname, x = df_selected)
  
}


wb
######################################################################
# Scale differences
######################################################################


names(d)

outcomes <- names(d)[grep("scale", names(d))]
outcomes

for (i in c("Adult", "Youth")) {
  
  df.means <- data.frame()
  for (j in outcomes) {
    
    # Calculate means for each level of the 4-level variable
    means <- d %>% 
      filter(adult2 == i) %>% 
      group_by(time) %>%  
      summarise(mean_value = mean(get(j), na.rm = TRUE)) %>%
      pivot_wider(names_from = time, values_from = mean_value)
    means <- means %>% mutate_all(function(x) round(x,2))
    
    # Check if both pre and post values are non-missing
    if (!any(is.na(means$pre)) && !any(is.na(means$post))) {
      
      # Conduct one-way ANOVA
      formula <- as.formula(paste(j, "~ time"))  # Replace with your 4-level variable
      anova_result <- aov(formula, data = d)
      p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]  # Extract p-value from ANOVA summary
      
      # Format results
      df <- means %>%
        mutate(outcome = j,
               p.fmt = ifelse(p_value < .001, "<0.001", formatC(p_value, digits = 3, format = "f")),
               asterisk = case_when(
                 p_value < 0.001 ~ "***",
                 p_value < 0.01  ~ "**",
                 p_value < 0.05  ~ "*",
                 p_value < 0.10  ~ "~",
                 TRUE            ~ ""
               )) %>%
        select(outcome, pre, post, p.fmt, asterisk)  # Adjust to ensure order
      
      df.means <- rbind(df.means, df)  # Add results for each outcome to df.means
    } else {
      # If missing values, add a row indicating NA for the p-value and asterisk
      df <- means %>%
        mutate(outcome = j,
               p.fmt = NA,
               asterisk = "") %>%
        select(outcome, pre, post, p.fmt, asterisk)  # Adjust to ensure order
      
      df.means <- rbind(df.means, df)  # Add results for each outcome to df.means
    }
  }
  
  sheetname <- paste0("Scales_", i) 
  addWorksheet(wb, sheetname)
  writeData(wb, sheet = sheetname, x = df.means)
  
}

# Print the final data frame with means and ANOVA p-values
print(df.means)

wb


######################################################################
# Save workbook
######################################################################




  
setwd("C:/Users/vonbe/OneDrive/Adam/BCH Local/Other projects/CSKT Aware/Analyses/DescriptiveAnalyses")
saveWorkbook(wb, "EncampmentResultsYr3_110424.xlsx", overwrite = TRUE)

