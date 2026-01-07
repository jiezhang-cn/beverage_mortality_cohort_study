library(survival)
library(data.table)
library(dplyr)
library(ggplot2)
########################################All-cause & cause specific mortality data curated#############################
death_update <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/death.csv")
death_cause_update <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/death_cause.csv")
names(death_update) <- c("dnx_death_id", "eid", "ins_index", "dsource", "source", "date_of_death")
death_update <- death_update[, c("eid", "ins_index", "dsource", "source", "date_of_death")]
names(death_cause_update) <- c("dnx_death_cause_id", "dnx_death_id", "eid", "ins_index", "arr_index", "level", "cause_icd10")
death_cause_update <- death_cause_update[, c("eid", "ins_index", "arr_index", "level", "cause_icd10")]

baseline_date <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/R_files/baseline_date")
death_update <- merge(death_update,baseline_date,all.x = T)
str(death_update)
death_update$date_of_death <- as.Date(death_update$date_of_death)
max(death_update$date_of_death)
death_update$follow_to_death_time <- round(as.numeric(difftime(death_update$date_of_death, death_update$baseline_date, units = "days")) / 365.25, 2)
summary(death_update$follow_to_death_time)


#We defined the following 3 broad categories (ie, causes with >500 deaths) of cause-specific mortality using the International Statistical Classification of Diseases and Related Health Problems, Tenth Revision (ICD-10), codes: cancer (C00-D48), cardiovascular disease (I00-I79), and respiratory diseases (J09-J18 and J40-J47). In addition, for cancer and cardiovascular disease, we further defined the common causes of death (ie, causes with >500 deaths) within these broad categories: colorectal cancer (C18-C20), bronchus and lung cancer (C34), female breast cancer (C50), and pancreatic cancer (C25); ischemic heart diseases (I20-I25); and stroke (I60-I69).
str(death_cause_update)
death_cause_update <- death_cause_update[death_cause_update$level==1,]

death_cause_update$broad_cause <- NA

death_cause_update$broad_cause[grepl("^[C]", death_cause_update$cause_icd10) | 
                                 grepl("^D[0-4]", death_cause_update$cause_icd10)] <- 1  # cancer

death_cause_update$broad_cause[grepl("^I[0-7]", death_cause_update$cause_icd10)] <- 2  # cvd

death_cause_update$broad_cause[grepl("^J(09|1[0-8]|4[0-7])", death_cause_update$cause_icd10)] <- 3  # respiratory

death_cause_update$broad_cause[grepl("^K[2-9]", death_cause_update$cause_icd10)] <- 4  # digestive

death_cause_update$broad_cause[grepl("^F[0-8]", death_cause_update$cause_icd10) | 
                                 grepl("^G[0-9]", death_cause_update$cause_icd10)] <- 5  # nervous_system

death_cause_update$broad_cause[is.na(death_cause_update$broad_cause)] <- 6

table(death_cause_update$broad_cause, useNA = "always")


death_cause_update$cvd_death <- ifelse(death_cause_update$broad_cause == 1, 1, 0) 
death_cause_update$cancer_death <- ifelse(death_cause_update$broad_cause == 2, 1, 0)
death_cause_update$respiratory_death <- ifelse(death_cause_update$broad_cause == 3, 1, 0)
death_cause_update$digestive_death <- ifelse(death_cause_update$broad_cause == 4, 1, 0)
death_cause_update$nervous_system_death <- ifelse(death_cause_update$broad_cause == 5, 1, 0)


death_cause_update$specific_cancer <- NA
death_cause_update$specific_cancer[grep("^C(18|19|20)", death_cause_update$cause_icd10)] <- 1  #"colorectal"
death_cause_update$specific_cancer[grep("^C34", death_cause_update$cause_icd10)] <- 2 #"lung"
death_cause_update$specific_cancer[grep("^C50", death_cause_update$cause_icd10)] <- 3 #"breast"
death_cause_update$specific_cancer[grep("^C25", death_cause_update$cause_icd10)] <- 4 #"pancreatic"
death_cause_update$specific_cancer[grep("^C22", death_cause_update$cause_icd10)] <- 5 #"liver"
death_cause_update$specific_cancer[grep("^C16", death_cause_update$cause_icd10)] <- 6 #"stomach"
death_cause_update$specific_cancer[grep("^C61", death_cause_update$cause_icd10)] <- 7 #"prostate"
death_cause_update$specific_cancer[grep("^C56", death_cause_update$cause_icd10)] <- 8 #"ovarian"
death_cause_update$specific_cancer[grep("^C67", death_cause_update$cause_icd10)] <- 9 #"bladder"
death_cause_update$specific_cancer[grep("^C15", death_cause_update$cause_icd10)] <- 10 #"esophageal"
death_cause_update$specific_cancer[grep("^C64", death_cause_update$cause_icd10)] <- 11 #"kidney"
death_cause_update$specific_cancer[grep("^C71", death_cause_update$cause_icd10)] <- 12 #"brain"
death_cause_update$specific_cancer[grep("^C43", death_cause_update$cause_icd10)] <- 13 #"melanoma"


death_cause_update$specific_cvd <- NA
death_cause_update$specific_cvd[grep("^I2[0-5]", death_cause_update$cause_icd10)] <- 1 #"ihd" (ischemic heart disease)
death_cause_update$specific_cvd[grep("^I6[0-9]", death_cause_update$cause_icd10)] <- 2 #"stroke"
death_cause_update$specific_cvd[grep("^I50", death_cause_update$cause_icd10)] <- 3 #"heart_failure"
death_cause_update$specific_cvd[grep("^I1[0-5]", death_cause_update$cause_icd10)] <- 4 #"hypertensive_disease"
death_cause_update$specific_cvd[grep("^I26|^I27|^I28", death_cause_update$cause_icd10)] <- 5 #"pulmonary_heart_disease"
death_cause_update$specific_cvd[grepl("^I[0-7]", death_cause_update$cause_icd10) & 
                                  is.na(death_cause_update$specific_cvd)] <- 6 #"other_cvd"

table(death_cause_update$broad_cause, useNA = "always")
table(death_cause_update$specific_cancer, useNA = "always")
table(death_cause_update$specific_cvd, useNA = "always")
table(death_cause_update$cvd_death, useNA = "always")
table(death_cause_update$cancer_death, useNA = "always")
table(death_cause_update$respiratory_death, useNA = "always")
table(death_cause_update$digestive_death, useNA = "always")
table(death_cause_update$nervous_system_death, useNA = "always")

death_data <- merge(death_update,death_cause_update,all.x = T)


diet_questionnaire_completed_date <- read.delim("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/diet_questionnaire_completed_date.tsv")
names(diet_questionnaire_completed_date) <- c("eid","diet_questionnaire_completed_date0","diet_questionnaire_completed_date1","diet_questionnaire_completed_date2","diet_questionnaire_completed_date3","diet_questionnaire_completed_date4")
str(diet_questionnaire_completed_date)


diet_questionnaire_completed_date <- within(diet_questionnaire_completed_date, {
  diet_questionnaire_completed_date0 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date0))
  diet_questionnaire_completed_date1 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date1))
  diet_questionnaire_completed_date2 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date2))
  diet_questionnaire_completed_date3 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date3))
  diet_questionnaire_completed_date4 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date4))
})


diet_questionnaire_completed_date$diet_questionnaire_completed_date <- 
  do.call(pmax, c(list(diet_questionnaire_completed_date$diet_questionnaire_completed_date0,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date1,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date2,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date3,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date4),
                  na.rm = TRUE))


print("Summary of new date variable:")
summary(diet_questionnaire_completed_date$diet_questionnaire_completed_date)

print("Number of NA values:")
sum(is.na(diet_questionnaire_completed_date$diet_questionnaire_completed_date))

print("Date range:")
range(diet_questionnaire_completed_date$diet_questionnaire_completed_date, na.rm = TRUE)

names(ukb_beverage_data)
names(imputed_data)
names(imputed_data_2)

beverage_20w_data <- merge(ukb_beverage_data[,c(1,545:560)],imputed_data,all.x=T)
beverage_20w_data <- beverage_20w_data[complete.cases(beverage_20w_data),]
beverage_20w_data <- merge(beverage_20w_data,imputed_data_2[,c(1,7,9:17)],all.x=T)
beverage_20w_data <- merge(beverage_20w_data,diet_questionnaire_completed_date[,c("eid","diet_questionnaire_completed_date")],all.x=T)
str(beverage_20w_data)
beverage_20w_data$lowfat_milk_intake <- beverage_20w_data$low_fat_milk_volume/259
beverage_20w_data$fullfat_milk_intake <- beverage_20w_data$full_fat_milk_volume/259


beverage_20w_data$death[beverage_20w_data$eid %in% death_data$eid] <- 1
beverage_20w_data$death[is.na(beverage_20w_data$death)] <- 0
str(death_data)
beverage_20w_data <- merge(beverage_20w_data,death_data[,c("eid","date_of_death","cause_icd10","broad_cause",
                                                           "cvd_death","cancer_death","respiratory_death","digestive_death",
                                                           "nervous_system_death","specific_cancer","specific_cvd")],all.x=T)
max(beverage_20w_data$date_of_death,na.rm = T)
end_date <- as.Date("2024-07-31")

beverage_20w_data <- within(beverage_20w_data, {
  follow_to_death_time <- case_when(
    !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, diet_questionnaire_completed_date, units = "days")) / 365.25,
    !is.na(diet_questionnaire_completed_date) ~ as.numeric(difftime(end_date, diet_questionnaire_completed_date, units = "days")) / 365.25,
    TRUE ~ NA_real_
  )
})

summary(beverage_20w_data$follow_to_death_time)

##################################Beverage patterns identification#######################################
summary(beverage_20w_data$coffee_intake)
beverage_20w_data$coffee_intake_cat[beverage_20w_data$coffee_intake==0] <- 0
beverage_20w_data$coffee_intake_cat[beverage_20w_data$coffee_intake>0 & beverage_20w_data$coffee_intake<=3] <- 1
beverage_20w_data$coffee_intake_cat[beverage_20w_data$coffee_intake>3] <- 2
beverage_20w_data$coffee_intake_cat <- as.factor(beverage_20w_data$coffee_intake_cat)

summary(beverage_20w_data$tea_intake)
beverage_20w_data$tea_intake_cat[beverage_20w_data$tea_intake==0] <- 0
beverage_20w_data$tea_intake_cat[beverage_20w_data$tea_intake>0 & beverage_20w_data$tea_intake<=4] <- 1
beverage_20w_data$tea_intake_cat[beverage_20w_data$tea_intake>4] <- 2
beverage_20w_data$tea_intake_cat <- as.factor(beverage_20w_data$tea_intake_cat)


summary(beverage_20w_data$plain_water_intake)
beverage_20w_data$plain_water_intake_cat[beverage_20w_data$plain_water_intake==0] <- 0
beverage_20w_data$plain_water_intake_cat[beverage_20w_data$plain_water_intake>0 & beverage_20w_data$plain_water_intake<=3] <- 1
beverage_20w_data$plain_water_intake_cat[beverage_20w_data$plain_water_intake>3] <- 2
beverage_20w_data$plain_water_intake_cat <- as.factor(beverage_20w_data$plain_water_intake_cat)


summary(beverage_20w_data$SSB_intake)
beverage_20w_data$SSB_intake_cat[beverage_20w_data$SSB_intake==0] <- 0
beverage_20w_data$SSB_intake_cat[beverage_20w_data$SSB_intake>0 & beverage_20w_data$SSB_intake<=1] <- 1
beverage_20w_data$SSB_intake_cat[beverage_20w_data$SSB_intake>1] <- 2
beverage_20w_data$SSB_intake_cat <- as.factor(beverage_20w_data$SSB_intake_cat)


summary(beverage_20w_data$ASB_intake)
beverage_20w_data$ASB_intake_cat[beverage_20w_data$ASB_intake==0] <- 0
beverage_20w_data$ASB_intake_cat[beverage_20w_data$ASB_intake>0 & beverage_20w_data$ASB_intake<=1] <- 1
beverage_20w_data$ASB_intake_cat[beverage_20w_data$ASB_intake>1] <- 2
beverage_20w_data$ASB_intake_cat <- as.factor(beverage_20w_data$ASB_intake_cat)


summary(beverage_20w_data$NJ_intake)
beverage_20w_data$NJ_intake_cat[beverage_20w_data$NJ_intake==0] <- 0
beverage_20w_data$NJ_intake_cat[beverage_20w_data$NJ_intake>0 & beverage_20w_data$NJ_intake<=1] <- 1
beverage_20w_data$NJ_intake_cat[beverage_20w_data$NJ_intake>1] <- 2
beverage_20w_data$NJ_intake_cat <- as.factor(beverage_20w_data$NJ_intake_cat)


summary(beverage_20w_data$lowfat_milk_intake)
beverage_20w_data$lowfat_milk_intake_cat[beverage_20w_data$lowfat_milk_intake==0] <- 0
beverage_20w_data$lowfat_milk_intake_cat[beverage_20w_data$lowfat_milk_intake>0 & beverage_20w_data$lowfat_milk_intake<=1] <- 1
beverage_20w_data$lowfat_milk_intake_cat[beverage_20w_data$lowfat_milk_intake>1] <- 2
beverage_20w_data$lowfat_milk_intake_cat <- as.factor(beverage_20w_data$lowfat_milk_intake_cat)


summary(beverage_20w_data$fullfat_milk_intake)
beverage_20w_data$fullfat_milk_intake_cat[beverage_20w_data$fullfat_milk_intake==0] <- 0
beverage_20w_data$fullfat_milk_intake_cat[beverage_20w_data$fullfat_milk_intake>0 & beverage_20w_data$fullfat_milk_intake<=1] <- 1
beverage_20w_data$fullfat_milk_intake_cat[beverage_20w_data$fullfat_milk_intake>1] <- 2
beverage_20w_data$fullfat_milk_intake_cat <- as.factor(beverage_20w_data$fullfat_milk_intake_cat)



#correlations among beverages#
str(beverage_20w_data)

library(pheatmap)
library(RColorBrewer)

beverage_vars <- c("SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake", 
                   "plain_water_intake", "coffee_intake", "tea_intake")


beverage_data <- beverage_20w_data[, beverage_vars]
cor_matrix <- cor(beverage_data, use = "complete.obs")

n <- nrow(beverage_data)
p_matrix <- matrix(NA, nrow = ncol(beverage_data), ncol = ncol(beverage_data))
colnames(p_matrix) <- rownames(p_matrix) <- colnames(beverage_data)

for(i in 1:ncol(beverage_data)) {
  for(j in 1:ncol(beverage_data)) {
    if(i != j) {
      cor_test_result <- cor.test(beverage_data[,i], beverage_data[,j])
      p_matrix[i,j] <- cor_test_result$p.value
    } else {
      p_matrix[i,j] <- 0
    }
  }
}

display_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(display_matrix) <- rownames(cor_matrix)
colnames(display_matrix) <- colnames(cor_matrix)

for(i in 1:nrow(cor_matrix)) {
  for(j in 1:ncol(cor_matrix)) {
    if(i < j) {  
      display_matrix[i,j] <- as.character(round(cor_matrix[i,j], 2))
    } else if(i > j) {  
      if(p_matrix[i,j] < 0.01) {
        display_matrix[i,j] <- "**"
      } else if(p_matrix[i,j] < 0.05) {
        display_matrix[i,j] <- "*"
      } else {
        display_matrix[i,j] <- ""
      }
    } else { 
      display_matrix[i,j] <- "1"
    }
  }
}

my_colors <- colorRampPalette(c("#5BCCD9", "white", "#F291A3"))(100)

pheatmap(cor_matrix,
         display_numbers = display_matrix,
         number_color = "black",
         fontsize_number = 8,  
         color = my_colors,
         breaks = seq(-1, 1, length.out = 101),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "complete",
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 9,
         fontsize_col = 9,
         angle_col = 45,
         legend = TRUE,
         legend_breaks = c(-1, -0.5, 0, 0.5, 1),
         legend_labels = c("-1", "-0.5", "0", "0.5", "1"),
         cellwidth = 30,   
         cellheight = 30, 
         border_color = "#D3D3D3", 
         cutree_rows = 3,
         cutree_cols = 3)

cor_dist <- as.dist(1 - abs(cor_matrix))

pheatmap(cor_matrix,
         display_numbers = display_matrix,
         number_color = "black",
         fontsize_number = 8,
         color = my_colors,
         breaks = seq(-1, 1, length.out = 101),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         clustering_distance_rows = cor_dist,
         clustering_distance_cols = cor_dist,
         clustering_method = "ward.D2",
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 9,
         fontsize_col = 9,
         angle_col = 45,
         legend = TRUE,
         legend_breaks = c(-1, -0.5, 0, 0.5, 1),
         legend_labels = c("-1", "-0.5", "0", "0.5", "1"),
         cellwidth = 30,   
         cellheight = 30,  
         border_color = "#D3D3D3",  
         cutree_rows = 3,
         cutree_cols = 3)

hc_rows <- hclust(cor_dist, method = "ward.D2")
clusters_3 <- cutree(hc_rows, k = 3)
cat("\nclusters (k=3):\n")
for(i in 1:3) {
  cat(sprintf("cluster %d: %s\n", i, paste(names(clusters_3)[clusters_3 == i], collapse = ", ")))
}


summary(beverage_20w_data$coffee_intake)
beverage_20w_data$coffee_tea_type <- ifelse(beverage_20w_data$coffee_intake_cat==2 | beverage_20w_data$tea_intake_cat==2,1,0)
beverage_20w_data$coffee_tea_type <- as.factor(beverage_20w_data$coffee_tea_type)
table(beverage_20w_data$coffee_tea_type)

beverage_20w_data$SSB_ASB_type <- ifelse(beverage_20w_data$SSB_intake_cat==2 | beverage_20w_data$ASB_intake_cat==2,1,0)
beverage_20w_data$SSB_ASB_type <- as.factor(beverage_20w_data$SSB_ASB_type)
table(beverage_20w_data$SSB_ASB_type)


beverage_20w_data$NJ_water_type <- ifelse(beverage_20w_data$NJ_intake_cat==2 | beverage_20w_data$plain_water_intake_cat==2,1,0)
beverage_20w_data$NJ_water_type <- as.factor(beverage_20w_data$NJ_water_type)
table(beverage_20w_data$NJ_water_type)


table(beverage_20w_data$coffee_tea_type[beverage_20w_data$SSB_ASB_type==0 & beverage_20w_data$NJ_water_type==0])
table(beverage_20w_data$SSB_ASB_type[beverage_20w_data$coffee_tea_type==0 & beverage_20w_data$NJ_water_type==0])
table(beverage_20w_data$NJ_water_type[beverage_20w_data$coffee_tea_type==0 & beverage_20w_data$SSB_ASB_type==0])


beverage_20w_data$coffee_tea_preference <- ifelse(
  beverage_20w_data$coffee_tea_type == "1" & 
    beverage_20w_data$SSB_ASB_type == "0" & 
    beverage_20w_data$NJ_water_type == "0", 1, 0)
table(beverage_20w_data$coffee_tea_preference)

beverage_20w_data$SSB_ASB_preference <- ifelse(
  beverage_20w_data$SSB_ASB_type == "1" & 
    beverage_20w_data$coffee_tea_type == "0" & 
    beverage_20w_data$NJ_water_type == "0", 1, 0)
table(beverage_20w_data$SSB_ASB_preference)


beverage_20w_data$NJ_water_preference <- ifelse(
  beverage_20w_data$NJ_water_type == "1" & 
    beverage_20w_data$coffee_tea_type == "0" & 
    beverage_20w_data$SSB_ASB_type == "0", 1, 0)
table(beverage_20w_data$NJ_water_preference)


# Load required packages
library(dplyr)
library(fmsb)

# Data preprocessing - create preference groups
beverage_20w_data <- beverage_20w_data %>%
  mutate(
    preference_group = case_when(
      coffee_tea_preference == 1 ~ "Coffee_Tea_Preference",
      SSB_ASB_preference == 1 ~ "SSB_ASB_Preference", 
      NJ_water_preference == 1 ~ "NJ_Water_Preference",
      TRUE ~ "Mixed_Preference"  # For cases where none of the preferences are 1 or multiple preferences
    )
  )
# Remove the filter - now including all preference groups including Mixed_Preference

# Define beverage columns
beverage_cols <- c("coffee_intake", "tea_intake", "plain_water_intake", 
                   "SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake")

# Calculate standardized Z-scores for each beverage
radar_data <- beverage_20w_data %>%
  # Standardize each beverage variable (Z-score transformation)
  mutate(
    across(all_of(beverage_cols), ~ as.numeric(scale(.)[,1]), .names = "{.col}_z")
  ) %>%
  # Calculate mean Z-scores for each preference group
  group_by(preference_group) %>%
  summarise(
    Coffee = mean(coffee_intake_z, na.rm = TRUE),
    Tea = mean(tea_intake_z, na.rm = TRUE),
    Water = mean(plain_water_intake_z, na.rm = TRUE),
    SSB = mean(SSB_intake_z, na.rm = TRUE),
    ASB = mean(ASB_intake_z, na.rm = TRUE),
    NaturalJuice = mean(NJ_intake_z, na.rm = TRUE),
    LowfatMilk = mean(lowfat_milk_intake_z, na.rm = TRUE),
    FullfatMilk = mean(fullfat_milk_intake_z, na.rm = TRUE),
    .groups = 'drop'
  )

# Transform to positive values (shift minimum to 0 for better visualization)
min_val <- min(radar_data[,-1], na.rm = TRUE)
radar_data[,-1] <- radar_data[,-1] - min_val

print("Standardized radar data (Z-scores shifted to positive values):")
# Fix: Only round the numeric columns
radar_data_display <- radar_data
radar_data_display[,-1] <- round(radar_data_display[,-1], 3)
print(radar_data_display)

# Prepare data for fmsb radarchart
beverage_cols_clean <- radar_data[, -1]

# Calculate max and min for scaling
max_val <- ceiling(max(beverage_cols_clean, na.rm = TRUE) * 10) / 10
min_val_chart <- 0

# Create the radar chart data frame
radar_df <- rbind(
  rep(max_val, ncol(beverage_cols_clean)),  # max values for all beverages
  rep(min_val_chart, ncol(beverage_cols_clean)),  # min values for all beverages
  beverage_cols_clean                       # actual data for each preference group
)

# Set column names (beverages)
colnames(radar_df) <- colnames(beverage_cols_clean)

# Set row names
rownames(radar_df) <- c("max", "min", radar_data$preference_group)

print("Final radar chart data:")
print(round(radar_df, 3))

# Create radar chart
par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))

# Define colors for each preference group (now including Mixed_Preference)
colors <- c("#33A02C", "darkgrey", "#1F78B4", "#E31A1C")  # Green, Red, Blue, Dark Grey
colors_alpha <- c("#33A02C20", "#69696920", "#1F78B420", "#E31A1C20")  # Semi-transparent (darkgrey with transparency)


# Draw radar chart
radarchart(
  radar_df,
  axistype = 1,
  
  # Grid customization
  cglcol = "grey",          # Grid line color
  cglty = 1,                # Grid line type
  axislabcol = "black",     # Axis label color
  caxislabels = round(seq(0, max_val, length.out = 5), 2),  # Axis scale labels
  cglwd = 0.8,              # Grid line width
  
  # Polygon customization
  pcol = colors,            # Line colors for each group
  pfcol = colors_alpha,     # Fill colors for each group
  plwd = 2.5,               # Line width
  plty = 1,                 # Line type
  
  # Labels
  vlcex = 1.0,              # Variable label size
  title = "Standardized Beverage Intake Patterns by Preference Group"
)

# Add legend
legend(
  x = 0.7, y = 1.3,
  legend = radar_data$preference_group,
  col = colors,
  lty = 1,
  lwd = 2.5,
  cex = 1.0,
  bty = "n"
)

# Add subtitle explaining the standardization
mtext("Z-scores standardized and shifted to positive values", 
      side = 3, line = 0.5, cex = 0.8, col = "gray30")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Standardized beverage intake Z-scores by preference group:\n")
cat("(Higher values indicate above-average intake relative to population)\n\n")

# Create a properly formatted summary table
summary_table <- data.frame(
  Preference_Group = radar_data$preference_group,
  Coffee = round(radar_data$Coffee, 3),
  Tea = round(radar_data$Tea, 3),
  Water = round(radar_data$Water, 3),
  SSB = round(radar_data$SSB, 3),
  ASB = round(radar_data$ASB, 3),
  NaturalJuice = round(radar_data$NaturalJuice, 3),
  LowfatMilk = round(radar_data$LowfatMilk, 3),
  FullfatMilk = round(radar_data$FullfatMilk, 3)
)
print(summary_table)

# Calculate and display original means for context
cat("\n=== ORIGINAL INTAKE MEANS FOR CONTEXT ===\n")
original_means <- beverage_20w_data %>%
  group_by(preference_group) %>%
  summarise(
    Coffee = round(mean(coffee_intake, na.rm = TRUE), 2),
    Tea = round(mean(tea_intake, na.rm = TRUE), 2),
    Water = round(mean(plain_water_intake, na.rm = TRUE), 2),
    SSB = round(mean(SSB_intake, na.rm = TRUE), 2),
    ASB = round(mean(ASB_intake, na.rm = TRUE), 2),
    NaturalJuice = round(mean(NJ_intake, na.rm = TRUE), 2),
    LowfatMilk = round(mean(lowfat_milk_intake, na.rm = TRUE), 2),
    FullfatMilk = round(mean(fullfat_milk_intake, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print(original_means)

# Statistical significance tests using ANOVA for continuous variables
cat("\n=== STATISTICAL TESTS ===\n")
cat("ANOVA tests for differences in beverage intake across preference groups:\n")

for(i in 1:length(beverage_cols)) {
  var_name <- beverage_cols[i]
  if(sum(!is.na(beverage_20w_data[[var_name]])) > 0) {
    formula_str <- paste(var_name, "~ preference_group")
    anova_result <- aov(as.formula(formula_str), data = beverage_20w_data)
    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
    f_stat <- summary(anova_result)[[1]][["F value"]][1]
    
    cat(sprintf("%-20s: F = %6.3f, p = %7.4f %s\n", 
                var_name, f_stat, p_value,
                ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                              ifelse(p_value < 0.05, "*", " ")))))
  }
}

cat("\nSignificance codes: *** p<0.001, ** p<0.01, * p<0.05\n")

# Post-hoc tests for significant results (Tukey HSD)
cat("\n=== POST-HOC TESTS (for significant results) ===\n")
for(i in 1:length(beverage_cols)) {
  var_name <- beverage_cols[i]
  if(sum(!is.na(beverage_20w_data[[var_name]])) > 0) {
    formula_str <- paste(var_name, "~ preference_group")
    anova_result <- aov(as.formula(formula_str), data = beverage_20w_data)
    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
    
    if(p_value < 0.05) {
      cat(sprintf("\nTukey HSD for %s:\n", var_name))
      tukey_result <- TukeyHSD(anova_result)
      print(tukey_result)
    }
  }
}

# Print sample sizes for each preference group
cat("\n=== SAMPLE SIZES ===\n")
sample_sizes <- beverage_20w_data %>%
  count(preference_group, name = "n")
print(sample_sizes)



###############################Beverage patterns & mortality risk####################################################
beverage_20w_data$beverage_preference[beverage_20w_data$NJ_water_preference==1] <- 0
beverage_20w_data$beverage_preference[beverage_20w_data$coffee_tea_preference==1] <- 1
beverage_20w_data$beverage_preference[beverage_20w_data$SSB_ASB_preference==1] <- 2
beverage_20w_data$beverage_preference[beverage_20w_data$coffee_tea_preference==0 & 
                                        beverage_20w_data$SSB_ASB_preference==0 &
                                        beverage_20w_data$NJ_water_preference==0] <- 3
table(beverage_20w_data$beverage_preference)
beverage_20w_data$beverage_preference <- as.factor(beverage_20w_data$beverage_preference)

library(WeightIt)

weights <- weightit(beverage_preference ~ age + sex + ethnic + education_years + household_income + bmi + 
                      smk_num + smk_qyr + alcohol_intake_frequency+diet_score + 
                      PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                    data = beverage_20w_data,
                    method = "ps")

survfit_weighted <- survfit(Surv(time = follow_to_death_time,
                                 event = death) ~ beverage_preference,
                            data = beverage_20w_data,
                            weights = weights$weights)

library(survminer)
ggsurvplot(survfit_weighted,
           fun = function(y)(1-y),
           size=1,
           censor=F,
           pval = T,
           conf.int = F,  
           surv.scale="percent",
           xlab="Follow-up years",
           ylab="Cumulative All-Cause Mortality", 
           ylim=c(0,0.14),
           xlim=c(0,15),
           font.x=13,
           font.y=13,
           font.legend=10,
           font.tickslab=10,
           break.x.by = 5, break.y.by = 0.02,
           palette = c("#1F78B4","#33A02C","#E31A1C","darkgrey")
           
)
ggsave("C:/Users/zhangjie/Desktop/beverage_preference_mortality_survival_plot.pdf", width = 8, height = 6, dpi = 300)



cox_model <- coxph(Surv(time = follow_to_death_time,
                        event = death) ~ beverage_preference+ age + sex + ethnic + 
                     education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency + diet_score + 
                     PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                   data = beverage_20w_data,)
summary(cox_model)





###specific-death###
library(survival)
library(cmprsk)
library(riskRegression)
library(prodlim)

beverage_20w_data$competing_risk_event <- 0
beverage_20w_data$competing_risk_event[beverage_20w_data$cvd_death == 1 ] <- 1
beverage_20w_data$competing_risk_event[beverage_20w_data$cancer_death == 1] <- 2
beverage_20w_data$competing_risk_event[beverage_20w_data$respiratory_death == 1] <- 3
beverage_20w_data$competing_risk_event[beverage_20w_data$digestive_death == 1] <- 4
beverage_20w_data$competing_risk_event[beverage_20w_data$nervous_system_death == 1] <- 5
beverage_20w_data$competing_risk_event[beverage_20w_data$death == 1 & 
                                         beverage_20w_data$competing_risk_event == 0] <- 6

table(beverage_20w_data$competing_risk_event)

beverage_preference_csc <- CSC(Hist(follow_to_death_time, competing_risk_event) ~ 
                                 beverage_preference +
                                 age + sex + ethnic + 
                                 education_years + household_income + bmi + 
                                 smk_num + smk_qyr + alcohol_intake_frequency + 
                                 PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                               data = beverage_20w_data)


summary(beverage_preference_csc$models[[1]])$coefficients  # CVD death
summary(beverage_preference_csc$models[[2]])$coefficients  # cancer death
summary(beverage_preference_csc$models[[3]])$coefficients  # respiratory death
summary(beverage_preference_csc$models[[4]])$coefficients  # digestive death
summary(beverage_preference_csc$models[[5]])$coefficients  # nervous_system death




################################Valiation analysis for cluster of patterns########################################################
library(cluster)

# Prepare data
beverage_vars <- c("SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake", 
                   "plain_water_intake", "coffee_intake", "tea_intake")

beverage_data <- beverage_20w_data[, beverage_vars]
beverage_data <- na.omit(beverage_data)

# Calculate correlation matrix and distance
cor_matrix <- cor(beverage_data, use = "complete.obs")
cor_dist <- as.dist(1 - abs(cor_matrix))

# Hierarchical clustering
hc_rows <- hclust(cor_dist, method = "ward.D2")

# Calculate silhouette scores and clustering quality for different k
silhouette_scores <- numeric()
cluster_quality <- list()

for(k in 2:6) {
  clusters <- cutree(hc_rows, k = k)
  sil <- silhouette(clusters, cor_dist)
  avg_sil <- mean(sil[, 3])
  silhouette_scores[k-1] <- avg_sil
  
  # Calculate clustering quality (within vs between correlation)
  within_cors <- numeric()
  between_cors <- numeric()
  
  for(i in 1:k) {
    cluster_vars <- beverage_vars[clusters == i]
    other_vars <- beverage_vars[clusters != i]
    
    if(length(cluster_vars) > 1 && length(other_vars) > 0) {
      # Within-cluster correlation
      within_cor <- cor_matrix[cluster_vars, cluster_vars]
      within_cor_vec <- abs(within_cor[upper.tri(within_cor)])
      within_cors <- c(within_cors, mean(within_cor_vec))
      
      # Between-cluster correlation
      between_cor <- cor_matrix[cluster_vars, other_vars, drop = FALSE]
      between_cor_vec <- abs(as.vector(between_cor))
      between_cors <- c(between_cors, mean(between_cor_vec))
    }
  }
  
  cluster_quality[[k-1]] <- list(
    within = mean(within_cors),
    between = mean(between_cors),
    ratio = mean(within_cors) / mean(between_cors)
  )
}
best_k_sil <- which.max(silhouette_scores) + 1

# Plot enhanced silhouette coefficient figure
# Set layout
layout(matrix(c(1,2), nrow=1), widths=c(2,1))
par(mar=c(5,5,4,2))

# Left panel: Silhouette coefficient trend
plot(2:6, silhouette_scores, type = "b", pch = 19, col = "blue",
     xlab = "Number of clusters (k)", ylab = "Average silhouette coefficient",
     main = "Silhouette Coefficient for Different k Values", 
     ylim = c(0, max(silhouette_scores) * 1.5),
     cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.3,
     lwd = 2, cex = 1.5)

# Mark optimal k
points(best_k_sil, silhouette_scores[best_k_sil-1], 
       col = "red", pch = 19, cex = 2.5)

# Add reference lines
abline(h = c(0.25, 0.5, 0.7), lty = 2, col = "gray60", lwd = 1)
text(6.3, 0.25, "Weak", pos = 3, cex = 0.9, col = "gray40")
text(6.3, 0.5, "Reasonable", pos = 3, cex = 0.9, col = "gray40")
text(6.3, 0.7, "Strong", pos = 3, cex = 0.9, col = "gray40")

# Add value labels
text(2:6, silhouette_scores, 
     labels = sprintf("%.3f", silhouette_scores),
     pos = 3, cex = 0.85, col = "blue")

# Add clustering quality ratio information
for(k in 2:6) {
  quality_ratio <- cluster_quality[[k-1]]$ratio
  text(k, silhouette_scores[k-1] * 0.85,
       labels = sprintf("Ratio=%.2f", quality_ratio),
       pos = 1, cex = 0.75, col = "darkgreen")
}

# Add legend
legend("topright", 
       legend = c(
         sprintf("Optimal k=%d (Sil=%.3f)", best_k_sil, max(silhouette_scores)),
         "Within/Between Cor Ratio"
       ),
       text.col = c("red", "darkgreen"),
       bty = "n",
       cex = 0.9)

# Right panel: Clustering quality barplot
par(mar=c(5,4,4,2))

k_values <- 2:6
within_means <- sapply(cluster_quality, function(x) x$within)
between_means <- sapply(cluster_quality, function(x) x$between)

bp <- barplot(rbind(within_means, between_means),
              beside = TRUE,
              names.arg = k_values,
              col = c("#F291A3", "#5BCCD9"),
              ylim = c(0, max(within_means) * 1.3),
              xlab = "Number of clusters (k)",
              ylab = "Average |correlation|",
              main = "Within vs Between Cluster Correlation",
              cex.lab = 1.1, cex.axis = 1.0, cex.main = 1.2,
              border = NA)

# Add value labels
text(bp[1,], within_means, 
     labels = sprintf("%.3f", within_means),
     pos = 3, cex = 0.8)
text(bp[2,], between_means, 
     labels = sprintf("%.3f", between_means),
     pos = 3, cex = 0.8)

# Add legend
legend("topright",
       legend = c("Within-cluster", "Between-cluster"),
       fill = c("#F291A3", "#5BCCD9"),
       border = NA,
       cex = 0.9,
       bty = "n")

# Add grid lines
abline(h = seq(0, max(within_means), by = 0.05), 
       col = "gray90", lty = 1)

# Reset layout
par(mfrow=c(1,1))

#Cluster Bootstrap
set.seed(123)
n_boot <- 5000
n_samples <- nrow(beverage_data)
n_vars <- ncol(beverage_data)

hc_original <- hclust(d, method = "ward.D2")
clusters_real <- cutree(hc_original, k = 3)

cat("=== actual cluster (k=3) ===\n")
for(i in 1:3) {
  vars <- names(clusters_real)[clusters_real == i]
  cat("Cluster", i, ":", paste(vars, collapse = ", "), "\n")
}

set.seed(456)
clusters_random <- sample(1:3, n_vars, replace = TRUE)
names(clusters_random) <- colnames(beverage_data)

while(length(unique(clusters_random)) < 3) {
  clusters_random <- sample(1:3, n_vars, replace = TRUE)
  names(clusters_random) <- colnames(beverage_data)
}

cat("\n=== random cluster (k=3) ===\n")
for(i in 1:3) {
  vars <- names(clusters_random)[clusters_random == i]
  if(length(vars) > 0) {
    cat("Cluster", i, ":", paste(vars, collapse = ", "), "\n")
  }
}

# 3. Bootstrap validation
bootstrap_stability_real <- function(original_clusters) {
  jaccard_scores <- matrix(0, nrow = 3, ncol = n_boot)
  successful_boots <- 0
  
  cat("\n Bootstrap validation...\n")
  
  for(b in 1:n_boot) {
    if(b %% 1000 == 0) cat("complete", b, "/", n_boot, "\n")
    
    boot_size <- min(20000, n_samples)
    boot_idx <- sample(1:n_samples, boot_size, replace = TRUE)
    data_boot <- beverage_data[boot_idx, ]
    
    tryCatch({
      r_boot <- cor(data_boot, use = "pairwise.complete.obs")
      d_boot <- as.dist(1 - abs(r_boot))
      hc_boot <- hclust(d_boot, method = "ward.D2")
      
      if(nrow(hc_boot$merge) == (n_vars - 1)) {
        clusters_boot <- cutree(hc_boot, k = 3)
        
        if(length(unique(clusters_boot)) == 3) {
          successful_boots <- successful_boots + 1

          for(i in 1:3) {
            original_members <- names(original_clusters)[original_clusters == i]
            if(length(original_members) == 0) next
            
            max_jaccard <- 0
            for(j in 1:3) {
              boot_members <- names(clusters_boot)[clusters_boot == j]
              intersection <- length(intersect(original_members, boot_members))
              union <- length(union(original_members, boot_members))
              
              if(union > 0) {
                jaccard <- intersection / union
                max_jaccard <- max(max_jaccard, jaccard)
              }
            }
            jaccard_scores[i, successful_boots] <- max_jaccard
          }
        }
      }
    }, error = function(e) {})
  }
  
  cat(sprintf("Bootstrap: %d/%d (%.1f%%)\n", 
              successful_boots, n_boot, 100*successful_boots/n_boot))
  
  return(list(
    scores = jaccard_scores[, 1:successful_boots],
    n_success = successful_boots
  ))
}

bootstrap_stability_random <- function(original_clusters) {
  jaccard_scores <- matrix(0, nrow = 3, ncol = n_boot)
  successful_boots <- 0
  for(b in 1:n_boot) {
    if(b %% 1000 == 0) cat("  complete", b, "/", n_boot, "\n")

    clusters_boot_random <- sample(1:3, n_vars, replace = TRUE)
    names(clusters_boot_random) <- colnames(beverage_data)
    
    if(length(unique(clusters_boot_random)) == 3) {
      successful_boots <- successful_boots + 1
      
      for(i in 1:3) {
        original_members <- names(original_clusters)[original_clusters == i]
        if(length(original_members) == 0) next
        
        max_jaccard <- 0
        for(j in 1:3) {
          boot_members <- names(clusters_boot_random)[clusters_boot_random == j]
          intersection <- length(intersect(original_members, boot_members))
          union <- length(union(original_members, boot_members))
          
          if(union > 0) {
            jaccard <- intersection / union
            max_jaccard <- max(max_jaccard, jaccard)
          }
        }
        jaccard_scores[i, successful_boots] <- max_jaccard
      }
    }
  }
  
  cat(sprintf("Bootstrap: %d/%d (%.1f%%)\n", 
              successful_boots, n_boot, 100*successful_boots/n_boot))
  
  return(list(
    scores = jaccard_scores[, 1:successful_boots],
    n_success = successful_boots
  ))
}

result_real <- bootstrap_stability_real(clusters_real)
result_random <- bootstrap_stability_random(clusters_random)

###############################Sensitive Analyses#############################################
##excluded first 2 years death participants##
sensentive_analyses_data <- beverage_20w_data[beverage_20w_data$follow_to_death_time>2,]

cox_model <- coxph(Surv(time = follow_to_death_time,
                        event = death) ~ beverage_preference+ age + sex + ethnic + 
                     education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency + diet_score + 
                     PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                   data = sensentive_analyses_data,)
summary(cox_model)


##excluded participants with prevalent CVD, Diabetes, Cancer ##
prevalent_cvd_diabetes_cancer_participant <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/R_files/prevalent_cvd_diabetes_cancer_participant.csv")
names(prevalent_cvd_diabetes_cancer_participant) <- c("eid","prevalent_cvd","prevalent_diabetes","prevalent_cancer")

sensentive_analyses_data2 <- beverage_20w_data %>%
  left_join(
    prevalent_cvd_diabetes_cancer_participant %>%
      select(eid, prevalent_cvd, prevalent_diabetes, prevalent_cancer),
    by = "eid"
  ) %>%
  mutate(
    prevalent_cvd_binary = ifelse(
      grepl("[1-4]", prevalent_cvd) & 
        !is.na(prevalent_cvd) &
        !prevalent_cvd %in% c("-3", "-7"),
      1, 0
    ),
    prevalent_diabetes_binary = ifelse(
      grepl("1", prevalent_diabetes) & 
        !is.na(prevalent_diabetes) &
        !prevalent_diabetes %in% c("-3", "-1"),
      1, 0
    ),
    prevalent_cancer_binary = ifelse(
      grepl("1", prevalent_cancer) & 
        !is.na(prevalent_cancer) &
        !prevalent_cancer %in% c("-3", "-1"),
      1, 0
    )
  )

cox_model <- coxph(Surv(time = follow_to_death_time,
                        event = death) ~ beverage_preference + age + sex + ethnic + 
                     education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency + diet_score + 
                     PA_mod_vig_150 + overall_health_rating + total_energy_intake +
                     prevalent_cvd_binary + prevalent_diabetes_binary + prevalent_cancer_binary,
                   data = sensentive_analyses_data2)
summary(cox_model)



##additional adjustments of medications (antihypertensive_medications, cholesterol_lowering_medications, diabetes_medications)##
sensentive_analyses_data3 <- merge(beverage_20w_data,ckm_ukb_data[,c("eid","antihypertensive_medications",
                                                                     "cholesterol_lowering_medications","diabetes_medications")],all.x = T)
cox_model <- coxph(Surv(time = follow_to_death_time,
                        event = death) ~ beverage_preference+ age + sex + ethnic + 
                     education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency + diet_score + 
                     PA_mod_vig_150 + overall_health_rating + total_energy_intake+
                     antihypertensive_medications+cholesterol_lowering_medications+
                     diabetes_medications,
                   data = sensentive_analyses_data3,)
summary(cox_model)



##participants with 2+/3+ complete 24h recall diet data##
str(beverage_20w_data)
library(pheatmap)
library(RColorBrewer)

beverage_vars <- c("SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake", 
                   "plain_water_intake", "coffee_intake", "tea_intake")


beverage_data <- beverage_20w_data[beverage_20w_data$eid %in% eids_with_2plus_nonmissing_diet_recall_info , beverage_vars]
cor_matrix <- cor(beverage_data, use = "complete.obs")

n <- nrow(beverage_data)
p_matrix <- matrix(NA, nrow = ncol(beverage_data), ncol = ncol(beverage_data))
colnames(p_matrix) <- rownames(p_matrix) <- colnames(beverage_data)

for(i in 1:ncol(beverage_data)) {
  for(j in 1:ncol(beverage_data)) {
    if(i != j) {
      cor_test_result <- cor.test(beverage_data[,i], beverage_data[,j])
      p_matrix[i,j] <- cor_test_result$p.value
    } else {
      p_matrix[i,j] <- 0
    }
  }
}

display_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(display_matrix) <- rownames(cor_matrix)
colnames(display_matrix) <- colnames(cor_matrix)

for(i in 1:nrow(cor_matrix)) {
  for(j in 1:ncol(cor_matrix)) {
    if(i < j) { 
      display_matrix[i,j] <- as.character(round(cor_matrix[i,j], 2))
    } else if(i > j) {  
      if(p_matrix[i,j] < 0.01) {
        display_matrix[i,j] <- "**"
      } else if(p_matrix[i,j] < 0.05) {
        display_matrix[i,j] <- "*"
      } else {
        display_matrix[i,j] <- ""
      }
    } else {  
      display_matrix[i,j] <- "1"
    }
  }
}

my_colors <- colorRampPalette(c("#5BCCD9", "white", "#F291A3"))(100)

cor_dist <- as.dist(1 - abs(cor_matrix))

pheatmap(cor_matrix,
         display_numbers = display_matrix,
         number_color = "black",
         fontsize_number = 8,
         color = my_colors,
         breaks = seq(-1, 1, length.out = 101),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         clustering_distance_rows = cor_dist,
         clustering_distance_cols = cor_dist,
         clustering_method = "ward.D2",
         show_rownames = TRUE,
         show_colnames = TRUE,
         fontsize_row = 9,
         fontsize_col = 9,
         angle_col = 45,
         legend = TRUE,
         legend_breaks = c(-1, -0.5, 0, 0.5, 1),
         legend_labels = c("-1", "-0.5", "0", "0.5", "1"),
         cellwidth = 30,   
         cellheight = 30,  
         border_color = "#D3D3D3",  
         cutree_rows = 4,
         cutree_cols = 4)


