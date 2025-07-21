library(survival)
########################################All-cause & cause specific mortality#############################
death <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/death.txt")
death_cause <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/death_cause.txt")

death_update <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/death.csv")
death_cause_update <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/death_cause.csv")
names(death_update) <- c("dnx_death_id", "eid", "ins_index", "dsource", "source", "date_of_death")
death_update <- death_update[, c("eid", "ins_index", "dsource", "source", "date_of_death")]
names(death_cause_update) <- c("dnx_death_cause_id", "dnx_death_id", "eid", "ins_index", "arr_index", "level", "cause_icd10")
death_cause_update <- death_cause_update[, c("eid", "ins_index", "arr_index", "level", "cause_icd10")]


death <- merge(death,ckm_ukb_data[,c("eid","baseline_date")],all.x = T)
str(death)
death$date_of_death <- as.Date(death$date_of_death)
max(death$date_of_death)
death$follow_to_death_time <- round(as.numeric(difftime(death$date_of_death, death$baseline_date, units = "days")) / 365.25, 2)
summary(death$follow_to_death_time)


#We defined the following 3 broad categories (ie, causes with >500 deaths) of cause-specific mortality using the International Statistical Classification of Diseases and Related Health Problems, Tenth Revision (ICD-10), codes: cancer (C00-D48), cardiovascular disease (I00-I79), and respiratory diseases (J09-J18 and J40-J47). In addition, for cancer and cardiovascular disease, we further defined the common causes of death (ie, causes with >500 deaths) within these broad categories: colorectal cancer (C18-C20), bronchus and lung cancer (C34), female breast cancer (C50), and pancreatic cancer (C25); ischemic heart diseases (I20-I25); and stroke (I60-I69).
str(death_cause)
death_cause <- death_cause[death_cause$level==1,]


# 创建主要分类
death_cause$broad_cause <- NA
death_cause$broad_cause[grep("^[C]", death_cause$cause_icd10) | 
                          grep("^D[0-4]", death_cause$cause_icd10)] <- 1  #"cancer"
death_cause$broad_cause[grep("^I[0-7]", death_cause$cause_icd10)] <- 2  #"cvd"
death_cause$broad_cause[grep("^J(09|1[0-8]|4[0-7])", death_cause$cause_icd10)] <- 3 #"respiratory"

# 创建cancer二分类变量 
death_cause$cancer_death <- ifelse(death_cause$broad_cause == 1, 1, 0)
# 创建cvd二分类变量
death_cause$cvd_death <- ifelse(death_cause$broad_cause == 2, 1, 0) 
# 创建respiratory二分类变量
death_cause$respiratory_death <- ifelse(death_cause$broad_cause == 3, 1, 0)


# 创建具体癌症类型
death_cause$specific_cancer <- NA
death_cause$specific_cancer[grep("^C(18|19|20)", death_cause$cause_icd10)] <- 1  #"colorectal"
death_cause$specific_cancer[grep("^C34", death_cause$cause_icd10)] <- 2 #"lung"
death_cause$specific_cancer[grep("^C50", death_cause$cause_icd10)] <- 3 #"breast"
death_cause$specific_cancer[grep("^C25", death_cause$cause_icd10)] <- 4 #"pancreatic"

# 创建具体心血管疾病类型
death_cause$specific_cvd <- NA
death_cause$specific_cvd[grep("^I2[0-5]", death_cause$cause_icd10)] <- 1 #"ihd"
death_cause$specific_cvd[grep("^I6[0-9]", death_cause$cause_icd10)] <- 2 #"stroke"

# 检查各类别的死亡人数
table(death_cause$broad_cause, useNA = "always")
table(death_cause$specific_cancer, useNA = "always")
table(death_cause$specific_cvd, useNA = "always")

death_data <- merge(death_update,death_cause_update,all.x = T)



##################################CKM stage 20w analysis#######################################
diet_questionnaire_completed_date <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/diet_questionnaire_completed_date.tsv")
names(diet_questionnaire_completed_date) <- c("eid","diet_questionnaire_completed_date0","diet_questionnaire_completed_date1","diet_questionnaire_completed_date2","diet_questionnaire_completed_date3","diet_questionnaire_completed_date4")
str(diet_questionnaire_completed_date)

# 1. 首先将字符串日期转换为日期格式，空字符串转为NA
diet_questionnaire_completed_date <- within(diet_questionnaire_completed_date, {
  diet_questionnaire_completed_date0 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date0))
  diet_questionnaire_completed_date1 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date1))
  diet_questionnaire_completed_date2 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date2))
  diet_questionnaire_completed_date3 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date3))
  diet_questionnaire_completed_date4 = as.Date(gsub("T.*", "", diet_questionnaire_completed_date4))
})

# 2. 对每行取最晚的日期
diet_questionnaire_completed_date$diet_questionnaire_completed_date <- 
  do.call(pmax, c(list(diet_questionnaire_completed_date$diet_questionnaire_completed_date0,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date1,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date2,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date3,
                       diet_questionnaire_completed_date$diet_questionnaire_completed_date4),
                  na.rm = TRUE))

# 3. 检查结果
# 查看新变量的摘要
print("Summary of new date variable:")
summary(diet_questionnaire_completed_date$diet_questionnaire_completed_date)

# 检查NA的数量
print("Number of NA values:")
sum(is.na(diet_questionnaire_completed_date$diet_questionnaire_completed_date))

# 查看日期范围
print("Date range:")
range(diet_questionnaire_completed_date$diet_questionnaire_completed_date, na.rm = TRUE)

names(ukb_beverage_data)
str(imputed_data)
beverage_20w_data <- merge(ukb_beverage_data[,c(1,545:552)],imputed_data,all.x=T)
beverage_20w_data <- merge(beverage_20w_data,diet_questionnaire_completed_date[,c("eid","diet_questionnaire_completed_date")],all.x=T)
str(beverage_20w_data)



beverage_20w_data$death[beverage_20w_data$eid %in% death_data$eid] <- 1
beverage_20w_data$death[is.na(beverage_20w_data$death)] <- 0
str(death_data)
beverage_20w_data <- merge(beverage_20w_data,death_data[,c("eid","date_of_death","cause_icd10","broad_cause",
                                                           "cvd_death","cancer_death","respiratory_death","specific_cancer","specific_cvd")],all.x=T)
beverage_20w_data$cvd_death[is.na(beverage_20w_data$cvd_death)] <- 0
beverage_20w_data$respiratory_death[is.na(beverage_20w_data$respiratory_death)] <- 0
beverage_20w_data$cancer_death[is.na(beverage_20w_data$cancer_death)] <- 0

end_date <- as.Date("2024-07-31")

# 4. 计算随访时间
beverage_20w_data <- within(beverage_20w_data, {
  # 对于已经死亡的人，使用死亡日期
  # 对于仍在随访的人，使用2024-07-31
  follow_to_death_time <- case_when(
    !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, diet_questionnaire_completed_date, units = "days")) / 365.25,
    !is.na(diet_questionnaire_completed_date) ~ as.numeric(difftime(end_date, diet_questionnaire_completed_date, units = "days")) / 365.25,
    TRUE ~ NA_real_
  )
})

summary(beverage_20w_data$follow_to_death_time)
names(ckm_ukb_data)
beverage_20w_data <- merge(beverage_20w_data, 
                           ckm_ukb_data[ckm_ukb_data$ckm_stage!="NA", colnames(ckm_ukb_data) %in% c("eid","ckm_stage")], 
                           all.x=TRUE)
names(beverage_20w_data)



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
beverage_20w_data$lowfat_milk_intake_cat[beverage_20w_data$lowfat_milk_intake>0 & beverage_20w_data$lowfat_milk_intake<=5] <- 1
beverage_20w_data$lowfat_milk_intake_cat[beverage_20w_data$lowfat_milk_intake>5] <- 2
beverage_20w_data$lowfat_milk_intake_cat <- as.factor(beverage_20w_data$lowfat_milk_intake_cat)


summary(beverage_20w_data$fullfat_milk_intake)
beverage_20w_data$fullfat_milk_intake_cat[beverage_20w_data$fullfat_milk_intake==0] <- 0
beverage_20w_data$fullfat_milk_intake_cat[beverage_20w_data$fullfat_milk_intake>0 & beverage_20w_data$fullfat_milk_intake<=1] <- 1
beverage_20w_data$fullfat_milk_intake_cat[beverage_20w_data$fullfat_milk_intake>1] <- 2
beverage_20w_data$fullfat_milk_intake_cat <- as.factor(beverage_20w_data$fullfat_milk_intake_cat)


################################beverage difference among CKM satge###################################
# Load required packages
library(dplyr)
library(fmsb)

# Data preprocessing
beverage_20w_data <- beverage_20w_data %>%
  mutate(
    ckm_group = case_when(
      ckm_stage %in% c(0, 1) ~ "CKM_0-1",
      ckm_stage == 2 ~ "CKM_2", 
      ckm_stage %in% c(3, 4) ~ "CKM_3-4",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ckm_group))

# Define beverage columns
beverage_cols <- c("coffee_intake", "tea_intake", "plain_water_intake", 
                   "SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake")

# Calculate standardized Z-scores for each beverage
radar_data <- beverage_20w_data %>%
  filter(!is.na(ckm_group)) %>%
  # Standardize each beverage variable (Z-score transformation)
  mutate(
    across(all_of(beverage_cols), ~ as.numeric(scale(.)[,1]), .names = "{.col}_z")
  ) %>%
  # Calculate mean Z-scores for each CKM group
  group_by(ckm_group) %>%
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
  beverage_cols_clean                       # actual data for each CKM group
)

# Set column names (beverages)
colnames(radar_df) <- colnames(beverage_cols_clean)

# Set row names
rownames(radar_df) <- c("max", "min", radar_data$ckm_group)

print("Final radar chart data:")
print(round(radar_df, 3))

# Create radar chart
par(mfrow = c(1, 1), mar = c(2, 2, 3, 2))

# Define colors for each CKM group
colors <- c("#E31A1C", "#1F78B4", "#33A02C")  # Red, Blue, Green
colors_alpha <- c("#E31A1C20", "#1F78B420", "#33A02C20")  # Semi-transparent

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
  title = "Standardized Beverage Intake Patterns by CKM Stage"
)

# Add legend
legend(
  x = 0.7, y = 1.3,
  legend = radar_data$ckm_group,
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
cat("Standardized beverage intake Z-scores by CKM stage:\n")
cat("(Higher values indicate above-average intake relative to population)\n\n")

# Create a properly formatted summary table
summary_table <- data.frame(
  CKM_Group = radar_data$ckm_group,
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
  filter(!is.na(ckm_group)) %>%
  group_by(ckm_group) %>%
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
cat("ANOVA tests for differences in beverage intake across CKM groups:\n")

for(i in 1:length(beverage_cols)) {
  var_name <- beverage_cols[i]
  if(sum(!is.na(beverage_20w_data[[var_name]])) > 0) {
    formula_str <- paste(var_name, "~ ckm_group")
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
    formula_str <- paste(var_name, "~ ckm_group")
    anova_result <- aov(as.formula(formula_str), data = beverage_20w_data)
    p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
    
    if(p_value < 0.05) {
      cat(sprintf("\nTukey HSD for %s:\n", var_name))
      tukey_result <- TukeyHSD(anova_result)
      print(tukey_result)
    }
  }
}

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
      TRUE ~ "Other"  # For cases where none of the preferences are 1
    )
  ) %>%
  filter(preference_group != "Other")  # Remove cases with no clear preference

# Define beverage columns
beverage_cols <- c("coffee_intake", "tea_intake", "plain_water_intake", 
                   "SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake")

# Calculate standardized Z-scores for each beverage
radar_data <- beverage_20w_data %>%
  filter(preference_group != "Other") %>%
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

# Define colors for each preference group
colors <- c("#33A02C", "#1F78B4", "#E31A1C")  # Red, Blue, Green
colors_alpha <- c("#33A02C20", "#1F78B420", "#E31A1C20")  # Semi-transparent

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
  filter(preference_group != "Other") %>%
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
  filter(preference_group != "Other") %>%
  count(preference_group, name = "n")
print(sample_sizes)


###########################Associations of beverages with mortality##############################
data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==3 | beverage_20w_data$ckm_stage==4,]
data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==2,]
data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==0 | beverage_20w_data$ckm_stage==1,]


survial_event <- Surv(time = data_subset$follow_to_death_time,
                      event = data_subset$death)

#coffee-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +tdi+pack_years_smoking+chesse_intake+	
                     frequency_unenthusiasm+nap_frequency+frequency_tiredness+	
                     ease_skin_tanning+household_income+	
                     live_with_partner+shorter_than10+sleep_duration_group+	
                     plumper_than10+education_years+
                     smoking_status,
                   data = data_subset)
summary(cox_model)


#tea-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +tdi+pack_years_smoking+chesse_intake+	
                     frequency_tiredness+	
                     ease_skin_tanning+household_income+	
                     live_with_partner+physical_activity_group+
                     employed+living_flat_vs_house+financial_diffculty+sleep_duration_group+	
                     plumper_than10+education_years+use_gym+
                     smoking_status,
                   data = data_subset)
summary(cox_model)


#low-fat milk-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +tdi+pack_years_smoking+chesse_intake+fed_up_feeling+	
                     frequency_unenthusiasm+nap_frequency+	
                     frequency_tiredness+ease_skin_tanning+
                     household_income+live_with_partner+physical_activity_group+
                     shorter_than10+living_flat_vs_house+financial_diffculty+	
                     sleep_duration_group+
                     plumper_than10+education_years+
                     smoking_status,
                   data = data_subset)
summary(cox_model)



#full-fat milk-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +tdi+pack_years_smoking+chesse_intake+fed_up_feeling+	
                     frequency_unenthusiasm+ease_skin_tanning+
                     household_income+live_with_partner+physical_activity_group+
                     living_flat_vs_house+financial_diffculty+	
                     plumper_than10+renting_from_council_vs_own+use_open_fire+
                     use_gym+education_years+
                     smoking_status,
                   data = data_subset)
summary(cox_model)



#SSB-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +pack_years_smoking+chesse_intake+
                     frequency_unenthusiasm+nap_frequency+frequency_tiredness+
                     household_income+live_with_partner+	
                     employed+living_flat_vs_house+
                     financial_diffculty+sleep_duration_group+
                     renting_from_council_vs_own+education_years+smoking_status,
                   data = data_subset)
summary(cox_model)



#ASB-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +fed_up_feeling+pack_years_smoking+chesse_intake+
                     nap_frequency+frequency_tiredness+	
                     ease_skin_tanning+
                     household_income+	
                     physical_activity_group+		
                     shorter_than10+employed+
                     employed+living_flat_vs_house+
                     plumper_than10+sleep_duration_group+
                     renting_from_council_vs_own+education_years+smoking_status,
                   data = data_subset)
summary(cox_model)



#NJ-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +fed_up_feeling+pack_years_smoking+chesse_intake+
                     frequency_unenthusiasm+frequency_tiredness+
                     ease_skin_tanning+household_income+	
                     live_with_partner+physical_activity_group+
                     employed+living_flat_vs_house+financial_diffculty+
                     sleep_duration_group+	
                     plumper_than10+use_gym+
                     education_years+smoking_status,
                   data = data_subset)
summary(cox_model)



#water-mortality association
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +tdi+fed_up_feeling+pack_years_smoking+chesse_intake+
                     frequency_unenthusiasm+frequency_tiredness+
                     ease_skin_tanning+household_income+	
                     live_with_partner+physical_activity_group+employed+
                     shorter_than10+living_flat_vs_house+use_gym+
                     education_years+smoking_status,
                   data = data_subset)
summary(cox_model)






# Install and load the forestplot package if not already installed
if (!require(forestplot)) {
  install.packages("forestplot")
  library(forestplot)
}

# Reordering beverages as requested
beverages <- c("Coffee", "Tea", "Low-fat milk", "Full-fat milk", "SSBs", "ASBs", "Fruit juice","Plain water")


# Hazard ratios for cat2 from the newest Cox model output (reordered to match)
hazard_ratios <- c(
  0.8865,      # coffee_intake_cat2
  0.8275,      # tea_intake_cat2
  1.0625,      # lowfat_milk_intake_cat2
  1.1147,      # fullfat_milk_intake_cat2
  1.1106,      # SSB_intake_cat2
  1.2106,      # ASB_intake_cat2
  0.8745,      # NJ_intake_cat2 (Natural/Fruit Juice)
  1.1534       # plain_water_intake_cat2
)

# Confidence intervals (reordered to match)
lower_ci <- c(
  0.7866,      # coffee_intake_cat2
  0.7280,      # tea_intake_cat2
  0.9357,      # lowfat_milk_intake_cat2
  0.9437,      # fullfat_milk_intake_cat2
  0.9868,      # SSB_intake_cat2
  1.0678,      # ASB_intake_cat2
  0.7710,      # NJ_intake_cat2
  1.0179       # plain_water_intake_cat2
)

upper_ci <- c(
  0.9991,      # coffee_intake_cat2
  0.9407,      # tea_intake_cat2
  1.2065,      # lowfat_milk_intake_cat2
  1.3167,      # fullfat_milk_intake_cat2
  1.2500,      # SSB_intake_cat2
  1.3725,      # ASB_intake_cat2
  0.9920,      # NJ_intake_cat2
  1.3069       # plain_water_intake_cat2
)


# Format the HR and CI as text
hr_text <- sprintf("%.2f (%.2f to %.2f)", hazard_ratios, lower_ci, upper_ci)

# Create a matrix for labeltext
tabletext <- cbind(
  beverages,
  hr_text
)

# Create the forestplot
forestplot(
  labeltext = tabletext,
  mean = hazard_ratios,
  lower = lower_ci,
  upper = upper_ci,
  xlog = FALSE,
  boxsize = 0.2,
  lineheight = unit(8, "mm"),
  col = fpColors(box = "#0487D9", line = "#0487D9"),
  title = "All cause mortality",
  zero = 1,
  clip = c(0.5, 1.5),
  xticks = c(0.5, 1.0, 1.5),
  is.summary = c(rep(FALSE, 8)),
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.9),
    ticks = gpar(cex = 0.9),
    xlab = gpar(cex = 0.9)
  ),
  xlab = "Hazard ratio",
  hrzl_lines = list("1" = gpar(lty = 1, col = "#444444")),
  graph.pos = 2,  # Position of the graph (after the second column)
  colgap = unit(5, "mm"),
  col.spacing = unit(10, "mm"),
  new_page = TRUE
)




########################################specific death##############################################
beverage_20w_data$death_specific[beverage_20w_data$cvd_death==1] <- 1
beverage_20w_data$death_specific[beverage_20w_data$cancer_death==1] <- 2
beverage_20w_data$death_specific[beverage_20w_data$cvd_death!=1 & beverage_20w_data$cancer_death!=1 
                                 & beverage_20w_data$death==1] <- 3
beverage_20w_data$death_specific[is.na(beverage_20w_data$death_specific)] <- 0
table(beverage_20w_data$death_specific)


# 加载必要的包
library(survival)
library(cmprsk)
library(riskRegression)
library(prodlim)
# 首先确认您的死亡类别编码:
# 0 = 存活
# 1 = 心血管疾病死亡
# 2 = 癌症死亡
# 3 = 其他原因死亡

# 准备竞争风险分析数据(不做，模型拟合时间过长！！！)
# 1. 创建事件变量，使用心血管疾病死亡作为事件1，其他死亡作为竞争风险
# 假设我们要分析CVD死亡风险，将cancer和其他原因死亡作为竞争风险

# 竞争风险变量: 0=存活, 1=CVD死亡, 2=竞争事件(癌症或其他原因死亡)
beverage_20w_data$cvd_cr <- 0
beverage_20w_data$cvd_cr[beverage_20w_data$death_specific == 1] <- 1  # CVD死亡

# 癌症死亡风险分析: 0=存活, 1=癌症死亡, 2=竞争事件(CVD或其他原因死亡)
beverage_20w_data$cancer_cr <- 0
beverage_20w_data$cancer_cr[beverage_20w_data$death_specific == 2] <- 1  # 癌症死亡


data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==3 | beverage_20w_data$ckm_stage==4,]
data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==0,]

survial_event <- Surv(time = data_subset$follow_to_death_time,
                      event = data_subset$cvd_cr)

cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex,
                   data = data_subset)
summary(cox_model)


cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + race + education + household_income+
                     bmi + diet_score + smk_num + smk_qyr  +  PA_mod_vig_150 + alcohol_intake_frequency+
                     overall_health_rating,
                   data = data_subset)
summary(cox_model)





###############################statistics for details#################################################
# Create survival object
survial_event <- Surv(time = data_subset$follow_to_death_time, event = data_subset$death)

# Ensure all categorical beverage variables are factors with proper reference levels
beverage_cat_vars <- c("coffee_intake_cat", "tea_intake_cat", "plain_water_intake_cat", 
                       "SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", 
                       "lowfat_milk_intake_cat", "fullfat_milk_intake_cat")

for (var in beverage_cat_vars) {
  data_subset[[var]] <- factor(data_subset[[var]], levels = c("0", "1", "2"))
}

# Calculate deaths and person-years for each continuous variable (not categorical)
# This maps between categorical and continuous variable names
continuous_vars_map <- list(
  coffee_intake_cat = "coffee_intake",
  tea_intake_cat = "tea_intake",
  plain_water_intake_cat = "plain_water_intake",
  SSB_intake_cat = "SSB_intake",
  ASB_intake_cat = "ASB_intake",
  NJ_intake_cat = "NJ_intake",
  lowfat_milk_intake_cat = "lowfat_milk_intake",
  fullfat_milk_intake_cat = "fullfat_milk_intake"
)

# Calculate deaths and person-years
calculate_deaths_person_years <- function(data, cat_var, cont_var) {
  result <- list()
  
  # Get levels from categorical variable
  cat_levels <- levels(data[[cat_var]])
  
  for (level in cat_levels) {
    # For level 0
    if (level == "0") {
      # Select rows where the continuous variable is 0
      subset_data <- data[data[[cont_var]] == 0, ]
    }
    # For level 1 (medium consumption)
    else if (level == "1") {
      # Logic depends on the specific beverage
      if (cat_var %in% c("coffee_intake_cat", "plain_water_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 3, ]
      } else if (cat_var == "tea_intake_cat") {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 4, ]
      } else if (cat_var %in% c("SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", "fullfat_milk_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 1, ]
      } else if (cat_var == "lowfat_milk_intake_cat") {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 5, ]
      }
    }
    # For level 2 (high consumption)
    else if (level == "2") {
      # Logic depends on the specific beverage
      if (cat_var %in% c("coffee_intake_cat", "plain_water_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 3, ]
      } else if (cat_var == "tea_intake_cat") {
        subset_data <- data[data[[cont_var]] > 4, ]
      } else if (cat_var %in% c("SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", "fullfat_milk_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 1, ]
      } else if (cat_var == "lowfat_milk_intake_cat") {
        subset_data <- data[data[[cont_var]] > 5, ]
      }
    }
    
    # Calculate deaths and person-years
    deaths <- sum(subset_data$death, na.rm = TRUE)
    person_years <- sum(subset_data$follow_to_death_time, na.rm = TRUE)
    
    # Round person-years to nearest integer
    person_years_rounded <- round(person_years)
    
    result[[level]] <- list(
      deaths = deaths,
      person_years = person_years_rounded
    )
  }
  
  return(result)
}

# Calculate for each beverage type
deaths_person_years <- list()
for (cat_var in beverage_cat_vars) {
  cont_var <- continuous_vars_map[[cat_var]]
  deaths_person_years[[cat_var]] <- calculate_deaths_person_years(data_subset, cat_var, cont_var)
}

# Age-sex adjusted model with all beverages
age_sex_model <- coxph(
  survial_event ~ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + 
    ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + 
    fullfat_milk_intake_cat + plain_water_intake_cat +
    age + sex,
  data = data_subset
)

# Multivariable model with all beverages
multi_model <- coxph(
  survial_event ~ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + 
    ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + 
    fullfat_milk_intake_cat + plain_water_intake_cat +
    age + sex + race + education + household_income +
    bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + 
    alcohol_intake_frequency + overall_health_rating,
  data = data_subset
)

# Get model summaries
age_sex_summary <- summary(age_sex_model)
multi_summary <- summary(multi_model)

# Extract hazard ratios and confidence intervals
extract_hr_ci <- function(model_summary, cat_var_names) {
  coef_table <- model_summary$coefficients
  conf_table <- model_summary$conf.int
  
  results <- list()
  
  for (var in cat_var_names) {
    # Find indices for this variable
    var_indices <- grep(paste0("^", var), rownames(coef_table))
    
    if (length(var_indices) > 0) {
      # Extract HRs and CIs
      results[[var]] <- list(
        hr = conf_table[var_indices, 1],
        lower_ci = conf_table[var_indices, 3],
        upper_ci = conf_table[var_indices, 4]
      )
    }
  }
  
  return(results)
}

age_sex_results <- extract_hr_ci(age_sex_summary, beverage_cat_vars)
multi_results <- extract_hr_ci(multi_summary, beverage_cat_vars)

# For P-trend: Run models with continuous variables
continuous_vars <- unlist(continuous_vars_map)

p_trend_age_sex <- list()
p_trend_multi <- list()
hr_continuous_age_sex <- list()
hr_continuous_multi <- list()

for (var in continuous_vars) {
  # Create formula just for this continuous variable
  formula_age_sex <- as.formula(paste(
    "survial_event ~", var, "+ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + fullfat_milk_intake_cat + plain_water_intake_cat - ", 
    paste0(gsub("intake$", "intake_cat", var)), "+ age + sex"
  ))
  
  model_age_sex <- coxph(formula_age_sex, data = data_subset)
  p_trend_age_sex[[var]] <- summary(model_age_sex)$coefficients[var, 5]
  
  # Extract HR and CI for continuous variable (per 1 cup/day)
  hr_continuous_age_sex[[var]] <- list(
    hr = exp(coef(model_age_sex)[var]),
    lower_ci = exp(confint(model_age_sex)[var, 1]),
    upper_ci = exp(confint(model_age_sex)[var, 2])
  )
  
  # Multivariable model for p-trend
  formula_multi <- as.formula(paste(
    "survial_event ~", var, "+ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + fullfat_milk_intake_cat + plain_water_intake_cat - ", 
    paste0(gsub("intake$", "intake_cat", var)), 
    "+ age + sex + race + education + household_income + bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + alcohol_intake_frequency + overall_health_rating"
  ))
  
  model_multi <- coxph(formula_multi, data = data_subset)
  p_trend_multi[[var]] <- summary(model_multi)$coefficients[var, 5]
  
  # Extract HR and CI for continuous variable (per 1 cup/day)
  hr_continuous_multi[[var]] <- list(
    hr = exp(coef(model_multi)[var]),
    lower_ci = exp(confint(model_multi)[var, 1]),
    upper_ci = exp(confint(model_multi)[var, 2])
  )
}

# Create a table to display the results
consumption_levels <- list(
  coffee_intake_cat = c("0", "0~<=3", ">3"),
  tea_intake_cat = c("0", "0~<=4", ">4"),
  plain_water_intake_cat = c("0", "0~<=3", ">3"),
  SSB_intake_cat = c("0", "0~<=1", ">1"),
  ASB_intake_cat = c("0", "0~<=1", ">1"),
  NJ_intake_cat = c("0", "0~<=1", ">1"),
  lowfat_milk_intake_cat = c("0", "0~<=5", ">5"),
  fullfat_milk_intake_cat = c("0", "0~<=1", ">1")
)

# Create table function
create_result_table <- function(age_sex_results, multi_results, p_trend_age_sex, p_trend_multi, 
                                hr_continuous_age_sex, hr_continuous_multi, 
                                consumption_levels, deaths_person_years) {
  result_table <- data.frame(
    Beverage = character(),
    Consumption = character(),
    Deaths_PersonYears = character(),
    Age_Sex_HR = character(),
    Multi_HR = character(),
    Every1Cup_HR = character(),
    P_trend = character(),
    stringsAsFactors = FALSE
  )
  
  for (var in names(consumption_levels)) {
    base_var <- continuous_vars_map[[var]]
    display_var <- gsub("_intake_cat$", "", var)
    
    # Get deaths/person-years data for this beverage
    dp_data <- deaths_person_years[[var]]
    
    # Reference level (0)
    ref_row <- data.frame(
      Beverage = display_var,
      Consumption = consumption_levels[[var]][1],
      Deaths_PersonYears = sprintf("%d/%d", 
                                   dp_data[["0"]]$deaths, 
                                   dp_data[["0"]]$person_years),
      Age_Sex_HR = "1",
      Multi_HR = "1",
      Every1Cup_HR = sprintf("%.2f (%.2f-%.2f)", 
                             hr_continuous_age_sex[[base_var]]$hr,
                             hr_continuous_age_sex[[base_var]]$lower_ci,
                             hr_continuous_age_sex[[base_var]]$upper_ci),
      P_trend = sprintf("%.3f", p_trend_age_sex[[base_var]]),
      stringsAsFactors = FALSE
    )
    
    # Level 1
    if (length(age_sex_results[[var]]$hr) >= 1) {
      level1_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][2],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["1"]]$deaths, 
                                     dp_data[["1"]]$person_years),
        Age_Sex_HR = sprintf("%.2f (%.2f-%.2f)", 
                             age_sex_results[[var]]$hr[1], 
                             age_sex_results[[var]]$lower_ci[1], 
                             age_sex_results[[var]]$upper_ci[1]),
        Multi_HR = sprintf("%.2f (%.2f-%.2f)", 
                           multi_results[[var]]$hr[1], 
                           multi_results[[var]]$lower_ci[1], 
                           multi_results[[var]]$upper_ci[1]),
        Every1Cup_HR = sprintf("%.2f (%.2f-%.2f)", 
                               hr_continuous_multi[[base_var]]$hr,
                               hr_continuous_multi[[base_var]]$lower_ci,
                               hr_continuous_multi[[base_var]]$upper_ci),
        P_trend = sprintf("%.3f", p_trend_multi[[base_var]]),
        stringsAsFactors = FALSE
      )
    } else {
      level1_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][2],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["1"]]$deaths, 
                                     dp_data[["1"]]$person_years),
        Age_Sex_HR = "NA",
        Multi_HR = "NA",
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    }
    
    # Level 2
    if (length(age_sex_results[[var]]$hr) >= 2) {
      level2_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][3],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["2"]]$deaths, 
                                     dp_data[["2"]]$person_years),
        Age_Sex_HR = sprintf("%.2f (%.2f-%.2f)", 
                             age_sex_results[[var]]$hr[2], 
                             age_sex_results[[var]]$lower_ci[2], 
                             age_sex_results[[var]]$upper_ci[2]),
        Multi_HR = sprintf("%.2f (%.2f-%.2f)", 
                           multi_results[[var]]$hr[2], 
                           multi_results[[var]]$lower_ci[2], 
                           multi_results[[var]]$upper_ci[2]),
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    } else {
      level2_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][3],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["2"]]$deaths, 
                                     dp_data[["2"]]$person_years),
        Age_Sex_HR = "NA",
        Multi_HR = "NA",
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    }
    
    # Add rows to the table
    result_table <- rbind(result_table, ref_row, level1_row, level2_row)
  }
  
  return(result_table)
}

# Generate and display the results table
results_table <- create_result_table(age_sex_results, multi_results, 
                                     p_trend_age_sex, p_trend_multi,
                                     hr_continuous_age_sex, hr_continuous_multi,
                                     consumption_levels, deaths_person_years)
print(results_table)

# If knitr is available, display with better formatting
if (require(knitr)) {
  kable(results_table, 
        col.names = c("Beverage", "Consumption Level", "No of deaths/person years", 
                      "HR (95% CI) Age-Sex Adjusted", 
                      "HR (95% CI) Multivariable",
                      "Every 1 cup/day",
                      "P trend"),
        align = c("l", "l", "c", "c", "c", "c", "c"))
}


##################################################################################################################
# Create survival object for CVD deaths
survial_event <- Surv(time = data_subset$follow_to_death_time, event = data_subset$cvd_cr == 1)

# Ensure all categorical beverage variables are factors with proper reference levels
beverage_cat_vars <- c("coffee_intake_cat", "tea_intake_cat", "plain_water_intake_cat", 
                       "SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", 
                       "lowfat_milk_intake_cat", "fullfat_milk_intake_cat")

for (var in beverage_cat_vars) {
  data_subset[[var]] <- factor(data_subset[[var]], levels = c("0", "1", "2"))
}

# Calculate deaths and person-years for each continuous variable (not categorical)
# This maps between categorical and continuous variable names
continuous_vars_map <- list(
  coffee_intake_cat = "coffee_intake",
  tea_intake_cat = "tea_intake",
  plain_water_intake_cat = "plain_water_intake",
  SSB_intake_cat = "SSB_intake",
  ASB_intake_cat = "ASB_intake",
  NJ_intake_cat = "NJ_intake",
  lowfat_milk_intake_cat = "lowfat_milk_intake",
  fullfat_milk_intake_cat = "fullfat_milk_intake"
)

# Calculate CVD deaths and person-years
calculate_deaths_person_years <- function(data, cat_var, cont_var) {
  result <- list()
  
  # Get levels from categorical variable
  cat_levels <- levels(data[[cat_var]])
  
  for (level in cat_levels) {
    # For level 0
    if (level == "0") {
      # Select rows where the continuous variable is 0
      subset_data <- data[data[[cont_var]] == 0, ]
    }
    # For level 1 (medium consumption)
    else if (level == "1") {
      # Logic depends on the specific beverage
      if (cat_var %in% c("coffee_intake_cat", "plain_water_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 3, ]
      } else if (cat_var == "tea_intake_cat") {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 4, ]
      } else if (cat_var %in% c("SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", "fullfat_milk_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 1, ]
      } else if (cat_var == "lowfat_milk_intake_cat") {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 5, ]
      }
    }
    # For level 2 (high consumption)
    else if (level == "2") {
      # Logic depends on the specific beverage
      if (cat_var %in% c("coffee_intake_cat", "plain_water_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 3, ]
      } else if (cat_var == "tea_intake_cat") {
        subset_data <- data[data[[cont_var]] > 4, ]
      } else if (cat_var %in% c("SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", "fullfat_milk_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 1, ]
      } else if (cat_var == "lowfat_milk_intake_cat") {
        subset_data <- data[data[[cont_var]] > 5, ]
      }
    }
    
    # Calculate CVD deaths and person-years
    # 使用cvd_cr == 1来计算CVD死亡
    deaths <- sum(subset_data$cvd_cr == 1, na.rm = TRUE)
    person_years <- sum(subset_data$follow_to_death_time, na.rm = TRUE)
    
    # Round person-years to nearest integer
    person_years_rounded <- round(person_years)
    
    result[[level]] <- list(
      deaths = deaths,
      person_years = person_years_rounded
    )
  }
  
  return(result)
}

# Calculate for each beverage type
deaths_person_years <- list()
for (cat_var in beverage_cat_vars) {
  cont_var <- continuous_vars_map[[cat_var]]
  deaths_person_years[[cat_var]] <- calculate_deaths_person_years(data_subset, cat_var, cont_var)
}

# Age-sex adjusted model with all beverages
age_sex_model <- coxph(
  survial_event ~ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + 
    ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + 
    fullfat_milk_intake_cat + plain_water_intake_cat +
    age + sex,
  data = data_subset
)

# Multivariable model with all beverages
multi_model <- coxph(
  survial_event ~ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + 
    ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + 
    fullfat_milk_intake_cat + plain_water_intake_cat +
    age + sex + race + education + household_income +
    bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + 
    alcohol_intake_frequency + overall_health_rating,
  data = data_subset
)

# Get model summaries
age_sex_summary <- summary(age_sex_model)
multi_summary <- summary(multi_model)

# Extract hazard ratios and confidence intervals
extract_hr_ci <- function(model_summary, cat_var_names) {
  coef_table <- model_summary$coefficients
  conf_table <- model_summary$conf.int
  
  results <- list()
  
  for (var in cat_var_names) {
    # Find indices for this variable
    var_indices <- grep(paste0("^", var), rownames(coef_table))
    
    if (length(var_indices) > 0) {
      # Extract HRs and CIs
      results[[var]] <- list(
        hr = conf_table[var_indices, 1],
        lower_ci = conf_table[var_indices, 3],
        upper_ci = conf_table[var_indices, 4]
      )
    }
  }
  
  return(results)
}

age_sex_results <- extract_hr_ci(age_sex_summary, beverage_cat_vars)
multi_results <- extract_hr_ci(multi_summary, beverage_cat_vars)

# For P-trend: Run models with continuous variables
continuous_vars <- unlist(continuous_vars_map)

p_trend_age_sex <- list()
p_trend_multi <- list()
hr_continuous_age_sex <- list()
hr_continuous_multi <- list()

for (var in continuous_vars) {
  # Create formula just for this continuous variable
  formula_age_sex <- as.formula(paste(
    "survial_event ~", var, "+ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + fullfat_milk_intake_cat + plain_water_intake_cat - ", 
    paste0(gsub("intake$", "intake_cat", var)), "+ age + sex"
  ))
  
  model_age_sex <- coxph(formula_age_sex, data = data_subset)
  p_trend_age_sex[[var]] <- summary(model_age_sex)$coefficients[var, 5]
  
  # Extract HR and CI for continuous variable (per 1 cup/day)
  hr_continuous_age_sex[[var]] <- list(
    hr = exp(coef(model_age_sex)[var]),
    lower_ci = exp(confint(model_age_sex)[var, 1]),
    upper_ci = exp(confint(model_age_sex)[var, 2])
  )
  
  # Multivariable model for p-trend
  formula_multi <- as.formula(paste(
    "survial_event ~", var, "+ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + fullfat_milk_intake_cat + plain_water_intake_cat - ", 
    paste0(gsub("intake$", "intake_cat", var)), 
    "+ age + sex + race + education + household_income + bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + alcohol_intake_frequency + overall_health_rating"
  ))
  
  model_multi <- coxph(formula_multi, data = data_subset)
  p_trend_multi[[var]] <- summary(model_multi)$coefficients[var, 5]
  
  # Extract HR and CI for continuous variable (per 1 cup/day)
  hr_continuous_multi[[var]] <- list(
    hr = exp(coef(model_multi)[var]),
    lower_ci = exp(confint(model_multi)[var, 1]),
    upper_ci = exp(confint(model_multi)[var, 2])
  )
}

# Create a table to display the results
consumption_levels <- list(
  coffee_intake_cat = c("0", "0~<=3", ">3"),
  tea_intake_cat = c("0", "0~<=4", ">4"),
  plain_water_intake_cat = c("0", "0~<=3", ">3"),
  SSB_intake_cat = c("0", "0~<=1", ">1"),
  ASB_intake_cat = c("0", "0~<=1", ">1"),
  NJ_intake_cat = c("0", "0~<=1", ">1"),
  lowfat_milk_intake_cat = c("0", "0~<=5", ">5"),
  fullfat_milk_intake_cat = c("0", "0~<=1", ">1")
)

# Create table function
create_result_table <- function(age_sex_results, multi_results, p_trend_age_sex, p_trend_multi, 
                                hr_continuous_age_sex, hr_continuous_multi, 
                                consumption_levels, deaths_person_years) {
  result_table <- data.frame(
    Beverage = character(),
    Consumption = character(),
    Deaths_PersonYears = character(),
    Age_Sex_HR = character(),
    Multi_HR = character(),
    Every1Cup_HR = character(),
    P_trend = character(),
    stringsAsFactors = FALSE
  )
  
  for (var in names(consumption_levels)) {
    base_var <- continuous_vars_map[[var]]
    display_var <- gsub("_intake_cat$", "", var)
    
    # Get deaths/person-years data for this beverage
    dp_data <- deaths_person_years[[var]]
    
    # Reference level (0)
    ref_row <- data.frame(
      Beverage = display_var,
      Consumption = consumption_levels[[var]][1],
      Deaths_PersonYears = sprintf("%d/%d", 
                                   dp_data[["0"]]$deaths, 
                                   dp_data[["0"]]$person_years),
      Age_Sex_HR = "1",
      Multi_HR = "1",
      Every1Cup_HR = sprintf("%.2f (%.2f-%.2f)", 
                             hr_continuous_age_sex[[base_var]]$hr,
                             hr_continuous_age_sex[[base_var]]$lower_ci,
                             hr_continuous_age_sex[[base_var]]$upper_ci),
      P_trend = sprintf("%.3f", p_trend_age_sex[[base_var]]),
      stringsAsFactors = FALSE
    )
    
    # Level 1
    if (length(age_sex_results[[var]]$hr) >= 1) {
      level1_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][2],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["1"]]$deaths, 
                                     dp_data[["1"]]$person_years),
        Age_Sex_HR = sprintf("%.2f (%.2f-%.2f)", 
                             age_sex_results[[var]]$hr[1], 
                             age_sex_results[[var]]$lower_ci[1], 
                             age_sex_results[[var]]$upper_ci[1]),
        Multi_HR = sprintf("%.2f (%.2f-%.2f)", 
                           multi_results[[var]]$hr[1], 
                           multi_results[[var]]$lower_ci[1], 
                           multi_results[[var]]$upper_ci[1]),
        Every1Cup_HR = sprintf("%.2f (%.2f-%.2f)", 
                               hr_continuous_multi[[base_var]]$hr,
                               hr_continuous_multi[[base_var]]$lower_ci,
                               hr_continuous_multi[[base_var]]$upper_ci),
        P_trend = sprintf("%.3f", p_trend_multi[[base_var]]),
        stringsAsFactors = FALSE
      )
    } else {
      level1_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][2],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["1"]]$deaths, 
                                     dp_data[["1"]]$person_years),
        Age_Sex_HR = "NA",
        Multi_HR = "NA",
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    }
    
    # Level 2
    if (length(age_sex_results[[var]]$hr) >= 2) {
      level2_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][3],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["2"]]$deaths, 
                                     dp_data[["2"]]$person_years),
        Age_Sex_HR = sprintf("%.2f (%.2f-%.2f)", 
                             age_sex_results[[var]]$hr[2], 
                             age_sex_results[[var]]$lower_ci[2], 
                             age_sex_results[[var]]$upper_ci[2]),
        Multi_HR = sprintf("%.2f (%.2f-%.2f)", 
                           multi_results[[var]]$hr[2], 
                           multi_results[[var]]$lower_ci[2], 
                           multi_results[[var]]$upper_ci[2]),
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    } else {
      level2_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][3],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["2"]]$deaths, 
                                     dp_data[["2"]]$person_years),
        Age_Sex_HR = "NA",
        Multi_HR = "NA",
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    }
    
    # Add rows to the table
    result_table <- rbind(result_table, ref_row, level1_row, level2_row)
  }
  
  return(result_table)
}

# Generate and display the results table
results_table <- create_result_table(age_sex_results, multi_results, 
                                     p_trend_age_sex, p_trend_multi,
                                     hr_continuous_age_sex, hr_continuous_multi,
                                     consumption_levels, deaths_person_years)
print(results_table)

# If knitr is available, display with better formatting
if (require(knitr)) {
  kable(results_table, 
        col.names = c("Beverage", "Consumption Level", "No of deaths(CVD)/person years", 
                      "HR (95% CI) Age-Sex Adjusted", 
                      "HR (95% CI) Multivariable",
                      "Every 1 cup/day",
                      "P trend"),
        align = c("l", "l", "c", "c", "c", "c", "c"))
}


############################################################################################################
# Create survival object for Cancer deaths
survial_event <- Surv(time = data_subset$follow_to_death_time, event = data_subset$cancer_cr == 1)

# Ensure all categorical beverage variables are factors with proper reference levels
beverage_cat_vars <- c("coffee_intake_cat", "tea_intake_cat", "plain_water_intake_cat", 
                       "SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", 
                       "lowfat_milk_intake_cat", "fullfat_milk_intake_cat")

for (var in beverage_cat_vars) {
  data_subset[[var]] <- factor(data_subset[[var]], levels = c("0", "1", "2"))
}

# Calculate deaths and person-years for each continuous variable (not categorical)
# This maps between categorical and continuous variable names
continuous_vars_map <- list(
  coffee_intake_cat = "coffee_intake",
  tea_intake_cat = "tea_intake",
  plain_water_intake_cat = "plain_water_intake",
  SSB_intake_cat = "SSB_intake",
  ASB_intake_cat = "ASB_intake",
  NJ_intake_cat = "NJ_intake",
  lowfat_milk_intake_cat = "lowfat_milk_intake",
  fullfat_milk_intake_cat = "fullfat_milk_intake"
)

# Calculate Cancer deaths and person-years
calculate_deaths_person_years <- function(data, cat_var, cont_var) {
  result <- list()
  
  # Get levels from categorical variable
  cat_levels <- levels(data[[cat_var]])
  
  for (level in cat_levels) {
    # For level 0
    if (level == "0") {
      # Select rows where the continuous variable is 0
      subset_data <- data[data[[cont_var]] == 0, ]
    }
    # For level 1 (medium consumption)
    else if (level == "1") {
      # Logic depends on the specific beverage
      if (cat_var %in% c("coffee_intake_cat", "plain_water_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 3, ]
      } else if (cat_var == "tea_intake_cat") {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 4, ]
      } else if (cat_var %in% c("SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", "fullfat_milk_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 1, ]
      } else if (cat_var == "lowfat_milk_intake_cat") {
        subset_data <- data[data[[cont_var]] > 0 & data[[cont_var]] <= 5, ]
      }
    }
    # For level 2 (high consumption)
    else if (level == "2") {
      # Logic depends on the specific beverage
      if (cat_var %in% c("coffee_intake_cat", "plain_water_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 3, ]
      } else if (cat_var == "tea_intake_cat") {
        subset_data <- data[data[[cont_var]] > 4, ]
      } else if (cat_var %in% c("SSB_intake_cat", "ASB_intake_cat", "NJ_intake_cat", "fullfat_milk_intake_cat")) {
        subset_data <- data[data[[cont_var]] > 1, ]
      } else if (cat_var == "lowfat_milk_intake_cat") {
        subset_data <- data[data[[cont_var]] > 5, ]
      }
    }
    
    # Calculate Cancer deaths and person-years
    # 使用cancer_cr == 1来计算癌症死亡
    deaths <- sum(subset_data$cancer_cr == 1, na.rm = TRUE)
    person_years <- sum(subset_data$follow_to_death_time, na.rm = TRUE)
    
    # Round person-years to nearest integer
    person_years_rounded <- round(person_years)
    
    result[[level]] <- list(
      deaths = deaths,
      person_years = person_years_rounded
    )
  }
  
  return(result)
}

# Calculate for each beverage type
deaths_person_years <- list()
for (cat_var in beverage_cat_vars) {
  cont_var <- continuous_vars_map[[cat_var]]
  deaths_person_years[[cat_var]] <- calculate_deaths_person_years(data_subset, cat_var, cont_var)
}

# Age-sex adjusted model with all beverages
age_sex_model <- coxph(
  survial_event ~ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + 
    ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + 
    fullfat_milk_intake_cat + plain_water_intake_cat +
    age + sex,
  data = data_subset
)

# Multivariable model with all beverages
multi_model <- coxph(
  survial_event ~ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + 
    ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + 
    fullfat_milk_intake_cat + plain_water_intake_cat +
    age + sex + race + education + household_income +
    bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + 
    alcohol_intake_frequency + overall_health_rating,
  data = data_subset
)

# Get model summaries
age_sex_summary <- summary(age_sex_model)
multi_summary <- summary(multi_model)

# Extract hazard ratios and confidence intervals
extract_hr_ci <- function(model_summary, cat_var_names) {
  coef_table <- model_summary$coefficients
  conf_table <- model_summary$conf.int
  
  results <- list()
  
  for (var in cat_var_names) {
    # Find indices for this variable
    var_indices <- grep(paste0("^", var), rownames(coef_table))
    
    if (length(var_indices) > 0) {
      # Extract HRs and CIs
      results[[var]] <- list(
        hr = conf_table[var_indices, 1],
        lower_ci = conf_table[var_indices, 3],
        upper_ci = conf_table[var_indices, 4]
      )
    }
  }
  
  return(results)
}

age_sex_results <- extract_hr_ci(age_sex_summary, beverage_cat_vars)
multi_results <- extract_hr_ci(multi_summary, beverage_cat_vars)

# For P-trend: Run models with continuous variables
continuous_vars <- unlist(continuous_vars_map)

p_trend_age_sex <- list()
p_trend_multi <- list()
hr_continuous_age_sex <- list()
hr_continuous_multi <- list()

for (var in continuous_vars) {
  # Create formula just for this continuous variable
  formula_age_sex <- as.formula(paste(
    "survial_event ~", var, "+ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + fullfat_milk_intake_cat + plain_water_intake_cat - ", 
    paste0(gsub("intake$", "intake_cat", var)), "+ age + sex"
  ))
  
  model_age_sex <- coxph(formula_age_sex, data = data_subset)
  p_trend_age_sex[[var]] <- summary(model_age_sex)$coefficients[var, 5]
  
  # Extract HR and CI for continuous variable (per 1 cup/day)
  hr_continuous_age_sex[[var]] <- list(
    hr = exp(coef(model_age_sex)[var]),
    lower_ci = exp(confint(model_age_sex)[var, 1]),
    upper_ci = exp(confint(model_age_sex)[var, 2])
  )
  
  # Multivariable model for p-trend
  formula_multi <- as.formula(paste(
    "survial_event ~", var, "+ coffee_intake_cat + tea_intake_cat + SSB_intake_cat + ASB_intake_cat + NJ_intake_cat + lowfat_milk_intake_cat + fullfat_milk_intake_cat + plain_water_intake_cat - ", 
    paste0(gsub("intake$", "intake_cat", var)), 
    "+ age + sex + race + education + household_income + bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + alcohol_intake_frequency + overall_health_rating"
  ))
  
  model_multi <- coxph(formula_multi, data = data_subset)
  p_trend_multi[[var]] <- summary(model_multi)$coefficients[var, 5]
  
  # Extract HR and CI for continuous variable (per 1 cup/day)
  hr_continuous_multi[[var]] <- list(
    hr = exp(coef(model_multi)[var]),
    lower_ci = exp(confint(model_multi)[var, 1]),
    upper_ci = exp(confint(model_multi)[var, 2])
  )
}

# Create a table to display the results
consumption_levels <- list(
  coffee_intake_cat = c("0", "0~<=3", ">3"),
  tea_intake_cat = c("0", "0~<=4", ">4"),
  plain_water_intake_cat = c("0", "0~<=3", ">3"),
  SSB_intake_cat = c("0", "0~<=1", ">1"),
  ASB_intake_cat = c("0", "0~<=1", ">1"),
  NJ_intake_cat = c("0", "0~<=1", ">1"),
  lowfat_milk_intake_cat = c("0", "0~<=5", ">5"),
  fullfat_milk_intake_cat = c("0", "0~<=1", ">1")
)

# Create table function
create_result_table <- function(age_sex_results, multi_results, p_trend_age_sex, p_trend_multi, 
                                hr_continuous_age_sex, hr_continuous_multi, 
                                consumption_levels, deaths_person_years) {
  result_table <- data.frame(
    Beverage = character(),
    Consumption = character(),
    Deaths_PersonYears = character(),
    Age_Sex_HR = character(),
    Multi_HR = character(),
    Every1Cup_HR = character(),
    P_trend = character(),
    stringsAsFactors = FALSE
  )
  
  for (var in names(consumption_levels)) {
    base_var <- continuous_vars_map[[var]]
    display_var <- gsub("_intake_cat$", "", var)
    
    # Get deaths/person-years data for this beverage
    dp_data <- deaths_person_years[[var]]
    
    # Reference level (0)
    ref_row <- data.frame(
      Beverage = display_var,
      Consumption = consumption_levels[[var]][1],
      Deaths_PersonYears = sprintf("%d/%d", 
                                   dp_data[["0"]]$deaths, 
                                   dp_data[["0"]]$person_years),
      Age_Sex_HR = "1",
      Multi_HR = "1",
      Every1Cup_HR = sprintf("%.2f (%.2f-%.2f)", 
                             hr_continuous_age_sex[[base_var]]$hr,
                             hr_continuous_age_sex[[base_var]]$lower_ci,
                             hr_continuous_age_sex[[base_var]]$upper_ci),
      P_trend = sprintf("%.3f", p_trend_age_sex[[base_var]]),
      stringsAsFactors = FALSE
    )
    
    # Level 1
    if (length(age_sex_results[[var]]$hr) >= 1) {
      level1_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][2],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["1"]]$deaths, 
                                     dp_data[["1"]]$person_years),
        Age_Sex_HR = sprintf("%.2f (%.2f-%.2f)", 
                             age_sex_results[[var]]$hr[1], 
                             age_sex_results[[var]]$lower_ci[1], 
                             age_sex_results[[var]]$upper_ci[1]),
        Multi_HR = sprintf("%.2f (%.2f-%.2f)", 
                           multi_results[[var]]$hr[1], 
                           multi_results[[var]]$lower_ci[1], 
                           multi_results[[var]]$upper_ci[1]),
        Every1Cup_HR = sprintf("%.2f (%.2f-%.2f)", 
                               hr_continuous_multi[[base_var]]$hr,
                               hr_continuous_multi[[base_var]]$lower_ci,
                               hr_continuous_multi[[base_var]]$upper_ci),
        P_trend = sprintf("%.3f", p_trend_multi[[base_var]]),
        stringsAsFactors = FALSE
      )
    } else {
      level1_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][2],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["1"]]$deaths, 
                                     dp_data[["1"]]$person_years),
        Age_Sex_HR = "NA",
        Multi_HR = "NA",
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    }
    
    # Level 2
    if (length(age_sex_results[[var]]$hr) >= 2) {
      level2_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][3],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["2"]]$deaths, 
                                     dp_data[["2"]]$person_years),
        Age_Sex_HR = sprintf("%.2f (%.2f-%.2f)", 
                             age_sex_results[[var]]$hr[2], 
                             age_sex_results[[var]]$lower_ci[2], 
                             age_sex_results[[var]]$upper_ci[2]),
        Multi_HR = sprintf("%.2f (%.2f-%.2f)", 
                           multi_results[[var]]$hr[2], 
                           multi_results[[var]]$lower_ci[2], 
                           multi_results[[var]]$upper_ci[2]),
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    } else {
      level2_row <- data.frame(
        Beverage = "",
        Consumption = consumption_levels[[var]][3],
        Deaths_PersonYears = sprintf("%d/%d", 
                                     dp_data[["2"]]$deaths, 
                                     dp_data[["2"]]$person_years),
        Age_Sex_HR = "NA",
        Multi_HR = "NA",
        Every1Cup_HR = "",
        P_trend = "",
        stringsAsFactors = FALSE
      )
    }
    
    # Add rows to the table
    result_table <- rbind(result_table, ref_row, level1_row, level2_row)
  }
  
  return(result_table)
}

# Generate and display the results table
results_table <- create_result_table(age_sex_results, multi_results, 
                                     p_trend_age_sex, p_trend_multi,
                                     hr_continuous_age_sex, hr_continuous_multi,
                                     consumption_levels, deaths_person_years)
print(results_table)

# If knitr is available, display with better formatting
if (require(knitr)) {
  kable(results_table, 
        col.names = c("Beverage", "Consumption Level", "No of deaths(Cancer)/person years", 
                      "HR (95% CI) Age-Sex Adjusted", 
                      "HR (95% CI) Multivariable",
                      "Every 1 cup/day",
                      "P trend"),
        align = c("l", "l", "c", "c", "c", "c", "c"))
}



#############################################exposure-response plot##############################################
names(beverage_20w_data)
ckm_berverage_data_subset <- beverage_20w_data[(beverage_20w_data$ckm_stage==2) &
                                                 !is.na(beverage_20w_data$ckm_stage) ,]
ckm_berverage_data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==3 | beverage_20w_data$ckm_stage==4,]
"#74a892"
"#f0b6ad"
"#dc8864","#DC886419"
"#ba4848","#F6E6E6"
"#642915"


library(plotRCS)
rcsplot(data = ckm_berverage_data_subset[ckm_berverage_data_subset$coffee_intake<=8 & !is.na(ckm_berverage_data_subset$coffee_intake),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "coffee_intake",
        covariates = c("plain_water_intake", "tea_intake", "ASB_intake", "NJ_intake", 
                       "lowfat_milk_intake", "fullfat_milk_intake", "SSB_intake",
                       "age", "sex", "ethnic", "tdi", "pack_years_smoking", 
                       "chesse_intake", "nap_frequency", "frequency_tiredness", "household_income", 
                       "live_with_partner", "shorter_than10", 
                       "sleep_duration_group","plumper_than10",
                       "education_years","smoking_status"),
        ref.value=0,
        knots = knot(3),
        ybreaks= seq(0.7, 1.4, by = 0.1),
        linesize=1,
        linecolor="#ba4848",
)


rcsplot(data = ckm_berverage_data_subset[ckm_berverage_data_subset$tea_intake<=8 & !is.na(ckm_berverage_data_subset$tea_intake),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "tea_intake",
        covariates = c("plain_water_intake", "coffee_intake", "ASB_intake", "NJ_intake", 
                       "lowfat_milk_intake", "fullfat_milk_intake", "SSB_intake",
                       "age", "sex", "ethnic", "tdi", "household_income", 
                       "live_with_partner", "physical_activity_group", "employed", "living_flat_vs_house", "financial_diffculty", 
                       "sleep_duration_group", "plumper_than10", "education_years","use_gym",
                       "smoking_status"
                       ),
        ref.value=0,
        knots = knot(3),
        ybreaks= seq(0.6, 1.4, by = 0.1),
        linesize=1,
        linecolor="#ba4848",
)



rcsplot(data = ckm_berverage_data_subset[ckm_berverage_data_subset$plain_water_intake<=8 & !is.na(ckm_berverage_data_subset$plain_water_intake),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "plain_water_intake",
        covariates = c("coffee_intake", "tea_intake", "ASB_intake", "NJ_intake", 
                       "lowfat_milk_intake", "fullfat_milk_intake", 
                       "age", "sex", "ethnic", "fed_up_feeling", "tdi", 
                       "pack_years_smoking", "ease_skin_tanning", "household_income",
                       "live_with_partner", "physical_activity_group", 
                       "shorter_than10", "living_flat_vs_house", "education_years",
                       "use_gym","smoking_status",
                       "SSB_intake"),
        ref.value=0,
        knots = knot(3),
        ybreaks= seq(0.7, 1.4, by = 0.1),
        linesize=1,
        linecolor="#ba4848",
)        




# Load the splines package
library(splines)
library(rms)
# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "SSB_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(ckm_berverage_data_subset$SSB_intake)
quantile(ckm_berverage_data_subset$SSB_intake,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + ",
                       "pol(SSB_intake, 3) + ASB_intake + NJ_intake + ",
                       "lowfat_milk_intake + fullfat_milk_intake+ ",
                       "age + sex + ethnic +pack_years_smoking+chesse_intake+
                         frequency_unenthusiasm+nap_frequency+frequency_tiredness+
                         household_income+live_with_partner+	
                         employed+living_flat_vs_house+
                         financial_diffculty+sleep_duration_group+
                         renting_from_council_vs_own+education_years+smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$SSB_intake),])

# 预测相对风险
SSB_range <- seq(0, 3, by = 0.1)
pred <- Predict(fit, SSB_intake = SSB_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, SSB_intake)  

# 绘图
ggplot(pred, aes(x = SSB_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(0.7, 1.5, by = 0.1)) +
  labs(x = "SSB intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/5
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 5, height = 5)



# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "ASB_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + ",
                       "SSB_intake + pol(ASB_intake,3) + NJ_intake + ",
                       "lowfat_milk_intake + fullfat_milk_intake+ plain_water_intake +",
                       "age + sex + ethnic + fed_up_feeling + pack_years_smoking + ",
                       "chesse_intake + nap_frequency + frequency_tiredness + physical_activity_group + employed + ",
                       "sleep_duration_group + sleep_duration_group + renting_from_council_vs_own + education_years + smoking_status +",
                       "plumper_than10",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$ASB_intake),])

# 预测相对风险
ASB_range <- seq(0, 3, by = 0.1)
pred <- Predict(fit, ASB_intake = ASB_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, ASB_intake)  

# 绘图
ggplot(pred, aes(x = ASB_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(0.8, 1.5, by = 0.1)) +
  labs(x = "ASB intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/5
  )

ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 5, height = 5)



# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "NJ_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(ckm_berverage_data_subset$NJ_intake)
quantile(ckm_berverage_data_subset$NJ_intake,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + ",
                       "SSB_intake + ASB_intake + pol(NJ_intake,3) + ",
                       "lowfat_milk_intake + fullfat_milk_intake+ ",
                       "age + sex + ethnic + fed_up_feeling + pack_years_smoking + ",
                       "chesse_intake + frequency_tiredness + ease_skin_tanning + household_income + live_with_partner + ",
                       "physical_activity_group + employed + living_flat_vs_house +financial_diffculty +	plumper_than10 + education_years + smoking_status+",
                       "plain_water_intake",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$NJ_intake),])

# 预测相对风险
NJ_range <- seq(0, 3, by = 0.1)
pred <- Predict(fit, NJ_intake = NJ_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, NJ_intake) 

# 绘图
ggplot(pred, aes(x = NJ_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_y_continuous(breaks = seq(0.7, 1.2, by = 0.1)) +
  labs(x = "Fruit juice intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/5
  )

ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 5, height = 5)









# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "lowfat_milk_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + ",
                       "SSB_intake + ASB_intake + NJ_intake + ",
                       "pol(lowfat_milk_intake,3) + fullfat_milk_intake+ ",
                       "age + sex + ethnic + fed_up_feeling + tdi + ",
                       "chesse_intake + frequency_unenthusiasm + frequency_tiredness + ease_skin_tanning + household_income + ",
                       "live_with_partner + shorter_than10 + living_flat_vs_house + financial_diffculty +sleep_duration_group + plumper_than10+ education_years +	smoking_status+",
                       "plain_water_intake",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$lowfat_milk_intake),])

# 预测相对风险
lowfat_milk_range <- seq(0, 6, by = 0.1)
pred <- Predict(fit, lowfat_milk_intake = lowfat_milk_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, lowfat_milk_intake) 

# 绘图
ggplot(pred, aes(x = lowfat_milk_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  scale_y_continuous(breaks = seq(0.7, 1.3, by = 0.1),expand = c(0.2,0)) +
  labs(x = "Low-fat milk intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/5
  )

ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 5, height = 5)






# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "fullfat_milk_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + ",
                       "SSB_intake + ASB_intake + NJ_intake + ",
                       "lowfat_milk_intake + pol(fullfat_milk_intake,3)+ ",
                       "age + sex + tdi + pack_years_smoking + chesse_intake + ",
                       "household_income + live_with_partner + physical_activity_group + plumper_than10 + use_open_fire + ",
                       "use_gym + smoking_status + ",
                       " plain_water_intake",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$fullfat_milk_intake),])

# 预测相对风险
fullfat_milk_range <- seq(0, 4, by = 0.1)
pred <- Predict(fit, fullfat_milk_intake = fullfat_milk_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, fullfat_milk_intake) 

# 绘图
ggplot(pred, aes(x = fullfat_milk_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 4, by = 0.5)) +
  scale_y_continuous(breaks = seq(0.9, 2.0, by = 0.1)) +
  labs(x = "Full-fat milk intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/5
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 5, height = 5)


##############################beverage type associations##########################
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

# 加载必要的包
library(ggplot2)
library(dplyr)
library(tidyr)

# 创建CKM阶段分组
beverage_20w_data$ckm_group <- ifelse(beverage_20w_data$ckm_stage %in% c(0, 1), "CKM 0-1",
                                      ifelse(beverage_20w_data$ckm_stage == 2, "CKM 2",
                                             ifelse(beverage_20w_data$ckm_stage %in% c(3, 4), "CKM 3-4", NA)))

# 计算每种偏好在各CKM阶段的百分比
preference_percentages <- beverage_20w_data %>%
  filter(!is.na(ckm_group)) %>%
  group_by(ckm_group) %>%
  summarise(
    coffee_tea_pct = mean(coffee_tea_preference == 1, na.rm = TRUE) * 100,
    SSB_ASB_pct = mean(SSB_ASB_preference == 1, na.rm = TRUE) * 100,
    NJ_water_pct = mean(NJ_water_preference == 1, na.rm = TRUE) * 100,
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(coffee_tea_pct, SSB_ASB_pct, NJ_water_pct),
               names_to = "preference_type", 
               values_to = "percentage") %>%
  mutate(preference_type = case_when(
    preference_type == "coffee_tea_pct" ~ "Coffee/Tea",
    preference_type == "SSB_ASB_pct" ~ "SSB/ASB", 
    preference_type == "NJ_water_pct" ~ "Natural Juice/Water"
  ))

# 绘制百分比柱状图
ggplot(preference_percentages, aes(x = ckm_group, y = percentage, fill = preference_type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "各种饮料偏好在不同CKM阶段的百分比分布",
       subtitle = "显示偏好值为1所占的百分比",
       x = "CKM阶段", 
       y = "百分比 (%)",
       fill = "饮料偏好类型") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom") +
  scale_fill_manual(values = c("Coffee/Tea" = "#66c2a5", 
                               "Natural Juice/Water" = "#fc8d62", 
                               "SSB/ASB" = "#8da0cb")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))

# 查看具体的百分比数据
preference_summary <- preference_percentages %>%
  pivot_wider(names_from = preference_type, values_from = percentage)

print(preference_summary)

# 也可以查看原始计数和百分比的详细统计
detailed_stats <- beverage_20w_data %>%
  filter(!is.na(ckm_group)) %>%
  group_by(ckm_group) %>%
  summarise(
    total_n = n(),
    coffee_tea_n = sum(coffee_tea_preference == 1, na.rm = TRUE),
    coffee_tea_pct = mean(coffee_tea_preference == 1, na.rm = TRUE) * 100,
    SSB_ASB_n = sum(SSB_ASB_preference == 1, na.rm = TRUE),
    SSB_ASB_pct = mean(SSB_ASB_preference == 1, na.rm = TRUE) * 100,
    NJ_water_n = sum(NJ_water_preference == 1, na.rm = TRUE),
    NJ_water_pct = mean(NJ_water_preference == 1, na.rm = TRUE) * 100,
    .groups = 'drop'
  )

print(detailed_stats)

data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==2,]
data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==3 | beverage_20w_data$ckm_stage==4,]
data_subset <- beverage_20w_data[beverage_20w_data$ckm_stage==0 | beverage_20w_data$ckm_stage==1,]


survial_event <- Surv(time = data_subset$follow_to_death_time,
                      event = data_subset$death)

cox_model <- coxph(survial_event ~coffee_tea_preference +
                     age + sex + ethnic + tdi +pack_years_smoking+chesse_intake+	
                     nap_frequency+	frequency_tiredness+
                     household_income+live_with_partner+
                     shorter_than10+sleep_duration_group+plumper_than10+	
                     education_years+smoking_status+physical_activity_group+
                     employed+living_flat_vs_house+financial_diffculty+
                     use_gym,
                   data = data_subset)
summary(cox_model)


cox_model <- coxph(survial_event ~SSB_ASB_preference +
                     age + sex + ethnic +chesse_intake+	
                     nap_frequency+household_income+employed+
                     education_years+smoking_status+fed_up_feeling+pack_years_smoking+
                     frequency_tiredness+physical_activity_group+sleep_duration_group+
                     plumper_than10+renting_from_council_vs_own,
                   data = data_subset)
summary(cox_model)



cox_model <- coxph(survial_event ~NJ_water_preference +
                     age + sex + ethnic +fed_up_feeling+pack_years_smoking+
                     chesse_intake+frequency_tiredness+ease_skin_tanning+
                     household_income+live_with_partner+physical_activity_group+
                     employed+living_flat_vs_house+	
                     financial_diffculty+plumper_than10+education_years+
                     smoking_status+tdi+	
                     shorter_than10+use_gym,
                   data = data_subset)
summary(cox_model)

# 偏好类别变量
preferences <- c("Coffee/Tea preference", "SSB/ASB preference", "Natural juice/Plain water preference")

# 从Cox模型输出中提取的风险比
hazard_ratios <- c(
  0.96,        # coffee_tea_preference (0.9474)
  1.12,        # SSB_ASB_preference (1.4271)
  1.03         # NJ_water_preference (0.9962)
)

# 置信区间下限
lower_ci <- c(
  0.88,        # coffee_tea_preference (0.8658)
  0.94,        # SSB_ASB_preference (1.2465)
  0.93         # NJ_water_preference (0.8808)
)

# 置信区间上限
upper_ci <- c(
  1.04,        # coffee_tea_preference (1.0367)
  1.33,        # SSB_ASB_preference (1.6340)
  1.15         # NJ_water_preference (1.1267)
)

# 格式化HR和CI文本（保留两位小数）
hr_text <- sprintf("%.2f (%.2f to %.2f)", hazard_ratios, lower_ci, upper_ci)

# 创建标签文本矩阵
tabletext <- cbind(
  c("Beverage Preference", preferences),
  c("HR (95% CI)", hr_text)
)

# 创建森林图
forestplot(
  labeltext = tabletext,
  mean = c(NA, hazard_ratios),
  lower = c(NA, lower_ci),
  upper = c(NA, upper_ci),
  xlog = FALSE,
  boxsize = 0.2,
  lineheight = unit(8, "mm"),
  col = fpColors(box = "#0487D9", line = "#0487D9"),
  title = "All cause mortality - Beverage Preferences",
  zero = 1,
  clip = c(0.8, 1.7),
  xticks = c(0.8, 1.0, 1.2, 1.4, 1.6),
  is.summary = c(TRUE, rep(FALSE, 3)),
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.9),
    ticks = gpar(cex = 0.9),
    xlab = gpar(cex = 0.9),
    summary = gpar(cex = 1.0, fontface = "bold")
  ),
  xlab = "Hazard ratio",
  hrzl_lines = list("1" = gpar(lty = 1, col = "#444444")),
  graph.pos = 2,
  colgap = unit(5, "mm"),
  col.spacing = unit(10, "mm"),
  new_page = TRUE
)



beverage_20w_data$beverage_preference[beverage_20w_data$NJ_water_preference==1] <- 0
beverage_20w_data$beverage_preference[beverage_20w_data$coffee_tea_preference==1] <- 1
beverage_20w_data$beverage_preference[beverage_20w_data$SSB_ASB_preference==1] <- 2
table(beverage_20w_data$beverage_preference)
beverage_20w_data$beverage_preference <- as.factor(beverage_20w_data$beverage_preference)
dataset_beverage_preference <- beverage_20w_data[!is.na(beverage_20w_data$beverage_preference),]


dataset_beverage_preference_dataset <- dataset_beverage_preference[dataset_beverage_preference$ckm_stage %in% c(0,1),]
dataset_beverage_preference_dataset <- dataset_beverage_preference[dataset_beverage_preference$ckm_stage %in% c(2),]
dataset_beverage_preference_dataset <- dataset_beverage_preference[dataset_beverage_preference$ckm_stage %in% c(3,4),]
dataset_beverage_preference_dataset$death_age <- dataset_beverage_preference_dataset$age+dataset_beverage_preference_dataset$follow_to_death_time
summary(dataset_beverage_preference_dataset$death_age)

# 使用倾向性评分加权
library(WeightIt)

# 计算倾向性评分权重
weights <- weightit(beverage_preference ~ age + sex + ethnic + tdi + pack_years_smoking + 
                      chesse_intake + nap_frequency + frequency_tiredness +
                      household_income + live_with_partner + shorter_than10 + 
                      sleep_duration_group + plumper_than10 + education_years + 
                      smoking_status + physical_activity_group + employed + 
                      living_flat_vs_house + financial_diffculty + use_gym + 
                      fed_up_feeling + renting_from_council_vs_own + ease_skin_tanning,
                    data = dataset_beverage_preference_dataset,
                    method = "ps")

# 在 survfit 中使用权重
survfit_weighted <- survfit(Surv(time = follow_to_death_time,
                                 event = death) ~ beverage_preference,
                            data = dataset_beverage_preference_dataset,
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
           ylim=c(0,0.30),
           xlim=c(0,15),
           font.x=13,
           font.y=13,
           font.legend=10,
           font.tickslab=10,
           break.x.by = 5, break.y.by = 0.05,
           palette = c("#7AB3BF","#D97A07","#D90404")
           
)
ggsave("C:/Users/张杰/Desktop/beverage_preference_mortality_survival_plot.pdf", width = 8, height = 6, dpi = 300)



cox_model <- coxph(Surv(time = follow_to_death_time,
                        event = death) ~ beverage_preference+age + sex + ethnic + tdi + pack_years_smoking + 
                     chesse_intake + nap_frequency + frequency_tiredness +
                     household_income + live_with_partner + shorter_than10 + 
                     sleep_duration_group + plumper_than10 + education_years + 
                     smoking_status + physical_activity_group + employed + 
                     living_flat_vs_house + financial_diffculty + use_gym + 
                     fed_up_feeling + renting_from_council_vs_own + ease_skin_tanning,
                   data = dataset_beverage_preference_dataset,)
summary(cox_model)

# 创建数据框
data <- data.frame(
  group = factor(c("Low", "Medium", "High"), levels = c("High", "Medium", "Low")),
  hr = c(0.9402, 1, 1.2589),
  lower = c(0.8287, NA, 1.0729),
  upper = c(1.0667, NA, 1.4772)
)

# 创建图表
ggplot(data, aes(x = hr, y = group, color = group)) +
  geom_point(size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0,size = 1.2) +
  scale_x_continuous(name = "HR", limits = c(0.8, 1.5), breaks = seq(0.8, 1.4, by = 0.1)) +
  scale_color_manual(values = c("Low" ="#D97A07" , "Medium" = "#7AB3BF", "High" = "#D90404")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.grid = element_blank(),  # 移除所有网格线
    axis.line.x = element_line(color = "black"),  # 添加 x 轴轴线
    axis.ticks.x = element_line(color = "black"),  # 添加 x 轴刻度线
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")  # 添加边距
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray")

# 保存图表
ggsave("C:/Users/张杰/Desktop/beverage_preference_forest_plot.pdf", width = 6, height = 2, dpi = 300)




############################### 50w analyses######################################
names(coffee_tea_alternative_data)
str(imputed_data)
beverage_50w_data <- merge(coffee_tea_alternative_data[,c(1,328:330)],imputed_data,all.x=T)
beverage_50w_data <- merge(beverage_50w_data,ckm_ukb_data[,c("eid","baseline_date")],all.x=T)
str(beverage_50w_data)



beverage_50w_data$death[beverage_50w_data$eid %in% death_data$eid] <- 1
beverage_50w_data$death[is.na(beverage_50w_data$death)] <- 0
str(death_data)
beverage_50w_data <- merge(beverage_50w_data,death_data[,c("eid","date_of_death","cause_icd10","broad_cause",
                                                           "cvd_death","cancer_death","respiratory_death","specific_cancer","specific_cvd")],all.x=T)
beverage_50w_data$cvd_death[is.na(beverage_50w_data$cvd_death)] <- 0
beverage_50w_data$respiratory_death[is.na(beverage_50w_data$respiratory_death)] <- 0
beverage_50w_data$cancer_death[is.na(beverage_50w_data$cancer_death)] <- 0

end_date <- as.Date("2022-12-31")

# 4. 计算随访时间
beverage_50w_data <- within(beverage_50w_data, {
  # 对于已经死亡的人，使用死亡日期
  # 对于仍在随访的人，使用2022年底
  follow_to_death_time <- case_when(
    !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, baseline_date, units = "days")) / 365.25,
    !is.na(baseline_date) ~ as.numeric(difftime(end_date, baseline_date, units = "days")) / 365.25,
    TRUE ~ NA_real_
  )
})

summary(beverage_50w_data$follow_to_death_time)


names(ckm_ukb_data)
beverage_50w_data <- merge(beverage_50w_data, 
                           ckm_ukb_data[ckm_ukb_data$ckm_stage!="NA", colnames(ckm_ukb_data) %in% c("eid","ckm_stage")], 
                           all.x=TRUE)
names(beverage_50w_data)


beverage_50w_data$coffee_intake_cat[beverage_50w_data$coffee_intake==0] <- 0
beverage_50w_data$coffee_intake_cat[beverage_50w_data$coffee_intake>0 & beverage_50w_data$coffee_intake<=2] <- 1
beverage_50w_data$coffee_intake_cat[beverage_50w_data$coffee_intake>2] <- 2
beverage_50w_data$coffee_intake_cat <- as.factor(beverage_50w_data$coffee_intake_cat)

summary(beverage_50w_data$tea_intake)
beverage_50w_data$tea_intake_cat[beverage_50w_data$tea_intake==0] <- 0
beverage_50w_data$tea_intake_cat[beverage_50w_data$tea_intake>0 & beverage_50w_data$tea_intake<=2] <- 1
beverage_50w_data$tea_intake_cat[beverage_50w_data$tea_intake>2] <- 2
beverage_50w_data$tea_intake_cat <- as.factor(beverage_50w_data$tea_intake_cat)

summary(beverage_50w_data$water_intake)
beverage_50w_data$water_intake[beverage_50w_data$water_intake==-1] <- NA
beverage_50w_data$water_intake_cat[beverage_50w_data$water_intake==0] <- 0
beverage_50w_data$water_intake_cat[beverage_50w_data$water_intake>0 & beverage_50w_data$water_intake<=2] <- 1
beverage_50w_data$water_intake_cat[beverage_50w_data$water_intake>2] <- 2
beverage_50w_data$water_intake_cat <- as.factor(beverage_50w_data$water_intake_cat)

data_subset <- beverage_50w_data[beverage_50w_data$ckm_stage==3 | beverage_50w_data$ckm_stage==4,]
data_subset <- beverage_50w_data[beverage_50w_data$ckm_stage==1,]


survial_event <- Surv(time = data_subset$follow_to_death_time,
                      event = data_subset$death)
cox_model <- coxph(survial_event ~ coffee_intake + tea_intake + water_intake +
                     age + sex + race + education + household_income+
                     bmi + diet_score + smk_num + smk_qyr  +  PA_mod_vig_150 + alcohol_intake_frequency+
                     overall_health_rating,
                   data = data_subset)
summary(cox_model)
##################################CKM stage for 500,000 (Coffee, Tea, Water)###########################################
ckm_berverage_data_subset <- beverage_50w_data[(beverage_50w_data$ckm_stage==2) &
                                                 !is.na(beverage_50w_data$ckm_stage) ,-32]

ckm_berverage_data_subset <- beverage_50w_data[(beverage_50w_data$ckm_stage==3 | beverage_50w_data$ckm_stage==4) &
                                                 !is.na(beverage_50w_data$ckm_stage) ,-32]
"#74a892"
"#f0b6ad"
"#dc8864"
"#ba4848"
"#642915"
names(ckm_berverage_data_subset)


# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "coffee_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ rcs(coffee_intake,3) + tea_intake + water_intake+",
                       "age + sex + race + education + household_income + ",
                       "bmi + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + ",
                       "alcohol_intake_frequency + overall_health_rating",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$coffee_intake),])

# 预测相对风险
coffee_range <- seq(0, 6, by = 0.1)
pred <- Predict(fit, coffee_intake = coffee_range, fun = exp,ref.zero=T)


# 绘图
ggplot(pred, aes(x = coffee_intake, y = yhat)) +
  geom_line(color = "#642915", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill=NA,
              color = "#642915",
              linetype = "dashed") + # 置信区间用虚线
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(limits = c(0.2, 2)) +
  labs(x = "Cups per day",
       y = "Hazard ratios for mortality") +
  ggtitle("Coffee") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # 移除竖直网格线
    panel.border = element_rect(fill = NA, color = "gray"),
    aspect.ratio = 1
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)



# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "tea_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + rcs(tea_intake,4) + water_intake+",
                       "age + sex + race + education + household_income + ",
                       "bmi_cat + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + ",
                       "alcohol_intake_frequency + overall_health_rating",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$tea_intake),])

# 预测相对风险
tea_range <- seq(0, 8, by = 0.01)
pred <- Predict(fit, tea_intake = tea_range, fun = exp,ref.zero=T)


# 绘图
ggplot(pred, aes(x = tea_intake, y = yhat)) +
  geom_line(color = "#642915", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill=NA,
              color = "#642915",
              linetype = "dashed") + # 置信区间用虚线
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(limits = c(0.2, 2)) +
  labs(x = "Cups per day",
       y = "Hazard ratios for mortality") +
  ggtitle("Tea") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # 移除竖直网格线
    panel.border = element_rect(fill = NA, color = "gray"),
    aspect.ratio = 1
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)




# 首先设置数据分布
dd <- datadist(ckm_berverage_data_subset)
dd$limits["Adjust to", "water_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + rcs(water_intake,4)+",
                       "age + sex + race + education + household_income + ",
                       "bmi_cat + diet_score + smk_num + smk_qyr + PA_mod_vig_150 + ",
                       "alcohol_intake_frequency + overall_health_rating",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=ckm_berverage_data_subset[!is.na(ckm_berverage_data_subset$water_intake),])

# 预测相对风险
water_range <- seq(0, 8, by = 0.1)
pred <- Predict(fit, water_intake = water_range, fun = exp,ref.zero=T)



# 绘图
ggplot(pred, aes(x = water_range, y = yhat)) +
  geom_line(color = "#642915", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill=NA,
              color = "#642915",
              linetype = "dashed") + # 置信区间用虚线
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  scale_y_continuous(limits = c(0.2, 3)) +
  labs(x = "Cups per day",
       y = "Hazard ratios for mortality") +
  ggtitle("Plain water") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(), # 移除竖直网格线
    panel.border = element_rect(fill = NA, color = "gray"),
    aspect.ratio = 1
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)




