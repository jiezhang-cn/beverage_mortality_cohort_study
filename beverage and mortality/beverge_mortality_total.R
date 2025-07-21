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


death_update <- merge(death_update,ckm_ukb_data[,c("eid","baseline_date")],all.x = T)
str(death_update)
death_update$date_of_death <- as.Date(death_update$date_of_death)
max(death_update$date_of_death)
death_update$follow_to_death_time <- round(as.numeric(difftime(death_update$date_of_death, death_update$baseline_date, units = "days")) / 365.25, 2)
summary(death_update$follow_to_death_time)


#We defined the following 3 broad categories (ie, causes with >500 deaths) of cause-specific mortality using the International Statistical Classification of Diseases and Related Health Problems, Tenth Revision (ICD-10), codes: cancer (C00-D48), cardiovascular disease (I00-I79), and respiratory diseases (J09-J18 and J40-J47). In addition, for cancer and cardiovascular disease, we further defined the common causes of death (ie, causes with >500 deaths) within these broad categories: colorectal cancer (C18-C20), bronchus and lung cancer (C34), female breast cancer (C50), and pancreatic cancer (C25); ischemic heart diseases (I20-I25); and stroke (I60-I69).
str(death_cause_update)
death_cause_update <- death_cause_update[death_cause_update$level==1,]


# 创建主要分类
# We defined the following 3 broad categories (ie, causes with >500 deaths) of cause-specific mortality using the International Statistical Classification of Diseases and Related Health Problems, Tenth Revision (ICD-10), codes: cancer (C00-D48), cardiovascular disease (I00-I79), and respiratory diseases (J09-J18 and J40-J47). In addition, for cancer and cardiovascular disease, we further defined the common causes of death (ie, causes with >500 deaths) within these broad categories: colorectal cancer (C18-C20), bronchus and lung cancer (C34), female breast cancer (C50), and pancreatic cancer (C25); ischemic heart diseases (I20-I25); and stroke (I60-I69).

str(death_cause_update)
death_cause_update <- death_cause_update[death_cause_update$level==1,]

# 创建主要分类（扩展为5个类别）
death_cause_update$broad_cause <- NA
death_cause_update$broad_cause[grep("^[C]", death_cause_update$cause_icd10) | 
                                 grep("^D[0-4]", death_cause_update$cause_icd10)] <- 1  #"cancer"
death_cause_update$broad_cause[grep("^I[0-7]", death_cause_update$cause_icd10)] <- 2  #"cvd"
death_cause_update$broad_cause[grep("^J(09|1[0-8]|4[0-7])", death_cause_update$cause_icd10)] <- 3 #"respiratory"
death_cause_update$broad_cause[grep("^K[2-9]", death_cause_update$cause_icd10)] <- 4 #"digestive"
death_cause_update$broad_cause[grep("^F[0-8]", death_cause_update$cause_icd10) | 
                                 grep("^G[0-9]", death_cause_update$cause_icd10)] <- 5 #"nervous_system"

# 创建cancer二分类变量 
death_cause_update$cancer_death <- ifelse(death_cause_update$broad_cause == 1, 1, 0)
# 创建cvd二分类变量
death_cause_update$cvd_death <- ifelse(death_cause_update$broad_cause == 2, 1, 0) 
# 创建respiratory二分类变量
death_cause_update$respiratory_death <- ifelse(death_cause_update$broad_cause == 3, 1, 0)
# 创建digestive二分类变量
death_cause_update$digestive_death <- ifelse(death_cause_update$broad_cause == 4, 1, 0)
# 创建nervous_system二分类变量
death_cause_update$nervous_system_death <- ifelse(death_cause_update$broad_cause == 5, 1, 0)

# 创建具体癌症类型（扩展版）
death_cause_update$specific_cancer <- NA
death_cause_update$specific_cancer[grep("^C(18|19|20)", death_cause_update$cause_icd10)] <- 1  #"colorectal"
death_cause_update$specific_cancer[grep("^C34", death_cause_update$cause_icd10)] <- 2 #"lung"
death_cause_update$specific_cancer[grep("^C50", death_cause_update$cause_icd10)] <- 3 #"breast"
death_cause_update$specific_cancer[grep("^C25", death_cause_update$cause_icd10)] <- 4 #"pancreatic"
death_cause_update$specific_cancer[grep("^C22", death_cause_update$cause_icd10)] <- 5 #"liver"
death_cause_update$specific_cancer[grep("^C16", death_cause_update$cause_icd10)] <- 6 #"stomach"
death_cause_update$specific_cancer[grep("^C61", death_cause_update$cause_icd10)] <- 7 #"prostate"
death_cause_update$specific_cancer[grep("^C53", death_cause_update$cause_icd10)] <- 8 #"cervical"
death_cause_update$specific_cancer[grep("^C56", death_cause_update$cause_icd10)] <- 9 #"ovarian"
death_cause_update$specific_cancer[grep("^C67", death_cause_update$cause_icd10)] <- 10 #"bladder"
death_cause_update$specific_cancer[grep("^C15", death_cause_update$cause_icd10)] <- 11 #"esophageal"
death_cause_update$specific_cancer[grep("^C64", death_cause_update$cause_icd10)] <- 12 #"kidney"
death_cause_update$specific_cancer[grep("^C71", death_cause_update$cause_icd10)] <- 13 #"brain"
death_cause_update$specific_cancer[grep("^C43", death_cause_update$cause_icd10)] <- 14 #"melanoma"

# 创建具体心血管疾病类型（扩展版）
death_cause_update$specific_cvd <- NA
death_cause_update$specific_cvd[grep("^I2[0-5]", death_cause_update$cause_icd10)] <- 1 #"ihd" (ischemic heart disease)
death_cause_update$specific_cvd[grep("^I6[0-9]", death_cause_update$cause_icd10)] <- 2 #"stroke"
death_cause_update$specific_cvd[grep("^I50", death_cause_update$cause_icd10)] <- 3 #"heart_failure"
death_cause_update$specific_cvd[grep("^I1[0-5]", death_cause_update$cause_icd10)] <- 4 #"hypertensive_disease"
death_cause_update$specific_cvd[grep("^I05|^I06|^I07|^I08|^I09", death_cause_update$cause_icd10)] <- 5 #"rheumatic_heart_disease"
death_cause_update$specific_cvd[grep("^I3[0-9]|^I4[0-9]", death_cause_update$cause_icd10)] <- 6 #"other_heart_diseases"
death_cause_update$specific_cvd[grep("^I7[0-9]", death_cause_update$cause_icd10)] <- 7 #"arterial_diseases"
death_cause_update$specific_cvd[grep("^I26|^I27|^I28", death_cause_update$cause_icd10)] <- 8 #"pulmonary_heart_disease"


# 检查各类别的死亡人数
table(death_cause_update$broad_cause, useNA = "always")
table(death_cause_update$specific_cancer, useNA = "always")
table(death_cause_update$specific_cvd, useNA = "always")
table(death_cause_update$digestive_death, useNA = "always")
table(death_cause_update$nervous_system_death, useNA = "always")

death_data <- merge(death_update,death_cause_update,all.x = T)



##################################total 20w analysis#######################################
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



#traditional confoundings adjustments
names(beverage_20w_data)
names(imputed_data_2)
beverage_20w_data <- merge(beverage_20w_data,imputed_data_2[,c(1,5,7,9:17)])

survial_event <- Surv(time = beverage_20w_data$follow_to_death_time,
                      event = beverage_20w_data$death)
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic + education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency+ diet_score + 
                     PA_mod_vig_150 + overall_health_rating,
                   data = beverage_20w_data)
summary(cox_model)


#coffee-mortality association
survial_event <- Surv(time = beverage_20w_data$follow_to_death_time,
                      event = beverage_20w_data$death)
cox_model <- coxph(survial_event ~ coffee_intake_cat + tea_intake_cat  + SSB_intake_cat  + ASB_intake_cat  + NJ_intake_cat  + 
                     lowfat_milk_intake_cat  + fullfat_milk_intake_cat +plain_water_intake_cat +
                     age + sex + ethnic +tdi+pack_years_smoking+chesse_intake+	
                     frequency_unenthusiasm+nap_frequency+frequency_tiredness+	
                     ease_skin_tanning+household_income+	
                     live_with_partner+shorter_than10+sleep_duration_group+	
                     plumper_than10+education_years+
                     smoking_status,
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
                   data = beverage_20w_data)
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
  0.9294,      # coffee_intake_cat2
  0.8656,      # tea_intake_cat2
  0.9969,      # lowfat_milk_intake_cat2
  1.1722,      # fullfat_milk_intake_cat2
  1.1723,      # SSB_intake_cat2
  1.1707,      # ASB_intake_cat2
  0.9696,      # NJ_intake_cat2 (Natural/Fruit Juice)
  1.1272       # plain_water_intake_cat2
)

# Confidence intervals (reordered to match)
lower_ci <- c(
  0.8817,      # coffee_intake_cat2
  0.8189,      # tea_intake_cat2
  0.9731,      # lowfat_milk_intake_cat2
  1.0940,      # fullfat_milk_intake_cat2
  1.1115,      # SSB_intake_cat2
  1.0988,      # ASB_intake_cat2
  0.9185,      # NJ_intake_cat2
  1.0669       # plain_water_intake_cat2
)

upper_ci <- c(
  0.9877,      # coffee_intake_cat2
  0.9149,      # tea_intake_cat2
  1.0540,      # lowfat_milk_intake_cat2
  1.2561,      # fullfat_milk_intake_cat2
  1.2364,      # SSB_intake_cat2
  1.2474,      # ASB_intake_cat2
  1.0235,      # NJ_intake_cat2
  1.1909       # plain_water_intake_cat2
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


#############################################exposure-response plot##############################################
names(beverage_20w_data)
"#74a892"
"#f0b6ad"
"#dc8864","#DC886419"
"#ba4848","#F6E6E6"
"#642915"


library(plotRCS)
rcsplot(data = beverage_20w_data[beverage_20w_data$coffee_intake<=8 & !is.na(beverage_20w_data$coffee_intake),],
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
        ybreaks= seq(0.8, 1.5, by = 0.1),
        linesize=1,
        linecolor="#0487D9",
)


rcsplot(data = beverage_20w_data[beverage_20w_data$tea_intake<=8 & !is.na(beverage_20w_data$tea_intake),],
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
        ybreaks= seq(0.8, 1.4, by = 0.1),
        linesize=1,
        linecolor="#0487D9",
)



rcsplot(data = beverage_20w_data[beverage_20w_data$plain_water_intake<=8 & !is.na(beverage_20w_data$plain_water_intake),],
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
        ybreaks= seq(0.8, 1.3, by = 0.1),
        linesize=1,
        linecolor="#0487D9",
)        




# Load the splines package
library(splines)
library(rms)
# 首先设置数据分布
dd <- datadist(beverage_20w_data)
dd$limits["Adjust to", "SSB_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(beverage_20w_data$SSB_intake)
quantile(beverage_20w_data$SSB_intake,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ coffee_intake + tea_intake + ",
                       "pol(SSB_intake, 3) + ASB_intake + NJ_intake +",
                       "lowfat_milk_intake + fullfat_milk_intake+ plain_water_intake +",
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
           data=beverage_20w_data[!is.na(beverage_20w_data$SSB_intake),])

# 预测相对风险
SSB_range <- seq(0, 6, by = 0.1)
pred <- Predict(fit, SSB_intake = SSB_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, SSB_intake)  

# 绘图
ggplot(pred, aes(x = SSB_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#0487D91A") + # 置信区间用虚线
  geom_line(color = "#0487D9", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 6, by = 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 2.0, by = 0.2),expand = c(0, 0)) +
  labs(x = "SSB intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)



# 首先设置数据分布
dd <- datadist(beverage_20w_data)
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
           data=beverage_20w_data[!is.na(beverage_20w_data$ASB_intake),])

# 预测相对风险
ASB_range <- seq(0, 5, by = 0.1)
pred <- Predict(fit, ASB_intake = ASB_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, ASB_intake)  

# 绘图
ggplot(pred, aes(x = ASB_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#0487D91A") + # 置信区间用虚线
  geom_line(color = "#0487D9", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 5, by = 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 1.6, by = 0.2),expand = c(0.2, 0.1)) +
  labs(x = "ASB intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)



# 首先设置数据分布
dd <- datadist(beverage_20w_data)
dd$limits["Adjust to", "NJ_intake"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(beverage_20w_data$NJ_intake)
quantile(beverage_20w_data$NJ_intake,0.9,na.rm = T)
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
           data=beverage_20w_data[!is.na(beverage_20w_data$NJ_intake),])

# 预测相对风险
NJ_range <- seq(0, 5, by = 0.1)
pred <- Predict(fit, NJ_intake = NJ_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, NJ_intake) 

# 绘图
ggplot(pred, aes(x = NJ_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#0487D91A") + # 置信区间用虚线
  geom_line(color = "#0487D9", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 5, by = 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 1.6, by = 0.1),expand = c(0, 0.1)) +
  labs(x = "NJ intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)









# 首先设置数据分布
dd <- datadist(beverage_20w_data)
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
           data=beverage_20w_data[!is.na(beverage_20w_data$lowfat_milk_intake),])

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
              fill="#0487D91A") + # 置信区间用虚线
  geom_line(color = "#0487D9", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 6, by = 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 1.2, by = 0.1),expand = c(0, 0.2)) +
  labs(x = "lowfat_milk intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )

ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)






# 首先设置数据分布
dd <- datadist(beverage_20w_data)
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
           data=beverage_20w_data[!is.na(beverage_20w_data$fullfat_milk_intake),])

# 预测相对风险
fullfat_milk_range <- seq(0, 6, by = 0.1)
pred <- Predict(fit, fullfat_milk_intake = fullfat_milk_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, fullfat_milk_intake) 

# 绘图
ggplot(pred, aes(x = fullfat_milk_intake, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#0487D91A") + # 置信区间用虚线
  geom_line(color = "#0487D9", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 6, by = 1),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 1.6, by = 0.1),expand = c(0, 0.2)) +
  labs(x = "fullfat_milk intake (cups/d)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )

ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)


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



survial_event <- Surv(time = beverage_20w_data$follow_to_death_time,
                      event = beverage_20w_data$death)

cox_model <- coxph(survial_event ~coffee_tea_preference +
                     age + sex + ethnic + tdi +pack_years_smoking+chesse_intake+	
                     nap_frequency+	frequency_tiredness+
                     household_income+live_with_partner+
                     shorter_than10+sleep_duration_group+plumper_than10+	
                     education_years+smoking_status+physical_activity_group+
                     employed+living_flat_vs_house+financial_diffculty+
                     use_gym,
                   data = beverage_20w_data)
summary(cox_model)


cox_model <- coxph(survial_event ~SSB_ASB_preference +
                     age + sex + ethnic +chesse_intake+	
                     nap_frequency+household_income+employed+
                     education_years+smoking_status+fed_up_feeling+pack_years_smoking+
                     frequency_tiredness+physical_activity_group+sleep_duration_group+
                     plumper_than10+renting_from_council_vs_own,
                   data = beverage_20w_data)
summary(cox_model)



cox_model <- coxph(survial_event ~NJ_water_preference +
                     age + sex + ethnic +fed_up_feeling+pack_years_smoking+
                     chesse_intake+frequency_tiredness+ease_skin_tanning+
                     household_income+live_with_partner+physical_activity_group+
                     employed+living_flat_vs_house+	
                     financial_diffculty+plumper_than10+education_years+
                     smoking_status+tdi+	
                     shorter_than10+use_gym,
                   data = beverage_20w_data)
summary(cox_model)

# 偏好类别变量
preferences <- c("Coffee/Tea preference", "SSB/ASB preference", "Natural juice/Plain water preference")

# 从Cox模型输出中提取的风险比
hazard_ratios <- c(
  0.95,        # coffee_tea_preference (0.9474)
  1.43,        # SSB_ASB_preference (1.4271)
  1.00         # NJ_water_preference (0.9962)
)

# 置信区间下限
lower_ci <- c(
  0.87,        # coffee_tea_preference (0.8658)
  1.25,        # SSB_ASB_preference (1.2465)
  0.88         # NJ_water_preference (0.8808)
)

# 置信区间上限
upper_ci <- c(
  1.04,        # coffee_tea_preference (1.0367)
  1.63,        # SSB_ASB_preference (1.6340)
  1.13         # NJ_water_preference (1.1267)
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
                    data = dataset_beverage_preference,
                    method = "ps")

# 在 survfit 中使用权重
survfit_weighted <- survfit(Surv(time = follow_to_death_time,
                                 event = death) ~ beverage_preference,
                            data = dataset_beverage_preference,
                            weights = weights$weights)

library(survminer)
ggsurvplot(survfit_weighted,
           fun = function(y)(1-y),
           size=1,
           censor=F,
           pval = T,
           conf.int = T,  
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
           palette = c("#7AB3BF","#D97A07","#D90404")
           
)
ggsave("beverage_preference_mortality_survival_plot.pdf", width = 8, height = 6, dpi = 300)



cox_model <- coxph(Surv(time = follow_to_death_time,
                        event = death) ~ beverage_preference+age + sex + ethnic + tdi + pack_years_smoking + 
                     chesse_intake + nap_frequency + frequency_tiredness +
                     household_income + live_with_partner + shorter_than10 + 
                     sleep_duration_group + plumper_than10 + education_years + 
                     smoking_status + physical_activity_group + employed + 
                     living_flat_vs_house + financial_diffculty + use_gym + 
                     fed_up_feeling + renting_from_council_vs_own + ease_skin_tanning,
                   data = dataset_beverage_preference,)
summary(cox_model)

# 创建数据框
data <- data.frame(
  group = factor(c("Low", "Medium", "High"), levels = c("High", "Medium", "Low")),
  hr = c(0.9332, 1, 1.1663),
  lower = c(0.8840, NA, 1.0821),
  upper = c(0.9851, NA, 1.2570)
)

# 创建图表
ggplot(data, aes(x = hr, y = group, color = group)) +
  geom_point(size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0,size = 1.2) +
  scale_x_continuous(name = "HR", limits = c(0.8, 1.4), breaks = seq(0.8, 1.4, by = 0.1)) +
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



