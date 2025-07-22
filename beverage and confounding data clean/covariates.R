library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)

ukb_beverage_covariates_data <- fread("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_beverage_covariates_data")

###############################################Covariates###################################################################
#Diet score (≥4 among following 7) 
#Fruit 3 servings/day (Amount per serving: 1309 – 1 piece  1319 – 5 pieces) 
#Vegetable 3 servings/day  (Amount per serving: 3 heaped tablespoons )
#(Shell)fish ≥2 servings/week (Amount per serving: Once/week )
#Processed meats ≤1 serving/week (Amount per serving: 1349 – 1 piece/day 3680 – 0 pieces/day if indicated having never eaten meat )
#Unprocessed meats ≤1.5 serving/wk  (Amount per serving: 1359-1389 – once/week3680 – 0 pieces/day if indicated havingnever eaten meat )
#Whole grains 3 servings/day  (1438/1448 – 1 slice/day 1458/1468 – 1 bowl/day)
#Refined grains ≤1.5 servings/day  (1438/1448 – 1 slice/day 1458/1468 – 1 bowl/day)
names(ukb_beverage_covariates_data)
ukb_beverage_covariates_data$cooked_vegetable_intake_f1289_0_0[ukb_beverage_covariates_data$cooked_vegetable_intake_f1289_0_0==-10] <- 0.5
ukb_beverage_covariates_data$cooked_vegetable_intake_f1289_0_0[ukb_beverage_covariates_data$cooked_vegetable_intake_f1289_0_0<0] <- NA
ukb_beverage_covariates_data$salad_raw_vegetable_intake_f1299_0_0[ukb_beverage_covariates_data$salad_raw_vegetable_intake_f1299_0_0==-10] <- 0.5
ukb_beverage_covariates_data$salad_raw_vegetable_intake_f1299_0_0[ukb_beverage_covariates_data$salad_raw_vegetable_intake_f1299_0_0<0] <- NA
ukb_beverage_covariates_data$vegetable <- ifelse((ukb_beverage_covariates_data$cooked_vegetable_intake_f1289_0_0 + ukb_beverage_covariates_data$salad_raw_vegetable_intake_f1299_0_0)/3 >= 3, 1, 0)
table(ukb_beverage_covariates_data$vegetable)


ukb_beverage_covariates_data$fresh_fruit_intake_f1309_0_0[ukb_beverage_covariates_data$fresh_fruit_intake_f1309_0_0==-10] <- 0.5
ukb_beverage_covariates_data$fresh_fruit_intake_f1309_0_0[ukb_beverage_covariates_data$fresh_fruit_intake_f1309_0_0<0] <- NA
ukb_beverage_covariates_data$dried_fruit_intake_f1319_0_0[ukb_beverage_covariates_data$dried_fruit_intake_f1319_0_0==-10] <- 0.5
ukb_beverage_covariates_data$dried_fruit_intake_f1319_0_0[ukb_beverage_covariates_data$dried_fruit_intake_f1319_0_0<0] <- NA
ukb_beverage_covariates_data$fruit <- ifelse((ukb_beverage_covariates_data$fresh_fruit_intake_f1309_0_0 + (ukb_beverage_covariates_data$dried_fruit_intake_f1319_0_0/5)) >= 3, 1, 0)
table(ukb_beverage_covariates_data$fruit)


ukb_beverage_covariates_data$oily_fish_intake_f1329_0_0[ukb_beverage_covariates_data$oily_fish_intake_f1329_0_0<0] <- NA
ukb_beverage_covariates_data$nonoily_fish_intake_f1339_0_0[ukb_beverage_covariates_data$nonoily_fish_intake_f1339_0_0<0] <- NA
ukb_beverage_covariates_data$fish <- ifelse((ukb_beverage_covariates_data$oily_fish_intake_f1329_0_0 + ukb_beverage_covariates_data$nonoily_fish_intake_f1339_0_0) >= 3, 1, 0)
table(ukb_beverage_covariates_data$fish)


ukb_beverage_covariates_data$processed_meat_intake_f1349_0_0[ukb_beverage_covariates_data$processed_meat_intake_f1349_0_0<0] <- NA
ukb_beverage_covariates_data$processed_meat <- ifelse((ukb_beverage_covariates_data$processed_meat_intake_f1349_0_0) <= 1, 1, 0)
table(ukb_beverage_covariates_data$processed_meat)


ukb_beverage_covariates_data$beef_intake_f1369_0_0[ukb_beverage_covariates_data$beef_intake_f1369_0_0<0] <- NA
ukb_beverage_covariates_data$lambmutton_intake_f1379_0_0[ukb_beverage_covariates_data$lambmutton_intake_f1379_0_0<0] <- NA
ukb_beverage_covariates_data$pork_intake_f1389_0_0[ukb_beverage_covariates_data$pork_intake_f1389_0_0<0] <- NA
ukb_beverage_covariates_data$red_meat <- ifelse((ukb_beverage_covariates_data$beef_intake_f1369_0_0 + ukb_beverage_covariates_data$lambmutton_intake_f1379_0_0+ukb_beverage_covariates_data$pork_intake_f1389_0_0) <= 3, 1, 0)
table(ukb_beverage_covariates_data$red_meat)


ukb_beverage_covariates_data$cereal_intake_f1458_0_0[ukb_beverage_covariates_data$cereal_intake_f1458_0_0==-10] <- 0.5
ukb_beverage_covariates_data$cereal_intake_f1458_0_0[ukb_beverage_covariates_data$cereal_intake_f1458_0_0<0] <- NA
ukb_beverage_covariates_data$bread_intake_f1438_0_0[ukb_beverage_covariates_data$bread_intake_f1438_0_0==-10] <- 0.5
ukb_beverage_covariates_data$bread_intake_f1438_0_0[ukb_beverage_covariates_data$bread_intake_f1438_0_0<0] <- NA


ukb_beverage_covariates_data$whole_grains <- ifelse(((ukb_beverage_covariates_data$cereal_intake_f1458_0_0 + ukb_beverage_covariates_data$bread_intake_f1438_0_0)/7 >= 3) &
                                                      (ukb_beverage_covariates_data$bread_type_f1448_0_0 %in% c(3)) &
                                                      (ukb_beverage_covariates_data$cereal_type_f1468_0_0 %in% c(1,3,4)),
                                                    1, 0)
table(ukb_beverage_covariates_data$whole_grains)

ukb_beverage_covariates_data$refined_grains <- ifelse(((ukb_beverage_covariates_data$cereal_intake_f1458_0_0 + ukb_beverage_covariates_data$bread_intake_f1438_0_0)/7 <= 1.5) &
                                                        (ukb_beverage_covariates_data$bread_type_f1448_0_0 %in% c(1,2,4)) &
                                                        (ukb_beverage_covariates_data$cereal_type_f1468_0_0 %in% c(2,5)),
                                                      1, 0)
table(ukb_beverage_covariates_data$refined_grains)

ukb_beverage_covariates_data$diet_score <- ukb_beverage_covariates_data$vegetable+ukb_beverage_covariates_data$fruit+ukb_beverage_covariates_data$fish+ukb_beverage_covariates_data$processed_meat+ukb_beverage_covariates_data$red_meat+ukb_beverage_covariates_data$whole_grains+ukb_beverage_covariates_data$refined_grains
summary(ukb_beverage_covariates_data$diet_score) 


#Alcohol consumption
names(ukb_beverage_covariates_data)

# 步骤1：选择需要的列并重命名
ukb_alc_dose <- ukb_beverage_covariates_data[, c("eid", 
                                                 "average_weekly_red_wine_intake_f1568_0_0",
                                                 "average_weekly_champagne_plus_white_wine_intake_f1578_0_0",
                                                 "average_weekly_beer_plus_cider_intake_f1588_0_0",
                                                 "average_weekly_spirits_intake_f1598_0_0",
                                                 "average_weekly_fortified_wine_intake_f1608_0_0",
                                                 "average_monthly_red_wine_intake_f4407_0_0",
                                                 "average_monthly_champagne_plus_white_wine_intake_f4418_0_0",
                                                 "average_monthly_beer_plus_cider_intake_f4429_0_0",
                                                 "average_monthly_spirits_intake_f4440_0_0",
                                                 "average_monthly_fortified_wine_intake_f4451_0_0")]

# 重命名列
names(ukb_alc_dose) <- c("eid", "red_wine_w", "white_wine_w", "beer_w", "liquor_w", "fortified_wine_w",
                         "red_wine_m", "white_wine_m", "beer_m", "liquor_m", "fortified_wine_m")

# 步骤2：替换 -3 和 -1 值（如果需要的话）
replace_values <- function(x, mean_value) {
  ifelse(x %in% c(-3, -1), mean_value, x)
}

mean_values <- c(3.9, 2.7, 3.0, 1.9, 0.2, 1.0, 1.0, 1.0, 0.7, 0.2)
for (i in 2:11) {  # 从第2列开始，因为第1列是 eid
  ukb_alc_dose[[i]] <- replace_values(ukb_alc_dose[[i]], mean_values[i-1])
}

# 步骤3：将 NA 值替换为 0
ukb_alc_dose[is.na(ukb_alc_dose)] <- 0

# 步骤4：计算新的变量
ukb_alc_dose$red_wine <- ukb_alc_dose$red_wine_w + ukb_alc_dose$red_wine_m * 0.25
ukb_alc_dose$white_wine <- ukb_alc_dose$white_wine_w + ukb_alc_dose$white_wine_m * 0.25
ukb_alc_dose$fortified_wine <- ukb_alc_dose$fortified_wine_w + ukb_alc_dose$fortified_wine_m * 0.25
ukb_alc_dose$beer <- ukb_alc_dose$beer_w + ukb_alc_dose$beer_m * 0.25
ukb_alc_dose$liquor <- ukb_alc_dose$liquor_w + ukb_alc_dose$liquor_m * 0.25

# 步骤5：计算摄入量
ukb_alc_dose$wine_intake <- (ukb_alc_dose$red_wine + ukb_alc_dose$white_wine) * 1.6 * 8 + ukb_alc_dose$fortified_wine * 1 * 8
ukb_alc_dose$red_wine_intake <- ukb_alc_dose$red_wine * 1.6 * 8
ukb_alc_dose$white_wine_intake <- ukb_alc_dose$white_wine * 1.6 * 8
ukb_alc_dose$beer_intake <- ukb_alc_dose$beer * 2.6 * 8
ukb_alc_dose$liquor_intake <- ukb_alc_dose$liquor * 1 * 8
ukb_alc_dose$total_alcohol <- ukb_alc_dose$red_wine * 1.6 * 8 + 
  ukb_alc_dose$white_wine * 1.6 * 8 + 
  ukb_alc_dose$fortified_wine * 1 * 8 + 
  ukb_alc_dose$beer * 2.6 * 8 + 
  ukb_alc_dose$liquor * 1 * 8

summary(ukb_alc_dose$total_alcohol)
names(ukb_alc_dose)
ukb_beverage_covariates_data <- merge(ukb_beverage_covariates_data,ukb_alc_dose[,c(1,22)],all.x=T)


#physical activity
names(ukb_beverage_covariates_data)
pa_data <- ukb_beverage_covariates_data[, c("eid",
                                            "number_of_daysweek_of_moderate_physical_activity_10_minutes_f884_0_0",
                                            "number_of_daysweek_of_vigorous_physical_activity_10_minutes_f904_0_0",
                                            "duration_of_moderate_activity_f894_0_0",
                                            "duration_of_vigorous_activity_f914_0_0")]

# 步骤2：重命名列以便于处理
names(pa_data) <- c("n_eid", "mod_day", "vig_day", "mod_dur", "vig_dur")

# 步骤3：将 -1 和 -3 替换为 NA
replace_values <- function(x) {
  ifelse(x %in% c(-1, -3), NA, x)
}

pa_data[, 2:5] <- lapply(pa_data[, 2:5], replace_values)

# 步骤4：计算新变量
pa_data$mod_dur[pa_data$mod_day == 0] <- NA
pa_data$vig_dur[pa_data$vig_day == 0] <- NA

pa_data$mod_min <- pa_data$mod_dur * pa_data$mod_day
pa_data$vig_min <- pa_data$vig_dur * pa_data$vig_day * 2
pa_data$pa_min <- rowSums(pa_data[, c("mod_min", "vig_min")], na.rm = T)

# 步骤5：创建 pa150ggg 变量
pa_data$pa150ggg <- ifelse((pa_data$pa_min < 150) & !is.na(pa_data$pa_min), 0, 1)
pa_data$pa150ggg[is.na(pa_data$pa_min)] <- NA

# 步骤6：频率统计
pa150ggg_freq <- table(pa_data$pa150ggg, useNA = "ifany")
pa150ggg_prop <- prop.table(pa150ggg_freq) * 100

# 打印结果
print(pa150ggg_freq)
print(pa150ggg_prop)

names(pa_data)
names(pa_data)[c(1,8,9)] <- c("eid","PA_min","PA_mod_vig_150")
ukb_beverage_covariates_data <- merge(ukb_beverage_covariates_data,pa_data[,c(1,8,9)],all.x=T)
names(ukb_beverage_covariates_data)


#total energy intake
total_energy_intake_data <- ukb_beverage_covariates_data[ukb_beverage_covariates_data$eid %in% ukb_beverage_data$eid, ]
setDT(total_energy_intake_data)
energy_cols <- grep("energy_f100002", names(total_energy_intake_data), value = TRUE)
total_energy_intake_data[, total_energy_intake := rowMeans(.SD, na.rm = TRUE), .SDcols = energy_cols]
print(total_energy_intake_data[, .(eid, total_energy_intake)])

names(total_energy_intake_data)
ukb_beverage_covariates_data <- merge(ukb_beverage_covariates_data,total_energy_intake_data[,c(1,94)],all.x=T)

#covariates_supplement_data
covariates_supplement_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/covariates_supplement_data.csv")
names(covariates_supplement_data) <- c("eid","qualifications","age_completed_full_time_education","household_income",
                                       "energy_from_beverages_instance0","energy_from_beverages_instance1","energy_from_beverages_instance2","energy_from_beverages_instance3","energy_from_beverages_instance4",
                                       "total_weight_beverages_only_instance0","total_weight_beverages_only_instance1","total_weight_beverages_only_instance2","total_weight_beverages_only_instance3","total_weight_beverages_only_instance4")

energy_cols <- grep("^energy_from_beverages_instance", names(covariates_supplement_data), value = TRUE)
covariates_supplement_data$energy_from_beverages <- apply(
  covariates_supplement_data[, energy_cols], 
  1, 
  function(x) {
    # 检查是否所有值都是NA
    if(all(is.na(x))) {
      return(NA)
    } else {
      # 如果不是全NA，则计算平均值，忽略NA
      return(mean(x, na.rm = TRUE))
    }
  }
)
summary(covariates_supplement_data$energy_from_beverages)

weight_cols <- grep("^total_weight_beverages_only_instance", names(covariates_supplement_data), value = TRUE)

covariates_supplement_data$total_weight_beverages_only <- apply(
  covariates_supplement_data[, weight_cols], 
  1, 
  function(x) {
    if(all(is.na(x))) {
      return(NA)
    } else {
      return(mean(x, na.rm = TRUE))
    }
  }
)
summary(covariates_supplement_data$total_weight_beverages_only)

names(covariates_supplement_data)
ukb_beverage_covariates_data <- merge(ukb_beverage_covariates_data,covariates_supplement_data[,c(1:3,15,16)],all.x=T)

########################################Covariates Used################################################
names(ukb_beverage_covariates_data)
covariates_data <- ukb_beverage_covariates_data[,c("eid","age_at_recruitment_f21022_0_0","sex_f31_0_0","ethnic_background_f21000_0_0",
                                                   "qualifications","age_completed_full_time_education","average_total_household_income_before_tax_f738_0_0",
                                                   "body_mass_index_bmi_f21001_0_0","smoking_status_f20116_0_0","alcohol_intake_frequency_f1558_0_0","diet_score","total_alcohol",
                                                   "PA_min","PA_mod_vig_150","overall_health_rating_f2178_0_0")]
names(covariates_data)

smoking_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/Smoking_data_participant.csv")
names(smoking_data) <- c("eid","smoking_status","current_num_smk","started_age_smk","stopped_age_smk")
smoking_data$smk_num[smoking_data$smoking_status==0] <- 0
smoking_data$smk_num[smoking_data$smoking_status==1] <- 10
smoking_data$smk_num[smoking_data$smoking_status==2 & (smoking_data$current_num_smk>0 & smoking_data$current_num_smk<=5)] <- 20
smoking_data$smk_num[smoking_data$smoking_status==2 & (smoking_data$current_num_smk>5 & smoking_data$current_num_smk<=10)] <- 21
smoking_data$smk_num[smoking_data$smoking_status==2 & (smoking_data$current_num_smk>10 & smoking_data$current_num_smk<=15)] <- 22
smoking_data$smk_num[smoking_data$smoking_status==2 & (smoking_data$current_num_smk>15 & smoking_data$current_num_smk<=20)] <- 23
smoking_data$smk_num[smoking_data$smoking_status==2 & (smoking_data$current_num_smk>20 & smoking_data$current_num_smk<=25)] <- 24
smoking_data$smk_num[smoking_data$smoking_status==2 & (smoking_data$current_num_smk>25)] <- 25
smoking_data$smk_num[is.na(smoking_data$smk_num)] <- 99
table(smoking_data$smk_num)
smoking_data$smk_qyr[smoking_data$smoking_status==0 | smoking_data$smoking_status==2] <- 0
smoking_data$smk_qyr[smoking_data$smoking_status==1 & ((smoking_data$stopped_age_smk-smoking_data$started_age_smk)>0 & (smoking_data$stopped_age_smk-smoking_data$started_age_smk)<5)] <- 1
smoking_data$smk_qyr[smoking_data$smoking_status==1 & ((smoking_data$stopped_age_smk-smoking_data$started_age_smk)>=5 & (smoking_data$stopped_age_smk-smoking_data$started_age_smk)<10)] <- 2
smoking_data$smk_qyr[smoking_data$smoking_status==1 & ((smoking_data$stopped_age_smk-smoking_data$started_age_smk)>=10)] <- 3
smoking_data$smk_qyr[is.na(smoking_data$smk_qyr)] <- 99
table(smoking_data$smk_qyr)


names(covariates_data) <- c("eid","age","sex","race","education","age_completed_full_time_education","household_income","bmi",
                            "smoking_status","alcohol_intake_frequency","diet_score","total_alcohol","PA_min","PA_mod_vig_150","overall_health_rating")
covariates_data$race <- ifelse(covariates_data$race %in% c(1,1001,2001,3001,4001), 0, 1)
covariates_data$education[covariates_data$education==-3] <- NA
covariates_data$education <- ifelse(grepl("1", covariates_data$education), 1, 0)
table(covariates_data$education)
covariates_data$smoking_status[covariates_data$smoking_status==-3] <- NA
covariates_data$alcohol_intake_frequency[covariates_data$alcohol_intake_frequency==-3] <- NA
covariates_data$overall_health_rating[covariates_data$overall_health_rating<0] <- NA
covariates_data$household_income[covariates_data$household_income<0] <- NA
covariates_data$bmi_cat[covariates_data$bmi>=18.5 & covariates_data$bmi<25] <- 0
covariates_data$bmi_cat[covariates_data$bmi<18.5] <- 1
covariates_data$bmi_cat[covariates_data$bmi>=25 & covariates_data$bmi<30] <- 2
covariates_data$bmi_cat[covariates_data$bmi>=30 & covariates_data$bmi<35] <- 3
covariates_data$bmi_cat[covariates_data$bmi>=35] <- 4
table(covariates_data$bmi_cat)

str(covariates_data)
covariates_data$sex <- factor(covariates_data$sex)
covariates_data$race <- factor(covariates_data$race)
covariates_data$bmi_cat <- factor(covariates_data$bmi_cat)
covariates_data$education <- factor(covariates_data$education)
covariates_data$household_income <- factor(covariates_data$household_income)
covariates_data$PA_mod_vig_150 <- factor(covariates_data$PA_mod_vig_150)
covariates_data$smoking_status <- factor(covariates_data$smoking_status)
covariates_data$alcohol_intake_frequency  <- factor(covariates_data$alcohol_intake_frequency)
covariates_data$overall_health_rating <- factor(covariates_data$overall_health_rating)
covariates_data <- merge(covariates_data,smoking_data[c("eid","smk_num","smk_qyr")],all.x=T)
names(covariates_data)
covariates_data <- covariates_data[,-c("age_completed_full_time_education")]



library(mice)
setDT(covariates_data)
factor_cols <- c("sex","race", "education","household_income","bmi_cat", "smoking_status", "alcohol_intake_frequency","PA_mod_vig_150", "overall_health_rating","smk_num","smk_qyr")
covariates_data[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
set.seed(123)
imp_2 <- mice(covariates_data, m = 1, maxit = 5, seed = 123)
summary(imp_2)
imputed_data_2 <- as.data.table(complete(imp_2, 1))
imputed_data_2[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
str(imputed_data_2)