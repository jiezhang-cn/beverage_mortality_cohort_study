water_turnover_calculate_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/water_turnover_calculate_data_participant.csv")
names(water_turnover_calculate_data) <- c("eid","age","sex","baseline_date","diet_questionnaire_date0",
                                          "diet_questionnaire_date1","diet_questionnaire_date2","diet_questionnaire_date3",
                                          "diet_questionnaire_date4","height","weight",
                                          "fat_intake0","fat_intake1","fat_intake2","fat_intake3","fat_intake4",
                                          "protein_intake0","protein_intake1","protein_intake2","protein_intake3","protein_intake4",
                                          "carbo_intake0","carbo_intake1","carbo_intake2","carbo_intake3","carbo_intake4",
                                          "vig_pa0","vig_pa1","vig_pa2","vig_pa3","vig_pa4",
                                          "mod_pa0","mod_pa1","mod_pa2","mod_pa3","mod_pa4",
                                          "lig_pa0","lig_pa1","lig_pa2","lig_pa3","lig_pa4","fat_free_mass")


str(water_turnover_calculate_data)

aaa <- water_turnover_calculate_data
# 将字符串日期转换为日期对象
aaa$baseline_date <- as.Date(aaa$baseline_date)
aaa$diet_questionnaire_date0 <- as.Date(gsub("T.*$", "", aaa$diet_questionnaire_date0))
aaa$diet_questionnaire_date1 <- as.Date(gsub("T.*$", "", aaa$diet_questionnaire_date1))
aaa$diet_questionnaire_date2 <- as.Date(gsub("T.*$", "", aaa$diet_questionnaire_date2))
aaa$diet_questionnaire_date3 <- as.Date(gsub("T.*$", "", aaa$diet_questionnaire_date3))
aaa$diet_questionnaire_date4 <- as.Date(gsub("T.*$", "", aaa$diet_questionnaire_date4))

# 计算每个饮食问卷日期与基准日期之间的天数差异
aaa$Dif0_day <- as.numeric(difftime(aaa$diet_questionnaire_date0, aaa$baseline_date, units = "days"))
aaa$Dif1_day <- as.numeric(difftime(aaa$diet_questionnaire_date1, aaa$baseline_date, units = "days"))
aaa$Dif2_day <- as.numeric(difftime(aaa$diet_questionnaire_date2, aaa$baseline_date, units = "days"))
aaa$Dif3_day <- as.numeric(difftime(aaa$diet_questionnaire_date3, aaa$baseline_date, units = "days"))
aaa$Dif4_day <- as.numeric(difftime(aaa$diet_questionnaire_date4, aaa$baseline_date, units = "days"))

# 计算不同时间点的年龄
aaa$age0 <- aaa$Dif0_day/365.25 + aaa$age
aaa$age1 <- aaa$Dif1_day/365.25 + aaa$age
aaa$age2 <- aaa$Dif2_day/365.25 + aaa$age
aaa$age3 <- aaa$Dif3_day/365.25 + aaa$age
aaa$age4 <- aaa$Dif4_day/365.25 + aaa$age



# Mifflin-St. Jeor Basal metabolic rate calculations
# For females (sex=0)
aaa$BMR_M0 <- ifelse(aaa$sex == 0, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age0 - 161, NA)
aaa$BMR_M1 <- ifelse(aaa$sex == 0, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age1 - 161, NA)
aaa$BMR_M2 <- ifelse(aaa$sex == 0, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age2 - 161, NA)
aaa$BMR_M3 <- ifelse(aaa$sex == 0, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age3 - 161, NA)
aaa$BMR_M4 <- ifelse(aaa$sex == 0, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age4 - 161, NA)

# For males (sex=1)
aaa$BMR_M0 <- ifelse(aaa$sex == 1, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age0 + 5, aaa$BMR_M0)
aaa$BMR_M1 <- ifelse(aaa$sex == 1, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age1 + 5, aaa$BMR_M1)
aaa$BMR_M2 <- ifelse(aaa$sex == 1, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age2 + 5, aaa$BMR_M2)
aaa$BMR_M3 <- ifelse(aaa$sex == 1, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age3 + 5, aaa$BMR_M3)
aaa$BMR_M4 <- ifelse(aaa$sex == 1, 10*aaa$weight + 6.25*aaa$height - 5*aaa$age4 + 5, aaa$BMR_M4)

# 首先将摄入的脂肪、蛋白质和碳水化合物（以g为单位）转换为能量（kcal）
# 脂肪: 1g = 9 kcal, 蛋白质: 1g = 4 kcal, 碳水化合物: 1g = 4 kcal

# 计算能量摄入（Energy intake）
aaa$Fat_energy0 <- ifelse(!is.na(aaa$fat_intake0), aaa$fat_intake0 * 9, NA)
aaa$Fat_energy1 <- ifelse(!is.na(aaa$fat_intake1), aaa$fat_intake1 * 9, NA)
aaa$Fat_energy2 <- ifelse(!is.na(aaa$fat_intake2), aaa$fat_intake2 * 9, NA)
aaa$Fat_energy3 <- ifelse(!is.na(aaa$fat_intake3), aaa$fat_intake3 * 9, NA)
aaa$Fat_energy4 <- ifelse(!is.na(aaa$fat_intake4), aaa$fat_intake4 * 9, NA)

aaa$Pro_energy0 <- ifelse(!is.na(aaa$protein_intake0), aaa$protein_intake0 * 4, NA)
aaa$Pro_energy1 <- ifelse(!is.na(aaa$protein_intake1), aaa$protein_intake1 * 4, NA)
aaa$Pro_energy2 <- ifelse(!is.na(aaa$protein_intake2), aaa$protein_intake2 * 4, NA)
aaa$Pro_energy3 <- ifelse(!is.na(aaa$protein_intake3), aaa$protein_intake3 * 4, NA)
aaa$Pro_energy4 <- ifelse(!is.na(aaa$protein_intake4), aaa$protein_intake4 * 4, NA)

aaa$Carbo_energy0 <- ifelse(!is.na(aaa$carbo_intake0), aaa$carbo_intake0 * 4, NA)
aaa$Carbo_energy1 <- ifelse(!is.na(aaa$carbo_intake1), aaa$carbo_intake1 * 4, NA)
aaa$Carbo_energy2 <- ifelse(!is.na(aaa$carbo_intake2), aaa$carbo_intake2 * 4, NA)
aaa$Carbo_energy3 <- ifelse(!is.na(aaa$carbo_intake3), aaa$carbo_intake3 * 4, NA)
aaa$Carbo_energy4 <- ifelse(!is.na(aaa$carbo_intake4), aaa$carbo_intake4 * 4, NA)

# 计算食物热效应 (THERMIC EFFECT OF FEEDING, TEF)
# 脂肪TEF系数: 2.5%, 蛋白质TEF系数: 25%, 碳水化合物TEF系数: 7.5%
aaa$TEF0 <- ifelse(!is.na(aaa$Fat_energy0) & !is.na(aaa$Pro_energy0) & !is.na(aaa$Carbo_energy0),
                   0.025*aaa$Fat_energy0 + 0.25*aaa$Pro_energy0 + 0.075*aaa$Carbo_energy0, NA)

aaa$TEF1 <- ifelse(!is.na(aaa$Fat_energy1) & !is.na(aaa$Pro_energy1) & !is.na(aaa$Carbo_energy1),
                   0.025*aaa$Fat_energy1 + 0.25*aaa$Pro_energy1 + 0.075*aaa$Carbo_energy1, NA)

aaa$TEF2 <- ifelse(!is.na(aaa$Fat_energy2) & !is.na(aaa$Pro_energy2) & !is.na(aaa$Carbo_energy2),
                   0.025*aaa$Fat_energy2 + 0.25*aaa$Pro_energy2 + 0.075*aaa$Carbo_energy2, NA)

aaa$TEF3 <- ifelse(!is.na(aaa$Fat_energy3) & !is.na(aaa$Pro_energy3) & !is.na(aaa$Carbo_energy3),
                   0.025*aaa$Fat_energy3 + 0.25*aaa$Pro_energy3 + 0.075*aaa$Carbo_energy3, NA)

aaa$TEF4 <- ifelse(!is.na(aaa$Fat_energy4) & !is.na(aaa$Pro_energy4) & !is.na(aaa$Carbo_energy4),
                   0.025*aaa$Fat_energy4 + 0.25*aaa$Pro_energy4 + 0.075*aaa$Carbo_energy4, NA)



library(dplyr)

# 确保转换函数正确
convert_to_hours <- function(code) {
  case_when(
    code == 0 ~ 0,
    code == 10 ~ 5/60,
    code == 12 ~ 1.5,
    code == 24 ~ 3,
    code == 46 ~ 5,
    code == 600 ~ 7,
    code == 1030 ~ 20/60,
    code == 3060 ~ 45/60,
    TRUE ~ NA_real_
  )
}

# 对每个时间点分开处理，避免循环中的引用问题
for (i in 0:4) {
  # 定义列名
  vig_col <- paste0("vig_pa", i)
  mod_col <- paste0("mod_pa", i)
  lig_col <- paste0("lig_pa", i)
  
  # 创建临时数据框以添加计算列
  temp_df <- data.frame(row_id = 1:nrow(aaa))
  
  # 转换时间编码为小时 - 检查列是否存在
  if (vig_col %in% names(aaa)) {
    temp_df$vig_hours <- convert_to_hours(aaa[[vig_col]])
  } else {
    temp_df$vig_hours <- 0  # 默认为0
  }
  
  if (mod_col %in% names(aaa)) {
    temp_df$mod_hours <- convert_to_hours(aaa[[mod_col]])
  } else {
    temp_df$mod_hours <- 0  # 默认为0
  }
  
  if (lig_col %in% names(aaa)) {
    temp_df$lig_hours <- convert_to_hours(aaa[[lig_col]])
  } else {
    temp_df$lig_hours <- 0  # 默认为0
  }
  
  # 计算每种活动的MET小时
  temp_df$vig_met_hours <- temp_df$vig_hours * 8
  temp_df$mod_met_hours <- temp_df$mod_hours * 4
  temp_df$lig_met_hours <- temp_df$lig_hours * 2
  
  # 检查是否所有活动时间都是NA
  all_na_hours <- is.na(temp_df$vig_hours) & is.na(temp_df$mod_hours) & is.na(temp_df$lig_hours)
  
  # 计算总活动时间 - 只有当至少有一个非NA值时才计算
  temp_df$total_activity_hours <- ifelse(all_na_hours,
                                         NA,
                                         pmin(rowSums(
                                           cbind(temp_df$vig_hours, temp_df$mod_hours, temp_df$lig_hours),
                                           na.rm = TRUE), 24))
  
  # 计算休息时间和MET - 只在总活动时间有效时才计算
  temp_df$rest_hours <- ifelse(is.na(temp_df$total_activity_hours),
                               NA,
                               24 - temp_df$total_activity_hours)
  
  temp_df$rest_met_hours <- temp_df$rest_hours * 1
  
  # 创建包含所有MET小时的数据框
  met_hours_df <- data.frame(
    vig = temp_df$vig_met_hours,
    mod = temp_df$mod_met_hours,
    lig = temp_df$lig_met_hours,
    rest = temp_df$rest_met_hours
  )
  
  # 检查每行是否全部为NA
  all_na_mets <- apply(met_hours_df, 1, function(x) all(is.na(x)))
  
  # 计算总MET小时 - 忽略NA值
  temp_df$total_met_hours <- rowSums(met_hours_df, na.rm = TRUE)
  
  # 如果所有MET时间都是NA，则总和也设为NA
  temp_df$total_met_hours[all_na_mets] <- NA
  
  # 计算每日平均MET值
  temp_df$PA_MET <- temp_df$total_met_hours / 24
  
  # 将结果添加到原始数据框
  aaa[[paste0("vig_hours", i)]] <- temp_df$vig_hours
  aaa[[paste0("mod_hours", i)]] <- temp_df$mod_hours
  aaa[[paste0("lig_hours", i)]] <- temp_df$lig_hours
  aaa[[paste0("vig_met_hours", i)]] <- temp_df$vig_met_hours
  aaa[[paste0("mod_met_hours", i)]] <- temp_df$mod_met_hours
  aaa[[paste0("lig_met_hours", i)]] <- temp_df$lig_met_hours
  aaa[[paste0("total_activity_hours", i)]] <- temp_df$total_activity_hours
  aaa[[paste0("rest_hours", i)]] <- temp_df$rest_hours
  aaa[[paste0("rest_met_hours", i)]] <- temp_df$rest_met_hours
  aaa[[paste0("total_met_hours", i)]] <- temp_df$total_met_hours
  aaa[[paste0("PA_MET", i)]] <- temp_df$PA_MET
}

# 计算PA相关能量消耗 - 处理NA值
calc_PA_en <- function(PA_MET, weight) {
  ifelse(is.na(PA_MET) | is.na(weight), NA, PA_MET * 3.5 * weight / 200)
}

aaa$PA_en0 <- calc_PA_en(aaa$PA_MET0, aaa$weight)
aaa$PA_en1 <- calc_PA_en(aaa$PA_MET1, aaa$weight)
aaa$PA_en2 <- calc_PA_en(aaa$PA_MET2, aaa$weight)
aaa$PA_en3 <- calc_PA_en(aaa$PA_MET3, aaa$weight)
aaa$PA_en4 <- calc_PA_en(aaa$PA_MET4, aaa$weight)

# 总能量消耗 - 处理NA值
# 如果任何组成部分是NA，则总能量消耗也是NA
calc_TEE <- function(PA_en, TEF, BMR) {
  ifelse(is.na(PA_en) | is.na(TEF) | is.na(BMR), NA, PA_en + TEF + BMR)
}

aaa$TEE_M0 <- calc_TEE(aaa$PA_en0, aaa$TEF0, aaa$BMR_M0)
aaa$TEE_M1 <- calc_TEE(aaa$PA_en1, aaa$TEF1, aaa$BMR_M1)
aaa$TEE_M2 <- calc_TEE(aaa$PA_en2, aaa$TEF2, aaa$BMR_M2)
aaa$TEE_M3 <- calc_TEE(aaa$PA_en3, aaa$TEF3, aaa$BMR_M3)
aaa$TEE_M4 <- calc_TEE(aaa$PA_en4, aaa$TEF4, aaa$BMR_M4)

# PA水平 - 处理NA值
calc_PA_level <- function(TEE, BMR) {
  ifelse(is.na(TEE) | is.na(BMR) | BMR == 0, NA, TEE / BMR)
}

aaa$PA_level_M0 <- calc_PA_level(aaa$TEE_M0, aaa$BMR_M0)
aaa$PA_level_M1 <- calc_PA_level(aaa$TEE_M1, aaa$BMR_M1)
aaa$PA_level_M2 <- calc_PA_level(aaa$TEE_M2, aaa$BMR_M2)
aaa$PA_level_M3 <- calc_PA_level(aaa$TEE_M3, aaa$BMR_M3)
aaa$PA_level_M4 <- calc_PA_level(aaa$TEE_M4, aaa$BMR_M4)

###########################################Water turnover calculation##########################################
assement_center <- read.csv("C:/Users/张杰/Desktop/Cohort Data/UKB/UKB_data/assement_center", sep="")
aaa <- merge(aaa,assement_center,all.x = T)
names(aaa)[148] <- "center"
aaa <- aaa[!is.na(aaa$diet_questionnaire_date0) | !is.na(aaa$diet_questionnaire_date1) | !is.na(aaa$diet_questionnaire_date2) | !is.na(aaa$diet_questionnaire_date3) | !is.na(aaa$diet_questionnaire_date4), ]
str(aaa)

ukb_GSODR_info <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_GSODR_info.csv")
str(ukb_GSODR_info)

library(dplyr)
library(tidyr)

# 1. 为每个问卷日期提取年份
aaa <- aaa %>%
  mutate(
    year0 = if_else(!is.na(diet_questionnaire_date0), as.integer(format(diet_questionnaire_date0, "%Y")), NA_integer_),
    year1 = if_else(!is.na(diet_questionnaire_date1), as.integer(format(diet_questionnaire_date1, "%Y")), NA_integer_),
    year2 = if_else(!is.na(diet_questionnaire_date2), as.integer(format(diet_questionnaire_date2, "%Y")), NA_integer_),
    year3 = if_else(!is.na(diet_questionnaire_date3), as.integer(format(diet_questionnaire_date3, "%Y")), NA_integer_),
    year4 = if_else(!is.na(diet_questionnaire_date4), as.integer(format(diet_questionnaire_date4, "%Y")), NA_integer_)
  )

# 2. 获取每个center和年份组合的气象数据平均值
center_year_avg <- ukb_GSODR_info %>%
  group_by(center, YEAR) %>%
  summarize(
    TEMP_avg = mean(TEMP, na.rm = TRUE),
    RH_avg = mean(RH, na.rm = TRUE),
    ELEVATION_avg = mean(ELEVATION, na.rm = TRUE),
    .groups = "drop"
  )

# 3. 创建函数获取特定center和year的气象数据
get_weather_data <- function(center_val, year_val, center_year_data) {
  if(is.na(center_val) | is.na(year_val)) {
    return(list(TEMP = NA_real_, RH = NA_real_, ELEVATION = NA_real_))
  }
  
  result <- center_year_data %>%
    filter(center == center_val & YEAR == year_val)
  
  if(nrow(result) == 0) {
    return(list(TEMP = NA_real_, RH = NA_real_, ELEVATION = NA_real_))
  }
  
  return(list(
    TEMP = result$TEMP_avg[1],
    RH = result$RH_avg[1],
    ELEVATION = result$ELEVATION_avg[1]
  ))
}

# 4. 为每个问卷日期添加相应的气象数据
for(i in 0:4) {
  # 创建一个临时数据框存储计算结果
  temp_df <- data.frame(
    eid = aaa$eid,
    center = aaa$center,
    year = aaa[[paste0("year", i)]]
  )
  
  # 与center_year_avg连接
  temp_df <- temp_df %>%
    left_join(center_year_avg, by = c("center", "year" = "YEAR"))
  
  # 将结果添加到aaa数据框
  aaa[[paste0("TEMP", i)]] <- temp_df$TEMP_avg
  aaa[[paste0("RH", i)]] <- temp_df$RH_avg
  aaa[[paste0("ELEVATION", i)]] <- temp_df$ELEVATION_avg
}

# 5. 检查结果
head(aaa[, c("eid", "center", "year0", "TEMP0", "RH0", "ELEVATION0", 
             "year1", "TEMP1", "RH1", "ELEVATION1")])




# Water turnover model A
aaa$WTa_M0 <- (1076*aaa$PA_level_M0) + (14.34*aaa$weight) + (374.9*aaa$sex) + (5.823*aaa$RH0) + 
  (1070*0) + (104.6*0) + (0.4726*aaa$ELEVATION0) - (0.3529*aaa$age0*aaa$age0) + 
  (24.78*aaa$age0) + (1.865*aaa$TEMP0*aaa$TEMP0) - (19.66*aaa$TEMP0) - 713.1

aaa$WTa_M1 <- (1076*aaa$PA_level_M1) + (14.34*aaa$weight) + (374.9*aaa$sex) + (5.823*aaa$RH1) + 
  (1070*0) + (104.6*0) + (0.4726*aaa$ELEVATION1) - (0.3529*aaa$age1*aaa$age1) + 
  (24.78*aaa$age1) + (1.865*aaa$TEMP1*aaa$TEMP1) - (19.66*aaa$TEMP1) - 713.1

aaa$WTa_M2 <- (1076*aaa$PA_level_M2) + (14.34*aaa$weight) + (374.9*aaa$sex) + (5.823*aaa$RH2) + 
  (1070*0) + (104.6*0) + (0.4726*aaa$ELEVATION2) - (0.3529*aaa$age2*aaa$age2) + 
  (24.78*aaa$age2) + (1.865*aaa$TEMP2*aaa$TEMP2) - (19.66*aaa$TEMP2) - 713.1

aaa$WTa_M3 <- (1076*aaa$PA_level_M3) + (14.34*aaa$weight) + (374.9*aaa$sex) + (5.823*aaa$RH3) + 
  (1070*0) + (104.6*0) + (0.4726*aaa$ELEVATION3) - (0.3529*aaa$age3*aaa$age3) + 
  (24.78*aaa$age3) + (1.865*aaa$TEMP3*aaa$TEMP3) - (19.66*aaa$TEMP3) - 713.1

aaa$WTa_M4 <- (1076*aaa$PA_level_M4) + (14.34*aaa$weight) + (374.9*aaa$sex) + (5.823*aaa$RH4) + 
  (1070*0) + (104.6*0) + (0.4726*aaa$ELEVATION4) - (0.3529*aaa$age4*aaa$age4) + 
  (24.78*aaa$age4) + (1.865*aaa$TEMP4*aaa$TEMP4) - (19.66*aaa$TEMP4) - 713.1

# 检查结果
summary(aaa[, c("WTa_M0", "WTa_M1", "WTa_M2", "WTa_M3", "WTa_M4")])
 
# Dietary water calculations (model A) Nutrients 2016, 8(10), 630  https://doi.org/10.3390/nu8100630
aaa$Dietary_watera0 <- aaa$WTa_M0 * 0.85
aaa$Dietary_watera1 <- aaa$WTa_M1 * 0.85
aaa$Dietary_watera2 <- aaa$WTa_M2 * 0.85
aaa$Dietary_watera3 <- aaa$WTa_M3 * 0.85
aaa$Dietary_watera4 <- aaa$WTa_M4 * 0.85

aaa$Dietary_watera <- rowMeans(aaa[, c("Dietary_watera0", "Dietary_watera1", "Dietary_watera2", 
                                       "Dietary_watera3", "Dietary_watera4")], na.rm = TRUE)

# Drinking water calculations (model A)
aaa$Drinking_watera0 <- aaa$WTa_M0 * 0.85 * 0.73
aaa$Drinking_watera1 <- aaa$WTa_M1 * 0.85 * 0.73
aaa$Drinking_watera2 <- aaa$WTa_M2 * 0.85 * 0.73
aaa$Drinking_watera3 <- aaa$WTa_M3 * 0.85 * 0.73
aaa$Drinking_watera4 <- aaa$WTa_M4 * 0.85 * 0.73

aaa$Drinking_watera <- rowMeans(aaa[, c("Drinking_watera0", "Drinking_watera1", "Drinking_watera2", 
                                        "Drinking_watera3", "Drinking_watera4")], na.rm = TRUE)



# Water turnover model B
aaa$WTb_M0 <- (861.9*aaa$PA_level_M0) + (37.34*aaa$fat_free_mass) + (4.288*aaa$RH0) + (699.7*0) + 
  (105.0*0) + (0.5140*aaa$ELEVATION0) - (0.3625*aaa$age0*aaa$age0) + (29.42*aaa$age0) + 
  (1.937*aaa$TEMP0*aaa$TEMP0) - (23.15*aaa$TEMP0) - 984.8

aaa$WTb_M1 <- (861.9*aaa$PA_level_M1) + (37.34*aaa$fat_free_mass) + (4.288*aaa$RH1) + (699.7*0) + 
  (105.0*0) + (0.5140*aaa$ELEVATION1) - (0.3625*aaa$age1*aaa$age1) + (29.42*aaa$age1) + 
  (1.937*aaa$TEMP1*aaa$TEMP1) - (23.15*aaa$TEMP1) - 984.8

aaa$WTb_M2 <- (861.9*aaa$PA_level_M2) + (37.34*aaa$fat_free_mass) + (4.288*aaa$RH2) + (699.7*0) + 
  (105.0*0) + (0.5140*aaa$ELEVATION2) - (0.3625*aaa$age2*aaa$age2) + (29.42*aaa$age2) + 
  (1.937*aaa$TEMP2*aaa$TEMP2) - (23.15*aaa$TEMP2) - 984.8

aaa$WTb_M3 <- (861.9*aaa$PA_level_M3) + (37.34*aaa$fat_free_mass) + (4.288*aaa$RH3) + (699.7*0) + 
  (105.0*0) + (0.5140*aaa$ELEVATION3) - (0.3625*aaa$age3*aaa$age3) + (29.42*aaa$age3) + 
  (1.937*aaa$TEMP3*aaa$TEMP3) - (23.15*aaa$TEMP3) - 984.8

aaa$WTb_M4 <- (861.9*aaa$PA_level_M4) + (37.34*aaa$fat_free_mass) + (4.288*aaa$RH4) + (699.7*0) + 
  (105.0*0) + (0.5140*aaa$ELEVATION4) - (0.3625*aaa$age4*aaa$age4) + (29.42*aaa$age4) + 
  (1.937*aaa$TEMP4*aaa$TEMP4) - (23.15*aaa$TEMP4) - 984.8

# 检查结果
summary(aaa[, c("WTb_M0", "WTb_M1", "WTb_M2", "WTb_M3", "WTb_M4")])

# Dietary water calculations (model B)
aaa$Dietary_waterb0 <- aaa$WTb_M0 * 0.85
aaa$Dietary_waterb1 <- aaa$WTb_M1 * 0.85
aaa$Dietary_waterb2 <- aaa$WTb_M2 * 0.85
aaa$Dietary_waterb3 <- aaa$WTb_M3 * 0.85
aaa$Dietary_waterb4 <- aaa$WTb_M4 * 0.85

aaa$Dietary_waterb <- rowMeans(aaa[, c("Dietary_waterb0", "Dietary_waterb1", "Dietary_waterb2", 
                                       "Dietary_waterb3", "Dietary_waterb4")], na.rm = TRUE)

# Drinking water calculations (model B)
aaa$Drinking_waterb0 <- aaa$WTb_M0 * 0.85 * 0.73
aaa$Drinking_waterb1 <- aaa$WTb_M1 * 0.85 * 0.73
aaa$Drinking_waterb2 <- aaa$WTb_M2 * 0.85 * 0.73
aaa$Drinking_waterb3 <- aaa$WTb_M3 * 0.85 * 0.73
aaa$Drinking_waterb4 <- aaa$WTb_M4 * 0.85 * 0.73

aaa$Drinking_waterb <- rowMeans(aaa[, c("Drinking_waterb0", "Drinking_waterb1", "Drinking_waterb2", 
                                        "Drinking_waterb3", "Drinking_waterb4")], na.rm = TRUE)

data.table::fwrite(aaa, "C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_water_turnover.csv")
