library(survival)
ukb_age_related_diseases_dataset <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/age_related_diseases_dataset_participant.csv")
str(ukb_age_related_diseases_dataset)

library(dplyr)

# 正确的重命名语法：新名字 = 旧名字
ukb_diseases <- ukb_age_related_diseases_dataset %>%
  rename(
    # 基本信息
    eid = Participant.ID,
    date_lost_followup = Date.lost.to.follow.up,
    date_assessment_center = Date.of.attending.assessment.centre...Instance.0,
    
    # 痴呆症相关
    date_dementia_all = Date.of.all.cause.dementia.report,
    date_alzheimer = Date.of.alzheimer.s.disease.report,
    date_vascular_dementia = Date.of.vascular.dementia.report,
    date_frontotemporal_dementia = Date.of.frontotemporal.dementia.report,
    
    # 帕金森病相关
    date_parkinsonism_all = Date.of.all.cause.parkinsonism.report,
    date_parkinson = Date.of.parkinson.s.disease.report,
    date_psp = Date.of.progressive.supranuclear.palsy.report,
    date_msa = Date.of.multiple.system.atrophy.report,
    
    # 肺部疾病
    date_emphysema = Date.J43.first.reported..emphysema.,
    date_copd = Date.J44.first.reported..other.chronic.obstructive.pulmonary.disease.,
    
    # 肝脏疾病
    date_alcoholic_liver = Date.K70.first.reported..alcoholic.liver.disease.,
    date_chronic_hepatitis = Date.K73.first.reported..chronic.hepatitis..not.elsewhere.classified.,
    date_liver_cirrhosis = Date.K74.first.reported..fibrosis.and.cirrhosis.of.liver.,
    date_inflammatory_liver = Date.K75.first.reported..other.inflammatory.liver.diseases.,
    date_other_liver = Date.K76.first.reported..other.diseases.of.liver.,
    
    # 关节疾病
    date_seropos_ra = Date.M05.first.reported..seropositive.rheumatoid.arthritis.,
    date_other_ra = Date.M06.first.reported..other.rheumatoid.arthritis.,
    date_gout = Date.M10.first.reported..gout.,
    date_polyarthrosis = Date.M15.first.reported..polyarthrosis.,
    date_hip_arthrosis = Date.M16.first.reported..coxarthrosis..arthrosis.of.hip..,
    date_knee_arthrosis = Date.M17.first.reported..gonarthrosis..arthrosis.of.knee..,
    date_hand_arthrosis = Date.M18.first.reported..arthrosis.of.first.carpometacarpal.joint.,
    date_other_arthrosis = Date.M19.first.reported..other.arthrosis.,
    
    # 骨质疏松
    date_osteoporosis_fracture = Date.M80.first.reported..osteoporosis.with.pathological.fracture.,
    date_osteoporosis_no_fracture = Date.M81.first.reported..osteoporosis.without.pathological.fracture.,
    
    # 糖尿病
    date_t2dm = Date.E11.first.reported..non.insulin.dependent.diabetes.mellitus.,
    
    # 冠心病 (CAD) - I20-I25
    date_angina = Date.I20.first.reported..angina.pectoris.,
    date_ami = Date.I21.first.reported..acute.myocardial.infarction.,
    date_subsequent_mi = Date.I22.first.reported..subsequent.myocardial.infarction.,
    date_mi_complications = Date.I23.first.reported..certain.current.complications.following.acute.myocardial.infarction.,
    date_acute_ihd = Date.I24.first.reported..other.acute.ischaemic.heart.diseases.,
    date_chronic_ihd = Date.I25.first.reported..chronic.ischaemic.heart.disease.,
    
    # 心力衰竭 (HF) - I50
    date_heart_failure = Date.I50.first.reported..heart.failure.,
    
    # 外周动脉疾病 (PAD) - I70, I73
    date_atherosclerosis = Date.I70.first.reported..atherosclerosis.,
    date_pvd = Date.I73.first.reported..other.peripheral.vascular.diseases.,
    
    # 中风 (Stroke) - Data-field Category 43
    date_stroke = Date.of.stroke
  )

# 单独处理癌症相关变量
for(i in 0:21) {
  # 重命名癌症日期
  old_name_date <- paste0("Date.of.cancer.diagnosis...Instance.", i)
  new_name_date <- paste0("date_cancer_", i)
  if(old_name_date %in% names(ukb_diseases)) {
    names(ukb_diseases)[names(ukb_diseases) == old_name_date] <- new_name_date
  }
  
  # 重命名癌症类型
  old_name_type <- paste0("Type.of.cancer..ICD10...Instance.", i)
  new_name_type <- paste0("cancer_type_icd10_", i)
  if(old_name_type %in% names(ukb_diseases)) {
    names(ukb_diseases)[names(ukb_diseases) == old_name_type] <- new_name_type
  }
}


# 检查结果
str(ukb_diseases)
names(beverage_20w_data)
ukb_diseases <- merge(beverage_20w_data[,c(1,35)],ukb_diseases,all.x = T)



###########################################diseases incidence######################################################
# 先检查数据维度
dim(ukb_diseases)

# 优化版本 - 避免rowwise操作
library(dplyr)
library(lubridate)

# 首先转换所有日期变量为Date格式
date_cols <- c("date_alzheimer", "date_vascular_dementia", "date_frontotemporal_dementia",
               "date_parkinsonism_all", "date_parkinson", "date_psp", "date_msa",
               "date_emphysema", "date_copd",
               "date_alcoholic_liver", "date_chronic_hepatitis", "date_liver_cirrhosis", 
               "date_inflammatory_liver", "date_other_liver",
               "date_seropos_ra", "date_other_ra",
               "date_gout", "date_osteoporosis_fracture", "date_osteoporosis_no_fracture",
               "date_polyarthrosis", "date_hip_arthrosis", "date_knee_arthrosis",
               "date_hand_arthrosis", "date_other_arthrosis")

# 批量转换日期
ukb_diseases <- ukb_diseases %>%
  mutate(across(all_of(date_cols), ~ ifelse(.x == "", NA, .x))) %>%
  mutate(across(all_of(date_cols), ~ as.Date(.x)))

# 使用pmin函数代替rowwise + min，这样更快
ukb_diseases <- ukb_diseases %>%
  mutate(
    # 创建合并的疾病日期（使用pmin更高效）
    date_emphysema_copd = pmin(date_emphysema, date_copd, na.rm = TRUE),
    date_emphysema_copd = ifelse(is.na(date_emphysema) & is.na(date_copd), NA, date_emphysema_copd),
    date_emphysema_copd = as.Date(date_emphysema_copd, origin = "1970-01-01"),
    
    date_chronic_liver_diseases = pmin(date_alcoholic_liver, date_chronic_hepatitis, 
                                       date_liver_cirrhosis, date_inflammatory_liver, 
                                       date_other_liver, na.rm = TRUE),
    date_chronic_liver_diseases = ifelse(is.na(date_alcoholic_liver) & is.na(date_chronic_hepatitis) & 
                                           is.na(date_liver_cirrhosis) & is.na(date_inflammatory_liver) & 
                                           is.na(date_other_liver), NA, date_chronic_liver_diseases),
    date_chronic_liver_diseases = as.Date(date_chronic_liver_diseases, origin = "1970-01-01"),
    
    date_ra = pmin(date_seropos_ra, date_other_ra, na.rm = TRUE),
    date_ra = ifelse(is.na(date_seropos_ra) & is.na(date_other_ra), NA, date_ra),
    date_ra = as.Date(date_ra, origin = "1970-01-01"),
    
    date_osteoporosis = pmin(date_osteoporosis_fracture, date_osteoporosis_no_fracture, na.rm = TRUE),
    date_osteoporosis = ifelse(is.na(date_osteoporosis_fracture) & is.na(date_osteoporosis_no_fracture), NA, date_osteoporosis),
    date_osteoporosis = as.Date(date_osteoporosis, origin = "1970-01-01"),
    
    date_osteoarthritis = pmin(date_polyarthrosis, date_hip_arthrosis, date_knee_arthrosis,
                               date_hand_arthrosis, date_other_arthrosis, na.rm = TRUE),
    date_osteoarthritis = ifelse(is.na(date_polyarthrosis) & is.na(date_hip_arthrosis) & 
                                   is.na(date_knee_arthrosis) & is.na(date_hand_arthrosis) & 
                                   is.na(date_other_arthrosis), NA, date_osteoarthritis),
    date_osteoarthritis = as.Date(date_osteoarthritis, origin = "1970-01-01")
  )

# 创建incident变量（更简洁的方式）
create_incident_var <- function(data, disease_date_col, base_date_col = "diet_questionnaire_completed_date") {
  case_when(
    is.na(data[[base_date_col]]) ~ NA_real_,
    is.na(data[[disease_date_col]]) ~ 0,
    data[[disease_date_col]] < data[[base_date_col]] ~ NA_real_,
    data[[disease_date_col]] >= data[[base_date_col]] ~ 1,
    TRUE ~ 0
  )
}

ukb_diseases <- ukb_diseases %>%
  mutate(
    incident_alzheimer = create_incident_var(., "date_alzheimer"),
    incident_vascular_dementia = create_incident_var(., "date_vascular_dementia"),
    incident_frontotemporal_dementia = create_incident_var(., "date_frontotemporal_dementia"),
    incident_parkinsonism_all = create_incident_var(., "date_parkinsonism_all"),
    incident_parkinson = create_incident_var(., "date_parkinson"),
    incident_emphysema_copd = create_incident_var(., "date_emphysema_copd"),
    incident_chronic_liver_diseases = create_incident_var(., "date_chronic_liver_diseases"),
    incident_ra = create_incident_var(., "date_ra"),
    incident_gout = create_incident_var(., "date_gout"),
    incident_osteoporosis = create_incident_var(., "date_osteoporosis"),
    incident_osteoarthritis = create_incident_var(., "date_osteoarthritis")
  )

print("Incident variables created successfully!")

# 快速检查前几个变量
table(ukb_diseases$incident_alzheimer, useNA = "always")
table(ukb_diseases$incident_emphysema_copd, useNA = "always")
table(ukb_diseases$incident_ra, useNA = "always")
table(ukb_diseases$incident_gout, useNA = "always")
table(ukb_diseases$incident_parkinsonism_all, useNA = "always")
str(ukb_diseases)


# 首先计算最大随访日期（用于censoring）
all_disease_dates <- c(ukb_diseases$date_dementia_all, 
                       ukb_diseases$date_alzheimer,
                       ukb_diseases$date_vascular_dementia,
                       ukb_diseases$date_frontotemporal_dementia,
                       ukb_diseases$date_parkinsonism_all,
                       ukb_diseases$date_parkinson,
                       ukb_diseases$date_emphysema_copd,
                       ukb_diseases$date_chronic_liver_diseases,
                       ukb_diseases$date_ra,
                       ukb_diseases$date_gout,
                       ukb_diseases$date_osteoporosis,
                       ukb_diseases$date_osteoarthritis)

max_followup_date <- max(all_disease_dates, na.rm = TRUE)
print(paste("最大随访日期:", max_followup_date))

# 创建follow_time计算函数
create_followup_time <- function(data, incident_col, disease_date_col, 
                                 base_date_col = "diet_questionnaire_completed_date",
                                 lost_followup_col = "date_lost_followup") {
  case_when(
    is.na(data[[incident_col]]) ~ NA_real_,
    is.na(data[[base_date_col]]) ~ NA_real_,
    data[[incident_col]] == 1 ~ as.numeric(data[[disease_date_col]] - data[[base_date_col]]) / 365.25,
    data[[incident_col]] == 0 & !is.na(data[[lost_followup_col]]) & 
      data[[lost_followup_col]] >= data[[base_date_col]] ~ 
      as.numeric(data[[lost_followup_col]] - data[[base_date_col]]) / 365.25,
    data[[incident_col]] == 0 & (is.na(data[[lost_followup_col]]) | 
                                   data[[lost_followup_col]] < data[[base_date_col]]) ~ 
      as.numeric(max_followup_date - data[[base_date_col]]) / 365.25,
    TRUE ~ NA_real_
  )
}

# 批量创建所有follow_time变量
ukb_diseases <- ukb_diseases %>%
  mutate(
    follow_time_alzheimer = create_followup_time(., "incident_alzheimer", "date_alzheimer"),
    follow_time_vascular_dementia = create_followup_time(., "incident_vascular_dementia", "date_vascular_dementia"),
    follow_time_frontotemporal_dementia = create_followup_time(., "incident_frontotemporal_dementia", "date_frontotemporal_dementia"),
    follow_time_parkinsonism_all = create_followup_time(., "incident_parkinsonism_all", "date_parkinsonism_all"),
    follow_time_parkinson = create_followup_time(., "incident_parkinson", "date_parkinson"),
    follow_time_emphysema_copd = create_followup_time(., "incident_emphysema_copd", "date_emphysema_copd"),
    follow_time_chronic_liver_diseases = create_followup_time(., "incident_chronic_liver_diseases", "date_chronic_liver_diseases"),
    follow_time_ra = create_followup_time(., "incident_ra", "date_ra"),
    follow_time_gout = create_followup_time(., "incident_gout", "date_gout"),
    follow_time_osteoporosis = create_followup_time(., "incident_osteoporosis", "date_osteoporosis"),
    follow_time_osteoarthritis = create_followup_time(., "incident_osteoarthritis", "date_osteoarthritis")
  )

print("Follow-up time variables created successfully!")

# 检查结果 - 汇总所有疾病的发病情况和随访时间
diseases <- c("alzheimer", "vascular_dementia", "frontotemporal_dementia", 
              "parkinsonism_all", "parkinson", "emphysema_copd", 
              "chronic_liver_diseases", "ra", "gout", "osteoporosis", "osteoarthritis")

results_summary <- data.frame(
  Disease = diseases,
  Cases = sapply(diseases, function(x) sum(ukb_diseases[[paste0("incident_", x)]] == 1, na.rm = TRUE)),
  Controls = sapply(diseases, function(x) sum(ukb_diseases[[paste0("incident_", x)]] == 0, na.rm = TRUE)),
  Excluded = sapply(diseases, function(x) sum(is.na(ukb_diseases[[paste0("incident_", x)]]))),
  Mean_Followup = sapply(diseases, function(x) round(mean(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2)),
  Median_Followup = sapply(diseases, function(x) round(median(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2)),
  Min_Followup = sapply(diseases, function(x) round(min(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2)),
  Max_Followup = sapply(diseases, function(x) round(max(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2))
)

print("疾病发病情况和随访时间汇总:")
print(results_summary)



str(ukb_diseases)

library(dplyr)
library(lubridate)


# 首先转换癌症日期和ICD-10编码为合适格式
cancer_date_cols <- paste0("date_cancer_", 0:21)
cancer_icd10_cols <- paste0("cancer_type_icd10_", 0:21)

ukb_diseases <- ukb_diseases %>%
  mutate(across(all_of(cancer_date_cols), ~ ifelse(.x == "", NA, .x))) %>%
  mutate(across(all_of(cancer_date_cols), ~ as.Date(.x))) %>%
  mutate(across(all_of(cancer_icd10_cols), ~ ifelse(.x == "", NA, .x)))

# 确保基线日期是Date格式
ukb_diseases <- ukb_diseases %>%
  mutate(
    diet_questionnaire_completed_date = as.Date(diet_questionnaire_completed_date),
    date_lost_followup = as.Date(date_lost_followup)
  )

# 定义癌症类型的ICD-10编码
cancer_definitions <- list(
  colorectal = c("C18", "C19", "C20"),
  lung = c("C33", "C34"),
  esophageal = c("C15"),
  liver = c("C22"),
  pancreatic = c("C25"),
  brain = c("C71"),
  leukemia = c("C91", "C92", "C93", "C94", "C95"),
  lymphoma = c("C81", "C82", "C83", "C84", "C85", "C86", "C88"),
  breast = c("C50"),
  ovarian = c("C56"),
  prostate = c("C61")
)

# 创建函数来检查ICD-10编码是否匹配特定癌症类型
check_cancer_type <- function(icd10_code, cancer_codes) {
  if (is.na(icd10_code)) return(FALSE)
  
  # 提取ICD-10编码的前3个字符
  icd10_prefix <- substr(icd10_code, 1, 3)
  
  return(icd10_prefix %in% cancer_codes)
}

# 为每种癌症类型创建最早诊断日期
for (cancer_type in names(cancer_definitions)) {
  print(paste("Processing", cancer_type, "cancer..."))
  
  # 创建一个矩阵来存储每个人每个instance的癌症匹配情况
  cancer_matches <- matrix(FALSE, nrow = nrow(ukb_diseases), ncol = 22)
  
  # 检查每个instance的ICD-10编码
  for (i in 0:21) {
    icd10_col <- paste0("cancer_type_icd10_", i)
    cancer_matches[, i+1] <- sapply(ukb_diseases[[icd10_col]], 
                                    function(x) check_cancer_type(x, cancer_definitions[[cancer_type]]))
  }
  
  # 为每个人找到最早的癌症诊断日期
  earliest_dates <- rep(as.Date(NA), nrow(ukb_diseases))
  
  for (row in 1:nrow(ukb_diseases)) {
    matching_instances <- which(cancer_matches[row, ])
    
    if (length(matching_instances) > 0) {
      # 获取匹配instance的日期
      matching_dates <- c()
      for (inst in matching_instances - 1) {
        date_col <- paste0("date_cancer_", inst)
        date_val <- ukb_diseases[[date_col]][row]
        if (!is.na(date_val)) {
          matching_dates <- c(matching_dates, date_val)
        }
      }
      
      # 转换为Date格式并找到最早日期
      if (length(matching_dates) > 0) {
        matching_dates <- as.Date(matching_dates, origin = "1970-01-01")
        earliest_dates[row] <- min(matching_dates, na.rm = TRUE)
      }
    }
  }
  
  # 添加到数据框，确保是Date格式
  date_col_name <- paste0("date_", cancer_type, "_cancer")
  ukb_diseases[[date_col_name]] <- as.Date(earliest_dates, origin = "1970-01-01")
}

# 创建incident变量
cancer_types <- names(cancer_definitions)

for (cancer_type in cancer_types) {
  print(paste("Creating incident variable for", cancer_type, "cancer..."))
  
  date_col <- paste0("date_", cancer_type, "_cancer")
  incident_col <- paste0("incident_", cancer_type, "_cancer")
  
  # 创建incident变量
  ukb_diseases <- ukb_diseases %>%
    mutate(
      !!incident_col := case_when(
        is.na(diet_questionnaire_completed_date) ~ NA_real_,
        is.na(!!sym(date_col)) ~ 0,
        !!sym(date_col) < diet_questionnaire_completed_date ~ NA_real_,
        !!sym(date_col) >= diet_questionnaire_completed_date ~ 1,
        TRUE ~ 0
      )
    )
}

# 计算癌症的最大随访日期
all_cancer_dates <- c()
for (cancer_type in cancer_types) {
  date_col <- paste0("date_", cancer_type, "_cancer")
  all_cancer_dates <- c(all_cancer_dates, ukb_diseases[[date_col]])
}
max_cancer_followup_date <- as.Date(max(all_cancer_dates, na.rm = TRUE), origin = "1970-01-01")

print(paste("最大癌症随访日期:", max_cancer_followup_date))

# 创建follow_time变量
for (cancer_type in cancer_types) {
  print(paste("Creating follow-up time for", cancer_type, "cancer..."))
  
  date_col <- paste0("date_", cancer_type, "_cancer")
  incident_col <- paste0("incident_", cancer_type, "_cancer")
  followup_col <- paste0("follow_time_", cancer_type, "_cancer")
  
  ukb_diseases <- ukb_diseases %>%
    mutate(
      !!followup_col := case_when(
        is.na(!!sym(incident_col)) ~ NA_real_,
        is.na(diet_questionnaire_completed_date) ~ NA_real_,
        !!sym(incident_col) == 1 ~ as.numeric(!!sym(date_col) - diet_questionnaire_completed_date) / 365.25,
        !!sym(incident_col) == 0 & !is.na(date_lost_followup) & 
          date_lost_followup >= diet_questionnaire_completed_date ~ 
          as.numeric(date_lost_followup - diet_questionnaire_completed_date) / 365.25,
        !!sym(incident_col) == 0 & (is.na(date_lost_followup) | 
                                      date_lost_followup < diet_questionnaire_completed_date) ~ 
          as.numeric(max_cancer_followup_date - diet_questionnaire_completed_date) / 365.25,
        TRUE ~ NA_real_
      )
    )
}

# 创建汇总统计
cancer_summary <- data.frame(
  Cancer_Type = cancer_types,
  Cases = sapply(cancer_types, function(x) {
    incident_col <- paste0("incident_", x, "_cancer")
    sum(ukb_diseases[[incident_col]] == 1, na.rm = TRUE)
  }),
  Controls = sapply(cancer_types, function(x) {
    incident_col <- paste0("incident_", x, "_cancer")
    sum(ukb_diseases[[incident_col]] == 0, na.rm = TRUE)
  }),
  Excluded = sapply(cancer_types, function(x) {
    incident_col <- paste0("incident_", x, "_cancer")
    sum(is.na(ukb_diseases[[incident_col]]))
  }),
  Mean_Followup = sapply(cancer_types, function(x) {
    followup_col <- paste0("follow_time_", x, "_cancer")
    round(mean(ukb_diseases[[followup_col]], na.rm = TRUE), 2)
  }),
  Median_Followup = sapply(cancer_types, function(x) {
    followup_col <- paste0("follow_time_", x, "_cancer")
    round(median(ukb_diseases[[followup_col]], na.rm = TRUE), 2)
  })
)

print("癌症发病情况和随访时间汇总:")
print(cancer_summary)





library(dplyr)
library(lubridate)

# ============================================================================
# 第一部分：处理非癌症疾病
# ============================================================================

# 首先转换所有日期变量为Date格式
date_cols <- c("date_alzheimer", "date_vascular_dementia", "date_frontotemporal_dementia",
               "date_parkinsonism_all", "date_parkinson", "date_psp", "date_msa",
               "date_emphysema", "date_copd",
               "date_alcoholic_liver", "date_chronic_hepatitis", "date_liver_cirrhosis", 
               "date_inflammatory_liver", "date_other_liver",
               "date_seropos_ra", "date_other_ra",
               "date_gout", "date_osteoporosis_fracture", "date_osteoporosis_no_fracture",
               "date_polyarthrosis", "date_hip_arthrosis", "date_knee_arthrosis",
               "date_hand_arthrosis", "date_other_arthrosis")

# 批量转换日期
ukb_diseases <- ukb_diseases %>%
  mutate(across(all_of(date_cols), ~ ifelse(.x == "", NA, .x))) %>%
  mutate(across(all_of(date_cols), ~ as.Date(.x)))

# 转换其他重要的日期变量
ukb_diseases <- ukb_diseases %>%
  mutate(
    date_dementia_all = ifelse(date_dementia_all == "", NA, date_dementia_all),
    date_dementia_all = as.Date(date_dementia_all),
    
    date_lost_followup = ifelse(date_lost_followup == "", NA, date_lost_followup),
    date_lost_followup = as.Date(date_lost_followup),
    
    date_assessment_center = ifelse(date_assessment_center == "", NA, date_assessment_center),
    date_assessment_center = as.Date(date_assessment_center)
  )

# 使用pmin函数创建合并的疾病日期
ukb_diseases <- ukb_diseases %>%
  mutate(
    # 创建合并的疾病日期（使用pmin更高效）
    date_emphysema_copd = pmin(date_emphysema, date_copd, na.rm = TRUE),
    date_emphysema_copd = ifelse(is.na(date_emphysema) & is.na(date_copd), NA, date_emphysema_copd),
    date_emphysema_copd = as.Date(date_emphysema_copd, origin = "1970-01-01"),
    
    date_chronic_liver_diseases = pmin(date_alcoholic_liver, date_chronic_hepatitis, 
                                       date_liver_cirrhosis, date_inflammatory_liver, 
                                       date_other_liver, na.rm = TRUE),
    date_chronic_liver_diseases = ifelse(is.na(date_alcoholic_liver) & is.na(date_chronic_hepatitis) & 
                                           is.na(date_liver_cirrhosis) & is.na(date_inflammatory_liver) & 
                                           is.na(date_other_liver), NA, date_chronic_liver_diseases),
    date_chronic_liver_diseases = as.Date(date_chronic_liver_diseases, origin = "1970-01-01"),
    
    date_ra = pmin(date_seropos_ra, date_other_ra, na.rm = TRUE),
    date_ra = ifelse(is.na(date_seropos_ra) & is.na(date_other_ra), NA, date_ra),
    date_ra = as.Date(date_ra, origin = "1970-01-01"),
    
    date_osteoporosis = pmin(date_osteoporosis_fracture, date_osteoporosis_no_fracture, na.rm = TRUE),
    date_osteoporosis = ifelse(is.na(date_osteoporosis_fracture) & is.na(date_osteoporosis_no_fracture), NA, date_osteoporosis),
    date_osteoporosis = as.Date(date_osteoporosis, origin = "1970-01-01"),
    
    date_osteoarthritis = pmin(date_polyarthrosis, date_hip_arthrosis, date_knee_arthrosis,
                               date_hand_arthrosis, date_other_arthrosis, na.rm = TRUE),
    date_osteoarthritis = ifelse(is.na(date_polyarthrosis) & is.na(date_hip_arthrosis) & 
                                   is.na(date_knee_arthrosis) & is.na(date_hand_arthrosis) & 
                                   is.na(date_other_arthrosis), NA, date_osteoarthritis),
    date_osteoarthritis = as.Date(date_osteoarthritis, origin = "1970-01-01")
  )

# 创建incident变量函数
create_incident_var <- function(data, disease_date_col, base_date_col = "diet_questionnaire_completed_date") {
  case_when(
    is.na(data[[base_date_col]]) ~ NA_real_,
    is.na(data[[disease_date_col]]) ~ 0,
    data[[disease_date_col]] < data[[base_date_col]] ~ NA_real_,
    data[[disease_date_col]] >= data[[base_date_col]] ~ 1,
    TRUE ~ 0
  )
}

# 创建所有非癌症疾病的incident变量
ukb_diseases <- ukb_diseases %>%
  mutate(
    incident_dementia_all = create_incident_var(., "date_dementia_all"),
    incident_alzheimer = create_incident_var(., "date_alzheimer"),
    incident_vascular_dementia = create_incident_var(., "date_vascular_dementia"),
    incident_frontotemporal_dementia = create_incident_var(., "date_frontotemporal_dementia"),
    incident_parkinsonism_all = create_incident_var(., "date_parkinsonism_all"),
    incident_parkinson = create_incident_var(., "date_parkinson"),
    incident_emphysema_copd = create_incident_var(., "date_emphysema_copd"),
    incident_chronic_liver_diseases = create_incident_var(., "date_chronic_liver_diseases"),
    incident_ra = create_incident_var(., "date_ra"),
    incident_gout = create_incident_var(., "date_gout"),
    incident_osteoporosis = create_incident_var(., "date_osteoporosis"),
    incident_osteoarthritis = create_incident_var(., "date_osteoarthritis")
  )

print("Non-cancer incident variables created successfully!")

# ============================================================================
# 第二部分：处理癌症相关疾病
# ============================================================================

# 转换癌症日期和ICD-10编码
cancer_date_cols <- paste0("date_cancer_", 0:21)
cancer_icd10_cols <- paste0("cancer_type_icd10_", 0:21)

ukb_diseases <- ukb_diseases %>%
  mutate(across(all_of(cancer_date_cols), ~ ifelse(.x == "", NA, .x))) %>%
  mutate(across(all_of(cancer_date_cols), ~ as.Date(.x))) %>%
  mutate(across(all_of(cancer_icd10_cols), ~ ifelse(.x == "", NA, .x)))

# 定义癌症类型的ICD-10编码
cancer_definitions <- list(
  colorectal = c("C18", "C19", "C20"),
  lung = c("C33", "C34"),
  esophageal = c("C15"),
  liver = c("C22"),
  pancreatic = c("C25"),
  brain = c("C71"),
  leukemia = c("C91", "C92", "C93", "C94", "C95"),
  lymphoma = c("C81", "C82", "C83", "C84", "C85", "C86", "C88"),
  breast = c("C50"),
  ovarian = c("C56"),
  prostate = c("C61")
)

# 创建函数来检查ICD-10编码是否匹配特定癌症类型
check_cancer_type <- function(icd10_code, cancer_codes) {
  if (is.na(icd10_code)) return(FALSE)
  
  # 提取ICD-10编码的前3个字符
  icd10_prefix <- substr(icd10_code, 1, 3)
  
  return(icd10_prefix %in% cancer_codes)
}

# 为每种癌症类型创建最早诊断日期
for (cancer_type in names(cancer_definitions)) {
  print(paste("Processing", cancer_type, "cancer..."))
  
  # 创建一个矩阵来存储每个人每个instance的癌症匹配情况
  cancer_matches <- matrix(FALSE, nrow = nrow(ukb_diseases), ncol = 22)
  
  # 检查每个instance的ICD-10编码
  for (i in 0:21) {
    icd10_col <- paste0("cancer_type_icd10_", i)
    cancer_matches[, i+1] <- sapply(ukb_diseases[[icd10_col]], 
                                    function(x) check_cancer_type(x, cancer_definitions[[cancer_type]]))
  }
  
  # 为每个人找到最早的癌症诊断日期
  earliest_dates <- rep(as.Date(NA), nrow(ukb_diseases))
  
  for (row in 1:nrow(ukb_diseases)) {
    matching_instances <- which(cancer_matches[row, ])
    
    if (length(matching_instances) > 0) {
      # 获取匹配instance的日期
      matching_dates <- c()
      for (inst in matching_instances - 1) {
        date_col <- paste0("date_cancer_", inst)
        date_val <- ukb_diseases[[date_col]][row]
        if (!is.na(date_val)) {
          matching_dates <- c(matching_dates, date_val)
        }
      }
      
      # 转换为Date格式并找到最早日期
      if (length(matching_dates) > 0) {
        matching_dates <- as.Date(matching_dates, origin = "1970-01-01")
        earliest_dates[row] <- min(matching_dates, na.rm = TRUE)
      }
    }
  }
  
  # 添加到数据框，确保是Date格式
  date_col_name <- paste0("date_", cancer_type, "_cancer")
  ukb_diseases[[date_col_name]] <- as.Date(earliest_dates, origin = "1970-01-01")
}

# 创建癌症的incident变量
cancer_types <- names(cancer_definitions)

for (cancer_type in cancer_types) {
  print(paste("Creating incident variable for", cancer_type, "cancer..."))
  
  date_col <- paste0("date_", cancer_type, "_cancer")
  incident_col <- paste0("incident_", cancer_type, "_cancer")
  
  # 创建incident变量
  ukb_diseases <- ukb_diseases %>%
    mutate(
      !!incident_col := case_when(
        is.na(diet_questionnaire_completed_date) ~ NA_real_,
        is.na(!!sym(date_col)) ~ 0,
        !!sym(date_col) < diet_questionnaire_completed_date ~ NA_real_,
        !!sym(date_col) >= diet_questionnaire_completed_date ~ 1,
        TRUE ~ 0
      )
    )
}

print("Cancer incident variables created successfully!")

# ============================================================================
# 第三部分：创建所有follow_time变量
# ============================================================================

# 计算最大随访日期
all_disease_dates <- c(
  ukb_diseases$date_dementia_all, 
  ukb_diseases$date_alzheimer,
  ukb_diseases$date_vascular_dementia,
  ukb_diseases$date_frontotemporal_dementia,
  ukb_diseases$date_parkinsonism_all,
  ukb_diseases$date_parkinson,
  ukb_diseases$date_emphysema_copd,
  ukb_diseases$date_chronic_liver_diseases,
  ukb_diseases$date_ra,
  ukb_diseases$date_gout,
  ukb_diseases$date_osteoporosis,
  ukb_diseases$date_osteoarthritis
)

# 添加癌症日期
for (cancer_type in cancer_types) {
  date_col <- paste0("date_", cancer_type, "_cancer")
  all_disease_dates <- c(all_disease_dates, ukb_diseases[[date_col]])
}

# 计算最大随访日期
max_followup_date <- max(all_disease_dates, na.rm = TRUE)
max_followup_date <- as.Date(max_followup_date, origin = "1970-01-01")
print(paste("最大随访日期:", max_followup_date))

# 创建follow_time计算函数
create_followup_time <- function(data, incident_col, disease_date_col, 
                                 base_date_col = "diet_questionnaire_completed_date",
                                 lost_followup_col = "date_lost_followup") {
  
  # 确保日期列是Date格式
  base_date <- as.Date(data[[base_date_col]])
  disease_date <- as.Date(data[[disease_date_col]])
  lost_date <- as.Date(data[[lost_followup_col]])
  
  case_when(
    is.na(data[[incident_col]]) ~ NA_real_,
    is.na(base_date) ~ NA_real_,
    data[[incident_col]] == 1 ~ as.numeric(disease_date - base_date) / 365.25,
    data[[incident_col]] == 0 & !is.na(lost_date) & lost_date >= base_date ~ 
      as.numeric(lost_date - base_date) / 365.25,
    data[[incident_col]] == 0 & (is.na(lost_date) | lost_date < base_date) ~ 
      as.numeric(max_followup_date - base_date) / 365.25,
    TRUE ~ NA_real_
  )
}

# 创建所有非癌症疾病的follow_time变量
ukb_diseases <- ukb_diseases %>%
  mutate(
    follow_time_dementia_all = create_followup_time(., "incident_dementia_all", "date_dementia_all"),
    follow_time_alzheimer = create_followup_time(., "incident_alzheimer", "date_alzheimer"),
    follow_time_vascular_dementia = create_followup_time(., "incident_vascular_dementia", "date_vascular_dementia"),
    follow_time_frontotemporal_dementia = create_followup_time(., "incident_frontotemporal_dementia", "date_frontotemporal_dementia"),
    follow_time_parkinsonism_all = create_followup_time(., "incident_parkinsonism_all", "date_parkinsonism_all"),
    follow_time_parkinson = create_followup_time(., "incident_parkinson", "date_parkinson"),
    follow_time_emphysema_copd = create_followup_time(., "incident_emphysema_copd", "date_emphysema_copd"),
    follow_time_chronic_liver_diseases = create_followup_time(., "incident_chronic_liver_diseases", "date_chronic_liver_diseases"),
    follow_time_ra = create_followup_time(., "incident_ra", "date_ra"),
    follow_time_gout = create_followup_time(., "incident_gout", "date_gout"),
    follow_time_osteoporosis = create_followup_time(., "incident_osteoporosis", "date_osteoporosis"),
    follow_time_osteoarthritis = create_followup_time(., "incident_osteoarthritis", "date_osteoarthritis")
  )

# 创建所有癌症的follow_time变量
for (cancer_type in cancer_types) {
  print(paste("Creating follow-up time for", cancer_type, "cancer..."))
  
  date_col <- paste0("date_", cancer_type, "_cancer")
  incident_col <- paste0("incident_", cancer_type, "_cancer")
  followup_col <- paste0("follow_time_", cancer_type, "_cancer")
  
  ukb_diseases <- ukb_diseases %>%
    mutate(
      !!followup_col := case_when(
        is.na(!!sym(incident_col)) ~ NA_real_,
        is.na(diet_questionnaire_completed_date) ~ NA_real_,
        !!sym(incident_col) == 1 ~ as.numeric(!!sym(date_col) - diet_questionnaire_completed_date) / 365.25,
        !!sym(incident_col) == 0 & !is.na(date_lost_followup) & 
          date_lost_followup >= diet_questionnaire_completed_date ~ 
          as.numeric(date_lost_followup - diet_questionnaire_completed_date) / 365.25,
        !!sym(incident_col) == 0 & (is.na(date_lost_followup) | 
                                      date_lost_followup < diet_questionnaire_completed_date) ~ 
          as.numeric(max_followup_date - diet_questionnaire_completed_date) / 365.25,
        TRUE ~ NA_real_
      )
    )
}

print("All follow-up time variables created successfully!")
# 检查final_results的结构和内容
print("检查final_results的结构:")
str(final_results)
print("前几行数据:")
head(final_results)
print("列名:")
names(final_results)

# 检查是否有错误信息的结果
if("error" %in% names(final_results)) {
  error_results <- final_results %>%
    filter(!is.na(error))
  
  if(nrow(error_results) > 0) {
    print("有错误的分析:")
    print(error_results)
  }
}

# ============================================================================
# 第四部分：生成汇总统计
# ============================================================================

# 非癌症疾病汇总
non_cancer_diseases <- c("dementia_all", "alzheimer", "vascular_dementia", "frontotemporal_dementia", 
                         "parkinsonism_all", "parkinson", "emphysema_copd", 
                         "chronic_liver_diseases", "ra", "gout", "osteoporosis", "osteoarthritis")

non_cancer_summary <- data.frame(
  Disease = non_cancer_diseases,
  Cases = sapply(non_cancer_diseases, function(x) sum(ukb_diseases[[paste0("incident_", x)]] == 1, na.rm = TRUE)),
  Controls = sapply(non_cancer_diseases, function(x) sum(ukb_diseases[[paste0("incident_", x)]] == 0, na.rm = TRUE)),
  Excluded = sapply(non_cancer_diseases, function(x) sum(is.na(ukb_diseases[[paste0("incident_", x)]]))),
  Mean_Followup = sapply(non_cancer_diseases, function(x) round(mean(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2)),
  Median_Followup = sapply(non_cancer_diseases, function(x) round(median(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2))
)

# 癌症疾病汇总
cancer_summary <- data.frame(
  Cancer_Type = paste0(cancer_types, "_cancer"),
  Cases = sapply(cancer_types, function(x) {
    incident_col <- paste0("incident_", x, "_cancer")
    sum(ukb_diseases[[incident_col]] == 1, na.rm = TRUE)
  }),
  Controls = sapply(cancer_types, function(x) {
    incident_col <- paste0("incident_", x, "_cancer")
    sum(ukb_diseases[[incident_col]] == 0, na.rm = TRUE)
  }),
  Excluded = sapply(cancer_types, function(x) {
    incident_col <- paste0("incident_", x, "_cancer")
    sum(is.na(ukb_diseases[[incident_col]]))
  }),
  Mean_Followup = sapply(cancer_types, function(x) {
    followup_col <- paste0("follow_time_", x, "_cancer")
    round(mean(ukb_diseases[[followup_col]], na.rm = TRUE), 2)
  }),
  Median_Followup = sapply(cancer_types, function(x) {
    followup_col <- paste0("follow_time_", x, "_cancer")
    round(median(ukb_diseases[[followup_col]], na.rm = TRUE), 2)
  })
)

print("=== 非癌症疾病发病情况和随访时间汇总 ===")
print(non_cancer_summary)

print("\n=== 癌症疾病发病情况和随访时间汇总 ===")
print(cancer_summary)

# 检查是否有负值
print("\n=== 检查随访时间是否有负值 ===")
all_diseases <- c(non_cancer_diseases, paste0(cancer_types, "_cancer"))

for(disease in all_diseases) {
  follow_time_col <- paste0("follow_time_", disease)
  if(follow_time_col %in% names(ukb_diseases)) {
    negative_count <- sum(ukb_diseases[[follow_time_col]] < 0, na.rm = TRUE)
    if(negative_count > 0) {
      cat(disease, ": ", negative_count, " 个负值\n")
    }
  }
}

print("\n=== 数据处理完成！现在可以进行生存分析了 ===")

# 显示最终数据框的维度和结构
cat("\n最终数据框维度:", dim(ukb_diseases), "\n")
cat("总变量数:", ncol(ukb_diseases), "\n")

# 列出所有创建的incident和follow_time变量
incident_vars <- names(ukb_diseases)[grepl("^incident_", names(ukb_diseases))]
followup_vars <- names(ukb_diseases)[grepl("^follow_time_", names(ukb_diseases))]

cat("\n创建的incident变量 (", length(incident_vars), "个):\n")
cat(paste(incident_vars, collapse = ", "), "\n")

cat("\n创建的follow_time变量 (", length(followup_vars), "个):\n")
cat(paste(followup_vars, collapse = ", "), "\n")




###############################comorbidity risk#########################################
names(ukb_diseases)
names(beverage_20w_data)
names(imputed_data_2)
ukb_diseases <- merge(ukb_diseases,beverage_20w_data,all.x = T)
ukb_diseases <- merge(ukb_diseases,imputed_data_2[,c(1,7,9,10,11,13,14,15,16,17)],all.x=T)


library(survival)
library(dplyr)
library(broom)

# 定义疾病列表（癌症）
cancer_diseases <- c("colorectal_cancer", "lung_cancer", "esophageal_cancer", 
                     "liver_cancer", "pancreatic_cancer", "brain_cancer", 
                     "leukemia_cancer", "lymphoma_cancer", "breast_cancer", 
                     "ovarian_cancer", "prostate_cancer")

# 定义疾病列表（非癌症）
non_cancer_diseases <- c("dementia_all", "alzheimer", "vascular_dementia", 
                         "frontotemporal_dementia", "parkinsonism_all", "parkinson",
                         "emphysema_copd", "chronic_liver_diseases", "ra", 
                         "gout", "osteoporosis", "osteoarthritis")

# 合并所有疾病
all_diseases <- c(cancer_diseases, non_cancer_diseases)

# 定义暴露变量
exposures <- c("coffee_tea_preference", "SSB_ASB_preference", "NJ_water_preference")

# 定义协变量（检查变量名是否存在）
available_vars <- names(ukb_diseases)
covariates <- c("age", "sex", "ethnic", "education_years", "household_income", 
                "bmi", "smoking_status", "alcohol_intake_frequency", 
                "diet_score", "PA_mod_vig_150", "overall_health_rating", 
                "smk_num", "smk_qyr")

# 检查哪些协变量存在
missing_covariates <- covariates[!covariates %in% available_vars]
available_covariates <- covariates[covariates %in% available_vars]

if(length(missing_covariates) > 0) {
  print(paste("缺失的协变量:", paste(missing_covariates, collapse = ", ")))
  print(paste("可用的协变量:", paste(available_covariates, collapse = ", ")))
  
  # 使用可用的协变量
  covariates <- available_covariates
}

# 创建分析数据集，排除缺失值过多的个体
analysis_data <- ukb_diseases %>%
  select(eid, all_of(exposures), all_of(covariates), 
         starts_with("incident_"), starts_with("follow_time_")) %>%
  # 移除关键变量缺失的个体
  filter(!is.na(age), !is.na(sex)) %>%
  # 创建种族的二分类变量（如果ethnic存在）
  {if("ethnic" %in% names(.)) {
    mutate(., race = ifelse(ethnic == "White", "White", "Non-White"))
  } else .}

print(paste("分析数据集样本数:", nrow(analysis_data)))

# 定义Cox回归分析函数
run_cox_analysis <- function(data, exposure_var, outcome_var, followup_var, covariates) {
  
  # 检查变量是否存在
  required_vars <- c(exposure_var, outcome_var, followup_var, covariates)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  
  if(length(missing_vars) > 0) {
    return(data.frame(
      exposure = exposure_var,
      outcome = outcome_var,
      error = paste("Missing variables:", paste(missing_vars, collapse = ", "))
    ))
  }
  
  # 创建分析用的数据
  analysis_subset <- data %>%
    select(all_of(required_vars)) %>%
    filter(complete.cases(.)) %>%
    filter(get(followup_var) > 0)  # 确保随访时间为正
  
  if(nrow(analysis_subset) < 50) {
    return(data.frame(
      exposure = exposure_var,
      outcome = outcome_var,
      n = nrow(analysis_subset),
      error = "Insufficient sample size"
    ))
  }
  
  # 检查事件数
  events <- sum(analysis_subset[[outcome_var]] == 1, na.rm = TRUE)
  
  if(events < 10) {
    return(data.frame(
      exposure = exposure_var,
      outcome = outcome_var,
      n = nrow(analysis_subset),
      events = events,
      error = "Insufficient events"
    ))
  }
  
  # 构建Cox模型公式
  if("race" %in% names(analysis_subset)) {
    covariate_formula <- paste(c(covariates[covariates != "ethnic"], "race"), collapse = " + ")
  } else {
    covariate_formula <- paste(covariates, collapse = " + ")
  }
  
  formula_str <- paste("Surv(", followup_var, ",", outcome_var, ") ~", 
                       exposure_var, "+", covariate_formula)
  
  tryCatch({
    # 拟合Cox模型
    cox_model <- coxph(as.formula(formula_str), data = analysis_subset)
    
    # 提取结果
    results <- tidy(cox_model, conf.int = TRUE) %>%
      filter(grepl(exposure_var, term)) %>%
      mutate(
        exposure = exposure_var,
        outcome = outcome_var,
        n = nrow(analysis_subset),
        events = events,
        hr = exp(estimate),
        hr_lower = exp(conf.low),
        hr_upper = exp(conf.high)
      ) %>%
      select(exposure, outcome, term, n, events, hr, hr_lower, hr_upper, p.value)
    
    return(results)
    
  }, error = function(e) {
    return(data.frame(
      exposure = exposure_var,
      outcome = outcome_var,
      n = nrow(analysis_subset),
      events = events,
      error = as.character(e)
    ))
  })
}

# 运行所有分析
print("开始Cox回归分析...")

all_results <- list()
counter <- 1

for(exposure in exposures) {
  for(disease in all_diseases) {
    
    incident_var <- paste0("incident_", disease)
    followup_var <- paste0("follow_time_", disease)
    
    print(paste("分析", counter, ":", exposure, "vs", disease))
    
    result <- run_cox_analysis(
      data = analysis_data,
      exposure_var = exposure,
      outcome_var = incident_var,
      followup_var = followup_var,
      covariates = covariates
    )
    
    all_results[[counter]] <- result
    counter <- counter + 1
  }
}

# 合并所有结果
final_results <- do.call(rbind, all_results)

# 筛选成功的分析结果
successful_results <- final_results %>%
  filter(is.na(error)) %>%
  arrange(exposure, outcome)

# 筛选有统计学意义的结果
significant_results <- successful_results %>%
  filter(p.value < 0.05) %>%
  arrange(p.value)

print("=== Cox回归分析完成 ===")
print(paste("总分析数:", nrow(final_results)))
print(paste("成功分析数:", nrow(successful_results)))
print(paste("有统计学意义的关联数:", nrow(significant_results)))

# 显示成功分析的汇总
print("\n=== 成功分析的结果汇总 ===")
if(nrow(successful_results) > 0) {
  print(successful_results %>%
          select(exposure, outcome, n, events, hr, hr_lower, hr_upper, p.value) %>%
          mutate(
            hr_ci = paste0(round(hr, 3), " (", round(hr_lower, 3), "-", round(hr_upper, 3), ")"),
            p_value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
          ) %>%
          select(exposure, outcome, n, events, hr_ci, p_value))
}

# 显示有统计学意义的结果
print("\n=== 有统计学意义的关联 (p<0.05) ===")
if(nrow(significant_results) > 0) {
  print(significant_results %>%
          select(exposure, outcome, term, n, events, hr, hr_lower, hr_upper, p.value) %>%
          mutate(
            hr_ci = paste0(round(hr, 3), " (", round(hr_lower, 3), "-", round(hr_upper, 3), ")"),
            p_value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
          ) %>%
          select(exposure, outcome, term, n, events, hr_ci, p_value))
}

# 显示失败的分析
failed_results <- final_results %>%
  filter(!is.na(error)) %>%
  select(exposure, outcome, error) %>%
  unique()

if(nrow(failed_results) > 0) {
  print("\n=== 失败的分析 ===")
  print(failed_results)
}


##############plot################################
library(ggplot2)
library(dplyr)

# 处理数据
plot_data <- final_results_by_ckm %>%
  filter(!is.na(hr), !is.na(p.value), events >= 50) %>%
  filter(hr > 0, hr < 10, p.value > 0, p.value <= 1) %>%
  mutate(
    neg_log10_p = -log10(p.value),
    disease_type = case_when(
      grepl("colorectal|liver|pancreatic|esophageal", outcome) ~ "消化系统癌症",
      grepl("lung", outcome) ~ "呼吸系统癌症", 
      grepl("brain|dementia|alzheimer", outcome) ~ "神经系统疾病",
      grepl("breast|ovarian|prostate", outcome) ~ "生殖系统癌症",
      grepl("leukemia|lymphoma", outcome) ~ "血液系统癌症",
      grepl("ra|gout|osteo", outcome) ~ "肌肉骨骼疾病",
      TRUE ~ "其他疾病"
    ),
    ckm_simple = case_when(
      ckm_group == "CKM 0-1" ~ "低风险",
      ckm_group == "CKM 2" ~ "中风险", 
      ckm_group == "CKM 3-4" ~ "高风险"
    ),
    significant = p.value < 0.05,
    exposure_label = case_when(
      exposure == "coffee_tea_preference" ~ "咖啡茶偏好",
      exposure == "SSB_ASB_preference" ~ "含糖/人工甜味饮料偏好", 
      exposure == "NJ_water_preference" ~ "天然果汁/水偏好",
      TRUE ~ exposure
    ),
    # 创建具体疾病名称的中文标签
    disease_label = case_when(
      outcome == "incident_colorectal_cancer" ~ "结直肠癌",
      outcome == "incident_lung_cancer" ~ "肺癌",
      outcome == "incident_esophageal_cancer" ~ "食管癌",
      outcome == "incident_liver_cancer" ~ "肝癌",
      outcome == "incident_pancreatic_cancer" ~ "胰腺癌",
      outcome == "incident_brain_cancer" ~ "脑癌",
      outcome == "incident_leukemia_cancer" ~ "白血病",
      outcome == "incident_lymphoma_cancer" ~ "淋巴瘤",
      outcome == "incident_breast_cancer" ~ "乳腺癌",
      outcome == "incident_ovarian_cancer" ~ "卵巢癌",
      outcome == "incident_prostate_cancer" ~ "前列腺癌",
      outcome == "incident_dementia_all" ~ "痴呆症",
      outcome == "incident_alzheimer" ~ "阿尔茨海默病",
      outcome == "incident_vascular_dementia" ~ "血管性痴呆",
      outcome == "incident_frontotemporal_dementia" ~ "额颞叶痴呆",
      outcome == "incident_parkinsonism_all" ~ "帕金森综合征",
      outcome == "incident_parkinson" ~ "帕金森病",
      outcome == "incident_emphysema_copd" ~ "肺气肿/慢阻肺",
      outcome == "incident_chronic_liver_diseases" ~ "慢性肝病",
      outcome == "incident_ra" ~ "类风湿关节炎",
      outcome == "incident_gout" ~ "痛风",
      outcome == "incident_osteoporosis" ~ "骨质疏松",
      outcome == "incident_osteoarthritis" ~ "骨关节炎",
      TRUE ~ gsub("incident_", "", gsub("_", " ", outcome))
    )
  )

# 检查数据
cat("处理后数据行数:", nrow(plot_data), "\n")
cat("显著关联数量:", sum(plot_data$significant), "\n")

# 配色方案
disease_colors <- c(
  "消化系统癌症" = "#d62728",
  "呼吸系统癌症" = "#ff7f0e",
  "神经系统疾病" = "#2ca02c",
  "生殖系统癌症" = "#9467bd",
  "血液系统癌症" = "#8c564b",
  "肌肉骨骼疾病" = "#e377c2",
  "其他疾病" = "#7f7f7f"
)

ckm_shapes <- c("低风险" = 16, "中风险" = 17, "高风险" = 15)

# 创建火山图，显示所有显著关联的疾病名称
p1 <- ggplot(plot_data, aes(x = hr, y = neg_log10_p)) +
  geom_point(aes(color = disease_type, shape = ckm_simple, size = significant, alpha = significant)) +
  scale_color_manual(values = disease_colors, name = "疾病类型") +
  scale_shape_manual(values = ckm_shapes, name = "CKM风险组") +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.5), guide = "none") +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1.0, 1.5, 2.0, 3.0),
    labels = c("0.5", "0.75", "1.0", "1.5", "2.0", "3.0")
  ) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", alpha = 0.7) +
  # 标注所有显著的关联
  geom_text(
    aes(label = disease_label),
    data = filter(plot_data, significant),
    size = 2.5,
    hjust = 0.5,
    vjust = -0.5,
    check_overlap = TRUE,
    color = "black"
  ) +
  labs(
    x = "风险比 (HR)",
    y = "-log₁₀(P值)",
    title = "饮料偏好与疾病风险关联分析",
    subtitle = "标注了所有显著关联的疾病 (P < 0.05, 事件数 ≥ 50)",
    caption = "虚线表示HR = 1和P = 0.05的阈值"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.box = "vertical",
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13)
  )

# 显示图形
print(p1)
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 18, height = 9)




































