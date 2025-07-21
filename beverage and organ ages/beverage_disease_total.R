library(survival)
ukb_age_related_diseases_dataset <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/age_related_diseases_dataset_participant.csv")
str(ukb_age_related_diseases_dataset)

library(dplyr)
library(lubridate)
library(survival)
library(broom)
library(ggplot2)

# ============================================================================
# 第一部分：数据预处理和变量重命名
# ============================================================================

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

print("变量重命名完成!")

# ============================================================================
# 第二部分：合并其他数据集
# ============================================================================

# 合并饮料数据和其他协变量数据
ukb_diseases <- merge(ukb_diseases, beverage_20w_data, all.x = TRUE)
ukb_diseases <- merge(ukb_diseases, imputed_data_2[,c(1,7,9,10,11,13,14,15,16,17)], all.x = TRUE)

print("数据合并完成!")

# ============================================================================
# 第三部分：处理非癌症疾病
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
               "date_hand_arthrosis", "date_other_arthrosis",
               # 心血管疾病
               "date_t2dm", "date_angina", "date_ami", "date_subsequent_mi", 
               "date_mi_complications", "date_acute_ihd", "date_chronic_ihd",
               "date_heart_failure", "date_atherosclerosis", "date_pvd", "date_stroke")

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
    date_assessment_center = as.Date(date_assessment_center),
    
    # 确保基线日期是Date格式
    diet_questionnaire_completed_date = as.Date(diet_questionnaire_completed_date)
  )

# 使用pmin函数创建合并的疾病日期
ukb_diseases <- ukb_diseases %>%
  mutate(
    # 肺部疾病
    date_emphysema_copd = pmin(date_emphysema, date_copd, na.rm = TRUE),
    date_emphysema_copd = ifelse(is.na(date_emphysema) & is.na(date_copd), NA, date_emphysema_copd),
    date_emphysema_copd = as.Date(date_emphysema_copd, origin = "1970-01-01"),
    
    # 肝脏疾病
    date_chronic_liver_diseases = pmin(date_alcoholic_liver, date_chronic_hepatitis, 
                                       date_liver_cirrhosis, date_inflammatory_liver, 
                                       date_other_liver, na.rm = TRUE),
    date_chronic_liver_diseases = ifelse(is.na(date_alcoholic_liver) & is.na(date_chronic_hepatitis) & 
                                           is.na(date_liver_cirrhosis) & is.na(date_inflammatory_liver) & 
                                           is.na(date_other_liver), NA, date_chronic_liver_diseases),
    date_chronic_liver_diseases = as.Date(date_chronic_liver_diseases, origin = "1970-01-01"),
    
    # 类风湿关节炎
    date_ra = pmin(date_seropos_ra, date_other_ra, na.rm = TRUE),
    date_ra = ifelse(is.na(date_seropos_ra) & is.na(date_other_ra), NA, date_ra),
    date_ra = as.Date(date_ra, origin = "1970-01-01"),
    
    # 骨质疏松
    date_osteoporosis = pmin(date_osteoporosis_fracture, date_osteoporosis_no_fracture, na.rm = TRUE),
    date_osteoporosis = ifelse(is.na(date_osteoporosis_fracture) & is.na(date_osteoporosis_no_fracture), NA, date_osteoporosis),
    date_osteoporosis = as.Date(date_osteoporosis, origin = "1970-01-01"),
    
    # 骨关节炎
    date_osteoarthritis = pmin(date_polyarthrosis, date_hip_arthrosis, date_knee_arthrosis,
                               date_hand_arthrosis, date_other_arthrosis, na.rm = TRUE),
    date_osteoarthritis = ifelse(is.na(date_polyarthrosis) & is.na(date_hip_arthrosis) & 
                                   is.na(date_knee_arthrosis) & is.na(date_hand_arthrosis) & 
                                   is.na(date_other_arthrosis), NA, date_osteoarthritis),
    date_osteoarthritis = as.Date(date_osteoarthritis, origin = "1970-01-01"),
    
    # CAD复合变量 (I20-I25: 任何冠心病相关疾病)
    date_cad = pmin(date_angina, date_ami, date_subsequent_mi, 
                    date_mi_complications, date_acute_ihd, date_chronic_ihd, na.rm = TRUE),
    date_cad = ifelse(is.na(date_angina) & is.na(date_ami) & is.na(date_subsequent_mi) & 
                        is.na(date_mi_complications) & is.na(date_acute_ihd) & is.na(date_chronic_ihd), 
                      NA, date_cad),
    date_cad = as.Date(date_cad, origin = "1970-01-01"),
    
    # PAD复合变量 (I70和I73: 外周动脉疾病)
    date_pad = pmin(date_atherosclerosis, date_pvd, na.rm = TRUE),
    date_pad = ifelse(is.na(date_atherosclerosis) & is.na(date_pvd), NA, date_pad),
    date_pad = as.Date(date_pad, origin = "1970-01-01")
  )

print("非癌症疾病日期变量处理完成!")

# ============================================================================
# 第四部分：处理癌症疾病（包括膀胱癌）
# ============================================================================

# 转换癌症日期和ICD-10编码
cancer_date_cols <- paste0("date_cancer_", 0:21)
cancer_icd10_cols <- paste0("cancer_type_icd10_", 0:21)

ukb_diseases <- ukb_diseases %>%
  mutate(across(all_of(cancer_date_cols), ~ ifelse(.x == "", NA, .x))) %>%
  mutate(across(all_of(cancer_date_cols), ~ as.Date(.x))) %>%
  mutate(across(all_of(cancer_icd10_cols), ~ ifelse(.x == "", NA, .x)))

# 定义癌症类型的ICD-10编码（增加膀胱癌）
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
  prostate = c("C61"),
  bladder = c("C67")  # 新增膀胱癌
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

print("癌症疾病日期变量处理完成!")

# ============================================================================
# 第五部分：创建incident变量
# ============================================================================

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
    incident_osteoarthritis = create_incident_var(., "date_osteoarthritis"),
    # 心血管疾病
    incident_t2dm = create_incident_var(., "date_t2dm"),
    incident_cad = create_incident_var(., "date_cad"),
    incident_hf = create_incident_var(., "date_heart_failure"),
    incident_pad = create_incident_var(., "date_pad"),
    incident_stroke = create_incident_var(., "date_stroke")
  )

# 创建癌症的incident变量（包括膀胱癌）
cancer_types <- names(cancer_definitions)

for (cancer_type in cancer_types) {
  print(paste("Creating incident variable for", cancer_type, "cancer..."))
  
  date_col <- paste0("date_", cancer_type, "_cancer")
  incident_col <- paste0("incident_", cancer_type, "_cancer")
  
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

print("所有incident变量创建完成!")

# ============================================================================
# 第六部分：创建follow_time变量
# ============================================================================

# 计算最大随访日期
all_disease_dates <- c(
  # 非癌症疾病
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
  ukb_diseases$date_osteoarthritis,
  # 心血管疾病
  ukb_diseases$date_t2dm,
  ukb_diseases$date_cad,
  ukb_diseases$date_heart_failure,
  ukb_diseases$date_pad,
  ukb_diseases$date_stroke
)

# 添加癌症日期（包括膀胱癌）
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
    follow_time_osteoarthritis = create_followup_time(., "incident_osteoarthritis", "date_osteoarthritis"),
    # 心血管疾病
    follow_time_t2dm = create_followup_time(., "incident_t2dm", "date_t2dm"),
    follow_time_cad = create_followup_time(., "incident_cad", "date_cad"),
    follow_time_hf = create_followup_time(., "incident_hf", "date_heart_failure"),
    follow_time_pad = create_followup_time(., "incident_pad", "date_pad"),
    follow_time_stroke = create_followup_time(., "incident_stroke", "date_stroke")
  )

# 创建所有癌症的follow_time变量（包括膀胱癌）
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

print("所有follow_time变量创建完成!")

# 首先合并数据
merged_data <- merge(ukb_diseases, death_data[, c("eid", "follow_to_death_time")], 
                     by = "eid", all.x = TRUE)

# 使用pmin函数进行调整（只对有死亡时间的个体）
follow_time_vars <- c(
  "follow_time_dementia_all", "follow_time_alzheimer", 
  "follow_time_vascular_dementia", "follow_time_frontotemporal_dementia",
  "follow_time_parkinsonism_all", "follow_time_parkinson",
  "follow_time_emphysema_copd", "follow_time_chronic_liver_diseases",
  "follow_time_ra", "follow_time_gout",
  "follow_time_osteoporosis", "follow_time_osteoarthritis",
  "follow_time_t2dm", "follow_time_cad",
  "follow_time_hf", "follow_time_pad",
  "follow_time_stroke", "follow_time_colorectal_cancer",
  "follow_time_lung_cancer", "follow_time_esophageal_cancer",
  "follow_time_liver_cancer", "follow_time_pancreatic_cancer",
  "follow_time_brain_cancer", "follow_time_leukemia_cancer",
  "follow_time_lymphoma_cancer", "follow_time_breast_cancer",
  "follow_time_ovarian_cancer", "follow_time_prostate_cancer",
  "follow_time_bladder_cancer"
)

for (var in follow_time_vars) {
  # 对有死亡时间记录的个体，取疾病随访时间和死亡时间的最小值
  has_death_time <- !is.na(merged_data$follow_to_death_time)
  merged_data[has_death_time, var] <- pmin(merged_data[has_death_time, var], 
                                           merged_data$follow_to_death_time[has_death_time], 
                                           na.rm = FALSE)
}

# 如果原ukb_diseases中没有follow_to_death_time，则移除
if (!"follow_to_death_time" %in% names(ukb_diseases)) {
  merged_data$follow_to_death_time <- NULL
}

ukb_diseases <- merged_data

# ============================================================================
# 第七部分：生成汇总统计
# ============================================================================

# 非癌症疾病汇总
non_cancer_diseases <- c("dementia_all", "alzheimer", "vascular_dementia", "frontotemporal_dementia", 
                         "parkinsonism_all", "parkinson", "emphysema_copd", 
                         "chronic_liver_diseases", "ra", "gout", "osteoporosis", "osteoarthritis")

# 心血管疾病汇总
cardio_diseases <- c("t2dm", "cad", "hf", "pad", "stroke")

# 合并非癌症和心血管疾病
all_non_cancer_diseases <- c(non_cancer_diseases, cardio_diseases)

non_cancer_summary <- data.frame(
  Disease = all_non_cancer_diseases,
  Cases = sapply(all_non_cancer_diseases, function(x) sum(ukb_diseases[[paste0("incident_", x)]] == 1, na.rm = TRUE)),
  Controls = sapply(all_non_cancer_diseases, function(x) sum(ukb_diseases[[paste0("incident_", x)]] == 0, na.rm = TRUE)),
  Excluded = sapply(all_non_cancer_diseases, function(x) sum(is.na(ukb_diseases[[paste0("incident_", x)]]))),
  Mean_Followup = sapply(all_non_cancer_diseases, function(x) round(mean(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2)),
  Median_Followup = sapply(all_non_cancer_diseases, function(x) round(median(ukb_diseases[[paste0("follow_time_", x)]], na.rm = TRUE), 2))
)

# 癌症疾病汇总（包括膀胱癌）
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

print("=== 非癌症疾病（包括心血管疾病）发病情况和随访时间汇总 ===")
print(non_cancer_summary)

print("\n=== 癌症疾病发病情况和随访时间汇总（包括膀胱癌） ===")
print(cancer_summary)

# ============================================================================
# 第八部分：Cox回归分析
# ============================================================================

# 定义疾病列表（包括膀胱癌）
cancer_diseases <- c("colorectal_cancer", "lung_cancer", "esophageal_cancer", 
                     "liver_cancer", "pancreatic_cancer", "brain_cancer", 
                     "leukemia_cancer", "lymphoma_cancer", "breast_cancer", 
                     "ovarian_cancer", "prostate_cancer", "bladder_cancer")  # 添加膀胱癌

non_cancer_diseases <- c("dementia_all", "alzheimer", "vascular_dementia", 
                         "frontotemporal_dementia", "parkinsonism_all", "parkinson",
                         "emphysema_copd", "chronic_liver_diseases", "ra", 
                         "gout", "osteoporosis", "osteoarthritis")

# 心血管疾病
cardio_diseases <- c("t2dm", "cad", "hf", "pad", "stroke")

# 合并所有疾病
all_diseases <- c(cancer_diseases, non_cancer_diseases, cardio_diseases)

# 定义暴露变量
exposures <- c("coffee_tea_preference", "SSB_ASB_preference", "NJ_water_preference")

# 定义协变量
available_vars <- names(ukb_diseases)
covariates <- c("age", "sex", "ethnic", "education_years", "household_income", 
                "bmi", "smoking_status", "alcohol_intake_frequency", 
                "diet_score", "PA_mod_vig_150", "overall_health_rating")

# 检查哪些协变量存在
available_covariates <- covariates[covariates %in% available_vars]
missing_covariates <- covariates[!covariates %in% available_vars]

if(length(missing_covariates) > 0) {
  print(paste("缺失的协变量:", paste(missing_covariates, collapse = ", ")))
}
print(paste("可用的协变量:", paste(available_covariates, collapse = ", ")))

# 创建分析数据集
analysis_data <- ukb_diseases %>%
  select(eid, all_of(exposures), all_of(available_covariates), 
         starts_with("incident_"), starts_with("follow_time_")) %>%
  filter(!is.na(age), !is.na(sex)) %>%
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
      covariates = available_covariates
    )
    
    all_results[[counter]] <- result
    counter <- counter + 1
  }
}

# 合并所有结果
final_results <- do.call(rbind, all_results)

# 筛选成功的分析结果
successful_results <- final_results %>%
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

# ============================================================================
# FDR调整分析
# ============================================================================
successful_results <- successful_results[successful_results$outcome!="incident_frontotemporal_dementia",]

# 对successful_results进行FDR调整
print("=== 进行FDR调整前的结果统计 ===")
print(paste("总分析数:", nrow(successful_results)))
print(paste("名义显著结果数 (p < 0.05):", sum(successful_results$p.value < 0.05)))
print(paste("高度显著结果数 (p < 0.01):", sum(successful_results$p.value < 0.01)))
print(paste("极显著结果数 (p < 0.001):", sum(successful_results$p.value < 0.001)))

# 进行FDR调整
successful_results_fdr <- successful_results %>%
  mutate(
    # Benjamini-Hochberg FDR调整
    fdr_bh = p.adjust(p.value, method = "BH"),
    # Benjamini-Yekutieli FDR调整 (更保守)
    # Bonferroni调整 (最保守)
    p_bonferroni = p.adjust(p.value, method = "bonferroni"),
    # 标记不同显著性水平
    significant_nominal = p.value < 0.05,
    significant_fdr_bh = fdr_bh < 0.05,
    significant_bonferroni = p_bonferroni < 0.05,
    # 创建显著性等级
    significance_level = case_when(
      p_bonferroni < 0.05 ~ "Bonferroni significant",
      fdr_bh < 0.05 ~ "FDR-BH significant",
      p.value < 0.05 ~ "Nominally significant",
      TRUE ~ "Not significant"
    )
  ) %>%
  arrange(fdr_bh)

print("\n=== FDR调整后的结果统计 ===")
print(paste("FDR-BH显著结果数 (FDR < 0.05):", sum(successful_results_fdr$fdr_bh < 0.05)))
print(paste("Bonferroni显著结果数 (p < 0.05):", sum(successful_results_fdr$p_bonferroni < 0.05)))

# 显著性等级统计
significance_summary <- successful_results_fdr %>%
  count(significance_level) %>%
  arrange(desc(n))

print("\n=== 显著性等级分布 ===")
print(significance_summary)

# 显示FDR显著的结果
fdr_significant_results <- successful_results_fdr %>%
  filter(fdr_bh < 0.05) %>%
  select(exposure, outcome, term, n, events, hr, hr_lower, hr_upper, 
         p.value, fdr_bh, p_bonferroni, significance_level) %>%
  mutate(
    hr_ci = paste0(round(hr, 3), " (", round(hr_lower, 3), "-", round(hr_upper, 3), ")"),
    p_value_formatted = case_when(
      p.value < 0.001 ~ "<0.001",
      p.value < 0.01 ~ sprintf("%.3f", p.value),
      TRUE ~ sprintf("%.3f", p.value)
    ),
    fdr_bh_formatted = case_when(
      fdr_bh < 0.001 ~ "<0.001",
      fdr_bh < 0.01 ~ sprintf("%.3f", fdr_bh),
      TRUE ~ sprintf("%.3f", fdr_bh)
    )
  )

print("\n=== FDR-BH显著的结果 (FDR < 0.05) ===")
if(nrow(fdr_significant_results) > 0) {
  print(fdr_significant_results %>%
          select(exposure, outcome, n, events, hr_ci, p_value_formatted, fdr_bh_formatted, significance_level))
} else {
  print("没有通过FDR调整的显著结果")
}

# ============================================================================
# 第九部分：可视化分析（可选）
# ============================================================================
print(successful_results,n=84)
# 创建火山图的数据处理
# Create volcano plot data processing with updated classifications
if(nrow(successful_results) > 0) {
  plot_data <- successful_results %>%
    filter(events >= 10) %>%  # Only include analyses with events >= 10
    mutate(
      neg_log10_p = -log10(p.value),
      # Updated disease classification with diabetes separate
      disease_type = case_when(
        grepl("colorectal|liver|pancreatic|esophageal", outcome) ~ "Digestive System Cancers",
        grepl("lung", outcome) ~ "Respiratory System Cancers", 
        grepl("brain|dementia|alzheimer|parkinson", outcome) ~ "Neurological Diseases",
        grepl("breast|ovarian|prostate|bladder", outcome) ~ "Genitourinary Cancers",
        grepl("leukemia|lymphoma", outcome) ~ "Hematologic Cancers",
        grepl("ra|gout|osteo", outcome) ~ "Musculoskeletal Diseases",
        grepl("t2dm", outcome) ~ "Diabetes",  # Diabetes as separate category
        grepl("cad|hf|pad|stroke", outcome) ~ "Cardiovascular Diseases",
        TRUE ~ "Other Diseases"
      ),
      # Beverage type shapes
      beverage_shape = case_when(
        exposure == "coffee_tea_preference" ~ "Coffee/Tea",        # Circle
        exposure == "NJ_water_preference" ~ "Natural Juice/Water", # Square
        exposure == "SSB_ASB_preference" ~ "SSB/ASB",             # Triangle
        TRUE ~ exposure
      ),
      significant = p.value < 0.05,
      exposure_label = case_when(
        exposure == "coffee_tea_preference" ~ "Coffee/Tea Preference",
        exposure == "SSB_ASB_preference" ~ "SSB/ASB Preference", 
        exposure == "NJ_water_preference" ~ "Natural Juice/Water Preference",
        TRUE ~ exposure
      ),
      # Create specific disease name labels in English
      disease_label = case_when(
        outcome == "incident_colorectal_cancer" ~ "Colorectal Cancer",
        outcome == "incident_lung_cancer" ~ "Lung Cancer",
        outcome == "incident_esophageal_cancer" ~ "Esophageal Cancer",
        outcome == "incident_liver_cancer" ~ "Liver Cancer",
        outcome == "incident_pancreatic_cancer" ~ "Pancreatic Cancer",
        outcome == "incident_brain_cancer" ~ "Brain Cancer",
        outcome == "incident_leukemia_cancer" ~ "Leukemia",
        outcome == "incident_lymphoma_cancer" ~ "Lymphoma",
        outcome == "incident_breast_cancer" ~ "Breast Cancer",
        outcome == "incident_ovarian_cancer" ~ "Ovarian Cancer",
        outcome == "incident_prostate_cancer" ~ "Prostate Cancer",
        outcome == "incident_bladder_cancer" ~ "Bladder Cancer",
        outcome == "incident_dementia_all" ~ "All-cause Dementia",
        outcome == "incident_alzheimer" ~ "Alzheimer's Disease",
        outcome == "incident_vascular_dementia" ~ "Vascular Dementia",
        outcome == "incident_frontotemporal_dementia" ~ "Frontotemporal Dementia",
        outcome == "incident_parkinsonism_all" ~ "All-cause Parkinsonism",
        outcome == "incident_parkinson" ~ "Parkinson's Disease",
        outcome == "incident_emphysema_copd" ~ "Emphysema/COPD",
        outcome == "incident_chronic_liver_diseases" ~ "Chronic Liver Diseases",
        outcome == "incident_ra" ~ "Rheumatoid Arthritis",
        outcome == "incident_gout" ~ "Gout",
        outcome == "incident_osteoporosis" ~ "Osteoporosis",
        outcome == "incident_osteoarthritis" ~ "Osteoarthritis",
        outcome == "incident_t2dm" ~ "Type 2 Diabetes",
        outcome == "incident_cad" ~ "Coronary Artery Disease",
        outcome == "incident_hf" ~ "Heart Failure",
        outcome == "incident_pad" ~ "Peripheral Artery Disease",
        outcome == "incident_stroke" ~ "Stroke",
        TRUE ~ gsub("incident_", "", gsub("_", " ", outcome))
      )
    )
  
  # Updated color scheme with diabetes as separate category
  disease_colors <- c(
    "Digestive System Cancers" = "#d62728",
    "Respiratory System Cancers" = "#ff7f0e",
    "Neurological Diseases" = "#2ca02c",
    "Genitourinary Cancers" = "#9467bd",
    "Hematologic Cancers" = "#8c564b",
    "Musculoskeletal Diseases" = "#e377c2",
    "Diabetes" = "#17becf",                    # New color for diabetes
    "Cardiovascular Diseases" = "#1f77b4",
    "Other Diseases" = "#7f7f7f"
  )
  
  # Shape mapping for beverage types
  beverage_shapes <- c(
    "Coffee/Tea" = 16,           # Circle
    "Natural Juice/Water" = 15,  # Square
    "SSB/ASB" = 17              # Triangle
  )
  
  # Create volcano plot with updated aesthetics
  p1 <- ggplot(plot_data, aes(x = hr, y = neg_log10_p)) +
    geom_point(aes(color = disease_type, 
                   shape = beverage_shape,
                   size = significant, 
                   alpha = significant)) +
    scale_color_manual(values = disease_colors, name = "Disease Type") +
    scale_shape_manual(values = beverage_shapes, name = "Beverage Type") +
    scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2), guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.5), guide = "none") +
    scale_x_log10(
      breaks = c(0.5, 0.75, 1.0, 1.5, 2.0, 3.0),
      labels = c("0.5", "0.75", "1.0", "1.5", "2.0", "3.0")
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.7) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", alpha = 0.7) +
    # Annotate all significant associations
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
      x = "Hazard Ratio (HR)",
      y = "-log₁₀(P-value)",
      title = "Beverage Preference and Disease Risk Association Analysis",
      subtitle = "All significant disease associations are labeled (P < 0.05, Events ≥ 10)\nShapes indicate beverage types: ● Coffee/Tea, ■ Natural Juice/Water, ▲ SSB/ASB",
      caption = "Dashed lines indicate thresholds for HR = 1 and P = 0.05"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      plot.caption = element_text(hjust = 0.5, size = 10),
      legend.position = "right",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13),
      # Adjust legend spacing
      legend.spacing.y = unit(0.3, "cm"),
      legend.margin = margin(t = 0, r = 10, b = 0, l = 0)
    ) +
    guides(
      color = guide_legend(order = 1, title = "Disease Type"),
      shape = guide_legend(order = 2, title = "Beverage Type", 
                           override.aes = list(size = 4))
    )
  
  # Display the plot
  print(p1)
  
  # Save the plot if needed
  ggsave("C:/Users/张杰/Desktop/beverage_disease_volcano_plot_updated.pdf", plot = p1, width = 16, height = 10, dpi = 300)
}
