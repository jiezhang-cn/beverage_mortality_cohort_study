CKM_UKB_data <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/CKM_variables_participant.tsv")
AF_Date <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/AF_Date (1).tsv")
CVD_diagnosed_self_reported <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/CVD__diagnosed_self_reported.tsv")
Diabetes_diagnosed_self_reported <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/Diabetes__diagnosed_self_reported.tsv")
Total_cholesterol <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/Total_Cholesterol.tsv")



CKM_UKB_data <- merge(CKM_UKB_data,AF_Date,all.x=T)
CKM_UKB_data <- merge(CKM_UKB_data,CVD_diagnosed_self_reported,all.x=T)
CKM_UKB_data <- merge(CKM_UKB_data,Diabetes_diagnosed_self_reported,all.x=T)
CKM_UKB_data <- merge(CKM_UKB_data,Total_cholesterol,all.x=T)
str(CKM_UKB_data)


library(dplyr)

ckm_ukb_data <- CKM_UKB_data %>%
  rename(
    eid = Participant.ID,
    age = Age.at.recruitment,
    sex = Sex,
    ethnicity = `Ethnic.background...Instance.0`,
    smoking = `Smoking.status...Instance.0`,
    bmi = `Body.mass.index..BMI....Instance.0`,
    waist = `Waist.circumference...Instance.0`,
    dbp_auto1 = `Diastolic.blood.pressure..automated.reading...Instance.0...Array.0`,
    dbp_auto2 = `Diastolic.blood.pressure..automated.reading...Instance.0...Array.1`,
    dbp_manual1 = `Diastolic.blood.pressure..manual.reading...Instance.0...Array.0`,
    dbp_manual2 = `Diastolic.blood.pressure..manual.reading...Instance.0...Array.1`,
    sbp_auto1 = `Systolic.blood.pressure..automated.reading...Instance.0...Array.0`,
    sbp_auto2 = `Systolic.blood.pressure..automated.reading...Instance.0...Array.1`,
    sbp_manual1 = `Systolic.blood.pressure..manual.reading...Instance.0...Array.0`,
    sbp_manual2 = `Systolic.blood.pressure..manual.reading...Instance.0...Array.1`,
    hdl = `HDL.cholesterol...Instance.0`,
    glucose = `Glucose...Instance.0`,
    hba1c = `Glycated.haemoglobin..HbA1c....Instance.0`,
    tg = `Triglycerides...Instance.0`,
    creatinine = `Creatinine...Instance.0`,
    urine_creat = `Creatinine..enzymatic..in.urine...Instance.0`,
    urine_creat_flag = `Creatinine..enzymatic..in.urine.result.flag...Instance.0`,
    urine_microalbumin = `Microalbumin.in.urine...Instance.0`,
    urine_microalbumin_flag = `Microalbumin.in.urine.result.flag...Instance.0`,
    medication = `Medication.for.cholesterol..blood.pressure.or.diabetes...Instance.0`,
    baseline_date = `Date.of.attending.assessment.centre...Instance.0`,
    af_source = `Source.of.report.of.I48..atrial.fibrillation.and.flutter.`,
    hf_date = `Date.I50.first.reported..heart.failure.`,
    athero_date = `Date.I70.first.reported..atherosclerosis.`,
    pvd_date = `Date.I73.first.reported..other.peripheral.vascular.diseases.`,
    stroke_date = `Date.of.stroke`,
    angina_date = `Date.I20.first.reported..angina.pectoris.`,
    mi_date = `Date.I21.first.reported..acute.myocardial.infarction.`,
    submi_date = `Date.I22.first.reported..subsequent.myocardial.infarction.`,
    micomp_date = `Date.I23.first.reported..certain.current.complications.following.acute.myocardial.infarction.`,
    acuteihd_date = `Date.I24.first.reported..other.acute.ischaemic.heart.diseases.`,
    chronicihd_date = `Date.I25.first.reported..chronic.ischaemic.heart.disease.`,
    af_date = `Date.I48.first.reported..atrial.fibrillation.and.flutter.`,
    cvd_self_reported = `Vascular.heart.problems.diagnosed.by.doctor...Instance.0`,
    diabetes_self_reported = `Diabetes.diagnosed.by.doctor...Instance.0`,
    tc= `Cholesterol...Instance.0`,
  )
names(ckm_ukb_data)

###############Medication##########################
medication_data <- read.delim("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/medication_data.tsv")
names(medication_data) <- c("eid","code")
head(medication_data)


#antihypertensive medications
ckm_ukb_data$antihypertensive_medications <- ifelse(grepl("2", ckm_ukb_data$medication), 1, 0)
table(ckm_ukb_data$antihypertensive_medications)


#Cholesterol lowering medications
ckm_ukb_data$cholesterol_lowering_medications <- ifelse(grepl("1", ckm_ukb_data$medication), 1, 0)
table(ckm_ukb_data$cholesterol_lowering_medications)



#insulin, or oral hypoglycemic agents
ckm_ukb_data$insulin <- ifelse(grepl("3", ckm_ukb_data$medication),1,0)
table(ckm_ukb_data$insulin)

hypo_codes <- c(
  "1140884600", "1140874686", "1140883066", "1140874744",
  "1140910566", "1140874746", "1141152590", "1141156984",
  "1141171646", "1141171652", "1140874718", "1141168660",
  "1141177600", "1141189090", "1141189094", "1140874646",
  "1141177606", "1140874674", "1140868902", "1141173882"
)

pattern <- paste(hypo_codes, collapse="|")
medication_data$oral_hypoglycemic_agents <- ifelse(grepl(pattern, medication_data$code), 1, 0)
table(medication_data$oral_hypoglycemic_agents)


#statin 
statin_codes <- c("1141146234", "1141192414", "1140910632", "1140888594", 
                  "1140864592", "1141146138", "1140861970", "1140888648", 
                  "1141192410", "1141188146", "1140861958", "1140881748", 
                  "1141200040")
pattern <- paste(statin_codes, collapse="|")
medication_data$statin <- ifelse(grepl(pattern, medication_data$code), 1, 0)
table(medication_data$statin)



ckm_ukb_data <- merge(ckm_ukb_data,medication_data[,c("eid","oral_hypoglycemic_agents","statin")],all.x = T)
ckm_ukb_data$diabetes_medications <- ifelse((ckm_ukb_data$insulin==1 | ckm_ukb_data$oral_hypoglycemic_agents==1),1,0)
table(ckm_ukb_data$diabetes_medications)



################clinical CVD#######################
#History of chronic heart failure, coronary heart disease, heart attack, or stroke
# Create vector of date columns to compare
date_cols <- c("athero_date", "pvd_date", "stroke_date", "angina_date", 
               "mi_date", "submi_date", "micomp_date", "acuteihd_date",
               "chronicihd_date", "af_date")

# Convert empty strings to NA for comparison
for(col in date_cols) {
  ckm_ukb_data[[col]][ckm_ukb_data[[col]] == ""] <- NA
}

# Convert baseline_date and other date columns to Date type
ckm_ukb_data$baseline_date <- as.Date(ckm_ukb_data$baseline_date)
for(col in date_cols) {
  ckm_ukb_data[[col]] <- as.Date(ckm_ukb_data[[col]])
}

# Create clinical_cvd variable
# Any date earlier than baseline = 1, otherwise 0
ckm_ukb_data$clinical_cvd <- 0
for(col in date_cols) {
  ckm_ukb_data$clinical_cvd[which(ckm_ukb_data[[col]] < ckm_ukb_data$baseline_date)] <- 1
}


table(ckm_ukb_data$clinical_cvd)





###############UACR######################
#a numeric value for Microalbumin in urine<6.7 mg/L will be NA, replace with 6.7
ckm_ukb_data$urine_microalbumin[is.na(ckm_ukb_data$urine_microalbumin) & ckm_ukb_data$urine_microalbumin_flag=="<6.7"] <- 6.7
summary(ckm_ukb_data$urine_microalbumin)

#transform the Creatinine (enzymatic) in urine (micromole/L) into g/L
ckm_ukb_data$urine_creat_gl <- ckm_ukb_data$urine_creat * 113.12 / 1000000
summary(ckm_ukb_data$urine_creat_gl)


ckm_ukb_data$UACR <- ckm_ukb_data$urine_microalbumin/ckm_ukb_data$urine_creat_gl
summary(ckm_ukb_data$UACR)
table(ckm_ukb_data$UACR>30)

###############eGFR######################
calculate_eGFR <- function(creatinine, age, sex) {
  # 转换血清肌酐单位：从umol/L到mg/dL
  Scr <- creatinine / 88.4
  
  # 根据性别设置K和α值
  # sex: 0为女性，1为男性
  K <- ifelse(sex == 0, 0.7, 0.9)
  alpha <- ifelse(sex == 0, -0.241, -0.302)
  
  # 计算标准化的Scr/K
  Scr_K <- Scr/K
  
  # 计算min(Scr/K, 1)^α
  min_term <- pmin(Scr_K, 1)^alpha
  
  # 计算max(Scr/K, 1)^-1.200
  max_term <- pmax(Scr_K, 1)^(-1.200)
  
  # 计算年龄项
  age_term <- 0.9938^age
  
  # 性别项
  sex_term <- ifelse(sex == 0, 1.012, 1)
  
  # 最终计算
  egfr <- 142 * min_term * max_term * age_term * sex_term
  
  return(egfr)
}

# 应用函数计算eGFR
ckm_ukb_data$eGFR <- calculate_eGFR(
  creatinine = ckm_ukb_data$creatinine,
  age = ckm_ukb_data$age,
  sex = ckm_ukb_data$sex
)

# 查看计算结果的摘要统计
summary(ckm_ukb_data$eGFR)



###############SBP & DBP######################
# 处理DBP
# 计算自动测量的均值
ckm_ukb_data$dbp_auto_mean <- rowMeans(
  cbind(ckm_ukb_data$dbp_auto1, ckm_ukb_data$dbp_auto2), 
  na.rm = TRUE
)

# 计算手动测量的均值
ckm_ukb_data$dbp_manual_mean <- rowMeans(
  cbind(ckm_ukb_data$dbp_manual1, ckm_ukb_data$dbp_manual2), 
  na.rm = TRUE
)

# 合并DBP（优先使用自动测量值，如果自动测量值为NA则使用手动测量值）
ckm_ukb_data$DBP <- ifelse(
  is.na(ckm_ukb_data$dbp_auto_mean),
  ckm_ukb_data$dbp_manual_mean,
  ckm_ukb_data$dbp_auto_mean
)

# 处理SBP
# 计算自动测量的均值
ckm_ukb_data$sbp_auto_mean <- rowMeans(
  cbind(ckm_ukb_data$sbp_auto1, ckm_ukb_data$sbp_auto2), 
  na.rm = TRUE
)

# 计算手动测量的均值
ckm_ukb_data$sbp_manual_mean <- rowMeans(
  cbind(ckm_ukb_data$sbp_manual1, ckm_ukb_data$sbp_manual2), 
  na.rm = TRUE
)

# 合并SBP（优先使用自动测量值，如果自动测量值为NA则使用手动测量值）
ckm_ukb_data$SBP <- ifelse(
  is.na(ckm_ukb_data$sbp_auto_mean),
  ckm_ukb_data$sbp_manual_mean,
  ckm_ukb_data$sbp_auto_mean
)

# 删除中间变量
ckm_ukb_data <- subset(ckm_ukb_data, 
                       select = -c(dbp_auto1, dbp_auto2, dbp_manual1, dbp_manual2,
                                   sbp_auto1, sbp_auto2, sbp_manual1, sbp_manual2,
                                   dbp_auto_mean, dbp_manual_mean,
                                   sbp_auto_mean, sbp_manual_mean))

# 查看结果
summary(ckm_ukb_data$DBP)
summary(ckm_ukb_data$SBP)


###############hypertension######################
ckm_ukb_data$hypertension_dignosis <- ifelse(grepl("4", ckm_ukb_data$cvd_self_reported), 1, 0)
table(ckm_ukb_data$hypertension_dignosis)

ckm_ukb_data$hypertension <- with(ckm_ukb_data, 
                                  case_when(
                                    antihypertensive_medications == 1 | hypertension_dignosis == 1 ~ 1,
                                    !is.na(antihypertensive_medications) & !is.na(hypertension_dignosis) ~ 0,
                                    TRUE ~ NA_real_
                                  ))

###############ethnicity######################
# 创建新的ethnicity变量
ckm_ukb_data$ethnicity_group <- with(ckm_ukb_data, 
                                     case_when(
                                       ethnicity %in% c(1, 1001, 1002, 1003) ~ 1,  # White
                                       ethnicity %in% c(2, 2001, 2002, 2003, 2004) ~ 2,  # Mixed
                                       ethnicity %in% c(3, 3001, 3002, 3003, 3004, 5) ~ 3,  # Asian or Asian British (including Chinese)
                                       ethnicity %in% c(4, 4001, 4002, 4003) ~ 4,  # Black or Black British
                                       ethnicity == 6 ~ 6,  # Other ethnic group
                                       ethnicity == -1 ~ -1,  # Do not know
                                       ethnicity == -3 ~ -3,  # Prefer not to answer
                                       TRUE ~ NA_real_
                                     )
)
table(ckm_ukb_data$ethnicity_group)


###############Overweight/obesity######################
# 创建超重/肥胖变量
ckm_ukb_data$overweight_obesity <- with(ckm_ukb_data,
                                        case_when(
                                          # 亚裔（包括Chinese）
                                          ethnicity_group == 3 & bmi >= 23 ~ 1,
                                          # 非亚裔
                                          ethnicity_group != 3 & bmi >= 25 ~ 1,
                                          # 其他情况（包括BMI低于阈值的情况）
                                          !is.na(bmi) ~ 0,
                                          # BMI缺失值
                                          TRUE ~ NA_real_
                                        )
)

# 检查结果
table(ckm_ukb_data$overweight_obesity, useNA = "ifany")
prop.table(table(ckm_ukb_data$overweight_obesity)) * 100


###############Abdominal obesity######################
# 创建腹型肥胖变量
ckm_ukb_data$abdominal_obesity <- with(ckm_ukb_data,
                                       case_when(
                                         # 亚裔女性 (sex=0) ≥80cm
                                         ethnicity_group == 3 & sex == 0 & waist >= 80 ~ 1,
                                         # 亚裔男性 (sex=1) ≥90cm
                                         ethnicity_group == 3 & sex == 1 & waist >= 90 ~ 1,
                                         # 非亚裔女性 ≥88cm
                                         ethnicity_group != 3 & sex == 0 & waist >= 88 ~ 1,
                                         # 非亚裔男性 ≥102cm
                                         ethnicity_group != 3 & sex == 1 & waist >= 102 ~ 1,
                                         # 其他情况（腰围低于阈值）
                                         !is.na(waist) ~ 0,
                                         # 腰围缺失值
                                         TRUE ~ NA_real_
                                       )
)

# 检查结果
table(ckm_ukb_data$abdominal_obesity, useNA = "ifany")
prop.table(table(ckm_ukb_data$abdominal_obesity)) * 100


###############Hypertriglyceridemia######################
# 查看tg的单位和分布
summary(ckm_ukb_data$tg)

# 创建高甘油三酯血症变量
# 假设数据中的tg单位是mmol/L，需要转换为mg/dL (乘以88.57)
ckm_ukb_data$hypertriglyceridemia <- with(ckm_ukb_data,
                                          case_when(
                                            # 转换为mg/dL后判断是否≥135
                                            (tg * 88.57) >= 135 ~ 1,
                                            # 其他情况（低于阈值）
                                            !is.na(tg) ~ 0,
                                            # 缺失值
                                            TRUE ~ NA_real_
                                          )
)

# 检查结果
table(ckm_ukb_data$hypertriglyceridemia, useNA = "ifany")
prop.table(table(ckm_ukb_data$hypertriglyceridemia)) * 100

# 查看tg分布在两组中的情况
summary(ckm_ukb_data$tg[ckm_ukb_data$hypertriglyceridemia == 0])
summary(ckm_ukb_data$tg[ckm_ukb_data$hypertriglyceridemia == 1])



################Prediabetes#######################
ckm_ukb_data$diabetes_dignosis <- ifelse(grepl("1", ckm_ukb_data$diabetes_self_reported), 1, 0)
table(ckm_ukb_data$diabetes_dignosis)

names(ckm_ukb_data)
# 创建糖尿病前期变量，先将HbA1c从mmol/mol转换为%
ckm_ukb_data$prediabetes <- with(ckm_ukb_data,
                                 case_when(
                                   # 排除已确诊糖尿病或使用降糖药物的病例
                                   diabetes_dignosis == 1 | diabetes_medications == 1 ~ 0,
                                   
                                   # 将mmol/mol转换为%后判断是否在5.7%-6.4%之间
                                   ((0.0915 * hba1c) + 2.15) >= 5.7 & 
                                     ((0.0915 * hba1c) + 2.15) < 6.5 ~ 1,
                                   
                                   # 如果有hba1c的数据，但不满足条件
                                   !is.na(hba1c) ~ 0,
                                   
                                   # 数据缺失
                                   TRUE ~ NA_real_
                                 )
)

# 检查结果
table(ckm_ukb_data$prediabetes, useNA = "ifany")
prop.table(table(ckm_ukb_data$prediabetes)) * 100



################Diabetes#######################
# 创建糖尿病变量
ckm_ukb_data$diabetes <- with(ckm_ukb_data,
                              case_when(
                                # 已确诊糖尿病或使用降糖药物的病例
                                diabetes_dignosis == 1 | diabetes_medications == 1 ~ 1,
                                
                                # HbA1c ≥ 6.5% (转换自mmol/mol)
                                ((0.0915 * hba1c) + 2.15) >= 6.5 ~ 1,
                                
                                # 如果有hba1c的数据，且不满足以上所有条件
                                !is.na(hba1c) ~ 0,
                                
                                # 数据缺失且没有诊断/用药信息
                                TRUE ~ NA_real_
                              )
)

# 检查结果
table(ckm_ukb_data$diabetes, useNA = "ifany")
prop.table(table(ckm_ukb_data$diabetes)) * 100



################MetS###################################
ckm_ukb_data <- within(ckm_ukb_data, {
  # 1. 腰围标准 (根据种族区分, ethnicity_group==3为asian)
  mets_waist <- case_when(
    ethnicity_group == 3 & sex == 0 & waist >= 80 ~ 1,
    ethnicity_group == 3 & sex == 1 & waist >= 90 ~ 1,
    ethnicity_group != 3 & !is.na(ethnicity_group) & sex == 0 & waist >= 88 ~ 1,
    ethnicity_group != 3 & !is.na(ethnicity_group) & sex == 1 & waist >= 102 ~ 1,
    !is.na(waist) & !is.na(ethnicity_group) & !is.na(sex) ~ 0,
    TRUE ~ NA_real_
  )
  
  # 2. HDL胆固醇标准 (从mmol/L到mg/dL乘以38.67)
  mets_hdl <- case_when(
    sex == 0 & (hdl * 38.67) < 50~ 1,
    sex == 1 & (hdl * 38.67) < 40~ 1,
    !is.na(hdl) & !is.na(sex) ~ 0,
    TRUE ~ NA_real_
  )
  
  # 3. 甘油三酯标准 (从mmol/L到mg/dL乘以88.57)
  mets_tg <- case_when(
    (tg * 88.57) >= 150 ~ 1,
    !is.na(tg) ~ 0,
    TRUE ~ NA_real_
  )
  
  # 4. 血压标准
  mets_bp <- case_when(
    SBP >= 130 | DBP >= 80 | antihypertensive_medications == 1 ~ 1,
    !is.na(SBP) & !is.na(DBP) & !is.na(antihypertensive_medications) ~ 0,
    TRUE ~ NA_real_
  )
  
  # 5. HbA1c标准 (从mmol/mol转换为%)
  mets_hba1c <- case_when(
    ((0.0915 * hba1c) + 2.15) >= 5.7 | diabetes_medications == 1~ 1,
    !is.na(hba1c) & !is.na(diabetes_medications)~ 0,
    TRUE ~ NA_real_
  )
  
  # 计算满足的条件数量
  mets_count <- mets_waist + mets_hdl + mets_tg + mets_bp + mets_hba1c
  
  # 判断是否为代谢综合征（满足3个及以上条件）
  mets <- case_when(
    !is.na(mets_count) & mets_count >= 3 ~ 1,
    !is.na(mets_count) ~ 0,
    TRUE ~ NA_real_
  )
})

# 检查结果
# 查看各组分的分布
component_vars <- c("mets_waist", "mets_hdl", "mets_tg", "mets_bp", "mets_hba1c")
sapply(component_vars, function(x) table(ckm_ukb_data[[x]], useNA = "ifany"))

# 查看满足条件数量的分布
table(ckm_ukb_data$mets_count, useNA = "ifany")

# 查看最终代谢综合征的分布及比例
table(ckm_ukb_data$mets, useNA = "ifany")
prop.table(table(ckm_ukb_data$mets)) * 100

# 创建一个汇总表格
component_summary <- data.frame(
  Component = c("Large Waist", "Low HDL", "High TG", "High BP", "High HbA1c", "MetS"),
  Present = sapply(c(component_vars, "mets"), 
                   function(x) sum(ckm_ukb_data[[x]] == 1, na.rm = TRUE)),
  Total = sapply(c(component_vars, "mets"), 
                 function(x) sum(!is.na(ckm_ukb_data[[x]]))),
  Percentage = sapply(c(component_vars, "mets"), 
                      function(x) mean(ckm_ukb_data[[x]] == 1, na.rm = TRUE) * 100)
)
print(component_summary)

##################################CKD############################################################
# 创建KDIGO分类和CKD风险分层
ckm_ukb_data <- within(ckm_ukb_data, {
  
  # 1. 创建eGFR分类 (G stage)
  gfr_stage <- case_when(
    eGFR >= 90 ~ "G1",
    eGFR >= 60 & eGFR < 90 ~ "G2",
    eGFR >= 45 & eGFR < 60 ~ "G3a",
    eGFR >= 30 & eGFR < 45 ~ "G3b",
    eGFR >= 15 & eGFR < 30 ~ "G4",
    eGFR < 15 ~ "G5",
    TRUE ~ NA_character_
  )
  
  # 2. 创建UACR分类 (A stage)
  # UACR单位：mg/g
  albuminuria_stage <- case_when(
    UACR < 30 ~ "A1",
    UACR >= 30 & UACR < 300 ~ "A2",
    UACR >= 300 ~ "A3",
    TRUE ~ NA_character_
  )
  
  # 3. 根据KDIGO风险矩阵定义CKD风险等级
  ckd_risk <- case_when(
    # Low Risk (绿色区域)
    (gfr_stage == "G1" & albuminuria_stage == "A1") |
      (gfr_stage == "G2" & albuminuria_stage == "A1") ~ "Low risk",
    
    # Moderately increased risk (黄色区域)
    (gfr_stage == "G1" & albuminuria_stage == "A2") |
      (gfr_stage == "G2" & albuminuria_stage == "A2") |
      (gfr_stage == "G3a" & albuminuria_stage == "A1") ~ "Moderately increased risk",
    
    # High risk (橙色区域)
    (gfr_stage == "G1" & albuminuria_stage == "A3") |
      (gfr_stage == "G2" & albuminuria_stage == "A3") |
      (gfr_stage == "G3a" & albuminuria_stage == "A2") |
      (gfr_stage == "G3b" & albuminuria_stage == "A1") ~ "High risk",
    
    # Very high risk (红色区域)
    (gfr_stage == "G3a" & albuminuria_stage == "A3") |
      (gfr_stage == "G3b" & (albuminuria_stage == "A2" | albuminuria_stage == "A3")) |
      (gfr_stage == "G4" & (albuminuria_stage %in% c("A1", "A2", "A3"))) |
      (gfr_stage == "G5" & (albuminuria_stage %in% c("A1", "A2", "A3"))) ~ "Very high risk",
    
    # 有完整数据但不满足上述条件的情况
    !is.na(gfr_stage) & !is.na(albuminuria_stage) ~ "Other",
    
    # 缺失值处理
    TRUE ~ NA_character_
  )
  
  # 转换为有序因子
  ckd_risk <- factor(ckd_risk, 
                     levels = c("Low risk", 
                                "Moderately increased risk",
                                "High risk",
                                "Very high risk"),
                     ordered = TRUE)
})

# 检查结果
cat("\nDistribution of GFR stages:\n")
table(ckm_ukb_data$gfr_stage, useNA = "ifany")

cat("\nDistribution of Albuminuria stages:\n")
table(ckm_ukb_data$albuminuria_stage, useNA = "ifany")

cat("\nDistribution of CKD Risk Categories:\n")
table(ckm_ukb_data$ckd_risk, useNA = "ifany")

# 创建GFR和蛋白尿分期的交叉表
cat("\nCross-tabulation of GFR and Albuminuria stages:\n")
print(table(ckm_ukb_data$gfr_stage, 
            ckm_ukb_data$albuminuria_stage, 
            useNA = "ifany"))

# 计算各风险等级的比例
risk_summary <- data.frame(
  Risk_Level = levels(ckm_ukb_data$ckd_risk),
  N = as.numeric(table(ckm_ukb_data$ckd_risk)),
  Percentage = round(prop.table(table(ckm_ukb_data$ckd_risk)) * 100, 2)
)

print("\nCKD Risk Level Summary:")
print(risk_summary)

ckm_ukb_data$ckd <- ifelse(ckm_ukb_data$ckd_risk=="Low risk",0,1)
table(ckm_ukb_data$ckd)


################subclinical CVD#######################
#Any of the following criterion is met: 
#1) Very high-risk CKD in KDIGO classification: UACR ≥ 300 mg/g and eGFR ≤ 45-59 ml/min/1.73m2, UACR ≥ 30 mg/g and eGFR ≤ 30-44 ml/min/1.73m2, or eGFR ≤ 29 ml/min/1.73m2.
#2) Predicted 10-year CVD risk ≥ 20%

names(ckm_ukb_data)
# 首先检查关键变量的分布
vars_check <- c("age", "sex", "smoking", "tc", "hdl", "SBP", "eGFR", 
                "diabetes", "antihypertensive_medications", "statin")

for(var in vars_check) {
  cat("\nSummary of", var, ":\n")
  print(summary(ckm_ukb_data[[var]]))
}

# 创建CVD风险计算函数
calculate_cvd_risk <- function(data) {
  # 检查必需变量
  required_vars <- c("age", "sex", "tc", "hdl", "SBP", "eGFR", "diabetes",
                     "smoking", "antihypertensive_medications", "statin")
  
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if(length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  data <- within(data, {
    # 创建计算所需的变量，添加NA检查
    age_centered <- (age - 55)/10
    nonhdl_centered <- (tc - hdl - 3.5)  
    hdl_centered <- (hdl - 1.3)/0.3
    min_sbp_centered <- (pmin(SBP, 110) - 110)/20
    max_sbp_centered <- (pmax(SBP, 110) - 130)/20
    min_egfr_centered <- (pmin(eGFR, 60) - 60)/-15
    max_egfr_centered <- (pmax(eGFR, 60) - 90)/-15
    
    # 将分类变量转换为0/1，添加NA检查
    diabetes_binary <- ifelse(!is.na(diabetes) & diabetes == 1, 1, 0)
    current_smoker <- ifelse(!is.na(smoking) & smoking == 2, 1, 0) # smoking 2为current
    antihypertensive <- ifelse(!is.na(antihypertensive_medications) & 
                                 antihypertensive_medications == 1, 1, 0)
    statin_binary <- ifelse(!is.na(statin) & statin == 1, 1, 0)
    
    # 创建完整性检查向量
    complete_data <- !is.na(age) & !is.na(sex) & !is.na(tc) & !is.na(hdl) & 
      !is.na(SBP) & !is.na(eGFR) & !is.na(diabetes) & 
      !is.na(smoking) & !is.na(antihypertensive_medications) & 
      !is.na(statin)
    
    # 计算交互项
    anti_sbp_interaction <- antihypertensive * max_sbp_centered
    statin_lipid_interaction <- statin_binary * nonhdl_centered
    age_lipid_interaction <- age_centered * nonhdl_centered
    age_hdl_interaction <- age_centered * hdl_centered
    age_sbp_interaction <- age_centered * max_sbp_centered
    age_diabetes_interaction <- age_centered * diabetes_binary
    age_smoking_interaction <- age_centered * current_smoker
    age_egfr_interaction <- age_centered * min_egfr_centered
    
    # 分性别计算log-odds
    female_log_odds <- ifelse(complete_data & sex == "0",
                              -3.307728 + 
                                0.7939329 * age_centered +
                                0.0305239 * nonhdl_centered - 
                                0.1606857 * hdl_centered - 
                                0.2394003 * min_sbp_centered + 
                                0.360078 * max_sbp_centered +
                                0.8667604 * diabetes_binary +
                                0.5360739 * current_smoker +
                                0.6045917 * min_egfr_centered +
                                0.0433769 * max_egfr_centered +
                                0.3151672 * antihypertensive -
                                0.1477655 * statin_binary -
                                0.0663612 * anti_sbp_interaction +
                                0.1197879 * statin_lipid_interaction -
                                0.0819715 * age_lipid_interaction +
                                0.0306769 * age_hdl_interaction -
                                0.0946348 * age_sbp_interaction -
                                0.27057 * age_diabetes_interaction -
                                0.078715 * age_smoking_interaction -
                                0.1637806 * age_egfr_interaction,
                              NA_real_)
    
    male_log_odds <- ifelse(complete_data & sex == "1",
                            -3.031168 + 
                              0.7688528 * age_centered +
                              0.0736174 * nonhdl_centered - 
                              0.0954431 * hdl_centered - 
                              0.4347345 * min_sbp_centered + 
                              0.3362658 * max_sbp_centered +
                              0.7692857 * diabetes_binary +
                              0.4386871 * current_smoker +
                              0.5378979 * min_egfr_centered +
                              0.0164827 * max_egfr_centered +
                              0.288879 * antihypertensive -
                              0.1337349 * statin_binary -
                              0.0475924 * anti_sbp_interaction +
                              0.150273 * statin_lipid_interaction -
                              0.0517874 * age_lipid_interaction +
                              0.0191169 * age_hdl_interaction -
                              0.1049477 * age_sbp_interaction -
                              0.2251948 * age_diabetes_interaction -
                              0.0895067 * age_smoking_interaction -
                              0.1543702 * age_egfr_interaction,
                            NA_real_)
    
    # 合并男女结果
    log_odds <- coalesce(female_log_odds, male_log_odds)
    
    # 计算风险
    cvd_risk <- ifelse(!is.na(log_odds), exp(log_odds)/(1 + exp(log_odds)), NA_real_)
  })
  
  return(data$cvd_risk)
}

# 应用函数
ckm_ukb_data$cvd_risk <- calculate_cvd_risk(ckm_ukb_data)

# 检查结果
summary(ckm_ukb_data$cvd_risk)
hist(ckm_ukb_data$cvd_risk, breaks=50, main="Distribution of CVD Risk Scores", 
     xlab="10-year CVD Risk")




# 创建亚临床CVD变量
ckm_ukb_data <- within(ckm_ukb_data, {
  # 1. 首先判断CKD高危标准
  high_risk_ckd <- case_when(
    # 条件1: UACR ≥ 300 mg/g 且 eGFR ≤ 45-59 ml/min/1.73m2
    UACR >= 300 & eGFR >= 45 & eGFR <= 59 ~ 1,
    
    # 条件2: UACR ≥ 30 mg/g 且 eGFR ≤ 30-44 ml/min/1.73m2
    UACR >= 30 & eGFR >= 30 & eGFR <= 44 ~ 1,
    
    # 条件3: eGFR ≤ 29 ml/min/1.73m2
    eGFR <= 29 ~ 1,
    
    # 其他情况
    !is.na(UACR) & !is.na(eGFR) ~ 0,
    
    # 缺失值处理
    TRUE ~ NA_real_
  )
  
  # 2. 判断10年CVD风险≥20%
  high_cvd_risk <- ifelse(!is.na(cvd_risk) & cvd_risk >= 0.20, 1, 0)
  
  # 3. 合并判断亚临床CVD
  subclinical_cvd <- case_when(
    ckd_risk=="Very high risk" | high_cvd_risk == 1 ~ 1,
    !is.na(high_risk_ckd) & !is.na(high_cvd_risk) ~ 0,
    TRUE ~ NA_real_
  )
})

# 检查结果
cat("\nDistribution of High Risk CKD:\n")
table(ckm_ukb_data$high_risk_ckd, useNA = "ifany")

cat("\nDistribution of High CVD Risk:\n")
table(ckm_ukb_data$high_cvd_risk, useNA = "ifany")

cat("\nDistribution of Subclinical CVD:\n")
table(ckm_ukb_data$subclinical_cvd, useNA = "ifany")






######################################################CKM Stage########################################################
####################################CKM Stage 0###############################################
#Individuals without overweight/obesity, metabolic risk factors (hypertension, hypertriglyceridemia, MetS, diabetes), CKD, or subclinical/clinical CVD
str(ckm_ukb_data)
ckm_ukb_data <- within(ckm_ukb_data, {
  
  # Stage 0: 无CKM健康风险因素
  ckm_stage_0 <- case_when(
    # 无超重/肥胖
    overweight_obesity == 0 &
      
      # 无代谢风险因素
      abdominal_obesity == 0  &      # 无腹部肥胖
      hypertension == 0 &           # 无高血压
      hypertriglyceridemia == 0 &   # 无高甘油三酯血症
      mets == 0 &                   # 无代谢综合征
      prediabetes == 0 &               # 无血糖升高
      diabetes == 0 &               # 无糖尿病
      
      # 无CKD (eGFR正常且无蛋白尿)
      ckd==0 &
      
      # 无亚临床/临床CVD
      subclinical_cvd == 0 &
      clinical_cvd == 0 ~ 1,
    
    # 如果所有条件都有值但不满足定义，则为0
    !is.na(overweight_obesity) &
      !is.na(abdominal_obesity) &
      !is.na(hypertension) &
      !is.na(hypertriglyceridemia) &
      !is.na(mets) &
      !is.na(prediabetes) &
      !is.na(diabetes) &
      !is.na(eGFR) &
      !is.na(UACR) &
      !is.na(subclinical_cvd) &
      !is.na(clinical_cvd) ~ 0,
    
    # 其他情况为NA
    TRUE ~ NA_real_
  )
})

# 检查Stage 0的分布情况
cat("\n=== Stage 0 Distribution ===\n")
print(table(ckm_ukb_data$ckm_stage_0, useNA = "ifany"))
cat("\nStage 0 Percentage: ", 
    round(mean(ckm_ukb_data$ckm_stage_0 == 1, na.rm = TRUE) * 100, 2), "%\n")




####################################CKM Stage 1###############################################
ckm_ukb_data <- within(ckm_ukb_data, {
  
  # Stage 1: 仅有超重/肥胖或腹型肥胖或脂肪组织功能障碍
  ckm_stage_1 <- case_when(
    # Stage 0已经排除
    (ckm_stage_0 == 0 | is.na(ckm_stage_0)) &
      
      # 必须满足以下条件之一:
      (overweight_obesity == 1 |    # BMI超重/肥胖
         abdominal_obesity == 1 |     # 腹型肥胖
         prediabetes == 1) &  # HbA1c异常
      
      # 无其他代谢风险因素
      hypertension == 0 &   # 无高血压
      hypertriglyceridemia == 0 &    # 无高甘油三酯血症
      mets == 0 &                    # 无代谢综合征
      diabetes == 0 &                # 无糖尿病
      
      # 无CKD
      ckd==0 &
      
      # 无亚临床/临床CVD
      subclinical_cvd == 0 &
      clinical_cvd == 0 ~ 1,
    
    # 如果所有条件都有值但不满足定义，则为0
    !is.na(overweight_obesity) &
      !is.na(abdominal_obesity) &
      !is.na(prediabetes) &
      !is.na(hypertension) &
      !is.na(hypertriglyceridemia) &
      !is.na(mets) &
      !is.na(diabetes) &
      !is.na(eGFR) &
      !is.na(UACR) &
      !is.na(subclinical_cvd) &
      !is.na(clinical_cvd) ~ 0,
    
    # 其他情况为NA
    TRUE ~ NA_real_
  )
})

# 检查Stage 1的分布情况
cat("\n=== Stage 1 Distribution ===\n")
print(table(ckm_ukb_data$ckm_stage_1, useNA = "ifany"))
cat("\nStage 1 Percentage: ", 
    round(mean(ckm_ukb_data$ckm_stage_1 == 1, na.rm = TRUE) * 100, 2), "%\n")





####################################CKM Stage 2###############################################
ckm_ukb_data <- within(ckm_ukb_data, {
  
  # Stage 2: 代谢风险因素和CKD
  ckm_stage_2 <- case_when(
    # Stage 0和Stage 1已经排除
    (ckm_stage_0 == 0 | is.na(ckm_stage_0)) &
      (ckm_stage_1 == 0 | is.na(ckm_stage_1)) &
      
      # 满足以下任一条件:
      (
        # 代谢风险因素
        hypertriglyceridemia == 1 |        # 高甘油三酯血症
          hypertension == 1 |        # 高血压
          mets == 1 |                        # 代谢综合征
          diabetes == 1 |                    # 糖尿病
          
          # CKD
          ckd_risk=="Moderately increased risk" |
          ckd_risk=="High risk"
      ) &
      
      # 无亚临床/临床CVD
      subclinical_cvd == 0 &
      clinical_cvd == 0 ~ 1,
    
    # 如果所有条件都有值但不满足定义，则为0
    !is.na(hypertriglyceridemia) &
      !is.na(hypertension) &
      !is.na(mets) &
      !is.na(diabetes) &
      !is.na(eGFR) &
      !is.na(UACR) &
      !is.na(subclinical_cvd) &
      !is.na(clinical_cvd) ~ 0,
    
    # 其他情况为NA
    TRUE ~ NA_real_
  )
})

# 检查Stage 2的分布情况
cat("\n=== Stage 2 Distribution ===\n")
print(table(ckm_ukb_data$ckm_stage_2, useNA = "ifany"))
cat("\nStage 2 Percentage: ", 
    round(mean(ckm_ukb_data$ckm_stage_2 == 1, na.rm = TRUE) * 100, 2), "%\n")


####################################CKM Stage 3###############################################
ckm_ukb_data <- within(ckm_ukb_data, {
  
  # Stage 3: 亚临床CVD或CVD高风险等效
  ckm_stage_3 <- case_when(
    # Stage 0、1和2已经排除
    (ckm_stage_0 == 0 | is.na(ckm_stage_0)) &
      (ckm_stage_1 == 0 | is.na(ckm_stage_1)) &
      (ckm_stage_2 == 0 | is.na(ckm_stage_2)) &
      
      # 必须满足以下条件之一:
      (
        # 有亚临床CVD
        subclinical_cvd == 1 ) &
      
      # 且必须同时具有以下任一条件:
      (
        overweight_obesity == 1 |      # 超重/肥胖
          abdominal_obesity == 1 |       # 腹型肥胖
          hypertriglyceridemia == 1 |    # 高甘油三酯血症
          hypertension == 1 |    # 高血压
          mets == 1 |                    # 代谢综合征
          diabetes == 1 |                # 糖尿病
          ckd==1       # CKD
      ) &
      
      # 无临床CVD
      clinical_cvd == 0 ~ 1,
    
    # 如果所有条件都有值但不满足定义，则为0
    !is.na(subclinical_cvd) &
      !is.na(ckd) &
      !is.na(overweight_obesity) &
      !is.na(abdominal_obesity) &
      !is.na(hypertriglyceridemia) &
      !is.na(hypertension) &
      !is.na(mets) &
      !is.na(diabetes) &
      !is.na(eGFR) &
      !is.na(UACR) &
      !is.na(clinical_cvd) ~ 0,
    
    # 其他情况为NA
    TRUE ~ NA_real_
  )
})

# 检查Stage 3的分布情况
cat("\n=== Stage 3 Distribution ===\n")
print(table(ckm_ukb_data$ckm_stage_3, useNA = "ifany"))
cat("\nStage 3 Percentage: ", 
    round(mean(ckm_ukb_data$ckm_stage_3 == 1, na.rm = TRUE) * 100, 2), "%\n")

####################################CKM Stage 4###############################################
ckm_ukb_data <- within(ckm_ukb_data, {
  
  # Stage 4: Clinical CVD in CKM
  ckm_stage_4 <- case_when(
    # Stage 0-3已经排除
    (ckm_stage_0 == 0 | is.na(ckm_stage_0)) &
      (ckm_stage_1 == 0 | is.na(ckm_stage_1)) &
      (ckm_stage_2 == 0 | is.na(ckm_stage_2)) &
      (ckm_stage_3 == 0 | is.na(ckm_stage_3)) &
      
      # 必须有临床CVD
      clinical_cvd == 1 &
      
      # 且必须同时具有以下任一条件:
      (
        overweight_obesity == 1 |      # 超重/肥胖
          abdominal_obesity == 1 |       # 腹型肥胖
          hypertriglyceridemia == 1 |    # 高甘油三酯血症
          hypertension == 1 |    # 高血压
          mets == 1 |                    # 代谢综合征
          diabetes == 1 |                # 糖尿病
          ckd == 1       # CKD
      ) ~ 1,
    
    # 如果所有条件都有值但不满足定义，则为0
    !is.na(clinical_cvd) &
      !is.na(overweight_obesity) &
      !is.na(abdominal_obesity) &
      !is.na(hypertriglyceridemia) &
      !is.na(hypertension) &
      !is.na(mets) &
      !is.na(diabetes) &
      !is.na(eGFR) &
      !is.na(UACR) ~ 0,
    
    # 其他情况为NA
    TRUE ~ NA_real_
  )
})

# 检查Stage 4的分布情况
cat("\n=== Stage 4 Distribution ===\n")
print(table(ckm_ukb_data$ckm_stage_4, useNA = "ifany"))
cat("\nStage 4 Percentage: ", 
    round(mean(ckm_ukb_data$ckm_stage_4 == 1, na.rm = TRUE) * 100, 2), "%\n")

# 检查所有Stage的互斥性
cat("\n=== All Stages Cross-tabulation ===\n")
stages_table <- table(Stage_0 = ckm_ukb_data$ckm_stage_0,
                      Stage_1 = ckm_ukb_data$ckm_stage_1,
                      Stage_2 = ckm_ukb_data$ckm_stage_2,
                      Stage_3 = ckm_ukb_data$ckm_stage_3,
                      Stage_4 = ckm_ukb_data$ckm_stage_4,
                      useNA = "ifany")
print(stages_table)

# 最终的CKM分期汇总
cat("\n=== Final CKM Staging Summary ===\n")
ckm_ukb_data$ckm_stage <- with(ckm_ukb_data, 
                               case_when(
                                 ckm_stage_0 == 1 ~ 0,
                                 ckm_stage_1 == 1 ~ 1,
                                 ckm_stage_2 == 1 ~ 2,
                                 ckm_stage_3 == 1 ~ 3,
                                 ckm_stage_4 == 1 ~ 4,
                                 TRUE ~ NA_real_
                               )
)

# 创建不包含NA的频数表
stage_counts <- table(ckm_ukb_data$ckm_stage)
stage_props <- prop.table(stage_counts) * 100

stage_summary <- data.frame(
  Stage = c("Stage 0", "Stage 1", "Stage 2", "Stage 3", "Stage 4"),
  N = as.numeric(stage_counts),
  Percentage = round(as.numeric(stage_props), 2)
)

print(stage_summary)


library(ggplot2)
library(dplyr)
library(tidyr)

# 1. CKM分期的总体分布 - 条形图
p1 <- ggplot(stage_summary, aes(x = Stage, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), 
            color = "white") +
  labs(title = "CKM Stage Distribution",
       x = "Stage",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey90")) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 20))

# 2. 风险因素分布热图
# 准备数据
risk_factors <- c("overweight_obesity","abdominal_obesity", "prediabetes", "hypertension", 
                  "mets", "hypertriglyceridemia", "diabetes",
                  "ckd","subclinical_cvd","clinical_cvd" )
risk_factor_names <- c("Overweight/Obesity", "Abdominal Obesity","Prediabetes", "Hypertension", 
                       "MetS", "Hypertriglyceridemia","Diabetes",
                       "CKD","Subclinical CVD","Clinical CVD")

# 计算每个阶段中各风险因素的比例
stage_risk_matrix <- matrix(nrow = length(risk_factors), ncol = 5)
for(i in 0:4) {
  stage_data <- subset(ckm_ukb_data, ckm_stage == i)
  for(j in 1:length(risk_factors)) {
    stage_risk_matrix[j, i+1] <- mean(stage_data[[risk_factors[j]]] == 1, na.rm = TRUE) * 100
  }
}

# 转换为数据框
heatmap_data <- as.data.frame(stage_risk_matrix)
colnames(heatmap_data) <- paste("Stage", 0:4)
heatmap_data$Risk_Factor <- risk_factor_names
heatmap_data_long <- gather(heatmap_data, Stage, Percentage, -Risk_Factor)

# 创建热图
p2 <- ggplot(heatmap_data_long, aes(x = Stage, y = Risk_Factor, fill = Percentage)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            color = "black", size = 3.5) +
  labs(title = "Risk Factor Distribution Across CKM Stages",
       fill = "Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# 垂直排列两个图
library(gridExtra)
combined_plot <- grid.arrange(p1, p2, ncol = 1, heights = c(1, 1.2))