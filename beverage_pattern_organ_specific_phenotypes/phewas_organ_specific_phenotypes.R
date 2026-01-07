#################################Beverage & organ-related phenotypes: Total analyses############################################
organ_related_phenotype <- fread("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/organ_related_phenotype.csv")
organ_related_phenotype_2 <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/cardiovascular_index_dataset_participant.csv")
names(organ_related_phenotype)
names(organ_related_phenotype_2)


simplified_names <- c(
  "eid",  # Participant.ID
  "age",  # Age.at.recruitment
  "sex",  # Sex
  "ankle_width",  # Ankle.spacing.width
  "vo2max_kg",  # V02max per kg bodyweight
  
  "aao_distensibility",  # Ascending aorta distensibility
  "aao_max_area",  # Ascending aorta maximum area
  "aao_min_area",  # Ascending aorta minimum area
  "dao_distensibility",  # Descending aorta distensibility
  "dao_max_area",  # Descending aorta maximum area
  "dao_min_area",  # Descending aorta minimum area
  
  "idp_version_i2",  # IDP pipeline version Instance 2
  "idp_version_i3",  # IDP pipeline version Instance 3
  
  "la_ef",  # LA ejection fraction
  "la_max_vol",  # LA maximum volume
  "la_min_vol",  # LA minimum volume
  "la_sv",  # LA stroke volume
  "lv_co",  # LV cardiac output

  paste0("lv_circ_strain_", 1:16),
  "lv_circ_strain_global",
  
  "lv_ef",  # LV ejection fraction
  "lv_edv",  # LV end diastolic volume
  "lv_esv",  # LV end systolic volume
  
  paste0("lv_long_strain_", 1:6),
  "lv_long_strain_global",
  
  paste0("lv_wall_thick_", 1:16),
  "lv_wall_thick_global",
  
  "lv_mass",  # LV myocardial mass
  
  paste0("lv_rad_strain_", 1:16),
  "lv_rad_strain_global",
  
  "lv_sv",  # LV stroke volume

  "ra_ef",  # RA ejection fraction
  "ra_max_vol",  # RA maximum volume
  "ra_min_vol",  # RA minimum volume
  "ra_sv",  # RA stroke volume
  
  "rv_ef",  # RV ejection fraction
  "rv_edv",  # RV end diastolic volume
  "rv_esv",  # RV end systolic volume
  "rv_sv"   # RV stroke volume
)

names(organ_related_phenotype_2) <- simplified_names

names(organ_related_phenotype)
organ_related_phenotype <- merge(organ_related_phenotype[,c(1:82)],organ_related_phenotype_2[,c(1,4:11,14:89)],all.x = T)
names(organ_related_phenotype)


# 1. FEV1/FVC ratio 
organ_related_phenotype$FEV1_FVC_ratio <- organ_related_phenotype$FEV1 / organ_related_phenotype$FVC

# 2. Waist-hip circumference ratio 
organ_related_phenotype$waist_hip_ratio <- organ_related_phenotype$waist_circumference / organ_related_phenotype$hip_circumference

# 3. Heel bone mineral density average 
organ_related_phenotype$heel_BMD_average <- (organ_related_phenotype$heel_BMD_left + organ_related_phenotype$heel_BMD_right) / 2

# 4. Handgrip strength average
organ_related_phenotype$handgrip_strength_average <- (organ_related_phenotype$hand_grip_strength_left + organ_related_phenotype$hand_grip_strength_right) / 2


brain_mri_cat_aparc192_aseg190_famd135 <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/brain_mri_cat_aparc192_aseg190_famd135.csv")
names(brain_mri_cat_aparc192_aseg190_famd135)

organ_related_phenotype <- merge(organ_related_phenotype,brain_mri_cat_aparc192_aseg190_famd135[,c(1:3,13:214,256:367)],all.x = T)
names(organ_related_phenotype)



recognitive_tests <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/cognitive_function_2016_dataset_participant.csv")
names(recognitive_tests)

library(dplyr)

recognitive_tests <- recognitive_tests %>%
  rename(
    eid = Participant.ID,
    
    # Fluid intelligence 
    fluid_int_0 = `Fluid.intelligence.score...Instance.0`,
    fluid_int_1 = `Fluid.intelligence.score...Instance.1`,
    fluid_int_attempted_0 = `Number.of.fluid.intelligence.questions.attempted.within.time.limit...Instance.0`,
    fluid_int_attempted_1 = `Number.of.fluid.intelligence.questions.attempted.within.time.limit...Instance.1`,
    
    # Trail making test 
    trail1_duration_0 = `Duration.to.complete.numeric.path..trail..1....Instance.0`,
    trail1_duration_1 = `Duration.to.complete.numeric.path..trail..1....Instance.1`,
    trail2_duration_0 = `Duration.to.complete.alphanumeric.path..trail..2....Instance.0`,
    trail2_duration_1 = `Duration.to.complete.alphanumeric.path..trail..2....Instance.1`,
    trail1_errors_0 = `Total.errors.traversing.numeric.path..trail..1....Instance.0`,
    trail1_errors_1 = `Total.errors.traversing.numeric.path..trail..1....Instance.1`,
    trail2_errors_0 = `Total.errors.traversing.alphanumeric.path..trail..2....Instance.0`,
    trail2_errors_1 = `Total.errors.traversing.alphanumeric.path..trail..2....Instance.1`,
    
    # Symbol digit matching 
    symbol_correct_0 = `Number.of.symbol.digit.matches.made.correctly...Instance.0`,
    symbol_correct_1 = `Number.of.symbol.digit.matches.made.correctly...Instance.1`,
    symbol_attempted_0 = `Number.of.symbol.digit.matches.attempted...Instance.0`,
    symbol_attempted_1 = `Number.of.symbol.digit.matches.attempted...Instance.1`,
    
    # Matching rounds 
    match_errors_r1 = `Number.of.incorrect.matches.in.round...Instance.0...Array.0`,
    match_errors_r2 = `Number.of.incorrect.matches.in.round...Instance.0...Array.1`,
    match_errors_r3 = `Number.of.incorrect.matches.in.round...Instance.0...Array.2`,
    match_time_r1 = `Time.to.complete.round...Instance.0...Array.0`,
    match_time_r2 = `Time.to.complete.round...Instance.0...Array.1`,
    match_time_r3 = `Time.to.complete.round...Instance.0...Array.2`,
    
    # Memory & reasoning 
    max_digits_recalled_0 = `Maximum.digits.remembered.correctly...Instance.0`,
    puzzles_solved_1 = `Number.of.puzzles.correctly.solved...Instance.1`
  )

names(recognitive_tests)

organ_related_phenotype <- merge(organ_related_phenotype,recognitive_tests,all.x = T)
names(organ_related_phenotype)



ukb_phewas_data <- fread("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/ukb_phewas_data.csv")
names(ukb_phewas_data)
adipose_data <- ukb_phewas_data[,c(1,103:142,226:327)]
names(adipose_data)
library(dplyr)

adipose_data_simplified <- adipose_data %>%
  select(`Participant ID`, matches("Instance 2")) %>%
  select(-matches("participant - p\\d+_i2")) %>%
  rename_with(~{
    .x %>%
      gsub(" \\| Instance 2.*$", "", .) %>%
      gsub("^Total abdominal adipose tissue index$", "total_abd_adipose_idx", .) %>%
      gsub("^Weight-to-muscle ratio$", "weight_muscle_ratio", .) %>%
      gsub("^Abdominal fat ratio$", "abd_fat_ratio", .) %>%
      gsub("^Muscle fat infiltration$", "muscle_fat_infil", .) %>%
      gsub("^10P Liver PDFF \\(proton density fat fraction\\)$", "liver_pdff", .) %>%
      gsub("^Posterior thigh muscle fat infiltration \\(MFI\\) \\(left\\)$", "post_thigh_mfi_left", .) %>%
      gsub("^Posterior thigh muscle fat infiltration \\(MFI\\) \\(right\\)$", "post_thigh_mfi_right", .) %>%
      
      gsub("VAT error indicator", "vat_error", .) %>%
      gsub("ASAT error indicator", "asat_error", .) %>%
      gsub("Anterior thigh error indicator \\(left\\)", "ant_thigh_error_left", .) %>%
      gsub("Posterior thigh error indicator \\(left\\)", "post_thigh_error_left", .) %>%
      gsub("Anterior thigh error indicator \\(right\\)", "ant_thigh_error_right", .) %>%
      gsub("Posterior thigh error indicator \\(right\\)", "post_thigh_error_right", .) %>%
      gsub("FR liver PDFF mean error indicator", "liver_pdff_error", .) %>%
      
      gsub("^Anterior thigh fat-free muscle volume \\(right\\)$", "ant_thigh_ffmv_right", .) %>%
      gsub("^Posterior thigh fat-free muscle volume \\(right\\)$", "post_thigh_ffmv_right", .) %>%
      gsub("^Anterior thigh fat-free muscle volume \\(left\\)$", "ant_thigh_ffmv_left", .) %>%
      gsub("^Posterior thigh fat-free muscle volume \\(left\\)$", "post_thigh_ffmv_left", .) %>%
      
      gsub("^Visceral adipose tissue volume \\(VAT\\)$", "vat_volume", .) %>%
      gsub("^Abdominal subcutaneous adipose tissue volume \\(ASAT\\)$", "asat_volume", .) %>%
      gsub("^Total thigh fat-free muscle volume$", "total_thigh_ffmv", .) %>%
      gsub("^Total trunk fat volume$", "total_trunk_fat_vol", .) %>%
      gsub("^Total adipose tissue volume$", "total_adipose_vol", .) %>%
      gsub("^Total lean tissue volume$", "total_lean_vol", .) %>%
      
      gsub("^L1-L4 area$", "l1_l4_area", .) %>%
      
      gsub("^Android bone mass$", "android_bone_mass", .) %>%
      gsub("^Android fat mass$", "android_fat_mass", .) %>%
      gsub("^Android lean mass$", "android_lean_mass", .) %>%
      gsub("^Android tissue fat percentage$", "android_fat_pct", .) %>%
      gsub("^Android total mass$", "android_total_mass", .) %>%
      
      gsub("^Arm fat mass \\(left\\)$", "arm_fat_mass_left", .) %>%
      gsub("^Arm lean mass \\(left\\)$", "arm_lean_mass_left", .) %>%
      gsub("^Arm tissue fat percentage \\(left\\)$", "arm_fat_pct_left", .) %>%
      gsub("^Arm total mass \\(left\\)$", "arm_total_mass_left", .) %>%
      
      gsub("^Arm fat mass \\(right\\)$", "arm_fat_mass_right", .) %>%
      gsub("^Arm lean mass \\(right\\)$", "arm_lean_mass_right", .) %>%
      gsub("^Arm tissue fat percentage \\(right\\)$", "arm_fat_pct_right", .) %>%
      gsub("^Arm total mass \\(right\\)$", "arm_total_mass_right", .) %>%
      
      gsub("^Arms fat mass$", "arms_fat_mass", .) %>%
      gsub("^Arms lean mass$", "arms_lean_mass", .) %>%
      gsub("^Arms tissue fat percentage$", "arms_fat_pct", .) %>%
      gsub("^Arms total mass$", "arms_total_mass", .) %>%
      
      gsub("^Gynoid bone mass$", "gynoid_bone_mass", .) %>%
      gsub("^Gynoid fat mass$", "gynoid_fat_mass", .) %>%
      gsub("^Gynoid lean mass$", "gynoid_lean_mass", .) %>%
      gsub("^Gynoid tissue fat percentage$", "gynoid_fat_pct", .) %>%
      gsub("^Gynoid total mass$", "gynoid_total_mass", .) %>%
      
      gsub("^Leg fat mass \\(left\\)$", "leg_fat_mass_left", .) %>%
      gsub("^Leg lean mass \\(left\\)$", "leg_lean_mass_left", .) %>%
      gsub("^Leg tissue fat percentage \\(left\\)$", "leg_fat_pct_left", .) %>%
      gsub("^Leg total mass \\(left\\)$", "leg_total_mass_left", .) %>%
      
      gsub("^Leg fat mass \\(right\\)$", "leg_fat_mass_right", .) %>%
      gsub("^Leg lean mass \\(right\\)$", "leg_lean_mass_right", .) %>%
      gsub("^Leg tissue fat percentage \\(right\\)$", "leg_fat_pct_right", .) %>%
      gsub("^Leg total mass \\(right\\)$", "leg_total_mass_right", .) %>%
      
      gsub("^Legs fat mass$", "legs_fat_mass", .) %>%
      gsub("^Legs lean mass$", "legs_lean_mass", .) %>%
      gsub("^Legs tissue fat percentage$", "legs_fat_pct", .) %>%
      gsub("^Legs total mass$", "legs_total_mass", .) %>%
      
      gsub("^Total fat mass$", "total_fat_mass", .) %>%
      gsub("^Total fat-free mass$", "total_fat_free_mass", .) %>%
      gsub("^Total lean mass$", "total_lean_mass", .) %>%
      gsub("^Total tissue fat percentage$", "total_fat_pct", .) %>%
      gsub("^Total tissue mass$", "total_tissue_mass", .) %>%
      gsub("^Total mass$", "total_mass", .) %>%
      
      gsub("^Trunk fat mass$", "trunk_fat_mass", .) %>%
      gsub("^Trunk lean mass$", "trunk_lean_mass", .) %>%
      gsub("^Trunk tissue fat percentage$", "trunk_fat_pct", .) %>%
      gsub("^Trunk total mass$", "trunk_total_mass", .) %>%
      
      gsub("^VAT \\(visceral adipose tissue\\) mass$", "vat_mass", .) %>%
      gsub("^VAT \\(visceral adipose tissue\\) volume$", "vat_vol", .) %>%
      
      tolower(.)
  }) %>%
  rename(eid = 1)

names(adipose_data_simplified)
dim(adipose_data_simplified)

organ_related_phenotype <- merge(organ_related_phenotype,adipose_data_simplified,all.x = T)
names(organ_related_phenotype)



library(RNOmni)
library(dplyr)

vars_to_normalize <- setdiff(names(organ_related_phenotype), "eid")
organ_related_phenotype_RNT <- organ_related_phenotype


for (var in vars_to_normalize) {
  if (is.numeric(organ_related_phenotype_RNT[[var]])) {
    non_na_indices <- !is.na(organ_related_phenotype_RNT[[var]])
    if (sum(non_na_indices) > 0) {
      organ_related_phenotype_RNT[[var]][non_na_indices] <- 
        RankNorm(organ_related_phenotype_RNT[[var]][non_na_indices], 
                 k = 0.375, 
                 ties.method = "average")
    }
  }
}


beverage_organ_phenotype_data <- merge(beverage_20w_data,ukb_water_turnover[,c(1,208)],all.x = T)
names(beverage_organ_phenotype_data)


beverage_organ_phenotype_data$coffee_WT_met_pect <- beverage_organ_phenotype_data$coffee_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$coffee_WT_met_pect)
beverage_organ_phenotype_data$tea_WT_met_pect <- beverage_organ_phenotype_data$tea_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$tea_WT_met_pect)
beverage_organ_phenotype_data$plain_water_WT_met_pect <- beverage_organ_phenotype_data$plain_water_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$plain_water_WT_met_pect)
beverage_organ_phenotype_data$SSB_WT_met_pect <- beverage_organ_phenotype_data$SSB_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$ASB_WT_met_pect)
beverage_organ_phenotype_data$ASB_WT_met_pect <- beverage_organ_phenotype_data$ASB_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$ASB_WT_met_pect)
beverage_organ_phenotype_data$NJ_WT_met_pect <- beverage_organ_phenotype_data$NJ_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$NJ_WT_met_pect)
beverage_organ_phenotype_data$low_fat_milk_WT_met_pect <- beverage_organ_phenotype_data$low_fat_milk_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$low_fat_milk_WT_met_pect)
beverage_organ_phenotype_data$full_fat_milk_WT_met_pect <- beverage_organ_phenotype_data$full_fat_milk_volume/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$full_fat_milk_WT_met_pect)
beverage_organ_phenotype_data$totoal_WT_met_pect <- (beverage_organ_phenotype_data$coffee_volume+beverage_organ_phenotype_data$tea_volume+beverage_organ_phenotype_data$plain_water_volume+
                                                 beverage_organ_phenotype_data$SSB_volume+beverage_organ_phenotype_data$ASB_volume+
                                                 beverage_organ_phenotype_data$NJ_volume+beverage_organ_phenotype_data$low_fat_milk_volume+
                                                 beverage_organ_phenotype_data$full_fat_milk_volume)/beverage_organ_phenotype_data$Drinking_waterb
summary(beverage_organ_phenotype_data$totoal_WT_met_pect)


quantile(beverage_organ_phenotype_data$totoal_WT_met_pect,c(0.2,0.4,0.6,0.8),na.rm=T)
beverage_organ_phenotype_data$totoal_WT_met_pect_cat[beverage_organ_phenotype_data$totoal_WT_met_pect>=0.9139998  & beverage_organ_phenotype_data$totoal_WT_met_pect<=1.1201480  ] <- 0
beverage_organ_phenotype_data$totoal_WT_met_pect_cat[beverage_organ_phenotype_data$totoal_WT_met_pect>=0.6759667  & beverage_organ_phenotype_data$totoal_WT_met_pect<0.9139998  ] <- 2
beverage_organ_phenotype_data$totoal_WT_met_pect_cat[beverage_organ_phenotype_data$totoal_WT_met_pect<0.6759667  ] <- 1
beverage_organ_phenotype_data$totoal_WT_met_pect_cat[beverage_organ_phenotype_data$totoal_WT_met_pect>=1.1201480  & beverage_organ_phenotype_data$totoal_WT_met_pect<1.3851251  ] <- 3
beverage_organ_phenotype_data$totoal_WT_met_pect_cat[beverage_organ_phenotype_data$totoal_WT_met_pect>1.3851251  ] <- 4
beverage_organ_phenotype_data$totoal_WT_met_pect_cat <- as.factor(beverage_organ_phenotype_data$totoal_WT_met_pect_cat)
table(beverage_organ_phenotype_data$totoal_WT_met_pect_cat)


beverage_organ_phenotype_data <- merge(beverage_organ_phenotype_data,organ_related_phenotype_RNT,all.x = T)
names(beverage_organ_phenotype_data)


##################################Regression Analyses: multiple organ-related phenotypes####################################
library(dplyr)

beverage_organ_phenotype_data <- beverage_organ_phenotype_data %>%
  mutate(
    coffee_WT_cat = case_when(
      is.na(coffee_WT_met_pect) ~ NA_real_,
      coffee_WT_met_pect == 0 ~ 0,
      coffee_WT_met_pect <= 0.4 ~ 1,
      TRUE ~ 2
    ),
    coffee_WT_cat = factor(coffee_WT_cat, levels = c(0, 1, 2)),
    
    tea_WT_cat = case_when(
      is.na(tea_WT_met_pect) ~ NA_real_,
      tea_WT_met_pect == 0 ~ 0,
      tea_WT_met_pect <= 1 ~ 1,
      TRUE ~ 2
    ),
    tea_WT_cat = factor(tea_WT_cat, levels = c(0, 1, 2)),
    
    NJ_WT_cat = case_when(
      is.na(NJ_WT_met_pect) ~ NA_real_,
      NJ_WT_met_pect == 0 ~ 0,
      NJ_WT_met_pect <= 0.3 ~ 1,
      TRUE ~ 2
    ),
    NJ_WT_cat = factor(NJ_WT_cat, levels = c(0, 1, 2))
  )

cat("\n\nCoffee categories:\n")
print(table(beverage_organ_phenotype_data$coffee_WT_cat, useNA = "always"))

cat("\nTea categories:\n")
print(table(beverage_organ_phenotype_data$tea_WT_cat, useNA = "always"))

cat("\nNatural juice categories:\n")
print(table(beverage_organ_phenotype_data$NJ_WT_cat, useNA = "always"))



names(beverage_organ_phenotype_data)

library(dplyr)
library(broom)
library(pbapply)
library(parallel)

covariates <- "age + sex + ethnic + education_years + household_income + bmi + smk_num + smk_qyr + alcohol_intake_frequency + diet_score + PA_mod_vig_150 + overall_health_rating + total_energy_intake"

other_beverages <- c("coffee_WT_met_pect", "tea_WT_met_pect", "plain_water_WT_met_pect",
                     "SSB_WT_met_pect", "ASB_WT_met_pect", "NJ_WT_met_pect", 
                     "low_fat_milk_WT_met_pect", "full_fat_milk_WT_met_pect")

outcomes <- c(
  "pulse_rate", "FVC", "FEV1", "PEF", "body_fat_percentage", "whole_body_water_mass", 
  "DBP", "SBP", "FEV1_FVC_ratio", "waist_hip_ratio", "heel_BMD_average", 
  "handgrip_strength_average", "ankle_width", "vo2max_kg",
  
  "aao_distensibility", "aao_max_area", "aao_min_area", "dao_distensibility", 
  "dao_max_area", "dao_min_area", "la_ef", "la_max_vol", "la_min_vol", "la_sv",
  "lv_co", paste0("lv_circ_strain_", 1:16), "lv_circ_strain_global",
  "lv_ef", "lv_edv", "lv_esv",
  paste0("lv_long_strain_", 1:6), "lv_long_strain_global",
  paste0("lv_wall_thick_", 1:16), "lv_wall_thick_global", "lv_mass",
  paste0("lv_rad_strain_", 1:16), "lv_rad_strain_global", "lv_sv",
  "ra_ef", "ra_max_vol", "ra_min_vol", "ra_sv",
  "rv_ef", "rv_edv", "rv_esv", "rv_sv",
  
  "WBC_count", "RBC_count", "hemoglobin", "hematocrit", "MCV", "MCH", "MCHC", 
  "RBC_distribution_width", "platelet_count", "platelet_crit", "mean_platelet_volume", 
  "platelet_distribution_width", "lymphocyte_count", "monocyte_count", "neutrophil_count", 
  "eosinophil_count", "basophil_count", "nucleated_RBC_count", "lymphocyte_percentage",
  "monocyte_percentage", "neutrophil_percentage", "eosinophil_percentage",
  "basophil_percentage", "nucleated_RBC_percentage", "reticulocyte_percentage",
  "reticulocyte_count", "mean_reticulocyte_volume", "mean_sphered_cell_volume",
  "immature_reticulocyte_fraction", "high_light_scatter_reticulocyte_pct",
  "high_light_scatter_reticulocyte_count",
  
  "creatinine_urine", "potassium_urine", "sodium_urine", "albumin", 
  "alkaline_phosphatase", "ALT", "apolipoprotein_A", "apolipoprotein_B", "AST", 
  "direct_bilirubin", "urea", "calcium", "cholesterol", "creatinine", "CRP", 
  "cystatin_C", "GGT", "glucose", "HbA1c", "HDL_cholesterol", "IGF1", "LDL_direct", 
  "lipoprotein_A", "phosphate", "SHBG", "total_bilirubin", "testosterone", 
  "total_protein", "triglycerides", "urate", "vitamin_D",
  
  "area_L_total", "area_R_total", 
  paste0("area_L_", c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", 
                      "cuneus", "entorhinal", "frontalpole", "fusiform", "inferiorparietal",
                      "inferiortemporal", "insula", "isthmuscingulate", "lateraloccipital",
                      "lateralorbitofrontal", "lingual", "medialorbitofrontal", "middletemporal",
                      "paracentral", "parahippocampal", "parsopercularis", "parsorbitalis",
                      "parstriangularis", "pericalcarine", "postcentral", "posteriorcingulate",
                      "precentral", "precuneus", "rostralanteriorcingulate", "rostralmiddlefrontal",
                      "superiorfrontal", "superiorparietal", "superiortemporal", "supramarginal",
                      "transversetemporal")),
  paste0("area_R_", c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", 
                      "cuneus", "entorhinal", "frontalpole", "fusiform", "inferiorparietal",
                      "inferiortemporal", "insula", "isthmuscingulate", "lateraloccipital",
                      "lateralorbitofrontal", "lingual", "medialorbitofrontal", "middletemporal",
                      "paracentral", "parahippocampal", "parsopercularis", "parsorbitalis",
                      "parstriangularis", "pericalcarine", "postcentral", "posteriorcingulate",
                      "precentral", "precuneus", "rostralanteriorcingulate", "rostralmiddlefrontal",
                      "superiorfrontal", "superiorparietal", "superiortemporal", "supramarginal",
                      "transversetemporal")),
  
  "thickness_L_mean", "thickness_R_mean",
  paste0("thickness_L_", c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", 
                           "cuneus", "entorhinal", "frontalpole", "fusiform", "inferiorparietal",
                           "inferiortemporal", "insula", "isthmuscingulate", "lateraloccipital",
                           "lateralorbitofrontal", "lingual", "medialorbitofrontal", "middletemporal",
                           "paracentral", "parahippocampal", "parsopercularis", "parsorbitalis",
                           "parstriangularis", "pericalcarine", "postcentral", "posteriorcingulate",
                           "precentral", "precuneus", "rostralanteriorcingulate", "rostralmiddlefrontal",
                           "superiorfrontal", "superiorparietal", "superiortemporal", "supramarginal",
                           "transversetemporal")),
  paste0("thickness_R_", c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", 
                           "cuneus", "entorhinal", "frontalpole", "fusiform", "inferiorparietal",
                           "inferiortemporal", "insula", "isthmuscingulate", "lateraloccipital",
                           "lateralorbitofrontal", "lingual", "medialorbitofrontal", "middletemporal",
                           "paracentral", "parahippocampal", "parsopercularis", "parsorbitalis",
                           "parstriangularis", "pericalcarine", "postcentral", "posteriorcingulate",
                           "precentral", "precuneus", "rostralanteriorcingulate", "rostralmiddlefrontal",
                           "superiorfrontal", "superiorparietal", "superiortemporal", "supramarginal",
                           "transversetemporal")),
  
  paste0("vol_L_", c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", 
                     "cuneus", "entorhinal", "frontalpole", "fusiform", "inferiorparietal",
                     "inferiortemporal", "insula", "isthmuscingulate", "lateraloccipital",
                     "lateralorbitofrontal", "lingual", "medialorbitofrontal", "middletemporal",
                     "paracentral", "parahippocampal", "parsopercularis", "parsorbitalis",
                     "parstriangularis", "pericalcarine", "postcentral", "posteriorcingulate",
                     "precentral", "precuneus", "rostralanteriorcingulate", "rostralmiddlefrontal",
                     "superiorfrontal", "superiorparietal", "superiortemporal", "supramarginal",
                     "transversetemporal")),
  paste0("vol_R_", c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", 
                     "cuneus", "entorhinal", "frontalpole", "fusiform", "inferiorparietal",
                     "inferiortemporal", "insula", "isthmuscingulate", "lateraloccipital",
                     "lateralorbitofrontal", "lingual", "medialorbitofrontal", "middletemporal",
                     "paracentral", "parahippocampal", "parsopercularis", "parsorbitalis",
                     "parstriangularis", "pericalcarine", "postcentral", "posteriorcingulate",
                     "precentral", "precuneus", "rostralanteriorcingulate", "rostralmiddlefrontal",
                     "superiorfrontal", "superiorparietal", "superiortemporal", "supramarginal",
                     "transversetemporal")),
  
  paste0("aseg_vol_", c("L_HolesBeforeFixing", "R_HolesBeforeFixing", "3rdVentricle", 
                        "4thVentricle", "5thVentricle", "L_AccumbensArea", "R_AccumbensArea",
                        "L_Amygdala", "R_Amygdala", "BrainStem", "BrainSeg", "BrainSegNotVent",
                        "BrainSegNotVentSurf", "CC_Anterior", "CC_Central", "CC_MidAnterior",
                        "CC_MidPosterior", "CC_Posterior", "CSF", "L_Caudate", "R_Caudate",
                        "L_CerebellumCortex", "R_CerebellumCortex", "L_CerebellumWM", "R_CerebellumWM",
                        "L_CerebralWhiteMatter", "R_CerebralWhiteMatter", "L_Cortex", "R_Cortex",
                        "eTIV", "L_Hippocampus", "R_Hippocampus", "L_InfLatVent", "R_InfLatVent",
                        "L_LateralVentricle", "R_LateralVentricle", "OpticChiasm", "L_Pallidum",
                        "R_Pallidum", "L_Putamen", "R_Putamen", "SubCortGray", "SupraTentorial",
                        "SupraTentorialNotVent", "L_Thalamus", "R_Thalamus", "TotalGray",
                        "L_VentralDC", "R_VentralDC", "VentricleChoroid", "WM_hypointensities",
                        "L_choroid_plexus", "R_choroid_plexus", "non_WM_hypointensities",
                        "L_vessel", "R_vessel", "BrainSegVol_to_eTIV", "MaskVol_to_eTIV")),
  
  paste0("dti_fa_", c("L_acoustic_radiation", "R_acoustic_radiation", "L_anterior_thalamic_radiation",
                      "R_anterior_thalamic_radiation", "L_cingulum_cingulate", "R_cingulum_cingulate",
                      "L_corticospinal_tract", "R_corticospinal_tract", "forceps_major", "forceps_minor",
                      "L_inferior_fronto_occipital_fasciculus", "R_inferior_fronto_occipital_fasciculus",
                      "L_inferior_longitudinal_fasciculus", "R_inferior_longitudinal_fasciculus",
                      "L_medial_lemniscus", "R_medial_lemniscus", "middle_cerebellar_peduncle",
                      "L_cingulum_parahippocampal", "R_cingulum_parahippocampal", "L_posterior_thalamic_radiation",
                      "R_posterior_thalamic_radiation", "L_superior_longitudinal_fasciculus",
                      "R_superior_longitudinal_fasciculus", "L_superior_thalamic_radiation",
                      "R_superior_thalamic_radiation", "L_uncinate_fasciculus", "R_uncinate_fasciculus")),
  
  paste0("dti_md_", c("L_acoustic_radiation", "R_acoustic_radiation", "L_anterior_thalamic_radiation",
                      "R_anterior_thalamic_radiation", "L_cingulum_cingulate", "R_cingulum_cingulate",
                      "L_corticospinal_tract", "R_corticospinal_tract", "forceps_major", "forceps_minor",
                      "L_inferior_fronto_occipital_fasciculus", "R_inferior_fronto_occipital_fasciculus",
                      "L_inferior_longitudinal_fasciculus", "R_inferior_longitudinal_fasciculus",
                      "L_medial_lemniscus", "R_medial_lemniscus", "middle_cerebellar_peduncle",
                      "L_cingulum_parahippocampal", "R_cingulum_parahippocampal", "L_posterior_thalamic_radiation",
                      "R_posterior_thalamic_radiation", "L_superior_longitudinal_fasciculus",
                      "R_superior_longitudinal_fasciculus", "L_superior_thalamic_radiation",
                      "R_superior_thalamic_radiation", "L_uncinate_fasciculus", "R_uncinate_fasciculus")),
  
  "fluid_int_0", "fluid_int_1", "fluid_int_attempted_0", "fluid_int_attempted_1",
  "trail1_duration_0", "trail1_duration_1", "trail2_duration_0", "trail2_duration_1",
  "trail1_errors_0", "trail1_errors_1", "trail2_errors_0", "trail2_errors_1",
  "symbol_correct_0", "symbol_correct_1", "symbol_attempted_0", "symbol_attempted_1",
  "match_errors_r1", "match_errors_r2", "match_errors_r3",
  "match_time_r1", "match_time_r2", "match_time_r3",
  "max_digits_recalled_0", "puzzles_solved_1",
  
  "total_abd_adipose_idx", "weight_muscle_ratio", "abd_fat_ratio", "muscle_fat_infil",
  "liver_pdff", "post_thigh_mfi_left", "post_thigh_mfi_right",
  "ant_thigh_ffmv_right", "post_thigh_ffmv_right", "ant_thigh_ffmv_left", "post_thigh_ffmv_left",
  "vat_volume", "asat_volume", "total_thigh_ffmv", "total_trunk_fat_vol", "total_adipose_vol",
  "total_lean_vol", "l1_l4_area",
  "android_bone_mass", "android_fat_mass", "android_lean_mass", "android_fat_pct", "android_total_mass",
  "arm_lean_mass_left", "arm_fat_pct_left", "arm_total_mass_left",
  "arm_lean_mass_right", "arm_fat_pct_right", "arm_total_mass_right",
  "arms_fat_mass", "arms_lean_mass", "arms_fat_pct", "arms_total_mass",
  "gynoid_bone_mass", "gynoid_fat_mass", "gynoid_lean_mass", "gynoid_fat_pct", "gynoid_total_mass",
  "leg_lean_mass_left", "leg_fat_pct_left", "leg_total_mass_left",
  "leg_lean_mass_right", "leg_fat_pct_right", "leg_total_mass_right",
  "legs_fat_mass", "legs_lean_mass", "legs_fat_pct", "legs_total_mass",
  "total_fat_mass", "total_fat_free_mass", "total_lean_mass", "total_fat_pct",
  "total_tissue_mass", "total_mass", "trunk_lean_mass", "trunk_fat_pct",
  "trunk_total_mass", "vat_mass", "vat_vol"
)

is_brain_imaging <- function(outcome) {
  grepl("^(area_|thickness_|vol_|aseg_vol_)", outcome) & 
    !grepl("^(dti_fa_|dti_md_)", outcome)
}

run_lm_analysis <- function(data, exposure, outcome, covariates, other_beverages = NULL, 
                            need_brain_adjustment = FALSE) {
  
  formula_parts <- c(covariates)
  
  if (!is.null(other_beverages) && length(other_beverages) > 0) {
    if (grepl("_cat$", exposure)) {
      exposure_base <- gsub("_cat$", "_met_pect", exposure)
    } else {
      exposure_base <- exposure
    }
    
    beverages_to_adjust <- setdiff(other_beverages, exposure_base)
    
    if (length(beverages_to_adjust) > 0) {
      formula_parts <- c(formula_parts, paste(beverages_to_adjust, collapse = " + "))
    }
  }
  
  if (need_brain_adjustment) {
    formula_parts <- c(formula_parts, "brain_vol_grey_white_norm")
  }
  
  formula_str <- paste0(outcome, " ~ ", exposure, " + ", paste(formula_parts, collapse = " + "))
  
  tryCatch({
    model <- lm(as.formula(formula_str), data = data)
        result <- tidy(model) %>%
      filter(grepl(paste0("^", exposure), term)) %>%
      mutate(
        outcome = outcome,
        exposure = exposure,
        n = nobs(model)
      ) %>%
      select(exposure, outcome, term, n, estimate, std.error, statistic, p.value)
    
    if (nrow(result) == 0) {
      return(data.frame(
        exposure = exposure,
        outcome = outcome,
        term = exposure,
        n = nobs(model),
        estimate = NA,
        std.error = NA,
        statistic = NA,
        p.value = NA,
        error = "No matching term found in model",
        stringsAsFactors = FALSE
      ))
    }
    
    return(result)
    
  }, error = function(e) {
    return(data.frame(
      exposure = exposure,
      outcome = outcome,
      term = exposure,
      n = NA,
      estimate = NA,
      std.error = NA,
      statistic = NA,
      p.value = NA,
      error = as.character(e$message),
      stringsAsFactors = FALSE
    ))
  })
}

check_var_exists <- function(var_name, data) {
  exists <- var_name %in% names(data)
  if (exists) {
    var_class <- class(data[[var_name]])[1]
    n_unique <- length(unique(data[[var_name]]))
    n_missing <- sum(is.na(data[[var_name]]))
    return(list(exists = TRUE, class = var_class, n_unique = n_unique, n_missing = n_missing))
  } else {
    return(list(exists = FALSE))
  }
}



setup_parallel <- function(n_cores = 12) {
  num_cores <- detectCores()
  use_cores <- min(n_cores, max(1, num_cores - 2))  
  cl <- makeCluster(use_cores)
  
  clusterExport(cl, c(
    "beverage_organ_phenotype_data",
    "covariates",
    "other_beverages",
    "outcomes",
    "run_lm_analysis",
    "is_brain_imaging"
  ), envir = .GlobalEnv)
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(broom)
  })
  
  return(cl)
}

cl <- setup_parallel(n_cores = 12)

##beverage_preference
if ("beverage_preference" %in% names(beverage_organ_phenotype_data)) {
  cat(sprintf("\start %d outcomes...\n", length(outcomes)))
  
  time_start <- Sys.time()
  results_preference <- pblapply(outcomes, function(outcome) {
    need_brain_adj <- is_brain_imaging(outcome)
    run_lm_analysis(
      data = beverage_organ_phenotype_data,
      exposure = "beverage_preference",
      outcome = outcome,
      covariates = covariates,
      other_beverages = NULL,
      need_brain_adjustment = need_brain_adj
    )
  }, cl = cl) %>% bind_rows()
  time_end <- Sys.time()
  
  cat(sprintf("\n✓ analyses complete！: %.1f secs\n", 
              as.numeric(difftime(time_end, time_start, units = "secs"))))
  cat(sprintf("  - succusses: %d\n", nrow(results_preference)))
  cat(sprintf("  - succusses: %d\n", sum(!is.na(results_preference$p.value))))
  cat(sprintf("  - failed: %d\n", sum(is.na(results_preference$p.value))))
  
  if (sum(is.na(results_preference$p.value)) > 0) {
    cat("\nfailed example:\n")
    failed <- results_preference %>% filter(is.na(p.value)) %>% head(3)
    if ("error" %in% names(failed)) {
      print(failed %>% select(exposure, outcome, error))
    }
  }
} else {
  cat("\n✗ skip: beverage_preference does not existed\n")
  results_preference <- data.frame()
}

