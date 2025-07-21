olink_data_organ_age <- fread("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/olink_data_organ_age")

###############################################Individual drink & Organ ages###################################################################
olink_data_organ_age <- fread("~/organ_age_proteomics/olink_data_organ_age")
names(olink_data_organ_age)
names(beverage_20w_data)
names(imputed_data_2)

beverage_organ_age_data <- merge(olink_data_organ_age[,c(1:15,17:29)],beverage_20w_data[c(1:9,46:54)],all.x=T)
beverage_organ_age_data <- merge(beverage_organ_age_data,imputed_data_2,all.x=T)

str(beverage_organ_age_data)
setDT(beverage_organ_age_data)

# 加载必要的包
library(data.table)
library(dplyr)

# 设置数据为data.table（如果还不是的话）
setDT(beverage_organ_age_data)

# 定义年龄差异类型
age_gap_types <- c("Heart_age_gap", "Kidney_age_gap", "Artery_age_gap",
                   "Brain_age_gap", "Adipose_age_gap", "Muscle_age_gap", 
                   "Liver_age_gap", "Immune_age_gap", "Lung_age_gap", 
                   "Intestine_age_gap", "Pancreas_age_gap")

# 定义回归函数
run_regression_analysis <- function(age_gap_type) {
  # 创建公式
  formula_str <- paste(age_gap_type, 
                       "~ coffee_intake + tea_intake + SSB_intake + ASB_intake + NJ_intake +", 
                       "lowfat_milk_intake + fullfat_milk_intake + plain_water_intake +",
                       "age + sex + race + education + household_income + bmi +", 
                       "smoking_status + alcohol_intake_frequency + diet_score +",
                       "total_alcohol + PA_min + PA_mod_vig_150 + overall_health_rating")
  
  # 移除换行符和多余空格
  formula_str <- gsub("\\s+", " ", formula_str)
  
  # 选择需要的变量
  model_vars <- c(age_gap_type, 
                  # 饮料变量
                  "coffee_intake", "tea_intake", "SSB_intake", "ASB_intake", 
                  "NJ_intake", "lowfat_milk_intake", "fullfat_milk_intake", "plain_water_intake",
                  # 协变量
                  "age", "sex", "race", "education", "household_income", "bmi", 
                  "smoking_status", "alcohol_intake_frequency", "diet_score",
                  "total_alcohol", "PA_min", "PA_mod_vig_150", "overall_health_rating")
  
  # 转换为data.frame并创建分析数据集
  df <- as.data.frame(beverage_organ_age_data)
  
  # 检查变量是否存在
  missing_vars <- model_vars[!model_vars %in% names(df)]
  if(length(missing_vars) > 0) {
    cat("Missing variables for", age_gap_type, ":", paste(missing_vars, collapse = ", "), "\n")
    return(NULL)
  }
  
  # 选择完整案例
  analysis_data <- df[, model_vars]
  analysis_data <- analysis_data[complete.cases(analysis_data), ]
  
  cat("Complete cases for", age_gap_type, ":", nrow(analysis_data), "\n")
  
  # 检查数据量
  if(nrow(analysis_data) < 100) {
    cat("Insufficient data for", age_gap_type, "(n =", nrow(analysis_data), ")\n")
    return(NULL)
  }
  
  # 拟合模型
  tryCatch({
    model <- lm(as.formula(formula_str), data = analysis_data)
    
    # 提取结果
    coefs <- summary(model)$coefficients
    result <- data.frame(
      term = rownames(coefs),
      estimate = coefs[, "Estimate"],
      std.error = coefs[, "Std. Error"],
      statistic = coefs[, "t value"],
      p.value = coefs[, "Pr(>|t|)"],
      age_gap_type = age_gap_type,
      n_obs = nrow(analysis_data),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }, error = function(e) {
    cat("Error in regression for", age_gap_type, ":", e$message, "\n")
    return(NULL)
  })
}

# 运行所有回归分析
cat("Starting regression analyses...\n")
cat("=", rep("=", 50), "\n")

all_results <- list()
successful_analyses <- 0

for(i in seq_along(age_gap_types)) {
  age_gap <- age_gap_types[i]
  cat("\nProcessing", i, "of", length(age_gap_types), ":", age_gap, "\n")
  cat("-", rep("-", 40), "\n")
  
  result <- run_regression_analysis(age_gap)
  
  if(!is.null(result)) {
    all_results[[age_gap]] <- result
    successful_analyses <- successful_analyses + 1
    cat("✓ Successfully processed", age_gap, "\n")
  } else {
    cat("✗ Failed to process", age_gap, "\n")
  }
}

cat("\n", "=", rep("=", 50), "\n")
cat("Summary: Successfully analyzed", successful_analyses, "out of", length(age_gap_types), "organs\n")

# 合并结果并进行FDR校正
if(length(all_results) > 0) {
  final_results <- do.call(rbind, all_results)
  rownames(final_results) <- NULL
  
  cat("Total results:", nrow(final_results), "coefficients\n\n")
  
  # 提取饮料相关的结果
  beverage_terms <- c("coffee_intake", "tea_intake", "SSB_intake", "ASB_intake", 
                      "NJ_intake", "lowfat_milk_intake", "fullfat_milk_intake", 
                      "plain_water_intake")
  
  beverage_results <- final_results[final_results$term %in% beverage_terms, ]
  
  # ================================
  # 方法1：对所有饮料-器官组合进行FDR校正
  # ================================
  
  cat("Applying FDR correction to all", nrow(beverage_results), "beverage-organ associations...\n")
  
  beverage_results$p.value.fdr <- p.adjust(beverage_results$p.value, method = "fdr")
  
  # 添加显著性标记
  beverage_results$significance_raw <- ifelse(beverage_results$p.value < 0.001, "***",
                                              ifelse(beverage_results$p.value < 0.01, "**",
                                                     ifelse(beverage_results$p.value < 0.05, "*", "")))
  
  beverage_results$significance_fdr <- ifelse(beverage_results$p.value.fdr < 0.001, "***",
                                              ifelse(beverage_results$p.value.fdr < 0.01, "**",
                                                     ifelse(beverage_results$p.value.fdr < 0.05, "*", "")))
  
  # 按饮料类型和器官排序
  beverage_results <- beverage_results[order(beverage_results$term, beverage_results$age_gap_type), ]
  
  # ================================
  # 结果展示
  # ================================
  
  cat("\nBEVERAGE INTAKE ASSOCIATIONS WITH ORGAN AGE GAPS (FDR CORRECTED)\n")
  cat("=", rep("=", 80), "\n")
  
  # 创建显示表格
  results_display <- data.frame(
    Organ = beverage_results$age_gap_type,
    Beverage = beverage_results$term,
    Beta = sprintf("%.4f", beverage_results$estimate),
    SE = sprintf("%.4f", beverage_results$std.error),
    P_raw = sprintf("%.4f", beverage_results$p.value),
    P_FDR = sprintf("%.4f", beverage_results$p.value.fdr),
    Sig_raw = beverage_results$significance_raw,
    Sig_FDR = beverage_results$significance_fdr,
    N = beverage_results$n_obs
  )
  
  print(results_display)
  
  # ================================
  # 显著性结果汇总
  # ================================
  
  cat("\n\nSIGNIFICANT ASSOCIATIONS SUMMARY:\n")
  cat("=", rep("=", 50), "\n")
  
  # 原始p值显著结果
  sig_raw <- beverage_results[beverage_results$p.value < 0.05, ]
  cat("Raw p-value < 0.05:", nrow(sig_raw), "associations\n")
  
  # FDR校正后显著结果
  sig_fdr <- beverage_results[beverage_results$p.value.fdr < 0.05, ]
  cat("FDR-corrected p-value < 0.05:", nrow(sig_fdr), "associations\n")
  
  # ================================
  # 详细显著结果
  # ================================
  
  if(nrow(sig_raw) > 0) {
    cat("\n\nSIGNIFICANT ASSOCIATIONS (Raw p < 0.05):\n")
    cat("-", rep("-", 60), "\n")
    sig_raw_display <- sig_raw[, c("age_gap_type", "term", "estimate", "p.value", "significance_raw")]
    sig_raw_display$estimate <- sprintf("%.4f", sig_raw_display$estimate)
    sig_raw_display$p.value <- sprintf("%.4f", sig_raw_display$p.value)
    print(sig_raw_display)
  }
  
  if(nrow(sig_fdr) > 0) {
    cat("\n\nSIGNIFICANT ASSOCIATIONS (FDR-corrected p < 0.05):\n")
    cat("-", rep("-", 60), "\n")
    sig_fdr_display <- sig_fdr[, c("age_gap_type", "term", "estimate", "p.value", "p.value.fdr", "significance_fdr")]
    sig_fdr_display$estimate <- sprintf("%.4f", sig_fdr_display$estimate)
    sig_fdr_display$p.value <- sprintf("%.4f", sig_fdr_display$p.value)
    sig_fdr_display$p.value.fdr <- sprintf("%.4f", sig_fdr_display$p.value.fdr)
    print(sig_fdr_display)
  } else {
    cat("\n\nNo associations remain significant after FDR correction.\n")
  }
  
  # ================================
  # 按饮料类型汇总
  # ================================
  
  cat("\n\nSUMMARY BY BEVERAGE TYPE:\n")
  cat("=", rep("=", 40), "\n")
  
  for(beverage in beverage_terms) {
    bev_data <- beverage_results[beverage_results$term == beverage, ]
    if(nrow(bev_data) > 0) {
      cat("\n", toupper(gsub("_", " ", beverage)), ":\n")
      
      # 原始显著性
      significant_organs_raw <- bev_data[bev_data$p.value < 0.05, "age_gap_type"]
      if(length(significant_organs_raw) > 0) {
        cat("  Raw p < 0.05:", paste(significant_organs_raw, collapse = ", "), "\n")
      }
      
      # FDR校正后显著性
      significant_organs_fdr <- bev_data[bev_data$p.value.fdr < 0.05, "age_gap_type"]
      if(length(significant_organs_fdr) > 0) {
        cat("  FDR p < 0.05:", paste(significant_organs_fdr, collapse = ", "), "\n")
        
        # 显示效应方向
        fdr_sig_data <- bev_data[bev_data$p.value.fdr < 0.05, ]
        positive_effects <- sum(fdr_sig_data$estimate > 0)
        negative_effects <- sum(fdr_sig_data$estimate < 0)
        
        if(positive_effects > 0) cat("    Accelerated aging:", positive_effects, "organs\n")
        if(negative_effects > 0) cat("    Decelerated aging:", negative_effects, "organs\n")
      }
      
      if(length(significant_organs_raw) == 0 && length(significant_organs_fdr) == 0) {
        cat("  No significant associations\n")
      }
    }
  }
  
  # ================================
  # 保存结果
  # ================================
  
  # 将结果保存到全局环境
  beverage_organ_associations <<- beverage_results
  
  cat("\n\nResults saved as 'beverage_organ_associations'\n")
  cat("Total tests performed:", nrow(beverage_results), "\n")
  cat("FDR correction applied across all", nrow(beverage_results), "tests\n")
  
} else {
  cat("No successful regressions completed.\n")
}

cat("\nAnalysis completed!\n")


summary(beverage_organ_age_data$coffee_intake)
beverage_organ_age_data$coffee_tea_type <- ifelse(beverage_organ_age_data$coffee_intake_cat==2 | beverage_organ_age_data$tea_intake_cat==2,1,0)
beverage_organ_age_data$coffee_tea_type <- as.factor(beverage_organ_age_data$coffee_tea_type)
table(beverage_organ_age_data$coffee_tea_type)

beverage_organ_age_data$SSB_ASB_type <- ifelse(beverage_organ_age_data$SSB_intake_cat==2 | beverage_organ_age_data$ASB_intake_cat==2,1,0)
beverage_organ_age_data$SSB_ASB_type <- as.factor(beverage_organ_age_data$SSB_ASB_type)
table(beverage_organ_age_data$SSB_ASB_type)


beverage_organ_age_data$NJ_water_type <- ifelse(beverage_organ_age_data$NJ_intake_cat==2 | beverage_organ_age_data$plain_water_intake_cat==2,1,0)
beverage_organ_age_data$NJ_water_type <- as.factor(beverage_organ_age_data$NJ_water_type)
table(beverage_organ_age_data$NJ_water_type)


beverage_organ_age_data$coffee_tea_preference <- ifelse(
  beverage_organ_age_data$coffee_tea_type == "1" & 
    beverage_organ_age_data$SSB_ASB_type == "0" & 
    beverage_organ_age_data$NJ_water_type == "0", 1, 0)
table(beverage_organ_age_data$coffee_tea_preference)

beverage_organ_age_data$SSB_ASB_preference <- ifelse(
  beverage_organ_age_data$SSB_ASB_type == "1" & 
    beverage_organ_age_data$coffee_tea_type == "0" & 
    beverage_organ_age_data$NJ_water_type == "0", 1, 0)
table(beverage_organ_age_data$SSB_ASB_preference)

beverage_organ_age_data$NJ_water_preference <- ifelse(
  beverage_organ_age_data$NJ_water_type == "1" & 
    beverage_organ_age_data$coffee_tea_type == "0" & 
    beverage_organ_age_data$SSB_ASB_type == "0", 1, 0)
table(beverage_organ_age_data$NJ_water_preference)



# 加载必要的包
library(data.table)
library(dplyr)

# 设置数据为data.table（如果还不是的话）
setDT(beverage_organ_age_data)

# 定义年龄差异类型
age_gap_types <- c("Heart_age_gap", "Kidney_age_gap", "Artery_age_gap",
                   "Brain_age_gap", "Adipose_age_gap", "Muscle_age_gap", 
                   "Liver_age_gap", "Immune_age_gap", "Lung_age_gap", 
                   "Intestine_age_gap", "Pancreas_age_gap")

# 检查偏好变量的分布
cat("BEVERAGE PREFERENCE DISTRIBUTIONS:\n")
cat("=", rep("=", 50), "\n")
cat("Coffee/Tea preference:\n")
print(table(beverage_organ_age_data$coffee_tea_preference))
cat("\nSSB/ASB preference:\n")
print(table(beverage_organ_age_data$SSB_ASB_preference))
cat("\nNJ/Water preference:\n")
print(table(beverage_organ_age_data$NJ_water_preference))
cat("\n")

# 定义回归函数 - 修改为使用偏好变量
run_preference_regression_analysis <- function(age_gap_type) {
  # 创建公式 - 使用偏好变量替代单独的饮料变量
  formula_str <- paste(age_gap_type, 
                       "~ coffee_tea_preference + SSB_ASB_preference + NJ_water_preference +",
                       "age + sex + race + education + household_income + bmi +", 
                       "smoking_status + alcohol_intake_frequency + diet_score +",
                       "total_alcohol + PA_min + PA_mod_vig_150 + overall_health_rating")
  
  # 移除换行符和多余空格
  formula_str <- gsub("\\s+", " ", formula_str)
  
  # 选择需要的变量
  model_vars <- c(age_gap_type, 
                  # 偏好变量
                  "coffee_tea_preference", "SSB_ASB_preference", "NJ_water_preference",
                  # 协变量
                  "age", "sex", "race", "education", "household_income", "bmi", 
                  "smoking_status", "alcohol_intake_frequency", "diet_score",
                  "total_alcohol", "PA_min", "PA_mod_vig_150", "overall_health_rating")
  
  # 转换为data.frame并创建分析数据集
  df <- as.data.frame(beverage_organ_age_data)
  
  # 检查变量是否存在
  missing_vars <- model_vars[!model_vars %in% names(df)]
  if(length(missing_vars) > 0) {
    cat("Missing variables for", age_gap_type, ":", paste(missing_vars, collapse = ", "), "\n")
    return(NULL)
  }
  
  # 选择完整案例
  analysis_data <- df[, model_vars]
  analysis_data <- analysis_data[complete.cases(analysis_data), ]
  
  cat("Complete cases for", age_gap_type, ":", nrow(analysis_data), "\n")
  
  # 检查数据量
  if(nrow(analysis_data) < 100) {
    cat("Insufficient data for", age_gap_type, "(n =", nrow(analysis_data), ")\n")
    return(NULL)
  }
  
  # 检查偏好变量的变异性
  preference_vars <- c("coffee_tea_preference", "SSB_ASB_preference", "NJ_water_preference")
  for(var in preference_vars) {
    var_table <- table(analysis_data[[var]])
    if(length(var_table) < 2 || min(var_table) < 10) {
      cat("Warning: Limited variation in", var, "for", age_gap_type, "\n")
      print(var_table)
    }
  }
  
  # 拟合模型
  tryCatch({
    model <- lm(as.formula(formula_str), data = analysis_data)
    
    # 提取结果
    coefs <- summary(model)$coefficients
    result <- data.frame(
      term = rownames(coefs),
      estimate = coefs[, "Estimate"],
      std.error = coefs[, "Std. Error"],
      statistic = coefs[, "t value"],
      p.value = coefs[, "Pr(>|t|)"],
      age_gap_type = age_gap_type,
      n_obs = nrow(analysis_data),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }, error = function(e) {
    cat("Error in regression for", age_gap_type, ":", e$message, "\n")
    return(NULL)
  })
}

# 运行所有回归分析
cat("\nStarting beverage preference regression analyses...\n")
cat("=", rep("=", 60), "\n")

all_preference_results <- list()
successful_analyses <- 0

for(i in seq_along(age_gap_types)) {
  age_gap <- age_gap_types[i]
  cat("\nProcessing", i, "of", length(age_gap_types), ":", age_gap, "\n")
  cat("-", rep("-", 40), "\n")
  
  result <- run_preference_regression_analysis(age_gap)
  
  if(!is.null(result)) {
    all_preference_results[[age_gap]] <- result
    successful_analyses <- successful_analyses + 1
    cat("✓ Successfully processed", age_gap, "\n")
  } else {
    cat("✗ Failed to process", age_gap, "\n")
  }
}

cat("\n", "=", rep("=", 60), "\n")
cat("Summary: Successfully analyzed", successful_analyses, "out of", length(age_gap_types), "organs\n")

# 合并结果并进行FDR校正
if(length(all_preference_results) > 0) {
  final_preference_results <- do.call(rbind, all_preference_results)
  rownames(final_preference_results) <- NULL
  
  cat("Total results:", nrow(final_preference_results), "coefficients\n\n")
  
  # 提取偏好相关的结果
  preference_terms <- c("coffee_tea_preference", "SSB_ASB_preference", "NJ_water_preference")
  
  preference_results <- final_preference_results[final_preference_results$term %in% preference_terms, ]
  
  # ================================
  # 对所有偏好-器官组合进行FDR校正
  # ================================
  
  cat("Applying FDR correction to all", nrow(preference_results), "preference-organ associations...\n")
  
  preference_results$p.value.fdr <- p.adjust(preference_results$p.value, method = "fdr")
  
  # 添加显著性标记
  preference_results$significance_raw <- ifelse(preference_results$p.value < 0.001, "***",
                                                ifelse(preference_results$p.value < 0.01, "**",
                                                       ifelse(preference_results$p.value < 0.05, "*", "")))
  
  preference_results$significance_fdr <- ifelse(preference_results$p.value.fdr < 0.001, "***",
                                                ifelse(preference_results$p.value.fdr < 0.01, "**",
                                                       ifelse(preference_results$p.value.fdr < 0.05, "*", "")))
  
  # 按偏好类型和器官排序
  preference_results <- preference_results[order(preference_results$term, preference_results$age_gap_type), ]
  
  # ================================
  # 结果展示
  # ================================
  
  cat("\nBEVERAGE PREFERENCE ASSOCIATIONS WITH ORGAN AGE GAPS (FDR CORRECTED)\n")
  cat("=", rep("=", 80), "\n")
  
  # 创建显示表格
  results_display <- data.frame(
    Organ = preference_results$age_gap_type,
    Preference = preference_results$term,
    Beta = sprintf("%.4f", preference_results$estimate),
    SE = sprintf("%.4f", preference_results$std.error),
    P_raw = sprintf("%.4f", preference_results$p.value),
    P_FDR = sprintf("%.4f", preference_results$p.value.fdr),
    Sig_raw = preference_results$significance_raw,
    Sig_FDR = preference_results$significance_fdr,
    N = preference_results$n_obs
  )
  
  print(results_display)
  
  # ================================
  # 显著性结果汇总
  # ================================
  
  cat("\n\nSIGNIFICANT ASSOCIATIONS SUMMARY:\n")
  cat("=", rep("=", 50), "\n")
  
  # 原始p值显著结果
  sig_raw <- preference_results[preference_results$p.value < 0.05, ]
  cat("Raw p-value < 0.05:", nrow(sig_raw), "associations\n")
  
  # FDR校正后显著结果
  sig_fdr <- preference_results[preference_results$p.value.fdr < 0.05, ]
  cat("FDR-corrected p-value < 0.05:", nrow(sig_fdr), "associations\n")
  
  # ================================
  # 详细显著结果
  # ================================
  
  if(nrow(sig_raw) > 0) {
    cat("\n\nSIGNIFICANT ASSOCIATIONS (Raw p < 0.05):\n")
    cat("-", rep("-", 60), "\n")
    sig_raw_display <- sig_raw[, c("age_gap_type", "term", "estimate", "p.value", "significance_raw")]
    sig_raw_display$estimate <- sprintf("%.4f", sig_raw_display$estimate)
    sig_raw_display$p.value <- sprintf("%.4f", sig_raw_display$p.value)
    print(sig_raw_display)
  }
  
  if(nrow(sig_fdr) > 0) {
    cat("\n\nSIGNIFICANT ASSOCIATIONS (FDR-corrected p < 0.05):\n")
    cat("-", rep("-", 60), "\n")
    sig_fdr_display <- sig_fdr[, c("age_gap_type", "term", "estimate", "p.value", "p.value.fdr", "significance_fdr")]
    sig_fdr_display$estimate <- sprintf("%.4f", sig_fdr_display$estimate)
    sig_fdr_display$p.value <- sprintf("%.4f", sig_fdr_display$p.value)
    sig_fdr_display$p.value.fdr <- sprintf("%.4f", sig_fdr_display$p.value.fdr)
    print(sig_fdr_display)
  } else {
    cat("\n\nNo associations remain significant after FDR correction.\n")
  }
  
  
  # ================================
  # 偏好类型对比分析
  # ================================
  
  cat("\n\nPREFERENCE TYPE COMPARISON:\n")
  cat("=", rep("=", 40), "\n")
  
  # 统计每种偏好的显著关联数量
  for(preference in preference_terms) {
    pref_data <- preference_results[preference_results$term == preference, ]
    raw_sig_count <- sum(pref_data$p.value < 0.05)
    fdr_sig_count <- sum(pref_data$p.value.fdr < 0.05)
    
    cat(sprintf("%-30s: %d raw sig, %d FDR sig\n", 
                gsub("_", " ", toupper(preference)), 
                raw_sig_count, 
                fdr_sig_count))
  }
  
  # ================================
  # 效应大小分析
  # ================================
  
  cat("\n\nEFFECT SIZE ANALYSIS:\n")
  cat("=", rep("=", 40), "\n")
  
  for(preference in preference_terms) {
    pref_data <- preference_results[preference_results$term == preference, ]
    if(nrow(pref_data) > 0) {
      cat("\n", preference_labels[preference], ":\n")
      cat(sprintf("  Mean effect size: %.4f\n", mean(pref_data$estimate)))
      cat(sprintf("  Effect size range: [%.4f, %.4f]\n", 
                  min(pref_data$estimate), max(pref_data$estimate)))
      
      # 最大正效应和负效应
      max_pos_idx <- which.max(pref_data$estimate)
      max_neg_idx <- which.min(pref_data$estimate)
      
      if(pref_data$estimate[max_pos_idx] > 0) {
        cat(sprintf("  Largest positive effect: %.4f (%s, p=%.4f)\n",
                    pref_data$estimate[max_pos_idx],
                    pref_data$age_gap_type[max_pos_idx],
                    pref_data$p.value[max_pos_idx]))
      }
      
      if(pref_data$estimate[max_neg_idx] < 0) {
        cat(sprintf("  Largest negative effect: %.4f (%s, p=%.4f)\n",
                    pref_data$estimate[max_neg_idx],
                    pref_data$age_gap_type[max_neg_idx],
                    pref_data$p.value[max_neg_idx]))
      }
    }
  }
  
  # ================================
  # 保存结果
  # ================================
  
  # 将结果保存到全局环境
  beverage_preference_organ_associations <<- preference_results
  
  cat("\n\nResults saved as 'beverage_preference_organ_associations'\n")
  cat("Total tests performed:", nrow(preference_results), "\n")
  cat("FDR correction applied across all", nrow(preference_results), "tests\n")
  
  # ================================
  # 与原始饮料分析比较（如果存在）
  # ================================
  
  if(exists("beverage_organ_associations")) {
    cat("\n\nCOMPARISON WITH INDIVIDUAL BEVERAGE ANALYSIS:\n")
    cat("=", rep("=", 50), "\n")
    
    # 比较显著关联数量
    original_sig_raw <- sum(beverage_organ_associations$p.value < 0.05)
    original_sig_fdr <- sum(beverage_organ_associations$p.value.fdr < 0.05)
    
    preference_sig_raw <- sum(preference_results$p.value < 0.05)
    preference_sig_fdr <- sum(preference_results$p.value.fdr < 0.05)
    
    cat(sprintf("Individual beverages: %d raw sig, %d FDR sig\n", 
                original_sig_raw, original_sig_fdr))
    cat(sprintf("Preference types: %d raw sig, %d FDR sig\n", 
                preference_sig_raw, preference_sig_fdr))
  }
  
} else {
  cat("No successful preference regressions completed.\n")
}

cat("\nBeverage preference analysis completed!\n")


######################################plot#############################################
str(beverage_organ_associations)
# 加载必要的包
library(pheatmap)
library(dplyr)
library(RColorBrewer)
library(tidyr)

# 检查数据结构
str(beverage_organ_associations)

# 准备数据进行热图绘制
# 包含所有年龄类型（包括前两种）
all_age_types <- c("Conventional_age", "Organismal_age", "Heart_age_gap", "Kidney_age_gap", "Artery_age_gap",
                   "Brain_age_gap", "Adipose_age_gap", "Muscle_age_gap", 
                   "Liver_age_gap", "Immune_age_gap", "Lung_age_gap", 
                   "Intestine_age_gap", "Pancreas_age_gap")

# 筛选数据
heatmap_data <- beverage_organ_associations[beverage_organ_associations$age_gap_type %in% all_age_types, ]

# 创建宽格式数据用于热图 - 使用base R方法避免包冲突
# 创建estimate矩阵
estimate_wide <- reshape(heatmap_data[, c("term", "age_gap_type", "estimate")], 
                         idvar = "term", timevar = "age_gap_type", direction = "wide")
rownames(estimate_wide) <- estimate_wide$term
estimate_wide$term <- NULL
colnames(estimate_wide) <- gsub("estimate.", "", colnames(estimate_wide))
heatmap_matrix <- as.matrix(estimate_wide)

# 创建原始p值矩阵（用于显著性标注）
pvalue_raw_wide <- reshape(heatmap_data[, c("term", "age_gap_type", "p.value")], 
                           idvar = "term", timevar = "age_gap_type", direction = "wide")
rownames(pvalue_raw_wide) <- pvalue_raw_wide$term
pvalue_raw_wide$term <- NULL
colnames(pvalue_raw_wide) <- gsub("p.value.", "", colnames(pvalue_raw_wide))
pvalue_raw_matrix <- as.matrix(pvalue_raw_wide)

# 确保矩阵维度一致
common_rows <- intersect(rownames(heatmap_matrix), rownames(pvalue_raw_matrix))
common_cols <- intersect(colnames(heatmap_matrix), colnames(pvalue_raw_matrix))
heatmap_matrix <- heatmap_matrix[common_rows, common_cols]
pvalue_raw_matrix <- pvalue_raw_matrix[common_rows, common_cols]

# 创建显著性标记矩阵（基于原始p值）
sig_labels <- matrix("", nrow = nrow(pvalue_raw_matrix), ncol = ncol(pvalue_raw_matrix))
rownames(sig_labels) <- rownames(pvalue_raw_matrix)
colnames(sig_labels) <- colnames(pvalue_raw_matrix)

# 添加显著性星号（基于原始p值）
sig_labels[pvalue_raw_matrix < 0.001 & !is.na(pvalue_raw_matrix)] <- "***"
sig_labels[pvalue_raw_matrix >= 0.001 & pvalue_raw_matrix < 0.01 & !is.na(pvalue_raw_matrix)] <- "**"
sig_labels[pvalue_raw_matrix >= 0.01 & pvalue_raw_matrix < 0.05 & !is.na(pvalue_raw_matrix)] <- "*"

# 清理饮料名称（行名）
beverage_names <- c(
  "ASB_intake" = "Artificially Sweetened Beverages",
  "coffee_intake" = "Coffee",
  "fullfat_milk_intake" = "Full-fat Milk",
  "lowfat_milk_intake" = "Low-fat Milk",
  "NJ_intake" = "Natural Juice",
  "plain_water_intake" = "Plain Water",
  "SSB_intake" = "Sugar-sweetened Beverages",
  "tea_intake" = "Tea"
)

# 清理年龄类型名称（列名）- 包含所有13种
age_type_names <- c(
  "Conventional_age" = "Conventional Age",
  "Organismal_age" = "Organismal Age", 
  "Adipose_age_gap" = "Adipose",
  "Artery_age_gap" = "Artery",
  "Brain_age_gap" = "Brain",
  "Heart_age_gap" = "Heart",
  "Immune_age_gap" = "Immune",
  "Intestine_age_gap" = "Intestine",
  "Kidney_age_gap" = "Kidney",
  "Liver_age_gap" = "Liver",
  "Lung_age_gap" = "Lung",
  "Muscle_age_gap" = "Muscle",
  "Pancreas_age_gap" = "Pancreas"
)

# 更新矩阵的行名和列名
# 更新行名
current_row_names <- rownames(heatmap_matrix)
new_row_names <- ifelse(current_row_names %in% names(beverage_names), 
                        beverage_names[current_row_names], 
                        current_row_names)
rownames(heatmap_matrix) <- new_row_names
rownames(sig_labels) <- new_row_names
rownames(pvalue_raw_matrix) <- new_row_names

# 更新列名
current_col_names <- colnames(heatmap_matrix)
new_col_names <- ifelse(current_col_names %in% names(age_type_names), 
                        age_type_names[current_col_names], 
                        current_col_names)
colnames(heatmap_matrix) <- new_col_names
colnames(sig_labels) <- new_col_names
colnames(pvalue_raw_matrix) <- new_col_names

# 重新排序列，确保 Conventional Age 和 Organismal Age 在前面
desired_order <- c("Conventional Age", "Organismal Age", 
                   "Heart", "Kidney", "Artery", "Brain", "Adipose", "Muscle",
                   "Liver", "Immune", "Lung", "Intestine", "Pancreas")

# 获取实际存在的列名
existing_cols <- intersect(desired_order, colnames(heatmap_matrix))
if(length(existing_cols) > 0) {
  heatmap_matrix <- heatmap_matrix[, existing_cols, drop = FALSE]
  sig_labels <- sig_labels[, existing_cols, drop = FALSE]
  pvalue_raw_matrix <- pvalue_raw_matrix[, existing_cols, drop = FALSE]
}

# 设置颜色
colors <- colorRampPalette(c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
                             "white", "#FDBF6F", "#FF7F00", "#E31A1C", "#B10026"))(100)

# 自定义显示数值的函数（保留两位小数）
display_numbers <- matrix(paste0(sprintf("%.4f", heatmap_matrix), "\n", sig_labels), 
                          nrow = nrow(heatmap_matrix))

# 确定合适的颜色范围
range_values <- range(heatmap_matrix, na.rm = TRUE)
max_abs <- max(abs(range_values))
color_breaks <- seq(-max_abs, max_abs, length.out = 101)

# 绘制热图
pheatmap(heatmap_matrix,
              color = colors,
              breaks = color_breaks,
              display_numbers = display_numbers,
              number_color = "black",
              fontsize_number = 7,
              cluster_rows = TRUE,
              cluster_cols = FALSE,  # 不对列进行聚类以保持逻辑顺序
              clustering_distance_rows = "euclidean",
              clustering_method = "complete",
              scale = "none",
              main = "Associations between Beverage Intake and Age/Organ Age Gaps\n(β coefficients with raw p-value significance)",
              fontsize = 10,
              fontsize_row = 9,
              fontsize_col = 9,
              angle_col = "45",
              cellwidth = 35,
              cellheight = 25,
              border_color = "white",
              na_col = "grey90",
              legend = TRUE,
              legend_breaks = seq(-max_abs, max_abs, length.out = 7),
              legend_labels = sprintf("%.2f", seq(-max_abs, max_abs, length.out = 7)))


########################################Beverage/WT % & Organ ages################################
str(beverage_organ_age_data)
names(beverage_organ_age_data)
beverage_organ_age_data <- merge(beverage_organ_age_data,water_turnover_data[,c(1,89:97)],all.x = T)

rcsplot(data = beverage_organ_age_data[!is.na(beverage_organ_age_data$totoal_WT_met_pect),],
        outcome ="Pancreas_age_gap",
        exposure = "coffee_WT_met_pect",
        covariates = c("age","sex", "race" , "education" ,"household_income" , "bmi" , 
                       "smoking_status" , "alcohol_intake_frequency" ,"diet_score",
                       "total_alcohol" , "PA_min" , "PA_mod_vig_150" , "overall_health_rating"),
        ref.value=0,
        knots = knot(3),
        xbreaks= seq(0, 1.2, by = 0.2),
        ybreaks= seq(-0.4, 0.3, by = 0.1),
        linesize=1,
        linecolor="#0487D9"
)


# Load required packages
library(cluster)
library(e1071)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(splines)
library(rms)

# 首先设置数据分布（使用之前的代码获取预测数据）
dd <- datadist(beverage_organ_age_data)
dd$limits["Adjust to", "SSB_WT_met_pect"] <- 0
options(datadist='dd')

# 定义所有age_gap变量
age_gap_vars <- c("Conventional_age_gap", "Organismal_age_gap", "Brain_age_gap", 
                  "Artery_age_gap", "Liver_age_gap", "Immune_age_gap",
                  "Intestine_age_gap", "Lung_age_gap", "Heart_age_gap",
                  "Pancreas_age_gap", "Muscle_age_gap", "Adipose_age_gap", 
                  "Kidney_age_gap")

# 创建协变量公式
covariates <- "coffee_WT_met_pect+tea_WT_met_pect+NJ_WT_met_pect+ASB_WT_met_pect+low_fat_milk_WT_met_pect+full_fat_milk_WT_met_pect+plain_water_WT_met_pect+age + sex + race + education + household_income + bmi + smoking_status + alcohol_intake_frequency+ diet_score + total_alcohol + PA_mod_vig_150 + overall_health_rating"

# 预测范围
SSB_range <- seq(0, quantile(beverage_organ_age_data$SSB_WT_met_pect, 0.9, na.rm = T), length.out = 5)

# 存储所有轨迹数据
trajectory_matrix <- matrix(NA, nrow = length(age_gap_vars), ncol = length(SSB_range))
rownames(trajectory_matrix) <- gsub("_age_gap", "", age_gap_vars)
colnames(trajectory_matrix) <- paste0("SSB_", round(SSB_range, 4))

# 拟合模型并获取轨迹
for(i in 1:length(age_gap_vars)) {
  age_gap <- age_gap_vars[i]
  cat("Processing:", age_gap, "\n")
  
  # 创建模型公式
  model_formula <- paste(age_gap, "~ pol(SSB_WT_met_pect, 3) +", covariates)
  
  # 拟合模型
  fit <- ols(as.formula(model_formula), 
             data = beverage_organ_age_data[!is.na(beverage_organ_age_data$SSB_WT_met_pect),])
  
  # 进行预测
  pred <- Predict(fit, SSB_WT_met_pect = SSB_range, ref.zero = TRUE, conf.int = FALSE)
  
  # 存储轨迹
  trajectory_matrix[i, ] <- as.numeric(pred$yhat)
}

# 1. 确定最优聚类数
# 计算不同聚类数的有效性指数
set.seed(123)
validity_indices <- data.frame()

for(k in 2:6) {
  fcm_result <- cmeans(trajectory_matrix, centers = k, iter.max = 10000, verbose = FALSE)
  
  # 计算各种有效性指数
  pc <- sum(fcm_result$membership^2) / nrow(trajectory_matrix)  # Partition Coefficient
  pe <- -sum(fcm_result$membership * log(fcm_result$membership)) / nrow(trajectory_matrix)  # Partition Entropy
  
  validity_indices <- rbind(validity_indices, 
                            data.frame(k = k, PC = pc, PE = pe))
}

# 绘制有效性指数图
p_validity <- validity_indices %>%
  pivot_longer(cols = c(PC, PE), names_to = "Index", values_to = "Value") %>%
  ggplot(aes(x = k, y = Value, color = Index)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2:6) +
  labs(title = "Fuzzy C-Means Clustering Validity Indices",
       x = "Number of Clusters (k)",
       y = "Index Value") +
  theme_classic() +
  facet_wrap(~ Index, scales = "free_y")

print(p_validity)

# 2. 执行最优的模糊c均值聚类
# 选择PC最大或PE最小的k值
optimal_k <- validity_indices$k[which.max(validity_indices$PC)]
cat("Optimal number of clusters:", optimal_k, "\n")

# 执行模糊聚类
set.seed(123)
fcm_final <- cmeans(trajectory_matrix, centers = optimal_k, iter.max = 10000, verbose = TRUE)

# 获取聚类结果
cluster_assignment <- apply(fcm_final$membership, 1, which.max)
membership_scores <- fcm_final$membership

# 创建结果数据框
cluster_results <- data.frame(
  Age_Gap_Type = rownames(trajectory_matrix),
  Cluster = cluster_assignment,
  Max_Membership = apply(membership_scores, 1, max)
)

# 添加详细的隶属度信息
for(i in 1:optimal_k) {
  cluster_results[paste0("Membership_C", i)] <- membership_scores[, i]
}

# 3. 可视化聚类结果
# 准备绘图数据
plot_data <- data.frame(
  Age_Gap_Type = rep(rownames(trajectory_matrix), each = length(SSB_range)),
  SSB_WT_met_pect = rep(SSB_range, times = nrow(trajectory_matrix)),
  Beta_coefficient = as.vector(t(trajectory_matrix)),
  Cluster = rep(cluster_assignment, each = length(SSB_range))
)

plot_data$Cluster <- as.factor(plot_data$Cluster)
plot_data$Age_Gap_Type <- factor(plot_data$Age_Gap_Type, levels = gsub("_age_gap", "", age_gap_vars))

# 创建聚类颜色
cluster_colors <- rainbow(optimal_k)
names(cluster_colors) <- paste0("Cluster ", 1:optimal_k)

# 绘制聚类轨迹图
p_clusters <- ggplot(plot_data, aes(x = SSB_WT_met_pect, y = Beta_coefficient, 
                                    color = Cluster, group = Age_Gap_Type)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  scale_color_manual(values = cluster_colors, 
                     name = "Cluster",
                     labels = paste0("Cluster ", 1:optimal_k)) +
  labs(title = paste("Fuzzy C-Means Clustering of Age Gap Trajectories (k =", optimal_k, ")"),
       x = "SSB WT Metabolite Percentage",
       y = "Beta coefficient") +
  theme_classic() +
  theme(legend.position = "right")

print(p_clusters)

# 4. 分面图显示每个聚类
p_facet_clusters <- ggplot(plot_data, aes(x = SSB_WT_met_pect, y = Beta_coefficient, 
                                          color = Age_Gap_Type)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_wrap(~ paste("Cluster", Cluster), scales = "free_y") +
  labs(title = "Age Gap Trajectories by Fuzzy Clusters",
       x = "SSB WT Metabolite Percentage",
       y = "Beta coefficient",
       color = "Age Gap Type") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

print(p_facet_clusters)

# 5. 隶属度热图
membership_long <- membership_scores %>%
  as.data.frame() %>%
  mutate(Age_Gap_Type = rownames(trajectory_matrix)) %>%
  pivot_longer(cols = -Age_Gap_Type, names_to = "Cluster", values_to = "Membership") %>%
  mutate(Cluster = paste("Cluster", gsub("V", "", Cluster)))

p_membership <- ggplot(membership_long, aes(x = Cluster, y = Age_Gap_Type, fill = Membership)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "lightblue", high = "darkblue",
                       midpoint = 0.5, name = "Membership\nScore") +
  labs(title = "Fuzzy Membership Scores",
       x = "Cluster", y = "Age Gap Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_membership)

# 6. 输出聚类结果
cat("\n=== Fuzzy C-Means Clustering Results ===\n")
print(cluster_results)

cat("\n=== Cluster Summary ===\n")
for(i in 1:optimal_k) {
  cat("Cluster", i, ":\n")
  cluster_members <- cluster_results$Age_Gap_Type[cluster_results$Cluster == i]
  cat("Members:", paste(cluster_members, collapse = ", "), "\n")
  cat("Average max membership:", 
      round(mean(cluster_results$Max_Membership[cluster_results$Cluster == i]), 3), "\n\n")
}

# 7. 聚类中心轨迹
centers_data <- data.frame(
  SSB_WT_met_pect = rep(SSB_range, times = optimal_k),
  Beta_coefficient = as.vector(t(fcm_final$centers)),
  Cluster = rep(1:optimal_k, each = length(SSB_range))
)

p_centers <- ggplot(centers_data, aes(x = SSB_WT_met_pect, y = Beta_coefficient, 
                                      color = as.factor(Cluster))) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = cluster_colors, name = "Cluster Center") +
  labs(title = "Fuzzy C-Means Cluster Centers",
       x = "SSB WT Metabolite Percentage",
       y = "Beta coefficient") +
  theme_classic()

print(p_centers)


#########################################Beverage preference & Organ ages by CKM############################################################
str(beverage_organ_age_data)

summary(beverage_20w_data$coffee_intake)
beverage_organ_age_data$coffee_tea_type <- ifelse(beverage_organ_age_data$coffee_intake_cat==2 | beverage_organ_age_data$tea_intake_cat==2,1,0)
beverage_organ_age_data$coffee_tea_type <- as.factor(beverage_organ_age_data$coffee_tea_type)
table(beverage_organ_age_data$coffee_tea_type)

beverage_organ_age_data$SSB_ASB_type <- ifelse(beverage_organ_age_data$SSB_intake_cat==2 | beverage_organ_age_data$ASB_intake_cat==2,1,0)
beverage_organ_age_data$SSB_ASB_type <- as.factor(beverage_organ_age_data$SSB_ASB_type)
table(beverage_organ_age_data$SSB_ASB_type)


beverage_organ_age_data$NJ_water_type <- ifelse(beverage_organ_age_data$NJ_intake_cat==2 | beverage_organ_age_data$plain_water_intake_cat==2,1,0)
beverage_organ_age_data$NJ_water_type <- as.factor(beverage_organ_age_data$NJ_water_type)
table(beverage_organ_age_data$NJ_water_type)



# 加载必要的包
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

# 设置数据为data.table
setDT(beverage_organ_age_data)

# 检查CKM stage分布
cat("Original CKM stage distribution:\n")
print(table(beverage_organ_age_data$ckm_stage, useNA = "always"))

# 定义器官年龄差异变量
organ_age_gaps <- c("Brain_age_gap", "Artery_age_gap", "Liver_age_gap", 
                    "Immune_age_gap", "Intestine_age_gap", "Lung_age_gap",
                    "Heart_age_gap", "Pancreas_age_gap", "Muscle_age_gap", 
                    "Adipose_age_gap", "Kidney_age_gap")

# 选择需要的数据
plot_data <- beverage_organ_age_data[, c("eid", "ckm_stage", "coffee_tea_type", 
                                         "SSB_ASB_type", "NJ_water_type", 
                                         organ_age_gaps), with = FALSE]

# 合并CKM stage 3和4
plot_data$ckm_stage_grouped <- ifelse(plot_data$ckm_stage >= 3, 3, plot_data$ckm_stage)

# 移除缺失的CKM stage
plot_data <- plot_data[!is.na(ckm_stage_grouped)]

cat("Complete cases with CKM stage:", nrow(plot_data), "\n")

# 检查饮料类型分布
cat("\nOriginal beverage type distributions:\n")
cat("Coffee/Tea type (0=Tea, 1=Coffee):\n")
print(table(plot_data$coffee_tea_type, useNA = "always"))
cat("SSB/ASB type (0=SSB, 1=ASB):\n")
print(table(plot_data$SSB_ASB_type, useNA = "always"))
cat("Natural Juice/Water type (0=Water, 1=Natural Juice):\n")
print(table(plot_data$NJ_water_type, useNA = "always"))

# 创建互相排斥的组
# 1. 咖啡偏好组：coffee_tea_type == 1 且 SSB_ASB_type == 0 且 NJ_water_type == 0
plot_data$coffee_only <- ifelse(
  plot_data$coffee_tea_type == "1" & 
    plot_data$SSB_ASB_type == "0" & 
    plot_data$NJ_water_type == "0", 1, 0)

# 2. ASB偏好组：SSB_ASB_type == 1 且 coffee_tea_type == 0 且 NJ_water_type == 0
plot_data$asb_only <- ifelse(
  plot_data$SSB_ASB_type == "1" & 
    plot_data$coffee_tea_type == "0" & 
    plot_data$NJ_water_type == "0", 1, 0)

# 3. 天然果汁偏好组：NJ_water_type == 1 且 coffee_tea_type == 0 且 SSB_ASB_type == 0
plot_data$nj_only <- ifelse(
  plot_data$NJ_water_type == "1" & 
    plot_data$coffee_tea_type == "0" & 
    plot_data$SSB_ASB_type == "0", 1, 0)

# 检查互相排斥组的样本量
cat("\nMutually exclusive group sizes:\n")
cat("Coffee only (coffee=1, SSB/ASB=0, NJ/Water=0):", sum(plot_data$coffee_only == 1, na.rm = TRUE), "\n")
cat("ASB only (ASB=1, Coffee/Tea=0, NJ/Water=0):", sum(plot_data$asb_only == 1, na.rm = TRUE), "\n")
cat("Natural Juice only (NJ=1, Coffee/Tea=0, SSB/ASB=0):", sum(plot_data$nj_only == 1, na.rm = TRUE), "\n")

# 转换为长格式
plot_data_long <- plot_data %>%
  pivot_longer(cols = all_of(organ_age_gaps), 
               names_to = "organ", 
               values_to = "age_gap") %>%
  filter(!is.na(age_gap))

# 清理器官名称
organ_names_clean <- c(
  "Brain_age_gap" = "Brain",
  "Artery_age_gap" = "Artery", 
  "Liver_age_gap" = "Liver",
  "Immune_age_gap" = "Immune",
  "Intestine_age_gap" = "Intestine",
  "Lung_age_gap" = "Lung",
  "Heart_age_gap" = "Heart",
  "Pancreas_age_gap" = "Pancreas",
  "Muscle_age_gap" = "Muscle",
  "Adipose_age_gap" = "Adipose",
  "Kidney_age_gap" = "Kidney"
)

plot_data_long$organ_clean <- organ_names_clean[plot_data_long$organ]

# 1. 总体趋势（所有人）
overall_summary <- plot_data_long %>%
  group_by(ckm_stage_grouped, organ_clean) %>%
  summarise(
    mean_age_gap = mean(age_gap, na.rm = TRUE),
    se_age_gap = sd(age_gap, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(group_type = "Overall")

# 2. 纯咖啡偏好组
coffee_only_summary <- plot_data_long %>%
  filter(coffee_only == 1) %>%
  group_by(ckm_stage_grouped, organ_clean) %>%
  summarise(
    mean_age_gap = mean(age_gap, na.rm = TRUE),
    se_age_gap = sd(age_gap, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(group_type = "Coffee preference only")

# 3. 纯ASB偏好组
asb_only_summary <- plot_data_long %>%
  filter(asb_only == 1) %>%
  group_by(ckm_stage_grouped, organ_clean) %>%
  summarise(
    mean_age_gap = mean(age_gap, na.rm = TRUE),
    se_age_gap = sd(age_gap, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(group_type = "ASB preference only")

# 4. 纯天然果汁偏好组
nj_only_summary <- plot_data_long %>%
  filter(nj_only == 1) %>%
  group_by(ckm_stage_grouped, organ_clean) %>%
  summarise(
    mean_age_gap = mean(age_gap, na.rm = TRUE),
    se_age_gap = sd(age_gap, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(group_type = "Natural Juice preference only")

# 合并所有数据
all_summary <- bind_rows(
  overall_summary,
  coffee_only_summary,
  asb_only_summary,
  nj_only_summary
)

# 移除样本量过小的组合（比如n<10）
all_summary <- all_summary %>%
  filter(n >= 10)

# 设置颜色和线型
color_palette <- c(
  "Overall" = "black",
  "Coffee preference only" = "#D97A07",
  "ASB preference only" = "#D90404", 
  "Natural Juice preference only" = "#7AB3BF"
)

line_types <- c(
  "Overall" = "solid",
  "Coffee preference only" = "dashed",
  "ASB preference only" = "dotted",
  "Natural Juice preference only" = "dotdash"
)

# 创建CKM stage标签
ckm_labels <- c("0" = "Stage 0", "1" = "Stage 1", "2" = "Stage 2", "3" = "Stage 3-4")

# ==================== 方案2：分别创建图形 ====================

# 图形1：除Artery外的所有器官（10个器官）
non_artery_data <- all_summary %>% filter(organ_clean != "Artery")

p_non_artery <- ggplot(non_artery_data, aes(x = ckm_stage_grouped, y = mean_age_gap, 
                                            color = group_type, linetype = group_type)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ organ_clean, scales = "free_y", ncol = 4) +
  labs(
    title = "Organ Age Gaps by CKM Stage and Exclusive Beverage Preferences",
    subtitle = "All organs except Artery - free Y-axis scales for optimal visualization",
    x = "CKM Stage",
    y = "Age Gap (years)", 
    color = "Group",
    linetype = "Group",
    caption = "Exclusive groups: Coffee only, ASB only, Natural Juice only (excluding other preferences)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "white"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3),
    labels = ckm_labels
  ) +
  scale_color_manual(values = color_palette) +
  scale_linetype_manual(values = line_types) +
  guides(
    color = guide_legend(ncol = 2),
    linetype = guide_legend(ncol = 2)
  )

# 图形2：只包含Artery，Y轴范围设置为-0.2到0.2
artery_data <- all_summary %>% filter(organ_clean == "Artery")

p_artery_only <- ggplot(artery_data, aes(x = ckm_stage_grouped, y = mean_age_gap, 
                                         color = group_type, linetype = group_type)) +
  geom_line(size = 1.0) +
  geom_point(size = 2.5) +
  labs(
    title = "Artery Age Gap by CKM Stage and Exclusive Beverage Preferences",
    subtitle = "Y-axis range fixed at -0.2 to 0.2 for detailed comparison",
    x = "CKM Stage",
    y = "Age Gap (years)", 
    color = "Group",
    linetype = "Group",
    caption = "Exclusive groups: Coffee only, ASB only, Natural Juice only"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, color = "gray50")
  ) +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3),
    labels = ckm_labels
  ) +
  scale_y_continuous(
    limits = c(-0.2, 0.2),
    breaks = seq(-0.2, 0.2, 0.1),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_color_manual(values = color_palette) +
  scale_linetype_manual(values = line_types) +
  guides(
    color = guide_legend(ncol = 2),
    linetype = guide_legend(ncol = 2)
  )

# 显示图形
cat("=== 图形1：除Artery外的所有器官 ===\n")
print(p_non_artery)

cat("\n=== 图形2：Artery单独图 (Y轴范围: -0.2 到 0.2) ===\n")
print(p_artery_only)

# 保存图形
ggsave("C:/Users/张杰/Desktop/organ_age_gaps_without_artery.pdf", plot = p_non_artery, 
       width = 16, height = 12, dpi = 300, bg = "white")

ggsave("C:/Users/张杰/Desktop/artery_age_gap_detailed.pdf", plot = p_artery_only, 
       width = 6, height = 7, dpi = 300, bg = "white")














