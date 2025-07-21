library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)

confunding_variables <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/mortality_indenpendetly_associated_variables_dataset_participant.csv")
names(confunding_variables)

names(confunding_variables) <- c("eid","age","sex","fed_up_feeling",
                                 "tdi","height_comparative_10","chesse_intake",
                                 "employment_status","type_accommodation",
                                 "maternal_smoking","illness_injury_bereavement_stress_2_years",
                                 "frequency_unenthusiasm","sleep_duration",
                                 "nap_frequency","weight_comparative_10",
                                 "pack_years_smoking","ever_smoked","frequency_tiredness",
                                 "smoking_status","own_rent_accommodation",
                                 "ethnic","ease_skin_tanning","heating_type",
                                 "household_income","education","live_with_partner",
                                 "physical_activity_group","leisure_social_activities")



#######################Variable coding#####################################
#All variable responses of “Prefer not to answer”, “Do not know”, and “Not applicable” were recoded to NA.
#Following multiple imputation, any variable values where the participant responded “Prefer not
#to answer” in the original dataset were recoded to NA. All nominal categorical variables were
#coded as unordered factors, with the reference level set as the most frequent response reported
#in the UK Biobank dataset. Ordered categorical variables were coded with the reference level
#set as the lowest response (e.g., “Never”, “Rarely”, or “None”). Dichotomous categorical
#variables were coded with the reference level set as “No.” All variables with “mark all that apply”
#response categories were converted into multiple dummy variables, with each unique response
#option used to create a yes/no dichotomous variable. 


confunding_variables$fed_up_feeling[confunding_variables$fed_up_feeling %in% c(-1,-3)] <- NA

str(confunding_variables$height_comparative_10)
confunding_variables$shorter_than10[confunding_variables$height_comparative_10 %in% c(-1,-3)] <- NA
confunding_variables$shorter_than10[confunding_variables$height_comparative_10 %in% c(2,3)] <- 0
confunding_variables$shorter_than10[confunding_variables$height_comparative_10 %in% c(1)] <- 1
table(confunding_variables$shorter_than10)


str(confunding_variables$chesse_intake)
confunding_variables$chesse_intake[confunding_variables$chesse_intake %in% c(-1,-3)] <- NA
table(confunding_variables$chesse_intake)


str(confunding_variables$employment_status)
confunding_variables$employed <- ifelse(
  confunding_variables$employment_status %in% c("-3", "-7"), 
  NA,
  ifelse(grepl("1", confunding_variables$employment_status), 1, 0)
)
table(confunding_variables$employed, useNA = "always")



str(confunding_variables$type_accommodation)
confunding_variables$living_flat_vs_house[confunding_variables$type_accommodation %in% c(-1,-3,4)] <- NA
confunding_variables$living_flat_vs_house[confunding_variables$type_accommodation %in% c(1)] <- 0
confunding_variables$living_flat_vs_house[confunding_variables$type_accommodation %in% c(2)] <- 1
table(confunding_variables$living_flat_vs_house)



str(confunding_variables$maternal_smoking)
confunding_variables$maternal_smoking[confunding_variables$maternal_smoking %in% c(-1,-3)] <- NA
table(confunding_variables$maternal_smoking)



str(confunding_variables$illness_injury_bereavement_stress_2_years)
confunding_variables$financial_diffculty[confunding_variables$illness_injury_bereavement_stress_2_years %in% c(-3)] <- NA
confunding_variables$financial_diffculty[confunding_variables$illness_injury_bereavement_stress_2_years %in% c(1,2,3,4,5,-7)] <- 0
confunding_variables$financial_diffculty[confunding_variables$illness_injury_bereavement_stress_2_years %in% c(6)] <- 1
table(confunding_variables$financial_diffculty)


str(confunding_variables$frequency_unenthusiasm)
confunding_variables$frequency_unenthusiasm[confunding_variables$frequency_unenthusiasm %in% c(-1,-3)] <- NA
table(confunding_variables$frequency_unenthusiasm)



str(confunding_variables$sleep_duration)
confunding_variables$sleep_duration_group[confunding_variables$sleep_duration %in% c(-1,-3)] <- NA
confunding_variables$sleep_duration_group[confunding_variables$sleep_duration %in% c(7,8,9)] <- 0
confunding_variables$sleep_duration_group[confunding_variables$sleep_duration<7] <- 1
confunding_variables$sleep_duration_group[confunding_variables$sleep_duration>9] <- 2
table(confunding_variables$sleep_duration_group)


table(confunding_variables$nap_frequency)
confunding_variables$nap_frequency[confunding_variables$nap_frequency %in% c(-3)] <- NA


str(confunding_variables$weight_comparative_10)
confunding_variables$plumper_than10[confunding_variables$weight_comparative_10 %in% c(-1,-3)] <- NA
confunding_variables$plumper_than10[confunding_variables$weight_comparative_10 %in% c(1,3)] <- 0
confunding_variables$plumper_than10[confunding_variables$weight_comparative_10 %in% c(2)] <- 1
table(confunding_variables$plumper_than10)


table(confunding_variables$ever_smoked)
str(confunding_variables$pack_years_smoking)
confunding_variables$pack_years_smoking[(confunding_variables$ever_smoked %in% 0) & is.na(confunding_variables$pack_years_smoking)] <- 0
summary(confunding_variables$pack_years_smoking)


str(confunding_variables$smoking_status)
confunding_variables$smoking_status[confunding_variables$smoking_status %in% c(-1,-3)] <- NA
table(confunding_variables$smoking_status)


str(confunding_variables$frequency_tiredness)
confunding_variables$frequency_tiredness[confunding_variables$frequency_tiredness %in% c(-1,-3)] <- NA
table(confunding_variables$frequency_tiredness)


str(confunding_variables$own_rent_accommodation)
confunding_variables$renting_from_council_vs_own[confunding_variables$own_rent_accommodation %in% c(-3)] <- NA
confunding_variables$renting_from_council_vs_own[confunding_variables$own_rent_accommodation %in% c(1)] <- 0
confunding_variables$renting_from_council_vs_own[confunding_variables$own_rent_accommodation %in% c(3)] <- 1
table(confunding_variables$renting_from_council_vs_own)


str(confunding_variables$ethnic)
confunding_variables$ethnic[confunding_variables$ethnic %in% c(-1,-3)] <- NA
confunding_variables$ethnic[confunding_variables$ethnic %in% c(1,1001,1002,1003)] <- 0
confunding_variables$ethnic[confunding_variables$ethnic %in% c(2,2001,2002,2003,2004)] <- 1
confunding_variables$ethnic[confunding_variables$ethnic %in% c(3,3001,3002,3003,3004,5)] <- 2
confunding_variables$ethnic[confunding_variables$ethnic %in% c(4,4001,4002,4003)] <- 3
confunding_variables$ethnic[confunding_variables$ethnic %in% c(5)] <- 4
table(confunding_variables$ethnic)



str(confunding_variables$ease_skin_tanning)
confunding_variables$ease_skin_tanning[confunding_variables$ease_skin_tanning %in% c(-1,-3)] <- NA



table(confunding_variables$heating_type)
confunding_variables$use_open_fire <- ifelse(
  confunding_variables$heating_type %in% c("-3", "-1"), 
  NA,
  ifelse(grepl("6", confunding_variables$heating_type), 1, 0)
)
table(confunding_variables$use_open_fire, useNA = "always")




str(confunding_variables$household_income)
confunding_variables$household_income[confunding_variables$household_income %in% c(-1,-3)] <- NA



table(confunding_variables$education)
confunding_variables$education_years <- sapply(confunding_variables$education, function(x) {
  if (x == "-3") return(NA)    # 不想回答
  if (x == "-7") return(7)     # 无资格
  
  quals <- unlist(strsplit(x, "\\|"))
  years <- ifelse(quals == "1", 20,    # 大学学位
                  ifelse(quals == "2", 13,    # A级/AS级
                         ifelse(quals == "3", 10,    # O级/GCSE
                                ifelse(quals == "4", 10,    # CSE
                                       ifelse(quals == "5", 19,    # NVQ/HND/HNC
                                              ifelse(quals == "6", 15, NA))))))   # 其他专业资格
  
  # 移除NA值再取最大值
  years <- years[!is.na(years)]
  
  if(length(years) == 0) {
    return(NA)  # 如果没有有效值，返回NA
  } else {
    return(max(years))  # 返回最高教育年限
  }
})

# 查看结果
table(confunding_variables$education_years, useNA = "always")



str(confunding_variables$live_with_partner)
confunding_variables$live_with_partner <- ifelse(
  confunding_variables$live_with_partner %in% c("-3"), 
  NA,
  ifelse(grepl("1", confunding_variables$live_with_partner), 1, 0)
)
table(confunding_variables$live_with_partner, useNA = "always")


str(confunding_variables$use_gym)
confunding_variables$use_gym <- ifelse(
  confunding_variables$leisure_social_activities %in% c("-3"), 
  NA,
  ifelse(grepl("1", confunding_variables$leisure_social_activities), 1, 0)
)
table(confunding_variables$use_gym, useNA = "always")



str(confunding_variables)
factor_vars <- c("chesse_intake", "frequency_unenthusiasm", "nap_frequency", 
                 "frequency_tiredness", "ethnic", "ease_skin_tanning", 
                 "household_income", "live_with_partner", "physical_activity_group", 
                 "shorter_than10", "employed", "living_flat_vs_house", 
                 "financial_diffculty", "sleep_duration_group", "plumper_than10", 
                 "renting_from_council_vs_own", "use_open_fire", 
                 "education_years", "use_gym","smoking_status")

confunding_variables[factor_vars] <- lapply(confunding_variables[factor_vars], as.factor)
str(confunding_variables[factor_vars])


library(mice)
setDT(confunding_variables)
factor_cols <- c("chesse_intake", "frequency_unenthusiasm", "nap_frequency", 
                 "frequency_tiredness", "ethnic", "ease_skin_tanning", 
                 "household_income", "live_with_partner", "physical_activity_group", 
                 "shorter_than10", "employed", "living_flat_vs_house", 
                 "financial_diffculty", "sleep_duration_group", "plumper_than10", 
                 "renting_from_council_vs_own", "use_open_fire", 
                 "education_years", "use_gym","smoking_status")
confunding_variables[, (factor_cols) := lapply(.SD, as.numeric), .SDcols = factor_cols]
set.seed(123)
imp <- mice(confunding_variables[,c("age","sex","ethnic","fed_up_feeling","tdi","pack_years_smoking",
                                    "chesse_intake", "frequency_unenthusiasm", "nap_frequency", 
                                    "frequency_tiredness","ease_skin_tanning", 
                                    "household_income", "live_with_partner", "physical_activity_group", 
                                    "shorter_than10", "employed", "living_flat_vs_house", 
                                    "financial_diffculty", "sleep_duration_group", "plumper_than10", 
                                    "renting_from_council_vs_own", "use_open_fire", 
                                    "education_years", "use_gym","smoking_status")], m = 1, maxit = 5, seed = 123)
summary(imp)
imputed_data <- as.data.table(complete(imp, 1))
imputed_data[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
str(imputed_data)
imputed_data$eid <- confunding_variables$eid
imputed_data <- imputed_data[,c(26,1:25)]



############################################confounding adjustment#####################################
names(ukb_beverage_data)
beverage_20w_data <- merge(ukb_beverage_data[,c(1,545:552)],imputed_data,all.x=T)
str(beverage_20w_data)

# 定义因变量（饮料摄入量变量）
dependent_vars <- c("SSB_intake", "ASB_intake", "NJ_intake", "lowfat_milk_intake", 
                    "fullfat_milk_intake", "plain_water_intake", "coffee_intake", "tea_intake")

# 定义自变量
independent_vars <- c("age", "sex", "ethnic", "fed_up_feeling", "tdi", "pack_years_smoking",
                      "chesse_intake", "frequency_unenthusiasm", "nap_frequency", 
                      "frequency_tiredness", "ease_skin_tanning", "household_income",
                      "live_with_partner", "physical_activity_group", "shorter_than10",
                      "employed", "living_flat_vs_house", "financial_diffculty",
                      "sleep_duration_group", "plumper_than10", "renting_from_council_vs_own",
                      "use_open_fire", "education_years", "use_gym", "smoking_status")

# 创建回归公式
formula_str <- paste("~", paste(independent_vars, collapse = " + "))

# 存储结果的列表
regression_results <- list()

# 对每个因变量进行线性回归
for(outcome in dependent_vars) {
  cat("\n=== 线性回归分析：", outcome, "===\n")
  
  # 构建完整公式
  full_formula <- as.formula(paste(outcome, formula_str))
  
  # 执行线性回归
  model <- lm(full_formula, data = beverage_20w_data)
  
  # 存储模型
  regression_results[[outcome]] <- model
  
  # 打印模型摘要
  print(summary(model))
  
  # 打印模型诊断信息
  cat("\n模型诊断信息：\n")
  cat("R-squared:", summary(model)$r.squared, "\n")
  cat("Adjusted R-squared:", summary(model)$adj.r.squared, "\n")
  cat("F-statistic p-value:", pf(summary(model)$fstatistic[1], 
                                 summary(model)$fstatistic[2], 
                                 summary(model)$fstatistic[3], 
                                 lower.tail = FALSE), "\n")
  
  cat("\n" , rep("=", 80), "\n")
}

# 创建结果汇总表并进行多重校正
library(broom)

# 提取所有模型的系数
all_coefficients <- data.frame()

for(outcome in dependent_vars) {
  model_tidy <- tidy(regression_results[[outcome]])
  model_tidy$outcome <- outcome
  all_coefficients <- rbind(all_coefficients, model_tidy)
}

# 移除截距项
all_coefficients_no_intercept <- all_coefficients[all_coefficients$term != "(Intercept)", ]

# 进行FDR和Bonferroni调整
all_coefficients_no_intercept$p.value.fdr <- p.adjust(all_coefficients_no_intercept$p.value, 
                                                      method = "fdr")
all_coefficients_no_intercept$p.value.bonferroni <- p.adjust(all_coefficients_no_intercept$p.value, 
                                                             method = "bonferroni")

# 显示原始显著性系数（p < 0.05）
cat("\n=== 原始显著性系数汇总 (p < 0.05) ===\n")
significant_coeffs_raw <- all_coefficients_no_intercept[all_coefficients_no_intercept$p.value < 0.05, ]
print(significant_coeffs_raw[order(significant_coeffs_raw$p.value), 
                             c("outcome", "term", "estimate", "std.error", "statistic", "p.value")])

# 显示FDR调整后的显著性系数（q < 0.05）
cat("\n=== FDR调整后显著性系数汇总 (q < 0.05) ===\n")
significant_coeffs_fdr <- all_coefficients_no_intercept[all_coefficients_no_intercept$p.value.fdr < 0.05, ]
print(significant_coeffs_fdr[order(significant_coeffs_fdr$p.value.fdr), 
                             c("outcome", "term", "estimate", "std.error", "statistic", "p.value", "p.value.fdr")])

# 显示Bonferroni调整后的显著性系数（p < 0.05）
cat("\n=== Bonferroni调整后显著性系数汇总 (p < 0.05) ===\n")
significant_coeffs_bonferroni <- all_coefficients_no_intercept[all_coefficients_no_intercept$p.value.bonferroni < 0.05, ]
print(significant_coeffs_bonferroni[order(significant_coeffs_bonferroni$p.value.bonferroni), 
                                    c("outcome", "term", "estimate", "std.error", "statistic", "p.value", "p.value.bonferroni")])

# 打印所有显著结果数量对比
cat("\n=== 显著结果数量对比 ===\n")
cat("原始显著结果数量 (p < 0.05):", nrow(significant_coeffs_raw), "\n")
cat("FDR调整后显著结果数量 (q < 0.05):", nrow(significant_coeffs_fdr), "\n")
cat("Bonferroni调整后显著结果数量 (p < 0.05):", nrow(significant_coeffs_bonferroni), "\n")

# 查看所有Bonferroni调整后的显著结果
print(significant_coeffs_bonferroni, n = nrow(significant_coeffs_bonferroni))

print(significant_coeffs_bonferroni,n=183)

fwrite(all_coefficients_no_intercept, "beverage_confounding_results.csv")


#######################correlations among beverages###############################
str(beverage_20w_data)

# 加载必要的包
library(pheatmap)
library(RColorBrewer)

# 选择饮品相关的数值变量
beverage_vars <- c("SSB_intake", "ASB_intake", "NJ_intake", 
                   "lowfat_milk_intake", "fullfat_milk_intake", 
                   "plain_water_intake", "coffee_intake", "tea_intake")

# 提取相关变量的数据
beverage_data <- beverage_20w_data[, beverage_vars]

# 计算相关系数矩阵
cor_matrix <- cor(beverage_data, use = "complete.obs")

# 计算p值矩阵
n <- nrow(beverage_data)
p_matrix <- matrix(NA, nrow = ncol(beverage_data), ncol = ncol(beverage_data))
colnames(p_matrix) <- rownames(p_matrix) <- colnames(beverage_data)

for(i in 1:ncol(beverage_data)) {
  for(j in 1:ncol(beverage_data)) {
    if(i != j) {
      # 计算相关系数的p值
      cor_test_result <- cor.test(beverage_data[,i], beverage_data[,j])
      p_matrix[i,j] <- cor_test_result$p.value
    } else {
      p_matrix[i,j] <- 0
    }
  }
}

# 创建显示矩阵：严格按照右上角显示相关系数，左下角显示显著性
display_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(display_matrix) <- rownames(cor_matrix)
colnames(display_matrix) <- colnames(cor_matrix)

for(i in 1:nrow(cor_matrix)) {
  for(j in 1:ncol(cor_matrix)) {
    if(i < j) {  # 右上角：显示相关系数
      display_matrix[i,j] <- as.character(round(cor_matrix[i,j], 2))
    } else if(i > j) {  # 左下角：显示显著性
      if(p_matrix[i,j] < 0.01) {
        display_matrix[i,j] <- "**"
      } else if(p_matrix[i,j] < 0.05) {
        display_matrix[i,j] <- "*"
      } else {
        display_matrix[i,j] <- ""
      }
    } else {  # 对角线
      display_matrix[i,j] <- "1"
    }
  }
}

# 自定义颜色函数：负相关#5BCCD9，正相关#F291A3
my_colors <- colorRampPalette(c("#5BCCD9", "white", "#F291A3"))(100)

# 绘制带聚类的热图，调整单元格大小和边框颜色
pheatmap(cor_matrix,
         display_numbers = display_matrix,
         number_color = "black",
         fontsize_number = 8,  # 减小字体以适应更小的单元格
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
         main = "饮品摄入量相关系数矩阵热图（层次聚类）\n(右上角:相关系数, 左下角:显著性)",
         legend = TRUE,
         legend_breaks = c(-1, -0.5, 0, 0.5, 1),
         legend_labels = c("-1", "-0.5", "0", "0.5", "1"),
         cellwidth = 30,   # 减小单元格宽度
         cellheight = 30,  # 减小单元格高度
         border_color = "#D3D3D3",  # 浅灰色边框
         cutree_rows = 3,
         cutree_cols = 3)

# 使用基于相关系数距离的聚类方法
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
         main = "饮品摄入量相关系数矩阵热图（基于相关性聚类）\n(右上角:相关系数, 左下角:显著性)",
         legend = TRUE,
         legend_breaks = c(-1, -0.5, 0, 0.5, 1),
         legend_labels = c("-1", "-0.5", "0", "0.5", "1"),
         cellwidth = 30,   # 减小单元格宽度
         cellheight = 30,  # 减小单元格高度
         border_color = "#D3D3D3",  # 浅灰色边框
         cutree_rows = 3,
         cutree_cols = 3)

# 打印聚类结果
cat("聚类结果:\n")
hc_rows <- hclust(cor_dist, method = "ward.D2")
clusters_3 <- cutree(hc_rows, k = 3)
cat("\n变量聚类分组 (k=3):\n")
for(i in 1:3) {
  cat(sprintf("组 %d: %s\n", i, paste(names(clusters_3)[clusters_3 == i], collapse = ", ")))
}

# 验证显示矩阵是否正确
cat("\n显示矩阵预览:\n")
print(display_matrix)

# 统计显著相关的数量
sig_001 <- sum(p_matrix < 0.01 & p_matrix > 0, na.rm = TRUE)
sig_005 <- sum(p_matrix < 0.05 & p_matrix >= 0.01, na.rm = TRUE)

cat(sprintf("\n显著相关统计:\n"))
cat(sprintf("p < 0.01 (**): %d 对\n", sig_001))
cat(sprintf("0.01 ≤ p < 0.05 (*): %d 对\n", sig_005))








########################################################
create_custom_heatmap <- function(data) {
  data_binned <- data %>%
    pivot_longer(cols = everything(),
                 names_to = "beverage_type",
                 values_to = "intake") %>%
    filter(!is.na(intake)) %>%
    mutate(
      # 创建分箱 - 使用更明确的条件
      intake_bin = case_when(
        intake == 0 ~ "0",
        intake > 0 & intake <= 1 ~ "0-1",
        intake > 1 & intake <= 2 ~ "1-2",
        intake > 2 & intake <= 3 ~ "2-3",
        intake > 3 & intake <= 4 ~ "3-4",
        intake > 4 & intake <= 5 ~ "4-5",
        intake > 5 & intake <= 8 ~ "5-8",
        intake > 8 ~ ">8",
        TRUE ~ "Other"
      ),
      # 设置因子水平，确保正确排序
      intake_bin = factor(intake_bin,
                          levels = c("0", "0-1", "1-2", "2-3", "3-4", "4-5", "5-8", ">8")),
      # 重新排序饮料类型
      beverage_type = factor(beverage_type,
                             levels = c("ASB_intake", "coffee_intake", "fullfat_milk_intake",
                                        "lowfat_milk_intake", "NJ_intake", "plain_water_intake",
                                        "SSB_intake", "tea_intake"))
    ) %>%
    # 计算频数和比例
    count(beverage_type, intake_bin, .drop = FALSE) %>%
    group_by(beverage_type) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  cat("数据行数:", nrow(data_binned), "\n")
  cat("比例范围:", range(data_binned$prop), "\n")
  cat("饮料类型:", levels(data_binned$beverage_type), "\n")
  cat("摄入量分箱:", levels(data_binned$intake_bin), "\n")
  
  p <- ggplot(data_binned, aes(x = intake_bin, y = beverage_type)) +
    geom_tile(aes(fill = prop), color = "white", size = 0.5) +
    scale_fill_gradient(
      low = "#E8F4FD",
      high = "#1547A0",
      name = "比例",
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, max(data_binned$prop))
    ) +
    # 修改这里：同时显示百分比和频数
    geom_text(aes(label = paste0(round(prop*100, 1), "%\n(n=", n, ")")),
              color = ifelse(data_binned$prop > 0.3, "white", "black"),
              size = 2.8,
              fontface = "bold",
              lineheight = 0.85) +
    labs(
      title = "饮料摄入量分布热力图",
      x = "摄入量区间 (单位/天)",
      y = "饮料类型"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
      axis.text.y = element_text(size = 11),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

# 尝试运行修复版本
plot_custom_fixed <- create_custom_heatmap(beverage_data)
plot_custom_fixed





