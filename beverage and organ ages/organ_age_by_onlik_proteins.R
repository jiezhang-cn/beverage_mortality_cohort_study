library(data.table)
library(dplyr)
library(purrr)


olink_data <- fread("/home/userb5/organ_age_proteomics/olink_data.txt")
olink_proteomics_covariates_data <- fread("/home/userb5/organ_age_proteomics/olink_proteomics_covariates_data")


########################################data clean###########################################
# 使用pivot_wider()函数转换olink格式(长格式到宽格式)
olink_data_ins_index0 <- olink_data[olink_data$ins_index==0,c(1,3,4)]

olink_data_reshaped <- olink_data_ins_index0 %>%
  pivot_wider(names_from = protein_id, values_from = result)

rm(olink_data_ins_index0)
rm(olink_data)

#remove number of proteins NAs>1000 individuals
olink_data_analyzed <- olink_data_reshaped[rowSums(is.na(olink_data_reshaped))<1000,]

#remove individuals withdraw consent from UK Biobank
olink_data_analyzed <- olink_data_analyzed[!(olink_data_analyzed$eid %in% withdraw62663$V1),]

#remove proteins with missing values in over 10% of samples
na_percentages <- olink_data_analyzed %>%
  select(2:2924) %>%
  map_dbl(~ mean(is.na(.)) * 100)
columns_to_remove <- names(which(na_percentages > 10))
olink_data_analyzed <- olink_data_analyzed %>%
  select(-all_of(columns_to_remove))

cat("移除的列数：", length(columns_to_remove), "\n")
cat("移除的列名：", columns_to_remove, "\n")
cat("剩余的列数：", ncol(olink_data_analyzed), "\n")


###############################impution & normal transformed##################################
#proteomics data was then imputed using the k-nearest neighbors algorithm, 
#with the default value of k = 211 (square root of the sample size 44841), 
#as implemented in the impute.knn
olink_data_matrix <- as.matrix(olink_data_analyzed[,2:2917])
olink_data_imputed <- impute.knn(olink_data_matrix ,k = 211, rowmax = 0.5, colmax = 0.8, maxp = 1500, rng.seed=123456)

#NPX were subsequently Z score normal transformed
olink_data_normalized <- as.data.frame(olink_data_imputed$data)
olink_data_normalized <- as_tibble(olink_data_normalized)

olink_data_normalized <- olink_data_normalized %>%
  mutate(across(everything(), ~ scale(.) %>% as.vector()))

head(olink_data_normalized)
summary(olink_data_normalized$`393`)

id_column <- olink_data_analyzed %>% select(1)
olink_data <- bind_cols(id_column, olink_data_normalized)
glimpse(olink_data)


###############################uniprot meaning annotation##################################
head(olink_data)
head(uniprot_meaning)

library(dplyr)
library(stringr)

# 创建从 coding 到 meaning 的映射
name_mapping <- uniprot_meaning %>%
  mutate(short_name = str_extract(meaning, "^[^;]+")) %>%
  select(coding, short_name) %>%
  as.data.frame() %>%
  setNames(c("coding", "short_name"))

name_mapping <- setNames(name_mapping$short_name, name_mapping$coding)

# 获取 olink_data 的当前列名
current_names <- colnames(olink_data)

# 创建新的列名
new_names <- sapply(current_names, function(x) {
  if (x == "eid") {
    return("eid")
  } else if (x %in% names(name_mapping)) {
    return(name_mapping[x])
  } else {
    return(x)
  }
})

# 重命名 olink_data 的列
olink_data <- olink_data %>%
  rename_with(~ new_names, everything())

# 查看结果
head(olink_data)
fwrite(olink_data, "/home/userb5/organ_age_proteomics/olink_data.gz", compress = "gzip")



############################Established Organ Age#################################################
#https://www.biorxiv.org/content/10.1101/2024.06.07.597771v1.full

library(readxl)
aging_model_weights <- read_excel("media-1.xlsx", sheet = "ST4_aging_model_weights")

aging_model_weights <- aging_model_weights %>%
  rename_with(~ str_to_upper(.), .cols = 3:2918)

# 创建一个函数来修改蛋白质名称
modify_protein_names <- function(aging_model_weights, olink_data) {
  # 创建一个名称映射
  name_mapping <- c(
    "NTPROBNP" = "NTproBNP",
    "C19ORF12" = "C19orf12",
    "HLA_DRA" = "HLA-DRA",
    "HLA_E" = "HLA-E",
    "C2ORF69" = "C2orf69",
    "C7ORF50" = "C7orf50",
    "C9ORF40" = "C9orf40",
    "ERVV_1" = "ERVV-1",
    "HLA_A" = "HLA-A"
  )
  
  # 修改列名
  for (old_name in names(name_mapping)) {
    if (old_name %in% names(aging_model_weights)) {
      aging_model_weights <- aging_model_weights %>%
        rename_with(~ str_replace(., old_name, name_mapping[old_name]))
    }
  }
  
  return(aging_model_weights)
}

# 使用函数修改 aging_model_weights
aging_model_weights <- modify_protein_names(aging_model_weights, olink_data)

# 将 aging_model_weights 中的 NA 替换为 0
aging_model_weights <- aging_model_weights %>%
  mutate(across(-organ, ~replace_na(., 0)))


############################Organ age estimation###########################################
###predicted age###
library(dplyr)
library(purrr)

# 将 aging_model_weights 中的 NA 替换为 0
aging_model_weights <- aging_model_weights %>%
  mutate(across(-organ, ~replace_na(., 0)))

# 函数：计算特定器官的年龄
calculate_organ_age <- function(organ_name, olink_data, aging_model_weights) {
  # 提取特定器官的权重和截距
  organ_weights <- aging_model_weights %>%
    filter(organ == organ_name) %>%
    select(-organ, -intercept)
  
  intercept <- aging_model_weights %>%
    filter(organ == organ_name) %>%
    pull(intercept)
  
  # 找出共同的蛋白质
  common_proteins <- intersect(names(olink_data)[-1], names(organ_weights))
  
  # 选择共同的蛋白质
  olink_data_ordered <- olink_data %>%
    select(eid, all_of(common_proteins))
  
  organ_weights_ordered <- organ_weights %>%
    select(all_of(common_proteins)) %>%
    as.matrix()
  
  # 计算器官年龄
  age_column <- intercept + as.matrix(olink_data_ordered[,-1]) %*% t(organ_weights_ordered)
  
  # 返回结果
  tibble(!!paste0(organ_name, "_age") := as.vector(age_column))
}

# 获取所有器官名称
organ_names <- aging_model_weights$organ

# 为每个器官计算年龄
organ_ages <- map(organ_names, ~calculate_organ_age(.x, olink_data, aging_model_weights)) %>%
  bind_cols()

# 将结果与原始数据合并
olink_data_organ_age <- bind_cols(olink_data[,1], organ_ages)

# 查看结果
summary(olink_data_organ_age)




###age gaps###
colnames(olink_proteomics_covariates_data)
olink_data_organ_age <- merge(olink_data_organ_age,olink_proteomics_covariates_data[,1:4],all.x=T)
colnames(olink_data_organ_age)[15:17] <- c("chronological_age","sex","bmi")

fwrite(olink_data_organ_age[1:16], "/home/userb5/organ_age_proteomics/olink_data_organ_age", compress = "gzip")


head(olink_data_organ_age)


# 定义预测年龄列表
predicted_ages <- c("Conventional_age", "Organismal_age", "Brain_age", "Artery_age", "Liver_age", 
                    "Immune_age", "Intestine_age", "Lung_age", "Heart_age", "Pancreas_age", 
                    "Muscle_age", "Adipose_age", "Kidney_age")

# 函数：计算 Age gap 并进行 z-score 标准化
calculate_age_gap <- function(data, predicted_age) {
  model <- lm(paste(predicted_age, "~ chronological_age"), data = data)
  residuals <- residuals(model)
  age_gap <- residuals
  age_gap_z <- scale(age_gap)
  return(age_gap_z)
}

# 计算所有 Age gaps
age_gaps <- map(predicted_ages, ~calculate_age_gap(olink_data_organ_age, .)) %>%
  setNames(paste0(predicted_ages, "_gap"))

# 将 Age gaps 添加到原始数据框
olink_data_organ_age <- bind_cols(olink_data_organ_age, as.data.frame(age_gaps))

# 查看结果
head(olink_data_organ_age)


fwrite(olink_data_organ_age, "/home/userb5/organ_age_proteomics/olink_data_organ_age")





# 定义要与 chronological_age 进行相关性分析的年龄列
age_columns <- c(
  'Conventional_age', 'Organismal_age', 'Brain_age', 'Artery_age', 
  'Liver_age', 'Immune_age', 'Intestine_age', 'Lung_age', 'Heart_age', 
  'Pancreas_age', 'Muscle_age', 'Adipose_age', 'Kidney_age'
)

# 计算相关系数
correlations <- sapply(olink_data_organ_age[age_columns], 
                       function(x) cor(x, olink_data_organ_age$chronological_age))

# 对相关系数进行降序排序
correlations_sorted <- sort(correlations, decreasing = TRUE)

# 打印结果
cat("与 chronological age 的相关系数：\n")
print(correlations_sorted)

#Conventional_age   Organismal_age        Brain_age       Artery_age        Liver_age       Immune_age 
#0.9125504        0.9008399        0.7144027        0.7129098        0.6171344        0.5516623 
#Intestine_age         Lung_age        Heart_age     Pancreas_age       Muscle_age      Adipose_age 
#0.4681996        0.3587069        0.3374259        0.3335419        0.3143684        0.2567268 
#Kidney_age 
#0.1576985 
