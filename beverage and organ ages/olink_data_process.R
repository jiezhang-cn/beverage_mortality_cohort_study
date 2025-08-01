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




























































