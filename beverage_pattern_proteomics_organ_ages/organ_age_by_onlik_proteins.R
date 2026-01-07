library(data.table)
library(dplyr)
library(purrr)


olink_data <- fread("/home/userb5/organ_age_proteomics/olink_data.txt")
olink_proteomics_covariates_data <- fread("/home/userb5/organ_age_proteomics/olink_proteomics_covariates_data")


########################################data clean###########################################
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

name_mapping <- uniprot_meaning %>%
  mutate(short_name = str_extract(meaning, "^[^;]+")) %>%
  select(coding, short_name) %>%
  as.data.frame() %>%
  setNames(c("coding", "short_name"))

name_mapping <- setNames(name_mapping$short_name, name_mapping$coding)

current_names <- colnames(olink_data)

new_names <- sapply(current_names, function(x) {
  if (x == "eid") {
    return("eid")
  } else if (x %in% names(name_mapping)) {
    return(name_mapping[x])
  } else {
    return(x)
  }
})

olink_data <- olink_data %>%
  rename_with(~ new_names, everything())

head(olink_data)
fwrite(olink_data, "/home/userb5/organ_age_proteomics/olink_data.gz", compress = "gzip")



############################Established Organ Age#################################################
#https://www.biorxiv.org/content/10.1101/2024.06.07.597771v1.full

library(readxl)
aging_model_weights <- read_excel("media-1.xlsx", sheet = "ST4_aging_model_weights")

aging_model_weights <- aging_model_weights %>%
  rename_with(~ str_to_upper(.), .cols = 3:2918)

modify_protein_names <- function(aging_model_weights, olink_data) {
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
  
  for (old_name in names(name_mapping)) {
    if (old_name %in% names(aging_model_weights)) {
      aging_model_weights <- aging_model_weights %>%
        rename_with(~ str_replace(., old_name, name_mapping[old_name]))
    }
  }
  
  return(aging_model_weights)
}

aging_model_weights <- modify_protein_names(aging_model_weights, olink_data)

aging_model_weights <- aging_model_weights %>%
  mutate(across(-organ, ~replace_na(., 0)))


############################Organ age estimation###########################################
###predicted age###
library(dplyr)
library(purrr)

aging_model_weights <- aging_model_weights %>%
  mutate(across(-organ, ~replace_na(., 0)))

calculate_organ_age <- function(organ_name, olink_data, aging_model_weights) {
  organ_weights <- aging_model_weights %>%
    filter(organ == organ_name) %>%
    select(-organ, -intercept)
  
  intercept <- aging_model_weights %>%
    filter(organ == organ_name) %>%
    pull(intercept)

  common_proteins <- intersect(names(olink_data)[-1], names(organ_weights))
  
  olink_data_ordered <- olink_data %>%
    select(eid, all_of(common_proteins))
  
  organ_weights_ordered <- organ_weights %>%
    select(all_of(common_proteins)) %>%
    as.matrix()
  
  age_column <- intercept + as.matrix(olink_data_ordered[,-1]) %*% t(organ_weights_ordered)
  
  tibble(!!paste0(organ_name, "_age") := as.vector(age_column))
}

organ_names <- aging_model_weights$organ

organ_ages <- map(organ_names, ~calculate_organ_age(.x, olink_data, aging_model_weights)) %>%
  bind_cols()

olink_data_organ_age <- bind_cols(olink_data[,1], organ_ages)

summary(olink_data_organ_age)




###age gaps###
colnames(olink_proteomics_covariates_data)
olink_data_organ_age <- merge(olink_data_organ_age,olink_proteomics_covariates_data[,1:4],all.x=T)
colnames(olink_data_organ_age)[15:17] <- c("chronological_age","sex","bmi")

fwrite(olink_data_organ_age[1:16], "/home/userb5/organ_age_proteomics/olink_data_organ_age", compress = "gzip")


head(olink_data_organ_age)

predicted_ages <- c("Conventional_age", "Organismal_age", "Brain_age", "Artery_age", "Liver_age", 
                    "Immune_age", "Intestine_age", "Lung_age", "Heart_age", "Pancreas_age", 
                    "Muscle_age", "Adipose_age", "Kidney_age")

calculate_age_gap <- function(data, predicted_age) {
  model <- lm(paste(predicted_age, "~ chronological_age"), data = data)
  residuals <- residuals(model)
  age_gap <- residuals
  age_gap_z <- scale(age_gap)
  return(age_gap_z)
}

age_gaps <- map(predicted_ages, ~calculate_age_gap(olink_data_organ_age, .)) %>%
  setNames(paste0(predicted_ages, "_gap"))

olink_data_organ_age <- bind_cols(olink_data_organ_age, as.data.frame(age_gaps))
head(olink_data_organ_age)

fwrite(olink_data_organ_age, "/home/userb5/organ_age_proteomics/olink_data_organ_age")



age_columns <- c(
  'Conventional_age', 'Organismal_age', 'Brain_age', 'Artery_age', 
  'Liver_age', 'Immune_age', 'Intestine_age', 'Lung_age', 'Heart_age', 
  'Pancreas_age', 'Muscle_age', 'Adipose_age', 'Kidney_age'
)

correlations <- sapply(olink_data_organ_age[age_columns], 
                       function(x) cor(x, olink_data_organ_age$chronological_age))

correlations_sorted <- sort(correlations, decreasing = TRUE)
print(correlations_sorted)

#Conventional_age   Organismal_age        Brain_age       Artery_age        Liver_age       Immune_age 
#0.9125504        0.9008399        0.7144027        0.7129098        0.6171344        0.5516623 
#Intestine_age         Lung_age        Heart_age     Pancreas_age       Muscle_age      Adipose_age 
#0.4681996        0.3587069        0.3374259        0.3335419        0.3143684        0.2567268 
#Kidney_age 
#0.1576985 
