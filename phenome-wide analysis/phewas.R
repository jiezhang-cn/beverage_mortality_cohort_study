library(data.table)
ukb_phewas_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_phewas_data.csv")
ukb_nmr_metabolomic_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_nmr_metabolomic_data.csv")
ukb_brain_mri_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_brain_mri_data.csv")

ukb_phewas_data <- merge(ukb_phewas_data,ukb_nmr_metabolomic_data,all.x = T)
ukb_phewas_data <- merge(ukb_phewas_data,ukb_brain_mri_data,all.x = T)
write.table(names(ukb_phewas_data), "column_names.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# 清理UK Biobank数据的函数
clean_ukb_data <- function(ukb_phewas_data) {
  
  # 获取所有列名
  all_columns <- colnames(ukb_phewas_data)
  cat("原始数据列数:", length(all_columns), "\n")
  
  # 1. 去除所有Instance.3变量
  columns_step1 <- all_columns[!grepl("Instance\\.3", all_columns)]
  cat("步骤1: 去除Instance.3变量后，列数:", length(columns_step1), "\n")
  
  # 2. 定义需要去除的NMR代谢组学Instance.1变量
  nmr_instance1_patterns <- c(
    "Total\\.Cholesterol.*Instance\\.1",
    "Total\\.Cholesterol\\.Minus\\.HDL\\.C.*Instance\\.1",
    "Remnant\\.Cholesterol.*Instance\\.1",
    "VLDL\\.Cholesterol.*Instance\\.1",
    "Clinical\\.LDL\\.Cholesterol.*Instance\\.1",
    "LDL\\.Cholesterol.*Instance\\.1",
    "HDL\\.Cholesterol.*Instance\\.1",
    "Total\\.Triglycerides.*Instance\\.1",
    "Triglycerides\\.in\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.LDL.*Instance\\.1",
    "Triglycerides\\.in\\.HDL.*Instance\\.1",
    "Total\\.Phospholipids\\.in\\.Lipoprotein\\.Particles.*Instance\\.1",
    "Phospholipids\\.in\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.LDL.*Instance\\.1",
    "Phospholipids\\.in\\.HDL.*Instance\\.1",
    "Total\\.Esterified\\.Cholesterol.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.LDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.HDL.*Instance\\.1",
    "Total\\.Free\\.Cholesterol.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.LDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.HDL.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Lipoprotein\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.VLDL.*Instance\\.1",
    "Total\\.Lipids\\.in\\.LDL.*Instance\\.1",
    "Total\\.Lipids\\.in\\.HDL.*Instance\\.1",
    "Total\\.Concentration\\.of\\.Lipoprotein\\.Particles.*Instance\\.1",
    "Concentration\\.of\\.VLDL\\.Particles.*Instance\\.1",
    "Concentration\\.of\\.LDL\\.Particles.*Instance\\.1",
    "Concentration\\.of\\.HDL\\.Particles.*Instance\\.1",
    "Average\\.Diameter\\.for\\.VLDL\\.Particles.*Instance\\.1",
    "Average\\.Diameter\\.for\\.LDL\\.Particles.*Instance\\.1",
    "Average\\.Diameter\\.for\\.HDL\\.Particles.*Instance\\.1",
    "Phosphoglycerides.*Instance\\.1",
    "Triglycerides\\.to\\.Phosphoglycerides\\.ratio.*Instance\\.1",
    "Total\\.Cholines.*Instance\\.1",
    "Phosphatidylcholines.*Instance\\.1",
    "Sphingomyelins.*Instance\\.1",
    "Apolipoprotein\\.B.*Instance\\.1",
    "Apolipoprotein\\.A1.*Instance\\.1",
    "Apolipoprotein\\.B\\.to\\.Apolipoprotein\\.A1\\.ratio.*Instance\\.1",
    "Total\\.Fatty\\.Acids.*Instance\\.1",
    "Degree\\.of\\.Unsaturation.*Instance\\.1",
    "Omega\\.3\\.Fatty\\.Acids.*Instance\\.1",
    "Omega\\.6\\.Fatty\\.Acids.*Instance\\.1",
    "Polyunsaturated\\.Fatty\\.Acids.*Instance\\.1",
    "Monounsaturated\\.Fatty\\.Acids.*Instance\\.1",
    "Saturated\\.Fatty\\.Acids.*Instance\\.1",
    "Linoleic\\.Acid.*Instance\\.1",
    "Docosahexaenoic\\.Acid.*Instance\\.1",
    "Omega\\.3\\.Fatty\\.Acids\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Omega\\.6\\.Fatty\\.Acids\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Polyunsaturated\\.Fatty\\.Acids\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Monounsaturated\\.Fatty\\.Acids\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Saturated\\.Fatty\\.Acids\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Linoleic\\.Acid\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Docosahexaenoic\\.Acid\\.to\\.Total\\.Fatty\\.Acids\\.percentage.*Instance\\.1",
    "Polyunsaturated\\.Fatty\\.Acids\\.to\\.Monounsaturated\\.Fatty\\.Acids\\.ratio.*Instance\\.1",
    "Omega\\.6\\.Fatty\\.Acids\\.to\\.Omega\\.3\\.Fatty\\.Acids\\.ratio.*Instance\\.1",
    "Alanine.*Instance\\.1",
    "Glutamine.*Instance\\.1",
    "Glycine.*Instance\\.1",
    "Histidine.*Instance\\.1",
    "Total\\.Concentration\\.of\\.Branched\\.Chain\\.Amino\\.Acids.*Instance\\.1",
    "Isoleucine.*Instance\\.1",
    "Leucine.*Instance\\.1",
    "Valine.*Instance\\.1",
    "Phenylalanine.*Instance\\.1",
    "Tyrosine.*Instance\\.1",
    "Glucose.*Instance\\.1",
    "Lactate.*Instance\\.1",
    "Pyruvate.*Instance\\.1",
    "Citrate.*Instance\\.1",
    "X3\\.Hydroxybutyrate.*Instance\\.1",
    "Acetate.*Instance\\.1",
    "Acetoacetate.*Instance\\.1",
    "Acetone.*Instance\\.1",
    "Creatinine.*Instance\\.1",
    "Albumin.*Instance\\.1",
    "Glycoprotein\\.Acetyls.*Instance\\.1",
    "Concentration\\.of\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL.*Instance\\.1",
    "Cholesterol\\.in\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.Chylomicrons\\.and\\.Extremely\\.Large\\.VLDL.*Instance\\.1",
    "Concentration\\.of\\.Very\\.Large\\.VLDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Very\\.Large\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.Very\\.Large\\.VLDL.*Instance\\.1",
    "Cholesterol\\.in\\.Very\\.Large\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Very\\.Large\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Very\\.Large\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.Very\\.Large\\.VLDL.*Instance\\.1",
    "Concentration\\.of\\.Large\\.VLDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Large\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.Large\\.VLDL.*Instance\\.1",
    "Cholesterol\\.in\\.Large\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Large\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Large\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.Large\\.VLDL.*Instance\\.1",
    "Concentration\\.of\\.Medium\\.VLDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Medium\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.Medium\\.VLDL.*Instance\\.1",
    "Cholesterol\\.in\\.Medium\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Medium\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Medium\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.Medium\\.VLDL.*Instance\\.1",
    "Concentration\\.of\\.Small\\.VLDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Small\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.Small\\.VLDL.*Instance\\.1",
    "Cholesterol\\.in\\.Small\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Small\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Small\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.Small\\.VLDL.*Instance\\.1",
    "Concentration\\.of\\.Very\\.Small\\.VLDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Very\\.Small\\.VLDL.*Instance\\.1",
    "Phospholipids\\.in\\.Very\\.Small\\.VLDL.*Instance\\.1",
    "Cholesterol\\.in\\.Very\\.Small\\.VLDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Very\\.Small\\.VLDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Very\\.Small\\.VLDL.*Instance\\.1",
    "Triglycerides\\.in\\.Very\\.Small\\.VLDL.*Instance\\.1",
    "Concentration\\.of\\.IDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.IDL.*Instance\\.1",
    "Phospholipids\\.in\\.IDL.*Instance\\.1",
    "Cholesterol\\.in\\.IDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.IDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.IDL.*Instance\\.1",
    "Triglycerides\\.in\\.IDL.*Instance\\.1",
    "Concentration\\.of\\.Large\\.LDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Large\\.LDL.*Instance\\.1",
    "Phospholipids\\.in\\.Large\\.LDL.*Instance\\.1",
    "Cholesterol\\.in\\.Large\\.LDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Large\\.LDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Large\\.LDL.*Instance\\.1",
    "Triglycerides\\.in\\.Large\\.LDL.*Instance\\.1",
    "Concentration\\.of\\.Medium\\.LDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Medium\\.LDL.*Instance\\.1",
    "Phospholipids\\.in\\.Medium\\.LDL.*Instance\\.1",
    "Cholesterol\\.in\\.Medium\\.LDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Medium\\.LDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Medium\\.LDL.*Instance\\.1",
    "Triglycerides\\.in\\.Medium\\.LDL.*Instance\\.1",
    "Concentration\\.of\\.Small\\.LDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Small\\.LDL.*Instance\\.1",
    "Phospholipids\\.in\\.Small\\.LDL.*Instance\\.1",
    "Cholesterol\\.in\\.Small\\.LDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Small\\.LDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Small\\.LDL.*Instance\\.1",
    "Triglycerides\\.in\\.Small\\.LDL.*Instance\\.1",
    "Concentration\\.of\\.Very\\.Large\\.HDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Very\\.Large\\.HDL.*Instance\\.1",
    "Phospholipids\\.in\\.Very\\.Large\\.HDL.*Instance\\.1",
    "Cholesterol\\.in\\.Very\\.Large\\.HDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Very\\.Large\\.HDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Very\\.Large\\.HDL.*Instance\\.1",
    "Triglycerides\\.in\\.Very\\.Large\\.HDL.*Instance\\.1",
    "Concentration\\.of\\.Large\\.HDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Large\\.HDL.*Instance\\.1",
    "Phospholipids\\.in\\.Large\\.HDL.*Instance\\.1",
    "Cholesterol\\.in\\.Large\\.HDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Large\\.HDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Large\\.HDL.*Instance\\.1",
    "Triglycerides\\.in\\.Large\\.HDL.*Instance\\.1",
    "Concentration\\.of\\.Medium\\.HDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Medium\\.HDL.*Instance\\.1",
    "Phospholipids\\.in\\.Medium\\.HDL.*Instance\\.1",
    "Cholesterol\\.in\\.Medium\\.HDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Medium\\.HDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Medium\\.HDL.*Instance\\.1",
    "Triglycerides\\.in\\.Medium\\.HDL.*Instance\\.1",
    "Concentration\\.of\\.Small\\.HDL\\.Particles.*Instance\\.1",
    "Total\\.Lipids\\.in\\.Small\\.HDL.*Instance\\.1",
    "Phospholipids\\.in\\.Small\\.HDL.*Instance\\.1",
    "Cholesterol\\.in\\.Small\\.HDL.*Instance\\.1",
    "Cholesteryl\\.Esters\\.in\\.Small\\.HDL.*Instance\\.1",
    "Free\\.Cholesterol\\.in\\.Small\\.HDL.*Instance\\.1",
    "Triglycerides\\.in\\.Small\\.HDL.*Instance\\.1",
    "Phospholipids\\.to\\.Total\\.Lipids.*percentage.*Instance\\.1",
    "Cholesterol\\.to\\.Total\\.Lipids.*percentage.*Instance\\.1",
    "Cholesteryl\\.Esters\\.to\\.Total\\.Lipids.*percentage.*Instance\\.1",
    "Free\\.Cholesterol\\.to\\.Total\\.Lipids.*percentage.*Instance\\.1",
    "Triglycerides\\.to\\.Total\\.Lipids.*percentage.*Instance\\.1"
  )
  
  # 查找匹配的列
  nmr_cols_to_remove <- c()
  for (pattern in nmr_instance1_patterns) {
    matching_cols <- columns_step1[grepl(pattern, columns_step1)]
    nmr_cols_to_remove <- c(nmr_cols_to_remove, matching_cols)
  }
  
  # 去除重复
  nmr_cols_to_remove <- unique(nmr_cols_to_remove)
  
  # 应用第2步过滤
  columns_step2 <- columns_step1[!columns_step1 %in% nmr_cols_to_remove]
  cat("步骤2: 去除NMR Instance.1变量后，列数:", length(columns_step2), "\n")
  cat("去除的NMR Instance.1变量数:", length(nmr_cols_to_remove), "\n")
  
  # 3. 定义Harvard-Oxford脑区域模式（需要去除的）
  harvard_oxford_patterns <- c(
    "Volume\\.of\\.grey\\.matter\\.in\\.Frontal\\.Pole",
    "Volume\\.of\\.grey\\.matter\\.in\\.Insular\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Superior\\.Frontal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Middle\\.Frontal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Inferior\\.Frontal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Precentral\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Temporal\\.Pole",
    "Volume\\.of\\.grey\\.matter\\.in\\.Superior\\.Temporal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Middle\\.Temporal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Inferior\\.Temporal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Postcentral\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Superior\\.Parietal\\.Lobule",
    "Volume\\.of\\.grey\\.matter\\.in\\.Supramarginal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Angular\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Lateral\\.Occipital\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Intracalcarine\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Frontal\\.Medial\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Juxtapositional\\.Lobule\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Subcallosal\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Paracingulate\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Cingulate\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Precuneous\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Cuneal\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Frontal\\.Orbital\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Parahippocampal\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Lingual\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Temporal\\.Fusiform\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Temporal\\.Occipital\\.Fusiform\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Occipital\\.Fusiform\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Frontal\\.Operculum\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Central\\.Opercular\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Parietal\\.Operculum\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Planum\\.Polare",
    "Volume\\.of\\.grey\\.matter\\.in\\.Heschl\\.s\\.Gyrus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Planum\\.Temporale",
    "Volume\\.of\\.grey\\.matter\\.in\\.Supracalcarine\\.Cortex",
    "Volume\\.of\\.grey\\.matter\\.in\\.Occipital\\.Pole",
    "Volume\\.of\\.grey\\.matter\\.in\\.Thalamus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Caudate",
    "Volume\\.of\\.grey\\.matter\\.in\\.Putamen",
    "Volume\\.of\\.grey\\.matter\\.in\\.Pallidum",
    "Volume\\.of\\.grey\\.matter\\.in\\.Hippocampus",
    "Volume\\.of\\.grey\\.matter\\.in\\.Amygdala",
    "Volume\\.of\\.grey\\.matter\\.in\\.Ventral\\.Striatum",
    "Volume\\.of\\.grey\\.matter\\.in\\.Brain\\.Stem",
    "Volume\\.of\\.grey\\.matter\\.in\\.I\\.IV\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.V\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.VI\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.Crus\\.I\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.Crus\\.II\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.VIIb\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.VIIIa\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.VIIIb\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.IX\\.Cerebellum",
    "Volume\\.of\\.grey\\.matter\\.in\\.X\\.Cerebellum"
  )
  
  # 查找Harvard-Oxford格式的脑变量
  ho_cols_to_remove <- c()
  for (pattern in harvard_oxford_patterns) {
    matching_cols <- columns_step2[grepl(pattern, columns_step2)]
    ho_cols_to_remove <- c(ho_cols_to_remove, matching_cols)
  }
  
  # 去除重复
  ho_cols_to_remove <- unique(ho_cols_to_remove)
  
  # 应用第3步过滤
  final_columns <- columns_step2[!columns_step2 %in% ho_cols_to_remove]
  cat("步骤3: 去除Harvard-Oxford脑变量后，列数:", length(final_columns), "\n")
  cat("去除的Harvard-Oxford脑变量数:", length(ho_cols_to_remove), "\n")
  
  # 创建清理后的数据框
  cleaned_data <- ukb_phewas_data[, final_columns, drop = FALSE]
  
  cat("\n总结:\n")
  cat("原始列数:", length(all_columns), "\n")
  cat("最终列数:", length(final_columns), "\n")
  cat("总共去除列数:", length(all_columns) - length(final_columns), "\n")
  
  return(cleaned_data)
}

# 使用函数
# 假设你的数据框名为 ukb_phewas_data
cleaned_ukb_phewas_data <- clean_ukb_data(ukb_phewas_data)

# 查看清理后的数据结构
str(cleaned_ukb_phewas_data)
dim(cleaned_ukb_phewas_data)

# 可选：保存清理后的数据
fwrite(cleaned_ukb_phewas_data, "C:/Users/张杰/Desktop/ckm_beverage_mortality/data/cleaned_ukb_phewas_data.csv")

####################################PheWAS########################################################
olink_data <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/olink_data.gz")
names(cleaned_ukb_phewas_data)[1] <- "eid"
phewas_data <- merge(beverage_20w_data,imputed_data_2[,c(1,7,9,10,11,13,14,15,16,17)],all.x=T)
phewas_data <- merge(phewas_data,cleaned_ukb_phewas_data,all.x=T)
phewas_data <- merge(phewas_data,olink_data,all.x=T)
write.table(names(phewas_data), "phewas_data_column_names.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

fwrite(phewas_data, "C:/Users/张杰/Desktop/ckm_beverage_mortality/data/beverage_phewas_data.csv")

# 加载必要的包
library(data.table)
library(dplyr)
library(broom)
library(ggplot2)

# 完整的PheWAS分析函数 - 添加Olink_Proteomics类别
perform_phewas_strict <- function(data, exposures, covariates) {
  
  # 确保数据是data.frame格式
  if (is.data.table(data)) {
    data <- as.data.frame(data)
  }
  
  # 严格按照用户定义的表型分类
  biomarker_categories <- list(
    "Biochemical" = c(
      "Albumin...Instance.0.participant...p23479_i0.",
      "Albumin...Instance.0.participant...p30600_i0.",
      "Apolipoprotein.B...Instance.0.participant...p23439_i0.",
      "Apolipoprotein.B...Instance.0.participant...p30640_i0.",
      "Creatinine...Instance.0.participant...p23478_i0.",
      "Creatinine...Instance.0.participant...p30700_i0.",
      "Glucose...Instance.0.participant...p23470_i0.",
      "Glucose...Instance.0.participant...p30740_i0.",
      "Microalbumin.in.urine...Instance.0",
      "Creatinine..enzymatic..in.urine...Instance.0",
      "Potassium.in.urine...Instance.0",
      "Sodium.in.urine...Instance.0",
      "Alkaline.phosphatase...Instance.0",
      "Alanine.aminotransferase...Instance.0",
      "Apolipoprotein.A...Instance.0",
      "Aspartate.aminotransferase...Instance.0",
      "Direct.bilirubin...Instance.0",
      "Urea...Instance.0",
      "Calcium...Instance.0.participant...p30680_i0.",
      "Calcium...Instance.0.participant...p100024_i0.",
      "Cholesterol...Instance.0",
      "Cystatin.C...Instance.0",
      "Gamma.glutamyltransferase...Instance.0",
      "Glycated.haemoglobin..HbA1c....Instance.0",
      "HDL.cholesterol...Instance.0",
      "IGF.1...Instance.0",
      "LDL.direct...Instance.0",
      "Lipoprotein.A...Instance.0",
      "Oestradiol...Instance.0",
      "Phosphate...Instance.0",
      "Rheumatoid.factor...Instance.0",
      "SHBG...Instance.0",
      "Total.bilirubin...Instance.0",
      "Testosterone...Instance.0",
      "Total.protein...Instance.0",
      "Triglycerides...Instance.0",
      "Urate...Instance.0",
      "Vitamin.D...Instance.0.participant...p30890_i0.",
      "Vitamin.D...Instance.0.participant...p100021_i0.",
      "C.reactive.protein...Instance.0"
    ),
    
    "Pulmonary" = c(
      "Forced.expiratory.volume.in.1.second..FEV1...Best.measure...Instance.0",
      "Forced.vital.capacity..FVC...Best.measure...Instance.0",
      "Forced.expiratory.volume.in.1.second..FEV1...predicted...Instance.0",
      "Forced.expiratory.volume.in.1.second..FEV1...predicted.percentage...Instance.0",
      "Forced.vital.capacity..FVC....Instance.0...Array.0",
      "Forced.vital.capacity..FVC....Instance.0...Array.1",
      "Forced.vital.capacity..FVC....Instance.0...Array.2",
      "Forced.expiratory.volume.in.1.second..FEV1....Instance.0...Array.0",
      "Forced.expiratory.volume.in.1.second..FEV1....Instance.0...Array.1",
      "Forced.expiratory.volume.in.1.second..FEV1....Instance.0...Array.2",
      "Forced.expiratory.volume.in.1.second..FEV1..Z.score",
      "Forced.vital.capacity..FVC..Z.score",
      "FEV1..FVC.ratio.Z.score"
    ),
    
    "Inflammatory" = c(
      "White.blood.cell..leukocyte..count...Instance.0",
      "Platelet.count...Instance.0",
      "Lymphocyte.count...Instance.0",
      "Monocyte.count...Instance.0",
      "Neutrophill.count...Instance.0",
      "Lymphocyte.percentage...Instance.0",
      "Monocyte.percentage...Instance.0",
      "Neutrophill.percentage...Instance.0"
    ),
    
    "Cardiac" = c(
      "LV.ejection.fraction...Instance.2.participant...p22420_i2.",
      "LV.ejection.fraction...Instance.2.participant...p24103_i2.",
      "LV.end.diastolic.volume...Instance.2.participant...p22421_i2.",
      "LV.end.diastolic.volume...Instance.2.participant...p24100_i2.",
      "LV.end.systolic.volume...Instance.2.participant...p22422_i2.",
      "LV.end.systolic.volume...Instance.2.participant...p24101_i2.",
      "LV.stroke.volume...Instance.2.participant...p22423_i2.",
      "LV.stroke.volume...Instance.2.participant...p24102_i2.",
      "Cardiac.output...Instance.2",
      "Cardiac.index...Instance.2",
      "Average.heart.rate...Instance.2",
      "Body.surface.area...Instance.2"
    ),
    
    "Cognitive" = c(
      "Fluid.intelligence.score...Instance.0.participant...p20016_i0.",
      "Fluid.intelligence.score...Instance.0.participant...p20191_i0.",
      "Maximum.digits.remembered.correctly...Instance.0.participant...p4282_i0.",
      "Maximum.digits.remembered.correctly...Instance.0.participant...p20240_i0.",
      "Prospective.memory.result...Instance.0",
      "Mean.time.to.correctly.identify.matches...Instance.0",
      "Number.of.incorrect.matches.in.round...Instance.0...Array.1.participant...p399_i0_a1.",
      "Number.of.incorrect.matches.in.round...Instance.0...Array.1.participant...p20132_i0_a1.",
      "Number.of.incorrect.matches.in.round...Instance.0...Array.2.participant...p399_i0_a2.",
      "Number.of.incorrect.matches.in.round...Instance.0...Array.2.participant...p20132_i0_a2.",
      "Number.of.incorrect.matches.in.round...Instance.0...Array.3",
      "Number.of.symbol.digit.matches.made.correctly...Instance.0",
      "Number.of.puzzles.correctly.solved...Instance.2",
      "Duration.to.complete.numeric.path..trail..1....Instance.2",
      "Duration.to.complete.alphanumeric.path..trail..2....Instance.2"
    ),
    
    "Body_Composition" = c(
      "Total.abdominal.adipose.tissue.index...Instance.2",
      "Weight.to.muscle.ratio...Instance.2",
      "Abdominal.fat.ratio...Instance.2",
      "Muscle.fat.infiltration...Instance.2",
      "X10P.Liver.PDFF..proton.density.fat.fraction....Instance.2",
      "Posterior.thigh.muscle.fat.infiltration..MFI...left....Instance.2",
      "Posterior.thigh.muscle.fat.infiltration..MFI...right....Instance.2",
      "VAT.error.indicator...Instance.2",
      "ASAT.error.indicator...Instance.2",
      "Anterior.thigh.error.indicator..left....Instance.2",
      "Posterior.thigh.error.indicator..left....Instance.2",
      "Anterior.thigh.error.indicator..right....Instance.2",
      "Posterior.thigh.error.indicator..right....Instance.2",
      "FR.liver.PDFF.mean.error.indicator...Instance.2",
      "Anterior.thigh.fat.free.muscle.volume..right....Instance.2",
      "Posterior.thigh.fat.free.muscle.volume..right....Instance.2",
      "Anterior.thigh.fat.free.muscle.volume..left....Instance.2",
      "Posterior.thigh.fat.free.muscle.volume..left....Instance.2",
      "Visceral.adipose.tissue.volume..VAT....Instance.2",
      "Abdominal.subcutaneous.adipose.tissue.volume..ASAT....Instance.2",
      "Total.thigh.fat.free.muscle.volume...Instance.2",
      "Total.trunk.fat.volume...Instance.2",
      "Total.adipose.tissue.volume...Instance.2",
      "Total.lean.tissue.volume...Instance.2",
      "L1.L4.area...Instance.2",
      "L1.L4.average.height...Instance.2",
      "L1.L4.average.width...Instance.2",
      "L1.L4.BMC..bone.mineral.content....Instance.2",
      "L1.L4.BMD..bone.mineral.density....Instance.2",
      "L1.L4.BMD..bone.mineral.density..T.score...Instance.2",
      "Femur.lower.neck.BMD..bone.mineral.density...right....Instance.2",
      "Femur.neck.BMD..bone.mineral.density...right....Instance.2",
      "Femur.neck.BMD..bone.mineral.density..T.score..right....Instance.2",
      "Femur.shaft.BMD..bone.mineral.density...right....Instance.2",
      "Femur.total.BMD..bone.mineral.density...right....Instance.2",
      "Femur.total.BMD..bone.mineral.density..T.score..right....Instance.2",
      "Femur.troch.BMD..bone.mineral.density...right....Instance.2",
      "Femur.troch.BMD..bone.mineral.density..T.score..right....Instance.2",
      "Femur.upper.neck.BMD..bone.mineral.density...right....Instance.2",
      "Femur.upper.neck.BMD..bone.mineral.density..T.score..right....Instance.2",
      "Femur.wards.BMD..bone.mineral.density...right....Instance.2",
      "Femur.wards.BMD..bone.mineral.density..T.score..right....Instance.2",
      "Arm.BMC..bone.mineral.content...left....Instance.2",
      "Arm.BMD..bone.mineral.density...left....Instance.2",
      "Arm.BMC..bone.mineral.content...right....Instance.2",
      "Arm.BMD..bone.mineral.density...right....Instance.2",
      "Arms.BMC..bone.mineral.content....Instance.2",
      "Arms.BMD..bone.mineral.density....Instance.2",
      "Head.BMD..bone.mineral.density....Instance.2",
      "Leg.BMD..bone.mineral.density...left....Instance.2",
      "Leg.BMC..bone.mineral.content...right....Instance.2",
      "Leg.BMD..bone.mineral.density...right....Instance.2",
      "Legs.BMC..bone.mineral.content....Instance.2",
      "Legs.BMD..bone.mineral.density....Instance.2",
      "Pelvis.BMD..bone.mineral.density....Instance.2",
      "Ribs.BMD..bone.mineral.density....Instance.2",
      "Spine.BMD..bone.mineral.density....Instance.2",
      "Total.BMC..bone.mineral.content....Instance.2",
      "Total.BMD..bone.mineral.density....Instance.2",
      "Total.BMD..bone.mineral.density...left....Instance.2",
      "Total.BMD..bone.mineral.density...right....Instance.2",
      "Total.BMD..bone.mineral.density..T.score...Instance.2",
      "Trunk.BMC..bone.mineral.content....Instance.2",
      "Trunk.BMD..bone.mineral.density....Instance.2",
      "Trunk.BMD..bone.mineral.density...left....Instance.2",
      "Trunk.BMD..bone.mineral.density...right....Instance.2",
      "Android.bone.mass...Instance.2",
      "Android.fat.mass...Instance.2",
      "Android.lean.mass...Instance.2",
      "Android.tissue.fat.percentage...Instance.2",
      "Android.total.mass...Instance.2",
      "Arm.fat.mass..left....Instance.2.participant...p23124_i2.",
      "Arm.fat.mass..left....Instance.2.participant...p23249_i2.",
      "Arm.lean.mass..left....Instance.2",
      "Arm.tissue.fat.percentage..left....Instance.2",
      "Arm.total.mass..left....Instance.2",
      "Arm.fat.mass..right....Instance.2.participant...p23120_i2.",
      "Arm.fat.mass..right....Instance.2.participant...p23253_i2.",
      "Arm.lean.mass..right....Instance.2",
      "Arm.tissue.fat.percentage..right....Instance.2",
      "Arm.total.mass..right....Instance.2",
      "Arms.fat.mass...Instance.2",
      "Arms.lean.mass...Instance.2",
      "Arms.tissue.fat.percentage...Instance.2",
      "Arms.total.mass...Instance.2",
      "Gynoid.bone.mass...Instance.2",
      "Gynoid.fat.mass...Instance.2",
      "Gynoid.lean.mass...Instance.2",
      "Gynoid.tissue.fat.percentage...Instance.2",
      "Gynoid.total.mass...Instance.2",
      "Leg.fat.mass..left....Instance.2.participant...p23116_i2.",
      "Leg.fat.mass..left....Instance.2.participant...p23266_i2.",
      "Leg.lean.mass..left....Instance.2",
      "Leg.tissue.fat.percentage..left....Instance.2",
      "Leg.total.mass..left....Instance.2",
      "Leg.fat.mass..right....Instance.2.participant...p23112_i2.",
      "Leg.fat.mass..right....Instance.2.participant...p23270_i2.",
      "Leg.lean.mass..right....Instance.2",
      "Leg.tissue.fat.percentage..right....Instance.2",
      "Leg.total.mass..right....Instance.2",
      "Legs.fat.mass...Instance.2",
      "Legs.lean.mass...Instance.2",
      "Legs.tissue.fat.percentage...Instance.2",
      "Legs.total.mass...Instance.2",
      "Total.fat.mass...Instance.2",
      "Total.fat.free.mass...Instance.2",
      "Total.lean.mass...Instance.2",
      "Total.tissue.fat.percentage...Instance.2",
      "Total.tissue.mass...Instance.2",
      "Total.mass...Instance.2",
      "Trunk.fat.mass...Instance.2.participant...p23128_i2.",
      "Trunk.fat.mass...Instance.2.participant...p23284_i2.",
      "Trunk.lean.mass...Instance.2",
      "Trunk.tissue.fat.percentage...Instance.2",
      "Trunk.total.mass...Instance.2",
      "VAT..visceral.adipose.tissue..mass...Instance.2",
      "VAT..visceral.adipose.tissue..volume...Instance.2",
      "Femur.shaft.BMD..bone.mineral.density...left....Instance.2",
      "Femur.total.BMD..bone.mineral.density...left....Instance.2",
      "Femur.upper.neck.BMD..bone.mineral.density...left....Instance.2",
      "Femur.total.BMD..bone.mineral.density..T.score..left....Instance.2",
      "Femur.troch.BMD..bone.mineral.density...left....Instance.2",
      "Femur.upper.neck.BMD..bone.mineral.density..T.score..left....Instance.2",
      "Femur.wards.BMD..bone.mineral.density...left....Instance.2",
      "Femur.troch.BMD..bone.mineral.density..T.score..left....Instance.2",
      "Femur.neck.BMD..bone.mineral.density...left....Instance.2",
      "Femur.neck.BMD..bone.mineral.density..T.score..left....Instance.2",
      "Femur.wards.BMD..bone.mineral.density..T.score..left....Instance.2",
      "Femur.lower.neck.BMD..bone.mineral.density...left....Instance.2",
      "Trunk.bone.area...Instance.2",
      "Head.bone.area...Instance.2",
      "Head.BMC..bone.mineral.content....Instance.2",
      "Pelvis.bone.area...Instance.2",
      "Pelvis.BMC..bone.mineral.content....Instance.2",
      "Ribs.bone.area...Instance.2",
      "Ribs.BMC..bone.mineral.content....Instance.2",
      "Spine.bone.area...Instance.2",
      "Spine.BMC..bone.mineral.content....Instance.2",
      "Arm.bone.area..left....Instance.2",
      "Arm.bone.area..right....Instance.2",
      "Leg.bone.area..left....Instance.2",
      "Leg.bone.area..right....Instance.2",
      "Arms.combined.bone.area...Instance.2",
      "Legs.combined.bone.area...Instance.2",
      "Leg.BMC..bone.mineral.content...left....Instance.2"
    ),
    
    "NMR_Metabolomics" = c(
      # 基础脂质指标
      "Average.Diameter.for.VLDL.Particles...Instance.0",
      "Average.Diameter.for.LDL.Particles...Instance.0", 
      "Average.Diameter.for.HDL.Particles...Instance.0",
      "Phosphoglycerides...Instance.0",
      "Total.Cholines...Instance.0",
      "Phosphatidylcholines...Instance.0",
      "Sphingomyelins...Instance.0",
      "Apolipoprotein.A1...Instance.0",
      "Apolipoprotein.B.to.Apolipoprotein.A1.ratio...Instance.0",
      
      # 脂肪酸
      "Total.Fatty.Acids...Instance.0",
      "Omega.3.Fatty.Acids...Instance.0",
      "Omega.6.Fatty.Acids...Instance.0",
      "Polyunsaturated.Fatty.Acids...Instance.0",
      "Monounsaturated.Fatty.Acids...Instance.0",
      "Saturated.Fatty.Acids...Instance.0",
      "Linoleic.Acid...Instance.0",
      "Docosahexaenoic.Acid...Instance.0",
      "Omega.3.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0",
      "Omega.6.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0",
      "Polyunsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0",
      "Monounsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0",
      "Saturated.Fatty.Acids.to.Total.Fatty.Acids.percentage...Instance.0",
      "Linoleic.Acid.to.Total.Fatty.Acids.percentage...Instance.0",
      "Docosahexaenoic.Acid.to.Total.Fatty.Acids.percentage...Instance.0",
      
      # 氨基酸
      "Alanine...Instance.0",
      "Glutamine...Instance.0", 
      "Glycine...Instance.0",
      "Histidine...Instance.0",
      "Total.Concentration.of.Branched.Chain.Amino.Acids..Leucine...Isoleucine...Valine....Instance.0",
      "Isoleucine...Instance.0",
      "Leucine...Instance.0",
      "Valine...Instance.0",
      "Phenylalanine...Instance.0",
      "Tyrosine...Instance.0",
      
      # 代谢物
      "Lactate...Instance.0",
      "Pyruvate...Instance.0",
      "Citrate...Instance.0",
      "X3.Hydroxybutyrate...Instance.0",
      "Acetate...Instance.0",
      "Acetoacetate...Instance.0",
      "Acetone...Instance.0",
      "Glycoprotein.Acetyls...Instance.0",
      
      # 脂蛋白颗粒
      "Concentration.of.Chylomicrons.and.Extremely.Large.VLDL.Particles...Instance.0",
      "Total.Lipids.in.Chylomicrons.and.Extremely.Large.VLDL...Instance.0",
      "Phospholipids.in.Chylomicrons.and.Extremely.Large.VLDL...Instance.0",
      "Cholesterol.in.Chylomicrons.and.Extremely.Large.VLDL...Instance.0",
      "Cholesteryl.Esters.in.Chylomicrons.and.Extremely.Large.VLDL...Instance.0",
      "Free.Cholesterol.in.Chylomicrons.and.Extremely.Large.VLDL...Instance.0",
      "Triglycerides.in.Chylomicrons.and.Extremely.Large.VLDL...Instance.0",
      
      # 超大VLDL
      "Concentration.of.Very.Large.VLDL.Particles...Instance.0",
      "Total.Lipids.in.Very.Large.VLDL...Instance.0",
      "Phospholipids.in.Very.Large.VLDL...Instance.0",
      "Cholesterol.in.Very.Large.VLDL...Instance.0",
      "Cholesteryl.Esters.in.Very.Large.VLDL...Instance.0",
      "Free.Cholesterol.in.Very.Large.VLDL...Instance.0",
      "Triglycerides.in.Very.Large.VLDL...Instance.0",
      
      # 大VLDL  
      "Concentration.of.Large.VLDL.Particles...Instance.0",
      "Total.Lipids.in.Large.VLDL...Instance.0",
      "Phospholipids.in.Large.VLDL...Instance.0",
      "Cholesterol.in.Large.VLDL...Instance.0",
      "Cholesteryl.Esters.in.Large.VLDL...Instance.0",
      "Free.Cholesterol.in.Large.VLDL...Instance.0",
      "Triglycerides.in.Large.VLDL...Instance.0",
      
      # 中等VLDL
      "Concentration.of.Medium.VLDL.Particles...Instance.0",
      "Total.Lipids.in.Medium.VLDL...Instance.0",
      "Phospholipids.in.Medium.VLDL...Instance.0",
      "Cholesterol.in.Medium.VLDL...Instance.0",
      "Cholesteryl.Esters.in.Medium.VLDL...Instance.0",
      "Free.Cholesterol.in.Medium.VLDL...Instance.0",
      "Triglycerides.in.Medium.VLDL...Instance.0",
      
      # 小VLDL
      "Concentration.of.Small.VLDL.Particles...Instance.0",
      "Total.Lipids.in.Small.VLDL...Instance.0",
      "Phospholipids.in.Small.VLDL...Instance.0",
      "Cholesterol.in.Small.VLDL...Instance.0",
      "Cholesteryl.Esters.in.Small.VLDL...Instance.0",
      "Free.Cholesterol.in.Small.VLDL...Instance.0",
      "Triglycerides.in.Small.VLDL...Instance.0",
      
      # 极小VLDL
      "Concentration.of.Very.Small.VLDL.Particles...Instance.0",
      "Total.Lipids.in.Very.Small.VLDL...Instance.0",
      "Phospholipids.in.Very.Small.VLDL...Instance.0",
      "Cholesterol.in.Very.Small.VLDL...Instance.0",
      "Cholesteryl.Esters.in.Very.Small.VLDL...Instance.0",
      "Free.Cholesterol.in.Very.Small.VLDL...Instance.0",
      "Triglycerides.in.Very.Small.VLDL...Instance.0",
      
      # IDL
      "Concentration.of.IDL.Particles...Instance.0",
      "Total.Lipids.in.IDL...Instance.0",
      "Phospholipids.in.IDL...Instance.0",
      "Cholesterol.in.IDL...Instance.0",
      "Cholesteryl.Esters.in.IDL...Instance.0",
      "Free.Cholesterol.in.IDL...Instance.0",
      "Triglycerides.in.IDL...Instance.0",
      
      # 大LDL
      "Concentration.of.Large.LDL.Particles...Instance.0",
      "Total.Lipids.in.Large.LDL...Instance.0",
      "Phospholipids.in.Large.LDL...Instance.0",
      "Cholesterol.in.Large.LDL...Instance.0",
      "Cholesteryl.Esters.in.Large.LDL...Instance.0",
      "Free.Cholesterol.in.Large.LDL...Instance.0",
      "Triglycerides.in.Large.LDL...Instance.0",
      
      # 中等LDL
      "Concentration.of.Medium.LDL.Particles...Instance.0",
      "Total.Lipids.in.Medium.LDL...Instance.0",
      "Phospholipids.in.Medium.LDL...Instance.0",
      "Cholesterol.in.Medium.LDL...Instance.0",
      "Cholesteryl.Esters.in.Medium.LDL...Instance.0",
      "Free.Cholesterol.in.Medium.LDL...Instance.0",
      "Triglycerides.in.Medium.LDL...Instance.0",
      
      # 小LDL
      "Concentration.of.Small.LDL.Particles...Instance.0",
      "Total.Lipids.in.Small.LDL...Instance.0",
      "Phospholipids.in.Small.LDL...Instance.0",
      "Cholesterol.in.Small.LDL...Instance.0",
      "Cholesteryl.Esters.in.Small.LDL...Instance.0",
      "Free.Cholesterol.in.Small.LDL...Instance.0",
      "Triglycerides.in.Small.LDL...Instance.0",
      
      # 超大HDL
      "Concentration.of.Very.Large.HDL.Particles...Instance.0",
      "Total.Lipids.in.Very.Large.HDL...Instance.0",
      "Phospholipids.in.Very.Large.HDL...Instance.0",
      "Cholesterol.in.Very.Large.HDL...Instance.0",
      "Cholesteryl.Esters.in.Very.Large.HDL...Instance.0",
      "Free.Cholesterol.in.Very.Large.HDL...Instance.0",
      "Triglycerides.in.Very.Large.HDL...Instance.0",
      
      # 大HDL
      "Concentration.of.Large.HDL.Particles...Instance.0",
      "Total.Lipids.in.Large.HDL...Instance.0",
      "Phospholipids.in.Large.HDL...Instance.0",
      "Cholesterol.in.Large.HDL...Instance.0",
      "Cholesteryl.Esters.in.Large.HDL...Instance.0",
      "Free.Cholesterol.in.Large.HDL...Instance.0",
      "Triglycerides.in.Large.HDL...Instance.0",
      
      # 中等HDL
      "Concentration.of.Medium.HDL.Particles...Instance.0",
      "Total.Lipids.in.Medium.HDL...Instance.0",
      "Phospholipids.in.Medium.HDL...Instance.0",
      "Cholesterol.in.Medium.HDL...Instance.0",
      "Cholesteryl.Esters.in.Medium.HDL...Instance.0",
      "Free.Cholesterol.in.Medium.HDL...Instance.0",
      "Triglycerides.in.Medium.HDL...Instance.0",
      
      # 小HDL
      "Concentration.of.Small.HDL.Particles...Instance.0",
      "Total.Lipids.in.Small.HDL...Instance.0",
      "Phospholipids.in.Small.HDL...Instance.0",
      "Cholesterol.in.Small.HDL...Instance.0",
      "Cholesteryl.Esters.in.Small.HDL...Instance.0",
      "Free.Cholesterol.in.Small.HDL...Instance.0",
      "Triglycerides.in.Small.HDL...Instance.0"
    ),
    
    "Brain_Structure" = c(
      # 皮层下结构体积
      "Volume.of.thalamus..left....Instance.2",
      "Volume.of.thalamus..right....Instance.2",
      "Volume.of.caudate..left....Instance.2",
      "Volume.of.caudate..right....Instance.2",
      "Volume.of.putamen..left....Instance.2",
      "Volume.of.putamen..right....Instance.2",
      "Volume.of.pallidum..left....Instance.2",
      "Volume.of.pallidum..right....Instance.2",
      "Volume.of.hippocampus..left....Instance.2",
      "Volume.of.hippocampus..right....Instance.2",
      "Volume.of.amygdala..left....Instance.2",
      "Volume.of.amygdala..right....Instance.2",
      "Volume.of.accumbens..left....Instance.2",
      "Volume.of.accumbens..right....Instance.2",
      
      # 左半球皮层表面积
      "Area.of.TotalSurface..left.hemisphere....Instance.2.participant...p26721_i2.",
      "Area.of.TotalSurface..left.hemisphere....Instance.2.participant...p26923_i2.",
      "Area.of.bankssts..left.hemisphere....Instance.2.participant...p26722_i2.",
      "Area.of.bankssts..left.hemisphere....Instance.2.participant...p26924_i2.",
      "Area.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p26723_i2.",
      "Area.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p26925_i2.",
      "Area.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p27143_i2.",
      "Area.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p26724_i2.",
      "Area.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p26926_i2.",
      "Area.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p27144_i2.",
      "Area.of.cuneus..left.hemisphere....Instance.2.participant...p26725_i2.",
      "Area.of.cuneus..left.hemisphere....Instance.2.participant...p26927_i2.",
      "Area.of.cuneus..left.hemisphere....Instance.2.participant...p27145_i2.",
      "Area.of.entorhinal..left.hemisphere....Instance.2.participant...p26726_i2.",
      "Area.of.entorhinal..left.hemisphere....Instance.2.participant...p26928_i2.",
      "Area.of.entorhinal..left.hemisphere....Instance.2.participant...p27072_i2.",
      "Area.of.entorhinal..left.hemisphere....Instance.2.participant...p27146_i2.",
      "Area.of.fusiform..left.hemisphere....Instance.2.participant...p26727_i2.",
      "Area.of.fusiform..left.hemisphere....Instance.2.participant...p26929_i2.",
      "Area.of.fusiform..left.hemisphere....Instance.2.participant...p27147_i2.",
      "Area.of.inferiorparietal..left.hemisphere....Instance.2.participant...p26728_i2.",
      "Area.of.inferiorparietal..left.hemisphere....Instance.2.participant...p26930_i2.",
      "Area.of.inferiorparietal..left.hemisphere....Instance.2.participant...p27148_i2.",
      "Area.of.inferiortemporal..left.hemisphere....Instance.2.participant...p26729_i2.",
      "Area.of.inferiortemporal..left.hemisphere....Instance.2.participant...p26931_i2.",
      "Area.of.inferiortemporal..left.hemisphere....Instance.2.participant...p27149_i2.",
      "Area.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p26730_i2.",
      "Area.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p26932_i2.",
      "Area.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p27150_i2.",
      "Area.of.lateraloccipital..left.hemisphere....Instance.2.participant...p26731_i2.",
      "Area.of.lateraloccipital..left.hemisphere....Instance.2.participant...p26933_i2.",
      "Area.of.lateraloccipital..left.hemisphere....Instance.2.participant...p27151_i2.",
      "Area.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p26732_i2.",
      "Area.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p26934_i2.",
      "Area.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p27152_i2.",
      "Area.of.lingual..left.hemisphere....Instance.2.participant...p26733_i2.",
      "Area.of.lingual..left.hemisphere....Instance.2.participant...p26935_i2.",
      "Area.of.lingual..left.hemisphere....Instance.2.participant...p27153_i2.",
      "Area.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p26734_i2.",
      "Area.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p26936_i2.",
      "Area.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p27154_i2.",
      "Area.of.middletemporal..left.hemisphere....Instance.2.participant...p26735_i2.",
      "Area.of.middletemporal..left.hemisphere....Instance.2.participant...p26937_i2.",
      "Area.of.middletemporal..left.hemisphere....Instance.2.participant...p27155_i2.",
      "Area.of.parahippocampal..left.hemisphere....Instance.2.participant...p26736_i2.",
      "Area.of.parahippocampal..left.hemisphere....Instance.2.participant...p26938_i2.",
      "Area.of.parahippocampal..left.hemisphere....Instance.2.participant...p27156_i2.",
      "Area.of.paracentral..left.hemisphere....Instance.2.participant...p26737_i2.",
      "Area.of.paracentral..left.hemisphere....Instance.2.participant...p26939_i2.",
      "Area.of.paracentral..left.hemisphere....Instance.2.participant...p27157_i2.",
      "Area.of.parsopercularis..left.hemisphere....Instance.2.participant...p26738_i2.",
      "Area.of.parsopercularis..left.hemisphere....Instance.2.participant...p26940_i2.",
      "Area.of.parsopercularis..left.hemisphere....Instance.2.participant...p27158_i2.",
      "Area.of.parsorbitalis..left.hemisphere....Instance.2.participant...p26739_i2.",
      "Area.of.parsorbitalis..left.hemisphere....Instance.2.participant...p26941_i2.",
      "Area.of.parsorbitalis..left.hemisphere....Instance.2.participant...p27159_i2.",
      "Area.of.parstriangularis..left.hemisphere....Instance.2.participant...p26740_i2.",
      "Area.of.parstriangularis..left.hemisphere....Instance.2.participant...p26942_i2.",
      "Area.of.parstriangularis..left.hemisphere....Instance.2.participant...p27160_i2.",
      "Area.of.pericalcarine..left.hemisphere....Instance.2.participant...p26741_i2.",
      "Area.of.pericalcarine..left.hemisphere....Instance.2.participant...p26943_i2.",
      "Area.of.pericalcarine..left.hemisphere....Instance.2.participant...p27161_i2.",
      "Area.of.postcentral..left.hemisphere....Instance.2.participant...p26742_i2.",
      "Area.of.postcentral..left.hemisphere....Instance.2.participant...p26944_i2.",
      "Area.of.postcentral..left.hemisphere....Instance.2.participant...p27162_i2.",
      "Area.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p26743_i2.",
      "Area.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p26945_i2.",
      "Area.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p27163_i2.",
      "Area.of.precentral..left.hemisphere....Instance.2.participant...p26744_i2.",
      "Area.of.precentral..left.hemisphere....Instance.2.participant...p26946_i2.",
      "Area.of.precentral..left.hemisphere....Instance.2.participant...p27164_i2.",
      "Area.of.precuneus..left.hemisphere....Instance.2.participant...p26745_i2.",
      "Area.of.precuneus..left.hemisphere....Instance.2.participant...p26947_i2.",
      "Area.of.precuneus..left.hemisphere....Instance.2.participant...p27165_i2.",
      "Area.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p26746_i2.",
      "Area.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p26948_i2.",
      "Area.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p27166_i2.",
      "Area.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p26747_i2.",
      "Area.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p26949_i2.",
      "Area.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p27167_i2.",
      "Area.of.superiorfrontal..left.hemisphere....Instance.2.participant...p26748_i2.",
      "Area.of.superiorfrontal..left.hemisphere....Instance.2.participant...p26950_i2.",
      "Area.of.superiorfrontal..left.hemisphere....Instance.2.participant...p27168_i2.",
      "Area.of.superiorparietal..left.hemisphere....Instance.2.participant...p26749_i2.",
      "Area.of.superiorparietal..left.hemisphere....Instance.2.participant...p26951_i2.",
      "Area.of.superiorparietal..left.hemisphere....Instance.2.participant...p27169_i2.",
      "Area.of.superiortemporal..left.hemisphere....Instance.2.participant...p26750_i2.",
      "Area.of.superiortemporal..left.hemisphere....Instance.2.participant...p26952_i2.",
      "Area.of.superiortemporal..left.hemisphere....Instance.2.participant...p27170_i2.",
      "Area.of.supramarginal..left.hemisphere....Instance.2.participant...p26751_i2.",
      "Area.of.supramarginal..left.hemisphere....Instance.2.participant...p26953_i2.",
      "Area.of.supramarginal..left.hemisphere....Instance.2.participant...p27171_i2.",
      "Area.of.frontalpole..left.hemisphere....Instance.2.participant...p26752_i2.",
      "Area.of.frontalpole..left.hemisphere....Instance.2.participant...p26954_i2.",
      "Area.of.transversetemporal..left.hemisphere....Instance.2.participant...p26753_i2.",
      "Area.of.transversetemporal..left.hemisphere....Instance.2.participant...p26955_i2.",
      "Area.of.transversetemporal..left.hemisphere....Instance.2.participant...p27172_i2.",
      "Area.of.insula..left.hemisphere....Instance.2.participant...p26754_i2.",
      "Area.of.insula..left.hemisphere....Instance.2.participant...p27173_i2.",
      
      # 左半球皮层厚度
      "Mean.thickness.of.GlobalMeanMean.thickness..left.hemisphere....Instance.2",
      "Mean.thickness.of.bankssts..left.hemisphere....Instance.2",
      "Mean.thickness.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p26757_i2.",
      "Mean.thickness.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p27174_i2.",
      "Mean.thickness.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p26758_i2.",
      "Mean.thickness.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p27175_i2.",
      "Mean.thickness.of.cuneus..left.hemisphere....Instance.2.participant...p26759_i2.",
      "Mean.thickness.of.cuneus..left.hemisphere....Instance.2.participant...p27176_i2.",
      "Mean.thickness.of.entorhinal..left.hemisphere....Instance.2.participant...p26760_i2.",
      "Mean.thickness.of.entorhinal..left.hemisphere....Instance.2.participant...p27086_i2.",
      "Mean.thickness.of.entorhinal..left.hemisphere....Instance.2.participant...p27177_i2.",
      "Mean.thickness.of.fusiform..left.hemisphere....Instance.2.participant...p26761_i2.",
      "Mean.thickness.of.fusiform..left.hemisphere....Instance.2.participant...p27178_i2.",
      "Mean.thickness.of.inferiorparietal..left.hemisphere....Instance.2.participant...p26762_i2.",
      "Mean.thickness.of.inferiorparietal..left.hemisphere....Instance.2.participant...p27179_i2.",
      "Mean.thickness.of.inferiortemporal..left.hemisphere....Instance.2.participant...p26763_i2.",
      "Mean.thickness.of.inferiortemporal..left.hemisphere....Instance.2.participant...p27180_i2.",
      "Mean.thickness.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p26764_i2.",
      "Mean.thickness.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p27181_i2.",
      "Mean.thickness.of.lateraloccipital..left.hemisphere....Instance.2.participant...p26765_i2.",
      "Mean.thickness.of.lateraloccipital..left.hemisphere....Instance.2.participant...p27182_i2.",
      "Mean.thickness.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p26766_i2.",
      "Mean.thickness.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p27183_i2.",
      "Mean.thickness.of.lingual..left.hemisphere....Instance.2.participant...p26767_i2.",
      "Mean.thickness.of.lingual..left.hemisphere....Instance.2.participant...p27184_i2.",
      "Mean.thickness.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p26768_i2.",
      "Mean.thickness.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p27185_i2.",
      "Mean.thickness.of.middletemporal..left.hemisphere....Instance.2.participant...p26769_i2.",
      "Mean.thickness.of.middletemporal..left.hemisphere....Instance.2.participant...p27186_i2.",
      "Mean.thickness.of.parahippocampal..left.hemisphere....Instance.2.participant...p26770_i2.",
      "Mean.thickness.of.parahippocampal..left.hemisphere....Instance.2.participant...p27187_i2.",
      "Mean.thickness.of.paracentral..left.hemisphere....Instance.2.participant...p26771_i2.",
      "Mean.thickness.of.paracentral..left.hemisphere....Instance.2.participant...p27188_i2.",
      "Mean.thickness.of.parsopercularis..left.hemisphere....Instance.2.participant...p26772_i2.",
      "Mean.thickness.of.parsopercularis..left.hemisphere....Instance.2.participant...p27189_i2.",
      "Mean.thickness.of.parsorbitalis..left.hemisphere....Instance.2.participant...p26773_i2.",
      "Mean.thickness.of.parsorbitalis..left.hemisphere....Instance.2.participant...p27190_i2.",
      "Mean.thickness.of.parstriangularis..left.hemisphere....Instance.2.participant...p26774_i2.",
      "Mean.thickness.of.parstriangularis..left.hemisphere....Instance.2.participant...p27191_i2.",
      "Mean.thickness.of.pericalcarine..left.hemisphere....Instance.2.participant...p26775_i2.",
      "Mean.thickness.of.pericalcarine..left.hemisphere....Instance.2.participant...p27192_i2.",
      "Mean.thickness.of.postcentral..left.hemisphere....Instance.2.participant...p26776_i2.",
      "Mean.thickness.of.postcentral..left.hemisphere....Instance.2.participant...p27193_i2.",
      "Mean.thickness.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p26777_i2.",
      "Mean.thickness.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p27194_i2.",
      "Mean.thickness.of.precentral..left.hemisphere....Instance.2.participant...p26778_i2.",
      "Mean.thickness.of.precentral..left.hemisphere....Instance.2.participant...p27195_i2.",
      "Mean.thickness.of.precuneus..left.hemisphere....Instance.2.participant...p26779_i2.",
      "Mean.thickness.of.precuneus..left.hemisphere....Instance.2.participant...p27196_i2.",
      "Mean.thickness.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p26780_i2.",
      "Mean.thickness.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p27197_i2.",
      "Mean.thickness.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p26781_i2.",
      "Mean.thickness.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p27198_i2.",
      "Mean.thickness.of.superiorfrontal..left.hemisphere....Instance.2.participant...p26782_i2.",
      "Mean.thickness.of.superiorfrontal..left.hemisphere....Instance.2.participant...p27199_i2.",
      "Mean.thickness.of.superiorparietal..left.hemisphere....Instance.2.participant...p26783_i2.",
      "Mean.thickness.of.superiorparietal..left.hemisphere....Instance.2.participant...p27200_i2.",
      "Mean.thickness.of.superiortemporal..left.hemisphere....Instance.2.participant...p26784_i2.",
      "Mean.thickness.of.superiortemporal..left.hemisphere....Instance.2.participant...p27201_i2.",
      "Mean.thickness.of.supramarginal..left.hemisphere....Instance.2.participant...p26785_i2.",
      "Mean.thickness.of.supramarginal..left.hemisphere....Instance.2.participant...p27202_i2.",
      "Mean.thickness.of.frontalpole..left.hemisphere....Instance.2",
      "Mean.thickness.of.transversetemporal..left.hemisphere....Instance.2.participant...p26787_i2.",
      "Mean.thickness.of.transversetemporal..left.hemisphere....Instance.2.participant...p27203_i2.",
      "Mean.thickness.of.insula..left.hemisphere....Instance.2.participant...p26788_i2.",
      "Mean.thickness.of.insula..left.hemisphere....Instance.2.participant...p27204_i2.",
      
      # 左半球皮层体积
      "Volume.of.bankssts..left.hemisphere....Instance.2",
      "Volume.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p26790_i2.",
      "Volume.of.caudalanteriorcingulate..left.hemisphere....Instance.2.participant...p27205_i2.",
      "Volume.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p26791_i2.",
      "Volume.of.caudalmiddlefrontal..left.hemisphere....Instance.2.participant...p27206_i2.",
      "Volume.of.cuneus..left.hemisphere....Instance.2.participant...p26792_i2.",
      "Volume.of.cuneus..left.hemisphere....Instance.2.participant...p27207_i2.",
      "Volume.of.entorhinal..left.hemisphere....Instance.2.participant...p26793_i2.",
      "Volume.of.entorhinal..left.hemisphere....Instance.2.participant...p27100_i2.",
      "Volume.of.entorhinal..left.hemisphere....Instance.2.participant...p27208_i2.",
      "Volume.of.fusiform..left.hemisphere....Instance.2.participant...p26794_i2.",
      "Volume.of.fusiform..left.hemisphere....Instance.2.participant...p27209_i2.",
      "Volume.of.inferiorparietal..left.hemisphere....Instance.2.participant...p26795_i2.",
      "Volume.of.inferiorparietal..left.hemisphere....Instance.2.participant...p27210_i2.",
      "Volume.of.inferiortemporal..left.hemisphere....Instance.2.participant...p26796_i2.",
      "Volume.of.inferiortemporal..left.hemisphere....Instance.2.participant...p27211_i2.",
      "Volume.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p26797_i2.",
      "Volume.of.isthmuscingulate..left.hemisphere....Instance.2.participant...p27212_i2.",
      "Volume.of.lateraloccipital..left.hemisphere....Instance.2.participant...p26798_i2.",
      "Volume.of.lateraloccipital..left.hemisphere....Instance.2.participant...p27213_i2.",
      "Volume.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p26799_i2.",
      "Volume.of.lateralorbitofrontal..left.hemisphere....Instance.2.participant...p27214_i2.",
      "Volume.of.lingual..left.hemisphere....Instance.2.participant...p26800_i2.",
      "Volume.of.lingual..left.hemisphere....Instance.2.participant...p27215_i2.",
      "Volume.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p26801_i2.",
      "Volume.of.medialorbitofrontal..left.hemisphere....Instance.2.participant...p27216_i2.",
      "Volume.of.middletemporal..left.hemisphere....Instance.2.participant...p26802_i2.",
      "Volume.of.middletemporal..left.hemisphere....Instance.2.participant...p27217_i2.",
      "Volume.of.parahippocampal..left.hemisphere....Instance.2.participant...p26803_i2.",
      "Volume.of.parahippocampal..left.hemisphere....Instance.2.participant...p27218_i2.",
      "Volume.of.paracentral..left.hemisphere....Instance.2.participant...p26804_i2.",
      "Volume.of.paracentral..left.hemisphere....Instance.2.participant...p27219_i2.",
      "Volume.of.parsopercularis..left.hemisphere....Instance.2.participant...p26805_i2.",
      "Volume.of.parsopercularis..left.hemisphere....Instance.2.participant...p27220_i2.",
      "Volume.of.parsorbitalis..left.hemisphere....Instance.2.participant...p26806_i2.",
      "Volume.of.parsorbitalis..left.hemisphere....Instance.2.participant...p27221_i2.",
      "Volume.of.parstriangularis..left.hemisphere....Instance.2.participant...p26807_i2.",
      "Volume.of.parstriangularis..left.hemisphere....Instance.2.participant...p27222_i2.",
      "Volume.of.pericalcarine..left.hemisphere....Instance.2.participant...p26808_i2.",
      "Volume.of.pericalcarine..left.hemisphere....Instance.2.participant...p27223_i2.",
      "Volume.of.postcentral..left.hemisphere....Instance.2.participant...p26809_i2.",
      "Volume.of.postcentral..left.hemisphere....Instance.2.participant...p27224_i2.",
      "Volume.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p26810_i2.",
      "Volume.of.posteriorcingulate..left.hemisphere....Instance.2.participant...p27225_i2.",
      "Volume.of.precentral..left.hemisphere....Instance.2.participant...p26811_i2.",
      "Volume.of.precentral..left.hemisphere....Instance.2.participant...p27226_i2.",
      "Volume.of.precuneus..left.hemisphere....Instance.2.participant...p26812_i2.",
      "Volume.of.precuneus..left.hemisphere....Instance.2.participant...p27227_i2.",
      "Volume.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p26813_i2.",
      "Volume.of.rostralanteriorcingulate..left.hemisphere....Instance.2.participant...p27228_i2.",
      "Volume.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p26814_i2.",
      "Volume.of.rostralmiddlefrontal..left.hemisphere....Instance.2.participant...p27229_i2.",
      "Volume.of.superiorfrontal..left.hemisphere....Instance.2.participant...p26815_i2.",
      "Volume.of.superiorfrontal..left.hemisphere....Instance.2.participant...p27230_i2.",
      "Volume.of.superiorparietal..left.hemisphere....Instance.2.participant...p26816_i2.",
      "Volume.of.superiorparietal..left.hemisphere....Instance.2.participant...p27231_i2.",
      "Volume.of.superiortemporal..left.hemisphere....Instance.2.participant...p26817_i2.",
      "Volume.of.superiortemporal..left.hemisphere....Instance.2.participant...p27232_i2.",
      "Volume.of.supramarginal..left.hemisphere....Instance.2.participant...p26818_i2.",
      "Volume.of.supramarginal..left.hemisphere....Instance.2.participant...p27233_i2.",
      "Volume.of.frontalpole..left.hemisphere....Instance.2",
      "Volume.of.transversetemporal..left.hemisphere....Instance.2.participant...p26820_i2.",
      "Volume.of.transversetemporal..left.hemisphere....Instance.2.participant...p27234_i2.",
      "Volume.of.insula..left.hemisphere....Instance.2.participant...p26821_i2.",
      "Volume.of.insula..left.hemisphere....Instance.2.participant...p27235_i2.",
      
      # 右半球皮层表面积
      "Area.of.TotalSurface..right.hemisphere....Instance.2.participant...p26822_i2.",
      "Area.of.TotalSurface..right.hemisphere....Instance.2.participant...p26956_i2.",
      "Area.of.bankssts..right.hemisphere....Instance.2.participant...p26823_i2.",
      "Area.of.bankssts..right.hemisphere....Instance.2.participant...p26957_i2.",
      "Area.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p26824_i2.",
      "Area.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p26958_i2.",
      "Area.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p27236_i2.",
      "Area.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p26825_i2.",
      "Area.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p26959_i2.",
      "Area.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p27237_i2.",
      "Area.of.cuneus..right.hemisphere....Instance.2.participant...p26826_i2.",
      "Area.of.cuneus..right.hemisphere....Instance.2.participant...p26960_i2.",
      "Area.of.cuneus..right.hemisphere....Instance.2.participant...p27238_i2.",
      "Area.of.entorhinal..right.hemisphere....Instance.2.participant...p26827_i2.",
      "Area.of.entorhinal..right.hemisphere....Instance.2.participant...p26961_i2.",
      "Area.of.entorhinal..right.hemisphere....Instance.2.participant...p27114_i2.",
      "Area.of.entorhinal..right.hemisphere....Instance.2.participant...p27239_i2.",
      "Area.of.fusiform..right.hemisphere....Instance.2.participant...p26828_i2.",
      "Area.of.fusiform..right.hemisphere....Instance.2.participant...p26962_i2.",
      "Area.of.fusiform..right.hemisphere....Instance.2.participant...p27240_i2.",
      "Area.of.inferiorparietal..right.hemisphere....Instance.2.participant...p26829_i2.",
      "Area.of.inferiorparietal..right.hemisphere....Instance.2.participant...p26963_i2.",
      "Area.of.inferiorparietal..right.hemisphere....Instance.2.participant...p27241_i2.",
      "Area.of.inferiortemporal..right.hemisphere....Instance.2.participant...p26830_i2.",
      "Area.of.inferiortemporal..right.hemisphere....Instance.2.participant...p26964_i2.",
      "Area.of.inferiortemporal..right.hemisphere....Instance.2.participant...p27242_i2.",
      "Area.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p26831_i2.",
      "Area.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p26965_i2.",
      "Area.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p27243_i2.",
      "Area.of.lateraloccipital..right.hemisphere....Instance.2.participant...p26832_i2.",
      "Area.of.lateraloccipital..right.hemisphere....Instance.2.participant...p26966_i2.",
      "Area.of.lateraloccipital..right.hemisphere....Instance.2.participant...p27244_i2.",
      "Area.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p26833_i2.",
      "Area.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p26967_i2.",
      "Area.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p27245_i2.",
      "Area.of.lingual..right.hemisphere....Instance.2.participant...p26834_i2.",
      "Area.of.lingual..right.hemisphere....Instance.2.participant...p26968_i2.",
      "Area.of.lingual..right.hemisphere....Instance.2.participant...p27246_i2.",
      "Area.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p26835_i2.",
      "Area.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p26969_i2.",
      "Area.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p27247_i2.",
      "Area.of.middletemporal..right.hemisphere....Instance.2.participant...p26836_i2.",
      "Area.of.middletemporal..right.hemisphere....Instance.2.participant...p26970_i2.",
      "Area.of.middletemporal..right.hemisphere....Instance.2.participant...p27248_i2.",
      "Area.of.parahippocampal..right.hemisphere....Instance.2.participant...p26837_i2.",
      "Area.of.parahippocampal..right.hemisphere....Instance.2.participant...p26971_i2.",
      "Area.of.parahippocampal..right.hemisphere....Instance.2.participant...p27249_i2.",
      "Area.of.paracentral..right.hemisphere....Instance.2.participant...p26838_i2.",
      "Area.of.paracentral..right.hemisphere....Instance.2.participant...p26972_i2.",
      "Area.of.paracentral..right.hemisphere....Instance.2.participant...p27250_i2.",
      "Area.of.parsopercularis..right.hemisphere....Instance.2.participant...p26839_i2.",
      "Area.of.parsopercularis..right.hemisphere....Instance.2.participant...p26973_i2.",
      "Area.of.parsopercularis..right.hemisphere....Instance.2.participant...p27251_i2.",
      "Area.of.parsorbitalis..right.hemisphere....Instance.2.participant...p26840_i2.",
      "Area.of.parsorbitalis..right.hemisphere....Instance.2.participant...p26974_i2.",
      "Area.of.parsorbitalis..right.hemisphere....Instance.2.participant...p27252_i2.",
      "Area.of.parstriangularis..right.hemisphere....Instance.2.participant...p26841_i2.",
      "Area.of.parstriangularis..right.hemisphere....Instance.2.participant...p26975_i2.",
      "Area.of.parstriangularis..right.hemisphere....Instance.2.participant...p27253_i2.",
      "Area.of.pericalcarine..right.hemisphere....Instance.2.participant...p26842_i2.",
      "Area.of.pericalcarine..right.hemisphere....Instance.2.participant...p26976_i2.",
      "Area.of.pericalcarine..right.hemisphere....Instance.2.participant...p27254_i2.",
      "Area.of.postcentral..right.hemisphere....Instance.2.participant...p26843_i2.",
      "Area.of.postcentral..right.hemisphere....Instance.2.participant...p26977_i2.",
      "Area.of.postcentral..right.hemisphere....Instance.2.participant...p27255_i2.",
      "Area.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p26844_i2.",
      "Area.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p26978_i2.",
      "Area.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p27256_i2.",
      "Area.of.precentral..right.hemisphere....Instance.2.participant...p26845_i2.",
      "Area.of.precentral..right.hemisphere....Instance.2.participant...p26979_i2.",
      "Area.of.precentral..right.hemisphere....Instance.2.participant...p27257_i2.",
      "Area.of.precuneus..right.hemisphere....Instance.2.participant...p26846_i2.",
      "Area.of.precuneus..right.hemisphere....Instance.2.participant...p26980_i2.",
      "Area.of.precuneus..right.hemisphere....Instance.2.participant...p27258_i2.",
      "Area.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p26847_i2.",
      "Area.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p26981_i2.",
      "Area.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p27259_i2.",
      "Area.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p26848_i2.",
      "Area.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p26982_i2.",
      "Area.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p27260_i2.",
      "Area.of.superiorfrontal..right.hemisphere....Instance.2.participant...p26849_i2.",
      "Area.of.superiorfrontal..right.hemisphere....Instance.2.participant...p26983_i2.",
      "Area.of.superiorfrontal..right.hemisphere....Instance.2.participant...p27261_i2.",
      "Area.of.superiorparietal..right.hemisphere....Instance.2.participant...p26850_i2.",
      "Area.of.superiorparietal..right.hemisphere....Instance.2.participant...p26984_i2.",
      "Area.of.superiorparietal..right.hemisphere....Instance.2.participant...p27262_i2.",
      "Area.of.superiortemporal..right.hemisphere....Instance.2.participant...p26851_i2.",
      "Area.of.superiortemporal..right.hemisphere....Instance.2.participant...p26985_i2.",
      "Area.of.superiortemporal..right.hemisphere....Instance.2.participant...p27263_i2.",
      "Area.of.supramarginal..right.hemisphere....Instance.2.participant...p26852_i2.",
      "Area.of.supramarginal..right.hemisphere....Instance.2.participant...p26986_i2.",
      "Area.of.supramarginal..right.hemisphere....Instance.2.participant...p27264_i2.",
      "Area.of.frontalpole..right.hemisphere....Instance.2.participant...p26853_i2.",
      "Area.of.frontalpole..right.hemisphere....Instance.2.participant...p26987_i2.",
      "Area.of.transversetemporal..right.hemisphere....Instance.2.participant...p26854_i2.",
      "Area.of.transversetemporal..right.hemisphere....Instance.2.participant...p26988_i2.",
      "Area.of.transversetemporal..right.hemisphere....Instance.2.participant...p27265_i2.",
      "Area.of.insula..right.hemisphere....Instance.2.participant...p26855_i2.",
      "Area.of.insula..right.hemisphere....Instance.2.participant...p27266_i2.",
      
      # 右半球皮层厚度
      "Mean.thickness.of.GlobalMeanMean.thickness..right.hemisphere....Instance.2",
      "Mean.thickness.of.bankssts..right.hemisphere....Instance.2",
      "Mean.thickness.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p26858_i2.",
      "Mean.thickness.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p27267_i2.",
      "Mean.thickness.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p26859_i2.",
      "Mean.thickness.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p27268_i2.",
      "Mean.thickness.of.cuneus..right.hemisphere....Instance.2.participant...p26860_i2.",
      "Mean.thickness.of.cuneus..right.hemisphere....Instance.2.participant...p27269_i2.",
      "Mean.thickness.of.entorhinal..right.hemisphere....Instance.2.participant...p26861_i2.",
      "Mean.thickness.of.entorhinal..right.hemisphere....Instance.2.participant...p27128_i2.",
      "Mean.thickness.of.entorhinal..right.hemisphere....Instance.2.participant...p27270_i2.",
      "Mean.thickness.of.fusiform..right.hemisphere....Instance.2.participant...p26862_i2.",
      "Mean.thickness.of.fusiform..right.hemisphere....Instance.2.participant...p27271_i2.",
      "Mean.thickness.of.inferiorparietal..right.hemisphere....Instance.2.participant...p26863_i2.",
      "Mean.thickness.of.inferiorparietal..right.hemisphere....Instance.2.participant...p27272_i2.",
      "Mean.thickness.of.inferiortemporal..right.hemisphere....Instance.2.participant...p26864_i2.",
      "Mean.thickness.of.inferiortemporal..right.hemisphere....Instance.2.participant...p27273_i2.",
      "Mean.thickness.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p26865_i2.",
      "Mean.thickness.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p27274_i2.",
      "Mean.thickness.of.lateraloccipital..right.hemisphere....Instance.2.participant...p26866_i2.",
      "Mean.thickness.of.lateraloccipital..right.hemisphere....Instance.2.participant...p27275_i2.",
      "Mean.thickness.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p26867_i2.",
      "Mean.thickness.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p27276_i2.",
      "Mean.thickness.of.lingual..right.hemisphere....Instance.2.participant...p26868_i2.",
      "Mean.thickness.of.lingual..right.hemisphere....Instance.2.participant...p27277_i2.",
      "Mean.thickness.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p26869_i2.",
      "Mean.thickness.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p27278_i2.",
      "Mean.thickness.of.middletemporal..right.hemisphere....Instance.2.participant...p26870_i2.",
      "Mean.thickness.of.middletemporal..right.hemisphere....Instance.2.participant...p27279_i2.",
      "Mean.thickness.of.parahippocampal..right.hemisphere....Instance.2.participant...p26871_i2.",
      "Mean.thickness.of.parahippocampal..right.hemisphere....Instance.2.participant...p27280_i2.",
      "Mean.thickness.of.paracentral..right.hemisphere....Instance.2.participant...p26872_i2.",
      "Mean.thickness.of.paracentral..right.hemisphere....Instance.2.participant...p27281_i2.",
      "Mean.thickness.of.parsopercularis..right.hemisphere....Instance.2.participant...p26873_i2.",
      "Mean.thickness.of.parsopercularis..right.hemisphere....Instance.2.participant...p27282_i2.",
      "Mean.thickness.of.parsorbitalis..right.hemisphere....Instance.2.participant...p26874_i2.",
      "Mean.thickness.of.parsorbitalis..right.hemisphere....Instance.2.participant...p27283_i2.",
      "Mean.thickness.of.parstriangularis..right.hemisphere....Instance.2.participant...p26875_i2.",
      "Mean.thickness.of.parstriangularis..right.hemisphere....Instance.2.participant...p27284_i2.",
      "Mean.thickness.of.pericalcarine..right.hemisphere....Instance.2.participant...p26876_i2.",
      "Mean.thickness.of.pericalcarine..right.hemisphere....Instance.2.participant...p27285_i2.",
      "Mean.thickness.of.postcentral..right.hemisphere....Instance.2.participant...p26877_i2.",
      "Mean.thickness.of.postcentral..right.hemisphere....Instance.2.participant...p27286_i2.",
      "Mean.thickness.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p26878_i2.",
      "Mean.thickness.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p27287_i2.",
      "Mean.thickness.of.precentral..right.hemisphere....Instance.2.participant...p26879_i2.",
      "Mean.thickness.of.precentral..right.hemisphere....Instance.2.participant...p27288_i2.",
      "Mean.thickness.of.precuneus..right.hemisphere....Instance.2.participant...p26880_i2.",
      "Mean.thickness.of.precuneus..right.hemisphere....Instance.2.participant...p27289_i2.",
      "Mean.thickness.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p26881_i2.",
      "Mean.thickness.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p27290_i2.",
      "Mean.thickness.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p26882_i2.",
      "Mean.thickness.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p27291_i2.",
      "Mean.thickness.of.superiorfrontal..right.hemisphere....Instance.2.participant...p26883_i2.",
      "Mean.thickness.of.superiorfrontal..right.hemisphere....Instance.2.participant...p27292_i2.",
      "Mean.thickness.of.superiorparietal..right.hemisphere....Instance.2.participant...p26884_i2.",
      "Mean.thickness.of.superiorparietal..right.hemisphere....Instance.2.participant...p27293_i2.",
      "Mean.thickness.of.superiortemporal..right.hemisphere....Instance.2.participant...p26885_i2.",
      "Mean.thickness.of.superiortemporal..right.hemisphere....Instance.2.participant...p27294_i2.",
      "Mean.thickness.of.supramarginal..right.hemisphere....Instance.2.participant...p26886_i2.",
      "Mean.thickness.of.supramarginal..right.hemisphere....Instance.2.participant...p27295_i2.",
      "Mean.thickness.of.frontalpole..right.hemisphere....Instance.2",
      "Mean.thickness.of.transversetemporal..right.hemisphere....Instance.2.participant...p26888_i2.",
      "Mean.thickness.of.transversetemporal..right.hemisphere....Instance.2.participant...p27296_i2.",
      "Mean.thickness.of.insula..right.hemisphere....Instance.2.participant...p26889_i2.",
      "Mean.thickness.of.insula..right.hemisphere....Instance.2.participant...p27297_i2.",
      
      # 右半球皮层体积
      "Volume.of.bankssts..right.hemisphere....Instance.2",
      "Volume.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p26891_i2.",
      "Volume.of.caudalanteriorcingulate..right.hemisphere....Instance.2.participant...p27298_i2.",
      "Volume.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p26892_i2.",
      "Volume.of.caudalmiddlefrontal..right.hemisphere....Instance.2.participant...p27299_i2.",
      "Volume.of.cuneus..right.hemisphere....Instance.2.participant...p26893_i2.",
      "Volume.of.cuneus..right.hemisphere....Instance.2.participant...p27300_i2.",
      "Volume.of.entorhinal..right.hemisphere....Instance.2.participant...p26894_i2.",
      "Volume.of.entorhinal..right.hemisphere....Instance.2.participant...p27142_i2.",
      "Volume.of.entorhinal..right.hemisphere....Instance.2.participant...p27301_i2.",
      "Volume.of.fusiform..right.hemisphere....Instance.2.participant...p26895_i2.",
      "Volume.of.fusiform..right.hemisphere....Instance.2.participant...p27302_i2.",
      "Volume.of.inferiorparietal..right.hemisphere....Instance.2.participant...p26896_i2.",
      "Volume.of.inferiorparietal..right.hemisphere....Instance.2.participant...p27303_i2.",
      "Volume.of.inferiortemporal..right.hemisphere....Instance.2.participant...p26897_i2.",
      "Volume.of.inferiortemporal..right.hemisphere....Instance.2.participant...p27304_i2.",
      "Volume.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p26898_i2.",
      "Volume.of.isthmuscingulate..right.hemisphere....Instance.2.participant...p27305_i2.",
      "Volume.of.lateraloccipital..right.hemisphere....Instance.2.participant...p26899_i2.",
      "Volume.of.lateraloccipital..right.hemisphere....Instance.2.participant...p27306_i2.",
      "Volume.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p26900_i2.",
      "Volume.of.lateralorbitofrontal..right.hemisphere....Instance.2.participant...p27307_i2.",
      "Volume.of.lingual..right.hemisphere....Instance.2.participant...p26901_i2.",
      "Volume.of.lingual..right.hemisphere....Instance.2.participant...p27308_i2.",
      "Volume.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p26902_i2.",
      "Volume.of.medialorbitofrontal..right.hemisphere....Instance.2.participant...p27309_i2.",
      "Volume.of.middletemporal..right.hemisphere....Instance.2.participant...p26903_i2.",
      "Volume.of.middletemporal..right.hemisphere....Instance.2.participant...p27310_i2.",
      "Volume.of.parahippocampal..right.hemisphere....Instance.2.participant...p26904_i2.",
      "Volume.of.parahippocampal..right.hemisphere....Instance.2.participant...p27311_i2.",
      "Volume.of.paracentral..right.hemisphere....Instance.2.participant...p26905_i2.",
      "Volume.of.paracentral..right.hemisphere....Instance.2.participant...p27312_i2.",
      "Volume.of.parsopercularis..right.hemisphere....Instance.2.participant...p26906_i2.",
      "Volume.of.parsopercularis..right.hemisphere....Instance.2.participant...p27313_i2.",
      "Volume.of.parsorbitalis..right.hemisphere....Instance.2.participant...p26907_i2.",
      "Volume.of.parsorbitalis..right.hemisphere....Instance.2.participant...p27314_i2.",
      "Volume.of.parstriangularis..right.hemisphere....Instance.2.participant...p26908_i2.",
      "Volume.of.parstriangularis..right.hemisphere....Instance.2.participant...p27315_i2.",
      "Volume.of.pericalcarine..right.hemisphere....Instance.2.participant...p26909_i2.",
      "Volume.of.pericalcarine..right.hemisphere....Instance.2.participant...p27316_i2.",
      "Volume.of.postcentral..right.hemisphere....Instance.2.participant...p26910_i2.",
      "Volume.of.postcentral..right.hemisphere....Instance.2.participant...p27317_i2.",
      "Volume.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p26911_i2.",
      "Volume.of.posteriorcingulate..right.hemisphere....Instance.2.participant...p27318_i2.",
      "Volume.of.precentral..right.hemisphere....Instance.2.participant...p26912_i2.",
      "Volume.of.precentral..right.hemisphere....Instance.2.participant...p27319_i2.",
      "Volume.of.precuneus..right.hemisphere....Instance.2.participant...p26913_i2.",
      "Volume.of.precuneus..right.hemisphere....Instance.2.participant...p27320_i2.",
      "Volume.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p26914_i2.",
      "Volume.of.rostralanteriorcingulate..right.hemisphere....Instance.2.participant...p27321_i2.",
      "Volume.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p26915_i2.",
      "Volume.of.rostralmiddlefrontal..right.hemisphere....Instance.2.participant...p27322_i2.",
      "Volume.of.superiorfrontal..right.hemisphere....Instance.2.participant...p26916_i2.",
      "Volume.of.superiorfrontal..right.hemisphere....Instance.2.participant...p27323_i2.",
      "Volume.of.superiorparietal..right.hemisphere....Instance.2.participant...p26917_i2.",
      "Volume.of.superiorparietal..right.hemisphere....Instance.2.participant...p27324_i2.",
      "Volume.of.superiortemporal..right.hemisphere....Instance.2.participant...p26918_i2.",
      "Volume.of.superiortemporal..right.hemisphere....Instance.2.participant...p27325_i2.",
      "Volume.of.supramarginal..right.hemisphere....Instance.2.participant...p26919_i2.",
      "Volume.of.supramarginal..right.hemisphere....Instance.2.participant...p27326_i2.",
      "Volume.of.frontalpole..right.hemisphere....Instance.2",
      "Volume.of.transversetemporal..right.hemisphere....Instance.2.participant...p26921_i2.",
      "Volume.of.transversetemporal..right.hemisphere....Instance.2.participant...p27327_i2.",
      "Volume.of.insula..right.hemisphere....Instance.2.participant...p26922_i2.",
      "Volume.of.insula..right.hemisphere....Instance.2.participant...p27328_i2.",
      
      # 杏仁核亚区
      "Volume.of.Lateral.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Basal.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Accessory.Basal.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Anterior.amygdaloid.area.AAA..left.hemisphere....Instance.2",
      "Volume.of.Central.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Medial.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Cortical.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Corticoamygdaloid.transitio..left.hemisphere....Instance.2",
      "Volume.of.Paralaminar.nucleus..left.hemisphere....Instance.2",
      "Volume.of.Whole.amygdala..left.hemisphere....Instance.2",
      "Volume.of.Lateral.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Basal.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Accessory.Basal.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Anterior.amygdaloid.area.AAA..right.hemisphere....Instance.2",
      "Volume.of.Central.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Medial.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Cortical.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Corticoamygdaloid.transitio..right.hemisphere....Instance.2",
      "Volume.of.Paralaminar.nucleus..right.hemisphere....Instance.2",
      "Volume.of.Whole.amygdala..right.hemisphere....Instance.2",
      
      # 海马亚区
      "Volume.of.Hippocampal.tail..left.hemisphere....Instance.2",
      "Volume.of.subiculum.body..left.hemisphere....Instance.2",
      "Volume.of.CA1.body..left.hemisphere....Instance.2",
      "Volume.of.subiculum.head..left.hemisphere....Instance.2",
      "Volume.of.hippocampal.fissure..left.hemisphere....Instance.2",
      "Volume.of.presubiculum.head..left.hemisphere....Instance.2",
      "Volume.of.CA1.head..left.hemisphere....Instance.2",
      "Volume.of.presubiculum.body..left.hemisphere....Instance.2",
      "Volume.of.parasubiculum..left.hemisphere....Instance.2",
      "Volume.of.molecular.layer.HP.head..left.hemisphere....Instance.2",
      "Volume.of.molecular.layer.HP.body..left.hemisphere....Instance.2",
      "Volume.of.GC.ML.DG.head..left.hemisphere....Instance.2",
      "Volume.of.CA3.body..left.hemisphere....Instance.2",
      "Volume.of.GC.ML.DG.body..left.hemisphere....Instance.2",
      "Volume.of.CA4.head..left.hemisphere....Instance.2",
      "Volume.of.CA4.body..left.hemisphere....Instance.2",
      "Volume.of.fimbria..left.hemisphere....Instance.2",
      "Volume.of.CA3.head..left.hemisphere....Instance.2",
      "Volume.of.HATA..left.hemisphere....Instance.2",
      "Volume.of.Whole.hippocampal.body..left.hemisphere....Instance.2",
      "Volume.of.Whole.hippocampal.head..left.hemisphere....Instance.2",
      "Volume.of.Whole.hippocampus..left.hemisphere....Instance.2",
      "Volume.of.Hippocampal.tail..right.hemisphere....Instance.2",
      "Volume.of.subiculum.body..right.hemisphere....Instance.2",
      "Volume.of.CA1.body..right.hemisphere....Instance.2",
      "Volume.of.subiculum.head..right.hemisphere....Instance.2",
      "Volume.of.hippocampal.fissure..right.hemisphere....Instance.2",
      "Volume.of.presubiculum.head..right.hemisphere....Instance.2",
      "Volume.of.CA1.head..right.hemisphere....Instance.2",
      "Volume.of.presubiculum.body..right.hemisphere....Instance.2",
      "Volume.of.parasubiculum..right.hemisphere....Instance.2",
      "Volume.of.molecular.layer.HP.head..right.hemisphere....Instance.2",
      "Volume.of.molecular.layer.HP.body..right.hemisphere....Instance.2",
      "Volume.of.GC.ML.DG.head..right.hemisphere....Instance.2",
      "Volume.of.CA3.body..right.hemisphere....Instance.2",
      "Volume.of.GC.ML.DG.body..right.hemisphere....Instance.2",
      "Volume.of.CA4.head..right.hemisphere....Instance.2",
      "Volume.of.CA4.body..right.hemisphere....Instance.2",
      "Volume.of.fimbria..right.hemisphere....Instance.2",
      "Volume.of.CA3.head..right.hemisphere....Instance.2",
      "Volume.of.HATA..right.hemisphere....Instance.2",
      "Volume.of.Whole.hippocampal.body..right.hemisphere....Instance.2",
      "Volume.of.Whole.hippocampal.head..right.hemisphere....Instance.2",
      "Volume.of.Whole.hippocampus..right.hemisphere....Instance.2",
      
      # 丘脑亚区
      "Volume.of.MGN..left.hemisphere....Instance.2",
      "Volume.of.LGN..left.hemisphere....Instance.2",
      "Volume.of.PuI..left.hemisphere....Instance.2",
      "Volume.of.PuM..left.hemisphere....Instance.2",
      "Volume.of.L.Sg..left.hemisphere....Instance.2",
      "Volume.of.VPL..left.hemisphere....Instance.2",
      "Volume.of.CM..left.hemisphere....Instance.2",
      "Volume.of.VLa..left.hemisphere....Instance.2",
      "Volume.of.PuA..left.hemisphere....Instance.2",
      "Volume.of.MDm..left.hemisphere....Instance.2",
      "Volume.of.Pf..left.hemisphere....Instance.2",
      "Volume.of.VAmc..left.hemisphere....Instance.2",
      "Volume.of.MDl..left.hemisphere....Instance.2",
      "Volume.of.CeM..left.hemisphere....Instance.2",
      "Volume.of.VA..left.hemisphere....Instance.2",
      "Volume.of.MV.Re...left.hemisphere....Instance.2",
      "Volume.of.VM..left.hemisphere....Instance.2",
      "Volume.of.CL..left.hemisphere....Instance.2",
      "Volume.of.PuL..left.hemisphere....Instance.2",
      "Volume.of.Pt..left.hemisphere....Instance.2",
      "Volume.of.AV..left.hemisphere....Instance.2",
      "Volume.of.Pc..left.hemisphere....Instance.2",
      "Volume.of.VLp..left.hemisphere....Instance.2",
      "Volume.of.LP..left.hemisphere....Instance.2",
      "Volume.of.LGN..right.hemisphere....Instance.2",
      "Volume.of.MGN..right.hemisphere....Instance.2",
      "Volume.of.PuI..right.hemisphere....Instance.2",
      "Volume.of.PuM..right.hemisphere....Instance.2",
      "Volume.of.L.Sg..right.hemisphere....Instance.2",
      "Volume.of.VPL..right.hemisphere....Instance.2",
      "Volume.of.CM..right.hemisphere....Instance.2",
      "Volume.of.VLa..right.hemisphere....Instance.2",
      "Volume.of.PuA..right.hemisphere....Instance.2",
      "Volume.of.MDm..right.hemisphere....Instance.2",
      "Volume.of.Pf..right.hemisphere....Instance.2",
      "Volume.of.VAmc..right.hemisphere....Instance.2",
      "Volume.of.MDl..right.hemisphere....Instance.2",
      "Volume.of.VA..right.hemisphere....Instance.2",
      "Volume.of.MV.Re...right.hemisphere....Instance.2",
      "Volume.of.CeM..right.hemisphere....Instance.2",
      "Volume.of.VM..right.hemisphere....Instance.2",
      "Volume.of.PuL..right.hemisphere....Instance.2",
      "Volume.of.CL..right.hemisphere....Instance.2",
      "Volume.of.VLp..right.hemisphere....Instance.2",
      "Volume.of.Pc..right.hemisphere....Instance.2",
      "Volume.of.Pt..right.hemisphere....Instance.2",
      "Volume.of.AV..right.hemisphere....Instance.2",
      "Volume.of.LP..right.hemisphere....Instance.2",
      "Volume.of.LD..left.hemisphere....Instance.2",
      "Volume.of.LD..right.hemisphere....Instance.2",
      "Volume.of.Whole.thalamus..left.hemisphere....Instance.2",
      "Volume.of.Whole.thalamus..right.hemisphere....Instance.2",
      
      # 脑干
      "Volume.of.Medulla..whole.brain....Instance.2",
      "Volume.of.Pons..whole.brain....Instance.2",
      "Volume.of.SCP..whole.brain....Instance.2",
      "Volume.of.Midbrain..whole.brain....Instance.2",
      "Volume.of.Whole.brainstem..whole.brain....Instance.2"
    ),
    
    # 新增Olink_Proteomics类别
    "Olink_Proteomics" = c(
      "CCL24", "CCL25", "CCL26", "CCL27", "CCL28", "CCL3", "CCL4", "CCL5", "CCL7", "CCL23",
      "CCL22", "CCL21", "CCL20", "CCL2", "CCL19", "CCL18", "CCL17", "CCL16", "CCL15", "CCL14",
      "CCL13", "CCL8", "CD109", "CD200R1", "CD200", "CD2", "CD1C", "CD177", "CD164L2", "CD164",
      "CD163", "CD160", "CD14", "CCN1", "CD101", "CCT5", "CCS", "CCNE1", "CCND2", "CCN5",
      "CCN4", "CCN3", "CCN2", "CD207", "CCL11", "CALCOCO1", "CARHSP1", "CAPS", "CAPN3", "CAPG",
      "CANT1", "CAMSAP1", "CAMLG", "CAMKK1", "CALY", "CALCOCO2", "CACNA1C", "CALCB", "CALCA", "CALB2",
      "CALB1", "CADPS", "CACYBP", "CACNB3", "CACNB1", "CACNA1H", "CASC3", "CBLIF", "CCDC80", "CCDC50",
      "CCDC28A", "CCDC134", "CCAR2", "CC2D1A", "CBX2", "CBS", "CBLN4", "CBLN1", "CASP1", "CAT",
      "CASQ2", "CASP9", "CASP8", "CASP7", "CASP4", "CASP3", "CASP2", "CASP10", "CCER2", "CDC27",
      "CDH3", "CDH23", "CDH22", "CDH2", "CDH17", "CDH15", "CDH1", "CDCP1", "CDC42BPB", "CDC37",
      "CD86", "CDC26", "CDC25A", "CDC123", "CDAN1", "CDA", "CD99L2", "CD99", "CD93", "CD8A",
      "CDH4", "CDNF", "CEACAM5", "CEACAM3", "CEACAM21", "CEACAM20", "CEACAM19", "CEACAM18", "CEACAM16", "CEACAM1",
      "CDSN", "CDON", "CDH5", "CDKN2D", "CDKN1A", "CDKL5", "CDK5RAP3", "CDK1", "CDHR5", "CDHR2",
      "CDHR1", "CDH6", "CEACAM6", "CD84", "CD300A", "CD3D", "CD38", "CD36", "CD34", "CD33",
      "CD302", "CD300LG", "CD300LF", "CD300E", "CD300C", "CD209", "CD2AP", "CD28", "CD276", "CD274",
      "CD27", "CD248", "CD244", "CD226", "CD22", "CD3E", "CD5L", "CD82", "CD80", "CD79B",
      "CD74", "CD72", "CD70", "CD7", "CD69", "CD63", "CD6", "CD3G", "CD59", "CD58",
      "CD55", "CD5", "CD48", "CD46", "CD40LG", "CD40", "CD4", "CD83", "ATP6V1G1", "AZI2",
      "AXL", "AXIN1", "ATXN3", "ATXN2L", "ATXN2", "ATXN10", "ATRN", "ATRAID", "ATP6V1G2", "ATP1B2",
      "ATP6V1F", "ATP6V1D", "ATP6AP2", "ATP5PO", "ATP5IF1", "ATP5F1D", "ATP2B4", "ATP1B4", "ATP1B3", "AZU1",
      "BAIAP2", "BCHE", "BCAT2", "BCAT1", "BCAN", "BCAM", "BAX", "BATF", "BAP18", "BANK1",
      "BAMBI", "B2M", "BAG6", "BAG4", "BAG3", "BACH1", "BABAM1", "B4GAT1", "B4GALT1", "B3GNT7",
      "B3GAT3", "BCL2", "ATP1B1", "ARHGAP1", "ARID4B", "ARID3A", "ARHGEF5", "ARHGEF12", "ARHGEF10", "ARHGEF1",
      "ARHGAP5", "ARHGAP45", "ARHGAP30", "ARHGAP25", "APOM", "ARG2", "ARG1", "ARFIP1", "ARF6", "AREG",
      "ARAF", "APRT", "APPL2", "APP", "ARL13B", "ASAH2", "ATG4A", "ATG16L1", "ATF4", "ATF2",
      "ASS1", "ASRGL1", "ASPSCR1", "ASPN", "ASGR2", "ASGR1", "ARL2BP", "ASAH1", "ARTN", "ART5",
      "ART3", "ARSB", "ARSA", "ARNTL", "ARNT", "ARMCX2", "ATOX1", "C1QBP", "C2CD2L", "C2",
      "C1S", "C1RL", "C1R", "C1QTNF9", "C1QTNF6", "C1QTNF5", "C1QTNF1", "C1QL2", "BTC", "C1QA",
      "C1GALT1C1", "C19orf12", "BTNL10", "BTN3A2", "BTN2A1", "BTN1A1", "BTLA", "BTD", "C2orf69", "CA12",
      "CA9", "CA8", "CA7", "CA6", "CA5A", "CA4", "CA3", "CA2", "CA14", "CA13",
      "C3", "CA11", "CA1", "C9orf40", "C9", "C8B", "C7orf50", "C7", "C5", "C4BPB",
      "CABP2", "BST2", "BGN", "BLVRB", "BLOC1S3", "BLOC1S2", "BLNK", "BLMH", "BIRC2", "BIN2",
      "BID", "BHMT2", "BHLHE40", "BCL2L1", "BGLAP", "BEX3", "BECN1", "BDNF", "BCR", "BCL7B",
      "BCL7A", "BCL2L15", "BCL2L11", "BMP10", "BPIFB2", "BSND", "BSG", "BRSK2", "BRME1", "BRK1",
      "BRDT", "BRD3", "BRD2", "BRD1", "BRAP", "BMP4", "BPIFB1", "BPIFA2", "BOLA2_BOLA2B", "BOLA1",
      "BOC", "BNIP3L", "BNIP2", "BMPER", "BMP6", "BST1", "CEACAM8", "PPT1", "PPP1R12A", "PPP1R12B",
      "PPP1R14A", "PPP1R14D", "PPP1R2", "PPP1R9B", "PPP2R5A", "PPP3R1", "PPME1", "PPY", "PQBP1", "PRAME",
      "PRAP1", "PRC1", "PRCP", "PRDX1", "PPP1CC", "PODXL2", "PPBP", "POF1B", "POLR2A", "POLR2F",
      "POMC", "PON1", "PON2", "PON3", "POSTN", "PPM1F", "PPCDC", "PPIB", "PPIE", "PPIF",
      "PPL", "PPM1A", "PPM1B", "PRDX2", "PRDX3", "PRTFDC1", "PRR4", "PRR5", "PRRT3", "PRSS2",
      "PRSS22", "PRSS27", "PRSS53", "PRSS8", "PROK1", "PRTG", "PRTN3", "PRUNE2", "PSAP", "PSAPL1",
      "PSCA", "PSG1", "PROS1", "PRDX5", "PRKAR2A", "PRDX6", "PREB", "PRELP", "PRG2", "PRG3",
      "PRKAB1", "PRKAG3", "PRKAR1A", "PROCR", "PRKCQ", "PRKD2", "PRKG1", "PRKRA", "PRL", "PRND",
      "PROC", "PSIP1", "PIK3AP1", "PHLDB1", "PHLDB2", "PHOSPHO1", "PHYKPL", "PI16", "PI3", "PIBF1",
      "PIGR", "PGR", "PIK3IP1", "PIKFYVE", "PILRA", "PILRB", "PINLYP", "PITHD1", "PKD1", "PHACTR2",
      "PEBP1", "PFDN6", "PECAM1", "PECR", "PENK", "PEPD", "PER3", "PF4", "PFDN2", "PFDN4",
      "PGM2", "PFKFB2", "PGA4", "PGD", "PGF", "PGLYRP1", "PGLYRP2", "PGLYRP4", "PKD2", "PKLR",
      "PMS1", "PLXDC1", "PLXDC2", "PLXNA4", "PLXNB2", "PLXNB3", "PM20D1", "PMCH", "PMM2", "PLSCR3",
      "PMVK", "PNLIP", "PNLIPRP1", "PNLIPRP2", "PNMA1", "PNMA2", "PNPT1", "PLTP", "PKN3", "PLAUR",
      "PLA2G10", "PLA2G15", "PLA2G1B", "PLA2G2A", "PLA2G4A", "PLA2G7", "PLAT", "PLAU", "PLPBP", "PLB1",
      "PLCB1", "PLCB2", "PLEKHO1", "PLG", "PLIN1", "PLIN3", "PODXL", "RETN", "REG3G", "REG4",
      "RELB", "RELT", "REN", "REPS1", "REST", "RET", "REG1B", "REXO2", "RFC4", "RGCC",
      "RGL2", "RGMA", "RGMB", "RGS10", "REG3A", "RASGRF1", "RBP5", "RASSF2", "RBFOX3", "RBKS",
      "RBM17", "RBM19", "RBM25", "RBP1", "RBP2", "REG1A", "RBP7", "RBPMS", "RBPMS2", "RCC1",
      "RCOR1", "RECK", "REEP4", "RGS8", "RHOC", "RPGR", "RNF5", "ROBO1", "ROBO2", "ROBO4",
      "ROR1", "RP2", "RPA2", "RPE", "RNF41", "RPL14", "RPS10", "RRAS", "RRM2", "RRM2B",
      "RRP15", "RSPO1", "RNF43", "RICTOR", "RNASE3", "RIDA", "RILP", "RILPL2", "RIPK4", "RLN1",
      "RLN2", "RNASE1", "RNASE10", "RNF4", "RNASE4", "RNASE6", "RNASEH2A", "RNASET2", "RNF149", "RNF168",
      "RNF31", "RSPO3", "PTPRK", "PTP4A3", "PTPN1", "PTPN6", "PTPN9", "PTPRB", "PTPRC", "PTPRF",
      "PTPRH", "PTK7", "PTPRM", "PTPRN2", "PTPRR", "PTPRS", "PTPRZ1", "PTRHD1", "PTS", "PTN",
      "PSMA1", "PSPN", "PSMC3", "PSMD1", "PSMD5", "PSMD9", "PSME1", "PSME2", "PSMG3", "PSMG4",
      "PTH1R", "PSRC1", "PSTPIP2", "PTEN", "PTGDS", "PTGES2", "PTGR1", "PTH", "PTTG1", "PTX3",
      "RALY", "RAB6B", "RABEP1", "RABEPK", "RABGAP1L", "RAC3", "RAD23B", "RAD51", "RALB", "RAB44",
      "RANBP1", "RANBP2", "RANGAP1", "RAP1A", "RAPGEF2", "RARRES1", "RARRES2", "RAB6A", "PVALB", "QSOX1",
      "PVR", "PXDNL", "PXN", "PYDC1", "PYY", "PZP", "QDPR", "QPCT", "RAB3GAP1", "RAB10",
      "RAB11FIP3", "RAB27B", "RAB2B", "RAB33A", "RAB37", "RAB39B", "RASA1", "PEAR1", "NCK2", "NAPRT",
      "NARS1", "NBL1", "NBN", "NCAM1", "NCAM2", "NCAN", "NCF2", "NAMPT", "NCLN", "NCR1",
      "NCR3LG1", "NCS1", "NDRG1", "NDST1", "NDUFA5", "NAP1L4", "MYL6B", "MZT1", "MYLPF", "MYO6",
      "MYO9B", "MYOC", "MYOM1", "MYOM2", "MYOM3", "MZB1", "NAGPA", "NAA10", "NAA80", "NAAA",
      "NACC1", "NADK", "NAGA", "NAGK", "NDUFB7", "NDUFS6", "NGRN", "NFKB1", "NFKB2", "NFKBIE",
      "NFU1", "NFX1", "NFYA", "NGF", "NGFR", "NFE2", "NHLRC3", "NID1", "NID2", "NINJ1",
      "NIT1", "NIT2", "NLGN1", "NFIC", "NEB", "NELL1", "NECAP2", "NECTIN1", "NECTIN2", "NECTIN4",
      "NEDD4L", "NEDD9", "NEFL", "NEK7", "NFATC3", "NELL2", "NENF", "NEO1", "NEXN", "NFASC",
      "NFAT5", "NFATC1", "NLGN2", "MORF4L2", "MMUT", "MN1", "MNAT1", "MNDA", "MOCS2", "MOG",
      "MORC3", "MORF4L1", "MMP8", "MORN4", "MPHOSPH8", "MPI", "MPIG6B", "MPO", "MPRIP", "MRC1",
      "MMP9", "MICALL2", "MLN", "MICB_MICA", "MIF", "MILR1", "MINDY1", "MINK1", "MITD1", "MKI67",
      "MLLT1", "MMP7", "MME", "MMP1", "MMP10", "MMP12", "MMP13", "MMP15", "MMP3", "MRI1",
      "MRPL24", "MYBPC2", "MTUS1", "MUC13", "MUC16", "MUC2", "MUCL3", "MVK", "MXRA8", "MYBPC1",
      "MTSS1", "MYCBP2", "MYDGF", "MYH4", "MYH7B", "MYH9", "MYL1", "MYL3", "MTSS2", "MRPL28",
      "MSRA", "MRPL46", "MRPL52", "MRPL58", "MRPS16", "MSLN", "MSLNL", "MSMB", "MSR1", "MTR",
      "MST1", "MSTN", "MTDH", "MTHFD2", "MTHFSD", "MTIF3", "MTPN", "MYL4", "PAPPA", "PAIP2B",
      "PAK4", "PALLD", "PALM", "PALM2", "PALM3", "PAM", "PAMR1", "PAG1", "PARD3", "PARK7",
      "PARP1", "PAXX", "PBK", "PBLD", "PBXIP1", "PAGR1", "ORM1", "OXCT1", "OSBPL2", "OSCAR",
      "OSM", "OSMR", "OSTN", "OTOA", "OTUD6B", "OTUD7B", "PAFAH2", "OXT", "P4HB", "PACS2",
      "PADI2", "PADI4", "PAEP", "PAFAH1B3", "PCARE", "PCBD1", "PDIA4", "PDE5A", "PDGFA", "PDGFB",
      "PDGFC", "PDGFRA", "PDGFRB", "PDIA2", "PDIA3", "PDE1C", "PDIA5", "PDLIM5", "PDLIM7", "PDP1",
      "PDRG1", "PDXDC1", "PDZD2", "PDE4D", "PCBP2", "PCSK7", "PCDH1", "PCDH12", "PCDH17", "PCDH7",
      "PCDH9", "PCDHB15", "PCNA", "PDCL2", "PCSK9", "PCYT2", "PDAP1", "PDCD1", "PDCD1LG2", "PDCD5",
      "PDCD6", "PDZK1", "NRCAM", "NPPB", "NPPC", "NPR1", "NPTN", "NPTX1", "NPTX2", "NPTXR",
      "NPY", "NPL", "NRGN", "NRN1", "NRP1", "NRP2", "NRTN", "NRXN3", "NSFL1C", "NME1",
      "NOS2", "NME3", "NMI", "NMNAT1", "NMRK2", "NMT1", "NOMO1", "NOP56", "NOS1", "NPHS2",
      "NOS3", "NOTCH1", "NOTCH2", "NOTCH3", "NPC2", "NPDC1", "NPHS1", "NT5C", "NT5C1A", "OGT",
      "NXPH3", "OBP2B", "OCLN", "ODAM", "OFD1", "OGA", "OGFR", "OGN", "NXPE4", "OLFM4",
      "OLR1", "OMD", "OMG", "OMP", "OPHN1", "OPLAH", "NXPH1", "NT5C3A", "NUCB2", "NT5E",
      "NTF3", "NTF4", "NTproBNP", "NTRK2", "NTRK3", "NUB1", "NUBP1", "NUP50", "NUDC", "NUDT10",
      "NUDT15", "NUDT16", "NUDT2", "NUDT5", "NUMB", "OPTC", "RTBDN", "TNFRSF1A", "TNFRSF11A", "TNFRSF11B",
      "TNFRSF12A", "TNFRSF13B", "TNFRSF13C", "TNFRSF14", "TNFRSF17", "TNFRSF19", "TNFRSF10B", "TNFRSF1B", "TNFRSF21", "TNFRSF4",
      "TNFRSF6B", "TNFRSF8", "TNFRSF9", "TNFSF10", "TNFRSF10C", "TMED10", "TMPRSS15", "TMED4", "TMED8", "TMEM106A",
      "TMEM132A", "TMEM25", "TMOD4", "TMPRSS11B", "TMPRSS11D", "TNFRSF10A", "TMPRSS5", "TMSB10", "TNC", "TNF",
      "TNFAIP2", "TNFAIP8", "TNFAIP8L2", "TNFSF11", "TNFSF12", "TPR", "TPBGL", "TPD52L2", "TPK1", "TPM3",
      "TPMT", "TPP1", "TPPP2", "TPPP3", "TP53INP1", "TPRKB", "TPSAB1", "TPSD1", "TPSG1", "TPT1",
      "TRAF2", "TRAF3", "TP73", "TNFSF13", "TNXB", "TNFSF13B", "TNFSF14", "TNFSF8", "TNIP1", "TNN",
      "TNNI3", "TNPO1", "TNR", "TP53I3", "TOMM20", "TOP1", "TOP1MT", "TOP2B", "TOR1AIP1", "TP53",
      "TP53BP1", "TRAF3IP2", "TFF1", "TEF", "TEK", "TERF1", "TET2", "TEX101", "TEX33", "TF",
      "TFAP2A", "TDP1", "TFF2", "TFF3", "TFPI", "TFPI2", "TFRC", "TG", "TGFA", "TDRKH",
      "TAX1BP1", "TCL1A", "TBC1D17", "TBC1D23", "TBC1D5", "TBCA", "TBCB", "TBCC", "TBL1X", "TBR1",
      "TDO2", "TCL1B", "TCN1", "TCN2", "TCOF1", "TCP11", "TCTN3", "TDGF1", "TGFB1", "TGFB2",
      "TJAP1", "TIMD4", "TIMM10", "TIMM8A", "TIMP1", "TIMP2", "TIMP3", "TIMP4", "TINAGL1", "TIGAR",
      "TJP3", "TK1", "TLR1", "TLR2", "TLR3", "TLR4", "TMCO5A", "TIGIT", "TGFBI", "THBS4",
      "TGFBR1", "TGFBR2", "TGFBR3", "TGM2", "TGOLN2", "THAP12", "THBD", "THBS2", "TIE1", "THOP1",
      "THPO", "THRAP3", "THSD1", "THTPA", "THY1", "TIA1", "TMED1", "VWA1", "VSIR", "VSNL1",
      "VSTM1", "VSTM2B", "VSTM2L", "VTA1", "VTCN1", "VTI1A", "VSIG2", "VWA5A", "VWC2", "VWC2L",
      "VWF", "WARS", "WAS", "WASF1", "VSIG4", "VCPKMT", "VMO1", "VEGFA", "VEGFB", "VEGFC",
      "VEGFD", "VGF", "VIM", "VIPR1", "VIT", "VSIG10L", "VNN1", "VNN2", "VPS28", "VPS37A",
      "VPS4B", "VPS53", "VSIG10", "WASF3", "WASHC3", "ZHX2", "YTHDF3", "YWHAQ", "YY1", "ZBP1",
      "ZBTB16", "ZBTB17", "ZCCHC8", "ZFYVE19", "YJU2", "ZNF174", "ZNF75D", "ZNF830", "ZNRD2", "ZNRF4",
      "ZP3", "ZP4", "YOD1", "WASL", "WWP2", "WDR46", "WFDC1", "WFDC12", "WFDC2", "WFIKKN1",
      "WFIKKN2", "WIF1", "WNT9A", "YES1", "XCL1", "XG", "XIAP", "XPNPEP2", "XRCC4", "YAP1",
      "YARS1", "ZPR1", "TUBB3", "TSPAN15", "TSPAN7", "TSPAN8", "TSPYL1", "TST", "TTF2", "TTN",
      "TTR", "TSNAX", "TWF2", "TXK", "TXLNA", "TXN", "TXNDC15", "TXNDC5", "TXNDC9", "TSPAN1",
      "TRDMT1", "TRIM26", "TREH", "TREM2", "TREML1", "TREML2", "TRIAP1", "TRIM21", "TRIM24", "TRIM25",
      "TSLP", "TRIM40", "TRIM5", "TRIM58", "TRPV3", "TSC1", "TSC22D1", "TSHB", "TXNL1", "TXNRD1",
      "VAMP5", "UROD", "UROS", "USO1", "USP25", "USP28", "USP47", "USP8", "UXS1", "UPK3A",
      "VAMP8", "VASH1", "VASN", "VASP", "VAT1", "VAV3", "VCAM1", "UPK3BL1", "TYMP", "UFD1",
      "TYRO3", "TYRP1", "UBAC1", "UBE2B", "UBE2L6", "UBE2Z", "UBQLN3", "UBXN1", "UPB1", "UGDH",
      "UHRF2", "ULBP2", "UMOD", "UNC5D", "UNC79", "UNG", "VCAN", "TARS1", "SFRP1", "SERPINI1",
      "SERPINI2", "SESTD1", "SETMAR", "SEZ6", "SEZ6L", "SEZ6L2", "SF3B4", "SERPING1", "SFRP4", "SFTPA1",
      "SFTPA2", "SFTPD", "SGSH", "SH2B3", "SH2D1A", "SERPINH1", "SERPINA11", "SERPINB6", "SERPINA3", "SERPINA4",
      "SERPINA5", "SERPINA6", "SERPINA7", "SERPINA9", "SERPINB1", "SERPINB5", "SERPINF2", "SERPINB8", "SERPINB9", "SERPINC1",
      "SERPIND1", "SERPINE1", "SERPINE2", "SERPINF1", "SH3BGRL2", "SH3BP1", "SLA2", "SIRPB1", "SIRT1", "SIRT2",
      "SIRT5", "SIT1", "SKAP1", "SKAP2", "SKIV2L", "SIL1", "SLAMF1", "SLAMF6", "SLAMF7", "SLAMF8",
      "SLC12A2", "SLC13A1", "SLC16A1", "SIRPA", "SH3GL3", "SIAE", "SH3GLB2", "SHBG", "SHC1", "SHD",
      "SHH", "SHISA5", "SHMT1", "SHPK", "SIGLEC9", "SIGLEC1", "SIGLEC10", "SIGLEC15", "SIGLEC5", "SIGLEC6",
      "SIGLEC7", "SIGLEC8", "SLC1A4", "SCARB1", "SARG", "SART1", "SAT1", "SAT2", "SATB1", "SBSN",
      "SCAMP3", "SCARA5", "SAMD9L", "SCARB2", "SCARF1", "SCARF2", "SCG2", "SCG3", "SCGB1A1", "SCGB2A2",
      "SAP18", "RTKN2", "S100A14", "RTN4IP1", "RTN4R", "RUVBL1", "RWDD1", "RYR1", "S100A11", "S100A12",
      "S100A13", "SAG", "S100A16", "S100A3", "S100A4", "S100G", "S100P", "SAA4", "SAFB2", "SCGB3A1",
      "SCGB3A2", "SEMA4C", "SEL1L", "SELE", "SELENOP", "SELL", "SELP", "SELPLG", "SEMA3F", "SEMA3G",
      "SDK2", "SEMA4D", "SEMA6C", "SEMA7A", "SEPTIN3", "SEPTIN7", "SEPTIN8", "SEPTIN9", "SEC31A", "SCGN",
      "SCPEP1", "SCIN", "SCLY", "SCN2A", "SCN2B", "SCN3A", "SCN3B", "SCN4B", "SCP2", "SDHB",
      "SCRG1", "SCRIB", "SCRN1", "SCT", "SDC1", "SDC4", "SDCCAG8", "SERPINA1", "STAU1", "ST3GAL1",
      "ST6GAL1", "ST8SIA1", "STAB2", "STAM", "STAMBP", "STAT2", "STAT5B", "SSNA1", "STC1", "STC2",
      "STEAP4", "STIP1", "STK11", "STK24", "STK4", "ST13", "SPON2", "SRC", "SPP1", "SPRED2",
      "SPRING1", "SPRR1B", "SPRR3", "SPRY2", "SPTBN2", "SPTLC1", "SSH3", "SRP14", "SRPK2", "SRPX",
      "SSB", "SSBP1", "SSC4D", "SSC5D", "STOML2", "STX16", "SV2A", "SWAP70", "SYAP1", "SYNGAP1",
      "SYT1", "SYTL4", "TAB2", "TACC3", "SUSD4", "TADA3", "TAFA5", "TAGLN3", "TALDO1", "TANK",
      "TAP1", "TARBP2", "SUSD5", "STX1B", "SUGP1", "STX3", "STX4", "STX5", "STX6", "STX7",
      "STX8", "STXBP1", "STXBP3", "SUSD2", "SUGT1", "SULT1A1", "SULT2A1", "SUMF1", "SUMF2",
      "SUOX", "SUSD1", "TARM1", "SMPD3", "SMAD3", "SMAD5", "SMARCA2", "SMC3", "SMNDC1", "SMOC1",
      "SMOC2", "SMPD1", "SMAD1", "SMPDL3A", "SMPDL3B", "SMS", "SMTN", "SNAP23", "SNAP25", "SNAP29",
      "SMAD2", "SLC27A4", "SLC9A3R2", "SLC28A1", "SLC34A3", "SLC39A14", "SLC39A5", "SLC44A4", "SLC4A1", "SLC51B",
      "SLC9A3R1", "SLURP1", "SLIRP", "SLIT2", "SLITRK1", "SLITRK2", "SLITRK6", "SLK", "SLMAP", "SNAPIN",
      "SNCA", "SPINK4", "SPACA5_SPACA5B", "SPAG1", "SPARC", "SPARCL1", "SPART", "SPESP1", "SPINK1", "SPINK2",
      "SOX2", "SPINK5", "SPINK6", "SPINK8", "SPINT1", "SPINT2", "SPINT3", "SPOCK1", "SOX9", "SNCG",
      "SOD1", "SNED1", "SNRPB2", "SNU13", "SNX15", "SNX18", "SNX2", "SNX5", "SNX9", "SOWAHA",
      "SOD2", "SOD3", "SORBS1", "SORCS2", "SORD", "SORT1", "SOST", "SPON1", "ELN", "EIF4E",
      "EIF4EBP1", "EIF4G1", "EIF4G3", "EIF5", "EIF5A", "ELAC1", "ELAVL4", "EIF2S2", "ELOA", "ELOB",
      "ENAH", "ENDOU", "ENG", "ENO1", "ENO2", "EIF4B", "EEF1D", "EGFL7", "EFCAB14", "EFCAB2",
      "EFEMP1", "EFHD1", "EFNA1", "EFNA4", "EFNB2", "EGF", "EIF2AK3", "EGFLAM", "EGFR", "EGLN1",
      "EHBP1", "EHD3", "EIF1AX", "EIF2AK2", "ENO3", "ENOPH1", "ERBB4", "EPHB6", "EPHX2", "EPN1",
      "EPO", "EPPK1", "EPS8L2", "ERBB2", "ERBB3", "EPHA4", "ERBIN", "ERC2", "ERCC1", "EREG",
      "ERI1", "ERMAP", "ERN1", "EPHB4", "ENOX2", "ENTPD6", "ENPEP", "ENPP2", "ENPP5", "ENPP6",
      "ENPP7", "ENSA", "ENTPD2", "ENTPD5", "EPHA2", "ENTR1", "EP300", "EPB41L5", "EPCAM", "EPGN",
      "EPHA1", "EPHA10", "ERP29", "DOK1", "DNLZ", "DNM1", "DNM3", "DNMBP", "DNPEP", "DNPH1",
      "DOC2B", "DOCK9", "DNAJC9", "DOK2", "DPEP1", "DPEP2", "DPP10", "DPP4", "DPP6", "DPP7",
      "DNER", "DKKL1", "DNAJA2", "DLG4", "DLGAP5", "DLK1", "DLL1", "DLL4", "DMD", "DMP1",
      "DNAJA1", "DNAJC6", "DNAJA4", "DNAJB1", "DNAJB14", "DNAJB2", "DNAJB6", "DNAJB8", "DNAJC21", "DPT",
      "DPY30", "ECSCR", "DYNLT3", "EBAG9", "EBI3_IL27", "ECE1", "ECHDC3", "ECHS1", "ECI2", "ECM1",
      "DYNC1H1", "EDA2R", "EDAR", "EDDM3B", "EDEM2", "EDF1", "EDIL3", "EDN1", "DYNLT1", "DRAXIN",
      "DTX2", "DRG2", "DSC2", "DSCAM", "DSG2", "DSG3", "DSG4", "DTD1", "DTNB", "DXO",
      "DTX3", "DTYMK", "DUOX2", "DUSP13", "DUSP29", "DUSP3", "DUT", "EDNRB", "FMNL1", "FKBP7",
      "FKBPL", "FLI1", "FLRT2", "FLT1", "FLT3", "FLT3LG", "FLT4", "FKBP4", "FMR1", "FN1",
      "FNDC1", "FNTA", "FOLH1", "FOLR1", "FOLR2", "FKBP5", "FGF3", "FGFR4", "FGF5", "FGF6",
      "FGF7", "FGF9", "FGFBP1", "FGFBP2", "FGFBP3", "FGFR2", "FKBP1B", "FGL1", "FGR", "FH",
      "FHIP2A", "FHIT", "FIS1", "FKBP14", "FOLR3", "FOS", "GAD2", "FXYD5", "FYB1", "FZD10",
      "FZD8", "GABARAP", "GABARAPL1", "GABRA4", "GAD1", "FUT8", "GADD45B", "GADD45GIP1", "GAGE2A", "GAL",
      "GALNT10", "GALNT2", "GALNT3", "FXN", "FOSB", "FSTL1", "FOXJ3", "FOXO1", "FOXO3", "FRMD4B",
      "FRMD7", "FRZB", "FSHB", "FST", "FUT3_FUT5", "FSTL3", "FTCD", "FUCA1", "FUOM", "FURIN",
      "FUS", "FUT1", "GALNT5", "FABP6", "F3", "F7", "F9", "FABP1", "FABP2", "FABP3",
      "FABP4", "FABP5", "F2", "FABP9", "FADD", "FAM13A", "FAM171A2", "FAM171B", "FAM172A", "FAM20A",
      "F2R", "ERP44", "EVPL", "ERVV.1", "ESAM", "ESM1", "ESPL1", "ESR1", "ESYT2", "EVI2B",
      "EVI5", "F13B", "EXOSC10", "EXTL1", "EZR", "F10", "F11", "F11R", "F12", "FAM3B",
      "FAM3C", "FGA", "FCRL5", "FCRL6", "FCRLB", "FDX1", "FDX2", "FEN1", "FES", "FETUB",
      "FCRL2", "FGD3", "FGF12", "FGF16", "FGF19", "FGF2", "FGF20", "FGF21", "FCRL3", "FAM3D",
      "FCAR", "FAP", "FARSA", "FAS", "FASLG", "FBLN2", "FBN2", "FBP1", "FCAMR", "FCRL1",
      "FCER1A", "FCER2", "FCGR2A", "FCGR2B", "FCGR3B", "FCN1", "FCN2", "FGF23", "DKK4", "CNTF",
      "CMC1", "CMIP", "CNDP1", "CNGB3", "CNP", "CNPY2", "CNPY4", "CNST", "CLU", "CNTN1",
      "CNTN2", "CNTN3", "CNTN4", "CNTN5", "CNTNAP2", "CNTNAP4", "CLUL1", "CLEC4G", "CLMP", "CLEC4M",
      "CLEC5A", "CLEC6A", "CLEC7A", "CLGN", "CLIC5", "CLINT1", "CLIP2", "CLTA", "CLNS1A", "CLPP",
      "CLPS", "CLSPN", "CLSTN1", "CLSTN2", "CLSTN3", "COCH", "COL15A1", "CPB2", "CORO1A", "CORO6",
      "COX5B", "COX6B1", "CPA1", "CPA2", "CPA4", "CPB1", "COPE", "CPE", "CPLX2", "CPM",
      "CPOX", "CPPED1", "CPQ", "CPTP", "COQ7", "COL18A1", "COL6A3", "COL1A1", "COL24A1", "COL28A1",
      "COL2A1", "COL3A1", "COL4A1", "COL4A4", "COL5A1", "COPB2", "COL9A1", "COL9A2", "COLEC12", "COMMD1",
      "COMMD9", "COMP", "COMT", "CPVL", "CFHR2", "CES2", "CES3", "CETN2", "CETN3", "CFB",
      "CFC1", "CFD", "CFH", "CERT", "CFHR4", "CFHR5", "CFI", "CFP", "CGA", "CGB3_CGB5_CGB8",
      "CGN", "CES1", "CEBPA", "CEP112", "CEBPB", "CELA2A", "CELA3A", "CELSR2", "CEMIP2", "CEND1",
      "CENPF", "CENPJ", "CEP85", "CEP152", "CEP164", "CEP170", "CEP20", "CEP290", "CEP350", "CEP43",
      "CGREF1", "CHAC2", "CLEC12A", "CIT", "CKAP4", "CKB", "CKMT1A_CKMT1B", "CLASP1", "CLC", "CLEC10A",
      "CLEC11A", "CINP", "CLEC14A", "CLEC1A", "CLEC1B", "CLEC2L", "CLEC3B", "CLEC4A", "CLEC4C", "CIRBP",
      "CHAD", "CHM", "CHCHD10", "CHCHD6", "CHEK2", "CHGA", "CHGB", "CHI3L1", "CHIT1", "CHL1",
      "CILP", "CHMP1A", "CHMP6", "CHP1", "CHRDL1", "CHRDL2", "CHRM1", "CIAPIN1", "CLEC4D", "DCBLD2",
      "DAND5", "DAPK2", "DAPP1", "DARS1", "DBH", "DBI", "DBN1", "DBNL", "DAB2", "DCC",
      "DCDC2C", "DCLRE1C", "DCN", "DCTD", "DCTN1", "DCTN2", "DAG1", "CXCL10", "CXCL6", "CXCL11",
      "CXCL12", "CXCL13", "CXCL14", "CXCL16", "CXCL17", "CXCL3", "CXCL5", "DAAM1", "CXCL8", "CXCL9",
      "CYB5A", "CYB5R2", "CYP24A1", "CYTH3", "CYTL1", "DCTN6", "DCTPP1", "DGKZ", "DEFB116", "DEFB118",
      "DEFB4A_DEFB4B", "DENND2B", "DENR", "DFFA", "DGCR6", "DGKA", "DEFB103A_DEFB103B", "DHODH", "DHPS", "DHRS4L2",
      "DIABLO", "DIPK1C", "DIPK2B", "DKK1", "DEFB104A_DEFB104B", "DCUN1D1", "DDT", "DCUN1D2", "DCXR", "DDA1",
      "DDAH1", "DDC", "DDHD2", "DDI2", "DDR1", "DEFA1_DEFA1B", "DDX1", "DDX25", "DDX39A", "DDX4",
      "DDX53", "DDX58", "DECR1", "DKK3", "CRYM", "CRNN", "CRTAC1", "CRTAM", "CRTAP", "CRX",
      "CRYBB1", "CRYBB2", "CRYGD", "CRKL", "CRYZL1", "CSDE1", "CSF1", "CSF1R", "CSF2", "CSF2RA",
      "CSF2RB", "CRLF1", "CPXM1", "CRELD1", "CPXM2", "CR1", "CR2", "CRACR2A", "CRADD", "CREB3",
      "CREBZF", "CREG1", "CRISP3", "CRELD2", "CRH", "CRHBP", "CRHR1", "CRIM1", "CRIP2", "CRISP2",
      "CSF3", "CSF3R", "CTSO", "CTRL", "CTSB", "CTSC", "CTSD", "CTSE", "CTSF", "CTSH",
      "CTSL", "CTRB1", "CTSV", "CTSZ", "CUZD1", "CWC15", "CX3CL1", "CXADR", "CTRC", "CSH1",
      "CST6", "CSNK1D", "CSNK2A1", "CSPG4", "CSPG5", "CSRP3", "CST3", "CST5", "CTNNA1", "CST7",
      "CSTB", "CTAG1A_CTAG1B", "CTBS", "CTF1", "CTHRC1", "CTLA4", "CXCL1", "GALNT7", "KLK8", "KLK12",
      "KLK13", "KLK14", "KLK15", "KLK3", "KLK4", "KLK6", "KLK7", "KLK10", "KLKB1", "KLRB1",
      "KLRC1", "KLRD1", "KLRF1", "KLRK1", "KRT14", "KLK11", "KIAA2013", "KIR3DL2", "KIF1C", "KIF20B",
      "KIF22", "KIFBP", "KIR2DL2", "KIR2DL3", "KIR2DS4", "KIR3DL1", "KLK1", "KIRREL1", "KIRREL2", "KIT",
      "KITLG", "KLB", "KLF4", "KLHL41", "KRT17", "KRT18", "LCAT", "LAP3", "LARP1", "LAT",
      "LAT2", "LATS1", "LAYN", "LBP", "LBR", "LAMP3", "LCN15", "LCN2", "LCP1", "LDLR",
      "LDLRAP1", "LECT2", "LEFTY2", "LAMTOR5", "KRT19", "LACTB2", "KRT5", "KRT6C", "KRT8", "KYAT1",
      "KYNU", "L1CAM", "L3HYPDH", "LACRT", "LAMP2", "LAG3", "LAIR1", "LAIR2", "LAMA1", "LAMA4",
      "LAMB1", "LAMP1", "LEG1", "IST1", "IPCEF1", "IQGAP2", "IRAG2", "IRAK1", "IRAK4", "ISLR2",
      "ISM1", "ISM2", "INSL5", "ITGA11", "ITGA2", "ITGA5", "ITGA6", "ITGAL", "ITGAM", "ITGAV",
      "INSR", "IL6ST", "ING1", "IL7", "IL7R", "IL9", "ILKAP", "IMMT", "IMPA1", "IMPACT",
      "IMPG1", "INSL4", "INHBB", "INHBC", "INPP1", "INPP5D", "INPP5J", "INPPL1", "INSL3", "ITGAX",
      "ITGB1", "KCTD5", "JMJD1C", "JPT2", "JUN", "KAZALD1", "KAZN", "KCNC4", "KCNH2", "KCNIP4",
      "JAM3", "KDM3A", "KDR", "KEL", "KHDC3L", "KHK", "KIAA0319", "KIAA1549", "JCHAIN", "ITGB1BP1",
      "ITIH4", "ITGB1BP2", "ITGB2", "ITGB5", "ITGB6", "ITGB7", "ITGBL1", "ITIH1", "ITIH3", "JAM2",
      "ITIH5", "ITM2A", "ITPA", "ITPR1", "ITPRIP", "IVD", "IZUMO1", "KIAA1549L", "MAPK9", "MAP1LC3A",
      "MAP1LC3B2", "MAP2", "MAP2K1", "MAP2K6", "MAP3K5", "MAP4K5", "MAPK13", "MANSC1", "MAPKAPK2", "MAPRE3",
      "MAPT", "MARCO", "MARS1", "MASP1", "MATN2", "MANSC4", "LYPD8", "MAG", "LYPLA2", "LYSMD3",
      "LYVE1", "LYZL2", "LZTFL1", "M6PR", "MAD1L1", "MAEA", "MANF", "MAGEA3", "MAGED1", "MAMDC2",
      "MAMDC4", "MAN1A2", "MAN2B2", "MANEAL", "MATN3", "MAVS", "METAP2", "MEP1A", "MEP1B", "MEPE",
      "MERTK", "MESD", "MET", "METAP1", "METAP1D", "MELTF", "MFAP3", "MFAP3L", "MFAP4", "MFAP5",
      "MFGE8", "MGLL", "MGMT", "MENT", "MAX", "MDH1", "MB", "MBL2", "MCAM", "MCEE",
      "MCEMP1", "MCFD2", "MCTS1", "MDGA1", "MEGF9", "MDK", "MDM1", "MECR", "MED18", "MED21",
      "MEGF10", "MEGF11", "MIA", "LMNB2", "LILRA5", "LILRA6", "LILRB1", "LILRB2", "LILRB4", "LILRB5",
      "LIPF", "LMNB1", "LILRA3", "LMOD1", "LMOD2", "LONP1", "LPA", "LPCAT2", "LPL", "LPO",
      "LILRA4", "LELP1", "LGALS7_LGALS7B", "LEO1", "LEP", "LEPR", "LETM1", "LGALS1", "LGALS3", "LGALS3BP",
      "LGALS4", "LILRA2", "LGALS8", "LGALS9", "LGMN", "LHB", "LHPP", "LIF", "LIFR", "LPP",
      "LRCH4", "LXN", "LTA", "LTA4H", "LTB", "LTBP2", "LTBP3", "LTBR", "LTO1", "LUZP2",
      "LSM8", "LY6D", "LY75", "LY9", "LY96", "LYAR", "LYN", "LYPD1", "LSP1", "LRFN2",
      "LRRC25", "LRG1", "LRIG1", "LRIG3", "LRP1", "LRP11", "LRP2", "LRP2BP", "LRPAP1", "LSM1",
      "LRRC37A2", "LRRC38", "LRRC59", "LRRFIP1", "LRRN1", "LRTM1", "LRTM2", "LYPD3", "IL6R", "GRPEL1",
      "GPRC5C", "GRAP2", "GRHPR", "GRIK2", "GRIN2B", "GRK5", "GRN", "GRP", "GPR15L", "GRSF1",
      "GSAP", "GSN", "GSR", "GSTA1", "GSTA3", "GSTM4", "GPR37", "GOPC", "GPC1", "GORASP2",
      "GOT1", "GP1BA", "GP1BB", "GP2", "GP5", "GP6", "GPA33", "GPR158", "GPC5", "GPD1",
      "GPHA2", "GPI", "GPIHBP1", "GPKOW", "GPR101", "GSTP1", "GSTT2B", "HEG1", "HCG22", "HCLS1",
      "HDAC8", "HDAC9", "HDDC2", "HDGF", "HDGFL2", "HEBP1", "HBQ1", "HEPACAM2", "HEPH", "HEXIM1",
      "HGF", "HGFAC", "HGS", "HHEX", "HBZ", "GTF2IRD1", "GZMH", "GTPBP2", "GUCA2A", "GUCY2C",
      "GUK1", "GUSB", "GYS1", "GZMA", "GZMB", "HBEGF", "H2AP", "HADH", "HAGH", "HAO1",
      "HARS1", "HAVCR1", "HAVCR2", "HIF1A", "GFRA2", "GDF15", "GDF2", "GDNF", "GET3", "GFAP",
      "GFER", "GFOD2", "GFRA1", "GCLM", "GFRA3", "GFRAL", "GGA1", "GGACT", "GGCT", "GGH",
      "GGT1", "GCNT1", "GAMT", "GBA", "GAPDH", "GART", "GAS2", "GAS6", "GASK1A", "GAST",
      "GATA3", "GATD3", "GCHFR", "GBP1", "GBP2", "GBP4", "GBP6", "GC", "GCC1", "GCG",
      "GGT5", "GH1", "GMPR2", "GLP1R", "GLRX", "GLRX5", "GLT8D2", "GLYR1", "GM2A", "GMFG",
      "GMPR", "GLO1", "GNAS", "GNE", "GNGT1", "GNLY", "GNPDA1", "GNPDA2", "GOLGA3", "GLOD4",
      "GH2", "GIPC2", "GHR", "GHRHR", "GHRL", "GID8", "GIGYF2", "GIMAP7", "GIMAP8", "GIP",
      "GLI2", "GIPC3", "GIPR", "GIT1", "GJA8", "GKN1", "GLA", "GLB1", "GOLM2", "IL15RA",
      "IL12A_IL12B", "IL12B", "IL12RB1", "IL12RB2", "IL13", "IL13RA1", "IL13RA2", "IL15", "IL10RB", "IL16",
      "IL17A", "IL17C", "IL17D", "IL17F", "IL17RA", "IL17RB", "IL11", "IGFBP2", "IGLON5", "IGFBP3",
      "IGFBP4", "IGFBP6", "IGFBP7", "IGFBPL1", "IGFL4", "IGHMBP2", "IGLC2", "IL10RA", "IGSF21", "IGSF3",
      "IGSF8", "IGSF9", "IKBKG", "IKZF2", "IL10", "IL18", "IL18BP", "IL34", "IL2RA", "IL2RB",
      "IL2RG", "IL3", "IL31", "IL31RA", "IL32", "IL33", "IL24", "IL36A", "IL36G", "IL3RA",
      "IL4", "IL4R", "IL5", "IL5RA", "IL25", "IL18R1", "IL1RL2", "IL18RAP", "IL19", "IL1A",
      "IL1B", "IL1R1", "IL1R2", "IL1RAP", "IL1RL1", "IL22RA1", "IL1RN", "IL2", "IL20", "IL20RA",
      "IL20RB", "IL21R", "IL22", "IL6", "HSBP1", "HPSE", "HRAS", "HRC", "HRG", "HS1BP3",
      "HS3ST3B1", "HS6ST1", "HS6ST2", "HPCAL1", "HSD11B1", "HSD17B14", "HSD17B3", "HSDL2", "HSP90B1", "HSPA1A",
      "HSPA2", "HPGDS", "HIP1", "HMGCL", "HIP1R", "HJV", "HK2", "HLA.A", "HLA.DRA", "HLA.E",
      "HMBS", "HMCN2", "HNRNPUL1", "HMGCS1", "HMMR", "HMOX1", "HMOX2", "HNF1A", "HNMT", "HNRNPK",
      "HSPB1", "HSPB6", "IFNW1", "IFIT3", "IFNAR1", "IFNG", "IFNGR1", "IFNGR2", "IFNL1", "IFNL2",
      "IFNLR1", "IFI30", "IFT20", "IGBP1", "IGDCC3", "IGDCC4", "IGF1R", "IGF2BP3", "IGF2R", "IFIT1",
      "HSPG2", "ICAM3", "HTR1A", "HTR1B", "HTRA2", "HYAL1", "HYOU1", "ICA1", "ICAM1", "ICAM2",
      "IDUA", "ICAM4", "ICAM5", "ICOSLG", "ID4", "IDI2", "IDO1", "IDS", "IGFBP1", "AK2",
      "ALDH1A1", "ALCAM", "AKT3", "AKT2", "AKT1S1", "AKR7L", "AKR1C4", "AKR1B10", "AKR1B1", "AKAP12",
      "AHCY", "AK1", "AIFM1", "AIF1L", "AIF1", "AIDA", "AHSP", "AHSG", "AHSA1", "AHNAK2",
      "AHNAK", "ALDH2", "ALDH3A1", "AMOT", "ANGPTL2", "ANGPTL1", "ANGPT2", "ANGPT1", "ANG", "AMY2A",
      "AMY1A_AMY1B_AMY1C", "AMPD3", "AMOTL2", "ALDH5A1", "AMN", "AMIGO2", "AMIGO1", "AMFR", "AMDHD2", "AMBP",
      "AMBN", "ALPP", "ALPI", "ALMS1", "ANGPTL3", "ADAMTS1", "ADD1", "ADCYAP1R1", "ADAMTSL5", "ADAMTSL4",
      "ADAMTSL2", "ADAMTS8", "ADAMTS4", "ADAMTS16", "ADAMTS15", "ADAMTS13", "ACY1", "ADAM9", "ADAM8", "ADAM23",
      "ADAM22", "ADAM15", "ADAM12", "ADA2", "ADA", "ACYP1", "ACY3", "ADGRB3", "ADGRD1", "ADRA2A",
      "AGT", "AGRP", "AGRN", "AGR3", "AGR2", "AGER", "AGBL2", "AFP", "AFM", "AFAP1",
      "ADGRE1", "ADM", "ADIPOQ", "ADH4", "ADH1B", "ADGRV1", "ADGRG2", "ADGRG1", "ADGRF5", "ADGRE5",
      "ADGRE2", "AGXT", "ANGPTL4", "ACTA2", "ACP1", "ACP5", "ACP6", "ACRBP", "ACRV1", "ACSL1",
      "ACOT13", "ACTN2", "ACTN4", "ACVRL1", "APOL1", "APOH", "APOF", "ACOX1", "A1BG", "ABRAXAS2",
      "AAMDC", "AARSD1", "ABCA2", "ABHD14B", "ABL1", "ABO", "ACHE", "ACAA1", "ACADM", "ACADSB",
      "ACAN", "ACE", "ACE2", "APOE", "ANXA1", "ANXA5", "ANXA4", "ANXA3", "ANXA2", "ANXA11",
      "ANXA10", "ANGPTL7", "ANPEP", "ANP32C", "ANKRD54", "ANKRA2", "ANKMY2", "ANK2", "AOC1", "AOC3",
      "APOA4", "APOD", "APOC1", "APOBR", "APOB", "AP1G2", "APOA2", "APOA1", "AP2B1", "APLP1",
      "AP3B1", "AP3S2", "APBB1IP", "APCS", "APEX1", "SERPINA12", "GPNMB"
    )
  )
  
  # 获取所有定义的表型作为结局变量
  all_outcomes <- unlist(biomarker_categories, use.names = FALSE)
  
  # 只保留在数据中存在的变量
  available_outcomes <- all_outcomes[all_outcomes %in% names(data)]
  cat("定义的表型总数:", length(all_outcomes), "\n")
  cat("数据中可用的表型数:", length(available_outcomes), "\n")
  
  # 初始化结果列表
  all_results <- list()
  
  # 对每个暴露变量进行分析
  for (exposure in exposures) {
    cat("分析暴露变量:", exposure, "\n")
    
    exposure_results <- data.frame()
    
    # 对每个表型进行回归分析
    for (i in seq_along(available_outcomes)) {
      outcome <- available_outcomes[i]
      
      if (i %% 50 == 0) {
        cat("  已处理", i, "/", length(available_outcomes), "个表型\n")
      }
      
      # 检查变量是否存在
      required_vars <- c("eid", exposure, outcome, covariates)
      if (!all(required_vars %in% names(data))) {
        next
      }
      
      # 创建分析数据集
      analysis_data <- data[, required_vars, drop = FALSE]
      analysis_data <- analysis_data[complete.cases(analysis_data), ]
      
      if (nrow(analysis_data) < 100) {
        next  # 样本量太小
      }
      
      # 检查outcome变量是否有变异
      if (is.numeric(analysis_data[[outcome]])) {
        if (var(analysis_data[[outcome]], na.rm = TRUE) == 0) {
          next  # 无变异
        }
      }
      
      # 构建回归公式
      formula_str <- paste(outcome, "~", exposure, "+", paste(covariates, collapse = " + "))
      
      tryCatch({
        # 拟合模型
        model <- lm(as.formula(formula_str), data = analysis_data)
        
        # 提取结果
        model_summary <- tidy(model)
        
        # 查找暴露变量的系数
        exposure_rows <- model_summary[grepl(paste0("^", exposure), model_summary$term), ]
        
        if (nrow(exposure_rows) > 0) {
          # 如果是分类变量，取第一个非参考类别的结果
          exposure_row <- exposure_rows[1, ]
          
          # 确定表型类别
          phenotype_category <- "Other"
          for (cat_name in names(biomarker_categories)) {
            if (outcome %in% biomarker_categories[[cat_name]]) {
              phenotype_category <- cat_name
              break
            }
          }
          
          result_row <- data.frame(
            exposure = exposure,
            outcome = outcome,
            category = phenotype_category,
            n_samples = nrow(analysis_data),
            beta = exposure_row$estimate,
            se = exposure_row$std.error,
            p_value = exposure_row$p.value,
            t_statistic = exposure_row$statistic,
            ci_lower = exposure_row$estimate - 1.96 * exposure_row$std.error,
            ci_upper = exposure_row$estimate + 1.96 * exposure_row$std.error,
            term = exposure_row$term,
            stringsAsFactors = FALSE
          )
          
          exposure_results <- rbind(exposure_results, result_row)
        }
        
      }, error = function(e) {
        # 静默处理错误
      })
    }
    
    # 多重检验校正
    if (nrow(exposure_results) > 0) {
      exposure_results$p_bonferroni <- p.adjust(exposure_results$p_value, method = "bonferroni")
      exposure_results$p_fdr <- p.adjust(exposure_results$p_value, method = "fdr")
      exposure_results$significant_bonferroni <- exposure_results$p_bonferroni < 0.05
      exposure_results$significant_fdr <- exposure_results$p_fdr < 0.05
      
      all_results[[exposure]] <- exposure_results
      cat("完成", exposure, "分析，共", nrow(exposure_results), "个有效结果\n")
    }
  }
  
  return(all_results)
}

# 创建综合曼哈顿图 - 三个暴露在同一图上，用不同形状区分
create_combined_manhattan_plot <- function(results, p_threshold = 0.05) {
  
  # 合并所有结果
  combined_data <- do.call(rbind, results)
  
  if (is.null(combined_data) || nrow(combined_data) == 0) {
    cat("没有结果数据\n")
    return(NULL)
  }
  
  # 计算-log10(p值)
  combined_data$neg_log_p <- -log10(combined_data$p_value)
  combined_data$significant <- combined_data$p_fdr < p_threshold
  
  # 按类别排序表型，便于x轴显示
  category_order <- c("Biochemical", "Pulmonary", "Inflammatory", "Cardiac", 
                      "Cognitive", "Body_Composition", "NMR_Metabolomics", 
                      "Brain_Structure", "Olink_Proteomics", "Other")
  
  # 重新排序数据
  combined_data$category <- factor(combined_data$category, levels = category_order)
  combined_data <- combined_data[order(combined_data$category, combined_data$outcome), ]
  
  # 为每个表型添加x轴位置
  unique_outcomes <- unique(combined_data$outcome)
  combined_data$x_pos <- match(combined_data$outcome, unique_outcomes)
  
  # 按类别着色
  category_colors <- c(
    "Biochemical" = "#1f77b4",
    "Pulmonary" = "#ff7f0e",
    "Inflammatory" = "#2ca02c", 
    "Cardiac" = "#d62728",
    "Cognitive" = "#9467bd",
    "Body_Composition" = "#8c564b",
    "NMR_Metabolomics" = "#e377c2",
    "Brain_Structure" = "#7f7f7f",
    "Olink_Proteomics" = "#ff69b4",
    "Other" = "#bcbd22"
  )
  
  # 暴露变量的形状映射
  exposure_shapes <- c(
    "coffee_tea_preference" = 16,  # 圆形
    "NJ_water_preference" = 15,   # 正方形
    "SSB_ASB_preference" = 17     # 三角形
  )
  
  # 暴露变量的标签映射
  exposure_labels <- c(
    "coffee_tea_preference" = "Coffee/Tea",
    "NJ_water_preference" = "Juice/Water", 
    "SSB_ASB_preference" = "SSB/ASB"
  )
  
  # 为显著性调整点的大小
  combined_data$point_size <- ifelse(combined_data$significant, 2.5, 1.5)
  
  # 计算FDR阈值对应的-log10(p值) - 红色线
  fdr_significant_p <- combined_data$p_value[combined_data$p_fdr < p_threshold]
  if(length(fdr_significant_p) > 0) {
    fdr_threshold_p <- max(fdr_significant_p, na.rm = TRUE)
    fdr_threshold_neg_log_p <- -log10(fdr_threshold_p)
  } else {
    # 如果没有FDR显著的结果，使用理论FDR阈值
    fdr_threshold_neg_log_p <- -log10(p_threshold)
  }
  
  # 计算Bonferroni校正阈值 - 蓝色线
  total_tests <- nrow(combined_data)
  bonferroni_threshold_p <- 0.05 / total_tests
  bonferroni_threshold_neg_log_p <- -log10(bonferroni_threshold_p)
  
  # 创建分段的y轴刻度 - 增加30以上的压缩
  max_neg_log_p <- max(combined_data$neg_log_p, na.rm = TRUE)
  min_neg_log_p <- min(combined_data$neg_log_p, na.rm = TRUE)
  
  # 定义多段y轴转换
  if(max_neg_log_p > 30) {
    # 0-5: 正常比例
    # 5-30: 压缩到0.5倍
    # 30+: 压缩到0.2倍
    breaks_low <- seq(0, 5, by = 1)
    breaks_mid <- seq(10, 30, by = 5)
    breaks_high <- seq(40, ceiling(max_neg_log_p), by = 20)
    y_breaks <- c(breaks_low, breaks_mid, breaks_high)
    
    # 创建转换函数
    transform_y <- function(y) {
      ifelse(y <= 5, y,
             ifelse(y <= 30, 5 + (y - 5) * 0.5,
                    5 + 25 * 0.5 + (y - 30) * 0.2))
    }
    
    # 应用转换
    combined_data$neg_log_p_trans <- transform_y(combined_data$neg_log_p)
    y_breaks_trans <- transform_y(y_breaks)
    
    # 转换阈值线位置
    red_line_trans <- transform_y(fdr_threshold_neg_log_p)
    blue_line_trans <- transform_y(bonferroni_threshold_neg_log_p)
    
  } else if(max_neg_log_p > 10) {
    # 如果最大值中等，使用原来的单段压缩
    breaks_low <- seq(0, 5, by = 1)
    breaks_high <- seq(10, ceiling(max_neg_log_p), by = 10)
    y_breaks <- c(breaks_low, breaks_high)
    
    # 创建转换函数，在5-10之间压缩
    transform_y <- function(y) {
      ifelse(y <= 5, y, 5 + (y - 5) * 0.3)
    }
    
    # 应用转换
    combined_data$neg_log_p_trans <- transform_y(combined_data$neg_log_p)
    y_breaks_trans <- transform_y(y_breaks)
    
    # 转换阈值线位置
    red_line_trans <- transform_y(fdr_threshold_neg_log_p)
    blue_line_trans <- transform_y(bonferroni_threshold_neg_log_p)
    
  } else {
    # 如果数据范围不大，使用正常显示
    combined_data$neg_log_p_trans <- combined_data$neg_log_p
    y_breaks <- seq(0, ceiling(max_neg_log_p), by = 1)
    y_breaks_trans <- y_breaks
    red_line_trans <- fdr_threshold_neg_log_p
    blue_line_trans <- bonferroni_threshold_neg_log_p
  }
  
  # 计算每个类别的x轴范围，用于添加类别标签
  category_ranges <- combined_data %>%
    group_by(category) %>%
    summarise(
      start = min(x_pos, na.rm = TRUE),
      end = max(x_pos, na.rm = TRUE),
      mid = (min(x_pos, na.rm = TRUE) + max(x_pos, na.rm = TRUE)) / 2,
      .groups = 'drop'
    ) %>%
    filter(!is.na(category))
  
  # 创建曼哈顿图
  p <- ggplot(combined_data, aes(x = x_pos, y = neg_log_p_trans, 
                                 color = category, 
                                 shape = exposure,
                                 size = significant)) +
    geom_point(alpha = 0.7) +
    
    # 添加类别分隔线
    {
      if(nrow(category_ranges) > 1) {
        geom_vline(xintercept = category_ranges$end[-nrow(category_ranges)] + 0.5, 
                   color = "lightgray", alpha = 0.5, linetype = "solid", size = 0.3)
      }
    } +
    
    # 添加FDR阈值线 (FDR < 0.05) - 红色线
    geom_hline(yintercept = red_line_trans, linetype = "dashed", 
               color = "red", alpha = 0.6, size = 0.8) +
    
    # 添加Bonferroni校正阈值线 - 蓝色线
    geom_hline(yintercept = blue_line_trans, linetype = "dashed", 
               color = "blue", alpha = 0.6, size = 0.8) +
    
    # 颜色映射
    scale_color_manual(values = category_colors, name = "Phenotype Category") +
    
    # 形状映射
    scale_shape_manual(values = exposure_shapes, 
                       labels = exposure_labels,
                       name = "Beverage preference") +
    
    # 大小映射
    scale_size_manual(values = c("TRUE" = 2.5, "FALSE" = 1.5), 
                      labels = c("FALSE" = "Non-significant", "TRUE" = "FDR < 0.05"),
                      name = "Significance") +
    
    # 自定义y轴
    scale_y_continuous(
      breaks = y_breaks_trans,
      labels = y_breaks,
      name = "-log₁₀(P-value)"
    ) +
    
    # 自定义x轴 - 显示类别范围
    scale_x_continuous(
      breaks = category_ranges$mid,
      labels = gsub("_", " ", category_ranges$category),
      name = "Phenotype Categories"
    ) +
    
    # 坐标轴和标题
    labs(
      title = "PheWAS Manhattan Plot: Beverage Preferences vs Phenotypes",
      subtitle = paste("Red line: FDR < 0.05; Blue line: Bonferroni correction (p <", 
                       format(bonferroni_threshold_p, scientific = TRUE, digits = 2), ")")
    ) +
    
    # 主题设置
    theme_minimal() +
    theme(
      # x轴标签设置
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.ticks.x = element_line(color = "gray50"),
      
      # 调整图例
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 10),
      
      # 调整标题
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      
      # 坐标轴标题
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      
      # 图例标题和文字
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      
      # 背景
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    
    # 调整图例布局
    guides(
      color = guide_legend(override.aes = list(size = 3), 
                           title.position = "top", 
                           nrow = 2),
      shape = guide_legend(override.aes = list(size = 3),
                           title.position = "top"),
      size = guide_legend(title.position = "top")
    )
  
  # 添加压缩指示文本
  if(max_neg_log_p > 30) {
    p <- p + 
      annotate("text", x = max(combined_data$x_pos) * 0.98, 
               y = max(combined_data$neg_log_p_trans) * 0.95, 
               label = "Y-axis compressed:\n5-30 (0.5×)\n30+ (0.2×)", 
               size = 2.5, color = "gray50", hjust = 1, vjust = 1)
  } else if(max_neg_log_p > 10) {
    p <- p + 
      annotate("text", x = max(combined_data$x_pos) * 0.98, 
               y = max(combined_data$neg_log_p_trans) * 0.95, 
               label = "Y-axis compressed\nbetween 5-10 (0.3×)", 
               size = 2.5, color = "gray50", hjust = 1, vjust = 1)
  }
  
  # 添加类别计数信息
  category_counts <- combined_data %>%
    group_by(category) %>%
    summarise(n = n_distinct(outcome), .groups = 'drop')
  
  count_text <- paste(category_counts$category, " (n=", category_counts$n, ")", sep = "", collapse = "; ")
  
  p <- p + 
    annotate("text", x = min(combined_data$x_pos), 
             y = max(combined_data$neg_log_p_trans) * 0.02, 
             label = paste("Phenotype counts:", count_text), 
             size = 2, color = "gray60", hjust = 0, vjust = 0)
  
  # 添加阈值信息
  p <- p + 
    annotate("text", x = min(combined_data$x_pos), 
             y = max(combined_data$neg_log_p_trans) * 0.08, 
             label = paste("FDR threshold: -log10(p) =", round(fdr_threshold_neg_log_p, 2),
                           "; Bonferroni threshold: -log10(p) =", round(bonferroni_threshold_neg_log_p, 2)), 
             size = 2, color = "gray60", hjust = 0, vjust = 0)
  
  return(p)
}


# 生成结果摘要表
create_summary_table <- function(results, p_threshold = 0.05) {
  
  summary_list <- list()
  
  for (exposure in names(results)) {
    result_data <- results[[exposure]]
    
    if (nrow(result_data) == 0) next
    
    # 显著性结果
    significant_fdr <- sum(result_data$p_fdr < p_threshold, na.rm = TRUE)
    significant_bonf <- sum(result_data$p_bonferroni < p_threshold, na.rm = TRUE)
    
    # 按类别统计
    category_summary <- result_data %>%
      group_by(category) %>%
      summarise(
        n_total = n(),
        n_significant_fdr = sum(p_fdr < p_threshold, na.rm = TRUE),
        .groups = 'drop'
      )
    
    summary_list[[exposure]] <- list(
      total_phenotypes = nrow(result_data),
      significant_fdr = significant_fdr,
      significant_bonferroni = significant_bonf,
      category_breakdown = category_summary
    )
  }
  
  return(summary_list)
}

# 主分析代码
# 定义变量
exposures <- c("coffee_tea_preference", "SSB_ASB_preference", "NJ_water_preference")

covariates <- c("age", "sex", "ethnic", "education_years", "household_income", 
                "bmi", "smoking_status", "alcohol_intake_frequency", 
                "diet_score", "PA_mod_vig_150", "overall_health_rating", 
                "smk_num", "smk_qyr")

# 执行PheWAS分析
cat("开始严格定义的PheWAS分析（包含Olink蛋白质组学）...\n")

phewas_results <- perform_phewas_strict(
  data = phewas_data,
  exposures = exposures,
  covariates = covariates
)

# 生成结果摘要
summary_results <- create_summary_table(phewas_results)

# 打印摘要
for (exposure in names(summary_results)) {
  cat("\n=== ", exposure, " ===\n")
  cat("总表型数:", summary_results[[exposure]]$total_phenotypes, "\n")
  cat("FDR显著 (p<0.05):", summary_results[[exposure]]$significant_fdr, "\n")
  cat("Bonferroni显著 (p<0.05):", summary_results[[exposure]]$significant_bonferroni, "\n")
  
  print(summary_results[[exposure]]$category_breakdown)
}

# 创建综合曼哈顿图
combined_manhattan <- create_combined_manhattan_plot(phewas_results)

# 显示图形
if (!is.null(combined_manhattan)) {
  print(combined_manhattan)
  
  # 保存图形
  ggsave("phewas_combined_manhattan_plot.pdf", combined_manhattan, 
         width = 10, height = 8)
}

# 保存结果
if (length(phewas_results) > 0) {
  all_results_combined <- do.call(rbind, phewas_results)
  
  # 保存到CSV文件
  fwrite(all_results_combined, "phewas_results.csv")
  
  # 保存显著结果
  significant_results <- all_results_combined[all_results_combined$p_fdr < 0.05, ]
  if (nrow(significant_results) > 0) {
    fwrite(significant_results, "phewas_significant_results.csv")
    cat("显著结果数:", nrow(significant_results), "\n")
    
    # 按暴露变量和类别统计显著结果
    sig_summary <- significant_results %>%
      group_by(exposure, category) %>%
      summarise(
        n_significant = n(),
        min_p = min(p_value),
        .groups = 'drop'
      ) %>%
      arrange(exposure, min_p)
    
    print("显著结果按类别统计:")
    print(sig_summary)
  }
  
  cat("\n分析完成！\n")
  cat("所有结果已保存到: phewas_results.csv\n")
  cat("综合曼哈顿图已保存到: phewas_combined_manhattan_plot.pdf\n")
  if (nrow(significant_results) > 0) {
    cat("显著结果已保存到: phewas_significant_results.csv\n")
  }
} else {
  cat("没有生成任何结果\n")
}


#######################################omni plot################################################
library(ComplexHeatmap)
library(circlize)
library(dplyr)
library(stringr)
library(grid)

data <- significant_results[significant_results$category %in% c("Biochemical","Body_Composition",
                                                                "Inflammatory","Cognitive","Pulmonary"),]

length(unique(data$outcome))                                                                



# 数据预处理
plot_data <- data %>%
  mutate(
    outcome_clean = outcome_short,
    exposure_clean = exposure_short,
    
    # 使用FDR校正的显著性
    significant = significant_fdr,
    
    # 限制beta值范围到-1到1之间
    beta_capped = case_when(
      beta < -1 ~ -1,
      beta > 1 ~ 1,
      TRUE ~ beta
    )
  ) %>%
  distinct(exposure_clean, outcome_clean, .keep_all = TRUE)

# 检查数据
cat("处理后的数据行数:", nrow(plot_data), "\n")
cat("Beta值范围（原始）:", range(plot_data$beta), "\n")
cat("Beta值范围（限制后）:", range(plot_data$beta_capped), "\n")

# 创建矩阵数据 - 转置：行是饮料，列是结局
# 效应值矩阵（使用限制后的值）
beta_matrix <- plot_data %>%
  select(outcome_clean, exposure_clean, beta_capped) %>%
  tidyr::pivot_wider(names_from = outcome_clean, values_from = beta_capped, values_fill = 0) %>%
  as.data.frame()

exposure_names <- beta_matrix$exposure_clean
beta_matrix <- beta_matrix %>% select(-exposure_clean) %>% as.matrix()
rownames(beta_matrix) <- exposure_names

# 原始beta值矩阵（用于显示文本）
beta_original_matrix <- plot_data %>%
  select(outcome_clean, exposure_clean, beta) %>%
  tidyr::pivot_wider(names_from = outcome_clean, values_from = beta, values_fill = NA) %>%
  as.data.frame()

beta_orig_names <- beta_original_matrix$exposure_clean
beta_original_matrix <- beta_original_matrix %>% select(-exposure_clean) %>% as.matrix()
rownames(beta_original_matrix) <- beta_orig_names

# 创建结局分类注释
outcome_categories <- plot_data %>%
  select(outcome_clean, category) %>%
  distinct()

category_vector <- outcome_categories$category
names(category_vector) <- outcome_categories$outcome_clean

# 按类别对结局进行排序
outcome_order <- outcome_categories %>%
  arrange(category, outcome_clean) %>%
  pull(outcome_clean)

# 重新排序矩阵列
beta_matrix <- beta_matrix[, outcome_order, drop = FALSE]
beta_original_matrix <- beta_original_matrix[, outcome_order, drop = FALSE]

# 定义颜色函数
col_fun = colorRamp2(
  c(-1, 0, 1), 
  c("#2166AC", "white", "#B2182B")
)

# 创建列注释
category_colors <- c(
  "Biochemical" = "#E31A1C",
  "Pulmonary" = "#1F78B4", 
  "Inflammatory" = "#33A02C",
  "Cognitive" = "#FF7F00",
  "Body_Composition" = "#6A3D9A"
)

categories_for_annotation <- category_vector[colnames(beta_matrix)]

col_ha = columnAnnotation(
  Category = categories_for_annotation,
  col = list(Category = category_colors),
  annotation_name_gp = gpar(fontsize = 10),
  height = unit(0.5, "cm")
)

# 修改的cell function - 增大气泡，文字在气泡下方
cell_fun = function(j, i, x, y, width, height, fill) {
  # 绘制单元格浅灰色边框
  grid.rect(x = x, y = y, width = width, height = height,
            gp = gpar(fill = "transparent", col = "grey85", lwd = 1.5))
  
  # 获取当前单元格的值
  row_name = rownames(beta_matrix)[i]
  col_name = colnames(beta_matrix)[j]
  
  # 从原始矩阵获取值
  beta_val = beta_matrix[row_name, col_name]
  beta_orig = beta_original_matrix[row_name, col_name]
  
  # 只处理有效数据（非0且非NA）
  if (!is.na(beta_val) && beta_val != 0 && !is.na(beta_orig)) {
    # 大幅增大圆圈大小 - 占据单元格的大部分空间
    r = min(unit.c(width, height)) * 2.5 # 从0.35增加到0.45
    
    # 边框设置
    border_width = 0.5
    border_color = "black"
    
    # 绘制圆圈 - 稍微向上偏移，为下方文字留空间
    circle_y = y + height * 0.08  # 圆圈稍微向上
    grid.circle(x = x, y = circle_y, r = r, 
                gp = gpar(fill = fill, 
                          col = border_color, 
                          lwd = border_width))
    
    # 添加效应值文本（显示在圆圈下方）
    text_y = y - height * 0.25  # 文字在圆圈下方
    text_size = 6  # 稍微减小字体以避免重叠
    text_color = "black"  # 统一使用黑色，确保可读性
    
    # 格式化显示的数值
    if (abs(beta_orig) >= 100) {
      display_text = sprintf("%.0f", beta_orig)
    } else if (abs(beta_orig) >= 10) {
      display_text = sprintf("%.1f", beta_orig)
    } else {
      display_text = sprintf("%.2f", beta_orig)
    }
    
    grid.text(display_text, x, text_y, 
              gp = gpar(fontsize = text_size, col = text_color, fontface = "bold"))
  }
}

# 按类别分别进行聚类
clustered_outcomes <- c()
for(cat in unique(outcome_categories$category)) {
  cat_outcomes <- outcome_categories %>% 
    filter(category == cat) %>% 
    pull(outcome_clean)
  
  cat_outcomes_in_matrix <- intersect(cat_outcomes, colnames(beta_matrix))
  
  if(length(cat_outcomes_in_matrix) > 1) {
    sub_matrix <- beta_matrix[, cat_outcomes_in_matrix, drop = FALSE]
    
    if(sum(sub_matrix != 0, na.rm = TRUE) > 0) {
      col_dist <- stats::dist(t(sub_matrix))  # 明确使用stats包的dist函数
      col_hc <- hclust(col_dist, method = "complete")
      clustered_outcomes <- c(clustered_outcomes, cat_outcomes_in_matrix[col_hc$order])
    } else {
      clustered_outcomes <- c(clustered_outcomes, cat_outcomes_in_matrix)
    }
  } else if(length(cat_outcomes_in_matrix) == 1) {
    clustered_outcomes <- c(clustered_outcomes, cat_outcomes_in_matrix)
  }
}

# 使用分类聚类的结果重新排序
if(length(clustered_outcomes) > 0) {
  beta_matrix <- beta_matrix[, clustered_outcomes, drop = FALSE]
  beta_original_matrix <- beta_original_matrix[, clustered_outcomes, drop = FALSE]
  categories_for_annotation <- category_vector[colnames(beta_matrix)]
  
  # 更新列注释
  col_ha = columnAnnotation(
    Category = categories_for_annotation,
    col = list(Category = category_colors),
    annotation_name_gp = gpar(fontsize = 10),
    height = unit(0.5, "cm")
  )
  
  # 计算合适的尺寸以创建接近正方形的单元格
  n_cols <- ncol(beta_matrix)
  n_rows <- nrow(beta_matrix)
  
  # 增大单元格尺寸以容纳更大的气泡和下方文字
  cell_size <- 1.5  # 从1.2增加到1.5 cm
  
  # 重新创建热图 - 强制正方形单元格
  ht_main = Heatmap(
    beta_matrix,
    name = "Effect Size (β)",
    col = col_fun,
    rect_gp = gpar(type = "none"),
    cell_fun = cell_fun,
    row_title = "Beverage Preferences",
    column_title = "Health Outcomes",
    row_names_gp = gpar(fontsize = 14, fontface = "bold"),
    column_names_gp = gpar(fontsize = 9, angle = 45),
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    top_annotation = col_ha,
    na_col = "transparent",
    heatmap_legend_param = list(
      title = "Effect Size (β)",
      title_gp = gpar(fontsize = 12, fontface = "bold"),
      labels_gp = gpar(fontsize = 10),
      legend_height = unit(5, "cm"),
      at = c(-1, -0.5, 0, 0.5, 1),
      labels = c("≤-1", "-0.5", "0", "0.5", "≥1")
    ),
    # 强制设置正方形单元格
    width = unit(n_cols * cell_size, "cm"),
    height = unit(n_rows * cell_size, "cm")
  )
}

# 计算PDF尺寸
pdf_width <- n_cols * cell_size  # 额外空间给图例和标签
pdf_height <- n_rows * cell_size  # 额外空间给标题和注释

# 保存图片
pdf("beverage_effects_bubble_heatmap_clustered.pdf", width = pdf_width, height = pdf_height)
draw(ht_main)
dev.off()

cat("转置聚类图片已保存为: beverage_effects_bubble_heatmap_clustered.pdf\n")
cat("PDF尺寸:", pdf_width, "x", pdf_height, "inches\n")



######################################metabolomics plot###############################################
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(dplyr)


# 读取数据
data <- significant_results[significant_results$category=="NMR_Metabolomics",]

# 数据预处理
# 创建effect size矩阵
effect_matrix <- data %>%
  select(exposure, outcome, beta) %>%
  pivot_wider(names_from = outcome, values_from = beta, values_fill = 0) %>%
  column_to_rownames("exposure")

# 创建p值矩阵
p_matrix <- data %>%
  select(exposure, outcome, p_value) %>%
  pivot_wider(names_from = outcome, values_from = p_value, values_fill = 1) %>%
  column_to_rownames("exposure")

# 过滤：只保留beta绝对值 >= 0.01的代谢物
# 计算每列（代谢物）的最大绝对值
max_abs_beta <- apply(abs(effect_matrix), 2, max, na.rm = TRUE)
keep_metabolites <- max_abs_beta >= 0.01

# 过滤矩阵
effect_matrix_filtered <- effect_matrix[, keep_metabolites]
p_matrix_filtered <- p_matrix[, keep_metabolites]

cat("原始代谢物数量:", ncol(effect_matrix), "\n")
cat("过滤后代谢物数量:", ncol(effect_matrix_filtered), "\n")
cat("移除的代谢物数量:", sum(!keep_metabolites), "\n")

# 创建显著性标记矩阵
sig_matrix <- ifelse(p_matrix_filtered < 0.001, "***",
                     ifelse(p_matrix_filtered < 0.01, "**",
                            ifelse(p_matrix_filtered < 0.05, "*", "")))

# 简化代谢物名称（去除...Instance.0后缀）
colnames(effect_matrix_filtered) <- gsub("\\.\\.\\.Instance\\.0", "", colnames(effect_matrix_filtered))
colnames(sig_matrix) <- gsub("\\.\\.\\.Instance\\.0", "", colnames(sig_matrix))

# 进一步简化代谢物名称
simplified_names <- colnames(effect_matrix_filtered)

# 创建更简洁的代谢物名称映射
name_mapping <- c(
  # Fatty acids and ratios
  "Monounsaturated.Fatty.Acids" = "MUFA",
  "Saturated.Fatty.Acids" = "SFA", 
  "Omega.3.Fatty.Acids" = "Omega-3 FA",
  "Total.Fatty.Acids" = "Total FA",
  "PUFA" = "PUFA",
  "Polyunsaturated.Fatty.Acids" = "PUFA",
  "Omega.6.Fatty.Acids" = "Omega-6 FA",
  "Linoleic.Acid" = "Linoleic acid",
  "Ratio.of.22.6.docosahexaenoic.acid.to.total.fatty.acids" = "DHA/Total FA",
  "Docosahexaenoic.Acid.to.Total.Fatty.Acids.percentage" = "DHA/Total FA %",
  "Ratio.of.18.2.linoleic.acid.to.total.fatty.acids" = "LA/Total FA", 
  "Linoleic.Acid.to.Total.Fatty.Acids.percentage" = "LA/Total FA %",
  "Ratio.of.omega.3.fatty.acids.to.total.fatty.acids" = "Omega-3/Total FA",
  "Omega.3.Fatty.Acids.to.Total.Fatty.Acids.percentage" = "Omega-3/Total FA %",
  "Ratio.of.omega.6.fatty.acids.to.total.fatty.acids" = "Omega-6/Total FA",
  "Omega.6.Fatty.Acids.to.Total.Fatty.Acids.percentage" = "Omega-6/Total FA %",
  "Ratio.of.omega.6.fatty.acids.to.omega.3.fatty.acids" = "Omega-6/Omega-3",
  "Saturated.Fatty.Acids.to.Total.Fatty.Acids.percentage" = "SFA/Total FA %",
  "Polyunsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage" = "PUFA/Total FA %",
  "Monounsaturated.Fatty.Acids.to.Total.Fatty.Acids.percentage" = "MUFA/Total FA %",
  
  # Particle sizes
  "Average.Diameter.for.VLDL.Particles" = "VLDL diameter",
  "Average.Diameter.for.HDL.Particles" = "HDL diameter",
  "VLDL.diameter" = "VLDL diameter",
  "HDL.diameter" = "HDL diameter",
  
  # Apolipoproteins and phospholipids
  "Phosphoglycerides" = "Phosphoglycerides",
  "Total.Cholines" = "Total cholines", 
  "Phosphatidylcholines" = "PC",
  "Apolipoprotein.A1" = "ApoA1",
  
  # Lipid concentrations
  "Total.Lipids.in.IDL" = "IDL lipids",
  "Cholesterol.in.Medium.LDL" = "Medium LDL-C",
  "Cholesteryl.Esters.in.Medium.LDL" = "Medium LDL-CE", 
  "Total.Lipids.in.Large.HDL" = "Large HDL lipids",
  "Total.Lipids.in.Medium.HDL" = "Medium HDL lipids",
  "HDL.lipids" = "HDL lipids",
  "IDL.lipids" = "IDL lipids",
  "Large.HDL.lipids" = "Large HDL lipids",
  "Medium.LDL.C" = "Medium LDL-C",
  "Medium.LDL.CE" = "Medium LDL-CE",
  
  # Other metabolites  
  "Lactate" = "Lactate"
)

# 应用名称映射
for(i in 1:length(simplified_names)) {
  if(simplified_names[i] %in% names(name_mapping)) {
    simplified_names[i] <- name_mapping[simplified_names[i]]
  }
}

# 更新矩阵列名
colnames(effect_matrix_filtered) <- simplified_names
colnames(sig_matrix) <- simplified_names

# 创建代谢物分类
metabolite_categories <- data.frame(
  metabolite = simplified_names,
  category = case_when(
    grepl("diameter|Diameter", simplified_names) ~ "Particle size",
    grepl("Concentration.*Particles", simplified_names) ~ "Particle concentration",
    grepl("FA|acid|MUFA|PUFA|SFA|Omega|ratio", simplified_names) ~ "Fatty acids",
    grepl("Alanine|Glutamine|Histidine|Glycine", simplified_names) ~ "Amino acids",
    grepl("Lactate|Acetate|Acetone", simplified_names) ~ "Other metabolites",
    grepl("Apolipoprotein|ApoA1|Phosphoglycerides|PC|cholines", simplified_names) ~ "Apolipoproteins",
    grepl("Triglycerides|Cholesterol|LDL|HDL|IDL|lipids", simplified_names) ~ "Lipid concentrations",
    TRUE ~ "Other"
  )
)

# 准备注释颜色
# 为代谢物分类创建颜色映射
unique_categories <- unique(metabolite_categories$category)
category_colors <- brewer.pal(min(8, length(unique_categories)), "Set3")[1:length(unique_categories)]
names(category_colors) <- unique_categories

# 为不同的exposure类型创建不同的颜色
exposure_types <- c("coffee_tea_preference" = "Coffee/Tea preference", 
                    "SSB_ASB_preference" = "SSB/ASB preference", 
                    "NJ_water_preference" = "Natural juice/Water preference")

exposure_colors <- c("Coffee/Tea preference" = "#E31A1C",      # 红色
                     "SSB/ASB preference" = "#1F78B4",          # 蓝色  
                     "Natural juice/Water preference" = "#33A02C") # 绿色

# 定义原始beta值的颜色范围
beta_range <- range(as.vector(as.matrix(effect_matrix_filtered)), na.rm = TRUE)
col_fun <- colorRamp2(c(beta_range[1], 0, beta_range[2]), c("blue", "white", "red"))

# 创建列注释（代谢物分类）
ha_column <- HeatmapAnnotation(
  metabolite_category = metabolite_categories$category,
  col = list(metabolite_category = category_colors),
  height = unit(1, "cm"),
  annotation_name_gp = gpar(fontsize = 8),
  annotation_legend_param = list(
    metabolite_category = list(title = "Metabolite Category", 
                               title_gp = gpar(fontsize = 10),
                               labels_gp = gpar(fontsize = 8))
  )
)

# 创建行注释（exposure类型）- 使用不同颜色
exposure_labels <- exposure_types[rownames(effect_matrix_filtered)]
ha_row <- rowAnnotation(
  exposure_type = exposure_labels,
  col = list(exposure_type = exposure_colors),
  width = unit(1, "cm"),
  annotation_name_gp = gpar(fontsize = 8),
  annotation_legend_param = list(
    exposure_type = list(title = "Exposure Type", 
                         title_gp = gpar(fontsize = 10),
                         labels_gp = gpar(fontsize = 8))
  )
)

# 创建热图，使用简化的名称
ht_simplified <- Heatmap(
  as.matrix(effect_matrix_filtered),
  name = "Effect size\n(Beta)",
  col = col_fun,
  
  # 行设置
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 10),
  row_labels = exposure_types[rownames(effect_matrix_filtered)],
  
  # 列设置
  column_names_side = "bottom",
  column_names_rot = 45,
  column_names_gp = gpar(fontsize = 9),  # 稍微增大字体
  
  # 聚类设置
  cluster_rows = TRUE,
  cluster_columns = TRUE,
  clustering_distance_rows = "euclidean",
  clustering_distance_columns = "euclidean",
  
  # 添加显著性标记
  cell_fun = function(j, i, x, y, width, height, fill) {
    if(ncol(sig_matrix) >= j && nrow(sig_matrix) >= i) {
      if(sig_matrix[i, j] != "") {
        grid.text(sig_matrix[i, j], x, y, gp = gpar(fontsize = 8, col = "black"))
      }
    }
  },
  
  # 添加注释
  top_annotation = ha_column,
  left_annotation = ha_row,
  
  # 热图尺寸
  width = unit(18, "cm"),  # 稍微增加宽度
  height = unit(8, "cm"),
  
  # 添加标题
  column_title = "Metabolomics Association Heatmap (|Beta| >= 0.01)",
  column_title_gp = gpar(fontsize = 12, fontface = "bold"),
  
  # 图例设置
  heatmap_legend_param = list(
    title_gp = gpar(fontsize = 10),
    labels_gp = gpar(fontsize = 8),
    legend_height = unit(4, "cm")
  )
)

# 绘制简化版热图
draw(ht_simplified, merge_legend = TRUE)

# 保存简化版热图
pdf("metabolomics_heatmap_simplified_names.pdf", width = 16, height = 10)
draw(ht_simplified, merge_legend = TRUE)
dev.off()


# 显示过滤结果摘要
cat("\n过滤后beta值范围:\n")
print(summary(as.vector(as.matrix(effect_matrix_filtered))))

cat("\n被移除的代谢物 (beta绝对值 < 0.01):\n")
removed_metabolites <- colnames(effect_matrix)[!keep_metabolites]
print(gsub("\\.\\.\\.Instance\\.0", "", removed_metabolites))

cat("\n保留的代谢物数量按分类:\n")
print(table(metabolite_categories$category))



########################################Proteomics plot ####################################################
# 读取数据（假设数据已经保存为data变量）
data <- significant_results[significant_results$category=="Olink_Proteomics",]
str(data)

# 加载必要的包
library(ggplot2)
library(dplyr)
library(forcats)

# 为每种exposure选择显著性前十的outcome（基于p_value排序）
top_outcomes <- data %>%
  group_by(exposure_short) %>%
  arrange(p_value) %>%
  slice_head(n = 10) %>%
  ungroup()
print(top_outcomes,n=40)

# 创建棒棒糖图 - 使用正确的类别名称
lollipop_plot <- top_outcomes %>%
  ggplot(aes(x = fct_reorder(outcome_short, beta), y = beta)) +
  # 添加垂直线（棒棒糖的"棒"）
  geom_segment(aes(x = outcome_short, xend = outcome_short, 
                   y = 0, yend = beta), 
               color = "gray60", size = 1) +
  # 添加点（棒棒糖的"糖"），使用形状和颜色映射
  geom_point(aes(color = exposure_short, 
                 shape = exposure_short,
                 size = -log10(p_value)), 
             alpha = 0.8) +
  # 设置颜色映射 - 使用正确的类别名称
  scale_color_manual(
    name = "Beverage Preference",
    values = c(
      "Coffee/Tea" = "#F29E38",  # 橙色
      "Natural Juice/Water" = "#95BFA4",  # 绿色 - 注意这里改为正确的名称
      "SSB/ASB" = "#F2889B"  # 粉红色
    )
  ) +
  # 设置形状映射 - 使用正确的类别名称
  scale_shape_manual(
    name = "Beverage Preference",
    values = c(
      "Coffee/Tea" = 16,  # 实心圆点 ●
      "Natural Juice/Water" = 15,  # 实心方块 ■ - 注意这里改为正确的名称
      "SSB/ASB" = 17  # 实心三角形 ▲
    )
  ) +
  # 设置点的大小范围
  scale_size_continuous(
    name = "-log10(p-value)", 
    range = c(2, 6)
  ) +
  # 添加水平参考线
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  # 设置标签
  labs(
    title = "Top 10 Significant Outcomes by Exposure",
    x = "Protein",
    y = "Beta Coefficient"
  ) +
  # 设置主题
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  # 翻转坐标轴
  coord_flip() +
  # 设置图例
  guides(
    color = guide_legend(
      override.aes = list(size = 4),
      title = "Beverage Preference"
    ),
    shape = guide_legend(
      override.aes = list(size = 4),
      title = "Beverage Preference"
    ),
    size = guide_legend(
      title = "-log10(p-value)"
    )
  )

# 显示图形
print(lollipop_plot)

# 保存图形
ggsave("beverage_protein_lollipop.pdf", 
       plot = lollipop_plot, 
       width = 8, height = 6, 
       dpi = 300)


