#######################################first occurrence data#####################################################
ukb_data20250522 <- fread("/home/user2/jzhang_data/ukb_phenotype_data/ukb62663_dataset20250522.csv")
names(ukb_data20250522)


ukb_data_dictionary <- read.csv("~/jzhang_data/ukb_phenotype_data/app62663_20230823044232.dataset.data_dictionary.csv")
names(ukb_data_dictionary)
ukb_data_dictionary


library(dplyr)
library(stringr)

first_occurrences_cols <- ukb_data_dictionary %>%
  filter(startsWith(folder_path, "Health-related outcomes > First occurrences >"))

field_id_map <- first_occurrences_cols %>%
  transmute(
    field_id = str_remove(name, "^p"),
    title
  )

all_cols <- names(ukb_data20250522)
pattern <- paste0("^(", paste(field_id_map$field_id, collapse = "|"), ")-")
matched_cols <- all_cols[str_detect(all_cols, pattern)]

cols <- c("eid", "21022-0.0", "31-0.0", "53-0.0", matched_cols)
first_occurrences_dataset <- ukb_data20250522 %>%
  select(all_of(cols))

mc_field_id <- str_replace(matched_cols, "-.*$", "")
rename_map <- tibble(
  old = matched_cols,
  field_id = mc_field_id
) %>%
  left_join(field_id_map, by = "field_id") %>%
  transmute(old, new = title)

stopifnot(!any(is.na(rename_map$new)))

rename_vec <- setNames(rename_map$old, rename_map$new)
first_occurrences_dataset <- first_occurrences_dataset %>%
  rename(!!!rename_vec)

names(first_occurrences_dataset)
names(first_occurrences_dataset)[2:4] <- c("age","sex","baseline_date")
head(names(first_occurrences_dataset),100)



library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# first_occurrences_dataset <- first_occurrences_dataset %>%
#   mutate(
#     baseline_date = as.Date(baseline_date)
#   ) %>%
#   mutate(across(
#     .cols = starts_with("Date "),
#     .fns  = ~ as.Date(.)
#   ))

censor_date <- as.Date("2023-07-31")

date_cols <- names(first_occurrences_dataset) %>%
  keep(~ str_starts(.x, "Date ") & str_detect(.x, " first reported \\("))

special_dates <- as.Date(c(
  "1900-01-01", # Code has no event date
  "1901-01-01", # Before DOB
  "1902-02-02", # = DOB
  "1903-03-03", # same calendar year as DOB
  "1909-09-09", # future placeholder
  "2037-07-07"  # future placeholder
))

first_occurrences_dataset <- first_occurrences_dataset %>%
  mutate(across(
    .cols = all_of(date_cols),
    .fns  = ~ replace(., . %in% special_dates, as.Date(NA))
  ))

extract_icd   <- function(x) str_match(x, "^Date\\s+([A-Z0-9]+)\\s+first reported \\(")[, 2]
extract_label <- function(x) str_match(x, "\\((.+)\\)\\s*$")[, 2]

icd_codes <- extract_icd(date_cols)
labels    <- extract_label(date_cols)

if (anyNA(icd_codes) || anyNA(labels)) {
  bad <- date_cols[is.na(icd_codes) | is.na(labels)]
  stop("unmatch\n", paste(bad, collapse = "\n"))
}

slug <- function(s) str_replace_all(s, "\\s+", "_")
new_prevalent_names <- paste0(icd_codes, "_", slug(labels), "_prevalent")
new_incident_names  <- paste0(icd_codes, "_", slug(labels), "_incident")
new_follow_names    <- paste0(icd_codes, "_", slug(labels), "_follow_time")

make_three_cols <- function(df, date_col, nm_prev, nm_inci, nm_ft) {
  event_date <- df[[date_col]]
  
  prevalent <- ifelse(!is.na(event_date) & event_date < df$baseline_date, 1L, 0L)
  
  incident  <- ifelse(!is.na(event_date) & event_date > df$baseline_date, 1L, 0L)
  
  end_date <- ifelse(is.na(event_date), censor_date, event_date)

  end_date <- as.Date(end_date, origin = "1970-01-01")
  
  follow_time <- as.numeric(end_date - df$baseline_date, units = "days") / 365.25
  
  tibble(
    !!nm_prev := prevalent,
    !!nm_inci := incident,
    !!nm_ft   := follow_time
  )
}

# 6) 批量生成并合并
new_blocks <- map2(
  date_cols,
  seq_along(date_cols),
  ~ make_three_cols(
    df       = first_occurrences_dataset,
    date_col = .x,
    nm_prev  = new_prevalent_names[.y],
    nm_inci  = new_incident_names[.y],
    nm_ft    = new_follow_names[.y]
  )
)

first_occurrences_dataset <- bind_cols(first_occurrences_dataset, !!!new_blocks)

stopifnot(
  all(c(new_prevalent_names, new_incident_names, new_follow_names) %in% names(first_occurrences_dataset))
)

head(select(first_occurrences_dataset,
            any_of(c(new_prevalent_names[1:3],
                     new_incident_names[1:3],
                     new_follow_names[1:3]))))



table(first_occurrences_dataset$I20_angina_pectoris_prevalent)

first_occurrences_dataset_subset <- first_occurrences_dataset %>%
  select(
    eid,
    ends_with("_prevalent"),
    ends_with("_incident"),
    ends_with("_follow_time")
  )
fwrite(first_occurrences_dataset_subset, file = "/home/user2/jzhang_data/ukb_phenotype_data/first_occurrences_dataset.csv",row.names = F)




names(ukb_data_dictionary)
exclude_groups <- c(
  "Health-related outcomes > First occurrences > Certain conditions originating in the perinatal period",
  "Health-related outcomes > First occurrences > Certain infectious and parasitic diseases",
  "Health-related outcomes > First occurrences > Congenital disruptions and chromosomal abnormalities",
  "Health-related outcomes > First occurrences > Pregnancy, childbirth and the puerperium"
)


drop_titles <- ukb_data_dictionary %>%
  filter(folder_path %in% exclude_groups) %>%
  filter(str_starts(title, "Date ") & str_detect(title, " first reported \\(")) %>%
  pull(title) %>%
  unique()

drop_icd   <- extract_icd(drop_titles)
drop_label <- extract_label(drop_titles)

drop_slug  <- slug(drop_label)
drop_prefixes <- paste0(drop_icd, "_", drop_slug, "_")

drop_cols <- c(
  paste0(drop_prefixes, "prevalent"),
  paste0(drop_prefixes, "incident"),
  paste0(drop_prefixes, "follow_time")
)

cols_to_drop <- intersect(names(first_occurrences_dataset_subset), drop_cols)

first_occurrences_dataset_subset <- first_occurrences_dataset_subset %>%
  select(-any_of(cols_to_drop))




########################### PheWAS for disease incidence ########################################################
ukb_beverage_patterns_data <- read.csv("~/ukb_beverage_patterns_data.csv")
names(ukb_beverage_patterns_data)

names(first_occurrences_dataset_subset)
ukb_beverage_patterns_data <- merge(ukb_beverage_patterns_data,first_occurrences_dataset_subset,all.x = T)
ukb_beverage_patterns_data$beverage_preference <- as.factor(ukb_beverage_patterns_data$beverage_preference)

#test
a <- ukb_beverage_patterns_data[ukb_beverage_patterns_data$I70_atherosclerosis_prevalent==0,]
cox_model <- coxph(Surv(time = I70_atherosclerosis_follow_time,
                        event = I70_atherosclerosis_incident) ~ beverage_preference+ age + sex + ethnic + 
                        education_years + household_income + 
                        bmi + smk_num + smk_qyr + alcohol_intake_frequency + 
                        diet_score + PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                   data = a,)
summary(cox_model)

library(dplyr)
library(stringr)
library(purrr)
library(broom)
library(survival)
library(tidyr)
library(data.table)
library(progressr)
library(rlang)


df <- ukb_beverage_patterns_data
handlers(global = TRUE)
handlers("progress")  

covariates <- c(
  "age", "sex", "ethnic", "education_years", "household_income",
  "bmi", "smk_num", "smk_qyr", "alcohol_intake_frequency",
  "diet_score", "PA_mod_vig_150", "overall_health_rating","total_energy_intake"
)

event_threshold <- 200

exposure <- "beverage_preference"
if (!is.factor(df[[exposure]])) {
  df[[exposure]] <- factor(df[[exposure]])
}
if ("0" %in% levels(df[[exposure]])) {
  df[[exposure]] <- stats::relevel(df[[exposure]], ref = "0")
}



all_cols <- names(df)
incident_cols <- grep("_incident$", all_cols, value = TRUE)
follow_cols   <- grep("_follow_time$", all_cols, value = TRUE)
prev_cols     <- grep("_prevalent$", all_cols, value = TRUE)

prefix_from <- function(v, suf) sub(paste0(suf, "$"), "", v)
incident_prefix <- prefix_from(incident_cols, "_incident")
follow_prefix   <- prefix_from(follow_cols,   "_follow_time")
prev_prefix     <- prefix_from(prev_cols,     "_prevalent")

phenos <- Reduce(intersect, list(incident_prefix, follow_prefix, prev_prefix))
message("Detected phenotypes: ", length(phenos))


get_col <- function(data, colname) {
  data[[colname]]
}


fit_one_pheno <- function(ph) {
  inc_col <- paste0(ph, "_incident")
  ft_col  <- paste0(ph, "_follow_time")
  prev_col<- paste0(ph, "_prevalent")
  d <- df %>%
    filter(.data[[prev_col]] == 0) %>%
    filter(!is.na(.data[[inc_col]]), !is.na(.data[[ft_col]])) %>%
    filter(if_all(all_of(c(exposure, covariates)), ~ !is.na(.x)))
  
  n_events <- sum(d[[inc_col]] == 1, na.rm = TRUE)
  n <- nrow(d)
  if (is.na(n) || n == 0 || n_events <= event_threshold) {
    return(tibble(
      phenotype = ph, n = n, events = n_events,
      term = character(0), hr = numeric(0), lcl = numeric(0), ucl = numeric(0), p = numeric(0)
    ))
  }
  surv_obj <- Surv(time = get_col(d, ft_col), event = get_col(d, inc_col))

  rhs_vars <- c(exposure, covariates)
  fml_rhs <- reformulate(rhs_vars)  # ~ exposure + covariates
  fml <- as.formula(paste("surv_obj ~", paste(rhs_vars, collapse = " + ")))
  
  fit <- try(coxph(fml, data = d, ties = "efron"), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(tibble(
      phenotype = ph, n = n, events = n_events,
      term = character(0), hr = numeric(0), lcl = numeric(0), ucl = numeric(0), p = numeric(0)
    ))
  }
  
  tt <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
  
  out <- tt %>%
    filter(str_starts(term, paste0("^", exposure))) %>%
    transmute(
      phenotype = ph,
      n = n,
      events = n_events,
      term,
      hr  = estimate,
      lcl = conf.low,
      ucl = conf.high,
      p   = p.value
    )
  out
}

fit_one_pheno_with_pb <- function(ph, p) {
  on.exit(p())
  fit_one_pheno(ph)
}

results_long <- with_progress({
  p <- progressor(steps = length(phenos))
  map_dfr(phenos, ~ fit_one_pheno_with_pb(.x, p))
})

results_long <- results_long %>% distinct()


min_p_by_pheno <- results_long %>%
  group_by(phenotype) %>%
  summarise(min_p = min(p, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    min_p_FDR  = p.adjust(min_p, method = "BH"),
    min_p_Bonf = p.adjust(min_p, method = "bonferroni")
  )

results_long <- results_long %>%
  mutate(
    p_FDR_global  = p.adjust(p, method = "BH"),
    p_Bonf_global = p.adjust(p, method = "bonferroni")
  ) %>%
  left_join(min_p_by_pheno, by = "phenotype")



auto_cat <- function(ph) {
  icd <- sub("_.*$", "", ph)              
  lead <- substr(icd, 1, 1)               
  dplyr::case_when(
    lead %in% c("A","B") ~ "Infectious diseases",
    lead %in% c("C","D") ~ "Blood/immune and neoplasms",
    lead == "E" ~ "Endocrine, nutritional and metabolic diseases",
    lead == "F" ~ "Mental and behavioural disorders",
    lead == "G" ~ "Nervous system disorders",
    lead == "H" ~ "Eye/ear disorders",
    lead == "I" ~ "Circulatory system disorders",
    lead == "J" ~ "Respiratory system disorders",
    lead == "K" ~ "Digestive system disorders",
    lead == "L" ~ "Skin and subcutaneous tissue disorders",
    lead == "M" ~ "Musculoskeletal and connective tissue disorders",
    lead == "N" ~ "Genitourinary system disorders",
    lead == "O" ~ "Pregnancy/childbirth",
    lead == "P" ~ "Perinatal conditions",
    lead == "Q" ~ "Congenital malformations",
    lead == "R" ~ "Symptoms/abnormal findings",
    lead %in% c("S","T") ~ "Injury/poisoning",
    lead %in% c("V","W","X","Y") ~ "External causes",
    lead == "Z" ~ "Health status/contact",
    TRUE ~ "Other/Unknown"
  )
}

results_long <- results_long %>%
  mutate(phenotype_category = auto_cat(phenotype)) %>%
  relocate(phenotype_category, .after = phenotype)


fwrite(results_long,"/home/user2/beverage_patterns_phewas.csv",row.names = F)




####################################phewas vis#######################################################
beverage_patterns_phewas <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/beverage_patterns_phewas.csv")
str(beverage_patterns_phewas)
table(beverage_patterns_phewas$phenotype_category)


library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)

df <- beverage_patterns_phewas %>%
  filter(term == "beverage_preference3") %>%
  mutate(
    log10p   = -log10(p),
    beta     = log(hr),
    lcl_beta = log(lcl),
    ucl_beta = log(ucl),
    sig      = p_FDR_global <= 0.05
  )


cat_levels <- c(
  "Circulatory system disorders",
  "Endocrine, nutritional and metabolic diseases",
  "Mental and behavioural disorders",
  "Respiratory system disorders",
  "Digestive system disorders",
  "Nervous system disorders",
  "Genitourinary system disorders",
  "Musculoskeletal and connective tissue disorders",
  "Blood/immune and neoplasms",
  "Skin and subcutaneous tissue disorders",
  "Eye/ear disorders"
)

df <- df %>%
  mutate(phenotype_category = phenotype_category |> trimws() |> stringr::str_squish())

ord <- df %>%
  arrange(phenotype_category, p) %>%
  group_by(phenotype_category) %>%
  summarise(y_levels_cat = list(phenotype), .groups = "drop") %>%
  tidyr::unnest(y_levels_cat) %>%
  pull(y_levels_cat)

y_levels <- rev(ord)  

top_dat_all <- df %>%
  mutate(
    phenotype_category = fct_relevel(phenotype_category, cat_levels),
    phenotype = fct_relevel(phenotype, y_levels)
  )

forest_dat <- df %>%
  filter(sig) %>%
  mutate(
    phenotype_category = fct_relevel(phenotype_category, cat_levels),
    phenotype = fct_relevel(phenotype, y_levels)
  )


cat_pos <- top_dat_all %>%
  distinct(phenotype, phenotype_category) %>%
  mutate(y_num = as.numeric(phenotype)) %>%
  group_by(phenotype_category) %>%
  summarise(y_mid = median(y_num), .groups = "drop") %>%
  mutate(y_mid_fac = levels(top_dat_all$phenotype)[pmax(1, pmin(length(levels(top_dat_all$phenotype)), round(y_mid)))])


cat_colors <- c(
  "Circulatory system disorders" = "#BC5090",
  "Endocrine, nutritional and metabolic diseases" = "#FF7F0E",
  "Mental and behavioural disorders" = "#19A0AA",
  "Respiratory system disorders" = "#1F77B4",
  "Digestive system disorders" = "#9467BD",
  "Nervous system disorders" = "#2CA02C",
  "Genitourinary system disorders" = "#7F7F7F",
  "Musculoskeletal and connective tissue disorders" = "#8C564B",
  "Blood/immune and neoplasms" = "#E377C2",
  "Skin and subcutaneous tissue disorders" = "#17BECF",
  "Eye/ear disorders" = "#D62728"
)


g_top <- ggplot(top_dat_all,
                aes(x = log10p, y = phenotype, color = phenotype_category)) +
  geom_vline(xintercept = -log10(0.05), linetype = "22", color = "grey55") +
  geom_text(
    data = cat_pos,
    aes(x = -0.5, y = y_mid_fac, label = phenotype_category, color = phenotype_category),
    inherit.aes = FALSE, hjust = 1, size = 3.6, fontface = "bold"
  ) +
  geom_point(aes(shape = sig), size = 2.6, stroke = 1.0, fill = "white") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 23)) +
  scale_color_manual(values = cat_colors, limits = cat_levels, drop = TRUE, guide = "none") +
  scale_x_continuous(
    position = "top",
    limits  = c(-1, 50),
    breaks  = c(0, 5, 10, 20, 30, 40, 50)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
  )

print(g_top)
ggsave("C:/Users/zhangjie/Desktop/Rplot.pdf", width = 6, height = 6)



forest_dat <- forest_dat %>%
  mutate(phenotype = forcats::fct_reorder(phenotype, hr, .desc = TRUE))
g_forest <- ggplot(forest_dat,
                   aes(y = phenotype, x = beta, color = phenotype_category)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "grey65") +
  geom_errorbarh(aes(xmin = lcl_beta, xmax = ucl_beta), height = 0, linewidth = 0.9) +
  geom_point(shape = 15, size = 3) +
  scale_color_manual(values = cat_colors, limits = cat_levels, drop = TRUE, guide = "none") +
  scale_x_continuous(
    breaks = log(c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2)),
    labels = c("0.5", "0.75", "1.0", "1.25", "1.5", "1.75", "2.0")
  ) +
  labs(title = "Forest plot (FDR ≤ 0.05)", x = "HR (95% CI) on log scale", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = ggtext::element_markdown(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
  )

print(g_forest)
ggsave("C:/Users/zhangjie/Desktop/Rplot01.pdf", width = 6, height = 4)
