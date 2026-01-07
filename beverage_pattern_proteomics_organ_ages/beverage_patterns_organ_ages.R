############################################Regression analysis for beverage preferences & organ age gaps##################################################
names(beverage_organ_age_data)
table(beverage_organ_age_data$preference_group)

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

beverage_organ_age_data$preference_group <- factor(
  beverage_organ_age_data$preference_group,
  levels = c("NJ_Water_Preference", "Coffee_Tea_Preference", 
             "Mixed_Preference", "SSB_ASB_Preference")
)

run_preference_regression <- function(age_gap_type) {
  
  formula_str <- paste(age_gap_type, 
                       "~ preference_group +", 
                       "age + sex + ethnic + education_years + household_income + 
                        bmi + smk_num + smk_qyr + alcohol_intake_frequency + 
                        diet_score + PA_mod_vig_150 + overall_health_rating + total_energy_intake")
  
  model_vars <- c(age_gap_type, "preference_group",
                  "age", "sex", "ethnic", "education_years", "household_income", 
                        "bmi", "smk_num", "smk_qyr", "alcohol_intake_frequency", 
                        "diet_score", "PA_mod_vig_150", "overall_health_rating","total_energy_intake")
  
  df <- as.data.frame(beverage_organ_age_data)
  analysis_data <- df[, model_vars]
  analysis_data <- analysis_data[complete.cases(analysis_data), ]
  
  if(nrow(analysis_data) < 100) {
    return(NULL)
  }
  
  tryCatch({
    model <- lm(as.formula(formula_str), data = analysis_data)
    coefs <- summary(model)$coefficients
    
    ci <- confint(model)
    
    result <- data.frame(
      term = rownames(coefs),
      estimate = coefs[, "Estimate"],
      std.error = coefs[, "Std. Error"],
      statistic = coefs[, "t value"],
      p.value = coefs[, "Pr(>|t|)"],
      ci_lower = ci[, 1],
      ci_upper = ci[, 2],
      age_gap_type = age_gap_type,
      n_obs = nrow(analysis_data),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }, error = function(e) {
    cat("Error in", age_gap_type, ":", e$message, "\n")
    return(NULL)
  })
}

age_gap_types <- c("Heart_age_gap", "Kidney_age_gap", 
                   "Artery_age_gap", "Brain_age_gap", "Adipose_age_gap", 
                   "Muscle_age_gap", "Liver_age_gap", "Immune_age_gap", 
                   "Lung_age_gap", "Intestine_age_gap", "Pancreas_age_gap")

all_results <- list()
cat("Running regression analyses...\n")

for(age_gap in age_gap_types) {
  cat("  Processing:", age_gap, "...")
  result <- run_preference_regression(age_gap)
  if(!is.null(result)) {
    all_results[[age_gap]] <- result
    cat(" ✓\n")
  } else {
    cat(" ✗\n")
  }
}

final_results <- do.call(rbind, all_results)
rownames(final_results) <- NULL

preference_terms <- c("preference_groupCoffee_Tea_Preference", 
                      "preference_groupMixed_Preference", 
                      "preference_groupSSB_ASB_Preference")

preference_results <- final_results[final_results$term %in% preference_terms, ]
preference_results$p.value.fdr <- p.adjust(preference_results$p.value, method = "fdr")
preference_results$sig_raw <- ifelse(preference_results$p.value < 0.001, "***",
                                     ifelse(preference_results$p.value < 0.01, "**",
                                            ifelse(preference_results$p.value < 0.05, "*", "")))
preference_results$sig_fdr <- ifelse(preference_results$p.value.fdr < 0.001, "***",
                                     ifelse(preference_results$p.value.fdr < 0.01, "**",
                                            ifelse(preference_results$p.value.fdr < 0.05, "*", "")))


display_results <- data.frame(
  Organ = gsub("_age_gap", "", preference_results$age_gap_type),
  Group = gsub("preference_group", "", preference_results$term),
  Beta = sprintf("%.4f", preference_results$estimate),
  SE = sprintf("%.4f", preference_results$std.error),
  CI_95 = sprintf("[%.4f, %.4f]", preference_results$ci_lower, preference_results$ci_upper),
  P_value = sprintf("%.4e", preference_results$p.value),
  P_FDR = sprintf("%.4e", preference_results$p.value.fdr),
  Sig = preference_results$sig_raw,
  N = preference_results$n_obs
)

print(display_results, row.names = FALSE)


if(sum(preference_results$p.value < 0.05) > 0) {
  cat("\nSIGNIFICANT ASSOCIATIONS (p < 0.05):\n")
  sig_results <- preference_results[preference_results$p.value < 0.05, ]
  sig_results <- sig_results[order(sig_results$p.value), ]
  
  for(i in 1:nrow(sig_results)) {
    row <- sig_results[i, ]
    organ <- gsub("_age_gap", "", row$age_gap_type)
    group <- gsub("preference_group", "", row$term)
    direction <- ifelse(row$estimate > 0, "↑ accelerated", "↓ decelerated")
    cat(sprintf("  %s vs NJ_Water - %s: β=%.4f [%.4f, %.4f], p=%.4e %s\n",
                group, organ, row$estimate, row$ci_lower, row$ci_upper, 
                row$p.value, direction))
  }
}

preference_group_associations <<- preference_results


plot_data <- preference_results %>%
  mutate(
    Organ = gsub("_age_gap", "", age_gap_type),
    Organ = factor(Organ, levels = c("Heart", "Kidney", "Artery", "Brain", "Adipose",
                                     "Muscle", "Liver", "Immune", "Lung", "Intestine", "Pancreas")),
    Preference = gsub("preference_group", "", term),
    Preference = case_when(
      Preference == "Coffee_Tea_Preference" ~ "Coffee/Tea",
      Preference == "Mixed_Preference" ~ "Mixed",
      Preference == "SSB_ASB_Preference" ~ "SSB/ASB",
      TRUE ~ Preference
    ),
    Preference = factor(Preference, levels = c("SSB/ASB", "Mixed", "Coffee/Tea")),
    sig_label = case_when(
      p.value.fdr < 0.05 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    estimate_capped = pmax(pmin(estimate, 0.5), -0.5)
  )

color_palette <- c("#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", 
                   "white", 
                   "#FEEAA8", "#FDBB84", "#FC8D59", "#E34A33", "#B30000")

p <- ggplot(plot_data, aes(x = Organ, y = Preference)) +
  geom_tile(aes(fill = estimate_capped), color = "white", size = 1.5) +
  scale_fill_gradientn(
    colors = color_palette,
    limits = c(-0.2, 0.2),
    breaks = seq(-0.2, 0.2, 0.05),
    name = "β coefficient",
    guide = guide_colorbar(
      barwidth = 1.5,
      barheight = 15,
      frame.colour = "black",
      ticks.colour = "black",
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  geom_text(aes(label = sig_label), 
            size = 6, 
            color = "black",
            fontface = "bold",
            vjust = 0.7) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                               color = "black", size = 11),
    axis.text.y = element_text(color = "black", size = 11),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_fixed(ratio = 1)
print(p)

ggsave("preference_organ_aging_heatmap.pdf", 
       plot = p, 
       width = 12, 
       height = 4.5, 
       dpi = 300,
       bg = "white")