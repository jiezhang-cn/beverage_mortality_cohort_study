names(ukb_beverage_data)
str(imputed_data)
water_turnover_data <- merge(ukb_beverage_data[,c(1,545:560)],imputed_data,all.x=T)
water_turnover_data <- merge(water_turnover_data,diet_questionnaire_completed_date[,c("eid","diet_questionnaire_completed_date")],all.x=T)
str(water_turnover_data)


water_turnover_data$death[water_turnover_data$eid %in% death_update$eid] <- 1
water_turnover_data$death[is.na(water_turnover_data$death)] <- 0
str(death_data)
water_turnover_data <- merge(water_turnover_data,death_data[,c("eid","date_of_death","cause_icd10","broad_cause",
                                                           "cvd_death","cancer_death","respiratory_death","specific_cancer","specific_cvd")],all.x=T)
water_turnover_data$cvd_death[is.na(water_turnover_data$cvd_death)] <- 0
water_turnover_data$respiratory_death[is.na(water_turnover_data$respiratory_death)] <- 0
water_turnover_data$cancer_death[is.na(water_turnover_data$cancer_death)] <- 0

end_date <- as.Date("2024-07-31")

# 4. 计算随访时间
water_turnover_data <- within(water_turnover_data, {
  # 对于已经死亡的人，使用死亡日期
  # 对于仍在随访的人，使用2024-07-31
  follow_to_death_time <- case_when(
    !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, diet_questionnaire_completed_date, units = "days")) / 365.25,
    !is.na(diet_questionnaire_completed_date) ~ as.numeric(difftime(end_date, diet_questionnaire_completed_date, units = "days")) / 365.25,
    TRUE ~ NA_real_
  )
})

summary(water_turnover_data$follow_to_death_time)
names(ckm_ukb_data)
water_turnover_data <- merge(water_turnover_data, 
                           ckm_ukb_data[ckm_ukb_data$ckm_stage!="NA", colnames(ckm_ukb_data) %in% c("eid","ckm_stage")], 
                           all.x=TRUE)
names(water_turnover_data)




ukb_water_turnover <- read.csv("C:/Users/张杰/Desktop/ckm_beverage_mortality/data/ukb_water_turnover.csv")
names(ukb_water_turnover)
water_turnover_data <- merge(water_turnover_data,ukb_water_turnover[,c(1,173:206)],all.x = T)
names(water_turnover_data)

water_turnover_data$coffee_WT_met_pect <- water_turnover_data$coffee_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$coffee_WT_met_pect)
water_turnover_data$tea_WT_met_pect <- water_turnover_data$tea_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$tea_WT_met_pect)
water_turnover_data$plain_water_WT_met_pect <- water_turnover_data$plain_water_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$plain_water_WT_met_pect)
water_turnover_data$SSB_WT_met_pect <- water_turnover_data$SSB_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$SSB_WT_met_pect)
water_turnover_data$ASB_WT_met_pect <- water_turnover_data$ASB_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$ASB_WT_met_pect)
water_turnover_data$NJ_WT_met_pect <- water_turnover_data$NJ_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$NJ_WT_met_pect)
water_turnover_data$low_fat_milk_WT_met_pect <- water_turnover_data$low_fat_milk_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$low_fat_milk_WT_met_pect)
water_turnover_data$full_fat_milk_WT_met_pect <- water_turnover_data$full_fat_milk_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$full_fat_milk_WT_met_pect)
water_turnover_data$totoal_WT_met_pect <- (water_turnover_data$coffee_volume+water_turnover_data$tea_volume+water_turnover_data$plain_water_volume+
                                             water_turnover_data$SSB_volume+water_turnover_data$ASB_volume+
                                             water_turnover_data$NJ_volume+water_turnover_data$low_fat_milk_volume+
                                             water_turnover_data$full_fat_milk_volume)/water_turnover_data$Drinking_waterb
summary(water_turnover_data$totoal_WT_met_pect)
quantile(water_turnover_data$totoal_WT_met_pect,c(0.1,0.95),na.rm=T)


################################individual beverage percent in water turnover associations with mortality total population#################################
survial_event <- Surv(time = water_turnover_data$follow_to_death_time,
                      event = water_turnover_data$death)

cox_model <- coxph(survial_event ~ totoal_WT_met_pect+
                     age + sex + ethnic + tdi + pack_years_smoking + 
                     chesse_intake + nap_frequency + frequency_tiredness +
                     household_income + live_with_partner + shorter_than10 + 
                     sleep_duration_group + plumper_than10 + education_years + 
                     smoking_status + physical_activity_group + employed + 
                     living_flat_vs_house + financial_diffculty + use_gym + 
                     fed_up_feeling + renting_from_council_vs_own + ease_skin_tanning,
                   data = water_turnover_data)
summary(cox_model)


library(plotRCS)
rcsplot(data = water_turnover_data[!is.na(water_turnover_data$totoal_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "totoal_WT_met_pect",
        covariates = c( "age", "sex", "ethnic", "tdi", "pack_years_smoking", 
                        "chesse_intake", "nap_frequency", "frequency_tiredness", "household_income", 
                        "live_with_partner", "shorter_than10", 
                        "sleep_duration_group","plumper_than10",
                        "education_years","smoking_status","physical_activity_group","employed",
                        "living_flat_vs_house","financial_diffculty","use_gym",
                        "fed_up_feeling","renting_from_council_vs_own","ease_skin_tanning"),
        ref.value=0,
        knots = knot(5),
        xbreaks= seq(0.4, 2, by = 0.5),
        ybreaks= seq(0.8, 1.4, by = 0.1),
        linesize=1,
        linecolor="#729DA6",
)   




library(plotRCS)
rcsplot(data = water_turnover_data[!is.na(water_turnover_data$coffee_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "coffee_WT_met_pect",
        covariates = c( "age", "sex", "ethnic", "tdi", "pack_years_smoking", 
                        "chesse_intake", "nap_frequency", "frequency_tiredness", "household_income", 
                        "live_with_partner", "shorter_than10", 
                        "sleep_duration_group","plumper_than10",
                        "education_years","smoking_status"),
        ref.value=0,
        knots = knot(3),
        xbreaks= seq(0, 1.5, by = 0.5),
        ybreaks= seq(0.8, 1.6, by = 0.2),
        linesize=1,
        linecolor="#729DA6",
)        

rcsplot(data = water_turnover_data[!is.na(water_turnover_data$tea_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "tea_WT_met_pect",
        covariates = c("age", "sex", "ethnic", "tdi", "household_income", 
                       "live_with_partner", "physical_activity_group", "employed", "living_flat_vs_house", "financial_diffculty", 
                       "sleep_duration_group", "plumper_than10", "education_years","use_gym",
                       "smoking_status"
        ),
        ref.value=0,
        knots = knot(3),
        xbreaks= seq(0, 2, by = 0.5),
        ybreaks= seq(0.8, 1.4, by = 0.1),
        linesize=1,
        linecolor="#729DA6",
)



rcsplot(data = water_turnover_data[!is.na(water_turnover_data$plain_water_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "plain_water_WT_met_pect",
        covariates = c(
          "age", "sex", "ethnic", "fed_up_feeling", "tdi", 
          "pack_years_smoking", "ease_skin_tanning", "household_income",
          "live_with_partner", "physical_activity_group", 
          "shorter_than10", "living_flat_vs_house", "education_years",
          "use_gym","smoking_status"),
        ref.value=0,
        knots = knot(3),
        xbreaks= seq(0, 1.2, by = 0.2),
        ybreaks= seq(0.8, 1.4, by = 0.1),
        linesize=1,
        linecolor="#729DA6",
)




# Load the splines package
library(splines)
library(rms)
# 首先设置数据分布
dd <- datadist(water_turnover_data)
dd$limits["Adjust to", "SSB_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(water_turnover_data$SSB_WT_met_pect)
quantile(water_turnover_data$SSB_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(SSB_WT_met_pect, 3) + ",
                       "age + sex + ethnic +pack_years_smoking+chesse_intake+
                         frequency_unenthusiasm+nap_frequency+frequency_tiredness+
                         household_income+live_with_partner+	
                         employed+living_flat_vs_house+
                         financial_diffculty+sleep_duration_group+
                         renting_from_council_vs_own+education_years+smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=water_turnover_data[!is.na(water_turnover_data$SSB_WT_met_pect),])

# 预测相对风险
SSB_range <- seq(0, 1.5, by = 0.1)
pred <- Predict(fit, SSB_WT_met_pect = SSB_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, SSB_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = SSB_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#729DA61A") + # 置信区间用虚线
  geom_line(color = "#729DA6", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.5),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(1, 4, by = 0.5),expand = c(0, 0.2)) +
  labs(x = "SSB intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)


# 首先设置数据分布
dd <- datadist(water_turnover_data)
dd$limits["Adjust to", "ASB_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(water_turnover_data$ASB_WT_met_pect)
quantile(water_turnover_data$ASB_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(ASB_WT_met_pect, 3) + ",
                       "age + sex + ethnic + fed_up_feeling + pack_years_smoking + ",
                       "chesse_intake + nap_frequency + frequency_tiredness + physical_activity_group + employed + ",
                       "sleep_duration_group + sleep_duration_group + renting_from_council_vs_own + education_years + smoking_status +",
                       "plumper_than10",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=water_turnover_data[!is.na(water_turnover_data$ASB_WT_met_pect),])

# 预测相对风险
ASB_range <- seq(0, 1.3, by = 0.1)
pred <- Predict(fit, ASB_WT_met_pect = ASB_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, ASB_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = ASB_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#729DA61A") + # 置信区间用虚线
  geom_line(color = "#729DA6", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 1.3, by = 0.2),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 3, by = 0.5),expand = c(0, 0.01)) +
  labs(x = "ASB intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)


# 首先设置数据分布
dd <- datadist(water_turnover_data)
dd$limits["Adjust to", "NJ_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(water_turnover_data$NJ_WT_met_pect)
quantile(water_turnover_data$NJ_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(NJ_WT_met_pect, 4) + ",
                       "age + sex + ethnic + fed_up_feeling + pack_years_smoking + ",
                       "chesse_intake + frequency_tiredness + ease_skin_tanning + household_income + live_with_partner + ",
                       "physical_activity_group + employed + living_flat_vs_house +financial_diffculty +	plumper_than10 + education_years + smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=water_turnover_data[!is.na(water_turnover_data$NJ_WT_met_pect),])

# 预测相对风险
NJ_range <- seq(0, 1, by = 0.1)
pred <- Predict(fit, NJ_WT_met_pect = NJ_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, NJ_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = NJ_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#729DA61A") + # 置信区间用虚线
  geom_line(color = "#729DA6", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.5,3, by = 0.5),expand = c(0, 0)) +
  labs(x = "NJ intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)


# 首先设置数据分布
dd <- datadist(water_turnover_data)
dd$limits["Adjust to", "low_fat_milk_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(water_turnover_data$low_fat_milk_WT_met_pect)
quantile(water_turnover_data$low_fat_milk_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(low_fat_milk_WT_met_pect, 4) + ",
                       "age + sex + ethnic + fed_up_feeling + tdi + ",
                       "chesse_intake + frequency_unenthusiasm + frequency_tiredness + ease_skin_tanning + household_income + ",
                       "live_with_partner + shorter_than10 + living_flat_vs_house + financial_diffculty +sleep_duration_group + plumper_than10+ education_years +	smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=water_turnover_data[!is.na(water_turnover_data$low_fat_milk_WT_met_pect),])

# 预测相对风险
lowfat_milk_range <- seq(0, 0.6, by = 0.05)
pred <- Predict(fit, low_fat_milk_WT_met_pect = lowfat_milk_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, low_fat_milk_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = low_fat_milk_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#729DA61A") + # 置信区间用虚线
  geom_line(color = "#729DA6", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 0.6, by = 0.1),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.4, 1.6, by = 0.2),expand = c(0, 0.25)) +
  labs(x = "Low-fat milk intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)


# 首先设置数据分布
dd <- datadist(water_turnover_data)
dd$limits["Adjust to", "full_fat_milk_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(water_turnover_data$full_fat_milk_WT_met_pect)
quantile(water_turnover_data$full_fat_milk_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(full_fat_milk_WT_met_pect, 4) + ",
                       "age + sex + tdi + pack_years_smoking + chesse_intake + ",
                       "household_income + live_with_partner + physical_activity_group + plumper_than10 + use_open_fire + ",
                       "use_gym + smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=water_turnover_data[!is.na(water_turnover_data$full_fat_milk_WT_met_pect),])

# 预测相对风险
fullfat_milk_range <- seq(0, 0.25, by = 0.01)
pred <- Predict(fit, full_fat_milk_WT_met_pect = fullfat_milk_range, fun = exp,ref.zero=T)
pred$yhat <- exp(pred$yhat)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
anova(fit, full_fat_milk_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = full_fat_milk_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#729DA61A") + # 置信区间用虚线
  geom_line(color = "#729DA6", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 0.25, by = 0.05),expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0.8, 1.6, by = 0.2),expand = c(0, 0.1)) +
  labs(x = "Full-fat milk intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 5)




################################individual beverage percent in water turnover associations with mortality by ckm stages#################################
data_subset <- water_turnover_data[water_turnover_data$ckm_stage==3 | water_turnover_data$ckm_stage==4,]
data_subset <- water_turnover_data[water_turnover_data$ckm_stage==2,]


survial_event <- Surv(time = data_subset$follow_to_death_time,
                      event = data_subset$death)

cox_model <- coxph(survial_event ~ tea_WT_met_pect+
                     age + sex + ethnic +fed_up_feeling+pack_years_smoking+chesse_intake+	
                     frequency_tiredness+	
                     ease_skin_tanning+household_income+	
                     live_with_partner+physical_activity_group+employed+	
                     living_flat_vs_house+		
                     financial_diffculty+	
                     plumper_than10+education_years+
                     smoking_status,
                   data = data_subset)
summary(cox_model)


library(plotRCS)
rcsplot(data = data_subset[!is.na(data_subset$coffee_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "coffee_WT_met_pect",
        covariates = c( "age", "sex", "ethnic", "tdi", "pack_years_smoking", 
                        "chesse_intake", "nap_frequency", "frequency_tiredness", "household_income", 
                        "live_with_partner", "shorter_than10", 
                        "sleep_duration_group","plumper_than10",
                        "education_years","smoking_status"),
        ref.value=0,
        knots = knot(3),
        ybreaks= seq(0.6, 1.6, by = 0.2),
        linesize=1,
        linecolor="#ba4848",
)        

rcsplot(data = data_subset[!is.na(data_subset$tea_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "tea_WT_met_pect",
        covariates = c("age", "sex", "ethnic", "tdi", "household_income", 
                       "live_with_partner", "physical_activity_group", "employed", "living_flat_vs_house", "financial_diffculty", 
                       "sleep_duration_group", "plumper_than10", "education_years","use_gym",
                       "smoking_status"
        ),
        ref.value=0,
        knots = knot(3),
        ybreaks= seq(0.4, 1.4, by = 0.1),
        linesize=1,
        linecolor="#ba4848",
)



rcsplot(data = data_subset[!is.na(data_subset$plain_water_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "plain_water_WT_met_pect",
        covariates = c(
                       "age", "sex", "ethnic", "fed_up_feeling", "tdi", 
                       "pack_years_smoking", "ease_skin_tanning", "household_income",
                       "live_with_partner", "physical_activity_group", 
                       "shorter_than10", "living_flat_vs_house", "education_years",
                       "use_gym","smoking_status",
                       "SSB_intake"),
        ref.value=0,
        knots = knot(3),
        ybreaks= seq(0.8, 1.4, by = 0.1),
        linesize=1,
        linecolor="#ba4848",
)




# Load the splines package
library(splines)
library(rms)
# 首先设置数据分布
dd <- datadist(data_subset)
dd$limits["Adjust to", "SSB_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(data_subset$SSB_WT_met_pect)
quantile(data_subset$SSB_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(SSB_WT_met_pect, 4) + ",
                       "age + sex + ethnic + chesse_intake + nap_frequency + ",
                       "household_income + employed + education_years + smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=data_subset[!is.na(data_subset$SSB_WT_met_pect),])

# 预测相对风险
SSB_range <- seq(0, 1.2, by = 0.2)
pred <- Predict(fit, SSB_WT_met_pect = SSB_range, fun = exp,ref.zero=T)

anova(fit, SSB_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = SSB_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 1.2, by = 0.2)) +
  scale_y_continuous(breaks = seq(1, 4.5, by = 0.5)) +
  labs(x = "SSB intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)


# 首先设置数据分布
dd <- datadist(data_subset)
dd$limits["Adjust to", "ASB_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(data_subset$ASB_WT_met_pect)
quantile(data_subset$ASB_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(ASB_WT_met_pect, 3) + ",
                       "age + sex + ethnic + fed_up_feeling + pack_years_smoking + ",
                       "chesse_intake + nap_frequency + frequency_tiredness + physical_activity_group + employed + ",
                       "sleep_duration_group + sleep_duration_group + renting_from_council_vs_own + education_years + smoking_status +",
                       "plumper_than10",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=data_subset[!is.na(data_subset$ASB_WT_met_pect),])

# 预测相对风险
ASB_range <- seq(0, 1.2, by = 0.1)
pred <- Predict(fit, ASB_WT_met_pect = ASB_range, fun = exp,ref.zero=T)

anova(fit, ASB_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = ASB_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 1.2, by = 0.2)) +
  scale_y_continuous(breaks = seq(0.5, 4, by = 0.5)) +
  labs(x = "ASB intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)


# 首先设置数据分布
dd <- datadist(data_subset)
dd$limits["Adjust to", "NJ_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(data_subset$NJ_WT_met_pect)
quantile(data_subset$NJ_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(NJ_WT_met_pect, 4) + ",
                       "age + sex + ethnic + fed_up_feeling + pack_years_smoking + ",
                       "chesse_intake + frequency_tiredness + ease_skin_tanning + household_income + live_with_partner + ",
                       "physical_activity_group + employed + living_flat_vs_house +financial_diffculty +	plumper_than10 + education_years + smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=data_subset[!is.na(data_subset$NJ_WT_met_pect),])

# 预测相对风险
NJ_range <- seq(0, 0.8, by = 0.1)
pred <- Predict(fit, NJ_WT_met_pect = NJ_range, fun = exp,ref.zero=T)

anova(fit, NJ_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = NJ_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.2)) +
  scale_y_continuous(breaks = seq(0.5,3, by = 0.5)) +
  labs(x = "NJ intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)


# 首先设置数据分布
dd <- datadist(data_subset)
dd$limits["Adjust to", "low_fat_milk_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(data_subset$low_fat_milk_WT_met_pect)
quantile(data_subset$low_fat_milk_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(low_fat_milk_WT_met_pect, 4) + ",
                       "age + sex + ethnic + fed_up_feeling + tdi + ",
                       "chesse_intake + frequency_unenthusiasm + frequency_tiredness + ease_skin_tanning + household_income + ",
                       "live_with_partner + shorter_than10 + living_flat_vs_house + financial_diffculty +sleep_duration_group + plumper_than10+ education_years +	smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=data_subset[!is.na(data_subset$low_fat_milk_WT_met_pect),])

# 预测相对风险
lowfat_milk_range <- seq(0, 0.5, by = 0.1)
pred <- Predict(fit, low_fat_milk_WT_met_pect = lowfat_milk_range, fun = exp,ref.zero=T)

anova(fit, low_fat_milk_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = low_fat_milk_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.1)) +
  scale_y_continuous(breaks = seq(0.2, 2.0, by = 0.2)) +
  labs(x = "Low-fat milk intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)


# 首先设置数据分布
dd <- datadist(data_subset)
dd$limits["Adjust to", "full_fat_milk_WT_met_pect"] <- 0  # 设置参考值为0
options(datadist='dd')

summary(data_subset$full_fat_milk_WT_met_pect)
quantile(data_subset$full_fat_milk_WT_met_pect,0.9,na.rm = T)
# 创建模型公式
model_formula <- paste("Surv(follow_to_death_time,death) ~ ",
                       "pol(full_fat_milk_WT_met_pect, 4) + ",
                       "age + sex + tdi + pack_years_smoking + chesse_intake + ",
                       "household_income + live_with_partner + physical_activity_group + plumper_than10 + use_open_fire + ",
                       "use_gym + smoking_status",
                       collapse = " ")
# 拟合模型
fit <- cph(as.formula(model_formula), 
           x=TRUE, 
           y=TRUE,
           data=data_subset[!is.na(data_subset$full_fat_milk_WT_met_pect),])

# 预测相对风险
fullfat_milk_range <- seq(0, 0.25, by = 0.05)
pred <- Predict(fit, full_fat_milk_WT_met_pect = fullfat_milk_range, fun = exp,ref.zero=T)

anova(fit, full_fat_milk_WT_met_pect)  

# 绘图
ggplot(pred, aes(x = full_fat_milk_WT_met_pect, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill="#F6E6E6") + # 置信区间用虚线
  geom_line(color = "#ba4848", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 0.25, by = 0.05)) +
  scale_y_continuous(breaks = seq(0.5, 3, by = 0.5)) +
  labs(x = "Full-fat milk intake / WT (%)",
       y = "Hazard ratio (95% CI)") +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), # 移除竖直网格线
    aspect.ratio = 5/6
  )
ggsave("C:/Users/张杰/Desktop/Rplot.pdf", width = 6, height = 6)













