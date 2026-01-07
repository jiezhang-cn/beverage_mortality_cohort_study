#####################################################Data curated##################################################
ukb_water_turnover <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/ukb_water_turnover.csv")
names(ukb_water_turnover)
names(beverage_20w_data)
water_turnover_data <- merge(beverage_20w_data,ukb_water_turnover[,c(1,192:208)],all.x=T)
names(water_turnover_data)


water_turnover_data$coffee_WT_met_pect <- water_turnover_data$coffee_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$coffee_WT_met_pect)
water_turnover_data$tea_WT_met_pect <- water_turnover_data$tea_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$tea_WT_met_pect)
water_turnover_data$plain_water_WT_met_pect <- water_turnover_data$plain_water_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$plain_water_WT_met_pect)
water_turnover_data$SSB_WT_met_pect <- water_turnover_data$SSB_volume/water_turnover_data$Drinking_waterb
summary(water_turnover_data$ASB_WT_met_pect)
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
quantile(water_turnover_data$totoal_WT_met_pect,c(0.1,0.9),na.rm=T)
water_turnover_data$totoal_fluid_intake <- (water_turnover_data$coffee_volume+water_turnover_data$tea_volume+water_turnover_data$plain_water_volume+
                                                  water_turnover_data$SSB_volume+water_turnover_data$ASB_volume+
                                                  water_turnover_data$NJ_volume+water_turnover_data$low_fat_milk_volume+
                                                  water_turnover_data$full_fat_milk_volume)
summary(water_turnover_data$totoal_fluid_intake)



#####################################################Total fluid intake (water turnover%) & mortality risk##################################################
survial_event <- Surv(time = water_turnover_data$follow_to_death_time,
                      event = water_turnover_data$death)

cox_model <- coxph(survial_event ~ totoal_WT_met_pect+
                     age + sex + ethnic + 
                     education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency + 
                     PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                   data = water_turnover_data)
summary(cox_model)


library(plotRCS)
rcsplot(data = water_turnover_data[!is.na(water_turnover_data$totoal_WT_met_pect),],
        outcome = "death",
        time = "follow_to_death_time",
        exposure = "totoal_WT_met_pect",
        covariates = c( "age", "sex", "ethnic", "education_years", "household_income", 
                        "bmi", "smk_num", "smk_qyr", "alcohol_intake_frequency", 
                        "diet_score", "PA_mod_vig_150", "overall_health_rating","total_energy_intake"),
        ref.value=1,
        knots = knot(3),
        xbreaks= seq(0.4, 2, by = 0.2),
        ybreaks= seq(0.8, 1.3, by = 0.1),
        linesize=1,
        linecolor="#729DA6",
)   



quantile(water_turnover_data$totoal_WT_met_pect[water_turnover_data$coffee_tea_preference==1],c(0.2,0.4,0.6,0.8),na.rm = T)
quantile(water_turnover_data$totoal_WT_met_pect[water_turnover_data$SSB_ASB_preference==1],c(0.2,0.4,0.6,0.8),na.rm = T)
quantile(water_turnover_data$totoal_WT_met_pect[water_turnover_data$NJ_water_preference==1],c(0.2,0.4,0.6,0.8),na.rm = T)
quantile(water_turnover_data$totoal_WT_met_pect[water_turnover_data$coffee_tea_preference==0 & 
                                                  water_turnover_data$SSB_ASB_preference==0 &
                                                  water_turnover_data$NJ_water_preference==0],c(0.2,0.4,0.6,0.8),na.rm = T)


water_turnover_data_subset <- water_turnover_data[water_turnover_data$coffee_tea_preference==1,]

water_turnover_data_subset <- water_turnover_data[water_turnover_data$coffee_tea_preference==0 & 
                                                    water_turnover_data$SSB_ASB_preference==0 &
                                                    water_turnover_data$NJ_water_preference==0,]

water_turnover_data_subset$totoal_WT_met_pect_cat[water_turnover_data_subset$totoal_WT_met_pect>=0.7971480      & water_turnover_data_subset$totoal_WT_met_pect<=1.0481185           ] <- 0
water_turnover_data_subset$totoal_WT_met_pect_cat[water_turnover_data_subset$totoal_WT_met_pect>=0.4745797     & water_turnover_data_subset$totoal_WT_met_pect<0.7971480           ] <- 1
water_turnover_data_subset$totoal_WT_met_pect_cat[water_turnover_data_subset$totoal_WT_met_pect<0.4745797     ] <- 2
water_turnover_data_subset$totoal_WT_met_pect_cat[water_turnover_data_subset$totoal_WT_met_pect>1.0481185     & water_turnover_data_subset$totoal_WT_met_pect<=1.3895219            ] <- 3
water_turnover_data_subset$totoal_WT_met_pect_cat[water_turnover_data_subset$totoal_WT_met_pect>1.3895219      ] <- 4
water_turnover_data_subset$totoal_WT_met_pect_cat <- as.factor(water_turnover_data_subset$totoal_WT_met_pect_cat)
table(water_turnover_data_subset$totoal_WT_met_pect_cat)

survial_event <- Surv(time = water_turnover_data_subset$follow_to_death_time,
                      event = water_turnover_data_subset$death)
cox_model <- coxph(survial_event ~ totoal_WT_met_pect_cat+coffee_WT_met_pect+tea_WT_met_pect+plain_water_WT_met_pect+
                     SSB_WT_met_pect+ASB_WT_met_pect+NJ_WT_met_pect+low_fat_milk_WT_met_pect+
                     full_fat_milk_WT_met_pect+
                     age + sex + ethnic + 
                     education_years + household_income + bmi + 
                     smk_num + smk_qyr + alcohol_intake_frequency + diet_score+
                     PA_mod_vig_150 + overall_health_rating + total_energy_intake,
                   data = water_turnover_data_subset)
summary(cox_model)

