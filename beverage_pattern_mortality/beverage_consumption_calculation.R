library(tidyr)
library(dplyr)
library(tidyverse)

##########################################20w Drinks Data Clean###########################################
ukb_beverage_data <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/ukb_beverage_data")
names(ukb_beverage_data)

eids <- unique(c(ukb_beverage_data$eid[!is.na(ukb_beverage_data$typical_diet_yesterday_f100020_0_0)],
                 ukb_beverage_data$eid[!is.na(ukb_beverage_data$typical_diet_yesterday_f100020_1_0)],
                 ukb_beverage_data$eid[!is.na(ukb_beverage_data$typical_diet_yesterday_f100020_2_0)],
                 ukb_beverage_data$eid[!is.na(ukb_beverage_data$typical_diet_yesterday_f100020_3_0)],
                 ukb_beverage_data$eid[!is.na(ukb_beverage_data$typical_diet_yesterday_f100020_4_0)]))
ukb_beverage_data <- ukb_beverage_data[(ukb_beverage_data$eid %in% eids),]


eids <- unique(c(ukb_beverage_data$eid[ukb_beverage_data$daily_dietary_data_not_credible_f100026_0_0==0],
                 ukb_beverage_data$eid[ukb_beverage_data$daily_dietary_data_not_credible_f100026_1_0==0],
                 ukb_beverage_data$eid[ukb_beverage_data$daily_dietary_data_not_credible_f100026_2_0==0],
                 ukb_beverage_data$eid[ukb_beverage_data$daily_dietary_data_not_credible_f100026_3_0==0],
                 ukb_beverage_data$eid[ukb_beverage_data$daily_dietary_data_not_credible_f100026_4_0==0]))
ukb_beverage_data <- ukb_beverage_data[!(ukb_beverage_data$eid %in% eids),]


##pure milk##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("milk_intake_f100520_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("milk_intake_f100520_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}

# Create low-fat milk intake variables
for (i in 0:4) {
  milk_intake_col <- paste0("milk_intake_f100520_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  low_fat_milk_col <- paste0("low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[milk_intake_col]],
    NA
  )
}


# Create full-fat milk intake variables
for (i in 0:4) {
  milk_intake_col <- paste0("milk_intake_f100520_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  full_fat_milk_col <- paste0("full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[milk_intake_col]],
    NA
  )
}

##milk from porridge intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("porridge_intake_f100770_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("porridge_intake_f100770_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 200] <- 2
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create porridge milk intake variables
for (i in 0:4) {
  porridge_intake_col <- paste0("porridge_intake_f100770_", i, "_0")
  liquid_used_to_make_porridge_col <- paste0("liquid_used_to_make_porridge_f20105_", i, "_0")
  porridge_milk_col <- paste0("porridge_milk_intake_", i, "_0")
  
  ukb_beverage_data[[porridge_milk_col]] <- ifelse(
    ukb_beverage_data[[liquid_used_to_make_porridge_col]]==79,
    ukb_beverage_data[[porridge_intake_col]],
    NA
  )
}


# Create porridge low fat milk intake variables
for (i in 0:4) {
  porridge_milk_col <- paste0("porridge_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  porridge_low_fat_milk_col <- paste0("porridge_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[porridge_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[porridge_milk_col]],
    NA
  )
}


# Create porridge full fat milk intake variables
for (i in 0:4) {
  porridge_milk_col <- paste0("porridge_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  porridge_full_fat_milk_col <- paste0("porridge_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[porridge_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[porridge_milk_col]],
    NA
  )
}


##milk from cereal intake##
# total cereal intake
# List of cereal types
cereal_types <- c("muesli", "oat_crunch", "sweetened_cereal", "plain_cereal", 
                  "bran_cereal", "wholewheat_cereal", "other_cereal")

# Corresponding field codes
field_codes <- c("100800", "100810", "100820", "100830", "100840", "100850", "100860")

# Process each cereal type
for (j in 1:length(cereal_types)) {
  cereal_type <- cereal_types[j]
  field_code <- field_codes[j]
  
  # Set NA values based on typical diet yesterday
  for (i in 0:4) {
    ukb_beverage_data[[paste0(cereal_type, "_intake_f", field_code, "_", i, "_0")]][
      ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
    ] <- NA
  }
  
  # Convert specific values (200 to 2, 555 to 0.5) for cereal intake
  for (i in 0:4) {
    col_name <- paste0(cereal_type, "_intake_f", field_code, "_", i, "_0")
    ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 200] <- 2
    ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
  }
}


# Calculate total cereal intake for each instance
for (i in 0:4) {
  # Create a list of column names for this instance
  cols_to_sum <- sapply(cereal_types, function(type) {
    paste0(type, "_intake_f", field_codes[which(cereal_types == type)], "_", i, "_0")
  })
  
  # Calculate the sum, ignoring NA values
  ukb_beverage_data[[paste0("total_cereal_intake_", i, "_0")]] <- rowSums(
    ukb_beverage_data[, cols_to_sum], na.rm = T
  )
}


# Create cereal milk intake variables
for (i in 0:4) {
  total_cereal_intake_col <- paste0("total_cereal_intake_", i, "_0")
  milk_added_to_cereal_col <- paste0("milk_added_to_cereal_f100890_", i, "_0")
  cereal_milk_col <- paste0("cereal_milk_intake_", i, "_0")
  
  ukb_beverage_data[[cereal_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_added_to_cereal_col]]==1,
    ukb_beverage_data[[total_cereal_intake_col]],
    NA
  )
}


# Create cereal low fat milk intake variables
for (i in 0:4) {
  cereal_milk_col <- paste0("cereal_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  cereal_low_fat_milk_col <- paste0("cereal_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[cereal_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[cereal_milk_col]],
    NA
  )
}

# Create cereal full fat milk intake variables
for (i in 0:4) {
  cereal_milk_col <- paste0("cereal_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  cereal_full_fat_milk_col <- paste0("cereal_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[cereal_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[cereal_milk_col]],
    NA
  )
}


##milk from Instant coffee intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("instant_coffee_intake_f100250_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("instant_coffee_intake_f100250_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create Instant coffee milk intake variables
for (i in 0:4) {
  instant_coffee_intake_col <- paste0("instant_coffee_intake_f100250_", i, "_0")
  added_milk_to_instant_coffee_col <- paste0("added_milk_to_instant_coffee_f100260_", i, "_0")
  instant_coffee_milk_col <- paste0("instant_coffee_milk_intake_", i, "_0")
  
  ukb_beverage_data[[instant_coffee_milk_col]] <- ifelse(
    ukb_beverage_data[[added_milk_to_instant_coffee_col]]==1,
    ukb_beverage_data[[instant_coffee_intake_col]],
    NA
  )
}


# Create Instant coffee low fat milk intake variables
for (i in 0:4) {
  instant_coffee_milk_col <- paste0("instant_coffee_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  instant_coffee_low_fat_milk_col <- paste0("instant_coffee_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[instant_coffee_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[instant_coffee_milk_col]],
    NA
  )
}


# Create Instant coffee full fat milk intake variables
for (i in 0:4) {
  instant_coffee_milk_col <- paste0("instant_coffee_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  instant_coffee_full_fat_milk_col <- paste0("instant_coffee_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[instant_coffee_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[instant_coffee_milk_col]],
    NA
  )
}

##milk from filtered coffee intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("filtered_coffee_intake_f100270_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("filtered_coffee_intake_f100270_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create filtered_coffee milk intake variables
for (i in 0:4) {
  filtered_coffee_intake_col <- paste0("filtered_coffee_intake_f100270_", i, "_0")
  added_milk_to_filtered_coffee_col <- paste0("added_milk_to_filtered_coffee_f100280_", i, "_0")
  filtered_coffee_milk_col <- paste0("filtered_coffee_milk_intake_", i, "_0")
  
  ukb_beverage_data[[filtered_coffee_milk_col]] <- ifelse(
    ukb_beverage_data[[added_milk_to_filtered_coffee_col]]==1,
    ukb_beverage_data[[filtered_coffee_intake_col]],
    NA
  )
}


# Create filtered_coffee low fat milk intake variables
for (i in 0:4) {
  filtered_coffee_milk_col <- paste0("filtered_coffee_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  filtered_coffee_low_fat_milk_col <- paste0("filtered_coffee_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[filtered_coffee_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[filtered_coffee_milk_col]],
    NA
  )
}

# Create filtered_coffee full fat milk intake variables
for (i in 0:4) {
  filtered_coffee_milk_col <- paste0("filtered_coffee_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  filtered_coffee_full_fat_milk_col <- paste0("filtered_coffee_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[filtered_coffee_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[filtered_coffee_milk_col]],
    NA
  )
}

##milk from espresso intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("espresso_intake_f100310_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("espresso_intake_f100310_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create espresso milk intake variables
for (i in 0:4) {
  espresso_intake_col <- paste0("espresso_intake_f100310_", i, "_0")
  added_milk_to_espresso_col <- paste0("added_milk_to_espresso_f100320_", i, "_0")
  espresso_milk_col <- paste0("espresso_milk_intake_", i, "_0")
  
  ukb_beverage_data[[espresso_milk_col]] <- ifelse(
    ukb_beverage_data[[added_milk_to_espresso_col]]==1,
    ukb_beverage_data[[espresso_intake_col]],
    NA
  )
}


# Create espresso low fat milk intake variables
for (i in 0:4) {
  espresso_milk_col <- paste0("espresso_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  espresso_low_fat_milk_col <- paste0("espresso_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[espresso_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[espresso_milk_col]],
    NA
  )
}


# Create espresso full fat milk intake variables
for (i in 0:4) {
  espresso_milk_col <- paste0("espresso_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  espresso_full_fat_milk_col <- paste0("espresso_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[espresso_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[espresso_milk_col]],
    NA
  )
}

##milk from other coffee intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("other_coffee_type_f100330_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("other_coffee_type_f100330_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create other_coffee milk intake variables
for (i in 0:4) {
  other_coffee_intake_col <- paste0("other_coffee_type_f100330_", i, "_0")
  added_milk_to_other_coffee_col <- paste0("added_milk_to_other_coffee_type_f100350_", i, "_0")
  other_coffee_milk_col <- paste0("other_coffee_milk_intake_", i, "_0")
  
  ukb_beverage_data[[other_coffee_milk_col]] <- ifelse(
    ukb_beverage_data[[added_milk_to_other_coffee_col]]==1,
    ukb_beverage_data[[other_coffee_intake_col]],
    NA
  )
}


# Create other_coffee low fat milk intake variables
for (i in 0:4) {
  other_coffee_milk_col <- paste0("other_coffee_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  other_coffee_low_fat_milk_col <- paste0("other_coffee_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[other_coffee_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[other_coffee_milk_col]],
    NA
  )
}

# Create other_coffee full fat milk intake variables
for (i in 0:4) {
  other_coffee_milk_col <- paste0("other_coffee_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  other_coffee_full_fat_milk_col <- paste0("other_coffee_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[other_coffee_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[other_coffee_milk_col]],
    NA
  )
}

##milk from latte intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("latte_intake_f100300_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("latte_intake_f100300_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create latte low fat milk intake variables
for (i in 0:4) {
  latte_milk_col <- paste0("latte_intake_f100300_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  latte_low_fat_milk_col <- paste0("latte_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[latte_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[latte_milk_col]],
    NA
  )
}

# Create latte full fat milk intake variables
for (i in 0:4) {
  latte_milk_col <- paste0("latte_intake_f100300_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  latte_full_fat_milk_col <- paste0("latte_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[latte_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[latte_milk_col]],
    NA
  )
}


##milk from cappuccino intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("cappuccino_intake_f100290_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("cappuccino_intake_f100290_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create cappuccino low fat milk intake variables
for (i in 0:4) {
  cappuccino_milk_col <- paste0("cappuccino_intake_f100290_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  cappuccino_low_fat_milk_col <- paste0("cappuccino_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[cappuccino_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[cappuccino_milk_col]],
    NA
  )
}


# Create cappuccino full fat milk intake variables
for (i in 0:4) {
  cappuccino_milk_col <- paste0("cappuccino_intake_f100290_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  cappuccino_full_fat_milk_col <- paste0("cappuccino_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[cappuccino_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[cappuccino_milk_col]],
    NA
  )
}




##milk from standard_tea intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("standard_tea_intake_f100400_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("standard_tea_intake_f100400_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
}


# Create standard_tea milk intake variables
for (i in 0:4) {
  standard_tea_intake_col <- paste0("standard_tea_intake_f100400_", i, "_0")
  added_milk_to_standard_tea_col <- paste0("added_milk_to_standard_tea_f100460_", i, "_0")
  standard_tea_milk_col <- paste0("standard_tea_milk_intake_", i, "_0")
  
  ukb_beverage_data[[standard_tea_milk_col]] <- ifelse(
    ukb_beverage_data[[added_milk_to_standard_tea_col]]==1,
    ukb_beverage_data[[standard_tea_intake_col]],
    NA
  )
}


# Create standard_tea low fat milk intake variables
for (i in 0:4) {
  standard_tea_milk_col <- paste0("standard_tea_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  standard_tea_low_fat_milk_col <- paste0("standard_tea_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[standard_tea_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[standard_tea_milk_col]],
    NA
  )
}


# Create standard_tea full fat milk intake variables
for (i in 0:4) {
  standard_tea_milk_col <- paste0("standard_tea_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  standard_tea_full_fat_milk_col <- paste0("standard_tea_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[standard_tea_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[standard_tea_milk_col]],
    NA
  )
}


##milk from rooibos_tea intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("rooibos_tea_intake_f100410_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("rooibos_tea_intake_f100410_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
}


# Create rooibos_tea milk intake variables
for (i in 0:4) {
  rooibos_tea_intake_col <- paste0("rooibos_tea_intake_f100410_", i, "_0")
  added_milk_to_rooibos_tea_col <- paste0("added_milk_to_rooibos_tea_f100480_", i, "_0")
  rooibos_tea_milk_col <- paste0("rooibos_tea_milk_intake_", i, "_0")
  
  ukb_beverage_data[[rooibos_tea_milk_col]] <- ifelse(
    ukb_beverage_data[[added_milk_to_rooibos_tea_col]]==1,
    ukb_beverage_data[[rooibos_tea_intake_col]],
    NA
  )
}


# Create rooibos_tea low fat milk intake variables
for (i in 0:4) {
  rooibos_tea_milk_col <- paste0("rooibos_tea_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  rooibos_tea_low_fat_milk_col <- paste0("rooibos_tea_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[rooibos_tea_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2102, 2103),
    ukb_beverage_data[[rooibos_tea_milk_col]],
    NA
  )
}

# Create rooibos_tea full fat milk intake variables
for (i in 0:4) {
  rooibos_tea_milk_col <- paste0("rooibos_tea_milk_intake_", i, "_0")
  milk_type_col <- paste0("type_milk_consumed_f100920_", i, "_0")
  rooibos_tea_full_fat_milk_col <- paste0("rooibos_tea_full_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[rooibos_tea_full_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(2104),
    ukb_beverage_data[[rooibos_tea_milk_col]],
    NA
  )
}

names(ukb_beverage_data)



##milk from yogurt intake##
# Set NA values based on typical diet yesterday
for (i in 0:4) {
  ukb_beverage_data[[paste0("yogurt_intake_f102090_", i, "_0")]][
    ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
  ] <- NA
}

# Convert specific values (600 to 6, 555 to 0.5) for milk intake
for (i in 0:4) {
  col_name <- paste0("yogurt_intake_f102090_", i, "_0")
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 300] <- 3
  ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
}


# Create yogurt low fat milk intake variables
for (i in 0:4) {
  yogurt_milk_col <- paste0("yogurt_intake_f102090_", i, "_0")
  milk_type_col <- paste0("type_of_yogurt_eaten_f20106_", i, "_0")
  yogurt_low_fat_milk_col <- paste0("yogurt_low_fat_milk_intake_", i, "_0")
  
  ukb_beverage_data[[yogurt_low_fat_milk_col]] <- ifelse(
    ukb_beverage_data[[milk_type_col]] %in% c(210),
    ukb_beverage_data[[yogurt_milk_col]],
    NA
  )
}



# List of variables to process
variables <- c(
  "drinking_water_intake_f100150",
  "low_calorie_drink_intake_f100160",
  "fizzy_drink_intake_f100170",
  "squash_intake_f100180",
  "orange_juice_intake_f100190",
  "grapefruit_juice_intake_f100200",
  "pure_fruitvegetable_juice_intake_f100210",
  "fruit_smoothie_intake_f100220",
  "dairy_smoothie_intake_f100230",
  "green_tea_intake_f100420",
  "herbal_tea_intake_f100430",
  "other_tea_intake_f100440",
  "added_milk_to_standard_tea_f100460",
  "other_nonalcoholic_drinks_f100510",
  "flavoured_milk_intake_f100530",
  "low_calorie_hot_chocolate_intake_f100540",
  "hot_chocolate_intake_f100550",
  "other_drink_intake_f100560"
)

# Process each variable
for (var in variables) {
  # Set NA values based on typical diet yesterday
  for (i in 0:4) {
    ukb_beverage_data[[paste0(var, "_", i, "_0")]][
      ukb_beverage_data[[paste0("typical_diet_yesterday_f100020_", i, "_0")]] == 0
    ] <- NA
  }
  
  # Convert specific values (600 to 6, 555 to 0.5) for intake
  for (i in 0:4) {
    col_name <- paste0(var, "_", i, "_0")
    ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 600] <- 6
    ukb_beverage_data[[col_name]][ukb_beverage_data[[col_name]] == 555] <- 0.5
  }
}




############## Milk intake ######################
names(ukb_beverage_data)
milk_columns <- grep("milk_intake", names(ukb_beverage_data), value = TRUE)
milk_columns <- milk_columns[!grepl("fat_milk_intake", milk_columns)]
milk_columns

process_milk_intake <- function(data, var_prefix) {
  cols <- paste0(var_prefix, "_", 0:4, "_0")
  
  if (var_prefix == "milk_intake_f100520") {
    new_var_name <- "milk_intake"
  } else if (var_prefix == "flavoured_milk_intake_f100530") {
    new_var_name <- "flavoured_milk_intake"
  } else {
    new_var_name <- gsub("_[0-9]+_[0-9]+$", "", var_prefix)
  }
  
  data[[new_var_name]] <- rowMeans(data[cols], na.rm = TRUE)
  return(data)
}

ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "milk_intake_f100520")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "porridge_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "cereal_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "flavoured_milk_intake_f100530")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "instant_coffee_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "filtered_coffee_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "espresso_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "other_coffee_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "standard_tea_milk_intake")
ukb_beverage_data <- process_milk_intake(ukb_beverage_data, "rooibos_tea_milk_intake")

new_vars <- c("milk_intake", "porridge_milk_intake", "cereal_milk_intake", 
              "flavoured_milk_intake", "instant_coffee_milk_intake", 
              "filtered_coffee_milk_intake", "espresso_milk_intake", 
              "other_coffee_milk_intake", "standard_tea_milk_intake", 
              "rooibos_tea_milk_intake")

lapply(ukb_beverage_data[new_vars], summary)





############## Low fat milk intake ######################
names(ukb_beverage_data)
milk_columns <- grep("low_fat_milk_intake", names(ukb_beverage_data), value = TRUE)
milk_columns

process_milk_intake <- function(data, var_prefix) {
  cols <- paste0(var_prefix, "_", 0:4, "_0")
  new_var_name <- gsub("_[0-9]+_[0-9]+$", "", var_prefix)
  data[[new_var_name]] <- rowMeans(data[cols], na.rm = TRUE)
  return(data)
}

low_fat_vars <- c(
  "low_fat_milk_intake",
  "porridge_low_fat_milk_intake",
  "cereal_low_fat_milk_intake",
  "instant_coffee_low_fat_milk_intake",
  "filtered_coffee_low_fat_milk_intake",
  "espresso_low_fat_milk_intake",
  "other_coffee_low_fat_milk_intake",
  "latte_low_fat_milk_intake",
  "cappuccino_low_fat_milk_intake",
  "standard_tea_low_fat_milk_intake",
  "rooibos_tea_low_fat_milk_intake",
  "yogurt_low_fat_milk_intake"
)

for (var in low_fat_vars) {
  ukb_beverage_data <- process_milk_intake(ukb_beverage_data, var)
}

lapply(ukb_beverage_data[low_fat_vars], summary)


############## full fat milk intake ######################
names(ukb_beverage_data)
milk_columns <- grep("full_fat_milk_intake", names(ukb_beverage_data), value = TRUE)
milk_columns

process_milk_intake <- function(data, var_prefix) {
  cols <- paste0(var_prefix, "_", 0:4, "_0")
  new_var_name <- gsub("_[0-9]+_[0-9]+$", "", var_prefix)
  data[[new_var_name]] <- rowMeans(data[cols], na.rm = TRUE)
  return(data)
}

full_fat_vars <- c(
  "full_fat_milk_intake",
  "porridge_full_fat_milk_intake",
  "cereal_full_fat_milk_intake",
  "instant_coffee_full_fat_milk_intake",
  "filtered_coffee_full_fat_milk_intake",
  "espresso_full_fat_milk_intake",
  "other_coffee_full_fat_milk_intake",
  "latte_full_fat_milk_intake",
  "cappuccino_full_fat_milk_intake",
  "standard_tea_full_fat_milk_intake",
  "rooibos_tea_full_fat_milk_intake"
)

for (var in full_fat_vars) {
  ukb_beverage_data <- process_milk_intake(ukb_beverage_data, var)
}

lapply(ukb_beverage_data[full_fat_vars], summary)


############## Yogurt ######################
names(ukb_beverage_data)
yogurt_intake_cols <- paste0("yogurt_intake_f102090_", 0:4, "_0")
ukb_beverage_data$yogurt_intake <- rowMeans(ukb_beverage_data[yogurt_intake_cols], na.rm = TRUE)
summary(ukb_beverage_data$yogurt_intake)





############## Coffee ######################
names(ukb_beverage_data)
coffee_columns <- grep("coffee", names(ukb_beverage_data), value = TRUE)
coffee_columns <- coffee_columns[!grepl("milk", coffee_columns)]
coffee_columns

process_beverage_intake <- function(data, var_prefix) {
  base_name <- gsub("_f[0-9]+$", "", var_prefix)
  cols <- paste0(var_prefix, "_", 0:4, "_0")
  new_var_name <- base_name
  data[[new_var_name]] <- rowMeans(data[cols], na.rm = TRUE)
  return(data)
}

coffee_vars <- c(
  "instant_coffee_intake_f100250",
  "filtered_coffee_intake_f100270",
  "other_coffee_type_f100330",
  "latte_intake_f100300",
  "espresso_intake_f100310",
  "cappuccino_intake_f100290"
)

for (var in coffee_vars) {
  ukb_beverage_data <- process_beverage_intake(ukb_beverage_data, var)
}

new_vars <- gsub("_f[0-9]+$", "", coffee_vars)
lapply(ukb_beverage_data[new_vars], summary)



############## Tea ######################
names(ukb_beverage_data)
tea_columns <- grep("tea", names(ukb_beverage_data), value = TRUE)
tea_columns <- tea_columns[!grepl("milk", tea_columns)]
tea_columns

process_beverage_intake <- function(data, var_prefix) {
  base_name <- gsub("_f[0-9]+$", "", var_prefix)
  cols <- paste0(var_prefix, "_", 0:4, "_0")
  new_var_name <- base_name
  data[[new_var_name]] <- rowMeans(data[cols], na.rm = TRUE)
  return(data)
}

tea_vars <- c(
  "standard_tea_intake_f100400",
  "rooibos_tea_intake_f100410",
  "green_tea_intake_f100420",
  "herbal_tea_intake_f100430",
  "other_tea_intake_f100440"
)

for (var in tea_vars) {
  ukb_beverage_data <- process_beverage_intake(ukb_beverage_data, var)
}

new_tea_vars <- gsub("_f[0-9]+$", "", tea_vars)
lapply(ukb_beverage_data[new_tea_vars], summary)




############## Drinks ######################
names(ukb_beverage_data)
beverage_columns <- names(ukb_beverage_data)[c(37:81,222:236)]
beverage_columns

process_beverage_intake <- function(data, var_prefix) {
  base_name <- gsub("_f[0-9]+$", "", var_prefix)
  cols <- paste0(var_prefix, "_", 0:4, "_0")
  new_var_name <- base_name
  data[[new_var_name]] <- rowMeans(data[cols], na.rm = TRUE)
  return(data)
}

beverage_vars <- c(
  "drinking_water_intake_f100150",
  "low_calorie_drink_intake_f100160",
  "fizzy_drink_intake_f100170",
  "squash_intake_f100180",
  "orange_juice_intake_f100190",
  "grapefruit_juice_intake_f100200",
  "pure_fruitvegetable_juice_intake_f100210",
  "fruit_smoothie_intake_f100220",
  "dairy_smoothie_intake_f100230",
  "low_calorie_hot_chocolate_intake_f100540",
  "hot_chocolate_intake_f100550",
  "other_drink_intake_f100560"
)

for (var in beverage_vars) {
  ukb_beverage_data <- process_beverage_intake(ukb_beverage_data, var)
}

new_beverage_vars <- gsub("_f[0-9]+$", "", beverage_vars)
lapply(ukb_beverage_data[new_beverage_vars], summary)






######################## Drink Catergroies##########################
#SSB(arbonated drinks, fizzy drinks and squash)
names(ukb_beverage_data)
SSB_intake_cols <- names(ukb_beverage_data)[535:536]
ukb_beverage_data$SSB_intake <- rowSums(ukb_beverage_data[SSB_intake_cols], na.rm = TRUE)
ukb_beverage_data$SSB_intake[is.na(ukb_beverage_data$SSB_intake)] <- 0
summary(ukb_beverage_data$SSB_intake)



#ASB (low-calorie drinks)
names(ukb_beverage_data)
ASB_intake_cols <- names(ukb_beverage_data)[534]
ukb_beverage_data$ASB_intake <- rowSums(ukb_beverage_data[ASB_intake_cols], na.rm = TRUE)
ukb_beverage_data$ASB_intake[is.na(ukb_beverage_data$ASB_intake)] <- 0
summary(ukb_beverage_data$ASB_intake)


#NJ (pure fruit/vegetable juice, grapefruit juice and orange juice as NJ)
names(ukb_beverage_data)
NJ_intake_cols <- names(ukb_beverage_data)[537:539]
ukb_beverage_data$NJ_intake <- rowSums(ukb_beverage_data[NJ_intake_cols], na.rm = TRUE)
ukb_beverage_data$NJ_intake[is.na(ukb_beverage_data$NJ_intake)] <- 0
summary(ukb_beverage_data$NJ_intake)


#Low_fat Milk 
names(ukb_beverage_data)
low_fat_milk_intake_cols <- names(ukb_beverage_data)[c(498,501:509)]
ukb_beverage_data$lowfat_milk_intake <- rowSums(ukb_beverage_data[low_fat_milk_intake_cols], na.rm = TRUE)
ukb_beverage_data$lowfat_milk_intake[is.na(ukb_beverage_data$lowfat_milk_intake)] <- 0
summary(ukb_beverage_data$lowfat_milk_intake)



#full_fat Milk 
names(ukb_beverage_data)
full_fat_milk_intake_cols <- names(ukb_beverage_data)[c(510,513:520)]
ukb_beverage_data$fullfat_milk_intake <- rowSums(ukb_beverage_data[full_fat_milk_intake_cols], na.rm = TRUE)
ukb_beverage_data$fullfat_milk_intake[is.na(ukb_beverage_data$fullfat_milk_intake)] <- 0
summary(ukb_beverage_data$fullfat_milk_intake)


#plain water 
names(ukb_beverage_data)
plain_water_intake_cols <- names(ukb_beverage_data)[37:41]
ukb_beverage_data$plain_water_intake <- rowMeans(ukb_beverage_data[plain_water_intake_cols], na.rm = TRUE)
ukb_beverage_data$plain_water_intake[is.na(ukb_beverage_data$plain_water_intake)] <- 0
summary(ukb_beverage_data$plain_water_intake)


#coffee  
names(ukb_beverage_data)
coffee_intake_cols <- names(ukb_beverage_data)[522:527]
ukb_beverage_data$coffee_intake <- rowSums(ukb_beverage_data[coffee_intake_cols], na.rm = TRUE)
ukb_beverage_data$coffee_intake[is.na(ukb_beverage_data$coffee_intake)] <- 0
summary(ukb_beverage_data$coffee_intake)


#tea  
names(ukb_beverage_data)
tea_intake_cols <- names(ukb_beverage_data)[528:532]
ukb_beverage_data$tea_intake <- rowSums(ukb_beverage_data[tea_intake_cols], na.rm = TRUE)
ukb_beverage_data$tea_intake[is.na(ukb_beverage_data$tea_intake)] <- 0
summary(ukb_beverage_data$tea_intake)



####################################Calcuating Volume#################################################
names(ukb_beverage_data)

ukb_beverage_data$tea_volume <- ukb_beverage_data$tea_intake*190
summary(ukb_beverage_data$tea_volume)

coffee_190ml <- data.frame(
  instant_coffee = ukb_beverage_data$instant_coffee_intake,
  filtered_coffee = ukb_beverage_data$filtered_coffee_intake,
  other_coffee = ukb_beverage_data$other_coffee_type,
  cappuccino = ukb_beverage_data$cappuccino_intake,
  latte = ukb_beverage_data$latte_intake
)

ukb_beverage_data$coffee_volume <- rowSums(coffee_190ml, na.rm = TRUE) * 190 + 
  ifelse(is.na(ukb_beverage_data$espresso_intake), 0, ukb_beverage_data$espresso_intake) * 30

summary(ukb_beverage_data$coffee_volume)

ukb_beverage_data$plain_water_volume <- ukb_beverage_data$plain_water_intake*250
summary(ukb_beverage_data$plain_water_volume)


ukb_beverage_data$SSB_volume <- ukb_beverage_data$SSB_intake*330
summary(ukb_beverage_data$SSB_volume)

ukb_beverage_data$ASB_volume <- ukb_beverage_data$ASB_intake*330
summary(ukb_beverage_data$ASB_volume)

ukb_beverage_data$NJ_volume <- ukb_beverage_data$NJ_intake*250
summary(ukb_beverage_data$NJ_volume)

cereal_lowfat <- data.frame(
  porridge = ukb_beverage_data$porridge_low_fat_milk_intake,
  cereal = ukb_beverage_data$cereal_low_fat_milk_intake
) * 100

coffee_lowfat <- data.frame(
  instant = ukb_beverage_data$instant_coffee_low_fat_milk_intake,
  filtered = ukb_beverage_data$filtered_coffee_low_fat_milk_intake,
  espresso = ukb_beverage_data$espresso_low_fat_milk_intake,
  other = ukb_beverage_data$other_coffee_low_fat_milk_intake
) * 25

latte_capp_lowfat <- data.frame(
  latte = ukb_beverage_data$latte_low_fat_milk_intake,
  cappuccino = ukb_beverage_data$cappuccino_low_fat_milk_intake
) * 160


tea_lowfat <- data.frame(
  standard = ukb_beverage_data$standard_tea_low_fat_milk_intake,
  rooibos = ukb_beverage_data$rooibos_tea_low_fat_milk_intake
) * 35

direct_lowfat <- data.frame(
  milk = ukb_beverage_data$low_fat_milk_intake
)

ukb_beverage_data$low_fat_milk_volume <- 
  rowSums(cereal_lowfat, na.rm = TRUE) +
  rowSums(coffee_lowfat, na.rm = TRUE) +
  rowSums(latte_capp_lowfat, na.rm = TRUE) +
  rowSums(tea_lowfat, na.rm = TRUE) +
  rowSums(direct_lowfat, na.rm = TRUE)


cereal_fullfat <- data.frame(
  porridge = ukb_beverage_data$porridge_full_fat_milk_intake,
  cereal = ukb_beverage_data$cereal_full_fat_milk_intake
) * 100

coffee_fullfat <- data.frame(
  instant = ukb_beverage_data$instant_coffee_full_fat_milk_intake,
  filtered = ukb_beverage_data$filtered_coffee_full_fat_milk_intake,
  espresso = ukb_beverage_data$espresso_full_fat_milk_intake,
  other = ukb_beverage_data$other_coffee_full_fat_milk_intake
) * 25

latte_capp_fullfat <- data.frame(
  latte = ukb_beverage_data$latte_full_fat_milk_intake,
  cappuccino = ukb_beverage_data$cappuccino_full_fat_milk_intake
) * 160

tea_fullfat <- data.frame(
  standard = ukb_beverage_data$standard_tea_full_fat_milk_intake,
  rooibos = ukb_beverage_data$rooibos_tea_full_fat_milk_intake
) * 35

direct_fullfat <- data.frame(
  milk = ukb_beverage_data$full_fat_milk_intake
)

ukb_beverage_data$full_fat_milk_volume <- 
  rowSums(cereal_fullfat, na.rm = TRUE) +
  rowSums(coffee_fullfat, na.rm = TRUE) +
  rowSums(latte_capp_fullfat, na.rm = TRUE) +
  rowSums(tea_fullfat, na.rm = TRUE) +
  rowSums(direct_fullfat, na.rm = TRUE)

summary(ukb_beverage_data$low_fat_milk_volume)
summary(ukb_beverage_data$full_fat_milk_volume)


names(ukb_beverage_data)
water_vars <- c("coffee_consumed_f100240_0_0",
                "coffee_consumed_f100240_1_0",
                "coffee_consumed_f100240_2_0",
                "coffee_consumed_f100240_3_0",
                "coffee_consumed_f100240_4_0")

non_missing_count <- rowSums(!is.na(ukb_beverage_data[, water_vars]))
filtered_data <- ukb_beverage_data[non_missing_count >= 2, ]
eids_with_2plus_nonmissing_diet_recall_info <- filtered_data$eid




########################################50w coffee tea water data#######################################################
coffee_tea_alternative_data <- read.csv("C:/Users/zhangjie/Desktop/ckm_beverage_mortality/data/ukb_beverage_data")
names(coffee_tea_alternative_data)

#tea 
coffee_tea_alternative_data$tea_intake <- coffee_tea_alternative_data$tea_intake_f1488_0_0 
coffee_tea_alternative_data$tea_intake[coffee_tea_alternative_data$tea_intake==-1 | coffee_tea_alternative_data$tea_intake==-3] <- NA 
coffee_tea_alternative_data$tea_intake[coffee_tea_alternative_data$tea_intake==-10] <- 0.5
summary(coffee_tea_alternative_data$tea_intake)

#coffee
coffee_tea_alternative_data$coffee_intake <- coffee_tea_alternative_data$coffee_intake_f1498_0_0 
coffee_tea_alternative_data$coffee_intake[coffee_tea_alternative_data$coffee_intake==-1 | coffee_tea_alternative_data$coffee_intake==-3] <- NA 
coffee_tea_alternative_data$coffee_intake[coffee_tea_alternative_data$coffee_intake==-10] <- 0.5 
summary(coffee_tea_alternative_data$coffee_intake)


#water
coffee_tea_alternative_data$water_intake <- coffee_tea_alternative_data$water_intake_f1528_0_0 
coffee_tea_alternative_data$water_intake[coffee_tea_alternative_data$coffee_intake==-1 | coffee_tea_alternative_data$water_intake==-3] <- NA 
coffee_tea_alternative_data$water_intake[coffee_tea_alternative_data$water_intake==-10] <- 0.5 
summary(coffee_tea_alternative_data$water_intake)
