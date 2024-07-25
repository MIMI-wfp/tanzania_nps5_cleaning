################################################################################
########### SCRIPT FOR EXTRACTING AND CLEANING CONSUMPTION QUANTITIES ##########
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 23-07-2024

# In this script, I will extract and clean food consumption data from the Tanzania
# NPS Wave 5:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# After cleaning, I will calculate household consumption quantities for each food
# item. This processed data will be used to generate the MIMI base models.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr", "readxl", "ggplot2",
                 "gtExtras")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA: 

# Get edible portions: 

edible_portion <- read_excel("raw_data/TNPSW5_averages.xlsx", 
                      sheet = "wave5_NCT_v1.0") %>% 
  dplyr::select(itemcode, Edible_factor_in_FCT) %>% 
  rename(item_code = itemcode,
         edible_portion = Edible_factor_in_FCT) %>% 
  mutate(edible_portion = as.numeric(edible_portion))

# Get food group data: 
food_groups <- read_csv("raw_data/food_groups.csv") %>% 
  dplyr::select(itemcode, food_group) %>% 
  rename(item_code = itemcode)

# Read in food items dictionary: 
food_items <- read_csv("raw_data/food-id.csv") %>% 
  rename(item_code = itemcode)

# Read in unit conversion factors: 
unit_conv <- read_csv("raw_data/unitconv.csv") %>% 
  dplyr::select(itemcode, cons_unit, conv_fac) %>% 
  rename(item_code = itemcode,
         unit = cons_unit) 

# Read in new food consumption module: 
tza_food_consumption <- read_csv("raw_data/hh_sec_ja1.csv") %>% 
  dplyr::select(y5_hhid, 
                itemcode, 
                hh_ja01, # Did household consume? 
                hh_ja02_1, # Quantity consumed (UNIT)
                hh_ja02_2, # Quantity consumed (QUANTITY)
                hh_ja02b_1, # How many kg/g/l/ml was the UNIT (estimated)
                hh_ja02b_2, # How many kg/g/l/ml was the QUANTITY (estimated)
                hh_ja09_1, # UNIT (weighed)
                hh_ja09_2) %>% # QUANTITY (weighed)
  rename(hhid = y5_hhid,
         item_code = itemcode,
         consumed = hh_ja01,
         unit = hh_ja02_1,
         quantity = hh_ja02_2,
         est_metric_unit = hh_ja02b_1, 
         est_metric_quantity = hh_ja02b_2,
         weighed_unit = hh_ja09_1,
         weighed_quantity = hh_ja09_2) %>% 
  filter(consumed == 1) %>%  # Only keep entries where household consumed
  select(-consumed) %>% # Drop the consumed column
  left_join(food_items, by = "item_code") %>% # Join food item names
  left_join(edible_portion, by = "item_code") %>%  # Join edible portion ratio
  left_join(unit_conv, by = c("item_code", "unit")) %>%  # Join unit conversion factors
  select(hhid, item_code, itemname, everything())  # Re-order variables
  
rm(edible_portion, food_items)

#-------------------------------------------------------------------------------

# CALCULATE CONSUMPTION QUANTITIES

# Initially calculate consumption quantities for households reporting consumption
# in metric units (1-4):

tza_food_consumption <- tza_food_consumption %>% 
  mutate(quantity_g = case_when(
    unit == 1 | unit == 2 | unit == 3 | unit == 4 ~ quantity * conv_fac * edible_portion,
    TRUE ~ NA_real_
  )) %>% 
  dplyr::select(-unit, -quantity, -conv_fac) # Drop the unit and quantity columns

# When other units were reported, food items were either weighed, or if not, the
# metric quantities were estimated:

# Use actual weights preferentially
tza_food_consumption <- tza_food_consumption %>% 
  rename(unit = weighed_unit) %>% 
  left_join(unit_conv, by = c("item_code", "unit")) %>% 
  mutate(quantity_g = case_when(
    !is.na(weighed_quantity) & is.na(quantity_g) ~ weighed_quantity * conv_fac * edible_portion,
    TRUE ~ quantity_g
  )) %>% 
  dplyr::select(-unit, -weighed_quantity, -conv_fac)

# Then finally if neither of the above were available, use estimated metric quantities: 
tza_food_consumption <- tza_food_consumption %>% 
  rename(unit = est_metric_unit) %>%
  left_join(unit_conv, by = c("item_code", "unit")) %>%
  mutate(quantity_g = case_when(
    !is.na(est_metric_quantity) & is.na(quantity_g) ~ est_metric_quantity * conv_fac * edible_portion,
    TRUE ~ quantity_g
  )) %>%
  dplyr::select(-unit, -est_metric_quantity, -conv_fac, -edible_portion) %>% 
  mutate(quantity_100g = quantity_g / 100)

rm(unit_conv)

#-------------------------------------------------------------------------------

# CLEAN INTAKE QUANTITIES: 

# Firstly mutate values so that they are presented per AFE, per day
tza_food_consumption <- tza_food_consumption %>% 
  mutate(quantity_g = quantity_g / 7,
         quantity_100g = quantity_100g / 7)

# Then check for any negative values:
tza_food_consumption %>% 
  filter(quantity_g < 0 | quantity_100g < 0) # No negative values listed.

# FILTER OUT EXTREME OUTLIERS:

# To do this, firstly filter out extreme values by applying log10 trans:
tza_food_consumption <- tza_food_consumption %>% 
  mutate(log_quantity_g = log10(quantity_g))

# Generate cut points for values that are >+2SDs from the mean intake of each food item:
quant_cutpoints <- tza_food_consumption %>% 
  group_by(item_code) %>% 
  summarise(mean_log = mean(log_quantity_g, na.rm = TRUE),
            sd_log = sd(log_quantity_g, na.rm = TRUE)) %>% 
  mutate(upper_cut = mean_log + 2*sd_log) %>% 
  dplyr::select(item_code, upper_cut)

# Apply the cut points to the data, replacing quantities above the cut-point with NA: 
tza_food_consumption <- tza_food_consumption %>% 
  left_join(quant_cutpoints, by = "item_code") %>% 
  mutate(quantity_g = case_when(
    log_quantity_g > upper_cut ~ NA_real_,
    TRUE ~ quantity_g
  )) %>% 
  dplyr::select(-log_quantity_g, -upper_cut)

# If quantity_g is NA, then quantity_100g will also be NA:
tza_food_consumption <- tza_food_consumption %>% 
  mutate(quantity_100g = case_when(
    is.na(quantity_g) ~ NA_real_,
    TRUE ~ quantity_100g))

# Replace NA values with median reported intake: 
tza_food_consumption <- tza_food_consumption %>% 
  group_by(item_code) %>% 
  mutate(quantity_g = ifelse(is.na(quantity_g), median(quantity_g, na.rm = TRUE), quantity_g),
         quantity_100g = ifelse(is.na(quantity_100g), median(quantity_100g, na.rm = TRUE), quantity_100g))

#-------------------------------------------------------------------------------

# CHECK INTAKE DISTRIBUTIONS

# Check intake distributions of top 10 consumed food items as these are going to 
# be the most influential on the base model: 
top10_consumed <- tza_food_consumption %>% 
  group_by(item_code, itemname) %>% 
  summarise(mean_intake = round(mean(quantity_g, na.rm = TRUE), digits = 1),
            sd_intake = round(sd(quantity_g, na.rm = TRUE), digits = 1),
            median_intake = round(median(quantity_g, na.rm = TRUE), digits = 1),
            n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  head(10)

top10_consumed %>% 
  dplyr::select(itemname, mean_intake, sd_intake, median_intake, n) %>% 
  rename(`Food Item` = itemname,
         `Mean intake (g)` = mean_intake,
         `S.D.` = sd_intake,
         `Median intake (g)` = median_intake,
         `n households` = n) %>% gt() 

# The values all appear plausible, I will additionally produce histograms as an 
# additional check: 

intake_histogram <- function(code, itemname) {
  tza_food_consumption %>% 
    filter(item_code == code) %>% 
    ggplot(aes(x = quantity_g)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    # xlim(0, 600) + ylim(0,2000) +
    labs(title = paste("Histogram of Intake Quantity for:", itemname),
         x = "Intake Quantity (grams), per AFE/day",
         y = "Frequency")
}

# Store the outputs of this loop function in a list:
top10_histograms <- lapply(1:10, function(i) intake_histogram(top10_consumed$item_code[i], 
                                                              top10_consumed$itemname[i]))

# Arrange these histograms into a single figure:
top10_histograms <- do.call(gridExtra::grid.arrange, c(top10_histograms, ncol = 2))

# These intake distributions look plausible and follow an expected distribution 
# (normal distribution with a right skew).

#-------------------------------------------------------------------------------

# RENAME VARIABLES AND SAVE DATA: 
tza_food_consumption <- tza_food_consumption %>% 
  left_join(food_groups, by = "item_code") %>%
  dplyr::select(hhid, item_code, quantity_100g, food_group, quantity_g)

write_csv(tza_food_consumption, "processed_data/tza_nps2021_food_consumption.csv")

rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################






