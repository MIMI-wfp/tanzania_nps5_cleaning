################################################################################
########### SCRIPT FOR EXTRACTING AND CLEANING CONSUMPTION QUANTITIES ##########
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 14-08-2024

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

# Some households used the old food consumption module (j1) - therefore need to 
# read this data in too for pre-processing:

# Get edible portions:
edible_portionj1 <- read_csv("raw_data/ediblep_j1.csv") %>% 
  dplyr::select(itemcode, mean_EP) %>% 
  rename(item_code = itemcode,
         edible_portion = mean_EP)

# Get food groups data:
food_groupsj1 <- read_csv("raw_data/food_groups_j1.csv") %>% 
  dplyr::select(itemcode, food_group) %>% 
  rename(item_code = itemcode)

# Read in unit conversion factors: 
unit_convj1 <- read_csv("raw_data/unitconv_j1.csv") %>% 
  dplyr::select(itemcode, cons_unit, conv_fac) %>% 
  rename(item_code = itemcode,
         unit = cons_unit)

# Read in food_items dictionary: 
food_itemsj1 <- read_csv("raw_data/food-id_j1.csv") %>% 
  rename(item_code = itemcode)

# Read in food consumption module: 
tza_food_consumption_j1 <- read_csv("raw_data/hh_sec_j1.csv") %>% 
  dplyr::select(y5_hhid, 
                itemcode,
                hh_j01, # Did household consume? 
                hh_j02_1, # Quantity consumed (UNIT) 
                hh_j02_2) %>%  # Quantity consumed (QUANTITY) 
  rename(hhid = y5_hhid,
         item_code = itemcode,
         consumed = hh_j01,
         unit = hh_j02_1,
         quantity = hh_j02_2) %>%
  filter(consumed == 1) %>%  # Only keep entries where household consumed
  select(-consumed) %>% # Drop the consumed column
  left_join(food_itemsj1, by = "item_code") %>% # Join food item names
  left_join(edible_portionj1, by = "item_code") %>%  # Join edible portion ratio
  left_join(unit_convj1, by = c("item_code", "unit")) %>%  # Join unit conversion factors
  select(hhid, item_code, itemname, everything())  # Re-order variables

rm(edible_portionj1, food_itemsj1)

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

# CALCULATE CONSUMPTION QUANTITIES FOR MODULE J1: 

j1_na <- tza_food_consumption_j1 %>% 
  filter(is.na(conv_fac)) %>% 
  dplyr::select(hhid) %>% 
  distinct()

# There were 61 households that had insufficient data to calculate consumption
# quantities, therefore remove these households from the dataset:
tza_food_consumption_j1 <- tza_food_consumption_j1 %>% 
  anti_join(j1_na, by = "hhid")

tza_food_consumption_j1 <- tza_food_consumption_j1 %>% 
  mutate(quantity_g = quantity * conv_fac * edible_portion) %>%
  mutate(quantity_100g = quantity_g / 100) %>%
  dplyr::select(hhid, item_code, itemname, quantity_g, quantity_100g)

# Append the two datasets together:
tza_food_consumption <- bind_rows(tza_food_consumption, tza_food_consumption_j1)

rm(tza_food_consumption_j1, j1_na)

#-------------------------------------------------------------------------------

# CALCULATE CONSUMPTION PER DAY:

# Firstly mutate values so that they are presented per day
tza_food_consumption <- tza_food_consumption %>% 
  mutate(quantity_g = quantity_g / 7,
         quantity_100g = quantity_100g / 7)

# Then check for any negative values:
tza_food_consumption %>% 
  filter(quantity_g < 0 | quantity_100g < 0) # No negative values listed.

#-------------------------------------------------------------------------------

# ADD FOOD GROUPS: 

food_groups <- bind_rows(food_groups, food_groupsj1) %>% 
  distinct() 

# Add food-groups:  
tza_food_consumption <- tza_food_consumption %>% 
  left_join(food_groups, by = "item_code") %>%
  dplyr::select(hhid, item_code, quantity_100g, food_group, quantity_g)

# Correct food group names so that they are consistent with the other countries: 
tza_food_consumption <- tza_food_consumption %>%
  mutate(food_group = case_when(
    food_group == "Grains, roots, and tubers" ~ "grains_roots_tubers",
    food_group == "Nuts and seeds" ~ "nuts_seeds",
    food_group == "Dairy" ~ "dairy",
    food_group == "Meat, poultry, and fish" ~ "meat_poultry_fish",
    food_group == "Eggs" ~ "eggs",
    food_group == "Dark leafy greens and vegetables" ~ "green_leafy_veg",
    food_group == "Other Vitamin A-rich fruits and vegetables" ~ "vita_fruit_veg",
    food_group == "Other vegetables" ~ "other_veg",
    food_group == "Other fruits" ~ "other_fruit",
    food_group == "Misc" ~ "misc",
    TRUE ~ food_group
  ))

#-------------------------------------------------------------------------------

# WRITE DATA: 

# write_csv(tza_food_consumption, "processed_data/tza_nps2021_food_consumption.csv")

rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################






