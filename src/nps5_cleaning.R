################################################################################
############## SCRIPT FOR CLEANING RAW DATA FROM THE TANZANIA NPS-5 ############
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 04-07-2024

# In this script, I will clean the raw data from the Tanzania NPS-5 survey: 
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# After cleaning, I will generate MIMI base models for Tanzania.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr", "readxl", "ggplot2")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# Calculate AFE for households in survey: 
source("src/afe_calculation.R")

# Note that - only 4,613 of the households have the demographic data required to 
# calculate AFE, will need to check survey documentation to find out the reason 
# why.

#-------------------------------------------------------------------------------

# READ IN REQUIRED DATA TO CALCULATE CONSUMPTION QUANTITIES: 

# Read in food items dictionary: 
food_items <- read_csv("raw_data/food-id.csv") %>% 
  rename(item_code = itemcode)

# Read in edible portion ratios:
edible_portion <- read_csv("raw_data/ediblep.csv") %>% 
  rename(item_code = itemcode,
         edible_portion = mean_EP) %>% 
  dplyr::select(item_code, edible_portion)

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
  left_join(unit_conv, by = c("item_code", "unit"))

  mutate(quantity_g = case_when(
    !is.na(weighed_quantity) & is.na(quantity_g) ~ weighed_quantity * edible_portion,
    TRUE ~ quantity_g
  ))
