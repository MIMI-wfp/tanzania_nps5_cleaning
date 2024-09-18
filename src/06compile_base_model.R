################################################################################
################### SCRIPT FOR COMPILING BASE MODEL - TZA NPSW5 ################
################################################################################

# Author: Mo Osman
# Date created: 25-07-2024
# Last edited: 14-08-2024

# In this script, I will compile the pre-processed data to produce the base 
# apparent intake model for Tanzania NPS Wave 5:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "here")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# CLEAN DATA:

# Firstly clean data by filtering out households that are reporting implausible
# consumption quantities per day/AFE:

# Read in food consumption data and AFE's: 
tza_food_consumption <- read_csv("processed_data/tza_nps2021_food_consumption.csv")
tza_hh_afe <- read_csv("processed_data/tza_nps2021_hh_info.csv") %>% 
  dplyr::select(hhid, afe)

# Merge data and divide consumption quantities by AFE: 
tza_food_consumption <- tza_food_consumption %>% 
  left_join(tza_hh_afe, by = "hhid") %>% 
  mutate(quantity_g = quantity_g/afe,
         quantity_100g = quantity_100g/afe)

rm(tza_hh_afe)

# FILTER OUT EXTREME OUTLIERS:

# To do this, firstly filter out extreme values by applying log10 trans:
tza_food_consumption <- tza_food_consumption %>% 
  mutate(log_quantity_g = log10(quantity_g))

# Generate cut points for values that are >+3SDs from the mean intake of each food item:
quant_cutpoints <- tza_food_consumption %>% 
  group_by(item_code) %>% 
  summarise(mean_log = mean(log_quantity_g, na.rm = TRUE),
            sd_log = sd(log_quantity_g, na.rm = TRUE)) %>% 
  mutate(upper_cut = mean_log + 3*sd_log) %>% 
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

rm(quant_cutpoints)

#-------------------------------------------------------------------------------

# RE-WRITE DATA: 

# Need to re-write cleaned data, firstly multiply quantities by AFE (as data will
# be stored like this in the database prior to compiling the model):
tza_food_consumption <- tza_food_consumption %>% 
  mutate(quantity_g = quantity_g*afe,
         quantity_100g = quantity_100g*afe) %>% 
  dplyr::select(-afe)

# Write data:
# write_csv(tza_food_consumption, "processed_data/tza_nps2021_food_consumption.csv")

#-------------------------------------------------------------------------------

# LOAD FUNCTION REQUIRED TO COMPILE BASE MODEL: 
source("src/05base_model_functions.R")

#-------------------------------------------------------------------------------

# COMPILE BASE MODEL:
base_ai <- apparent_intake("tza_nps2021")

#-------------------------------------------------------------------------------

# WRITE DATA: 
write_csv(base_ai, "processed_data/tza_nps2021_base_ai.csv")

rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################
