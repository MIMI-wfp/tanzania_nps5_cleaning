################################################################################
############## SCRIPT FOR CLEANING RAW DATA FROM THE TANZANIA NPS-5 ############
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 

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


#-------------------------------------------------------------------------------

# Read in food consumption module: 
tza_food_consumption <- read_csv("raw_data/hh_sec_j1.csv")

tza_food_consumption <- tza_food_consumption %>% 
  dplyr::select(y5_hhid, 
                itemcode,
                # To create variable for food group
                hh_j02_1,
                hh_j02_2) %>% 
  rename(hhid = y5_hhid,
         item_code = itemcode,
         unit = hh_j02_1,
         quantity = hh_j02_2)

# Remove entries for which quantity is NA: 
tza_food_consumption <- tza_food_consumption %>% 
  filter(!is.na(quantity))
