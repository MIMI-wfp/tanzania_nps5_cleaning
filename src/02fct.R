################################################################################
########################## SCRIPT FOR PREPARING NCT/FCT ########################
################################################################################

# Author: Mo Osman
# Date created: 24-07-2024
# Last edited: 

# In this script, I will prepare the food/nutrient composition table for the 
# food items listed in Tanzania NPS wave 5. 

# I utilised a food/nutrient composition table that was prepared for wave 4 of
# survey by the MAPS team at LSHTM. This can be downloaded from here: 
# https://github.com/rgoto55/TNPSW4/tree/main/files

# Since wave 5 of the survey contained new food items, I created an updated version
# of the NCT/FCT for wave 5. Weighted averages were re-calculated where necessary.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "readxl")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ DATA: 
wave5_nct <- read_xlsx("raw_data/TNPSW5_averages.xlsx", 
                       sheet = "wave5_NCT_v1.0")

#-------------------------------------------------------------------------------

# SELECT RELEVANT VARIABLES AND RENAME: 
wave5_nct <- wave5_nct %>% 
  dplyr::select(itemcode, itemname, ENERCkcal, VITA_RAEmcg, THIAmg, RIBFmg, 
                NIAmg, VITB6_mg_standardised, FOLmcg_standardised, VITB12mcg, 
                FEmg, ZNmg) %>% 
  rename(item_code = itemcode, 
         item_name = itemname,
         energy_kcal = ENERCkcal,
         vita_rae_mcg = VITA_RAEmcg,
         thia_mg = THIAmg,
         ribo_mg = RIBFmg,
         niac_mg = NIAmg,
         vitb6_mg = VITB6_mg_standardised,
         folate_mcg = FOLmcg_standardised,
         vitb12_mcg = VITB12mcg,
         fe_mg = FEmg,
         zn_mg = ZNmg)

#-------------------------------------------------------------------------------

# WRITE DATA: 
write_csv(wave5_nct, "processed_data/tza_nps2021_fct.csv")

rm(list = ls())

################################################################################
############################### END OF SCRIPT ##################################
################################################################################