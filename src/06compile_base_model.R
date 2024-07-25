################################################################################
################### SCRIPT FOR COMPILING BASE MODEL - TZA NPSW5 ################
################################################################################

# Author: Mo Osman
# Date created: 25-07-2024
# Last edited: 

# In this script, I will compile the pre-processed data to produce the base 
# apparent intake model for Tanzania NPS Wave 5:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# LOAD FUNCTIONS: 
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
