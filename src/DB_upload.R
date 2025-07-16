################################################################################
##################### PREPARATION OF DATA FOR DB UPLOAD ########################
################################################################################

# Author: Mo Osman
# Date created: 16-07-2025

# NPS data requested by the APP team for upload to the MIMI database.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "srvyr", "getPass", "DBI", "RMySQL")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN REQUIRED DATA: 
hh_information <- read_csv("processed_data/tza_nps2021_hh_info.csv")
food_consumption <- read_csv("processed_data/tza_nps2021_food_consumption.csv")
fct <- read_csv("processed_data/tza_nps2021_fct.csv")
base_ai <- read_csv("processed_data/tza_nps2021_base_ai.csv")

#-------------------------------------------------------------------------------

# Prepare hh_information so that it is consistent with the database: 
hh_information <- hh_information |>
  mutate(iso3 = "TZA",
         survey = "nps2021") |>
  dplyr::select(iso3, survey, hhid, adm1, adm2, ea, res, sep_quintile, 
                res_quintile, year, month, survey_wgt, afe)

# Grab the pc_expenditure variable
pc_expenditure <- read_csv("raw_data/consumption_real_y5.csv") |> 
  rename(hhid = y5_hhid,
         pc_expenditure = expmR_pae) |> 
  dplyr::select(hhid, pc_expenditure)

hh_information <- hh_information |>
  left_join(pc_expenditure, by = "hhid")

rm(pc_expenditure)

#-------------------------------------------------------------------------------

# Prepare food_consumption and food_group: 
food_group <- food_consumption |> 
  dplyr::select(item_code, food_group) |> 
  distinct() |> 
  arrange(item_code) |> 
  mutate(iso3 = "TZA",
         survey = "nps2021") |> 
  dplyr::select(iso3, survey, item_code, food_group)

food_consumption <- food_consumption |>
  mutate(iso3 = "TZA",
         survey = "nps2021") |>
  dplyr::select(iso3, survey, hhid, item_code, quantity_g, quantity_100g) 

#-------------------------------------------------------------------------------

# Prepare fct: 
fct <- fct |> 
  mutate(iso3 = "TZA",
         survey = "nps2021") |>
  dplyr::select(iso3, survey, everything())

#-------------------------------------------------------------------------------

# Prepare base_ai: 
base_ai <- base_ai |> 
  mutate(iso3 = "TZA", 
         survey = "nps2021") |>
  dplyr::select(iso3, survey, everything())

#-------------------------------------------------------------------------------

# CREATE ML_targets

# Define the harmonised average requirements
h_ar <- data.frame(iso3 = "TZA",
                   energy_kcal = 2100,
                   vita_rae_mcg = 490,
                   thia_mg = 0.9,
                   ribo_mg = 1.3,
                   niac_mg = 11,
                   vitb6_mg = 1.3,
                   folate_mcg = 250,
                   vitb12_mcg = 2,
                   fe_mg = 22.4,
                   ca_mg = 860,
                   zn_mg = 10.2)

# Calculate MAR (mean adequacy ratio): 
ML_targets <- base_ai |>
  mutate(va_nar = vita_rae_mcg / h_ar$vita_rae_mcg,
         fol_nar = folate_mcg / h_ar$folate_mcg,
         vb12_nar = vitb12_mcg / h_ar$vitb12_mcg,
         fe_nar = fe_mg / h_ar$fe_mg,
         zn_nar = zn_mg / h_ar$zn_mg) |>
  # Truncate NAR values at 1: 
  mutate(across(va_nar:zn_nar, ~ifelse(. > 1, 1, .))) |> 
  # Average to obtain MAR:
  mutate(overall_mar = (va_nar + fol_nar + vb12_nar + fe_nar + zn_nar) / 5) |> 
  dplyr::select(iso3, survey, hhid, vita_rae_mcg, folate_mcg, vitb12_mcg, fe_mg,
                zn_mg, overall_mar)

rm(h_ar)

#-------------------------------------------------------------------------------

# # CONNECT TO THE DATABASE: 
# con <- dbConnect(RMySQL::MySQL(),
#                  dbname = "mimi_db",
#                  host = "127.0.0.1",
#                  port = 3306,
#                  user = getPass("Enter username: "),
#                  password = getPass("Enter password: "))
# 
# #-------------------------------------------------------------------------------
# 
# # APPEND DATA:
# 
# # Household info:
# dbWriteTable(con, name = "hh_information", value = hh_information, append = TRUE,
#              row.names = FALSE)
# 
# rm(hh_information)
# 
# # Food consumption:
# dbWriteTable(con, name = "food_consumption", value = food_consumption, append = TRUE,
#              row.names = FALSE)
# 
# rm(food_consumption)
# 
# # Food group:
# dbWriteTable(con, name = "food_group", value = food_group, append = TRUE,
#              row.names = FALSE)
# 
# rm(food_group)
# 
# # Food composition table:
# dbWriteTable(con, name = "fct", value = fct, append = TRUE, row.names = FALSE)
# 
# rm(fct)
# 
# # Base ai: 
# dbWriteTable(con, name = "base_ai", value = base_ai, append = TRUE,
#              row.names = FALSE)
# 
# rm(base_ai)
# 
# # ML targets:
# dbWriteTable(con, name = "ML_targets", value = ML_targets, append = TRUE,
#              row.names = FALSE)
# 
# rm(ML_targets)
# 
# #-------------------------------------------------------------------------------
# 
# # DISCONNECT FROM THE DATABASE:
# dbDisconnect(con)

rm(list = ls())

################################################################################
################################ END OF SCRIPT #################################
################################################################################