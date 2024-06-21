################################################################################
################ SCRIPT FOR AFE CALCULATION - TANZANIA NPS WAVE 5 ##############
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 

# In this script, I will calculate AFE for households in the Tanzania NPS-5 survey:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Specify rounding settings:
options(scipen = 10, digits = 3)

# List of assumptions made for AFE calculations: 

# 1 AFE = 2291kcal/day as calculated using the FAO/WHO/UNU (2004) equations for a 55kg female

# PAL = 1.76 (active/moderately active lifestyle) - reference: table 5.1 FAO/WHO/UNU (2004)

# Average men's weight = 65kg (Assumed - HOWEVER need to revisit)
# Average women's weight = 55kg (Assumed - HOWEVER need to revisit)

# Average energy cost of lactation = 505kcal - reference:

# Average total energy cost of a preganancy = 77,100kcal (reference: table 6.3 FAO/WHO/UNU (2004))
# Average length of a pregnancy = 280days
# Therefore average daily energy cost during pregnancy = 275kcal/day
# There is no data in the NPS-5 to determine pregnancy trimester

#-------------------------------------------------------------------------------

# READ IN DEMOGRAPHIC AND ANTHROPOMETRIC DATA:

anthropometric <- read_csv("raw_data/hh_sec_v.csv") %>%
  rename(weight = hh_v05, height = hh_v06) %>%
  select(y5_hhid, indidy5, weight)

demographic <- read_csv("raw_data/hh_sec_b.csv") %>%
  rename(sex = hh_b02,
         age = hh_b04,
         eat7d = hh_b07, # Individuals consuming foods in the last 7-days
         moid = hh_b15_2) %>% # Biological mothers of children <2 years
  select(y5_hhid, indidy5, sex, age, eat7d, moid)

#-------------------------------------------------------------------------------

# ESTIMATING ENERGY REQUIREMENTS AND AFE's FOR THOSE AGED < 24-months:

# Read in data where child's age (in months is available): 
u2 <- read_csv("raw_data/npsy5.child.anthro.csv")

# Filter only for those that have consumed food in the last 7-days at home: 
u2 <- u2 %>% 
  left_join(demographic, by= c('y5_hhid', 'indidy5')) %>%
  filter(eat7d == 1) %>% 
  select(y5_hhid, indidy5, age_months)

# Assign energy requirements for different age groups: 
u2 <- u2 %>%
  mutate(kcalreq = case_when(
    age_months <= 2 ~ 0,   # only breast feeding - no food intake
    age_months >= 3 & age_months <= 5 ~ 76,  # energy from food is 76kcal per day for 3-5 months of age
    age_months >= 6 & age_months <= 8 ~ 269,  # 269kcal per day for 6-8 months of age
    age_months >= 9 & age_months <= 11 ~ 451,   # 451kcal per day for 9-11 months of age
    age_months >= 12 & age_months <= 23 ~ 746)) %>%  # 746kcal per day for 12-23 months of age
  filter(age_months<=23)  # select children below 24 months

# AFE calculation for children below 2 years old:
afeu2 <- u2 %>%
  mutate(afeu2 = kcalreq/2346) %>% # 1AFE = 2346kcal
  select(y5_hhid, indidy5, afeu2)

# CHECK THE VALUES ABOVE

# Keep age for under 2's as this will be used later when calculating energy 
# requirements of lactating mothers: 
u2 <- u2 %>% dplyr::select(-kcalreq)

#-------------------------------------------------------------------------------

# ESTIMATING ENERGY REQUIREMENT FOR THOSE AGED >2YEARS:

# Merge anthropometric and demographic data and use this merged data-set for the
# calculation of total energy expenditure (TEE):

tee_calc <- left_join(anthropometric, demographic, by= c('y5_hhid', 'indidy5')) %>%
  group_by(sex, age) %>% # Calculate mean weight for age groups that have data available
  summarise(meanw = mean(weight, na.rm=TRUE)) %>%
  # Assign average weight for men and women >15 years old (as this data was not 
  # collected for men >15, and there were extreme outliers in the women's data)
  mutate(meanw = ifelse(age >= 15 & sex == 1, 65, 
                        ifelse(age >= 15 & sex == 2, 55, meanw))) %>%
  # Remove under 2's as these will have a seperate calculation: 
  filter(age >= 2) %>% 
  # Set a PAL at 1.76 for all over 18's:
  mutate(PAL = ifelse(age > 18, 1.76, NA))

# TEE FOR CHILDREN (2-18 years old) (Table 4.2 and 4.3 in FAO/WHO/UNU (2004)):
tee_calc <- tee_calc %>% 
  mutate(TEE = ifelse(sex == 1 & age <= 18, 1.298 + (0.265 * meanw) - 0.0011 * (meanw^2), 
                      ifelse(sex == 2 & age <= 18, 1.102 + (0.273 * meanw) - 0.0019 * (meanw^2), NA))) %>% 
  mutate(TEE = TEE * 239.005736) # convert to kcal/day: 

# TEE FOR ADULTS (Table 5.2 in FAO/WHO/UNU (2004)):
# Firstly need to calculate BMR for different age categories: 
tee_calc <- tee_calc %>% 
  mutate(BMR = case_when(
    sex == 1 & age >18 & age <= 30 ~ 15.057 * meanw + 692.2,
    sex == 1 & age >30 & age < 60 ~ 11.472 * meanw + 873.1,
    sex == 1 & age >= 60 ~ 11.711 * meanw + 587.7,
    sex == 2 & age >18 & age <= 30 ~ 14.818 * meanw + 486.6,
    sex == 2 & age >30 & age < 60 ~ 8.126 * meanw + 845.6, 
    sex == 2 & age >= 60 ~ 9.082 * meanw + 658.5,
    TRUE ~ NA)) 

# Get TEE by multiplying BMR by PAL for over 18's: 
tee_calc <- tee_calc %>% 
  mutate(TEE = ifelse(age > 18, BMR * PAL, TEE))

#-------------------------------------------------------------------------------

# GETTING AVERAGE MEN'S AND WOMEN'S WEIGHTS TO INTEGRATE INTO ABOVE CALCULATIONS, 
# ***** TO REVISIT *****

# Weights are recorded in an unclear manner in the DHS - Revist: 

# dhs <- read_dta("raw_data/TZHR82DT/TZHR82FL.dta")
# 
# # Extract weight variables for men from Tanzania DHS: 
# dhs_women <- dhs %>% 
#   dplyr::select(hhid, 
#                 starts_with("ha0_"),
#                 starts_with("ha1_"),
#                 starts_with("ha2_"))
# 
# # Extract weight variables for men from Tanzania DHS: 
# dhs_men <- dhs %>% 
#   dplyr::select(hhid, 
#                 starts_with("hb0_"),
#                 starts_with("hb1_"),
#                 starts_with("hb2_"))


#-------------------------------------------------------------------------------

# ENERGY REQUIREMENT FOR LACTATING WOMEN - IDENTIFY LACTATING WOMEN:

# Get demographic info of lactating mothers: 
# (Mothers assumed to be lactating if they have children under 2 years old) 

demographic_lact <- demographic %>%
  left_join(u2, by = c('y5_hhid', 'indidy5')) %>%
  dplyr::select(y5_hhid, indidy5, sex, age_months, moid, eat7d) %>%
  filter(eat7d == 1) %>% # Women that consumed foods in last 7-days
  # For children under 2, mark the mother as lactating:
  mutate(lact_m = ifelse(age_months <= 23, moid, NA)) %>%
  mutate(infant_age = ifelse(age_months <= 6, "<6months",
                             ifelse(age_months > 6, ">6months", NA))) %>%
  filter(!is.na(lact_m)) %>% 
  dplyr::select(y5_hhid, lact_m, infant_age) %>% 
  rename(indidy5 = lact_m) %>% # rename lact_m to indidy5 before merging demographic data
  left_join(demographic %>% dplyr::select(y5_hhid, indidy5, sex, age),
            by = c('y5_hhid', 'indidy5')) %>%
  filter(age <= 53) # Specify max age as 53.

# Calculate the energy requirements for these lactating women: 
# Usual energy requirements + 505kcal/day for women with babies <6 months old
# Usual energy requirements +460kcal/day for women with babies >6 months old
# (Chapter 7 in FAO/WHO/UNU (2004)):

afe_lact <- demographic_lact %>% 
  left_join(tee_calc %>% dplyr::select(sex, age, TEE),
            by = c("sex", "age")) %>%
  mutate (TEE = case_when(infant_age == "<6months" ~ TEE + 505,
                          infant_age == ">6months" ~ TEE + 460,
                          TRUE ~ TEE)) %>%
  mutate(afe = TEE / 2291) # AFE = Total energy expenditure / 2291kcal/day

rm(demographic_lact, u2)

#-------------------------------------------------------------------------------

# ENERGY REQUIREMENT FOR PREGNANT WOMEN: 

# Identify pregnant women: 
pregnant <- read_csv("raw_data/hh_sec_d.csv") %>% 
  dplyr::select(y5_hhid, indidy5, hh_d36a) %>% 
  rename(pregnant = hh_d36a) %>% 
  filter(pregnant == 1)

# Attach demographic data:
demographic_preg <- pregnant %>% 
  left_join(demographic %>% dplyr::select(y5_hhid, indidy5, age, sex, eat7d), 
            by = c("y5_hhid", "indidy5")) %>% 
  filter(eat7d == 1) %>% # Filter for women who ate at home in last 7-days
  dplyr::select(-c("pregnant", "eat7d"))

# Calculate energy requirments for these pregant women: 
# Usual energy requirements +275kcal/day: 
afe_preg <- demographic_preg %>% 
  left_join(tee_calc %>% dplyr::select(sex, age, TEE),
            by = c("sex", "age")) %>% 
  mutate(TEE = TEE + 275) %>% 
  mutate(afe = TEE / 2291) # AFE = Total energy expenditure / 2291kcal/day


#-------------------------------------------------------------------------------

# CALCULATE AFE FOR ALL OTHER INDIVIDUALS: 
afe_other <- demographic %>% 
  dplyr::select(y5_hhid, indidy5, sex, age) %>% 
  left_join(tee_calc %>% dplyr::select(age, sex, TEE), 
            by = c("age", "sex")) %>% 
  # FILTER OUT UNDER 2'S, AND PREGNANT AND LACTATING WOMEN!!!
  mutate(afe = TEE / 2291) # AFE = Total energy expenditure / 2291kcal/day

#-------------------------------------------------------------------------------

# CALCULATE TOTAL AFE PER HOUSEHOLD: 


#-------------------------------------------------------------------------------

rm(list = ls())