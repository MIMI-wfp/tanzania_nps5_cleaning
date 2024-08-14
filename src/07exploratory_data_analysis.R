################################################################################
########################## TNPSW5 EXPLORATORY ANALYSES #########################
################################################################################

# Author: Mo Osman
# Date created: 14-08-2024
# Last edited: 

# In this script, I will perform exploratory analyses on the base models that have
# been generated for the MIMI team using the Tanzania NPS wave 5 data.

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "ggplot2", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

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