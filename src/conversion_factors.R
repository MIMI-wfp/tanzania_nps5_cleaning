# Script to add conversion factors to the new food items: 

# Read in food items dictionary:
food_items <- read_csv("raw_data/food-id.csv") %>% 
  dplyr::select(itemcode, itemname)

# Read in conversion factors: 
conversion_factors <- read_csv("raw_data/unitconv.csv") %>% 
  filter(cons_unit %in% c(1:4)) %>% 
  dplyr::select(-itemname)

# Left join conversion factors to extended food item list: 
ext_conversion_factors <- food_items %>% 
  left_join(conversion_factors, by = "itemcode")

# Export csv's for further manual editing: 
write_csv(ext_conversion_factors, "raw_data/unitconv.csv")
write_csv(food_items, "raw_data/food-id.csv")
