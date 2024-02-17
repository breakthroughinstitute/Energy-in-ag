#Rebound analysis
#author: Daniel Blaustein-Rejto
#date: 2/16/24

# Setup -------------------------------------------------------------------
library(tidyverse)

# Load Data ---------------------------------------------------------------
#d <- read_csv("raw_data/FOFA2050CountryData_Crop-production.csv") #Import FOFA 2050 data
d <- read_csv("int_data/country_and_crop_scenarios_filtered.csv") #import fofa and scenario data filtered to crops and countries included, with regions assigned.
CO2_loss <- read_csv("int_data/LUC_CO2_crops.csv") %>% select(country = ADMIN, id, tot_H, tot_S) %>% #Import csv with total kg CO2 emitted (veg CO2 loss and soil CO2 loss) per ha LUC of a particular crop, by country in Africa. This table include values using Searchinger's 40% (S) and Houghton's 25% (H) estimates of soil carbon loss for cropland conversion
  mutate(country = recode(country, "Republic of the Congo" = "Congo", "Ivory Coast" = "Côte d’Ivoire" , "eSwatini" = "Eswatini")) #change African country names in World_CO2_loss so that they match Africa list


# Define rebound scenarios --------
#define vector of rebound values e.g. 10%, 20% that refer to the relationship between change in arable land area from 2012 to 2050 and change in yield from 2012 to 2050
r <- seq(from = 0, to = 1, by = 0.1)

# Calculate baseline and high yield 
d <- d %>%  
  mutate(h_y = p_2050/h_h,   #calculate yield for high and baseline scenario in kcalories. units irrelevant as long as consistent
         b_y = p_2050/b_h) %>%
  select(Item, Element, CountryName, h_a, a_2012, h_y, b_y, region) #remove unneeded columns
  
# Calculate arable land for each rebound value for the high scenario

# Use map_dfc to iterate over r and calculate arable land area for each rebound value for the high scenario
new_columns <- map_dfc(r, ~{
  # Calculate arable and luc columns based on the current value of r, with arable land calculated by multiplying percent change in yield by rebound e.g. 100% rebound means arable land increases as much as yield does; 50% means it increases half as much
  a <- d$h_a * (1 + ((d$h_y / d$b_y - 1) * .x)) #arable
  l <- a - d$a_2012 #calculate land use change for each rebound value
  
  # Create a new dataframe with these two columns and set their names
  new_cols_df <- data.frame(a, l) %>%
    setNames(c(paste0("a_", .x), paste0("l_", .x)))
  
  return(new_cols_df)
})

# Bind the new columns to the original dataframe
d <- bind_cols(d, new_columns)

d <- select(d, -c(h_a, a_2012, h_y, b_y)) #remove unneeded columns


# # Calculate total LUC CO2 by country and crop for each rebound scenario -------------------------------------------------

#sum irrigated and rainfed LUC by country, item and scenario, keeping region variable. note this combines LUC for bananas & plantains into banp
d <- d %>% group_by(Item, CountryName, region) %>% summarize(across(a_0:l_1, \(x) sum(x, na.rm=T)))

#join co2/ha value to LUC dataframe and calculate total LUC kg CO2 loss/emissions by crop
LUC_CO2 <- inner_join(d, CO2_loss, by = c("CountryName" = "country", "Item" = "id")) %>%  #eliminates crops (just bean) in LUC dataset that do not have corresponding crop match in the CO2_loss dataset (derived from MAPSPAM analysis). 
  pivot_longer(a_0:l_1, names_to = c("var", "rebound"), names_sep = "_", values_to = "val") %>%  #make long
  pivot_wider(names_from = var, values_from = val) %>% #make separate columns for land use change and arable land
  pivot_longer(c(tot_H, tot_S), names_to = c("y", "c_loss_ratio"), names_sep = "_", values_to = "co2_ha") %>%
  select(-y) %>%
  mutate(co2_low = l * co2_ha, #calculate total co2 loss by multiplying CO2/ha by LUC
         co2_high = ifelse(l<0, 0, l)*co2_ha)  

#The last line above calculates a conservative LUC CO2 value where all negative LUC values, for each crop and country combination, are set to 0. 
#This is meant to model a scenario where arable land expansion does *not* go into abandoned farmland. 
#We do not account in this study for restoration, revegetation, or shifting cropland e.g. where wheat area in Algeria is abandoned and used to acommodate an increase in maize or rice.

# Aggregate LUC CO2 emissions by region ----------------------------------
LUC_CO2_region <- LUC_CO2 %>% group_by(region, rebound, c_loss_ratio) %>% 
  summarize(co2_low = sum(co2_low, na.rm=T), 
            co2_high = sum(co2_high, na.rm=T),
            luc = sum(l, na.rm=T),
            a = sum(a, na.rm=T))

#save rebound LUC and LUC emission values for combining in regression analysis and graphing
write_csv(LUC_CO2_region, "int_data/rebound_luc_by_region.csv")