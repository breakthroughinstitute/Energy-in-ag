# Setup -------------------------------------------------------------------
library(tidyverse)
# Load Data ---------------------------------------------------------------
d <- read_csv("raw_data/FOFA2050CountryData_Crop-production.csv") #Import FOFA 2050 data

d <- d %>% #filter data to arable land, harvested area, yield and cropping intensity for 2012 and 2050 (projected) only
  filter(Scenario == "Business As Usual", Year %in% c(2012, 2050), !Indicator %in% c("Climate shifter", "Technology shifter")) %>% 
  select(-c(Domain, CountryCode, Region, Units, Scenario)) %>% 
  pivot_wider(names_from = Indicator,values_from = Value) %>% #create column for each variable
  rename(a = `Arable land`, h = `Harvested area`, c = `Cropping intensity`, y = `Crop yield`) %>% 
  mutate(h = 1000*h) %>% #convert h from 1000 ha to ha
  mutate(p = h*y) %>%  #calculate production in tonnes
  pivot_wider(names_from = Year, values_from = c(a, h, c, y, p)) #pivot to create column for each variable-year combination 
  #units:  a: hectares; h: hectares, c: harvests/year, y: tonnes/ha, p: tonnes

# Calculate baseline, projected and high harvested area, arable, LUC --------
d <- d %>%  
  mutate(b_h = p_2050/y_2012,     #baseline harvested area = 2050 production/2012 yield = (2050 harvested area * 2050 yield) / 2012 yield
         p_h = p_2050/y_2050,      #projected harvested area = 2050 production/2050 yield = 2050 harvested area * (2050 yield/2050 yield) = 2050 harvested area
         
         b_a = b_h/c_2012,   #baseline arable land = baseline harvested area/2012 cropping intensity
         p_a = p_h/c_2050,   #projected arable land = projected harvested area/2050 cropping intensity
         
         h_a = ifelse(p_a < a_2012, p_a, a_2012),  #high arable land area is set to 2012 arable land area, to represent scenario w/ high enough yields to avoid land use change Except its set to projected arable area when that is less than 2012 arable area
         h_h = h_a*c_2050,  #high harvested area = high arable land * 2050 cropping intensity
         
         b_p = p_2050, #production is 2050 for all scenarios
         p_p = p_2050, #production is 2050 for all scenarios
         h_p = p_2050) %>%  #production is 2050 for all scenarios
  #calculate land use change (l) = arable area under scenario - 2012 arable area. all in ha
  mutate(b_l = b_a - a_2012, 
         p_l = p_a - a_2012,
         h_l = h_a  - a_2012)


# Convert from tons to kcal -----------------------------------------------
#Convert FOFA crop names for kcal conversion for each scenario
kcal <- read_csv("raw_data/cal_per_tonne_by_crop.csv")

#define crosswalk of crops in kcal data to crop groupings (e.g. citrus fruit) in FOFA. 
swp <- c("Yams", "Sweet potatoes")
pulses <- c("Bambara beans", "Beans, dry", "Broad beans, horse beans, dry", "Carobs", "Chick peas", "Lentils", "Cow peas, dry", "Peas, dry", "Pigeon peas", "Lupins", "Vetches")
veg <- c("Artichokes", "Asparagus",  "Beans, green", "Cabbages and other brassicas", "Carrots and turnips", "Cauliflowers and broccoli", 
  "Chillies and peppers, dry", "Chillies and peppers, green","Cucumbers and gherkins", "Eggplants (aubergines)", "Lettuce and chicory", 
  "Okra","Onions, dry", "Onions, shallots, green", "Peas, green", "Pumpkins, squash and gourds", "Spinach", "String beans", "Tomatoes")
cit <- c("Grapefruit (inc. pomelos)", "Lemons and limes", "Oranges","Tangerines, mandarins, clementines, satsumas")
fruit <- c("Apples", "Apricots", "Avocados", "Blueberries", "Cashewapple", "Cherries", "Cranberries", "Currants", "Dates", "Figs", "Gooseberries",  "Grapes", "Kiwi fruit",  "Mangoes, mangosteens, guavas",  "Papayas", "Peaches and nectarines", 
  "Pears", "Persimmons", "Pineapples", "Plums and sloes", "Quinces", "Raspberries", "Strawberries", "Watermelons")
cereals <- c("Buckwheat", "Canary seed", "Fonio", "Grain, mixed", "Oats", "Quinoa", "Rye", "Triticale")
oilseeds <- c("Hempseed", "Kapokseed in shell", "Karite nuts (sheanuts)", "Linseed", "Melonseed", "Poppy seed", "Safflower seed")
roots <- c("Taro (cocoyam)", "Yautia (cocoyam)" )

#Calculate mean kcal per ton for FOFA groupings
swp_yam <- tibble(crop = "Sweet Potato and Yams", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% swp)), 2]$kcal_per_tonne))
dried_pulses <- tibble(crop = "Dried pulses", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% pulses)), 2]$kcal_per_tonne))
other_veg <- tibble(crop = "Other vegetables", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% veg)), 2]$kcal_per_tonne))
citrus <- tibble(crop = "Citrus fruits", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% cit)), 2]$kcal_per_tonne))
other_fruit <- tibble(crop = "Other fruit", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% fruit)), 2]$kcal_per_tonne))
other_cereals <- tibble(crop = "Other cereals", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% cereals)), 2]$kcal_per_tonne))
other_oilseeds <- tibble(crop = "Other oilseeds", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% oilseeds)), 2]$kcal_per_tonne))
other_roots <- tibble(crop = "Other roots", kcal_per_tonne = mean(kcal[c(which(kcal$crop %in% roots)), 2]$kcal_per_tonne))

#add to dataframe of kcal conversions
kcal <- bind_rows(kcal, swp_yam, dried_pulses, other_veg, citrus, other_fruit, other_cereals, other_oilseeds, other_roots)  

#rename items that we we filter to. ##We omit non-caloric crops and crops we can't match between dataframes: cocoa beans, tea, tobacco, rubber, coffee (green), "other crops", and "other fibre crops".
d$Item <- dplyr::recode(d$Item,"Growing of bananas" = "Bananas", "Growing of barley" = "Barley", "Growing of cassava" = "Cassava",
                        "Growing of coconuts" =  "Coconuts" , "Growing of raw cotton" = "Cottonseed", "Growing of groundnuts" = "Groundnuts, with shell",
                        "Growing of grain maize" =  "Maize","Growing of millet" =  "Millet","Growing of oil palm fruit" =  "Oil palm fruit",
                        "Growing of paddy rice" = "Rice, paddy", "Growing of potatoes" = "Potatoes",
                        "Growing of rape and mustardseed" = "Rapeseed", #note this assumes mustardseed production is negligible compared to rapeseed (or kcal/ton of both are similar). This is reasonable given that in 2021 global rapeseed production was ~71 million vs. ~0.5 million for mustardseed
                        "Growing of sesame seed" ="Sesame seed", "Growing of sorghum" = "Sorghum", "Growing of sugar beet" =  "Sugar beet",
                        "Growing of sugar cane" =  "Sugar cane", "Growing of sunflower seed" =  "Sunflower seed", "Growing of wheat" = "Wheat",
                        "Growing of olives" =  "Olives", "Growing of plantains" =  "Plantains and others", "Growing of soybeans" = "Soybeans",
                        "Growing of sweet potato and yams" = "Sweet Potato and Yams",
                        "Growing of other vegetables" = "Other vegetables", "Growing of dried pulses" = "Dried pulses", 
                        "Growing of other fruits" = "Other fruit", "Growing of other cereals" = "Other cereals", "Growing of other oilseeds" = "Other oilseeds",
                        "Growing of other roots and tubers" = "Other roots", "Growing of citrus fruits" = "Citrus fruits")


#join kcal/ton to primary dataframe
#list crops from FOFA and kcal excluded
fofa_excluded_from_kcal <- unique(anti_join(d, kcal, by = c("Item" = "crop"))$Item) #coffee, rubber, tea, tobacco, "other fibre crops", cocoa beans, and "other crops"

kcal_excluded_from_fofa <- anti_join(kcal, d, by = c("crop" = "Item")) %>% 
    filter(!crop %in% c(cereals, cit, fruit, oilseeds, pulses, roots, swp, veg)) %>% select(crop) %>% pull()
#indicates that nuts, green maize, and mushrooms are omitted

write.csv(fofa_excluded_from_kcal, "results/Supplementary/fofa_excluded_from_kcal.csv")
write.csv(kcal_excluded_from_fofa, "results/Supplementary/kcal_excluded_from_fofa.csv")

    
d <- inner_join(d, kcal, by = c("Item" = "crop")) #inner join to keep only crops w/ kcal conversions

#multiply all production columns by kcal/ton
d <- d %>% mutate(across(c(p_2012, p_2050, ends_with("p")), ~.x*kcal_per_tonne))

#clean environment
rm(citrus, kcal, cereals, cit, fruit, oilseeds, pulses, roots, swp, veg, dried_pulses, swp_yam, other_veg, other_fruit, other_cereals, other_oilseeds, other_roots) 

# Define and create regions for aggregation -----------------------------------------------------
#Define regions. Member countries within each region are from the UN Statistics division : https://unstats.un.org/unsd/methodology/m49/overview/
country_groupings <- readxl::read_excel("raw_data/UNSD — Methodology.xlsx")

Africa <- country_groupings %>% filter(`Region Name`=="Africa") %>% select(`Country or Area`) %>% arrange(`Country or Area`) %>% pull()
W_Afr <- country_groupings %>% filter(`Intermediate Region Name`=="Western Africa") %>% select(`Country or Area`) %>% arrange(`Country or Area`) %>% pull()
E_Afr <- country_groupings %>% filter(`Intermediate Region Name`=="Eastern Africa") %>% select(`Country or Area`) %>% arrange(`Country or Area`) %>% pull()
C_Afr <- country_groupings %>% filter(`Intermediate Region Name`=="Middle Africa") %>% select(`Country or Area`) %>% arrange(`Country or Area`) %>% pull()
S_Afr <- country_groupings %>% filter(`Intermediate Region Name`=="Southern Africa") %>% select(`Country or Area`) %>% arrange(`Country or Area`) %>% pull()
N_Afr <- country_groupings %>% filter(`Sub-region Name`=="Northern Africa") %>% select(`Country or Area`) %>% arrange(`Country or Area`) %>% pull()

#define countries in Africa without matching names in FOFA in order to make names consistent for joining
Africa[which(!Africa %in% unique(d$CountryName))]

#match all possible, based on manual examination of FOFA countries. Most non-matching ones aren't listed separately in FOFA
d$CountryName <- recode(d$CountryName, "Republic of the Congo" = "Congo", "Côte d'Ivoire"="Côte d’Ivoire", "Swaziland" = "Eswatini")


#Filter out countries not included in CO2_loss dataset
CO2_loss <- read_csv("int_data/LUC_CO2_crops.csv") %>% select(country = ADMIN, id, tot_H, tot_S) #Import csv with total kg CO2 emitted (veg CO2 loss and soil CO2 loss) per ha LUC of a particular crop, by country in Africa. This table include values using Searchinger's 40% (S) and Houghton's 25% (H) estimates of soil carbon loss for cropland conversion

CO2_loss <- CO2_loss %>% mutate(country = recode(country, "Republic of the Congo" = "Congo", "Ivory Coast" = "Côte d’Ivoire" , "eSwatini" = "Eswatini")) #change African country names in World_CO2_loss so that they match Africa list

d <- d %>% filter(CountryName %in% CO2_loss$country) #filter out countries not included in CO2_loss dataset

#add column for region
d <- d %>% mutate(region = case_when(CountryName %in% W_Afr ~ "W_Afr", #assign region name if country is in the list of countries for that region
                                     CountryName %in% E_Afr ~ "E_Afr",
                                     CountryName %in% C_Afr ~ "C_Afr",
                                     CountryName %in% S_Afr ~ "S_Afr",
                                     CountryName %in% N_Afr ~ "N_Afr"),
                  Africa = case_when(CountryName %in% Africa ~ "Africa"))

africa_scenario_countries <- d %>% filter(Africa=="Africa") %>% select(CountryName) %>% distinct() %>% pull()
un_countries_excluded_from_fofa <- Africa[which(!Africa %in% unique(d$CountryName))]
write.csv(africa_scenario_countries, file = "int_data/countries_included_in_africa_in_fofa.csv")
write.csv(un_countries_excluded_from_fofa, file = "int_data/countries_excluded_in_africa_in_fofa.csv")

# Filter to crops included in CO2 loss/QGIS dataset -----------------------
#Convert FOFA crop names to MAPSPAM style crop names
d[d=="Bananas"] <- "banp"
d[d=="Plantains and others"] <- "banp"
d[d =="Barley"] <- "barl"
#no bean or coffee item in FOFA & kcal conversion corresponding to MAPSPAM bean & coff items
d[d =="Cassava"] <- "cass"
d[d=="Cottonseed"] <- "cott"
d[d =="Groundnuts, with shell"] <- "grou"
d[d=="Maize"] <- "maiz"
d[d=="Millet"] <- "mill"
d[d=="Other oilseeds"] <- "ooil"
d[d =="Dried pulses"] <- "opul"
d[d =="Potatoes"] <- "pota"
d[d =="Rice, paddy"] <- "rice"
d[d=="Sorghum"] <- "sorg"
d[d=="Soybeans"] <- "soyb"
d[d =="Sugar beet"] <- "sugb"
d[d=="Sugar cane"] <- "sugc"
d[d=="Sweet Potato and Yams"] <- "swpy"
d[d=="Wheat"] <- "whea"

d <- d %>% filter(Item %in% unique(CO2_loss$id)) #filter out crops not included in CO2_loss dataset
write_csv(d, "int_data/country_and_crop_scenarios_filtered.csv")

# Aggregate by region -----------------------------------------------------
scenario_d <- d %>% 
  select(-ends_with("2012"), -ends_with("2050"), -kcal_per_tonne) %>%  #select only columns for scenarios
  filter(is.na(Africa)==F) %>% 
  pivot_longer(cols = b_h:h_l, names_to = c("scenario", "var"), values_to = "val", names_pattern = "(.)_(.)") %>% #create columns for each variable, but not each scenario. make dataframe long, 
  group_by(Item, Element, CountryName, region, Africa, scenario, var) %>% summarize(val=sum(val)) %>% #summarize for crops that werent grouped in FOFA but are in MAPSPAM e.g. bananas and plantains
  pivot_wider(names_from = var, values_from = val) #then spread variable names

#summarize each variable by Africa region and scenario
regions_scenario <- scenario_d %>% group_by(region, scenario) %>%
  summarize(harv_area = sum(h, na.rm=T),
            yield = sum(p, na.rm=T)/sum(h, na.rm=T), #calculate yield as total production (in kcal) per harvested area (ha)
            luc = sum(l, na.rm=T),
            arable_area = sum(a, na.rm=T),
            production = sum(p, na.rm=T)) #should be same for every scenario

#summarize each variable across Africa by scenario
Africa_scenario <- scenario_d %>% group_by(Africa, scenario) %>%
  summarize(harv_area = sum(h, na.rm=T),
            yield = sum(p, na.rm=T)/sum(h, na.rm=T), #calculate yield as total production (in kcal) per harvested area (ha)
            luc = sum(l, na.rm=T),
            arable_area = sum(a, na.rm=T),
            production = sum(p, na.rm=T)) %>% #should be same for every scenario
  rename(region = Africa)

regions_scenario <- rbind(regions_scenario, Africa_scenario)

#save Scenarios' (yield, harvested area, and production) data
write_csv(regions_scenario,"int_data/scenarios.csv")

# # Calculate total LUC CO2 by country and scenario -------------------------------------------------

#sum irrigated and rainfed LUC by country, item and scenario, keeping region variable. note this combines LUC for bananas & plantains into banp
d <- d %>% group_by(Item, CountryName, region) %>% summarize(across(c(b_l, p_l, h_l), \(x) sum(x, na.rm=T))) %>%
  filter(CountryName %in% Africa) #Only include African countries

#list crops that are in fofa and kcal that are excluded from analysis since missing in mapspam & searchinger
fofa_kcal_excluded_from_mapspam_searchinger <- anti_join(d, CO2_loss, by = c("Item" = "id")) %>% ungroup() %>% select(Item) %>% unique() %>% pull() 
write.csv(fofa_kcal_excluded_from_mapspam_searchinger, "int_data/fofa_kcal_excluded_from_mapspam_searchinger.csv")

#join co2/ha value to LUC dataframe and calculate total LUC CO2 loss/emissions by crop
LUC_CO2 <- inner_join(d, CO2_loss, by = c("CountryName" = "country", "Item" = "id")) %>%  #should eliminate crops in LUC dataset that do not have corresponding crop match in the CO2_loss dataset (derived from MAPSPAM analysis). 
  pivot_longer(c(b_l, p_l, h_l), names_to = c("scenario", "x"), names_sep = "_", values_to = "luc") %>%  #make long
  pivot_longer(c(tot_H, tot_S), names_to = c("y", "c_loss_ratio"), names_sep = "_", values_to = "co2_ha") %>%
  select(-x,-y) %>%
  mutate(co2_low = luc * co2_ha, #calculate total co2 loss by multiplying CO2/ha by LUC
         co2_high = ifelse(luc<0, 0, luc)*co2_ha)  

#The last line above calculates a conservative LUC CO2 value where all negative LUC values, for each crop and country combination, are set to 0. 
#This is meant to model a scenario where arable land expansion does *not* go into abandoned farmland. 
#We do not account in this study for restoration, revegetation, or shifting cropland e.g. where wheat area in Algeria is abandoned and used to acommodate an increase in maize or rice.

#Aggregate LUC CO2 emissions by country
LUC_CO2_country <- LUC_CO2 %>% group_by(CountryName, scenario, c_loss_ratio, region) %>% 
  summarize(co2_low = sum(co2_low, na.rm=T), 
            co2_high = sum(co2_high, na.rm=T),
            luc = sum(luc, na.rm=T)) %>% 
  rename(Country = CountryName)

#Aggregate LUC CO2 emissions by region
LUC_CO2_region <- LUC_CO2_country %>% group_by(region, scenario, c_loss_ratio) %>% 
  summarize(co2_low = sum(co2_low, na.rm=T), 
            co2_high = sum(co2_high, na.rm=T),
            luc = sum(luc, na.rm=T))

# Save output -------------------------------------------------------------
write_csv(LUC_CO2,"int_data/luc_co2_country_crop.csv")
write_csv(LUC_CO2_country,"int_data/luc_co2_country.csv")
write_csv(LUC_CO2_region,"int_data/luc_co2_region.csv")