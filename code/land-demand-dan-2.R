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
         p_h = p_2050/y_2050,     #projected harvested area = 2050 production/2050 yield = 2050 harvested area * (2050 yield/2050 yield) = 2050 harvested area
         h_h = a_2012 * c_2050, #high harvested area = 2012 arable area, harvested with 2050 cropping intensity = (2012 harvested area/2012 cropping intensity)*2050 cropping intensity 
         
         b_a = b_h/c_2012,   #baseline arable land = baseline harvested area/2012 cropping intensity
         p_a = p_h/c_2050,   #projected arable land = projected harvested area/2050 cropping intensity
         h_a = h_h/c_2050,   #high arable land area = projected harvested area/2050 cropping intensity = 2012 arable land area. This indicate no change in  area.
         
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
fofa_excluded <- unique(anti_join(d, kcal, by = c("Item" = "crop"))$Item) #coffee, rubber, tea, tobacco, "other fibre crops", cocoa beans, and "other crops"

kcal_excluded <- anti_join(kcal, d, by = c("crop" = "Item")) %>% 
    filter(!crop %in% c(cereals, cit, fruit, oilseeds, pulses, roots, swp, veg)) %>% select(crop) %>% pull()
#indicates that nuts, green maize, and mushrooms are omitted
    
d <- inner_join(d, kcal, by = c("Item" = "crop")) #inner join to keep only crops w/ kcal conversions

#multiply all production columns by kcal/ton
d <- d %>% mutate(across(c(p_2012, p_2050, ends_with("p")), ~.x*kcal_per_tonne))

#clean environment
rm(citrus, kcal, cereals, cit, fruit, oilseeds, pulses, roots, swp, veg, dried_pulses, swp_yam, other_veg, other_fruit, other_cereals, other_oilseeds, other_roots) 

# Aggregate by region -----------------------------------------------------
#Define regions

#Member countries within each region are from the UN Statistics division : https://unstats.un.org/unsd/methodology/m49/overview/
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

#add column for region
d <- d %>% mutate(region = case_when(CountryName %in% W_Afr ~ "W_Afr", #assign region name if country is in the list of countries for that region
                                     CountryName %in% E_Afr ~ "E_Afr",
                                     CountryName %in% C_Afr ~ "C_Afr",
                                     CountryName %in% S_Afr ~ "S_Afr",
                                     CountryName %in% N_Afr ~ "N_Afr"),
                  Africa = case_when(CountryName %in% Africa ~ "Africa"))

africa_scenario_countries <- d %>% filter(Africa=="Africa") %>% select(CountryName) %>% distinct() %>% pull()
un_countries_excluded_from_fofa <- Africa[which(!Africa %in% unique(d$CountryName))]
write.csv(africa_scenario_countries, file = "int_data/countries_included_in_africa.csv")
write.csv(un_countries_excluded_from_fofa, file = "int_data/countries_excluded_in_africa.csv")

#select only columns for scenarios
scenario_d <- select(d, -ends_with("2012"), -ends_with("2050"), -kcal_per_tonne) %>% filter(is.na(Africa)==F)

#create columns for each variable, but not each scenario. make dataframe long, then spread variable names
scenario_d <- scenario_d %>% pivot_longer(cols = b_h:h_l, names_to = c("scenario", "var"), values_to = "val", names_pattern = "(.)_(.)") %>%
  pivot_wider(names_from = var, values_from = val)

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

# Calculate avg CO2 loss by crop & country --------------------------------

#Import csv with total kg CO2 emitted (veg CO2 loss and soil CO2 loss) per ha LUC of a particular crop, by country in Africa. 
#This table include values using Searchinger's 40% (S) and Houghton's 25% (H) estimates of soil carbon loss for cropland conversion
CO2_loss <- read_csv("int_data/LUC_CO2_crops.csv") %>% select(country = ADMIN, crop, tot_H, tot_S)

#change African country names in World_CO2_loss so that they match Africa list
CO2_loss <- CO2_loss %>% mutate(country = recode(country, "Republic of the Congo" = "Congo", "Ivory Coast" = "Côte d’Ivoire" , "eSwatini" = "Eswatini")) 

# # Calculate total LUC CO2 by country and scenario -------------------------------------------------
#sum irrigated and rainfed LUC by country, item and scenario, keeping region variable
LUC <- d %>% dplyr::group_by(Item, CountryName) %>% dplyr::summarize(across(c(b_l, p_l, h_l), sum, na.rm=T), region) %>%
  filter(CountryName %in% Africa) #Only include African countries

#Convert FOFA crop names to MAPSPAM style crop names
LUC[LUC=="Bananas"] <- "banps"
LUC[LUC =="Barley"] <- "barls"
LUC[LUC =="Cassava"] <- "cass"
LUC[LUC =="Groundnuts, with shell"] <- "grou"
LUC[LUC=="Maize"] <- "maiz"
LUC[LUC=="Millet"] <- "mill"
LUC[LUC=="Other oilseeds"] <- "ooil"
LUC[LUC =="Rice, paddy"] <- "rice"
LUC[LUC =="Dried pulses"] <- "opul"
LUC[LUC =="Potatoes"] <- "pota"
LUC[LUC=="Sorghum"] <- "sorg"
LUC[LUC =="Sugar beet"] <- "sugb"
LUC[LUC=="Sugar cane"] <- "sugc"
LUC[LUC=="Wheat"] <- "whea"
LUC[LUC=="Sweet Potato and Yams"] <- "swpy"
LUC[LUC=="Soybeans"] <- "soyb"
LUC[LUC=="Cottonseed"] <- "cott"

#join co2/ha value to LUC dataframe and calculate total LUC CO2 loss/emissions by crop
LUC_CO2 <- inner_join(LUC, CO2_loss, by = c("CountryName" = "country", "Item" = "crop")) %>%  #should eliminate crops in LUC dataset that do not have corresponding crop match in the CO2_loss dataset (derived from MAPSPAM analysis). 
  pivot_longer(c(b_l, p_l, h_l), names_to = c("scenario", "x"), names_sep = "_", values_to = "luc_low") %>%  #make long
  pivot_longer(c(tot_H, tot_S), names_to = c("y", "c_loss_ratio"), names_sep = "_", values_to = "co2_ha") %>%
  select(-x,-y) %>% 
  mutate(luc_high = ifelse(luc_low<0, 0, luc_low)) %>% #Calculate more conservative CO2 value where  all negative LUC values = 0. This assumes that arable land expansion does not go into abandoned farmland and that there is no change in vegetation or below-ground carbon.
  mutate(co2_low = luc_low * co2_ha, co2_high = luc_high*co2_ha) #calculate total co2 loss by multiplying CO2/ha by LUC

#Aggregate LUC CO2 emissions by country
LUC_CO2_country <- LUC_CO2 %>% group_by(CountryName, scenario, c_loss_ratio) %>% 
  summarize(co2_low = sum(co2_low, na.rm=T), co2_high = sum(co2_high, na.rm=T)) %>%
  rename(Country = CountryName)

#Aggregate LUC CO2 emissions by region
LUC_CO2_region <- LUC_CO2 %>% group_by(region, scenario, c_loss_ratio) %>% 
  summarize(co2_low = sum(co2_low, na.rm=T), co2_high = sum(co2_high, na.rm=T))

# Save output -------------------------------------------------------------
write_csv(LUC_CO2,"int_data/luc_co2_country_crop.csv")
write_csv(LUC_CO2_country,"int_data/luc_co2_country.csv")
write_csv(LUC_CO2_region,"int_data/luc_co2_region.csv")


# Old Rebound analysis ----------------------------------------------------
#Rebound analysis: Arable land area in 2012 = arable land area in the high scenario.Multiply arable land area 2012 by the rebound effect to calculate new High scenario arable land area and new LUC. Attach carbon loss data (kg/ha). Multiply new LUC values by crop and country specific carbon loss values and sum across all countries and crops to calculate Africa's LUC emissions. 
#Step 1. Filter and select Africa specific 2012 arable land area, sum irrigated & rainfed arable land areas for each country and crop, and attach carbon loss factors.
d_rebound <- d %>% select(Item, Element, CountryName, a_2012) %>% filter(CountryName %in% Africa)
d_rebound <- d_rebound %>% dplyr::group_by(Item, CountryName) %>% dplyr::summarize(tot_a_2012 = sum(a_2012, na.rm=T)) 
d_rebound[d_rebound$Item=="Growing of bananas",]$Item <- "banps"
d_rebound[d_rebound$Item=="Growing of barley",]$Item <- "barls"
d_rebound[d_rebound$Item=="Growing of cassava",]$Item <- "cass"
d_rebound[d_rebound$Item=="Growing of groundnuts",]$Item<- "grou"
d_rebound[d_rebound$Item=="Growing of grain maize",]$Item <- "maiz"
d_rebound[d_rebound$Item=="Growing of millet",]$Item<- "mill"
d_rebound[d_rebound$Item=="Growing of other oilseeds",]$Item<- "ooil"
d_rebound[d_rebound$Item=="Growing of paddy rice",]$Item<- "rice"
d_rebound[d_rebound$Item=="Growing of dried pulses",]$Item<- "opul"
d_rebound[d_rebound$Item=="Growing of potatoes",]$Item<- "pota"
d_rebound[d_rebound$Item=="Growing of sorghum",]$Item<- "sorg"
d_rebound[d_rebound$Item=="Growing of sugar beet",]$Item<- "sugb"
d_rebound[d_rebound$Item=="Growing of sugar cane",]$Item<- "sugc"
d_rebound[d_rebound$Item=="Growing of wheat",]$Item<- "whea"
d_rebound[d_rebound$Item=="Growing of sweet potato and yams",]$Item<- "swpy"
d_rebound[d_rebound$Item=="Growing of soybeans",]$Item<- "soyb"
colnames(d_rebound) <- c("Crop", "Country", "tot_a_2012")
d_rebound <- right_join(d_rebound, CO2_loss)

#Step 2. Multiply arable land 2012 by rebound effects, and calculate new LUC and LUC emissions values
rebound <- c(1.1, 1.2, 1.3,1.4,1.5, 1.6, 1.7, 1.8, 1.9, 2)
length(rebound) #10
new_LUC_emis <- matrix(0, nrow(d_rebound), 10)

for (i in 1:nrow(d_rebound)){
  for (j in 1:10) {
    new_LUC_emis[i,j] <- ((d_rebound$tot_a_2012[i] * rebound[j]) - d_rebound$tot_a_2012[i]) * d_rebound$`CO2/ha`[i]
  }
}

#Step 3. Convert LUC values from kg CO2 to Gt CO2, add country and crop names, and column names.
kg_Gt <- 1.00E-12
colnames(new_LUC_emis) <- c("LUC_emis_0.1","LUC_emis_0.2","LUC_emis_0.3","LUC_emis_0.4","LUC_emis_0.5","LUC_emis_0.6","LUC_emis_0.7", "LUC_emis_0.8", "LUC_emis_0.9", "LUC_emis_1")
new_LUC_emis <- as.data.frame(new_LUC_emis)
Afr_LUC_emis <- as.data.frame(colSums(new_LUC_emis, na.rm = T))
Afr_LUC_emis <- (Afr_LUC_emis * kg_Gt)/38 #annualized LUC emissions

#Step 4. Calculate new harvested area, input use, and emissions for Africa
#read in regions_scenario.csv and multiply the high harvested area by the rebound effect.
  regions_scenario_dan2 <- read_csv("~/Google Drive/My Drive/Food & Farming/Energy in ag/LandDemand/regions_scenario_dan2.csv")
harv_area <- regions_scenario_dan2 %>% select(region, scenario, harv_area) %>% filter(scenario == 'h')
#import predictions data 
predictions_clean <- read_csv("~/Google Drive/My Drive/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/Calc LUC_EnergyAg/Energy_Ag model predicted values/predictions_clean.csv")
predictions_clean$...1 <- NULL
predictions_clean <- predictions_clean %>% filter(Scenario == "h")
harv_area$region <- recode(harv_area$region, Central = "c", East = "e", West = "w", South = "s", North = "n")
predictions_clean <- inner_join(predictions_clean, harv_area, by = "region")

#join region specific harvested areas to predictions and calculate input use or emissions
new_input <- matrix(0, nrow(predictions_clean), 10) #nrow = nrow(harv_area), ncol = number of values in rebound vector
for (i in 1:nrow(predictions_clean)) {
  for (j in 1:10) {
    new_input[i,j] <- predictions_clean$value[i] * rebound[j] * predictions_clean$harv_area[i]
  }
}
new_input <- cbind(predictions_clean, new_input)
new_input$scenario <- NULL
#units, machinery emissions, on farm fuel emissions, and synthetic emissions are in Gg CO2. pesticides use is in tons used, fertilizer used is in kilograms used.
pest_ef <- 25.5 #kg CO2/kg applied
tons_kg <- 1000 #for pesticides 
kg_Gt <- 1.00E-12
Gg_Gt <- 0.000001
kg_Gg <- 1e-6
fert_ef <- 5.66 # kg Co2e/kg N
which(new_input$input == "p") #rows 1,6,11,16,21,26 are pesticides use rows
which(new_input$input == "synNuse") # rows  4,  9, 14, 19, 24, 29 are fertilizer use rows
new_input[c(1,6,11,16,21,26), c(6:15)] <- new_input[c(1,6,11,16,21,26), c(6:15)] * pest_ef * tons_kg * kg_Gg  #multiply the new input values in the pesticides use rows by pest_ef and convert to Gg
new_input[c(4,  9, 14, 19, 24, 29), c(6:15)] <- new_input[c(4,  9, 14, 19, 24, 29), c(6:15)]* fert_ef * kg_Gg  #multiply the new input values in the fertilizer use rows by the fertilizer emissions factor and convert go Gg
new_input[, c(6:15)] <- new_input[, c(6:15)] * Gg_Gt #all input emissions should be in Gg CO2eq, so convert to Gt CO2eq.
Afr_input_emis <- as.data.frame(colSums(new_input[, c(6:15)]))
#Step 5. Calculate difference in emissions and create graph.
Afr_LUC_emis <- t(Afr_LUC_emis) 
Afr_input_emis <- t(Afr_input_emis)
colnames(Afr_LUC_emis) <- c("0.1", "0.2", "0.3","0.4","0.5", "0.6", "0.7", "0.8", "0.9", "1")
colnames(Afr_input_emis) <- c("0.1", "0.2", "0.3","0.4","0.5", "0.6", "0.7", "0.8", "0.9", "1")
Africa_emis <- rbind(Afr_LUC_emis, Afr_input_emis)
Africa_emis <- as.data.frame(t(Africa_emis))
colnames(Africa_emis) <- c("LUC_emis", "input_emis") #rownames = rebound effect 
#2.047528 is the total emissions for the baseline value for Africa
Africa_emis$diff_emis <- (Africa_emis$LUC_emis + Africa_emis$input_emis) - 2.047528

rebound_chart <- c(0.1, 0.2, 0.3,0.4)
ggplot()+ geom_line(aes(x=rebound_chart, y=Africa_emis$diff_emis[c(1:4)])) + #graph diagonal line showing relationship between rebound /elasticity value  on x axis and difference in emissions on y.  
  geom_hline(yintercept = 0, color = "grey") #highlight y=0 where high scenario starts having higher emissions 

#Step 6. Save intermediate datasets
write.csv(Africa_emis, "~/Google Drive/My Drive/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/energy_ag_analysis/Results/Africa_emis_rebound.csv")
write.csv(new_input, "~/Google Drive/My Drive/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/energy_ag_analysis/Results/new_input_rebound.csv")
write.csv(new_LUC_emis, "~/Google Drive/My Drive/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/energy_ag_analysis/Results/new_LUC_emis_rebound.csv")
write.csv(d_rebound, "~/Google Drive/My Drive/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/energy_ag_analysis/Results/d_rebound.csv")

