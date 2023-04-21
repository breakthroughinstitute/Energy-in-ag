# Setup -------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(reshape2)

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
unique(anti_join(d, kcal, by = c("Item" = "crop"))$Item)

anti_join(kcal, d, by = c("crop" = "Item")) %>% 
    filter(!crop %in% c(cereals, cit, fruit, oilseeds, pulses, roots, swp, veg)) 
#indicates that nuts, green maize, and mushrooms are omitted
    
d <- inner_join(d, kcal, by = c("Item" = "crop")) #inner join to keep only crops w/ kcal converions

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

#select only columns for scenarios
#d <- select(d, -ends_with("2012"), -ends_with("2050"), -kcal_per_tonne) 

#create columns for each variable, but not each scenario
##make dataframe long, then spread variable names
#d <- d %>% pivot_longer(cols = b_h:h_l, names_to = c("scenario", "var"), values_to = "val", names_pattern = "(.)_(.)") %>% 
 # pivot_wider(names_from = var, values_from = val)



# #summarize each variable by region and scenario
# regions_scenario <- d %>% group_by(region, scenario) %>% 
#   summarize(harv_area = sum(h, na.rm=T),
#             yield = sum(p, na.rm=T)/sum(h, na.rm=T),
#             luc = sum(l, na.rm=T),
#             arable_area = sum(a, na.rm=T),
#             production = sum(p, na.rm=T)) #should be same for every scenario
# 
# #summarize each variable across Africa by scenario
# Africa_scenario <- d %>% group_by(Africa, scenario) %>% 
#   summarize(harv_area = sum(h, na.rm=T),
#             yield = sum(p, na.rm=T)/sum(h, na.rm=T),
#             luc = sum(l, na.rm=T),
#             arable_area = sum(a, na.rm=T),
#             production = sum(p, na.rm=T)) %>% #should be same for every scenario  
#   rename(region = Africa)
# 
# regions_scenario <- rbind(regions_scenario, Africa_scenario) %>% filter(is.na(region)==F)

#save Scenarios' (yield, harvested area, and production) data
#write.csv(regions_scenario,"LandDemand/regions_scenario_dan.csv")

# Calculate avg CO2 loss by crop & country --------------------------------

#Import World CO2 Loss file: kg CO2 emitted (veg CO2 loss and soil CO2 loss) ha LUC of a particular crop. 
#This table include LUC emissions (CO2 equiv)/ha using Searchinger's (40%) and Houghton's (25%) estimates of soil carbon loss for cropland conversion for each crop by country
World_CO2_loss <- read.csv("Energy and GHG Analysis (2019)/Analysis/Calc LUC_EnergyAg/Land Use Change Emissions/World_CO2_loss.csv")

CO2_loss <- as.data.frame(World_CO2_loss[, c(2:36)]) #only include cols on carbon emissions/ha (in terms of veg and soil, not totals) using Houhgton's estimates. 
#change African country names in World_CO2_loss so that they match Africa list starting at line 85
CO2_loss[CO2_loss$Country == "Congo",]$Country <- "Republic of the Congo" 
CO2_loss[CO2_loss$Country == "Tanzania, United Republic of",]$Country <- "United Republic Of Tanzania" 

       #Item=="Growing of bananas",]$Item <- "banps"

CO2_loss_Africa <- filter(CO2_loss, Country %in% Africa)
CO2_loss_Africa$maiz_veg_H <- as.numeric(as.character(unlist(CO2_loss_Africa$maiz_veg_H)))
CO2_loss_Africa$maiz_soil_H <- as.numeric(as.character(unlist(CO2_loss_Africa$maiz_soil_H)))
#DATA FILL. 2 steps.

#1. Replaces 0's with NAs. Fill missing values with avg CO2 loss across all countries for the given crop (essentially fill missing vaues with average of non-zero CO2 loss values across all countries for given crop's (veg or soil) column of CO2 emissions. 
CO2_loss_Africa[CO2_loss_Africa == 0] <- NA #only applies for cols 3,4, the banana cols
for (i in 2:ncol(CO2_loss_Africa)) {
  for (j in 1: nrow(CO2_loss_Africa)) {
    ifelse(is.na(CO2_loss_Africa[j,i]), CO2_loss_Africa[j,i] <- mean(as.numeric(unlist(CO2_loss_Africa[,i])),na.rm = TRUE), CO2_loss_Africa[j,i])
  }
}

#Replace banana CO2 loss with average of all other crops' CO2 losses for soil and veg since banana has missing alues across all countries.

cols_soil <- grep("*_soil_H", colnames(CO2_loss_Africa)) #store cols for soil
cols_soil <- cols_soil[-1] #take out banana col
cols_veg <- grep("*_veg_H", colnames(CO2_loss_Africa)) #store cols for veg
cols_veg <- cols_veg[-1] #take out banana col

CO2_loss_Africa_s <- CO2_loss_Africa[, cols_soil] #subset out cols with soil carbon loss values
CO2_loss_Africa_s$banps_soil <- rowMeans(CO2_loss_Africa_s) #banana carbon loss value equals the avg carbon loss values of all of the other crops

CO2_loss_Africa_v <- CO2_loss_Africa[, cols_veg] #subset out cols with veg carbon loss values
CO2_loss_Africa_v$banps_veg <- rowMeans(CO2_loss_Africa_v) #banana carbon loss value equals the avg carbon loss values of all of the other crops

#Store new banana values into original dataset
CO2_loss_Africa$banps_soil_H <- CO2_loss_Africa_s$banps_soil 
CO2_loss_Africa$banps_veg_H<-CO2_loss_Africa_v$banps_veg

#Add together soil and veg CO2 losses for each crop by country
CO2_Afr_crop <- as.data.frame(matrix(0, nrow = nrow(CO2_loss_Africa), ncol = 1)) #ncol equal number of crops, nrow equals number of countries
CO2_Afr_crop$banps <- CO2_loss_Africa$banps_soil_H + CO2_loss_Africa$banps_veg_H
CO2_Afr_crop$maiz <- CO2_loss_Africa$maiz_soil_H + CO2_loss_Africa$maiz_veg_H
CO2_Afr_crop$barls <- CO2_loss_Africa$barls_soil_H + CO2_loss_Africa$barls_veg_H
CO2_Afr_crop$cass <- CO2_loss_Africa$cass_soil_H+ CO2_loss_Africa$cass_veg_H
CO2_Afr_crop$mill <- CO2_loss_Africa$mill_soil_H + CO2_loss_Africa$mill_veg_H
CO2_Afr_crop$grou <- CO2_loss_Africa$grou_soil_H + CO2_loss_Africa$grou_veg_H
CO2_Afr_crop$ooil <- CO2_loss_Africa$ooil_soil_H + CO2_loss_Africa$ooil_veg_H
CO2_Afr_crop$opul <- CO2_loss_Africa$opul_soil_H + CO2_loss_Africa$opul_veg_H
CO2_Afr_crop$pota <- CO2_loss_Africa$pota_soil_H + CO2_loss_Africa$pota_veg_H
CO2_Afr_crop$rice <- CO2_loss_Africa$rice_soil_H+ CO2_loss_Africa$rice_veg_H
CO2_Afr_crop$sorg <- CO2_loss_Africa$sorg_soil_H + CO2_loss_Africa$sorg_veg_H
CO2_Afr_crop$soyb <- CO2_loss_Africa$soyb_soil_H + CO2_loss_Africa$soyb_veg_H
CO2_Afr_crop$sugb <- CO2_loss_Africa$sugb_soil_H + CO2_loss_Africa$sugb_veg_H
CO2_Afr_crop$sugc <- CO2_loss_Africa$sugc_soil_H + CO2_loss_Africa$sugc_veg_H
CO2_Afr_crop$swpy <- CO2_loss_Africa$swpy_soil_H + CO2_loss_Africa$swpy_veg_H
CO2_Afr_crop$whea <-CO2_loss_Africa$whea_soil_H + CO2_loss_Africa$whea_veg_H
CO2_Afr_crop$V1 <- CO2_loss_Africa$Country
colnames(CO2_Afr_crop)[1] <- "Country"

# # Lower estimate of LUC -------------------------------------------------
# # Leave negative LUC values as-is. This does not reflect shifting cropland. Negative LUC means that farmland is abandoned and becomes native vegetation.
# 
# #sum irrigated and rainfed LUC by country, item and scenario
# LUC <- d %>% dplyr::group_by(Item, CountryName, scenario) %>% dplyr::summarize(total_luc = sum(l, na.rm=T)) %>% 
#   filter(CountryName %in% Africa) #Only include African countries
# 
# #Convert FOFA crop names to MAPSPAM style crop names
# LUC[LUC=="Bananas"] <- "banps"
# LUC[LUC =="Barley"] <- "barls"
# LUC[LUC =="Cassava"] <- "cass"
# LUC[LUC =="Groundnuts, with shell"] <- "grou"
# LUC[LUC=="Grain maize"] <- "maiz"
# LUC[LUC=="Millet"] <- "mill"
# LUC[LUC=="Other oilseeds"] <- "ooil"
# LUC[LUC =="Rice, paddy"] <- "rice"
# LUC[LUC =="Dried pulses"] <- "opul"
# LUC[LUC =="Potatoes"] <- "pota"
# LUC[LUC=="Sorghum"] <- "sorg"
# LUC[LUC =="Sugar beet"] <- "sugb"
# LUC[LUC=="Sugar cane"] <- "sugc"
# LUC[LUC=="Wheat"] <- "whea"
# LUC[LUC=="Sweet potato and yams"] <- "swpy"
# LUC[LUC=="Soybeans"] <- "soyb"

#join co2/ha value to LUC dataframe
CO2_Afr_crop2 <- melt(CO2_Afr_crop) #will allow for joining data
colnames(CO2_Afr_crop2) <- c("Country", "Crop", "CO2/ha")
# LUC_CO2 <- right_join(LUC, CO2_Afr_crop2, by = c("CountryName" = "Country", "Item" = "Crop")) #should eliminate crops in LUC dataset that do not have corresponding crop match in the CO2_Afr_crop2 dataset (derived from MAPSPAM analysis). 
# 
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
d_rebound <- right_join(d_rebound, CO2_Afr_crop2)

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
# #Multiply by CO2/ha by LUC
# LUC_CO2 <- mutate(LUC_CO2, CO2 = total_luc * `CO2/ha`) 
# 
# #Aggregate CO2 emissions by country (add up all crops)
# LUC_CO2_country <- LUC_CO2 %>% dplyr::group_by(CountryName, scenario) %>% summarize(luc_co2 = sum(CO2, na.rm=T)) %>% 
#   rename(Country = CountryName) %>% filter(is.na(scenario)==F) %>% spread(key = scenario, value = luc_co2) 
# 
# write.csv(LUC_CO2_country,"LandDemand/CO2_Afr_1_dan.csv")  #save
# 
# # Higher estimate of LUC --------------------------------------------------
# #Make all negative LUC values 0. This assumes that arable land expansion does not go into abandoned farmland and that there is no change in vegetation or below-ground carbon.
# 
# #Convert all negative values across all scenarios to 0
# LUC$total_luc[LUC$total_luc<0] <- 0
# 
# #join co2/ha value to LUC dataframe
# #right join eliminates crops in LUC dataset that do not have corresponding crop match in the CO2_Afr_crop2 dataset
# LUC_CO2_0 <- right_join(LUC, CO2_Afr_crop2, by = c("CountryName" = "Country", "Item" = "Crop")) 
# 
# #Multiply by CO2/ha by LUC
# LUC_CO2_0 <- mutate(LUC_CO2_0, CO2 = total_luc * `CO2/ha`) 
# 
# #Aggregate CO2 emissions by country (add up all crops)
# LUC_CO2_0_country <- LUC_CO2_0 %>% dplyr::group_by(CountryName, scenario) %>% summarize(luc_co2 = sum(CO2, na.rm=T)) %>% 
#   rename(Country = CountryName) %>% filter(is.na(scenario)==F) %>% spread(key = scenario, value = luc_co2) 
# 
# write.csv(LUC_CO2_0_country,"LandDemand/CO2_Afr_3_dan.csv")  #save
# 
# # Combine and Save Lower & Upper LUC Estimates ----------------------------
# #Upper and lower total LUC by country. Units for LUC are in kg CO2e 
# LUC_CO2_country <- rename(LUC_CO2_country, Base_lwr = b, Proj_lwr = p, High_lwr = h) #lwr bounds - leaves negative values as is
# LUC_CO2_0_country <- rename(LUC_CO2_0_country, Base_upr = b, Proj_upr = p, High_upr = h) #upr bounds - negatives values are converted to 0
# LUC_country_bounds <- right_join(LUC_CO2_0_country, LUC_CO2_country)
# write.csv(LUC_country_bounds,"LandDemand/LUC_Afr_scenario_dan.csv")
# 
# #Upper and lower LUC by region. Units for LUC are in kg CO2e 
# #Aggregate CO2 emissions by region 
# LUC_bounds_E <- filter(LUC_country_bounds , Country %in% E_Afr)
# LUC_bounds_W <- filter(LUC_country_bounds , Country %in% W_Afr)
# LUC_bounds_C<- filter(LUC_country_bounds , Country %in% C_Afr)
# LUC_bounds_S<- filter(LUC_country_bounds , Country %in% S_Afr)
# LUC_bounds_N <- filter(LUC_country_bounds , Country %in% N_Afr)
# LUC_bounds_Africa <- filter(LUC_country_bounds, Country %in% Africa)
# 
# LUC_tot_region <-  as.data.frame(rbind(colSums(LUC_bounds_E[, c(2:7)]), colSums(LUC_bounds_W[, c(2:7)]), colSums(LUC_bounds_C[, c(2:7)]), colSums(LUC_bounds_N[, c(2:7)]),colSums(LUC_bounds_S[, c(2:7)]), colSums(LUC_bounds_Africa[, c(2:7)])))
# LUC_tot_region$Region <- c("East", "West", "Central","Northern", "Southern", "Africa")
# 
# write.csv(LUC_tot_region,"LandDemand/LUC_tot_region_dan.csv")
