**README**
 
*Folders:*
- code: contains R (.R) and R markdown (.Rmd) files. Running these reads in data from "raw_data" folder, generates intermediate results in "int_data folder" that are also used in the code, and generates final results in "results" folder. 
- results: 
- data: 


*Code:* 
First, run calculate_calories_per_cropland.Rmd
- We omit all country-crop combinations lacking complete data (where there are any NAs in the time frame). We then convert total crop production (in tons) from all countries from 1961-2017 to kilocalories, using FAO's nutritive factors. We then calculate kilocalories per hectare, a measure of total yields by country. 
INPUT: FAOSTAT_production_all_crops.csv, cal_per_tonne_by_crop.csv, FAOSTAT_Cropland.csv
OUTPUT: prod.cropland.csv

Second, run World_CarbonLoss.rmd
- Reads in vegetation and soil carbon by crop and country, generated from QGIS analysis of 2000 an 2010 MAPSPAM and Searchinger carbon storage data. Calculates total, and individual soil and vegetation CO2 loss per hectare for each crop by country (crop- and country-specific emissions factors.
- INPUT: crop-specific xlsx files in int_data/qgis_output
- OUTPUT: LUC_CO2_crops.csv

Third, run land_demand.R
Reads in the Future of Food and Agriculture report's (FAO Global Perspectives database) projections of crop production from 2012-2050 for all countries to construct model scenarios; calculates harvested area, arable land, and yields for the baseline, projected, and high yield scenarios. Using crop- and country-specific emissions factors, calculates lower and upper estimates of LUC and LUC emissions for each scenario for each African country and then aggregates by region.
INPUT: FOFA2050CountryData_Crop-production 7.csv, cal_per_tonne_by_crop.csv World_CO2_loss.csv
OUTPUT:regions_scenario_dan.csv, LUC_tot_region_dan.csv

*Misc*
- Calc LUC_EnergyAg: contains files with MAPSPAM tifs and .qgzs, GHG_emis.csv files, and searchinger.asc and .qgz files. Use unsure. 

- Energy-Ag: All files are from Feb 10, 2020 and so are presumed obsolete. 
