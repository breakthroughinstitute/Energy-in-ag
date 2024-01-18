**README**
 
*Folders:*
- code: contains R (.R) and R markdown (.Rmd) files. Running these reads in data from "raw_data" folder, generates intermediate results in "int_data folder" that are also used in the code, and generates final results in "results" folder. 

*Raw Data:*
- faostat
-- Fertilizer N use per ha of cropland - downloaded 04/21/2023. Units are kg N/ha. FAO metadata indicates values are "the ratio between the totals by nutrient of agricultural use of chemical or mineral fertilizers, reported in the FAOSTAT domain “Inputs/Fertilizers by Nutrient” for nitrogen (N)...and the area of cropland reported in the FAOSTAT domain “Inputs/Land Use”"
-- Fertilizer N Agricultural Use  - downloaded 04/21/2023. Units are tonnes nutrient nitrogen (N).\
-- fert_use_emissions.csv - Total emissions from synthetic fertilizer application. Downloaded 04/21/2023 from https://www.fao.org/faostat/en/#data/GT, selecting "Synthetic Fertilizers" and "Emissions (CO2eq) (AR5)". Original units are kilotonnes (gigagrams) CO2e

-qgis
-- MAPSPAM_2000_africa_physical_area - MAPSPAM2000v3r7 physical area data for each crop, clipped to Africa, downloaded 04/26/2022. Units are in hectares.
-- MAPSPAM_2010_africa_physical_area - MAPSPAM2010v1r1 physical area data for each crop, clipped to Africa, downloaded 04/26/2022. Units are in hectares.
-- Africa.shp - shapefile of Africa. Created in R using shapefiles from Natural Earth.
-- Searchinger files - .tif files of carbon content in native vegetation and soil, from lpjml model. Source: Searchinger et al 2018. Downloaded: 04/26/2022. Units: tonnes C/ha.
-- ne_10m_admin_0_countries - shapefile of country borders. Source: Natural Earth. Downloaded: 03/17/2022. 

*int_data/qgis_output* 
- Contains files generated by QGIS analysis of MAPSPAM and Searchinger carbon storage data. Data is aggregated by crop and country, with one spreadsheet for each crop. Values represent the total vegetation and soil carbon content in native vegetation that cropland expanded into from 2000 to 2010, and the change in physical area from 2000 to 2010 for that crop in hectares. The change in physical area only accounts for croland expansion; we convert negative land-use change values to 0, thereby assuming no land abandonment.

*Code:* 
First, run calculate_calories_per_cropland.Rmd
- We omit all country-crop combinations lacking complete data (where there are any NAs in the time frame). We then convert total crop production (in tons) from all countries from 1961-2017 to kilocalories, using FAO's nutritive factors. We then calculate kilocalories per hectare, a measure of total yields by country. This is used in the regression analysis.
INPUT: FAOSTAT_production_all_crops.csv, cal_per_tonne_by_crop.csv, FAOSTAT_Cropland.csv
OUTPUT: prod.cropland.csv

Second, run World_CarbonLoss.rmd
- Reads in total vegetation and soil carbon (in tonnes C) that was in native vegetation converted to cropland between 2000 and 2010, by crop and country, generated from QGIS analysis MAPSPAM and Searchinger carbon storage data, as well as LUC (in hectares) for that time period. Calculates soil and vegetation loss in kg CO2 per hectare for each crop by country (crop- and country-specific emissions factors). It calculates two versions of the potential soil CO2 loss per hectare, using 25% loss (Houghton) and 40% loss (Searchinger). It also accounts for carbon content of crop vegetation.
- INPUT: crop-specific xlsx files in int_data/qgis_output
- OUTPUT: LUC_CO2_crops.csv

Third, run land_demand.R
Reads in the Future of Food and Agriculture report's (FAO Global Perspectives database) projections of crop production from 2012-2050 for all countries to construct model scenarios; calculates harvested area, arable land, and yields for the baseline, projected, and high yield scenarios. Using crop- and country-specific emissions factors, calculates lower and upper estimates of LUC and LUC emissions for each scenario for each African country and then aggregates by region.
INPUT: FOFA2050CountryData_Crop-production 7.csv, cal_per_tonne_by_crop.csv World_CO2_loss.csv
OUTPUT:regions_scenario_dan.csv, LUC_tot_region_dan.csv

*Results:*
Includes graphs and tables generated by code.

*Misc*
- Calc LUC_EnergyAg: contains files with MAPSPAM tifs and .qgzs, GHG_emis.csv files, and searchinger.asc and .qgz files. Use unsure. 

- Energy-Ag: All files are from Feb 10, 2020 and so are presumed obsolete. 
