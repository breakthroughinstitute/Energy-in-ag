library(rgl)
library(rgdal)
library(raster)

setwd("/Volumes/GoogleDrive/My Drive/BTI Research/Food & Farming/Energy in ag/Energy and GHG Analysis (2019)/Analysis/Calc LUC_EnergyAg/QGIS_MAPSPAM_analysis")
shp <- readOGR(dsn="ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

africa_shp <- subset(shp, CONTINENT=="Africa")

## Store the Africa shapefile so that you don't have to import the whole world next time:
writeOGR(africa_shp, ".", "africa-rgdal", driver="ESRI Shapefile")
