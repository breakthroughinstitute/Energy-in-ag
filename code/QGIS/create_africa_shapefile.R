#load a package to read in and modify shapefiles
library(sf)

#read the shapefile 
shp <- st_read("raw_data/qgis/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

#subset to Africa
africa_shp <- shp[shp$CONTINENT == "Africa", ]

# Save the Africa shapefile so that you don't have to import the whole world next time:
st_write(africa_shp, "raw_data/qgis/africa-rgdal", driver="ESRI Shapefile")