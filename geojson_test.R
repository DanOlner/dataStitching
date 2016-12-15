#test geojsonio: can I save a shapefile as topojson?
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat")
lapply(geolibs, require, character.only = TRUE)

library(geojsonio)

dz <- readShapeSpatial('C:/Data/MapPolygons/Scotland/2011/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp')

#https://cran.r-project.org/web/packages/geojsonio/geojsonio.pdf

file_to_geojson('C:/Data/MapPolygons/Scotland/2011/SG_DataZoneBdry_2011/SG_DataZone_Bdry_2011.shp', method='local', output='shp_local')

