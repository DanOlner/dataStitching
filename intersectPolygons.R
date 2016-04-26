#Check again if R can do intersect/area better than QGIS python
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

library(dplyr)
library(tidyr)

rm(list=ls(all=TRUE))

#get the two shapefiles
ed71 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1971/Scotland_enumerationdistricts_1971_dissolvedZoneID", 
                layer="ed71dissolve")

iz11 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
                layer="scotland_ig_2011")

#http://gis.stackexchange.com/questions/163445/r-solution-for-topologyexception-input-geom-1-is-invalid-self-intersection-er
ed71 <- gBuffer(ed71, byid=TRUE, width=0)
iz11 <- gBuffer(iz11, byid=TRUE, width=0)

ptm <- proc.time()

#Try raster intersect
#http://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
ris <- intersect(ed71,iz11)

proc.time() - ptm