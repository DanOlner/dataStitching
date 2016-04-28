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

#Testing if self-intersects are messing with things in PyQGIS
OA2001 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2001/Scotland_outputareas_2001", 
                layer="scotland_oa_2001_dissolvedTo_OneIDperRow")

OA2001_noSelfIntersect <- gBuffer(OA2001, byid=TRUE, width=0)

writeOGR(OA2001_noSelfIntersect, "C:/Data/MapPolygons/Scotland/2001/Scotland_outputareas_2001", 
         "scotland_oa_2001_dissolvedTo_OneIDperRow_noSelfIntersect", driver="ESRI Shapefile", overwrite_layer = T)

#What about postcode sectors?
PCS91 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
                 layer = "scotland_pcs_1991_uniqueIDsperRow_LochsRemoved")

PCS91_noSelfIntersect <- gBuffer(PCS91, byid=TRUE, width=0)

writeOGR(PCS91_noSelfIntersect, "C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
         "scotland_pcs_1991_uniqueIDsperRow_LochsRemoved_noSelfIntersect", driver="ESRI Shapefile", overwrite_layer = T)








testy <- readOGR(dsn="C:/Data/Census/Intersects", 
                layer="Scotland_2001_OAs_to_2011_IZ")

# testy2 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2010/ScottishCouncilAreas2010_Derivedbyaggregating2011OAs_usingNRSexactfitCensusIndex", 
#                 layer="scotland_ca_2010")


#to get coords quickly
# plot(testy2, col="green")

#This is crashy awfulness!
# xy=locator(2,"p") 

xy = data.frame(x=c(259069,261039),y=c(671297,669684))

plot(testy, col="green", xlim=xy$x,ylim=xy$y)





