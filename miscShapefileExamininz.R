geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

#just checking on zone area
pcs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
                     layer="scotland_pcs_1991")

oas <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_outputareas_1991", 
               layer="Scotland_oa_1991_area")

izs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")


#find area

pcs@data$area <- gArea(pcs, byid=T)
oas@data$area <- gArea(oas, byid=T)
izs@data$area <- gArea(izs, byid=T)
  
mean(pcs@data$area)/1000000
mean(oas@data$area)/1000000
mean(izs@data$area)/1000000
#sd(pcs@data$area)/1000000
#sd(oas@data$area)/1000000
