source("Function_Shapefile_uniqueIDs_to_singleRowFeatures.R")
source("Function_DataFromSmalltoLargeGeog.R")


#Checking on zone area----
pcs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
                     layer="scotland_pcs_1991")

oas <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_outputareas_1991", 
               layer="Scotland_oa_1991_area")

izs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

wards <- readOGR(dsn="C:/Data/MapPolygons/EnglandWalesMerged/EnglandWales2011WardsClipped",
                 layer="englandWales2011WardsClipped")

#find area
pcs@data$area <- gArea(pcs, byid=T)
oas@data$area <- gArea(oas, byid=T)
izs@data$area <- gArea(izs, byid=T)
  
mean(pcs@data$area)/1000000
mean(oas@data$area)/1000000
mean(izs@data$area)/1000000
#sd(pcs@data$area)/1000000
#sd(oas@data$area)/1000000

#~~~~~~~~~~~~~~~~~
#Unique-IDing the 1991 OAs----

#Does this work btw?
#cf. http://www.inside-r.org/packages/cran/maptools/docs/unionSpatialPolygons
OA91 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_outputareas_1991", 
                layer="Scotland_oa_1991_area")

#Not unique
unique(OA91@data$LABEL) %>% length

#Yup, that did the trick! I think... right number of features certainly
#Oh no it didn't. OK, function!
#OA91dissolve <- unionSpatialPolygons(OA91,OA91@data$LABEL)

OA91dissolve <- IDtoSingleRow(OA91,2)

#Usual looksee in QGIS
#Non-unique ID clones in the original...
dups <- subset(OA91, duplicated(OA91@data$LABEL)|duplicated(OA91@data$LABEL, fromLast = T))

writeOGR(dups, "C:/Data/temp/MigrationIntersectTests/random",
         "91_OAs_dups", driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(OA91dissolve, "C:/Data/temp/MigrationIntersectTests/random",
         "91_OAs_unionSpatialPolygons", driver="ESRI Shapefile", overwrite_layer = T)

#Yup, that's all good. Next:
#Does it make any difference sticking unique-ID into the larger geography? 
#Actually, it shouldn't I don't think
#because area proportions by ID are used to aggregate

#Test with 2011IZs. Err. Need intersect files first.

#Oh actually: need to check on 2011 IZ11 file (loaded above)
#Already unique. Bonza! I did this check already, didn't I?
unique(izs@data$interzone) %>% length

#So on with intersect.

#~~~~~~~~~~~~~~~~~~~
# Intersects: how many zones into larger zones?----
# And how many are split, by how much, etc?

#~~~~~~~~~~~~~~~~~~~
# Save IZs as CSV, add centroid locs
izs@data <- cbind(izs@data, data.frame(gCentroid(izs, byid=T)))
#izs@data$centroids <- data.frame(gCentroid(izs, byid=T))

izs_df <- data.frame(izs) %>% rename(x_centroid =x, y_centroid = y)

write.csv(izs_df,"C:/Data/Census/QGIS/IZsWithCentroids.csv", row.names = F)











