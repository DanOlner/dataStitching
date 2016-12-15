#aggregate Scots 2001 OAs to 2011 DZs.

source("Function_DataFromSmalltoLargeGeog.R")

#Shapefile to aggregate to
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/SG_DataZoneBdry_2011", 
                      layer="SG_DataZone_Bdry_2011")

#Get the intersect geog:
its <- readOGR(dsn="Intersects/otherIntersects", 
               layer="Scots2001OAto2011DZ")

#Get the data to be re-assigned in CSV form
#Should be from 2001, and at 2001 OA level.
#So should match number of 2001 OAs, which is...?
#42604.
unique(its@data$name) %>% length
#None. Good.
its@data$name[is.na(its@data$name)]

dataToReassign <-read.csv("")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,dataToReassign,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "", driver="ESRI Shapefile", overwrite_layer = T)

