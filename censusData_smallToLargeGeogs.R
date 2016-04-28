#Stick time-consistent data (same columns) through the mill to get same zone re-assignment
source("Function_DataFromSmalltoLargeGeog.R")

#Large zones that all data will have in common
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
               layer="scotland_pcs_1991_uniqueIDsperRow_LochsRemoved")

#~~~~~~~
#1971----

its <- readOGR(dsn="Intersects", 
               layer="Scots_71Eds_to_91PCS")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/71.csv")


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result71 <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/1971_to_2011_CoB_raw",
         "1971_CoB_from_17EDs_to_91_postcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~
#1981----

its <- readOGR(dsn="Intersects", 
               layer="Scots_81Eds_to_91PCS")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/81.csv")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result81 <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result81, "StitchOutputs/1971_to_2011_CoB_raw",
         "1981_CoB_from_81EDs_to_91_postcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~~~
#2001----

its <- readOGR(dsn="Intersects", 
               layer="Scots_01OAs_to_91PCS")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/01.csv")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result01 <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result01, "StitchOutputs/1971_to_2011_CoB_raw",
         "2001_CoB_from_01OAs_to_91_postcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)


#~~~~~~~~~~~
#2011----

its <- readOGR(dsn="Intersects", 
               layer="Scots_11OAs_to_91PCS")

#err...
df <- data.frame(its)

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/11.csv")

its_smallZoneIDColumn <- 2
its_largeZoneIDColumn <- 21
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result11 <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result11, "StitchOutputs/1971_to_2011_CoB_raw",
         "2011_CoB_from_11OAs_to_91_postcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)








#~~~~~~
#Save versions with values as % of CoB total e.g. % total scots in this zone----
#(matching vals in regression)
result71_perc <- result71

#http://stackoverflow.com/a/9624444/5023561
result71_perc@data[,c(2:15)] <- prop.table(result71_perc@data[,c(2:15)])*100

df <- data.frame(result71_perc)
df2 <- data.frame(result71)

#Or instead: let's do % of each zone since in theory we have total pop count per zone
#So actually - 
#https://stat.ethz.ch/pipermail/r-help/2007-June/133260.html
#prop.table(mymat, margin=2)*100
#So column margin (2) gives e.g. "proportion of Scots relative to total Scots in Scotland"
#Margin 1 gives e.g. "% of scots as total pop of this zone".

#Let's try the latter first
result71_percPerZone <- result71
result71_percPerZone@data[,c(2:15)] <- prop.table(as.matrix(result71_percPerZone@data[,c(2:15)]),margin = 1)*100

#Tick!
df <- data.frame(result71_percPerZone)

result81_percPerZone <- result81
result81_percPerZone@data[,c(2:15)] <- prop.table(as.matrix(result81_percPerZone@data[,c(2:15)]),margin = 1)*100

writeOGR(result71_percPerZone, "StitchOutputs/1971_to_2011_CoB_percentZoneTotal",
         "1971_CoB_from_71EDs_to_91_postcodeSectors_percZoneTotal", driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(result81_percPerZone, "StitchOutputs/1971_to_2011_CoB_percentZoneTotal",
         "1981_CoB_from_81EDs_to_91_postcodeSectors_percZoneTotal", driver="ESRI Shapefile", overwrite_layer = T)










