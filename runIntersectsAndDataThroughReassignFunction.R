#Stick all the data and intersects into a function
#For re-assigning data in small zones to larger ones, by area
#Intersect shapefile previously calculated in QGIS/Python
source("Function_reAssignVarsFromSmalltoLargeGeog.R")
source("Function_Shapefile_uniqueIDs_to_singleRowFeatures.R")

#Re-assign 1971 country of birth data
#Intersect file then data file
#Load here to check ID and column positions
its <- readOGR(dsn="C:/Data/MapPolygons/Intersections/Scotland/1971_dissolved_EDsTo2011_IGs", 
        layer="1971_dissolved_EDsTo2011_IGs")

dta <- read.csv("1971/Scotland/1971Scotland_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv")

#Large zones to attach resulting data to
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
dta_dataColumns <- c(3:17)

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,dta_dataColumns)

#save result!
writeOGR(result, "C:/Data/temp/MigrationIntersectTests/linkResults71_11_viaFunction",
         "iz11_data_viaFunction", driver="ESRI Shapefile", overwrite_layer = T)

#Thought the result was different - just QGIS being odd. Good good.

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#1981 Enumeration districts----

#Run 81 EDs through unique ID test/combo function
ed81 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1981/Scotland_enumerationdistricts_1981", 
        layer="Scotland_ed_1981")

names(ed81)
ed81_df <- data.frame(ed81)#'name' field is empty.

#Stick in function to check same IDs are all in the same polygon feature on the same row
ed81_uniqueIDtoSingleRows <- IDtoSingleRow(ed81,1)

#Need to check that worked again. Let's look at the original, just the duplicate IDs
#And the combo output
# ed81_dups <- subset(ed81,duplicated(ed81@data$label)|duplicated(ed81@data$label,fromLast = T))
# 
# #Save all those to look at in QGIS
# writeOGR(ed81_uniqueIDtoSingleRows, "C:/Data/temp/MigrationIntersectTests/81test",
#          "ed81_uniqueRowIDs", driver="ESRI Shapefile", overwrite_layer = T)
# writeOGR(ed81_dups, "C:/Data/temp/MigrationIntersectTests/81test",
#          "ed81_dups", driver="ESRI Shapefile", overwrite_layer = T)

#Yup, all working. So keep/use those zones for the data matching
writeOGR(ed81_uniqueIDtoSingleRows, 
         "C:/Data/MapPolygons/Scotland/1981/Scotland_enumerationdistricts_1981_dissolved_zoneIDs",
         "ed81_dissolved_zoneIDs", driver="ESRI Shapefile", overwrite_layer = T)

#Need to nip off and intersect that now...
#Done. Load new intersect.
its <- readOGR(dsn="C:/Data/MapPolygons/Intersections/Scotland/1981_dissolved_EDsTo2011_IGs", 
               layer="1981_dissolved_EDsTo2011_IGs")

dta <- read.csv("1981/Scotland/Scotland_1981_CoB_OutputArea_Total_noMaleFemale_countyNamesAdded.csv")

#how are the IDs looking across shapefile and data?
#First: unique ID in data rows? Yup.
unique(dta$Zone.ID) %>% length

#And of course they're unique in the shapefile cos we just made them so... right? Yup. Phew
unique(ed81_uniqueIDtoSingleRows@data$label) %>% length

#Difference: 17767 IDs in data; 17711 geog-zone IDs.

#Match number?
table(dta$Zone.ID %in% ed81_uniqueIDtoSingleRows@data$label)
#table(as.character(dta$Zone.ID) %in% as.character(ed81_uniqueIDtoSingleRows@data$label))

#Uh oh.
ed81_unique_df <- data.frame(ed81_uniqueIDtoSingleRows)

#Well, I can see some the same, so...
#Oh, it's spaces in the census data. Right.
as.character(ed81_uniqueIDtoSingleRows@data$label[1]) == as.character(dta$Zone.ID[1])

dta$Zone.ID <- gsub(" ", "", dta$Zone.ID)
table(dta$Zone.ID %in% ed81_uniqueIDtoSingleRows@data$label)
#FALSE  TRUE 
#56 17711 
#So a match for every zone. Anything about the 56 to say what the crack is?
#Not really. Worth noting to track down later, but having data for all zones is good
#Except of course where it might cause differences in totals for different zone aggregations
notz <- dta[!(dta$Zone.ID %in% ed81_uniqueIDtoSingleRows@data$label),]

#Might as well save that to replace the previous labels
write.csv(dta,"1981/Scotland/Scotland_1981_CoB_OutputArea_Total_noMaleFemale_countyNamesAdded_spacesRemovedFromLabels.csv")

#Let's shove all that through the re-assigner and look at it.
its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
dta_dataColumns <- c(2:19)

result2 <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,dta_dataColumns)

#save both scales for looking at in QGIS
writeOGR(result2, "C:/Data/temp/MigrationIntersectTests/linkResults81_11",
         "iz11_81to11_data", driver="ESRI Shapefile", overwrite_layer = T)

#Link data to original zones
ed81link <- merge(ed81_uniqueIDtoSingleRows,dta,by.x="label", by.y="Zone.ID")

writeOGR(ed81link, "C:/Data/temp/MigrationIntersectTests/linkResults81_11",
         "ed81_81to11_data", driver="ESRI Shapefile", overwrite_layer = T)





