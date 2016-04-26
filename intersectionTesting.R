#Testing using shapefile intersect to re-assign zone count values to different zones
#Via QGIS/Python intersect - 
#which is the only approach I can find that provides me with the zone names
geolibs <- c("ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, library, character.only = TRUE)

library(dplyr)
library(tidyr)

rm(list=ls(all=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~
#1971 EDs to 2011 IGs----

#Get the 1971 country of birth data
cob71 <- read.csv("1971/Scotland/1971Scotland_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv")

#Load test intersect: 71 EDs into 2011 IGs.
intersect <- readOGR(dsn="C:/Data/MapPolygons/Intersections/Scotland/1971_EDsTo2011_IGs", 
                 layer="1971_EDsTo2011_IGs")

#We also need original ED areas to create fractions of the full area
#For making fractions of each ED's CoB value to re-assign to IZs.
ed71 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1971/Scotland_enumerationdistricts_1971", 
                     layer="scotland_ed_1971")

#Note! 15779 unique zone codes out of 16528 elements? Uh oh.
#Same number for OPCS code.
length(unique(ed71@data$ZONE_CODE))

#If I keep only the ones with the same code, are they in the same place or wot?
#Keep only dups then look in QGIS
ed71dups <- subset(ed71,duplicated(ed71@data$ZONE_CODE)|duplicated(ed71@data$ZONE_CODE,fromLast = T))
writeOGR(ed71dups, "C:/Data/temp/MigrationIntersectTests/random", "ed71dups", driver="ESRI Shapefile")

#Can I just dissolve by ID? Yes! Bonza!
#Only data required: ID of zone.
#SpatialPolygon has the ID which is actually all I need
ed71dissolve <- gUnaryUnion(ed71, id = ed71@data$ZONE_CODE)

#Convert back to spatialpolygonDF with ID as zone ID
#To get polygon IDs: https://stat.ethz.ch/pipermail/r-help/2005-December/085175.html
#So to be clear on what's happening:
#Set zone ID to polygon ID when dissolving
#Then use that same ID to rebuild as DF for saving.
#(Which wouldn't work if we cared about the data, but if there *was data
#We could e.g. do some summarising first)
ids <- as.data.frame(sapply(slot(ed71dissolve, "polygons"), function(x) slot(x, "ID")))
row.names(ids) <- ids[,1]

#It's got the right projection from the spatialPolygons object already
ed71dissolve_SPDF <- SpatialPolygonsDataFrame(
  ed71dissolve,
  data=ids
  )

names(ed71dissolve_SPDF@data) <- "Zone ID"

#Save and use this
writeOGR(ed71dissolve_SPDF, "C:/Data/MapPolygons/Scotland/1971/Scotland_enumerationdistricts_1971_dissolvedZoneID",
         "ed71dissolve", driver="ESRI Shapefile", overwrite_layer = T)



#Reload the just-made "duplicates dissolved to multiparts with their own row" as the official ed71 data...
ed71 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1971/Scotland_enumerationdistricts_1971_dissolvedZoneID", 
                layer="ed71dissolve")

#We're still out by 111 unique obs, by the look of it. Wrong way round though...
#15779 geog zones vs 15890 rows of data. Some of those could be in-the-sea-and-air types.
#Let's check they're all actually unique
#Yup
unique(cob71$Zone.Code) %>% length
#Yup!
unique(ed71@data$Zone.ID) %>% length

#So what are the extra 111 zones then? 
#Oh, I should check all the data zones are present in the geog zones
#Yup.
table(cob71$Zone.Code %in% ed71@data$Zone.ID)

#So what ids are in the CoB data but we have no polygon for?
noPolygon <- cob71[!(cob71$Zone.Code %in% ed71@data$Zone.ID),]
yesPolygons <- cob71[(cob71$Zone.Code %in% ed71@data$Zone.ID),]

#~~~~~~~~~~~~~~~~
#So 1971 polygons dissolved
#And we need to drop 111 rows from CoB data... but on with the link!
#Get new intersect from 1971 dissolve

#Load test intersect: 71 EDs into 2011 IGs.
intersect <- readOGR(dsn="C:/Data/MapPolygons/Intersections/Scotland/1971_dissolved_EDsTo2011_IGs", 
                     layer="1971_dissolved_EDsTo2011_IGs")

intersect@data$area <- gArea(intersect, byid=T)
ed71@data$area <- gArea(ed71, byid=T)

intersect_df <- data.frame(intersect) %>% arrange(Zone.ID)
ed71_df <- data.frame(ed71) %>% arrange(Zone.ID)

#The intersect shapefile gives me:
#Possibly multiple '71 ED IDs: each one gives an area assigned to the larger IZ.
#I need to turn those areas into fractions of the area from the '71 ED.
#Those fractions can then be used to multiply CoB amounts 
#To assign to the '11 IZ.

#1. Merge the '71 areas into the intersection
#Note:
unique(ed71_df$Zone.ID) %>% length
unique(intersect_df$Zone.ID) %>% length

#So there are less zones in the intersect.
#There are a few way outside Scotland so it might be those
#It's the right way round though - we should have areas for all zones
#present in the intersect. Right? Yup.
table(unique(intersect_df$Zone.ID) %in% unique(ed71_df$Zone.ID))

#This now has areas in... though we're not going to use it!
intersect_df <- left_join(intersect_df,ed71_df, by = 'Zone.ID')
#%>%   rename(c('area.x' = 'area_intersect','area.y' = 'area_71ED'))

#make fraction
intersect_df$fraction <- intersect_df$area.x/intersect_df$area.y

#If the intersect previously worked, those fractions should sum to one...
intersect_df <- intersect_df %>% group_by(Zone.ID) %>% 
  mutate(fractionTest = sum(fraction))

#Not perfectly, though mostly.
#I suspect it's because 71EDs are being clipped by water edge boundaries.
#I should clip them first before continuing to apply.
#Let's press on now though to get test done.

#Actually: just using the total area sum from the intersect is an implicit way of doing the same thing
#Thusly:
intersect_df <- intersect_df %>% group_by(Zone.ID) %>% 
  mutate(area_71ED_fromIntersect = sum(area.x))

#Now repeat based on this total instead
intersect_df$fraction <- intersect_df$area.x/intersect_df$area_71ED_fromIntersect

#And test. And yes, it's one where previously it wasn't.
intersect_df <- intersect_df %>% group_by(Zone.ID) %>% 
  mutate(fractionTest2 = sum(fraction))

#All that assumes I'm correct it's geog edges causing the problem, but it must be.

#Just need to merge the fraction (with the OA codes) into the CoB data. Then sum by OA code.
#So: merge fraction and its destination OA into the CoB data (which will grow accordingly)

#Use only the CoB obs we actually have polygons for...
useCoB71 <- cob71[(cob71$Zone.Code %in% unique(intersect_df$Zone.ID)),]
#intersect_df2 <- data.frame(intersect_df)

#I do not understand why this doesn't work...
#mrg <- left_join(useCoB71,intersect_df2 %>% select(Zone.ID,interzone,fraction), by=c("Zone.Code"="Zone.ID"))
mrg <- merge(useCoB71,intersect_df[,c('Zone.ID','interzone','fraction')],by.x = "Zone.Code",by.y="Zone.ID")

#Perfect. Now just need to multiply through by the fraction then sum by IZ.

#Take unchanged columns from result, add in country columns multiplied by fraction
mrg2 <- cbind(mrg[,c(1,2,18,19)],mrg[,c(3:17)] * mrg$fraction)

#Sum back to 2011 IZs per country
izResult <- aggregate(mrg2[,5:19],by=list(mrg2$interzone),sum)
#yup, right number!
unique(mrg2$interzone) %>% length

#check totals are correct i.e. same total number of English in EDs as IZs
#Yup!
sum(mrg2$England)
sum(izResult$England)

#Attach both of those to the original data, save to take a look.
ed71dissolve_SPDF2 <- merge(ed71dissolve_SPDF,useCoB71,by.x="Zone ID",by.y="Zone.Code")

#Load izs
izs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

izData <- merge(izs,izResult,by.x = "interzone", by.y = "Group.1")

#save 'em for temp looksee.
writeOGR(ed71dissolve_SPDF2, "C:/Data/temp/MigrationIntersectTests/linkResults71_11",
         "ed71_data", driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(izData, "C:/Data/temp/MigrationIntersectTests/linkResults71_11",
         "iz11_data", driver="ESRI Shapefile", overwrite_layer = T)



#~~~~~~~~~~~~~~~~~~~~~~~~
#1981 EDs to 2011 IGs----

#Because 1971 zones are looking... messy. 
#To test, let's try something that might be less messy.
#First-up, let's just check if we have anything like a zone-number match up between the two.
eighty1 <- read.csv("1981/Scotland/Scotland_1981_CoB_OutputArea_Total_noMaleFemale_countyNamesAdded.csv")

ed81 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1981/Scotland_enumerationdistricts_1981", 
                layer="Scotland_ed_1981")






#~~~~~~~~~~~~~~~~~~~~~~~~
#2001 OAs to 2011 IGs----
#The data
twoThousand1 <- read.csv("2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv")

#The zones.
oa01zones <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2001/Scotland_outputareas_2001", 
                     layer="scotland_oa_2001")




#~~~~~~~~~~~~~~~~~~~~~~~~
#2011 OAs to 2011 IGs----
oa11 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_output_areas_2011", 
                layer="scotland_oac_2011")

#Check on uniqueness of fields. Giz the data...
#46351 obs in the original and here
oa11_df <- data.frame(oa11)

#46351 again
unique(oa11_df$objectid) %>% length
#OA code? Yup, same number
unique(oa11_df$code) %>% length

#46351 (after realising there were extra gumph columns I hadn't removed, see below)
twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted.csv")

#All unique again
#unique(twoThEleven$Zone.Code) %>% length
#See? That worked!
#Oh right - they're random text description fields at the end. Think I'll remove those from the original data.
#(So none of this will work once I've done that)
#four <- twoThEleven[!(twoThEleven$Zone.Code %in% unique(oa11_df$code)),]

#Check the codes are actually the *same* codes...
#Yup, seem to be. Good good.
(unique(oa11_df$code) %in% unique(twoThEleven$Zone.Code)) %>% table()
(oa11_df$code %in% twoThEleven$Zone.Code) %>% table()




