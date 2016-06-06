source("Function_DataFromSmalltoLargeGeog.R")
#Census geography harmonising: 5-census, 1991 small-zone, target zone 2011 Scots intermediate geography

#First-up: 2011 output areas nest into IZs. Just need a look-up and aggregate
#2011 output area to IZ lookup. File via
#http://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/census-datasets/2011-census/2011-indexes
lookup <- read.csv("2011/Scotland/lookup_OA_DZ_IZ_2011.csv")

#check it's got the right number of IDs. Yup!
unique(lookup$IntermediateZone2011Code) %>% length

#Get the 2011 CoB output area data (processed already)
CoB2011 <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv")

#merge IZ codes into the COB data
#they all match, right? Yup!
table(lookup$OutputArea2011Code %in% CoB2011$Zone.Code )

#Why can't I get dplyr join to ever work??
CoB2011 <- merge(CoB2011,lookup[,c(1,3)],by.x = 'Zone.Code', by.y = 'OutputArea2011Code')

#Aggregate by intermediate geography
#Select not working...
#Ah - masking raster functions or vice versa I think. Load order matter?
#https://github.com/hadley/dplyr/issues/643
CoB2011agg <- CoB2011 %>% dplyr::select(2:58) %>% 
  group_by(IntermediateZone2011Code) %>% 
  summarise_each(funs(sum))

#Column sums the same? Yup. Bonza.
apply(CoB2011[,2:10],2,sum)
apply(CoB2011agg[,2:10],2,sum)

#Save.
write.csv(CoB2011agg,
          "2011/Scotland/CoB_IntermediateGeog_viaLookupFromOutputArea/2011_CoB_intermediateGeog.csv",
          row.names = F)

#reload after putting through the column re-assign code
cob11 <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/11_IntermediateGeog.csv")

#Then stick that through the country column harmoniser. Which I've done now, right?
#Yup.
#So now it just needs attaching to the geography and saving with the others.
cob11geog <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
                     layer="scotland_ig_2011")

#1279 elements, should all merge...
cob11geog <- merge(cob11geog,cob11,by.x = 'interzone', by.y = 'IntermediateZone2011Code')

#Did that work? Yes.
cob11geog_df <- data.frame(cob11geog)

#Save as 2011 result with other stitching, keep only some columns
writeOGR(cob11geog, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",
         "2011_IntermediateGeog_via_lookupFromOAs", driver="ESRI Shapefile", overwrite_layer = T)


#Now, geog re-assign the other four.
cob71 <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/71.csv")
cob81 <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/81.csv")
cob91SAS <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/91SAS.csv")
cob01 <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/01.csv")

#~~~~~~~~~~~~~~~~~~~~
#GEOG RE-ASSIGNS-----
#And now for the various geog re-assigns for these four, based on previously done intersects.

#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

#~~~~~~~~~
#1971-----

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="1971_dissolved_EDsTo2011_IGs")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,cob71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",
         "1971_CoB_from_71EDs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~
#1981-----

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="1981_dissolved_EDsTo2011_IGs")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,cob81,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",
         "1981_CoB_from_81EDs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~
#1991-----

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="Scots_91OAs_dissolved_to11_IZs")

#look at merged columns to find right ID to match 91 OAs...?
its_df <- data.frame(its)

#1991 Scots OA columns: have (I think) two leading digits that need to be dropped to match the geography
#Checking...
unique(its_df$LABEL) %in% unique(cob91SAS$Zone.ID) %>% table

#Remove two leading chars from census data, check again
cob91SAS$Zone.ID_2 <- substring(cob91SAS$Zone.ID, 3)

#Check again... Better but far from perfect
#FALSE  TRUE 
#2910 35162 
unique(its_df$LABEL) %in% unique(cob91SAS$Zone.ID_2) %>% table

#What are the falses looking like? I don't think it's a set number on the right...
notz <- data.frame(unique(its_df[!(unique(its_df$LABEL) %in% unique(cob91SAS$Zone.ID_2)),1]))

#Hang on. Do we have trailing spaces anywhere too? Yup!
cob91SAS$Zone.ID_2[1]
#In the intersect? Newp. At least not here.
as.character(its_df$LABEL[1])

#Remove spaces from all, check again. Only trailing spaces, I'm fairly sure...
cob91SAS$Zone.ID_2 <- trimws(cob91SAS$Zone.ID, which='both')
cob91SAS$Zone.ID_2 <- substring(cob91SAS$Zone.ID_2, 3)

its@data$LABEL <- trimws(its@data$LABEL, which='both')

#That's more like it! 
#FALSE  TRUE 
#1 38071
unique(its_df$LABEL) %in% unique(cob91SAS$Zone.ID_2) %>% table

#What's the one remaining one?
oonq <- data.frame(unique(its_df$LABEL))
oonq$unique.its_df.LABEL[!(oonq$unique.its_df.LABEL) %in% unique(cob91SAS$Zone.ID_2)]

#Oh: "Loch". OK then! 

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
#Use the processed ID column
dta_zoneIDcolumn <- 16
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,cob91SAS,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",
         "1991_CoB_from_01OAs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)


#~~~~~~~~~
#2001-----

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="Scots_2001OAs_dissolved_to2011_IZs")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,cob01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw",
         "2001_CoB_from_2001OAs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)






