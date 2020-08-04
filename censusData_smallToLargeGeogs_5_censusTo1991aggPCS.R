#Stick time-consistent data (same columns) through the mill to get same zone re-assignment
source("Function_DataFromSmalltoLargeGeog.R")

#Large zones that all data will have in common
# lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
#                layer="scotland_pcs_1991_uniqueIDsperRow_LochsRemoved")

#Aggregated PCSs 822 zones
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
               layer="pseudoPCS_aggregated4CorrectCount")

#~~~~COB~~~~----

#~~~~~~~~
#1971----

its <- readOGR(dsn="Intersects/Scots_5_census_91_PCSzeroCounts_targetGeog", 
               layer="71EDsTo91PCSzeroCount")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/71.csv")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth",
#          "1971_CoB_from_71EDs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)

#better column abbreviation
writeSpatialShape(result,"StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/1971_CoB_from_71EDs_to_91_aggPostcodeSectors")


#~~~~~~~
#1981----

its <- readOGR(dsn="Intersects/Scots_5_census_91_PCSzeroCounts_targetGeog", 
               layer="81EDsTo91PCSzeroCount")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/81.csv")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth",
#          "1981_CoB_from_81EDs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)
#better column abbreviation
writeSpatialShape(result,"StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/1981_CoB_from_81EDs_to_91_aggPostcodeSectors")


#~~~~~~~~~~~
#2001----

#Can use intersect file from 3 census
its <- readOGR(dsn="Intersects/scots_3censusLBS_91PCSzeroCountIsTargetGeog", 
               layer="2001OAsTo91PCSzerocount")

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/01.csv")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth",
#          "2001_CoB_from_01OAs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)
#better column abbreviation
writeSpatialShape(result,"StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/2001_CoB_from_01OAs_to_91_aggPostcodeSectors")


#~~~~~~~~~~~
#2011----

its <- readOGR(dsn="Intersects/scots_3censusLBS_91PCSzeroCountIsTargetGeog", 
               layer="2011OAsTo91PCSzerocount")

#err...
#df <- data.frame(its)

dta <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/11.csv")

its_smallZoneIDColumn <- 2
its_largeZoneIDColumn <- 21
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth",
#          "2011_CoB_from_11OAs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)
#better column abbreviation
writeSpatialShape(result,"StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/2011_CoB_from_11OAs_to_91_aggPostcodeSectors")


#~~~~~~~~
#1991 done in 91_PCSzeroCountZones_agg...

#Most other variables (for 91 to 11) already done for 3 census
#But some bits left to do:

#~~~~EMPLOYMENT~~~~----

#71/81 Copied across and tweaked from household_n_employmentCensusDataTidy.R

ea71 <- read.csv("1971/Scotland/Scots_71_economicallyActive.csv")
ea81 <- read.csv("1981/Scotland/Scots_1981_economicallyActive_allTotals.csv")

#Ready to be stuffed through the geog re-assign
#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
               layer="pseudoPCS_aggregated4CorrectCount")

#1971----

#Total persons EA and total persons not in employment all that's required.
#Via eyeballing '71 CASWEB...
#First job: sum male/female (rows)
ea71b <- data.frame(ZoneCode = ea71[,1], 
                    total = apply(ea71[,c(3:5)],1,sum),
                    working = apply(ea71[,c(6:8)],1,sum),
                    Unempl = apply(ea71[,c(9:11)],1,sum),
                    sick = apply(ea71[,c(12:14)],1,sum)
)

#"Total" doesn't seem quite to be the total of the others
#Reckon I'll use the ratio of "working" and "seeking work".
#And presume "sick" means "long-term sick". 
#Though this is an EA table so that could be wrong. The sick numbers seem too high though...?

#ratio of working and seeking work, then...
ea71b$EA <- ea71b$working + ea71b$Unempl
ea71b$percentEmployed <- (1 - (ea71b$Unempl/ea71b$EA)) * 100

ea71b <- ea71b[,c(1,4,6:7)]

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_91_PCSzeroCounts_targetGeog", 
               layer="71EDsTo91PCSzeroCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:4)#from dta

result <- moveData(its,ea71b,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#debugonce(moveData(its,hse71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols))

#Oops. %employed again - do AFTER geog summing!
result@data$percentEmployed <- (1 - (result@data$Unempl/result@data$EA)) * 100

#save result!
writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/Employment",
         "1971_econActive_from_71EDs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)


#1981----

#Total persons EA and total persons not in employment all that's required.
#Via eyeballing '71 CASWEB...
#First job: sum male/female (rows)
ea81b <- data.frame(ZoneCode = ea81[,1], 
                    EA = ea81[,2],
                    Unempl = ea81[,12]
)

#Lrg same as above

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_91_PCSzeroCounts_targetGeog", 
               layer="81EDsTo91PCSzeroCount")

#spaces in '81 IDs
ea81b$ZoneCode <- trimws(ea81b$ZoneCode,which = 'both')
unique(its@data$label) %in% unique(ea81b$ZoneCode) %>% table#better!

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its,ea81b,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#debugonce(moveData(its,hse71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols))

#%employed - do AFTER geog summing!
result@data$percentEmployed <- (1 - (result@data$Unempl/result@data$EA)) * 100

#save result!
writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/Employment",
         "1981_econActive_from_81EDs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~YEAR OF ARRIVAL~~~~----

#tidy the file
# yoa <- read_csv('2011/Scotland/OutputAreaStd_ALL/QS801SCtidy.csv')
# yoa[yoa=='-'] <- 0

#write_csv(yoa,'2011/Scotland/OutputAreaStd_ALL/QS801SCtidyZeroes.csv')

yoa <- read_csv('2011/Scotland/OutputAreaStd_ALL/QS801SCtidyZeroes.csv')

#Do some summing to different values
names(yoa)

#keep: 
#born in the UK
#arrived before 71
#71/81
#81/91
#91/01
#01/11
yoa2 <- yoa[,c(1,3)]

yoa2$arrivedBefore1971 <- apply(yoa[,c(4:7)],1,sum)
yoa2$arrived1971to80 <- yoa$`Arrived 1971-1980`
yoa2$arrived1981to90 <- yoa$`Arrived 1981-1990`
yoa2$arrived1991to2000 <- yoa$`Arrived 1991-2000`
yoa2$arrived2001to2011 <- apply(yoa[,c(11:14)],1,sum)

write_csv(yoa2,'2011/Scotland/OutputAreaStd_ALL/QS801SCtidyZeroes_reducedCols.csv')

#yoa <- read_csv('2011/Scotland/OutputAreaStd_ALL/QS801SCtidyZeroes_reducedCols.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Ready for geog aggregation
its <- readOGR(dsn="Intersects/scots_3censusLBS_91PCSzeroCountIsTargetGeog", 
               layer="2011OAsTo91PCSzerocount")

dta <- read.csv('2011/Scotland/OutputAreaStd_ALL/QS801SCtidyZeroes_reducedCols.csv')

#dta[,c(2:7)] <- sapply(dta[,c(2:7)],as.numeric)

dta[is.na(dta)] <- 0
apply(dta[,c(2:7)],2,sum)

table(dta$zonecode %in% its@data$code)

its_smallZoneIDColumn <- 2
its_largeZoneIDColumn <- 21
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:7)#from dta

result <- moveData(its,dta,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

chk <- data.frame(result)
apply(chk[,c(3:8)],2,sum)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/YearOfArrival",
#          "2011_YearOfArrival_from_11OAs_to_91_aggPostcodeSectors", driver="ESRI Shapefile", overwrite_layer = T)
writeSpatialShape(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/YearOfArrival/2011_YearOfArrival_from_11OAs_to_91_aggPostcodeSectors.shp")

#A version that's proportions of the total zone pop
#And before/after 2001
chk$totalPop <- chk$Born.in.the.UK + apply(chk[,c(4:8)],1,sum)
chk$arrivedBefore2001 <- apply(chk[,c(4:7)],1,sum)

chk$arrivedBefore2001AsPercentOfPop <- (chk$arrivedBefore2001/chk$totalPop)*100
chk$arrivedAfter2001AsPercentOfPop <- (chk$arrived2001to2011/chk$totalPop)*100
#ratio of the two
chk$ratioAfterOverBeforePercents <- chk$arrivedAfter2001AsPercentOfPop/chk$arrivedBefore2001AsPercentOfPop

result@data <- chk[,c(1,3,10,8,12,11,13)]

writeSpatialShape(result, "StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/YearOfArrival/2011_YearOfArrival_from_11OAs_to_91_aggPostcodeSectors_ProportionsOfTotalPop.shp")





