source("Function_DataFromSmalltoLargeGeog.R")

#Census: household number and economic activity variables for 71 to 11
#All need varying degrees of tidying, combining or lookuping

#1.Households (not the same as dwellings, but...)
#1971: 'total households' is a single column. Just needs geog re-assign
hse71 <- read.csv("1971/Scotland/1971Scotland_privateHouseholds.csv")

#1981: tenure. A single column again. 
#Some problems with zeroes - could do some work to re-assign based on neighbouring values 
#being too far from some SD threshold (given those zeros are stuck into neighbours)
#But for now, leave alone
hse81 <- read.csv("1981/Scotland/scots_1981_tenure.csv")

#1991: tenure, single column again
hse91 <- read.csv("1991/Scotland/Scotland_1991_tenure_totalHouseholds/Scotland_1991_tenure_totalHouseholds.csv")

#2001: single column. 
hse01 <- read.csv("2001/Scotland/Scotland_2001_amenities_countAllHouseholds.csv")


#We also have a count of actual dwellings to compare to here
#Could perhaps see if there's some spatial pattern to the difference?
#ie. is it closer in cities?
hvd <- read.csv("2001/Scotland/households_vs_dwellings.csv")

OAs01 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2001/Scotland_outputareas_2001", 
                 layer="scotland_oa_2001")

unique(OAs01@data$name) %in% unique(hvd$Zone.Code) %>% table

OAs01 <- merge(OAs01,hvd,by.x='name',by.y='Zone.Code')

OAs01@data$ratio <- OAs01@data$households/OAs01@data$dwellings

writeOGR(OAs01, "temp",
         "01_households_vs_dwellings", driver="ESRI Shapefile", overwrite_layer = T)

#2011 housing: tidying and lookuping and stuff-----
#Now this will need some tidying - probably in Excel first
#KS105SC: Household composition
#Tidied in excel - do the empty slots load correctly?
hse11 <- read.csv("2011/Scotland/OutputAreaStd_ALL/KS105SCtidy.csv", as.is = T)
#Huh, it kept the dashes this time
class(hse11$One.person.household..Aged.65.and.over)

hse11 <- data.frame(apply(hse11,2,function(x) gsub("-",0,x)))

#Are there any commas, spaces in there?
apply(hse11, 2, function(x) grepl(" ",x)) %>% table
apply(hse11, 2, function(x) grepl(",",x)) %>% table

#No commas, a bunch of spaces. Just remove them...?
#Can I get a 'true' for any rows containing a space?
truz <- hse11[ apply(hse11,1,function(x) any(grepl(" ",x)) ), ]
#apply(hse11,1,function(x) x )

#Hmm. Let's just try making numeric...
#Which is fine, no NAs. So whatever spaces there are don't matter
hse11[,2:17] <- apply(hse11[,2:17],2, function(x) as.numeric(x))
#apply(hse11[,2:17],2,sd)

#Then we just need to keep 'all households'. And save for posterity.
write.csv(hse11[,1:2],"2011/Scotland/OutputAreaStd_ALL/KS105SCtidy_justAllHouseholds.csv", row.names = F)
hse11 <- read.csv("2011/Scotland/OutputAreaStd_ALL/KS105SCtidy_justAllHouseholds.csv")

#2011 just needs aggregating by lookup, since we're targeting 2011IZs.
lookup <- read.csv("2011/Scotland/lookup_OA_DZ_IZ_2011.csv")

#check it's got the right number of IDs. Yup!
unique(lookup$IntermediateZone2011Code) %>% length

#merge IZ codes into the house data
#they all match, right? Yup!
table(lookup$OutputArea2011Code %in% hse11$ZoneCode )

hse11 <- merge(hse11,lookup[,c(1,3)],by.x = 'ZoneCode', by.y = 'OutputArea2011Code')

#Aggregate by intermediate geography
#Select not working...
#Ah - masking raster functions or vice versa I think. Load order matter?
#https://github.com/hadley/dplyr/issues/643
hse11agg <- hse11 %>% dplyr::select(2:3) %>% 
  group_by(IntermediateZone2011Code) %>% 
  summarise_each(funs(sum))

#Column sums the same? Yup. Bonza.
sum(hse11[,2])
#apply(hse11agg[,2],2,sum)
sum(hse11agg[,2])

#Save
write.csv("StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Households/households_2011_IZ_lookup.csv", row.names = F)

#Attach to IZ geography and save
hse11geog <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
                     layer="scotland_ig_2011")

hse11geog <- hse11geog[,1]

hse11geog_df <- data.frame(hse11geog)

unique(hse11geog_df$interzone) %in% unique(hse11agg$IntermediateZone2011Code) %>% table
unique(hse11geog@data$interzone) %in% unique(hse11agg$IntermediateZone2011Code) %>% table

#1279 elements, should all merge...
hse11geog <- merge(hse11geog,hse11agg,by.x = 'interzone', by.y = 'IntermediateZone2011Code')
#This works fine. Whu?
hse11geog2 <- merge(hse11geog_df,hse11agg,by.x = 'interzone', by.y = 'IntermediateZone2011Code')

#Any chance I can just merge that with the geog?
#Apparently so. Right.
hse11geog3 <- merge(hse11geog,hse11geog2, by = 'interzone')

#Did that work? Yes.
hse11geog_df <- data.frame(hse11geog3)

#Save as 2011 result with other stitching, keep only some columns
writeOGR(hse11geog3, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Households",
         "2011_householdCount_IntermediateGeog_via_lookupFromOAs", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~~~~
#71 to 01 housing: small to large geog re-assign----

#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="1971_dissolved_EDsTo2011_IGs")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(3)#from dta

result <- moveData(its,hse71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#debugonce(moveData(its,hse71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols))

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Households",
         "1971_householdCount_from_71EDs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)

#1981----
#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="1981_dissolved_EDsTo2011_IGs")

#I think ID  names may not match here. I thought that was 1991...
#unique(its@data$interzone) %in%  unique(lrg@data$interzone) %>% table
#Ah. None.
unique(its@data$label) %in% unique(hse81$Zone.ID) %>% table

its_df <- data.frame(its)
nchar(as.character(its_df$label[1]))
nchar(as.character(hse81$Zone.ID[1]))#Spaces!

hse81$Zone.ID <- trimws(hse81$Zone.ID,which = 'both')
unique(its@data$label) %in% unique(hse81$Zone.ID) %>% table#better!

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2)#from dta

result <- moveData(its,hse81,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Households",
         "1981_householdCount_from_81EDs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)


#1991----
#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="Scots_91OAs_dissolved_to11_IZs")

#ID needs tweaking, right? Yup!
unique(its@data$LABEL) %in% unique(hse91$Zone.ID) %>% table
#Same as before: two leading chars and remove spaces
its@data[1,1]
hse91$Zone.ID[1]

#drop first two chars
hse91$Zone.ID <- substring(hse91$Zone.ID,3)
#drop all spaces
hse91$Zone.ID <- trimws(hse91$Zone.ID, which = 'both')

#One remaining one is Loch innit?
unique(its@data$LABEL) %in% unique(hse91$Zone.ID) %>% table

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2)#from dta

result <- moveData(its,hse91,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Households",
         "1991_householdCount_from_91OAs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)


#2001----
#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="Scots_2001OAs_dissolved_to2011_IZs")

#Check IDs. Tick!
unique(its@data[,1]) %in% unique(hse01$Zone.Code) %>% table

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2)#from dta

result <- moveData(its,hse01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Households",
         "2001_householdCount_from_01OAs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~~~~~~~~~~~~~~~~
#EMPLOYMENT/ECONOMICALLY ACTIVE-----
ea71 <- read.csv("1971/Scotland/Scots_71_economicallyActive.csv")
ea81 <- read.csv("1981/Scotland/Scots_1981_economicallyActive_allTotals.csv")
ea91 <- read.csv("1991/Scotland/Scots_1991_SAS_economicPosition/Scots_1991_SAS_economicPosition.csv")
ea01 <- read.csv("2001/Scotland/2001_Scots_economicallyActive.csv")
ea11 <- read.csv("2011/Scotland/OutputAreaStd_ALL/KS601SCtidy.csv")

#~~~~~~~~~~
#2011:-----
#2011 first: file tidy and geog lookup, same as for housing.
ea11 <- data.frame(apply(ea11,2,function(x) gsub("-",0,x)))

#Some commas...
apply(ea11, 2, function(x) grepl(",",x)) %>% table
ea11 <- data.frame(apply(ea11,2,function(x) gsub(",","",x)))

ea11[,2:16] <- apply(ea11[,2:16],2, function(x) as.numeric(x))

write.csv(ea11,"2011/Scotland/OutputAreaStd_ALL/KS601SCtidy_zeroscommasremoved.csv", row.names = F)
ea11 <- read.csv("2011/Scotland/OutputAreaStd_ALL/KS601SCtidy_zeroscommasremoved.csv")

#Find economically active total vs EA unemployed
names(ea11)
#This works but data.frame allows naming of columns directly...
#ea11agg <- do.call(cbind, list(ea11[,1,drop = F],apply( ea11[,c(3:5)],1,sum)  ))
ea11agg <- data.frame(ZoneCode = ea11[,1,drop = F],
                      EA = (apply( ea11[,c(3:6)],1,sum)),
                      Unempl = ea11[,6])

#Update: do this later so we're not summing a % below!
#ea11agg$percentEmployed = (1 - (ea11agg$Unempl/ea11agg$EA))*100
#high <- ea11agg %>% filter(percentEmployed < 60) 

#Use lookup to assign to IZs
lookup <- read.csv("2011/Scotland/lookup_OA_DZ_IZ_2011.csv")

#merge IZ codes into the house data
#they all match, right? Yup!
table(lookup$OutputArea2011Code %in% ea11agg$ZoneCode )

ea11agg <- merge(ea11agg,lookup[,c(1,3)],by.x = 'ZoneCode', by.y = 'OutputArea2011Code')

#Aggregate by intermediate geography
#Select not working...
#Ah - masking raster functions or vice versa I think. Load order matter?
#https://github.com/hadley/dplyr/issues/643
ea11agg2 <- ea11agg %>% dplyr::select(2:4) %>% 
  group_by(IntermediateZone2011Code) %>% 
  summarise_each(funs(sum))

#NOW add %!
ea11agg2$percentEmployed = (1 - (ea11agg2$Unempl/ea11agg2$EA))*100

#Column sums the same? Yup. Bonza.
sum(ea11agg[,2])
#apply(hse11agg[,2],2,sum)
sum(ea11agg2[,2])

#Save
write.csv(ea11agg2,"StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/econActive_2011_IZ_lookup.csv", row.names = F)

#Attach to IZ geography and save
ea11geog <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
                     layer="scotland_ig_2011")

ea11geog <- ea11geog[,1]

ea11geog_df <- data.frame(ea11geog)

unique(ea11geog_df$interzone) %in% unique(ea11agg2$IntermediateZone2011Code) %>% table
unique(ea11geog@data$interzone) %in% unique(ea11agg2$IntermediateZone2011Code) %>% table

#1279 elements, should all merge... Nope, still struggling for no reason I can fathom.
ea11geog <- merge(ea11geog,ea11agg2,by.x = 'interzone', by.y = 'IntermediateZone2011Code')
#Whereas this works fine. Whu?
ea11geog2 <- merge(ea11geog_df,ea11agg2,by.x = 'interzone', by.y = 'IntermediateZone2011Code')

#Any chance I can just merge that with the geog?
#Apparently so. Right.
ea11geog3 <- merge(ea11geog,ea11geog2, by = 'interzone')

#Did that work? Yes.
ea11geog_df <- data.frame(ea11geog3)

#Save as 2011 result with other stitching, keep only some columns
writeOGR(ea11geog3, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment",
         "2011_econActive_IntermediateGeog_via_lookupFromOAs", driver="ESRI Shapefile", overwrite_layer = T)

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

#Ready to be stuffed through the geog re-assign
#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="1971_dissolved_EDsTo2011_IGs")

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
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment",
         "1971_econActive_from_71EDs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)


#1981----

#Total persons EA and total persons not in employment all that's required.
#Via eyeballing '71 CASWEB...
#First job: sum male/female (rows)
ea81b <- data.frame(ZoneCode = ea81[,1], 
                    EA = ea81[,2],
                    Unempl = ea81[,12]
                    )


#Ready to be stuffed through the geog re-assign
#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="1981_dissolved_EDsTo2011_IGs")

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

#Oops. %employed again - do AFTER geog summing!
result@data$percentEmployed <- (1 - (result@data$Unempl/result@data$EA)) * 100

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment",
         "1981_econActive_from_81EDs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)



#1991----

#Ready to be stuffed through the geog re-assign
#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="Scots_91OAs_dissolved_to11_IZs")

#ID needs tweaking, right? Yup!
unique(its@data$LABEL) %in% unique(ea91$Zone.ID) %>% table
#Same as before: two leading chars and remove spaces
its@data[1,1]
ea91$Zone.ID[1]

#drop first two chars
ea91$Zone.ID <- substring(ea91$Zone.ID,3)
#drop all spaces
ea91$Zone.ID <- trimws(ea91$Zone.ID, which = 'both')

#One remaining one is Loch innit?
unique(its@data$LABEL) %in% unique(ea91$Zone.ID) %>% table

#~~~
#12 and 166: total EA males and females
#On govt scheme or unemployed M/F is 67+78+221+232
ea91b <- data.frame(ZoneCode = ea91[,1],
                    EA = apply(ea91[,c(2,5)],1,sum),
                    Unempl = apply(ea91[,c(3:4,6:7)],1,sum)
                    )

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its,ea91b,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#debugonce(moveData(its,hse71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols))

#Oops. %employed again - do AFTER geog summing!
result@data$percentEmployed <- (1 - (result@data$Unempl/result@data$EA)) * 100

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment",
         "1991_econActive_from_91OAs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)


#2001----

#2001: we actually have the right columns to start with - EA and unempl.
#Just need to change names
names(ea01) <- c("ZoneCode","EA","Unempl")

#Ready to be stuffed through the geog re-assign
#Get large geog that re-assigned data will be attached to.
lrg <- readOGR(dsn="C:/Data/MapPolygons/Scotland/2011/Scotland_IntermediateGeography_2011", 
               layer="scotland_ig_2011")

#Get the intersect geog:
its <- readOGR(dsn="Intersects/Scots_5_census_11_IZ_targetGeog", 
               layer="Scots_2001OAs_dissolved_to2011_IZs")


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its,ea01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#debugonce(moveData(its,hse71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols))

#Oops. %employed again - do AFTER geog summing!
result@data$percentEmployed <- (1 - (result@data$Unempl/result@data$EA)) * 100

#save result!
writeOGR(result, "StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment",
         "2001_econActive_from_01OAs_to_2011_IntermediateGeog", driver="ESRI Shapefile", overwrite_layer = T)







