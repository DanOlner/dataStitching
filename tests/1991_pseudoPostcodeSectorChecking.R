source("Function_Shapefile_uniqueIDs_to_singleRowFeatures.R")

#Postcode sector zone / data checking.
#The base geography for the whole thing (in Scotland at any rate - wards down south)
#See http://www.nrscotland.gov.uk/files/geography/products/1991-census-bkgrd.pdf
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat")
lapply(geolibs, require, character.only = TRUE)

pseudo <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991", 
                         layer="scotland_oas_1991")

#processed actual postcode sectors to compare to
actual <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_postcodesectors_1991", 
                         layer="scotland_pcs_1991_uniqueIDsperRow_LochsRemoved")
 
# #filter out Lochs first...
# pseudo_df <- data.frame(pseudo)
# pseudo_df %>% filter(name == "Loch") %>% nrow
# 
# actual_df <- data.frame(actual)
# 
# #There's an empty NA row we need to drop first...
# table(0 + is.na(pseudo@data$name))
# 
# pseudo <- pseudo[!is.na(pseudo@data$name),]
# 
# #then drop Lochs
# pseudo <- pseudo[pseudo@data$name != "Loch",]

#Make pseudo zone IDs on same row
pseudo2 <- IDtoSingleRow(pseudo,3)

pseudo2@data$label[grepl("Loch",pseudo2@data$label,ignore.case = T)] %>% length

#Filter lochs out
pseudo2 <- pseudo2[!grepl("Loch",pseudo2@data$label,ignore.case = T),]

#Right, now it's down to 1002 rows. Save.
writeOGR(pseudo2, "C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991",
         "soctland_pseudo_pcs_IDtoSingleRow", driver="ESRI Shapefile", overwrite_layer = T)

#reload and use
# pseudo2 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991", 
#                   layer="soctland_pseudo_pcs_IDtoSingleRow")

#Uh oh - single error in polygon labelling.
#See below - Dundee docks have wrong label in the shapefile
#Currently 'w'
#Should be 6452AC (DD1 3 is postcode)
#Fix and re-save
#Currently a fecking factor
levels(pseudo2@data$label) <- c(levels(pseudo2@data$label), '6452AC')
pseudo2@data$label[pseudo2@data$label == 'w'] <- '6452AC'

#save again
writeOGR(pseudo2, "C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991",
         "soctland_pseudo_pcs_IDtoSingleRow", driver="ESRI Shapefile", overwrite_layer = T)

#reload and use
pseudo2 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991", 
                   layer="soctland_pseudo_pcs_IDtoSingleRow")

#~~~~~~

#I think we might have correct non-postcode matching IDs now?
cob91 <- read.csv("VariableCoding/CountryOfBirth_fiveCensusRecodes/91LBS.csv")

#cob91 <- cob91[order(0 + rowMeans(cob91[,3:16])!=0),]
#Remove rows that contain only zeroes
#1003 to 822
cob91 <- cob91[rowMeans(cob91[,3:16])!=0,]

head(pseudo2@data$label)
head(cob91$Zone.ID)

#Yes, they are looking similar.
#Less census than zone IDs. Huh.
zoneIDs <- unique(pseudo2@data$label)
cobIDs <- unique(cob91$Zone.ID)

#All none-zero census IDs have a zone to attach to
# table (censusIDs %in% zoneIDs)
# #179 pseudo-postcode sectors with no data
# table (zoneIDs %in% censusIDs)

#Update: now I've worked out that zero cols in the data
#Are counts < 1000 (and re-assigned to neighbours)
#Still have to work out what's the crack with IDs

#Let's also load pop numbers to check 
pop91 <- read.csv("1991/Scotland/Scots_1991_LBS_popBase_AllPersons_PostcodeSector/Scots_1991_LBS_popBase_AllPersons_PostcodeSector.csv")

#Same obs/IDs, got the right one. So?
popIDs <- unique(pop91$Zone.ID)

#Still a lot of extra IDs in the zones. Where?
table (popIDs %in% zoneIDs)
#In the data: one is shipping... unsure what the other one is.
pop91[!(popIDs %in% zoneIDs),]

table(zoneIDs %in% popIDs)

#OK, so now none missing in the zones. Right-ee-ho.
#Let's check on OAs.
oas91 <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_outputareas_1991", 
                            layer="91_OAs_unionSpatialPolygons")

#Load OA-level pop numbers
pop91oa <- read.csv("1991/Scotland/Scotland_1991_SAS_populationBase_TOTALPERSONS/Scotland_1991_SAS_populationBase_TOTALPERSONS.csv")

#Drop first two chars and spaces
pop91oa$Zone.ID2 <- substr(pop91oa$Zone.ID,3,10) %>% gsub(" ","",.)

#1 non-match in the geog. 
table(oas91@data$LABEL %in% pop91oa$Zone.ID2)

#Which is? Oh. A loch.
oas91@data$LABEL[!(oas91@data$LABEL %in% pop91oa$Zone.ID2)]
#oas91@data[grepl("Loch",oas91@data$LABEL),]


#Hum: it turns out there's a label naming convention 
#that should allow me to bin the data from OAs into PCSs
#Without going through any intersect re-assign stuff

#e.g. it looks like this...
#OA: 07BA11B, PCS: 5707BA (Drop 57...)
#OA: 17AR02, PCS: 6017AR (Drop 60...)
#Actually, it matches the original OA zone.id with those two digits too.
#May just have to remove spaces

#Field - s010064: residents present 1991, sums the first four. Can use that for comparison.

#So let's just check we can get matches on that
#All true. good good!
pop91$Zone.ID %in% substr(pop91oa$Zone.ID,1,6) %>% table

pop91oa$pcs_label <- substr(pop91oa$Zone.ID,1,6)

#Aggregate by PCS...
pop91oa_agg <- pop91oa %>% group_by(pcs_label) %>% 
  summarise(s64fromOAs = sum(s010064))

#Attach back to pop in pcs
pop91a <- left_join(pop91,pop91oa_agg,by=c('Zone.ID'='pcs_label'))

#So far so good. The sums are also exact. This may be doable.

#~~~~~~~~~~~~~~~~~~~
#Some zone-checking----

#~~~~~~~~~~~~
#Check it's the same zones with differences in the other tables
#Use CoB as example
cob91OA <- read.csv("1991/Scotland/1991_Scotland_SAS_CoB_OutputAreas/1991_Scotland_SAS_CoB_OutputAreas__noMaleFemale_countyNamesAdded.csv")

#Yup, same in this table - can nest em
cob91$Zone.ID %in% substr(cob91OA$Zone.ID,1,6) %>% table

cob91OA$pcs_label <- substr(cob91OA$Zone.ID,1,6)

#Aggregate by PCS. Use England/Scotland sums as example
cob91oa_agg <- cob91OA %>% group_by(pcs_label) %>% 
  summarise(EngfromOAs = sum(England), ScotFromOAs = sum(Scotland))

#Attach back to pop in pcs
cob91_chk <- left_join(cob91,cob91oa_agg,by=c('Zone.ID'='pcs_label'))

#Drop ones we don't need to look at
cob91_chk <- cob91_chk %>% dplyr::select(Zone.ID:Scotland,EngfromOAs:ScotFromOAs)

cob91_chk$missingEng <- cob91_chk$England - cob91_chk$EngfromOAs
cob91_chk$missingScots <- cob91_chk$Scotland - cob91_chk$ScotFromOAs

#~~~~~~~~~~~~~~
#Check again with different table. Economic position...
#Output areas
ep91oa <- read.csv("1991/Scotland/Scots_1991_SAS_economicPosition/Scots_1991_SAS_economicPosition.csv")

#Postcode sector
ep91pcs <- read.csv("1991/Scotland/Scots_1991_LBS_economicPosition_PostcodeSector/Scots_1991_LBS_economicPosition_PostcodeSector.csv")

#output area comes in male/female breakdown - need to sum.
#Let's compare 'economically active' in both.
#ep91oa male female EA is 12 and 166
ep91oa$econActiveTotal <- ep91oa$s080012 + ep91oa$s080166

#Can now do comparison...
#Tick on zones again
ep91pcs$Zone.ID %in% substr(ep91oa$Zone.ID,1,6) %>% table

ep91oa$pcs_label <- substr(ep91oa$Zone.ID,1,6)

#Aggregate by PCS. Use England/Scotland sums as example
ep91oa_agg <- ep91oa %>% group_by(pcs_label) %>% 
  summarise(EA_from_OA = sum(econActiveTotal))

#Attach back to pop in pcs
ep91_chk <- left_join(ep91pcs,ep91oa_agg,by=c('Zone.ID'='pcs_label'))

#Drop ones we don't need to look at
cob91_chk <- cob91_chk %>% dplyr::select(Zone.ID:Scotland,EngfromOAs:ScotFromOAs)

#They're all looking... sort of roughly in the right place! 
#Definitely worth checking. First point: the zero zones *are* all in the same place
#Which they wouldn't be if they'd all been calculated individually.
#And it would be madness to do it by table.
#The problem with picking up on them here is small counts at OA level - I think.


#~~~~~~~~~~
#Back to reassigns----

#Let's have a go at picking out those to be combined into one zone
#First-up: pick the PCS zero rows. The OA values in this row are the ones
#That will have been re-assigned to another row.
#Most of them are actually contiguous in the table itself
#But we probably want a better way of finding out where they went

#181 zeros with values that ended up in different columns
#s64fromOAs contains the values that were removed and reassigned elsewhere
reassigns <- pop91a[pop91a$l010064==0, c('Zone.ID','s64fromOAs')]

#How many of those are unique values? ... Not enough!
unique(reassigns$s64fromOAs) %>% length
reassigns <- reassigns[order(reassigns$s64fromOAs),]

#So we need to get on contiguous zones. Goody.
#(Done below)
table(pop91a$Zone.ID %in% pseudo2@data$label)
table(pseudo2@data$label %in% pop91a$Zone.ID)

#Errr...
plot(pseudo2[!(pseudo2@data$label %in% pop91a$Zone.ID),])

#W???
pseudo2@data$label[!(pseudo2@data$label %in% pop91a$Zone.ID)]

#W is in the shapefile data: Dundee port. Is it mis-labeled?
#What are the two in the data itself? One is shipping. The other?
pop91a[!(pop91a$Zone.ID  %in% pseudo2@data$label),]

#DD1 3 is the postcode... I think it's mislabelled in the shapefile... Yup.
#Fix that above, then carry on!
#Fixed... test: tick.
plot(pseudo2[pseudo2@data$label=='6452AC',])

#Better! All zones accounted for.
table(pseudo2@data$label %in% pop91a$Zone.ID)


#How many can we get just using the re-assign check?
#Keep unique-only (i.e. none with duplicates) of the zero rows
reassigns_uniques <- subset(reassigns,!duplicated(reassigns$s64fromOAs)&!duplicated(reassigns$s64fromOAs, fromLast = T))

#Get values looking to match on the subtracted value
#We only need to merge with non-zero PCSs - these are the zones we'll end up with
#Won't get them all this time, but should get 138
pop91a_newZones <- pop91a[pop91a$l010064!=0,]
#A lot of these will be zero, which is right: no count missing.
pop91a_newZones$missingCount <- pop91a_newZones$l010064 - pop91a_newZones$s64fromOAs

pop91a_newZones <- left_join(pop91a_newZones,reassigns_uniques,by = c('missingCount' = 's64fromOAs'))


#Actually, let's do that for the full list so I can still see the zero ones
pop91a$missingCount <- pop91a$l010064 - pop91a$s64fromOAs
pop91missings1 <- left_join(pop91a,reassigns_uniques,by = c('missingCount' = 's64fromOAs'))

#Hmm: for a lot of the remainder, the first four digits of the PCS match
#Wonder how many can be got from that combo?

#First up, how many four-digit/count pairs are unique in the zero reassigns?
reassigns$ZoneID4digit <- substring(reassigns$Zone.ID,1,4)

#Only short by one, you say? Eeenstering.
unique(reassigns[,c("s64fromOAs","ZoneID4digit")]) %>% nrow

#Let's select those and try another match
reassigns_uniquePair <- subset(reassigns,
                               !duplicated(reassigns[,c("s64fromOAs","ZoneID4digit")])
                               &!duplicated(reassigns[,c("s64fromOAs","ZoneID4digit")], fromLast = T))

#having re-created pop91a above...
pop91a$ZoneID4digit <- substring(pop91a$Zone.ID,1,4)

# pop91a <- merge(pop91a,reassigns_uniquePair,
#                 by.x = c('missingCount','ZoneID4digit'),
#                 by.y = c('s64fromOAs','ZoneID4digit'),
#                 all.x = T)

pop91missings2 <- left_join(pop91a,reassigns_uniquePair,by = c('missingCount' = 's64fromOAs','ZoneID4digit'))

table(!is.na(pop91missings1$Zone.ID.y))
table(!is.na(pop91missings2$Zone.ID.y))

#Well, that was a nice theory. Didn't really pan out...
#Actually, it did do better. But did it do more, or different?
#Mostly - one got lost in the four-digit check.
table(unique(pop91missings1$Zone.ID.y) %in% unique(pop91missings2$Zone.ID.y))

#So why are we such low match checks? Need to track down some of the examples. Look at on map?
saveToLook <- pseudo2

saveToLook@data <- merge(saveToLook@data, pop91missings2, by.x = "label", by.y="Zone.ID.x", all.x = T)

chk <- data.frame(saveToLook)

writeOGR(saveToLook, "C:/Data/temp/QGIS/census",
         "91_pseudoPCS_checkMatchings", driver="ESRI Shapefile", overwrite_layer = T)

#OK, so having looked:
#The remainder are (mostly, I think) zones that got multiple zone input into them
#So one approach:
#Try all combinations of pairs, then of triplets
#Which of these, when subtracted from the remaining, give us a match?

#So we need to just look at zeros we've yet to match
combo2 <- pop91missings2[pop91missings2$l010064==0 
                         & !(pop91missings2$Zone.ID.x %in% unique(pop91missings2$Zone.ID.y)),] %>% 
                           dplyr::select(Zone.ID = Zone.ID.x,s64fromOAs)
                         
#All pair combinations of this, with sums to match against
#http://stackoverflow.com/questions/30311908/expand-data-frame-into-combinations-of-row-pairs
combo2pairs <- data.frame(Zone.ID1 = combn(combo2$Zone.ID,2)[1,],
                          Zone.ID2 = combn(combo2$Zone.ID,2)[2,],
                          OAcountSum = combn(combo2$s64fromOAs,2)[1,] + combn(combo2$s64fromOAs,2)[2,])

#Don't need them to be unique this time.

#Sooo... do we find any from that? Same procedure as before...
#Though we need to check on uniqueness again.
unique(combo2pairs$OAcountSum) %>% length
#Use first zone for this ref
combo2pairs$ZoneID4digit <- substr(combo2pairs$Zone.ID1,1,4)
#This isn't bad. Let's try that.
unique(combo2pairs[,c('ZoneID4digit','OAcountSum')]) %>% nrow

combo2pairs_unique <- subset(combo2pairs,
                             !duplicated(combo2pairs[,c('ZoneID4digit','OAcountSum')]) &
                               !duplicated(combo2pairs[,c('ZoneID4digit','OAcountSum')],fromLast = T))
                               

#Matches? Matching again against missingCount
#The OA value for this zone gives us what it should be
#Any difference may have come from multiple other zones...
#... is the theory...
pop91missings2copy <- pop91missings2

pop91missings2copy <- left_join(pop91missings2copy, combo2pairs_unique, 
                                by = c('missingCount'= 'OAcountSum','ZoneID4digit'))

#How many additional matches on top of the single-matches?
#18, which is 32 extra zones accounted for. Onward!
table(!is.na(pop91missings2copy$Zone.ID1) & is.na(pop91missings2copy$Zone.ID.y))

#Look more closely...
looksee <- pop91missings2copy %>% dplyr::select(Zone.ID.x, l010064, s64fromOAs, 
                                                missingCount, Zone.ID.y, Zone.ID1, Zone.ID2)

#So: any that got matches where a single had matched:
#They're not from nearby zones. So the single match is correct.

#How many zones with differing values remain?
table((pop91missings2copy$missingCount > 0) 
      & is.na(pop91missings2copy$Zone.ID.y) 
      & is.na(pop91missings2copy$Zone.ID1))

#Only 16? Can I just check I've got the right number in total?
#122? Oh, that is right, isn't it? Cos of multiple zones feeding into that.
#So we had 88 single matches, 16 zones from pairs is 104. Leaves 16.
table(pop91missings2copy$missingCount > 0)

#~~~~~~~~~~~~~
#So repeat for three then four zones, then we'll probably be there.
#THREE!

pop91missings2 <- pop91missings2copy %>% dplyr::select(Zone.ID.x:Zone.ID2)

#So we need to just look at zeros we've yet to match
combo3 <- pop91missings2[pop91missings2$l010064==0 
      & !(pop91missings2$Zone.ID.x %in% unique(c(pop91missings2$Zone.ID.y,as.character(pop91missings2$Zone.ID1)))),] %>% 
  dplyr::select(Zone.ID = Zone.ID.x,s64fromOAs)

#All pair combinations of this, with sums to match against
#http://stackoverflow.com/questions/30311908/expand-data-frame-into-combinations-of-row-pairs
combo3triplets <- data.frame(TripleZone.ID1 = combn(combo3$Zone.ID,3)[1,],
                          TripleZone.ID2 = combn(combo3$Zone.ID,3)[2,],
                          TripleZone.ID3 = combn(combo3$Zone.ID,3)[3,],
                          OAcountSum = combn(combo3$s64fromOAs,3)[1,] 
                          + combn(combo3$s64fromOAs,3)[2,]
                          + combn(combo3$s64fromOAs,3)[3,])


#Same procedure as before...
#Though we need to check on uniqueness again.
unique(combo3triplets$OAcountSum) %>% length
#Use first zone for this ref
combo3triplets$ZoneID4digit <- substr(combo3triplets$TripleZone.ID1,1,4)
#This isn't bad. Let's try that.
unique(combo3triplets[,c('ZoneID4digit','OAcountSum')]) %>% nrow

combo3triplets_unique <- subset(combo3triplets,
                             !duplicated(combo3triplets[,c('ZoneID4digit','OAcountSum')]) &
                               !duplicated(combo3triplets[,c('ZoneID4digit','OAcountSum')],fromLast = T))


#Matches? Matching again against missingCount
#The OA value for this zone gives us what it should be
#Any difference may have come from multiple other zones...
#... is the theory...
pop91missings2copy <- pop91missings2

pop91missings2copy <- left_join(pop91missings2copy, combo3triplets_unique, 
                                by = c('missingCount'= 'OAcountSum','ZoneID4digit'))

#How many additional matches on top of the single-matches?
table(!is.na(pop91missings2copy$TripleZone.ID1) 
      & is.na(pop91missings2copy$Zone.ID1) 
      & is.na(pop91missings2copy$Zone.ID.y))

#Two zones. So six other zones into these, leaves... ten.

#Look more closely...
looksee <- pop91missings2copy %>% dplyr::select(Zone.ID.x, l010064, s64fromOAs, 
                                                missingCount, Zone.ID.y, Zone.ID1, Zone.ID2,
                                                TripleZone.ID1,TripleZone.ID2,TripleZone.ID3)

#I'm pretty sure the two matches are wrong...
#You know what... for the remainder, might do it manually.
#Given I've just seen a double that should have been picked up on and wasn't, somehow.
#Oh actually, it was: the triple's just wrong.
#We only have 

#Let's look at what we've got, excluding the triple

#How many zones with differing values remain?
table((pop91missings2copy$missingCount > 0) 
      & is.na(pop91missings2copy$Zone.ID.y) 
      & is.na(pop91missings2copy$Zone.ID1))


#~~~~~~~~~~
#Just try with four....

pop91missings4 <- pop91missings2copy %>% dplyr::select(Zone.ID.x:Zone.ID2)

#So we need to just look at zeros we've yet to match
combo4 <- pop91missings4[pop91missings4$l010064==0 
                         & !(pop91missings4$Zone.ID.x %in% unique(c(pop91missings4$Zone.ID.y,
                                                                    as.character(pop91missings4$Zone.ID1)))),] %>% 
  dplyr::select(Zone.ID = Zone.ID.x,s64fromOAs)

#All pair combinations of this, with sums to match against
#http://stackoverflow.com/questions/30311908/expand-data-frame-into-combinations-of-row-pairs
combo4quartets <- data.frame(TripleZone.ID1 = combn(combo4$Zone.ID,4)[1,],
                             TripleZone.ID2 = combn(combo4$Zone.ID,4)[2,],
                             TripleZone.ID3 = combn(combo4$Zone.ID,4)[3,],
                             TripleZone.ID4 = combn(combo4$Zone.ID,4)[4,],
                             OAcountSum = combn(combo4$s64fromOAs,4)[1,] 
                             + combn(combo4$s64fromOAs,4)[2,]
                             + combn(combo4$s64fromOAs,4)[3,]
                             + combn(combo4$s64fromOAs,4)[4,])


#Same procedure as before...
#Though we need to check on uniqueness again.
unique(combo4quartets$OAcountSum) %>% length
#Use first zone for this ref
combo4quartets$ZoneID4digit <- substr(combo4quartets$TripleZone.ID1,1,4)
#This isn't bad. Let's try that.
unique(combo4quartets[,c('ZoneID4digit','OAcountSum')]) %>% nrow

combo4quartets_unique <- subset(combo4quartets,
                                !duplicated(combo4quartets[,c('ZoneID4digit','OAcountSum')]) &
                                  !duplicated(combo4quartets[,c('ZoneID4digit','OAcountSum')],fromLast = T))


#Matches? Matching again against missingCount
#The OA value for this zone gives us what it should be
#Any difference may have come from multiple other zones...
#... is the theory...
pop91missings4copy <- pop91missings4

pop91missings4copy <- left_join(pop91missings4copy, combo4quartets_unique, 
                                by = c('missingCount'= 'OAcountSum','ZoneID4digit'))

#How many additional matches on top of the single-matches?
table(!is.na(pop91missings4copy$TripleZone.ID1) 
      & is.na(pop91missings4copy$Zone.ID1) 
      & is.na(pop91missings4copy$Zone.ID.y))

#I think just stick with pairs and have a look! Let's throw into the map to see.
#So why are we such low match checks? Need to track down some of the examples. Look at on map?
saveToLook <- pseudo2
plot(saveToLook[saveToLook@data$label=='6756AE',])

saveToLook <- merge(saveToLook, pop91missings4, by.x = "label", by.y="Zone.ID.x", all.x = T)

chk <- data.frame(saveToLook)

writeOGR(saveToLook, "C:/Data/temp/QGIS/census",
         "pairMatches_91_pseudoPCS_checkMatchings", driver="ESRI Shapefile", overwrite_layer = T)

#It would be useful to mark the zero-zones we found matches for...
#Note, need to default to single-match if there's a choice between both.
pairzones <- pop91missings4[is.na(pop91missings4$Zone.ID.y)
                            &!is.na(pop91missings4$Zone.ID1),c("Zone.ID1","Zone.ID2")]

pairzonesVec <- c(as.character(pairzones$Zone.ID1),as.character(pairzones$Zone.ID2))
pairzonesVec <- unique(pairzonesVec)#Just checking!

#combine those with the single zones... 
singleZones <- pop91missings4$Zone.ID.y[!is.na(pop91missings4$Zone.ID.y)]

allZones <- c(singleZones,pairzonesVec)
#they should all be unique... Yup. Phew.
unique(allZones) %>% length

#So these are all the zones that got reassigned. Save to look at, help track down what's left.
saveToLook <- pseudo2

#One less. Shipping, huh?
saveToLook <- saveToLook[saveToLook@data$label %in% allZones,]

plot(saveToLook[saveToLook@data$label=='6756AE',])
plot(pseudo2[pseudo2@data$label=='6756AE',])

chk <- data.frame(saveToLook)

writeOGR(saveToLook, "C:/Data/temp/QGIS/census",
         "Zero_PCSs_reassignsFound", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~~~~~~~~~~~
# Remaining matches----

#Looking all good so far (I think!) One pair error (having trawled through all pairs in QGIS)
#Let's take a look at what's left, removing that pair error:
#6332AT, got 6332BL and 6341AE - island/coastal area, many more unassigned.

#A copy before removing the possibly awry pair
pop91manual <- pop91missings4
pop91manual[pop91manual$Zone.ID.x=='6332AT',c("Zone.ID1","Zone.ID2")] <- NA

#Remove any pairs that already had a matching single. 
#Err. I think. Maybe I should check that... Yeah, they're not right.
#Should be five...
pop91manual[!is.na(pop91manual$Zone.ID.y) & !is.na(pop91manual$Zone.ID1) ,c("Zone.ID1","Zone.ID2")] %>% nrow
#Yup. Wipe.
pop91manual[!is.na(pop91manual$Zone.ID.y) & !is.na(pop91manual$Zone.ID1) ,c("Zone.ID1","Zone.ID2")] <- NA

#So what's left? mark for easy viewing
pop91manual$still_left <- NULL

#17 zones to re-assign to a smaller number of zeros...
pop91manual[pop91manual$missingCount > 0 
                       & (is.na(pop91manual$Zone.ID.y) & is.na(pop91manual$Zone.ID1)),] %>% nrow

pop91manual$still_left[pop91manual$missingCount > 0 
            & (is.na(pop91manual$Zone.ID.y) & is.na(pop91manual$Zone.ID1))] <- 1

#Reduce cols for viewing:
pop91manual <- pop91manual %>% dplyr::select(Zone.ID.x,l010064,s64fromOAs,missingCount,ZoneID4digit,Zone.ID.y,
  Zone.ID1,Zone.ID2,still_left)

#OK, we have:
#5602AH gets two: 397 from 5602AB, 5602AJ
#(Must be another 397...)
#5707AG gets two: 290 from 5707AC, 5707AD
#5707AX gets three: 949 from 5707AT, 5707AU, 5707AW
#6121AG gets four: 1672 from 6121AC, 6121AD, 6121AE, 6121AF
#6123AT gets four: 957 from 6123AN, 6123AP, 6123AQ, 6123AR
#6126AM gets two: 555 from 6126AK, 6126AL
#6126AC gets two: 482 from 6126AP, 6126AQ (easier seen on map, that one)
#6229AK gets four: 600 from 6229AD, 6229AE, 6229AF, 6229AG
#6332BM gets EIGHT: 2574 from 6332BD, 6332BE, 6332BF, 6332BG, 6332BH, 6332BJ, 6332BK, 6332BL
#6347AT: single - 6347AC
#6347AU: single - 6347AG (duplicate values, couldn't match. Geography match woulda!)
#6126AJ gets five: 1382 from 6126AD, 6126AE, 6126AF, 6126AG, 6126AH

#6341AA is 702; and AU is 1388. 702 and 1388 is 2090
#6341AA: this was the mistaken (probably) pair assignment. 
#There's a small group together, centre of glasgow - 
#in theory, the sums should add up between the two zero zones here...?
#The zones around them awaiting assignment sum to... 2090. Bingo.
#So how do they break down?
#6341AA - Leaving 280+101+215+106 = 702. AB AH AJ AL, 6341
#6341AU - 396+90+84+818 = 1388. Zones: AM AN AD AE, 6341

#5602AH 5602AB|5602AJ
#5707AG 5707AC|5707AD
#5707AX 5707AT|5707AU|5707AW
#6121AG 6121AC|6121AD|6121AE|6121AF
#6123AT 6123AN|6123AP|6123AQ|6123AR
#6126AM 6126AK|6126AL
#6126AC 6126AP|6126AQ 
#6229AK 6229AD|6229AE|6229AF|6229AG
#6332BM 6332BD|6332BE|6332BF|6332BG|6332BH|6332BJ|6332BK|6332BL
#6347AT 6347AC
#6347AU 6347AG 
#6126AJ 6126AD|6126AE|6126AF|6126AG|6126AH
#6341AA 6341AB|6341AH|6341AJ|6341AL
#6341AU 6341AM|6341AN|6341AD|6341AE

#Final additions
#6332AT 6332AA|6332AK|6332AL|6332AM
#6332CF 6332CA|6332CE|6332BZ
#6332CD 6332BN|6332BR|6332BS|6332BT|6332BQ|6332BX

zones = c(
'5602AH', 
'5707AG', 
'5707AX', 
'6121AG', 
'6123AT', 
'6126AM', 
'6126AC', 
'6229AK', 
'6332BM', 
'6347AT', 
'6347AU', 
'6126AJ', 
'6341AA', 
'6341AU',
'6332AT',
'6332CF',
'6332CD'
)

reassigns = c(
'5602AB|5602AJ',
'5707AC|5707AD',
'5707AT|5707AU|5707AW',
'6121AC|6121AD|6121AE|6121AF',
'6123AN|6123AP|6123AQ|6123AR',
'6126AK|6126AL',
'6126AP|6126AQ',
'6229AD|6229AE|6229AF|6229AG',
'6332BD|6332BE|6332BF|6332BG|6332BH|6332BJ|6332BK|6332BL',
'6347AC',
'6347AG',
'6126AD|6126AE|6126AF|6126AG|6126AH',
'6341AB|6341AH|6341AJ|6341AL',
'6341AM|6341AN|6341AD|6341AE',
'6332AA|6332AK|6332AL|6332AM',
'6332CA|6332CE|6332BZ',
'6332BN|6332BR|6332BS|6332BT|6332BQ|6332BX'
)

#Make a merge df for what we have so far (bar separated to keep in with below)
mrg_df <- data.frame(zones = zones,assigns = reassigns)


#6332AT - the awkward one deleted above. Let's see...
#1014 target. What surrounds it? We have a different zone code neighbouring...
#Let's leave until last and see what's left.

#6332CD gets...
#6332CF gets...
#These two are neighbouring Islands with a bunch of possible zones being binned into them.
#Let's see: others to leave to final check.

#Let's apply the non-awkward ones then look at what's left on the map for those last awkwards.
#All reassigns in one column, bar separated.

#Pairs first to get going
pop91manual2 <- unite(pop91manual,"final_reassignList",c(Zone.ID1,Zone.ID2), sep='|')

#Then individual assigns
pop91manual2$final_reassignList[!is.na(pop91manual2$Zone.ID.y)] <- 
  pop91manual2$Zone.ID.y[!is.na(pop91manual2$Zone.ID.y)]

#merge in what we have from above before awkwards
pop91manual2 <- left_join(pop91manual2, mrg_df, by = c('Zone.ID.x' = 'zones'))

#Stick those into shared column
pop91manual2$final_reassignList[!is.na(pop91manual2$assigns)] <- 
  as.character(pop91manual2$assigns[!is.na(pop91manual2$assigns)])

#Get rid of NANAs!
pop91manual2$final_reassignList[pop91manual2$final_reassignList=='NA|NA'] <- NA

#I should probably save this before anything crashes...
write.csv(pop91manual2, "temp/91PCS_reassignStage2.csv")

#Now what we did this for was... getting a full list of which zero zones we now know have a reassign
#Which is the list of zones in the final reassign list... how to get all those together?
zonz <- pop91manual2$final_reassignList[!is.na(pop91manual2$final_reassignList)]

zonzall <- unlist(sapply(zonz, function(x) strsplit(x,"\\|")))
zonzall_df <- data.frame(zonzall)

#These are all found...
zonzall_df$found = 1

#Yup, that was it. Mark on shapefile and look at...
saveToLook <- pseudo2

saveToLook <- merge(saveToLook, zonzall_df, by.x = "label", by.y="zonzall", all.x = T)

chk <- data.frame(saveToLook)

writeOGR(saveToLook, "C:/Data/temp/QGIS/census",
         "zeroZones_moreMatches", driver="ESRI Shapefile", overwrite_layer = T)

#Also need to save latest reassigns
saveToLook <- pseudo2

saveToLook <- merge(saveToLook, pop91manual2, by.x = "label", by.y="Zone.ID.x", all.x = T)

chk <- data.frame(saveToLook)

writeOGR(saveToLook, "C:/Data/temp/QGIS/census",
         "matchZonesLatestOhThatsAgreatname", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~~~~~~~
#FINAL REASSIGNS----

#How many should be left? Three... yes, correct.
#6332AT - the awkward one deleted above. Let's see...
#6332CD
#6332CF
#These two are neighbouring Islands with a bunch of possible zones being binned into them.

#6332AT: 1014 missing, four obvious zones...
#6332AA AK AL AM: yup, correct. So:
#6332AT 6332AA|AK|AL|AM

#6332CF: 1363... is 6332CA|6332CE|6332BZ
#Leaving these for 6332CD (899):
#6332CD BN BR BS BT BQ BX. Is that the right sum? ... Yup!
#6332CD 6332BN|6332BR|6332BS|6332BT|6332BQ|6332BX

#So final list:
#6332AT 6332AA|6332AK|6332AL|6332AM
#6332CF 6332CA|6332CE|6332BZ
#6332CD 6332BN|6332BR|6332BS|6332BT|6332BQ|6332BX

#Re-run all that again? 
#Actually, just going to throw them into the code above and repeat.

#Done: checked, all good. 
#Which means:
#"temp/91PCS_reassignStage2.csv"
#And "C:/Data/temp/QGIS/census/matchZonesLatestOhThatsAgreatname.shp"
#Are the final files.

#~~~~~~~~~~~~~~~~~~~~~
#Apply processed PCSs to shapefile and data----

#Shapefile first: dissolve into the groupings
#unionSpatialPolygons - set ID for each of the new groups
#Use the zone the counts are being added to as ID
pcs91reshaper <- pop91manual2

#Most will be the same single ID. We'll overwrite those...
pcs91reshaper$aggID <- pcs91reshaper$Zone.ID.x

subz <- pcs91reshaper[!is.na(pcs91reshaper$final_reassignList),]

#get a list that includes the single target and the zones going into that target
#Incredibly, that worked.
ids <- lapply(c(1:nrow(subz)), 
              function(x) c(subz$Zone.ID.x[x],
                            unlist(lapply(subz$final_reassignList[x], function(y) strsplit(y,"\\|"))))
              )

#Probably a quicker way but what the hell...
for(i in 1:length(ids)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  pcs91reshaper$aggID[pcs91reshaper$Zone.ID.x %in% ids[[i]] ] <- ids[[i]][1]
  
}

#Attach that to the data
diss <- merge(pseudo2, pcs91reshaper[,c("Zone.ID.x","aggID")], by.x = "label", by.y = "Zone.ID.x", all.x = T)

#Now can dissolve on that ID.
diss2 <- unionSpatialPolygons(diss, diss@data$aggID)

#Add some data back in
diss_sp <- SpatialPolygonsDataFrame(diss2,data = data.frame(label = row.names(diss2), row.names = row.names(diss2)))

diss_df <- data.frame(diss_sp)

#merge in source IDs so we know which were merged
diss_sp2 <- merge(diss_sp, pcs91reshaper[,c("Zone.ID.x","final_reassignList")], 
                 by.x='label', by.y='Zone.ID.x', all.x = T)

diss_df <- data.frame(diss_sp2)

#And that should be everything...
writeOGR(diss_sp2, "C:/Data/temp/QGIS/census",
         "checkOnFinalPCSagg", driver="ESRI Shapefile", overwrite_layer = T)

#Yup - save official copy
writeOGR(diss_sp2, "C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount",
         "pseudoPCS_aggregated4CorrectCount", 
         driver="ESRI Shapefile", overwrite_layer = T)





















