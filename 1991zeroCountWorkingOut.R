#WORKING OUT 1991 WARD REASSIGNS from EDs.
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

source('Function_Shapefile_uniqueIDs_to_singleRowFeatures.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Random thing: ward vs postcode sector size----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Use original postcode sector size for comparison
pcs <- readOGR(dsn='C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount',
               layer='pseudoPCS_aggregated4CorrectCount')

#This was made below
wards <- readOGR(dsn='C:/Data/MapPolygons/EnglandWalesMerged/1991/wards',layer='englandWalesMerge_wards_1991_dissolvedBYID.shp')

pcsAreas <- gArea(pcs,byid = T)/1000000
wardAreas <- gArea(wards,byid = T)/1000000

#Ah: very large rural areas!
mean(pcsAreas)
sd(pcsAreas)

mean(wardAreas)
sd(wardAreas)

#wards vs pcs: the geog being used for 91 LBS data
#(Well, when aggregated correctly, but this is good enough to compare)
pcsarea = data.frame(area = gArea(pcs, byid=T),type = 'pcs')
wardsarea = data.frame(area = gArea(wards, byid=T),type = 'wards')

areaz <- rbind(pcsarea,wardsarea)

ggplot(areaz,aes(y = area, x = type)) +
  geom_boxplot() +
  scale_y_log10()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Link ED popbase data to ED 91 shapefile----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Load 91 ED popbase, Eng/Wales
ed91 <- read_csv('C:/Data/Census/1991/EnglandWales/engwales_SAS_ED_totalPop_S010064/engwales_SAS_ED_totalPop_S010064.csv')

#There's yer zeroes
table(ed91$s010064==0)

#Seeing where they are on a map. 510mb
edGeog <- readOGR(dsn = 'C:/Data/MapPolygons/EnglandWalesMerged/1991/EDs',layer='engWalesMerge_ed_1991')

#match level. Will do for looking on map
#FALSE   TRUE 
#4 109802
table(edGeog@data$label %in% ed91$`Zone ID`)

#Attach what data we have to shapefile, save to just have a look at.
#Add flag for no value first
ed91$hasNoCount <- 0 + (ed91$s010064 == 0)

#testing
edGeog2 <- edGeog

#Ah. Now that's interesting: this reorders the data 
#but doesn't reorder the underlying #zones
#Oh, see here.
#http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html
#edGeog2@data <- merge(edGeog2@data,ed91,by.x='label', by.y='Zone ID', all.x = T)
edGeog2 <- merge(edGeog2,ed91,by.x='label', by.y='Zone ID', all.x = T)

#Remove pointless column
edGeog2@data <- edGeog2@data[,c(1,3,4)]

#save! Err. Where?
writeOGR(edGeog2,dsn='QGIS/polygon_data_combos',
         layer='91_EngWales_ED_popBases010064', driver='ESRI Shapefile',
         overwrite_layer = T)

#So the theory here:
#Aggregated up to ward level, we'll be able to spot ward differences
#in the LBS tables.
#Problem here: SAS counts also hidden < 50 people / 16 households.
#But they might, mostly, aggregate to wards correctly
#As there's an effort to keep them in sensible areas.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create ED / ward lookup from intersect
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load QGIS'd intersect file 
#intersect <- readOGR(dsn='Intersects/otherIntersects',layer='engWales91EDS_to_Wards')

#We'll have some slivers
#But for each ED pick the one with the largest area
#(They all tesselate)
#head(intersect@data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create ED / ward lookup----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Oh silly me: the ward code is nested in the ED code. Which I knew, right?
#So aggregating values is easy:
#Just the first six characters of the ED zone are the ward
ed91$wardCode <- substr(ed91$`Zone ID`,1,6)

edPopToWard <- ed91 %>% group_by(wardCode) %>% 
  summarise(wardPopCount = sum(s010064))

#Load wards to compare
#Update: not ward SAS - that will obv be the same. Need to compare to LBS
wards91LBS <- read_csv('1991/EnglandWales/engwales_LBS_ward_totalPop_l010064/engwales_totalPop_l010064.csv')

#Perfect
wards91LBS$`Zone ID` %>% unique %>% length
ed91$wardCode %>% unique %>% length
edPopToWard$wardCode %>% unique %>% length

both <- merge(wards91LBS,edPopToWard,by.x='Zone ID',by.y='wardCode')

both$ratio <- both$l010064/both$wardPopCount

#both$ratio[both$ratio>1 & ] %>% mean
chk <- both$ratio[both$ratio>0 & !is.na(both$ratio)]

chk <- chk[order(chk)]
chk <- chk[1:9362]

plot(chk)

#To look in QGIS:
#Mark zones where LBS is zero but ED-summed is positive
#These are the ones where a count's been re-assigned
both$LBSreassign <- 0 + (both$l010064==0 & both$wardPopCount > 0)
both$diff <- both$l010064 - both$wardPopCount

#save and link in QGIS
write_csv(both,'QGIS/1991EngWales_LBS_EDsumsCompare.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#why different ward count in shapefile and data?----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ED and ward table data both have the same ward count
#(EDs first six digits are ward codes)
#Ward count in the England/Wales shapefiles is higher.
#Wassup? Accounting for shipping (which is probably one zone in the shapefile)
wardShp <- readOGR(dsn='C:/Data/MapPolygons/EnglandWalesMerged/1991/wards',layer='englandWalesMerge_wards_1991')

#It may need dissolving to IDs too, but let's see.
#9811
wardShp@data$label %>% length
#9527
wardShp@data$label %>% unique %>% length

#To compare
#9930
wards91LBS$`Zone ID` %>% unique %>% length
#9930
ed91$wardCode %>% unique %>% length

#But the names are not unique - many shipping I think
#9930
wards91LBS$`Zone name` %>% unique %>% length

#Ah, that won't tell us: need to count shipping directly
#How many "shipping"?
#FALSE  TRUE 
#9527   403 
table(grepl("shipping",wards91LBS$`Zone name`,ignore.case = T))

#So are there no shipping zones in the shapefile data? Newp! Odd, I'd expect one.
table(grepl("shipping",wardShp@data$name,ignore.case = T))

#SO! Minus shipping, ward CODE count should match
wards91LBSMinusShipping <- wards91LBS[!(grepl("shipping",wards91LBS$`Zone name`,ignore.case = T)),]

#TRUE 
#9527
table(wards91LBSMinusShipping$`Zone ID` %in% wardShp$label)

#Ah, so the extra length in the shapefile is the IDs that need dissolving.
wardsShpDissolved <- IDtoSingleRow(wardShp,2)

#Merge ward names back in - might need em later
wardsShpDissolved@data <- merge(wardsShpDissolved@data,wards91LBS[,c(1,2)],
                                by.x = 'label', 
                                by.y = 'Zone ID',
                                all.x = T)

writeOGR(wardsShpDissolved,dsn='C:/Data/MapPolygons/EnglandWalesMerged/1991/wards',layer='englandWalesMerge_wards_1991_dissolvedBYID.shp', driver='ESRI Shapefile',overwrite_layer = T)

#So now?
table(wards91LBSMinusShipping$`Zone ID` %in% wardsShpDissolved$label)
table(wardsShpDissolved$label %in% wards91LBSMinusShipping$`Zone ID`)

#OK, all good. Correct number is 9527 once shipping removed.
#Back to working out the re-assignments.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Neighbours for first round of checks----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wardsShpDissolved <- readOGR(dsn='C:/Data/MapPolygons/EnglandWalesMerged/1991/wards',layer='englandWalesMerge_wards_1991_dissolvedBYID.shp')

#Merge in the 'both' df before getting contig, so index order stays correct
#wardsShpDissolved@data <- merge(wardsShpDissolved@data, both[,c(1,3:7)], by.x = 'label', by.y = 'Zone ID', all.x = T)
wardsShpDissolved <- merge(wardsShpDissolved, both[,c(1,3:7)], by.x = 'label', by.y = 'Zone ID', all.x = T)

names(wardsShpDissolved@data) <- c(
  'label',
  'name',
  'LBS_count',
  'ED_agg_count',
  'ratio',
  'LBSreassign',
  'diff'
)

chk <- data.frame(wardsShpDissolved)

contig <- poly2nb(wardsShpDissolved, row.names = wardsShpDissolved@data$label,queen = T)

#Indices of neighbours. Let's check it worked, quick look in QGIS.
head(wardsShpDissolved)

#Pick first, 01AAFA
wardsShpDissolved@data$label[unlist(contig[1])]

#Yup, all good. Now: just use the neighbour list for zones that have been set to zero
#So that's excluding both shipping and zones that actually have zero pop.
wardsShpDissolved@data[c(100:150),]

#Add index so that when we subset we can use that to fetch correct neighbour index
wards <- data.frame(wardsShpDissolved)

wards$index <- c(1:nrow(wards))

#Can now just keep the LBS-reassign zones
LBSreassigns <- wards[wards$LBSreassign==1,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing one-to-one matches----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#OK, so working through an example:
#Get first ward and its neighbours, look at.
#test <- wards[wards$label %in% wards$label[unlist(contig[LBSreassigns$index[1]])], ]

testNum <- 70

#hang on, those are indices. That was a ridiculous way to do that!
#Though I can't think of a better way...
#Maybe just match on label? Nah: contig gives us the original index
test <- wards[wards$index %in% unlist(contig[LBSreassigns$index[testNum]]), ]

#Mark those as not the target LBS
test$zeroSource = 0

target <- LBSreassigns[testNum,]
target$zeroSource = 1

#Add in the target zero-count zone
test <- rbind(test,target)

#Right: picked an example that *did* pick up a correct reassign value
#The first index did not.
#So how many can we get that way?

#I need to remind myself what we have
#LBSreassigns are the zero-count LBSs that have had their value reassigned to a neighbour
#LBSreassigns has $index - same order as wards, and as contig.
#contig contains neighbours for each row. Which matches index in LBSreassigns.

#So for each LBSreassign I want to get its contig (via index)
#Then check which one (if any) contains a match on the ED difference

#Been trying to do this in one line but not only a bugger to code
#But will make no sense later. Maybe do stage by stage so I stand some chance of understanding.
#Perhaps tighten later.

#New column for storing matching labels
LBSreassigns$zonesToCombine <- NA

#Cycle over all the zero-set zones by index
for(x in 1:nrow(LBSreassigns)){
  
  candidateNeighbours <- wards[wards$index %in% unlist(contig[LBSreassigns$index[x]]), ]
  
  candidateMatch <- candidateNeighbours$label[which(candidateNeighbours$diff == LBSreassigns$ED_agg_count[x])]
  
  #If there's a match
  if(candidateMatch %>% length > 0){
    LBSreassigns$zonesToCombine[x] <- as.character(candidateMatch)
  }
  
}

#So remaining non-matches (e.g. exact minus matches in Farringdon) must be 
#two or more assigned to another zone

#Save a new version of CSV for linking in QGIS with 'zero count has match / doesn't have match'
wardsToSave <- merge(wards,LBSreassigns[,c(1,9)], by = 'label', all.x = T)

#Not a zero zone
wardsToSave$zeroZones <- 'vanilla'
wardsToSave$zeroZones[wardsToSave$LBSreassign==1] <- 'zero zone (no match yet)'
wardsToSave$zeroZones[wardsToSave$LBSreassign==1 & !is.na(wardsToSave$zonesToCombine)] <- 'has single match zero zone'

table(wardsToSave$zeroZones)

write_csv(wardsToSave,'QGIS/1991EngWales_LBS_EDsumsCompare_firstPairMatches.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Look for two zero counts going into one zone----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Don't update over same column yet
LBSreassigns$zonesCombinePairs <- NA

#If zero-count zones share a neighbour, they're candidates.
#First, get our remaing LBSreassign candidates. Only 35 left!
possTwoLBSZeroes <- LBSreassigns[is.na(LBSreassigns$zonesToCombine),]

#Might it be a better idea just to assemble "neighbours plus neighbours of neighbours"?
#That has to include "shares a neighbour" of course.
#I think it might be.

#Tests: Farringdon Within / Without have something odd going on.
#Whats the sum of their neighbour differences?
chkSum <- wards$diff[wards$index %in% unlist(contig[wards$index[wards$name=='Farringdon Without']])]

chkSum
#Sum of all but Farringdon within
31+110+191
#two across the river
31+110+191+242+163
#Sum of farringdon's missings.
539+138

#Conclusion: it's hard to say if those are the ones that have been combined.
#Might be best to leave them out for now.

#Test: getting one set of neighbour neighbours
get <- 30

#Now not testing / doing for all
#for(get in 1:nrow(possTwoLBSZeroes)){
#leaves out the Farringdons
for(get in 1:nrow(possTwoLBSZeroes)){

  #Direct neighbours for the first zone (index matches ward)
  contig[possTwoLBSZeroes$index[get]]
  
  #Well that was straightforward
  #But maybe confusing later. So:
  #Inner contig index from our current zero zones gives us the indices for the neighbours
  #Then just put those back into contig to get next neighbours
  allNeighbours <- unlist(contig[ unlist(contig[possTwoLBSZeroes$index[get]]) ])
  
  #Repeating values (of course) so:
  allNeighbours <- unique(allNeighbours)
  
  # jpeg(paste0('R_outputs/91pcsEnglandChecks/',get,'.jpg'))
  
  #Should still be in the right order to look at, right? Yup!
  # plot(wardsShpDissolved[allNeighbours,])
  
  #For lookin'
  allNeigbfull <- wards[allNeighbours,]
  
  #Now, which of those are *also* zero-set that we haven't already found a pairing for?
  #Presumably this will sometimes return nothing or just one
  zeroSetNearNeighbours <- possTwoLBSZeroes[which(possTwoLBSZeroes$index %in% allNeighbours),]
  
  if(zeroSetNearNeighbours %>% nrow < 2){
    print(paste0(get,': not enough zero-set neighbours for a pair'))
    # dev.off()
    #print(zeroSetNearNeighbours)
    next
  }
  
  #Ah, not many: in the case of index 30, looks like a two and a three
  #Wonder if I can deal with both at the same time?
  #Oh no: there's one by itself I think.
  #Oh nono: I think it's pairs.
  
  #So we need to know for every pair of zero-set if they share a neighbour
  #First, get all pairs
  #combn from combinat library
  pairz <- combn(zeroSetNearNeighbours$index,2)
  
  #Hmm: pairz returns a single vector if there's only one pair.
  #So needs transposing. Might work.
  if(length(pairz)==2){
    pairz <- as.matrix(pairz)
  }
  
  #First pair
  pairz[,1]
  
  #loop through all pairs
  for(x in 1:ncol(pairz)){
  # for(x in 1:1){
  
    #Shared neighbours? Three.
    unlist(contig[pairz[1,x]]) %in% unlist(contig[pairz[2,x]])
    #Good lord, this is ugly. Note for later: this nearly snapped my brain.
    #it's just passing in the boolean vector once it's unlisted, but...
    candidateReassigns <- unlist(contig[pairz[1,x]])[unlist(contig[pairz[1,x]]) %in% unlist(contig[pairz[2,x]])]
    #Get the actual wards so we can get their pop count
    candidateReassigns <- wards[wards$index %in% candidateReassigns,]
    
    
    #So now we have x possible zones where those pairs got their values reassigned.
    #How to check?
    #Presuming it's exact, sum the pair's ward/ED difference
    #Then check for that value in the candidate neighbour list
    lookForThisSum <- abs(LBSreassigns$diff[LBSreassigns$index==pairz[1,x]])+
      abs(LBSreassigns$diff[LBSreassigns$index==pairz[2,x]])
    
    #if for now we don't find an exact match, move on
    if(length(candidateReassigns$index[candidateReassigns$diff==lookForThisSum])==0) {
      print(paste0(get,': no exact match for pair found'))
      # dev.off()
      next
    }
    
    #If we have an exact match...
    
    candidateReassigns$index[candidateReassigns$diff==lookForThisSum]
    
    print(paste0(get,':',candidateReassigns$index[candidateReassigns$diff==lookForThisSum],
                 '                         ******GOT A PAIR MATCH!******'))
  
    #Add label found
    #Add it to both in the found pair
    #This duplicates/overwrites but is a reasonable way to do it anyhoo
    LBSreassigns$zonesCombinePairs[LBSreassigns$index %in% pairz[,x]] <- 
      as.character(candidateReassigns$label[candidateReassigns$diff==lookForThisSum])
    
    
    # jpeg(paste0('R_outputs/91pcsEnglandChecks/',get,'.jpg'))
    # plot(wardsShpDissolved[allNeighbours,])
    # plot(wardsShpDissolved[zeroSetNearNeighbours$index,], add = T, col = 'green')
    # plot(wardsShpDissolved[candidateReassigns$index,], add = T, col = 'blue')
    # plot(wardsShpDissolved[candidateReassigns$index[candidateReassigns$diff==lookForThisSum],], add = T, col = 'red')
    # dev.off()
  
  }
  
  # dev.off()
  
  #So recapping again. Having got the remaining zero-set zones with no match yet
  #Look for those that share neighbours. I presume this won't happen with all remaining combos.
  #For those pairs that do share neighbours
  #check to see if their missing values are in that neighbour, summed.
  #(It's still very confusing to read though - hopefully map plots will help.)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Looking for almost-matches-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LBSreassignsBackup <- LBSreassigns
write_csv(LBSreassigns,'R_data/LBSreassigns_singlesPairsExact.csv')

#Well, that found three pairs. Woop! Which means we've got what left?
#table(0 + is.na(LBSreassigns$zonesToCombine))

#29 left
table(0 + (is.na(LBSreassigns$zonesToCombine) & is.na(LBSreassigns$zonesCombinePairs)) )

#For these, it's going to be worth looking at what's going on around them.
#Central London is odd, others might be odd - let's not use anything too crude.
#But...
remainz <- LBSreassigns[(is.na(LBSreassigns$zonesToCombine) & is.na(LBSreassigns$zonesCombinePairs)),]

#So let's just look at some of the remainz neighbours: any with big diffs / near matches?

#ISLANDS~~~~~~~~~~~~~~~~~~~~~~
#Ah: St. Agnes and some others are Islands; will deal with those shortly
plot(wardsShpDissolved[remainz$index[10],])

remainz$island <- 0 + sapply(remainz$index, function(x) contig[[x]][1] == 0) 

table(remainz$island)

#are they all the Isles of Scilly?
plot(wardsShpDissolved[remainz$index[remainz$island == 1],])

#Yes, but one's missing (St.Mary's). That's 441.
113+167+83+78
#... St.Mary's got 441 extra, match! 
#Yeah, there are no other re-assigns nearby.
#So that's three more down: they're all into St.Mary's: 16FDFD
plot(wardsShpDissolved[wards$label=='16FDFD',], add = T, col = 'green')

#So update to the reassigns:
LBSreassigns$zonesToCombine[LBSreassigns$index %in% remainz$index[remainz$island==1]] <- '16FDFD'

#~~~~~~~~~~~~~~~~~~~~~~~~~
#London
#Walbrook: has 1333
#I think neighbours may have this.
138+539+127+286+79+114
#Yup, checking map: all six into Walbrook
LBSreassigns$zonesToCombine[LBSreassigns$label %in% LBSreassigns$label[1:6]] <- '01AAGB'

#This makes for a very odd-shaped zone: might be better to combine with others

#Look at london some more: I'm sure a bunch of the double-zero-zones around this area
#(zero SAS and LBS)
#Have also been stuck into Walbrook.
#Is there a way to check which? Possibly...
#OK, yes, it looks fairly obvious which should be combined. Check doc notes section "Pairs and more"

#I *think* it's just all zones beginning 01AA...?
#I have these selected in QGIS. Correct number? Yes.
wards[grepl('01AA',wards$label),] %>% nrow

#So update... all combined with Walbrook
#But don't assign Walbrook to itself
londonCombo <- wards$name[grepl('01AA',wards$label)]
londonComboLabel <- wards$label[grepl('01AA',wards$label)]
#walbrook is 25

#And! In fact, Cripplegate and Aldersgate are fine.
londonCombo <- londonCombo[c(2:14,16:24)]
londonComboLabel <- londonComboLabel[c(2:14,16:24)]

#Ah wait: we don't yet have all those in LBSreassigns do we?
#Label cos "bridge" and "tower" are rather popular ward names elsewhere!
#Don't overwrite those already in LBSreassigns.
londonWardsToUse <- wards[(wards$label %in% londonComboLabel)
                          &!(wards$label %in% LBSreassigns$label),]

#Set those to the correct reassign (Walbrook) then add
londonWardsToUse$zonesToCombine <- '01AAGB'
londonWardsToUse$zonesCombinePairs <- NA

#Just in case
LBSreassigns2 <- rbind(londonWardsToUse,LBSreassigns)

#LBSreassigns$zonesToCombine[LBSreassigns$label %in% londonCombo] <- '01AAGB'


#Save again!
write_csv(LBSreassigns2,'R_data/LBSreassigns_singlesPairsExact_London_Scilly.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~

LBSreassigns <- read_csv('R_data/LBSreassigns_singlesPairsExact_London_Scilly.csv')

#What's left?
#OK, update with a new remainz: 19 left.
remainz <- LBSreassigns[(is.na(LBSreassigns$zonesToCombine) & is.na(LBSreassigns$zonesCombinePairs)),]

get <- 19

contig[remainz$index[get]]
neigbs <- unlist(contig[remainz$index[get]])

#This is 2nd order neighbours, might need soon...
# allNeighbours <- unlist(contig[ unlist(contig[possTwoLBSZeroes$index[get]]) ])
# allNeighbours <- unique(allNeighbours)

plot(wardsShpDissolved[neigbs,])

#for(i in 1:nrow(remainz)){
#Ignoring London
for(i in 1:nrow(remainz)){
  
  print(paste0(remainz$name[i],':',remainz$label[i]))
  print(remainz$diff[i])
  print(wards$diff[wards$index %in% unlist(contig[remainz$index[i]])])
  
}

#OK, a bunch of candidates with very close values. Let's work through them looking in QGIS.
#Aldermaston: only candidate
#Dorney: only candidate
#Long Marton: larger of the two
#Tolpuddle: only candidate
#Yarmouth: ""
#Aighton etc: ""
#Brafield: ""
#Bolton Manor: highest
#Scorton: highest (just, 750. Other is taken.)
#Ruyton: highest (easily)
#Ashford: only value (took a little tracking down but correct)
#Woodford Valley: highest (other one is tiny; no other candidates near)
#Llandegla: highest (other is neg value)
#Betws-y-Coed: highest
#Deiniol: highest
#Aberdovey: only
#Llywel: highest. Other is taken

#Myddfai: has no matches in the neighbour list. Is this a 3:1?
#No, it's next to one of the pairs. Must be another pair but a not-quite-match?
#640+710=1350
#Leave this until rest done...

#Bronllys: again, odd. No obvious candidate. Leave until end.

#LBSreassigns[LBSreassigns$zonesToCombine=='21GXFP',]

#So let's get those easy ones done first then deal with the last two awkward buggers.
easiz <- remainz[!(remainz$label %in% c('53TKFB','49SNFT')),]

#Can now get neighbour with highest diff as one to merge with
#No need for this, they're all empty currently
#easiz$zonesNearMatch <- NA

for(i in 1:nrow(easiz)){

  #unlist(contig[easiz$index[i]]) %>% print
  
  # print(easiz$diff[i])
  # print(wards$diff[wards$index %in% unlist(contig[easiz$index[i]])])
  # 
  # #Can just pick max from each. Check.
  # print(max(wards$diff[wards$index %in% unlist(contig[easiz$index[i]])]))
  # 
  #How to get index from contig based on max?
  diffs <- wards$diff[wards$index %in% unlist(contig[easiz$index[i]])]
  
  #Vectors will be in the correct order.  
  #pairIndex <- unlist(contig[easiz$index[i]])[diffs == max(diffs)]
  #Get label at same time
  pairLabel <- wards$label[wards$index == 
                             unlist(contig[easiz$index[i]])[diffs == max(diffs)]] %>% as.character()
  
  #Apply that to the zero zone
  easiz$zonesToCombine[i] <- pairLabel
  LBSreassigns$zonesToCombine[LBSreassigns$index == easiz$index[i]] <- pairLabel
  
}

#Check a few of those in QGIS, make sure it's correct... Yup, looks good.
#Two left to check, correct.
table(0 + (!is.na(LBSreassigns$zonesToCombine))|!is.na(LBSreassigns$zonesCombinePairs))

#Yup, these two.
LBSreassigns$name[(is.na(LBSreassigns$zonesToCombine))&is.na(LBSreassigns$zonesCombinePairs)]

#Save!
write_csv(LBSreassigns,'R_data/LBSreassigns_singlesPairsExact_London_Scilly_nearMatches.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FINAL TWO---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So we should be able to map these out here to see what's going on, I hope.
#Though maybe let's look in QGIS too.
#Yeah: I need to see - 
#Which zones have been marked off as 'taken' (i.e they'll be combined with others)
#So:
takenz <- c(LBSreassigns$zonesToCombine,LBSreassigns$zonesCombinePairs) %>% unique

#Add marker to wards and save for join
wardsSave <- wards
wardsSave$zoneIsTakenForCombining <- 0 + wardsSave$label %in% takenz

table(wardsSave$zoneIsTakenForCombining)

write_csv(wardsSave,'R_data/wardsMarkedTaken.csv')

#OK, found Myddai's home. That's odd - it's not contiguous and I can see no reason why.
#There are two more zones in the way.
#Only solution (since we need to make contigous zones)
#Join all four together. It'll be big... 
#So Myddfai 49SNFT
#Joins: Llanfihangel Aberbythych 49SNFP - this is where its 640 pop went.
#Zones between: 49SNFQ and 49SNFF

#Same for Bronllys, except three zones not four.
#Bronllys: 53TKFB
#Its count went to The Vale of Grwyney 53TKGC
#Need to also include this one between both: Gwernyfed 53TKFJ

#So in both cases, the target-zone-to-combine-to is the one with the pop.
#It should work out, I think, but keep an eye on it.

#Ah: two of these currently not on the zones-to-combine list
#In fact, three. Need to add those
addz <- wards[wards$label %in% c('53TKFJ','49SNFQ','49SNFF'),]

#so we can rbind
addz$zonesToCombine <- NA
addz$zonesCombinePairs <- NA

LBSreassigns2 <- rbind(LBSreassigns,addz)

#In with Llanfihangel Aberbythych
LBSreassigns2$zonesToCombine[LBSreassigns2$label %in% c('49SNFT','49SNFQ','49SNFF')] <- '49SNFP'

#In with Grwyney
LBSreassigns2$zonesToCombine[LBSreassigns2$label %in% c('53TKFB','53TKFJ')] <- '53TKGC'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#FINAL JOBS-----

#Combine the two columns
LBSreassigns2$finalZoneCombo <- ifelse(is.na(LBSreassigns2$zonesToCombine),
                                       LBSreassigns2$zonesCombinePairs,
                                       LBSreassigns2$zonesToCombine)

#OK, done! Save with only columns we need.
write_csv(LBSreassigns2[,c(1,2,11)],'R_data/LBSreassignsEngWalesFinal.csv')

#Now. Where to do the actual combining?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAKE AGGREGATED WARD SHAPEFILE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

LBSreassigns <- read_csv('R_data/LBSreassignsEngWalesFinal.csv')

#Code adapted from Scots PCS version which is last section here (for some reason):
#Census_DX/tests/1991_pseudoPostcodeSectorChecking.R

#We have our current wardsShpDissolved
names(wardsShpDissolved)
#Single shape per zone? Yup.
nrow(wardsShpDissolved) == unique(wardsShpDissolved$label) %>% length

#make a copy, keep only key columns
wardShp4Agg <- wardsShpDissolved[,c(1:2)]

#Hang on, I've set this up differently to the last time.
#Before I had all the zones that a target was going to get in its own column
#Bar-separated. Err, now?

#Actually, this might be easier - though I will then need to record all the zones that got merged.
#Sooooo.

#For the merge itself, I'm just doing it on some ID column.
#Most will be what they were. But if I just merge in, that should leave the remainder...
wardShp4Agg <- merge(wardShp4Agg,LBSreassigns[,c(1,3)],by = 'label',all.x = T)

#worked? Seems to. (Will check in map when done.)
chk <- data.frame(wardShp4Agg)

#Now just replace NAs that don't have reassigns from above with the existing zone name
#(So they'll just union with themselves)
wardShp4Agg@data$finalZoneCombo[is.na(wardShp4Agg@data$finalZoneCombo)] <- 
  as.character(wardShp4Agg@data$label[is.na(wardShp4Agg@data$finalZoneCombo)])

#check numbers. Tick!
table(0 + (wardShp4Agg$label!=wardShp4Agg$finalZoneCombo))

#Err. There's an issue here ... do I need to put those into row names?
unionz <- unionSpatialPolygons(wardShp4Agg, wardShp4Agg@data$finalZoneCombo)

#Well let's see if that worked...
row.names(unionz)

#Re-SP-dataframe that mofo with the IDs as a column
unionz_sp <- SpatialPolygonsDataFrame(unionz,
                                      data = data.frame(label = row.names(unionz), 
                                                        row.names = row.names(unionz)))

#Check in QGIS
writeSpatialShape(unionz_sp,'QGIS/testEngWalesWardCombine.shp')

#Last job: attach column with list of zone labels that have been combined
#Just use the final column name from the scots aggregate
#(reduced length cos saved with writeOGR)
#But will match previous code...

#Working with LBSreassigns so each label is unique to each zone being combined with
#Let's see if this works... tick!
combos <- LBSreassigns %>% group_by(finalZoneCombo) %>% 
  summarise(fnl_rsL = paste(label, collapse = '|'))

#merge into shapefile
unionz_sp <- merge(unionz_sp,combos,by.x='label',by.y='finalZoneCombo',all.x = T)

chk <- data.frame(unionz_sp)

#Looks good. Save again to double-check. Tick.
writeSpatialShape(unionz_sp,'QGIS/testEngWalesWardCombine.shp')

#save official copy
writeOGR(unionz_sp, "C:/Data/MapPolygons/EnglandWalesMerged/1991/wardsAggForCorrectCount",
         "wardsEngWales_aggregated4CorrectCount",
         driver="ESRI Shapefile", overwrite_layer = T)















