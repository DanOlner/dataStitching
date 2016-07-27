geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat",
             "data.table")
lapply(geolibs, require, character.only = TRUE)
#For 1991 postcode sectors, updated to new zones to get rid of zero counts
#Apply the same zone aggregation to the 1991 LBS tables we're using.

#Just apply the aggregation here, worry about the other details elsewhere.
#For which we need some indication of what those zones are. They're in the shapefile, right?
pcs91shp <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
                              layer="pseudoPCS_aggregated4CorrectCount")

#Yup
shp_df <- data.frame(pcs91shp)
shp_df$label <- as.character(shp_df$label)
shp_df$fnl_rsL <- as.character(shp_df$fnl_rsL)

#work only on those with more than one zone to aggregate
shp_df <- shp_df[!is.na(shp_df$fnl_rsL),]

zonez <- lapply(c(1:nrow(shp_df)), 
              function(x) c(shp_df$label[x],
                            unlist(lapply(shp_df$fnl_rsL[x], function(y) strsplit(y,"\\|"))))
)

#~~~~~~~~~~~~
#Get data----

ep91pcs <- read.csv("1991/Scotland/Scots_1991_LBS_economicPosition_PostcodeSector/Scots_1991_LBS_economicPosition_PostcodeSector.csv")

#most will remain the same...
ep91pcs$aggID <- ep91pcs$Zone.ID

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  ep91pcs$aggID[ep91pcs$Zone.ID %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg1 <- ep91pcs[,3:6] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

#Some things to do to the columns.
#20: economically active
#115: 'on a government scheme' (which I'm going to count as unemployed)
#134: unemployed

#To match the five census...
#Unempl	EA	prcntEm
names(agg1) <- c('label','EA','govtscheme','unemployed')

agg1$Unempl <- agg1$govtscheme + agg1$unemployed

agg1$prcntEm <- (1 - (agg1$Unempl/agg1$EA)) * 100

#keep/reorder
agg1 <- agg1[,c(1,5,2,6)]

#Attach to geography and save
geogz <- merge(pcs91shp,agg1,by = "label")
geogzdf <- data.frame(geogz)

#save
writeOGR(geogz, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/Employment",
         "1991_econActive_91LBS_noZeroPCS_straightMatch", driver="ESRI Shapefile", overwrite_layer = T)

#checking on zeroes: CoB vs econ active... yup, good.
#CoB91 <- read.csv("VariableCoding/CountryOfBirth_threeCensusRecodes_to91LBS/91LBS_PCS.csv")

#~~~~~~~~~~~~~~~~~~~~~~
#REPEAT FOR (WRONG) HOUSING----
#Data...
dwellings91pcs <- read.csv("1991/Scotland/1991_LBS_totalDwellings/1991_LBS_totalDwellings.csv")
# 
# #Hmm. I think zero columns may be different for household counts.
# dwellings91pcs$totalDwellings <- apply(dwellings91pcs[,c(3:10)],1,sum)
# 
# #How many zeroes?
# table(0 + (dwellings91pcs$totalDwellings == 0))
# #compared to?
# table(0 + (ep91pcs$l080134 == 0))
# 
# #Yup, it's more.

#Let's try averaging from continguous zones once we've got the aggregation. So... 
#Oh hang on, no: needs to happen *before* aggregation to keep the zeroes in the right place
orig_pcs91shp <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991", 
                    layer="soctland_pseudo_pcs_IDtoSingleRow")

df <- data.frame(orig_pcs91shp)

#Is this finding the centroids? Via
#https://cran.r-project.org/web/packages/spdep/vignettes/nb_igraph.html
coords <- coordinates(orig_pcs91shp)

#Yup, centroids
plot(coords, col='grey')
#plot(contig,coords,col='red',lwd=2,add=TRUE)

#Seven is the minimum number to avoid NAs
contig_nk <- knn2nb(knearneigh(coords,k=7), row.names = as.character(orig_pcs91shp@data$label))

#plot(contig_nk,coords,col='red',lwd=2,add=TRUE)
#mx2 <- nb2mat(contig_nk,zero.policy = T)

#Not sure what the point of the row.names arg is. I have a memory of being able to access it
#But we seem only to have an index. Which is fine!

#So let's just check one of those. Cycle over the returned row indices, subset...
subs <- orig_pcs91shp[orig_pcs91shp@data$label %in% sapply(contig_nk[[10]], function(x) as.character(df[x,])),]

#Yup, that works... 
plot(subs)

#Check these indices are in the same order... Well, there aren't the same number so it's going to be tricky!
#So use the labels themselves from the shapefile

#Find total dwellings first to find zeros
dwellings91pcs$totalDwellings <- apply(dwellings91pcs[,c(3:10)],1,sum)

#For any that are zero, find average of dwelling number in nearest ten neighbours
#But only for neighbours that themselves aren't zero...
dwellings91pcs$fromNeighbourAv <- 0
dwellings91pcs$newTotalDwellings <- 0

#Matching to the shapefile's label index (which matches the contig_nk order...)
for(i in 1:nrow(orig_pcs91shp)){
  
  #Get matching index from dwelling, as not quite in same order
  dwellingIndex <- which(as.character(dwellings91pcs$Zone.ID)==as.character(orig_pcs91shp@data$label[i]))
  
  #If the matching dwelling zone has 'total dwellings' of zero...
  # if(dwellings91pcs$totalDwellings[as.character(dwellings91pcs$Zone.ID)==as.character(orig_pcs91shp@data$label[i])] == 0){
  if(dwellings91pcs$totalDwellings[dwellingIndex] == 0){
    
    #print(i)
    #Set it to the average of its ten nearest neighbours, discounting those also with zero values
    #use the df copy
    zonesToAverage <- sapply(contig_nk[[i]], function(x) as.character(df[x,]))
    
    #Get non-zero vals
    valsToAverage <- dwellings91pcs$totalDwellings[dwellings91pcs$Zone.ID %in% zonesToAverage 
                                                   & dwellings91pcs$totalDwellings!=0]
    
    dwellings91pcs$newTotalDwellings[dwellingIndex] <- as.integer(mean(valsToAverage))
    #And mark where it came from
    dwellings91pcs$fromNeighbourAv[dwellingIndex] <- 1
    
  }
  
}

dwellings91pcs$finalTotalDwellings <- ifelse(dwellings91pcs$totalDwellings == 0, 
                                             dwellings91pcs$newTotalDwellings,
                                             dwellings91pcs$totalDwellings)

#Any still zero?
table(0 + (dwellings91pcs$finalTotalDwellings == 0), useNA = 'always')
#SHIPPING
dwellings91pcs[dwellings91pcs$finalTotalDwellings == 0,]

#~~~~~~~~~~~~~~~~
#Right, so... now use that to sum to the new PCSs.
dwellings91pcs$aggID <- dwellings91pcs$Zone.ID

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  dwellings91pcs$aggID[dwellings91pcs$Zone.ID %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg3 <- dwellings91pcs[,3:15] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))


names(agg3)[names(agg3)=="aggID"] <- "label"

agg3$fromNeighbourAv <- ifelse(agg3$fromNeighbourAv > 0, 1,0)

agg3 <- agg3[,c(1,13,11)]
names(agg3) <- c('label','totalDwellings','avFrom7NN')

#Attach to geography and save
geogz <- merge(pcs91shp,agg3,by = "label")
geogzdf <- data.frame(geogz)

#save
# writeOGR(geogz, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch", driver="ESRI Shapefile", overwrite_layer = T)

writeSpatialShape(geogz, 
                  "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/Dwellings/1991_dwellings_7nnAv4zeroes_91LBS_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#REPEAT FOR HOUSING, WITH RIGHT TABLE THIS TIME 1991----
dwellings91pcs2 <- read.csv("1991/Scotland/1991_LBS_totalDwellingsOccupancy/1991_LBS_totalDwellingsOccupancy.csv")
# 
# #Hmm. I think zero columns may be different for household counts.
dwellings91pcs2$totalDwellings <- apply(dwellings91pcs2[,c(3:18)],1,sum)
# 
# #How many zeroes?
table(0 + (dwellings91pcs2$totalDwellings == 0))
# #compared to?
table(0 + (ep91pcs$l080134 == 0))

#It's right this time. Hah. Soooo. 
#Right, so... now use that to sum to the new PCSs.
dwellings91pcs2$aggID <- dwellings91pcs2$Zone.ID

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  dwellings91pcs2$aggID[dwellings91pcs2$Zone.ID %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg3 <- dwellings91pcs2[,3:20] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))


names(agg3)[names(agg3)=="aggID"] <- "label"

agg3 <- agg3[,c(1,18)]

#Attach to geography and save
geogz <- merge(pcs91shp,agg3,by = "label")
geogzdf <- data.frame(geogz)

#save
# writeOGR(geogz, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch", driver="ESRI Shapefile", overwrite_layer = T)

writeSpatialShape(geogz, 
                  "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/Dwellings/1991_dwellings_91LBS_noZeroPCS.shp")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#REPEAT FOR COUNTRY OF BIRTH 1991----
CoB91 <- read.csv("VariableCoding/CountryOfBirth_threeCensusRecodes_to91LBS/91LBS_PCS.csv")

#most will remain the same...
CoB91$aggID <- CoB91$Zone.ID

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  CoB91$aggID[CoB91$Zone.ID %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- CoB91[,3:43] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))


names(agg2)[names(agg2)=="aggID"] <- "label"

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")
geogzdf <- data.frame(geogz)

#save
writeOGR(geogz, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
         "1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch", driver="ESRI Shapefile", overwrite_layer = T)














