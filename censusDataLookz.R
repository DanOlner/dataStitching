#Look at the five and three census data, see what we have.
geolibs <- c("ggplot2","RColorBrewer","spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat",
             "data.table")
lapply(geolibs, require, character.only = TRUE)

library(pryr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Five census CoB----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load all the CoB 5-census data
fiveCensus71 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/CountryOfBirth/1971_CoB_from_71EDs_to_2011_IntermediateGeog.shp")
fiveCensus81 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/CountryOfBirth/1981_CoB_from_81EDs_to_2011_IntermediateGeog.shp")
fiveCensus91 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/CountryOfBirth/1991_CoB_from_01OAs_to_2011_IntermediateGeog.shp")
fiveCensus01 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/CountryOfBirth/2001_CoB_from_2001OAs_to_2011_IntermediateGeog.shp")
fiveCensus11 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/CountryOfBirth/2011_CoB_IntermediateGeog_via_lookupFromOAs.shp")

fc71_df <- data.frame(fiveCensus71)

#A lot of columns we can drop from all of those. Might as well do that when they're merged.
allFiveList <- c(fiveCensus71, fiveCensus81, fiveCensus91, fiveCensus01, fiveCensus11)

#add column marking which census
yrz <- seq(1971,2011,by = 10)

#Dunno why the lapply doesn't work...
#lapply(seq(1:5), function(x) allFiveList[[x]]@data$year <- yrz[x])
for(x in 1:5){
  allFiveList[[x]]@data$year <- yrz[x]
}

#test <- data.frame(allFiveList[[2]])]
allFive <- do.call(rbind,c(allFiveList,makeUniqueIDs = T))

#Tick!
test <- data.frame((allFive))

#Give the countries back some slightly more sensible names
orignames <- names(allFive)

#From the CoB stitching code for the 5 censuses
countryNames <- c(
  'England',
  'Scotland',
  'Wales',
  'Rest of UK',
  'Irish Republic',
  'Old Commonwealth',
  'Africa (New-C)',
  'India',
  'Pakistan',
  'Other Europe',
  'SE Asia New C',
  'Caribbean New C',
  'New Commonwealth other',
  'Rest of World'
)

names(allFive) <- c(orignames[1:11],countryNames,orignames[26])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Look at total CoB cats for each decade, see how they change.-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Sum per decade
allFive_df <- data.frame(allFive)

fiveC_decadeSums <- allFive_df %>% 
  dplyr::select(England:year) %>% 
  group_by(year) %>% 
  summarise_each(funs(sum))

#Long by CoB
fiveC_decadeSumsLong <- fiveC_decadeSums %>% gather(CoB, count, England:Rest.of.World)

#Colours from http://stackoverflow.com/questions/21352683/randomising-qualitative-colours-for-large-sets-in-ggplot
cbPalette <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
  "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
  "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
  "#8A7C64", "#599861")

#http://www.cse.unsw.edu.au/~mike/myrlibrary.old/RColorBrewer/html/ColorBrewer.html
cbPalette <- brewer.pal(12,"Paired")

#Re-order based on max for all years
reodz <- fiveC_decadeSumsLong %>% group_by(CoB) %>% 
  mutate(av = max(count)) 

#Christ knows why mutate turns it into a list. This took far too long to work out. I hate reordering!
fiveC_decadeSumsLong$CoB <- reorder(fiveC_decadeSumsLong$CoB,-unlist(reodz[,4]))

#output <- ggplot(fiveC_decadeSumsLong, aes(x = year, y = count, colour = CoB)) +
# output <- ggplot(fiveC_decadeSumsLong %>% filter(CoB != "Scotlnd"), aes(x = year, y = count, colour = CoB)) +

#Log?
output <- ggplot(fiveC_decadeSumsLong %>% filter(CoB != "Scotland", CoB != "England"), aes(x = year, y = count, colour = CoB)) +
  #geom_line(size =1 )
  geom_line(size =1.5, alpha = 0.3) +
  geom_point(size = 6.5, colour = "#595959") +
  geom_point(size = 6) +
  #scale_y_log10() + ylab("count (log 10)") +
  scale_colour_manual(values=cbPalette)

output

#ggsave("R_outputs/5census_countNoScotsEngLog.png",output,width = 7,height = 4)
ggsave("R_outputs/5census_countNoScotsEng.png",output,width = 7,height = 4)


#~~~~~~~~~~~~~~
#Drop other europe, order appropriately... 
#nooo, don't need to. Removing them leaves the same underlying order for the remainder, obv.
# reodz <- fiveC_decadeSumsLong %>% group_by(CoB) %>% 
#   mutate(av = max(count)) 
# 
# #Christ knows why mutate turns it into a list. This took far too long to work out. I hate reordering!
# fiveC_decadeSumsLong$CoB <- reorder(fiveC_decadeSumsLong$CoB,-unlist(reodz[,4]))


output <- ggplot(fiveC_decadeSumsLong %>% filter(!(CoB %in% c("Scotland","England","Other.Europe"))), 
                 aes(x = year, y = count, colour = CoB)) +
  #geom_line(size =1 )
  geom_line(size =1.5, alpha = 0.3) +
  geom_point(size = 6.5, colour = "#595959") +
  geom_point(size = 6) +
  #scale_y_log10() + ylab("count (log 10)") +
  #To match previous colours
  scale_colour_manual(values=cbPalette[2:12])

output

ggsave("R_outputs/5census_countNoScotsEngRestOFEurope.png",output,width = 7,height = 4)

#~~~~~~~~~~~~~~
#Run again for first three decades
output <- ggplot(fiveC_decadeSumsLong %>% filter(!(year %in% c(2001,2011)),CoB != "Scotland", CoB != "England"), aes(x = year, y = count, colour = CoB)) +
  #geom_line(size =1 )
  geom_line(size =1.5, alpha = 0.3) +
  geom_point(size = 6.5, colour = "#595959") +
  geom_point(size = 6) +
  #scale_y_log10() + ylab("count (log 10)") +
  scale_colour_manual(values=cbPalette)

output

#ggsave("R_outputs/5census_countNoScotsEngLog.png",output,width = 7,height = 4)
ggsave("R_outputs/5census_first3_countNoScotsEng.png",output,width = 7,height = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Five census econ active--------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load all the CoB 5-census data
fiveCensus71_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/1971_econActive_from_71EDs_to_2011_IntermediateGeog.shp")
fiveCensus81_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/1981_econActive_from_81EDs_to_2011_IntermediateGeog.shp")
fiveCensus91_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/1991_econActive_from_91OAs_to_2011_IntermediateGeog.shp")
fiveCensus01_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/2001_econActive_from_01OAs_to_2011_IntermediateGeog.shp")
fiveCensus11_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/2011_econActive_IntermediateGeog_via_lookupFromOAs.shp")

#fc71_df <- data.frame(fiveCensus71)
#A lot of columns we can drop from all of those. Might as well do that when they're merged.
allFiveList_EA <- c(fiveCensus71_EA, fiveCensus81_EA, fiveCensus91_EA, fiveCensus01_EA, fiveCensus11_EA)

lapply(allFiveList_EA,names)

#Get same columns
allFiveList_EA[1:4] <- lapply(allFiveList_EA[1:4],function(x) x[,c(1,12:14)])
#EA and unempl wrong way round in 71
allFiveList_EA[[1]] <- allFiveList_EA[[1]][,c(1,3,2,4)]

#add column marking which census
yrz <- seq(1971,2011,by = 10)

#Dunno why the lapply doesn't work...
#lapply(seq(1:5), function(x) allFiveList[[x]]@data$year <- yrz[x])
for(x in 1:5){
  allFiveList_EA[[x]]@data$year <- yrz[x]
}


#test <- data.frame(allFiveList[[2]])]
allFive_EA <- do.call(rbind,c(allFiveList_EA,makeUniqueIDs = T))

#Tick!
test <- data.frame((allFive_EA))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Three census CoB--------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

threeCensus91 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp")
threeCensus01 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2001_CountryOfBirthRecode_91LBS_noZeroPCS.shp")
threeCensus11 <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2011_CountryOfBirthRecode_91LBS_noZeroPCS.shp")

#tc71_df <- data.frame(fiveCensus71)

#Accidentally added two middle other other cols. Drop second ones. 
#Also make equal length and make cols same names
names(threeCensus91)
names(threeCensus01)
names(threeCensus11)

threeCensus91 <- threeCensus91[,c(1:11,13:42)]
threeCensus01 <- threeCensus01[,c(2:12,14:43)]
threeCensus11 <- threeCensus11[,c(2:12,14:43)]

#We know they were in the same order
names(threeCensus91) <- names(threeCensus01)

all3List <- c(threeCensus91, threeCensus01, threeCensus11)

#add column marking which census
yrz <- seq(1991,2011,by = 10)

#Dunno why the lapply doesn't work...
#lapply(seq(1:5), function(x) allthreeList[[x]]@data$year <- yrz[x])
for(x in 1:3){
  all3List[[x]]@data$year <- yrz[x]
}

#test <- data.frame(allFiveList[[2]])]
all3 <- do.call(rbind,c(all3List,makeUniqueIDs = T))

#Tick!
test <- data.frame((all3))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Look at total CoB cats for each decade, see how they change.-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Sum per decade
all3_df <- data.frame(all3)

threeC_decadeSums <- all3_df %>% 
  dplyr::select(Channel_Is:year) %>% 
  group_by(year) %>% 
  summarise_each(funs(sum))

#Random colours
#http://stackoverflow.com/questions/21352683/randomising-qualitative-colours-for-large-sets-in-ggplot
cols = rainbow(40, s=.6, v=.9)[sample(1:40,40)]

#Long by CoB
threeC_decadeSumsLong <- threeC_decadeSums %>% gather(CoB, count, Channel_Is:Iran)

#Order by largest group in 2011 (3rd row in threeC_decadeSums)
levels(threeC_decadeSumsLong$CoB)

reodz <- threeC_decadeSumsLong %>% group_by(CoB) %>% 
  mutate(av = max(count)) 

#Christ knows why mutate turns it into a list. This took far too long to work out. I hate reordering!
threeC_decadeSumsLong$CoB <- reorder(threeC_decadeSumsLong$CoB,-unlist(reodz[,4]))

#Label quintiles: CoB groups in each year
threeC_decadeSumsLong <- threeC_decadeSumsLong %>% group_by(year) %>% 
  mutate(CoB_groups = as.numeric(cut_number(count,4)))

#pick median value, apply to all of each CoB across years
#(Or same CoBs won't appear across all years)
threeC_decadeSumsLong <- threeC_decadeSumsLong %>% group_by(CoB) %>% 
  mutate(CoB_groupsMedian = median(CoB_groups))

#threeC_decadeSumsLong$CoB %>% length
#reodz[,4] %>% length

#threeC_decadeSumsLong$CoB <- factor(threeC_decadeSumsLong$CoB, threeC_decadeSumsLong$count[])

# output <- ggplot(threeC_decadeSumsLong, aes(x = year, y = count, colour = CoB)) +
# output <- ggplot(threeC_decadeSumsLong %>% filter(CoB != "Scotlnd"), aes(x = year, y = count, colour = CoB)) +
# output <- ggplot(threeC_decadeSumsLong %>% filter(CoB != "Scotland", CoB != "England"), aes(x = year, y = count, colour = CoB)) +
#   geom_line(size =1, alpha = 0.3) +
#   geom_point(size = 2) +
#   scale_colour_manual(values=cols)
# 
# output

for(i in 1:max(threeC_decadeSumsLong$CoB_groupsMedian)){

  output <- ggplot(threeC_decadeSumsLong 
                   #%>% filter(CoB != "Scotland", CoB != "England", CoB_groupsMedian == i), 
                   %>% filter(CoB_groupsMedian == i), 
                   aes(x = year, y = count, colour = CoB)) +
    geom_line(size =1.5, alpha = 0.3) +
    geom_point(size = 4.5, colour = "#595959") +
    geom_point(size = 4) +
    #scale_y_log10() + ylab("count (log 10)") +
    scale_colour_manual(values=cbPalette)
  
  output
  
  #ggsave("R_outputs/3census_noEngScot_log.png",output,width = 9,height = 5)
  ggsave(paste0("R_outputs/3census_quarters_incEngScot",i,".png"),output,width = 9,height = 5)

}

#FACET
output <- ggplot(threeC_decadeSumsLong, 
                 #%>% filter(CoB != "Scotland", CoB != "England", CoB_groupsMedian == i), 
                 #%>% filter(CoB_groupsMedian == i), 
                 aes(x = year, y = count, colour = CoB)) +
  geom_line(size =1.5, alpha = 0.3) +
  geom_point(size = 4.5, colour = "#595959") +
  geom_point(size = 4) +
  facet_wrap(~CoB_groupsMedian) 
  #scale_y_log10() + ylab("count (log 10)") +
  #scale_colour_manual(values=cbPalette)

output

#ggsave("R_outputs/3census_noEngScot_log.png",output,width = 9,height = 5)
ggsave(paste0("R_outputs/3census_quarters_incEngScotFacet.png"),output,width = 9,height = 5)


# #Look at just individual countries that match across all three
# 
# #(produced in LBS_3Census_CoB_stitchingCategories.R)
# #Get just names from foundMatches. First index
# singleCountryNames <- sapply(foundMatches, function(x) x[[1]][1])
# 
# #Not the same fecking list. Good good. Manual then.
# singleCountryNames[4] <- unique(threeC_decadeSumsLong$CoB)[16] %>% as.character
# singleCountryNames[7] <- unique(threeC_decadeSumsLong$CoB)[19] %>% as.character
# singleCountryNames[14] <- unique(threeC_decadeSumsLong$CoB)[26] %>% as.character
# singleCountryNames[26] <- unique(threeC_decadeSumsLong$CoB)[38] %>% as.character
# 
# output <- ggplot(threeC_decadeSumsLong %>%  
#                    #filter(CoB %in% singleCountryNames[3:27]), 
#                    filter(!CoB %in% singleCountryNames), 
#                  aes(x = year, y = count, colour = CoB)) +
#   geom_line(size =1.5, alpha = 0.3) +
#   geom_point(size = 4.5, colour = "#595959") +
#   geom_point(size = 4) +
#   #scale_y_log10() + ylab("count (log 10)") +
#   scale_colour_manual(values=cols)
# 
# output


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Just Scot / Eng / other----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fiveScotEngOther <- cbind(fiveC_decadeSums[,c(1:3)], data.frame(restOfWorld = apply(fiveC_decadeSums[,c(4:15)],1,sum)))

fiveScotEngOther_Long <- fiveScotEngOther %>% gather(CoB, count, England:restOfWorld)

fiveScotEngOther_Long$CoB <- factor(fiveScotEngOther_Long$CoB, levels = c('Scotlnd','England','restOfWorld'))

#http://www.cse.unsw.edu.au/~mike/myrlibrary.old/RColorBrewer/html/ColorBrewer.html
cbPalette <- brewer.pal(3,"Paired")
#cbPalette <- brewer.pal(3,"BrBG")

#output <- ggplot(fiveC_decadeSumsLong, aes(x = year, y = count, colour = CoB)) +
# output <- ggplot(fiveC_decadeSumsLong %>% filter(CoB != "Scotlnd"), aes(x = year, y = count, colour = CoB)) +
output <- ggplot(fiveScotEngOther_Long, aes(x = year, y = count, colour = CoB)) +
  #geom_line(size =1 )
  geom_line(size =1.5, alpha = 0.3) +
  geom_point(size = 7) +
  scale_colour_manual(values=cbPalette)

output

ggsave("R_outputs/5census_engScotRest_count.png",output,width = 7,height = 4)

#~~~~~~~~~~~~~~~~~~~~~~~
#Proportions for those three
propz <- prop.table(as.matrix(fiveScotEngOther[,c(2:4)]), margin = 1) * 100

propz <- cbind(fiveScotEngOther[,1,drop = F],data.frame(propz))

fiveScotEngOther_Long <- propz %>% gather(CoB, percent, England:restOfWorld)

fiveScotEngOther_Long$CoB <- factor(fiveScotEngOther_Long$CoB, levels = c('Scotlnd','England','restOfWorld'))

#http://www.cse.unsw.edu.au/~mike/myrlibrary.old/RColorBrewer/html/ColorBrewer.html
cbPalette <- brewer.pal(3,"Paired")
#cbPalette <- brewer.pal(3,"BrBG")

#output <- ggplot(fiveC_decadeSumsLong, aes(x = year, y = count, colour = CoB)) +
# output <- ggplot(fiveC_decadeSumsLong %>% filter(CoB != "Scotlnd"), aes(x = year, y = count, colour = CoB)) +

output <- ggplot(fiveScotEngOther_Long, aes(x = year, y = percent, colour = CoB)) +
  #geom_line(size =1 )
  geom_line(size =1.5, alpha = 0.3) +
  geom_point(size = 7) +
  scale_colour_manual(values=cbPalette) +
  #geom_text(aes(label = paste0(round(percent,1),"%"), y = percent), size = 3, colour="black")
  #geom_text(aes(label = paste0(round(percent,1),"%"), y = (1.1*percent)), size = 3, colour="black")
  geom_text(aes(label = paste0(round(percent,1),"%"), 
                y = (ifelse(percent > 25,percent,1.3*percent))), size = 3, colour="black")

output

ggsave("R_outputs/5census_engScotRest_percent.png",output,width = 7,height = 4)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get 3 census economically active data------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load all the CoB 5-census data
threeCensus91_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/Employment/1991_econActive_91LBS_noZeroPCS_straightMatch.shp")
threeCensus01_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/Employment/2001_econActive_91LBS_noZeroPCS.shp")
threeCensus11_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/Employment/2011_econActive_91LBS_noZeroPCS.shp")

#fc71_df <- data.frame(threeCensus71)
#A lot of columns we can drop from all of those. Might as well do that when they're merged.
allThreeList_EA <- c(threeCensus91_EA, threeCensus01_EA, threeCensus11_EA)

lapply(allThreeList_EA,names)

#Get same columns
allThreeList_EA[2:3] <- lapply(allThreeList_EA[2:3],function(x) x[,c(2,4:6)])
#EA and unempl wrong way round in 91
allThreeList_EA[[1]] <- allThreeList_EA[[1]][,c(1,4,3,5)]
#And the names are wrong
names(allThreeList_EA[[1]]) <- c('label','EA','Unempl','percentEmp')

#add column marking which census
yrz <- seq(1991,2011,by = 10)

#Dunno why the lapply doesn't work...
#lapply(seq(1:5), function(x) allThreeList[[x]]@data$year <- yrz[x])
for(x in 1:3){
  allThreeList_EA[[x]]@data$year <- yrz[x]
}


#test <- data.frame(allThreeList[[2]])]
allThree_EA <- do.call(rbind,c(allThreeList_EA,makeUniqueIDs = T))

#Tick!
test <- data.frame((allThree_EA))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get 3 census house price data, correlate to employment------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Oh. Mostly already in the right shape?
house <- readRDS("Housing/1991to2001_twoYearBandMeanPrices_1991_PCSnoZeroes.rds")

#Aggregate: mean price by PCS zone
houseMeanz <- house %>% group_by(censusYear,label) %>% 
  summarise(meanPrice = mean(priceFinal))

#to match EA
houseMeanz$censusYear <- as.numeric(houseMeanz$censusYear)

#Couple of zones without house prices. Going to be 91 again - no houses there.

#Combine with employment levels for three censuses
allThree_EA_df <- data.frame(allThree_EA)

house_n_EA <- merge(houseMeanz,allThree_EA_df, by.x=c('label','censusYear'),by.y = c('label','year'), all.y = T)

names(house_n_EA)[names(house_n_EA)=="censusYear"] <- "year"

#Basic per-census corr
#Free scale
output <- ggplot(house_n_EA, aes(x = percentEmp, y = meanPrice)) +
  geom_point() +
  facet_wrap(~year, scales = 'free')

output
#Same scale
output <- ggplot(house_n_EA, aes(x = percentEmp, y = meanPrice)) +
  geom_point() +
  facet_wrap(~year)

output

#Break employment % into quintiles, mark
house_n_EA_q <- house_n_EA %>% group_by(year) %>% 
  mutate(percentEmp_quintiles = as.numeric(cut_number(percentEmp, 5)))

#Apply quintiles from first decade to the rest. Slight faff involved here...
first <- house_n_EA_q %>% dplyr::filter(year == 1991)
second <- house_n_EA_q %>% dplyr::filter(year == 2001)
third <- house_n_EA_q %>% dplyr::filter(year == 2011)

#labels are in same order, so...
second$percentEmp_quintiles <- first$percentEmp_quintiles
third$percentEmp_quintiles <- first$percentEmp_quintiles

#allz <- c(first,second,third)
#WHY NOT???
#house_n_EA_q2 <- do.call(rbind,c(allz,makeUniqueIDs = T))

house_n_EA_q2 <- rbind(first,second) %>% rbind(third)

house_n_EA_q2$percentEmp_quintiles <- factor(house_n_EA_q2$percentEmp_quintiles)
names(house_n_EA_q2)[names(house_n_EA_q2)=="percentEmp_quintiles"] <- "empl5"

#Same scale
output <- ggplot(house_n_EA_q2, aes(x = percentEmp, y = meanPrice, 
                                   #colour = factor(percentEmp_quintiles), shape = factor(percentEmp_quintiles))) +
                                   colour = empl5, shape = empl5)) +
  #facet_wrap(~year) +
  facet_wrap(~year, scales = 'free') +
  #facet_wrap(~year, scales = 'free') +
  annotate("segment", x = 80, xend = 80, y = -1, yend = 125000,colour = "grey", size = 2) +
  #annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,colour = "grey") +
    #facet_wrap(~year, scales = 'free_y') +
#   scale_x_log10() +
#   scale_y_log10() +
  geom_point(size = 1.5)

output

# ggsave("R_outputs/3census_empl_housing_quintiles_sameScale.png",output,width = 15,height = 5)
#ggsave("R_outputs/3census_empl_housing_3tiles.png",output,width = 15,height = 5)
#ggsave("R_outputs/3census_empl_housing_3tiles_sameScale.png",output,width = 15,height = 5)
ggsave("R_outputs/3census_empl_housing_3tiles_free_compline.png",output,width = 15,height = 5)

#Ooo! Identify those five groups on a map... maybe later.



#house_n_EA_q$percentEmp_quintiles <- cut_interval(house_n_EA_q$percentEmp, 5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 census house vs employment: facet against CoB------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Break down by country, then order each country's zone %s. 
#Then facet by country while sizing based on % (or % rank, actually).

#So: processing CoB to stick in with house_n_EA.
all3_df <- data.frame(all3)

#Hmm. May have to do the prop.tables on each year separately. Which we still have... 
one <- prop.table(threeCensus91[,c(3:41)] %>% data.frame %>% as.matrix, margin = 2) %>% data.frame
two <- prop.table(threeCensus01[,c(3:41)] %>% data.frame %>% as.matrix, margin = 2) %>% data.frame
three <- prop.table(threeCensus11[,c(3:41)] %>% data.frame %>% as.matrix, margin = 2) %>% data.frame

one <- one * 100
two <- two * 100
three <- three * 100

#Check that worked. Yup.
apply(three,2,sum)

#Add everything back in
one$year <- 1991
two$year <- 2001
three$year <- 2011

one$label <- threeCensus91@data[,1]
two$label <- threeCensus91@data[,1]
three$label <- threeCensus91@data[,1]


#Ah...
names(one)[names(one)=="year"]
names(two)[names(two)=="year"]
names(three)[names(three)=="year"]
names(house_n_EA)[names(house_n_EA)=="year"]

#It's not letting me do the merge all at once. Dunno why, it's only a one to many merge
#But can do individually
oneMerge <- left_join(one,house_n_EA,by = c("year","label"))
twoMerge <- left_join(two,house_n_EA,by = c("year","label"))
threeMerge <- left_join(three,house_n_EA,by = c("year","label"))

#CoBprops3census <- do.call(rbind,c(one,two,three,makeUniqueIDs = T))
#CoBprops3census <- rbind(one,two)
#CoBprops3census <- rbind(CoBprops3census,three)

CoBprops3census <- rbind(oneMerge,twoMerge)
CoBprops3census <- rbind(CoBprops3census,threeMerge)

#loooong
CoBprops3census_long <- CoBprops3census %>% gather(CoB,percent,1:39)

#equal size groups of CoB share per census year
CoBprops3census_long <- CoBprops3census_long %>% group_by(CoB,year) %>% 
  mutate(count = n())
  # mutate(quintile = as.numeric(cut_number(percent, 3)))

#Or don't. Do it above individually
#merge in the housing/employment data by year and zone
# allzMerge <- merge(CoBprops3census_long,house_n_EA,
#                    by.x = c('year','label'),
#                    by.y = c('censusYear','label'),
#                    all.x = T)

#allzMerge <- left_join(CoBprops3census_long,house_n_EA,by = c("year","label"))
#allzMerge <- left_join(CoBprops3census_long,house_n_EA,by = c("year","label"))

#Right!
output <- ggplot(CoBprops3census_long[CoBprops3census_long$year == 1991,], 
                 aes(x = percentEmp, y = meanPrice, size = percent)) +
  geom_point() +
  facet_wrap(~CoB)

output

output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('England','Scotland'),percent > 2), 
                 aes(x = percentEmp, y = meanPrice, colour = CoB, size = percent)) +
  geom_point() +
  scale_colour_manual(values=cols) +
  facet_wrap(~year)

output

#Look at all CoBs separately
#pick one to test
output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('Poland'),percent > 3), 
#output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('Poland')), 
                 aes(x = percentEmp, y = meanPrice, colour = percent, size = percent)) +
  geom_point() +
  scale_size_continuous(range = c(1, 18)) +
  geom_text(aes(label = label, y = meanPrice, x = percentEmp), size = 3, colour="black") +
  facet_wrap(~year)
  #facet_wrap(~year, scales = 'free')

output

ggsave("R_outputs/testFacet.png",output,width = 9,height = 4)


#repeat for all
for(i in unique(CoBprops3census_long$CoB)){
  
  if((CoBprops3census_long %>% filter(CoB == i, percent > 2) %>% nrow)> 1) {
  
  output <- ggplot(CoBprops3census_long %>% filter(CoB == i, percent > 1), 
                   #output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('Poland')), 
                   aes(x = percentEmp, y = meanPrice, colour = percent, size = percent)) +
    geom_point() +
    scale_size_continuous(range = c(1, 18)) +
    geom_text(aes(label = label, y = meanPrice, x = percentEmp), size = 3, colour="black") +
    facet_wrap(~year)
  #facet_wrap(~year, scales = 'free')
  
  output
  
  ggsave(paste0("R_outputs/facetCoB/",i,".png"),output,width = 12,height = 5)
  
  }

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 census employment: facet against CoB------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Break down by country, then order each country's zone %s. 
#Then facet by country while sizing based on % (or % rank, actually).

#So: processing CoB to stick in with house_n_EA.
all5_df <- data.frame(allFive)

#Use allFive - it's got updated names added

#Hmm. May have to do the prop.tables on each year separately. Which we still have... 
one <- prop.table(all5_df %>% filter(year == 1971) %>% dplyr::select(12:25) %>% as.matrix, margin = 2) %>% data.frame
two <- prop.table(all5_df %>% filter(year == 1981) %>% dplyr::select(12:25) %>% as.matrix, margin = 2) %>% data.frame
three <- prop.table(all5_df %>% filter(year == 1991) %>% dplyr::select(12:25) %>% as.matrix, margin = 2) %>% data.frame
four <- prop.table(all5_df %>% filter(year == 2001) %>% dplyr::select(12:25) %>% as.matrix, margin = 2) %>% data.frame
five <- prop.table(all5_df %>% filter(year == 2011) %>% dplyr::select(12:25) %>% as.matrix, margin = 2) %>% data.frame

one <- one * 100
two <- two * 100
three <- three * 100
four <- four * 100
five <- five * 100

#Check that worked. Yup.
apply(three,2,sum)

#Add everything back in
one$year <- 1971
two$year <- 1981
three$year <- 1991
four$year <- 2001
five$year <- 2011

one$label <- fiveCensus91@data[,1]
two$label <- fiveCensus91@data[,1]
three$label <- fiveCensus91@data[,1]
four$label <- fiveCensus91@data[,1]
five$label <- fiveCensus91@data[,1]

#Ah...
names(one)[names(one)=="year"]
names(two)[names(two)=="year"]
names(three)[names(three)=="year"]
names(house_n_EA)[names(house_n_EA)=="year"]

#ADD ECONOMICALLY ACTIVE HERE----

#It's not letting me do the merge all at once. Dunno why, it's only a one to many merge
#But can do individually
# oneMerge <- left_join(one,house_n_EA,by = c("year","label"))
# twoMerge <- left_join(two,house_n_EA,by = c("year","label"))
# threeMerge <- left_join(three,house_n_EA,by = c("year","label"))

#CoBprops3census <- do.call(rbind,c(one,two,three,makeUniqueIDs = T))
#CoBprops3census <- rbind(one,two)
#CoBprops3census <- rbind(CoBprops3census,three)

#Run just for CoB props... 
CoBprops5census <- rbind(one,two) %>% rbind(three) %>% rbind(four) %>% rbind(five)
#CoBprops5census <- rbind(CoBprops3census,threeMerge)

#loooong
CoBprops5census_long <- CoBprops5census %>% gather(CoB,percent,1:14)

#Or don't. Do it above individually
#merge in the housing/employment data by year and zone
# allzMerge <- merge(CoBprops3census_long,house_n_EA,
#                    by.x = c('year','label'),
#                    by.y = c('censusYear','label'),
#                    all.x = T)

#allzMerge <- left_join(CoBprops3census_long,house_n_EA,by = c("year","label"))
#allzMerge <- left_join(CoBprops3census_long,house_n_EA,by = c("year","label"))

#Right!
output <- ggplot(CoBprops5census_long[CoBprops5census_long$year == 1991,], 
                 aes(x = percentEmp, y = meanPrice, size = percent)) +
  geom_point() +
  facet_wrap(~CoB)

output

output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('England','Scotland'),percent > 2), 
                 aes(x = percentEmp, y = meanPrice, colour = CoB, size = percent)) +
  geom_point() +
  scale_colour_manual(values=cols) +
  facet_wrap(~year)

output

#Look at all CoBs separately
#pick one to test
output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('Poland'),percent > 3), 
#output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('Poland')), 
                 aes(x = percentEmp, y = meanPrice, colour = percent, size = percent)) +
  geom_point() +
  scale_size_continuous(range = c(1, 18)) +
  geom_text(aes(label = label, y = meanPrice, x = percentEmp), size = 3, colour="black") +
  facet_wrap(~year)
  #facet_wrap(~year, scales = 'free')

output

ggsave("R_outputs/testFacet.png",output,width = 9,height = 4)


#repeat for all
for(i in unique(CoBprops3census_long$CoB)){
  
  if((CoBprops3census_long %>% filter(CoB == i, percent > 2) %>% nrow)> 1) {
  
  output <- ggplot(CoBprops3census_long %>% filter(CoB == i, percent > 2), 
                   #output <- ggplot(CoBprops3census_long %>% filter(!CoB %in% c('Poland')), 
                   aes(x = percentEmp, y = meanPrice, colour = percent, size = percent)) +
    geom_point() +
    scale_size_continuous(range = c(1, 18)) +
    geom_text(aes(label = label, y = meanPrice, x = percentEmp), size = 3, colour="black") +
    facet_wrap(~year)
  #facet_wrap(~year, scales = 'free')
  
  output
  
  ggsave(paste0("R_outputs/facetCoB/",i,".png"),output,width = 12,height = 5)
  
  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 Census: spatial pattern exploring------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Attach coordinates to the previous 3-census data (centroids taken via coordinates)
#Convert to latlon for below
proj4string(threeCensus91) <- CRS("+init=epsg:27700")
threeCensusLatLon <- spTransform(threeCensus91, CRS("+proj=longlat +datum=WGS84"))

coordzLatLon <- coordinates(threeCensusLatLon)
coordz <- coordinates(threeCensus91)
#plot(coordz)

coordz <- data.frame(label = threeCensus91@data$label, eastings = coordz[,1],northings = coordz[,2])
coordzLatLon <- data.frame(label = threeCensus91@data$label, eastings = coordzLatLon[,1],northings = coordzLatLon[,2])

CoBprops3census_longGeo <- merge(CoBprops3census_long,coordz,by = "label")
CoBprops3census_longGeoLatLon <- merge(CoBprops3census_long,coordzLatLon,by = "label")

map <- get_map("Glasgow", zoom = 12, source = "osm", color = "bw")
mapPoints <- ggmap(map)
mapPoints

#output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England'),percent > 1), 
output <- ggplot(CoBprops3census_longGeo, 
# output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
#output <- ggplot(CoBprops3census_longGeo, 
                 aes(x = eastings, y = northings, colour = CoB, size = percent)) +
  geom_point() +
  #geom_polygon(data = threeCensus91@polygons) +
  scale_size_continuous(range = c(1, 18)) +
  facet_wrap(~year) +
  #Glasgow (national grid...)
  coord_cartesian(xlim = c(244447, 279006), ylim = c(674747,652751))  +
  guides(size = F)
#facet_wrap(~year, scales = 'free')

output

output <- mapPoints +
  #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
  geom_point(data = CoBprops3census_longGeoLatLon %>% filter(CoB %in% c('Pakistan')), 
             # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
             #output <- ggplot(CoBprops3census_longGeo, 
             aes(x = eastings, y = northings, colour = percent, size = percent)) +
  scale_size_continuous(range = c(1, 12)) +
  guides(size = F) +
  geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
            aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
  #facet_wrap(~year) +
  #scale_colour_manual(values=cols) +
  theme(line = element_blank(),
        #text = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
  #guides(size = F)
#        opts(axis.text.x = theme_blank(),axis.text.y = theme_blank())
  #scale_x_continuous(expand=c(0,0)) + 
  #scale_y_continuous(expand=c(0,0))

output

ggsave("R_outputs/glasgow_Pakistan_percent.png",output,width = 15,height = 5)

#~~~~~~~~~~~~~
#Output three of those, one for each census, for cycling visually through
#yrz <- seq(1991,2011,by = 10)

for(yrz in seq(1991,2011,by = 10)) {

  output <- mapPoints +
    #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
    geom_point(data = CoBprops3census_longGeoLatLon %>% filter(CoB %in% c('Pakistan'), year == yrz), 
               # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
               #output <- ggplot(CoBprops3census_longGeo, 
               aes(x = eastings, y = northings, colour = percent, size = percent)) +
    scale_size_continuous(range = c(1, 12)) +
    guides(size = F) +
    geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
              aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
    guides(size = F, colour = F) +
    #facet_wrap(~year) +
    #scale_colour_manual(values=cols) +
    theme(line = element_blank(),
          #text = element_blank(),
          title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) 
  #        opts(axis.text.x = theme_blank(),axis.text.y = theme_blank())
  #scale_x_continuous(expand=c(0,0)) + 
  #scale_y_continuous(expand=c(0,0))
  
  output
  
  ggsave(paste0("R_outputs/glasgow_percents_3census/",yrz,".png"),output,width = 8,height = 5)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 Census: spatial pattern exploring------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Attach coordinates to the previous 3-census data (centroids taken via coordinates)
#Convert to latlon for below
proj4string(fiveCensus91) <- CRS("+init=epsg:27700")
fiveCensusLatLon <- spTransform(fiveCensus91, CRS("+proj=longlat +datum=WGS84"))

coordz5LatLon <- coordinates(fiveCensusLatLon)
coordz5 <- coordinates(fiveCensus91)
#plot(coordz)

coordz5 <- data.frame(label = fiveCensus91@data$interzn, eastings = coordz5[,1],northings = coordz5[,2])
coordz5LatLon <- data.frame(label = fiveCensus91@data$interzn, eastings = coordz5LatLon[,1],northings = coordz5LatLon[,2])

CoBprops5census_longGeo <- merge(CoBprops5census_long,coordz5,by = "label")
CoBprops5census_longGeoLatLon <- merge(CoBprops5census_long,coordz5LatLon,by = "label")

map <- get_map("Glasgow", zoom = 12, source = "osm", color = "bw")
mapPoints <- ggmap(map)
mapPoints

#output <- ggplot(CoBprops5census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
output <- ggplot(CoBprops5census_longGeo %>% filter(CoB %in% c('Pakistan')), 
# output <- ggplot(CoBprops5census_longGeo, 
                 # output <- ggplot(CoBprops5census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
                 #output <- ggplot(CoBprops5census_longGeo, 
                 aes(x = eastings, y = northings, colour = CoB, size = percent)) +
  geom_point() +
  #geom_polygon(data = fiveCensus91@polygons) +
  scale_size_continuous(range = c(1, 18)) +
  facet_wrap(~year) +
  #Glasgow (national grid...)
  #coord_cartesian(xlim = c(244447, 279006), ylim = c(674747,652751))  +
  guides(size = F)
#facet_wrap(~year, scales = 'free')

output

#Not very informative!
for(yrz in seq(1971,2011,by = 10)) {

#Just one country...
  output <- ggplot(CoBprops5census_longGeo %>% filter(CoB %in% c('Pakistan'), year == yrz), 
  # output <- ggplot(CoBprops5census_longGeo, 
                   # output <- ggplot(CoBprops5census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
                   #output <- ggplot(CoBprops5census_longGeo, 
                   aes(x = eastings, y = northings, size = percent)) +
    geom_point() +
    #geom_polygon(data = fiveCensus91@polygons) +
    scale_size_continuous(range = c(1, 7)) +
    theme(line = element_blank(),
          #text = element_blank(),
          title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    #facet_wrap(~year) +
    #Glasgow (national grid...)
    #coord_cartesian(xlim = c(244447, 279006), ylim = c(674747,652751))  +
    guides(size = F)
  #facet_wrap(~year, scales = 'free')
  
  #output
  
  ggsave(paste0("R_outputs/Scots_percents_5census/",yrz,".png"),output,width = 6,height = 8)

}


#With map

output <- mapPoints +
  #geom_point(data = CoBprops5census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
  geom_point(data = CoBprops5census_longGeoLatLon %>% filter(CoB %in% c('Pakistan')), 
             # output <- ggplot(CoBprops5census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
             #output <- ggplot(CoBprops5census_longGeo, 
             aes(x = eastings, y = northings, colour = percent, size = percent)) +
  scale_size_continuous(range = c(1, 12)) +
  guides(size = F) +
  geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
            aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
  #facet_wrap(~year) +
  #scale_colour_manual(values=cols) +
  theme(line = element_blank(),
        #text = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
#guides(size = F)
#        opts(axis.text.x = theme_blank(),axis.text.y = theme_blank())
#scale_x_continuous(expand=c(0,0)) + 
#scale_y_continuous(expand=c(0,0))

output

ggsave("R_outputs/glasgow_Pakistan_percent.png",output,width = 15,height = 5)

#~~~~~~~~~~~~~
#Output five of those, one for each census, for cycling visually through
#yrz <- seq(1991,2011,by = 10)

for(CoBpick in unique(CoBprops5census_long$CoB)){

  for(yrz in seq(1971,2011,by = 10)) {

    output <- mapPoints +
      #geom_point(data = CoBprops5census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
      geom_point(data = CoBprops5census_longGeoLatLon %>% filter(CoB == CoBpick, year == yrz), 
                 # output <- ggplot(CoBprops5census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
                 #output <- ggplot(CoBprops5census_longGeo, 
                 aes(x = eastings, y = northings, colour = percent, size = percent)) +
      scale_size_continuous(range = c(1, 12)) +
      guides(size = F) +
#       geom_text(data = CoBprops5census_longGeoLatLon %>% filter(CoB == CoBpick, year == yrz), 
#                 aes(label = paste0(round(percent,2),"%"), x = eastings, y = northings), size = 3, colour="black") +
      guides(size = F, colour = F) +
      #facet_wrap(~year) +
      #scale_colour_manual(values=cols) +
      theme(line = element_blank(),
            #text = element_blank(),
            title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    #        opts(axis.text.x = theme_blank(),axis.text.y = theme_blank())
    #scale_x_continuous(expand=c(0,0)) + 
    #scale_y_continuous(expand=c(0,0))
    
    output
    
    ggsave(paste0("R_outputs/glasgow_percents_5census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
  
}}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 Census: clustering based on per zone pop %------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So starting with proportion tables again but along the other axis
#need to break  down again to do this
# getProp <- function(oneYear) prop.table(oneYear) * 100
# 
# rez <- lapply(c(
#   as.matrix(all3_df %>% filter(year == 1991) %>% dplyr::select(3:41))
#   #threeMerge %>% filter(year == 2001) %>% dplyr::select(1:39),
#   #threeMerge %>% filter(year == 2011) %>% dplyr::select(1:39)
#   ),
#   function(x) getProp(x)
#   )

getProp <- function(yearz){
  
  oneZoneProps <- all3_df %>% filter(year == yearz) %>% dplyr::select(3:41)
  oneZoneProps <- prop.table(as.matrix(oneZoneProps), margin = 1) %>% data.frame
  oneZoneProps <- oneZoneProps * 100
  
  return(oneZoneProps)
  
}

#test <- getProp(1991)

rez <- rbind(getProp(1991), getProp(2001)) %>% rbind(getProp(2011))

#Add back label/year columns
perZoneCoBprops_3census <- cbind(rez,all3_df[,c(1,42)])

#Long...
perZoneCoBprops_3censusLONG<- perZoneCoBprops_3census %>% gather(CoB, percent, Channel_Is:Iran)

#~~~~~~~~~~~~~~~~~~~~~~~
#Having already got projections/coords above...
#Oh no, need them again.
proj4string(threeCensus91) <- CRS("+init=epsg:27700")
threeCensusLatLon <- spTransform(threeCensus91, CRS("+proj=longlat +datum=WGS84"))

coordzLatLon <- coordinates(threeCensusLatLon)
coordz <- coordinates(threeCensus91)

coordz <- data.frame(label = threeCensus91@data$label, eastings = coordz[,1],northings = coordz[,2])
coordzLatLon <- data.frame(label = threeCensus91@data$label, eastings = coordzLatLon[,1],northings = coordzLatLon[,2])


#Add TTWA for subsetting (via intersect)
ttwaIntersect <- readShapeSpatial("Intersects/otherIntersects/PCS_noZeros_TTWA_Scots.shp")

#pick only those where PCS intersect area is largest
ttwaIntersect@data$area <- gArea(ttwaIntersect, byid = T)
testdf <- data.frame(ttwaIntersect)

PCS_ttwas <- testdf %>% group_by(label) %>% 
  dplyr::filter(area == max(area))

#merge in
perZoneCoBprops_3censusLONG <- merge(perZoneCoBprops_3censusLONG,PCS_ttwas[,c(1,4)],by = "label")

perZoneCoBprops_3censusLongGeo <- merge(perZoneCoBprops_3censusLONG,coordz,by = "label")
perZoneCoBprops_3censusLongGeoLatLon <- merge(perZoneCoBprops_3censusLONG,coordzLatLon,by = "label")

map <- get_map("Glasgow", zoom = 12, source = "osm", color = "bw")
mapPoints <- ggmap(map)
#mapPoints


#output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England'),percent > 1), 
# output <- ggplot(perZoneCoBprops_3censusLongGeo, 
output <- ggplot(perZoneCoBprops_3censusLongGeo %>% filter(!CoB %in% c('Scotland','England')), 
# output <- ggplot(perZoneCoBprops_3censusLongGeo, 
                 aes(x = eastings, y = northings, colour = CoB, size = percent)) +
  geom_point() +
  #geom_polygon(data = threeCensus91@polygons) +
  scale_size_continuous(range = c(1, 18)) +
  facet_wrap(~year) +
  #Glasgow (national grid...)
  coord_cartesian(xlim = c(244447, 279006), ylim = c(674747,652751))  +
  guides(size = F)
#facet_wrap(~year, scales = 'free')

output

output <- mapPoints +
  #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
  geom_point(data = perZoneCoBprops_3censusLongGeoLatLon %>% filter(CoB %in% c('Pakistan')), 
             # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
             #output <- ggplot(CoBprops3census_longGeo, 
             aes(x = eastings, y = northings, colour = percent, size = percent)) +
  scale_size_continuous(range = c(1, 8)) +
  guides(size = F) +
  geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
            aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
  facet_wrap(~year) +
  #scale_colour_manual(values=cols) +
  theme(line = element_blank(),
        #text = element_blank(),
        title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) 
#guides(size = F)
#        opts(axis.text.x = theme_blank(),axis.text.y = theme_blank())
#scale_x_continuous(expand=c(0,0)) + 
#scale_y_continuous(expand=c(0,0))

output

#ggsave("R_outputs/glasgow_Pakistan_percent.png",output,width = 15,height = 5)

for(CoBpick in unique(perZoneCoBprops_3censusLongGeoLatLon$CoB)){
  
  for(yrz in seq(1991,2011,by = 10)) {
    
    output <- mapPoints +
      #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
      geom_point(data = perZoneCoBprops_3censusLongGeoLatLon 
                 %>% filter(CoB == CoBpick,year == yrz), 
                 # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
                 #output <- ggplot(CoBprops3census_longGeo, 
                 aes(x = eastings, y = northings, colour = percent, size = percent)) +
      scale_size_continuous(range = c(1, 8)) +
      guides(size = F) +
#       geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
#                 aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
      #facet_wrap(~year) +
      #scale_colour_manual(values=cols) +
      theme(line = element_blank(),
            #text = element_blank(),
            title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    
    output
    
    #ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
    ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census_facet/",CoBpick,yrz,".png"),output,width = 8,height = 5)
    
  }
  
}

#facet year instead. Subset to just Glasgow
for(CoBpick in unique(perZoneCoBprops_3censusLongGeoLatLon$CoB)){
  
    output <- mapPoints +
      #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
      geom_point(data = perZoneCoBprops_3censusLongGeoLatLon 
                 %>% filter(CoB == CoBpick, name == "Glasgow"), 
                 # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
                 #output <- ggplot(CoBprops3census_longGeo, 
                 aes(x = eastings, y = northings, colour = percent, size = percent)) +
      #scale_size_continuous(range = c(1, 8)) +
      guides(size = F) +
#       geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
#                 aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
      facet_wrap(~year) +
      #scale_colour_manual(values=cols) +
      theme(line = element_blank(),
            #text = element_blank(),
            title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    
    output
    
    #ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
    ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census_facet/",CoBpick,".png"),output,width = 8,height = 5)
  
}



#facet year instead. Repeat for Edinburgh
map2 <- get_map("Edinburgh", zoom = 12, source = "osm", color = "bw")
mapPoints2 <- ggmap(map2)
mapPoints2

for(CoBpick in unique(perZoneCoBprops_3censusLongGeoLatLon$CoB)){
  
  output <- mapPoints2 +
    #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
    geom_point(data = perZoneCoBprops_3censusLongGeoLatLon 
               %>% filter(CoB == CoBpick, name == "Edinburgh"), 
               # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
               #output <- ggplot(CoBprops3census_longGeo, 
               aes(x = eastings, y = northings, colour = percent, size = percent)) +
    #scale_size_continuous(range = c(1, 8)) +
    guides(size = F) +
    #       geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
    #                 aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
    facet_wrap(~year) +
    #scale_colour_manual(values=cols) +
    theme(line = element_blank(),
          #text = element_blank(),
          title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) 
  
  output
  
  #ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
  ggsave(paste0("R_outputs/edinburgh_PerZonePercents_3census_facet/",CoBpick,".png"),output,width = 8,height = 5)
  
}

#Out of interest... where are the most English? Just top PCS first.
engPCS <- perZoneCoBprops_3censusLONG %>% filter(year==2011,CoB=="England") %>% arrange(-percent)

#Berwick - so nearer to border. Kinda makes sense. Oh - the town itself is actually in England?


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Five Census: clustering based on per zone pop %------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So starting with proportion tables again but along the other axis
#need to break  down again to do this
getProp5 <- function(yearz){
  
  oneZoneProps <- all5_df %>% filter(year == yearz) %>% dplyr::select(12:25)
  oneZoneProps <- prop.table(as.matrix(oneZoneProps), margin = 1) %>% data.frame
  oneZoneProps <- oneZoneProps * 100
  
  return(oneZoneProps)
  
}

test <- getProp5(1991) %>% data.frame

rez <- rbind(getProp5(1971), getProp5(1981)) %>% 
  rbind(getProp5(1991)) %>% 
  rbind(getProp5(2001)) %>% 
  rbind(getProp5(2011))

#Add back label/year columns
perZoneCoBprops_5census <- cbind(rez,all5_df[,c(1,26)])

#Long...
perZoneCoBprops_5censusLONG<- perZoneCoBprops_5census %>% gather(CoB, percent, England:Rest.of.World)

#~~~~~~~~~~~~~~~~~~~~~~~
#Having already got projections/coords above...
proj4string(fiveCensus91) <- CRS("+init=epsg:27700")
fiveCensusLatLon <- spTransform(fiveCensus91, CRS("+proj=longlat +datum=WGS84"))

coordz5LatLon <- coordinates(fiveCensusLatLon)
coordz5 <- coordinates(fiveCensus91)
#plot(coordz)

coordz5 <- data.frame(label = fiveCensus91@data$interzn, eastings = coordz5[,1],northings = coordz5[,2])
coordz5LatLon <- data.frame(label = fiveCensus91@data$interzn, eastings = coordz5LatLon[,1],northings = coordz5LatLon[,2])



#Add TTWA for subsetting (via intersect)
ttwaIntersect2 <- readShapeSpatial("Intersects/otherIntersects/2011IZs_TTWA_Scots.shp")

#pick only those where PCS intersect area is largest
ttwaIntersect2@data$area <- gArea(ttwaIntersect2, byid = T)
testdf <- data.frame(ttwaIntersect2)

IZs_ttwas <- testdf %>% group_by(label) %>% 
  dplyr::filter(area == max(area))

#merge in
perZoneCoBprops_5censusLONG <- merge(perZoneCoBprops_5censusLONG,IZs_ttwas[,c(1,13)],
                                     by.x='interzn', by.y='interzone')

names(perZoneCoBprops_5censusLONG)[names(perZoneCoBprops_5censusLONG)=="name_2"] <- "name"


perZoneCoBprops5census_longGeo <- merge(perZoneCoBprops_5censusLONG,coordz5,
                                 by.x = "interzn",
                                 by.y = "label")

perZoneCoBprops5census_longGeoLatLon <- merge(perZoneCoBprops_5censusLONG,coordz5LatLon,
                                       by.x = "interzn",
                                       by.y = "label")




map <- get_map("Glasgow", zoom = 12, source = "osm", color = "bw")
mapPoints <- ggmap(map)
mapPoints



#NOT CHANGED TO FIVE CENSUS YET
for(CoBpick in unique(perZoneCoBprops_3censusLongGeoLatLon$CoB)){
  
  for(yrz in seq(1991,2011,by = 10)) {
    
    output <- mapPoints +
      #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
      geom_point(data = perZoneCoBprops_3censusLongGeoLatLon 
                 %>% filter(CoB == CoBpick,year == yrz), 
                 # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
                 #output <- ggplot(CoBprops3census_longGeo, 
                 aes(x = eastings, y = northings, colour = percent, size = percent)) +
      scale_size_continuous(range = c(1, 8)) +
      guides(size = F) +
      #       geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
      #                 aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
      #facet_wrap(~year) +
      #scale_colour_manual(values=cols) +
      theme(line = element_blank(),
            #text = element_blank(),
            title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank()) 
    
    output
    
    #ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
    ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census_facet/",CoBpick,yrz,".png"),output,width = 8,height = 5)
    
  }
  
}

#facet year instead. Subset to just Glasgow
for(CoBpick in unique(perZoneCoBprops5census_longGeoLatLon$CoB)){
  
  output <- mapPoints +
    #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
    geom_point(data = perZoneCoBprops5census_longGeoLatLon 
               %>% filter(CoB == CoBpick, name == "Glasgow"), 
               # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
               #output <- ggplot(CoBprops3census_longGeo, 
               aes(x = eastings, y = northings, colour = percent, size = percent)) +
    #scale_size_continuous(range = c(1, 8)) +
    guides(size = F) +
    #       geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
    #                 aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
    facet_wrap(~year) +
    #scale_colour_manual(values=cols) +
    theme(line = element_blank(),
          #text = element_blank(),
          title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) 
  
  output
  
  #ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
  ggsave(paste0("R_outputs/glasgow_PerZonePercents_5census_facet/",CoBpick,".png"),output,width = 8,height = 5)
  
}



#facet year instead. Repeat for Edinburgh
map2 <- get_map("Edinburgh", zoom = 12, source = "osm", color = "bw")
mapPoints2 <- ggmap(map2)
mapPoints2

#facet year instead. Subset to just Glasgow
for(CoBpick in unique(perZoneCoBprops5census_longGeoLatLon$CoB)){
  
  output <- mapPoints2 +
    #geom_point(data = CoBprops3census_longGeoLatLon %>% filter(!CoB %in% c('Scotland','England')), 
    geom_point(data = perZoneCoBprops5census_longGeoLatLon 
               %>% filter(CoB == CoBpick, name == "Edinburgh"), 
               # output <- ggplot(CoBprops3census_longGeo %>% filter(!CoB %in% c('Scotland','England')), 
               #output <- ggplot(CoBprops3census_longGeo, 
               aes(x = eastings, y = northings, colour = percent, size = percent)) +
    #scale_size_continuous(range = c(1, 8)) +
    guides(size = F) +
    #       geom_text(data = CoBprops3census_longGeoLatLon %>% filter(percent > 1.5), 
    #                 aes(label = label, x = eastings, y = northings), size = 3, colour="black") +
    facet_wrap(~year) +
    #scale_colour_manual(values=cols) +
    theme(line = element_blank(),
          #text = element_blank(),
          title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) 
  
  output
  
  #ggsave(paste0("R_outputs/glasgow_PerZonePercents_3census/",CoBpick,yrz,".png"),output,width = 8,height = 5)
  ggsave(paste0("R_outputs/edinburgh_PerZonePercents_5census_facet/",CoBpick,".png"),output,width = 8,height = 5)
  
}




#Out of interest... where are the most English? Just top PCS first.
engPCS <- perZoneCoBprops_3censusLONG %>% filter(year==2011,CoB=="England") %>% arrange(-percent)

#Berwick - so nearer to border. Kinda makes sense. Oh - the town itself is actually in England?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Three-census: CoB zone pop % vs EA, boxplots etc------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So I already have this data for three-census. Somewhere.
#Needs merging in. Easy or no?
perZoneCoBprops_3censusLONG_EA <- merge(perZoneCoBprops_3censusLONG,house_n_EA,
                                        by.x = c('label','year'),by.y = c('label','year'))

#for ordering:
#mean zone employment per year per CoB - do they locate in high or low?
perZoneCoBprops_3censusLONG_EA <- perZoneCoBprops_3censusLONG_EA %>% 
  group_by(year,CoB) %>% 
  mutate(meanEmpl_per_CoB = mean(percentEmp))

#Mark largest migrant groups
#perZoneCoBprops_3censusLONG_EA <- perZoneCoBprops_3censusLONG_EA %>% group_by(CoB) %>% 
#  mutate(averageCoBPercent = mean(percent))

#restrict results to glasgow/edinburgh...
#perZoneCoBprops_3censusLONG_EA <- merge(perZoneCoBprops_3censusLONG_EA,PCS_ttwas[,c(1,4)],by = 'label')


city <- perZoneCoBprops_3censusLONG_EA %>% filter(name == "Glasgow")
#city <- perZoneCoBprops_3censusLONG_EA %>% filter(name == "Edinburgh")

#reorder... when we know what
#Ah: need to restrict to locations where particular CoBs are *mostly* going
#So pick a certain top % of each CoB group in each 
#http://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
top <- 20

for(yr in seq(1991,2011, by = 10)){
#yr = 2011

#topPercent <- perZoneCoBprops_3censusLONG_EA %>% 
topPercent <- city %>% 
  group_by(year,CoB) %>% 
  filter(percent > quantile(percent,prob=1-top/100))
  #top_n(10,percent)
  
#Order by median percent emply value
topPercent <- topPercent %>% 
  group_by(year,CoB) %>% 
  mutate(median = median(percentEmp))

#Work with sub-group to get order correct... for one year.
#Or don't, for now, so we can compare across years.

#Or do, cos it's ugly. Try and colour by CoB consistently.
topPercentYear <- topPercent %>% filter(year == yr)

#order in the chart's going to be median...
topPercentYear$CoB <- reorder(topPercentYear$CoB, -topPercentYear$median)

#Some faff getting consistent colour
#Match a key against alphatical order of countries...
#This is the order they appear in the chart.
countries <- data.frame(countries = levels(topPercentYear$CoB), 
                        num = 1:(levels(topPercentYear$CoB) %>% length))

countries <- countries[order(countries$countries),]
countries$newnum <- 1:nrow(countries)
#reorder by this. Should give us our consistent country alphabetical ref
countries <- countries[order(countries$num),]

cols2 <- cols[countries$newnum]

#Boxplot: per census year, which CoBs are locating in highest or lowest empl?
#output <- ggplot(topPercent %>% filter(year == 2011),
#Means from http://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
output <- ggplot(topPercentYear,
  aes(x = CoB, y = percentEmp, fill = CoB)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0)) +
  scale_fill_manual(values = cols2) +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  guides(fill = F)

output

# ggsave(paste0("R_outputs/3censusBoxplots/top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
# ggsave(paste0("R_outputs/3censusBoxplots/top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
# ggsave(paste0("R_outputs/3censusBoxplots/edinburgh_top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
# ggsave(paste0("R_outputs/3censusBoxplots/edinburgh_top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
ggsave(paste0("R_outputs/3censusBoxplots/",top,"glasgow_top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
              output,width = 8,height = 4)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Five-census: CoB zone pop % vs EA, boxplots etc------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

perZoneCoBprops_5censusLONG

#Need to get EA for 5 census
fiveCensus71_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/1971_econActive_from_71EDs_to_2011_IntermediateGeog.shp")
fiveCensus81_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/1981_econActive_from_81EDs_to_2011_IntermediateGeog.shp")
fiveCensus91_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/1991_econActive_from_91OAs_to_2011_IntermediateGeog.shp")
fiveCensus01_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/2001_econActive_from_01OAs_to_2011_IntermediateGeog.shp")
fiveCensus11_EA <- readShapeSpatial("C:/Data/Census/StitchOutputs/Scotland/fiveCensus_to2011_IZs_raw/Employment/2011_econActive_IntermediateGeog_via_lookupFromOAs.shp")

#fc71_df <- data.frame(fiveCensus71)
#A lot of columns we can drop from all of those. Might as well do that when they're merged.
allfiveList_EA <- c(fiveCensus71_EA,fiveCensus81_EA,fiveCensus91_EA, fiveCensus01_EA, fiveCensus11_EA)

lapply(allfiveList_EA,names)

#Get same columns
allfiveList_EA[2:4] <- lapply(allfiveList_EA[2:4],function(x) x[,c(1,12:14)])
#EA and unempl wrong way round in 91
allfiveList_EA[[1]] <- allfiveList_EA[[1]][,c(1,13,12,14)]

#add column marking which census
yrz <- seq(1971,2011,by = 10)

#Dunno why the lapply doesn't work...
#lapply(seq(1:5), function(x) allfiveList[[x]]@data$year <- yrz[x])
for(x in 1:5){
  allfiveList_EA[[x]]@data$year <- yrz[x]
}


#test <- data.frame(allfiveList[[2]])]
allfive_EA <- do.call(rbind,c(allfiveList_EA,makeUniqueIDs = T))

#Tick!
test <- data.frame((allfive_EA))

#~~~~~~~~~~~~~~

allfive_EA_df <- data.frame(allfive_EA)

#Match to per zone data
perZoneCoBprops_5censusLONG_EA <- merge(perZoneCoBprops_5censusLONG, allfive_EA_df,
                                        by = c('interzn','year'))

names(perZoneCoBprops_5censusLONG_EA)[names(perZoneCoBprops_5censusLONG_EA)=='prcntEm'] <- 'percentEmp'

#city <- perZoneCoBprops_5censusLONG_EA %>% filter(name == "Edinburgh")
city <- perZoneCoBprops_5censusLONG_EA %>% filter(name == "Glasgow")

#reorder... when we know what
#Ah: need to restrict to locations where particular CoBs are *mostly* going
#So pick a certain top % of each CoB group in each 
#http://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
top <- 20

for(yr in seq(1971,2011, by = 10)){
  #yr = 2011
  
  #topPercent <- perZoneCoBprops_3censusLONG_EA %>% 
  topPercent <- city %>% 
    group_by(year,CoB) %>% 
    filter(percent > quantile(percent,prob=1-top/100))
    #top_n(10,percent)
  
  #Order by median percent emply value
  topPercent <- topPercent %>% 
    group_by(year,CoB) %>% 
    mutate(median = median(percentEmp))
  
  #Work with sub-group to get order correct... for one year.
  #Or don't, for now, so we can compare across years.
  
  #Or do, cos it's ugly. Try and colour by CoB consistently.
  topPercentYear <- topPercent %>% filter(year == yr)
  
  #order in the chart's going to be median...
  topPercentYear$CoB <- reorder(topPercentYear$CoB, -topPercentYear$median)
  
  #Some faff getting consistent colour
  #Match a key against alphatical order of countries...
  #This is the order they appear in the chart.
  countries <- data.frame(countries = levels(topPercentYear$CoB), 
                          num = 1:(levels(topPercentYear$CoB) %>% length))
  
  countries <- countries[order(countries$countries),]
  countries$newnum <- 1:nrow(countries)
  #reorder by this. Should give us our consistent country alphabetical ref
  countries <- countries[order(countries$num),]
  
  cols2 <- cols[countries$newnum]
  
  #Boxplot: per census year, which CoBs are locating in highest or lowest empl?
  #output <- ggplot(topPercent %>% filter(year == 2011),
  #Means from http://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
  output <- ggplot(topPercentYear,
                   aes(x = CoB, y = percentEmp, fill = CoB)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0)) +
    scale_fill_manual(values = cols2) +
    stat_summary(fun.y=mean, colour="darkred", geom="point", 
                 shape=18, size=3,show_guide = FALSE) +
    guides(fill = F)
  
  output
  
  # ggsave(paste0("R_outputs/3censusBoxplots/top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
  #               output,width = 8,height = 4)
  # ggsave(paste0("R_outputs/3censusBoxplots/top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
  #               output,width = 8,height = 4)
  # ggsave(paste0("R_outputs/3censusBoxplots/edinburgh_top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
  #               output,width = 8,height = 4)
  ggsave(paste0("R_outputs/5censusBoxplots/",top,"glasgow_top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
         output,width = 8,height = 4)
  
}




