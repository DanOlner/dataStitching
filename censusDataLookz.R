#Look at the five and three census data, see what we have.
geolibs <- c("ggplot2","RColorBrewer","spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat",
             "data.table","pryr","geoR","plyr","data.table")
lapply(geolibs, require, character.only = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Five census CoB----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
allFive_EA_df <- data.frame((allFive_EA))

#~~~~~~~~~~~~~~
#Check between-census EA correlations
percentEmpWide <- do.call(cbind,
                  list(allFive_EA_df$prcntEm[allFive_EA_df$year==1971],
                       allFive_EA_df$prcntEm[allFive_EA_df$year==1981],
                       allFive_EA_df$prcntEm[allFive_EA_df$year==1991],
                       allFive_EA_df$prcntEm[allFive_EA_df$year==2001],
                       allFive_EA_df$prcntEm[allFive_EA_df$year==2011]
                       )
                  ) %>% data.frame()

#invert: wanna see % UNemployed
percentEmpWide <- 100 - percentEmpWide 

names(percentEmpWide) <- c('1971',
                           '1981',
                           '1991',
                           '2001',
                           '2011')

pairs((percentEmpWide))

#Reckon I just wanna see particular ones
plot(percentEmpWide$`1971`,percentEmpWide$`1981`)
plot(percentEmpWide$`1981`,percentEmpWide$`1991`)
plot(percentEmpWide$`1991`,percentEmpWide$`2001`)
plot(percentEmpWide$`2001`,percentEmpWide$`2011`)

#Back to original: think I can do lag between years with one thingyo
#Need the lag to be by place, silly person. Year should be in correct order
#Plus drop 71, no lag possible
lagz <- allFive_EA_df %>% 
  arrange(interzn, year) %>% 
  group_by(interzn) %>% 
  mutate(change = prcntEm - lag(prcntEm)) %>% 
  filter(year != 1971)

ggplot() +
  geom_boxplot(data = lagz, aes(x = factor(year), y = change))

#save to look in QGIS
#Different column for each year's change
lagz4gis <- lagz %>% dplyr::select(interzn,year,change) %>% 
  spread(year, change)

#check zones are in same order. Ah, newp! Yeah, I arranged remember? Will need to merge in.
fiveCensus11_EA@data$interzn == lagz4gis$interzn

useThis <- fiveCensus11_EA

useThis@data <- merge(useThis@data,lagz4gis,by = 'interzn')
  
useThisdf <- data.frame(useThis)
fiveCensus11_EA_df <- data.frame(fiveCensus11_EA)

writeSpatialShape(useThis, 'QGIS/temp/employment5censusChange.shp')
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

#Update: I appear to have fixed it. Did I? Yup!
table(names(threeCensus91)==names(threeCensus01))
table(names(threeCensus11)==names(threeCensus01))

#So no longer need all this
# threeCensus91 <- threeCensus91[,c(1:11,13:42)]
# threeCensus01 <- threeCensus01[,c(2:12,14:43)]
# threeCensus11 <- threeCensus11[,c(2:12,14:43)]
#We know they were in the same order
#names(threeCensus91) <- names(threeCensus01)

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

#Save! I keep on having to do the above...
saveRDS(all3,'R_data/all3.rds')

all3_df <- data.frame(all3)

saveRDS(all3_df,'R_data/all3_df.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 Census CoB: across zone shares----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Question: doesn't using across-zone shares make it autocorrelate to zone pop?
#Avoiding prop.table, use mutate_each to get per column proportions for each group.

#Add in total pop column
all3_df$totalPop <- apply(all3_df[,c(4:42)],1,sum)

#It'll annoy me the year col being in the wrong position
all3_df <- all3_df[,c(1:42,44,43)]

all3_df_shares <- all3_df %>% 
  dplyr::select(4:44) %>% 
  group_by(year) %>% 
  #mutate(prop = prop.table(.,margin = 2))#NONONO!
  #mutate_each(  funs(  ((.)/sum(.))*100  ), var = c(4:43) )#YESYESYES
  mutate_each(  funs(  ((.)/sum(.))*100  ) )#YESYESYES


#pre-selecting variables (for some reason) replaces originals
#with the proportions and keeps the correct column names
#So just add back in the zone references
all3_df_shares <- cbind(all3_df[,c(1:3)],all3_df_shares)

#Did that work? Yup!
apply(all3_df_shares[all3_df$year==2011,c(4:43)],2,sum)

#~~~~~~~~~~~~~~~~~~~~
#Now: I think all of them are just going to correlate with total pop per zone, aren't they?
#So pick some examples
corz <- all3_df_shares[all3_df_shares$year %in% c(1991,2011),c("Irish_Repu","totalPop","year")]
corz <- all3_df_shares[all3_df_shares$year %in% c(1991,2011),c("Pakistan","totalPop","year")]
corz <- all3_df_shares[all3_df_shares$year %in% c(1991,2011),c("England","totalPop","year")]

#corzwide <- dcast(corz,Irish_Repu+totalPop~year)

#And what if I just keep the top number? What's the correlation to pop then?
#Say, top 50
corztop <- corz %>% 
  group_by(year) %>% 
  top_n(n = 100,wt = Irish_Repu)

corztop <- corz

#**** **
#Annoying failure to use any widening function
# corzwide <- do.call(cbind,list(corz[corz$year==1991,c(1:2)],
#                                corz[corz$year==2011,c(1:2)]))
corzwide <- do.call(cbind,list(corztop[corztop$year==1991,c(1:2)],
                               corztop[corztop$year==2011,c(1:2)]))

#names(corzwide) <- c('Irish_Repu91','totalPop91','Irish_Repu11','totalPop11')
names(corzwide) <- c('cob91','totalPop91','cob11','totalPop11')

pairs((corzwide))
pairs(log(corzwide))

tst <- lm(cob11~totalPop91,corzwide)
summary(tst)

tst2 <- lm(cob11~cob91,corzwide)
summary(tst2)

tst3 <- lm(cob11~cob91+totalPop91,corzwide)
summary(tst3)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Look at total CoB cats for each decade, see how they change.-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#Load all the CoB 3-census data
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
  annotate("segment", x = 80, xend = 80, y = 0, yend = 125000,colour = "grey", size = 2) +
  #annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,colour = "grey") +
    #facet_wrap(~year, scales = 'free_y') +
   #scale_x_log10() +
   #scale_y_log10() +
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

#TTWA
getCity <- 'Edinburgh'
#getCity <- 'Glasgow'

city <- perZoneCoBprops_3censusLONG_EA %>% filter(name == getCity)
#city <- perZoneCoBprops_3censusLONG_EA %>% filter(name == "Edinburgh")

#highlightCoB <- 'England'

#reorder... when we know what
#Ah: need to restrict to locations where particular CoBs are *mostly* going
#So pick a certain top % of each CoB group in each 
#http://stackoverflow.com/questions/1563961/how-to-find-top-n-of-records-in-a-column-of-a-dataframe-using-r
top <- 10

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

#Some faff getting consistent colour
#Match a key against alphatical order of countries...
#This is the order they appear in the chart.

#do this for first year, keep the same order...
if(yr==1991){

  countries <- data.frame(countries = levels(topPercentYear$CoB), 
                          num = 1:(levels(topPercentYear$CoB) %>% length))
  
  countries <- countries[order(countries$countries),]
  countries$newnum <- 1:nrow(countries)
  #reorder by this. Should give us our consistent country alphabetical ref
  countries <- countries[order(countries$num),]
  
  cols2 <- cols[countries$newnum]
  
  #For using to order vars
  ninetyOneOrder <- topPercentYear[,c('CoB','median')] %>% 
    distinct(CoB) %>% 
    rename(median91 = median)

}

#Use 1991's order
#order in the chart's going to be median...
#Slightly fiddly cos different number of zones.
#So merge in 1991 medians - they match against CoB
topPercentYear2 <- merge(topPercentYear,ninetyOneOrder,by = 'CoB')

#topPercentYear$CoB <- reorder(topPercentYear$CoB, -topPercentYear$median91)
topPercentYear2$CoB <- reorder(topPercentYear2$CoB, -topPercentYear2$median91)

#orig
#topPercentYear$CoB <- reorder(topPercentYear$CoB, -topPercentYear$median)


#Boxplot: per census year, which CoBs are locating in highest or lowest empl?
#output <- ggplot(topPercent %>% filter(year == 2011),
#Means from http://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
output <- ggplot(topPercentYear2,
  aes(x = CoB, y = percentEmp, fill = CoB)) +
  geom_boxplot() +
  #geom_boxplot(data = subset(topPercentYear, CoB==highlightCoB), aes(x = CoB, y = percentEmp, fill = CoB), lwd=2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0)) +
  scale_fill_manual(values = cols2) +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  guides(fill = F) +
  coord_cartesian(ylim = c(75,100))#edinburgh
  #coord_cartesian(ylim = c(60,100))#glasgow
  

output

# ggsave(paste0("R_outputs/3censusBoxplots/top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
# ggsave(paste0("R_outputs/3censusBoxplots/top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
# ggsave(paste0("R_outputs/3censusBoxplots/edinburgh_top_percent_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
# ggsave(paste0("R_outputs/3censusBoxplots/edinburgh_top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)

# ggsave(paste0("R_outputs/3censusBoxplots/",top,"glasgow_top_n_employment_boxplot",yr,"highlightCoB",highlightCoB,"_top_",top,"percent.png"),
#               output,width = 8,height = 4)
ggsave(paste0("R_outputs/3censusBoxplots/ordered1991/",top,"_",getCity,"_top_n_employment_boxplot",yr,"_top_",top,"percent.png"),
              output,width = 8,height = 4)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Three-census: CoB zone pop % vs EA: graphing over time------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So using same topPercent approach used above. Recreate here.

#Initally for all zones
city <- perZoneCoBprops_3censusLONG_EA
# city <- perZoneCoBprops_3censusLONG_EA %>% filter(name == "Glasgow")

topPercent <- city %>% 
  group_by(year,CoB) %>% 
  filter(percent > quantile(percent,prob=1-top/100))
#top_n(10,percent)

#Get median percent emply value and interquartiles
topPercent <- topPercent %>% 
  group_by(year,CoB) %>% 
  mutate(median = median(percentEmp), 
         bottomQtr = quantile(percentEmp)[2],
         topQtr = quantile(percentEmp)[4])

#CoB will have three unique median values, one for each census year
#Can use that to get SD of its change
topPercent <- topPercent %>% 
  group_by(CoB) %>% 
  mutate(sd = sd(unique(median)))

chk <- topPercent %>% arrange(year,CoB)

#What's SD look like?
#Two outliers. Wonder what?
hist(topPercent$sd)

#~~~~~~~~~~~~~~~~
dodge <- position_dodge(width=0.8)

#select subset
topPercentSub <- topPercent[topPercent$sd < 1.5,]
topPercentSub <- topPercent
topPercentSub <- topPercent[topPercent$sd > 4,]

#plot over time
output <- ggplot(topPercentSub, aes(x = year, y = median, colour = CoB, group = CoB)) +
  geom_line() +
  geom_errorbar(width = 0.1, 
                aes(ymin=bottomQtr, 
                    ymax=topQtr), 
                position = dodge) 
  
output
  
#Output those in groups that are easier to look at
CoBs <- unique(topPercent$CoB)
sds <- unique(topPercent$sd)

sds <- sds[order(sds)]

#That said... I think I probably need to work with the rank in each year and how that's changed

#~~~~~~~~~~~
#Try with rank
#For each census year, rank the median emply% for the top x% of the CoB group

#Only need to keep rank position for the medians
#not every zone value
rankz <- topPercent %>% 
  group_by(year,CoB) %>% 
  summarise(median = max(median)) %>% 
  group_by(year) %>% 
  mutate(rank = rank(desc(median)))

output <- ggplot(rankz, aes(x = year, y = rank, colour = CoB, group = CoB)) +
  geom_line()

output

chk <- rankz[order(rankz$rank),]

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

#save that for use later. Takes a long while to recreate through backtracking
saveRDS(perZoneCoBprops_5censusLONG_EA,"R_data/perZoneCoBprops_5censusLONG_EA.rds")

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Five-census: CoB zone pop % vs EA: graphing over time-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#city <- perZoneCoBprops_5censusLONG_EA %>% filter(name == "Edinburgh")
#city <- perZoneCoBprops_5censusLONG_EA %>% filter(name == "Glasgow")
#city <- perZoneCoBprops_5censusLONG_EA

#output for all TTWAs
#Or just a selection, actually
#for(ttwa in unique(perZoneCoBprops_5censusLONG_EA$name)) {
for(ttwa in c('Glasgow','Edinburgh','Aberdeen')) {
  
#Highlight all the different CoBs
  for(CoBz in unique(perZoneCoBprops_5censusLONG_EA$CoB)){
  
  city <-  perZoneCoBprops_5censusLONG_EA %>% filter(name == ttwa)
  #city <-  perZoneCoBprops_5censusLONG_EA %>% filter(name == "Fraserburgh")

  topPercent <- city %>% 
    group_by(year,CoB) %>% 
    filter(percent > quantile(percent,prob=1-top/100))
  #top_n(10,percent)
  
  #Get median percent emply value and interquartiles
  topPercent <- topPercent %>% 
    group_by(year,CoB) %>% 
    mutate(median = median(percentEmp), 
           bottomQtr = quantile(percentEmp)[2],
           topQtr = quantile(percentEmp)[4])
  
  #CoB will have three unique median values, one for each census year
  #Can use that to get SD of its change
  topPercent <- topPercent %>% 
    group_by(CoB) %>% 
    mutate(sd = sd(unique(median)))
  
  chk <- topPercent %>% arrange(year,CoB)
  
  #What's SD look like?
  #Two outliers. Wonder what?
  #hist(topPercent$sd)
  
  #RANK A TEE RANK RANK
  rankz <- topPercent %>% 
    group_by(year,CoB) %>% 
    summarise(median = max(median)) %>% 
    group_by(year) %>% 
    mutate(rank = rank(desc(median)))
  
  #order factor by rank position in the last year
  #Not working for all...
  try(
    rankz$CoB <- reorder(rankz$CoB[rankz$year == 2011], rankz$rank[rankz$year == 2011])
  )
  
  #print("badger")
  
  output <- ggplot(rankz, aes(x = year, y = rank, colour = CoB)) +
    scale_colour_manual(values=cbPalette) +
    geom_line(size = 1, alpha = 0.35) +
    geom_point(size = 2) + 
    geom_line(data = rankz %>% filter(CoB == CoBz), size = 1.8, colour = 'white') +
    geom_line(data = rankz %>% filter(CoB == CoBz), size = 1.5, colour = 'red',alpha = 0.75) +
    geom_point(data = rankz %>% filter(CoB == CoBz), size = 3) +
    scale_y_reverse() +
    ggtitle(paste0(ttwa,'/',CoBz)) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  output
  
  ggsave(
    paste0("R_outputs/5census_TTWA_employmentRanks_CoBTop20percent_ScotlandHighlight/",
           ttwa,"_",CoBz,".png"),
         output,width = 6,height = 4)
  
  }
  
}



chk <- rankz[order(rankz$rank),]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(NOT) Spatial variance-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reasons for not doing this:
#Variogram is gonna find all zero zones being close together. 
#Not sure that's what I'm after. 
#It's also going to pick up on correlations between cities - generally, I'm not sure I like it!

#For which we'll need the actual coordinates
# all3_wcoords <- cbind(data.frame(all3),coordinates(all3))
# 
# #eastings, northings
# plot(all3_wcoords$`1`,all3_wcoords$`2`)
# 
# #one year
# varz <- all3_wcoords %>% filter(year == 1991) %>% dplyr::select(-(1:2))
# 
# #I could use the function, or I could look at graphng *all* point-pair distances
# #That would quickly get to be a lot of data, mind...
# #But let's look.
# 
# CoB <- varz[,c(25,41:42)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Playing with spatial weights matrices: 3 census edition----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Assuming 3C CoB data already loaded.

#Save a couple of the dataframes I'm using before a clear-out
saveRDS(all3_df,"R_data/all3_df.rds")
saveRDS(all3_zoneProps,"R_data/all3_zoneProps.rds")
#And the mx processed below for quick grabbing
saveRDS(mx,"R_data/nearestNeighb_matrix.rds")
#CoB props are the wrong orientation but this has got econ-active/housing data in it
saveRDS(CoBprops3census,"R_data/CoBprops3census.rds")

all3_df <- readRDS("R_data/all3_df.rds")
all3_zoneProps <- readRDS("R_data/all3_zoneProps.rds")
mx <- readRDS("R_data/nearestNeighb_matrix.rds")
CoBprops3census <- readRDS("R_data/CoBprops3census.rds")


#Get 8-nearest-neighbour contiguity matrix (normalised so we can get averages for nearby)
mx <- as.matrix(
  read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_8nearestNeighb.csv")
)
#16 nn
# mx <- as.matrix(
#   read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_16nearestNeighb.csv")
# )
# 
#Using all3_df that's got em all in. Pick the 1st year.



#Not funny. Not.
clusterlook <- all3_df %>% filter(year == 1991)

#check rows are in right order
table(mx[,1] == clusterlook$label)

#Proportions in each zone
clusterlook <- prop.table(as.matrix(clusterlook[,c(3:41)]), margin = 1) * 100
clusterlook <- data.frame(clusterlook)

#Check
table(apply(clusterlook,1,sum))

#drop name row from matrix (don't need to keep names in row, we know it's in right order)
mx <- mx[,-1]
#Oh! Why was it character? Why would it load like that?
apply(mx,1,class)

mx <- as.numeric(mx)
mx <- matrix(mx, nrow = 822, ncol = 822)
table(apply(mx,1,class))

#Check it's oriented correctly (since nearest-neighbours are not symmetrical)
#Currently eight nearest neighbours, row-normalised, so the 0.125s should sum to 1
#across rows
table(apply(mx,1,sum))

#And multiplying by an all 1s vector should produce the same result...
mx %*% rep(1,822) %>% table

#I think this is "which zones have me as neighbour"
#Which with contiguity would be the same.
#And yes, it's different. So first version has correct orientation.
mxt <- t(mx)
table(apply(mxt,1,sum))
mxt %*% rep(1,822) %>% table

#Get average of nearby zones via weights matrix
#Works now after faffy numeric conversion. 
#This? http://stackoverflow.com/questions/16518428/right-way-to-convert-data-frame-to-a-numeric-matrix-when-df-also-contains-strin
#weightsMatrix <- mx %*% as.matrix(clusterlook$England)
#weightsMatrix <- mx %*% seq(1:822)

#weightsMatrix <- mx %*% clusterlook$England

#plot(weightsMatrix,clusterlook$England)
#Well that's startlingly linear!
#plot(log(weightsMatrix),log(clusterlook$England))
#cor(log(weightsMatrix),log(clusterlook$England))

#We want to plot deviations from the mean
#plot(weightsMatrix - mean(weightsMatrix),clusterlook$England - mean(clusterlook$England))

#Run over all CoBs
for(val in seq(1:39)){
  weightsMatrix <- mx %*% clusterlook[,val]
#   weightz <- log(weightsMatrix)
#   eng <- log(clusterlook[,val])
  weightz <- (weightsMatrix)
  eng <- (clusterlook[,val])
  
  #plot(weightsMatrix - mean(weightsMatrix),clusterlook$England - mean(clusterlook$England))
  #plot(weightz - mean(weightz),eng - mean(eng))
  
  jpeg(paste0('R_outputs/testSpatialWeights/16nearestNeighbours/',val,'.jpg'))
  #plot(weightz - mean(weightz),eng - mean(eng))
  plot(weightz,eng,main = paste0(names(clusterlook[val]),':',cor(weightz,eng) ))
  dev.off()
  
  jpeg(paste0('R_outputs/testSpatialWeights/16nearestNeighbours/log/',val,'.jpg'))
  plot(log(weightz),log(eng),main = paste0(names(clusterlook[val]),':',cor(weightz,eng) ))
  dev.off()
  
}

#check random pairings of CoBs to see what the patterns look like generally
for(val in seq(1:500)){
  
  #Avoids Scotland
  weightsSampleNum <- sample(c(1:13,15:39),1)
  CoB_sampleNum <- sample(c(1:13,15:39),1)
  #CoB_sampleNum <- 14#Scotland
  
  weightsMatrix <- mx %*% clusterlook[,weightsSampleNum]
  #   weightz <- log(weightsMatrix)
  #   eng <- log(clusterlook[,val])
  weightz <- (weightsMatrix)
  #Random 
  eng <- (clusterlook[,CoB_sampleNum])
  
  #plot(weightsMatrix - mean(weightsMatrix),clusterlook$England - mean(clusterlook$England))
  #plot(weightz - mean(weightz),eng - mean(eng))
  
#   jpeg(paste0('R_outputs/testSpatialWeights/8nearestNeighbours/',val,'.jpg'))
#   #plot(weightz - mean(weightz),eng - mean(eng))
#   plot(weightz,eng,main = paste0(names(clusterlook[val]),':',cor(weightz,eng) ))
#   dev.off()
  
  jpeg(paste0('R_outputs/testSpatialWeights/8nearestNeighbours/randomCoBsLog/',val,'.jpg'))
  plot(log(weightz),log(eng),main = 
         paste0('Neighb av:',names(clusterlook[weightsSampleNum]),' vs pcs:',names(clusterlook[CoB_sampleNum]),
                ':',cor(weightz,eng) ))
  dev.off()
  
}

#US vs Canada: huh?
weightsMatrix <- mx %*% clusterlook[,38]
weightz <- (weightsMatrix)
pcs <- (clusterlook[,18])

plot(log10(weightz),log10(pcs),main = 
       paste0('Neighb av:',names(clusterlook[38]),' vs pcs:',names(clusterlook[18]),
              ':',cor(weightz,pcs) ))

plot((weightz),(pcs),main = 
       paste0('Neighb av:',names(clusterlook[38]),' vs pcs:',names(clusterlook[18]),
              ':',cor(weightz,pcs) ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test Geoff Meen regression on single set: 3-census----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Predicting: the share of migrants from CoB i in zone j at time t 
#latest census, or prev if we're looking at how it changes) is a function of:
#From: 
#The same share in a previous time period, in the same zones
#The weighted matrix - 
#i.e. the averaged values of surrounding areas for the same migrant group - in a previous time period
#Also the sum of all other migrant groups from that previous time period.

#Leaving out the various housing/labour market vars for the moment

#So into one dataset, column for each
#Picking one CoB to start with

#Is CoBprops3census proportions per zone?
#Newp! Needs redoing
#apply(CoBprops3census[,c(1:39)], 2, sum)

all3_zoneProps <-  prop.table(as.matrix(all3_df[,c(3:41)]), margin = 1) * 100

all3_zoneProps <- data.frame(all3_zoneProps)

#Add year and zone back in
all3_zoneProps <- cbind(all3_zoneProps,all3_df[,c(1,42)])

#check
apply(all3_zoneProps[,c(1:39)], 1, sum)

#Get spatial weights for all CoBs
mx <- as.matrix(
  read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_8nearestNeighb.csv")
)

#Queen contiguity (so we'll lose the Islands but I'm not sure that matters too much for this)
# mx <- as.matrix(
#   read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeightsQueenContig.csv")
# )

#Convert to numeric matrix
#No: this is wrong way round
#Important note: if using nearest neighbours, not continguity, it won't be symmetric
#mx <- apply(mx[,c(2:ncol(mx))],1,as.numeric)

mx <- mx[,-1]
mx <- as.numeric(mx)
mx <- matrix(mx, nrow = 822, ncol = 822)
table(apply(mx,1,class))

#weightsMatrix <- mx %*% clusterlook[,val]

#Split! Returns list of dfs! Split into 3 censuses
#http://stackoverflow.com/questions/9713294/split-data-frame-based-on-levels-of-a-factor-into-new-data-frames
#Add resulting columns to this...
weightsRez <- all3_zoneProps[,c(40,41)]

#test <- mx %*% t(as.matrix(all3_zoneProps[,1:39]))

for(cobnum in seq(1:39)){

  #Will get resulting neighbour averages for this CoB for all 3 years
  weightsMatrix <- lapply(split(all3_zoneProps, all3_zoneProps$year), 
                          function(x) mx %*% x[,c(cobnum)])
  
  #Fastest apparently
  #http://stackoverflow.com/questions/2851327/converting-a-list-of-data-frames-into-one-data-frame-in-r
  weightsRez <- cbind(weightsRez,do.call(rbind,weightsMatrix))
  
}

#rename cols to CoBs again
names(weightsRez) <- c('label','year',names(all3_zoneProps[1:39]))

#So in theory, that's all the neighbour averages found and stuck in weightsRez
#For all three years

#Get some images for correlations out
#Current CoB vs previous census CoB (one of them)
#vs current weights vs previous weights

#Run over all CoBs
#test
test <- names(all3_zoneProps[,c(1:39)])
val <- test[25]

for(val in names(all3_zoneProps[,c(1:39)])){
  
  #assemble into columns
  #This year's CoB is dependent on... 
  colz <- all3_zoneProps %>% filter(year == 2011) %>% dplyr::select_(val) 
  
  #Last census's
  colz <- cbind(colz, all3_zoneProps %>% filter(year == 2001) %>% dplyr::select_(val) )
  colz <- cbind(colz, all3_zoneProps %>% filter(year == 1991) %>% dplyr::select_(val) )
  
  #And let's look at how it correlates to both present and lagged weights
  colz <- cbind(colz, weightsRez %>% filter(year == 2011) %>% dplyr::select_(val))
  colz <- cbind(colz, weightsRez %>% filter(year == 2001) %>% dplyr::select_(val))
  colz <- cbind(colz, weightsRez %>% filter(year == 1991) %>% dplyr::select_(val))
  
  names(colz) <- c('2011','2001','1991','11_nn','01_nn','91_nn')
  #names(colz) <- c('2011','2001','1991','11_nn','01_nn','91_nn')
  
  #pairs(log10(colz[,c(1,2,4,5)]))
  #pairs((colz[,c(1,2,4,5)]))
  
  #jpeg(paste0('R_outputs/testSpatialWeights/pairs_queenContig/91-01/',val,'.jpg'), width = 800, height = 800)
  jpeg(paste0('R_outputs/testSpatialWeights/pairs_queenContig/01-11/',val,'.jpg'), width = 800, height = 800)
  #plot(weightz - mean(weightz),eng - mean(eng))
  # pairs(log(colz[,c(1,2,4,5)]), main = val)
  
  #Only more than 1> in those zones
  #pairs(log(colz[colz[,c(1:3)] > 1 ,c(2,3,5,6)]), main = val,
  #Or all
  #Removed log values - regression line can't deal with the NaNs that result
  #(And there are likely quite a few zeroes)
  
  #pairs((colz[,c(2,3,5,6)]), main = val,
  
  #01-11
  pairs((colz[,c(1,2,4,5)]), main = val,
  #91-01
  #pairs((colz[,c(2,3,5,6)]), main = val,
        panel = function(x,y) {
          points(x,y)
          #abline(lm(y~x))
          abline(a = 0, b = 1, col='red')
        })
  
  #window.options(width = 800, height = 800)
  
  #Try cutting out low percents again. Or I could keep x highest values?
  #Problem being, that list will change between censuses
  #but could do relative to one particular census

  
  #pairs(log(colz[,c(2,3,5,6)]), main = val)
  #plot(weightz,eng,main = paste0(names(clusterlook[val]),':',cor(weightz,eng) ))
  dev.off()
  
  # jpeg(paste0('R_outputs/testSpatialWeights/pairs/log/',val,'.jpg'), width = 800, height = 800)
  # 
  # 
  # #No regression line! Line of identity
  # #pairs(log(colz[,c(2,3,5,6)]), main = val,
  # pairs(log(colz[,c(1,2,4,5)]), main = val,
  #       panel = function(x,y) {
  #         points(x,y)
  #         abline(a = 0, b = 1, col='red')
  #       }
  # )
  # 
  # dev.off()
  #   jpeg(paste0('R_outputs/testSpatialWeights/pairs/log/',val,'.jpg'))
#   plot(log(weightz),log(eng),main = paste0(names(clusterlook[val]),':',cor(weightz,eng) ))
#   dev.off()
   
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Misc messinz----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Just look at 91/01 weights matrices for...

namez <- names(all3_zoneProps[,c(1:39)])
#val <- namez[11]#Europe-other
val <- namez[24]#India
#val <- namez[7]#Caribbean
#val <- namez[31]#Greece

#val <- namez[39]#Iran

#assemble into columns
#This year's CoB is dependent on... 
colz <- all3_zoneProps %>% filter(year == 2011) %>% dplyr::select_(val) 

#Last census's
colz <- cbind(colz, all3_zoneProps %>% filter(year == 2001) %>% dplyr::select_(val) )
colz <- cbind(colz, all3_zoneProps %>% filter(year == 1991) %>% dplyr::select_(val) )

#And let's look at how it correlates to both present and lagged weights
colz <- cbind(colz, weightsRez %>% filter(year == 2011) %>% dplyr::select_(val))
colz <- cbind(colz, weightsRez %>% filter(year == 2001) %>% dplyr::select_(val))
colz <- cbind(colz, weightsRez %>% filter(year == 1991) %>% dplyr::select_(val))

#names(colz) <- c('2011','2001','1991','11_nn','01_nn','91_nn')
names(colz) <- c('twentyEleven','twoThousandOne','nineteenNinetyOne','eleven_nn','ohOne_nn','ninetyOne_nn')

#Try running an actual regression on some of this. So...
#Share of migrants in zone i from CoB j at time t is the dependent
#regz <- lm(colz$`2011`~colz$`2001`+colz$`01_nn`)

#regz <- lm(twentyEleven ~ twoThousandOne + ohOne_nn, data = colz)
#Set intercept to zero
#regz <- lm(twentyEleven ~ 0 + twoThousandOne + ohOne_nn, data = colz)
#summary(regz)
#plot(regz)

#Weight vs weight
plot(colz[,c(6,5)], main = val)
#2011 vs 01nn
plot(colz[,c(5,1)], main = val)
#01 vs 91
plot(log10(colz[colz[,c(3,2)]>0.5,c(3,2)]), main = val)
plot(log(colz[colz[,c(3,2)]>0.5,c(3,2)]), main = val)
plot((colz[colz[,c(3,2)]>0.5,c(3,2)]), main = val)

#abline(a = -0.06667, b = 1.32, col='red')
abline(a = 0, b = 1.15, col='red')
abline(a = 0, b = 1, col='green')
#lines(lowess(colz[,c(6,5)]))

#identify(colz$`1991`, colz$`2001`, row.names(colz))
identify(colz$`91_nn`, colz$`01_nn`, row.names(colz))

pairs((colz[,c(2,3,5,6)]), main = val,
      panel = function(x,y) {
        points(x,y)
        #abline(lm(y~x))
        abline(a = 0, b = 1, col='red')
      })

#Iranian cluster in Glasgow
#all3_df$label[534]
#all3_df$label[c(509,510,535,536,537,538,539,540,729)]
#'6341AS','6341AT','6341BW','6341BX','6341BY','6341BZ','6341CA','6341CB','6350AF'

#Europe-other: the cluster in 91 that seemed to vanish in 01. Where?
#all3_df$label[c(266,267,275,277,820,821)]
#'6125AR','6125AS','6127AA','6127AC','6756AH','6756AJ'

#Let's try that with ggplot
#marking four quadrants split along the median
#so we got half of total zones above and below

#Save Europe other, have a look in the map...
write.csv(cbind(threeCensus91@data$label,colz),"C:/Data/Census/QGIS/zonePercents/Caribbean_3census.csv",row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Mantel test play----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Let's just look at a Mantel test type approach. I'm curious.
#Distance between data vs physical distance

#This has correct coords!
iz <- readOGR(dsn="C:/Data/Census/StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth", 
              layer="1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch")

#Is this finding the centroids? Via
#https://cran.r-project.org/web/packages/spdep/vignettes/nb_igraph.html
coords <- coordinates(iz)
geogDists <- spDists(coords)

#And 'distances' for some value - let's pick on India, 2001 nearest neighb av
#This is different: we need a full matrix of "distances" from a single value
#for all other values. 
#Full range of values, then the single value...
valDists <- sapply(colz[,5], function(x) abs(colz[,5] - x))

hist(valDists)

#Seems to be the right number of points, though whether they're in the right order...
#Well that was fairly useless!
#oh well, yes, obviously: a lot of zero values will be next to each other
#Doesn't distinguish high from low
plot(geogDists,valDists)

#Test summary is working...
df <- data.frame(x = seq(1:1000), y = seq(1:1000), z = runif(1000,1,1000))
model <- lm(y ~ x+z, data = df)
model
summary(model)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Testing movement to wealthier areas: India----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Wanting to see for the India 01/91 nearest neighbour av change
#Whether the drop in large zone % / gain in smaller ones
#indicates movement to e.g. wealthier areas
#So some colour labelling

#Use econ stats attached to cobprops 3 census
#Double-check they're in the right zone order
(all3_zoneProps$label == CoBprops3census$label) %>% table

#Good, can just attach cols then (broken down by year correctly)
#Keep just economic vars
econVars3census <- CoBprops3census %>% dplyr::select(year,meanPrice,percentEmp)

#Separate column for each year
#Would appear it's not set up for multiple cols
#Aaaand an error! OK, doing manually...
# econVars3census2 <- cbind(
#   econVars3census %>% spread(meanPrice,year),
#   econVars3census %>% spread(year,percentEmp)
# )

#Same order as other data, reverse date
econVars3census2 <- do.call(cbind, c(
  econVars3census %>% filter(year == 2011) %>% dplyr::select(meanPrice,percentEmp),
  econVars3census %>% filter(year == 2001) %>% dplyr::select(meanPrice,percentEmp),
  econVars3census %>% filter(year == 1991) %>% dplyr::select(meanPrice,percentEmp)
)) %>% data.frame

# econVars3census2 <- do.call(cbind, c(
#   econVars3census %>% filter(year == 2011),
#   econVars3census %>% filter(year == 2001),
#   econVars3census %>% filter(year == 1991)
# ))

names(econVars3census2) <- c('meanPrice_11','percentEmp_11',
                             'meanPrice_01','percentEmp_01',
                             'meanPrice_91','percentEmp_91')

#check we got houses in the right order
apply(econVars3census2,2,summary)

#save for mapping n shizzle
write.csv(cbind(all3_df$label,econVars3census2),"R_data/econVars3Census.csv", row.names = F)

#Set up for graphinz
val <- namez[24]#India
#val <- namez[39]#Iran

#assemble into columns
#This year's CoB is dependent on... 
colz <- all3_zoneProps %>% filter(year == 2011) %>% dplyr::select_(val) 

#Last census's
colz <- cbind(colz, all3_zoneProps %>% filter(year == 2001) %>% dplyr::select_(val) )
colz <- cbind(colz, all3_zoneProps %>% filter(year == 1991) %>% dplyr::select_(val) )

#And let's look at how it correlates to both present and lagged weights
colz <- cbind(colz, weightsRez %>% filter(year == 2011) %>% dplyr::select_(val))
colz <- cbind(colz, weightsRez %>% filter(year == 2001) %>% dplyr::select_(val))
colz <- cbind(colz, weightsRez %>% filter(year == 1991) %>% dplyr::select_(val))

#names(colz) <- c('2011','2001','1991','11_nn','01_nn','91_nn')
names(colz) <- c('twentyEleven','twoThousandOne','nineteenNinetyOne','eleven_nn','ohOne_nn','ninetyOne_nn')

#attach econ data
colz_econz <- cbind(colz,econVars3census2)


#Weight vs weight vs %employment '01
plot(colz_econz[,c(6,5)], main = val, col = ifelse(colz_econz[,10] < median(colz_econz[,10]),'red','green'))
#Weight vs weight vs %employment '91
plot(colz_econz[,c(6,5)], main = val, col = ifelse(colz_econz[,12] < median(colz_econz[,12]),'red','green'))
#Weight vs weight vs %house price mean '91
plot(colz_econz[,c(6,5)], main = val, col = ifelse(colz_econz[,9] < median(colz_econz[,9]),'red','green'))
abline(a = 0, b = 1, col='green')

plot(colz_econz[,c(10,9)], main = val)

#Where are those in the larger % groups?
identify(colz_econz$ninetyOne_nn, colz_econz$ohOne_nn, row.names(colz_econz))

all3_df$label[c(505,506,507,508,509,517,518,529,530,566,567,568,580)]

labelsCommaz <- gsub(" ","','","6341AA 6341AP 6341AQ 6341AR 6341AS 6341BB 6341BC 6341BP 6341BQ 6341DF 6341DG 6341DH 6341DX")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Comparing to zone total pop----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Cos some zones with tiny pops.. not very relevant?
#Checking again: yup, 100% in each zone
table(apply(all3_zoneProps[,c(1:39)],1,sum))

#sum pop per zone. Err. From?
all3_zoneProps$totpop <- apply(all3_df[,c(3:41)],1,sum)

hist(all3_zoneProps$totpop, breaks = 25)

totpop <- all3_zoneProps[,c(40,42)]
totpop <- totpop[order(totpop$totpop),]

#Actually, the important thing is population per area amount of course.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Use a range of NN weights matrices to get spatial decay of nearest-neighbour proportions----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all3_zoneProps <-  prop.table(as.matrix(all3_df[,c(3:41)]), margin = 1) * 100

all3_zoneProps <- data.frame(all3_zoneProps)

#Add year and zone back in
all3_zoneProps <- cbind(all3_zoneProps,all3_df[,c(1,42)])

#check
apply(all3_zoneProps[,c(1:39)], 1, sum)

#Get all NN spatial weights matrices for all CoBs
filez = list.files(path="StitchOutputs/Scotland/other/nearestNeighbour_range", 
                   pattern = "*.csv",full.names = T)

#Note: I forgot I'd done the matrix conversion here. It took a long time to track down!
matrices = lapply(filez, function(x) as.matrix(read.csv(x)))

#Convert to numeric matrix
#No: this is wrong way round
#Important note: if using nearest neighbours, not continguity, it won't be symmetric
#mx <- apply(mx[,c(2:ncol(mx))],1,as.numeric)
makeNumeric <- function(mxx){
  
  mxx <- mxx[,-1]
  mxx <- as.numeric(mxx)
  mxx <- matrix(mxx, nrow = 822, ncol = 822)
  #print(class(mxx))
  
}

matrices <- lapply(matrices, function(x) makeNumeric(x))




listOfWeightsResults <- list()

for(i in seq(1:length(matrices))){

  #Split! Returns list of dfs! Split into 3 censuses
  #http://stackoverflow.com/questions/9713294/split-data-frame-based-on-levels-of-a-factor-into-new-data-frames
  #Add resulting columns to this...
  weightsRez <- all3_zoneProps[,c(40,41)]
  
  #test <- mx %*% t(as.matrix(all3_zoneProps[,1:39]))
  
  for(cobnum in seq(1:39)){
    
    #Will get resulting neighbour averages for this CoB for all 3 years
    weightsMatrix <- lapply(split(all3_zoneProps, all3_zoneProps$year), 
                            function(x) matrices[[i]] %*% x[,c(cobnum)])
    
    #Fastest apparently
    #http://stackoverflow.com/questions/2851327/converting-a-list-of-data-frames-into-one-data-frame-in-r
    weightsRez <- cbind(weightsRez,do.call(rbind,weightsMatrix))
    
  }
  
  #rename cols to CoBs again
  names(weightsRez) <- c('label','year',names(all3_zoneProps[1:39]))
  
  listOfWeightsResults[[i]] <- weightsRez

}

#So now got five different weight results worked out i.e. for all CoBs, av NN values over those five.
#In this order in the list listOfWeightsResults:
filez

#So now if I want to look at values for a particular CoB
#In a particular year?
lookz <- listOfWeightsResults[[1]]

#Assemble one for testing
val <- namez[24]#India
#val <- namez[39]#Iran

yr <- 2011

#assemble into columns
#This year's CoB is dependent on... 
nn_bins <- all3_zoneProps %>% filter(year == yr) %>% dplyr::select_(val) 

#Get each nn band
for(i in seq(1:length(listOfWeightsResults))){
  
  nn_bins <- cbind(nn_bins, listOfWeightsResults[[i]] %>% filter(year == yr) %>% dplyr::select_(val))

}

#Long in correct bins before plotting
names(nn_bins) <- c('zone%','4nn','8nn','16nn','20nn','24nn')

#Need labels
nn_bins$label <- all3_df$label[1:822]

nn_bins_long <- gather(nn_bins,source,percent,1:6)

output <- ggplot(nn_bins_long, aes(x = source, y = percent, group = label, colour = label)) +
  geom_line() +
  guides(colour = F)

output

#And normalised to the zone%
#(Leaving zone% in for reference)
nn_bins_norm <- cbind(nn_bins[,c(1,7)],apply(nn_bins[,c(1:6)],c(2),function(x) nn_bins[,1]/x))

nn_bins_long_norm <- gather(nn_bins_norm[,c(2:8)],source,percent,2:7)

output <- ggplot(nn_bins_long_norm, aes(x = source, y = percent, group = label, colour = label)) +
  geom_line() +
  guides(colour = F)

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Look at eViewsReady estimation file----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

eViewsFile <- read_csv('R_data/estimation.csv')

#Check some correlations - weights matrices looking plausible?
plot(log(eViewsFile$xij1991),log(eViewsFile$w91q))
plot(log(eViewsFile$w91nn8),log(eViewsFile$w91q))
plot((eViewsFile$w91nn8),(eViewsFile$w91q))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1991 shares cf 2011 change----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#From CoB combo file - get shares and raw numbers
cob <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/countryOfBirth.csv')

#Bits of code nabbed from eViewsReady.R
cob$Population <- apply(cob[,c(4:42)],1,sum)

cobshares <- cob %>% 
  dplyr::select(4:44) %>% 
  group_by(censusYear) %>% 
  mutate_each(  funs(  ((.)/sum(.))*100  ) )











