#Reshape stitched data into a form ready for putting into eviews
library(dplyr)
library(readr)
library(reshape2)
library(tidyr)

#We only have back to 1990, so can only cover 3 censuses
geolibs <- c("spdep","dplyr", "tidyr","assertthat","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, require, character.only = TRUE)

#~~~~THREE-CENSUS~~~~----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 census eviews prep----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load all the joined data
cob <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/countryOfBirth.csv')
dwellings <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/dwellings.csv')
ea <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/econActive.csv')
prices <- read_csv('StitchOutputs/Scotland/LBS_3censusCombinedData/housePrices.csv')

#Start making into eviews-ready shape
#First-up: across-zone proportions for CoB categories.

#Add column for population total before finding proportions
#Do this before subsetting: we want proportions across zones
#For the WHOLE population
cob$Population <- apply(cob[,c(4:42)],1,sum)

#Proportions per year
cobprops <- cob %>% 
  dplyr::select(4:44) %>% 
  group_by(censusYear) %>% 
  mutate_each(  funs(  ((.)/sum(.))*100  ) )

#Did that work? Yup!
apply(cobprops[cobprops$censusYear==2011,c(1:41)],2,sum)

#CoB long, all other variables repeated for each.
#Old-school df behaviour should let me repeat each vector

#Geoff's subset
subz <- c(
  'Irish_Repu',
  'China',
  'South_Afri',
  'Africa_oth',
  'South_Amer',
  'Other_Midd',
  'Other_East',
  'Europe_oth',
  'Australia',
  'Canada',
  'Nigeria',
  'India',
  'Pakistan',
  'Hong_Kong',
  'Malaysia',
  'France',
  'Germany',
  'Italy',
  'Netherland',
  'Spain',
  'Poland',
  'United_Sta'
)

#guess at rich/poor split
richpoor <- data.frame(subz,rich = c(1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,1,1,1,0,1))

#https://gist.github.com/djhocking/62c76e63543ba9e94ebe
cobprops2 <- cobprops %>% 
  dplyr::select_(.dots = c(subz,'Population'))

#Err. How did that keep the census year? Because of the previous group by?

#make em long
cobpropslong91 <- cobprops2 %>%
  filter(censusYear == 1991) %>% 
  gather(cob,share,c(2:24))

cobpropslong11 <- cobprops2 %>%
  filter(censusYear == 2011) %>% 
  gather(cob,share,c(2:24))

#Population at the end will actually need repeating 
#in its own column to match the eviews file
#Just selecting 882 / a year's worth of labels 
#to make sure the repeating vector fits
eViewsFile <- do.call(cbind,list(cob$label[cob$censusYear==1991],cobpropslong11$share,cobpropslong91$share)) %>% data.frame()

#Population is the last 822. Remove.
eViewsFile <- eViewsFile[1:18084,]

#Got the right number at the end
nrow(eViewsFile) %% 822

#~~~~~~~~~~~~~~~
#ADD IN OTHER COLUMNS AS REPEATING VALUES----

#Add the population shares in as its own repeating column
eViewsFile$popsh91 <- cobprops2$Population[cobprops2$censusYear==1991]
eViewsFile$popsh01 <- cobprops2$Population[cobprops2$censusYear==2001]

eViewsFile$ph91 <- prices$meanPrice[prices$censusYear==1991]
eViewsFile$ph01 <- prices$meanPrice[prices$censusYear==2001]

#Right number, right place
table(eViewsFile$X1[eViewsFile$ph91==26345])

#Total pop in 91
eViewsFile$pop91 <- cob$Population[cob$censusYear==1991]

#Dwelling count
eViewsFile$hs91 <- dwellings$totalDwell[dwellings$censusYear==1991]

#crowding via the last two: people per dwelling
eViewsFile$pophs91 <- eViewsFile$pop91/eViewsFile$hs91

#Total pop in01
eViewsFile$pop01 <- cob$Population[cob$censusYear==2001]

#Dwelling count
eViewsFile$hs01 <- dwellings$totalDwell[dwellings$censusYear==2001]

#crowding via the last two: people per dwelling
eViewsFile$pophs01 <- eViewsFile$pop01/eViewsFile$hs01



#~~~~~~~~~~~
#Migrant share is actually per zone, not across zones
#And it's "non-UK born" as a percent of the population in that zone

#Can put the original in
#Then another that removes the migrant group under question
#Use original CoB with pop
migshare91 <- cob %>% filter(censusYear==1991)

migshare91$migshare <- ((migshare91$Population - 
  (migshare91$Channel_Is + migshare91$UK_part_no + migshare91$England + migshare91$Wales + migshare91$Northern_I + migshare91$Scotland))/migshare91$Population)*100

#repeat that into the eViews sheet
eViewsFile$migsh91 <- migshare91$migshare

#Right number, right place
table(eViewsFile$migsh91)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Then rather more faff: migrant share not counting own group----

#Make a new dataset with sum of all UK folk plus population.
#Then can make each other CoB long and do subtractions.
migshare91minusOwn <- cob %>% filter(censusYear==1991)

migshare91minusOwn$UK <- (migshare91$Channel_Is + migshare91$UK_part_no + migshare91$England + migshare91$Wales + migshare91$Northern_I + migshare91$Scotland)

long1991CoB <- migshare91minusOwn[,c(4:42)] %>% 
  gather(CoB,count)

#Set values to zero if migrant group is within UK - already counted above
#Currently unnecessary as it doesn't include and UK groups, but...
long1991CoB$count[long1991CoB$CoB 
                  %in% c('Channel_Is','UK_part_no','England','Scotland','Wales','Northern_I')] <- 0

#Add in UK and pop values
long1991CoB$UK <- migshare91minusOwn$UK
long1991CoB$Population <- migshare91minusOwn$Population

#Repeated for each CoB? Yup.
table(long1991CoB$UK)
table(long1991CoB$Population)

long1991CoB$migshareMinusOwn <- ((long1991CoB$Population - (long1991CoB$count + long1991CoB$UK))/
                                   long1991CoB$Population) * 100

#Keep only those CoB being used
long1991CoBFinal <- long1991CoB %>% filter(CoB %in% subz)

#Attach to eViews-ready, now the right length
eViewsFile$migsh91minusown <- long1991CoBFinal$migshareMinusOwn

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add in 1991 and 2000 Economically active----
eViewsFile$ea91 <- ea$percentEmp[ea$censusYear==1991]
eViewsFile$ea01 <- ea$percentEmp[ea$censusYear==2001]

table(eViewsFile$ea91)
table(eViewsFile$ea01)

names(eViewsFile)[1:3] <- c('code','xij2011','xij1991')

eViewsFile$lph91 <- log(eViewsFile$ph91)
eViewsFile$lpophs91 <- log(eViewsFile$pophs91)
eViewsFile$lph01 <- log(eViewsFile$ph01)
eViewsFile$lpophs01 <- log(eViewsFile$pophs01)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add weight matrix results----
#Done in miscDataReshaping
mx <- as.matrix(
  read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_8nearestNeighb.csv")
)

mxq <- as.matrix(
  read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeightsQueenContig.csv")
)

#Remove first columns
mx <- mx[,-1]
mxq <- mxq[,-1]
dim(mxq)

#Oh! Why was it character? Why would it load like that?
apply(mx,1,class)

mx <- as.numeric(mx)
mx <- matrix(mx, nrow = 822, ncol = 822)
table(apply(mx,1,class))

mxq <- as.numeric(mxq)
mxq <- matrix(mxq, nrow = 822, ncol = 822)
table(apply(mxq,1,class))

#Check it's oriented correctly (since nearest-neighbours are not symmetrical)
#Currently eight nearest neighbours, row-normalised, so the 0.125s should sum to 1
#across rows
table(apply(mx,1,sum))

#And multiplying by an all 1s vector should produce the same result...
mx %*% rep(1,822) %>% table

#~~~~~~~~~~~~~~
#Weights matrix for each CoB
#For the shorter list
weightsMatrix <- matrix(nrow = 822,ncol = length(subz))

for(val in seq(1:length(subz))){
  
  weightsMatrix[,val] <- mx %*% as.matrix(cobprops2[cobprops2$censusYear==1991,(val+1)])
  
}
  
#Keep only those we're using. These, in theory, should all still be in the correct order.
#But actually we need the index of those columns
#indz <- sapply(subz,function(x) grep(x,names(cob)))
#Actually, using cobprops, not necessary to subset.
#Which need 3 subtracting from them to match weights index
#weightsToKeep <- weightsMatrix[,(as.numeric(indz)-3)]

#And they should all be in the right order for melting and sticking in the thingyo.
eViewsFile$w91nn8 <- weightsMatrix %>% data.frame() %>% 
  gather(cob,value) %>%
  dplyr::select(value) %>% 
  unlist(use.names = F)

#Repeat for queen contig
weightsMatrix <- matrix(nrow = 822,ncol = length(subz))

for(val in seq(1:length(subz))){
  
  weightsMatrix[,val] <- mxq %*% as.matrix(cobprops2[cobprops2$censusYear==1991,(val+1)])
  
}

#Keep only those we're using. These, in theory, should all still be in the correct order.
#But actually we need the index of those columns
#indz <- sapply(subz,function(x) grep(x,names(cob)))
#Actually, using cobprops, not necessary to subset.
#Which need 3 subtracting from them to match weights index
#weightsToKeep <- weightsMatrix[,(as.numeric(indz)-3)]

#And they should all be in the right order for melting and sticking in the thingyo.
eViewsFile$w91q <- weightsMatrix %>% data.frame() %>% 
  gather(cob,value) %>%
  dplyr::select(value) %>% 
  unlist(use.names = F)

#Add in a CoB column just in case huh?
eViewsFile$CoB <- long1991CoBFinal$CoB

#Save!
write_csv(eViewsFile,'R_data/estimation.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#For employment data: aggregate to larger geographies----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Cos it doesn't make much sense at the PCS level as anything but a proxy for wealth
#So use regional aggregate values
#Majority area would probably be better but this should be very close to that
#For everything but very oddly shaped PCSs

#Load shapefiles we need
pcs <- readOGR(dsn = 'C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount', layer = 'pseudoPCS_aggregated4CorrectCount')

#A couple of geographies to aggregate to
ttwa <- readOGR(dsn = 'C:/Data/MapPolygons/Scotland/2001/Scotland_ttwa_2001', layer = 'scotland_ttwa_2001_dissolvedByName')

nuts3 <- readOGR(dsn = 'C:/Data/MapPolygons/Scotland/NUTS3_2008', layer = 'scotland_nuts3_2008')

#Just assume centroids of PCS are good enough to indicate which zones they're in
pcs_centroids <- data.frame(coordinates(pcs))
coordinates(pcs_centroids) <- ~X1+X2
proj4string(pcs_centroids) <- proj4string(ttwa)
plot(pcs_centroids)

ttwaThisPCSisIn <- pcs_centroids %over% ttwa
NUTS3thisPCSisIn <- pcs_centroids %over% nuts3

#dataframe for merging into the eViewsFile
#Needs to not change the order - could just repeat, right?
pcs_zones <- data.frame(pcs = pcs@data$label,ttwa = ttwaThisPCSisIn$name,nuts3 = NUTS3thisPCSisIn$name)

#Now use those for aggregating employment data
#(is in correct order still)
#pcs_zones$ea91 <- ea$percentEmp[ea$censusYear==1991]

#Can now average percent employment per zone
#Actually, that's the wrong thing to do.
#I think I need to find that percent again from the raw figures
#To minimise MAUP issues. SO.
pcs_zones <- cbind(pcs_zones, ea[ea$censusYear==1991,c("EA","Unempl")])

#Now, aggregate both of those values per zone we're targeting
pcs_zones <- pcs_zones %>% group_by(ttwa) %>% 
  mutate(ttwaTotEA = sum(EA), ttwaTotUnempl = sum(Unempl))

#Repeat for NUTS3
pcs_zones <- pcs_zones %>% group_by(nuts3) %>% 
  mutate(nuts3TotEA = sum(EA), nuts3TotUnempl = sum(Unempl))

#Percent employment for both
pcs_zones$ea_ttwa91 <- 
  ((pcs_zones$ttwaTotEA - pcs_zones$ttwaTotUnempl)/pcs_zones$ttwaTotEA)*100

pcs_zones$ea_nuts3_91 <- 
  ((pcs_zones$nuts3TotEA - pcs_zones$nuts3TotUnempl)/pcs_zones$nuts3TotEA)*100

#add to previously loaded eViewsFile
#Should repeat, should be in same order for PCS zones. Check... yup.
eViewsFile2 <- eViewsFile 
  
eViewsFile2$ea_ttwa91 <- pcs_zones$ea_ttwa91
eViewsFile2$ea_nuts3_91 <- pcs_zones$ea_nuts3_91

#Yup
plot(eViewsFile2$ea91,eViewsFile2$ea_ttwa91)
plot(eViewsFile2$ea91,eViewsFile2$ea_nuts3_91)
  
#rearrange
#eViewsFile2 <- eViewsFile2[,c(1:11,17:18,12:16)]

#SAVE
#write_csv(eViewsFile2,'R_data/estimation_aggEmploymentToLargerZones.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add employment change 91 to 01----
#eViewsFile2 <- read_csv('R_data/estimation_aggEmploymentToLargerZones.csv')

#Keep just percent employment
eak <- ea[,c(1,4,5)]
eak <- spread(eak,censusYear,percentEmp) %>% dplyr::select(1,2,3)

eak$deltaEmp9101 <- eak$`2001`-eak$`1991`

#Will repeat for each CoB
eViewsFile2$deltaEmp9101 <- eak$deltaEmp9101

write_csv(eViewsFile2,'R_data/estimation_aggEmploymentToLargerZonesDeltaEmp.csv')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add urban/rural classification to the file----

urbanRatios <- read_csv('R_data/pcsAggregatedZeroCounts_urbanRatios.csv')
eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmp.csv')

#Mark those 50%+ urban
urbanRatios$urbanFiftyPercentPlus <- 0 + (urbanRatios$intersectUrbanRatio > .5)

table(urbanRatios$urbanFiftyPercentPlus)

#all.equal(urbanRatios$label,eViewsFile$code[1:822])

eViewsFile2 <- merge(eViewsFile,urbanRatios[,c('label','urbanFiftyPercentPlus')], 
                     by.x='code',by.y='label',
                     all.x = T)

#Correct number
table(eViewsFile2$urbanFiftyPercentPlus)/22

#write_csv(eViewsFile2,'R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRural.csv')
saveRDS(eViewsFile2,'R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRural.rds')

#~~~~~~~~~~~~~~~~~~
#Add TTWA index----

#Load Scots TTWA
ttwa <- readOGR(dsn='C:/Data/MapPolygons/Scotland/2001/Scotland_ttwa_2001',
                layer='scotland_ttwa_2001_dissolvedByName')

#Ooooh I need an intersection.... Oh, think I've already done it
intz <- readOGR(dsn='C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/Intersects/otherIntersects', layer='PCS_noZeros_TTWA_Scots')

intz$area <- gArea(intz,byid = T)

#Pick TTWA that accounts for the largest chunk of each PCS
maxTTWA <- data.frame(intz) %>% 
  group_by(label) %>% 
  summarise(max = max(area))

maxTTWA2 <- data.frame(intz)[intz$area %in% maxTTWA$max,] 

#Well that was an ugly way of doing that. Seemed to work.
#Correct number of TTWAs? Yup.
unique(maxTTWA2$label) %>% length
  
#Merge in TTWAs with estimation data file
eViewsFile <- readRDS('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRural.rds')

eViewsFile <- merge(eViewsFile,maxTTWA2[,c('label','name')],
                    by.x = 'code',
                    by.y = 'label',
                    all.x = T)

#save again again
#csv isbeing weird on reload
saveRDS(eViewsFile,'R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.rds')
#write_csv(eViewsFile,'R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Output CoB shares per decade, one column each, for QGIS viewing----

eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRural.csv')

#Actually, we've only got 91/11 at the mo. Fair enough.
# eViewsWide91 <- dcast( 
#   eViewsFile %>% dplyr::select(code,xij1991,CoB),CoB ~ xij1991,
#   value.var = 'xij1991')

#Fine!
wide91 <- spread(eViewsFile %>% dplyr::select(code,xij1991,CoB),
                 CoB,xij1991)

#Better!
wide11 <- spread(eViewsFile %>% dplyr::select(code,xij2011,CoB),
                 CoB,xij2011)

#Need to name them...
updatedNames <- sapply(names(wide91[,c(2:23)]),
                       function(x) paste0('91_',x))

names(wide91) <- c('code',updatedNames)

updatedNames <- sapply(names(wide11[,c(2:23)]),
                       function(x) paste0('11_',x))

names(wide11) <- c('code',updatedNames)

#Stick together
widez <- cbind(wide91,wide11[,-1])

write_csv(widez,'R_data/3census_CoBShares1colEach91and11.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~FIVE-CENSUS~~~~----

#Get e-views-ready format (also useable here) from the 5-census/PCS91 data
#Which requires enlongamanating the CoBs first. Might keep all of them this time.
cob <- read_csv('StitchOutputs/Scotland/LBS_5censusCombinedData/countryOfBirth.csv')
ea <- read_csv('StitchOutputs/Scotland/LBS_5censusCombinedData/econActive.csv')

#For comparing
eViewzCompare <- read_csv('R_data/estimation_aggEmploymentToLargerZones.csv')

#Start making into eviews-ready shape
#First-up: across-zone proportions for CoB categories.

#Add column for population total before finding proportions
#Do this before subsetting: we want proportions across zones
#For the WHOLE population
cob$Population <- apply(cob[,c(3:16)],1,sum)

#Proportions per year
cobprops <- cob %>% 
  dplyr::select(3:18) %>% 
  group_by(censusYear) %>% 
  mutate_each(  funs(  ((.)/sum(.))*100  ) )

#Did that work? Yup!
apply(cobprops[cobprops$censusYear==1971,c(1:16)],2,sum)
apply(cobprops[cobprops$censusYear==1981,c(1:16)],2,sum)
apply(cobprops[cobprops$censusYear==2011,c(1:16)],2,sum)


#Put in every year this time as its own column
#make em long
cobpropslong <- list()

yrz <- c(1971,1981,1991,2001,2011)

for(x in c(1:5)){

  cobpropslong[[x]] <- cobprops %>%
  filter(censusYear == yrz[x]) %>% 
  gather(cob,share,c(1:14,16))
  
}

chk2 <- cobpropslong[[5]]


#Population at the end will actually need repeating 
#in its own column to match the eviews file
#Just selecting 882 / a year's worth of labels 
#to make sure the repeating vector fits
# eViewsFile <- do.call(cbind,list(cob$label[cob$censusYear==1991],
#                                  lapply(cobpropslong, function(x) x$share)
#                                  )) %>% data.frame()


eViewsFile <- data.frame(
  code = rep(cob$label[cob$censusYear==1991],length(unique(cobpropslong[[1]]$cob))),
  xij1971 = cobpropslong[[1]]$share,
  xij1981 = cobpropslong[[2]]$share,
  xij1991 = cobpropslong[[3]]$share,
  xij2001 = cobpropslong[[4]]$share,
  xij2011 = cobpropslong[[5]]$share,
  cob = rep(unique(cobpropslong[[1]]$cob),each = 822)
  )


#Population is the last 822. Keep for adding as its own repeating column
#Then remove. Should end up 14 categories
popkeep <- eViewsFile[(nrow(eViewsFile)-821):nrow(eViewsFile),]

names(popkeep) <- c('code',
                    'pop1971',
                    'pop1981',
                    'pop1991',
                    'pop2001',
                    'pop2011',
                    'cob'
                    )

eViewsFile <- eViewsFile[1:(nrow(eViewsFile)-822),]

#Check remaining cobs have right numbers... aye
table(eViewsFile$cob)

#Got the right number at the end.. aye
nrow(eViewsFile) %% 822

#Add population back in - repeating column per CoB
eViewsFile <- cbind(eViewsFile,popkeep[,c(2:6)])

#~~~~~~~~~~~~~~~~~~~
#Add econ active as repeating column for each census year----

#enwidenemanate, one column per census year
# eawide <- spread(ea %>% dplyr::select(censusYear,percentEmp),key=censusYear,value=percentEmp)
# eawide <- dcast(ea[,c(4,5)], censusYear~percentEmp,value.var = ea$percentEmp)

#**** knows why I can't get that to work.
eawide <- data.frame(
  ea1971 = ea$percentEmp[ea$censusYear==1971],
  ea1981 = ea$percentEmp[ea$censusYear==1981],
  ea1991 = ea$percentEmp[ea$censusYear==1991],
  ea2001 = ea$percentEmp[ea$censusYear==2001],
  ea2011 = ea$percentEmp[ea$censusYear==2011]
)

#There. Why was that so hard?
#EA will now repeat for each CoB
eViewsFile <- cbind(eViewsFile,eawide)

#check
table(eViewsFile$ea2011)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add weight matrix results, 5 census----
#Done in miscDataReshaping
mx <- as.matrix(
  read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeights_8nearestNeighb.csv")
)

mxq <- as.matrix(
  read.csv("StitchOutputs/Scotland/other/adjusted91PCS_spatialWeightsQueenContig.csv")
)

#Remove first columns
mx <- mx[,-1]
mxq <- mxq[,-1]
dim(mxq)

#Oh! Why was it character? Why would it load like that?
apply(mx,1,class)

mx <- as.numeric(mx)
mx <- matrix(mx, nrow = 822, ncol = 822)
table(apply(mx,1,class))

mxq <- as.numeric(mxq)
mxq <- matrix(mxq, nrow = 822, ncol = 822)
table(apply(mxq,1,class))

#Check it's oriented correctly (since nearest-neighbours are not symmetrical)
#Currently eight nearest neighbours, row-normalised, so the 0.125s should sum to 1
#across rows
table(apply(mx,1,sum))

#And multiplying by an all 1s vector should produce the same result...
mx %*% rep(1,822) %>% table

#~~~~~~~~~~~~~~
#Weights matrix for each CoB

#cobprops just needs population and censusYear columns swapping
#And population dropping: we only find matrix for CoBs
cobprops2 <- cobprops[,c(1:15)]

#For first four censuses
weightsMatrix <- list()

eViewsFile$w71nn8 <- 0
eViewsFile$w81nn8 <- 0
eViewsFile$w91nn8 <- 0
eViewsFile$w01nn8 <- 0

nn8names <- c(
  'w71nn8',
  'w81nn8',
  'w91nn8',
  'w01nn8'
)

for(x in c(1:4)){

  weightsMatrix[[x]] <- matrix(nrow = 822,ncol = length(unique(eViewsFile$cob)))
  
  for(val in seq(1:length(unique(eViewsFile$cob)))){
    
    weightsMatrix[[x]][,val] <- mx %*% as.matrix(cobprops2[cobprops2$censusYear==yrz[x],val])
    
  }
  
  #Keep only those we're using. These, in theory, should all still be in the correct order.
  #But actually we need the index of those columns
  #indz <- sapply(subz,function(x) grep(x,names(cob)))
  #Actually, using cobprops, not necessary to subset.
  #Which need 3 subtracting from them to match weights index
  #weightsToKeep <- weightsMatrix[,(as.numeric(indz)-3)]
  
  #And they should all be in the right order for melting and sticking in the thingyo.
  eViewsFile[,nn8names[x]] <- weightsMatrix[[x]] %>% data.frame() %>% 
    gather(cob,value) %>%
    dplyr::select(value) %>% 
    unlist(use.names = F)


}#end for x

#Repeat for queen contig ... or don't for now. That might be enough variables for now.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Then rather more faff: migrant share not counting own group, 5 census----
# eViewsFile$migsh71minusOwn <- 0
# eViewsFile$migsh81minusOwn <- 0
# eViewsFile$migsh91minusOwn <- 0
# eViewsFile$migsh01minusOwn <- 0
# 
# nn8names <- c(
#   'migsh71minusOwn',
#   'migsh81minusOwn',
#   'migsh91minusOwn',
#   'migsh01minusOwn'
# )
# 
# #ignore all the 91 names in here - just copied from above
# for(x in c(1:5)) {
# 
#   #Make a new dataset with sum of all UK folk plus population.
#   #Then can make each other CoB long and do subtractions.
#   migshare91minusOwn <- cob %>% filter(censusYear==yrz[x])
#   
#   migshare91minusOwn$UK <- (migshare91$Channel_Is + migshare91$UK_part_no + migshare91$England + migshare91$Wales + migshare91$Northern_I + migshare91$Scotland)
#   
#   long1991CoB <- migshare91minusOwn[,c(4:42)] %>% 
#     gather(CoB,count)
#   
#   #Set values to zero if migrant group is within UK - already counted above
#   #Currently unnecessary as it doesn't include and UK groups, but...
#   long1991CoB$count[long1991CoB$CoB 
#                     %in% c('Channel_Is','UK_part_no','England','Scotland','Wales','Northern_I')] <- 0
#   
#   #Add in UK and pop values
#   long1991CoB$UK <- migshare91minusOwn$UK
#   long1991CoB$Population <- migshare91minusOwn$Population
#   
#   #Repeated for each CoB? Yup.
#   table(long1991CoB$UK)
#   table(long1991CoB$Population)
#   
#   long1991CoB$migshareMinusOwn <- ((long1991CoB$Population - (long1991CoB$count + long1991CoB$UK))/
#                                      long1991CoB$Population) * 100
#   
#   #Keep only those CoB being used
#   long1991CoBFinal <- long1991CoB %>% filter(CoB %in% subz)
#   
#   #Attach to eViews-ready, now the right length
#   eViewsFile$migsh91minusown <- long1991CoBFinal$migshareMinusOwn
# 
# }

#Bits----

#Save what we have so far. Stick CoB at end
names(eViewsFile)
eViewsFile2 <- eViewsFile[,c(1:6,8:21,7)]

write_csv(eViewsFile2,'R_data/5Census_estimation_basic.csv')







