#Redoing all regressions and data prep in an attempt to keep things tidy
#And probably making messier in the process.
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat','purrr','forcats')
lapply(geolibs, require, character.only = TRUE)

source("Function_CoBRegressionFunctions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Assemble Scots datasheet----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using the CoB function so proportions can be formed based on any zone subset.
#Then add the other required variables to that.
cobScots <-
  c(
    'StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp',
    'StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2001_CountryOfBirthRecode_91LBS_noZeroPCS.shp',
    'StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2011_CountryOfBirthRecode_91LBS_noZeroPCS.shp'
  )

cobScot <- lapply(cobScots, function(x) readShapeSpatial(x))

#Drop not-needed cols
cobScot <- lapply(cobScot, function(x) x[,c(2,4:ncol(x))])

# #Look at one
# one <- cobScot[[2]] %>% data.frame
# 
# #Confirm that's different than we had before... 
# chk <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/backups_preNonEUEuropeCorrection/2001_CountryOfBirthRecode_91LBS_noZeroPCS.shp')
# chk <- chk[,c(2,4:ncol(chk))]
# 
# sum(one$Rest_of_wo)
# sum(chk$Rest_of_wo)

#So reminder: it was just non-EU-Western Europe in 2001 that I'd prev dropped in error. 
#So only 2001 differs and only in the 'europe other' sum.

geoffsOrigList <- c( 'Irish_Repu', 'China', 'South_Afri', 'Africa_oth', 'South_Amer', 'Other_Midd', 'Other_East', 'Europe_oth', 'Australia', 'Canada', 'Nigeria', 'India', 'Pakistan', 'Hong_Kong', 'Malaysia', 'France', 'Germany', 'Italy', 'Netherland', 'Spain', 'Poland', 'United_Sta' ) 

#rich/poor split
richpoor <- data.frame(geoffsOrigList,rich = c(1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,1,1,1,1,1,0,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Get other vars (which should all still be the same as before at this point)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#In fact, can't I just copy directly from previously?
eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

#keep only single zones and the vars we want
eViewsFile <- eViewsFile[!duplicated(eViewsFile$code),] %>% 
  dplyr::select(code,popsh91:pophs91,ea91:lpophs91,deltaEmp9101:name)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Compile sheet, run regression----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Copied function to Function_CoBRegressionFunctions.R

sheet <- compileSheetDiffZones(unique(cobScot[[1]]$label))

#save all that (not re-doing for each zone subset for now... can check on diff later)
#saveRDS(sheet,'R_data/sheetScotJuly17.rds')

#~~~~~~~~~~~~~~~
#Regressions----
#~~~~~~~~~~~~~~~

#Just CoB and weights
outputTofile(
  inputdata = sheet,
  #splitByFactor = 'rich',
  title = 'all CoB, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = F
)

outputTofile(
  inputdata = sheet[sheet$europe==1,],
  #splitByFactor = 'rich',
  title = 'Europe, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

#Need better labels than one and zero...
outputTofile(
  inputdata = sheet[sheet$rich==1,],
  #splitByFactor = 'rich',
  title = 'Rich, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

outputTofile(
  inputdata = sheet[sheet$rich==0,],
  #splitByFactor = 'rich',
  title = 'Poor, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#URBAN, JUST COB AND WEIGHTS
urbansheet <- sheet[sheet$urbanFiftyPercentPlus==1,]

outputTofile(
  inputdata = urbansheet,
  #splitByFactor = 'rich',
  title = 'all CoB, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

outputTofile(
  inputdata = urbansheet[urbansheet$europe==1,],
  #splitByFactor = 'rich',
  title = 'Europe, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

#Need better labels than one and zero...
outputTofile(
  inputdata = urbansheet[urbansheet$rich==1,],
  #splitByFactor = 'rich',
  title = 'Rich, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

outputTofile(
  inputdata = urbansheet[urbansheet$rich==0,],
  #splitByFactor = 'rich',
  title = 'Poor, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = 'xij11 ~ xij91 + w91q',
  myAppend = T
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#REPEAT ALL ABOVE FOR WHOLE VARIABLE LIST
lmforall = 'xij11 ~ xij91 + w91q + migShareMinusOwn91 + lph91 + hsperacre91 + lpophs91 + ea_ttwa91'

outputTofile(
  inputdata = sheet,
  #splitByFactor = 'rich',
  title = 'all CoB, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = sheet[sheet$europe==1,],
  #splitByFactor = 'rich',
  title = 'Europe, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

#Need better labels than one and zero...
outputTofile(
  inputdata = sheet[sheet$rich==1,],
  #splitByFactor = 'rich',
  title = 'Rich, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = sheet[sheet$rich==0,],
  #splitByFactor = 'rich',
  title = 'Poor, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#URBAN, JUST COB AND WEIGHTS
urbansheet <- sheet[sheet$urbanFiftyPercentPlus==1,]

outputTofile(
  inputdata = urbansheet,
  #splitByFactor = 'rich',
  title = 'all CoB, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = urbansheet[urbansheet$europe==1,],
  #splitByFactor = 'rich',
  title = 'Europe, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

#Need better labels than one and zero...
outputTofile(
  inputdata = urbansheet[urbansheet$rich==1,],
  #splitByFactor = 'rich',
  title = 'Rich, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = urbansheet[urbansheet$rich==0,],
  #splitByFactor = 'rich',
  title = 'Poor, URBAN zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/all.txt',
  lmfunction = lmforall,
  myAppend = T
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cities, urban only
#glasgow edinburgh aberdeen dundee
#Check on zone number too.
cities <- sheet[sheet$name %in% c('Glasgow','Edinburgh','Aberdeen','Dundee'),]

#UPDATE: I think I'm duplicate-counting here, these aren't the right numbers, are they?

#How many zones in each?
#Aberdeen    Dundee Edinburgh   Glasgow 
#946       682      2068      3850
table(cities[!duplicated(cities$label),]$name)
#If just urban
#Aberdeen    Dundee Edinburgh   Glasgow 
#330       418      1408      2794
table(cities[!duplicated(cities$label) & cities$urbanFiftyPercentPlus==1,'name'])

#Percent urban
#Aberdeen    Dundee   Edinburgh   Glasgow 
#34.88       61.29    68.08511    72.57
(table(cities$name[cities$urbanFiftyPercentPlus==1])/table(cities$name))*100

#~~~~~~~~~~~~~~~~~~~~~~~~
#Might as well do both urban and all
#Can't use TTWA average, it's the same value for all
lmforall = 'xij11 ~ xij91 + w91q + migShareMinusOwn91 + lph91 + hsperacre91 + lpophs91 + ea91'

outputTofile(
  inputdata = cities[cities$rich == 1,],
  splitByFactor = ('name'),
  title = 'Rich, Cities, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = F
)

outputTofile(
  inputdata = cities[cities$rich == 0,],
  splitByFactor = ('name'),
  title = 'Poor, Cities, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[(cities$rich == 1 & cities$urbanFiftyPercentPlus==1),],
  splitByFactor = ('name'),
  title = 'Rich, Cities, urban zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[(cities$rich == 0 & cities$urbanFiftyPercentPlus==1),],
  splitByFactor = ('name'),
  title = 'Poor, Cities, urban zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

#Dammit. That would look better by rich/poor for each city. Try again.
cities$rich[cities$rich==1] <- 'rich'
cities$rich[cities$rich==0] <- 'poor'

options("scipen"=100, "digits"=4)

outputTofile(
  inputdata = cities[cities$name == 'Glasgow',],
  splitByFactor = ('rich'),
  title = 'Glasgow, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = F
)

outputTofile(
  inputdata = cities[cities$name == 'Edinburgh',],
  splitByFactor = ('rich'),
  title = 'Edinburgh, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[cities$name == 'Aberdeen',],
  splitByFactor = ('rich'),
  title = 'Edinburgh, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[cities$name == 'Dundee',],
  splitByFactor = ('rich'),
  title = 'Edinburgh, all zones',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)


#Urban
outputTofile(
  inputdata = cities[cities$name == 'Glasgow' & cities$urbanFiftyPercentPlus == 1,],
  splitByFactor = ('rich'),
  title = 'Glasgow, urban',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[cities$name == 'Edinburgh' & cities$urbanFiftyPercentPlus == 1,],
  splitByFactor = ('rich'),
  title = 'Edinburgh, urban',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[cities$name == 'Aberdeen' & cities$urbanFiftyPercentPlus == 1,],
  splitByFactor = ('rich'),
  title = 'Aberdeen, urban',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

outputTofile(
  inputdata = cities[cities$name == 'Dundee' & cities$urbanFiftyPercentPlus == 1,],
  splitByFactor = ('rich'),
  title = 'Dundee, urban',
  file = 'regressionOutputs/3census/regressionsJuly2017/cities.txt',
  lmfunction = lmforall,
  myAppend = T
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Cities: recalculate proportions for each-----

#Cos I think it might make a large difference in this case. So what would need doing?
#OK, stuck it all into one function to make this easier.

#Then just need to subset by city each time. Let's stick to urban to compare
glasgow <- compileSheetDiffZones(unique(sheet$label[sheet$urbanFiftyPercentPlus==1 & sheet$name == 'Glasgow']))

#Did that work? Yup!
glasgow %>% group_by(cob) %>% summarise(sum(xij91))

#Put all of them into a single list to try and keep this tidy?
citiesList <- c('Glasgow','Edinburgh','Aberdeen','Dundee')

citySheets <- lapply(citiesList,
                     function(city) compileSheetDiffZones(unique(sheet$label[sheet$urbanFiftyPercentPlus==1 
                                                                          & sheet$name == city])))

#Tick.
lapply(citySheets, function(x) x %>% group_by(cob) %>% summarise(sum(xij91)))
lapply(citySheets, function(x) x %>% group_by(cob) %>% summarise(sum(xij11)))


#~~~~~~~~~~~~~~~~~~~~~~
#While we're here, save 3 census sheet and citysheets list
saveRDS(sheet,'R_data/sheet3census.rds')
saveRDS(citySheets,'R_data/citySheetsList3census.rds')




#Output rich/poor for each. Err, only first one should be new. Doh. Hack that...

for(i in 1:4){

  outputTofile(
    inputdata = citySheets[[i]],
    splitByFactor = ('rich'),
    title = citiesList[i],
    file = 'regressionOutputs/3census/regressionsJuly2017/cities_cob100atcitylevel.txt',
    lmfunction = lmforall,
    myAppend = ifelse(i == 1, F, T)
  )
  
}

#Just some dundee checks
chk <- cobScot[[1]]
plot(chk[chk$label %in% unique(citySheets[[4]]$label),])

#Ah: contains an awry zone that's not actually in Dundee. How did that get there?
#Are the other cities correct?
plot(chk[chk$label %in% unique(citySheets[[1]]$label),])
plot(chk[chk$label %in% unique(citySheets[[2]]$label),])
plot(chk[chk$label %in% unique(citySheets[[3]]$label),])

#Oh wait - the problem is going to be the urban thing, isn't it? Can check directly
#Yup, that's it.
#Merge in urban flag for mapping
chk2 <- eViewsFile[!duplicated(eViewsFile$code),c('code','urbanFiftyPercentPlus')]
chk <- sp::merge(chk,chk2,by.x = 'label', by.y = 'code', duplicateGeoms = F)
length(unique(chk$label))
length(unique(chk2$code))

#There's no reason that shouldn't work. Vexed. Just need to make new one.
colourinz <- data.frame(code = chk$label[chk$label %in% unique(eViewsFile$code[eViewsFile$name=='Glasgow'])])
colourinz <- merge(colourinz,chk2, by = 'code')

plot(chk[chk$label %in% unique(eViewsFile$code[eViewsFile$name=='Glasgow']),], 
     col=ifelse(colourinz$urbanFiftyPercentPlus==1,'blue','green'))

#~~~~~~~~~~~~~~~~~~~~~~~~~
#5 CENSUS REGRESSIONS-----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#Making a hacked function specifically for the 5 census.
#And maybe one day I'll come back and do this properly.


#Testing... changes work for three census. Let's see if they do for 5.
# sheet <- CoBtoRegressionReadyData(cobScot,zonesToKeep,geoffsOrigList,
#                                   c('Channel_Is','UK_part_no','England','Wales','Northern_I','Scotland'),
#                                   1991,c(1991,2001,2011))

cobScots5 <-
  c(
    'StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/1971_CoB_from_71EDs_to_91_aggPostcodeSectors.shp',
    'StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/1981_CoB_from_81EDs_to_91_aggPostcodeSectors.shp',
    'StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91SAS_noZeroPCS_straightMatch.shp',
    'StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/preNonEUEuropeBackup/2001_CoB_from_01OAs_to_91_aggPostcodeSectors.shp',
    'StitchOutputs/Scotland/LBS_postcodeSector_5Census_raw/CountryOfBirth/preNonEUEuropeBackup/2011_CoB_from_11OAs_to_91_aggPostcodeSectors.shp'
  )

cobScot5 <- lapply(cobScots5, function(x) readShapeSpatial(x))

#Drop not-needed cols. Note: 2001 and 2011 have annoyingly different names. Need to fix that here.
#Check on names first
lapply(cobScot5, names)

cobScot5[c(1:3)] <- lapply(cobScot5[c(1:3)], function(x) x[,c(2,4:ncol(x))])
cobScot5[c(4:5)] <- lapply(cobScot5[c(4:5)], function(x) x[,c(1,3:ncol(x))])

#Columns now match, apart from names. Fix names.
names(cobScot5[[4]]) <- names(cobScot5[[1]])
names(cobScot5[[5]]) <- names(cobScot5[[1]])

#I should probably save that at some point...

#Label rich/poor
richpoor5 <- data.frame(
  cob = names(cobScot5[[1]])[2:15],
  rich = c(1,1,1,1,1,1,0,0,0,1,0,0,0,0)
)

richpoor5$rich[richpoor5$rich==1] <- 'rich'
richpoor5$rich[richpoor5$rich==0] <- 'poor'

zonez <- unique(cobScot5[[1]]$label)
#cobz <- names(cobScot5[[1]][2:ncol(cobScot5[[1]])])
cobz <- c("Irish_Repu","Old_Common","Africa__Ne","India","Pakistan","Other_Euro","SE_Asia_Ne","Caribbean_","New_Common","Rest_of_Wo")

sheet5 <- CoBtoRegressionReadyData(cobScot5,zonez,cobz,
                                  c('Rest_of_UK','England','Wales','Scotland'),
                                  1971,c(1971,1981,1991,2001,2011))

#Look reasonable? Well, it sums correctly
hist(sheet5$xij2011[sheet5$cob=='Pakistan'],breaks=20)
sum(sheet5$xij2011[sheet5$cob=='Pakistan'])

#old eviewsready file. What's it look like?
eViewsFile5 <- read_csv('R_data/5Census_estimation_basic.csv')

#Keep only single set of zone values, pop71 and ea71
eViews71 <- eViewsFile5[!duplicated(eViewsFile5$code),] %>% 
  dplyr::select(code,pop1971,ea1971)

#Work out people per acre for 71 using pop1971 from that sheet
totalPop71 <- apply(data.frame(cobScot5[[1]])[2:15],1,sum)
areaz <- data.frame(label = cobScot5[[1]]$label, acres = gArea(cobScot5[[1]], byid = T) * 0.000247105381,totalPop71 = totalPop71)
areaz$popPerAcre71 <- areaz$totalPop71/areaz$acres


#Merge in the various bits
sheet5 <- merge(sheet5,richpoor5,by = 'cob', all.x = T)

sheet5 <- merge(sheet5,eViews71,by.x = 'label',by.y = 'code', all.x=T)

sheet5 <- merge(sheet5, areaz[,c(1,4)], by = 'label', all.x=T)

#Steal urban 50% plus from 3 census. Oh and TTWA names!
urban50 <- eViewsFile[,c('code',"urbanFiftyPercentPlus","name")]

sheet5 <- merge(sheet5, urban50, by.x = 'label', by.y = 'code', all.x=T)

saveRDS(sheet5,'R_data/sheet5.rds')

#~~~~~~~~~~~~~~~~~~
#5 CENSUS: RUN THE REGRESSIONS----
#~~~~~~~~~~~~~~~~~~

#Above code now in function in Function_CoBRegressionFunctions.R

#For tests on compileSheet function:
zonesToKeep <- unique(cobScot5[[1]]$label)
#cobz <- names(cobScot5[[1]][2:ncol(cobScot5[[1]])])
cobz <- c("Irish_Repu","Old_Common","Africa__Ne","India","Pakistan","Other_Euro","SE_Asia_Ne","Caribbean_","New_Common","Rest_of_Wo")




sheet5 <- compileSheetDiffZones5census(unique(cobScot5[[1]]$label))
saveRDS(sheet5,'R_data/sheet5.rds')


#Check separate city sheets work (urban only)
citiesList <- c('Glasgow','Edinburgh','Aberdeen','Dundee')

citySheets5 <- lapply(citiesList,
                     function(city) compileSheetDiffZones5census(unique(sheet5$label[sheet5$urbanFiftyPercentPlus==1 
                                                                             & sheet5$name == city])))
#It's gonna be easier to save that and reload actually...
saveRDS(citySheets5,'R_data/citySheets5.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Trying different lists of CoBs to see rich/poor diff. Hopefully these ones are more clearly rich/poor
#cobzIn = c("Old_Common","India","Pakistan","Other_Euro")

#Just drop rest of world. Others should be reasonably good poor/rich split
cobzIn = c("Africa__Ne","Caribbean_","SE_Asia_Ne",
           "Pakistan","Irish_Repu","Old_Common",
           "New_Common","Other_Euro","India")

sheet5sub <- compileSheetDiffZones5census(unique(cobScot5[[1]]$label),cobzIn)

citySheets5sub <- lapply(citiesList,
                     function(city) compileSheetDiffZones5census(unique(sheet5$label[sheet5$urbanFiftyPercentPlus==1 
                                                                             & sheet5$name == city]),
                                                                 cobzIn))
saveRDS(sheet5sub,'R_data/sheet5sub2.rds')
saveRDS(citySheets5sub,'R_data/citySheets5sub2.rds')

#check they look right
plot(sheet5sub$xij2011[sheet5sub$cob == "Pakistan"],sheet5sub$xij2001[sheet5sub$cob == "Pakistan"])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 CENSUS: PULL OUT COEFFICIENTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Rich CoBs: Irish Republic, Old Commonwealth, Europe
#Poor: Africa New Commonweath, India, Pakistan, South-East Asia New Commonwealth, Caribbean , Other New Commonwealth, Rest of World

#to have different year as  regressor
fiveCensusPullOutCoeffs <- function(year,filenameadd){

  formula5census <- paste0('xij2011 ~ xij',year,' + w',year,'q + migshareMinusOwn',year
                           ,' + ea',year,' + popPerAcre',year)
  
  #To start with, numbers I was pulling out in 5CensusRegressions.Rmd. 
  #Just re-run that code here, get it mapping to get the numbers.
  # sheet5 <- readRDS('R_data/sheet5.rds')
  
  #Oooo this is awful
  if(filenameadd=='allbutRoW'){
  
    #No rest of world
    sheet5 <- readRDS('R_data/sheet5sub2.rds')
    citySheets5 <- readRDS('R_data/citySheets5sub2.rds')
  
  } else {

    #2 rich 2 poor
    sheet5 <- readRDS('R_data/sheet5sub.rds')
    citySheets5 <- readRDS('R_data/citySheets5sub.rds')
  
  }
  
  #Separate city sheets
  citiesList <- c('Glasgow','Edinburgh','Aberdeen','Dundee')
  
  allScot <- lapply(split(sheet5,sheet5[,'rich']), function(x) summary(lm(data=x, formula = formula5census)))
  
  both <- rbind(allScot[[1]]$coefficients %>% data.frame,allScot[[2]]$coefficients %>% data.frame)
  both$source <- c(rep("5census_allScot_Poor",6),rep("5census_allScot_Rich",6))
  both$coeff <- rep(row.names(both)[1:6],2)
  
  #All scot, Urban zones, rich vs poor
  #https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
  allScotUrban <- lapply(split(sheet5[sheet5$urbanFiftyPercentPlus==1,],sheet5[sheet5$urbanFiftyPercentPlus==1,'rich']), 
                         function(x) summary(lm(data=x, formula = formula5census)))
  
  #There we go...
  both2 <- rbind(allScotUrban[[1]]$coefficients %>% data.frame,allScotUrban[[2]]$coefficients %>% data.frame)
  both2$source <- c(rep("5census_urban_allScot_Poor",6),rep("5census_urban_allScot_Rich",6))
  both2$coeff <- rep(row.names(both2)[1:6],2)
  
  both <- rbind(both,both2)
  
  
  dfs <- vector("list", length = 4)
  
  #CITIES 5 CENSUS 4 COB
  for(i in 1:4){
    
    print(citiesList[[i]])
    
    rez <- lapply(split(citySheets5[[i]],citySheets5[[i]][,'rich']), function(x) summary(lm(data=x, formula = formula5census)))
    
    dfs[[i]] <- rbind(rez[[1]]$coefficients %>% data.frame,rez[[2]]$coefficients %>% data.frame)
    dfs[[i]]$source <- c(rep(   paste0(citiesList[[i]],"_5census_urbanPoor")  ,6),
                  rep(paste0(citiesList[[i]],"_5census_urbanRich"),6))
    dfs[[i]]$coeff <- rep(row.names(dfs[[i]])[1:6],2)
    
  }
  
  allCities <- do.call(rbind,dfs)
  
  allrez <- rbind(both,allCities)
  
  #remove intercept cols
  allrez <- allrez %>% filter(coeff!="(Intercept)")
  
  #Save before setting zeroes, for use later
  #Hardcoding name changes
  # write.csv(allrez,paste0('regressionOutputs/5census/5censusTableCombo_4cobs',year,'.csv'), row.names = F)
  write.csv(allrez,paste0('regressionOutputs/5census/5censusTableCombo',filenameadd,'_',year,'.csv'), row.names = F)
  
  
  #For saving / showing in excel sheet. Mark significance by setting coeffs to exactly zero.
  allrez$Estimate <- ifelse(allrez$Pr...t.. < 0.05, allrez$Estimate, 0)
  table(0 + allrez$Estimate == 0)
  
  #enwidenemate
  allrezwide <- spread(allrez %>% dplyr::select(-one_of('Std..Error','t.value','Pr...t..')),
                       key = source, value = Estimate)
  
  #Set coeff order
  allrezwide$coeff <- factor(allrezwide$coeff,levels = c(paste0('xij',year),
                                                         paste0('w',year,'q'),
                                                                paste0('migshareMinusOwn',year),
                                                                       paste0('popPerAcre',year),
                                                                              paste0('ea',year)))
  
  allrezwide <- allrezwide[order(allrezwide$coeff),]
  
  
  #save
  write.csv(allrezwide,paste0('regressionOutputs/5census/wide',filenameadd,'_',year,'.csv'), row.names = F)
  # write.csv(allrezwide,paste0('regressionOutputs/5census/wide5censusTableCombo_',year,'.csv'), row.names = F)
  

}

#also use filenameadd to get right data
#This is horrible but I keep on missing a change, so...
fiveCensusPullOutCoeffs(1971,'allbutRoW')
fiveCensusPullOutCoeffs(1991,'allbutRoW')

fiveCensusPullOutCoeffs(1971,'4cobs')
fiveCensusPullOutCoeffs(1991,'4cobs')

#~~~~~~~~~~~~
#random tests
lookz <- sheet5 %>% filter(cob == 'Pakistan')
plot(lookz$xij2011 ~ lookz$ea1991)

#Glasgow
lookzcity <- citySheets5[[1]] %>% filter(cob == 'Pakistan')
plot(lookzcity$xij2011 ~ lookzcity$ea1991)
plot(lookzcity$xij2011 ~ lookzcity$migshareMinusOwn1991)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 CENSUS: PULL OUT COEFFICIENTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Which wasn't done in an Rmd was it? Did I have sheets saved somewhere?
#No, didn't save. Let's re-run and grab it all.
#Test. Just using sheet to get zones to keep
#glasgow <- compileSheetDiffZones(unique(sheet$label[sheet$urbanFiftyPercentPlus==1 & sheet$name == 'Glasgow']))
#Did that work? Yup!
#glasgow %>% group_by(cob) %>% summarise(sum(xij91))

#Just got the sheet from above. Have not refactored everything. Will only regress 2011 ~ 1991

#Redo shares based on each city's zones
citiesList <- c('Glasgow','Edinburgh','Aberdeen','Dundee')

citySheets <- lapply(citiesList,
                     function(city) compileSheetDiffZones(unique(sheet$label[sheet$urbanFiftyPercentPlus==1 
                                                                             & sheet$name == city])))


formula3census <- 'xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991 + lph91 + hsperacre91 + lpophs91 + ea91'


allScot3 <- lapply(split(sheet,sheet[,'rich']), function(x) summary(lm(data=x, formula = formula3census)))

both3 <- rbind(allScot3[[1]]$coefficients %>% data.frame,allScot3[[2]]$coefficients %>% data.frame)
both3$source <- c(rep("3census_allScot_Poor",8),rep("3census_allScot_Rich",8))
both3$coeff <- rep(row.names(both3)[1:8],2)

#All scot, Urban zones, rich vs poor
#https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
allScotUrban3 <- lapply(split(sheet[sheet$urbanFiftyPercentPlus==1,],sheet[sheet$urbanFiftyPercentPlus==1,'rich']), 
                       function(x) summary(lm(data=x, formula = formula3census)))


#Two more coeffs than 5 census
both3a <- rbind(allScotUrban3[[1]]$coefficients %>% data.frame,allScotUrban3[[2]]$coefficients %>% data.frame)
both3a$source <- c(rep("3census_urban_allScot_Poor",8),rep("3census_urban_allScot_Rich",8))
both3a$coeff <- rep(row.names(both3a)[1:8],2)

both3 <- rbind(both3,both3a)

dfs <- vector("list", length = 4)

#CITIES 3 CENSUS
for(i in 1:4){
  
  print(citiesList[[i]])
  
  rez <- lapply(split(citySheets[[i]],citySheets[[i]][,'rich']), 
                function(x) summary(lm(data=x, formula = formula3census)))
  
  dfs[[i]] <- rbind(rez[[1]]$coefficients %>% data.frame,rez[[2]]$coefficients %>% data.frame)
  dfs[[i]]$source <- c(rep(   paste0(citiesList[[i]],"_3census_urban_allScot_Poor")    ,8),
                       rep(paste0(citiesList[[i]],"_3census_urban_allScot_Rich"),8))
  dfs[[i]]$coeff <- rep(row.names(dfs[[i]])[1:8],2)
  
}

allCities3 <- do.call(rbind,dfs)

allrez3 <- rbind(both3,allCities3)

allrez3 <- allrez3 %>% filter(coeff!="(Intercept)")

#save
write.csv(allrez3,paste0('regressionOutputs/3census/3censusTableCombo_1991.csv'), row.names = F)

#Zeroes for p<0.05 for wide version
allrez3$Estimate <- ifelse(allrez3$Pr...t.. < 0.05, allrez3$Estimate, 0)
table(0 + allrez3$Estimate == 0)

#enwidenemate
allrez3wide <- spread(allrez3 %>% dplyr::select(-one_of('Std..Error','t.value','Pr...t..')),
                     key = source, value = Estimate)

#Set coeff order
allrez3wide$coeff <- factor(allrez3wide$coeff,levels = c("xij1991",
                                                         "w1991q",
                                                         "migshareMinusOwn1991",
                                                         "lph91",
                                                         "hsperacre91",
                                                         "lpophs91",
                                                         "ea91"))



allrez3wide <- allrez3wide[order(allrez3wide$coeff),]


#save
write.csv(allrez3wide,paste0('regressionOutputs/3census/wide3censusTableCombo_1991.csv'), row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPARE CITY PROPORTIONS TYPE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#VIZ 3 AND 5----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Starting with the basic thingyo of showing the values with error rates.
#Will label if sig too.
#At the minute, collating each coeff type together as I'm not sure they're comparable.
#Which will mean putting them in a certain order. 

#Reload
rez3census <- read_csv('regressionOutputs/3census/3censusTableCombo_1991.csv')

rez5census71 <- read_csv('regressionOutputs/5census/5censusTableComboallbutRoW_1971.csv')
rez5census91 <- read_csv('regressionOutputs/5census/5censusTableComboallbutRoW_1991.csv')
rez5census71fourcobs <- read_csv('regressionOutputs/5census/5censusTableCombo4cobs_1971.csv')
rez5census91fourcobs <- read_csv('regressionOutputs/5census/5censusTableCombo4cobs_1991.csv')

#one coeff per chart, ordered by type. So.
#Add label to mark four cobs (merged with 'source' column)
rez5census71fourcobs$source <- paste0(rez5census71fourcobs$source,'_fourcobs')
rez5census91fourcobs$source <- paste0(rez5census91fourcobs$source,'_fourcobs')

#Shouldn't need these - choice of coefficient subsets them anyway
# rez5census71$source <- paste0(rez5census71$source,'/1971')
# rez5census71fourcobs$source <- paste0(rez5census71fourcobs$source,'/1971')
# rez5census91$source <- paste0(rez5census91$source,'/1991')
# rez5census91fourcobs$source <- paste0(rez5census91fourcobs$source,'/1991')

#Some common coeffs, some only in 3 census - one only in 5 is popPerAcre
#But ggplot should deal with that.
#Actually I probably want to output each to its own image 
#Not facet. Too much for facetting.

#COllate all into one sheet.
#"source" will distinguish between types.
allz <- do.call(rbind, list(rez3census,rez5census71,rez5census91,rez5census71fourcobs,rez5census91fourcobs))

#Ah there's some coeff name mismatches aren't there?
unique(allz$coeff)

#Fix. Ah that appears to be the only one.
allz$coeff[allz$coeff=='ea91'] <- 'ea1991'


#Test
# df <- allz[allz$coeff=='xij1991',]
# 
# #Add conf interval
# df <- df %>% 
#   mutate(
#     minn = Estimate - (Std..Error * 1.96),
#     maxx = Estimate + (Std..Error * 1.96)
#   )
# 
# # ggplot(bothmodels, aes(x = fct_reorder(names,-coeffs), 
# #                        y = coeffs, colour = factor(year))) +
# #   geom_point(size = 5) +
# #   geom_errorbar(aes(ymin = minn, ymax = maxx), 
# #                 width = 0.5, position = 'dodge') +
# #   coord_flip()
# 
# #Dodge rich/poor
# df$rich <- 'poor'
# df$rich[grepl('rich',df$source,ignore.case = T)] <- 'rich'
# table(df$rich)
# 
# #Remove rich/poor labels from source
# df$source <- gsub('_Rich','',df$source)
# df$source <- gsub('_Poor','',df$source)
# df$source <- gsub('Rich_','',df$source)
# df$source <- gsub('Poor_','',df$source)
# df$source <- gsub('Rich','',df$source)
# df$source <- gsub('Poor','',df$source)
# 
# #Mark significant
# df$sig <- ifelse(df$Pr...t.. < 0.05, 0, 1)
# 
# ggplot(df, aes(x = source,y = Estimate, colour = rich, shape = factor(sig))) +
#   geom_point(size = 5, position = position_dodge(width = 0.75)) +
#   geom_errorbar(aes(ymin = minn, ymax = maxx),width = 0.5, position = 'dodge') +
#   coord_flip() +
#   scale_shape_manual(values=c(16,4))


#Version that has: 3 census~91 / 5 census 71 / 5 census 91 / 5 census 71 4 cob / 5 census 91 4 cob
#And removes refs to year in coeffs so they can be used in same facet plot
allz2 <- allz

allz2 <- allz2 %>% 
  mutate(
    minn = Estimate - (Std..Error * 1.96),
    maxx = Estimate + (Std..Error * 1.96)
  )

# ggplot(bothmodels, aes(x = fct_reorder(names,-coeffs), 
#                        y = coeffs, colour = factor(year))) +
#   geom_point(size = 5) +
#   geom_errorbar(aes(ymin = minn, ymax = maxx), 
#                 width = 0.5, position = 'dodge') +
#   coord_flip()

#Dodge rich/poor
allz2$rich <- 'poor'
allz2$rich[grepl('rich',allz2$source,ignore.case = T)] <- 'rich'
table(allz2$rich)

#Remove rich/poor labels from source
allz2$source <- gsub('_Rich','',allz2$source)
allz2$source <- gsub('_Poor','',allz2$source)
allz2$source <- gsub('Rich_','',allz2$source)
allz2$source <- gsub('Poor_','',allz2$source)
allz2$source <- gsub('Rich','',allz2$source)
allz2$source <- gsub('Poor','',allz2$source)

#Mark significant
allz2$sig <- ifelse(allz2$Pr...t.. < 0.05, 1, 0)

#Mark zones type. Separate from source.
#Where source is 3 census~91 / 5 census 71 / 5 census 91 / 5 census 71 4 cob / 5 census 91 4 cob
allz2$zones <- 'all Scotland'
allz2$zones[grepl('urban_allScot',allz2$source,ignore.case = T)] <- 'all Scotland urban'
allz2$zones[grepl('glasgow',allz2$source,ignore.case = T)] <- 'Glasgow'
allz2$zones[grepl('edinburgh',allz2$source,ignore.case = T)] <- 'Edinburgh'
allz2$zones[grepl('aberdeen',allz2$source,ignore.case = T)] <- 'Aberdeen'
allz2$zones[grepl('dundee',allz2$source,ignore.case = T)] <- 'Dundee'

table(allz2$zones)

#Then source can be:
names(allz2)[names(allz2)=='source'] <- 'oldsource'

allz2$source <- '3 census ~ 1991'
allz2$source[grepl('5census',allz2$oldsource,ignore.case = T)
             & grepl('1991',allz2$coeff,ignore.case = T)] <- '5 census ~ 1991'
allz2$source[grepl('5census',allz2$oldsource,ignore.case = T)
             & grepl('1971',allz2$coeff,ignore.case = T)] <- '5 census ~ 1971'
allz2$source[grepl('fourcobs',allz2$oldsource,ignore.case = T)
             & grepl('1991',allz2$coeff,ignore.case = T)] <- '5 census ~ 1991, 2 rich 2 poor'
allz2$source[grepl('fourcobs',allz2$oldsource,ignore.case = T)
             & grepl('1971',allz2$coeff,ignore.case = T)] <- '5 census ~ 1971, 2 rich 2 poor'
table(allz2$source)

#So in theory that's everything.
#Oh oops, forgot to select a variable. And indeed rename the variables?
#Yup!
unique(allz2$coeff)
names(allz2)[names(allz2)=='coeff'] <- 'oldcoeff'

allz2$coeff <- 'xij'
allz2$coeff[grepl('q',allz2$oldcoeff)] <- 'W'
allz2$coeff[grepl('migshare',allz2$oldcoeff)] <- 'share of migrants minus own'
allz2$coeff[grepl('lph',allz2$oldcoeff)] <- 'log house price'
allz2$coeff[grepl('hsperacre',allz2$oldcoeff)] <- 'density'
allz2$coeff[grepl('lpophs',allz2$oldcoeff)] <- 'crowding'
allz2$coeff[grepl('ea',allz2$oldcoeff)] <- 'econ active'
allz2$coeff[grepl('popPerAcre',allz2$oldcoeff)] <- 'people per acre'

table(allz2$coeff)

#Test with xij
# output <- ggplot(allz2 %>% filter(coeff=='xij'), aes(x = zones, y = Estimate, colour = rich, shape = factor(sig))) +
#   geom_point(size = 5, position = position_dodge(width = 0.5)) +
#   geom_errorbar(aes(ymin = minn, ymax = maxx),width = 0.5, position = 'dodge') +
#   coord_flip() +
#   scale_shape_manual(values=c(16,4)) +
#   facet_wrap(~source, nrow = 1)
# 
# ggsave('R_outputs/AllCoefficents_Oct17/facet_xij.png', dpi = 300, height = 6, width = 16)

#Working. Now:
#Factor up source and zone
#Loop over coeffs, output. 
allz2$zones <- factor(allz2$zones, levels = c(
"all Scotland", "all Scotland urban","Glasgow","Edinburgh","Aberdeen","Dundee"  
))

#Reverse order cos coord flip
allz2$zones <- factor(allz2$zones, levels = c(
"Dundee",
"Aberdeen",
"Edinburgh",
"Glasgow",
"all Scotland urban",
"all Scotland"
))

allz2$source <- factor(allz2$source, levels = c(
  "3 census ~ 1991","5 census ~ 1971","5 census ~ 1991","5 census ~ 1971, 2 rich 2 poor","5 census ~ 1991, 2 rich 2 poor"
))

#Remove Aberdeen and Dundee - large SEs are making the rest hard to see
allz2NoAberDundee <- allz2[!(allz2$zones %in% c('Aberdeen','Dundee')),]


for(coefftouse in unique(allz2$coeff)){
  
  subz <- allz2NoAberDundee %>% filter(coeff==coefftouse)
  
  #To make width sensible with varying source number for different coeffs
  numsources <- unique(subz$source) %>% length
  print(paste0('Number of sources: ', numsources))
  
  #cos if they're all one value it can't cope
  if(mean(subz$sig)==1) {
    sigpoints <- c(16,4)} else {
      sigpoints <- c(4,16)}

  output <- ggplot(subz, aes(x = zones, y = Estimate, colour = rich, shape = factor(sig))) +
    geom_point(size = 5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = minn, ymax = maxx),width = 0.5, position = 'dodge') +
    coord_flip() +
    scale_shape_manual(values=sigpoints) +
    facet_wrap(~source, nrow = 1, scales = 'free_x') +
    ggtitle(paste0('Coefficient: ',coefftouse)) +
    theme(plot.title = element_text(face="bold",hjust = 0.5)) +
    guides(shape = guide_legend(title='p < 0.05'))
  
  ggsave(paste0('R_outputs/coefficientFacetPlots/AllCoefficents_Oct17_freeAxis/facet_',coefftouse,'.png'), 
         dpi = 300, height = 6, width = 3 + (numsources * 3))

}




