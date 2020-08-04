#regressions on GB data
#possibly with addition of cultural distance
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat','purrr','forcats')
lapply(geolibs, require, character.only = TRUE)

source("Function_CoBRegressionFunctions.R")

#I am tempted just to re-assemble the data again. We don't have a lot of it do we?
#What is there?
#At the moment, only econ active.
#Code for the weights probably easier to keep in function form. Hmm.

#Maybe just another hack of the function for now.
#Actually, yes, because we will want migshare-minus-own too.
#So what does it need?
#Just the original list of shapefiles. Need to check these all have the right names.


#Get the GB CoB shapefile data----
#GB
cob91 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
cob01 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')
cob11 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')

cobs <- list(cob91,cob01,cob11)
#Drop not-needed cols
cobs <- lapply(cobs, function(x) x[,c(2,4:ncol(x))])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CULTURAL DISTANCE sheet prep----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get sheet first just for countries we have from the CD data.
#And those are what?
#"Australia" "Banglad."  "China"     "France"    "Hong Kong" "India"     "Iran"      "Ireland"   "Italy" "Nigeria" "Pakistan"  "Poland"    "Romania"   "S. Africa" "Spain"     "Turkey"    "U.K."      "U.S.A."    "Zimbabwe"  "Germany"
#Names won't quite match so...
#Minus the UK.
cultdistCoBs <- c(
  "Australia","Bangladesh","China","France","Hong_Kong","India","Iran","Irish_Repu","Italy","Nigeria","Pakistan","Poland","Romania","South_Afri","Spain","Turkey","United_Sta","Zimbabwe","Germany"
)




#check match... tick!
#table(cultdistCoBs %in% names(cob91))

#check we have full UK match. tick!
#table(c('Channel_Is','UK_part_no','England','Wales','Northern_I','Scotland') %in% names(cob91))


#Keep all zones and cultural distance CoBs
sheet <- CoBtoRegressionReadyData(
  listOfSHPS = cobs,
  zonesToKeep = cobs[[1]]$label,
  CoBsToKeep = cultdistCoBs,
  UKcobs = c('Channel_Is','UK_part_no','England','Wales','Northern_I','Scotland'),
  year4weightsNmigshare = c(1991,2001),
  censusYear = c(1991,2001,2011))

#Save. Quite a while to run that.
saveRDS(sheet,'R_data/gbsheet1.rds')

#Check shares sum to 100. Tick!
#sum(sheet$xij1991[sheet$cob=='Pakistan'])



#ADD IN CULTURAL DISTANCE TO UK.
#Load in from that proj...
cd <- readRDS('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CulturalDistance/data/culturalDistanceCountryPairs.rds')

#Need to update names to match CoB data
#Does alphabetical set them in the right order?
cultdistCoBs <- cultdistCoBs[order(cultdistCoBs)]

cdcountries <- unique(cd$country1)[order(unique(cd$country1))]
#Exclude UK and that's a matching order
cdcountries <- cdcountries[c(1:17,19:20)]

cdwcobnames <- cd

#Better way to do this surely? Well, worked anyway.
for(i in 1:19){
  cdwcobnames$country1[cdwcobnames$country1 == cdcountries[i]] <- cultdistCoBs[i]  
  cdwcobnames$country2[cdwcobnames$country2 == cdcountries[i]] <- cultdistCoBs[i]  
}

#And if we keep just country1 as UK, that's all we need initially.
ukcd <- cdwcobnames %>% filter(country1 == 'U.K.')

#NEXT: Flag top ten (most culturally distant) and bottom ten (closest to UK). Oh wait, it's nineteen!
#Flag here then merge
ukcd <- ukcd[order(-ukcd$value),]

ukcd$cdtoUKflag <- 'closer'
ukcd$cdtoUKflag[1:10] <- 'further'


#Oh ps. di is dissimilarity index, 'value' is cultural distance. Obvs.
#So should be able to merge in distances into the sheet
sheet2 <- merge(sheet,ukcd[,c('country2','value','cdtoUKflag')],by.x = 'cob',by.y = 'country2', all.x = T)

saveRDS(sheet2,'R_data/culturaldistsheet2.rds')

#~~~~~~~~~~~~~~~~~~~~~~~~~
#Table of different CoB categories----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#A couple of things while we're here:
#How does the CD list of countries differ from Geoff's list?
#How does the CD list differ from the rich poor list? 
geoffsOrigList <- c( 'Irish_Repu', 'China', 'South_Afri', 'Africa_oth', 'South_Amer', 'Other_Midd', 'Other_East', 'Europe_oth', 'Australia', 'Canada', 'Nigeria', 'India', 'Pakistan', 'Hong_Kong', 'Malaysia', 'France', 'Germany', 'Italy', 'Netherland', 'Spain', 'Poland', 'United_Sta' ) 

#And the rich/poor split?
#Why did I take 'Other_Euro' out of rich? Because it's a mix?
rich <- c('Irish_Repu','South_Afri','Other_EU_m','Australia','Hong_Kong','France','Germany','Italy','Spain','United_Sta')

#Drop labels, UK-connected CoBs
#fullList <- names(cob91)[4:length(names(cob91))]
fullList <- names(cob91)[c(6:14,19:length(names(cob91)))]

#Make a little df for comparison
compare <- data.frame(fullList = fullList)
#drop first three non cob rows


compare$rich <- 0
compare$rich[compare$fullList %in% rich] <- 1
compare$geoffslist <- 0
compare$geoffslist[compare$fullList %in% geoffsOrigList] <- 1
compare$hasCD <- 0
compare$hasCD[compare$fullList %in% ukcd$country2] <- 1
compare$culturallyclose <- 0
compare$culturallyclose[compare$fullList %in% ukcd$country2[ukcd$cdtoUKflag=='closer']] <- 1

#Order by CD, save
compare <- compare %>% arrange(-hasCD)

table(geoffsOrigList %in% rich)
table(ukcd$cdtoUKflag)

#~~~~~~~~~~~~
#OUTPUT REGRESSIONS TO TEXT, SPLIT SAMPLE AND INTERACTION----

options(scipen=1)

modz <- lm(data = sheet2[sheet2$cdtoUKflag=='closer',], formula = xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991)

summary(modz)
print(summary(modz),digits=2)

outputTofile(inputdata = sheet2, splitByFactor = 'cdtoUKflag', title = 'Great Britain', file = 'regressionOutputs/3census/GreatBritain/GB_culturalDistanceSplitSample.txt', lmfunction = 'xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991')

#Check on dummy use
modz <- lm(data = sheet2, formula = xij2011 ~ xij1991*cdtoUKflag + w1991q*cdtoUKflag + migshareMinusOwn1991*cdtoUKflag)

summary(modz)

#Hmm
outputTofile(inputdata = sheet2, title = 'Great Britain interactions', file = 'regressionOutputs/3census/GreatBritain/GB_culturalDistanceInterations.txt', lmfunction = 'xij2011 ~ xij1991*cdtoUKflag + w1991q*cdtoUKflag + migshareMinusOwn1991*cdtoUKflag')


#output continuous-continuous interaction, though I'm still a bit puzzled how to interpret this
#Look first
modz2 <- lm(data = sheet2, formula = xij2011 ~ xij1991*value + w1991q*value + migshareMinusOwn1991*value)

summary(modz2)

#Output also
outputTofile(inputdata = sheet2, title = 'Great Britain interactions continuous', file = 'regressionOutputs/3census/GreatBritain/GB_culturalDistanceInterationsCOntinuous.txt', lmfunction = 'xij2011 ~ xij1991*value + w1991q*value + migshareMinusOwn1991*value')


#OUTPUT TO PLOT. Probably need to add specific lines
#xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991

#Or plot normally
modz <- lm(data = sheet2[sheet2$cdtoUKflag=='closer',], formula = xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991)

plot(sheet2[sheet2$cdtoUKflag=='closer','xij2011'],
     sheet2[sheet2$cdtoUKflag=='closer','xij1991'])

plot(modz)


#~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPARE SPLIT SAMPLE AND INTERACTION----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#Same difference? Yup.
splitcloser <- lm(data = sheet2[sheet2$cdtoUKflag=='closer',], formula = xij2011 ~ xij1991)
splitfurther <- lm(data = sheet2[sheet2$cdtoUKflag=='further',], formula = xij2011 ~ xij1991)

interactz <- lm(data = sheet2, formula = xij2011 ~ xij1991*cdtoUKflag)

summary(splitcloser)
summary(splitfurther)
summary(interactz)


#Same for all coeffs?
splitcloser2 <- lm(data = sheet2[sheet2$cdtoUKflag=='closer',], formula = xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991)
splitfurther2 <- lm(data = sheet2[sheet2$cdtoUKflag=='further',], formula = xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991)

interactz2 <- lm(data = sheet2, formula = xij2011 ~ xij1991*cdtoUKflag + w1991q*cdtoUKflag + migshareMinusOwn1991*cdtoUKflag)

summary(splitcloser2)
summary(splitfurther2)
summary(interactz2)

#~~~~~~~~~~~~~~~~~~~~~~~~~
#BREAK DOWN BY TTWA THEN SPLIT SAMPLE 1: prep TTWA data----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#First I need a list of the TTWAs matched against the labels. I have that somewhere. Err.
#I can't find it. Pretty sure I have done it but, OK, re-run.
#Using non-dissolved version but should work, same labels matched against.
ttwas <- readOGR('C:/Data/MapPolygons/GreatBritain/2001/TTWAs','greatBritainTTWAs')

#Centroids for GB cob zones
centroidz <- coordinates(cob01) %>% data.frame

#Cast to spatial points
coordinates(centroidz) <- ~X1+X2

#Steal projection as we know they match
proj4string(centroidz) <- proj4string(ttwas)

ttwaover <- centroidz %over% ttwas

#check ids...
ttwaover %>% sample_n(20)
#Ah bit of a mess of column order. Needs combining

ttwaovercombo <- ttwaover


ttwaovercombo$NAME <- as.character(ttwaovercombo$NAME)
ttwaovercombo$name_1 <- as.character(ttwaovercombo$name_1)
ttwaovercombo$label_1 <- as.character(ttwaovercombo$label_1)

ttwaovercombo$LABEL <- ifelse(is.na(ttwaovercombo$LABEL),ttwaovercombo$label_1,ttwaovercombo$LABEL)
ttwaovercombo$NAME <- ifelse(is.na(ttwaovercombo$NAME),ttwaovercombo$name_1,ttwaovercombo$NAME)

ttwaovercombo %>% sample_n(20)


table(is.na(ttwaovercombo$LABEL))
table(is.na(ttwaovercombo$NAME))

#What are the remaining NAs? Dunno but no data present. Centroids in no TTWA
ttwaovercombo[is.na(ttwaovercombo$NAME),]

#Best check where those are.
plot(cob01[row.names(ttwaovercombo[is.na(ttwaovercombo$NAME),]),])

#The row names start at 0. To be more sure, copy over the row with the NAs in.
#Use directly.
cob01$NAs <- ttwaovercombo$LABEL
table(is.na(cob01$NAs))

cob01$unassigned <- 0
cob01$unassigned[is.na(cob01$NAs)] <- 1

# row.names(ttwaovercombo[is.na(ttwaovercombo$NAME),])

#Look in QGIS, locate, set.
writeOGR(cob01,'QGIS/temp','wards_missingTTWAmatches', driver='ESRI Shapefile', overwrite_layer = T)

#And then boring lookup for those remaining 5.
#Using SPID so add in ttwa refs before doing this.
#10174 = 196 / Shetland Islands
#9626 = 223 / Ullapool & Gairloch
#9621 = 110 / Invergordon
#9777 = 155 / Mull & Islay
#10168 = 179 / Pitlochry

#OK, combine into the ward index
#Might as well use cob01
combo <- cob01[,c('SP_ID','label')]

#Will include those 5 NAs, will replace in a mo
combo$ttwaname <- ttwaovercombo$NAME
combo$ttwacode <- ttwaovercombo$LABEL

table(is.na(combo$ttwaname))

#Check unique ttwa names before and after to make sure I don't introduce any typos causing extras
#(That's assuming I'm not actually adding any more! Don't think so...)
unique(combo$ttwaname) %>% length

#Check is na then overwrite
combo$ttwaname[combo$SP_ID=='10174'] 
combo$ttwaname[combo$SP_ID=='10174'] <- 'Shetland Islands'
combo$ttwacode[combo$SP_ID=='10174'] <- '196'
combo$ttwaname[combo$SP_ID=='9626'] 
combo$ttwaname[combo$SP_ID=='9626'] <- 'Ullapool & Gairloch'
combo$ttwacode[combo$SP_ID=='9626'] <- '223'
combo$ttwaname[combo$SP_ID=='9621'] 
combo$ttwaname[combo$SP_ID=='9621'] <- 'Invergordon'
combo$ttwacode[combo$SP_ID=='9621'] <- '110'
combo$ttwaname[combo$SP_ID=='9777'] 
combo$ttwaname[combo$SP_ID=='9777'] <- 'Mull & Islay'
combo$ttwacode[combo$SP_ID=='9777'] <- '155'
combo$ttwaname[combo$SP_ID=='10168'] 
combo$ttwaname[combo$SP_ID=='10168'] <- 'Pitlochry'
combo$ttwacode[combo$SP_ID=='10168'] <- '179'

#eyeball no typo additions
unique(combo$ttwaname)[order(unique(combo$ttwaname))]

#Yup, good. Save. Phew.
writeOGR(combo,'R_data','greatBritainWardsPCS_TTWAlookup',driver='ESRI Shapefile')

write.csv(combo %>% data.frame(), 'R_data/greatBritainWardsPCS_TTWAlookup.csv', row.names = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~
#BREAK DOWN BY TTWA THEN SPLIT SAMPLE 2: actually using----
#~~~~~~~~~~~~~~~~~~~~~~~~~

ttwalookup <- read_csv('R_data/greatBritainWardsPCS_TTWAlookup.csv')

#Based on sheet2 above. Check we have full match on labels. Tick!
table(sheet2$label %in% ttwalookup$label)

sheet2_w_ttwa <- merge(sheet2,ttwalookup, by = 'label')

#Then... running for each TTWA, for close, distant. Store coeffs. Coding from scratch, vexingly.
#Or copying own training...

#Check a single regression, get indices of number positions for putting into single dataframe
z <- summary(lm(data = sheet2, formula = xij2011 ~ xij1991 + w1991q + migshareMinusOwn1991))

z$coefficients

# models_closer <- sheet2_w_ttwa %>%
#   filter(cdtoUKflag == 'closer')
#   split(.$ttwaname) %>%
#   map(~lm(, data = .))

#~~~~~~~~~~~~~~~~~~~
#ADD GB EMPLOYMENT TO SHEET----
#~~~~~~~~~~~~~~~~~~~

#I should really be separating out sheet prep huh? Oh well.
#Just 1991 employment
econactive91 <- read.csv('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991econActive.csv')

#Tick
table(econactive91$label %in% sheet2_w_ttwa$label)

sheet2_w_ttwa <- merge(sheet2_w_ttwa,econactive91[,c(1,5)],by = 'label')

sheet2_w_ttwa <- sheet2_w_ttwa %>% 
  rename(percentEmployed1991 = percentEmployed)

#save as sheet2
#Note: the order in this document is COMPLETELY FUCKED.
#Some sheet2 created below this point, some above.
saveRDS(sheet2_w_ttwa,'R_data/culturaldistsheet2_w_acd_toUK_n_employment.rds')
sheet2 <- readRDS('R_data/culturaldistsheet2_w_acd_toUK_n_employment.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~
#TTWA again: ACD per TTWA before running separate regressions----
#~~~~~~~~~~~~~~~~~~~~~~~~~
  
#So average cultural distance for each entire TTWA.
#Just ACD to UK. So that's the same for each non-UK country per zone.
#So there's a per-zone value to average.

#Just need a single value per zone then.
zoneACDsToUK <- sheet2 %>% 
  filter(cob == 'Australia') %>% 
  dplyr::select(label,acd_justToUK,ttwaname)

#Kay, now we can group by TTWA and get a per-TTWA ACD to UK
zoneACDsToUK <- zoneACDsToUK %>% 
  group_by(ttwaname) %>% 
  mutate(TTWA_acd_justToUK = mean(acd_justToUK))

#Kay. What's that look like?
#One value per TTWA plz
ggplot(zoneACDsToUK %>% group_by(ttwaname) %>% summarise(perTTWAacd = max(TTWA_acd_justToUK)), 
       aes(perTTWAacd)) +
  geom_density()



#~~~~~~~~~~~~~~~~~~~~~~~~~
#LOOKING AT SOME PLOTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~

#What does this data look like? Reduce to the set we want to look at.
#sheetsmall <- sheet2 %>% dplyr::select(-one_of(cob,xij2001,label,migshareMinusOwn2001,w2001q,value))
sheetsmall <- sheet2 %>% dplyr::select(-cob,-xij2001,-label,-migshareMinusOwn2001,-w2001q,-value)

sheetsmall$cdtoUKflag <- factor(sheetsmall$cdtoUKflag)

#sample
sheetsmall <- sheetsmall %>% sample_frac(0.1)

#pairs(sheetsmall[,c(1:4)], panel = panel.smooth)
pairs(sheetsmall[,c(1:4)], panel = panel.smooth)
pairs(sheetsmall, panel = panel.smooth)

#Let's reduce the regression to a simpler form to think through
modz <- lm(data = sheet2, formula = xij2011 ~ xij1991*cdtoUKflag)
summary(modz)

#Check xij1991 ... OK, done the maths. Now let's plot and see.
#And let's actually set a 0/1 flag to do it properly, not rely on lm.
#https://stackoverflow.com/questions/14994870/converting-a-factor-with-2-levels-to-binary-values-0-1-in-r
sheet2$cdtoUKflagbinary <- 0
sheet2$cdtoUKflagbinary[sheet2$cdtoUKflag=='further'] <- 1
table(sheet2$cdtoUKflagbinary)

#OK, so. The simplest interaction just on xij itself.
#xij2011 = a + b*xij91 + c*xij91*CD[0/1]
#        = a + xij91(b + c*CD[0/1])

#A pain again, let's sample
sheetsample <- sheet2 %>% sample_frac(0.1)

plot(sheetsample$xij2011 ~ sheetsample$cdtoUKflagbinary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Question about migshare minus own---- 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Are the lower values because
#Shares are much smaller for whole of GB?
#Can I bring in Scots sheet to compare values?
#Does this have what I neeeeed? Yup, should do.
sheet5 <- readRDS('R_data/sheet5.rds')
unique(sheet5$cob)

#So can just pick out Pakistan from both as an example, yes?
scotPak <- sheet5[sheet5$cob=='Irish_Repu',]
gbPak <- sheet2[sheet2$cob=='Irish_Repu',]

summary(scotPak$xij2011)
summary(gbPak$xij2011)

scotline <- lm(xij2011 ~ migshareMinusOwn1991, scotPak)
gbline <- lm(xij2011 ~ migshareMinusOwn1991, gbPak)

summary(scotline)

#slope = 0.022930
plot(scotPak$xij2011 ~ scotPak$migshareMinusOwn1991)
abline(scotline)

summary(gbline)

#slope = 0.00135581
plot(gbPak$xij2011 ~ gbPak$migshareMinusOwn1991)
abline(gbline)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#AV CULT DIST FOR EACH COB----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sheet2 <- readRDS('R_data/culturaldistsheet2.rds')

#Load in per-CoB ACDs from the cultural dist project  
#Has each CoB's cult distance to everyone else in each zone (currently not including UK)
#So needs longamating and attaching to sheet2. 
#Country names don't match, I don't think...
acds <- readRDS("C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CulturalDistance/data/allCountries_toNeighboursCulturalDistance_excUK.rds")

unique(sheet2$cob)
names(acds)[39:57]

#Easier to change names back in cult dist while columns
#In theory could just replace names vector
#But don't trust the overwrite, it's a bit much
#acds <- acds %>% 
# rename(`Banglad.` = Bangladesh, )

#On the other hand, that's a pain in the arse.
#Let's keep record and check
chk <- names(acds)[39:57]
names(acds)[39:57] <- unique(sheet2$cob)

#acdschk <- readRDS("C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CulturalDistance/data/allCountries_toNeighboursCulturalDistance_excUK.rds")
#Tick
#names(acds)[39:57]
#names(acdschk)[39:57]

#Now long, then should match
acdslong <- gather(acds[,c(2,39:57)], key = cob, value = acd_noUK, Australia:Zimbabwe)

#Should both be same length now. Tick!
nrow(acdslong)
nrow(sheet2)
  
#And merge should be same size
sheet2 <- merge(sheet2,acdslong,by = c('label','cob'))

saveRDS(sheet2,'R_data/culturaldistsheet2_w_acd_nouk.rds')
sheet2 <- readRDS('R_data/culturaldistsheet2_w_acd_nouk.rds')


#PLOTS COMPARING ACD WITH SPATIAL SHARES, JUST TO CHECK
#So by country... (and maybe log for shares)
output <- ggplot(sheet2, aes(x = acd_noUK, y = xij2011)) +
  geom_point() +
  facet_wrap(~cob, scales = 'free')

ggsave('R_outputs/cd_vs_shares1.png',output,dpi=300,width=13,height=10)

#~~~~~~~~~~~~~~~

#Add column for ACD just to UK.
#i.e. all non-UK countries are average denominator
#Each value is each non-UK person CD to UK.
acdstouk <- readRDS('C:/Users/admin/Google Drive/CulturalDistance/data/acd1.rds')

#Only need the final ACD column and label for merge
acdstouk <- acdstouk[,c(2,39)]

#Is already long in this new format I seem to have gone over to.
#So merge straight in.
#Check. Tick.
table(acdstouk$label %in% sheet2$label)

#Change to better name
acdstouk <- acdstouk %>% rename(acd_justToUK = `U.K.`)

sheet2 <- merge(sheet2, acdstouk, by = 'label')

saveRDS(sheet2,'R_data/culturaldistsheet2_w_acd_toUK.rds')
sheet2 <- readRDS('R_data/culturaldistsheet2_w_acd_toUK.rds')

#~~~~~~~~~~~~~~~
#Repeat for excluding self (via remote server)----
acds <- readRDS("C:/Users/admin/Google Drive/CulturalDistance/data/allCountries_toNeighboursCulturalDistance_excUK_excSelf.rds")

#AH, DIFFERENT ORDER THIS TIME. Err. Wonder why?
unique(sheet2$cob)
names(acds)[39:57]

#Can still use to rename: same order, just shunted round one with Oz now last
names(acds)[39:56] <- c(unique(sheet2$cob)[2:19])

acdschk <- readRDS("C:/Users/admin/Google Drive/CulturalDistance/data/allCountries_toNeighboursCulturalDistance_excUK_excSelf.rds")

#Tick again!
names(acds)[39:57]
names(acdschk)[39:57]

#Now long, then should match
acdslong <- gather(acds[,c(2,39:57)], key = cob, value = acd_noUKnoSelf, Bangladesh:Australia)

sheet2 <- merge(sheet2,acdslong,by = c('label','cob'))

saveRDS(sheet2,'R_data/culturaldistsheet2_w_acd_nouk_excSelf.rds')

#save as CSV for QGIS
write.csv(sheet2,'R_data/culturaldistsheet2_w_acd_nouk_excSelf.csv', row.names = F)

#Actually, think I need to save separate countries


output <- ggplot(sheet2, aes(x = acd_noUKnoSelf, y = xij1991)) +
  geom_point() +
  facet_wrap(~cob, scales = 'free')

ggsave('R_outputs/cd_vs_shares1_excSelf1991.png',output,dpi=300,width=13,height=10)




#~~~~~~~~~~~~~~~
#Repeat for excluding self INCLUDING UK----
acds <- readRDS("C:/Users/admin/Google Drive/CulturalDistance/data/allCountries_toNeighboursCulturalDistance_INC_UK_excSelf.rds")

sheet2 <- readRDS('R_data/culturaldistsheet2_w_acd_nouk_excSelf.rds')

#AH, DIFFERENT ORDER THIS TIME. Err. Wonder why?
unique(sheet2$cob)
names(acds)[39:57]

#Can still use to rename: same order, just shunted round one with Oz now last
names(acds)[39:56] <- c(unique(sheet2$cob)[2:19])

acdschk <- readRDS("C:/Users/admin/Google Drive/CulturalDistance/data/allCountries_toNeighboursCulturalDistance_excUK_excSelf.rds")

#Tick again!
names(acds)[39:57]
names(acdschk)[39:57]

#Now long, then should match
acdslong <- gather(acds[,c(2,39:57)], key = cob, value = acd_INCUKnoSelf, Bangladesh:Australia)

sheet2 <- merge(sheet2,acdslong,by = c('label','cob'))

saveRDS(sheet2,'R_data/culturaldistsheet2_w_acd_INCUK_excSelf.rds')

#save as CSV for QGIS
#write.csv(sheet2,'R_data/culturaldistsheet2_w_acd_nouk_excSelf.csv', row.names = F)

#Actually, think I need to save separate countries


output <- ggplot(sheet2, aes(x = acd_INCUKnoSelf, y = xij2011)) +
  geom_point() +
  facet_wrap(~cob, scales = 'free')

ggsave('R_outputs/cd_vs_shares1_INCUK_excSelf2011.png',output,dpi=300,width=13,height=10)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#OUTPUT SOME DATA FOR MAPS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#E.g. bangladesh, india, pakistan
#As separate sheets so can be linked directly in QGIS
write.csv(sheet2 %>% filter(cob == 'Bangladesh'), 'R_data/culturalDistsubSheets/bangladesh.csv', row.names = F)
write.csv(sheet2 %>% filter(cob == 'India'), 'R_data/culturalDistsubSheets/india.csv', row.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#EACH-COB CULTURAL DIST REGRESSIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

options(scipen = 999)

#base
base <- lm(data = sheet2, formula = xij2011 ~ xij1991)
summary(base)


interactz <- lm(data = sheet2, formula = xij2011 ~ xij1991*acd_noUKnoSelf)
summary(interactz)


#What about including self (though I think that's compromised)
interactz2 <- lm(data = sheet2, formula = xij2011 ~ xij1991*acd_noUK)
summary(interactz2)


#Including UK / excluding self
interactz3 <- lm(data = sheet2, formula = xij2011 ~ xij1991*acd_INCUKnoSelf)
summary(interactz3)



#Let's use these two for comparison:
interactz_a <- lm(data = sheet2, formula = xij2011 ~ xij1991 + xij1991:acd_noUKnoSelf)
summary(interactz_a)

interactz3a <- lm(data = sheet2, formula = xij2011 ~ xij1991 + xij1991:acd_INCUKnoSelf)
summary(interactz3a)



#What about the original value? Also, are they correlated?
plot(sheet2$value ~ sheet2$acd_noUK)

#Hmm...
sheetsample <- sheet2 %>% sample_frac(0.1)

#This is weird!
plot(sheetsample$value ~ sheetsample$acd_noUK)
#This is predictable.
plot(sheetsample$acd_noUKnoSelf ~ sheetsample$acd_noUK)

#
plot(sheetsample$acd_noUKnoSelf ~ sheetsample$acd_INCUKnoSelf)

#
plot(sheetsample$value ~ sheetsample$acd_INCUKnoSelf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Looking at the mix in high-share zones----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Are they consistent - that is, is there a reason for them peaking at a particular point?
#Need cob11percents again...
cob11b <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_DX/StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/11CoB_threeCensus.csv')

cob11b <- cob11b %>%
  mutate(`U.K.` = Channel_Is + UK_part_no + England + Wales + Scotland + Northern_I)

cob11b$totalpop <- apply(cob11b %>% dplyr::select(Channel_Is:China),1,sum)

#Updated so UK is used in zone %s not the separate UK bits
#I'M GOING TO EXCLUDE UK FROM THIS ONE THOUGH
cob11percents <- cob11b %>% 
  mutate_at(c(6:14,19:37), funs(./totalpop))

table(apply(cob11percents %>% dplyr::select(c(6:14,19:37)),1,sum))

#Probably best to drop the columns that haven't been turned into %s
#To avoid confusion later
#cob11percents <- cob11percents[,!(names(cob11percents) %in% c('Channel_Is','UK_part_no','England','Scotland','Wales','Northern_I'))]

#REMOVE ALL UK TYPE COBS
cob11percents <- cob11percents[,!(names(cob11percents) %in% c('Channel_Is','UK_part_no','England','Scotland','Wales','Northern_I','U.K.'))]



#So now we're just selecting zones, looking at them
#INDIA: Zones above 0.2% shares
indiasub <- sheet2 %>% filter(cob=='India',xij2011 > 0.2)

#Select those percent zones
percents <- cob11percents %>% filter(cob11percents$label %in% indiasub$label)

#percentslong <- percents[,c(2,4:31)] %>% gather(key = country, value = percent, c(2:29))#shorter at end
#No UK
percentslong <- percents[,c(2,4:30)] %>% gather(key = country, value = percent, c(2:28))#shorter at end

percentslong$country <- factor(percentslong$country)

percentslong$country <- reorder(percentslong$country,percentslong$percent)


#There's an argument for only using the countries we've used in the cultural dist calcs
#But for now...
ggplot(percentslong, aes(x = label, y = percent, fill = country)) +
  geom_col() +
  coord_polar(theta = "x")

x = 1



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MOAR PLOTS for full sheet2----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

sheet2 <- readRDS('R_data/culturaldistsheet2_w_acd_toUK_n_employment.rds')

#Prior to Rmding up the regressions.

#Looking just at one country
acdcompare <- sheet2 %>% 
  filter(cob %in% c('Pakistan','France')) %>% 
  dplyr::select(cob,acd_noUKnoSelf,acd_INCUKnoSelf,acd_justToUK) %>% 
  gather(key = type, value = acd, acd_noUKnoSelf:acd_justToUK)

ggplot(acdcompare,aes(x = acd, colour = type)) +
  geom_density() +
  facet_wrap(~cob)

ggplot(sheet2 %>% filter(cob=='Pakistan'), aes(x = acd_INCUKnoSelf, y = acd_justToUK)) +
  geom_point()

ggplot(sheet2 %>% filter(cob=='Pakistan'), aes(x = acd_INCUKnoSelf, y = acd_noUKnoSelf)) +
  geom_point()
  
  
#Spread of values...
ggplot(sheet2, aes(x = cob, y = acd_noUKnoSelf)) +
  geom_boxplot()













  
