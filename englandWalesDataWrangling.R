#England / wales data wrangling
#Get it all into shape prior to aggregating / intersecting etc
#Plus verifying against shapefiles etc.
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat')
lapply(geolibs, require, character.only = TRUE)

#source("Function_DataFromSmalltoLargeGeog.R")
#source('Function_Shapefile_uniqueIDs_to_singleRowFeatures.R')
#source("Function_bulkColumnRecode.R")

source("Function_CoBRegressionFunctions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~3 CENSUS~~~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~
#COUNTRY OF BIRTH----

#1991: LBS is 'total persons'
#2001: univariate tables, again single column, no M/F.

#Both are currently separate files for England and Wales. Combine them.
#1991
eng <- read_csv('1991/England/England_LBS_ward_CoB_totalPersons/England_LBS_ward_CoB_totalPersons.csv')
wales <- read_csv('1991/Wales/Wales_LBS_ward_CoB_totalPersons/Wales_LBS_ward_CoB_totalPersons.csv')

combo <- rbind(eng,wales)

write_csv(combo,'1991/EnglandWales/EnglandWales_LBS_ward_CoB_totalPersons.csv')

#2001
eng <- read_csv('2001/England/2001_England_CoB_univariatetable_OA.csv')
wales <- read_csv('2001/Wales/2001_Wales_CoB_univariatetable_OA.csv')

combo <- rbind(eng,wales)

write_csv(combo,'2001/EnglandWales/2001_EnglandWales_CoB_OA_totalPersons.csv')

#~~~~~~~~~~~~~
#Checking 2011 CoB sources----

#Check on 2011 CoB categories I can get via NOMIS
#https://www.nomisweb.co.uk/census/2011/qs203ew

#Only a region at a time. 78 categories. The right ones not to lose any resolution?
#InFuse appears to be useless: 328 categories you need to select manually
#Then they're mostly empty in England (though populated for Wales and NI. And Scotland appears to be there too.)
sy <- read_csv('2011/England/Yorks_CoB_OA_NOMIS.csv')
wales <- read_csv('2011/England/Wales_CoB_OA_NOMIS.csv')

# namez <- names(sy) %>% gsub('; measures: Value|Country of Birth: |Europe: |United Kingdom: |Other Europe: |EU Countries: |Member countries in March 2001: |Accession countries April 2001 to March 2011: |Rest of Europe: |Africa: |','', .)

namez <- names(sy) %>% gsub('; measures: Value|Country of Birth: |Europe: |United Kingdom: ','', .)

#What names do we need? Scots sample to compare to.
scots <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.csv')

currentNamez <- names(scots)[4:42]

#Just need to manually check. 
capture.output(c(noquote(namez)), file = "R_data/checkNames.txt")
capture.output(c(noquote(currentNamez)), file = "R_data/checkNames.txt",append = T)

#Conclusion:
#InFuse gets closest to useful with its CoB (UK harmonised) table
#BUT IT IS MISSING CATEGORIES THAT NOMIS HAS IN ITS CSV DOWNLOAD.
#But NOMIS only lets you download one region at a time if you're getting OAs.
#(Presuming I am, in fact, getting OAs. I need to check that.)

#Geography appears correct
#now to get all regions and check all columns are populated
#Downloaded from https://www.nomisweb.co.uk/census/2011/qs203ew

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Check NOMIS regional CoB, combine if OK----

#Get them all. All in one folder
filez <- list.files(path = '2011/England/NOMIS_CoB_OA_EngWalesRegional',full.names = T)

nomisCOBs <- lapply(filez, function(x) read_csv(x))

#Check each has all positive values for each CoB
#Sum each region, turn each into own dataframe column
#Yup, those are all positive
df <- sapply(nomisCOBs, function(x) apply(x[,c(8:82)],2,sum)) %>% data.frame

#That's actually quite a useful breakdown. I should remember that's here and use it.
#And add regional breakdown to the final data.

#OK, so combine all those into one file for England/Wales.
#I can create a regional lookup later to attach to the ward/PCS shapefile.

nomisCOBcombo <- do.call(rbind,nomisCOBs)

#Unique OAs? Tick.
unique(nomisCOBcombo$geography) %>% length

#SAVE!
write_csv(nomisCOBcombo,'2011/EnglandWales/EngWales_CountryOfBirth_OA_2011_NOMIS.csv')

#~~~~~~~~~
#2011 sensible names----

#The names come with it: but remove regional sums and reduce columns
cob11 <- read_csv('2011/EnglandWales/EngWales_CountryOfBirth_OA_2011_NOMIS.csv')

#I have a name printout to stare at...
#Note the rural/urban col has nothing useful in it.

#geography and geography code are exactly the same field content, right? Yup.
#So don't want both.
#all.equal(cob11$geography,cob11$`geography code`)
table(cob11$geography==cob11$`geography code`)

#remove some easy bits
names(cob11) <- gsub("Country of Birth: |; measures: Value","",names(cob11))

#Keep only the last chunk after the colon (plus space)

#This for getting last element. 
#This for getting last element from list contents:
#http://stackoverflow.com/questions/36143119/how-to-get-last-subelement-of-every-element-of-a-list-in-r
names(cob11) <- strsplit(names(cob11),": ") %>% lapply(tail, n = 1) %>% unlist

#Can remove all columns called "Total" (as well as some at the start)
cob11 <- cob11[,-c(1,3,4,5)]
#keepz <- names(cob11)[names(cob11)!='Total']
#Well that's very unobvious...
#https://gist.github.com/djhocking/62c76e63543ba9e94ebe
#cob11 <- cob11 %>% dplyr::select_(.dots = keepz)

#Try again: work out which indices to remove
rmz <- which(names(cob11)=='Total')
cob11 <- cob11[,-rmz]

#Getting there! Look through and see. Yup, that's it.
write_csv(cob11,'2011/EnglandWales/EngWales_CountryOfBirth_OA_2011_NOMIS_countryNamesAdded.csv')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1991/2001 COB sensible names----

#Should in theory be same as Scotland data but need to check
cob91 <- read_csv('1991/EnglandWales/EnglandWales_LBS_ward_CoB_totalPersons.csv')
cob01 <- read_csv('2001/EnglandWales/2001_EnglandWales_CoB_OA_totalPersons.csv')
#Ah, 2011 won't be, but it already has the names in place
#So leave it alone
#cob11 <- read_csv('')

#Lord. It might be easier just to add them manually. Some will still be region headings.
#Let's look at CASWEB and figure it out.
#OK, so staring at CASWEB, let's first remove any sum columns.
cob91 <- cob91[,-c(3,4,5,6,12,17,21,22,23,29,32,37,46,47,52,56,63,74,90,98,103,104,108)]

#In theory, that should now match the number of countries we have for Scotland LBS 91 pre-combine?
namez <- read_csv('VariableCoding/1971to2011_CountryOfBirthCategories_Scotland_noRegionHeadings_allCols.csv')

#Does appear to be correct, let's just eyeball...
chk <- data.frame(now = names(cob91),then = namez[,4])

#Yes, all matches. Good good. Right!
names(cob91) <- unlist(namez[,4])

write_csv(cob91,'1991/EnglandWales/EnglandWales_LBS_ward_CoB_totalPersons_countryNamesAdded.csv')

#01 takes yonks to load, work on copy.
cob01a <- cob01

#Again, stripping out sum columns
cob01a <- cob01a[,-c(2,3,4,12,28,38,40,45,51,52,57,65,72,79)]

#So: MISTAKE IN PREVIOUS DELETION FOR SCOTLAND. Just one mistake: 
#"Non EU Countries in Western Europe" (0026)
#But of course it knocks everything else out of whack. Marvelous!
#Anyway, here it means the remaining columns should be one more than I previously used...
old2001names <- namez[!is.na(namez[,5]),5]

#So if I splice that category back in, it should be correct.
#This was the easiest way of getting dplyr'd df back to vector. Hmm.
old2001names <- data.frame(old2001names)
old2001names <- old2001names[,1]

#splice in the missing Europe cat between Sweden and Albania
old2001names <- c(old2001names[1:22],'Non.EU.Countries.in.Western.Europe',old2001names[23:68])

#eyeball against each other and CASWEB
chk <- data.frame(new = names(cob01a), old = old2001names)

#Yes, correct. Apply names and save.
names(cob01a) <- old2001names

write_csv(cob01a,'2001/EnglandWales/2001_EnglandWales_CoB_OA_totalPersons_countryNamesAdded.csv')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~COB: CHECKING ENGWALES vs SCOTS ON EACH DECADE~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#That is e.g. for 2001, making a single GB file. So getting those categories the same 
#because Scotland is a bit different to Eng/Wales
#Before then harmonising across censuses. Delightful!

#1991----
engWales91 <- read_csv('1991/EnglandWales/EnglandWales_LBS_ward_CoB_totalPersons_countryNamesAdded.csv')
scot91 <- read_csv('1991/Scotland//1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv')

#Ah, think those two are the same
gsub("\\."," ",names(engWales91))==names(scot91)

#Annoying... EYEBALL!! 
chk <- data.frame(names(engWales91),names(scot91))

#Yup, match. So make into GB. Assuming the IDs are unique, right? Right.
table(engWales91$Zone.ID %in% scot91$`Zone ID`)

#Cos we just checked they're identical (but formatting is different)
names(engWales91) <- names(scot91)
gb91 <- rbind(engWales91,scot91)

write_csv(gb91,'1991/GreatBritain/1991_GreatBritain_LBS_CoB_Ward_n_PCS_countyNamesAdded.csv')

#2001----

#Big file!
engWales01 <- read_csv('2001/EnglandWales/2001_EnglandWales_CoB_OA_totalPersons_countryNamesAdded.csv')
#Ah - this is where the missing "Non.EU.Countries.in.Western.Europe" is. Need to get that back.
#Let's just check I'm right that it's really not a sum column... newp, it's not. Shouldn't have deleted.
#OK, updated. Reload, double-check.
scot01 <- read_csv('2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv')

names(engWales01)
names(scot01)

chk <- data.frame(names(engWales01),names(scot01))

#Yup, all good now. Save!
names(engWales01) <- names(scot01)
gb01 <- rbind(engWales01,scot01)

write_csv(gb01,'2001/GreatBritain/2001_GreatBritain_CoB_OA_countyNamesAdded.csv')

#2011----

#Think this might be the one with the awkward non-matching categories.
engWales11 <- read_csv('2011/EnglandWales/EngWales_CountryOfBirth_OA_2011_NOMIS_countryNamesAdded.csv')
scot11 <- read_csv('2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv')

chk <- data.frame(names(engWales11),c(names(scot11),'-'))

#Won't let me look at that properly. Save and excel!
write_csv(chk,'R_data/bits/looksee.csv')

#Check matches. 2011 Scots is long-name.
sapply(names(engWales11),function(x) grepl(x,names(scot11)))  

#Some notes from looking:
#EngWales has North Africa. That will have to go into "other". Not present elsewhere.

#UPDATE 2011 TO MAKE ENG/WALES AND SCOTLAND CONSISTENT BEFORE COMBINING----
#From this 'ere printout wot I am lookin at
#Let's use the thingyo
combo <- list(
  
  list(engWales11,scot11),#all data to sum columns on
  list('1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:11','7'),
  list('UK part not specified','6,7','6'),#Oops, Isle of Man is in here too. And I double counted N Ire
  list('Other EU member countries','16,18','11,13,15'),
  list('Other EU accession countries','22','16:18,22:23'),
  list('Other Europe','24','24,26'),
  list('Other Central and Western Africa','26,28','28'),
  list('Other South and Eastern Africa','30,33','32'),
  list('Other Middle East','36','34:35'),
  list('Other Southern Asia','43,44','42'),
  list('Other South-East Asia','46','43,45,46'),
  list('Other Eastern Asia','39','38'),
  list('Other North America','49','48,50'),
  list('Caribbean','52:53','53'),
  list('Antarctica other Oceania','54,57','56'),
  list('Rest of world','25,34,56,58','55,57')#includes north africa from eng/wales
  
)

result <- recode(combo)
#Should be able to combine those two parts now...
chk <- result[[2]]


#Get a list of all those not containing other then check for matches across both
# engWales11noOther <- engWales11[,!grepl('other',names(engWales11),ignore.case = T)]
# #Yup, that'll do. 
# names(engWales11noOther)
# names(engWales11)[!(names(engWales11) %in% names(engWales11noOther))]

#So periods in the scots names
names(scot11)[grepl('south',names(scot11),ignore.case = T)]

#Will be in the right index orderrrr oh no, engNamez won't be. OK, so...
#Slightly more faff

#1. List of no-"other" names indexed by original column number
engWales11noOther <- data.frame(engcob = names(engWales11), engnum = c(1:ncol(engWales11)))
  
#Subset of names that don't contain 'other'
engWales11noOther <- engWales11noOther[!grepl('other',engWales11noOther$engcob,ignore.case = T),]

#Create index for scots names...
#Wait, this is a silly way of doing this, surely? Actually, no...
#scotNamez <- data.frame(scotcob = gsub('\\.',' ',names(scot11)), scotnum = 1:ncol(scot11))
scotNamez <- gsub('\\.',' ',names(scot11))

#can't see a non-loop way
#engWales11noOther$scotindex <- NA
engWales11noOther$scotindex <- lapply(engWales11noOther$engcob,function(x) which(grepl(x,scotNamez,ignore.case = T)))
#engWales11noOther$scotindex2 <- lapply(engWales11noOther$scotindex,function(x) ifelse(length))

#OK! Phew. Eyeball to see which of those are useable.
#table(length(engWales11noOther$scotindex[2])==0)

#Sod it, manually pull out those to keep
row.names(engWales11noOther) <- 1:nrow(engWales11noOther)

#Hong kong missing. It's in both...
keepz <- engWales11noOther[c(2:5,9:12,14:18,21:22,24:27,29:31,33:37,40),]
  
#Just gotta fix the china/hong-kong thing.
keepz$scotindex[keepz$engcob=='China'] <- 36
#And Ireland (got lost cos of the N Ire thing)
keepz$scotindex[keepz$engcob=='Ireland'] <- 8
#And Spain!
keepz$scotindex[grepl('Spain',keepz$engcob)] <- 14

keepz$engcob <- as.character(keepz$engcob)
keepz <- rbind(keepz,c('Hong Kong',38,37))



#So now: keep only those columns from Eng and Scot, change order to match
#Just order first so Hong Kong is in a sensible place
keepz <- keepz[order(as.numeric(keepz$engnum)),]

engSingles <- engWales11[,as.numeric(keepz$engnum)]
scotSingles <- scot11[,as.numeric(keepz$scotindex)]

#Correct column order?
chk <- data.frame(names(engSingles),names(scotSingles))

#damn european countries... Yup, match.
write_csv(chk,'R_data/bits/lookseeagain.csv')

#Now need to join up the four pieces
engAll <- cbind(engSingles,result[[1]])
scotAll <- cbind(scotSingles,result[[2]])

#Just checked column names are right so should be good here. But... 
write_csv(data.frame(names(engAll),names(scotAll)),'R_data/bits/lookseeagain.csv')

#Yup, good. Just a couple of changes to make...
names(scotAll) <- names(engAll)

allAll <- rbind(scotAll,engAll)

#shift zone code column to the start
allAll <- allAll[,c(30,1:29,31:45)]

#Shorten Hong Kong name
names(allAll)[grepl('Hong',names(allAll),ignore.case = T)] <- 'Hong Kong'
names(allAll)[grepl('Spain',names(allAll),ignore.case = T)] <- 'Spain'

#Think that might be it!
write_csv(allAll,'2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')
#allAll <- read_csv('2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')

#Check no categories were missed:
chk <- data.frame(names(engWales11),c(names(scot11),'-'),c(names(allAll),rep('-',16)))

#Looks... OK? No... Ireland I think is missing. Fixed. Faff!
write_csv(chk,'R_data/bits/lookseeagain.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#HARMONISE GB COB DECADES INTO THE SAME CATEGORIES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load em and look
gb91 <- read_csv('1991/GreatBritain/1991_GreatBritain_LBS_CoB_Ward_n_PCS_countyNamesAdded.csv')
gb01 <- read_csv('2001/GreatBritain/2001_GreatBritain_CoB_OA_countyNamesAdded.csv')
gb11 <- read_csv('2011/GreatBritain/2011_GreatBritain_CoB_OA_countyNamesAdded.csv')

#Are names roughly the same format? Yup.
lapply(list(gb91,gb01,gb11),names)

#Save that to stick in spreadsheet and remove as we combine
chk <- data.frame(gb91 = names(gb91),gb01 = c(names(gb01),rep('-',23)),gb11 = c(names(gb11),rep('-',47)))

write_csv(chk,'R_data/bits/3census_gb_removes.csv')

#A couple of 91 countries need their names changing to make sure they match with the other two
names(gb91)[grepl('United States',names(gb91))] <- 'United States'
names(gb91)[grepl('China',names(gb91))] <- 'China'

#Code from scots 3 census stitch...
foundMatches <- list()
#List grows one at a time...
j = 1

#cycle through all names in 91LBS (has the most categories)
for (i in 3:ncol(gb91)) {
  
  #Ignore these matches
  #"other" will vary across years and need checking
  #ignorz <- c("Channel.Islands","Isle.of.Man","other")
  #Actually, just going to remove them manually
  
  two001 <- which(grepl(names(gb91)[i],names(gb01), ignore.case = T))
  two011 <- which(grepl(names(gb91)[i],names(gb11), ignore.case = T))
  
  #print(length(two011))
  
  #need matches for both
  if(length(two011) != 0 & length(two001) != 0) {
    
    foundMatches[[j]] <- list(names(gb91)[i],i,two001,two011)
    
    j <- j + 1
    
  }
  
}

#check what that looks like
for(i in 1:length(foundMatches)){
  
  print(paste(i,foundMatches[i][[1]][1],
              foundMatches[i][[1]][2],
              foundMatches[i][[1]][3],
              foundMatches[i][[1]][4],
              sep = " "))
  
}

#remove channel islands/Isle of man
#And other middle east
#And other Europe
#And South America. 
#AND Caribbean... ARGH
#All done in the recode below
foundMatches <- foundMatches[-c(5,6,21,24,25,27)]


#Passing everything in to function to make sure names across everything remains consistent
#The second list: indexes columns to keep from the dataframes to re-attach after re-coding
#All others: common variable name and, for each dataframe, columns to sum
#(or just a single column)
#Add in 91 SAS
threeCensusCombo <- list(
  list(gb91,gb01,gb11),#all data to sum columns on
  list('1,2','1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:9','9','31'),
  list('UK part not specified','7','6','32'),
  list('Irish Republic','10,11','7,8','6'),#Ireland part not specified is already added in 2011, so do the same for 91/01
  list('South Africa','77','40','17'),#didn't get from single name search cos it's "afria" in '91, search missed it
  list('Africa other','16:19,21:23,25,78,44:45','34,36:37,39,42','36:37'),#excludes North Africa due to '11 scots eng mismatch
  list('Caribbean','26:33,80','62:63','43'),
  list('South America','81:82','66','28:29'),
  list('Other Middle East','84:85','45:46','38'),
  #list('Other Eastern Asia','88:90','49:52','40'),No, has to go in rest-of-world
  #list('Other South East Asia','86,89:90','58',''),#newp, will have to be rest of world! No S.E Asia in 01
  list('Other EU member states (non-accession)','47:48,51,53:55,58,61,66','10:13,16,18:20,22','33'),
  list('Other Europe','41:43,57,59:60,62:63,67:69,71','23:25,29:32,43','11,34:35'),#includes a mix of accession countries and other, as well as USSR
  list('Rest of world','13:14,37,39:40,46,72:76,86,88:92','33,49:54,58:60,64,67:69','25:26,39:42,44:45')#North Africa has to go here due to 2011 mismatch in Eng/Scot. South-East Asia has to go here too. 'Other Asia' 91 is fine here too, see 91 defs.
)

#Add automatically found results
threeCensusCombo <- c(threeCensusCombo,foundMatches)

#Then work out what's left.
#check what that looks like
for(i in 3:length(threeCensusCombo)){
  
  print(paste(i,threeCensusCombo[i][[1]][1],
              threeCensusCombo[i][[1]][2],
              threeCensusCombo[i][[1]][3],
              threeCensusCombo[i][[1]][4],
              sep = " "))
  
}

#Returns list of re-coded dataframes
#"Error: column_recode_lists_are_all_correct_length not equal to TRUE"
#Means lists defining recodes aren't all of a length that recodes in each dataframe
results <- recode(threeCensusCombo)

looksee <- results[[3]]

#Save as five dataframes, ready for geog re-assigning (apart from '91 which is correct already)
savens <- c('91LBS_PCS','01_OA','11_OA')

lapply(1:length(savens),function(x) write.csv(results[[x]],
                                              paste0("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/",savens[x],".csv"),
                                              row.names = F))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MERGING GB GEOGRAPHIES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We need a merged 91 PCS / WARD file from my aggregated versions.
#And GB output-area files for 2001 and 2011.
#The intersect I'll then do in QGIS.

#91 pcs and wards----

pcs91scot <- readOGR(dsn='C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount',layer='pseudoPCS_aggregated4CorrectCount')

wards91engWales <- readOGR(dsn='C:/Data/MapPolygons/EnglandWalesMerged/1991/wardsAggForCorrectCount',layer='wardsEngWales_aggregated4CorrectCount')

#can I just...?
#Dunno why makeUniqueIDs doesn't autocomplete.
gb <- rbind(wards91engWales,pcs91scot,makeUniqueIDs = T)

#Need to plot and check join...
writeOGR(gb, "C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~~~~
#2001 OAs----
#~~~~~~~~~~~~

#Ah, haven't downloaded them yet! And it turns out counties and unitary authorities are in different files
#FOR GOD KNOWS WHAT REASON
# oa01engCounties <- readOGR(dsn='C:/Data/MapPolygons/England/2001/England_oa_2001_gen_counties',layer='england_oa_2001_gen')
# oa01engUAs <- readOGR(dsn='C:/Data/MapPolygons/England/2001/England_ua_oa_2001_gen_unitaries',layer='england_ua_oa_2001_gen')
# 
# oa01both <- rbind(oa01engCounties,oa01engUAs,makeUniqueIDs = T)
# 
# writeOGR(oa01both, "C:/Data/MapPolygons/England/2001/England_oa_2001_gen","England_oa_2001_gen", driver="ESRI Shapefile", overwrite_layer = T)

#GET ENGLAND OAS, TWO FILES, COMBINE AND DISSOLVE BY ID----

#both!
# oa01eng <- readOGR(dsn='C:/Data/MapPolygons/England/2001/England_oa_2001_gen',layer='England_oa_2001_gen')
# 
# #Checks: one ID per polygon? Newp!
# unique(oa01eng$label) %>% length
# #Dissolve
# oa01engDissolve <- gUnaryUnion(oa01eng,id = oa01eng$label)
# 
# #quick, save! Oh yeah, it loses the IDs. May or may not be in the same order. It's in the row names isn't it? Yup.
# row.names(oa01engDissolve)[1:10]
# 
# oa01dissolve_spdf <- SpatialPolygonsDataFrame(oa01engDissolve,data.frame(zone.code = row.names(oa01engDissolve)),match.ID = F)
# 
# writeOGR(oa01dissolve_spdf, "C:/Data/MapPolygons/England/2001/England_oa_2001_gen","England_oa_2001_gen_dissolvedByID", driver="ESRI Shapefile", overwrite_layer = T)
# 
# #Oh it gets better! That was just counties - OAs from unitary authorities is, for some insane reason, in a different file.
# #It is at least the correct zones...
# 
# #Before that, let's just check it's actually getting the correct zones at all... Yup.
# chk <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/01_OA.csv')
# # oa01dissolve_spdf <- readOGR(dsn="C:/Data/MapPolygons/England/2001/England_oa_2001_gen",layer="England_oa_2001_gen_dissolvedByID")
# 
# #Yup, all good.
# table(oa01dissolve_spdf$zone.code %in% chk$`Zone Code` )

#SAME FOR WALES----

# oa01wales <- readOGR(dsn='C:/Data/MapPolygons/Wales/2001/Wales_oa_2001_clipped',layer='wales_oa_2001_clipped')
# 
# #Needs dissolving? Yes but there aren't many.
# unique(oa01wales$label) %>% length
# 
# oa01WalesDissolve <- gUnaryUnion(oa01wales,id = oa01wales$label)
# 
# oa01dissolve_spdf <- SpatialPolygonsDataFrame(oa01WalesDissolve,data.frame(zone.code = row.names(oa01WalesDissolve)),match.ID = F)
#  
# #Yup, looks good.
# writeOGR(oa01dissolve_spdf, "C:/Data/MapPolygons/Wales/2001/Wales_oa_2001_clipped","wales_oa_2001_clipped_dissolvedByID", driver="ESRI Shapefile", overwrite_layer = T)
 
#RELOAD NATIONS 2001 OAS, COMBINE----

oa01scot <- readOGR(dsn='C:/Data/MapPolygons/Scotland/2001/Scotland_outputareas_2001',layer='scotland_oa_2001_dissolvedTo_OneIDperRow_noSelfIntersect')

oa01eng <- readOGR(dsn="C:/Data/MapPolygons/England/2001/England_oa_2001_gen",layer="England_oa_2001_gen_dissolvedByID")

oa01wales <- readOGR("C:/Data/MapPolygons/Wales/2001/Wales_oa_2001_clipped","wales_oa_2001_clipped_dissolvedByID")

#First isn't working. Problem is what? Not any overlap of labels. 
#Oh, it's the names... like it sez. Duh.
names(oa01eng)
names(oa01wales)
names(oa01scot) <- 'zone_code'

#All
oa01gb <- rbind(oa01eng,oa01scot,makeUniqueIDs = T)
oa01gb <- rbind(oa01gb,oa01wales,makeUniqueIDs = T)

#all unique? Yup.
unique(oa01gb$zone_code) %>% length

#SAVE!
writeOGR(oa01gb, "C:/Data/MapPolygons/GreatBritain/2001/outputAreas","greatBritain2001_outputAreas_dissolvedByID", driver="ESRI Shapefile", overwrite_layer = T)

#Check match in CoB file
chk <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/01_OA.csv')

#Perfect. That's a nice surprise! Chances of repeating that with 2011 are slim...
table(oa01gb$zone_code %in% chk$`Zone Code` )


#~~~~~~~~~~~~
#2011 OAs----
#~~~~~~~~~~~~

#I think there's a chance these ones don't need dissolving. Scotland didn't, did it? Also, we don't need all those fields...
oa11scot <- readOGR('C:/Data/MapPolygons/Scotland/2011/Scotland_output_areas_2011','scotland_oac_2011')

#Yup, good
chk <- data.frame(oa11scot)
unique(oa11scot$code) %>% length

#1 field this time
oa11eng <- readOGR('C:/Data/MapPolygons/England/2011/England_outputareas_2011_gen','England_oa_2011_gen')

#Tick!
unique(oa11eng$CODE) %>% length

oa11wales <- readOGR('C:/Data/MapPolygons/Wales/2011/Wales_oa_2011_gen','wales_oa_2011_gen')

#TICK!
unique(oa11wales$code) %>% length

#OK, immediate combine then. Get names lined up.
#Oh. Why wouldn't @data work? Oh well, this does.
oa11scot <- oa11scot[,2]

names(oa11scot)
names(oa11wales)
names(oa11eng) <- 'code'#only different one

#Quite how it separates out the passed function argument, I don't know...
oa11gb <- do.call(rbind,list(oa11scot,oa11wales,oa11eng,makeUniqueIDs=T))

#Well that seemed to work.
unique(oa11gb$code) %>% length

#Check against 2011 cob codes
chk <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/11_OA.csv')

#Good lord! Full match again. Unprecedented!
table(oa11gb$code %in% chk$geography)

writeOGR(oa11gb, "C:/Data/MapPolygons/GreatBritain/2011/outputAreas","greatBritain2011_outputAreas", driver="ESRI Shapefile", overwrite_layer = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1991: AGGREGATE VARIABLES TO NEW LBS ZONES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Code taken from 91_PCSzeroCountZones_aggregateTables.R
pcs91shp <- readOGR("C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Yup
shp_df <- data.frame(pcs91shp)
shp_df$label <- as.character(shp_df$label)
shp_df$fnl_rsL <- as.character(shp_df$fnl_rsL)

#work only on those with more than one zone to aggregate
shp_df <- shp_df[!is.na(shp_df$fnl_rsL),]

#Each element a list of zones to combine
zonez <- lapply(c(1:nrow(shp_df)), 
                function(x) c(shp_df$label[x],
                              unlist(lapply(shp_df$fnl_rsL[x], function(y) strsplit(y,"\\|"))))
)

#~~~~~~~~~~~~~~~~~~

#Great Britain 91 CoB
CoB91 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/91LBS_PCS.csv")

#We seem to have more zones in CoB91 than in the the shapefile. What's the crack?
table(CoB91$Zone.ID %in% pcs91shp$label)
#But we do have all the geographies covered. 
table(pcs91shp$label %in% CoB91$Zone.ID)

#What are the CoB values for those we have no GB geography for?
noGeogz <- CoB91[!(CoB91$Zone.ID %in% pcs91shp$label),]

#Ah, we still have a lot of shipping I hadn't removed. Oops.

#~~~~~~~

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
agg2 <- CoB91[,3:36] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
zeroes <- agg2[apply(agg2[,2:34],1,sum)==0,]
zeroes <- merge(zeroes[,1],CoB91[,c(1:2)],by.x = 'label',by.y = 'Zone.ID')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
table(as.character(zeroes$Zone.name))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")
geogzdf <- data.frame(geogz)

#save
# writeOGR(geogz, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch", driver="ESRI Shapefile", overwrite_layer = T)

writeSpatialShape(geogz,
                  "StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2001 + 2011 COB: USING INTERSECTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Code from censusData_smallToLargeGeogs_3_censusTo1991aggPCS.R
#source("Function_DataFromSmalltoLargeGeog.R")

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#~~~~~~~~~~~~~~~~~~~~~
#2001 country of birth----

#Get the data
cob01 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/01_OA.csv")

#Get the intersect geog:
its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:ncol(cob01))#from dta

result <- moveData(its01,cob01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "2001_CountryOfBirthRecode_91LBS_noZeroPCS", driver="ESRI Shapefile", overwrite_layer = T)

#Better column abbreviation
writeSpatialShape(result,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~~~~
#2011 country of birth----

#Get the data
cob11 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/11_OA.csv")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:ncol(cob11))#from dta

result <- moveData(its11,cob11,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#save result!
# writeOGR(result, "StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth",
#          "2001_CountryOfBirthRecode_91LBS_noZeroPCS", driver="ESRI Shapefile", overwrite_layer = T)

#Better column abbreviation
writeSpatialShape(result,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Saving GB COB shapefiles and CSVs with a total pop column added----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Harmonised shapefiles----
cobs <- lapply(list.files(path='StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth',full.names = T,pattern = '*.shp'), function(x) readShapeSpatial(x))

lapply(cobs,names)

#Ah - don't dplyr spatial tings!
#cobs2 <- lapply(cobs,function(x) x %>% mutate(totalPop = rowSums(.[4:34])))

#Not pass by reference. Also silly. Java thinking, oops.
#lapply(cobs,function(x) x@data$totalPop <- apply(x@data[,4:34],1,sum))

for(i in 1:3){
  cobs[[i]]@data$totalPop <- apply(cobs[[i]]@data[,4:ncol(cobs[[i]]@data)],1,sum)
}

savens <- c('91CoBwithTotalPop','01CoBwithTotalPop','11CoBwithTotalPop')

lapply(1:length(savens),function(x) writeSpatialShape(cobs[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/",savens[x],".shp")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Each of the census CSVs (same CoB category, not same geography)----

cobs <- lapply(list.files(path='VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS',full.names = T,pattern = '*.csv'), function(x) read_csv(x))

#Watch out, loads in this order:01 / 11 / 91
#One ID column please
cobs[[3]] <- cobs[[3]][,c(1,3:ncol(cobs[[3]]))]

#Ah - don't dplyr spatial tings!
#cobs2 <- lapply(cobs,function(x) x %>% mutate(totalPop = rowSums(.[4:34])))

#Not pass by reference. Also silly. Java thinking, oops.
#lapply(cobs,function(x) x@data$totalPop <- apply(x@data[,4:34],1,sum))

for(i in 1:3){
  cobs[[i]]$totalPop <- apply(cobs[[i]][,2:ncol(cobs[[i]])],1,sum)
}

savens <- c('01OA_CoBwithTotalPop','11OA_CoBwithTotalPop','91LBS_CoBwithTotalPop')

lapply(1:length(savens),function(x) write_csv(cobs[[x]],paste0("VariableCoding/GreatBritain/CountryOfBirth_threeCensusRecodes_to91LBS/",savens[x],".csv")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SOME CHECKS ON ENG VS SCOTS ZONE SIZES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CoB_GB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','11CoBwithTotalPop')

#area per zone
CoB_GB$area <- gArea(CoB_GB,byid=T)/1000000

#sum(CoB_GB$area)
#Which should be metres, right? COnverted to KM. Err. Yup, that's the right ballpark.

CoB_GB$popPerKM2 <- CoB_GB$totalPop/CoB_GB$area

#About right? Probably.
ggplot(CoB_GB@data,aes(x = 1, y = popPerKM2)) +
  geom_boxplot() +
  scale_y_log10()


#Is there an easy way to tell the nations apart from the zone codes?
#I would say no! Need the originals
sample(CoB_GB$label,50)

scot <- readOGR('C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount','pseudoPCS_aggregated4CorrectCount')

engWales <- readOGR('C:/Data/MapPolygons/EnglandWalesMerged/1991/wardsAggForCorrectCount','wardsEngWales_aggregated4CorrectCount')

CoB_GB$country <- 'Scotland'
CoB_GB$country[CoB_GB$label %in% engWales$label] <- 'EngWales'

table(CoB_GB$country)

#So those >1000 per km2 as urban-ish...
moreThan <- CoB_GB[CoB_GB$popPerKM2 > 1000,]

#Size distribution across the countries for these?
ggplot(moreThan@data, aes(x = country, y = area)) +
  geom_boxplot() +
  scale_y_log10()

#what were the original scots countries?
orig <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_3Census_csvs/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.csv')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#REGRESSIONS PREP COB ONLY----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So that includes weights matrices to start with. I'll come back to other-migrant group...?
#Each decade in its own column
#coBs long
cob91 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
cob01 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')
cob11 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')

cobs <- list(cob91,cob01,cob11)

#Drop not-needed cols
cobs <- lapply(cobs, function(x) x[,c(2,4:ncol(x))])

#~~~~~~~~~~~~~~~~~~~~
#SCOTS OLD/NEW CHECKS

#Quite different results, though that might be due to different regression vars being included.
#But let's see.
#Just Scotland
# scotz <- readOGR('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth','2011_CountryOfBirthRecode_91LBS_noZeroPCS')
# 
# table(scotz$label %in% cobs[[1]]$label)
# cobsnewScots <- lapply(cobs, function(x) x[x$label %in% scotz$label,])
# 
# #old...
# cob91old <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
# cob01old <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2001_CountryOfBirthRecode_91LBS_noZeroPCS.shp')
# cob11old <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2011_CountryOfBirthRecode_91LBS_noZeroPCS.shp')
# 
# cobsold <- list(cob91old,cob01old,cob11old)
# 
# #Drop not-needed cols
# cobsold <- lapply(cobsold, function(x) x[,c(2,4:ncol(x))])
# 
# #~~~
# #Look at a couple
# cob11newlook <- cob11 %>% data.frame()
# cob11newlook <- cob11newlook[cob11newlook$label %in% scotz$label,]
# cob11oldlook <- cob11old %>% data.frame()
# 
# names(cob11newlook)
# names(cob11oldlook)
# 
# #take a look at those with the same cob name
# namenames <- names(cob11newlook)[names(cob11newlook) %in% names(cob11oldlook)]
# namenames <- namenames[4:length(namenames)]
# 
# for(name in namenames){
#   print(cor(cob11newlook[,name],cob11oldlook[,name]))
# }

#All good.

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# test <- gather(cobs[[1]]@data,cob, value, Channel_Is:China)
# head(test)

#First: subset of larger population zones, over 1000 people per km2
#Second: proportions
#Then gather, then regressions.
#Plus label country type

#Also: weights matrices.

#1. large pop zones. Use 2011 total pop
CoB_GB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','11CoBwithTotalPop')
CoB_GB$area <- gArea(CoB_GB,byid=T)/1000000
CoB_GB$popPerKM2 <- CoB_GB$totalPop/CoB_GB$area
moreThan <- CoB_GB[CoB_GB$popPerKM2 > 1000,]

#We just need the zone labels now
largePopZones <- as.character(moreThan$label)

cobsLargeZones <- lapply(cobs, function(x) x[x$label %in% largePopZones,])

#Just Scotland
# scotz <- readOGR('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth','2011_CountryOfBirthRecode_91LBS_noZeroPCS')
# 
# #table(scotz$label %in% cobsLargeZones[[1]]$label)
# cobsLargeZones <- lapply(cobsLargeZones, function(x) x[x$label %in% scotz$label,])
# #or all scots zones
# cobsLargeZones <- lapply(cobs, function(x) x[x$label %in% scotz$label,])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test a theory: if we use proportions for all zones 
#THEN reduce to "urban", what do the numbers look like?
# cobsLargeZones <- cobs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test theory two: looking just at reasonably large contiguous zones
#(those zones that have 2+ neighours of neighbours)
#Gives a different result
#This was calculated below. Reload to keep some pretence of sensible code order.
# urbanz <- readOGR('QGIS/temp','checkIsolatingUrban')
# urbanz <- urbanz[urbanz$hasNeighbo==1,]

# cobsLargeZones <- lapply(cobs, function(x) x[x$label %in% urbanz$label,])

#Tick
#cobsLargeZones[[3]]$label %>% length

#Find proportions for these larger-pop zones. Are we going to have a problem with there being 5000 zones...? We shall see!
#We can put them all into one dataframe before finding proportions
censusYear <- c(1991,2001,2011)

for(i in 1:3) cobsLargeZones[[i]]$censusYear <- censusYear[i]

#Combine into one
allCoB <- lapply(cobsLargeZones,data.frame)
allCoB <- do.call(rbind,allCoB)

#Do props
cobprops <- allCoB %>% 
  dplyr::select(2:ncol(allCoB)) %>% 
  group_by(censusYear) %>% 
  mutate_each(  funs(  ((.)/sum(.))*100  ) )

#Did that work? Yup!
apply(cobprops[cobprops$censusYear==2011,c(1:ncol(cobprops))],2,sum)

#Save for looking in QGIS. Separate census decades.. oops, it has no labels
# cobpropswlabels <- cobprops
# cobpropswlabels$label <- allCoB$label
#unique(sheet$label) %>% length#yup
# lapply(c(1991,2001,2011), function(x) write_csv(cobpropswlabels[cobpropswlabels$censusYear==x,],paste0('R_data/3CensusGB_proportions/',x,'.csv')))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#If we're checking impact of proportions (urban vs all)
#Then remove non-urban here
# cobpropswlabels <- cobpropswlabels[cobpropswlabels$label %in% largePopZones,]

#~~~~~

#Then: each decade in it own column. Each long.
long91 <- cobprops[cobprops$censusYear==1991,] %>% 
  gather(cob,xij91,Channel_Is:China)
long01 <- cobprops[cobprops$censusYear==2001,] %>% 
  gather(cob,xij01,Channel_Is:China)
long11 <- cobprops[cobprops$censusYear==2011,] %>% 
  gather(cob,xij11,Channel_Is:China)

#CoB will match for each. Label vector will repeat.
sheet <- data.frame(cob = long91$cob,xij91 = long91$xij91, xij01 = long01$xij01, xij11 = long11$xij11)
sheet$label <- allCoB$label
#unique(sheet$label) %>% length#yup


# write_csv(sheet,'StitchOutputs/GreatBritain/3censusCoB_sheetAllCountries.csv')

#Keep only non-UK countries in there.
dropz <- c(
  'Channel_Is',
  'UK_part_no',
  'England',
  'Scotland',
  'Wales',
  'Northern_I'
)

sheetNoUK <- sheet[!(sheet$cob %in% dropz),]
# write_csv(sheetNoUK,'StitchOutputs/GreatBritain/3censusCoB_sheet_nonUK.csv')

#OK, that's it. Just got some other labelling to be done, including separating out London.
#But can do basic regression now
allz <- lm(data = sheetNoUK, xij11 ~ xij91)
summary(allz)

#plot(sheetNoUK$xij11 ~ sheetNoUK$xij91, xlim=c(0,2))
plot(sheetNoUK$xij11 ~ sheetNoUK$xij91)
plot(log(sheetNoUK$xij11) ~ log(sheetNoUK$xij01))

#Is there something odd about the range of those values for the different decades?
decadez <- gather(sheetNoUK,decade,props,xij91:xij11)

ggplot(decadez,aes(x = decade, y= props)) +
  geom_boxplot()

ggplot(decadez,aes(x = decade, y= props)) +
  geom_boxplot() +
  scale_y_log10()


#So what's the crack with some of the large 91 values? Over 6% when we're talking about 5000 zones?
chk <- long91[long91$xij91 > 1,] %>% data.frame %>% 
  arrange(-xij91)

#Hmm. Some of those numbers a priori suggest high clusters. Which is what I think I'd expect: and they spread out in later decades. UK part not specified is odd though. Though we should lose all the UK bits really... let's do that for starters. 

#Label up rich/poor, edited list from previously... actually, just run to get the list in eViewsReady
#How many names matching do we have?
#Actually actually: I think we need a new list that matches what we have. So.
unique(sheetNoUK$cob)

#full list
# 'Irish_Repu',
# 'South_Afri',
# 'Africa_oth',
# 'Caribbean',
# 'South_Amer',
# 'Other_Midd',
# 'Other_EU_m',
# 'Other_Euro',
# 'Rest_of_wo',
# 'Australia',
# 'Kenya',
# 'Zimbabwe',
# 'Nigeria',
# 'Bangladesh',
# 'India',
# 'Pakistan',
# 'Hong_Kong',
# 'France',
# 'Germany',
# 'Italy',
# 'Spain',
# 'Poland',
# 'Romania',
# 'Turkey',
# 'United_Sta',
# 'Iran',
# 'China'

rich <- c(
  'Irish_Repu',
  'South_Afri',
  'Other_EU_m',
  #'Other_Euro',
  'Australia',
  'Hong_Kong',
  'France',
  'Germany',
  'Italy',
  'Spain',
  'United_Sta'
)

sheetNoUK$rich <- 0
sheetNoUK$rich[sheetNoUK$cob %in% rich] <- 1
unique(sheetNoUK$cob[sheetNoUK$rich==1])
unique(sheetNoUK$cob[sheetNoUK$rich==0])

richReg <- lm(data = sheetNoUK[sheetNoUK$rich==1,], xij11 ~ xij91)
summary(richReg)
poorReg <- lm(data = sheetNoUK[sheetNoUK$rich==0,], xij11 ~ xij91)
#poorReg <- lm(data = sheetNoUK[sheetNoUK$rich==0 & sheetNoUK$cob!='Rest_of_wo',], xij11 ~ xij91)#little diff
summary(poorReg)

plot(sheetNoUK$xij11[sheetNoUK$rich==1] ~ sheetNoUK$xij91[sheetNoUK$rich==1])
plot(sheetNoUK$xij11[sheetNoUK$rich==0] ~ sheetNoUK$xij91[sheetNoUK$rich==0])

#11 vs 01
richReg <- lm(data = sheetNoUK[sheetNoUK$rich==1,], xij11 ~ xij01)
summary(richReg)
poorReg <- lm(data = sheetNoUK[sheetNoUK$rich==0,], xij11 ~ xij01)
summary(poorReg)

plot(sheetNoUK$xij11[sheetNoUK$rich==1] ~ sheetNoUK$xij01[sheetNoUK$rich==1])
plot(sheetNoUK$xij11[sheetNoUK$rich==0] ~ sheetNoUK$xij01[sheetNoUK$rich==0])

#Check if London makes a difference.
#Zones selected in QGIS
london <- readOGR('QGIS','GB_LBSzonesLondon')

table(london$label %in% sheetNoUK$label)

sheetNoUK$london <- 0
sheetNoUK$london[sheetNoUK$label %in% london$label] <- 1
table(sheetNoUK$london)

londonReg <- lm(data = sheetNoUK[sheetNoUK$london==1,], xij11 ~ xij91)
summary(londonReg)
restReg <- lm(data = sheetNoUK[sheetNoUK$london==0,], xij11 ~ xij91)
summary(restReg)

londonPoor <- lm(data = sheetNoUK[sheetNoUK$london==1 & sheetNoUK$rich==0,], xij11 ~ xij91)
summary(londonPoor)
londonRich <- lm(data = sheetNoUK[sheetNoUK$london==0 & sheetNoUK$rich==1,], xij11 ~ xij91)
summary(londonRich)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Look at only places with a reasonable amount of contiguous zones----
#i.e those that have neighbours of neighbours
#For which we will need contiguity matrices... 
#well no we won't, not for this.
#Use one of the urban-only SPDFs
# urbanz <- cobsLargeZones[[1]]
# 
# contig <- poly2nb(urbanz, row.names = urbanz@data$label)
# #mx <- nb2mat(contig,zero.policy = T)
# #OK. Weights add to 1 over rows.
# #apply(mx,1,sum)
# 
# #If a combined list of all unique ids is larger when neighbours included
# #I have at least one neighbour of neighbour.
# #Could make it at least two...
# #So: is the length of unique neighbour indices for all my neighbours larger than
# #The length of just my immediate neighbours? If so, I have x neighbours of neighbours
# (length(contig[unlist(contig[[1]])] %>% unlist %>% unique) - length(unlist(contig[[1]]))) > 2
# 
# #Now can I do that in one go? BOOLZ!
# boolz <- lapply(1:nrow(urbanz), function(x) 
# (length(contig[unlist(contig[[x]])] %>% unlist %>% unique) - length(unlist(contig[[x]]))) > 2
# ) %>% unlist
# 
# #OK, we caught a bunch
# table(boolz)
# 
# urbanz$hasNeighboursOfNeighbours <- 0
# urbanz$hasNeighboursOfNeighbours[boolz] <- 1
#table(urbanz$hasNeighboursOfNeighbours)

#Save that to see
#writeSpatialShape(urbanz,'QGIS/temp/checkIsolatingUrban.shp')

#Not perfect but good enough to test the basic theory. Soooo no. No diff.

#And what about Scotland? Has it suddenly got different values?
#(scotz$label %in% unique(sheetNoUK$label)) %>% table


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GB WEIGHTS MATRICES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Random shp to get the polygons
zones <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')

#Need to reduce zones to those we've used above in the sheet
table(zones$label %in% sheetNoUK$label)

zonesub <- zones[zones@data$label %in% sheetNoUK$label,]

#just checking...tick.
# plot(zonesub[zonesub@data$label=='04BZFF',])
# plot(zones[zones@data$label=='04BZFF',])

#spdep
#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
contig <- poly2nb(zonesub, row.names = zonesub@data$label)

#Binary style, default is row-normalised
#mx <- nb2mat(contig,zero.policy = T,style = 'B')
#Row-normalised
mx <- nb2mat(contig,zero.policy = T)

#OK. Weights add to 1 over rows.
apply(mx,1,sum)

saveRDS(mx,"R_data/bits/GB_wardPCS_spatialWeightsQueenContig_largeZonePopSub.rds")

#~~~~~~~~~~~~~~~~~
#Apply to CoBs in each census year
#One column per CoB (cobprops minus censusyear)

#Oh hang on, we lost some zones along the way reducing to urban.
#So need to only keep weights matrix for them (cos it needs to apply to full proportions)

#Subset to correct CoBs here to save faff later
names(cobprops)
unique(sheetNoUK$cob)
names(cobprops) %in% unique(sheetNoUK$cob)

#Are these in the right order? Yup
cobpropsub <- cobprops[,names(cobprops) %in% c(as.character(unique(sheetNoUK$cob)),'censusYear')]
names(cobpropsub)
unique(sheetNoUK$cob) %>% as.character

weightsMatrix <- matrix(nrow = nrow(zonesub),ncol = (length(names(cobpropsub))-1))

for(val in seq(1:(length(names(cobpropsub))-1))){
# for(val in seq(1:33)){
  
  weightsMatrix[,val] <- mx %*% as.matrix(cobpropsub[cobpropsub$censusYear==1991,(val)])
  
}

#And they should now all be in the right order for melting and sticking in the thingyo.
sheetNoUK$w91q <- weightsMatrix %>% data.frame() %>% 
  gather(cob,value) %>%
  dplyr::select(value) %>% 
  unlist(use.names = F)


plot(sheetNoUK$xij91,sheetNoUK$w91q)

#Those numbers look right ... though there's an eensy population issue.
for(name in unique(sheetNoUK$cob)){
    #print(cor(sheetNoUK$xij91[sheetNoUK$cob==name],sheetNoUK$w91q[sheetNoUK$cob==name]))
  
  jpeg(paste0('R_outputs/GB_cobVsContiguityChecks/',name,'.jpg'))
  plot(sheetNoUK$xij91[sheetNoUK$cob==name],sheetNoUK$w91q[sheetNoUK$cob==name],
       main=name)
  abline(a=0,b=1)
  dev.off()
  
}

#OK, that seems to be working. Some regressions...
richReg <- lm(data = sheetNoUK[sheetNoUK$rich==1,], xij11 ~ xij91 + w91q)
summary(richReg)
poorReg <- lm(data = sheetNoUK[sheetNoUK$rich==0,], xij11 ~ xij91 + w91q)
summary(poorReg)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TIDY STRAIGHT RUN VERSION OF ABOVE: DATA TO REGRESSION VIA PROPORTION WEIGHTS----  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So I can drop a few things in. 
#It just needs the subset of zones (so that CoB % will be across that subset, not all zones)
#And a subset of CoBs.

#Or I might just try a tidier version of the above code. Let's see.
cob91 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
cob01 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')
cob11 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')

#Test with actual scots data
# cob91 <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
# cob01 <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2001_CountryOfBirthRecode_91LBS_noZeroPCS.shp')
# cob11 <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2011_CountryOfBirthRecode_91LBS_noZeroPCS.shp')

cobs <- list(cob91,cob01,cob11)

#Drop not-needed cols
cobs <- lapply(cobs, function(x) x[,c(2,4:ncol(x))])

#1. large pop zones. Use 2011 total pop
CoB_GB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','11CoBwithTotalPop')
CoB_GB$area <- gArea(CoB_GB,byid=T)/1000000
CoB_GB$popPerKM2 <- CoB_GB$totalPop/CoB_GB$area
moreThan <- CoB_GB[CoB_GB$popPerKM2 > 1000,]

#We just need the zone labels now
largePopZones <- as.character(moreThan$label)

#This is the one to replace for different zone subsets
cobsLargeZones <- lapply(cobs, function(x) x[x$label %in% largePopZones,])

#Scots subset of large-pop zones subset
# scotz <- readOGR('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth','2011_CountryOfBirthRecode_91LBS_noZeroPCS')
# 
# table(scotz$label %in% cobsLargeZones[[1]]$label)
cobsLargeZones <- lapply(cobsLargeZones, function(x) x[x$label %in% scotz$label,])

#all Scotland
cobsLargeZones <- lapply(cobs, function(x) x[x$label %in% scotz$label,])
unique(cobsLargeZones[[1]]$label) %>% length

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Subset of urban Scots zones used in the previous analysis
scotsUrbanRatios <- read_csv('R_data/pcsAggregatedZeroCounts_urbanRatios.csv')

#Mark those 50%+ urban
scotsUrbanRatios$urbanFiftyPercentPlus <- 0 + (scotsUrbanRatios$intersectUrbanRatio > .5)
table(scotsUrbanRatios$urbanFiftyPercentPlus)

#Keep labels matching that
scotsUrbanLabels <- scotsUrbanRatios$label[scotsUrbanRatios$urbanFiftyPercentPlus==1]

#use those in regression...
table(cobs[[1]]$label %in% scotsUrbanLabels)
cobsLargeZones <- lapply(cobs, function(x) x[x$label %in% scotsUrbanLabels,])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Find proportions for these larger-pop zones. Are we going to have a problem with there being 5000 zones...? We shall see!
#We can put them all into one dataframe before finding proportions
censusYear <- c(1991,2001,2011)
for(i in 1:3) cobsLargeZones[[i]]$censusYear <- censusYear[i]

#Combine into one
allCoB <- lapply(cobsLargeZones,data.frame)
allCoB <- do.call(rbind,allCoB)

#Do props
cobprops <- allCoB %>% 
  dplyr::select(2:ncol(allCoB)) %>% 
  group_by(censusYear) %>% 
  mutate_each(  funs(  ((.)/sum(.))*100  ) )

#Did that work? Yup!
apply(cobprops[cobprops$censusYear==2011,c(1:ncol(cobprops))],2,sum)

#Then: each decade in it own column. Each long.
long91 <- cobprops[cobprops$censusYear==1991,] %>% 
  gather(cob,xij91,1:(ncol(cobprops)-1))
long01 <- cobprops[cobprops$censusYear==2001,] %>% 
  gather(cob,xij01,1:(ncol(cobprops)-1))
long11 <- cobprops[cobprops$censusYear==2011,] %>% 
  gather(cob,xij11,1:(ncol(cobprops)-1))
# long01 <- cobprops[cobprops$censusYear==2001,] %>% 
#   gather(cob,xij01,Channel_Is:China)
# long11 <- cobprops[cobprops$censusYear==2011,] %>% 
#   gather(cob,xij11,Channel_Is:China)

#CoB will match for each. Label vector will repeat.
sheet <- data.frame(cob = long91$cob,xij91 = long91$xij91, xij01 = long01$xij01, xij11 = long11$xij11)
sheet$label <- allCoB$label

#~~~~~~~~~~~~
#Test removing non-urban zones AFTER proportions found
# table(unique(sheet$label) %in% largePopZones)
# sheet <- sheet[sheet$label %in% largePopZones,]
#Breaks weights calc below...

#~~~~~~~~~~~~
#Keep only non-UK countries in there.
# dropz <- c(
#   'Channel_Is',
#   'UK_part_no',
#   'England',
#   'Scotland',
#   'Wales',
#   'Northern_I'
# )
# 
# sheetNoUK <- sheet[!(sheet$cob %in% dropz),]

#Geoff's pre-formed list... does that change owt?
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

sheetNoUK <- sheet[(sheet$cob %in% subz),]

unique(sheet$cob)
unique(sheetNoUK$cob)

#Hmm...
unique(sheet$cob)[!(unique(sheet$cob) %in% unique(sheetNoUK$cob))]

rich <- c(
  'Irish_Repu',
  'South_Afri',
  'Other_EU_m',
  #'Other_Euro',
  'Australia',
  'Hong_Kong',
  'France',
  'Germany',
  'Italy',
  'Spain',
  'United_Sta'
)

sheetNoUK$rich <- 0
sheetNoUK$rich[sheetNoUK$cob %in% rich] <- 1

#Check if London makes a difference.
#Zones selected in QGIS
#london <- readOGR('QGIS','GB_LBSzonesLondon')

# table(london$label %in% sheetNoUK$label)
# 
# sheetNoUK$london <- 0
# sheetNoUK$london[sheetNoUK$label %in% london$label] <- 1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#GB WEIGHTS MATRICES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Random shp to get the polygons
zones <- cobs[[1]] 
#Need to reduce zones to those we've used above in the sheet
table(zones$label %in% sheetNoUK$label)

zonesub <- zones[zones@data$label %in% sheetNoUK$label,]

#spdep
#https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
contig <- poly2nb(zonesub, row.names = zonesub@data$label)

#Row-normalised
mx <- nb2mat(contig,zero.policy = T)

#OK. Weights add to 1 over rows.
apply(mx,1,sum)

#~~~~~~~~~~~~~~~~~
#Apply to CoBs in each census year
#One column per CoB (cobprops minus censusyear)

#Subset to correct CoBs here to save faff later
names(cobprops)
unique(sheetNoUK$cob)
names(cobprops) %in% unique(sheetNoUK$cob)

#Are these in the right order? Yup
cobpropsub <- cobprops[,names(cobprops) %in% c(as.character(unique(sheetNoUK$cob)),'censusYear')]
names(cobpropsub)
unique(sheetNoUK$cob) %>% as.character

weightsMatrix <- matrix(nrow = nrow(zonesub),ncol = (length(names(cobpropsub))-1))

for(val in seq(1:(length(names(cobpropsub))-1))){
  weightsMatrix[,val] <- mx %*% as.matrix(cobpropsub[cobpropsub$censusYear==1991,(val)])
}

#And they should now all be in the right order for melting and sticking in the thingyo.
sheetNoUK$w91q <- weightsMatrix %>% data.frame() %>% 
  gather(cob,value) %>%
  dplyr::select(value) %>% 
  unlist(use.names = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~

#OK, that seems to be working. Some regressions...
richReg <- lm(data = sheetNoUK[sheetNoUK$rich==1,], xij11 ~ xij91 + w91q)
summary(richReg)
poorReg <- lm(data = sheetNoUK[sheetNoUK$rich==0,], xij11 ~ xij91 + w91q)
summary(poorReg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MORE CHECKS COMPARING OLD vs NEW SCOTLAND----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#We've checked the underlying data is right. Did it stay correct after all the transformations above?
#And did we actually check all decades? Oh yeah, I did.
#Checking against the new data itself (raw and sheet)
cor(cob91[cob91$label %in% unique(sheetNoUK$label),'France'] %>% data.frame,sheetNoUK$xij91[sheetNoUK$cob=='France'])
#sheetNoUK$xij91[sheetNoUK$cob=='France'] %>% sum
#cob91[cob91$label %in% unique(sheetNoUK$label),'France'] %>% data.frame %>% sum

#Yup. Check rest
table(unique(sheetNoUK$cob) %in% names(cob91))

for(cob in unique(sheetNoUK$cob)){
  print(cor(cob91[cob91$label %in% unique(sheetNoUK$label),cob] %>% data.frame,sheetNoUK$xij91[sheetNoUK$cob==cob]))
}
for(cob in unique(sheetNoUK$cob)){
  print(cor(cob11[cob11$label %in% unique(sheetNoUK$label),cob] %>% data.frame,sheetNoUK$xij11[sheetNoUK$cob==cob]))
}


#OK, so there's nothing wrong with how the data ended up. No awry country name matches.


#Output all direct correlations. Anything weird with any 91/11 pairings?
for(name in unique(sheetNoUK$cob)){
  #print(cor(sheetNoUK$xij91[sheetNoUK$cob==name],sheetNoUK$w91q[sheetNoUK$cob==name]))
  
  jpeg(paste0('R_outputs/GB_cobVscobChecks/',name,'.jpg'))
  plot(sheetNoUK$xij11[sheetNoUK$cob==name]~sheetNoUK$xij91[sheetNoUK$cob==name],
       main=name)
  abline(a=0,b=1)
  dev.off()
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Coeffs for each CoB to compare to previous...
#What are the coeffs for each CoB
coeffs <- lapply(split(sheetNoUK, factor(sheetNoUK$cob)), function(x) summary(lm(data = x, xij11 ~ xij91 + w91q))$coefficients[2])

#Scots data directly
scotzz <- read_csv('R_data/estimation_aggEmploymentToLargerZones.csv')


coeffScotz <- lapply(split(scotzz, factor(scotzz$CoB)), function(x) summary(lm(data = x, xij2011 ~ xij1991 + w91q))$coefficients[2])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Save sheet for looking in QGIS-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Sheetnouk. Output a direct proportions and a weights dataset for looking at in QGIS.
#Needs spreading
xij91 <- sheetNoUK %>% 
  dplyr::select(cob,xij91,label) %>% 
  spread(cob,xij91)

write_csv(xij91,'R_data/bits/xij91GB.csv')

w91q <- sheetNoUK %>% 
  dplyr::select(cob,w91q,label) %>% 
  spread(cob,w91q)

write_csv(w91q,'R_data/bits/w91q.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~
#Recheck raw data matches---

# scotsCoBs <- names(cob91scots)[4:length(names(cob91scots))]
# gbCoBs <- names(cob91)[4:length(names(cob91))]
# 
# #A bunch of which will have been aggregated a little differently
# #Could do with working out which to compare those directly we know are exactly the same
# inBoth <- scotsCoBs[scotsCoBs %in% gbCoBs]
# 
# #GET CORRELATIONS TO CHECK WHERE DATA EXACTLY MATCHES.
# #Correlations for GB(scots) vs original scots
# #Can use to check regressions on those exact matches etc
# corz91 <- lapply(inBoth, function(name) 
#   {cor(cob91[cob91$label %in% cob91scots$label,name] %>% data.frame,cob91scots[,name] %>% data.frame) 
#   
#   })
# corz91 <- do.call(cbind,corz91)
# corz91 <- t(corz91)
# 
# corz01 <- lapply(inBoth, function(name) 
#   {cor(cob01[cob01$label %in% cob01scots$label,name] %>% data.frame,cob01scots[,name] %>% data.frame) 
#   
#   })
# corz01 <- do.call(cbind,corz01)
# corz01 <- t(corz01)
# 
# corz11 <- lapply(inBoth, function(name) 
#   {cor(cob11[cob11$label %in% cob11scots$label,name] %>% data.frame,cob11scots[,name] %>% data.frame) 
#   
#   })
# corz11 <- do.call(cbind,corz11)
# corz11 <- t(corz11)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~FUNCTIONING UP THE REGRESSION PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#So I can compare data for different forms, check where any regresion differences are coming from.
#So let's assemble some sets of data
#and look at what we'd then need to function it all up in a consistent way. Copying the code above AGAIN!

#GB
cob91 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
cob01 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')
cob11 <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')

#SCOTS
cob91scots <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp')
cob01scots <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2001_CountryOfBirthRecode_91LBS_noZeroPCS.shp')
cob11scots <- readShapeSpatial('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth/2011_CountryOfBirthRecode_91LBS_noZeroPCS.shp')

cobs <- list(cob91,cob01,cob11)
#Drop not-needed cols
cobs <- lapply(cobs, function(x) x[,c(2,4:ncol(x))])

cobScot <- list(cob91scots,cob01scots,cob11scots)
#Drop not-needed cols
cobScot <- lapply(cobScot, function(x) x[,c(2,4:ncol(x))])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHOOSE ZONES TO BE USED. CoB proportions will be 100% in those zones----

#All zones in GB
GBzones <- cob91$label %>% as.character()

#Zones with more than 1000 people per km2. Using 2011 total pop
CoB_GB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','11CoBwithTotalPop')
CoB_GB$area <- gArea(CoB_GB,byid=T)/1000000
CoB_GB$popPerKM2 <- CoB_GB$totalPop/CoB_GB$area
moreThan <- CoB_GB[CoB_GB$popPerKM2 > 1000,]

#We just need the zone labels now
GBzonesWithMoreThan1000perkm2 <- as.character(moreThan$label)

#Scots zones (822 postcode sectors)
scotzones <- readOGR('StitchOutputs/Scotland/LBS_postcodeSector_3Census_raw/CountryOfBirth','2011_CountryOfBirthRecode_91LBS_noZeroPCS') %>% data.frame %>% dplyr::select(label) %>% unlist(use.names = FALSE) %>% as.character()

#Subset of urban Scots zones used in the previous analysis
# scotsUrbanRatios <- read_csv('R_data/pcsAggregatedZeroCounts_urbanRatios.csv')
# #Mark those 50%+ urban
# scotsUrbanRatios$urbanFiftyPercentPlus <- 0 + (scotsUrbanRatios$intersectUrbanRatio > .5)
# table(scotsUrbanRatios$urbanFiftyPercentPlus)
# #Keep labels matching that
# scotsUrban <- scotsUrbanRatios$label[scotsUrbanRatios$urbanFiftyPercentPlus==1]
# saveRDS(scotsUrban, 'R_data/scotsUrban.rds')
scotsUrban <- readRDS('R_data/scotsUrban.rds')

#~~~~~~~~~~~~~~~~~~~~~
#CHOOSE COBS TO USE----
#~~~~~~~~~~~~
#Keep only non-UK countries in there.
# dropz <- c('Channel_Is','UK_part_no','England','Scotland','Wales','Northern_I')

#This is the one to replace for different zone subsets
#Geoff's orig list
geoffsOrigList <- c( 'Irish_Repu', 'China', 'South_Afri', 'Africa_oth', 'South_Amer', 'Other_Midd', 'Other_East', 'Europe_oth', 'Australia', 'Canada', 'Nigeria', 'India', 'Pakistan', 'Hong_Kong', 'Malaysia', 'France', 'Germany', 'Italy', 'Netherland', 'Spain', 'Poland', 'United_Sta' ) 

#How many dem in GB vs Scots-old?
table(geoffsOrigList %in% names(cob91))
geoffsOrigList[!(geoffsOrigList %in% names(cob91))]
#All these
table(geoffsOrigList %in% names(cob91scots))

#So another list that's CoBs from Geoff's list in both
geoffsInGBplusScots <- geoffsOrigList[(geoffsOrigList %in% names(cob91))]

#~~~~~~~~~~~~~~~~~~~~~~~~
#Run on whole----
#~~~~~~~~~~~~~~~~~~~~~~~~

outputTofile(inputdata = sheet, title = 'ALLZ', file = 'regressionOutputs/3census/tests_GBScots/GBcobsAll.txt', lmfunction = 'xij11 ~ xij91 + w91q')



#~~~~~~~~~~~~~~~~~~~~~
#COB FLAGS---
#~~~~~~~~~~~~~~~~~~~~~
rich <- c(
  'Irish_Repu',
  'South_Afri',
  'Other_EU_m',
  #'Other_Euro',
  'Australia',
  'Hong_Kong',
  'France',
  'Germany',
  'Italy',
  'Spain',
  'United_Sta'
)

sheet$rich <- 0
sheet$rich[sheet$cob %in% rich] <- 1
table(sheet$rich)
#All true.
table(rich %in% names(cob91))


#~~~~~~~~~~~~~~~~~~~~~
#ORIG SCOTS VS GB COB DIFFERENCES---
#~~~~~~~~~~~~~~~~~~~~~

# names(cob11)
# names(cob11scots)
# sharedCoBs <- names(cob11)[names(cob11) %in% names(cob11scots)[4:length(cob11scots)]]
# #Which ones were perfect matches, which had a difference? (For scots zones)
# corz <- lapply(sharedCoBs, function(cob) cor(cob91[cob91$label %in% scotzones,cob] %>% data.frame,cob91scots[cob91scots$label %in% scotzones,cob] %>% data.frame)) %>% data.frame %>% t
# #Save for later use!
# saveRDS(corz,'R_data/GBnScotsSharedCoBs.rds')

#So this is:
#For Cobs present in both the original scots and the new GB data
#Are they exactly the same data? Most are, a few have been aggregated differently
gb_vs_scotsCoBs <- readRDS('R_data/GBnScotsSharedCoBs.rds')

#~~~~~~~~~~~~~~~~~~~~~
#ZONE FLAGS---
#~~~~~~~~~~~~~~~~~~~~~

#LONDON
#Zones selected in QGIS
#london <- readOGR('QGIS','GB_LBSzonesLondon')

# table(london$label %in% sheetNoUK$label)
# 
# sheetNoUK$london <- 0
# sheetNoUK$london[sheetNoUK$label %in% london$label] <- 1


#~~~~~~~~~~~~~~~~~~~~~~~
#REGRESSIONS 1. Scots all zones-----
#~~~~~~~~~~~~~~~~~~~~~~~

#Use geoffsInGBplusScots: same names in both, to make comparison exact

sheet <- CoBtoRegressionReadyData(cobs,scotzones,geoffsInGBplusScots,c(1991,2001,2011))

sheet$rich <- 0
sheet$rich[sheet$cob %in% rich] <- 1

outputTofile(inputdata = sheet, splitByFactor = 'rich', title = 'GB-new-scots: Scots zones in new data RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/ScotsRichPoor.txt', lmfunction = 'xij11 ~ xij91 + w91q')

#~~
sheet2 <- CoBtoRegressionReadyData(cobScot,scotzones,geoffsInGBplusScots,c(1991,2001,2011))

sheet2$rich <- 0
sheet2$rich[sheet2$cob %in% rich] <- 1

#Darnit, doesn't automatically overwrite internal append.
outputTofile(inputdata = sheet2, splitByFactor = 'rich', title = 'Scot-old-scots: Scots zones in new data RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/ScotsRichPoor.txt', lmfunction = 'xij11 ~ xij91 + w91q', myAppend = T)


#~~~~~~~~~~~~~~~~~~~~~~~
#REGRESSIONS 2. Scots original urban-----
#~~~~~~~~~~~~~~~~~~~~~~~

#Use geoffsInGBplusScots: same names in both, to make comparison exact

sheet <- CoBtoRegressionReadyData(cobs,scotsUrban,geoffsInGBplusScots,c(1991,2001,2011))

sheet$rich <- 0
sheet$rich[sheet$cob %in% rich] <- 1

outputTofile(inputdata = sheet, splitByFactor = 'rich', title = 'GB-new-scots: Scots orig urban in new data RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/ScotsOrigUrbanRichPoor.txt', lmfunction = 'xij11 ~ xij91 + w91q')

#~~
sheet2 <- CoBtoRegressionReadyData(cobScot,scotsUrban,geoffsInGBplusScots,c(1991,2001,2011))

sheet2$rich <- 0
sheet2$rich[sheet2$cob %in% rich] <- 1

#Darnit, doesn't automatically overwrite internal append.
outputTofile(inputdata = sheet2, splitByFactor = 'rich', title = 'Scot-old-scots: Scots orig urban zones in new data RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/ScotsOrigUrbanRichPoor.txt', lmfunction = 'xij11 ~ xij91 + w91q', myAppend = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quick comparison of contiguities old n new----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Just using sheets above, load original w91q n compare
#eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZones.csv')
eViewsFile <- read_csv('R_data/estimation_aggEmploymentToLargerZonesDeltaEmpUrbanRuralTTWA.csv')

eViewsFile$rich <- 0
eViewsFile$rich[eViewsFile$CoB %in% rich] <- 1
table(eViewsFile$rich)

eViewsFileUrban <- eViewsFile[eViewsFile$urbanFiftyPercentPlus==1,]

#17 yes 5 no
table(unique(eViewsFile$CoB) %in% unique(sheet$cob))

#I should probably just run the regressions on this again first to see.
#tst <- lm(data = eViewsFileUrban, formula = xij2011 ~ xij1991 + w91q)

outputTofile(inputdata = eViewsFileUrban, splitByFactor = 'rich', title = 'The original eViewsFile URBAN RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/origEviewsFileUrbanRichPoor.txt', lmfunction = 'xij2011 ~ xij1991 + w91q')

outputTofile(inputdata = eViewsFile, splitByFactor = 'rich', title = 'The original eViewsFile ALL RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/origEviewsFileAllRichPoor.txt', lmfunction = 'xij2011 ~ xij1991 + w91q')

#Additional regressors
outputTofile(inputdata = eViewsFileUrban, splitByFactor = 'rich', title = 'The original eViewsFile more regressors URBAN RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/origEviewsFileMoreRegressorsUrbanRichPoor.txt', lmfunction = 'xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q')

outputTofile(inputdata = eViewsFile, splitByFactor = 'rich', title = 'The original eViewsFile more regressors ALL RICH vs POOR (1 vs 0)', file = 'regressionOutputs/3census/tests_GBScots/origEviewsFileMoreRegressorsAllRichPoor.txt', lmfunction = 'xij2011 ~ xij1991 + lph91 + lpophs91 + migsh91minusown + w91q')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~FIVE CENSUS GB EMPLOYMENT/ECON ACTIVE PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#All five censuses to stitch. So out of "economically active" what proportion are unemployed?
#With some confusion over where students go so need to read that.
#Then once saved, stick all through the geog reassign code.

#Update: reminder - you can't do % unemployed until AFTER you've summed counts in the new zones. Remember?

#1971 Econ Active----

#Want to end with ... let's check to match format of previous
#chk71 <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/1971_econActive_from_71EDs_to_91_aggPostcodeSectors.csv')
#chk81 <- read_csv('StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/1981_econActive_from_81EDs_to_91_aggPostcodeSectors.csv')

#Yup, just need econ active as 100%, unemployed count and then get % employed.
#Just gotta decide exactly what's EA for some of these. 

ea71 <- read_csv('1971/GreatBritain/gb_econActive1971.csv')

#Staring at 71 ea table to get codes.
#Male is in one column, female in two (cos early censuses were sexist as well as racist.)
#I think they're all total EA columns for ages 15 to 75+. Hmm...
#Total = 257+258+259
#Working = 260+263+266
#Seeking work = 261+264+267
#Sick = 262+265+268 (which may count as not econ active but defs don't say. Hum.)

ea71sums <- data.frame(total = apply(ea71[,c("c71s05_257","c71s05_258","c71s05_259")],1,sum),
                   working = apply(ea71[,c("c71s05_260","c71s05_263","c71s05_266")],1,sum),
                   seekingwork = apply(ea71[,c("c71s05_261","c71s05_264","c71s05_267")],1,sum),
                   sick = apply(ea71[,c("c71s05_262","c71s05_265","c71s05_268")],1,sum))

#Use total of working and seeking work for EA total. 
#Seeking work as unemployed
#ps I thought tibble was lazy eval, could do percent in creation, but seems not.
ea71final <- data.frame(
  zone = ea71$`Zone Code`,
  econActive = apply(ea71sums[,c("working","seekingwork")],1,sum),
  unemployed = ea71sums[,"seekingwork"]) 
#%>% 
#  mutate(percentEmployed = ((econActive-unemployed)/econActive)*100)

#There appears to be one zone where no-one is employed...?
#zonez <- ea71final %>% filter(percentEmployed < 1) %>% dplyr::select(zone)

#thing <- ea71[ea71$`Zone Code` %in% zonez$zone,]

#Hmm, some in England.I suspect city centre areas e.g. in London. Leave in...? Issue with summing? Note.

#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its71,ea71final,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#Add percent employed column
result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100

df <- data.frame(result)

#save both as shapefile and dataframe
writeSpatialShape(result,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive/1971econActive.shp')

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971econActive.csv')





#1981 Econ active----

ea81 <- read_csv('1981/GreatBritain/gb_81_econActive.csv')

#total persons in employment (full-time...) = 789
#total persons not in employment = 859
#total persons self-employed = 705
ea81final <- data.frame(zone = ea81$`Zone ID`,
                        econActive = ea81$`[81sas090789]`+ea81$`[81sas090859]`+ea81$`[81sas090705]`,
                        unemployed = ea81$`[81sas090859]`) 
#%>% 
#  mutate(percentEmployed = ((econActive-unemployed)/econActive)*100)

#Again with the many NaN... that's a lot! Why? Mark and look in qgis
#Might be one country.
#Oh it's various spots in national parks and city centres. OK.
#table(0 + is.nan(ea81final$percentEmployed))
#ea81final$isnan <- 0 + is.nan(ea81final$percentEmployed)
#write_csv(ea81final,'R_data/isnancheck.csv')


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its81,ea81final,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100
df <- data.frame(result)

writeSpatialShape(result,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive/1981econActive.shp')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1981econActive.csv')


#1991 Econ active----

#Where we will need to do the direct summing to the aggregated zones. But first the data.
ea91 <- read_csv('1991/GreatBritain/91_gb_econActive.csv')

#1: total persons
#20: econ active
#115: on govt scheme (gonna count as unemployed)
#134: unemployed
#172: econ inactive

ea91final <- data.frame(zone = ea91$`Zone ID`,
                        econActive = ea91$l080020,
                        unemployed = ea91$l080115 + ea91$l080134)



#ZONE AGGREGATION DIRECTLY (cos 91 is the decade we use the geog for)

#Annoyingly I can't find where I did 91 econ active for Scotland. So adapt GB cob.
pcs91shp <- readOGR("C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Yup
shp_df <- data.frame(pcs91shp)
shp_df$label <- as.character(shp_df$label)
shp_df$fnl_rsL <- as.character(shp_df$fnl_rsL)

#work only on those with more than one zone to aggregate
shp_df <- shp_df[!is.na(shp_df$fnl_rsL),]

#Each element a list of zones to combine
zonez <- lapply(c(1:nrow(shp_df)), 
                function(x) c(shp_df$label[x],
                              unlist(lapply(shp_df$fnl_rsL[x], function(y) strsplit(y,"\\|"))))
)


#most will remain the same...
ea91final$aggID <- ea91final$zone

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  ea91final$aggID[ea91final$zone %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- ea91final[,2:4] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
zeroes <- agg2[apply(agg2[,2:3],1,sum)==0,]
zeroes <- merge(zeroes[,1],ea91final,by.x = 'label',by.y = 'zone')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
table(as.character(zeroes$label))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

#add in percent
geogz$percentEmployed <- ((geogz$econActive-geogz$unemployed)/geogz$econActive)*100

geogzdf <- data.frame(geogz)

writeSpatialShape(geogz,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive/1991econActive.shp')
write_csv(geogzdf,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991econActive.csv')



#2001 Econ active----

#No multiple tick box option in CASWEB for 2001 means...
ea01scot <- read_csv('2001/Scotland/2001_Scots_economicallyActive.csv')
ea01eng <- read_csv('2001/England/england_2001_econActive_univariate.csv')
ea01wales <- read_csv('2001/Wales/wales_2001_econActive_univariate.csv')

#names(ea01scot)
#names(ea01eng)
#names(ea01wales)

#Just 02 and 12 for econ active and unemployed respectively.
ea01 <- do.call(rbind, list(ea01scot,ea01eng,ea01wales))

#Which is already in the right form. Yay! Just a change of names for consistency.
names(ea01) <- c('zone','econActive','unemployed')

#Write in case I need to reload without using readr
write_csv(ea01,'R_data/ea01.csv')


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

#uh oh, zone issue. Err. That's fine. Whut?
#Oh wait, we've had this before with a library clash...
table(ea01$zone %in% its01$zone_code)

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its01,ea01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#Err. It seemed to work anyway. Now I'm puzzled. Oh well, it worked!
result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100
df <- data.frame(result)

writeSpatialShape(result,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive/2001econActive.shp')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2001econActive.csv')


#2011 econ Active----

ea11 <- read_csv('2011/GreatBritain/2011_econActive/GB_2011_econActive_tidied.csv')

#f243: total
#f244: econ active
#f248: unemployed
#f251: econ inactive
#Ignore rest

ea11final <- ea11 %>% dplyr::select(code = GEO_CODE,econActive = F244, unemployed = F248)


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its11,ea11final,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

result$percentEmployed <- ((result$econActive-result$unemployed)/result$econActive)*100
df <- data.frame(result)

writeSpatialShape(result,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive/2011econActive.shp')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2011econActive.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5-census GB econ active checks----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Pretty pic comparing all. And these are in the right order, good...
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*Active*', full.names = T)

fivez <- lapply(files5,read_csv)

years <- seq(from = 1971, to = 2011, by = 10)
for(x in c(1:5)) fivez[[x]]$year <- years[x]

#Ready for combining
allz <- do.call("rbind",fivez)

#Order by a particular decade. Only need unemployment
unemp <- allz[,c("label", "percentEmployed","year")] %>% 
 spread(year,percentEmployed)

unemp <- unemp[order(-unemp$`1971`),] %>% 
  mutate(order = c(1:nrow(unemp)))

unemp <- unemp %>% 
  gather(key = year, value = percentEmployed, `1971`:`2011`)

ggplot(unemp,aes(y = percentEmployed, x = order, colour = year)) +
  geom_line()

#Well that didn't work! I mean, it did in theory, just not in practice.
#OK so let's just do some boxplots instead. I can come on to regional ones in a mo.
ggplot(unemp,aes(y = percentEmployed, x = year)) +
  geom_boxplot()

#ranks
empranks <- unemp %>% group_by(year) %>% 
  mutate(rank = rank(percentEmployed,ties.method = 'random'))

#wide to view in QGIS
empranks <- empranks %>% 
  dplyr::select(label,year,rank) %>% 
  spread(year,rank)
  
write_csv(empranks,'R_data/empranks.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~FIVE CENSUS GB COUNTRY OF BIRTH PROCESSING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Good lord the 3 census harmonising was hideous now I look at it. 
#I wonder if this will be equally hideous? No! Easy!
#I think I can use Scots names but need to check

names <- read.csv("VariableCoding/1971to2011_CountryOfBirthCategories_Scotland.csv", check.names = F, as.is = T)

#Updates:
#1991 LBS: Caribbean under Americas needs distinguishing from Caribbean/New Commonwealth
names[35,4] <- "Caribbean [heading]"

#Apply those labels to column names, checking they match as we go along

#1971----

#Able to download 71 and 81 for the whole of GB. Is the data all there...?
seventyOne <- read_csv('1971/GreatBritain/greatBritain_CoB1971.csv')

#Yup, all values populated by the look of it.
lapply(names(seventyOne), function(x) table(0 + is.na(seventyOne[,x])) )

#And country codes easy to pick out? Yup!
#e       s       w 
#103129  15890   6457 
table(substring(seventyOne$`Zone Code`,1,1))

#So this has male-female and we want to add country names.
#We've done this before for Scotland with exactly the same data...
#In CoB_columnNameUpdate.R


####
#First-up: sum male/female into single columns
#Just column-pair sums e.g. 439/400 are England, male/female
#Staring at casweb to check.
all71 <- seventyOne[,c(1,2)]

#Take pairs of male/female columns, sum them into new
for(n in seq(from = 3, to = 34, by = 2)){
  
  all71[,as.character(n)] <- seventyOne[,n] + seventyOne[,(n+1)]
  
}

#First two remain the same
names(all71) <- c(names(all71)[c(1:2)],names[1:16,1])

#remove 'new commonwealth' - it's a sum column, we don't want those
all71 <- all71[,-9]

#OK, checked. That's named and summed...
write_csv(all71,"1971/GreatBritain/1971_GreatBritain_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv")

#1981----

#Doesn't need m/f columns summing. Let's check the rest of it.
eightyOne <- read_csv('1981/GreatBritain/greatBritain_CoB1981.csv')

#Yup, all values populated by the look of it.
lapply(names(eightyOne), function(x) table(0 + is.na(eightyOne[,x])) )

#Trickier to pick out which country is which... No country-specific subcode
eightyOne$`Zone ID`[sample(100)]
#But we don't necessarily need to do that as long as it does match the GB
#zone shapefile. (Which needs collating still...)

#Staring a casweb table to check...
#two total columns to exclude: 320 is all. 341 is new commonwealth. Leaves 19.
eightyOne <- eightyOne[,-c(2,9)]

#Double-check the match is correct. Tick.
namecheck81 <- data.frame(codes = names(eightyOne)[2:19], names = names$`1981`[c(1:6,8:19)])

names(eightyOne) <- c("Zone ID",as.character(namecheck81$names))

#Done. Save.
write_csv(eightyOne,"1981/GreatBritain/1981_GreatBritain_CoB_EnumDistrict_countryNamesAdded.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Checks for 5-census GB COB harmonisation----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#First, can I use the GB 91-11 harmonised I've already done? What does it look like?
#91 will be the same as 01 and 11 here.
GB91_COB <- readOGR('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth','GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch') %>% 
  data.frame()

#Put those together so I can load in Excel and look
seventyOne <- read_csv('1971/GreatBritain/1971_GreatBritain_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv') 
eightyOne <- read_csv('1981/GreatBritain/1981_GreatBritain_CoB_EnumDistrict_countryNamesAdded.csv')

names(seventyOne)[3:17] %>% length
names(eightyOne)[2:19] %>% length
names(GB91_COB)[4:36] %>% length

gbCoBs <- data.frame(
  seventyOne = c(names(seventyOne)[3:17],rep(' ',33-15)),
  eightyOne = c(names(eightyOne)[2:19],rep(' ',33-18)),
  ninetyOneTo11 = names(GB91_COB)[4:36]
)

#Actually want row names for a change
write.csv(gbCoBs,'VariableCoding/GB_5Census_91to11alreadyHarmonisedFrom3census.csv')

#Remind me what 5-census categories we had just for Scotland?
scots5censusCOB <- read_csv('C:/Users/SMI2/Dropbox/SheffieldMethodsInstitute/Census_DX/StitchOutputs/Scotland/LBS_postcodeSector_5Census_csvs/1971_CoB_from_71EDs_to_91_aggPostcodeSectors.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 CENSUS GB PROCESSING FOR MIGRATION CONF----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Where I will be keeping to definite cats so a larger 'other' category than before
#And harmonising with existing GB 91-11 CoBs
#(GB data for 71/81 can be downloaded as is; the others are a pain)

#note that only the resulting 71 and 81 should need to then be geographically reassigned
#As 91-11 have already been done (10182 zones)
#But we still need to harmonise CoB across them all.

#So getting all that data and having a look again
seventyOne <- read_csv('1971/GreatBritain/1971_GreatBritain_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv') 
eightyOne <- read_csv('1981/GreatBritain/1981_GreatBritain_CoB_EnumDistrict_countryNamesAdded.csv')

ninetyOne <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp') %>% data.frame()

OhOne <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp') %>% data.frame()

Eleven <- readShapeSpatial('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp') %>% data.frame()

#Check the names are in the same order for the last three... yup
names(ninetyOne) == names(OhOne)
names(ninetyOne) == names(Eleven)

#FIVE CENSUS RECODE
#Recoding all of them here, no automation
fiveCensusCombo <- list(
  list(seventyOne, eightyOne, ninetyOne, OhOne, Eleven),#all data to sum columns on
  list('1,2','1','1,2,3','1,2,3','1,2,3'),#columns to keep from those
  list('England','3','2','15','15','15'),
  list('Scotland','4','3','16','16','16'),
  list('Wales','5','4','17','17','17'),
  list('Irish Republic','7','6','6','6','6'),
  list('Rest of UK','6','5','4,5,18','4,5,18','4,5,18'),
  list('India','12','11','24','24','24'),
  list('Pakistan','13','16','25','25','25'),
  list('Europe','16','17,18','12,13,27:32','12,13,27:32','12,13,27:32'),
  list('Rest of world','8:11,14,15,17','7:10,12:15,19','7:11,14,19:23,26,33:36','7:11,14,19:23,26,33:36','7:11,14,19:23,26,33:36')#North Africa has to go here due to 2011 mismatch in Eng/Scot. South-East Asia has to go here too. 'Other Asia' 91 is fine here too, see 91 defs.
)

#Then work out what's left.
#check what that looks like
for(i in 3:length(fiveCensusCombo)){
  
  print(paste(i,
              fiveCensusCombo[i][[1]][1],
              fiveCensusCombo[i][[1]][2],
              fiveCensusCombo[i][[1]][3],
              fiveCensusCombo[i][[1]][4],
              fiveCensusCombo[i][[1]][5],
              fiveCensusCombo[i][[1]][6],
              sep = " "))
  
}

#Returns list of re-coded dataframes
#"Error: column_recode_lists_are_all_correct_length not equal to TRUE"
#Means lists defining recodes aren't all of a length that recodes in each dataframe
results <- recode(fiveCensusCombo)

looksee <- results[[4]]

#Save as five dataframes, ready for geog re-assigning (apart from '91 which is correct already)
savens <- c('71ED','81ED','91LBS_PCS','01LBS_PCS','11LBS_PCS')

lapply(1:length(savens),function(x) write.csv(results[[x]],
                                              paste0("VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/",savens[x],".csv"),
                                              row.names = F))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Next: merge the awkward 71 country shapefiles here not in QGIS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Update: just merging all of them here as QGIS is being a pain
#1981 did work in QGIS to that's not done here. Just to make it nice and messy.

#Small job first: 71 ED for Scotland needs the column name editing to match
#Scot just has ZONE_CODE in capitals, needs to be lower case.
scot71 <- readOGR('C:/Data/MapPolygons/Merges/1971GB','scotland_ed_1971')
names(scot71)[1] <- 'zone_code'

#Just check it's matching now. Oops, should have loaded wales, much smaller!
eng71 <- readOGR('C:/Data/MapPolygons/Merges/1971GB','england_ed_1971')
names(scot71)[1] == names(eng71)[1]#tick

#And what's happened to wales...?
wales71 <- readOGR('C:/Data/MapPolygons/Merges/1971GB','wales_ed_1971')

#Keep only zone code
scot71 <- scot71[,1]
eng71 <- eng71[,1]
wales71 <- wales71[,1]

gb71 <- rbind(scot71,eng71,makeUniqueIDs = T)
gb71 <- rbind(gb71,wales71,makeUniqueIDs = T)

writeOGR(gb71, 'C:/Data/MapPolygons/Merges/1971GB','1971GB_enumerationDistrictMerge', driver = "ESRI Shapefile", overwrite_layer=TRUE)

# writeOGR(scot71, 'C:/Data/MapPolygons/Merges/1971GB','scotland_ed_1971', driver = "ESRI Shapefile", overwrite_layer=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Next: processing GB 71 and 81 enumeration district shapefiles----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~
#Already merged 3 countries in QGIS. Now just need to make sure we have single polygons per ID
#Load the GB files.
gb71eds <- readOGR('C:/Data/MapPolygons/Merges/1971GB','1971GB_enumerationDistrictMerge')
gb81eds <- readOGR('C:/Data/MapPolygons/Merges/1981GB','1981GB_enumerationDistrictMerge')

gb71eds_dissolveByID <- IDtoSingleRow(gb71eds,1)
gb81eds_dissolveByID <- IDtoSingleRow(gb81eds,1)

writeOGR(gb71eds_dissolveByID, "C:/Data/MapPolygons/GreatBritain/1971", "gb71eds_dissolvedByID", driver="ESRI Shapefile", overwrite_layer=TRUE)

writeOGR(gb81eds_dissolveByID, "C:/Data/MapPolygons/GreatBritain/1981", "gb81eds_dissolvedByID", driver="ESRI Shapefile", overwrite_layer=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Next: CoB-recoded 71 and 81 need aggregating to 91 new-PCS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#~~~~~~~~~~~~~~~~~~
#1971 COB----

#Get the data
cob71 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/71ED.csv")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(3:ncol(cob71))#from dta

result <- moveData(its71,cob71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#Do numbers match?
apply(cob71[,3:ncol(cob71)],2,sum)
df <- data.frame(result)
apply(df[,3:ncol(df)],2,sum)

#Some are NA. Same zones? Yup... Just the island off Barrow-in-Furness.
dfna <- df[is.na(df$England),]
dropz <- dfna$label %>% as.character()

#Drop those two.
result2 <- result[!(result$label %in% dropz),]

#Check again
apply(cob71[,3:ncol(cob71)],2,sum)
apply(data.frame(result2)[,3:ncol(result2)],2,sum)

#Some fairly hefty rounding errors I suspect but in the right ballpark

#Better column abbreviation
writeSpatialShape(result2,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1971_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~
#1981 COB----

#Get the data
cob81 <- read.csv("VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/81ED.csv")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:ncol(cob81))#from dta

result <- moveData(its81,cob81,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

head(result)

#Do numbers match?
apply(cob81[,2:ncol(cob81)],2,sum)
df <- data.frame(result)
apply(df[,3:ncol(df)],2,sum)

#We didn't lose those two zones this time but may need to later to keep everything matching.

#Better column abbreviation
writeSpatialShape(result,"StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1981_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 Census CoB: get them all matching----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Includes checking 91 to 11 are actually OK and checking we have all the same zones.
cob71shp <- readShapeSpatial("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1971_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")
cob81shp <- readShapeSpatial("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1981_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp")

cob71 <- cob71shp %>% data.frame
cob81 <- cob81shp %>% data.frame

#91 to 11 in CSV form - only needed CoB cats combining, were already in correct geog (I think. We'll see...)
cob91 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/91LBS_PCS.csv')
cob01 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/01LBS_PCS.csv')
cob11 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/11LBS_PCS.csv')

#Issue of those two zones missing in 71. OK to drop them from the rest I think. And they were?
#17FFFM 17FFFN
rmz <- cob81$label[!(cob81$label) %in% (cob71$label)]

cob81 <- cob81[!(cob81$label %in% rmz),]
cob91 <- cob91[!(cob91$label %in% rmz),]
cob01 <- cob01[!(cob01$label %in% rmz),]
cob11 <- cob11[!(cob11$label %in% rmz),]

#OK, all the same now, 10180 zones in total
table(cob71$label %in% cob91$label)

#Let's harmonise column names. Shapefile ones were abbreviated but they're all the same
lapply(list(cob71,cob81,cob91,cob01,cob11), names)
#Ah, keep on forgetting we're not doing pass by reference! Java head.
#lapply(list(cob71,cob81,cob91,cob01,cob11), function(x) names(x) <- names(cob11))

names(cob71) <- names(cob11)
names(cob81) <- names(cob11)

#Should probably save all those
write_csv(cob71,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/71LBS_PCS_sameZoneNumber.csv')
write_csv(cob81,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/81LBS_PCS_sameZoneNumber.csv')
write_csv(cob91,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/91LBS_PCS_sameZoneNumber.csv')
write_csv(cob01,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/01LBS_PCS_sameZoneNumber.csv')
write_csv(cob11,'VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/11LBS_PCS_sameZoneNumber.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert 3 census CoBs to CSVs----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cob91three <- 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_1991_CountryOfBirthRecode_91LBS_noZeroPCS_straightMatch.shp'
cob01three <- 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2001_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp'
cob11three <- 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_raw/CountryOfBirth/GB_2011_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp'

cobzThree <- lapply(list(cob91three,cob01three,cob11three), function(x) readShapeSpatial(x) %>% data.frame)

#Keerect
lapply(cobzThree, nrow)

namez <- c('91CoB_threeCensus','01CoB_threeCensus','11CoB_threeCensus')

lapply(1:length(namez),function(x) write_csv(cobzThree[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/",namez[x],".csv")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some checks----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sum(cobzThree[[1]]$Poland)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 Census CoB: to zone proportions----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cob71 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/71LBS_PCS_sameZoneNumber.csv')
cob81 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/81LBS_PCS_sameZoneNumber.csv')
cob91 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/91LBS_PCS_sameZoneNumber.csv')
cob01 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/01LBS_PCS_sameZoneNumber.csv')
cob11 <- read_csv('VariableCoding/GreatBritain/CountryOfBirth_fiveCensusRecodes_to91LBS/11LBS_PCS_sameZoneNumber.csv')

#Let's start with UK/non-UK zone percents. Save and look in QGIS.
cobz <- list(cob71,cob81,cob91,cob01,cob11)

#Dump columns we don't need
cobz <- lapply(cobz, function(x) x[,c(2,4:12)])

#Save those now as the CSV actual values before finding proportions
namez <- c('71CoBtotals','81CoBtotals','91CoBtotals','01CoBtotals','11CoBtotals')

lapply(1:length(namez),function(x) write_csv(cobz[[x]], paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/", namez[x],".csv")))

#Might leave Irish republic in its own category just to see what that looks like
#Actually, the easiest thing to do might be adding columns to this.
#So I want:
#UK total
#Non-UK total excluding Irish Rep
#Non-UK total including Irish Rep
#And then as zone percents once done

cobz <- lapply(cobz, function(x) {
  x$totalPop <- apply(x[,c(2:10)], 1, sum)
  return(x)})

cobz <- lapply(cobz, function(x) {
  x$UKtotal <- apply(x[,c(2,3,4,6)], 1, sum)
  return(x)})

cobz <- lapply(cobz, function(x) {
  x$nonUKtotal.exc.IreRepublic <- apply(x[,c(7:10)], 1, sum)
  return(x)})

cobz <- lapply(cobz, function(x) {
  x$nonUKtotal.inc.IreRepublic <- apply(x[,c(5,7:10)], 1, sum)
  return(x)})

#Just for uk/non-uk
cobzZonePercs <- lapply(cobz,function(x){
  
  x$UK.ZonePerc <- (x$UKtotal/x$totalPop)*100
  x$nonUK.excIre.ZonePerc <- (x$nonUKtotal.exc.IreRepublic/x$totalPop)*100
  x$nonUK.incIre.ZonePerc <- (x$nonUKtotal.inc.IreRepublic/x$totalPop)*100
  
  #prop table for the rest then join
  props <- prop.table(as.matrix(x[,c(2:10)]),1) * 100
  
  result <- cbind(x$label,props,x[,c(15:17)])
  
  return(result)
})

#save and looksee in QGIS
namez <- c('71zoneProportions','81zoneProportions','91zoneProportions','01zoneProportions','11zoneProportions')

lapply(1:length(namez),function(x) write_csv(cobzZonePercs[[x]],
                                              paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/",
                                                     namez[x],".csv")))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3 Census CoB: to zone proportions----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cob91three <- read_csv('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/91CoB_threeCensus.csv')
cob01three <- read_csv('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/01CoB_threeCensus.csv')
cob11three <- read_csv('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/11CoB_threeCensus.csv')

#Let's start with UK/non-UK zone percents. Save and look in QGIS.
cobz <- list(cob91three,cob01three,cob11three)

#Dump columns we don't need
cobz <- lapply(cobz, function(x) x[,c(2,4:36)])

#So I want:
#UK total
#Non-UK total excluding Irish Rep
#Non-UK total including Irish Rep
#And then as zone percents once done

cobz <- lapply(cobz, function(x) {
  x$totalPop <- apply(x[,c(2:34)], 1, sum)
  return(x)})

cobz <- lapply(cobz, function(x) {
  x$UKtotal <- apply(x[,c(2,3,13:16)], 1, sum)
  return(x)})

cobz <- lapply(cobz, function(x) {
  x$nonUKtotal.exc.IreRepublic <- apply(x[,c(5:12,17:34)], 1, sum)
  return(x)})

cobz <- lapply(cobz, function(x) {
  x$nonUKtotal.inc.IreRepublic <- apply(x[,c(4:12,17:34)], 1, sum)
  return(x)})

cobzZonePercs <- lapply(cobz,function(x){
  
  x$UK.ZonePerc <- (x$UKtotal/x$totalPop)*100
  x$nonUK.excIre.ZonePerc <- (x$nonUKtotal.exc.IreRepublic/x$totalPop)*100
  x$nonUK.incIre.ZonePerc <- (x$nonUKtotal.inc.IreRepublic/x$totalPop)*100
  
  #prop table for the rest then join
  props <- prop.table(as.matrix(x[,c(2:34)]),1) * 100
  
  result <- cbind(x$label,props,x[,c(39:41)])
  
  return(result)
})

#save and looksee in QGIS
namez <- c('91zoneProportionsThreeCensus','01zoneProportionsThreeCensus','11zoneProportionsThreeCensus')

lapply(1:length(namez),function(x) write_csv(cobzZonePercs[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/",namez[x],".csv")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 census Europe combo graph, inc. Rep Ireland----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load em all. We're after the total numbers.
files3 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/',pattern = '*CoB*', full.names = T)
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*totals*', full.names = T)

#Get them into the right order. Nearly gave me a heart attack...
files3 <- files3[c(3,1,2)]
files5 <- files5[c(3,4,5,1,2)]

threez <- lapply(files3,read_csv)
fivez <- lapply(files5,read_csv)

#So we're now after a single table for ggplot that will require each European county to have a column (then gathered) but 71 and 81 will all be set to zero for those.
#Even though they're not zero - but the "Europe" category for those contains those values. (Or should.)
#Equally, the "Europe" category for 91 to 11 will need to be set to zero

#So we actually only need 71 and 81 from fivez
#Keep only Europe
#And we only need the total
#NOTE, INCLUDING IRE REPUBLIC IN THIS ONE
fivez <- lapply(fivez[c(1,2)], function(x) x %>%
                  dplyr::select(Europe,`Irish Republic`) %>% 
                  summarise(Europe = sum(Europe + `Irish Republic`))%>%
                  mutate(Irish_Repu=0,Other_EU_m=0,Other_Euro=0,France=0,Germany=0,Italy=0,Spain=0,Poland=0,Romania=0) %>% 
                  gather(key = country, value = count))

#For 91 to 11, sum UK columns first and drop originals
#GB not UK so removed NI. Not sure if I should stick that in European total...
#No - we want non-UK totals. I didn't really need UK or GB here. Keep just in case.
# threez <- lapply(threez, function(x) x %>% 
#                    mutate(GB = Channel_Is + UK_part_no + England + Scotland + Wales) %>% 
#                    dplyr::select(-c(Channel_Is,UK_part_no,England,Scotland,Wales,Northern_I)) %>% 
#                    dplyr::select(GB,Irish_Repu,Other_EU_m,Other_Euro,France,Germany,Italy,Spain,Poland,Romania) %>% 
#                    summarise_each(funs(sum)))

#In fact we need to drop UK/GB for the graph, so...
threez <- lapply(threez, function(x) x %>% 
                   #mutate(GB = Channel_Is + UK_part_no + England + Scotland + Wales) %>% 
                   #dplyr::select(-c(Channel_Is,UK_part_no,England,Scotland,Wales,Northern_I)) %>% 
                   dplyr::select(Irish_Repu,Other_EU_m,Other_Euro,France,Germany,Italy,Spain,Poland,Romania) %>% summarise_each(funs(sum)) %>% 
                   mutate(Europe = 0) %>% 
                   gather(key = country, value = count))

#Add census years
years <- c(1971,1981)
for(x in c(1:2)) fivez[[x]]$year <- years[x]

years <- c(1991,2001,2011)
for(x in c(1:3)) threez[[x]]$year <- years[x]

#Ready for combining
allz <- rbind(do.call("rbind",fivez),do.call("rbind",threez))

#And now it's just a bar graph from those that I'm after.
output <- ggplot(allz,aes(x = year, y = count, fill = country)) +
  geom_bar(stat = 'identity', position = 'stack')

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 census Europe combo graph, exc. Rep Ireland----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load em all. We're after the total numbers.
files3 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/',pattern = '*CoB*', full.names = T)
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*totals*', full.names = T)

#Get them into the right order. Nearly gave me a heart attack...
files3 <- files3[c(3,1,2)]
files5 <- files5[c(3,4,5,1,2)]

threez <- lapply(files3,read_csv)
fivez <- lapply(files5,read_csv)

#save cob names
namezBoth <- data.frame(five = c(names(fivez[[1]]),rep(' ',26)), three = names(threez[[1]]))

write_csv(namezBoth,'R_data/namesBothForGB3n5census.csv')

#So we're now after a single table for ggplot that will require each European county to have a column (then gathered) but 71 and 81 will all be set to zero for those.
#Even though they're not zero - but the "Europe" category for those contains those values. (Or should.)
#Equally, the "Europe" category for 91 to 11 will need to be set to zero

#So we actually only need 71 and 81 from fivez
#Keep only Europe
#And we only need the total
#NOTE, INCLUDING IRE REPUBLIC IN THIS ONE
fivez <- lapply(fivez[c(1,2)], function(x) x %>%
                  dplyr::select(Europe) %>% 
                  summarise(Europe = sum(Europe))%>%
                  mutate(Other_EU_m=0,Other_Euro=0,France=0,Germany=0,Italy=0,Spain=0,Poland=0,Romania=0) %>% 
                  gather(key = country, value = count))

#For 91 to 11, sum UK columns first and drop originals
#GB not UK so removed NI. Not sure if I should stick that in European total...
#No - we want non-UK totals. I didn't really need UK or GB here. Keep just in case.
# threez <- lapply(threez, function(x) x %>% 
#                    mutate(GB = Channel_Is + UK_part_no + England + Scotland + Wales) %>% 
#                    dplyr::select(-c(Channel_Is,UK_part_no,England,Scotland,Wales,Northern_I)) %>% 
#                    dplyr::select(GB,Irish_Repu,Other_EU_m,Other_Euro,France,Germany,Italy,Spain,Poland,Romania) %>% 
#                    summarise_each(funs(sum)))

#In fact we need to drop UK/GB for the graph, so...
threez <- lapply(threez, function(x) x %>% 
                   #mutate(GB = Channel_Is + UK_part_no + England + Scotland + Wales) %>% 
                   #dplyr::select(-c(Channel_Is,UK_part_no,England,Scotland,Wales,Northern_I)) %>% 
                   dplyr::select(Other_EU_m,Other_Euro,France,Germany,Italy,Spain,Poland,Romania) %>% summarise_each(funs(sum)) %>% 
                   mutate(Europe = 0) %>% 
                   gather(key = country, value = count))

#Add census years
years <- c(1971,1981)
for(x in c(1:2)) fivez[[x]]$year <- years[x]

years <- c(1991,2001,2011)
for(x in c(1:3)) threez[[x]]$year <- years[x]

#Ready for combining
allz <- rbind(do.call("rbind",fivez),do.call("rbind",threez))

allz$count <- allz$count/1000000

commonHeight = 4
commonWidth = 6

#And now it's just a bar graph from those that I'm after.
output <- ggplot(allz,aes(x = factor(year), y = count, fill = country)) +
  geom_bar(stat = 'identity', position = 'stack') +
  xlab('census year') +
  ylab('count (millions)') +
  scale_fill_brewer(palette = 'Paired',direction = 1)
output

ggsave('R_outputs/ShefMigration2017/Europe_5censusCount.png', height = commonHeight, width = commonWidth, dpi = 150)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 Census all graphing, totals----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*totals*', full.names = T)

#Get them into the right order.
files5 <- files5[c(3,4,5,1,2)]

fivez <- lapply(files5,read_csv)

#Total for UK
fivez <- lapply(fivez, function(x) {x$UKtotal <- apply(x[,c(2:4,6)], 1, sum); return(x)})

#Add census years
years <- seq(from = 1971, to = 2011, by = 10)
for(x in c(1:5)) fivez[[x]]$year <- years[x]

#Ready for combining
allz <- do.call("rbind",fivez)

#Summarise each and gather
allzCount <- allz %>% 
  dplyr::select(2:12) %>% 
  group_by(year) %>% 
  summarise_each(funs(sum)) %>% 
  gather(key = place, value = count, England:UKtotal)

allzCount$count <- allzCount$count/1000000
places <- unique(allzCount$place)

#non UK inc Ire
output <- ggplot(allzCount[allzCount$place %in% places[c(4,6:9)],], aes(x = factor(year), y = count, fill = place)) +
  geom_col() +
  scale_fill_brewer(palette = 'Paired',direction = 1) +
  xlab('census year') +
  ylab('count (millions)')
output

ggsave('R_outputs/ShefMigration2017/5census_nonUKincIre.png', height = commonHeight, width = commonWidth, dpi = 150)

#All (but UK total not UK bits)
output <- ggplot(allzCount[!(allzCount$place %in% places[c(1:3,5)]),], aes(x = factor(year), y = count, fill = place)) +
  geom_col() +
  xlab('census year') +
  ylab('count (millions)') +
  scale_fill_brewer(palette = 'Paired',direction = 1)
output

ggsave('R_outputs/ShefMigration2017/5census_allPlusUKtotal.png', height = commonHeight, width = commonWidth,dpi = 150)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 Census non-UK graphing (exc Ire Rep)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load 5 census proportions
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*Proportions*', full.names = T)

#Get them into the right order.
files5 <- files5[c(3,4,5,1,2)]

fivez <- lapply(files5,read_csv)

years <- seq(from = 1971, to = 2011, by = 10)

#So now it's already long. Nice sneezy.
for(x in 1:5) fivez[[x]]$year <- years[x]

fivez_df <- do.call("rbind",fivez)

output <- ggplot(fivez_df, aes(x = factor(year), y = nonUK.excIre.ZonePerc)) +
  geom_boxplot() +
  scale_y_log10()
output

#And as alpha value points?
output <- ggplot(fivez_df, aes(x = factor(year), y = nonUK.excIre.ZonePerc)) +
  geom_point(size = 3, alpha = .01) +
  scale_y_log10()

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Modal non-UK CoB for those zones in the 5-10% bracket (3 census)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Already got five from above...

#Which I should then be able to map.
#But should prob use 3-census to get better list of CoBs in that
files3 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/',pattern = '*Proportions*', full.names = T)

#Get them into the right order.
files3 <- files3[c(3,1,2)]

threez <- lapply(files3,read_csv)

#label up same as the QGIS file for non-UK.
threez <- lapply(threez, function(x) {x$nonUKpropsBuckets <- '0-5'; return(x)})
threez <- lapply(threez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 5 & x$nonUK.excIre.ZonePerc < 10] <- '5-10'; return(x)})
threez <- lapply(threez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 10 & x$nonUK.excIre.ZonePerc < 20] <- '10-20'; return(x)})
threez <- lapply(threez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 20 & x$nonUK.excIre.ZonePerc < 30] <- '20-30'; return(x)})
threez <- lapply(threez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 30 & x$nonUK.excIre.ZonePerc < 68] <- '30-68'; return(x)})

table(threez[[1]]$nonUKpropsBuckets)

#https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
#Just for non-UK countries
threez <- lapply(threez, function(x) {
  x$modalCoB.excUK.Ire <- names(x[,c(5:12,17:34)])[apply(x[,c(5:12,17:34)],1,which.max)]
  return(x)
  })

#Save to look at in map.
#save and looksee in QGIS
namez <- c('91modalThreeCensus','01modalThreeCensus','11modalThreeCensus')

lapply(1:length(namez),function(x) write_csv(threez[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/",namez[x],".csv")))


#Sooo... a bar of the modal non-UK CoBs for each of the above?
#For 2011 to start with
modal11 <- threez[[3]]
#modal11 <- modal11[modal11$nonUKpropsBuckets!='0-5',]

#Summarise. What's the count of modal CoB for each of the proportion groups?
modal11countsPerPropGroup <- modal11 %>% 
  group_by(nonUKpropsBuckets,modalCoB.excUK.Ire) %>% 
  summarise(count = n())

output <- ggplot(modal11countsPerPropGroup, aes(x = 1, y = count, fill = modalCoB.excUK.Ire)) +
  geom_col() +
  facet_wrap(~nonUKpropsBuckets, scales = 'free_y')
output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Modal non-UK CoB for those zones in the 5-10% bracket (5 census)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*Proportions*', full.names = T)

#Get them into the right order.
files5 <- files5[c(3,4,5,1,2)]

fivez <- lapply(files5,read_csv)

fivez <- lapply(fivez, function(x) {x$nonUKpropsBuckets <- '0-5'; return(x)})
fivez <- lapply(fivez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 5 & x$nonUK.excIre.ZonePerc < 10] <- '5-10'; return(x)})
fivez <- lapply(fivez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 10 & x$nonUK.excIre.ZonePerc < 20] <- '10-20'; return(x)})
fivez <- lapply(fivez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 20 & x$nonUK.excIre.ZonePerc < 30] <- '20-30'; return(x)})
fivez <- lapply(fivez, function(x) {x$nonUKpropsBuckets[x$nonUK.excIre.ZonePerc >= 30 & x$nonUK.excIre.ZonePerc < 68] <- '30-68'; return(x)})

for(x in 1:5) print(table(fivez[[x]]$nonUKpropsBuckets))

#https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
#Just for non-UK countries/groups
fivez <- lapply(fivez, function(x) {
  x$modalCoB.excUK.Ire <- names(x[,c(7:10)])[apply(x[,c(7:10)],1,which.max)]
  return(x)
})

#Save to look at in map.
#save and looksee in QGIS
namez <- c('71modalFiveCensus','81modalFiveCensus','91modalFiveCensus','01modalFiveCensus','11modalFiveCensus')

lapply(1:length(namez),function(x) write_csv(fivez[[x]],paste0("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/",namez[x],".csv")))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5 CENSUS GB DATA COLLATION----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Country of birth / weights matrix etc for London area. Will do London local authorities I think.

#So the cobtoregressionready function requires:
#listOfSHPS, zonesToKeep, CoBsToKeep, UKcobs, year4weightsNmigshare, censusYear

#Ah note Bernard actually wants the weights matrix itself. I should be able to get the function to write it to CSV as it's being made.

#~~~~~~~~~~~~~~~~~~~~~
#Reduce to London LADs
#~~~~~~~~~~~~~~~~~~~~~
lads11 <- readOGR('C:/Data/MapPolygons/England/2011/England_lad_2011_gen_clipped','England_lad_2011_gen_clipped')

#London LADs via wikipedia. Three alterations to match:
#"City of Westminster" "Richmond"            "Kingston"
#Are Westminster and the other two upon Thames
LonLAs <- c('City of London','Westminster','Kensington and Chelsea','Hammersmith and Fulham','Wandsworth','Lambeth','Southwark','Tower Hamlets','Hackney','Islington','Camden','Brent','Ealing','Hounslow','Richmond upon Thames','Kingston upon Thames','Merton','Sutton','Croydon','Bromley','Lewisham','Greenwich','Bexley','Havering','Barking and Dagenham','Redbridge','Newham','Waltham Forest','Haringey','Enfield','Barnet','Harrow','Hillingdon')

#tick
table(LonLAs %in% lads11$NAME)

londonLADS11 <- lads11[lads11$NAME %in% LonLAs,]

#Great Britain tweaked 91 wards.
wardz91 <- readOGR('C:/Data/MapPolygons/GreatBritain/1991','GB_wards_pcs_agg4correctCount')

wardz91$londonLA <- wardz91 %over% londonLADS11

wardz91london <- wardz91[!is.na(wardz91$londonLA$NAME),]

plot(wardz91london, col="GREEN")
plot(londonLADS11, col="RED", add=T)

#Some only just connected. Checking this to see about reducing based on area in contact.
#https://stackoverflow.com/questions/14208016/find-best-matching-overlapping-polygons-in-r
#Oh that's REALLY slow. Perhaps this will do for now... Can do intersection later if necessary.
#Oh oh except some of those are obviously right on the edge. So. Intersection in QGIS for speed then!
#Need to save london LADs for that.
writeSpatialShape(londonLADS11,'R_data/londonLADs11.shp')
#And actually I can just use the subset of wards too - we just want to remove those with very small overlaps
writeSpatialShape(wardz91london[,c('label')],'R_data/london91WardsOverLADs11.shp')

#save those for later.
#saveRDS(wardz91london,'R_data/wards91agg_London_localAuth_over.rds')

wardz91london <- readRDS('R_data/wards91agg_London_localAuth_over.rds')

#Done in QGIS intersections. Reload.
interz <- readOGR('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/Intersects/otherIntersects','91aggwardsOverLondonLADs')

#Need to look at this a bit to work out how to use it.
#It doesn't include the full area of the edge wards so we need to find out which ones those are to make a comparison.
interz$area <- gArea(interz,byid = T)
View(data.frame(interz))

#Right, so we need to the areas from the full wards. Check there's a full match... yup
table(interz$label %in% wardz91london$label)

#Most of those should match
wardzArea <- data.frame(label = wardz91london$label,fullwardarea = gArea(wardz91london,byid = T))

interz <- merge(interz,wardzArea,by='label')

#Ah, right, no: measuring wrong thing. Look again. The point: the *sum* of ward areas, not each separate one. Sooo.
interz_df <- interz %>% data.frame()

interz_df <- interz_df %>% 
  group_by(label) %>% 
  mutate(totWardAreaInLAD = sum(area))

#Yes. It's now those numbers we need to check for diffs.
plot(interz_df$totWardAreaInLAD,interz_df$fullwardarea)

#Check overall match
interz_df$proportionInLAD <- (interz_df$totWardAreaInLAD/interz_df$fullwardarea)*100

#OK, some checks
table(interz_df$proportionInLAD < 90)

plot(wardz91london, col="GREEN")
plot(londonLADS11, col="RED", add=T)

#>80 to >50 doesn't seem to make much difference. Looks like the right set now.
plot(wardz91london[wardz91london$label %in% interz_df$label[(interz_df$proportionInLAD > 50)],], col="GREEN")
plot(londonLADS11, col="RED", add=T)

wardz91londonSub <- wardz91london[wardz91london$label %in% interz_df$label[(interz_df$proportionInLAD > 50)],]

plot(wardz91londonSub, col="GREEN")
plot(londonLADS11, col="RED", add=T)

#Perfect. Save!
saveRDS(wardz91londonSub,'R_Data/wards91londonLADmatchMoreThan50percentAreaOverlap.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Collate basic 5 census data for London wards----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get the data
#reload london wards
wardz91londonSub <- readRDS('R_Data/wards91londonLADmatchMoreThan50percentAreaOverlap.rds')

#30/10/17
#check order... tick
three <- read_csv('R_data/3censusCountryOfBirthCountsLondon.csv')
five <- read_csv('R_data/5censusCountryOfBirthCountsLondon.csv')

df <- data.frame(wardz91londonSub)

chk <- three$label[1:760]
table(df$label == chk)

chk <- five$label[1:760]
df$label == chk

#save as shapefile.
writeOGR(wardz91londonSub[,'label'], "R_data","london1991wards", driver="ESRI Shapefile", overwrite_layer = T)

#~~~~~~~~~

files3 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/',pattern = '*CoB*', full.names = T)
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*totals*', full.names = T)

#Get them into the right order. Nearly gave me a heart attack...
files3 <- files3[c(3,1,2)]
files5 <- files5[c(3,4,5,1,2)]

threez <- lapply(files3,read_csv)
fivez <- lapply(files5,read_csv)

#Three census sheets need the second column to be where CoBs start (because of the function. Robust much? Naaah.)
threez <- lapply(threez,function(x) x[,c(2,4:ncol(x))])

#Those need reattaching to shapefiles to work. Given we're only looking at London here, might just attach to that directly.
#Or copies thereof... just remind what the london wards file looks like
View(data.frame(wardz91londonSub))
#So we only need the labels
wardz91londonSub <- wardz91londonSub[,c('label')]

threeshp <- lapply(threez, function(x) {
  tmp <- wardz91londonSub
  merge(tmp,x,by='label')
  })

fiveshp <- lapply(fivez, function(x) {
  tmp <- wardz91londonSub
  merge(tmp,x,by='label')
  })

#Ah: conversion to shapefiles = space replaced with periods in var names. Marvelous.
#Oh wait... I think it happens somewhere later in the function. This is odd.
cobz <- c("Irish Republic","India","Pakistan","Europe","Rest of world" )
ukonly <- c("England","Scotland","Wales","Rest of UK")

sheet5 <- CoBtoRegressionReadyData(fiveshp,wardz91londonSub$label,CoBsToKeep = cobz, UKcobs = ukonly,year4weightsNmigshare = c(1971,1981,1991,2001,2011),censusYear = c(1971,1981,1991,2001,2011))

#Test why not working in function. Direct assign.
# listOfSHPS <- fiveshp
# zonesToKeep <- wardz91londonSub$label
# CoBsToKeep <- cobz
# UKcobs <- ukonly
# year4weightsNmigshare <- c(1971,1981,1991,2001)
# censusYear <- c(1971,1981,1991,2001,2011)

#Aaand three census. Which might work, you never know.

cobz3 <- c("Irish_Repu","South_Afri","Africa_oth","Caribbean","South_Amer","Other_Midd","Other_EU_m","Other_Euro","Rest_of_wo","Australia","Kenya","Zimbabwe","Nigeria","Bangladesh","India","Pakistan","Hong_Kong","France","Germany","Italy","Spain","Poland","Romania","Turkey","United_Sta","Iran","China")

ukonly3 <- c("Channel_Is","UK_part_no","England","Scotland","Wales","Northern_I")

sheet3 <- CoBtoRegressionReadyData(threeshp,wardz91londonSub$label,CoBsToKeep = cobz3, UKcobs = ukonly3,year4weightsNmigshare = c(1991,2001,2011),censusYear = c(1991,2001,2011))

#Save both of those
write_csv(sheet5,'R_data/london_countryOfBirth_5census.csv')
write_csv(sheet3,'R_data/london_countryOfBirth_3census.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert 5 census econ active to CSVs
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*Active*', full.names = T)

fivez <- lapply(files5,read_csv)

years <- seq(from = 1971, to = 2011, by = 10)
for(x in c(1:5)) fivez[[x]]$year <- years[x]

#Ready for combining
allz <- do.call("rbind",fivez)

#Remove unnecessary columns and reduce to London wards we're currently using
allz <- allz %>% 
  dplyr::select(label,econActive:year) %>%
  filter(label %in% wardz91londonSub$label)

#save!
write_csv(allz,'R_data/londonWards_5census_econActive_and_unemployed.csv')

#then just area, acres
wardAreas <- data.frame(label = wardz91londonSub$label,acres = gArea(wardz91londonSub,byid = T) * 0.000247105381)

write_csv(wardAreas,'R_data/london1991wards_acres.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Straight country of birth counts (needs CoB loaded into threez and fivez)
#Check what we've got
View(data.frame(threez[[1]]))
View(data.frame(fivez[[1]]))

#All need reducing to London zones... though if I'm saving as one sheet each,
#do that after
years <- seq(from = 1971, to = 2011, by = 10)
for(x in c(1:5)) fivez[[x]]$year <- years[x]

#Ready for combining
allz5 <- do.call("rbind",fivez)

years <- seq(from = 1991, to = 2011, by = 10)
for(x in c(1:3)) threez[[x]]$year <- years[x]

#Ready for combining
allz3 <- do.call("rbind",threez)

#reduce to London
allz3lon <- allz3[allz3$label %in% wardz91londonSub$label,]
allz5lon <- allz5[allz5$label %in% wardz91londonSub$label,]

#add in pop total columns
allz3lon$totalpop <- apply(allz3lon[,c(2:34)],1,sum)
allz5lon$totalpop <- apply(allz5lon[,c(2:10)],1,sum)

#save
write_csv(allz3lon,'R_data/3censusCountryOfBirthCountsLondon.csv')
write_csv(allz5lon,'R_data/5censusCountryOfBirthCountsLondon.csv')

#Can I get the name of the wards too? Ah, they're not in this.
View(data.frame(wardz91londonSub))

wardz <- readShapeSpatial('C:/Data/MapPolygons/England/1991/England_wa_1991/england_wa_1991.shp')

View(data.frame(wardz))

#Keep london
wardz <- wardz[wardz$label %in% allz3lon$label,]

wardzdf <- data.frame(wardz)

wardzdf <- wardzdf[!duplicated(wardzdf$label),]

#WRITE
write_csv(wardzdf,'R_data/london1991wardNames.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Breakdown of 1991 GB wards to engl-wales-scot----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Via the original shapefiles I'm hoping.
#So a sample
gb <- threez[[1]]

#get the wards for Eng/Wales/Scot. (Oh, postcode sectors for Scot.)
eng <- readShapeSpatial('C:/Data/MapPolygons/England/1991/England_wa_1991/england_wa_1991.shp') %>% data.frame
wales <- readShapeSpatial('C:/Data/MapPolygons/Wales/1991/Wales_wa_1991_gen3/wales_wa_1991_gen3.shp') %>% data.frame
scot <- readShapeSpatial('C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount/pseudoPCS_aggregated4CorrectCount.shp') %>% data.frame

#Eng and wales will have one or two too many
#But the GB data should contain them all
eng <- eng %>% 
  dplyr::select(label) %>% 
  mutate(country = 'England')

scot <- scot %>% 
  dplyr::select(label) %>% 
  mutate(country = 'Scotland')

wales <- wales %>% 
  dplyr::select(label) %>% 
  mutate(country = 'Wales')

gb_w_countries <- do.call(rbind,list(eng,wales,scot))

#Tick
table(gb$label %in% gb_w_countries$label)

#Reduce to correct zone number.
gb_w_countries <- gb_w_countries %>% 
  filter(label %in% gb$label) %>% 
  filter(!duplicated(label))

table(gb_w_countries$country)

write_csv(gb_w_countries,'R_data/gb91wards_countryLookup.csv')


#CHECK SOME THINGS ABOUT THOSE FILES
#For paper resubmission.
econactive <- read_csv('R_data/londonWards_5census_econActive_and_unemployed.csv') %>% 
#  filter(year!=1971) %>% 
  mutate(numberemployed = econActive - unemployed)

#Load version that bernie is using
#From paper: "ln Eit i t E defined as the total economically active minus the number unemployed."

#This is for 81-11
bdata <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Migration/BerniePaper/data_used_in_estimation.csv')

#Compare a single year, order by size since we can't match on zone name
orig81 <- econactive %>% filter(year==1981) %>% select(numberemployed) %>% arrange(-numberemployed)
b81 <- bdata %>% filter(year==1981) %>% select(`y(employment)`) %>% arrange(-`y(employment)`)

both <- cbind(orig81,b81)

#Ah shizzer, need the original so I can get 1971 too.
#UK needs summing
origCoB <- read_csv('R_data/5censusCountryOfBirthCountsLondon.csv') %>% 
  mutate(UK_born = England+Scotland+Wales+`Rest of UK`) %>% 
  select(-England,-Scotland,-Wales,-`Rest of UK`)
  


#So that's all fine. Now to get the means and SDs I need.
cobsperyear <- origCoB %>% 
  select(year,`Irish Republic`:UK_born) %>% 
  gather(key = country, value = count, `Irish Republic`:UK_born)

#INCLUDES TOTALPOP
summarystats_cob <- cobsperyear %>% 
  group_by(year,country) %>% 
  summarise(mean = round(mean(count),0), sd = sd(count) %>% round(0), 
            median = round(median(count),2), iqr = round(IQR(count),2),
            min = min(count) %>% round(0),
            max = max(count) %>% round(0))

#Err.
ggplot(cobsperyear %>% filter(!country %in% c('totalpop','UK_born')), aes(x = count, colour = country)) +
  geom_density() +
  facet_wrap(~year)

#Or... yes, better.
ggplot(cobsperyear %>% filter(year==2011), aes(x = count, colour = country)) +
  geom_density() +
  facet_wrap(~country, scales = 'free')




#Repeat for employment count / unemployment count
emp_stats <- econactive %>% 
  select(year,econActive,unemployed,numberemployed) %>% 
  gather(key = type, value = count, econActive:numberemployed) %>% 
  group_by(year,type) %>% 
  summarise(mean = round(mean(count),0), sd = sd(count) %>% round(0), 
            min = min(count) %>% round(0),
            max = max(count) %>% round(0))



#Aaaand just decided it's probably better to get CoB percentages and unemployed percentages. So.
cob.props <- origCoB %>% 
  mutate_at(vars(`Irish Republic`:`Rest of world`,UK_born), .funs = list(percent = ~(./totalpop)*100 ))

cobsperyear <- cob.props %>% 
  select(year,`Irish Republic_percent`:UK_born_percent) %>% 
  gather(key = country, value = count, `Irish Republic_percent`:UK_born_percent)

summarystats_cob <- cobsperyear %>% 
  group_by(year,country) %>% 
  summarise(mean = round(mean(count),2), sd = sd(count) %>% round(2), 
            median = round(median(count),2),
            iqr = round(IQR(count),2),
            min = min(count) %>% round(2),
            max = max(count) %>% round(2))

#And prop unemployment, we already have right?
econactive$percentunemployed = 100-econactive$percentEmployed

#Summary stats for that
emp_stats <- econactive %>% 
  select(year,percentunemployed) %>% 
  group_by(year) %>% 
  summarise(mean = round(mean(percentunemployed),2), median = round(median(percentunemployed),2),
            iqr = round(IQR(percentunemployed),2),
              sd = sd(percentunemployed) %>% round(2), 
            min = min(percentunemployed) %>% round(2),
            max = max(percentunemployed) %>% round(2))







