#Group CoB based on the higher-res categories we can get from 1991 LBS onward (for pseudo-PCS zones)
source("Function_bulkColumnRecode.R")

library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~SCOTLAND~~~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load all 3 decades for this version
ninety1_LBS <- read.csv("1991/Scotland/1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv")
twoThousand1 <- read.csv("2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv")
#twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted.csv", as.is = T)
#use comma-less version
twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv")

#version with all columns to get the indexing actually correct
namesNoHeadinz <- read.csv("VariableCoding/APRIL17UPDATE_1971to2011_CountryOfBirthCategories_Scotland_noRegionHeadings_allCols.csv")

#Drop ones we're not using here
namesNoHeadinz <- namesNoHeadinz[,5:7]

#~~~~~~~~~~~~~~~~~~~~
#Automate some list identification

#Base on names in 91 LBS. Matches from others? What row?
#which(grepl(namesNoHeadinz[5,1],namesNoHeadinz[,3]))
#thing <- which(grepl("slartibartfast",namesNoHeadinz[,3]))

foundMatches <- list()
#List grows one at a time...
j = 1

#cycle through all names in 91LBS
for (i in 3:nrow(namesNoHeadinz)) {
  # for (i in 5:6) {
  
  #Ignore these matches
  #"other" will vary across years and need checking
  #ignorz <- c("Channel.Islands","Isle.of.Man","other")
  #Actually, just going to remove them manually
  
  two001 <- which(grepl(namesNoHeadinz[i,1],namesNoHeadinz[,2], ignore.case = T))
  two011 <- which(grepl(namesNoHeadinz[i,1],namesNoHeadinz[,3], ignore.case = T))
  
  #print(length(two011))
  
  #need matches for both
  if(length(two011) != 0 & length(two001) != 0) {
    
    foundMatches[[j]] <- list(as.character(namesNoHeadinz[i,1]),i,two001,two011)
    
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

#That all worked apart from channel islands/isle of man
#Which picked up a match each time it appears
#'Other' categories (middle east and europe) will need working out too - those will vary 
#And need to be as common as we can make them
#Also need to remove Caribbean to check its breakdown (it'll pick up e.g. USA otherwise)
#Oh and South America. Which could include Central America
foundMatches <- foundMatches[c(1:4,7:26,28,29,32)]


#~~~~~~~~~~~~~~~~~~~~~~~~~
#3CENSUS LBS STITCHING----

#Passing everything in to function to make sure names across everything remains consistent
#The second list: indexes columns to keep from the dataframes to re-attach after re-coding
#All others: common variable name and, for each dataframe, columns to sum
#(or just a single column)
#Update: having missed 'other europe not in EU' 2001
#Add back in, adjust those indices after it (it's at 24)
threeCensusCombo <- list(
  list(ninety1_LBS,twoThousand1,twoThEleven),#all data to sum columns on
  list('1,2','1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:9','9','7'),
  list('UK part not specified','7','6','6'),#Oops, Isle of Man is in here too. And I double counted N Ire
  #list('Rest of UK','6:9','4,6,9','3,6:7'),#Leaving out 91/01 "ireland part not specified"
  list('Irish Republic','10','7','8'),
  list('China','87','47','36'),
  list('South Africa','77','40','30'),
  list('Africa other','16:19,21:23,25,72:76,78','33:34,36:37,39,42','28,32'),
  list('Caribbean','26:33,80','62:63','53'),
  list('South America','81:82','65','51:52'),
  list('Other Middle East','84:85','45:46','34:35'),
  list('Other East Asia','88,91','49:52','38'),
  #list('Other South East Asia','86,89:90','58',''),#newp, will have to be rest of world! No S.E Asia in 01
  list('Europe other','47:48,53,55,57:63,66:69,71','10:13,18,20,22:25,29:32','15:19,22:24,26'),
  list('Rest of world','37,41:46,86,89:90,92','43,53:54,58:59,64,68:69','42,44,46:47,50,52,56:57')
  
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

looksee <- results[[1]]

#Save as five dataframes, ready for geog re-assigning (apart from '91 which is correct already)
savens <- c('91LBS_PCS','01_OA','11_OA')

lapply(1:length(savens),function(x) write.csv(results[[x]], 
                                 paste0("VariableCoding/CountryOfBirth_threeCensusRecodes_to91LBS/",savens[x],".csv"),
                                 row.names = F))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ENGLAND/WALES~~~~----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load CoBs
#(Watch out for dplyr readr doing anything that might affect the stitch function)
cob91 <- read_csv('1991/EnglandWales/EnglandWales_LBS_ward_CoB_totalPersons_countryNamesAdded.csv')
cob01 <- read_csv('2001/EnglandWales/2001_EnglandWales_CoB_OA_totalPersons_countryNamesAdded.csv')
cob11 <- read_csv('2011/EnglandWales/EngWales_CountryOfBirth_OA_2011_NOMIS_countryNamesAdded.csv')

#As above, we can check for obvious common names. Need to remind myself how that worked...



foundMatches <- list()
#List grows one at a time...
j = 1

#cycle through all CoB names in cob11 (smallest list of names)
for (i in 2:length(names(cob11))) {
  # for (i in 5:6) {
  
  #Ignore these matches
  #"other" will vary across years and need checking
  #ignorz <- c("Channel.Islands","Isle.of.Man","other")
  #Actually, just going to remove them manually
  
  # ninety1in11 <- which(grepl(names(cob11)[i],names(cob91), ignore.case = T, fixed = T))
  # ohOnein11 <- which(grepl(names(cob11)[i],names(cob01), ignore.case = T, fixed = T))
  
  #Try with exact match. Yes, better I think.
  ninety1in11 <- which(names(cob11)[i]==names(cob91) %>% gsub("\\."," ",.))
  ohOnein11 <- which(names(cob11)[i]==names(cob01) %>% gsub("\\."," ",.))
  
  #print(length(two011))
  
  #need matches for both
  if(length(ninety1in11) != 0 & length(ohOnein11) != 0) {
    
    #To match stitch function. In this case, i is the index for 2011 CoB column
    foundMatches[[j]] <- list(as.character(names(cob11)[i]),ninety1in11,ohOnein11,i)
    
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


#So that left out the Irelands and the 'other's
#As grepl got multiple matches. So need to look at those.
#Check after stitch

#Examine those that did not find a match
#To figure out stitch number
#To match, remove periods again
allThree <- data.frame(ninetyOne = names(cob91) %>% gsub('\\.',' ',.),
                       ohOne = c(names(cob01) %>% gsub('\\.',' ',.),rep(NA,23)),
                       eleven = c(names(cob11), rep(NA,34)))

#Remove existing matches
foundMatchNames <- foundMatches %>% lapply(head, n = 1) %>% unlist

test <- apply(allThree,2,function(x) gsub(paste(foundMatchNames,collapse = '|'),NA,x)) %>% data.frame
#test <- apply(allThree,2,function(x) gsub(NA," ",x)) %>% data.frame

#save/excel/print
write_csv(test,'R_data/bits/engwalesCoBchecks.csv')

#Eng/wales stitch----

#Staring at that check file now in Excel
threeCensusCombo <- list(
  list(cob91,cob01,cob11),#all data to sum columns on
  list('1,2','1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:9','9','8:11'),
  list('UK part not specified','7','6','6:7'),
  list('Irish Republic','10','7','12'),
  list('China','87','47','37'),
  list('South Africa','77','40','31'),
  list('Africa other','16:19,21:23,25,72:76,78','33:34,36:37,39,42','25:26,28,33:34'),
  #Should Jamaica be in here? It seems to have its own category now but it might not in the Scots data...
  #Yeah,no: Scots 11 doesn't have Jamaica, only Caribbean. So need to remove it from the found list above to match across GB.
  #Just started this...
  list('Caribbean','26:33,80','61:62','53'),
  list('South America','81:82','64','51:52'),
  list('Other Middle East','84:85','45:46','34:35'),
  list('Other East Asia','88,91','48:51','38'),
  #list('Other South East Asia','86,89:90','58',''),#newp, will have to be rest of world! No S.E Asia in 01
  list('Europe other','47:48,53,55,57:63,66:69,71','10:13,18,20,22:24,28:31','15:19,22:24,26'),
  list('Rest of world','37,41:46,86,89:90,92','42,52:53,57:58,63,67:68','42,44,46:47,50,52,56:57')
  
)













