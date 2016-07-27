#Group CoB based on the higher-res categories we can get from 1991 LBS onward (for pseudo-PCS zones)
source("Function_bulkColumnRecode.R")

library(dplyr)

#Load all 3 decades for this version
ninety1_LBS <- read.csv("1991/Scotland/1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv")
twoThousand1 <- read.csv("2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv")
#twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted.csv", as.is = T)
#use comma-less version
twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv")

#version with all columns to get the indexing actually correct
namesNoHeadinz <- read.csv("VariableCoding/1971to2011_CountryOfBirthCategories_Scotland_noRegionHeadings_allCols.csv")

#Drop ones we're not using here
namesNoHeadinz <- namesNoHeadinz[,4:6]

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
#Add in 91 SAS
threeCensusCombo <- list(
  list(ninety1_LBS,twoThousand1,twoThEleven),#all data to sum columns on
  list('1,2','1','1'),#columns to keep from those
  list('Channel Islands Isle of Man','8:9','9','7'),
  list('Rest of UK','6:9','4,6,9','3,6:7'),#Leaving out 91/01 "ireland part not specified"
  list('Irish Republic','10','7','8'),
  list('China','87','46','36'),
  list('South Africa','77','39','30'),
  list('Africa other','16:19,21:23,25,72:76,78','32:33,35:36,38,41','28,32'),
  list('Caribbean','26:33,80','61:62','53'),
  list('South America','81:82','64','51:52'),
  list('Other Middle East','84:85','45:46','34:35'),
  list('Other East Asia','88,91','48:51','38'),
  #list('Other South East Asia','86,89:90','58',''),#newp, will have to be rest of world! No S.E Asia in 01
  list('Europe other','47:48,53,55,57:63,66:69,71','10:13,18,20,22:24,28:31','15:19,22:24,26'),
  list('Rest of world','37,41:46,86,89:90,92','42,52:53,57:58,63,67:68','42,44,46:47,50,52,56:57')
  
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






















