#Having got all the column names to something readable
#and removed any sum columns
#We're now onto finding common categories across as many decades as we can
#Initially: 1991 postcode sectors are the common zone.
#1991 LBS data has vastly more detailed CoB breakdown than SAS. This is at 1991 postcode level.
source("Function_bulkColumnRecode.R")

library(dplyr)

#Load all decades to look at
seventy1 <- read.csv("1971/Scotland/1971Scotland_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv")
eighty1 <- read.csv("1981/Scotland/Scotland_1981_CoB_OutputArea_Total_noMaleFemale_countyNamesAdded_spacesRemovedFromLabels.csv")
ninety1_SAS <- read.csv("1991/Scotland/1991_Scotland_SAS_CoB_OutputAreas/1991_Scotland_SAS_CoB_OutputAreas__noMaleFemale_countyNamesAdded.csv")
ninety1_LBS <- read.csv("1991/Scotland/1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv")
twoThousand1 <- read.csv("2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv")
#twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted.csv", as.is = T)
#use comma-less version
#twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv")
#Use 2011 aggregated already to intermediate zones
twoThEleven2 <- read.csv("2011/Scotland/CoB_IntermediateGeog_viaLookupFromOutputArea/2011_CoB_intermediateGeog.csv")

#2011 data issue:
#NAs introduced. Where are the non-numerics?
# twoThEleven2 <- data.frame(apply(twoThEleven[,2:ncol(twoThEleven)],2,as.numeric))
# 
# twoThEleven2[is.na(twoThEleven2),1:2]
# #row col
# #[1,] 44680   3
# which(is.na(twoThEleven2),1:2)
# 
# #Ah ha! Commas!
# twoThEleven[44680,4]
# table(0 + grepl(",",twoThEleven[2:nrow(twoThEleven),2:ncol(twoThEleven)]))
# 
# #Remove commas, re-save...
# twoThEleven <- data.frame(apply(twoThEleven,c(2),function(x) gsub(",","",x)))
# write.csv(twoThEleven, "2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv", row.names = F)

#see xxxCoB_stitchingCategories for the code getting rid of headings (just takes headings from above file and combines)
#namesNoHeadinz <- read.csv("VariableCoding/1971to2011_CountryOfBirthCategories_Scotland_noRegionHeadings.csv")

#version with all columns to get the indexing actually correct
namesNoHeadinz <- read.csv("VariableCoding/1971to2011_CountryOfBirthCategories_Scotland_noRegionHeadings_allCols.csv")
 

#drop 91 SAS for now, not using at present
namesNoHeadinz <- namesNoHeadinz[,-3]

#~~~~~~~~~~~~~~~~~~~~~~~~~
#FIVE CENSUS STITCHING----

#Passing everything in to function to make sure names across everything remains consistent
#The second list: indexes columns to keep from the dataframes to re-attach after re-coding
#All others: common variable name and, for each dataframe, columns to sum
#(or just a single column)
#Add in 91 SAS
fiveCensusCombo <- list(list(seventy1,eighty1,ninety1_SAS,ninety1_LBS,twoThousand1,twoThEleven2),#all data to sum columns on
  list('1','2','1','1,2','1','1'),#columns to keep from those
  list('England','3','3','2','3','2','2'),#Common name of column, index of columns to combine in order
  list('Scotland','4','4','3','4','3','4'),#doing in strings cos typing all this in lists here would be reet messy
  list('Wales','5','5','4','5','5','5'),
  list('Rest of UK','6','6','5','6:9','4,6,9','3,6:7'),#Leaving out 91/01 "ireland part not specified"
  list('Irish Republic','7','7','6','10','7','8'),
  list('Old Commonwealth','8','8','7','12:14','59,65:66','48,54:55'),#Canada, Oz, New Zealand
  #list('Africa','9','9:10','15:25,72:78','32:41','27:32'),#No: this doesn't split new commonwealth out
  list('Africa (New-C)','9','9:10','8:9','15:25','34:35,37:38,40','27,29,31'),
  list('India','12','12','12','35','55','40'),
  list('Pakistan','13','17','13','36','56','41'),
  list('Other Europe','16','18:19','17:18','47:69,71','10:26,28:31','9:23,26'),
  list('SE Asia New C','14','14','14','38:40','47,49:50','37,43,45'),#Quote marks because racist! But very specific Colonial categories
  list('Caribbean New C','10','11','10','26:33','61:62','53'),
  list('New Commonwealth other','11,15','13,15:16','11,15:16','34,37,41:46','42,57,54','39'),
  list('Rest of World','17','20','19:20','70,72:92',
       '27,32:33,36,39,41,43:46,48,51:53,58,60,63:64,67:68','28,30,32')
)


# fiveCensusCombo <- list(list(seventy1,eighty1,ninety1_LBS,twoThousand1,twoThEleven),#all data to sum columns on
#                         list('1','2','1,2','1','1'),#columns to keep from those
#                         list('England','3','3','3','2','2'),#Common name of column, index of columns to combine in order
#                         list('Scotland','4','4','4','3','4'),#doing in strings cos typing all this in lists here would be reet messy
#                         list('Wales','5','5','5','5','5'),
#                         list('Rest of UK','6','6','6:9','4,6,9','3,6:7'),#Leaving out 91/01 "ireland part not specified"
#                         list('Irish Republic','7','7','10','7','8'),
#                         list('Old Commonwealth','8','8','12:14','59,65:66','48,54:55'),#Canada, Oz, New Zealand
#                         #list('Africa','9','9:10','15:25,72:78','32:41','27:32'),#No: this doesn't split new commonwealth out
#                         list('Africa (New-C)','9','9:10','15:25','34:35,37:38,40','27,29,31'),
#                         list('India','12','12','35','55','40'),
#                         list('Pakistan','13','17','36','56','41'),
#                         list('Other Europe','16','18:19','47:69,71','10:26,28:31','9:23,26'),
#                         list('\"Far East\" (New-C)','14','14','38:40','47,49:50','37,43,45'),#Quote marks because racist! But very specific Colonial categories
#                         list('\"America\ (New-C)"','10','11','26:33','61:62','53'),
#                         list('New Commonwealth other','11,15','13,15:16','34,37,41:46','42,57,54','39'),
#                         list('Rest of World','17','20','70,72:92',
#                              '27,32:33,36,39,41,43:46,48,51:53,58,60,63:64,67:68','28,30,32')
# )

#Returns list of re-coded dataframes
#"Error: column_recode_lists_are_all_correct_length not equal to TRUE"
#Means lists defining recodes aren't all of a length that recodes in each dataframe
results <- recode(fiveCensusCombo)

looksee <- results[[3]]

#Save as five dataframes, ready for geog re-assigning (apart from '91 which is correct already)
savens <- c('71','81','91SAS','91LBS','01','11_IntermediateGeog')

lapply(1:length(savens),function(x) write.csv(results[[x]], 
                                 paste0("VariableCoding/CountryOfBirth_fiveCensusRecodes/",savens[x],".csv"),
                                 row.names = F))







