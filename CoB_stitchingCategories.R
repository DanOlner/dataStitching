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
#ninety1_SAS <- read.csv("1991/Scotland/1991_Scotland_SAS_CoB_OutputAreas/1991_Scotland_SAS_CoB_OutputAreas__noMaleFemale_countyNamesAdded.csv")
ninety1_LBS <- read.csv("1991/Scotland/1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv")
twoThousand1 <- read.csv("2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv")
#twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted.csv", as.is = T)
#use comma-less version
twoThEleven <- read.csv("2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted_commasRemoved.csv")

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

fiveCensusCombo <- list(list(seventy1,eighty1,ninety1_LBS,twoThousand1,twoThEleven),#all data to sum columns on
                        list('1','2','1,2','1','1'),#columns to keep from those
                        list('England','3','3','3','2','2'),#Common name of column, index of columns to combine in order
                        list('Scotland','4','4','4','3','4'),#doing in strings cos typing all this in lists here would be reet messy
                        list('Wales','5','5','5','5','5'),
                        list('Rest of UK','6','6','6,7,8,9','4,6,9','3,6,7')#Leaving out 91/01 "ireland part not specified"
                        )

#Returns list of re-coded dataframes
#"Error: column_recode_lists_are_all_correct_length not equal to TRUE"
#Means lists defining recodes aren't all of a length that recodes in each dataframe
results <- recode(fiveCensusCombo)

looksee <- results[[4]]








