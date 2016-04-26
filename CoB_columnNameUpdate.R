#Re-coding CoB column names with their sensible word names
#Without a lookup and with a little logic
library(dplyr)
library(tidyr)

#Country Names for each year, differing lengths of column
#Check.names: R doesn't like column names starting with numbers
#It'll require quote marks but better than e.g. "X1981"
names <- read.csv("VariableCoding/1971to2011_CountryOfBirthCategories_Scotland.csv", check.names = F, as.is = T)

#Updates:
#1991 LBS: Caribbean under Americas needs distinguishing from Caribbean/New Commonwealth
names[35,4] <- "Caribbean [heading]"

#Apply those labels to column names, checking they match as we go along

#~~~~~~~~
#1971----
seventyOne <- read.csv("1971/Scotland/1971Scotland_CoB_EnumDistrict.csv")


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
write.csv(all71,"1971/Scotland/1971Scotland_CoB_EnumDistrict_countryNamesAdded_n_MaleFemaleSummed.csv", row.names = F)

#~~~~~~~~
#1981----
#This is just total pop: no need to sum male/female
eightyOne <- read.csv("1981/Scotland/Scotland_1981_CoB_OutputArea_Total_noMaleFemale.csv",check.names = F)

#First count column is the total: drop it.
eightyOne2 <- eightyOne %>% select(1,3:21)

names(eightyOne2) <- c(names(eightyOne2)[1],names[1:19,2])

#remove 'new commonwealth' - it's a sum column, we don't want those
eightyOne2 <- eightyOne2[,-8]

write.csv(eightyOne2,"1981/Scotland/Scotland_1981_CoB_OutputArea_Total_noMaleFemale_countyNamesAdded.csv", row.names = F)

#~~~~~~~~
#1991 SAS output areas----
ninetyOneSAS <- read.csv(
  "1991/Scotland/1991_Scotland_SAS_CoB_OutputAreas/1991_Scotland_SAS_CoB_OutputAreas.csv",
  check.names = F)

#SAS table: male and female in odd/even column names
#Sum male,female
#Loses the column name, annoyingly.
all91sas <- data.frame(ninetyOneSAS[,c('Zone ID')])

#Take pairs of male/female columns, sum them into new
for(n in seq(from = 2, to = 45, by = 2)){
  
  all91sas[,as.character(n)] <- ninetyOneSAS[,n] + ninetyOneSAS[,(n+1)]
  
}

#Add names to columns
names(all91sas) <- c('Zone ID','ALL','UK',names[1:20,3])

#'UK' column is not quite sum of the nations
#But while it "Includes Channel Islands, Isle of Man and United Kingdom (part not stated)"
#The differences are not consistent. So drop, same as other sum columns

#remove sum columns we don't want before saving: 
#All, new commonwealth
all91sas <- all91sas[,!(names(all91sas) %in% c('ALL','UK','New Commonwealth'))]

write.csv(
  all91sas,
  "1991/Scotland/1991_Scotland_SAS_CoB_OutputAreas/1991_Scotland_SAS_CoB_OutputAreas__noMaleFemale_countyNamesAdded.csv",
  row.names = F)


#~~~~~~~~
#1991 LBS postcode sectors----
ninetyOneLBS <- read.csv(
  "1991/Scotland/1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF.csv",
  check.names = F)

#Able to get "total persons" column direct from CASWEB
#so no need to do any male/female summing
#Just need a rename.
#The name list should fit, it includes all sum columns (that we'll remove shortly...)
names(ninetyOneLBS) <- c('Zone ID','Zone name',names[1:113,4])

#View only shows 100 columns - this did, in fact, work
names(ninetyOneLBS)

#remove sum columns
#cols to remove:
#There are two Africas. Will this successfully remove both or throw an error?
#Oh, and two Asias too.
rmv <- c('ALL COUNTRIES OF BIRTH','Europe', 'European Community',
         'United Kingdom','Outside United Kingdom',
         'Old Commonwealth','New Commonwealth',
         'Africa','Eastern Africa','Southern Africa',
         'Western Africa','Caribbean [heading]','Asia',
         'South Asia','South East Asia',
         'Remainder of New Commonwealth',
         'European Community','Remainder of Europe',
         'Africa','America','Asia','Middle East',
         'Remainder of Asia')

#twenty three sum columns to remove. So we want ninetyOneLBS dropping to 92 cols
ninetyOneLBS2 <- ninetyOneLBS[,!(names(ninetyOneLBS) %in% rmv)]

#Not quite. What went wrong there?
names[!(names[,4] %in% names(ninetyOneLBS2)),4]

#Ah: it removed Caribbean that's under 'America'. 
#(vs Caribbean that's 'New Commonwealth')
#I'll do a renaming in the original list...
#OK, fixed via name update in original name list ("Caribbean [heading]")

write.csv(
  ninetyOneLBS2,
  "1991/Scotland/1991_Scotland_LBS_CoB_postcodeSectors/1991_Scotland_LBS_CoBPostcodeSectors_allPeople_notMF_countyNamesAdded.csv",
  row.names = F)

#~~~~~~~~
#2001----

twoFaazandOne <- read.csv(
  "2001/Scotland/2001_Scots_CoB_univariatetable.csv",
  check.names = F)

#Remove first data column, that's ALL PEOPLE. Not on name list.
twoFaazandOne <- twoFaazandOne[,-2]

#Again, already M/F combined. So just renaming and removing sum columns.
#82 names (having removed ALL PEOPLE) before sum columns removal?
names(twoFaazandOne) <- c('Zone Code',names[1:82,5])

#Remove sum columns again: new names to remove

#Note: republic of ireland has 'ireland part not specified' as nested
#But it's not a sum, two separate vals.
rmv <- c('Europe','United Kingdom',
         'Other Western Europe','EU Countries',
         'Non EU Countries in Western Europe',
         'Eastern Europe','Africa',
         'Central and Western Africa',
         'South and Eastern Africa',
         'Asia','Middle East','Far East',
         'South Asia','North America',
         'Oceania')
         
#Should drop down to 68 cols. Yup!
twoFaazandOne2 <- twoFaazandOne[,!(names(twoFaazandOne) %in% rmv)]

#Save
write.csv(
  twoFaazandOne2,
  "2001/Scotland/2001_Scots_CoB_univariatetable_countryNamesAdded.csv",
  row.names = F)


#~~~~~~~~
#2011----

#Table QS203SC from bulk download
twoFaazandEleven <- read.csv(
  "2011/Scotland/OutputAreaStd_ALL/QS203SC_formatted.csv",
  check.names = F, as.is = T)

#Need to do a little updating of blank cells here too. 
#Oh but look! The names are already there! In fact, I copied them from the CSV, didn't I...?
table(0 + (is.na(twoFaazandEleven)), useNA = 'always')
table(0 + (twoFaazandEleven == "-"), useNA = 'always')

twoFaazandEleven[twoFaazandEleven == "-"] <- 0

#Remove sum columns
#Made a little easier by the column names themselves this time.
rmv <- c('All people','Europe','Europe: United Kingdom',
         'Europe: Other Europe',"Europe: Other Europe: EU Countries",
         "Europe: Other Europe: EU Countries: Member countries in March 2001",
         "Europe: Other Europe: Accession countries April 2001 to March 2011",
         "Europe: Other Europe: Non EU countries",
         "Africa",
         "Africa: North Africa",
         "Africa: Central and Western Africa",
         "Africa: South and Eastern Africa",
         "Middle East and Asia",
         "Middle East and Asia: Middle East",
         "Middle East and Asia: Eastern Asia",
         "Middle East and Asia: Southern Asia",
         "Middle East and Asia: South-East Asia",
         "The Americas and the Caribbean",
         "The Americas and the Caribbean: North America",
         "Antarctica and Oceania"
         )

#20 names to remove, should end up with 57 cols
twoFaazandEleven2 <- twoFaazandEleven[,!(names(twoFaazandEleven) %in% rmv)]

#Currently character but can save as CSV no bovva
write.csv(
  twoFaazandEleven2,
  "2011/Scotland/OutputArea_bulk_CoB_formatted/QS203SC_formatted.csv",
  row.names = F)




















