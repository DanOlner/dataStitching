library(tidyverse)
library(sf)
library(tmap)
#library(rgdal)
source("Function_DataFromSmalltoLargeGeog.R")

#Adapting from same process for 'econ active':
#https://github.com/SheffieldMethodsInstitute/HarmonisedCountryOfBirthDatasets/blob/master/code/econActive_CensusVariableProcessing.R

#Table CaSWEB codes in this doc:
#https://docs.google.com/document/d/1IQ5nfUwxsehvx257s-iAmo74ez65EJAozmfpNs-MkIQ/edit?usp=sharing

#~~~~~~~~~~
#TENURE----
#~~~~~~~~~~

#4 common cats we're after: owner-occ, social rented, private rented, other

#~~~~~~~~~~~~~~~
#Tenure 1971----
#~~~~~~~~~~~~~~~

tenure1971 <- read_csv('1971/GreatBritain/EWS_Tenure_1971.csv')
#Keep just table row code numbers
names(tenure1971) <- gsub(x = names(tenure1971),pattern = 'c71s19_',replacement = '')

#Check that household vs people counts are doing what we think they're doing
#E.g. owner-occ households = 247; persons = 248
#So should see appropriate difference in count size
#Just look at the table: 248 (household count) is appropriately larger than household count, we're good I think.

#What columns are we after then?
#248: owner-occ persons
#250: Rented from council/new town/SSHA persons
#Sum these two for private rented
#252: private unfurnished persons
#254: private furnished persons

#For 'other' cat
#246, 256, 258, 260

tenure1971 <- tenure1971 %>%
  mutate(
    ownerocc = `248`,
    socialrent = `250`,
    privaterent = `252` + `254`,
    other = `246`+`256`+`258`+`260`
  ) %>% 
  dplyr::select(`Zone Code`,`OPCS Code`,ownerocc,socialrent,privaterent,other)

  
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

#Check match for 71 tenure zones against intersect
table(tenure1971$`Zone Code` %in% its71$zone_code)

#634 false. 0.5% fail rate there.
#Most have zero counts. Not all though.
falses <- tenure1971[!tenure1971$`Zone Code` %in% its71$zone_code,]

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(3:6)#from dta

result <- moveData(its71,tenure1971,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#Looks like it worked...
View(head(result@data,50))

df <- data.frame(result)

# write_csv(df,'data/census_sources/econActiveSources/output/1971econActive.csv')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971tenure.csv')



#~~~~~~~~~~~~~~~
#TENURE 1981----
#~~~~~~~~~~~~~~~

tenure1981 <- read_csv('1981/GreatBritain/1981_GB_tenure.csv')
#Keep just table row code numbers - just last three digits are unique, though they're rolling over to 1000 mid-table
names(tenure1981) <- gsub(x = names(tenure1981),pattern = '\\[81sas100|\\[81sas101|\\]',replacement = '')

#Cols we want and their combinations
#975: owner occ persons
#social rent: 991,007
#private rent: 023,039,055,071
#other: 135 (this is just non-permanent. Diff 'other' to 1971, right?)
tenure1981 <- tenure1981 %>%
  mutate(
    ownerocc = `975`,
    socialrent = `991`+`007`,
    privaterent = `023` + `039`+`055`+`071`,
    other = `135`
  ) %>% 
  dplyr::select(`Zone ID`,ownerocc,socialrent,privaterent,other)



#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

#Check match. 0.36% false. Ah, the GB file just doesn't have them. Wonder why not?
table(tenure1981$`Zone ID` %in% its81$label)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:5)#from dta

result <- moveData(its81,tenure1981,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

View(head(result@data,50))

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1981tenure.csv')





#~~~~~~~~~~~~~~~
#TENURE 1991----
#~~~~~~~~~~~~~~~

#LBS tables to be directly aggregated into tweaked 1991 geography (to deal with zero cells).
#Eng/Wales slightly different categories to Scotland, needs harmonising. Table LBS20 in both cases

tenure91_engwales <- read_csv('1991/EnglandWales/1991_LBS_EngWales_tenure.csv')
names(tenure91_engwales) <- gsub(x = names(tenure91_engwales),pattern = 'l200',replacement = '')

tenure91_scot <- read_csv('1991/Scotland/1991_LBS_Scotland_tenure.csv')
names(tenure91_scot) <- gsub(x = names(tenure91_scot),pattern = 'l200',replacement = '')

#Engwales combos:
#412,413 = ownerocc
#417,418 = social rent
#414,415,416 = private rent
#419 = other (non-permanent accomm)

#Do both separately then join
#Aiming for same 4 categories
tenure91_engwales <- tenure91_engwales %>%
  mutate(
    ownerocc = `412`+`413`,
    socialrent = `417`+`418`,
    privaterent = `414` + `415`+`416`,
    other = `419`
  ) %>% 
  dplyr::select(`Zone ID`,ownerocc,socialrent,privaterent,other)


#Scot combos
#494,495 = owner
#499,500,501,502 = social rent
#496,497,498 = private rent
#503 = other (nonpermanent)
tenure91_scot <- tenure91_scot %>%
  mutate(
    ownerocc = `494`+`495`,
    socialrent = `499`+`500`+`501`+`502`,
    privaterent = `496` + `497`+`498`,
    other = `503`
  ) %>% 
  dplyr::select(`Zone ID`,ownerocc,socialrent,privaterent,other)


#Link into one
tenure91 <- rbind(tenure91_engwales,tenure91_scot)



#ZONE AGGREGATION DIRECTLY (cos 91 is the decade we use the geog for)
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
tenure91$aggID <- tenure91$`Zone ID`

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  tenure91$aggID[tenure91$`Zone ID` %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- tenure91[,2:6] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
#zeroes <- agg2[apply(agg2[,2:3],1,sum)==0,]
#zeroes <- merge(zeroes[,1],ea91final,by.x = 'label',by.y = 'zone')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
#table(as.character(zeroes$label))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

geogzdf <- data.frame(geogz)

write_csv(geogzdf,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991tenure.csv')



#~~~~~~~~~~~~~~~
#TENURE 2001----
#~~~~~~~~~~~~~~~

tenure01_eng <- read_csv('2001/England/england2001_tenure_people.csv')
names(tenure01_eng) <- gsub(x = names(tenure01_eng),pattern = 'UV0430',replacement = '')

tenure01_wales <- read_csv('2001/Wales/wales2001_tenure_people.csv')
names(tenure01_wales) <- gsub(x = names(tenure01_wales),pattern = 'UV0430',replacement = '')


#Note: mistake in this table - structured to suggest “shared ownership” means the social rented cats. 
#It doesn’t it’s its own separate category.
tenure01_scot <- read_csv('2001/Scotland/scotland2001_tenure_people_difflayouttoengwales.csv')
names(tenure01_scot) <- gsub(x = names(tenure01_scot),pattern = 'UV0440',replacement = '')

#Trying newer tenure CSV source that apparently has corrections in.
#Via https://www2.gov.scot/Topics/Statistics/18531/13575
tenure01_scot_corrected <- read_csv('2001/Scotland/scotland_2001_tenure_updatedSource_afterError.csv')



#England and wales have same columns, can combine
tenure01_engwales <- rbind(tenure01_eng,tenure01_wales)

#Combos
#Owner: 002
#social r: 006
#private r: 009
#other: 014 (living rent-free - diff category to other others)
tenure01_engwales <- tenure01_engwales %>% 
  dplyr::select(`Zone ID`=`Zone Code`,ownerocc=`002`,socialrent=`006`,privaterent=`009`,other=`014`)


#Scotland combos:
#own = 002 + 005 (shared)
#social = 006+007+008
#private = 009+014
#other = 019 (living rent free)
tenure01_scot <- tenure01_scot %>% 
  mutate(
    ownerocc = `002`,
    socialrent = `006`+`007`+`008`,
    privaterent = `009`+`014`,
    other=`019`
  ) %>% 
  dplyr::select(`Zone ID`=`Zone Code`,ownerocc,socialrent,privaterent,other)

#Checking scots totals match correctly. This should match 001
tenure01_scotcheck <- tenure01_scot %>% 
  mutate(tot = ownerocc+socialrent+privaterent+other) %>% 
  dplyr::select(`001`,tot)
    
#Totals sometimes higher. How possible?

#Let's look at new source. Compile to four cats
tenure01_scot_corrected <- tenure01_scot_corrected %>% 
  mutate(
    ownerocc = `Owns outright` + `Owns with a mortgage or a loan` + `Shared ownership`,
    socialrent = `Rented from Council (Local Authority/Scottish Homes)` + `Other social rented`,
    privaterent = `Private landlord or letting agency`+`Employer of a household member`+`Relative or friend of a household member`,
    other=Other
  ) %>% 
  dplyr::select(`Zone ID`=`Output Area`,ownerocc,socialrent,privaterent,other)



# tenure01 <- rbind(tenure01_engwales,tenure01_scot)
#Using new Scots source
tenure01 <- rbind(tenure01_engwales,tenure01_scot_corrected)


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

#Check match to data. 8 false.
table(tenure01$`Zone ID` %in% its01$zone_code)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:5)#from dta

result <- moveData(its01,tenure01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2001tenure.csv')




#~~~~~~~~~~~~~~~
#Tenure 2011----
#~~~~~~~~~~~~~~~

#Had to split into two - combined and check
#Having removed stoopid header and footer manually!
tenure2011a <- read_csv('2011/GreatBritain/2011_tenure/2011_QS403UK_tenure_people_owned_sharedOnly.csv')
tenure2011b <- read_csv('2011/GreatBritain/2011_tenure/2011_QS403UK_tenure_people_socialrent_privaterent_rentfree.csv')

#What countries do we have? All three, bonza.
#table(grepl(pattern='W',x = tenure2011a$`2011 output area`))
#table(grepl(pattern='S',x = tenure2011a$`2011 output area`))
#table(grepl(pattern='E',x = tenure2011a$`2011 output area`))

#Check zone codes are in same order... tick.
table(tenure2011a$`2011 output area`==tenure2011b$`2011 output area`)
#Check the two zone code fields are the same thing... tick. Only need one
table(tenure2011a$`2011 output area`==tenure2011b$mnemonic)


#Combine into one set of cols
tenure2011 <- cbind(tenure2011a %>% dplyr::select(-`All categories: Tenure`),tenure2011b %>% dplyr::select(-`2011 output area`,-mnemonic,-`All categories: Tenure`))

#Recodinz I will just do directly...
#Note, am including shared ownership (part own/rent) in owned
tenure2011 <- tenure2011 %>% 
  mutate(
    ownerocc = owned+shared,socialrent = `Social rented: Total`,privaterent = `Private rented: Total`, other = `Living rent free`
  ) %>% 
  dplyr::select(-c(`2011 output area`,`Social rented: Total`,`Private rented: Total`,owned,shared,`Living rent free`))



#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

#Check match to data. 2 false.
table(tenure2011$mnemonic %in% its11$code)

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:5)#from dta

result <- moveData(its11,tenure2011,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2011tenure.csv')


#~~~~~~~~~~~~~~~~~~~~~~
#CHECK TENURE VARS-----
#~~~~~~~~~~~~~~~~~~~~~~

#Load and visualise
#List of cities taken from prep_landregistry_data.R
#Can add in Scots cities
# #Cities I'd like to keep
citiesToKeep <- c(
"London",
"Manchester",
"Birmingham",
"Bristol",
"Newcastle & Durham",
"Leeds",
"Leicester",
"Liverpool",
"Nottingham",
"Southampton",
"Sheffield & Rotherham",
"Reading & Bracknell",
"Brighton",
"Oxford",
"Glasgow",
"Edinburgh"
)

#Smaller list, chart too busy
citiesToKeep <- c(
  "London",
  "Manchester",
  "Birmingham",
  # "Bristol",
  "Leeds",
  "Sheffield & Rotherham",
  "Glasgow",
  "Edinburgh",
  "Cardiff"
)

#Load geography, has TTWA in it
GBgeog <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')

names(GBgeog)

#Load 71-11 Tenure
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'tenure',full.names = T)

years <- c(1971,1981,1991,2001,2011)
tenure <- lapply(1:5,function(x) read_csv(filez[x]) %>% mutate(year = years[x]))
tenure <- bind_rows(tenure)

#Check TTWA match. Tick!
table(citiesToKeep %in% GBgeog$ttwa)

#Link... don't need geographical info
#Check match. Tick.
table(GBgeog$zone %in% tenure$label)

tenure <- tenure %>% left_join(GBgeog %>% st_set_geometry(NULL) %>% dplyr::select(zone,ttwa), by = c('label'='zone'))

#Nooow. Need,say, owner-occ as proportion of total, right?
#And want to see social/privat rent as prop of total too plz

#We should select our cities first
#Then need to sum pops before finding proportions.
tenure_sums <- tenure %>%
  group_by(year,ttwa) %>% 
  summarise(ownerocc = sum(ownerocc),socialrent = sum(socialrent),privaterent=sum(privaterent),other=sum(other))
  
tenure_sums <- tenure_sums %>% 
  mutate(
    totalpop = ownerocc + socialrent + privaterent + other,
    ownerocc_prop = (ownerocc/totalpop)*100,
    socialrent_prop = (socialrent/totalpop)*100,
    privaterent_prop = (privaterent/totalpop)*100,
    other_prop = (other/totalpop)*100
    )

#Version without other
tenure_sums <- tenure_sums %>% 
  mutate(
    totalpop = ownerocc + socialrent + privaterent,
    ownerocc_prop = (ownerocc/totalpop)*100,
    socialrent_prop = (socialrent/totalpop)*100,
    privaterent_prop = (privaterent/totalpop)*100
    )


#Argument here for excluding other from the sums, just do relative props of owner + two types of rent
#Other prop changes too much, different categories.

#Plot that mofo. Filter down to the cities we picked
# ggplot(tenure_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = other_prop, colour = fct_reorder(ttwa,-other_prop))) +
# ggplot(tenure_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = privaterent_prop, colour = fct_reorder(ttwa,-privaterent_prop))) +
ggplot(tenure_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = socialrent_prop, colour = fct_reorder(ttwa,-socialrent_prop))) +
# ggplot(tenure_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = ownerocc_prop, colour = fct_reorder(ttwa,-ownerocc_prop))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette='Dark2')



#~~~~~~~~~~~~~~~----

#~~~~~~~~~~~~~~~~
#SOCIAL CLASS----
#~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~
#SOCIAL CLASS 1971----
#~~~~~~~~~~~~~~~~~~~~~

#10% sample for this one. So a huge sample, error bars would be small.
sec71 <- read_csv('1971/GreatBritain/1971_EWS_SEC_10percentsample.csv')
names(sec71) <- gsub(x = names(sec71),pattern = 'c71s23_',replacement = '')


#Has actual counts. Which are presumably 10% of actual pop.
#Using google doc to say which cats go where. 
#Code here.
#Using only economically active or retired. Will see if that matches elsewhere
sec71 <- sec71 %>% 
  mutate(
    # professional = `188`+`189`,
    # managerial = `186`+`187`,
    prof_managerial = `188`+`189`+`186`+`187`,
    skilled_nonmanual = `190`+`197`,
    skilled_manual = `193`+`194`+`195`+`198`+`199`+`201`,
    unskilled = `191`+`192`+`196`+`200`,
    other=`202`#occupation inadequately described
  ) %>% 
  dplyr::select(`Zone ID`=`Zone Code`,prof_managerial,skilled_nonmanual,skilled_manual,unskilled,other)


#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

#Check match for 71 tenure zones against intersect
# table(tenure1971$`Zone Code` %in% its71$zone_code)

#634 false. 0.5% fail rate there.
#Most have zero counts. Not all though.
falses <- tenure1971[!tenure1971$`Zone Code` %in% its71$zone_code,]

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:6)#from dta

result <- moveData(its71,sec71,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#Looks like it worked...
View(head(result@data,50))

df <- data.frame(result)

# write_csv(df,'data/census_sources/econActiveSources/output/1971econActive.csv')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971socialclass.csv')


#~~~~~~~~~~~~~~~~~~~~~
#SOCIAL CLASS 1981----
#~~~~~~~~~~~~~~~~~~~~~

#Again, 10% sample
# sec81 <- read_csv('1981/GreatBritain/1981_EWS_SEC_10percentsample.csv')
# names(sec81) <- gsub(x = names(sec81),pattern = '\\[81sas444|\\]',replacement = '')


#81 is only economically active, we don't have values for retired.
#Ah, actually, this table is econ active. We have another one.
# sec81 <- sec81 %>% 
#   mutate(
#     professional = `246`+`254`,
#     managerial = `230`+`238`,
#     skilled_nonmanual = `262`+`270`+`326`,
#     skilled_manual = `294`+`302`+`310`+`334`+`342`+`358`,
#     unskilled = `278`+`286`+`318`+`350`,
#     other=`366`#occupation inadequately described
#   ) %>% 
#   dplyr::select(`Zone ID`,professional,managerial,skilled_nonmanual,skilled_manual,unskilled,other)

sec81 <- read_csv('1981/GreatBritain/1981_EWS_SEG_EconActiveOrRetired.csv')
names(sec81) <- gsub(x = names(sec81),pattern = '\\[81sas505|\\]',replacement = '')

sec81 <- sec81 %>%
  mutate(
    # professional = `149`+`150`,
    # managerial = `147`+`148`,
    prof_managerial = `149`+`150`+`147`+`148`,
    skilled_nonmanual = `151`+`152`+`159`,
    skilled_manual = `155`+`156`+`160`+`161`+`163`,
    unskilled = `153`+`154`+`157`+`158`+`162`,
    other=`164`#occupation inadequately described
  ) %>%
  dplyr::select(`Zone ID`,prof_managerial,skilled_nonmanual,skilled_manual,unskilled,other)



#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

#Check match. 0.36% false. Ah, the GB file just doesn't have them. Wonder why not?
table(sec81$`Zone ID` %in% its81$label)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:6)#from dta

result <- moveData(its81,sec81,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

View(head(result@data,50))

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1981socialclass.csv')




#~~~~~~~~~~~~~~~~~~~~~
#SOCIAL CLASS 1991----
#~~~~~~~~~~~~~~~~~~~~~

#Apart from one category, this already matches our 5-cat social class breakdown
sec91 <- read_csv('1991/GreatBritain/1991_LBS_EWS_SEG.csv')
names(sec91) <- gsub(x = names(sec91),pattern = 'l930',replacement = '')


#Just going to download Scotland separately to check numbers
# sec91scot <- read_csv('1991/Scotland/1991_LBS_Scotland_socialclass.csv')
# names(sec91scot) <- gsub(x = names(sec91scot),pattern = 'l930',replacement = '')
# 
# #Keep only GB zones that are in Scotland, for direct comparison
# sec91GB_justscot <- sec91[sec91$`Zone ID` %in% sec91scot$`Zone ID`,]



#91 is 16+
sec91 <- sec91 %>% 
  mutate(
    # professional = `133`,
    # managerial = `145`,
    prof_managerial = `133`+`145`,
    skilled_nonmanual = `157`,
    skilled_manual = `169`+`205`,#205 is armed forces
    unskilled = `193`+`181`,#181 is 'partly skilled' - putting here match 2001 D category that includes semi-skilled in unskilled
    other=`217`#occupation inadequately described
  ) %>% 
  dplyr::select(`Zone ID`,prof_managerial,skilled_nonmanual,skilled_manual,unskilled,other)



#1991 attempt 2: previous table had all zero vals for Scotland
#Same categories here - and both the same as 1981. But different column numbering for scot vs eng/wales
sec91engwales <- read_csv('1991/EnglandWales/1991_EngWales_socialclass_LBS86.csv')
names(sec91engwales) <- gsub(x = names(sec91engwales),pattern = 'l860',replacement = '')

sec91scot <- read_csv('1991/Scotland/1991_scotland_socialclass_LBS86.csv')
names(sec91scot) <- gsub(x = names(sec91scot),pattern = 'l860',replacement = '')


sec91engwales <- sec91engwales %>% 
  mutate(
    prof_managerial = `023`+`037`+`051`+`065`,
    skilled_nonmanual = `079`+`093`+`191`,
    skilled_manual = `135`+`149`+`205`+`219`+`247`,
    unskilled = `107`+`121`+`163`+`177`+`233`,
    other=`261`#occupation inadequately described
  ) %>% 
  dplyr::select(`Zone ID`,prof_managerial,skilled_nonmanual,skilled_manual,unskilled,other)


sec91scot <- sec91scot %>% 
  mutate(
    prof_managerial = `027`+`043`+`059`+`075`,
    skilled_nonmanual = `091`+`107`+`219`,
    skilled_manual = `155`+`171`+`235`+`251`+`283`,
    unskilled = `123`+`139`+`187`+`203`+`267`,
    other=`299`#occupation inadequately described
  ) %>% 
  dplyr::select(`Zone ID`,prof_managerial,skilled_nonmanual,skilled_manual,unskilled,other)


#Join those two
sec91 <- rbind(sec91engwales,sec91scot)



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
sec91$aggID <- sec91$`Zone ID`

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  sec91$aggID[sec91$`Zone ID` %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- sec91[,2:7] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
#zeroes <- agg2[apply(agg2[,2:3],1,sum)==0,]
#zeroes <- merge(zeroes[,1],ea91final,by.x = 'label',by.y = 'zone')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
#table(as.character(zeroes$label))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

geogzdf <- data.frame(geogz)

write_csv(geogzdf,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991socialclass_LBS86.csv')



#~~~~~~~~~~~~~~~~~~~~~
#SOCIAL CLASS 2001----
#~~~~~~~~~~~~~~~~~~~~~

#Eng scot wales separate files, combine
sec01eng <- read_csv('2001/England/2001_England_socialclass.csv')
sec01wales <- read_csv('2001/Wales/2001_Wales_socialclass.csv')
sec01scot <- read_csv('2001/Scotland/2001_scotland_socialclass.csv')

#Names all match...
table(names(sec01eng) %in% names(sec01wales))
table(names(sec01eng) %in% names(sec01scot))

#Combine into one
sec01 <- do.call("rbind",list(sec01eng,sec01wales,sec01scot))

#Rename
sec01 <- sec01 %>% 
  mutate(unskilled = UV0500005 + UV0500006) %>% 
  dplyr::select(-c(UV0500001,UV0500005,UV0500006),`Zone Code`,prof_managerial = UV0500002,skilled_nonmanual=UV0500003,skilled_manual=UV0500004,unskilled)


#INTERSECT
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

#Check match to data. 8 false.
table(sec01$`Zone Code` %in% its01$zone_code)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:5)#from dta

result <- moveData(its01,sec01,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2001socialclass.csv')



#~~~~~~~~~~~~~~~~~~~~~
#SOCIAL CLASS 2011----
#~~~~~~~~~~~~~~~~~~~~~

#16-64 only
sec11engwales <- read_csv('2011/EnglandWales/2011_EngWales_approxsocialgrade16-64.csv') %>% rename(Outputarea = `2011 output area`)

#Scotland: Manually tidied / dashed replaced with zeroes
sec11scot <- read_csv('2011/Scotland/OutputAreaStd_ALL/QS613SC_tidied.csv') %>% dplyr::select(-`All people aged 16 to 64`)

#Check column match... Newp!
#table(names(sec11engwales) %in% names(sec11scot))
#The order matches, we can just replace
names(sec11scot)=names(sec11engwales)

#Link into single
sec11 <- rbind(sec11engwales,sec11scot)

#Can just rename cols and we're done
names(sec11) <- c("Outputarea","prof_managerial","skilled_nonmanual","skilled_manual","unskilled")



#INTERSECT
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

#Check match to data. 2 false.
table(sec11$Outputarea %in% its11$code)

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:5)#from dta

result <- moveData(its11,sec11,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2011socialclass.csv')





#~~~~~~~~~~~~~~~~~~~~~~
#CHECK SOCIAL CLASS VARS-----
#~~~~~~~~~~~~~~~~~~~~~~

#Load and visualise
#List of cities taken from prep_landregistry_data.R
#Can add in Scots cities
# #Cities I'd like to keep
citiesToKeep <- c(
  "London",
  "Manchester",
  "Birmingham",
  "Bristol",
  "Newcastle & Durham",
  "Leeds",
  "Leicester",
  "Liverpool",
  "Nottingham",
  "Southampton",
  "Sheffield & Rotherham",
  "Reading & Bracknell",
  "Brighton",
  "Oxford",
  "Glasgow",
  "Edinburgh"
)

#Smaller list, chart too busy
citiesToKeep <- c(
  "London",
  "Manchester",
  "Birmingham",
  # "Bristol",
  "Leeds",
  "Sheffield & Rotherham",
  "Glasgow",
  "Edinburgh",
  "Cardiff"
)

#Load geography, has TTWA in it
GBgeog <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')

names(GBgeog)

#Load 71-11 social class
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'social',full.names = T)

#First three decades have an 'other' category, two don't.
#Add an 'other' containing zeroes to those that don't.
years <- c(1971,1981,1991,2001,2011)
#Dropping other from the first three
sec <- lapply(1:5,function(x) read_csv(filez[x]) %>%
                mutate(year = years[x]))

sec[[4]]$other <- 0
sec[[5]]$other <- 0

#Now they all have same col names, should bind.
sec <- bind_rows(sec)


#REMOVE OTHER
# years <- c(1971,1981,1991,2001,2011)
# #Dropping other from the first three
# sec <- lapply(1:5,function(x) read_csv(filez[x]) %>%
#                 dplyr::select(label,prof_managerial,skilled_nonmanual,skilled_manual,unskilled) %>%
#                 mutate(year = years[x]))
# sec <- bind_rows(sec)

#Check TTWA match. Tick!
table(citiesToKeep %in% GBgeog$ttwa)

#Link... don't need geographical info
#Check match. Tick.
table(GBgeog$zone %in% sec$label)

sec <- sec %>% left_join(GBgeog %>% st_set_geometry(NULL) %>% dplyr::select(zone,ttwa), by = c('label'='zone'))

#Nooow. Need,say, owner-occ as proportion of total, right?
#And want to see social/privat rent as prop of total too plz

#We should select our cities first
#Then need to sum pops before finding proportions.
sec_sums <- sec %>%
  group_by(year,ttwa) %>% 
  summarise(prof_managerial = sum(prof_managerial),skilled_nonmanual = sum(skilled_nonmanual),
            skilled_manual=sum(skilled_manual),unskilled=sum(unskilled), other = sum(other))

sec_sums <- sec_sums %>% 
  mutate(
    totalpop = prof_managerial + skilled_nonmanual + skilled_manual + unskilled + other,
    prof_managerial_prop = (prof_managerial/totalpop)*100,
    skilled_nonmanual_prop = (skilled_nonmanual/totalpop)*100,
    skilled_manual_prop = (skilled_manual/totalpop)*100,
    unskilled_prop = (unskilled/totalpop)*100,
    other_prop = (other/totalpop)*100
  )



#Plot that mofo. Filter down to the cities we picked
ggplot(sec_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = skilled_manual_prop, colour = fct_reorder(ttwa,-skilled_manual_prop))) +
# ggplot(sec_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = unskilled_prop, colour = fct_reorder(ttwa,-unskilled_prop))) +
  # ggplot(sec_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = prof_managerial_prop, colour = fct_reorder(ttwa,-prof_managerial_prop)))+
  # ggplot(sec_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = skilled_nonmanual_prop, colour = fct_reorder(ttwa,-skilled_nonmanual_prop))) +
  # ggplot(sec_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = other_prop, colour = fct_reorder(ttwa,-other_prop))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette='Dark2')


#Test for whole of GB. From rank below, looks like shifts within cities could actually be there? Errrr.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK USING RANK OF SOCIAL CLASS PER DECADE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#e.g. if I rank proportion professional in each ward, does that give us something that can be compared across decades? Cos each decade will be internally sensible, possibly? Let's see.
#Plan:
#Create ranking: find proportion of (say) managerial/professional (or maybe proportion unskilled would be better? Try both - in theory, should end up with same result). Rank wards by that proportion.
#Take that proportion to check graphically - use "average rank per TTWA"
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'social',full.names = T)

#First three decades have an 'other' category, two don't.
#Add an 'other' containing zeroes to those that don't.
years <- c(1971,1981,1991,2001,2011)
#Dropping other from the first three
sec <- lapply(1:5,function(x) read_csv(filez[x]) %>%
                mutate(year = years[x]))

sec[[4]]$other <- 0
sec[[5]]$other <- 0

sec <- bind_rows(sec)


#Proportions of each SEC per ward
sec_props <- sec %>% 
  mutate(
    totalpop = prof_managerial + skilled_nonmanual + skilled_manual + unskilled + other,
    prof_managerial_prop = (prof_managerial/totalpop)*100,
    skilled_nonmanual_prop = (skilled_nonmanual/totalpop)*100,
    skilled_manual_prop = (skilled_manual/totalpop)*100,
    unskilled_prop = (unskilled/totalpop)*100,
    other_prop = (other/totalpop)*100
  ) %>% 
  dplyr::select(-c(prof_managerial:other),-fnl_rsL)


#Rank each SEC per ward (and in each decade). Tie method is average, should be fine
sec_ranks <- sec_props %>% 
  group_by(year) %>% 
  mutate_at(vars(prof_managerial_prop:other_prop),rank)
  
#Test rank is doing what I think i.e. higher rank number is higher proportion... tick
#Can't find how to keep old columns so merge back in
# sec_rankscheck <- sec_ranks %>% left_join(sec_props,by = c('year','label')) %>% 
#   dplyr::select(skilled_manual_prop.x,skilled_manual_prop.y)
# View(sec_rankscheck %>% filter(year==1971))


#Now to look at. Find average rank per TTWA
citiesToKeep <- c(
  "London",
  "Manchester",
  "Birmingham",
  # "Bristol",
  "Leeds",
  "Sheffield & Rotherham",
  "Glasgow",
  "Edinburgh",
  "Cardiff"
)

#Load geography, has TTWA in it
GBgeog <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')

sec_ranks <- sec_ranks %>% left_join(GBgeog %>% st_set_geometry(NULL) %>% dplyr::select(zone,ttwa), by = c('label'='zone'))


#Average rank per TTWA for a selected ranked SEC
#Note _prop here is rank
sec_avrank_perTTWA <- sec_ranks %>% 
  group_by(year,ttwa) %>% 
  summarise(
    managerial_professional_avrank = mean(prof_managerial_prop),
    unskilled_avrank = mean(unskilled_prop),
    skilled_manual_avrank = mean(skilled_manual_prop),
    skilled_nonmanual_avrank = mean(skilled_nonmanual_prop)
  )

#Try with median... about the same
sec_avrank_perTTWA <- sec_ranks %>% 
  group_by(year,ttwa) %>% 
  summarise(
    managerial_professional_avrank = median(prof_managerial_prop),
    unskilled_avrank = median(unskilled_prop),
    skilled_manual_avrank = median(skilled_manual_prop),
    skilled_nonmanual_avrank = median(skilled_nonmanual_prop)
  )


# ggplot(sec_avrank_perTTWA %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = managerial_professional_avrank, colour = fct_reorder(ttwa,-managerial_professional_avrank))) +
# ggplot(sec_avrank_perTTWA %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = unskilled_avrank, colour = fct_reorder(ttwa,-unskilled_avrank))) +
ggplot(sec_avrank_perTTWA %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = skilled_nonmanual_avrank, colour = fct_reorder(ttwa,-skilled_nonmanual_avrank))) +
# ggplot(sec_avrank_perTTWA %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = skilled_manual_avrank, colour = fct_reorder(ttwa,-skilled_manual_avrank))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette='Dark2') +
  # labs(colour="skilled manual")
  labs(colour="skilled nonmanual")
  # labs(colour="unskilled")
  # labs(colour="prof/managerial")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~----

#~~~~~~~
#AGE----
#~~~~~~~

#~~~~~~~~~~~~
#AGE 1971----
#~~~~~~~~~~~~

age1971 <- read_csv('1971/GreatBritain/1971_GB_age_peopleinprivatehouseholds_male_female.csv')
#Keep just table row code numbers
names(age1971) <- gsub(x = names(age1971),pattern = 'c71s07_',replacement = '')

#Has several cats that need combining across rows to get single ageband figures
#Looking at CASWEB col codes...
#And look at order of column indices. Can sum across four columns (except for first sum which is two)
age1971 <- age1971 %>% 
  mutate(
    "0-14" = .[[3]]+.[[4]]
  )

# View(head(age1971[,c(50:57)],50))


#Rest in steps of four
#Check sequence starts in right place, first age group column... tick
names(age1971)[seq(from =5, to = 56, by = 4)]

#Col names
colz <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")
colseq <- seq(from =5, to = 56, by = 4)

for(i in 1:13){

  age1971[,colz[i]] <- age1971[,colseq[i]] + age1971[,colseq[i]+1] + age1971[,colseq[i]+2] + age1971[,colseq[i]+3]
  # names(age1971)[names(age1971)=='x'] <- colz[i]
  
}

View(head(age1971[,c(57:71)],50))
View(head(age1971,50))

#Basic sanity check
age1971 <- age1971 %>% 
  dplyr::select(`Zone Code`,`0-14`:`75+`)

#Rename in case it causes problems...
# colz <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")
# 
# names(age1971) <- c(
#   "Zone Code",colz
# )

#Well that first one doesn't look right! Except it's a much larger cat, so maybe fine. Rest looks alright.
#Yup, checked numbers, is all right.
# x <- apply(x,2,sum)
# plot(x)

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

#Check match for 71 tenure zones against intersect
table(age1971$`Zone Code` %in% its71$zone_code)

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

debugonce(moveData)
result <- moveData(its71,age1971,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#Looks like it worked...
View(head(result@data,50))

df <- data.frame(result)

# write_csv(df,'data/census_sources/econActiveSources/output/1971econActive.csv')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971age.csv')


#~~~~~~~~~~~~
#AGE 1981----
#~~~~~~~~~~~~

age1981 <- read_csv('1981/GreatBritain/1981_GB_age_residents.csv')
#Keep just table row code numbers
names(age1981) <- gsub(x = names(age1981),pattern = '\\[81sas020|\\[81sas020|\\]',replacement = '')

#Correct all-persons column, just needs some renaming and binning
#0-14 / 15-19 / Then five year bands in all / 75+
colz <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")

#57,64,71 = 0-14
age1981 <- age1981 %>% 
  mutate(`0-14` = `057`+`064`+`071`,
         `15-19` = `078`+`085`,
         `75+` = `169` + `176` + `183`
  ) %>% 
  dplyr::select(-c(`057`,`064`,`078`,`085`,`071`,`169`,`176`,`183`))

#Drop in other col names from 092 to 162 then re-order
names(age1981)[c(2:12)] <- colz[c(3:13)]

age1981 <- age1981 %>% 
  dplyr::select(1,13,14,2:12,15)



#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its81 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB81EDs_to_GB91PCSagg4correctCount")

#Check match. 0.36% false. Ah, the GB file just doesn't have them. Wonder why not?
table(age1981$`Zone ID` %in% its81$label)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its81,age1981,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

View(head(result@data,50))

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1981age.csv')


#~~~~~~~~~~~~
#AGE 1991----
#~~~~~~~~~~~~

age1991 <- read_csv('1991/GreatBritain/GB_1991_age.csv')

names(age1991) <- gsub(x = names(age1991),pattern = 'l020',replacement = '')

#0-14 / 15-19 / Then five year bands in all / 75+
age1991 <- age1991 %>% 
  mutate(`0-14` = `012`+`023`+`034`,
         `15-19` = `045`+`056`+`067`,
         `75+` = `199` + `210` + `221`+`232`
  ) %>% 
  dplyr::select(-c(`001`,`012`,`023`,`034`,`045`,`056`,`067`,`199`,`210`,`221`,`232`))


#Drop in other col names from 092 to 162 then re-order
colz <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")
names(age1991)[c(3:13)] <- colz[c(3:13)]

age1991 <- age1991 %>% 
  dplyr::select(1,14,15,3:13,16)



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
age1991$aggID <- age1991$`Zone ID`

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  age1991$aggID[age1991$`Zone ID` %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- age1991[,2:16] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
#zeroes <- agg2[apply(agg2[,2:3],1,sum)==0,]
#zeroes <- merge(zeroes[,1],ea91final,by.x = 'label',by.y = 'zone')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
#table(as.character(zeroes$label))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

geogzdf <- data.frame(geogz)

write_csv(geogzdf,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991age.csv')




#~~~~~~~~~~~~
#AGE 2001----
#~~~~~~~~~~~~

#EWS separate... Nope, these tables too crude
# age2001_e <- read_csv('2001/England/2001_England_age.csv')
# age2001_s <- read_csv('2001/Scotland/2001_Scotland_age.csv')
# age2001_w <- read_csv('2001/Wales/2001_wales_age.csv')
# 
# #Names all looking identical. Only need trailing two digits
# names(age2001_e) <- gsub(x = names(age2001_e),pattern = 'ks00200',replacement = '')
# names(age2001_s) <- gsub(x = names(age2001_s),pattern = 'ks00200',replacement = '')
# names(age2001_w) <- gsub(x = names(age2001_w),pattern = 'ks00200',replacement = '')
# 
# #Combine
# age2001 <- do.call("rbind",list(age2001_e,age2001_w,age2001_s))
# 
# 
# age2001 <- age2001 %>% 
#   mutate(`0-14` = `02`+`03`+`04` +`05`,
#          `15-19` = `06`+`07`+`08`,
#          `75+` = `15` + `16` + `17`
#   ) %>% 
#   dplyr::select(-c(`02`,`03`,`04`,`05`,`06`,`07`,`08`,`15`,`16`,`17`))
# 
# 
# #Drop in other col names from 092 to 162 then re-order
# names(age2001)[c(2:12)] <- colz[c(3:13)]
# 
# age2001 <- age2001 %>% 
#   dplyr::select(1,14,15,3:13,16)



#Univariate tables with single age categories, all need binning.
age2001_e <- read_csv('2001/England/2001_england_age_univariate.csv')
age2001_s <- read_csv('2001/Scotland/2001_scotland_age_univariate.csv')
age2001_w <- read_csv('2001/Wales/2001_wales_age_univariate.csv')

#Combine
age2001 <- do.call("rbind",list(age2001_e,age2001_w,age2001_s))

#Only need trailing two digits
names(age2001) <- gsub(x = names(age2001),pattern = 'UV00400',replacement = '')

#Single age groups, all need binning into these. Just here for ref.
colz <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")

#Luckily, actual row numbers match CASWEB column names. Huzzah!
age2001 <- age2001 %>% 
  mutate(
    `0-14` = rowSums(.[2:16]),
    `15-19` = rowSums(.[17:21]),
    `20-24` = rowSums(.[22:26]),
    `25-29` = rowSums(.[27:31]),
    `30-34` = rowSums(.[32:36]),
    `35-39` = rowSums(.[37:41]),
    `40-44` = rowSums(.[42:46]),
    `45-49` = rowSums(.[47:51]),
    `50-54` = rowSums(.[52:56]),
    `55-59` = rowSums(.[57:61]),
    `60-64` = rowSums(.[62:66]),
    `65-69` = rowSums(.[67:71]),
    `70-74` = rowSums(.[72:76]),
    `75+` = rowSums(.[77:82]),
  ) %>% 
  dplyr::select(-c(2:82))



#INTERSECT
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

#Check match to data. 8 false.
table(age2001$`Zone Code` %in% its01$zone_code)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its01,age2001,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2001age.csv')


#~~~~~~~~~~~~
#AGE 2011----
#~~~~~~~~~~~~

#NOMIS million row limit, had to download in four tranches. Recombine:
# filez <- list.files(path = '2011/GreatBritain/2011_age',pattern = 'age',full.names = T)
# 
# years <- c(1971,1981,1991,2001,2011)
# tenure <- lapply(1:5,function(x) read_csv(filez[x]) %>% mutate(year = years[x]))
# tenure <- bind_rows(tenure)

#Single year again, EW then Scotland. Drop totalpop while loading
age2011ew <- read_csv('2011/EnglandWales/qs103ew_2011_oa/QS103EWDATA.CSV') %>% dplyr::select(1,3:103)

View(sample_n(age2011ew,150))

#Col names: 01 is total people; 02 = < 1; 03 = 1 year; 04 is 2 year etc.
#Err, there doesn't seem to be a 100 year old cat....?
#Oh, the last one is 100+

#Which means we can recode names thus:
names(age2011ew) <- c(
  "ZoneCode",
  0:99,
  "100+"
)

#SCOTLAND
age2011s <- read_csv('2011/Scotland/OutputAreaStd_ALL/QS103SC_headerfooter_colsremoved.csv') %>% 
  rename(`0`=`Under 1`,`100+`=`100 and over`)

#That's still going to have dashes for zeroes.
#Replace and convert back to numeric cols.
lapply(age2011s,class)
#View(head(age2011s %>% select(1:15)))

age2011s[age2011s=='-'] <- "0"

age2011s[,c(2:102)] <- lapply(age2011s[,c(2:102)],as.integer)
  

#GB
age2011 <- rbind(age2011ew,age2011s)

#And now for the actual recode. Actually, matches 2001 perfectly, can just re-use. Bonza.
#Apart from last few, we have more single-age years here. 
age2011 <- age2011 %>% 
  mutate(
    `0-14` = rowSums(.[2:16]),
    `15-19` = rowSums(.[17:21]),
    `20-24` = rowSums(.[22:26]),
    `25-29` = rowSums(.[27:31]),
    `30-34` = rowSums(.[32:36]),
    `35-39` = rowSums(.[37:41]),
    `40-44` = rowSums(.[42:46]),
    `45-49` = rowSums(.[47:51]),
    `50-54` = rowSums(.[52:56]),
    `55-59` = rowSums(.[57:61]),
    `60-64` = rowSums(.[62:66]),
    `65-69` = rowSums(.[67:71]),
    `70-74` = rowSums(.[72:76]),
    `75+` = rowSums(.[77:102]),
  ) %>% 
  dplyr::select(-c(2:102))


View(sample_n(age2011,150))


#INTERSECT
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

#Check match to data...
table(age2011$ZoneCode %in% its11$code)

#All the English codes are present though (the its11 intersect has a match for every zone). What the hell???
table(its11$code %in% age2011$ZoneCode)

x <- its11$code[!age2011$ZoneCode %in% its11$code]
#English codes not matching.
y <- age2011$ZoneCode[!age2011$ZoneCode %in% its11$code]

sample(its11$code,100)
#Eng sample not match: E02000619. They're all E02 codes.
#its11 sample:         E00093324

#That's a puzzle. Seem to have full matches to the intersect geog
#So intersecting will just exclude that large number of zones in the data whose zone is... somewhere else.
#Would like to know where the hell they are.
its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:15)#from dta

result <- moveData(its11,age2011,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2011age.csv')



#~~~~~~~~~~~~~~~~~~~
#CHECK AGE VARS-----
#~~~~~~~~~~~~~~~~~~~


#Smaller list, chart too busy
citiesToKeep <- c(
  "London",
  "Manchester",
  "Birmingham",
  # "Bristol",
  "Leeds",
  "Sheffield & Rotherham",
  "Glasgow",
  "Edinburgh",
  "Cardiff"
)

#Load geography, has TTWA in it
GBgeog <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')

names(GBgeog)

#Load 71-11 age
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'age',full.names = T)

years <- c(1971,1981,1991,2001,2011)
#Dropping other from the first three
age <- lapply(1:5,function(x) read_csv(filez[x]) %>%
                mutate(year = years[x]))


#Now they all have same col names, should bind.
age <- bind_rows(age)

#Link... don't need geographical info
#Check match. Tick.
table(GBgeog$zone %in% age$label)

age <- age %>% left_join(GBgeog %>% st_set_geometry(NULL) %>% dplyr::select(zone,ttwa), by = c('label'='zone'))

#Better col names plz
colz <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")

names(age) <- c(
  "label","fnl_rsL",
  colz,
  "year","ttwa"
)


#Sums per decade and TTWA
age_sums <- age %>%
  group_by(year,ttwa) %>% 
  summarise_at(vars(`0-14`:`75+`),funs(sum(.,na.rm=T) ))

age_sums <- age_sums %>% 
  ungroup() %>% 
  mutate(
    totalpop = rowSums(.[3:16])
  )


#Each age group as a proportion of total population in that zone
age_props <- age_sums %>%
  mutate_at(vars(`0-14`:`75+`), funs( (./totalpop)*100 ))


#Let's just look at total numbers for now, see how those have changed?
agesums_long <- age_sums %>% 
  dplyr::select(-totalpop) %>% 
  gather(key=agegroup, value = count,`0-14`:`75+`)


ggplot(agesums_long %>% filter(ttwa %in% citiesToKeep[2:8]),aes(x = year, y = count, colour=fct_reorder(ttwa,-count), group=ttwa)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette='Dark2') +
  labs(colour='ttwa')+
  facet_wrap(~agegroup, scales='free_y')





#Plot that mofo. Filter down to the cities we picked
ggplot(age_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = skilled_manual_prop, colour = fct_reorder(ttwa,-skilled_manual_prop))) +
  # ggplot(age_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = unskilled_prop, colour = fct_reorder(ttwa,-unskilled_prop))) +
  # ggplot(age_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = prof_managerial_prop, colour = fct_reorder(ttwa,-prof_managerial_prop)))+
  # ggplot(age_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = skilled_nonmanual_prop, colour = fct_reorder(ttwa,-skilled_nonmanual_prop))) +
  # ggplot(age_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = other_prop, colour = fct_reorder(ttwa,-other_prop))) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette='Dark2')




#~~~~~~~~~~~~~~~~~~~~~~~
#AGE: London changes----
#~~~~~~~~~~~~~~~~~~~~~~~

#Find proportion of each age group per ward/pcs
age_props <- age %>% 
  mutate(
    totalpop = rowSums(.[3:16])
  ) %>% 
  mutate_at(vars(`0-14`:`75+`), funs( (./totalpop)*100 ))
  
#Check proportions are correct...tick
apply(age_props %>% dplyr::select(`0-14`:`75+`),1,sum)


#London geog
london <- GBgeog %>% filter(ttwa=='London')

#Join age proportion data
london <- left_join(london,age_props,by=c('zone'='label'))

library(tmap)

map <- tm_shape(london) +
  tm_polygons(col = '75+', style = 'jenks', n = 12, palette = 'plasma') +
  tm_facets(by = 'year', ncol = 1, nrow = 1) 


tmap_animation(map, filename = 'R_outputs/CensusHarmonising/Age_London/london_age_75+b.gif', width = 2000, height = 1000, delay = 100)



#Gif for each age group
colz <- c("0-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")

for(agegp in colz){
  
  map <- tm_shape(london) +
    tm_polygons(col = agegp, style = 'jenks', n = 12, palette = 'plasma') +
    tm_facets(by = 'year', ncol = 1, nrow = 1) 
  
  
  tmap_animation(map, filename = paste0('R_outputs/CensusHarmonising/Age_London/london_age_',agegp,'_150ms.gif'), 
                 width = 2000, height = 1000, delay = 150)
  
  
}



#Also: what about age group as a proportion across London zones, rather than within
#So e.g. all zones sum to 100% for each age group?

#So join age to London first, then use London-only zones to get across-London proportions
london_props <- GBgeog %>% filter(ttwa=='London')

#Join age proportion data
london_props <- left_join(london_props,age, by=c('zone'='label'))

london_props <- london_props %>% 
  group_by(year) %>% 
  mutate_at(vars(`0-14`:`75+`), funs(  (./sum(.))*100  ) )



#Again, check that worked... tick
apply(london_props %>% st_set_geometry(NULL) %>% ungroup() %>% dplyr::select(`0-14`:`75+`),2,sum)

#Same thing in pipey
london_props %>% 
  st_set_geometry(NULL) %>% 
  ungroup() %>% 
  dplyr::select(`0-14`:`75+`) %>% 
  summarise_all(funs(sum))

#ACROSS LONDON PROPORTIONS
for(agegp in colz){
  
  map <- tm_shape(london_props) +
    tm_polygons(col = agegp, style = 'jenks', n = 12, palette = 'plasma') +
    tm_facets(by = 'year', ncol = 1, nrow = 1) 
  
  
  tmap_animation(map, filename = paste0('R_outputs/CensusHarmonising/Age_London/across_london/acrosslondon_age_',agegp,'_150ms.gif'), 
                 width = 2000, height = 1000, delay = 150)
  
  
}

#Look at over London to see where some of those places are.
#Pick a single map.
tmap_mode('view')

tm_shape(london_props %>% filter(year==2011)) +
  tm_polygons(col = '75+', style = 'jenks', n = 12, palette = 'plasma', alpha = 0.4)



#~~~~~~~~~~~~~~~~~~~~~~~
#TENURE: London changes----
#~~~~~~~~~~~~~~~~~~~~~~~

#GIF maps for tenure too.
#Load 71-11 Tenure
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'tenure',full.names = T)

years <- c(1971,1981,1991,2001,2011)
tenure <- lapply(1:5,function(x) read_csv(filez[x]) %>% mutate(year = years[x]))
tenure <- bind_rows(tenure)


tenure_props <- tenure %>% 
  mutate(
    totalpop = rowSums(.[3:6])
  ) %>% 
  mutate_at(vars(`ownerocc`:`other`), funs( (./totalpop)*100 ))

#Check proportions are correct...tick
apply(tenure_props %>% dplyr::select(`ownerocc`:`other`),1,sum)



GBgeog <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')

#London geog
london <- GBgeog %>% filter(ttwa=='London')

#Join tenure proportion data
london <- left_join(london,tenure_props,by=c('zone'='label'))

map <- tm_shape(london) +
  tm_polygons(col = 'ownerocc', style = 'jenks', n = 12, palette = 'plasma') +
  tm_facets(by = 'year', ncol = 1, nrow = 1) 


tmap_animation(map, filename = 'R_outputs/CensusHarmonising/tenure_London/london_tenure_ownerocc.gif', width = 2000, height = 1000, delay = 150)





#~~~~~~~~~~~~~~~
#SOME CHECKS----
#~~~~~~~~~~~~~~~

#Ward vs PCS zone size----

gb <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')
    
#Check on different area sizes, E/W v Scotland
gb$country2 <- "EngladWales"
gb$country2[gb$country=="Scotland"] <- "Scotland"

gb$area <- st_area(gb$geometry)
gb$area <- gb$area/1000000

ggplot(gb,aes(x = as.numeric(area),colour= country2)) +
  geom_boxplot()

library(tmap)

qtm(gb %>% filter(ttwa=="Glasgow"))
qtm(gb %>% filter(ttwa=="Manchester"))

ggplot(gb %>% filter(ttwa %in% c("Glasgow","Manchester")),aes(y = as.numeric(area),colour= ttwa)) +
  geom_boxplot() +
  ylab("log10 km^2") +
  xlab("") +
  scale_y_log10()



#1991 10% sample vs 100%----

comp1991 <- read_csv('1991/England/1991_England_10vs100percentsample_comparison.csv')
names(comp1991) <- gsub(x = names(comp1991),pattern = 'l71000',replacement = '')

#1 is 100% counts total residents
#7 is 10% sample
comp1991 <- comp1991 %>% mutate(prop = (`7`/`1`)*100)

View(sample_n(comp1991,200))

#Quite a spread...
plot(density(comp1991$prop,na.rm=T))

#Well, it might average out by ward? Oh wait, these are wards!



#How does tenure work as social class proxy? Cf to professional/managerial----


#Tenure
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'tenure',full.names = T)

years <- c(1971,1981,1991,2001,2011)
tenure <- lapply(1:5,function(x) read_csv(filez[x]) %>% mutate(year = years[x]))
tenure <- bind_rows(tenure)


#Load 71-11 social class
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'social',full.names = T)

#First three decades have an 'other' category, two don't.
#Add an 'other' containing zeroes to those that don't.
years <- c(1971,1981,1991,2001,2011)
#Dropping other from the first three
sec <- lapply(1:5,function(x) read_csv(filez[x]) %>%
                mutate(year = years[x]))

sec[[4]]$other <- 0
sec[[5]]$other <- 0
#Now they all have same col names, should bind.
sec <- bind_rows(sec)


sec_props <- sec %>% 
  mutate(
    totalpop = prof_managerial + skilled_nonmanual + skilled_manual + unskilled + other,
    prof_managerial_prop = (prof_managerial/totalpop)*100,
    skilled_nonmanual_prop = (skilled_nonmanual/totalpop)*100,
    skilled_manual_prop = (skilled_manual/totalpop)*100,
    unskilled_prop = (unskilled/totalpop)*100,
    other_prop = (other/totalpop)*100
  )

tenure_props <- tenure %>% 
  mutate(
    totalpop = ownerocc + socialrent + privaterent + other,
    ownerocc_prop = (ownerocc/totalpop)*100,
    socialrent_prop = (socialrent/totalpop)*100,
    privaterent_prop = (privaterent/totalpop)*100,
    other_prop = (other/totalpop)*100
  )



#Compare prop ownerocc to prop professional_manag
both <- left_join(
  sec_props %>% dplyr::select(label,year,prof_managerial_prop),
  tenure_props %>% dplyr::select(label,year,ownerocc_prop),
  by = c('year','label')
)

ggplot(both %>% sample_n(5000), aes(x = prof_managerial_prop, y = ownerocc_prop)) +
  geom_point(alpha=0.2) +
  geom_smooth(method='lm') +
  facet_wrap(~year)

summary(lm(data = both %>% filter(year==1971), ownerocc_prop~prof_managerial_prop))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#EDUCATION (AS PROXY FOR SOCIAL CLASS)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Only available common cat: degree/higher vocational. Match may not be perfect, let's see.
#Age groups differ a little too.

#~~~~~~~~~~~~~~~~~~
#EDUCATION 1971----
#~~~~~~~~~~~~~~~~~~

educ1971 <- read_csv('1971/GreatBritain/GB_1971_qualification.csv')
#Keep just table row code numbers
names(educ1971) <- gsub(x = names(educ1971),pattern = 'c71s23_',replacement = '')

#three columns:
#SEG of persons total: 358
#With HN or degree: 363
#Below that, TOTAL for HN or degree not in employment: 399 

#Actually no, looking at the numbers - that bottom TOTAL is the total for all HN/Degree
#Both in employment and not.
#Best check if that's the case for the total persons too...
#Yeah, we need:
#Actually actually: the SEG of persons latter total is non-EA. We don't want that (I'm reasonably sure...!)
#So it's 
#SEG of persons total: 358
#Total degree/HN: 399


#So assuming 358 is everyone (economically active)
#Can make a "non-degree / other" category by subtracting
educ1971 <- educ1971 %>% 
  mutate(
    degreehighervoc = `399`,
    other = `358` - degreehighervoc
  ) %>% dplyr::select(1,degreehighervoc,other)

View(sample_n(educ1971,200))



#INTERSECT
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its71 <- readOGR("Intersects/GreatBritain5CensusLBS_7181","GB71EDs_to_GB91PCSagg4correctCount")

table(educ1971$`Zone Code` %in% its71$zone_code)

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its71,educ1971,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

#Looks like it worked...
View(sample_n(result@data,50))

df <- data.frame(result)

# write_csv(df,'data/census_sources/econActiveSources/output/1971econActive.csv')
write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971highereducation_10percentsample.csv')



#~~~~~~~~~~~~~~~~~~
#NOT DONE: EDUCATION 1981----
#~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~
#EDUCATION 1991----
#~~~~~~~~~~~~~~~~~~

#LBS84
#52: "All persons qualified at level a, b or c aged 18 up to pensionable age"
#1: all persons age 18 or over
#10% sample again (but unlike 1981 we have total persons)
educ1991 <- read_csv('1991/GreatBritain/1991_GB_qualifications.csv')
names(educ1991) <- gsub(x = names(educ1991),pattern = 'l8400',replacement = '')

educ1991 <- educ1991 %>% 
  rename(degreehighervoc = `52`) %>% 
  mutate(other = `01` - degreehighervoc) %>% 
  dplyr::select(`Zone ID`,degreehighervoc,other)



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
educ1991$aggID <- educ1991$`Zone ID`

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  educ1991$aggID[educ1991$`Zone ID` %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- educ1991[,2:4] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Just check that the zero-count agg2 zones are just shipping
#zeroes <- agg2[apply(agg2[,2:3],1,sum)==0,]
#zeroes <- merge(zeroes[,1],ea91final,by.x = 'label',by.y = 'zone')
#All shipping. Tick. 
#So this means, presuming the same numbers, we'll be OK merging on the geography with other variables.
#table(as.character(zeroes$label))

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

geogzdf <- data.frame(geogz)
#View(sample_n(geogzdf,100))

write_csv(geogzdf,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991highereducation_10percentsample.csv')



#~~~~~~~~~~~~~~~~~~
#EDUCATION 2001----
#~~~~~~~~~~~~~~~~~~

#UV024 England Wales
#01 = all people (it's saying actually all people but that seems odd for a table on quals. Let's see.)
#06 = level 4/5 (which is what we want)
#Scotland: UV025... this is all people 16-74. Hmmph.

#So actually, for direct comparability, we need...
#England Wales - KS013. Now 16-74 year olds
#1 = all people in that age range
#6 = level 4/5

#Scotland: equivalent is same field numbers, so works. Also KS013
educ2001 <- bind_rows(
  read_csv('2001/England/2001_England_KS013_qualifications.csv'),
  read_csv('2001/Wales/2001_Wales_KS013_qualifications.csv'),
  read_csv('2001/Scotland/2001_Scotland_KS013_qualifications.csv')
)

names(educ2001) <- gsub(x = names(educ2001),pattern = 'ks013000',replacement = '')


educ2001 <- educ2001 %>% 
  rename(degreehighervoc = `6`) %>% 
  mutate(other = `1` - degreehighervoc) %>% 
  dplyr::select(`Zone Code`,degreehighervoc,other)

#View(sample_n(educ2001,100))

#INTERSECT
#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

its01 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2001OAs_to_91PCS_wards")

#Check match to data. 8 false.
table(educ2001$`Zone Code` %in% its01$zone_code)


its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its01,educ2001,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)
View(sample_n(df,100))

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2001highereducation.csv')



#~~~~~~~~~~~~~~~~~~
#EDUCATION 2011----
#~~~~~~~~~~~~~~~~~~

educ2011 <- read_csv('2011/GreatBritain/2011_GB_highest_qualification.csv')

#Check that first col is looking like tot pop in this group... yeeees? Could do with pop to check against
#View(sample_n(educ2011,100))
educ2011 <- educ2011 %>% 
  rename(degreehighervoc = `Highest level of qualification: Level 4 qualifications and above`) %>% 
  mutate(other = `All categories: Highest level of qualification` - degreehighervoc) %>% 
  dplyr::select(`2011 output area`,degreehighervoc,other)

#View(sample_n(educ2011,100))


#INTERSECT

#the shapefile being assigned to
lrg <- readOGR(dsn="C:/Data/MapPolygons/GreatBritain/1991","GB_wards_pcs_agg4correctCount")

#Get the intersect geog:
its11 <- readOGR("Intersects/GreatBritain3CensusLBS","GreatBritain2011OAs_to_91PCS_wards")

#Check match to data. 2 false.
table(educ2011$`2011 output area` %in% its11$code)

its_smallZoneIDColumn <- 1
its_largeZoneIDColumn <- 2
dta_zoneIDcolumn <- 1
lrgID <- 1#cos it won't always match lrg name in intersect cos of clashes e.g. "label_2"
datacols <- c(2:3)#from dta

result <- moveData(its11,educ2011,lrg,its_smallZoneIDColumn,its_largeZoneIDColumn,dta_zoneIDcolumn,lrgID,datacols)

df <- data.frame(result)
View(sample_n(df,200))

write_csv(df,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/2011highereducation.csv')


#~~~~~~~~~~~~~~~~~~~
#CHECK EDUC VARS-----
#~~~~~~~~~~~~~~~~~~~

#We're missing 1981 at the moment.

#Smaller list, chart too busy
citiesToKeep <- c(
  "London",
  "Manchester",
  "Birmingham",
  # "Bristol",
  "Leeds",
  "Sheffield & Rotherham",
  "Glasgow",
  "Edinburgh",
  "Cardiff"
)

#Load geography, has TTWA in it
GBgeog <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp')



#Load 71-11 educ minus 81
filez <- list.files(path = 'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs',pattern = 'higher',full.names = T)

years <- c(1971,1991,2001,2011)
educ <- lapply(1:4,function(x) read_csv(filez[x]) %>% mutate(year = years[x]))
educ <- bind_rows(educ)

#Check TTWA match. Tick!
table(citiesToKeep %in% GBgeog$ttwa)

#Link... don't need geographical info
#Check match. Tick.
table(GBgeog$zone %in% educ$label)

educ <- educ %>% left_join(GBgeog %>% st_set_geometry(NULL) %>% dplyr::select(zone,ttwa), by = c('label'='zone'))

#Nooow. Need,say, owner-occ as proportion of total, right?
#And want to see social/privat rent as prop of total too plz

#We should select our cities first
#Then need to sum pops before finding proportions.
educ_sums <- educ %>%
  group_by(year,ttwa) %>% 
  summarise(degreehighervoc =sum(degreehighervoc),other=sum(other))

educ_sums <- educ_sums %>% 
  mutate(
    totalpop = degreehighervoc + other,
    degreehighervoc_prop = (degreehighervoc/totalpop)*100,
    other_prop = (other/totalpop)*100
  )


ggplot(educ_sums %>% filter(ttwa %in% citiesToKeep), aes(x = year, y = degreehighervoc_prop, colour = fct_reorder(ttwa,-degreehighervoc_prop)))+   geom_line() +
  geom_point() +
  scale_color_brewer(palette='Dark2')


#Make a map!
educ_props <- educ %>% 
  mutate(
    totalpop = rowSums(.[3:4])
  ) %>% 
  mutate_at(vars(`degreehighervoc`:`other`), funs( (./totalpop)*100 ))

#Check proportions are correct...tick
apply(educ_props %>% dplyr::select(`degreehighervoc`:`other`),1,sum)


#London geog
london <- GBgeog %>% filter(ttwa=='London')

#Join educ proportion data
london <- left_join(london,educ_props,by=c('zone'='label'))

library(tmap)

map <- tm_shape(london) +
  tm_polygons(col = 'degreehighervoc', style = 'jenks', n = 12, palette = 'plasma') +
  tm_facets(by = 'year', ncol = 1, nrow = 1) 


tmap_animation(map, filename = 'R_outputs/CensusHarmonising/london_degreehighervoc.gif', width = 2000, height = 1000, delay = 150)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAP OVERVIEW FOR TTWAS AND OTHER THINGS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using population for 1991 wards/pcs. Find pop per km2, use as basis to show urban/rural
#Or actually can show as smooth with boundary laid over if I play in QGIS.

#Just getting total persons ('present residents') from LBS table 1
people91 <- read_csv('1991/GreatBritain/1991_GB_presentresidents.csv')

people91 <- people91 %>% 
  rename(people = l010001)


#Reassign to tweaked geog
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
people91$aggID <- people91$`Zone ID`

#label new zones with the zone being aggregated to
for(i in 1:length(zonez)) {
  
  #First is the new ID name, all are the ones to replace with it 
  #(though the first has already been done so could skip that...)
  people91$aggID[people91$`Zone ID` %in% zonez[[i]] ] <- zonez[[i]][1]
  
}

#aggregate by zone for all columns
#http://stackoverflow.com/questions/21295936/can-dplyr-summarise-over-several-variables-without-listing-each-one
agg2 <- people91[,3:4] %>% group_by(aggID) %>% 
  summarise_each(funs(sum))

names(agg2)[names(agg2)=="aggID"] <- "label"

#Attach to geography and save
geogz <- merge(pcs91shp,agg2,by = "label")

geogzdf <- data.frame(geogz)
#View(sample_n(geogzdf,100))

write_csv(geogzdf,'StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991peoplepresent_count.csv')

#Convert to sp
#Add field for people per km^2
geogz_sf <- st_as_sf(geogz)
geogz_sf$km2 <- as.numeric(st_area(geogz_sf$geometry))/1000000
#people per km^2
geogz_sf$people_per_km2 <- geogz_sf$people/geogz_sf$km2


st_write(geogz_sf, 'QGIS/1991_LBS_people_per_km2.shp')


#What's pop density distribution?
ggplot(geogz_sf, aes(x = people_per_km2)) +
  geom_histogram(bins = 50)










