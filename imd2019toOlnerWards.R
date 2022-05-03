#IMD 2019 / Olner ward combo
#Intersect LSOAs and Olner wards
#Get a population-weighted average IMD2019 score per ward, convert to rank
library(tidyverse)
library(sf)

#~~~~~~~~~~~~~~~~~~~~~
#MAKE INTERSECTION----
#~~~~~~~~~~~~~~~~~~~~~

#LSOA geography
lsoas <- st_read('C://Users/admin/Dropbox/imd2019/data/England_lsoa_2011_gen_clipped/england_lsoa_2011_gen_clipped.shp')

#Hmmph, thought it had population. We'll need that from elsewhere. 2019 pop estimates
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
#Edited down from xlsx
#I deleted those extra commas in excel, thanks for keeping all the commas excel...
popest2019 <- read_csv('R_data/2019popestimates/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.csv') %>% select(`LSOA Code`,popcount = Count)

#Tick
table(lsoas$code %in% popest2019$`LSOA Code`)

lsoas <- lsoas %>% left_join(
  popest2019,
  by = c("code" = "LSOA Code")
)

#Find the LSOA area - we'll need that for the denominator when splitting population counts with the smaller intersecting areas
lsoas <- lsoas %>% 
  mutate(area = st_area(lsoas))


#Olner ward geography, keep just England
wards <- st_read('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp') %>% filter(country=="England")
wards <- st_set_crs(wards, st_crs(lsoas))

#plot(st_geometry(wards))
#Hmm, intersection is surely going to be too slow for R

#OK, let's see...
#98 seconds, not bad
x <- proc.time()

lsoa.ward.intersect <- st_intersection(lsoas,wards)

proc.time() -x

#Let's look at a single TTWA, see if we got what we need (think so).. tick
# plot(st_geometry(lsoa.ward.intersect %>% filter(ttwa=="Sheffield & Rotherham")))
# plot(st_geometry(wards %>% filter(ttwa=="Sheffield & Rotherham")), border="red", add=T)

#Make areas for the new intersected polygons
lsoa.ward.intersect <- lsoa.ward.intersect %>% 
  mutate(intersectionareas = st_area(lsoa.ward.intersect))

#Can now make population counts for each little area
#I feel like sf has a method for doing automatically, I can't find it
#Plenty of slivers, of course - very unlikely to affect any or many rank positions
lsoa.ward.intersect <- lsoa.ward.intersect %>% 
  mutate(popcount_intersection = popcount * (as.numeric(intersectionareas) / as.numeric(area)))


#~~~~~~~~~~~~~~~~~~~~~
#GET IMD AND APPLY----
#~~~~~~~~~~~~~~~~~~~~~

imd2019 <- read_csv('C://Users/admin/Dropbox/imd2019/data/imd2019lsoa.csv')

#Check score and rank both stack in the same direction
#Oh, decile is one of the measurements... fine
chk <- imd2019 %>% 
  filter(`Indices of Deprivation` == 'a. Index of Multiple Deprivation (IMD)') %>% 
  select(FeatureCode,Measurement,Value) %>% 
  spread(key = Measurement, value = Value)

#Tick... so we use scores in our population-weighted means
#NOte it's reverse order: highest score is ranked number 1 (most deprived)
ggplot(chk, aes(x = Rank, y = Score)) +
  geom_point()


#Merge in IMD main index score into the intersection
#Check match first. Tick.
table(lsoa.ward.intersect$code %in% imd2019$FeatureCode)

lsoa.ward.intersect <- lsoa.ward.intersect %>% 
  left_join(
    imd2019 %>% 
      filter(`Indices of Deprivation` == 'a. Index of Multiple Deprivation (IMD)', Measurement == 'Score') %>% select(FeatureCode, IMD_score = Value),
    by = c('code' = 'FeatureCode')
  )

#Maybe just save this, in case of crashes
#saveRDS(lsoa.ward.intersect,'R_data/olnerward_lsoa_intersect_imd2019.rds')

#In theory, should just be able to use weighted mean directly now?
#https://stackoverflow.com/questions/42979452/using-summarise-with-weighted-mean-from-dplyr-in-r
#Might drop geography to speed up?
popweightedwardIMD <- lsoa.ward.intersect %>% 
  st_set_geometry(NULL) %>% 
  group_by(zone) %>% 
  summarise(IMDscore_popweighted = weighted.mean(IMD_score,popcount_intersection))

#Well... it did something. Test with one or two to make sure?
#Or just rank and check in QGIS, maybe, see if looks right.
#RANK IS OPPOSITE WAY ROUND TO SCORE: HIGHEST SCORE IS MOST DEPRIVED; LOWEST RANK NUMBR 1 IS MOST DEPRIVED
popweightedwardIMD$IMDrank_popweighted <- rank(-popweightedwardIMD$IMDscore_popweighted)

#no dups? Nope, all unique rank values
length(popweightedwardIMD$IMDrank_popweighted)
length(unique(popweightedwardIMD$IMDrank_popweighted))

#Reattach to geography and look in QGIS
wards <- wards %>% left_join(popweightedwardIMD,by = 'zone')

#Yup, is looking perfect in QGIS
st_write(wards,'QGIS/popweightedwardIMD.shp',delete_layer = T)






