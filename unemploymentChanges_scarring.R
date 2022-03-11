#Scarring and other stuff 2022
library(tidyverse)
library(sf)
library(tmap)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHANGES IN UNEMPLOYMENT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Get unemp figures and map
#https://statisticsglobe.com/merge-csv-files-in-r
ea <- list.files(path = "StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive", pattern = "*.shp", full.names = T) %>% 
  lapply(st_read) %>% 
  bind_rows()

ea <- st_set_crs(ea, "EPSG:27700")
st_crs(ea)

#Add censusYear
#Order should be correct, in order of files
ea$censusYear <- rep(seq(from = 1971, to = 2011, by = 10), each = nrow(ea)/5)

#Seems to have worked...
chk <- table(ea$label, ea$censusYear) %>% data.frame
mean(chk$Freq)

#Double check: populations should have gone up in each decade
ea %>% 
  st_set_geometry(NULL) %>% 
  group_by(censusYear) %>% 
  summarise(totaleconpop = sum(econActive,na.rm = T))
  # summarise(totaleconpop = sum(econActive,na.rm = T) + sum(unemployed,na.rm = T))


#Hmm, that's inconclusive! Better do it the slow way
# ea <- list.files(path = "StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive", pattern = "*.shp", full.names = T) %>% 
#   lapply(st_read)
# 
# years <- seq(from = 1971, to = 2011, by = 10)
# for(x in c(1:5)) ea[[x]]$censusYear <- years[x]
# 
# #Ready for combining
# ea <- do.call("rbind",ea)

#Same result as before... might just load that odd one and check it's right... tick. Overall drop in econ active + unemp? 
#Is that right? Seems wrong!
# chk01 <- st_read("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/EconActive/2001econActive.shp")
# sum(chk01$econActive+chk01$unemployed)



#Harrumph, don't have TTWAs. Need shapefile with that in
gb <- st_read("C:/Users/admin/Dropbox/SheffieldMethodsInstitute/CountryOfBirthOpenDataSets/data/gb_shapefile/gb_altered_wards_n_postcodesectors_w_lookup.shp")

gb <- gb %>% 
  st_set_geometry(NULL) %>% 
  select(zone,country,ttwa)

#Well you'd hope so... phew
table(ea$label %in% gb$zone)

ea <- ea %>% 
  left_join(gb, by = c("label" = "zone"))

rm(gb)

#Order TTWAs by most populous - might just look at a subselection of em
#Note: total econ pop != total pop. This doesn't include econ inactive. But will be good enough for this
#Note 2: summed for every Census decade. Still fine for us, just getting an ordered size list of TTWAs (approx)
ea$totaleconpop = ea$econActive + ea$unemployed

#Yeah, that works - Sheffield/Roth is 9th on list. So, important.
ttwa_order <- ea %>%
  st_set_geometry(NULL) %>% 
  group_by(ttwa) %>% 
  summarise(totaleconpop_perttwa = sum(totaleconpop)) %>% 
  arrange(-totaleconpop_perttwa)



#Random thing: where's the employment % coming from?
#I think maybe unemployed is *part* of "economically active"... which did I use?
#If so, econActive - unemployed = total employed. Then as prop of econ active...
#Ah yes, that's exactly what I did. Phew.
# ea <- ea %>% 
#   mutate(chk = ((econActive - unemployed ) / econActive ) * 100)



#MAP 1: ANIMATE SHEFFIELD EMPLOYMENT CHANGES OVER 5 CENSUSES----

#Check I have the right map
shef <- ea %>% filter(ttwa == "Sheffield & Rotherham")

map <- tm_shape(shef) +
  tm_polygons(col = 'percentEmp', style = 'jenks', n = 10, palette = 'plasma') +
  tm_facets(by = 'censusYear', ncol = 1, nrow = 1) 


tmap_animation(map, filename = 'R_outputs/Scarring/sheffield_unemploymentchange5census.gif', width = 800, height = 600, fps = 1)

#SAVE ea for rmarkdown



#PLOT 2: Top few TTWAs, unemp change over timE----

#Just want average unempl per TTWA
#Erm. Occurs to me, it should be population weighted by ward, right?
#Or - we've got the raw numbers here, just sum per TTWA then re-find proportion. 
#Yes, easier...
topttwas <- ea %>% filter(ttwa %in% ttwa_order$ttwa[1:10]) %>% 
  st_set_geometry(NULL) %>% 
  group_by(ttwa,censusYear) %>% 
  summarise(tot_econactive = sum(econActive), tot_unemployed = sum(unemployed)) %>% 
  mutate(percentUnemployed = (tot_unemployed / tot_econactive)*100)


ggplot(topttwas, aes(x = censusYear, y = percentUnemployed, colour = fct_reorder(ttwa,-percentUnemployed))) +
  geom_line() +
  geom_point() +
  scale_colour_brewer(palette = "Paired")

#What's interesting in that last plot - if we look at, say, 20:
#Between decades, direction is more or less the same for all TTWAS...
#EXCEPT 81-91, where we get several where unemployment goes UP (against what appears to be the general trend)
#Think I wanna see a map of that, maybe in QGIS actually

#That needs decadal change numbers - wanna see if directional. Err, so.
#Tempted to find numbers then reattach to geography to speed up...
allttwas <- ea %>% 
  st_set_geometry(NULL) %>% 
  group_by(ttwa,censusYear) %>% 
  summarise(tot_econactive = sum(econActive, na.rm=T), tot_unemployed = sum(unemployed, na.rm=T)) %>% 
  mutate(percentUnemployed = (tot_unemployed / tot_econactive)*100) %>% 
  ungroup()

#Have to make wide for viewing different decades in QGIS
#Oh, first, need to find differences
#Already ordered by census year, so can group by ttwa again
allttwas <- allttwas %>% 
  group_by(ttwa) %>% 
  mutate(percentUnemployedDiff = percentUnemployed - lag(percentUnemployed)) %>% 
  filter(!is.na(percentUnemployedDiff))#drop first decade, no change

#Widenemate
allttwas_diffswide <- allttwas %>%
  select(ttwa,censusYear,percentUnemployedDiff) %>% 
  spread(key = censusYear, value = percentUnemployedDiff) %>% 
  filter(!is.na(ttwa))

#Reattach geog and save for QGIS
#Need TTWA geog, in fact... Might work fine in R for plotting?
ttwamap <- st_read('C:/Data/MapPolygons/GreatBritain/2001/TTWAs/greatBritainTTWAs.shp')

#Tick
table(ttwamap$NAME %in% allttwas$ttwa)

#Don't actually need wide if plotting using tmap... though is it gonna be tricky to see? Let's see...
#Actually, just tried plotting just the st_geometry of the ttwas, it's sloooow. Let's do QGIS.
allttwas_diffswide <- ttwamap %>% 
  left_join(allttwas_diffswide, by = c('NAME' = 'ttwa'))

st_write(allttwas_diffswide, "QGIS/5census_unemploymentChangeBetweenCensuses_TTWA.shp")


#Just check - how many differ in their polarity of direction for each decade?
allttwas <- allttwas %>% 
  mutate(polaritypositive = percentUnemployedDiff > 0)

allttwas %>% 
  group_by(censusYear) %>% 
  summarise(mean(polaritypositive, na.rm=T))

#So yeah: 71-81 - unempl went up everywhere
#91-01 - dropped everywhere
#81-91 more mixed, 01-11 mixed but less so (77.7% of TTWAs, unempl went up.)

# PLOT 3: Z-SCORES----

#How does Sheffield differ relatively to the rest of GB in each decade? How does that look compared to the other top few TTWAs?

#Flip so we've got % UNemployed
ea <- ea %>% mutate(percentUnemployed = 100 - percentEmp)

#Get normalised value based on mean (Z scores)
#Z score = [mean(x) -x] / sd
ea <- ea %>% 
  group_by(censusYear) %>% 
  mutate(percentUnemployed_Zscore = (percentUnemployed - mean(percentUnemployed, na.rm=T))/sd(percentUnemployed, na.rm=T) )

#Just to look...
ea <- ea %>% 
  group_by(censusYear) %>% 
  mutate(meanpercentunemployed=mean(percentUnemployed, na.rm=T))

#Skeeeeew
ggplot(ea, aes(x = percentUnemployed_Zscore, colour = factor(censusYear))) +
  geom_density() +
  coord_cartesian(xlim=c(-5,2)) +
  scale_colour_brewer(palette = "Set1")

#Interesting here: 
#Only 1971 differs, with generally lower unemployment overall
#For the rest, thepattern remains very similar
#The geography of that may have changed of course, but... 

#Now to actually plot Sheffield's wards vs some other places
ggplot(ea %>% filter(ttwa %in% ttwa_order$ttwa[1:10]), aes(x = ttwa, y = percentUnemployed_Zscore)) +
  geom_point(size = 2, alpha = 0.5) +
  facet_wrap(~factor(censusYear)) +
  geom_hline(yintercept = 0, colour = 'green') +
  coord_flip()


#Violin plot? 
ggplot(ea %>% filter(ttwa %in% ttwa_order$ttwa[1:10]), aes(x = fct_reorder(ttwa,percentUnemployed_Zscore), y = percentUnemployed_Zscore)) +
  geom_point(size = 0.75, alpha = 0.3, colour = 'blue') +
  geom_violin(fill = 'black') +
  facet_wrap(~factor(censusYear)) +
  geom_hline(yintercept = 0, colour = 'green') +
  coord_flip(ylim=c(-2.5,7.5))

#Animated version with fixed x axis
#https://ryanpeek.org/2016-10-19-animated-gif_maps_in_r/

ea <- ea %>% mutate(ttwa = fct_reorder(ttwa, percentUnemployed_Zscore))

#save that version for use in output
saveRDS(ea,"R_data/econactive_ttwa_5census.rds")

#save images...
saveggplots <- function(year){
  
  ggplot(ea %>% filter(censusYear== year, ttwa %in% ttwa_order$ttwa[1:10]), aes(x = ttwa, y = percentUnemployed_Zscore)) +
    geom_point(size = 2, alpha = 0.3, colour = 'blue') +
    geom_violin(fill = 'black') +
    geom_hline(yintercept = 0, colour = 'green') +
    coord_flip(ylim=c(-2.5,7.5)) +
    labs(title = year) +
    theme(plot.title = element_text(size=50, hjust = 0.5))
  
  print(paste0("saving plot ", year))
  ggsave(filename = paste0("R_outputs/Scarring/animation_plots/",year,".png"),
         width = 7,height=7,dpi = 150)
  
}

seq(from = 1971, to=2011, by=10) %>% map_df(saveggplots)

library(magick)

list.files(path = "R_outputs/Scarring/animation_plots", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=0.5) %>% # animates, can opt for number of loops
  image_write("R_outputs/Scarring/census_unemployment_zscore_violinplots.gif")











