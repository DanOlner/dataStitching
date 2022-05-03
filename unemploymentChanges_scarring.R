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
##DUH: ECONACTIVE INCLUDES UNEMPLOYED
#ea$totaleconpop = ea$econActive + ea$unemployed

#Yeah, that works - Sheffield/Roth is 9th on list. So, important.
ttwa_order <- ea %>%
  st_set_geometry(NULL) %>% 
  group_by(ttwa) %>% 
  summarise(totaleconpop_per<- = sum(econActive)) %>% 
  arrange(-totaleconpop_perttwa)



#Random thing: where's the employment % coming from?
#I think maybe unemployed is *part* of "economically active"... which did I use?
#If so, econActive - unemployed = total employed. Then as prop of econ active...
#Ah yes, that's exactly what I did. Phew.
# ea <- ea %>% 
#   mutate(chk = ((econActive - unemployed ) / econActive ) * 100)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PLOT OF TTWA-LEVEL EMPLOYMENT FOR GB, WITH SHEF LOCATED----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Sum per TTWA all the various values to see GB as a whole
#I want min and max TTWAs, with Sheffield shown overlaid
ttwa_totemployment <- ea %>%
  st_set_geometry(NULL) %>% 
  group_by(ttwa,censusYear) %>% 
  summarise(econActive = sum(econActive, na.rm=T), unemployed = sum(unemployed, na.rm=T), employed = econActive - unemployed,
            percentemployed = (employed/econActive)*100) 




#Plot by a few that had the biggest and smallest decreases in employment 71-81, plot Sheffield against those.
#For which we need the change between Censuses per TTWA
ttwa_lags <- ttwa_totemployment %>% 
  group_by(ttwa) %>% 
  mutate(percentEmployedDiff = percentemployed - lag(percentemployed)) %>% 
  filter(!is.na(percentEmployedDiff))#drop first decade, no change

#Widenemate
ttwa_lags_wide <- ttwa_lags %>%
  select(ttwa,censusYear,percentEmployedDiff) %>% 
  spread(key = censusYear, value = percentEmployedDiff) %>% 
  filter(!is.na(ttwa)) %>% 
  rename(`71to81` = `1981`, `81to91` = `1991`, `91to01` = `2001`, `01to11` = `2011`) %>% 
  arrange(`71to81`)

#Pick top 5, bottom 5 and Sheffield
#(Using ordered lags to get highest, lowest changes but we want ttwa_totemployment for plotting)
tops <- ttwa_totemployment %>% 
  filter(ttwa %in% c(ttwa_lags_wide$ttwa[1:5],ttwa_lags_wide$ttwa[(nrow(ttwa_lags_wide)-4):nrow(ttwa_lags_wide)],"Sheffield & Rotherham"))

#11 of em, tick
unique(tops$ttwa)

#Mark sheffield as unique for plotting, mark top and bottom too
tops <- tops %>% 
  mutate(Sheffield = ifelse(ttwa == "Sheffield & Rotherham","Sheffield",NA)) %>% 
  mutate(Sheffield = replace(Sheffield, ttwa %in% ttwa_lags_wide$ttwa[1:5], "5 largest drops")) %>% 
  mutate(Sheffield = replace(Sheffield, ttwa %in% ttwa_lags_wide$ttwa[(nrow(ttwa_lags_wide)-4):nrow(ttwa_lags_wide)], "5 smallest drops")) %>% 
  mutate(Sheffield = factor(Sheffield, levels = c("5 largest drops","5 smallest drops","Sheffield")))
  

#Though if you think about it, the largest drops would HAVE to start higher to have room to drop that far
#Still - it does look like most of those large drops bounced back 81-91, Sheffield did not
#Can use this to describe bounce back though.


#Going to have to add Sheffield separately as we can't control line and shape size independently. Amazing!
ggplot(tops, aes(x = factor(censusYear), y = percentemployed, colour = Sheffield, shape = Sheffield, size = Sheffield, alpha = Sheffield, group = ttwa)) +
  geom_line() +
  geom_point() +
  scale_size_manual(values = c(1,1,3)) +
  scale_alpha_manual(values = c(0.6,0.6,1)) +
  geom_point(data = tops %>% filter(ttwa!="Sheffield & Rotherham"), aes(x = factor(censusYear), y = percentemployed), size = 3, alpha = 0.5) +
  geom_point(data = tops %>% filter(ttwa=="Sheffield & Rotherham"), aes(x = factor(censusYear), y = percentemployed), size = 7) +
  labs(colour="",shape="",size="",alpha="") +
  xlab("Census year") +
  ylab("Percent employed")

#Save tops for writeup
saveRDS(tops,"R_data/topbottom_ttwas_7181empdrop_inc_Sheffield.rds")


#Lots of interesting things there - note Sheffield didn't bounce back 81-91. The 5 largest drops did. 
#Which begs the question - how many other places saw drops in the following decade? How unique is Sheffield?
#Other thing to note: how high employment was in 1971 in the places that had the largest drops

#Which "tops" saw a drop 71-81 *and* a drop 81-91?
#Plus: What proportion actually did drop 71-81, and 81-91?
ttwa_lags_wide <- ttwa_lags_wide %>% 
  mutate(
    drop7181 = ifelse(`71to81` < 0,T,F),
    drop8191 = ifelse(`81to91` < 0,T,F),
    drop7181and8191 = ifelse(`71to81` < 0 & `81to91` < 0,T,F)
    )

#check props
table(ttwa_lags_wide$drop7181) %>% prop.table * 100#Every single TTWA, blimey
table(ttwa_lags_wide$drop8191) %>% prop.table * 100#Half and half

#So the next question makes no sense - 100% in 71to81, so yeah, all who dropped 81-91 also dropped decade earlier
#Better question's going to be - what places saw that continued drop? 
#Where were they? Did they start out with the most precipitous drop, as in the plot above?

#Mark all where 81-91 drop
ttwa_totemployment <- ttwa_totemployment %>% 
  mutate(drop8191 = ifelse( ttwa %in% ttwa_lags_wide$ttwa[ttwa_lags_wide$drop8191],T,F))


#Let's see if it's possible to see them all, just for the pattern
ggplot(ttwa_totemployment, aes(x = factor(censusYear), y = percentemployed, colour = drop8191, group = ttwa)) +
  geom_line() +
  geom_point() 

#OK, better thing to do here: let's look at 71 employment
#(And maybe 71-81 employment drop)
#For those who also dropped 81-91
totemployment1971 <- ttwa_totemployment %>% filter(censusYear==1971)

#Actually not that different
ggplot(totemployment1971, aes(x = percentemployed, colour = drop8191)) +
  geom_density()


#Map?
ttwamap <- st_read('C:/Data/MapPolygons/GreatBritain/2001/TTWAs/greatBritainTTWAs.shp')


ttwamap <- ttwamap %>% 
  left_join(
    totemployment1971 %>% select(ttwa,drop8191),
    by = c('NAME'='ttwa')
  )


#That's a weird geography! I don't know what's going on there. Clearly something...
tm_shape(ttwamap) +
  tm_polygons(col = 'drop8191', palette = 'plasma')

#Note: this is exactly what the bounceback regressions are trying to look at.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAP 1: ANIMATE SHEFFIELD EMPLOYMENT CHANGES OVER 5 CENSUSES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


#Err, don't use z scores??
ea <- ea %>% 
  group_by(censusYear) %>% 
  mutate(meanpercentunemployed = mean(percentUnemployed, na.rm=T))

ggplot(ea %>% filter(ttwa %in% ttwa_order$ttwa[1:10]), aes(x = fct_reorder(ttwa,percentUnemployed), y = percentUnemployed)) +
  geom_point(size = 0.75, alpha = 0.3, colour = 'blue') +
  geom_violin(fill = 'black') +
  facet_wrap(~factor(censusYear)) +
  geom_hline(aes(yintercept = meanpercentunemployed), colour = 'green') +
  coord_flip()
  # coord_flip(ylim=c(-2.5,7.5))


#Animated version with fixed x axis
#https://ryanpeek.org/2016-10-19-animated-gif_maps_in_r/

ea <- ea %>% mutate(ttwa = fct_reorder(ttwa, percentUnemployed_Zscore))

#save that version for use in output
saveRDS(ea,"R_data/econactive_ttwa_5census.rds")

#save images...
saveggplots <- function(year){

  #non z score version  
  # ggplot(ea %>% filter(censusYear== year, ttwa %in% ttwa_order$ttwa[1:10]), aes(x = ttwa, y = percentUnemployed)) +
  #   geom_point(size = 2, alpha = 0.3, colour = 'blue') +
  #   geom_violin(fill = 'black') +
  #   # geom_hline(yintercept = 0, colour = 'green') +
  #   geom_hline(aes(yintercept = meanpercentunemployed), colour = 'green') +
  #   coord_flip(ylim=c(0,40)) +
  #   labs(title = year) +
  #   theme(plot.title = element_text(size=50, hjust = 0.5))
  
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECKING ON UNEMPLOYMENT NUMBERS 1: INACTIVE PROPORTIONS 1991----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Looking back at the OG 1971 table...
ea71 <- read_csv('1971/GreatBritain/gb_econActive1971.csv')

#Just looking at 1991 LBS economic position LBS08.
#It's got a good 'economically inactive' breakdown - I'd like to see the geography of that
#(Using the original LBS wards huh? If I can find those.)
#I want to see the age breakdown too, plot-wise. 
#Err, going to be a very large amount of data - maybe stick to England for now

ea91 <- read_csv('1991/GreatBritain/LBS08_economicactivity_allvars.csv')

#Hmm, for the headline figures, I probably didn't want everyone in every geography did I? Could have compiled at larger areas.

#Let's just look at a map of the main econ inactive categories
#Shorter names to work with shp
ea91inactive <- ea91 %>% 
  select(zoneID = `Zone ID`, total = l080001, allInactive = l080172, studInactive = l080191, 
         sickInactive = l080210, retiInactive = l080229, othInactive = l080248)


#Get proportions
ea91inactive <- ea91inactive %>% 
  mutate(across(allInactive:othInactive, ~(./total)*100,  .names = "pc_{.col}")) %>% 
  mutate(sick_pcInac = (sickInactive/allInactive)*100 )
  

#orig LBS wards?
# wards91 <- st_read('C:/Data/MapPolygons/GreatBritain/1991/GB_wards_pcs_agg4correctCount.shp')
# 
# table(wards91$label %in% ea91$`Zone ID`)
# #Not the original wards... Hmmph, I seem not to have eng/wales/scotland joined anywhere pre-altered. Weird. Must have, surely? 
# table(ea91$`Zone ID` %in% wards91$label)

#Let's just look at England to start with then
england91wards <- st_read('C:/Data/MapPolygons/England/1991/England_wa_1991/england_wa_1991.shp')
table(ea91$`Zone ID` %in% england91wards$label)
table(england91wards$label %in% ea91$`Zone ID`)

wales91wards <- st_read('C:/Data/MapPolygons/Wales/1991/Wales_wa_1991_gen3/wales_wa_1991_gen3.shp')
table(wales91wards$label %in% ea91$`Zone ID`)

scots91wards <- st_read('C:/Data/MapPolygons/Scotland/1991/Scotland_census_pseudoPostcodeSectors_1991/soctland_pseudo_pcs_IDtoSingleRow.shp')
table(scots91wards$label %in% ea91$`Zone ID`)

#OK, join all those
gb91wards <- bind_rows(england91wards,wales91wards,scots91wards)

table(gb91wards$label %in% ea91$`Zone ID`)
#Not perfect. Why not? Leave for now
table(ea91inactive$zoneID %in% gb91wards$label)

gb91wards <- gb91wards %>% 
  left_join(ea91inactive, by = c('label' = 'zoneID'))

st_write(gb91wards,'qgis/econinactive_percents_gb91wards.shp')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECKING ON UNEMPLOYMENT NUMBERS 1: INACTIVE PROPORTIONS 1971----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Starting with EDs, might collate to 91 olnerwards at some point
ea71 <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/1971/GreatBritain/gb_econActive1971.csv')

#Looking at table in CASWEB...
#We have 'total / working / seeking work / sick' and one male / two female cats to sum

#Male/female combined
#Is unclear what that slight diff is. Let's try sick as prop of total
ea71 <- ea71 %>% 
  mutate(
    total = c71s05_257+c71s05_258+c71s05_259,
    working = c71s05_260+c71s05_263+c71s05_266,
    seekingwork = c71s05_261+c71s05_264+c71s05_267,
    sick = c71s05_262+c71s05_265+c71s05_268,
    check = working + seekingwork + sick,
    pc_sick_oftotal = (sick/total)*100
        )

ea71eds <- st_read('C:/Data/MapPolygons/GreatBritain/1971/gb71eds_dissolvedByID.shp')

#tick
table(ea71eds$zone_code %in% ea71$`Zone Code`)

ea71eds <- ea71eds %>% 
  left_join(
    ea71 %>% select(`Zone Code`, pc_sick_oftotal),
    by = c('zone_code' = 'Zone Code')
    )

st_write(ea71eds,'QGIS/ea71eds_percentsickoftotal.shp')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RECREATING VENABLES/RICE PAPER----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Rice, P.G., Venables, A.J., 2021. The persistent consequences of adverse shocks: how the 1970s shaped UK regional inequality. Oxford Review of Economic Policy 37, 132–151. https://doi.org/10.1093/oxrep/graa057

#Basics are not very complex.

#Figure 2a and b: 
#(a) Change in "ppt diff from average employment" 71-81 vs that change 71-11. (Showing where it's persisted.)
#(b) Same but 71-81 compared to 81-11. They're looking for bounce- back, so e.g. a -10% point drop would then be +10%. They're calling that "convergence" – discussed in the article above as:
#"The classic (or neoclassical) forces for convergence are simply that wage adjustment will cause some combination of replacement jobs moving into adversely affected places, and population moving out." 133/4

#So let's look at the same here... maybe for all the data at once to start with, before breaking down by...
#Oh, this is so going to be a multilevel thing! Or should be.
#Or maybe they actually already did it.

#Have to find ppt diff from averages first, don't I? Then diff between decades for the set decades.
ea <- ea %>%
  group_by(censusYear) %>% 
  mutate(pc_unempl_pptdiff_fromav = percentUnemployed - mean(percentUnemployed, na.rm=T))#already is ppt, so this should be fine?

#Did this above, but putting here to figure out how I've got opposite polarities... Oh, did I not re-run z-score after I got it backwards??
#Yup, that was it!
# ea <- ea %>% 
#   group_by(censusYear) %>% 
#   mutate(percentUnemployed_Zscore = (percentUnemployed - mean(percentUnemployed, na.rm=T))/sd(percentUnemployed, na.rm=T) )


#Then I need the various different decades next to each other for a decadal change...
ea_pptdiff_wide <- ea %>% 
  st_set_geometry(NULL) %>% 
  select(label,ttwa,censusYear,pc_unempl_pptdiff_fromav) %>% 
  spread(key = censusYear, value = pc_unempl_pptdiff_fromav)

#Need two diffs for plotting
ea_pptdiff_wide <- ea_pptdiff_wide %>% 
  mutate(
    diff71_81 = `1981`-`1971`,
    diff71_11 = `2011`-`1971`,
    diff81_11 = `2011`-`1981`
  )

#plot diffs, as per Venables paper
ggplot(ea_pptdiff_wide, aes(x = diff71_81, y = diff71_11)) +
  geom_point() +
  geom_smooth(method='lm')+
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')

ggplot(ea_pptdiff_wide, aes(x = diff71_81, y = diff81_11)) +
  geom_point() +
  geom_smooth(method='lm')+
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')


#Look at some TTWAs
plotppt_ttwa1 <- function(x){
  
  ggplot(ea_pptdiff_wide %>% filter(ttwa==x), aes(x = diff71_81, y = diff71_11)) +
    geom_point() +
    geom_smooth(method='lm') +
    geom_vline(xintercept = 0, colour = 'blue') +
    geom_hline(yintercept = 0, colour = 'blue')
  
}

plotppt_ttwa2 <- function(x){
  
  ggplot(ea_pptdiff_wide %>% filter(ttwa==x), aes(x = diff71_81, y = diff81_11)) +
    geom_point() +
    geom_smooth(method='lm') +
    geom_vline(xintercept = 0, colour = 'blue') +
    geom_hline(yintercept = 0, colour = 'blue')
  
}

plotppt_ttwa1('Sheffield & Rotherham')
plotppt_ttwa2('Sheffield & Rotherham')

plotppt_ttwa1('London')
plotppt_ttwa2('London')



ea_pptdiff_wide_geo <- left_join(
  ea %>% filter(censusYear==1971) %>% select(label),#just need labels for one arbitrary year
  ea_pptdiff_wide %>% select(label,diff71_81:diff81_11),
  by='label'
)

#Really annoying delete behaviour - no option to "delete if already exists" (or maybe there is, I can't find it)
st_write(ea_pptdiff_wide_geo, "qgis/ea_pptdiff_wide_geo.shp", delete_dsn = T)



#Rank position change might work better? Let's see
#1 is lowest unemployment (do we want that reversed?)
ea <- ea %>%
  group_by(censusYear) %>% 
  mutate(unempl_rank = rank(percentUnemployed))

ea_rank_wide <- ea %>% 
  st_set_geometry(NULL) %>% 
  select(label,ttwa,censusYear,unempl_rank) %>% 
  spread(key = censusYear, value = unempl_rank)

#Need two diffs for plotting
ea_rank_wide <- ea_rank_wide %>% 
  mutate(
    diff71_81 = `1981`-`1971`,
    diff71_11 = `2011`-`1971`,
    diff81_11 = `2011`-`1981`,
    diff81_91 = `1991`-`1981`,
    diff91_11 = `2011`-`1991`
  )

#plot diffs, as per Venables paper, using RANK this time
ggplot(ea_rank_wide, aes(x = diff71_81, y = diff71_11)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(ea_rank_wide, aes(x = diff71_81, y = diff81_11)) +
  geom_point() +
  geom_smooth(method='lm')


#Is this actually just something intrinsic to that comparison?
#How does 81-91 vs 81-11 look?
#Actually, more evidence of bounce back here... But yes, similar structure
ggplot(ea_rank_wide, aes(x = diff81_91, y = diff81_11)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(ea_rank_wide, aes(x = diff81_91, y = diff91_11)) +
  geom_point() +
  geom_smooth(method='lm')


#Now just for Sheffield TTWA 71-81
plot_ttwa1 <- function(x){

  ggplot(ea_rank_wide %>% filter(ttwa==x), aes(x = diff71_81, y = diff71_11)) +
    geom_point() +
    geom_smooth(method='lm') +
    geom_vline(xintercept = 0, colour = 'blue') +
    geom_hline(yintercept = 0, colour = 'blue')
  
}

plot_ttwa2 <- function(x){

  ggplot(ea_rank_wide %>% filter(ttwa==x), aes(x = diff71_81, y = diff81_11)) +
    geom_point() +
    geom_smooth(method='lm') +
    geom_vline(xintercept = 0, colour = 'blue') +
    geom_hline(yintercept = 0, colour = 'blue')

}

plot_ttwa1('Sheffield & Rotherham')
plot_ttwa2('Sheffield & Rotherham')

plot_ttwa1('London')
plot_ttwa2('London')

#OK, gonna have to see where the fook these rank changes have happened
#Sheffield mostly going up??
ea_rank_wide_geo <- left_join(
  ea %>% filter(censusYear==1971) %>% select(label),#just need labels for one arbitrary year
  ea_rank_wide %>% select(label,diff71_81:diff91_11),
  by='label'
)

st_write(ea_rank_wide_geo, "qgis/ea_rank_wide_geo.shp", delete_dsn = T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPARE MEASURES: PPT, Z SCORE ETC----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#While ppt and z-score will just be linear scale of each other, comparing won't be because ppt distances from the mean will differ / won't be standardised. If we're just after relative change in position of wards / LAs, whatever, not sure ppt works. Z-score has its own problems, but I think maybe ranking doing away with any sense of scale might not be great either.

#Z-score by Census decade... oh, we already found it above.
#Except, that was by TTWA wasn't it?
#Nope: per Census. Makes sense. Same here, we want to be comparing across GB.

#So let's just compare rank / ppt dist from mean / z score for a couple of places
#Err... they'll be exactly linear, right? It's only in the time comparison that issues will emerge
ggplot(ea %>% filter(ttwa=='Sheffield & Rotherham'), aes(x = pc_unempl_pptdiff_fromav, percentUnemployed_Zscore)) +
  geom_point() +
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')
  

#Oops. accidentally plotted every Census - but that does make the point:
#Z-score scale stays very close to identical across Censuses
#While ppt dist from mean varies between Censuses

#Z-score vs rank? Is effectively going to show a cumul dist function innit? Yup.
#But it does also show they don't vary across Censuses very much, same as Z-score.
#Which is what I think we want.
ggplot(ea %>% filter(ttwa=='Sheffield & Rotherham'), aes(x = unempl_rank, percentUnemployed_Zscore)) +
  geom_point()


#Z-score diffs:
ea_z_wide <- ea %>% 
  st_set_geometry(NULL) %>% 
  select(label,ttwa,censusYear,percentUnemployed_Zscore) %>% 
  spread(key = censusYear, value = percentUnemployed_Zscore)

#Need two diffs for plotting
ea_z_wide <- ea_z_wide %>% 
  mutate(
    diff71_81 = `1981`-`1971`,
    diff71_11 = `2011`-`1971`,
    diff81_11 = `2011`-`1981`,
    diff81_91 = `1991`-`1981`,
    diff91_11 = `2011`-`1991`
  )

#Now I'm guessing we'll see some quite large differences between diffs
#for ppt vs z-score
#(Which will require combining two dfs / renaming...)
ea_z_and_ppt_wide <- ea_z_wide %>% 
  select(label,ttwa,
         zdiff71_81 = diff71_81,
         zdiff71_11 = diff71_11,
         zdiff81_11 = diff81_11,
         zdiff81_91 = diff81_91,
         zdiff91_11 = diff91_11
         ) %>% 
  left_join(ea_pptdiff_wide %>% 
              select(label,
                     pptdiff71_81 = diff71_81,
                     pptdiff71_11 = diff71_11,
                     pptdiff81_11 = diff81_11
                     ),
            by = 'label'
            )


#compare:
#OK, so yeah: many many are not the same polarity
#i.e. often, z-score is showing a drop between Censuses where ppt shows a rise
ggplot(ea_z_and_ppt_wide %>% filter(ttwa=='London'), aes(x = zdiff71_81, y = pptdiff71_81)) +
  geom_point()  +
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')
ggplot(ea_z_and_ppt_wide %>% filter(ttwa=='Sheffield & Rotherham'), aes(x = zdiff71_81, y = pptdiff71_81)) +
  geom_point()  +
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')


#All wards
ggplot(ea_z_wide, aes(x = diff71_81, y = diff71_11)) +
  geom_point() +
  geom_smooth(method='lm') +
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')

#cf. ppt
ggplot(ea_pptdiff_wide, aes(x = diff71_81, y = diff71_11)) +
  geom_point() +
  geom_smooth(method='lm') +
  geom_vline(xintercept = 0, colour = 'blue') +
  geom_hline(yintercept = 0, colour = 'blue')


#So let's check z-score diff plots
#Now just for Sheffield TTWA 71-81
zplot_ttwa1 <- function(x){
  
  ggplot(ea_z_wide %>% filter(ttwa==x), aes(x = diff71_81, y = diff71_11)) +
    geom_point() +
    geom_smooth(method='lm') +
    geom_vline(xintercept = 0, colour = 'blue') +
    geom_hline(yintercept = 0, colour = 'blue')
  
}

zplot_ttwa2 <- function(x){
  
  ggplot(ea_z_wide %>% filter(ttwa==x), aes(x = diff71_81, y = diff81_11)) +
    geom_point() +
    geom_smooth(method='lm') +
    geom_vline(xintercept = 0, colour = 'blue') +
    geom_hline(yintercept = 0, colour = 'blue')
  
}

zplot_ttwa1('Sheffield & Rotherham')
zplot_ttwa2('Sheffield & Rotherham')

zplot_ttwa1('London')
plotppt_ttwa1('London')
zplot_ttwa2('London')

#Let's check out the geography of the z-plots...
ea_z_wide_geo <- left_join(
  ea %>% filter(censusYear==1971) %>% select(label),#just need labels for one arbitrary year
  ea_z_wide %>% select(label,diff71_81:diff91_11),
  by='label'
)

#st_write(ea_z_wide_geo, "qgis/ea_z_wide_geo.shp")
st_write(ea_z_wide_geo, "qgis/ea_z_wide_geo.shp", delete_dsn = T)



#~~~~~~~~~~~~~~~~~~~~~~~~
#STARING AT SHEFFIELD----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Actually looking at numbers
ttwa <- ea %>% filter(ttwa=='Sheffield & Rotherham')
ttwa <- ea %>% filter(ttwa=='London')

#overlay Sheffield on top of all
ttwa2 <- ea

ttwa_wide_percentunemp_shef <- ttwa %>% 
  st_set_geometry(NULL) %>% 
  select(label,censusYear,percentUnemployed) %>% 
  spread(key = censusYear, value = percentUnemployed) %>% 
  mutate(type = "Sheffield")

ttwa_wide_percentunemp_all <- ttwa2 %>% 
  st_set_geometry(NULL) %>% 
  select(label,censusYear,percentUnemployed) %>% 
  spread(key = censusYear, value = percentUnemployed) %>% 
  mutate(type = "all wards")


#join both
ttwa_wide_percentunemp <- rbind(ttwa_wide_percentunemp_shef,ttwa_wide_percentunemp_all)

# ttwa_wide_percentunemp$type <- factor(ttwa_wide_percentunemp$type, levels = c("all wards"))

#Too crude to see
#pairs(ttwa_wide_percentunemp %>% select(-label))

ggplot(ttwa_wide_percentunemp, aes(x = `1971`, y = `1981`, colour = type, shape = type, size = type)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  scale_color_manual(values=c("black", "red"))

ggplot(ttwa_wide_percentunemp, aes(x = `1971`, y = `2011`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

#So totally did bounce back. Of course.
ggplot(ttwa_wide_percentunemp, aes(x = `1981`, y = `2011`)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



#Overlaying sheffield on all (doing this way to control order)
#Like these!
ggplot() +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="all wards"), aes(x = `1971`, y = `1981`), alpha = 0.5) +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="Sheffield"), aes(x = `1971`, y = `1981`), colour="red", size = 2, shape = 17) +
  geom_abline(slope = 1, intercept = 0, colour='blue') 

ggplot() +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="all wards"), aes(x = `1981`, y = `1991`), alpha = 0.5) +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="Sheffield"), aes(x = `1981`, y = `1991`), colour="red", size = 2, shape = 17) +
  geom_abline(slope = 1, intercept = 0, colour='blue') 

ggplot() +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="all wards"), aes(x = `1971`, y = `2011`), alpha = 0.5) +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="Sheffield"), aes(x = `1971`, y = `2011`), colour="red", size = 2, shape = 17) +
  geom_abline(slope = 1, intercept = 0) 

ggplot() +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="all wards"), aes(x = `1981`, y = `2011`), alpha = 0.5) +
  geom_point(data = ttwa_wide_percentunemp %>% filter(type=="Sheffield"), aes(x = `1981`, y = `2011`), colour="red", size = 2, shape = 17) +
  geom_abline(slope = 1, intercept = 0) 






#But the paper's point is about equilibriating across the country, hence the whole-country comparison.
#And abstracting from decadal details. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#VARIOUS COMPARISONS OF PPT Z AND RANK, DISTRIBUTIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#diff from av: different spread across Censuses
ggplot(ea, aes(x = pc_unempl_pptdiff_fromav , y = factor(censusYear))) +
  geom_point(size = 0.75, alpha = 0.3, colour = 'blue') +
  geom_violin(fill = 'black') +
  geom_vline(xintercept = 0, colour = 'green') 

#Z score will have about the same range but not nec same shape?
ggplot(ea, aes(x = percentUnemployed_Zscore, y = factor(censusYear))) +
  geom_point(size = 0.75, alpha = 0.3, colour = 'blue') +
  geom_violin(fill = 'black') +
  geom_vline(xintercept = 0, colour = 'green') 

#And rank is just rank



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SEEING IF CAN USE AGE DATA FOR 16-64 DENOM FOR EMPLOYMENT COUNT----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Which will involve remembering where the age data was stored.
# Location of those newly harmonised vars:
# C:\Users\admin\Dropbox\SheffieldMethodsInstitute\Census_dx\StitchOutputs\GreatBritain\LBS_postcodeSectorWard_5Census_csvs
# Created in this script:
# newVarHarmonising2020.R

#Let's get one decade of age and econ active and see...
age71 <- read_csv("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971age.csv")

#Is that the range we got from the others? Yup. Starts at 15 not 16. Maybe that's OK.
#age91 <- read_csv("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1991age.csv")

ea71 <- read_csv("StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/1971econActive.csv")

#Hmm. Orig 1971 data doesn't actually contain "unemployed". It's got working, seeking work, sick.
#I think I probably made econactive working + seeking work, didn't I?
#Yes: englandWalesDataWrangling.R line 1867
#And yes - the number working is then just the inverse, econactive - unemployed ("seeking work")
#I think the same applies to all Census decades

#Which means I should be able to use total 15-64 age pop as denom
#Note: people over the age of 64 can be working, so it's less than ideal in my opinion, but hey

#1. Convert all EA to employed number, which is just inverse of unemployed / econ active for all (or should be!)
#Using previously combined version above...
ea <- ea %>% 
  mutate(employed = econActive - unemployed)


#Now to get employment rate compared to all 16-64 year olds (but has to be 15 to 64 year olds here)
#Will need age, with censusYear added, to join
age <- list.files(path = "StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs", pattern = "*age.csv", full.names = T) %>% 
  lapply(read_csv) %>% 
  bind_rows()

#Add censusYear
#Order should be correct, in order of files
age$censusYear <- rep(seq(from = 1971, to = 2011, by = 10), each = nrow(age)/5)

#Check worked - should be more 75+ every decade
#Yup - wow, that's a LOT more! Number of over 75s doubled?
age %>% 
  select(censusYear,X75.) %>% 
  group_by(censusYear) %>% 
  summarise(mean(X75., na.rm=T))

#Sum ages 15-64
age <- age %>% 
  mutate(`15 to 64` = X15.19+X20.24+X25.29+X30.34+X35.39+X40.44+X45.49+X50.54+X55.59+X60.64)

#Merge in key columns to EA
ea <- ea %>% 
  left_join(
    age %>% select(label,censusYear,`15 to 64`),
    by = c("label","censusYear")
    )

#Erm... oh actually, this does make sense. Actual number of 16-64 is going to be larger than econ active isn't it?
#(Though econ active can include people still working past 64 I believe...)
ggplot(
  ea %>% st_set_geometry(NULL) %>% filter(censusYear == 1981) %>% ungroup() %>% select(econActive,`15 to 64`) %>% gather(key = varname, value = count), 
  aes(x = count, colour = varname)
  ) +
  geom_boxplot()


#This might be more of a problem... Ah no, they mostly behave
ggplot(
  ea %>% st_set_geometry(NULL) %>% filter(censusYear == 1981) %>% ungroup() %>% select(employed,`15 to 64`) %>% gather(key = varname, value = count), 
  aes(x = count, colour = varname)
  ) +
  geom_boxplot() 

#Think that tiny number is probably possible in some London wards... actually, fairly spread out
#All but one are in 1971. Only other is 1981.
table(ea$employed > ea$`15 to 64`)
chk <- ea %>% filter(employed > `15 to 64`, censusYear == 1971)
plot(st_geometry(chk))


#Let's press on as if these are OK for now?
#So... employment rate, as proportion of 15 to 64 year olds
ea <- ea %>% 
  mutate(employmentrate15to64 = (employed / `15 to 64`) * 100)

#I don't trust this at all.
ggplot(
  ea %>% st_set_geometry(NULL) %>% ungroup() %>% select(censusYear,employmentrate15to64),aes(x = employmentrate15to64, y = factor(censusYear))) +
  geom_violin() +
  geom_vline(xintercept = 71)


#Compare to employed % diff of EA... yeah, this is fine
ggplot(
  ea %>% st_set_geometry(NULL) %>% ungroup() %>% select(censusYear,percentEmp),aes(x = percentEmp, y = factor(censusYear))) +
  geom_violin() +
  geom_vline(xintercept = mean(ea$percentEmp[ea$censusYear==1971],na.rm=T))



#Going back and looking at EA1971 numbers, age breakdown.
#Am also just trying to think if differing zeroes and numbers when the re-assignment got done might be the problem
#That could explain it - if there's a reason differing variables don't perfectly match when they're reassigned to wards?
#Trying to visualise - it seems to me the fractions *should* match, but I may be missing something.

#Anyway, checking age...
#Here's one I just made at country level with all vars
orig_ea71 <- read_csv('C:/Users/admin/Dropbox/SheffieldMethodsInstitute/Census_dx/1971/GreatBritain/1971_econactive_allvars_countrylevel.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MAKING DATA READY FOR REGRESSIONS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#I want to use both the percent rate diff from the original "employed as prop of econ active" as well as this new measure

#Label each, redo
ea <- ea %>%
  group_by(censusYear) %>% 
  mutate(ECONACTIVE_pc_empl_pptdiff_fromav = percentEmp - mean(percentEmp, na.rm=T))#already is ppt, so this should be fine?

#Then I need the various different decades next to each other for a decadal change...
ECONACTIVE_ea_pptdiff_wide <- ea %>% 
  st_set_geometry(NULL) %>% 
  select(label,ttwa,censusYear,ECONACTIVE_pc_empl_pptdiff_fromav) %>% 
  spread(key = censusYear, value = ECONACTIVE_pc_empl_pptdiff_fromav)

#Get diffs, all diffs in Venables regressions table 1 
ECONACTIVE_ea_pptdiff_wide <- ECONACTIVE_ea_pptdiff_wide %>% 
  mutate(
    EAdiff71_11 = `2011`-`1971`,
    EAdiff81_11 = `2011`-`1981`,
    EAdiff71_81 = `1981`-`1971`,
    EAdiff81_91 = `1991`-`1981`,
    EAdiff91_01 = `2001`-`1991`,
    EAdiff01_11 = `2011`-`2001`,
    EAdiff91_11 = `2011`-`1991`
  )


#Repeat for 15-64s as denom
ea <- ea %>%
  group_by(censusYear) %>% 
  mutate(AGEDENOM_pc_empl_pptdiff_fromav = employmentrate15to64 - mean(employmentrate15to64, na.rm=T))#already is ppt, so this should be fine?

#Err can I please see those?
# ggplot(ea, aes(x = AGEDENOM_pc_empl_pptdiff_fromav, y = factor(censusYear))) +
#   geom_violin()
# 
# #cf...
# ggplot(ea, aes(x = ECONACTIVE_pc_empl_pptdiff_fromav, y = factor(censusYear))) +
#   geom_violin()

#Press on
AGEDENOM_ea_pptdiff_wide <- ea %>% 
  st_set_geometry(NULL) %>% 
  select(label,ttwa,censusYear,AGEDENOM_pc_empl_pptdiff_fromav) %>% 
  spread(key = censusYear, value = AGEDENOM_pc_empl_pptdiff_fromav)

#Get diffs, all diffs in Venables regressions table 1 
AGEDENOM_ea_pptdiff_wide <- AGEDENOM_ea_pptdiff_wide %>% 
  mutate(
    AGEDENOMdiff71_11 = `2011`-`1971`,
    AGEDENOMdiff81_11 = `2011`-`1981`,
    AGEDENOMdiff71_81 = `1981`-`1971`,
    AGEDENOMdiff81_91 = `1991`-`1981`,
    AGEDENOMdiff91_01 = `2001`-`1991`,
    AGEDENOMdiff01_11 = `2011`-`2001`,
    AGEDENOMdiff91_11 = `2011`-`1991`
  )


#Join dem two, add in migration, that should be all of our regression vars
regressionvars <- AGEDENOM_ea_pptdiff_wide %>% 
  select(label,ttwa,AGEDENOMdiff71_11:AGEDENOMdiff91_11) %>% 
  left_join(
    ECONACTIVE_ea_pptdiff_wide %>% 
      select(label,EAdiff71_11:EAdiff91_11),
    by = "label"
  )

#And European-born numbers in 1971. As proportion of total pop.
#Ah ha, zone props already made, nice.
cob <- read_csv('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/71zoneProportions.csv')

#Check against both Europe and non-UK props
regressionvars <- regressionvars %>% 
  left_join(
    cob %>% select(label = `x$label`, Europe_percent = Europe, nonUK_percent71 = `nonUK.incIre.ZonePerc`),
    by = "label"
    )


#~~~~~~~~~~~~~~~~~~~~~
#DOING REGRESSIONS----
#~~~~~~~~~~~~~~~~~~~~~

#Save that version of ea, it's a bit of a faff compiling it
saveRDS(ea,"R_data/employment5Censusvars.rds")

#Same ones as table 1 in venables paper (plus migration interaction, see meen notes Gwilym meeting 5.4.22)
#Added in 81-11 as it's part of the diagrams in the paper and is a key point (odd not regressed in paper?)

#Explanatory side is staying the same regardless, just changing the outcome.
#Running whole set twice to see what diff ECONACTIVE vs age-denominator makes.

#Quickest way to do? Check on one, work out pipeline
#(Will be adding deprivation later but some geo-faff to be done)
agedenom <- lm(AGEDENOMdiff71_11 ~ AGEDENOMdiff71_81 * nonUK_percent71, data = regressionvars)
summary(agedenom)

#Attempt at interaction interp:
#Shows how much the diff71-81 coeff changes for a 1% change in nonUKpercent
#So, its goes UP by 0.016 for a 1% inc in non UK born in 1971

#Can I see plz?
plot(regressionvars$AGEDENOMdiff71_11 ~ regressionvars$AGEDENOMdiff71_81)
plot(regressionvars$AGEDENOMdiff81_11 ~ regressionvars$AGEDENOMdiff81_91)
plot(regressionvars$AGEDENOMdiff71_11 ~ regressionvars$nonUK_percent71)


#Actually, let's stick to EA for now - those 1971 weird numbers are going to warp things
#Note impact of nonUK = larger
ealm <- lm(EAdiff71_11 ~ EAdiff71_81 * nonUK_percent71, data = regressionvars)
summary(ealm)

plot(regressionvars$EAdiff71_11 ~ regressionvars$nonUK_percent71)
abline(lm(EAdiff71_11 ~ nonUK_percent71, data = regressionvars))


#**** THESE ARE ALL NOW OUTPUT IN scarring_regressions.Rmd ****

#Let's get some stargazer tables out.
#For all wards then just Sheffield.
#Probably just as easy to do manually, though need some var name changes.
#AGEDENOM first.
library(stargazer)

agedenomdata <- regressionvars %>% 
  select(label,ttwa,Eur_pc71 = Europe_percent, nonUK_pc71 = nonUK_percent71,`71-11`=AGEDENOMdiff71_11,`81-11`=AGEDENOMdiff81_11,`71-81`=AGEDENOMdiff71_81,
         `81-91`=AGEDENOMdiff81_91,`91-01`=AGEDENOMdiff91_01,`01-11`=AGEDENOMdiff01_11)

#Save for RMarkdown output
saveRDS(agedenomdata,"R_data/agedenomdata.rds")

agedenom1 <- lm(`71-11` ~ `71-81` * nonUK_pc71, data = agedenomdata)
agedenom2 <- lm(`81-11` ~ `71-81` * nonUK_pc71, data = agedenomdata)
agedenom3 <- lm(`81-91` ~ `71-81` * nonUK_pc71, data = agedenomdata)
agedenom4 <- lm(`91-01` ~ `71-81` * nonUK_pc71, data = agedenomdata)
agedenom5 <- lm(`01-11` ~ `71-81` * nonUK_pc71, data = agedenomdata)

stargazer(agedenom1,agedenom2,agedenom3,agedenom4,agedenom5, title="Denom: 15-64", align=TRUE, type = "html")

eadata <- regressionvars %>% 
  select(label,ttwa,Eur_pc71 = Europe_percent, nonUK_pc71 = nonUK_percent71,`71-11`=EAdiff71_11,`81-11`=EAdiff81_11,`71-81`=EAdiff71_81,
         `81-91`=EAdiff81_91,`91-01`=EAdiff91_01,`01-11`=EAdiff01_11)

#Save for RMarkdown output
saveRDS(eadata,"R_data/eadata.rds")

ea1 <- lm(`71-11` ~ `71-81` * nonUK_pc71, data = eadata)
ea2 <- lm(`81-11` ~ `71-81` * nonUK_pc71, data = eadata)
ea3 <- lm(`81-91` ~ `71-81` * nonUK_pc71, data = eadata)
ea4 <- lm(`91-01` ~ `71-81` * nonUK_pc71, data = eadata)
ea5 <- lm(`01-11` ~ `71-81` * nonUK_pc71, data = eadata)

stargazer(ea1,ea2,ea3,ea4,ea5, title="Denom: 15-64", align=TRUE, type = "html")


#Plot sheffield
plot(regressionvars$AGEDENOMdiff71_11[regressionvars$ttwa=="Sheffield & Rotherham"] ~ regressionvars$AGEDENOMdiff71_81[regressionvars$ttwa=="Sheffield & Rotherham"])
plot(regressionvars$AGEDENOMdiff71_11[regressionvars$ttwa=="Sheffield & Rotherham"] ~ regressionvars$nonUK_percent71[regressionvars$ttwa=="Sheffield & Rotherham"])

plot(regressionvars$EAdiff71_11[regressionvars$ttwa=="Sheffield & Rotherham"] ~ regressionvars$EAdiff71_81[regressionvars$ttwa=="Sheffield & Rotherham"])
plot(regressionvars$EAdiff71_11[regressionvars$ttwa=="Sheffield & Rotherham"] ~ regressionvars$nonUK_percent71[regressionvars$ttwa=="Sheffield & Rotherham"])



#Thing I also want to check:
#You get the same "bounceback" values from comparing e.g. 81-91 and 91-11, don't you?
#Yup
summary(lm(AGEDENOMdiff81_11 ~ AGEDENOMdiff81_91 * nonUK_percent71, data = regressionvars))
summary(lm(AGEDENOMdiff91_11 ~ AGEDENOMdiff81_91 * nonUK_percent71, data = regressionvars))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#EA DENOM SCARRING PLOTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#save eadata for writeup
saveRDS(eadata,"R_data/eadata.rds")

#Sheffield overlaid?
ggplot(eadata, aes(x = `71-81`, y = `71-11`)) +
  geom_point(alpha=0.2, size = 2) +
  geom_point(data = eadata %>% filter(ttwa=="Sheffield & Rotherham"), colour = 'red')
  
#Hmm. maybe not overlaid... or with London, for comparison
ggplot(eadata %>% filter(ttwa=='London'), aes(x = `71-81`, y = `71-11`)) +
  geom_point(alpha=0.2, size = 2) +
  geom_point(data = eadata %>% filter(ttwa=="Sheffield & Rotherham"), colour = 'red', size = 3)

ggplot(eadata %>% filter(ttwa=='London'), aes(x = `71-81`, y = `81-11`)) +
  geom_point(alpha=0.2, size = 2) +
  geom_point(data = eadata %>% filter(ttwa=="Sheffield & Rotherham"), colour = 'red', size = 3)


#JUST SHEFFIELD
library(cowplot)

p1 <- ggplot(eadata %>% filter(ttwa=='Sheffield & Rotherham'), aes(x = `71-81`, y = `71-11`)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, colour='blue') +
  geom_hline(yintercept = 0, colour='blue')

p2 <- ggplot(eadata %>% filter(ttwa=='Sheffield & Rotherham'), aes(x = `71-81`, y = `81-11`)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, colour='blue') +
  geom_hline(yintercept = 0, colour='blue')

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)



#Animation of changes in employment rate across Censuses
#Just for Sheffield, so make long..
shef <- eadata %>% 
  filter(ttwa=="Sheffield & Rotherham") %>% 
  select(label,`71-81`:`01-11`) %>% 
  gather(key = censusPeriod, value = pptchange_emp, `71-81`:`01-11`)

#Need the geography
table(shef$label %in% wards$zone)

shef <- wards %>% 
  select(zone) %>% 
  right_join(
  shef,
  by = c('zone'='label')
)


map <- tm_shape(shef) +
  tm_polygons(col = 'pptchange_emp', style = 'jenks', n = 21, palette = 'plasma') +
  tm_facets(by = 'censusPeriod', ncol = 1, nrow = 1) 

tmap_animation(map, filename = 'R_outputs/Scarring/sheffield_employmentchange_betweenCensusperiods.gif', width = 800, height = 600, fps = 0.5)




#REDOING DIRECT DECADE COMPARISONS FOR PERCENT EMPLOYED
ea_wide <- ea %>%
  st_set_geometry(NULL) %>% 
  select(label,censusYear, ttwa, percentEmp) %>% 
  spread(key = censusYear, value = percentEmp)

#Save for writeup
saveRDS(ea_wide, "R_data/ea_wide.rds")

p1 <- ggplot() +
  geom_point(data = ea_wide %>% filter(ttwa!="Sheffield & Rotherham"), aes(x = `1971`, y = `1981`), alpha = 0.5) +
  geom_point(data = ea_wide %>% filter(ttwa=="Sheffield & Rotherham"), aes(x = `1971`, y = `1981`), colour="red", size = 2, shape = 17) +
  geom_abline(slope = 1, intercept = 0, colour='blue') 

p2 <- ggplot() +
  geom_point(data = ea_wide %>% filter(ttwa!="Sheffield & Rotherham"), aes(x = `1981`, y = `1991`), alpha = 0.5) +
  geom_point(data = ea_wide %>% filter(ttwa=="Sheffield & Rotherham"), aes(x = `1981`, y = `1991`), colour="red", size = 2, shape = 17) +
  geom_abline(slope = 1, intercept = 0, colour='blue') 

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)

# ggplot() +
#   geom_point(data = ea %>% filter(type=="all wards"), aes(x = `1971`, y = `2011`), alpha = 0.5) +
#   geom_point(data = ea %>% filter(type=="Sheffield"), aes(x = `1971`, y = `2011`), colour="red", size = 2, shape = 17) +
#   geom_abline(slope = 1, intercept = 0) 
# 
# ggplot() +
#   geom_point(data = ea %>% filter(type=="all wards"), aes(x = `1981`, y = `2011`), alpha = 0.5) +
#   geom_point(data = ea %>% filter(type=="Sheffield"), aes(x = `1981`, y = `2011`), colour="red", size = 2, shape = 17) +
#   geom_abline(slope = 1, intercept = 0) 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECKING ON USING RANK INSTEAD OF EA OR AGE DENOM----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Have already played with this above. Has issues, but feel like it's an approach worth testing.
#First up - how close are EA and age denom approaches in terms of rank?

#Need to remove NAs, harrumph
cordata <- data.frame(
  ea = eadata$`71-11`,agedenom = agedenomdata$`71-11`
)

cordata <- cordata[!is.na(cordata$ea),]

#Not hugely close, order wise. Interesting.
cor(rank(cordata$ea),rank(cordata$agedenom), method = 'spearman')
#Is same, of course
cor(cordata$ea,cordata$agedenom, method = 'spearman')


#So - which would I choose to rank, is the next question?
#Let's look at each.
#Adding rank to the two data types
agedenomdata <- agedenomdata %>% 
  mutate(across(`71-11`:`01-11`, ~rank(.),  .names = "rank_{.col}"))

eadata <- eadata %>% 
  mutate(across(`71-11`:`01-11`, ~rank(.),  .names = "rank_{.col}"))

#Correlation comparison for age denom vs econ active
plot(agedenomdata$`71-11`[agedenomdata$ttwa=="Sheffield & Rotherham"] ~ agedenomdata$`71-81`[agedenomdata$ttwa=="Sheffield & Rotherham"])
plot(eadata$`71-11`[eadata$ttwa=="Sheffield & Rotherham"] ~ eadata$`71-81`[eadata$ttwa=="Sheffield & Rotherham"])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPARING TO 2019 IMD (POP WEIGHTED AV FOR OLNER WARDS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Is attached this wards shapefile, I only want the IMD rank values to join
wards <- st_read('QGIS/popweightedwardIMD.shp')

agedenomdata <- agedenomdata %>% left_join(
  wards %>% st_set_geometry(NULL) %>% select(zone,IMDrnk_),
  by = c('label'='zone')
)

eadata <- eadata %>% left_join(
  wards %>% st_set_geometry(NULL) %>% select(zone,IMDrnk_),
  by = c('label'='zone')
)


#Recreate some version of the Venables/Rice graph on p.145
#Ah good, it actually needs the employment rate for 2011
#Note: employment's one of the IMD domains. Hmmph.
#Can't we just see the direct correlation of 71-81 shock to IMD now?
ggplot(agedenomdata,aes(x = `71-81`,y=IMDrnk_)) +
  geom_point()

#Again, can't help but feel this is just telling us: those are urban areas.
#Check on some purely urban areas, esp London and Sheffield
ggplot(agedenomdata %>% filter(ttwa %in% c('London','Sheffield & Rotherham')),aes(x = `71-81`,y=IMDrnk_)) +
  geom_point() +
  facet_wrap(~ttwa)

#Check with ea data... that's a lot healthier looking
ggplot(eadata,aes(x = `71-81`,y=IMDrnk_)) +
  geom_point()

ggplot(eadata %>% filter(ttwa %in% c('London','Sheffield & Rotherham')),aes(x = `71-81`,y=IMDrnk_)) +
  geom_point() +
  facet_wrap(~ttwa, nrow=2)


#Sheffield ward IMD?
tm_shape(wards %>% filter(ttwa=='Sheffield & Rotherham')) +
  tm_polygons(col = 'IMDrnk_', style = 'fisher', n = 10, palette = 'plasma') +
  tm_layout(legend.outside = TRUE)



#Look at rank vs IMD - blech, let's not. Anyway, we're using econ active as denom
# ggplot(agedenomdata %>% filter(ttwa %in% c('London','Sheffield & Rotherham')),aes(x = `rank_71-11`,y=IMDrnk_)) +
#   geom_point() +
#   facet_wrap(~ttwa)
# 
# ggplot(eadata %>% filter(ttwa %in% c('London','Sheffield & Rotherham')),aes(x = `rank_71-11`,y=IMDrnk_)) +
#   geom_point() +
#   facet_wrap(~ttwa)


#RECREATE VENABLES IMD PLOT
#What do we need?
#y axis: IMD
#x axis: 2011 employment rate
#colour groups: 1st and 2nd deciles of employment rate changes 71-81
eadata <- eadata %>% 
  mutate(across(`71-11`:`01-11`, ~as.numeric(cut_number(.,10)),  .names = "deciles_{.col}"))

#And need to add in 2011 employment rate
#1st decile is biggest drop in employment
eadata <- eadata %>% 
  left_join(
    ea %>% st_set_geometry(NULL) %>% filter(censusYear==2011) %>% 
      select(label,percentEmp2011 = percentEmp),
    by = 'label'
  )

#Label deciles for 71-81 so we have three groups for viz:
#1st,2nd, rest
eadata <- eadata %>% 
  mutate(`deciles_71-81_3levels` = fct_collapse(factor(.$`deciles_71-81`), `1`="1",`2`="2",`3-10` = c("3","4","5","6","7","8","9","10")))


#71-81 shock wards - firmly more deprived even if employment in 2011 actually quite high
ggplot(eadata %>% filter(ttwa %in% c('London','Sheffield & Rotherham')), aes(x = percentEmp2011, y = IMDrnk_, colour = `deciles_71-81_3levels`, shape = `deciles_71-81_3levels`,
                                                                             size = `deciles_71-81_3levels`, alpha = `deciles_71-81_3levels`)) +
  geom_point() +
  facet_wrap(~ttwa, nrow=2) +
  scale_size_manual(values = c(2,2,1)) +
  scale_alpha_manual(values = c(1,1,0.6))




#Repeat for whole of GB
ggplot(eadata %>% filter(!is.na(`deciles_71-81_3levels`)), aes(x = percentEmp2011, y = IMDrnk_, colour = `deciles_71-81_3levels`, shape = `deciles_71-81_3levels`,
                                                                             size = `deciles_71-81_3levels`, alpha = `deciles_71-81_3levels`)) +
  geom_point() +
  scale_size_manual(values = c(2,2,1)) +
  scale_alpha_manual(values = c(1,1,0.6))

ggsave('R_outputs/Scarring/IMD_GB_shockdeciles.png', width = 11, height = 7)


#Look at each decile separately, against IMD rank
ggplot(eadata %>% filter(!is.na(`deciles_71-81_3levels`)), 
       aes(x = factor(`deciles_71-81`), y = IMDrnk_)) +
  geom_violin() +
  coord_flip()

ggplot(eadata %>% filter(!is.na(`deciles_71-81_3levels`)), 
       aes(x = factor(`deciles_71-81`), y = IMDrnk_)) +
  geom_jitter() +
  coord_flip()

#I would like to just look at urban wards for that, to separate that issue out. Can test with London.
#Still overall same.
ggplot(eadata %>% filter(!is.na(`deciles_71-81_3levels`), ttwa=='London'), 
       aes(x = factor(`deciles_71-81`), y = IMDrnk_)) +
  geom_violin() +
  coord_flip()




#How many more than average does sheffield have wards in the lower deciles?
#(National average is mathematically 10% of obs in each of course!)
#https://dplyr.tidyverse.org/reference/count.html
#"df %>% count(a, b) is roughly equivalent to df %>% group_by(a, b) %>% summarise(n = n())"
#Convert to factor first so dplyr can know what levels are missing for the zero count
ea_decileproportions <- eadata %>% 
  mutate(`deciles_71-81` = factor(`deciles_71-81`)) %>% 
  count(ttwa,`deciles_71-81`, .drop=F, name = "count") %>% 
  group_by(ttwa) %>% 
  mutate(tot = sum(count)) %>% #which is just the count of wards in the ttwa
  mutate(`decileprops_71-81` = (count/tot)*100)


#This calls for some barcharts!  
#Where's my list of top ttwas?``
#Sheffield relatively not hardest hit, in terms of wards in the most impacted decile.
ggplot(ea_decileproportions %>% filter(!is.na(`deciles_71-81`), ttwa %in% ttwa_order$ttwa[1:30]), aes(x = `deciles_71-81`, y = `decileprops_71-81`)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ttwa)



#I wonder if I can show exactly the same thing just by using population numbers / per km2 rather than 71-81 shock?
#That is: it's just that more urban / crowded areas are more deprived generally
#Cf. Paul Norman's work too... 
#I used 2019 pop estimates. Could do with just 1971 pop estimates really, see if they predict.
#Shapefile so we can get pop per km2
cob71 <- st_read('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_raw/CountryOfBirth/GB_1971_CountryOfBirthRecode_91LBSwards_noZeroPCS.shp')

#total pop
#https://stackoverflow.com/questions/28873057/sum-across-multiple-columns-with-dplyr
cob71 <- cob71 %>% 
  mutate(
    totalpop = rowSums(across(England:Rest_of_wo)),
    area = st_area(.),
    pop_per_km2_1971 = (totalpop/area)*1000000
  )

#Merge into eadata... those two falses are the NAs I've been getting anyway I think, won't make a diff to this plot/regression
table(eadata$label %in% cob71$label)

eadata <- eadata %>% 
  left_join(
    cob71 %>% st_set_geometry(NULL) %>% select(label,pop_per_km2_1971),
    by = 'label'
  )

#OK, check basic: 71-81 shock values vs pop... not so striking
plot(eadata$`71-81`~eadata$pop_per_km2_1971)
#A spread, but a clear pattern
plot(eadata$IMDrnk_~eadata$pop_per_km2_1971)


ggplot(eadata %>% filter(!is.na(`deciles_71-81_3levels`)), 
       aes(x = pop_per_km2_1971, y = IMDrnk_)) +
  geom_point(alpha=0.3) 


#And 71 populations vs shock decile ranks?
#Pop/urban clearly a factor, but this isn't as strong as the IMD pattern
#So is still interesting
#Need to mull what that means
ggplot(eadata %>% filter(!is.na(`deciles_71-81_3levels`), ttwa=='London'), 
       aes(x = factor(`deciles_71-81`), y = pop_per_km2_1971)) +
  geom_violin() +
  coord_flip()


#~~~~~~~~~~~~~~~~~~~~~~~~
#Dissolve TTWAs to GB (for scarring Sheffield map)----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Because QGIS is finding invalid geometries. Any better luck here?
ttwas <- st_read('C:/Data/MapPolygons/GreatBritain/2001/TTWAs/greatBritainTTWAs.shp')

ttwas.dissolve <- st_union(ttwas)

st_write(ttwas.dissolve, "QGIS/GB_dissolve.shp")




#~~~~~~~~~~~~~~~~~~~~~~~~
#Sheffield map!----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Deprivation + which wards had biggest shocks
#Repeat for London, I'd like to see that too

#wards is sf, has IMD rank. Need to add deciles 71-81
wardsformap <- wards %>% 
  rename(`IMD rank` = IMDrnk_) %>% 
  left_join(
    eadata %>% select(label,`deciles_71-81`),
    by=c('zone'='label')
  )


#Code from above.
x <- tm_shape(wardsformap %>% filter(ttwa=='Sheffield & Rotherham')) +
  tm_polygons(col = 'IMD rank', style = 'fisher', n = 6, palette = 'Blues') +#blues should work OK in B&W
tm_shape(wardsformap %>% filter(ttwa=='Sheffield & Rotherham', `deciles_71-81` == 1)) +
  tm_borders(col = 'black', lwd=5) +
tm_shape(wardsformap %>% filter(ttwa=='Sheffield & Rotherham', `deciles_71-81` == 2)) +
  tm_borders(col = 'black', lwd=4, lty=3) +
  tm_layout(legend.outside = TRUE)

tmap_save(x, 'R_outputs/Scarring/Sheffield_plus_IMDdecile1and2.png', width = 10, height  = 8)  

#View mode version
tmap_mode('view')

tm_shape(wardsformap %>% filter(ttwa=='Sheffield & Rotherham')) +
  tm_polygons(col = 'IMD rank', style = 'fisher', n = 6, palette = 'Blues', alpha=0.6) +#blues should work OK in B&W
  tm_shape(wardsformap %>% filter(ttwa=='Sheffield & Rotherham', `deciles_71-81` == 1)) +
  tm_borders(col = 'black', lwd=5) +
  tm_shape(wardsformap %>% filter(ttwa=='Sheffield & Rotherham', `deciles_71-81` == 2)) +
  tm_borders(col = 'black', lwd=4, lty=3) +
  tm_layout(legend.outside = TRUE) +
  tm_basemap(leaflet::providers$OpenStreetMap)


