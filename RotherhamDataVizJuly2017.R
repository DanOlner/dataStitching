#DATA VIZ FOR ROTHERHAM MEETUP JULY 2017
geolibs <- c("spdep","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster", "dplyr", "tidyr","assertthat","data.table",'readr','pryr','combinat','RColorBrewer')
lapply(geolibs, require, character.only = TRUE)

#~~~~~~~~~~~~~
#~~~~Data prep----
#~~~~~~~~~~~~~

#COB----

#Load 3 and 5 census CoB proportions
files3 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_3Census_csvs/CountryOfBirth/',pattern = '*CoB*', full.names = T)
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*totals*', full.names = T)

#Get them into the right order. Nearly gave me a heart attack...
files3 <- files3[c(3,1,2)]
files5 <- files5[c(3,4,5,1,2)]

threez <- lapply(files3,read_csv)
fivez <- lapply(files5,read_csv)

years <- seq(from = 1971, to = 2011, by = 10)

#So now it's already long. Nice sneezy.
for(x in 1:3) threez[[x]]$year <- years[(x+2)]
for(x in 1:5) fivez[[x]]$year <- years[x]

threez_cob <- do.call("rbind",threez)
fivez_cob <- do.call("rbind",fivez)

#Ah, I took two awkward zones out of the 5 census didn't I? That's fine, only working with Shef/Roth

#Reduce all data to 51 shef/roth zones.
#None have been pooled in that area (fnl_rsl all NA, no zone combos)
shefRoth <- readShapeSpatial('C:/Data/MapPolygons/GreatBritain/1991/SheffieldRotherham_GB_wards_pcs_agg4correctCount.shp')

rothOnly <- readShapeSpatial('C:/Data/MapPolygons/GreatBritain/1991/RotherhamOnly_GB_wards_pcs_agg4correctCount.shp')

#Getting ward names (can't do this for Scotland as they're PCSs.)
engwards <- readShapeSpatial('C:/Data/MapPolygons/England/1991/England_wa_1991/england_wa_1991.shp')

#Keep only 51 ward names we need
shefRoth <- engwards[engwards$label %in% shefRoth$label,]

#Reduce multiples with commas and 'ands'
shefRoth$name <- gsub(',','/',shefRoth$name)
shefRoth$name <- gsub(' and ','/',shefRoth$name)

#Reduce data to shef and rotherham zones
threez_cobShefRoth <- threez_cob[threez_cob$label %in% shefRoth$label,]
fivez_cobShefRoth <- fivez_cob[fivez_cob$label %in% shefRoth$label,]


#Employment----

#Just need 5 census, no diff over the two periods
files5 <- list.files('StitchOutputs/GreatBritain/LBS_postcodeSectorWard_5Census_csvs/',pattern = '*Active*', full.names = T)

#In right order...
fivez <- lapply(files5,read_csv)

years <- seq(from = 1971, to = 2011, by = 10)

#So now it's already long. Nice sneezy.
for(x in 1:5) fivez[[x]]$year <- years[x]

fivez_emp <- do.call("rbind",fivez)

fivez_empShefRoth <- fivez_emp[fivez_emp$label %in% shefRoth$label,]


#So keeping the whole of GB in there for comparisons relative to everywhere else.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~Viz----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Rank of employment, layer change over decades
fivez_empShefRoth <- fivez_empShefRoth %>% 
  group_by(year) %>% 
  mutate(rank = rank(percentEmployed,ties.method = 'random'))

#Factor up ward names to match order of rank for the decade we want (so compare to 1971)
wardnames <- merge(data.frame(shefRoth),
                   fivez_empShefRoth[fivez_empShefRoth$year==1971,c('rank','label')], by = 'label')

wardnames$name <- factor(wardnames$name)
wardnames$name <- reorder(wardnames$name,wardnames$rank)

fivez_empShefRoth <- merge(fivez_empShefRoth, wardnames[,c('label','name')], by = 'label')


output <- ggplot(fivez_empShefRoth, aes(x = name, y = rank, colour = factor(year))) + 
  geom_point() +
  theme(axis.text.x=element_text(angle=270,hjust=0, vjust = 0.2))

output

#No, rubbish. Let's just do the ol' boxplots
output <- ggplot(fivez_empShefRoth, aes(x = year, y = percentEmployed, group = year)) + 
  geom_boxplot()

output


#ranks might work if I took out from GB ranks. Issue of comparability...
#So rank GB emp data before subsetting. Just to see.
fivez_emp <- fivez_emp %>% 
  group_by(year) %>% 
  mutate(rank = rank(percentEmployed,ties.method = 'random'))

rankz <- fivez_emp[fivez_emp$label %in% shefRoth$label,]


#Where one is most unemployment
output <- ggplot(rankz, aes(x = year, y = rank, group = year)) + 
  geom_boxplot()

output

output <- ggplot(rankz, aes(x = year, y = rank, group = year)) + 
  geom_point()

output

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Graph change over years----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Top and bottom five, on average
topAndBottomX <- fivez_empShefRoth %>% group_by(label) %>% 
  summarise(avs = mean(percentEmployed)) %>% 
  mutate(rank = rank(avs)) %>% 
  filter(rank < 4 | rank > (max(rank)-3))

#order name factor by rank (already done above)
fivez_empShefRoth$name <- factor(fivez_empShefRoth$name)
fivez_empShefRoth$name <- reorder(fivez_empShefRoth$name,-fivez_empShefRoth$rank)

output <- ggplot() +
  geom_line(data = fivez_empShefRoth, 
            aes(x = factor(year), y = percentEmployed, group = label),
            alpha = 0.1, size = 0.25) +
  geom_line(data = fivez_empShefRoth[fivez_empShefRoth$label %in% topAndBottomX$label,], 
            aes(x = factor(year), y = percentEmployed, group = name, colour = name), size = 1.5) +
            scale_colour_brewer(palette = "Set2") +
  ggtitle("Sheffield/Rotherham\npercent in employment, change over 5 censuses\ntop & bottom 3 wards") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  ylab('percent employed') + 
  xlab('census year')

output

ggsave('RothShefOutputs/RothShefEmp5census.png',height=4,width=7,dpi=150)

#COMPARE TO WHOLE GB. Use sample to make betterer. Oh no, needs making wide then sampling then...
#Oh no oh no, I can use a label sample
labelsample <- sample(unique(fivez_emp$label),1000)

output <- ggplot() +
  geom_line(data = fivez_emp[fivez_emp$label %in% labelsample,], 
            aes(x = factor(year), y = percentEmployed, group = label),
            alpha = 0.1, size = 0.1) +
  geom_line(data = fivez_empShefRoth[fivez_empShefRoth$label %in% topAndBottomX$label,], 
            aes(x = factor(year), y = percentEmployed, group = name, colour = name), size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  ggtitle("Sheffield/Rotherham\npercent in employment\ncompared to Great Britain") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  ylab('percent employed') + 
  xlab('census year')

output

ggsave('RothShefOutputs/GBvsRothShefEmp5census.png',height=4,width=7,dpi=150)



#ROTHERHAM ONLY
topAndBottomX <- fivez_empShefRoth %>% 
  filter(label %in% rothOnly$label) %>% 
  group_by(label) %>% 
  summarise(avs = mean(percentEmployed)) %>% 
  mutate(rank = rank(avs)) %>% 
  filter(rank < 4 | rank > (max(rank)-3))


#whut? Oh, two Parks
#whut <- fivez_empShefRoth[fivez_empShefRoth$label %in% topAndBottomX$label,]

output <- ggplot() +
  geom_line(data = fivez_empShefRoth %>% filter(label %in% rothOnly$label), 
            aes(x = factor(year), y = percentEmployed, group = name),
            alpha = 0.1, size = 0.25) +
  geom_line(data = fivez_empShefRoth[fivez_empShefRoth$label %in% topAndBottomX$label,], 
            aes(x = factor(year), y = percentEmployed, group = name, colour = name), size = 1.5) +
  scale_colour_brewer(palette = "Set2")+
  ggtitle("Rotherham\npercent in employment, change over 5 censuses\ntop & bottom 3 wards") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  ylab('percent employed') + 
  xlab('census year')

output

ggsave('RothShefOutputs/RothOnlyEmp5census.png',height=4,width=7,dpi=150)
  #guides(group = F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Map data prep: diffs between censuses----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Employment diffs----

#So wide for choosing vars in QGIS. And can then easily add changes.
#Start with emp
empWide <- fivez_empShefRoth %>% 
  dplyr::select(label,percentEmployed,year) %>% 
  spread(year,percentEmployed)

empWide$diff71to11 <- empWide$`2011`-empWide$`1971`
empWide$diff91to11 <- empWide$`2011`-empWide$`1991`
empWide$diff91to01 <- empWide$`2001`-empWide$`1991`
empWide$diff01to11 <- empWide$`2011`-empWide$`2001`

empWide$diff71to81 <- empWide$`1981`-empWide$`1971`
empWide$diff71to91 <- empWide$`1991`-empWide$`1971`
empWide$diff81to91 <- empWide$`1991`-empWide$`1981`


write_csv(empWide,'R_data/empWide.csv')

#Non-UK-born diffs----

#This is still in raw numbers. Need to proportion up - per zone makes sense.

#Do proportion tables and recombine
fiveCoBprops <- lapply(split(fivez_cobShefRoth,fivez_cobShefRoth$year), function(x) prop.table(as.matrix(x[,c(2:10)]),1) * 100)

#This may or may not have worked...
fiveCoBprops <- lapply(fiveCoBprops, data.frame)
fiveCoBprops <- lapply(c(1:5), 
                       function(x) cbind(split(fivez_cobShefRoth,fivez_cobShefRoth$year)[[x]][,c('label','year')],
                                         fiveCoBprops[[x]]))

fiveCoBprops <- do.call('rbind',fiveCoBprops)

#Repeat for whole of GB
fiveCoBpropsGB <- lapply(split(fivez_cob,fivez_cob$year), function(x) prop.table(as.matrix(x[,c(2:10)]),1) * 100)

#This may or may not have worked...
fiveCoBpropsGB <- lapply(fiveCoBpropsGB, data.frame)
fiveCoBpropsGB <- lapply(c(1:5), 
                       function(x) cbind(split(fivez_cob,fivez_cob$year)[[x]][,c('label','year')],
                                         fiveCoBpropsGB[[x]]))

fiveCoBpropsGB <- do.call('rbind',fiveCoBpropsGB)

#Now we can sum the proportion columns per zone
fiveCoBprops <- fiveCoBprops %>% 
  mutate(UK = England+Scotland+Wales+Rest.of.UK,non.UK = Irish.Republic+India+Pakistan+Europe+Rest.of.world)

fiveCoBpropsGB <- fiveCoBpropsGB %>% 
  mutate(UK = England+Scotland+Wales+Rest.of.UK,non.UK = Irish.Republic+India+Pakistan+Europe+Rest.of.world)

#Prep for QGIS: single columns for each decade then for UK vs non-UK
forQGIS1 <- fiveCoBprops %>% 
  dplyr::select(label,year,UK) %>% 
  spread(key = year, value = UK)

forQGIS2 <- fiveCoBprops %>% 
  dplyr::select(label,year,non.UK) %>% 
  spread(key = year, value = non.UK)
  
names(forQGIS1) <- c('label',paste0(names(forQGIS1[2:6]),'.UK'))
names(forQGIS2) <- c('label',paste0(names(forQGIS2[2:6]),'non.UK'))

forQGIS <- merge(forQGIS1,forQGIS2,by='label')

#New change column
forQGIS$nonUK1971to2011 <- forQGIS$`2011non.UK`-forQGIS$`1971non.UK`

#save!
write_csv(forQGIS,'R_data/ShefRoth_CoBproportions5census.csv')

#~~~~~~~~~~~~~~~~~~~~~
#Graphing CoB data----
#~~~~~~~~~~~~~~~~~~~~~

#Looking just at non-UK
nonUK <- fiveCoBprops %>% 
  dplyr::select(label,year,non.UK)

#Create rank for ordering
nonUK <- nonUK %>% 
  group_by(year) %>% 
  mutate(rank = rank(non.UK,ties.method = 'random'))

#For GB background plotting
nonUKGB <- fiveCoBpropsGB %>% 
  dplyr::select(label,year,non.UK)

#Get ward names
nonUK <- merge(nonUK,data.frame(shefRoth),by='label')

# topAndBottomX <- nonUK %>% 
#   filter(label %in% shefRoth$label) %>% 
#   group_by(label) %>% 
#   summarise(avs = mean(non.UK)) %>% 
#   mutate(rank = rank(avs)) %>% 
#   filter(rank < 4 | rank > (max(rank)-3))

#Or just top five. Keeping same name just to save changing code
#And to make sure I'm confused later.
topAndBottomX <- nonUK %>% 
  filter(label %in% shefRoth$label) %>% 
  group_by(label) %>% 
  summarise(avs = mean(non.UK)) %>% 
  mutate(rank = rank(avs)) %>% 
  #filter(rank < 6)
  filter(rank > (max(rank)-5))

#factor order for names
nonUK$name <- factor(nonUK$name)
nonUK$name <- reorder(nonUK$name,-nonUK$rank)

#PLOT THAT MOFO
output <- ggplot() +
  geom_line(data = nonUK, 
            aes(x = factor(year), y = non.UK, group = label),
            alpha = 0.1, size = 0.25) +
  geom_line(data = nonUK[nonUK$label %in% topAndBottomX$label,], 
            aes(x = factor(year), y = non.UK, group = name, colour = name), size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  ggtitle("Sheffield/Rotherham\npercent non UK-born, change over 5 censuses\ntop 5 wards") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  ylab('percent non UK-born') + 
  xlab('census year')

output

ggsave('RothShefOutputs/ShefRothnonUK5census.png',height=4,width=7,dpi=150)


#COMPARE TO WHOLE GB. UUGLLLYY.
output <- ggplot() +
  geom_line(data = nonUKGB[nonUKGB$label %in% labelsample,], 
            aes(x = factor(year), y = non.UK, group = label),
            alpha = 0.1, size = 0.1) +
  geom_line(data = nonUK[nonUK$label %in% topAndBottomX$label,], 
            aes(x = factor(year), y = non.UK, group = name, colour = name), size = 1.5) +
  scale_colour_brewer(palette = "Set2")

output



#ROTHERHAM ONLY
topAndBottomX <- nonUK %>% 
  filter(label %in% rothOnly$label) %>% 
  group_by(label) %>% 
  summarise(avs = mean(non.UK)) %>% 
  mutate(rank = rank(avs)) %>% 
  #filter(rank < 6)
  filter(rank > (max(rank)-5))


output <- ggplot() +
  geom_line(data = nonUK %>% filter(label %in% rothOnly$label), 
            aes(x = factor(year), y = non.UK, group = label),
            alpha = 0.1, size = 0.25) +
  geom_line(data = nonUK[nonUK$label %in% topAndBottomX$label,], 
            aes(x = factor(year), y = non.UK, group = name, colour = name), size = 1.5) +
  scale_colour_brewer(palette = "Set2") +
  ggtitle("Rotherham\npercent non UK-born, change over 5 censuses\ntop 5 wards") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5)) +
  ylab('percent non UK-born') + 
  xlab('census year')

output

ggsave('RothShefOutputs/RothOnlynonUK5census.png',height=4,width=7,dpi=150)

#gwilym asked for some tables too, didn't he? So let's save some.
cob5forsaving <- merge(fiveCoBprops,data.frame(shefRoth),by='label')

cob5forsaving <- cob5forsaving[order(cob5forsaving$year),c(1,14,2:13)]

#Employment...
emp4saving <- merge(empWide,data.frame(shefRoth),by='label')
emp4saving <- emp4saving[,c(1,14,2:13)]

write_csv(cob5forsaving,"RothShefOutputs/countryOfBirthProportionsPerWard_1971to2011_SheffieldRotherham.csv")
write_csv(emp4saving,"RothShefOutputs/employmentPercentPerWard_1971to2011_SheffieldRotherham.csv")




