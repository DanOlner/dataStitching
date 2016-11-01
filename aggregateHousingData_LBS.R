#RoS housing data to index for three censuses. 
#PIP to 2011 IZs
#We only have back to 1990, so can only cover 3 censuses
geolibs <- c("dplyr", "tidyr","assertthat","ggmap","rgdal","rgeos","maptools","dplyr","tidyr","tmap","raster")
lapply(geolibs, require, character.only = TRUE)

#Get housing data
# hse <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/TIDIER_oldnew_addressBaseGeocodeMerge3.rds")
#hse <- read.csv("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/repeatSales_22_5_16.csv")
#saveRDS(hse,"C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/repeatSales_22_5_16.rds")
#hse <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/repeatSales_22_5_16.rds")
hse <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/SingleSalesPlusRepeatSales_filtered_July16.rds")

#Keep only geocoded ones
hse <- hse %>% filter(!is.na(eastingsFinal))

#Keep only relevant data: price, location, date
hse <- hse %>% dplyr::select(date,priceFinal,eastingsFinal,northingsFinal)

#Get geography to PIP to
izs <- readOGR(dsn="C:/Data/MapPolygons/Scotland/1991/pseudoPCS_aggregated4CorrectCount", 
                      layer="pseudoPCS_aggregated4CorrectCount")

#or use one I already have
izs <- lrg

hse <- data.frame(hse)

#Spatialise housing points
coordinates(hse) <- ~ eastingsFinal + northingsFinal
proj4string(hse) <- proj4string(izs)

hse_izlink <- hse %over% izs %>% dplyr::select(label)

hse_izlink_df <- data.frame(hse_izlink)

#So this should match the same order, be able to add straight in, right?
class(hse_izlink_df$label)
#It came out as a factor too, not a data.frame. Better.
#(This was after a lot of crashes...)

hse@data$label <- hse_izlink_df$label

#Just gonna save that in case it crashes again.
#Oh it crashed while saving.
saveRDS(hse,"C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/hse_w_91_PCS_noZeros.rds")

#Save as shapefile, wanna look at to check.
#replace NAs with something easily readable in QGIS (doesn't seem to be selecting NULL)
hse@data$label[is.na(hse@data$label)] %>% length

hse_df <- data.frame(hse)
hse_df[is.na(hse_df$label),]

plot(izs)
points(hse[is.na(hse$label),],col="green")

hse@data$label <- as.character(hse@data$label)
hse@data$label[is.na(hse@data$label)] <- 'no zone'
writeSpatialShape(hse,"C:/Data/temp/QGIS/census/check_hse_pip.shp")

#hse@data$label <- as.character(hse@data$label)

unique(hse@data$label) %>% length
table(0 + (is.na(unique(hse@data$label))))


#So some points that didn't get to go into any IZ. Where are they?
plot(izs)
points(hse[is.na(hse$label),], col="red")

#Checked in C:\Data\Census\QGIS/housesCodedToIZs_checks.qgs
#They're remote properties just over borders. Going to exclude.
#Gets us back to 1279 zones
nrow(hse[is.na(hse$label),])

#Just changed to no zone above
hse <- hse[hse@data$label!='no zone',]

#Or haven't if I just skipped all those checks
hse <- hse[!is.na(hse@data$label),]

#save that!
saveRDS(hse,"C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/oldnewcombined_spatialPointsDF_1991_PCSadded.rds")
hse <- readRDS("C:/Data/Housing/JessieExtDrive/Misc_RoS_R_Saves/oldnewcombined_spatialPointsDF_1991_PCSadded.rds")

#hse_df <- data.frame(hse)

#~~~~~~~~~~~~~~~~~~
#Find mean price values per IZ zone for bunch of time points

#Let's just check it looks plausible. Count per zone
#Yup, looks plausible!
hse_df <- data.frame(hse)
cnt <- aggregate(hse_df$priceFinal, by =list(hse_df$label), length)

#So at this point, all zones have at least some in.
summary(cnt)

#Label up some date periods.
#Six months each side of census dates.
hse_df$censusYear <- NA

hse_df$date <- as.Date(hse_df$date)

#Year bandwidth
# hse_df$censusYear[hse_df$date > (as.Date('01-10-1990', format = '%d-%m-%Y'))
#                   & hse_df$date < (as.Date('30-09-1991', format = '%d-%m-%Y'))] <- "1991"
# 
# hse_df$censusYear[hse_df$date > (as.Date('01-10-2000', format = '%d-%m-%Y'))
#                   & hse_df$date < (as.Date('30-09-2001', format = '%d-%m-%Y'))] <- "2001"
# 
# hse_df$censusYear[hse_df$date > (as.Date('01-10-2010', format = '%d-%m-%Y'))
#                   & hse_df$date < (as.Date('30-09-2011', format = '%d-%m-%Y'))] <- "2011"

#Larger range to get higher numbers per zone
# hse_df$censusYear[hse_df$date > (as.Date('01-01-1990', format = '%d-%m-%Y'))
#                   & hse_df$date < (as.Date('31-03-1992', format = '%d-%m-%Y'))] <- "1991"
# 
# hse_df$censusYear[hse_df$date > (as.Date('01-04-2000', format = '%d-%m-%Y'))
#                   & hse_df$date < (as.Date('31-03-2002', format = '%d-%m-%Y'))] <- "2001"
# 
# hse_df$censusYear[hse_df$date > (as.Date('01-04-2010', format = '%d-%m-%Y'))
#                   & hse_df$date < (as.Date('31-03-2012', format = '%d-%m-%Y'))] <- "2011"

#Changing to larger range - slight skew for earlier prices - to get more data
hse_df$censusYear[hse_df$date > (as.Date('01-01-1990', format = '%d-%m-%Y'))
                  & hse_df$date < (as.Date('31-03-1994', format = '%d-%m-%Y'))] <- "1991"

hse_df$censusYear[hse_df$date > (as.Date('01-04-1999', format = '%d-%m-%Y'))
                  & hse_df$date < (as.Date('31-03-2003', format = '%d-%m-%Y'))] <- "2001"

hse_df$censusYear[hse_df$date > (as.Date('01-04-2009', format = '%d-%m-%Y'))
                  & hse_df$date < (as.Date('31-03-2013', format = '%d-%m-%Y'))] <- "2011"

table(hse_df$censusYear, useNA = 'always')
#1991    2001    2011    <NA> 
#145006  160528   87843 1344003

#Keep only those with census dates
hse_c <- hse_df %>% filter(!is.na(censusYear))

#Check on counts per zone for those decades... Hmm, small numbers... 
#just upped to two-year band, better.
cnt <- data.frame(table(hse_c$censusYear,hse_c$label)) %>% arrange(Var1)

#Same as before: 1991 is missing three (has zero for those, I think.)
cnt %>% group_by(Var1) %>% summarise(min = min(Freq))
cnt %>% group_by(Var1) %>% summarise(count = n())

cnt %>% filter(Var1 == '1991', Freq == 0)
cnt %>% filter(Freq == 0)

#1991 is missing:
# Var1   Var2 Freq
# 1 1991 6332CD    0
# 2 1991 6756AF    0


#For just those three, what's the date pattern? Back to original pipped
threez <- hse[hse@data$label %in% c('S02001689','S02001992','S02001755'),]

threezdf <- data.frame(threez)

output <- ggplot(threezdf, aes(date, fill=label)) + 
  geom_area(alpha = 0.3, stat="bin", position="identity", colour="black",binwidth=365)
# output <- ggplot(alldates, aes(date, fill=type)) + geom_density(alpha = 0.2)
output

#So here's one: it wasn't developed until the late 90s:
#https://en.wikipedia.org/wiki/Duloch
#Inverness Inshes, also later 90s:
#https://en.wikipedia.org/wiki/Milton_of_Leys

#Harris does have some but not many. Must have got more developed.
#Let's look:
harris <- data.frame(hse[hse@data$label %in% c('S02001689'),]) %>% arrange(date)

#Yearly markers
harris$year <- as.numeric(cut(harris$date, breaks="year"))

#save to plot yearly build
write.csv(harris,"C:/Data/Census/QGIS/harrisyears.csv")

#So just to confirm
uniqueZonesCensus <- subset(hse_df, !duplicated(hse_df[,c('label','censusYear')]))

#1991 2001 2011 
#1276 1279 1279
table(uniqueZonesCensus$censusYear)

#Yup. So when I filter out the NAs that had no date assigned, that happens...

#Are they particularly low means in the others?
# cnt <- aggregate(hse_c$priceFinal, by =list(hse_c$label,hse_c$censusYear), length)
# 
# cnt[cnt$Group.1=="S02001689",]
# 
# sapply(unique(cnt$Group.2), function(x) mean(cnt$x[cnt$Group.2==x]))
# #1991      2001      2011 
# #167.38871 168.76310  99.44488
# sapply(unique(cnt$Group.2), function(x) sd(cnt$x[cnt$Group.2==x]))
#1991     2001     2011 
#94.93036 83.06930 45.42870

#save smaller range of years to look at in QGIS
#write.csv(hse_df[hse_df$date < as.Date('01-01-1993',format='%d-%m-%Y'),],"temp/housesBefore93.csv")

#Anyway: save housing-mapped-to-IZs result!

#Or don't. Those aren't yet actually zone / year averages. What exactly was I thinking there?
hse_c_avs <- hse_c %>% group_by(censusYear,label) %>% 
  summarise(meanPrice = mean(priceFinal), salesCount = n()) %>% 
  data.frame

summary(hse_c_avs$meanPrice)

#Check missing values for zones
chk4missinz  <- hse_c_avs %>% group_by(censusYear) %>% 
  summarise(countZones = n())

#Only one missing zone this time. 
#Update: wider range above, so now no missing. But what numbers of sales?
#One zone with only one sale. Quite a few with less than ten.
#Guess we'll have to have a cut-off somewhere.

#Some of those zones are probably gonna be averages from very small
#numbers of properties. I should probably look at that.
#So count of properties per zone per decade and boxplot
countOfProps <-  hse_c %>% 
  group_by(censusYear,label) %>% 
  summarise(propertyCount = n())

output <- ggplot(countOfProps, aes(x = factor(censusYear), y = propertyCount)) + 
  geom_boxplot() +
  scale_y_log10()

output

write.csv(hse_c_avs,"Housing/1991to2011_twoYearBandMeanPrices_1991_PCSnoZeroes_Nov16.csv", row.names = F)
saveRDS(hse_c_avs,"Housing/1991to2011_twoYearBandMeanPrices_1991_PCSnoZeroes_Nov16.rds")

















# #Repeating things...
# #OK, mean price per zone per census
# hse_summary <- hse_c %>% group_by(censusYear,label) %>% 
#   summarise(meanPricePerCensus_n_Zone = mean(priceFinal))
# 
# #Short by one zone. But right number of zones in general, right?
# #Err. One too many. WHAT? Oh, there's an NA. Did that get in there from the PiP?
# 
# #OK, dealt with PiP. We're now short by three zones.
# #But we have a list of them all. So for one decade or more...?
# unique(hse_summary$label) %>% length
# 
# #Probably the same one, probably had no values. Eer, newp.
# subset(unique(izs@data$label),!(unique(izs@data$label) %in% unique(hse_summary$label)))
# 
# #Ah: 1991 is missing three. Wassup with that?
# table(hse_summary$censusYear)
# 
# unique(hse_summary$label[hse_summary$censusYear=="1991"]) %>% length
# 
# writeOGR(izs[izs@data$label %in% unique(hse_summary$label[hse_summary$censusYear=="1991"]),], "temp",
#          "2011IZs_missingCheck", driver="ESRI Shapefile", overwrite_layer = T)
